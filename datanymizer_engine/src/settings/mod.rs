mod assert;
mod filter;
mod table;
mod templates;

use crate::{
    transformer::{TransformerDefaults, TransformerInitContext},
    transformers::Transformers,
    Transformer,
};
use anyhow::Result;
use config::{Config, ConfigError, File, FileFormat};
use serde::Deserialize;
use serde_json::Value as JsonValue;
use std::collections::HashMap;
use wildmatch::WildMatch;

pub use filter::{Filter, TableList};
pub use r#assert::{
    Assert, AssertError, AssertExpectation, AssertScope, AssertSeverity, ScalarExpectations,
    TableAssert,
};
pub use table::{Query, Table};
pub use templates::TemplatesCollection;

pub type Tables = Vec<Table>;

type TransformList = Vec<(String, Transformers)>;

#[derive(Debug, Deserialize, Clone)]
pub struct Settings {
    /// Tables list with transformation rules
    #[serde(default)]
    pub tables: Tables,

    /// Table order. All tables not listed are dumping at the beginning
    #[serde(default)]
    pub table_order: Vec<String>,

    /// Default transformers configuration
    #[serde(default)]
    pub default: TransformerDefaults,

    #[serde(default)]
    pub filter: Filter,

    /// SQL assertions executed before the dump starts.
    #[serde(default)]
    pub asserts: Vec<Assert>,

    /// Global values. Visible in any template.
    /// They may be shadowed by template variables.
    pub globals: Option<HashMap<String, JsonValue>>,

    pub templates: Option<TemplatesCollection>,

    #[serde(skip)]
    transform_map: Option<HashMap<String, TransformList>>,
}

impl Settings {
    pub fn new(path: String) -> Result<Self, ConfigError> {
        Self::from_source(File::with_name(&path))
    }

    pub fn from_yaml(config: &str) -> Result<Self, ConfigError> {
        Self::from_source(File::from_str(config, FileFormat::Yaml))
    }

    fn from_source<S>(source: S) -> Result<Self, ConfigError>
    where
        S: config::Source + Send + Sync + 'static,
    {
        let c = Config::builder().add_source(source).build()?;

        let mut settings: Self = c.try_deserialize()?;
        settings.validate()?;
        settings.preprocess();

        Ok(settings)
    }

    fn validate(&self) -> Result<(), ConfigError> {
        for (i, table) in self.tables.iter().enumerate() {
            let has_name = !table.name.is_empty();
            let has_names = table.names.as_ref().is_some_and(|n| !n.is_empty());

            if has_name && has_names {
                return Err(ConfigError::Message(format!(
                    "tables[{}]: cannot specify both `name` and `names` — use one or the other",
                    i
                )));
            }
            if !has_name && !has_names {
                return Err(ConfigError::Message(format!(
                    "tables[{}]: must specify either `name` or `names`",
                    i
                )));
            }
        }
        Ok(())
    }

    pub fn transformers_for(&self, table: &str) -> Option<&TransformList> {
        if let Some(m) = &self.transform_map {
            m.get(table)
        } else {
            panic!("No transform map");
        }
    }

    pub fn get_table(&self, name: &str) -> Option<&Table> {
        self.tables.iter().find(|t| t.name == name)
    }

    /// For a table that has been registered via [`register_table_transforms`], returns:
    /// - the display label of the config rule(s) that matched it. When both a
    ///   specific and a wildcard entry contributed, the label is rendered as
    ///   `"<specific> + <wildcard>"` so dry-run output makes the merge visible.
    /// - the list of anonymized column names
    ///
    /// Returns `None` if no config rule matches or no columns are anonymized.
    pub fn dry_run_info<T: AsRef<str>>(
        &self,
        full_name: &str,
        names: &[T],
    ) -> Option<(String, Vec<String>)> {
        let (specific, wildcard) = self.find_specific_and_wildcard(names);
        let key = self.transform_key_for(full_name, specific, wildcard)?;

        let transforms = self.transformers_for(&key)?;
        if transforms.is_empty() {
            return None;
        }

        let specific_label = specific.and_then(|t| Self::matching_pattern_label(t, names));
        let wildcard_label = wildcard.and_then(|t| Self::matching_pattern_label(t, names));

        let rule_label = match (specific_label, wildcard_label) {
            (Some(s), Some(w)) => format!("{s} + {w}"),
            (Some(s), None) => s,
            (None, Some(w)) => w,
            (None, None) => String::new(),
        };

        let columns = transforms
            .iter()
            .map(|(col_name, _)| col_name.clone())
            .collect();

        Some((rule_label, columns))
    }

    /// Returns the first pattern in `cfg` that matches any of `names`, used
    /// purely for human-readable dry-run labelling.
    fn matching_pattern_label<T: AsRef<str>>(cfg: &Table, names: &[T]) -> Option<String> {
        cfg.patterns()
            .into_iter()
            .find(|pat| {
                let matcher = WildMatch::new(pat);
                names.iter().any(|n| matcher.matches(n.as_ref()))
            })
            .map(|s| s.to_string())
    }

    /// Returns global and table-local assertions in execution order.
    pub fn all_asserts(&self) -> Vec<AssertScope<'_>> {
        let mut asserts = Vec::new();

        for assert in &self.asserts {
            asserts.push(AssertScope::Global(assert));
        }

        for table in &self.tables {
            for assert in &table.asserts {
                asserts.push(AssertScope::Table(TableAssert { table, assert }));
            }
        }

        asserts
    }

    /// Returns the most specific matching table config for the given candidate
    /// names. If both a specific (non-wildcard) entry and a wildcard entry
    /// match, the specific entry is returned — but at dump time the merged rule
    /// set actually applied is the union of both (see
    /// [`register_table_transforms`]).
    pub fn find_table<T: AsRef<str>>(&self, names: &[T]) -> Option<&Table> {
        let (specific, wildcard) = self.find_specific_and_wildcard(names);
        specific.or(wildcard)
    }

    /// Splits matching table configs into a "specific" match (exact `name`
    /// field, or non-wildcard pattern in `names` field) and a "wildcard" match
    /// (any pattern containing `*` or `?`).
    ///
    /// Specific match: candidate names are tried in order, so a fully-qualified
    /// name (e.g. `public.users`) beats a short name when both are listed —
    /// preserving the prior `find_table` behavior for callers that rely on it.
    /// Wildcard match: the first config-order entry wins, preserving prior
    /// "first wildcard wins" semantics for overlapping wildcards.
    ///
    /// Replaces the previous "first-match-wins" logic across the whole table
    /// list, which silently dropped rules from later entries when an earlier
    /// wildcard claimed the table.
    fn find_specific_and_wildcard<T: AsRef<str>>(
        &self,
        names: &[T],
    ) -> (Option<&Table>, Option<&Table>) {
        let is_wild = |s: &str| s.contains('*') || s.contains('?');

        // Specific match: name-order preferred (full_name beats short_name).
        let mut specific: Option<&Table> = None;
        'specific: for name in names {
            for table_cfg in &self.tables {
                for pattern in table_cfg.patterns() {
                    if pattern.is_empty() || is_wild(pattern) {
                        continue;
                    }
                    if pattern == name.as_ref() {
                        specific = Some(table_cfg);
                        break 'specific;
                    }
                }
            }
        }

        // Wildcard match: config-order preferred (first matching wildcard wins).
        let mut wildcard: Option<&Table> = None;
        'wildcard: for table_cfg in &self.tables {
            for pattern in table_cfg.patterns() {
                if pattern.is_empty() || !is_wild(pattern) {
                    continue;
                }
                let matcher = WildMatch::new(pattern);
                if names.iter().any(|n| matcher.matches(n.as_ref())) {
                    wildcard = Some(table_cfg);
                    break 'wildcard;
                }
            }
        }

        (specific, wildcard)
    }

    /// The key under which merged transforms are registered in `transform_map`.
    /// A specific entry with a non-wildcard `name` field uses that name (so
    /// existing exact-name-based callers like `transformers_for("users")`
    /// keep working); everything else uses the discovered `full_name`.
    fn transform_key_for(
        &self,
        full_name: &str,
        specific: Option<&Table>,
        wildcard: Option<&Table>,
    ) -> Option<String> {
        if specific.is_none() && wildcard.is_none() {
            return None;
        }
        Some(match specific {
            Some(s) if !s.name.is_empty() && !s.has_wildcards() => s.name.clone(),
            _ => full_name.to_string(),
        })
    }

    /// Public entry point used by the dumper to compute the lookup key for a
    /// given table without exposing the internal `Table` references.
    pub fn transform_key<T: AsRef<str>>(&self, full_name: &str, names: &[T]) -> Option<String> {
        let (specific, wildcard) = self.find_specific_and_wildcard(names);
        self.transform_key_for(full_name, specific, wildcard)
    }

    /// Registers resolved transforms for a discovered table, merging rules
    /// from any matching wildcard entry with rules from any matching specific
    /// entry. Specific rules win on column collision; wildcard rules apply
    /// only to columns that actually exist in the table (lenient mode).
    /// Called from the dumper after table/column metadata is known.
    pub fn register_table_transforms(
        &mut self,
        full_name: &str,
        short_name: &str,
        actual_columns: &[String],
    ) {
        let names = [full_name, short_name];
        let (specific, wildcard) = self.find_specific_and_wildcard(&names);
        if specific.is_none() && wildcard.is_none() {
            return;
        }

        // Layer 1: wildcard rules, filtered to columns that exist on this table
        let mut merged: HashMap<String, Transformers> = HashMap::new();
        if let Some(w) = wildcard {
            for (col, t) in w.rules.iter() {
                if actual_columns.iter().any(|c| c == col) {
                    merged.insert(col.clone(), t.clone());
                }
            }
        }

        // Layer 2: specific rules — applied unconditionally (strict mode), and
        // overwriting any wildcard rule for the same column
        if let Some(s) = specific {
            for (col, t) in s.rules.iter() {
                merged.insert(col.clone(), t.clone());
            }
        }

        if merged.is_empty() {
            return;
        }

        // rule_order: specific takes precedence; fall back to wildcard
        let explicit_rule_order = specific
            .and_then(|s| s.rule_order.clone())
            .or_else(|| wildcard.and_then(|w| w.rule_order.clone()))
            .unwrap_or_default();

        let mut transform_list: TransformList = merged.into_iter().map(|(k, v)| (k, v)).collect();
        transform_list
            .sort_by_cached_key(|(key, _)| explicit_rule_order.iter().position(|i| i == key));

        let key = self
            .transform_key_for(full_name, specific, wildcard)
            .expect("checked above that at least one matched");

        let map = self.transform_map.get_or_insert_with(HashMap::new);
        map.insert(key, transform_list);
    }

    fn preprocess(&mut self) {
        let mut init_ctx = TransformerInitContext::from_defaults(self.default.clone());

        // Assign extend templates to context
        if let Some(collection) = &self.templates {
            init_ctx.template_collection = collection.clone();
        }

        for table in self.tables.iter_mut() {
            for (_name, rule) in table.rules.iter_mut() {
                rule.init(&init_ctx);
            }
        }

        self.fill_transform_map();
    }

    fn fill_transform_map(&mut self) {
        let mut map = HashMap::with_capacity(self.tables.len());
        for table in &self.tables {
            // Only pre-resolve entries with an exact table name.
            // Wildcard/names entries are deferred to register_table_transforms()
            // at dump time when actual table metadata is available.
            if table.names.is_some() || table.has_wildcards() {
                continue;
            }
            map.insert(table.name.clone(), table.transform_list());
        }

        self.transform_map = Some(map);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{transformers::PersonNameTransformer, LocaleConfig};
    use serde_json::json;

    #[test]
    fn set_defaults() {
        let config = r#"
            tables:
              - name: user
                rules:
                  name:
                    person_name: {}
                  alias:
                    person_name:
                      locale: EN
            default:
              locale: RU
            "#;

        let s = Settings::from_yaml(config).unwrap();
        let rules = &s.tables.first().unwrap().rules;

        assert_eq!(
            rules["name"],
            Transformers::PersonName(PersonNameTransformer {
                locale: Some(LocaleConfig::RU)
            })
        );
        assert_eq!(
            rules["alias"],
            Transformers::PersonName(PersonNameTransformer {
                locale: Some(LocaleConfig::EN)
            })
        );
    }

    #[test]
    fn find_table() {
        let config = r#"
            tables:
              - name: companies
                rules:
                  name:
                    company_name: {}
              - name: users
                rules:
                  name:
                    person_name: {}
              - name: other_schema.users
                rules:
                  other_name:
                    person_name: {}
            "#;
        let s = Settings::from_yaml(config).unwrap();

        let t = s.find_table(&["some_table"]);
        assert!(t.is_none());

        let t = s.find_table(&["some_table", "users"]);
        assert_eq!(t.unwrap().name, "users");

        let t = s.find_table(&["users", "other_schema.users"]);
        assert_eq!(t.unwrap().name, "users");

        let t = s.find_table(&["other_schema.users", "users"]);
        assert_eq!(t.unwrap().name, "other_schema.users");
    }

    mod transformers_for {
        use super::*;

        fn rule_names(s: &Settings, t: &str) -> Vec<String> {
            s.transformers_for(t)
                .unwrap()
                .iter()
                .map(|(name, _)| name.to_string())
                .collect()
        }

        #[test]
        fn order() {
            let config = r#"
                tables:
                  - name: table1
                    rule_order:
                      - greeting
                      - options
                    rules:
                      options:
                        template:
                          format: "{greeting: \"{{ final.greeting }}\"}"
                      greeting:
                        template:
                          format: "dear {{ final.first_name }} {{ final.last_name }}"
                      first_name:
                        first_name: {}
                      last_name:
                        last_name: {}
                  - name: table2
                    rules:
                      first_name:
                        first_name: {}
                      last_name:
                        last_name: {}
                "#;
            let s = Settings::from_yaml(config).unwrap();

            let names = rule_names(&s, "table1");
            assert_eq!(names.len(), 4);
            assert!(names.contains(&"first_name".to_string()));
            assert!(names.contains(&"last_name".to_string()));
            assert_eq!(names[2], "greeting");
            assert_eq!(names[3], "options");

            let names = rule_names(&s, "table2");
            assert_eq!(names.len(), 2);
            assert!(names.contains(&"first_name".to_string()));
            assert!(names.contains(&"last_name".to_string()));

            assert_eq!(s.transformers_for("table3"), None);
        }
    }

    mod templates_for {
        use super::*;

        fn get_raw_templates(s: &Settings) -> Vec<String> {
            s.templates
                .clone()
                .unwrap()
                .raw
                .unwrap()
                .keys()
                .map(|key| key.to_string())
                .collect()
        }

        fn get_files_templates(s: &Settings) -> Vec<String> {
            s.templates.clone().unwrap().files.unwrap()
        }

        #[test]
        fn read_templates() {
            let config = r#"
                tables: []
                templates:
                  raw:
                    template1: "template1"
                    template2: |
                      template2-line-1
                      template2-line-2
                  files:
                    - ./templates/path1
                    - ./templates/path2
                "#;
            let s = Settings::from_yaml(config).unwrap();

            assert_eq!(get_raw_templates(&s).len(), 2);
            assert_eq!(get_files_templates(&s).len(), 2);
        }
    }

    #[test]
    fn collects_global_and_table_asserts() {
        let config = r#"
            asserts:
              - name: global_check
                sql: select count(*) from users
                expect:
                  eq: 1
            tables:
              - name: users
                rules: {}
                asserts:
                  - name: table_check
                    sql: select 1 where false
                    expect: no_rows
            "#;

        let settings = Settings::from_yaml(config).unwrap();
        let asserts = settings.all_asserts();

        assert_eq!(asserts.len(), 2);
        assert_eq!(asserts[0].assert().name, "global_check");
        assert_eq!(asserts[0].scope_name(), "global");
        assert_eq!(asserts[1].assert().name, "table_check");
        assert_eq!(asserts[1].scope_name(), "users");
    }

    #[test]
    fn parses_global_asserts() {
        let config = r#"
            asserts:
              - name: users_count
                sql: select count(*) from users
                expect:
                  eq: 0
            tables: []
            "#;

        let settings = Settings::from_yaml(config).unwrap();

        assert_eq!(settings.asserts.len(), 1);
        assert_eq!(
            settings.asserts[0].expect,
            AssertExpectation::Scalar(Box::new(ScalarExpectations {
                eq: Some(json!(0)),
                not_eq: None,
                gt: None,
                gte: None,
                lt: None,
                lte: None,
            }))
        );
    }

    mod validation {
        use super::*;

        #[test]
        fn rejects_both_name_and_names() {
            let config = r#"
                tables:
                  - name: "public.users"
                    names:
                      - "A.*"
                    rules:
                      email:
                        person_name: {}
                "#;
            let err = Settings::from_yaml(config).unwrap_err();
            assert!(
                err.to_string().contains("cannot specify both"),
                "Expected 'cannot specify both' error, got: {}",
                err
            );
        }

        #[test]
        fn rejects_neither_name_nor_names() {
            let config = r#"
                tables:
                  - rules:
                      email:
                        person_name: {}
                "#;
            let err = Settings::from_yaml(config).unwrap_err();
            assert!(
                err.to_string().contains("must specify either"),
                "Expected 'must specify either' error, got: {}",
                err
            );
        }

        #[test]
        fn rejects_empty_names_list() {
            let config = r#"
                tables:
                  - names: []
                    rules:
                      email:
                        person_name: {}
                "#;
            let err = Settings::from_yaml(config).unwrap_err();
            assert!(
                err.to_string().contains("must specify either"),
                "Expected 'must specify either' error, got: {}",
                err
            );
        }
    }

    mod wildcard_table_matching {
        use super::*;

        #[test]
        fn wildcard_name_matches() {
            let config = r#"
                tables:
                  - name: "public.*"
                    rules:
                      email:
                        person_name: {}
                "#;
            let s = Settings::from_yaml(config).unwrap();

            let t = s.find_table(&["public.users"]);
            assert!(t.is_some());

            let t = s.find_table(&["other.users"]);
            assert!(t.is_none());
        }

        #[test]
        fn exact_match_beats_wildcard() {
            let config = r#"
                tables:
                  - name: "public.*"
                    rules:
                      email:
                        person_name: {}
                  - name: public.users
                    rules:
                      email:
                        first_name: {}
                "#;
            let s = Settings::from_yaml(config).unwrap();

            // Exact match should win
            let t = s.find_table(&["public.users", "users"]);
            assert_eq!(t.unwrap().name, "public.users");

            // Wildcard should match other tables
            let t = s.find_table(&["public.orders", "orders"]);
            assert_eq!(t.unwrap().name, "public.*");
        }

        #[test]
        fn names_field_with_multiple_patterns() {
            let config = r#"
                tables:
                  - names:
                      - "A.*"
                      - "B.*"
                    rules:
                      email:
                        person_name: {}
                "#;
            let s = Settings::from_yaml(config).unwrap();

            assert!(s.find_table(&["A.users"]).is_some());
            assert!(s.find_table(&["B.orders"]).is_some());
            assert!(s.find_table(&["C.stuff"]).is_none());
        }

        #[test]
        fn first_wildcard_entry_wins() {
            let config = r#"
                tables:
                  - name: "public.*"
                    rules:
                      email:
                        person_name: {}
                  - name: "public.u*"
                    rules:
                      email:
                        first_name: {}
                "#;
            let s = Settings::from_yaml(config).unwrap();

            // First wildcard entry should win
            let t = s.find_table(&["public.users"]);
            assert_eq!(t.unwrap().name, "public.*");
        }

        #[test]
        fn names_field_with_exact_values() {
            let config = r#"
                tables:
                  - names:
                      - "A.users"
                      - "B.orders"
                    rules:
                      email:
                        person_name: {}
                "#;
            let s = Settings::from_yaml(config).unwrap();

            assert!(s.find_table(&["A.users"]).is_some());
            assert!(s.find_table(&["B.orders"]).is_some());
            assert!(s.find_table(&["A.orders"]).is_none());
        }

        #[test]
        fn backward_compat_no_wildcards() {
            let config = r#"
                tables:
                  - name: users
                    rules:
                      name:
                        person_name: {}
                "#;
            let s = Settings::from_yaml(config).unwrap();

            let t = s.find_table(&["public.users", "users"]);
            assert_eq!(t.unwrap().name, "users");
        }
    }

    mod register_table_transforms_tests {
        use super::*;

        fn transform_keys(s: &Settings, table: &str) -> Vec<String> {
            match s.transformers_for(table) {
                Some(list) => list.iter().map(|(name, _)| name.clone()).collect(),
                None => vec![],
            }
        }

        #[test]
        fn exact_and_wildcard_rules_merged() {
            let config = r#"
                tables:
                  - name: users
                    rules:
                      name:
                        person_name: {}
                  - name: "public.*"
                    rules:
                      email:
                        first_name: {}
                "#;
            let mut s = Settings::from_yaml(config).unwrap();

            // Pre-registered by fill_transform_map: exact entry only
            let keys = transform_keys(&s, "users");
            assert_eq!(keys, vec!["name".to_string()]);

            // After register_table_transforms with column metadata, the
            // wildcard's `email` rule (column exists) is merged in alongside
            // the specific entry's `name` rule.
            s.register_table_transforms(
                "public.users",
                "users",
                &["name".to_string(), "email".to_string()],
            );

            let keys = transform_keys(&s, "users");
            assert_eq!(keys.len(), 2);
            assert!(keys.contains(&"name".to_string()));
            assert!(keys.contains(&"email".to_string()));
        }

        #[test]
        fn specific_rules_override_wildcard_on_collision() {
            let config = r#"
                tables:
                  - name: users
                    rules:
                      email:
                        person_name: {}
                  - name: "*"
                    rules:
                      email:
                        first_name: {}
                "#;
            let mut s = Settings::from_yaml(config).unwrap();

            s.register_table_transforms(
                "public.users",
                "users",
                &["id".to_string(), "email".to_string()],
            );

            // The specific entry's `person_name` transformer should win
            let transforms = s.transformers_for("users").unwrap();
            assert_eq!(transforms.len(), 1);
            let (col, t) = &transforms[0];
            assert_eq!(col, "email");
            // Verify it's the PersonName transformer (specific), not FirstName (wildcard)
            assert!(matches!(t, Transformers::PersonName(_)));
        }

        #[test]
        fn wildcard_rules_apply_to_specifically_matched_tables() {
            // The original bug: specific entry below `*` was shadowed entirely;
            // and even when reordered, the wildcard's column rules were lost
            // for any table that hit a specific entry.
            // After the fix, both contribute.
            let config = r#"
                tables:
                  - name: "*"
                    rules:
                      notes:
                        template:
                          format: "[REDACTED]"
                  - names: [videoconference]
                    rules:
                      name:
                        first_name: {}
                "#;
            let mut s = Settings::from_yaml(config).unwrap();

            s.register_table_transforms(
                "public.videoconference",
                "videoconference",
                &["id".to_string(), "name".to_string(), "notes".to_string()],
            );

            let keys = transform_keys(&s, "public.videoconference");
            assert_eq!(keys.len(), 2);
            assert!(keys.contains(&"name".to_string()));
            assert!(keys.contains(&"notes".to_string()));
        }

        #[test]
        fn wildcard_columns_filtered_by_actual_columns_in_merge() {
            // Wildcard rule for `phone` applies only when the specifically
            // matched table actually has a `phone` column.
            let config = r#"
                tables:
                  - name: "*"
                    rules:
                      phone:
                        person_name: {}
                  - names: [videoconference]
                    rules:
                      name:
                        first_name: {}
                "#;
            let mut s = Settings::from_yaml(config).unwrap();

            // No `phone` column → wildcard rule is dropped, only specific runs
            s.register_table_transforms(
                "public.videoconference",
                "videoconference",
                &["id".to_string(), "name".to_string()],
            );
            let keys = transform_keys(&s, "public.videoconference");
            assert_eq!(keys, vec!["name".to_string()]);

            // With a `phone` column → both wildcard and specific contribute
            s.register_table_transforms(
                "public.videoconference2",
                "videoconference2",
                &["name".to_string(), "phone".to_string()],
            );
            let keys = transform_keys(&s, "public.videoconference2");
            // Note: "videoconference2" doesn't match the specific entry, so
            // only the wildcard fires here.
            assert_eq!(keys, vec!["phone".to_string()]);
        }

        #[test]
        fn names_field_registers_correctly() {
            let config = r#"
                tables:
                  - names:
                      - "A.*"
                      - "B.*"
                    rules:
                      email:
                        person_name: {}
                "#;
            let mut s = Settings::from_yaml(config).unwrap();

            s.register_table_transforms(
                "A.users",
                "users",
                &["id".to_string(), "email".to_string()],
            );
            s.register_table_transforms(
                "B.orders",
                "orders",
                &["id".to_string(), "email".to_string()],
            );

            let keys_a = transform_keys(&s, "A.users");
            assert_eq!(keys_a.len(), 1);
            assert!(keys_a.contains(&"email".to_string()));

            let keys_b = transform_keys(&s, "B.orders");
            assert_eq!(keys_b.len(), 1);
            assert!(keys_b.contains(&"email".to_string()));
        }

        #[test]
        fn wildcard_table_exact_column_missing_silently_skipped() {
            let config = r#"
                tables:
                  - name: "public.*"
                    rules:
                      email:
                        person_name: {}
                      phone:
                        person_name: {}
                "#;
            let mut s = Settings::from_yaml(config).unwrap();

            // Table has email but not phone — phone should be silently skipped
            s.register_table_transforms(
                "public.logs",
                "logs",
                &["id".to_string(), "email".to_string()],
            );

            let keys = transform_keys(&s, "public.logs");
            assert_eq!(keys.len(), 1);
            assert!(keys.contains(&"email".to_string()));
            assert!(!keys.contains(&"phone".to_string()));
        }

        #[test]
        fn names_mixed_exact_and_wildcard_strict_for_exact_match() {
            let config = r#"
                tables:
                  - names:
                      - "_sqlx_migrations"
                      - "ok*"
                    rules:
                      description:
                        person_name: {}
                "#;
            let mut s = Settings::from_yaml(config).unwrap();

            // _sqlx_migrations matches via exact pattern — should keep
            // missing exact columns (strict mode)
            s.register_table_transforms(
                "public._sqlx_migrations",
                "_sqlx_migrations",
                &["id".to_string(), "version".to_string()],
            );

            // "description" doesn't exist but should be kept (exact table match)
            let keys = transform_keys(&s, "public._sqlx_migrations");
            assert_eq!(keys.len(), 1);
            assert!(keys.contains(&"description".to_string()));
        }

        #[test]
        fn names_mixed_exact_and_wildcard_lenient_for_wildcard_match() {
            let config = r#"
                tables:
                  - names:
                      - "_sqlx_migrations"
                      - "ok*"
                    rules:
                      description:
                        person_name: {}
                "#;
            let mut s = Settings::from_yaml(config).unwrap();

            // ok_stuff matches via wildcard pattern — should silently skip
            // missing columns (lenient mode)
            s.register_table_transforms(
                "public.ok_stuff",
                "ok_stuff",
                &["id".to_string(), "name".to_string()],
            );

            // "description" doesn't exist and should be skipped (wildcard table match)
            assert!(s.transformers_for("public.ok_stuff").is_none());
        }

        #[test]
        fn overlapping_wildcards_first_entry_wins() {
            let config = r#"
                tables:
                  - name: "public.*"
                    rules:
                      email:
                        person_name: {}
                  - name: "*users*"
                    rules:
                      phone:
                        person_name: {}
                "#;
            let mut s = Settings::from_yaml(config).unwrap();

            // Both patterns match public.users — first entry (public.*) should win
            s.register_table_transforms(
                "public.users",
                "users",
                &["email".to_string(), "phone".to_string()],
            );

            let keys = transform_keys(&s, "public.users");
            // Should have email (from public.*), not phone (from *users*)
            assert!(keys.contains(&"email".to_string()));
            assert!(!keys.contains(&"phone".to_string()));
        }

        #[test]
        fn names_field_exact_values_registers_correctly() {
            let config = r#"
                tables:
                  - names:
                      - "A.users"
                      - "B.users"
                    rules:
                      email:
                        person_name: {}
                "#;
            let mut s = Settings::from_yaml(config).unwrap();

            // Should not be in transform_map from fill_transform_map
            assert!(s.transformers_for("A.users").is_none());

            s.register_table_transforms(
                "A.users",
                "users",
                &["id".to_string(), "email".to_string()],
            );

            let keys = transform_keys(&s, "A.users");
            assert_eq!(keys.len(), 1);
            assert!(keys.contains(&"email".to_string()));
        }
    }
}
