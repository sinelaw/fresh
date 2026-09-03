//! Re-export of `fresh_core::plugin_schemas` so editor-side modules can
//! address it via `crate::plugin_schemas::...` without a deeper
//! refactor. The actual logic lives in fresh-core because the plugin
//! runtime needs to call the validators synchronously from JS bindings.

pub use fresh_core::plugin_schemas::*;

// The committed `plugins/config-schema.json` ships from this crate, so the
// regression tests that read it back live here rather than next to `Config`
// in `fresh-editor-core`.
#[cfg(test)]
mod config_schema_tests {
    use crate::config::Config;

    /// Regression test for #2738: the committed config schema must NOT freeze
    /// the theme as a hard `enum`. It has to be a validated free-form string
    /// carrying the `x-enum-from: "$themes"` dynamic-source hint, so user-theme
    /// config values (paths / URIs / registry keys) validate and the settings
    /// dropdown is sourced live from the registry rather than a static list.
    #[test]
    fn test_config_schema_theme_is_dynamic_string_not_enum() {
        const SCHEMA_JSON: &str = include_str!("../plugins/config-schema.json");
        let schema: serde_json::Value =
            serde_json::from_str(SCHEMA_JSON).expect("config-schema.json must be valid JSON");

        let theme_options = &schema["$defs"]["ThemeOptions"];

        assert_eq!(
            theme_options["type"].as_str(),
            Some("string"),
            "ThemeOptions must be a plain string type"
        );
        assert!(
            theme_options.get("enum").is_none(),
            "ThemeOptions must NOT be a frozen enum (would reject user themes); \
             regenerate with scripts/gen_schema.sh"
        );
        assert_eq!(
            theme_options["x-enum-from"].as_str(),
            Some("$themes"),
            "ThemeOptions must carry the `$themes` dynamic-source hint so the \
             settings dropdown is populated from the live theme registry"
        );
    }

    /// Regression test for #2738: because `theme` is no longer a hard enum, an
    /// arbitrary user-theme config value (a relative path, a `file://` URI, or
    /// a `builtin://` form) is accepted by the schema's string type instead of
    /// being rejected as "not one of the allowed values".
    #[test]
    fn test_user_theme_config_value_validates_against_schema() {
        const SCHEMA_JSON: &str = include_str!("../plugins/config-schema.json");
        let schema: serde_json::Value =
            serde_json::from_str(SCHEMA_JSON).expect("config-schema.json must be valid JSON");
        let theme_options = &schema["$defs"]["ThemeOptions"];

        // The only constraint on a theme value is `type: string` — no `enum`,
        // no `pattern` — so every portable form the registry can emit validates.
        assert_eq!(theme_options["type"].as_str(), Some("string"));
        assert!(theme_options.get("enum").is_none());
        assert!(theme_options.get("pattern").is_none());

        // And a config carrying such a value deserializes cleanly.
        for value in [
            "my-custom-theme.json",
            "packages/nord/dark.json",
            "file:///home/user/.config/fresh/themes/x.json",
            "builtin://dark",
            "dark",
        ] {
            let cfg_json = format!(r#"{{"theme":"{}"}}"#, value);
            let cfg: Config = serde_json::from_str(&cfg_json)
                .unwrap_or_else(|e| panic!("theme value {:?} should deserialize: {}", value, e));
            assert_eq!(cfg.theme.0, value);
        }
    }
}
