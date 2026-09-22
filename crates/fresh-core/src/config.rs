//! Configuration types shared across crates

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

fn default_true() -> bool {
    true
}

fn settings_is_empty(v: &serde_json::Value) -> bool {
    match v {
        serde_json::Value::Null => true,
        serde_json::Value::Object(o) => o.is_empty(),
        _ => false,
    }
}

/// Settings for one plugin.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[schemars(extend("x-display-field" = "/enabled"))]
pub struct PluginConfig {
    /// Load and run this plugin (default: true).
    #[serde(default = "default_true")]
    pub enabled: bool,

    /// Plugin file path. Filled in automatically; don't set it by hand.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[schemars(extend("readOnly" = true))]
    pub path: Option<PathBuf>,

    /// The plugin's own settings, as defined by its `<plugin_name>.schema.json`.
    /// Shown in Settings as the plugin's page under "Plugins".
    #[serde(default, skip_serializing_if = "settings_is_empty")]
    #[schemars(extend("readOnly" = true))]
    pub settings: serde_json::Value,
}

impl Default for PluginConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            path: None,
            settings: serde_json::Value::Null,
        }
    }
}

impl PluginConfig {
    pub fn new_with_path(path: PathBuf) -> Self {
        Self {
            enabled: true,
            path: Some(path),
            settings: serde_json::Value::Null,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// An empty config `{}` deserializes with `enabled = true` (from the
    /// `default_true` helper) and no path. An explicit `false` is preserved.
    #[test]
    fn enabled_defaults_to_true_when_missing() {
        let c: PluginConfig = serde_json::from_str("{}").unwrap();
        assert!(c.enabled);
        assert!(c.path.is_none());

        let c: PluginConfig = serde_json::from_str(r#"{"enabled": false}"#).unwrap();
        assert!(!c.enabled);
    }

    /// `new_with_path` populates the path field, unlike `Default::default()`
    /// which leaves it `None`.
    #[test]
    fn new_with_path_sets_path_and_enabled() {
        let p = PathBuf::from("/plugins/foo.js");
        let c = PluginConfig::new_with_path(p.clone());
        assert!(c.enabled);
        assert_eq!(c.path.as_ref(), Some(&p));
    }
}
