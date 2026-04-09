//! Configuration types shared across crates

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

fn default_true() -> bool {
    true
}

/// Configuration for a single plugin
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[schemars(extend("x-display-field" = "/enabled"))]
pub struct PluginConfig {
    /// Whether this plugin is enabled (default: true)
    /// When disabled, the plugin will not be loaded or executed.
    #[serde(default = "default_true")]
    pub enabled: bool,

    /// Path to the plugin file (populated automatically when scanning)
    /// This is filled in by the plugin system and should not be set manually.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[schemars(extend("readOnly" = true))]
    pub path: Option<PathBuf>,
}

impl Default for PluginConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            path: None,
        }
    }
}

impl PluginConfig {
    pub fn new_with_path(path: PathBuf) -> Self {
        Self {
            enabled: true,
            path: Some(path),
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
