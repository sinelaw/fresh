use serde::{Deserialize, Serialize};

/// Source of a command (builtin or from a plugin)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, ts_rs::TS)]
#[ts(export)]
pub enum CommandSource {
    /// Built-in editor command
    Builtin,
    /// Command registered by a plugin (contains plugin filename without extension)
    Plugin(String),
}

/// A command registered by a plugin via the service bridge.
/// This is a simplified version that the editor converts to its internal Command type.
#[derive(Debug, Clone, Serialize, Deserialize, ts_rs::TS)]
#[ts(export)]
pub struct Command {
    /// Command name (e.g., "Open File")
    pub name: String,
    /// Command description
    pub description: String,
    /// The action name to trigger (for plugin commands, this is the function name)
    pub action_name: String,
    /// Plugin that registered this command
    pub plugin_name: String,
    /// Custom contexts required for this command (plugin-defined contexts like "vi-mode")
    pub custom_contexts: Vec<String>,
}

/// A single suggestion item for autocomplete
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, ts_rs::TS)]
#[serde(deny_unknown_fields)]
#[ts(export, rename = "PromptSuggestion")]
pub struct Suggestion {
    /// The text to display
    pub text: String,
    /// Optional description
    #[serde(default)]
    #[ts(optional)]
    pub description: Option<String>,
    /// The value to use when selected (defaults to text if None)
    #[serde(default)]
    #[ts(optional)]
    pub value: Option<String>,
    /// Whether this suggestion is disabled (greyed out, defaults to false)
    #[serde(default)]
    #[ts(optional)]
    pub disabled: Option<bool>,
    /// Optional keyboard shortcut
    #[serde(default)]
    #[ts(optional)]
    pub keybinding: Option<String>,
    /// Source of the command (for command palette) - internal, not settable by plugins
    #[serde(skip)]
    #[ts(skip)]
    pub source: Option<CommandSource>,
}

#[cfg(feature = "plugins")]
impl<'js> rquickjs::FromJs<'js> for Suggestion {
    fn from_js(_ctx: &rquickjs::Ctx<'js>, value: rquickjs::Value<'js>) -> rquickjs::Result<Self> {
        rquickjs_serde::from_value(value).map_err(|e| rquickjs::Error::FromJs {
            from: "object",
            to: "Suggestion",
            message: Some(e.to_string()),
        })
    }
}

impl Suggestion {
    pub fn new(text: String) -> Self {
        Self {
            text,
            description: None,
            value: None,
            disabled: None,
            keybinding: None,
            source: None,
        }
    }

    /// Check if this suggestion is disabled
    pub fn is_disabled(&self) -> bool {
        self.disabled.unwrap_or(false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// `is_disabled` mirrors the `disabled` option and treats `None` as enabled.
    #[test]
    fn is_disabled_reflects_disabled_field() {
        let mut s = Suggestion::new("foo".into());
        assert!(!s.is_disabled(), "None defaults to enabled");

        s.disabled = Some(false);
        assert!(!s.is_disabled());

        s.disabled = Some(true);
        assert!(s.is_disabled());
    }
}
