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
#[ts(export, rename = "PromptSuggestion")]
pub struct Suggestion {
    /// The text to display
    pub text: String,
    /// Optional description
    #[ts(optional)]
    pub description: Option<String>,
    /// The value to use when selected (defaults to text if None)
    #[ts(optional)]
    pub value: Option<String>,
    /// Whether this suggestion is disabled (greyed out, defaults to false)
    #[serde(default)]
    #[ts(optional)]
    pub disabled: Option<bool>,
    /// Optional keyboard shortcut
    #[ts(optional)]
    pub keybinding: Option<String>,
    /// Source of the command (for command palette)
    #[ts(optional)]
    pub source: Option<CommandSource>,
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
