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
#[ts(export)]
pub struct Suggestion {
    /// The text to display
    pub text: String,
    /// Optional description
    pub description: Option<String>,
    /// The value to use when selected (defaults to text if None)
    pub value: Option<String>,
    /// Whether this suggestion is disabled (greyed out)
    pub disabled: bool,
    /// Optional keyboard shortcut
    pub keybinding: Option<String>,
    /// Source of the command (for command palette)
    pub source: Option<CommandSource>,
}

impl Suggestion {
    pub fn new(text: String) -> Self {
        Self {
            text,
            description: None,
            value: None,
            disabled: false,
            keybinding: None,
            source: None,
        }
    }
}
