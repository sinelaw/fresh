use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use ts_rs::TS;

/// A menu item (action, separator, or submenu)
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema, PartialEq, Eq, TS)]
#[ts(export)]
#[serde(untagged)]
pub enum MenuItem {
    /// A separator line
    Separator { separator: bool },
    /// An action item
    Action {
        label: String,
        action: String,
        #[serde(default)]
        #[ts(type = "Record<string, any>")]
        args: HashMap<String, serde_json::Value>,
        #[serde(default)]
        when: Option<String>,
        /// Checkbox state condition (e.g., "line_numbers", "line_wrap")
        #[serde(default)]
        checkbox: Option<String>,
    },
    /// A submenu (for future extensibility)
    Submenu { label: String, items: Vec<Self> },
    /// A dynamic submenu whose items are generated at runtime
    /// The `source` field specifies what to generate (e.g., "themes")
    DynamicSubmenu { label: String, source: String },
    /// A disabled info label (no action)
    Label { info: String },
}

/// A top-level menu in the menu bar
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema, PartialEq, Eq, TS)]
#[ts(export)]
pub struct Menu {
    /// Internal identifier for the menu (used for keybinding matching).
    /// This should NOT be translated - use English names like "File", "Edit".
    /// If not set, the label is used for matching (for backward compatibility).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub id: Option<String>,
    /// Display label for the menu (can be translated)
    pub label: String,
    /// Menu items (actions, separators, or submenus)
    pub items: Vec<MenuItem>,
}
