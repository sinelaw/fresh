use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use ts_rs::TS;

/// Menu state context — provides named boolean states for menu item conditions.
///
/// Both `when` conditions (controlling enabled/disabled state) and `checkbox`
/// states (controlling checkmark display) look up values here.  The editor
/// computes these values each frame from its internal state and exposes the
/// context to the GUI layer via the `GuiApplication` trait so that
/// platform-native menus can reflect the same state as the TUI menu bar.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct MenuContext {
    states: HashMap<String, bool>,
}

impl MenuContext {
    pub fn new() -> Self {
        Self {
            states: HashMap::new(),
        }
    }

    /// Set a named boolean state.
    pub fn set(&mut self, name: impl Into<String>, value: bool) -> &mut Self {
        self.states.insert(name.into(), value);
        self
    }

    /// Get a named boolean state (defaults to `false` if not set).
    pub fn get(&self, name: &str) -> bool {
        self.states.get(name).copied().unwrap_or(false)
    }

    /// Builder-style setter.
    pub fn with(mut self, name: impl Into<String>, value: bool) -> Self {
        self.set(name, value);
        self
    }
}

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
    /// Context condition for menu visibility (e.g., "file_explorer_focused")
    /// If set, the menu is only shown when this condition evaluates to true
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub when: Option<String>,
}
