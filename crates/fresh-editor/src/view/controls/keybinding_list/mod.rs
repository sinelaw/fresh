//! Keybinding list control for displaying and editing keybindings
//!
//! This module provides a complete keybinding list component with:
//! - State management (`KeybindingListState`)
//! - Rendering (`render_keybinding_list`)
//! - Input handling (`KeybindingListState::handle_mouse`, `handle_key`)
//! - Layout/hit testing (`KeybindingListLayout`)

mod input;

use super::FocusState;
use ratatui::layout::Rect;
use ratatui::style::Color;
use serde_json::Value;

pub use input::KeybindingListEvent;

/// State for an object array control (keybindings, etc.)
#[derive(Debug, Clone)]
pub struct KeybindingListState {
    /// List of items as JSON values
    pub bindings: Vec<Value>,
    /// Currently focused item index (None = add-new row)
    pub focused_index: Option<usize>,
    /// Label for this control
    pub label: String,
    /// Focus state
    pub focus: FocusState,
    /// Schema for item type (for creating new entries via dialog)
    pub item_schema: Option<Box<crate::view::settings::schema::SettingSchema>>,
    /// JSON pointer to field within item to display as preview (e.g., "/action")
    pub display_field: Option<String>,
}

impl KeybindingListState {
    /// Create a new object array state
    pub fn new(label: impl Into<String>) -> Self {
        Self {
            bindings: Vec::new(),
            focused_index: None,
            label: label.into(),
            focus: FocusState::Normal,
            item_schema: None,
            display_field: None,
        }
    }

    /// Initialize from a JSON array value
    pub fn with_bindings(mut self, value: &Value) -> Self {
        if let Some(arr) = value.as_array() {
            self.bindings = arr.clone();
        }
        self
    }

    /// Set the focus state
    pub fn with_focus(mut self, focus: FocusState) -> Self {
        self.focus = focus;
        self
    }

    /// Set the item schema for creating new entries
    pub fn with_item_schema(
        mut self,
        schema: crate::view::settings::schema::SettingSchema,
    ) -> Self {
        self.item_schema = Some(Box::new(schema));
        self
    }

    /// Set the display field for previewing items
    pub fn with_display_field(mut self, field: String) -> Self {
        self.display_field = Some(field);
        self
    }

    /// Check if the control is enabled
    pub fn is_enabled(&self) -> bool {
        self.focus != FocusState::Disabled
    }

    /// Convert to JSON value
    pub fn to_value(&self) -> Value {
        Value::Array(self.bindings.clone())
    }

    /// Get the focused binding
    pub fn focused_binding(&self) -> Option<&Value> {
        self.focused_index.and_then(|idx| self.bindings.get(idx))
    }

    /// Focus next entry
    pub fn focus_next(&mut self) {
        match self.focused_index {
            None => {
                // From add-new to first entry (if any)
                if !self.bindings.is_empty() {
                    self.focused_index = Some(0);
                }
            }
            Some(idx) if idx + 1 < self.bindings.len() => {
                self.focused_index = Some(idx + 1);
            }
            Some(_) => {
                // Last entry, go to add-new
                self.focused_index = None;
            }
        }
    }

    /// Focus previous entry
    pub fn focus_prev(&mut self) {
        match self.focused_index {
            None => {
                // From add-new to last entry (if any)
                if !self.bindings.is_empty() {
                    self.focused_index = Some(self.bindings.len() - 1);
                }
            }
            Some(0) => {
                // First entry stays at first
            }
            Some(idx) => {
                self.focused_index = Some(idx - 1);
            }
        }
    }

    /// Boundary-aware down navigation for the Settings page, mirroring
    /// `MapState::focus_next`: move to the next sub-row and return
    /// `true`, or return `false` at the bottom edge (the add-new row)
    /// so the caller advances to the next setting. Layout is entries
    /// `[0..len]` then the trailing add-new sentinel (`None`).
    /// (The plain [`Self::focus_next`] wraps; this one stops.)
    pub fn nav_next(&mut self) -> bool {
        match self.focused_index {
            Some(idx) if idx + 1 < self.bindings.len() => {
                self.focused_index = Some(idx + 1);
                true
            }
            Some(_) => {
                // Last entry → add-new sentinel.
                self.focused_index = None;
                true
            }
            None => false, // On add-new → leave the control.
        }
    }

    /// Boundary-aware up navigation; companion to [`Self::nav_next`].
    pub fn nav_prev(&mut self) -> bool {
        match self.focused_index {
            Some(0) => false, // First entry → leave the control.
            Some(idx) => {
                self.focused_index = Some(idx - 1);
                true
            }
            None if !self.bindings.is_empty() => {
                // Add-new → last entry.
                self.focused_index = Some(self.bindings.len() - 1);
                true
            }
            None => false, // Empty list on add-new → leave.
        }
    }

    /// Seed focus when navigation enters the control: the first entry
    /// when arriving from above, the add-new sentinel (or last entry
    /// when empty) when arriving from below. Mirrors `MapState`.
    pub fn init_nav_focus(&mut self, from_above: bool) {
        self.focused_index = if from_above && !self.bindings.is_empty() {
            Some(0)
        } else {
            None
        };
    }

    /// Sub-row index for scroll-into-view: `1 + entry` for a focused
    /// entry (0 is the label), or the add-new row after the entries.
    pub fn sub_focus_row(&self) -> usize {
        match self.focused_index {
            Some(i) => 1 + i,
            None => 1 + self.bindings.len(),
        }
    }

    /// Remove the focused binding
    pub fn remove_focused(&mut self) {
        if let Some(idx) = self.focused_index {
            if idx < self.bindings.len() {
                self.bindings.remove(idx);
                // Adjust focus
                if self.bindings.is_empty() {
                    self.focused_index = None;
                } else if idx >= self.bindings.len() {
                    self.focused_index = Some(self.bindings.len() - 1);
                }
            }
        }
    }

    /// Remove binding at index
    pub fn remove_binding(&mut self, index: usize) {
        if index < self.bindings.len() {
            self.bindings.remove(index);
            // Adjust focus
            if let Some(focused) = self.focused_index {
                if focused >= self.bindings.len() {
                    self.focused_index = if self.bindings.is_empty() {
                        None
                    } else {
                        Some(self.bindings.len() - 1)
                    };
                }
            }
        }
    }

    /// Add a new binding
    pub fn add_binding(&mut self, binding: Value) {
        self.bindings.push(binding);
    }

    /// Update the binding at index
    pub fn update_binding(&mut self, index: usize, binding: Value) {
        if index < self.bindings.len() {
            self.bindings[index] = binding;
        }
    }

    /// Focus on a specific entry
    pub fn focus_entry(&mut self, index: usize) {
        if index < self.bindings.len() {
            self.focused_index = Some(index);
        }
    }

    /// Focus on the add-new row
    pub fn focus_add_row(&mut self) {
        self.focused_index = None;
    }
}

/// Colors for keybinding list rendering
#[derive(Debug, Clone, Copy)]
pub struct KeybindingListColors {
    pub label_fg: Color,
    pub key_fg: Color,
    pub action_fg: Color,
    /// Background color for unfocused rows. Should match the surrounding
    /// surface (e.g. `theme.popup_bg` in the settings UI) so the list
    /// doesn't paint a `Color::Reset` strip that falls back to the host
    /// terminal's default bg — which on a dark-terminal host running the
    /// light theme renders as a black band over the cream settings panel.
    pub row_bg: Color,
    /// Background color for focused entries
    pub focused_bg: Color,
    /// Foreground color for focused entries (text on focused background)
    pub focused_fg: Color,
    pub add_fg: Color,
}

impl Default for KeybindingListColors {
    fn default() -> Self {
        Self {
            label_fg: Color::White,
            key_fg: Color::Yellow,
            action_fg: Color::Cyan,
            row_bg: Color::Reset,
            focused_bg: Color::DarkGray,
            focused_fg: Color::White,
            add_fg: Color::Green,
        }
    }
}

/// Layout information for hit testing
#[derive(Debug, Clone, Default)]
pub struct KeybindingListLayout {
    /// (data_index, screen_area) for each visible entry
    pub entry_rects: Vec<(usize, Rect)>,
    pub add_rect: Option<Rect>,
}

impl KeybindingListLayout {
    /// Find what was clicked at the given coordinates
    pub fn hit_test(&self, x: u16, y: u16) -> Option<KeybindingListHit> {
        // Check entry areas
        for &(data_idx, rect) in self.entry_rects.iter() {
            if y == rect.y && x >= rect.x && x < rect.x + rect.width {
                return Some(KeybindingListHit::Entry(data_idx));
            }
        }

        // Check add row
        if let Some(ref add_rect) = self.add_rect {
            if y == add_rect.y && x >= add_rect.x && x < add_rect.x + add_rect.width {
                return Some(KeybindingListHit::AddRow);
            }
        }

        None
    }
}

/// Result of hit testing on a keybinding list
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeybindingListHit {
    /// Clicked on an entry
    Entry(usize),
    /// Clicked on add row
    AddRow,
}

/// Format a keybinding's key combination for display
pub fn format_key_combo(binding: &Value) -> String {
    // Check for keys array (chord binding) first
    if let Some(keys) = binding.get("keys").and_then(|k| k.as_array()) {
        let parts: Vec<String> = keys
            .iter()
            .map(|k| {
                let mut key_str = String::new();
                if let Some(mods) = k.get("modifiers").and_then(|m| m.as_array()) {
                    for m in mods {
                        if let Some(s) = m.as_str() {
                            key_str.push_str(&capitalize_mod(s));
                            key_str.push('+');
                        }
                    }
                }
                if let Some(key) = k.get("key").and_then(|k| k.as_str()) {
                    key_str.push_str(&capitalize_key(key));
                }
                key_str
            })
            .collect();
        return parts.join(" ");
    }

    // Single key binding
    let mut result = String::new();
    if let Some(mods) = binding.get("modifiers").and_then(|m| m.as_array()) {
        for m in mods {
            if let Some(s) = m.as_str() {
                result.push_str(&capitalize_mod(s));
                result.push('+');
            }
        }
    }
    if let Some(key) = binding.get("key").and_then(|k| k.as_str()) {
        result.push_str(&capitalize_key(key));
    }
    result
}

fn capitalize_mod(s: &str) -> String {
    match s.to_lowercase().as_str() {
        "ctrl" | "control" => "Ctrl".to_string(),
        "alt" => "Alt".to_string(),
        "shift" => "Shift".to_string(),
        "super" | "meta" | "cmd" => "Super".to_string(),
        _ => s.to_string(),
    }
}

fn capitalize_key(s: &str) -> String {
    if s.len() == 1 {
        s.to_uppercase()
    } else {
        let mut chars = s.chars();
        match chars.next() {
            None => String::new(),
            Some(c) => c.to_uppercase().chain(chars).collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keybinding_list_state_new() {
        let state = KeybindingListState::new("Keybindings");
        assert_eq!(state.label, "Keybindings");
        assert!(state.bindings.is_empty());
        assert!(state.focused_index.is_none());
    }

    #[test]
    fn nav_next_prev_stop_at_boundaries() {
        // Settings-page navigation: entries [0,1] then add-new (None).
        let mut s = KeybindingListState::new("Detectors");
        s.add_binding(serde_json::json!({"name": ".venv"}));
        s.add_binding(serde_json::json!({"name": "direnv"}));

        // Enter from above → first entry.
        s.init_nav_focus(true);
        assert_eq!(s.focused_index, Some(0));
        // Down through entries → add-new → leave.
        assert!(s.nav_next());
        assert_eq!(s.focused_index, Some(1));
        assert!(s.nav_next());
        assert_eq!(s.focused_index, None); // add-new sentinel
        assert!(!s.nav_next()); // at add-new → leave the control
        assert_eq!(s.focused_index, None);

        // Enter from below → add-new.
        s.init_nav_focus(false);
        assert_eq!(s.focused_index, None);
        // Up from add-new → last entry → first → leave.
        assert!(s.nav_prev());
        assert_eq!(s.focused_index, Some(1));
        assert!(s.nav_prev());
        assert_eq!(s.focused_index, Some(0));
        assert!(!s.nav_prev()); // at first → leave the control

        // sub_focus_row: 0 is the label, entries are 1+i, add-new last.
        s.focused_index = Some(1);
        assert_eq!(s.sub_focus_row(), 2);
        s.focused_index = None;
        assert_eq!(s.sub_focus_row(), 1 + s.bindings.len());
    }

    #[test]
    fn test_keybinding_list_navigation() {
        let mut state = KeybindingListState::new("Test");
        state.add_binding(serde_json::json!({"key": "a", "action": "test1"}));
        state.add_binding(serde_json::json!({"key": "b", "action": "test2"}));

        // Start at add-new (None)
        assert!(state.focused_index.is_none());

        // focus_next goes to first entry
        state.focus_next();
        assert_eq!(state.focused_index, Some(0));

        state.focus_next();
        assert_eq!(state.focused_index, Some(1));

        // At last entry, focus_next wraps to add-new
        state.focus_next();
        assert!(state.focused_index.is_none());

        // focus_prev from add-new goes to last entry
        state.focus_prev();
        assert_eq!(state.focused_index, Some(1));

        state.focus_prev();
        assert_eq!(state.focused_index, Some(0));

        // At first entry, focus_prev stays
        state.focus_prev();
        assert_eq!(state.focused_index, Some(0));
    }

    #[test]
    fn test_keybinding_list_remove() {
        let mut state = KeybindingListState::new("Test");
        state.add_binding(serde_json::json!({"key": "a", "action": "test1"}));
        state.add_binding(serde_json::json!({"key": "b", "action": "test2"}));
        state.focus_entry(0);

        state.remove_focused();
        assert_eq!(state.bindings.len(), 1);
        assert_eq!(state.focused_index, Some(0));
    }

    #[test]
    fn test_keybinding_list_hit_test() {
        let layout = KeybindingListLayout {
            entry_rects: vec![(0, Rect::new(2, 1, 40, 1)), (1, Rect::new(2, 2, 40, 1))],
            add_rect: Some(Rect::new(2, 3, 40, 1)),
        };

        assert_eq!(layout.hit_test(10, 1), Some(KeybindingListHit::Entry(0)));
        assert_eq!(layout.hit_test(10, 2), Some(KeybindingListHit::Entry(1)));
        assert_eq!(layout.hit_test(10, 3), Some(KeybindingListHit::AddRow));
        assert_eq!(layout.hit_test(0, 0), None);
    }
}
