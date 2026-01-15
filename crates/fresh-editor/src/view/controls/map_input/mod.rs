//! Map/Dictionary control for editing key-value pairs
//!
//! Renders as an expandable list of entries:
//! ```text
//! Languages:
//!   > rust
//!   > python
//!   ▼ javascript (expanded)
//!       tab_size: 2
//!       auto_indent: [x]
//!   [+ Add entry...]
//! ```
//!
//! This module provides a complete map control with:
//! - State management (`MapState`)
//! - Rendering (`render_map`)
//! - Input handling (`MapState::handle_mouse`, `handle_key`)
//! - Layout/hit testing (`MapLayout`)

mod input;
mod render;

use ratatui::layout::Rect;
use ratatui::style::Color;

pub use input::MapEvent;
pub use render::render_map;

use super::FocusState;

/// State for a map/dictionary control
#[derive(Debug, Clone)]
pub struct MapState {
    /// Map entries as (key, value) pairs where value is JSON
    pub entries: Vec<(String, serde_json::Value)>,
    /// Currently focused entry index (None = add-new field)
    pub focused_entry: Option<usize>,
    /// Text in the "add new" key field
    pub new_key_text: String,
    /// Cursor position in the new key field
    pub cursor: usize,
    /// Label for this map
    pub label: String,
    /// Focus state
    pub focus: FocusState,
    /// Expanded entry indices (for viewing/editing nested values)
    pub expanded: Vec<usize>,
    /// Schema for value type (for creating new entries)
    pub value_schema: Option<Box<crate::view::settings::schema::SettingSchema>>,
    /// JSON pointer to field within value to display as preview (e.g., "/command")
    pub display_field: Option<String>,
    /// Whether to disallow adding new entries (entries are auto-managed)
    pub no_add: bool,
}

impl MapState {
    /// Create a new map state
    pub fn new(label: impl Into<String>) -> Self {
        Self {
            entries: Vec::new(),
            focused_entry: None,
            new_key_text: String::new(),
            cursor: 0,
            label: label.into(),
            focus: FocusState::Normal,
            expanded: Vec::new(),
            value_schema: None,
            display_field: None,
            no_add: false,
        }
    }

    /// Set the display field (JSON pointer to field within value to show as preview)
    pub fn with_display_field(mut self, field: String) -> Self {
        self.display_field = Some(field);
        self
    }

    /// Set whether adding new entries is disallowed
    pub fn with_no_add(mut self, no_add: bool) -> Self {
        self.no_add = no_add;
        self
    }

    /// Get the display value for an entry (either from display_field or fallback to field count)
    pub fn get_display_value(&self, value: &serde_json::Value) -> String {
        if let Some(ref field) = self.display_field {
            if let Some(v) = value.pointer(field) {
                return match v {
                    serde_json::Value::String(s) => s.clone(),
                    serde_json::Value::Bool(b) => b.to_string(),
                    serde_json::Value::Number(n) => n.to_string(),
                    serde_json::Value::Null => "null".to_string(),
                    serde_json::Value::Array(arr) => format!("[{} items]", arr.len()),
                    serde_json::Value::Object(obj) => format!("{{{} fields}}", obj.len()),
                };
            }
        }
        // Fallback to showing field count
        match value {
            serde_json::Value::Object(obj) => format!("{{{} fields}}", obj.len()),
            serde_json::Value::Array(arr) => format!("[{} items]", arr.len()),
            other => other.to_string(),
        }
    }

    /// Set the entries from JSON value
    pub fn with_entries(mut self, value: &serde_json::Value) -> Self {
        if let Some(obj) = value.as_object() {
            self.entries = obj.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
            // Sort by key for consistent ordering
            self.entries.sort_by(|a, b| a.0.cmp(&b.0));
            // Default to first entry if any exist
            if !self.entries.is_empty() {
                self.focused_entry = Some(0);
            }
        }
        self
    }

    /// Set the value schema for creating new entries
    pub fn with_value_schema(
        mut self,
        schema: crate::view::settings::schema::SettingSchema,
    ) -> Self {
        self.value_schema = Some(Box::new(schema));
        self
    }

    /// Set the focus state
    pub fn with_focus(mut self, focus: FocusState) -> Self {
        self.focus = focus;
        self
    }

    /// Check if the control is enabled
    pub fn is_enabled(&self) -> bool {
        self.focus != FocusState::Disabled
    }

    /// Add a new entry with the given key and default value
    pub fn add_entry(&mut self, key: String, value: serde_json::Value) {
        if self.focus == FocusState::Disabled || key.is_empty() {
            return;
        }
        // Check for duplicate key
        if self.entries.iter().any(|(k, _)| k == &key) {
            return;
        }
        self.entries.push((key, value));
        self.entries.sort_by(|a, b| a.0.cmp(&b.0));
    }

    /// Add entry from the new_key_text field with default value
    pub fn add_entry_from_input(&mut self) {
        if self.new_key_text.is_empty() {
            return;
        }
        let key = std::mem::take(&mut self.new_key_text);
        self.cursor = 0;
        // Use an empty object as default value
        self.add_entry(key, serde_json::json!({}));
    }

    /// Remove an entry by index
    pub fn remove_entry(&mut self, index: usize) {
        if self.focus == FocusState::Disabled || index >= self.entries.len() {
            return;
        }
        self.entries.remove(index);
        // Adjust focused_entry if needed
        if let Some(focused) = self.focused_entry {
            if focused >= self.entries.len() {
                self.focused_entry = if self.entries.is_empty() {
                    None
                } else {
                    Some(self.entries.len() - 1)
                };
            }
        }
        // Remove from expanded list
        self.expanded.retain(|&idx| idx != index);
        // Adjust expanded indices
        self.expanded = self
            .expanded
            .iter()
            .map(|&idx| if idx > index { idx - 1 } else { idx })
            .collect();
    }

    /// Focus on an entry
    pub fn focus_entry(&mut self, index: usize) {
        if index < self.entries.len() {
            self.focused_entry = Some(index);
        }
    }

    /// Focus on the new entry field
    pub fn focus_new_entry(&mut self) {
        self.focused_entry = None;
        self.cursor = self.new_key_text.len();
    }

    /// Toggle expansion of an entry
    pub fn toggle_expand(&mut self, index: usize) {
        if index >= self.entries.len() {
            return;
        }
        if let Some(pos) = self.expanded.iter().position(|&i| i == index) {
            self.expanded.remove(pos);
        } else {
            self.expanded.push(index);
        }
    }

    /// Check if an entry is expanded
    pub fn is_expanded(&self, index: usize) -> bool {
        self.expanded.contains(&index)
    }

    /// Insert a character in the new key field
    pub fn insert(&mut self, c: char) {
        if self.focus == FocusState::Disabled || self.focused_entry.is_some() {
            return;
        }
        self.new_key_text.insert(self.cursor, c);
        self.cursor += 1;
    }

    /// Backspace in the new key field
    pub fn backspace(&mut self) {
        if self.focus == FocusState::Disabled || self.focused_entry.is_some() || self.cursor == 0 {
            return;
        }
        self.cursor -= 1;
        self.new_key_text.remove(self.cursor);
    }

    /// Move cursor left
    pub fn move_left(&mut self) {
        if self.cursor > 0 {
            self.cursor -= 1;
        }
    }

    /// Move cursor right
    pub fn move_right(&mut self) {
        if self.cursor < self.new_key_text.len() {
            self.cursor += 1;
        }
    }

    /// Move focus to previous entry. Returns true if handled, false if should move to prev item.
    pub fn focus_prev(&mut self) -> bool {
        match self.focused_entry {
            Some(0) => false, // At first entry, move to previous item
            Some(idx) => {
                self.focused_entry = Some(idx - 1);
                true
            }
            None if !self.entries.is_empty() => {
                self.focused_entry = Some(self.entries.len() - 1);
                true
            }
            None => false, // Empty map or at add-new with no entries, move to previous item
        }
    }

    /// Move focus to next entry. Returns true if handled, false if should move to next item.
    pub fn focus_next(&mut self) -> bool {
        match self.focused_entry {
            Some(idx) if idx + 1 < self.entries.len() => {
                self.focused_entry = Some(idx + 1);
                true
            }
            Some(_) if self.no_add => {
                // At last entry but no_add is true, move to next item
                false
            }
            Some(_) => {
                // At last entry, go to add-new
                self.focused_entry = None;
                self.cursor = self.new_key_text.len();
                true
            }
            None => false, // At add-new, move to next item
        }
    }

    /// Initialize focus when entering the control.
    /// `from_above`: true = start at first entry, false = start at add-new field (or last entry if no_add)
    pub fn init_focus(&mut self, from_above: bool) {
        if from_above && !self.entries.is_empty() {
            self.focused_entry = Some(0);
        } else if !from_above && !self.entries.is_empty() && self.no_add {
            // Coming from below with no_add, go to last entry
            self.focused_entry = Some(self.entries.len() - 1);
        } else if !self.no_add {
            // Can go to add-new field
            self.focused_entry = None;
            self.cursor = self.new_key_text.len();
        } else if !self.entries.is_empty() {
            // no_add and coming from above, start at first entry
            self.focused_entry = Some(0);
        } else {
            // Empty and no_add, shouldn't happen but handle gracefully
            self.focused_entry = None;
        }
    }

    /// Get the number of entries
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Check if the map is empty
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Convert entries to JSON value
    pub fn to_value(&self) -> serde_json::Value {
        let map: serde_json::Map<String, serde_json::Value> = self
            .entries
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        serde_json::Value::Object(map)
    }
}

/// Colors for the map control
#[derive(Debug, Clone, Copy)]
pub struct MapColors {
    pub label: Color,
    pub key: Color,
    pub value_preview: Color,
    pub border: Color,
    pub remove_button: Color,
    pub add_button: Color,
    pub focused: Color,
    pub cursor: Color,
    pub disabled: Color,
    pub expand_arrow: Color,
}

impl Default for MapColors {
    fn default() -> Self {
        Self {
            label: Color::White,
            key: Color::Cyan,
            value_preview: Color::Gray,
            border: Color::Gray,
            remove_button: Color::Red,
            add_button: Color::Green,
            focused: Color::Yellow,
            cursor: Color::Yellow,
            disabled: Color::DarkGray,
            expand_arrow: Color::White,
        }
    }
}

impl MapColors {
    pub fn from_theme(theme: &crate::view::theme::Theme) -> Self {
        Self {
            label: theme.editor_fg,
            key: theme.menu_highlight_fg,
            value_preview: theme.line_number_fg,
            border: theme.line_number_fg,
            remove_button: theme.diagnostic_error_fg,
            add_button: theme.diagnostic_info_fg,
            focused: theme.selection_bg,
            cursor: theme.cursor,
            disabled: theme.line_number_fg,
            expand_arrow: theme.editor_fg,
        }
    }
}

/// Layout information for hit testing
#[derive(Debug, Clone, Default)]
pub struct MapLayout {
    pub full_area: Rect,
    pub entry_areas: Vec<MapEntryLayout>,
    pub add_row_area: Option<Rect>,
}

/// Layout for an entry row
#[derive(Debug, Clone)]
pub struct MapEntryLayout {
    pub index: usize,
    pub row_area: Rect,
    pub expand_area: Rect,
    pub key_area: Rect,
    pub remove_area: Rect,
}

impl MapLayout {
    /// Find what was clicked at the given coordinates
    pub fn hit_test(&self, x: u16, y: u16) -> Option<MapHit> {
        // Check entry rows
        for entry in &self.entry_areas {
            if y == entry.row_area.y {
                if x >= entry.remove_area.x && x < entry.remove_area.x + entry.remove_area.width {
                    return Some(MapHit::RemoveButton(entry.index));
                }
                if x >= entry.expand_area.x && x < entry.expand_area.x + entry.expand_area.width {
                    return Some(MapHit::ExpandArrow(entry.index));
                }
                if x >= entry.key_area.x && x < entry.key_area.x + entry.key_area.width {
                    return Some(MapHit::EntryKey(entry.index));
                }
            }
        }

        // Check add row - clicking anywhere on the row focuses the input
        if let Some(ref add_row) = self.add_row_area {
            if y == add_row.y && x >= add_row.x && x < add_row.x + add_row.width {
                return Some(MapHit::AddRow);
            }
        }

        None
    }
}

/// Result of hit testing on a map control
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MapHit {
    /// Clicked on expand/collapse arrow
    ExpandArrow(usize),
    /// Clicked on entry key
    EntryKey(usize),
    /// Clicked on remove button for entry
    RemoveButton(usize),
    /// Clicked on add row (input field or button area)
    AddRow,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_map_state_new() {
        let state = MapState::new("Test");
        assert_eq!(state.label, "Test");
        assert!(state.entries.is_empty());
        assert!(state.focused_entry.is_none());
    }

    #[test]
    fn test_map_state_add_entry() {
        let mut state = MapState::new("Test");
        state.add_entry("key1".to_string(), serde_json::json!({"foo": "bar"}));
        assert_eq!(state.entries.len(), 1);
        assert_eq!(state.entries[0].0, "key1");
    }

    #[test]
    fn test_map_state_remove_entry() {
        let mut state = MapState::new("Test");
        state.add_entry("a".to_string(), serde_json::json!({}));
        state.add_entry("b".to_string(), serde_json::json!({}));
        state.remove_entry(0);
        assert_eq!(state.entries.len(), 1);
        assert_eq!(state.entries[0].0, "b");
    }

    #[test]
    fn test_map_state_navigation() {
        let mut state = MapState::new("Test").with_focus(FocusState::Focused);
        state.add_entry("a".to_string(), serde_json::json!({}));
        state.add_entry("b".to_string(), serde_json::json!({}));

        // Start at add-new
        assert!(state.focused_entry.is_none());

        // Go to last entry
        state.focus_prev();
        assert_eq!(state.focused_entry, Some(1));

        // Go to first entry
        state.focus_prev();
        assert_eq!(state.focused_entry, Some(0));

        // Go forward
        state.focus_next();
        assert_eq!(state.focused_entry, Some(1));

        // Go to add-new
        state.focus_next();
        assert!(state.focused_entry.is_none());
    }

    #[test]
    fn test_map_state_expand() {
        let mut state = MapState::new("Test");
        state.add_entry("key1".to_string(), serde_json::json!({}));

        assert!(!state.is_expanded(0));
        state.toggle_expand(0);
        assert!(state.is_expanded(0));
        state.toggle_expand(0);
        assert!(!state.is_expanded(0));
    }

    #[test]
    fn test_map_hit_test() {
        let layout = MapLayout {
            full_area: Rect::new(0, 0, 50, 5),
            entry_areas: vec![MapEntryLayout {
                index: 0,
                row_area: Rect::new(0, 1, 50, 1),
                expand_area: Rect::new(2, 1, 1, 1),
                key_area: Rect::new(4, 1, 10, 1),
                remove_area: Rect::new(40, 1, 3, 1),
            }],
            add_row_area: Some(Rect::new(0, 2, 50, 1)),
        };

        assert_eq!(layout.hit_test(2, 1), Some(MapHit::ExpandArrow(0)));
        assert_eq!(layout.hit_test(5, 1), Some(MapHit::EntryKey(0)));
        assert_eq!(layout.hit_test(40, 1), Some(MapHit::RemoveButton(0)));
        assert_eq!(layout.hit_test(5, 2), Some(MapHit::AddRow));
        assert_eq!(layout.hit_test(13, 2), Some(MapHit::AddRow));
        assert_eq!(layout.hit_test(0, 0), None);
    }
}
