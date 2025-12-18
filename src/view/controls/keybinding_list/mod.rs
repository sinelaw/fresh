//! Keybinding list control for displaying and editing keybindings
//!
//! This module provides a complete keybinding list component with:
//! - State management (`KeybindingListState`)
//! - Rendering (`render_keybinding_list`)
//! - Input handling (`KeybindingListState::handle_mouse`, `handle_key`)
//! - Layout/hit testing (`KeybindingListLayout`)

mod input;
mod render;

use super::FocusState;
use ratatui::layout::Rect;
use ratatui::style::Color;
use serde_json::Value;

pub use input::KeybindingListEvent;
pub use render::{format_key_combo, render_keybinding_list};

/// State for a keybinding list control
#[derive(Debug, Clone)]
pub struct KeybindingListState {
    /// List of keybindings as JSON values
    pub bindings: Vec<Value>,
    /// Currently focused binding index (None = add-new row)
    pub focused_index: Option<usize>,
    /// Label for this control
    pub label: String,
    /// Focus state
    pub focus: FocusState,
}

impl KeybindingListState {
    /// Create a new keybinding list state
    pub fn new(label: impl Into<String>) -> Self {
        Self {
            bindings: Vec::new(),
            focused_index: None,
            label: label.into(),
            focus: FocusState::Normal,
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
    pub focused_bg: Color,
    pub delete_fg: Color,
    pub add_fg: Color,
}

impl Default for KeybindingListColors {
    fn default() -> Self {
        Self {
            label_fg: Color::White,
            key_fg: Color::Yellow,
            action_fg: Color::Cyan,
            focused_bg: Color::DarkGray,
            delete_fg: Color::Red,
            add_fg: Color::Green,
        }
    }
}

/// Layout information for hit testing
#[derive(Debug, Clone, Default)]
pub struct KeybindingListLayout {
    pub entry_rects: Vec<Rect>,
    pub delete_rects: Vec<Rect>,
    pub add_rect: Option<Rect>,
}

impl KeybindingListLayout {
    /// Find what was clicked at the given coordinates
    pub fn hit_test(&self, x: u16, y: u16) -> Option<KeybindingListHit> {
        // Check delete buttons first (they overlap entry areas)
        for (idx, rect) in self.delete_rects.iter().enumerate() {
            if y == rect.y && x >= rect.x && x < rect.x + rect.width {
                return Some(KeybindingListHit::DeleteButton(idx));
            }
        }

        // Check entry areas
        for (idx, rect) in self.entry_rects.iter().enumerate() {
            if y == rect.y && x >= rect.x && x < rect.x + rect.width {
                return Some(KeybindingListHit::Entry(idx));
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
    /// Clicked on delete button for entry
    DeleteButton(usize),
    /// Clicked on add row
    AddRow,
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
            entry_rects: vec![Rect::new(2, 1, 40, 1), Rect::new(2, 2, 40, 1)],
            delete_rects: vec![Rect::new(38, 1, 3, 1), Rect::new(38, 2, 3, 1)],
            add_rect: Some(Rect::new(2, 3, 40, 1)),
        };

        assert_eq!(
            layout.hit_test(38, 1),
            Some(KeybindingListHit::DeleteButton(0))
        );
        assert_eq!(layout.hit_test(10, 1), Some(KeybindingListHit::Entry(0)));
        assert_eq!(layout.hit_test(10, 2), Some(KeybindingListHit::Entry(1)));
        assert_eq!(layout.hit_test(10, 3), Some(KeybindingListHit::AddRow));
        assert_eq!(layout.hit_test(0, 0), None);
    }
}
