//! Map input handling

use crossterm::event::{KeyCode, KeyEvent, MouseButton, MouseEvent, MouseEventKind};

use super::{MapHit, MapLayout, MapState};

/// Events that can be returned from map input handling
#[derive(Debug, Clone, PartialEq)]
pub enum MapEvent {
    /// An entry was added with the given key
    EntryAdded(String),
    /// An entry was removed
    EntryRemoved(usize),
    /// An entry was expanded or collapsed
    EntryToggled(usize, bool),
    /// Focus moved to a different entry
    FocusChanged(Option<usize>),
    /// Text in the new key field changed
    NewKeyChanged(String),
}

impl MapState {
    /// Handle a mouse event for this map control
    ///
    /// # Arguments
    /// * `event` - The mouse event to handle
    /// * `layout` - The control's rendered layout for hit testing
    ///
    /// # Returns
    /// * `Some(MapEvent)` if the event was consumed
    /// * `None` if the event was not relevant
    pub fn handle_mouse(&mut self, event: MouseEvent, layout: &MapLayout) -> Option<MapEvent> {
        if !self.is_enabled() {
            return None;
        }

        if let MouseEventKind::Down(MouseButton::Left) = event.kind {
            if let Some(hit) = layout.hit_test(event.column, event.row) {
                match hit {
                    MapHit::ExpandArrow(index) => {
                        self.toggle_expand(index);
                        return Some(MapEvent::EntryToggled(index, self.is_expanded(index)));
                    }
                    MapHit::EntryKey(index) => {
                        self.focus_entry(index);
                        return Some(MapEvent::FocusChanged(Some(index)));
                    }
                    MapHit::RemoveButton(index) => {
                        self.remove_entry(index);
                        return Some(MapEvent::EntryRemoved(index));
                    }
                    MapHit::AddRow => {
                        self.focus_new_entry();
                        return Some(MapEvent::FocusChanged(None));
                    }
                }
            }
        }
        None
    }

    /// Handle a keyboard event for this map control
    ///
    /// # Returns
    /// * `Some(MapEvent)` if the event was consumed
    /// * `None` if the event was not relevant
    pub fn handle_key(&mut self, key: KeyEvent) -> Option<MapEvent> {
        if !self.is_enabled() {
            return None;
        }

        match key.code {
            KeyCode::Enter => {
                if self.focused_entry.is_none() && !self.new_key_text.is_empty() {
                    let key = self.new_key_text.clone();
                    self.add_entry_from_input();
                    Some(MapEvent::EntryAdded(key))
                } else if let Some(index) = self.focused_entry {
                    // Toggle expand on Enter when focused on entry
                    self.toggle_expand(index);
                    Some(MapEvent::EntryToggled(index, self.is_expanded(index)))
                } else {
                    None
                }
            }
            KeyCode::Delete => {
                if let Some(index) = self.focused_entry {
                    self.remove_entry(index);
                    Some(MapEvent::EntryRemoved(index))
                } else {
                    None
                }
            }
            KeyCode::Backspace => {
                if self.focused_entry.is_none() && self.cursor > 0 {
                    self.backspace();
                    Some(MapEvent::NewKeyChanged(self.new_key_text.clone()))
                } else {
                    None
                }
            }
            KeyCode::Left => {
                self.move_left();
                None
            }
            KeyCode::Right => {
                self.move_right();
                None
            }
            KeyCode::Up => {
                self.focus_prev();
                Some(MapEvent::FocusChanged(self.focused_entry))
            }
            KeyCode::Down => {
                self.focus_next();
                Some(MapEvent::FocusChanged(self.focused_entry))
            }
            KeyCode::Char(' ') if self.focused_entry.is_some() => {
                // Space toggles expand when focused on entry
                if let Some(index) = self.focused_entry {
                    self.toggle_expand(index);
                    Some(MapEvent::EntryToggled(index, self.is_expanded(index)))
                } else {
                    None
                }
            }
            KeyCode::Char(c) => {
                if self.focused_entry.is_none() {
                    self.insert(c);
                    Some(MapEvent::NewKeyChanged(self.new_key_text.clone()))
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crossterm::event::KeyModifiers;
    use ratatui::layout::Rect;

    use crate::view::controls::map_input::MapEntryLayout;

    fn make_layout() -> MapLayout {
        MapLayout {
            full_area: Rect::new(0, 0, 50, 5),
            entry_areas: vec![MapEntryLayout {
                index: 0,
                row_area: Rect::new(0, 1, 50, 1),
                expand_area: Rect::new(2, 1, 1, 1),
                key_area: Rect::new(4, 1, 10, 1),
                remove_area: Rect::new(40, 1, 3, 1),
            }],
            add_row_area: Some(Rect::new(0, 2, 50, 1)),
        }
    }

    fn mouse_down(x: u16, y: u16) -> MouseEvent {
        MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: x,
            row: y,
            modifiers: KeyModifiers::empty(),
        }
    }

    #[test]
    fn test_click_expand_arrow() {
        let mut state = MapState::new("Test");
        state.add_entry("key1".to_string(), serde_json::json!({"foo": "bar"}));
        let layout = make_layout();

        let result = state.handle_mouse(mouse_down(2, 1), &layout);
        assert_eq!(result, Some(MapEvent::EntryToggled(0, true)));
        assert!(state.is_expanded(0));
    }

    #[test]
    fn test_click_remove_button() {
        let mut state = MapState::new("Test");
        state.add_entry("key1".to_string(), serde_json::json!({}));
        let layout = make_layout();

        let result = state.handle_mouse(mouse_down(40, 1), &layout);
        assert_eq!(result, Some(MapEvent::EntryRemoved(0)));
        assert!(state.entries.is_empty());
    }

    #[test]
    fn test_click_add_row() {
        let mut state = MapState::new("Test");
        state.new_key_text = "newkey".to_string();
        let layout = make_layout();

        // Clicking on the add row focuses it
        let result = state.handle_mouse(mouse_down(13, 2), &layout);
        assert_eq!(result, Some(MapEvent::FocusChanged(None)));
        assert!(state.focused_entry.is_none());
    }

    #[test]
    fn test_keyboard_navigation() {
        let mut state = MapState::new("Test");
        state.add_entry("a".to_string(), serde_json::json!({}));
        state.add_entry("b".to_string(), serde_json::json!({}));
        state.focus_new_entry();

        let up = KeyEvent::new(KeyCode::Up, KeyModifiers::empty());
        let result = state.handle_key(up);
        assert_eq!(result, Some(MapEvent::FocusChanged(Some(1))));

        let result = state.handle_key(up);
        assert_eq!(result, Some(MapEvent::FocusChanged(Some(0))));
    }

    #[test]
    fn test_enter_adds_entry() {
        let mut state = MapState::new("Test");
        state.new_key_text = "newkey".to_string();

        let enter = KeyEvent::new(KeyCode::Enter, KeyModifiers::empty());
        let result = state.handle_key(enter);
        assert_eq!(result, Some(MapEvent::EntryAdded("newkey".to_string())));
        assert_eq!(state.entries.len(), 1);
    }

    #[test]
    fn test_delete_removes_focused_entry() {
        let mut state = MapState::new("Test");
        state.add_entry("a".to_string(), serde_json::json!({}));
        state.focus_entry(0);

        let delete = KeyEvent::new(KeyCode::Delete, KeyModifiers::empty());
        let result = state.handle_key(delete);
        assert_eq!(result, Some(MapEvent::EntryRemoved(0)));
        assert!(state.entries.is_empty());
    }

    #[test]
    fn test_typing_in_new_key_field() {
        let mut state = MapState::new("Test");
        state.focus_new_entry();

        let key = KeyEvent::new(KeyCode::Char('a'), KeyModifiers::empty());
        let result = state.handle_key(key);
        assert_eq!(result, Some(MapEvent::NewKeyChanged("a".to_string())));
        assert_eq!(state.new_key_text, "a");
    }

    #[test]
    fn test_space_toggles_expansion() {
        let mut state = MapState::new("Test");
        state.add_entry("key1".to_string(), serde_json::json!({"foo": "bar"}));
        state.focus_entry(0);

        let space = KeyEvent::new(KeyCode::Char(' '), KeyModifiers::empty());
        let result = state.handle_key(space);
        assert_eq!(result, Some(MapEvent::EntryToggled(0, true)));
        assert!(state.is_expanded(0));
    }
}
