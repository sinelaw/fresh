//! Keybinding list input handling

use crossterm::event::{KeyCode, KeyEvent, MouseButton, MouseEvent, MouseEventKind};

use super::{KeybindingListHit, KeybindingListLayout, KeybindingListState};

/// Events that can be returned from keybinding list input handling
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KeybindingListEvent {
    /// A binding was removed
    BindingRemoved(usize),
    /// Focus moved to a different entry
    FocusChanged(Option<usize>),
    /// Add new binding requested (user clicked add row or pressed Enter on it)
    AddRequested,
    /// Edit binding requested (user pressed Enter on a binding)
    EditRequested(usize),
}

impl KeybindingListState {
    /// Handle a mouse event for this keybinding list
    ///
    /// # Arguments
    /// * `event` - The mouse event to handle
    /// * `layout` - The control's rendered layout for hit testing
    ///
    /// # Returns
    /// * `Some(KeybindingListEvent)` if the event was consumed
    /// * `None` if the event was not relevant
    pub fn handle_mouse(
        &mut self,
        event: MouseEvent,
        layout: &KeybindingListLayout,
    ) -> Option<KeybindingListEvent> {
        if !self.is_enabled() {
            return None;
        }

        if let MouseEventKind::Down(MouseButton::Left) = event.kind {
            if let Some(hit) = layout.hit_test(event.column, event.row) {
                match hit {
                    KeybindingListHit::DeleteButton(index) => {
                        self.remove_binding(index);
                        return Some(KeybindingListEvent::BindingRemoved(index));
                    }
                    KeybindingListHit::Entry(index) => {
                        self.focus_entry(index);
                        return Some(KeybindingListEvent::FocusChanged(Some(index)));
                    }
                    KeybindingListHit::AddRow => {
                        self.focus_add_row();
                        return Some(KeybindingListEvent::FocusChanged(None));
                    }
                }
            }
        }
        None
    }

    /// Handle a keyboard event for this keybinding list
    ///
    /// # Returns
    /// * `Some(KeybindingListEvent)` if the event was consumed
    /// * `None` if the event was not relevant
    pub fn handle_key(&mut self, key: KeyEvent) -> Option<KeybindingListEvent> {
        if !self.is_enabled() {
            return None;
        }

        match key.code {
            KeyCode::Enter => match self.focused_index {
                // On add row
                None => Some(KeybindingListEvent::AddRequested),
                // On an entry - request edit
                Some(index) => Some(KeybindingListEvent::EditRequested(index)),
            },
            KeyCode::Delete | KeyCode::Backspace => {
                if let Some(index) = self.focused_index {
                    self.remove_binding(index);
                    Some(KeybindingListEvent::BindingRemoved(index))
                } else {
                    None
                }
            }
            KeyCode::Up | KeyCode::Char('k') => {
                self.focus_prev();
                Some(KeybindingListEvent::FocusChanged(self.focused_index))
            }
            KeyCode::Down | KeyCode::Char('j') => {
                self.focus_next();
                Some(KeybindingListEvent::FocusChanged(self.focused_index))
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

    fn make_layout() -> KeybindingListLayout {
        KeybindingListLayout {
            entry_rects: vec![Rect::new(2, 1, 40, 1), Rect::new(2, 2, 40, 1)],
            delete_rects: vec![Rect::new(38, 1, 3, 1), Rect::new(38, 2, 3, 1)],
            add_rect: Some(Rect::new(2, 3, 40, 1)),
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
    fn test_click_delete_button() {
        let mut state = KeybindingListState::new("Test");
        state.add_binding(serde_json::json!({"key": "a", "action": "test"}));
        let layout = make_layout();

        let result = state.handle_mouse(mouse_down(38, 1), &layout);
        assert_eq!(result, Some(KeybindingListEvent::BindingRemoved(0)));
        assert!(state.bindings.is_empty());
    }

    #[test]
    fn test_click_entry() {
        let mut state = KeybindingListState::new("Test");
        state.add_binding(serde_json::json!({"key": "a", "action": "test"}));
        let layout = make_layout();

        let result = state.handle_mouse(mouse_down(10, 1), &layout);
        assert_eq!(result, Some(KeybindingListEvent::FocusChanged(Some(0))));
        assert_eq!(state.focused_index, Some(0));
    }

    #[test]
    fn test_click_add_row() {
        let mut state = KeybindingListState::new("Test");
        state.add_binding(serde_json::json!({"key": "a", "action": "test"}));
        state.focus_entry(0);
        let layout = make_layout();

        let result = state.handle_mouse(mouse_down(10, 3), &layout);
        assert_eq!(result, Some(KeybindingListEvent::FocusChanged(None)));
        assert!(state.focused_index.is_none());
    }

    #[test]
    fn test_keyboard_navigation() {
        let mut state = KeybindingListState::new("Test");
        state.add_binding(serde_json::json!({"key": "a", "action": "test1"}));
        state.add_binding(serde_json::json!({"key": "b", "action": "test2"}));

        let down = KeyEvent::new(KeyCode::Down, KeyModifiers::empty());
        let result = state.handle_key(down);
        assert_eq!(result, Some(KeybindingListEvent::FocusChanged(Some(0))));

        let result = state.handle_key(down);
        assert_eq!(result, Some(KeybindingListEvent::FocusChanged(Some(1))));

        let up = KeyEvent::new(KeyCode::Up, KeyModifiers::empty());
        let result = state.handle_key(up);
        assert_eq!(result, Some(KeybindingListEvent::FocusChanged(Some(0))));
    }

    #[test]
    fn test_enter_on_add_row() {
        let mut state = KeybindingListState::new("Test");

        let enter = KeyEvent::new(KeyCode::Enter, KeyModifiers::empty());
        let result = state.handle_key(enter);
        assert_eq!(result, Some(KeybindingListEvent::AddRequested));
    }

    #[test]
    fn test_enter_on_entry() {
        let mut state = KeybindingListState::new("Test");
        state.add_binding(serde_json::json!({"key": "a", "action": "test"}));
        state.focus_entry(0);

        let enter = KeyEvent::new(KeyCode::Enter, KeyModifiers::empty());
        let result = state.handle_key(enter);
        assert_eq!(result, Some(KeybindingListEvent::EditRequested(0)));
    }

    #[test]
    fn test_delete_removes_focused() {
        let mut state = KeybindingListState::new("Test");
        state.add_binding(serde_json::json!({"key": "a", "action": "test"}));
        state.focus_entry(0);

        let delete = KeyEvent::new(KeyCode::Delete, KeyModifiers::empty());
        let result = state.handle_key(delete);
        assert_eq!(result, Some(KeybindingListEvent::BindingRemoved(0)));
        assert!(state.bindings.is_empty());
    }
}
