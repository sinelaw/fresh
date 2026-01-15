//! Text list input handling

use crossterm::event::{KeyCode, KeyEvent, MouseButton, MouseEvent, MouseEventKind};

use super::{TextListHit, TextListLayout, TextListState};

/// Events that can be returned from text list input handling
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TextListEvent {
    /// An item was added
    ItemAdded(String),
    /// An item was removed
    ItemRemoved(usize),
    /// An item was changed
    ItemChanged(usize, String),
    /// Focus moved to a different item
    FocusChanged(Option<usize>),
}

impl TextListState {
    /// Handle a mouse event for this text list
    ///
    /// # Arguments
    /// * `event` - The mouse event to handle
    /// * `layout` - The control's rendered layout for hit testing
    ///
    /// # Returns
    /// * `Some(TextListEvent)` if the event was consumed
    /// * `None` if the event was not relevant
    pub fn handle_mouse(
        &mut self,
        event: MouseEvent,
        layout: &TextListLayout,
    ) -> Option<TextListEvent> {
        if !self.is_enabled() {
            return None;
        }

        if let MouseEventKind::Down(MouseButton::Left) = event.kind {
            if let Some(hit) = layout.hit_test(event.column, event.row) {
                match hit {
                    TextListHit::Button(Some(index)) => {
                        // Remove button clicked
                        self.remove_item(index);
                        return Some(TextListEvent::ItemRemoved(index));
                    }
                    TextListHit::Button(None) => {
                        // Add button clicked
                        if !self.new_item_text.is_empty() {
                            let item = self.new_item_text.clone();
                            self.add_item();
                            return Some(TextListEvent::ItemAdded(item));
                        }
                    }
                    TextListHit::TextField(Some(index)) => {
                        // Focus on existing item
                        self.focus_item(index);
                        return Some(TextListEvent::FocusChanged(Some(index)));
                    }
                    TextListHit::TextField(None) => {
                        // Focus on add-new field
                        self.focus_new_item();
                        return Some(TextListEvent::FocusChanged(None));
                    }
                }
            }
        }
        None
    }

    /// Handle a keyboard event for this text list
    ///
    /// # Returns
    /// * `Some(TextListEvent)` if the event was consumed
    /// * `None` if the event was not relevant
    pub fn handle_key(&mut self, key: KeyEvent) -> Option<TextListEvent> {
        if !self.is_enabled() {
            return None;
        }

        match key.code {
            KeyCode::Enter => {
                if self.focused_item.is_none() && !self.new_item_text.is_empty() {
                    let item = self.new_item_text.clone();
                    self.add_item();
                    Some(TextListEvent::ItemAdded(item))
                } else {
                    None
                }
            }
            KeyCode::Backspace => {
                if self.cursor > 0 {
                    self.backspace();
                    if let Some(idx) = self.focused_item {
                        Some(TextListEvent::ItemChanged(idx, self.items[idx].clone()))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            KeyCode::Delete => {
                if let Some(idx) = self.focused_item {
                    if idx < self.items.len() {
                        self.remove_item(idx);
                        return Some(TextListEvent::ItemRemoved(idx));
                    }
                }
                None
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
                Some(TextListEvent::FocusChanged(self.focused_item))
            }
            KeyCode::Down => {
                self.focus_next();
                Some(TextListEvent::FocusChanged(self.focused_item))
            }
            KeyCode::Char(c) => {
                self.insert(c);
                if let Some(idx) = self.focused_item {
                    Some(TextListEvent::ItemChanged(idx, self.items[idx].clone()))
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

    fn make_layout() -> TextListLayout {
        TextListLayout {
            rows: vec![
                super::super::TextListRowLayout {
                    text_area: Rect::new(2, 1, 22, 1),
                    button_area: Rect::new(25, 1, 3, 1),
                    index: Some(0),
                },
                super::super::TextListRowLayout {
                    text_area: Rect::new(2, 2, 22, 1),
                    button_area: Rect::new(25, 2, 3, 1),
                    index: None,
                },
            ],
            full_area: Rect::new(0, 0, 30, 3),
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
    fn test_click_remove_button() {
        let mut state = TextListState::new("Items").with_items(vec!["item".to_string()]);
        let layout = make_layout();

        let result = state.handle_mouse(mouse_down(25, 1), &layout);
        assert_eq!(result, Some(TextListEvent::ItemRemoved(0)));
        assert!(state.items.is_empty());
    }

    #[test]
    fn test_click_add_button() {
        let mut state = TextListState::new("Items");
        state.new_item_text = "new".to_string();
        let layout = make_layout();

        let result = state.handle_mouse(mouse_down(25, 2), &layout);
        assert_eq!(result, Some(TextListEvent::ItemAdded("new".to_string())));
        assert_eq!(state.items, vec!["new"]);
    }

    #[test]
    fn test_click_text_field() {
        let mut state = TextListState::new("Items").with_items(vec!["item".to_string()]);
        let layout = make_layout();

        let result = state.handle_mouse(mouse_down(10, 1), &layout);
        assert_eq!(result, Some(TextListEvent::FocusChanged(Some(0))));
        assert_eq!(state.focused_item, Some(0));
    }

    #[test]
    fn test_keyboard_navigation() {
        let mut state =
            TextListState::new("Items").with_items(vec!["a".to_string(), "b".to_string()]);
        state.focus_new_item();

        let up = KeyEvent::new(KeyCode::Up, KeyModifiers::empty());
        let result = state.handle_key(up);
        assert_eq!(result, Some(TextListEvent::FocusChanged(Some(1))));

        let result = state.handle_key(up);
        assert_eq!(result, Some(TextListEvent::FocusChanged(Some(0))));
    }

    #[test]
    fn test_enter_adds_item() {
        let mut state = TextListState::new("Items");
        state.new_item_text = "test".to_string();

        let enter = KeyEvent::new(KeyCode::Enter, KeyModifiers::empty());
        let result = state.handle_key(enter);
        assert_eq!(result, Some(TextListEvent::ItemAdded("test".to_string())));
        assert_eq!(state.items, vec!["test"]);
    }

    #[test]
    fn test_delete_removes_focused_item() {
        let mut state =
            TextListState::new("Items").with_items(vec!["a".to_string(), "b".to_string()]);
        state.focus_item(0);

        let delete = KeyEvent::new(KeyCode::Delete, KeyModifiers::empty());
        let result = state.handle_key(delete);
        assert_eq!(result, Some(TextListEvent::ItemRemoved(0)));
        assert_eq!(state.items, vec!["b"]);
    }

    #[test]
    fn test_typing_in_item() {
        let mut state = TextListState::new("Items").with_items(vec!["hello".to_string()]);
        state.focus_item(0);

        let key = KeyEvent::new(KeyCode::Char('!'), KeyModifiers::empty());
        let result = state.handle_key(key);
        assert_eq!(
            result,
            Some(TextListEvent::ItemChanged(0, "hello!".to_string()))
        );
    }
}
