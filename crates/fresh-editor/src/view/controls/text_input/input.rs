//! Text input handling

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers, MouseButton, MouseEvent, MouseEventKind};

use super::{FocusState, TextInputLayout, TextInputState};

/// Events that can be returned from text input handling
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TextInputEvent {
    /// Text was changed
    Changed(String),
    /// Input was submitted (Enter pressed)
    Submitted(String),
    /// Input was cancelled (Escape pressed)
    Cancelled,
    /// Input gained focus
    Focused,
    /// Mouse is hovering
    Hovered,
    /// Mouse left the area
    Left,
}

impl TextInputState {
    /// Handle a mouse event for this text input
    ///
    /// # Arguments
    /// * `event` - The mouse event to handle
    /// * `layout` - The control's rendered layout for hit testing
    ///
    /// # Returns
    /// * `Some(TextInputEvent)` if the event was consumed
    /// * `None` if the event was not relevant
    pub fn handle_mouse(
        &mut self,
        event: MouseEvent,
        layout: &TextInputLayout,
    ) -> Option<TextInputEvent> {
        if !self.is_enabled() {
            return None;
        }

        match event.kind {
            MouseEventKind::Down(MouseButton::Left) => {
                if layout.is_input(event.column, event.row) {
                    // Click in input area - could set cursor position based on click
                    if self.focus != FocusState::Focused {
                        self.focus = FocusState::Focused;
                        Some(TextInputEvent::Focused)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            MouseEventKind::Moved => {
                let inside = layout.contains(event.column, event.row);
                if inside {
                    if self.focus != FocusState::Focused && self.focus != FocusState::Hovered {
                        self.focus = FocusState::Hovered;
                    }
                    Some(TextInputEvent::Hovered)
                } else if self.focus == FocusState::Hovered {
                    self.focus = FocusState::Normal;
                    Some(TextInputEvent::Left)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Handle a keyboard event for this text input
    ///
    /// # Returns
    /// * `Some(TextInputEvent)` if the event was consumed
    /// * `None` if the event was not relevant
    pub fn handle_key(&mut self, key: KeyEvent) -> Option<TextInputEvent> {
        if !self.is_enabled() || self.focus != FocusState::Focused {
            return None;
        }

        match key.code {
            KeyCode::Enter => Some(TextInputEvent::Submitted(self.value.clone())),
            KeyCode::Esc => Some(TextInputEvent::Cancelled),
            KeyCode::Backspace => {
                if !self.value.is_empty() && self.cursor > 0 {
                    self.backspace();
                    Some(TextInputEvent::Changed(self.value.clone()))
                } else {
                    None
                }
            }
            KeyCode::Delete => {
                if self.cursor < self.value.len() {
                    self.delete();
                    Some(TextInputEvent::Changed(self.value.clone()))
                } else {
                    None
                }
            }
            KeyCode::Left => {
                if key.modifiers.contains(KeyModifiers::CONTROL) {
                    self.move_home();
                } else {
                    self.move_left();
                }
                None
            }
            KeyCode::Right => {
                if key.modifiers.contains(KeyModifiers::CONTROL) {
                    self.move_end();
                } else {
                    self.move_right();
                }
                None
            }
            KeyCode::Home => {
                self.move_home();
                None
            }
            KeyCode::End => {
                self.move_end();
                None
            }
            KeyCode::Char(c) => {
                self.insert(c);
                Some(TextInputEvent::Changed(self.value.clone()))
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::layout::Rect;

    fn make_layout() -> TextInputLayout {
        TextInputLayout {
            input_area: Rect::new(8, 0, 20, 1),
            full_area: Rect::new(0, 0, 28, 1),
            cursor_pos: None,
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

    fn mouse_move(x: u16, y: u16) -> MouseEvent {
        MouseEvent {
            kind: MouseEventKind::Moved,
            column: x,
            row: y,
            modifiers: KeyModifiers::empty(),
        }
    }

    #[test]
    fn test_click_focuses() {
        let mut state = TextInputState::new("Name");
        let layout = make_layout();

        let result = state.handle_mouse(mouse_down(10, 0), &layout);
        assert_eq!(result, Some(TextInputEvent::Focused));
        assert_eq!(state.focus, FocusState::Focused);
    }

    #[test]
    fn test_hover() {
        let mut state = TextInputState::new("Name");
        let layout = make_layout();

        let result = state.handle_mouse(mouse_move(10, 0), &layout);
        assert_eq!(result, Some(TextInputEvent::Hovered));

        let result = state.handle_mouse(mouse_move(30, 0), &layout);
        assert_eq!(result, Some(TextInputEvent::Left));
    }

    #[test]
    fn test_typing() {
        let mut state = TextInputState::new("Name").with_focus(FocusState::Focused);

        let a = KeyEvent::new(KeyCode::Char('a'), KeyModifiers::empty());
        let result = state.handle_key(a);
        assert_eq!(result, Some(TextInputEvent::Changed("a".to_string())));

        let b = KeyEvent::new(KeyCode::Char('b'), KeyModifiers::empty());
        state.handle_key(b);
        assert_eq!(state.value, "ab");
    }

    #[test]
    fn test_backspace() {
        let mut state = TextInputState::new("Name")
            .with_value("abc")
            .with_focus(FocusState::Focused);

        let bs = KeyEvent::new(KeyCode::Backspace, KeyModifiers::empty());
        let result = state.handle_key(bs);
        assert_eq!(result, Some(TextInputEvent::Changed("ab".to_string())));
    }

    #[test]
    fn test_submit() {
        let mut state = TextInputState::new("Name")
            .with_value("John")
            .with_focus(FocusState::Focused);

        let enter = KeyEvent::new(KeyCode::Enter, KeyModifiers::empty());
        let result = state.handle_key(enter);
        assert_eq!(result, Some(TextInputEvent::Submitted("John".to_string())));
    }

    #[test]
    fn test_cancel() {
        let mut state = TextInputState::new("Name")
            .with_value("John")
            .with_focus(FocusState::Focused);

        let esc = KeyEvent::new(KeyCode::Esc, KeyModifiers::empty());
        let result = state.handle_key(esc);
        assert_eq!(result, Some(TextInputEvent::Cancelled));
    }

    #[test]
    fn test_cursor_movement() {
        let mut state = TextInputState::new("Name")
            .with_value("hello")
            .with_focus(FocusState::Focused);

        let left = KeyEvent::new(KeyCode::Left, KeyModifiers::empty());
        state.handle_key(left);
        assert_eq!(state.cursor, 4);

        let home = KeyEvent::new(KeyCode::Home, KeyModifiers::empty());
        state.handle_key(home);
        assert_eq!(state.cursor, 0);

        let end = KeyEvent::new(KeyCode::End, KeyModifiers::empty());
        state.handle_key(end);
        assert_eq!(state.cursor, 5);
    }

    #[test]
    fn test_unfocused_ignores_keyboard() {
        let mut state = TextInputState::new("Name"); // Normal focus

        let a = KeyEvent::new(KeyCode::Char('a'), KeyModifiers::empty());
        let result = state.handle_key(a);
        assert!(result.is_none());
        assert!(state.value.is_empty());
    }
}
