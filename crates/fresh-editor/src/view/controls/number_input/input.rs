//! Number input handling

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers, MouseButton, MouseEvent, MouseEventKind};

use super::{FocusState, NumberInputLayout, NumberInputState};

/// Events that can be returned from number input handling
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumberInputEvent {
    /// Value was incremented
    Incremented(i64),
    /// Value was decremented
    Decremented(i64),
    /// Value was changed (after editing confirmed)
    Changed(i64),
    /// Started editing mode
    StartedEditing,
    /// Cancelled editing
    CancelledEditing,
    /// Mouse is hovering over the control
    Hovered,
    /// Mouse left the control area
    Left,
}

impl NumberInputState {
    /// Handle a mouse event for this number input
    ///
    /// # Arguments
    /// * `event` - The mouse event to handle
    /// * `layout` - The control's rendered layout for hit testing
    ///
    /// # Returns
    /// * `Some(NumberInputEvent)` if the event was consumed
    /// * `None` if the event was not relevant
    pub fn handle_mouse(
        &mut self,
        event: MouseEvent,
        layout: &NumberInputLayout,
    ) -> Option<NumberInputEvent> {
        if !self.is_enabled() {
            return None;
        }

        match event.kind {
            MouseEventKind::Down(MouseButton::Left) => {
                if layout.is_value(event.column, event.row) {
                    if !self.editing() {
                        self.start_editing();
                        Some(NumberInputEvent::StartedEditing)
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
                    if self.focus != FocusState::Focused {
                        self.focus = FocusState::Hovered;
                    }
                    Some(NumberInputEvent::Hovered)
                } else if self.focus == FocusState::Hovered {
                    self.focus = FocusState::Normal;
                    Some(NumberInputEvent::Left)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Handle a keyboard event for this number input
    ///
    /// # Returns
    /// * `Some(NumberInputEvent)` if the event was consumed
    /// * `None` if the event was not relevant
    pub fn handle_key(&mut self, key: KeyEvent) -> Option<NumberInputEvent> {
        if !self.is_enabled() {
            return None;
        }

        if self.editing() {
            let ctrl = key.modifiers.contains(KeyModifiers::CONTROL);
            let shift = key.modifiers.contains(KeyModifiers::SHIFT);

            match key.code {
                KeyCode::Enter => {
                    let old_value = self.value;
                    self.confirm_editing();
                    if self.value != old_value {
                        Some(NumberInputEvent::Changed(self.value))
                    } else {
                        Some(NumberInputEvent::CancelledEditing)
                    }
                }
                KeyCode::Esc => {
                    self.cancel_editing();
                    Some(NumberInputEvent::CancelledEditing)
                }
                KeyCode::Backspace if ctrl => {
                    self.delete_word_backward();
                    None
                }
                KeyCode::Backspace => {
                    self.backspace();
                    None
                }
                KeyCode::Delete if ctrl => {
                    self.delete_word_forward();
                    None
                }
                KeyCode::Delete => {
                    self.delete();
                    None
                }
                KeyCode::Left if ctrl && shift => {
                    self.move_word_left_selecting();
                    None
                }
                KeyCode::Left if ctrl => {
                    self.move_word_left();
                    None
                }
                KeyCode::Left if shift => {
                    self.move_left_selecting();
                    None
                }
                KeyCode::Left => {
                    self.move_left();
                    None
                }
                KeyCode::Right if ctrl && shift => {
                    self.move_word_right_selecting();
                    None
                }
                KeyCode::Right if ctrl => {
                    self.move_word_right();
                    None
                }
                KeyCode::Right if shift => {
                    self.move_right_selecting();
                    None
                }
                KeyCode::Right => {
                    self.move_right();
                    None
                }
                KeyCode::Home if shift => {
                    self.move_home_selecting();
                    None
                }
                KeyCode::Home => {
                    self.move_home();
                    None
                }
                KeyCode::End if shift => {
                    self.move_end_selecting();
                    None
                }
                KeyCode::End => {
                    self.move_end();
                    None
                }
                KeyCode::Char('a') if ctrl => {
                    self.select_all();
                    None
                }
                KeyCode::Char(c) => {
                    self.insert_char(c);
                    None
                }
                _ => None,
            }
        } else if self.focus == FocusState::Focused {
            match key.code {
                KeyCode::Enter => {
                    self.start_editing();
                    Some(NumberInputEvent::StartedEditing)
                }
                // Direct-typing entry: pressing a digit, minus, or period on a
                // focused number replaces the value with what the user typed.
                // start_editing() select-alls so the first inserted char wipes
                // the old value.
                KeyCode::Char(c) if c.is_ascii_digit() || c == '-' || c == '.' => {
                    self.start_editing();
                    self.insert_char(c);
                    Some(NumberInputEvent::StartedEditing)
                }
                _ => None,
            }
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crossterm::event::KeyModifiers;
    use ratatui::layout::Rect;

    fn make_layout() -> NumberInputLayout {
        NumberInputLayout {
            value_area: Rect::new(8, 0, 7, 1),
            decrement_area: Rect::default(),
            increment_area: Rect::default(),
            full_area: Rect::new(0, 0, 15, 1),
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
    fn test_click_value_starts_editing() {
        let mut state = NumberInputState::new(42, "Value");
        let layout = make_layout();

        let result = state.handle_mouse(mouse_down(10, 0), &layout);
        assert_eq!(result, Some(NumberInputEvent::StartedEditing));
        assert!(state.editing());
    }

    #[test]
    fn test_hover() {
        let mut state = NumberInputState::new(42, "Value");
        let layout = make_layout();

        let result = state.handle_mouse(mouse_move(10, 0), &layout);
        assert_eq!(result, Some(NumberInputEvent::Hovered));
        assert_eq!(state.focus, FocusState::Hovered);

        let result = state.handle_mouse(mouse_move(30, 0), &layout);
        assert_eq!(result, Some(NumberInputEvent::Left));
        assert_eq!(state.focus, FocusState::Normal);
    }

    #[test]
    fn test_keyboard_digit_starts_editing_and_replaces_value() {
        let mut state = NumberInputState::new(42, "Value").with_focus(FocusState::Focused);

        let key = KeyEvent::new(KeyCode::Char('7'), KeyModifiers::empty());
        let result = state.handle_key(key);
        assert_eq!(result, Some(NumberInputEvent::StartedEditing));
        assert!(state.editing());
        // start_editing() select-alls so the typed digit replaces the value.
        assert_eq!(state.display_text(), "7");
    }

    #[test]
    fn test_editing_confirm() {
        let mut state = NumberInputState::new(42, "Value");
        state.start_editing();
        // Select all and replace with new value
        state.select_all();
        state.insert_str("100");

        let enter = KeyEvent::new(KeyCode::Enter, KeyModifiers::empty());
        let result = state.handle_key(enter);
        assert_eq!(result, Some(NumberInputEvent::Changed(100)));
        assert!(!state.editing());
    }

    #[test]
    fn test_editing_cancel() {
        let mut state = NumberInputState::new(42, "Value");
        state.start_editing();
        // Modify the value
        state.select_all();
        state.insert_str("100");

        let esc = KeyEvent::new(KeyCode::Esc, KeyModifiers::empty());
        let result = state.handle_key(esc);
        assert_eq!(result, Some(NumberInputEvent::CancelledEditing));
        assert!(!state.editing());
        assert_eq!(state.value, 42);
    }

    #[test]
    fn test_editing_cursor_navigation() {
        let mut state = NumberInputState::new(12345, "Value");
        state.start_editing();
        assert_eq!(state.cursor_col(), 5); // Cursor at end

        let left = KeyEvent::new(KeyCode::Left, KeyModifiers::empty());
        state.handle_key(left);
        assert_eq!(state.cursor_col(), 4);

        let home = KeyEvent::new(KeyCode::Home, KeyModifiers::empty());
        state.handle_key(home);
        assert_eq!(state.cursor_col(), 0);

        let end = KeyEvent::new(KeyCode::End, KeyModifiers::empty());
        state.handle_key(end);
        assert_eq!(state.cursor_col(), 5);
    }

    #[test]
    fn test_editing_selection() {
        let mut state = NumberInputState::new(123, "Value");
        state.start_editing();

        // Select all with Ctrl+A
        let ctrl_a = KeyEvent::new(KeyCode::Char('a'), KeyModifiers::CONTROL);
        state.handle_key(ctrl_a);
        assert!(state.has_selection());

        // Type to replace selection
        let key_9 = KeyEvent::new(KeyCode::Char('9'), KeyModifiers::empty());
        state.handle_key(key_9);
        assert_eq!(state.display_text(), "9");
    }

    #[test]
    fn test_disabled_ignores_input() {
        let mut state = NumberInputState::new(5, "Value").with_focus(FocusState::Disabled);
        let layout = make_layout();

        let result = state.handle_mouse(mouse_down(10, 0), &layout);
        assert!(result.is_none());
        assert_eq!(state.value, 5);
    }
}
