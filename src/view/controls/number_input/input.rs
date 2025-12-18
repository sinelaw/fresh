//! Number input handling

use crossterm::event::{KeyCode, KeyEvent, MouseButton, MouseEvent, MouseEventKind};

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
                if layout.is_increment(event.column, event.row) {
                    self.increment();
                    Some(NumberInputEvent::Incremented(self.value))
                } else if layout.is_decrement(event.column, event.row) {
                    self.decrement();
                    Some(NumberInputEvent::Decremented(self.value))
                } else if layout.is_value(event.column, event.row) {
                    if !self.editing {
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

        if self.editing {
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
                KeyCode::Backspace => {
                    self.backspace();
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
                KeyCode::Up | KeyCode::Char('+') | KeyCode::Char('=') => {
                    self.increment();
                    Some(NumberInputEvent::Incremented(self.value))
                }
                KeyCode::Down | KeyCode::Char('-') => {
                    self.decrement();
                    Some(NumberInputEvent::Decremented(self.value))
                }
                KeyCode::Enter => {
                    self.start_editing();
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
            decrement_area: Rect::new(16, 0, 3, 1),
            increment_area: Rect::new(20, 0, 3, 1),
            full_area: Rect::new(0, 0, 23, 1),
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
    fn test_click_increment() {
        let mut state = NumberInputState::new(5, "Value");
        let layout = make_layout();

        let result = state.handle_mouse(mouse_down(20, 0), &layout);
        assert_eq!(result, Some(NumberInputEvent::Incremented(6)));
        assert_eq!(state.value, 6);
    }

    #[test]
    fn test_click_decrement() {
        let mut state = NumberInputState::new(5, "Value");
        let layout = make_layout();

        let result = state.handle_mouse(mouse_down(16, 0), &layout);
        assert_eq!(result, Some(NumberInputEvent::Decremented(4)));
        assert_eq!(state.value, 4);
    }

    #[test]
    fn test_click_value_starts_editing() {
        let mut state = NumberInputState::new(42, "Value");
        let layout = make_layout();

        let result = state.handle_mouse(mouse_down(10, 0), &layout);
        assert_eq!(result, Some(NumberInputEvent::StartedEditing));
        assert!(state.editing);
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
    fn test_keyboard_increment() {
        let mut state = NumberInputState::new(5, "Value").with_focus(FocusState::Focused);

        let up = KeyEvent::new(KeyCode::Up, KeyModifiers::empty());
        let result = state.handle_key(up);
        assert_eq!(result, Some(NumberInputEvent::Incremented(6)));
    }

    #[test]
    fn test_keyboard_decrement() {
        let mut state = NumberInputState::new(5, "Value").with_focus(FocusState::Focused);

        let down = KeyEvent::new(KeyCode::Down, KeyModifiers::empty());
        let result = state.handle_key(down);
        assert_eq!(result, Some(NumberInputEvent::Decremented(4)));
    }

    #[test]
    fn test_editing_confirm() {
        let mut state = NumberInputState::new(42, "Value");
        state.start_editing();
        state.edit_text = "100".to_string();

        let enter = KeyEvent::new(KeyCode::Enter, KeyModifiers::empty());
        let result = state.handle_key(enter);
        assert_eq!(result, Some(NumberInputEvent::Changed(100)));
        assert!(!state.editing);
    }

    #[test]
    fn test_editing_cancel() {
        let mut state = NumberInputState::new(42, "Value");
        state.start_editing();
        state.edit_text = "100".to_string();

        let esc = KeyEvent::new(KeyCode::Esc, KeyModifiers::empty());
        let result = state.handle_key(esc);
        assert_eq!(result, Some(NumberInputEvent::CancelledEditing));
        assert!(!state.editing);
        assert_eq!(state.value, 42);
    }

    #[test]
    fn test_disabled_ignores_input() {
        let mut state = NumberInputState::new(5, "Value").with_focus(FocusState::Disabled);
        let layout = make_layout();

        let result = state.handle_mouse(mouse_down(20, 0), &layout);
        assert!(result.is_none());
        assert_eq!(state.value, 5);
    }
}
