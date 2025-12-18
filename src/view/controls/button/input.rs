//! Button input handling

use crossterm::event::{MouseButton, MouseEvent, MouseEventKind};

use super::{ButtonLayout, ButtonState, FocusState};

/// Events that can be returned from button input handling
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ButtonEvent {
    /// Button was clicked (mouse released over button)
    Clicked,
    /// Mouse is hovering over the button
    Hovered,
    /// Mouse left the button area
    Left,
}

impl ButtonState {
    /// Handle a mouse event for this button
    ///
    /// # Arguments
    /// * `event` - The mouse event to handle
    /// * `layout` - The button's rendered layout for hit testing
    ///
    /// # Returns
    /// * `Some(ButtonEvent)` if the event was consumed and an action should be taken
    /// * `None` if the event was not relevant to this button
    pub fn handle_mouse(
        &mut self,
        event: MouseEvent,
        layout: &ButtonLayout,
    ) -> Option<ButtonEvent> {
        if !self.is_enabled() {
            return None;
        }

        let inside = layout.contains(event.column, event.row);

        match event.kind {
            MouseEventKind::Down(MouseButton::Left) if inside => {
                self.pressed = true;
                None // Wait for release to trigger click
            }
            MouseEventKind::Up(MouseButton::Left) => {
                let was_pressed = self.pressed;
                self.pressed = false;

                if inside && was_pressed {
                    Some(ButtonEvent::Clicked)
                } else {
                    None
                }
            }
            MouseEventKind::Moved => {
                if inside {
                    if self.focus != FocusState::Focused {
                        self.focus = FocusState::Hovered;
                    }
                    Some(ButtonEvent::Hovered)
                } else if self.focus == FocusState::Hovered {
                    self.focus = FocusState::Normal;
                    Some(ButtonEvent::Left)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Handle a keyboard event for this button (when focused)
    ///
    /// # Returns
    /// * `Some(ButtonEvent::Clicked)` if Enter or Space was pressed
    /// * `None` otherwise
    pub fn handle_key(&mut self, key: crossterm::event::KeyEvent) -> Option<ButtonEvent> {
        use crossterm::event::KeyCode;

        if !self.is_enabled() || self.focus != FocusState::Focused {
            return None;
        }

        match key.code {
            KeyCode::Enter | KeyCode::Char(' ') => Some(ButtonEvent::Clicked),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crossterm::event::{KeyCode, KeyModifiers};
    use ratatui::layout::Rect;

    fn make_layout() -> ButtonLayout {
        ButtonLayout {
            button_area: Rect::new(0, 0, 10, 1),
        }
    }

    fn mouse_down(x: u16, y: u16) -> MouseEvent {
        MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: x,
            row: y,
            modifiers: crossterm::event::KeyModifiers::empty(),
        }
    }

    fn mouse_up(x: u16, y: u16) -> MouseEvent {
        MouseEvent {
            kind: MouseEventKind::Up(MouseButton::Left),
            column: x,
            row: y,
            modifiers: crossterm::event::KeyModifiers::empty(),
        }
    }

    fn mouse_move(x: u16, y: u16) -> MouseEvent {
        MouseEvent {
            kind: MouseEventKind::Moved,
            column: x,
            row: y,
            modifiers: crossterm::event::KeyModifiers::empty(),
        }
    }

    #[test]
    fn test_click_inside_button() {
        let mut state = ButtonState::new("Test");
        let layout = make_layout();

        // Mouse down inside
        let result = state.handle_mouse(mouse_down(5, 0), &layout);
        assert!(result.is_none());
        assert!(state.pressed);

        // Mouse up inside - should click
        let result = state.handle_mouse(mouse_up(5, 0), &layout);
        assert_eq!(result, Some(ButtonEvent::Clicked));
        assert!(!state.pressed);
    }

    #[test]
    fn test_click_outside_button() {
        let mut state = ButtonState::new("Test");
        let layout = make_layout();

        // Mouse down inside
        state.handle_mouse(mouse_down(5, 0), &layout);
        assert!(state.pressed);

        // Mouse up outside - should not click
        let result = state.handle_mouse(mouse_up(15, 0), &layout);
        assert!(result.is_none());
        assert!(!state.pressed);
    }

    #[test]
    fn test_hover() {
        let mut state = ButtonState::new("Test");
        let layout = make_layout();

        // Move inside
        let result = state.handle_mouse(mouse_move(5, 0), &layout);
        assert_eq!(result, Some(ButtonEvent::Hovered));
        assert_eq!(state.focus, FocusState::Hovered);

        // Move outside
        let result = state.handle_mouse(mouse_move(15, 0), &layout);
        assert_eq!(result, Some(ButtonEvent::Left));
        assert_eq!(state.focus, FocusState::Normal);
    }

    #[test]
    fn test_disabled_button_ignores_input() {
        let mut state = ButtonState::new("Test").with_focus(FocusState::Disabled);
        let layout = make_layout();

        let result = state.handle_mouse(mouse_down(5, 0), &layout);
        assert!(result.is_none());
        assert!(!state.pressed);
    }

    #[test]
    fn test_keyboard_activation() {
        let mut state = ButtonState::new("Test").with_focus(FocusState::Focused);

        let enter = crossterm::event::KeyEvent::new(KeyCode::Enter, KeyModifiers::empty());
        let result = state.handle_key(enter);
        assert_eq!(result, Some(ButtonEvent::Clicked));

        let space = crossterm::event::KeyEvent::new(KeyCode::Char(' '), KeyModifiers::empty());
        let result = state.handle_key(space);
        assert_eq!(result, Some(ButtonEvent::Clicked));
    }

    #[test]
    fn test_unfocused_button_ignores_keyboard() {
        let mut state = ButtonState::new("Test"); // Normal focus

        let enter = crossterm::event::KeyEvent::new(KeyCode::Enter, KeyModifiers::empty());
        let result = state.handle_key(enter);
        assert!(result.is_none());
    }
}
