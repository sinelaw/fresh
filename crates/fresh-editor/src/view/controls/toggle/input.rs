//! Toggle input handling

use crossterm::event::{MouseButton, MouseEvent, MouseEventKind};

use super::{FocusState, ToggleLayout, ToggleState};

/// Events that can be returned from toggle input handling
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ToggleEvent {
    /// Toggle was toggled (value changed)
    Toggled(bool),
    /// Mouse is hovering over the toggle
    Hovered,
    /// Mouse left the toggle area
    Left,
}

impl ToggleState {
    /// Handle a mouse event for this toggle
    ///
    /// # Arguments
    /// * `event` - The mouse event to handle
    /// * `layout` - The toggle's rendered layout for hit testing
    ///
    /// # Returns
    /// * `Some(ToggleEvent)` if the event was consumed and an action should be taken
    /// * `None` if the event was not relevant to this toggle
    pub fn handle_mouse(
        &mut self,
        event: MouseEvent,
        layout: &ToggleLayout,
    ) -> Option<ToggleEvent> {
        if !self.is_enabled() {
            return None;
        }

        let inside = layout.contains(event.column, event.row);

        match event.kind {
            MouseEventKind::Down(MouseButton::Left) if inside => {
                self.toggle();
                Some(ToggleEvent::Toggled(self.checked))
            }
            MouseEventKind::Moved => {
                if inside {
                    if self.focus != FocusState::Focused {
                        self.focus = FocusState::Hovered;
                    }
                    Some(ToggleEvent::Hovered)
                } else if self.focus == FocusState::Hovered {
                    self.focus = FocusState::Normal;
                    Some(ToggleEvent::Left)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Handle a keyboard event for this toggle (when focused)
    ///
    /// # Returns
    /// * `Some(ToggleEvent::Toggled)` if Enter or Space was pressed
    /// * `None` otherwise
    pub fn handle_key(&mut self, key: crossterm::event::KeyEvent) -> Option<ToggleEvent> {
        use crossterm::event::KeyCode;

        if !self.is_enabled() || self.focus != FocusState::Focused {
            return None;
        }

        match key.code {
            KeyCode::Enter | KeyCode::Char(' ') => {
                self.toggle();
                Some(ToggleEvent::Toggled(self.checked))
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crossterm::event::{KeyCode, KeyModifiers};
    use ratatui::layout::Rect;

    fn make_layout() -> ToggleLayout {
        ToggleLayout {
            checkbox_area: Rect::new(8, 0, 3, 1),
            full_area: Rect::new(0, 0, 11, 1),
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

    fn mouse_move(x: u16, y: u16) -> MouseEvent {
        MouseEvent {
            kind: MouseEventKind::Moved,
            column: x,
            row: y,
            modifiers: crossterm::event::KeyModifiers::empty(),
        }
    }

    #[test]
    fn test_click_toggles() {
        let mut state = ToggleState::new(false, "Enable");
        let layout = make_layout();

        let result = state.handle_mouse(mouse_down(5, 0), &layout);
        assert_eq!(result, Some(ToggleEvent::Toggled(true)));
        assert!(state.checked);

        let result = state.handle_mouse(mouse_down(5, 0), &layout);
        assert_eq!(result, Some(ToggleEvent::Toggled(false)));
        assert!(!state.checked);
    }

    #[test]
    fn test_click_outside_ignored() {
        let mut state = ToggleState::new(false, "Enable");
        let layout = make_layout();

        let result = state.handle_mouse(mouse_down(15, 0), &layout);
        assert!(result.is_none());
        assert!(!state.checked);
    }

    #[test]
    fn test_hover() {
        let mut state = ToggleState::new(false, "Enable");
        let layout = make_layout();

        let result = state.handle_mouse(mouse_move(5, 0), &layout);
        assert_eq!(result, Some(ToggleEvent::Hovered));
        assert_eq!(state.focus, FocusState::Hovered);

        let result = state.handle_mouse(mouse_move(15, 0), &layout);
        assert_eq!(result, Some(ToggleEvent::Left));
        assert_eq!(state.focus, FocusState::Normal);
    }

    #[test]
    fn test_disabled_ignores_input() {
        let mut state = ToggleState::new(false, "Enable").with_focus(FocusState::Disabled);
        let layout = make_layout();

        let result = state.handle_mouse(mouse_down(5, 0), &layout);
        assert!(result.is_none());
        assert!(!state.checked);
    }

    #[test]
    fn test_keyboard_activation() {
        let mut state = ToggleState::new(false, "Enable").with_focus(FocusState::Focused);

        let enter = crossterm::event::KeyEvent::new(KeyCode::Enter, KeyModifiers::empty());
        let result = state.handle_key(enter);
        assert_eq!(result, Some(ToggleEvent::Toggled(true)));
        assert!(state.checked);

        let space = crossterm::event::KeyEvent::new(KeyCode::Char(' '), KeyModifiers::empty());
        let result = state.handle_key(space);
        assert_eq!(result, Some(ToggleEvent::Toggled(false)));
        assert!(!state.checked);
    }

    #[test]
    fn test_unfocused_ignores_keyboard() {
        let mut state = ToggleState::new(false, "Enable"); // Normal focus

        let enter = crossterm::event::KeyEvent::new(KeyCode::Enter, KeyModifiers::empty());
        let result = state.handle_key(enter);
        assert!(result.is_none());
        assert!(!state.checked);
    }
}
