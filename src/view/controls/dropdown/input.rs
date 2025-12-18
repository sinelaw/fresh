//! Dropdown input handling

use crossterm::event::{KeyCode, KeyEvent, MouseButton, MouseEvent, MouseEventKind};

use super::{DropdownLayout, DropdownState, FocusState};

/// Events that can be returned from dropdown input handling
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DropdownEvent {
    /// Dropdown was opened
    Opened,
    /// Dropdown was closed (confirmed selection)
    Closed,
    /// Selection was changed
    SelectionChanged(usize),
    /// Selection was cancelled (restored original)
    Cancelled,
    /// Mouse is hovering
    Hovered,
    /// Mouse left the area
    Left,
}

impl DropdownState {
    /// Handle a mouse event for this dropdown
    ///
    /// # Arguments
    /// * `event` - The mouse event to handle
    /// * `layout` - The control's rendered layout for hit testing
    ///
    /// # Returns
    /// * `Some(DropdownEvent)` if the event was consumed
    /// * `None` if the event was not relevant
    pub fn handle_mouse(
        &mut self,
        event: MouseEvent,
        layout: &DropdownLayout,
    ) -> Option<DropdownEvent> {
        if !self.is_enabled() {
            return None;
        }

        match event.kind {
            MouseEventKind::Down(MouseButton::Left) => {
                if self.open {
                    // Check if clicked on an option
                    if let Some(index) = layout.option_at(event.column, event.row) {
                        self.select(index);
                        return Some(DropdownEvent::SelectionChanged(index));
                    }
                    // Check if clicked on button (to close)
                    if layout.is_button(event.column, event.row) {
                        self.toggle_open();
                        return Some(DropdownEvent::Closed);
                    }
                    // Clicked outside - close and cancel
                    self.cancel();
                    return Some(DropdownEvent::Cancelled);
                } else {
                    // Closed - check if clicked on button to open
                    if layout.is_button(event.column, event.row) {
                        self.toggle_open();
                        return Some(DropdownEvent::Opened);
                    }
                }
                None
            }
            MouseEventKind::Moved => {
                let inside = layout.is_button(event.column, event.row)
                    || layout.option_at(event.column, event.row).is_some();

                if inside {
                    if self.focus != FocusState::Focused && self.focus != FocusState::Hovered {
                        self.focus = FocusState::Hovered;
                    }
                    Some(DropdownEvent::Hovered)
                } else if self.focus == FocusState::Hovered && !self.open {
                    self.focus = FocusState::Normal;
                    Some(DropdownEvent::Left)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Handle a keyboard event for this dropdown
    ///
    /// # Returns
    /// * `Some(DropdownEvent)` if the event was consumed
    /// * `None` if the event was not relevant
    pub fn handle_key(&mut self, key: KeyEvent) -> Option<DropdownEvent> {
        if !self.is_enabled() {
            return None;
        }

        // Only handle keys when focused
        if self.focus != FocusState::Focused && !self.open {
            return None;
        }

        match key.code {
            KeyCode::Enter | KeyCode::Char(' ') => {
                if self.open {
                    self.confirm();
                    Some(DropdownEvent::Closed)
                } else {
                    self.toggle_open();
                    Some(DropdownEvent::Opened)
                }
            }
            KeyCode::Esc => {
                if self.open {
                    self.cancel();
                    Some(DropdownEvent::Cancelled)
                } else {
                    None
                }
            }
            KeyCode::Up | KeyCode::Char('k') => {
                self.select_prev();
                Some(DropdownEvent::SelectionChanged(self.selected))
            }
            KeyCode::Down | KeyCode::Char('j') => {
                self.select_next();
                Some(DropdownEvent::SelectionChanged(self.selected))
            }
            KeyCode::Home => {
                if !self.options.is_empty() {
                    self.selected = 0;
                    Some(DropdownEvent::SelectionChanged(0))
                } else {
                    None
                }
            }
            KeyCode::End => {
                if !self.options.is_empty() {
                    self.selected = self.options.len() - 1;
                    Some(DropdownEvent::SelectionChanged(self.selected))
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

    fn make_layout(open: bool) -> DropdownLayout {
        let mut layout = DropdownLayout {
            button_area: Rect::new(10, 0, 15, 1),
            option_areas: Vec::new(),
            full_area: Rect::new(0, 0, 25, 1),
        };
        if open {
            layout.option_areas = vec![
                Rect::new(10, 1, 15, 1),
                Rect::new(10, 2, 15, 1),
                Rect::new(10, 3, 15, 1),
            ];
        }
        layout
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
    fn test_click_opens() {
        let mut state = DropdownState::new(
            vec!["A".to_string(), "B".to_string(), "C".to_string()],
            "Test",
        );
        let layout = make_layout(false);

        let result = state.handle_mouse(mouse_down(12, 0), &layout);
        assert_eq!(result, Some(DropdownEvent::Opened));
        assert!(state.open);
    }

    #[test]
    fn test_click_option_selects() {
        let mut state = DropdownState::new(
            vec!["A".to_string(), "B".to_string(), "C".to_string()],
            "Test",
        );
        state.open = true;
        let layout = make_layout(true);

        let result = state.handle_mouse(mouse_down(12, 2), &layout);
        assert_eq!(result, Some(DropdownEvent::SelectionChanged(1)));
        assert_eq!(state.selected, 1);
        assert!(!state.open);
    }

    #[test]
    fn test_click_outside_cancels() {
        let mut state = DropdownState::new(
            vec!["A".to_string(), "B".to_string(), "C".to_string()],
            "Test",
        )
        .with_selected(1);
        state.toggle_open();
        state.select_next();
        assert_eq!(state.selected, 2);

        let layout = make_layout(true);

        let result = state.handle_mouse(mouse_down(0, 5), &layout);
        assert_eq!(result, Some(DropdownEvent::Cancelled));
        assert!(!state.open);
        assert_eq!(state.selected, 1); // Restored
    }

    #[test]
    fn test_keyboard_navigation() {
        let mut state = DropdownState::new(
            vec!["A".to_string(), "B".to_string(), "C".to_string()],
            "Test",
        )
        .with_focus(FocusState::Focused);

        let down = KeyEvent::new(KeyCode::Down, KeyModifiers::empty());
        let result = state.handle_key(down);
        assert_eq!(result, Some(DropdownEvent::SelectionChanged(1)));

        let up = KeyEvent::new(KeyCode::Up, KeyModifiers::empty());
        let result = state.handle_key(up);
        assert_eq!(result, Some(DropdownEvent::SelectionChanged(0)));
    }

    #[test]
    fn test_enter_toggles() {
        let mut state = DropdownState::new(vec!["A".to_string(), "B".to_string()], "Test")
            .with_focus(FocusState::Focused);

        let enter = KeyEvent::new(KeyCode::Enter, KeyModifiers::empty());
        let result = state.handle_key(enter);
        assert_eq!(result, Some(DropdownEvent::Opened));
        assert!(state.open);

        let result = state.handle_key(enter);
        assert_eq!(result, Some(DropdownEvent::Closed));
        assert!(!state.open);
    }

    #[test]
    fn test_escape_cancels() {
        let mut state = DropdownState::new(
            vec!["A".to_string(), "B".to_string(), "C".to_string()],
            "Test",
        )
        .with_focus(FocusState::Focused);

        state.toggle_open();
        state.select_next();
        assert_eq!(state.selected, 1);

        let esc = KeyEvent::new(KeyCode::Esc, KeyModifiers::empty());
        let result = state.handle_key(esc);
        assert_eq!(result, Some(DropdownEvent::Cancelled));
        assert!(!state.open);
        assert_eq!(state.selected, 0); // Restored
    }
}
