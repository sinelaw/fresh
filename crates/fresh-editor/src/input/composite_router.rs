//! Input routing for composite buffers
//!
//! Routes keyboard and mouse input to the appropriate source buffer
//! based on focus state and cursor position within the composite view.

use crate::model::composite_buffer::CompositeBuffer;
use crate::view::composite_view::CompositeViewState;
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

/// Result of routing an input event
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RoutedEvent {
    /// Event affects composite view scrolling
    CompositeScroll(ScrollAction),
    /// Switch focus to another pane
    SwitchPane(Direction),
    /// Event not handled by composite router
    Unhandled,
}

/// Scroll actions for the composite view
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScrollAction {
    Up(usize),
    Down(usize),
}

/// Direction for navigation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Next,
    Prev,
}

/// Routes input events for a composite buffer
pub struct CompositeInputRouter;

impl CompositeInputRouter {
    /// Route a key event to the appropriate action.
    ///
    /// Only intercepts keys that need composite-specific handling (pane
    /// switching, hunk navigation, close). Everything else — arrows,
    /// Home/End, PageUp/PageDown, typing — returns `Unhandled` so the
    /// editor's normal key dispatch handles it natively.
    pub fn route_key_event(
        _composite: &CompositeBuffer,
        _view_state: &CompositeViewState,
        event: &KeyEvent,
    ) -> RoutedEvent {
        match (event.modifiers, event.code) {
            // Scroll (j/k act as line-by-line scroll in the composite view)
            (KeyModifiers::NONE, KeyCode::Char('j')) => {
                RoutedEvent::CompositeScroll(ScrollAction::Down(1))
            }
            (KeyModifiers::NONE, KeyCode::Char('k')) => {
                RoutedEvent::CompositeScroll(ScrollAction::Up(1))
            }

            // Pane switching
            (KeyModifiers::NONE, KeyCode::Tab) => RoutedEvent::SwitchPane(Direction::Next),
            (KeyModifiers::SHIFT, KeyCode::BackTab) => RoutedEvent::SwitchPane(Direction::Prev),

            // Hunk navigation (n/p/]/[) and close (q/Esc) are handled by the
            // Action system via CompositeBuffer context keybindings, making
            // them rebindable through the keybinding editor.
            _ => RoutedEvent::Unhandled,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::composite_buffer::{CompositeLayout, SourcePane};
    use crate::model::event::BufferId;

    fn create_test_composite() -> (CompositeBuffer, CompositeViewState) {
        let sources = vec![
            SourcePane::new(BufferId(1), "OLD", false),
            SourcePane::new(BufferId(2), "NEW", true),
        ];
        let composite = CompositeBuffer::new(
            BufferId(0),
            "Test Diff".to_string(),
            "diff-view".to_string(),
            CompositeLayout::default(),
            sources,
        );
        let view_state = CompositeViewState::new(BufferId(0), 2);
        (composite, view_state)
    }

    fn route(code: KeyCode, modifiers: KeyModifiers) -> RoutedEvent {
        let (composite, view_state) = create_test_composite();
        CompositeInputRouter::route_key_event(
            &composite,
            &view_state,
            &KeyEvent::new(code, modifiers),
        )
    }

    #[test]
    fn test_j_k_scroll_the_composite_view() {
        assert_eq!(
            route(KeyCode::Char('j'), KeyModifiers::NONE),
            RoutedEvent::CompositeScroll(ScrollAction::Down(1))
        );
        assert_eq!(
            route(KeyCode::Char('k'), KeyModifiers::NONE),
            RoutedEvent::CompositeScroll(ScrollAction::Up(1))
        );
    }

    #[test]
    fn test_tab_switches_pane() {
        assert_eq!(
            route(KeyCode::Tab, KeyModifiers::NONE),
            RoutedEvent::SwitchPane(Direction::Next)
        );
        assert_eq!(
            route(KeyCode::BackTab, KeyModifiers::SHIFT),
            RoutedEvent::SwitchPane(Direction::Prev)
        );
    }

    /// Arrows and typing fall through to the editor's native dispatch.
    #[test]
    fn test_other_keys_are_unhandled() {
        assert_eq!(
            route(KeyCode::Down, KeyModifiers::NONE),
            RoutedEvent::Unhandled
        );
        assert_eq!(
            route(KeyCode::Char('x'), KeyModifiers::NONE),
            RoutedEvent::Unhandled
        );
    }
}
