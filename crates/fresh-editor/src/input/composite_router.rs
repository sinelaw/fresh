//! Input routing for composite buffers
//!
//! Routes keyboard and mouse input to the appropriate source buffer
//! based on focus state and cursor position within the composite view.

use crate::model::composite_buffer::CompositeBuffer;
use crate::model::event::BufferId;
use crate::view::composite_view::CompositeViewState;
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

/// Result of routing an input event
#[derive(Debug, Clone)]
pub enum RoutedEvent {
    /// Event affects composite view scrolling
    CompositeScroll(ScrollAction),
    /// Switch focus to another pane
    SwitchPane(Direction),
    /// Navigate to next/previous hunk
    NavigateHunk(Direction),
    /// Route to a source buffer for editing
    ToSourceBuffer {
        buffer_id: BufferId,
        action: BufferAction,
    },
    /// Cursor movement within focused pane
    PaneCursor(CursorAction),
    /// Selection action
    Selection(SelectionAction),
    /// Yank/copy the selected text
    Yank,
    /// Event was blocked (e.g., editing read-only pane)
    Blocked(&'static str),
    /// Close the composite view
    Close,
    /// Event not handled by composite router
    Unhandled,
}

/// Selection actions for visual mode
#[derive(Debug, Clone, Copy)]
pub enum SelectionAction {
    /// Start visual selection at current position
    StartVisual,
    /// Start line-wise visual selection
    StartVisualLine,
    /// Clear selection
    ClearSelection,
    /// Extend selection up
    ExtendUp,
    /// Extend selection down
    ExtendDown,
    /// Extend selection left
    ExtendLeft,
    /// Extend selection right
    ExtendRight,
}

/// Scroll actions for the composite view
#[derive(Debug, Clone, Copy)]
pub enum ScrollAction {
    Up(usize),
    Down(usize),
    PageUp,
    PageDown,
    ToTop,
    ToBottom,
    ToRow(usize),
}

/// Direction for navigation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Next,
    Prev,
}

/// Actions that modify buffer content
#[derive(Debug, Clone)]
pub enum BufferAction {
    Insert(char),
    InsertString(String),
    Delete,
    Backspace,
    NewLine,
}

/// Cursor movement actions
#[derive(Debug, Clone, Copy)]
pub enum CursorAction {
    Up,
    Down,
    Left,
    Right,
    LineStart,
    LineEnd,
    WordLeft,
    WordRight,
    Top,
    Bottom,
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

            // Hunk navigation is handled by the Action system via CompositeBuffer
            // context keybindings (n/p/]/[), making them rebindable through the
            // keybinding editor.

            // Close composite view
            (KeyModifiers::NONE, KeyCode::Char('q')) | (KeyModifiers::NONE, KeyCode::Esc) => {
                RoutedEvent::Close
            }

            _ => RoutedEvent::Unhandled,
        }
    }

    /// Convert display coordinates to source buffer coordinates
    pub fn display_to_source(
        composite: &CompositeBuffer,
        _view_state: &CompositeViewState,
        display_row: usize,
        display_col: usize,
        pane_index: usize,
    ) -> Option<SourceCoordinate> {
        let aligned_row = composite.alignment.get_row(display_row)?;
        let source_ref = aligned_row.get_pane_line(pane_index)?;

        Some(SourceCoordinate {
            buffer_id: composite.sources.get(pane_index)?.buffer_id,
            byte_offset: source_ref.byte_range.start + display_col,
            line: source_ref.line,
            column: display_col,
        })
    }

    /// Determine which pane a click occurred in
    pub fn click_to_pane(
        view_state: &CompositeViewState,
        click_x: u16,
        area_x: u16,
    ) -> Option<usize> {
        let mut x = area_x;
        for (i, &width) in view_state.pane_widths.iter().enumerate() {
            if click_x >= x && click_x < x + width {
                return Some(i);
            }
            x += width + 1; // +1 for separator
        }
        None
    }

    /// Navigate to the next or previous hunk
    pub fn navigate_to_hunk(
        composite: &CompositeBuffer,
        view_state: &mut CompositeViewState,
        direction: Direction,
    ) -> bool {
        let current_row = view_state.scroll_row;
        let new_row = match direction {
            Direction::Next => composite.alignment.next_hunk_row(current_row),
            Direction::Prev => composite.alignment.prev_hunk_row(current_row),
        };

        if let Some(row) = new_row {
            view_state.scroll_row = row;
            true
        } else {
            false
        }
    }
}

/// Coordinates within a source buffer
#[derive(Debug, Clone)]
pub struct SourceCoordinate {
    pub buffer_id: BufferId,
    pub byte_offset: usize,
    pub line: usize,
    pub column: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::composite_buffer::{CompositeLayout, SourcePane};

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

    #[test]
    fn test_scroll_routing() {
        let (composite, view_state) = create_test_composite();

        let event = KeyEvent::new(KeyCode::Down, KeyModifiers::NONE);
        let result = CompositeInputRouter::route_key_event(&composite, &view_state, &event);

        matches!(result, RoutedEvent::CompositeScroll(ScrollAction::Down(1)));
    }

    #[test]
    fn test_pane_switch_routing() {
        let (composite, view_state) = create_test_composite();

        let event = KeyEvent::new(KeyCode::Tab, KeyModifiers::NONE);
        let result = CompositeInputRouter::route_key_event(&composite, &view_state, &event);

        matches!(result, RoutedEvent::SwitchPane(Direction::Next));
    }

    #[test]
    fn test_readonly_blocking() {
        let (composite, view_state) = create_test_composite();
        // Focused pane is 0 (OLD), which is read-only

        let event = KeyEvent::new(KeyCode::Char('x'), KeyModifiers::NONE);
        let result = CompositeInputRouter::route_key_event(&composite, &view_state, &event);

        matches!(result, RoutedEvent::Blocked(_));
    }

    #[test]
    fn test_editable_routing() {
        let (composite, mut view_state) = create_test_composite();
        view_state.focused_pane = 1; // NEW pane is editable

        let event = KeyEvent::new(KeyCode::Char('x'), KeyModifiers::NONE);
        let result = CompositeInputRouter::route_key_event(&composite, &view_state, &event);

        matches!(
            result,
            RoutedEvent::ToSourceBuffer {
                buffer_id: BufferId(2),
                action: BufferAction::Insert('x'),
            }
        );
    }
}
