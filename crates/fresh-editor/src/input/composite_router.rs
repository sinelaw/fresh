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
}

/// Routes input events for a composite buffer
pub struct CompositeInputRouter;

impl CompositeInputRouter {
    /// Route a key event to the appropriate action
    pub fn route_key_event(
        composite: &CompositeBuffer,
        view_state: &CompositeViewState,
        event: &KeyEvent,
    ) -> RoutedEvent {
        let focused_pane = composite.sources.get(view_state.focused_pane);

        match (event.modifiers, event.code) {
            // Scroll navigation
            (KeyModifiers::NONE, KeyCode::Up) | (KeyModifiers::NONE, KeyCode::Char('k')) => {
                RoutedEvent::CompositeScroll(ScrollAction::Up(1))
            }
            (KeyModifiers::NONE, KeyCode::Down) | (KeyModifiers::NONE, KeyCode::Char('j')) => {
                RoutedEvent::CompositeScroll(ScrollAction::Down(1))
            }
            (KeyModifiers::CONTROL, KeyCode::Char('u')) => {
                RoutedEvent::CompositeScroll(ScrollAction::PageUp)
            }
            (KeyModifiers::CONTROL, KeyCode::Char('d')) => {
                RoutedEvent::CompositeScroll(ScrollAction::PageDown)
            }
            (KeyModifiers::NONE, KeyCode::PageUp) => {
                RoutedEvent::CompositeScroll(ScrollAction::PageUp)
            }
            (KeyModifiers::NONE, KeyCode::PageDown) => {
                RoutedEvent::CompositeScroll(ScrollAction::PageDown)
            }
            (KeyModifiers::NONE, KeyCode::Home) | (KeyModifiers::NONE, KeyCode::Char('g')) => {
                RoutedEvent::CompositeScroll(ScrollAction::ToTop)
            }
            (KeyModifiers::SHIFT, KeyCode::Char('G')) | (KeyModifiers::NONE, KeyCode::End) => {
                RoutedEvent::CompositeScroll(ScrollAction::ToBottom)
            }

            // Pane switching
            (KeyModifiers::NONE, KeyCode::Tab) => RoutedEvent::SwitchPane(Direction::Next),
            (KeyModifiers::SHIFT, KeyCode::BackTab) => RoutedEvent::SwitchPane(Direction::Prev),
            (KeyModifiers::NONE, KeyCode::Char('h')) => RoutedEvent::SwitchPane(Direction::Prev),
            (KeyModifiers::NONE, KeyCode::Char('l')) => RoutedEvent::SwitchPane(Direction::Next),

            // Hunk navigation
            (KeyModifiers::NONE, KeyCode::Char('n')) => RoutedEvent::NavigateHunk(Direction::Next),
            (KeyModifiers::NONE, KeyCode::Char('p')) => RoutedEvent::NavigateHunk(Direction::Prev),
            (KeyModifiers::NONE, KeyCode::Char(']')) => RoutedEvent::NavigateHunk(Direction::Next),
            (KeyModifiers::NONE, KeyCode::Char('[')) => RoutedEvent::NavigateHunk(Direction::Prev),

            // Close
            (KeyModifiers::NONE, KeyCode::Char('q')) | (KeyModifiers::NONE, KeyCode::Esc) => {
                RoutedEvent::Close
            }

            // Visual selection
            (KeyModifiers::NONE, KeyCode::Char('v')) => {
                RoutedEvent::Selection(SelectionAction::StartVisual)
            }
            (KeyModifiers::SHIFT, KeyCode::Char('V')) => {
                RoutedEvent::Selection(SelectionAction::StartVisualLine)
            }

            // Yank (copy) selected text
            (KeyModifiers::NONE, KeyCode::Char('y')) => RoutedEvent::Yank,

            // Editing (if pane is editable)
            (KeyModifiers::NONE, KeyCode::Char(c)) => {
                if let Some(pane) = focused_pane {
                    if pane.editable {
                        RoutedEvent::ToSourceBuffer {
                            buffer_id: pane.buffer_id,
                            action: BufferAction::Insert(c),
                        }
                    } else {
                        RoutedEvent::Blocked("Pane is read-only")
                    }
                } else {
                    RoutedEvent::Unhandled
                }
            }
            (KeyModifiers::NONE, KeyCode::Backspace) => {
                if let Some(pane) = focused_pane {
                    if pane.editable {
                        RoutedEvent::ToSourceBuffer {
                            buffer_id: pane.buffer_id,
                            action: BufferAction::Backspace,
                        }
                    } else {
                        RoutedEvent::Blocked("Pane is read-only")
                    }
                } else {
                    RoutedEvent::Unhandled
                }
            }
            (KeyModifiers::NONE, KeyCode::Delete) => {
                if let Some(pane) = focused_pane {
                    if pane.editable {
                        RoutedEvent::ToSourceBuffer {
                            buffer_id: pane.buffer_id,
                            action: BufferAction::Delete,
                        }
                    } else {
                        RoutedEvent::Blocked("Pane is read-only")
                    }
                } else {
                    RoutedEvent::Unhandled
                }
            }
            (KeyModifiers::NONE, KeyCode::Enter) => {
                if let Some(pane) = focused_pane {
                    if pane.editable {
                        RoutedEvent::ToSourceBuffer {
                            buffer_id: pane.buffer_id,
                            action: BufferAction::NewLine,
                        }
                    } else {
                        RoutedEvent::Blocked("Pane is read-only")
                    }
                } else {
                    RoutedEvent::Unhandled
                }
            }

            // Cursor movement in focused pane
            (KeyModifiers::NONE, KeyCode::Left) => RoutedEvent::PaneCursor(CursorAction::Left),
            (KeyModifiers::NONE, KeyCode::Right) => RoutedEvent::PaneCursor(CursorAction::Right),
            (KeyModifiers::CONTROL, KeyCode::Left) => {
                RoutedEvent::PaneCursor(CursorAction::WordLeft)
            }
            (KeyModifiers::CONTROL, KeyCode::Right) => {
                RoutedEvent::PaneCursor(CursorAction::WordRight)
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
