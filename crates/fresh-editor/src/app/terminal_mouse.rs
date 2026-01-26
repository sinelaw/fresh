//! Terminal mouse event handling.
//!
//! This module handles forwarding mouse events to the terminal PTY when the terminal
//! is in alternate screen mode (used by programs like vim, less, htop, etc.).
//!
//! When in alternate screen mode, mouse events that fall within the terminal's content
//! area are converted to terminal escape sequences and sent to the PTY, allowing
//! full-screen terminal programs to receive and handle mouse input.

use super::*;
use crate::input::handler::{TerminalMouseButton, TerminalMouseEventKind};
use anyhow::Result as AnyhowResult;
use crossterm::event::{MouseButton, MouseEvent, MouseEventKind};
use ratatui::layout::Rect;

impl Editor {
    /// Check if mouse event should be forwarded to the terminal.
    /// Returns true if the event was forwarded (and handled).
    pub(super) fn try_forward_mouse_to_terminal(
        &mut self,
        col: u16,
        row: u16,
        mouse_event: MouseEvent,
    ) -> Option<AnyhowResult<bool>> {
        // Only forward if in terminal mode
        if !self.terminal_mode {
            return None;
        }

        // Find terminal buffer at this position
        let (buffer_id, content_rect) = self.get_terminal_content_area_at_position(col, row)?;

        // Only forward if terminal is in alternate screen mode
        if !self.is_terminal_in_alternate_screen(buffer_id) {
            return None;
        }

        // Forward the event
        Some(self.forward_mouse_to_terminal(col, row, content_rect, mouse_event))
    }

    /// Get the terminal buffer and its content area if the mouse position is over a terminal buffer.
    /// Returns the buffer ID and content rect if found.
    fn get_terminal_content_area_at_position(
        &self,
        col: u16,
        row: u16,
    ) -> Option<(BufferId, Rect)> {
        for (_, buffer_id, content_rect, _, _, _) in &self.cached_layout.split_areas {
            // Check if position is within content area
            if col >= content_rect.x
                && col < content_rect.x + content_rect.width
                && row >= content_rect.y
                && row < content_rect.y + content_rect.height
            {
                // Check if this is a terminal buffer
                if self.is_terminal_buffer(*buffer_id) {
                    return Some((*buffer_id, *content_rect));
                }
            }
        }
        None
    }

    /// Forward a mouse event to the terminal PTY.
    /// Converts screen coordinates to terminal-relative coordinates and sends the event.
    fn forward_mouse_to_terminal(
        &mut self,
        col: u16,
        row: u16,
        content_rect: Rect,
        mouse_event: MouseEvent,
    ) -> AnyhowResult<bool> {
        // Convert to terminal-relative coordinates (0-based from content area)
        let term_col = col.saturating_sub(content_rect.x);
        let term_row = row.saturating_sub(content_rect.y);

        // Convert crossterm MouseEventKind to our TerminalMouseEventKind
        let kind = match mouse_event.kind {
            MouseEventKind::Down(btn) => TerminalMouseEventKind::Down(convert_button(btn)),
            MouseEventKind::Up(btn) => TerminalMouseEventKind::Up(convert_button(btn)),
            MouseEventKind::Drag(btn) => TerminalMouseEventKind::Drag(convert_button(btn)),
            MouseEventKind::Moved => TerminalMouseEventKind::Moved,
            MouseEventKind::ScrollUp => TerminalMouseEventKind::ScrollUp,
            MouseEventKind::ScrollDown => TerminalMouseEventKind::ScrollDown,
            MouseEventKind::ScrollLeft | MouseEventKind::ScrollRight => {
                // Horizontal scroll not typically supported in terminal mouse protocols
                return Ok(false);
            }
        };

        // Send to terminal
        self.send_terminal_mouse(term_col, term_row, kind, mouse_event.modifiers);

        // Terminal renders itself, so we need to trigger a render
        Ok(true)
    }
}

/// Convert crossterm MouseButton to our TerminalMouseButton
fn convert_button(btn: MouseButton) -> TerminalMouseButton {
    match btn {
        MouseButton::Left => TerminalMouseButton::Left,
        MouseButton::Right => TerminalMouseButton::Right,
        MouseButton::Middle => TerminalMouseButton::Middle,
    }
}
