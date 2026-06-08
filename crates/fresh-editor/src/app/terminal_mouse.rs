//! Terminal mouse event handling.
//!
//! This module handles forwarding mouse events to the terminal PTY when the terminal
//! is in alternate screen mode (used by programs like vim, less, htop, etc.).
//!
//! When in alternate screen mode, mouse events that fall within the terminal's content
//! area are converted to terminal escape sequences and sent to the PTY, allowing
//! full-screen terminal programs to receive and handle mouse input.

use crate::app::window::Window;
use crate::input::handler::{TerminalMouseButton, TerminalMouseEventKind};
use crate::model::event::BufferId;
use anyhow::Result as AnyhowResult;
use crossterm::event::{MouseButton, MouseEvent, MouseEventKind};
use ratatui::layout::Rect;

impl Window {
    /// Check if mouse event should be forwarded to the terminal.
    /// Returns true if the event was forwarded (and handled).
    pub(crate) fn try_forward_mouse_to_terminal(
        &mut self,
        col: u16,
        row: u16,
        mouse_event: MouseEvent,
    ) -> Option<AnyhowResult<bool>> {
        // Only forward if in terminal mode.
        if !self.terminal_mode {
            return None;
        }

        // Find terminal buffer at this position.
        let (buffer_id, content_rect) = self.get_terminal_content_area_at_position(col, row)?;

        // Only forward if terminal is in alternate screen mode.
        if !self.is_terminal_in_alternate_screen(buffer_id) {
            return None;
        }

        // Forward the event.
        Some(self.forward_mouse_to_terminal(col, row, content_rect, mouse_event))
    }

    /// Detect a clickable file-path link in the live terminal grid at the given
    /// screen position.
    ///
    /// Returns the terminal buffer, the content-area-relative grid row, the
    /// detected link (path + optional line/col + column span), and the
    /// terminal's OSC 7 working directory (for resolving relative paths).
    ///
    /// Only fires in live terminal mode and *not* in alternate-screen mode
    /// (where mouse events are forwarded to the running full-screen program).
    /// The returned link is textual only — the caller resolves and checks it.
    pub(crate) fn detect_terminal_link_at(
        &self,
        col: u16,
        row: u16,
    ) -> Option<(
        BufferId,
        u16,
        crate::services::terminal::path_link::DetectedLink,
        Option<std::path::PathBuf>,
    )> {
        if !self.terminal_mode {
            return None;
        }
        let (buffer_id, content_rect) = self.get_terminal_content_area_at_position(col, row)?;
        // Alternate-screen programs own the mouse; don't shadow their clicks.
        if self.is_terminal_in_alternate_screen(buffer_id) {
            return None;
        }
        let term_col = col.saturating_sub(content_rect.x) as usize;
        let term_row = row.saturating_sub(content_rect.y);

        let &terminal_id = self.terminal_buffers.get(&buffer_id)?;
        let handle = self.terminal_manager.get(terminal_id)?;
        let (line, cwd) = {
            let state = handle.state.lock().ok()?;
            let line: String = state.get_line(term_row).iter().map(|c| c.c).collect();
            let cwd = state.cwd().map(|p| p.to_path_buf());
            (line, cwd)
        };

        let link = crate::services::terminal::path_link::detect_link_at(&line, term_col)?;
        Some((buffer_id, term_row, link, cwd))
    }

    /// Get the terminal buffer and its content area if the mouse position is over a terminal buffer.
    /// Returns the buffer ID and content rect if found.
    fn get_terminal_content_area_at_position(
        &self,
        col: u16,
        row: u16,
    ) -> Option<(BufferId, Rect)> {
        for (_, buffer_id, content_rect, _, _, _) in &self.layout_cache.split_areas {
            // Check if position is within content area.
            if col >= content_rect.x
                && col < content_rect.x + content_rect.width
                && row >= content_rect.y
                && row < content_rect.y + content_rect.height
                && self.is_terminal_buffer(*buffer_id)
            {
                return Some((*buffer_id, *content_rect));
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
        // Convert to terminal-relative coordinates (0-based from content area).
        let term_col = col.saturating_sub(content_rect.x);
        let term_row = row.saturating_sub(content_rect.y);

        // Convert crossterm MouseEventKind to our TerminalMouseEventKind.
        let kind = match mouse_event.kind {
            MouseEventKind::Down(btn) => TerminalMouseEventKind::Down(convert_button(btn)),
            MouseEventKind::Up(btn) => TerminalMouseEventKind::Up(convert_button(btn)),
            MouseEventKind::Drag(btn) => TerminalMouseEventKind::Drag(convert_button(btn)),
            MouseEventKind::Moved => TerminalMouseEventKind::Moved,
            MouseEventKind::ScrollUp => TerminalMouseEventKind::ScrollUp,
            MouseEventKind::ScrollDown => TerminalMouseEventKind::ScrollDown,
            MouseEventKind::ScrollLeft | MouseEventKind::ScrollRight => {
                // Horizontal scroll not typically supported in terminal mouse protocols.
                return Ok(false);
            }
        };

        // Send to terminal.
        self.send_terminal_mouse(term_col, term_row, kind, mouse_event.modifiers);

        // Terminal renders itself, so we need to trigger a render.
        Ok(true)
    }
}

/// Convert crossterm MouseButton to our TerminalMouseButton.
fn convert_button(btn: MouseButton) -> TerminalMouseButton {
    match btn {
        MouseButton::Left => TerminalMouseButton::Left,
        MouseButton::Right => TerminalMouseButton::Right,
        MouseButton::Middle => TerminalMouseButton::Middle,
    }
}
