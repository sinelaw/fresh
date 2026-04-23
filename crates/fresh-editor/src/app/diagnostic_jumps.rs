//! Cursor navigation between LSP diagnostics on `Editor`.
//!
//! `jump_to_next_error` and `jump_to_previous_error` walk the active
//! buffer's diagnostic overlays, find the nearest one in each direction,
//! and emit a MoveCursor event. Status messages report the diagnostic at
//! the new cursor position.

use rust_i18n::t;

use crate::model::event::Event;

use super::Editor;

impl Editor {
    /// Jump to next error/diagnostic
    pub(super) fn jump_to_next_error(&mut self) {
        let diagnostic_ns = self.lsp_diagnostic_namespace.clone();
        let cursor_pos = self.active_cursors().primary().position;
        let cursor_id = self.active_cursors().primary_id();
        let cursor = *self.active_cursors().primary();
        let state = self.active_state_mut();

        // Get all diagnostic overlay positions
        let mut diagnostic_positions: Vec<usize> = state
            .overlays
            .all()
            .iter()
            .filter_map(|overlay| {
                // Only consider LSP diagnostics (those in the diagnostic namespace)
                if overlay.namespace.as_ref() == Some(&diagnostic_ns) {
                    Some(overlay.range(&state.marker_list).start)
                } else {
                    None
                }
            })
            .collect();

        if diagnostic_positions.is_empty() {
            self.set_status_message(t!("diagnostics.none").to_string());
            return;
        }

        // Sort positions
        diagnostic_positions.sort_unstable();
        diagnostic_positions.dedup();

        // Find next diagnostic after cursor position
        let next_pos = diagnostic_positions
            .iter()
            .find(|&&pos| pos > cursor_pos)
            .or_else(|| diagnostic_positions.first()) // Wrap around
            .copied();

        if let Some(new_pos) = next_pos {
            let event = Event::MoveCursor {
                cursor_id,
                old_position: cursor.position,
                new_position: new_pos,
                old_anchor: cursor.anchor,
                new_anchor: None,
                old_sticky_column: cursor.sticky_column,
                new_sticky_column: 0,
            };
            self.active_event_log_mut().append(event.clone());
            self.apply_event_to_active_buffer(&event);
            // Diagnostics can be on any line; the viewport must scroll so the
            // user actually sees the error after pressing F8 (#1689).
            self.ensure_active_cursor_visible_for_navigation(true);

            // Show diagnostic message in status bar
            let state = self.active_state();
            if let Some(msg) = state.overlays.all().iter().find_map(|overlay| {
                let range = overlay.range(&state.marker_list);
                if range.start == new_pos && overlay.namespace.as_ref() == Some(&diagnostic_ns) {
                    overlay.message.clone()
                } else {
                    None
                }
            }) {
                self.set_status_message(msg);
            }
        }
    }

    /// Jump to previous error/diagnostic
    pub(super) fn jump_to_previous_error(&mut self) {
        let diagnostic_ns = self.lsp_diagnostic_namespace.clone();
        let cursor_pos = self.active_cursors().primary().position;
        let cursor_id = self.active_cursors().primary_id();
        let cursor = *self.active_cursors().primary();
        let state = self.active_state_mut();

        // Get all diagnostic overlay positions
        let mut diagnostic_positions: Vec<usize> = state
            .overlays
            .all()
            .iter()
            .filter_map(|overlay| {
                // Only consider LSP diagnostics (those in the diagnostic namespace)
                if overlay.namespace.as_ref() == Some(&diagnostic_ns) {
                    Some(overlay.range(&state.marker_list).start)
                } else {
                    None
                }
            })
            .collect();

        if diagnostic_positions.is_empty() {
            self.set_status_message(t!("diagnostics.none").to_string());
            return;
        }

        // Sort positions
        diagnostic_positions.sort_unstable();
        diagnostic_positions.dedup();

        // Find previous diagnostic before cursor position
        let prev_pos = diagnostic_positions
            .iter()
            .rev()
            .find(|&&pos| pos < cursor_pos)
            .or_else(|| diagnostic_positions.last()) // Wrap around
            .copied();

        if let Some(new_pos) = prev_pos {
            let event = Event::MoveCursor {
                cursor_id,
                old_position: cursor.position,
                new_position: new_pos,
                old_anchor: cursor.anchor,
                new_anchor: None,
                old_sticky_column: cursor.sticky_column,
                new_sticky_column: 0,
            };
            self.active_event_log_mut().append(event.clone());
            self.apply_event_to_active_buffer(&event);
            // Diagnostics can be on any line; the viewport must scroll so the
            // user actually sees the error after pressing F8 (#1689).
            self.ensure_active_cursor_visible_for_navigation(true);

            // Show diagnostic message in status bar
            let state = self.active_state();
            if let Some(msg) = state.overlays.all().iter().find_map(|overlay| {
                let range = overlay.range(&state.marker_list);
                if range.start == new_pos && overlay.namespace.as_ref() == Some(&diagnostic_ns) {
                    overlay.message.clone()
                } else {
                    None
                }
            }) {
                self.set_status_message(msg);
            }
        }
    }
}
