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
    /// `forwarding` is the configured `terminal.mouse_forwarding` policy.
    pub(crate) fn try_forward_mouse_to_terminal(
        &mut self,
        col: u16,
        row: u16,
        mouse_event: MouseEvent,
        forwarding: crate::config::TerminalMouseForwarding,
    ) -> Option<AnyhowResult<bool>> {
        // Only forward if the focused split is a live terminal.
        if !self.focused_terminal_live() {
            return None;
        }

        // Find terminal buffer at this position.
        let (buffer_id, content_rect) = self.get_terminal_content_area_at_position(col, row)?;

        let forward = match forwarding {
            // Legacy rule: forward every event to any alternate-screen
            // program, whether or not it asked for the mouse.
            crate::config::TerminalMouseForwarding::AltScreen => {
                self.is_terminal_in_alternate_screen(buffer_id)
            }
            // Button/motion events are forwarded only when the inner program
            // actually subscribed to the mouse (DECSET 1000/1002/1003) —
            // writing mouse escape sequences into a program that never
            // enabled reporting just injects garbage into its stdin. Shift is
            // the universal escape hatch (xterm convention): a shifted
            // press/drag is never forwarded, so text can always be
            // drag-selected even under a mouse-hungry program. Wheel events
            // additionally keep the alternate-screen rule: alternate-scroll
            // mode (arrow-key synthesis for pagers like `less`) lives in
            // `forward_mouse_to_terminal` and must keep seeing them.
            crate::config::TerminalMouseForwarding::Requested => {
                let wants_mouse = self.terminal_wants_mouse(buffer_id);
                let is_scroll = matches!(
                    mouse_event.kind,
                    MouseEventKind::ScrollUp
                        | MouseEventKind::ScrollDown
                        | MouseEventKind::ScrollLeft
                        | MouseEventKind::ScrollRight
                );
                let shift = mouse_event
                    .modifiers
                    .contains(crossterm::event::KeyModifiers::SHIFT);
                if is_scroll {
                    wants_mouse || self.is_terminal_in_alternate_screen(buffer_id)
                } else if matches!(mouse_event.kind, MouseEventKind::Moved) {
                    // Buttonless motion belongs only to all-motion tracking
                    // (1003); spamming it at click-only/button-drag programs
                    // is out of spec (and its echo can churn the PTY).
                    self.terminal_wants_mouse_motion(buffer_id)
                } else {
                    wants_mouse && !shift
                }
            }
        };
        if !forward {
            return None;
        }

        // Forward the event.
        Some(self.forward_mouse_to_terminal(col, row, content_rect, mouse_event))
    }

    /// Whether the inner program of `buffer_id`'s terminal enabled any
    /// mouse-reporting mode (DECSET 1000/1002/1003). The mouse belongs to
    /// the program only when it asked for it; otherwise presses stay with
    /// the editor (focus + drag-to-select).
    pub fn terminal_wants_mouse(&self, buffer_id: BufferId) -> bool {
        if let Some(terminal_id) = self.get_terminal_id(buffer_id) {
            if let Some(handle) = self.terminal_manager.get(terminal_id) {
                if let Ok(state) = handle.state.lock() {
                    return state.wants_mouse_events();
                }
            }
        }
        false
    }

    /// Whether the inner program enabled ALL-motion mouse tracking
    /// (DECSET 1003) — the only mode that legitimately receives
    /// buttonless motion reports.
    pub fn terminal_wants_mouse_motion(&self, buffer_id: BufferId) -> bool {
        if let Some(terminal_id) = self.get_terminal_id(buffer_id) {
            if let Some(handle) = self.terminal_manager.get(terminal_id) {
                if let Ok(state) = handle.state.lock() {
                    return state.wants_mouse_motion();
                }
            }
        }
        false
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
        if !self.focused_terminal_live() {
            return None;
        }
        let (buffer_id, content_rect) = self.get_terminal_content_area_at_position(col, row)?;
        // Alternate-screen programs own the mouse; don't shadow their clicks.
        if self.is_terminal_in_alternate_screen(buffer_id) {
            return None;
        }
        let term_col = col.saturating_sub(content_rect.x) as usize;
        let term_row = row.saturating_sub(content_rect.y);

        let terminal_id = self.get_terminal_id(buffer_id)?;
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

    /// Detect a clickable file-path link in the terminal *scrollback* view at
    /// the given screen position.
    ///
    /// The scrollback view is a normal read-only buffer (the synced terminal
    /// history) shown only for the active terminal buffer when not in live
    /// terminal mode. Clicks map through the standard screen→buffer-position
    /// machinery; we then read the buffer line under the cursor and detect a
    /// path link in it.
    ///
    /// Returns the terminal buffer, the detected link, and the terminal's
    /// OSC 7 working directory (for resolving relative paths).
    pub(crate) fn detect_terminal_scrollback_link_at(
        &self,
        col: u16,
        row: u16,
    ) -> Option<(
        BufferId,
        crate::services::terminal::path_link::DetectedLink,
        Option<std::path::PathBuf>,
    )> {
        // Scrollback links exist only when the focused split's terminal is in
        // read-only scrollback (a live terminal shows the grid instead).
        if self.focused_terminal_live() {
            return None;
        }
        let active = self.active_buffer();
        if !self.is_terminal_buffer(active) {
            return None;
        }

        let (split_id, content_rect) =
            self.layout_cache
                .split_areas
                .iter()
                .find_map(|(sid, bid, rect, _, _, _)| {
                    (*bid == active
                        && col >= rect.x
                        && col < rect.x + rect.width
                        && row >= rect.y
                        && row < rect.y + rect.height)
                        .then_some((*sid, *rect))
                })?;

        let state = self.buffers.get(&active)?;
        let gutter_width = state.margins.left_total_width() as u16;
        let cached_mappings = self.layout_cache.view_line_mappings.get(&split_id).cloned();
        let (fallback, compose_width) = self
            .buffers
            .splits()
            .and_then(|(_, vs)| vs.get(&split_id))
            .map(|vs| (vs.viewport.top_byte, vs.compose_width))
            .unwrap_or((0, None));

        // `allow_gutter_click = false`: a click in the gutter isn't on a path.
        let byte_pos = crate::app::click_geometry::screen_to_buffer_position(
            col,
            row,
            content_rect,
            gutter_width,
            &cached_mappings,
            fallback,
            false,
            compose_width,
        )?;

        let pos = crate::model::buffer_position::byte_to_2d(&state.buffer, byte_pos);
        let line_bytes = state.buffer.get_line(pos.line)?;
        let line = String::from_utf8_lossy(&line_bytes);
        let line = line.strip_suffix('\n').unwrap_or(&line);
        // `pos.column` is a byte offset within the line; convert to a char
        // column for the (char-indexed) detector.
        let char_col = line
            .char_indices()
            .take_while(|(b, _)| *b < pos.column)
            .count();

        let link = crate::services::terminal::path_link::detect_link_at(line, char_col)?;
        let cwd = self
            .get_terminal_id(active)
            .and_then(|tid| self.terminal_manager.get(tid))
            .and_then(|h| {
                h.state
                    .lock()
                    .ok()
                    .and_then(|s| s.cwd().map(|p| p.to_path_buf()))
            });

        Some((active, link, cwd))
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

impl super::Editor {
    /// Begin a text-selection drag on a terminal split that was showing the
    /// live PTY grid when the mouse went down (see
    /// `MouseState::terminal_drag_pending` — a bare click only focuses).
    ///
    /// Live terminals have no cursor/selection model of their own, so the
    /// split is dropped into read-only scrollback first — exactly the
    /// Ctrl+Space / scroll-up transition. `sync_terminal_to_buffer` pins the
    /// scrollback viewport to the first byte of the just-appended visible
    /// screen, making the scrollback view pixel-identical to the grid the
    /// user aimed at: grid row r is buffer line `top_line + r` and grid
    /// columns map 1:1 (wrap off, no gutter). That lets both the press
    /// origin and the current drag position resolve to exact byte positions
    /// without waiting for a re-render; the standard text-selection drag
    /// machinery then takes over (Ctrl+C copies through the editor
    /// clipboard as usual; Ctrl+Space resumes the live terminal).
    pub(super) fn begin_terminal_grid_selection(
        &mut self,
        split_id: crate::model::event::LeafId,
        buffer_id: BufferId,
        origin_col: u16,
        origin_row: u16,
        col: u16,
        row: u16,
    ) -> AnyhowResult<()> {
        self.active_window_mut().mouse_state.terminal_drag_pending = None;

        let Some(content_rect) =
            self.drop_terminal_grid_into_selection_scrollback(split_id, buffer_id)
        else {
            return Ok(());
        };

        // Resolve both grid positions to byte positions. Columns are taken
        // as byte offsets into the line (terminal rows are overwhelmingly
        // single-width; `snap_to_char_boundary` keeps multi-byte glyphs
        // safe), and subsequent drag motion refines through the standard
        // width-aware `handle_text_selection_drag` path anyway.
        let anchor =
            self.terminal_grid_byte_at(split_id, buffer_id, content_rect, origin_col, origin_row);
        let head = self.terminal_grid_byte_at(split_id, buffer_id, content_rect, col, row);
        let (Some(anchor), Some(head)) = (anchor, head) else {
            return Ok(());
        };

        if let Some(view_state) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.buffers.splits_mut())
            .and_then(|(_, vs)| vs.get_mut(&split_id))
        {
            let cursor = view_state.cursors.primary_mut();
            cursor.position = head;
            cursor.anchor = Some(anchor);
        }

        // Hand off to the standard drag machinery for subsequent motion.
        let ms = &mut self.active_window_mut().mouse_state;
        ms.dragging_text_selection = true;
        ms.drag_selection_split = Some(split_id);
        ms.drag_selection_anchor = Some(anchor);
        Ok(())
    }

    /// Double-click on the live terminal grid: select the word under the
    /// pointer. Same trick as [`Editor::begin_terminal_grid_selection`] — the
    /// live grid has no selection model, so the split first drops into
    /// implicit scrollback (pixel-identical view), then the standard
    /// word-selection and word-wise drag machinery take over.
    pub(super) fn begin_terminal_grid_word_selection(
        &mut self,
        split_id: crate::model::event::LeafId,
        buffer_id: BufferId,
        col: u16,
        row: u16,
    ) -> AnyhowResult<()> {
        self.active_window_mut().mouse_state.terminal_drag_pending = None;

        let Some(content_rect) =
            self.drop_terminal_grid_into_selection_scrollback(split_id, buffer_id)
        else {
            return Ok(());
        };
        let Some(pos) = self.terminal_grid_byte_at(split_id, buffer_id, content_rect, col, row)
        else {
            return Ok(());
        };

        if let Some(view_state) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.buffers.splits_mut())
            .and_then(|(_, vs)| vs.get_mut(&split_id))
        {
            let cursor = view_state.cursors.primary_mut();
            cursor.position = pos;
            cursor.anchor = None;
        }
        self.handle_action(crate::input::keybindings::Action::SelectWord)?;

        // Mirror `handle_editor_double_click`: arm word-wise drag extension.
        if let Some((sel_start, sel_end)) = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .and_then(|(_, vs)| vs.get(&split_id))
            .map(|vs| {
                let c = vs.cursors.primary();
                (c.selection_start(), c.selection_end())
            })
        {
            let ms = &mut self.active_window_mut().mouse_state;
            ms.dragging_text_selection = true;
            ms.drag_selection_split = Some(split_id);
            ms.drag_selection_anchor = Some(sel_start);
            ms.drag_selection_by_words = true;
            ms.drag_selection_word_end = Some(sel_end);
        }
        Ok(())
    }

    /// Triple-click on the live terminal grid: select the whole line under
    /// the pointer, via the same implicit-scrollback detour.
    pub(super) fn begin_terminal_grid_line_selection(
        &mut self,
        split_id: crate::model::event::LeafId,
        buffer_id: BufferId,
        col: u16,
        row: u16,
    ) -> AnyhowResult<()> {
        self.active_window_mut().mouse_state.terminal_drag_pending = None;

        let Some(content_rect) =
            self.drop_terminal_grid_into_selection_scrollback(split_id, buffer_id)
        else {
            return Ok(());
        };
        let Some(pos) = self.terminal_grid_byte_at(split_id, buffer_id, content_rect, col, row)
        else {
            return Ok(());
        };

        if let Some(view_state) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.buffers.splits_mut())
            .and_then(|(_, vs)| vs.get_mut(&split_id))
        {
            let cursor = view_state.cursors.primary_mut();
            cursor.position = pos;
            cursor.anchor = None;
        }
        self.handle_action(crate::input::keybindings::Action::SelectLine)?;
        Ok(())
    }

    /// Drop a *live* terminal grid split into read-only scrollback for a
    /// selection gesture, pinned pixel-identical to the grid (see
    /// [`Editor::begin_terminal_grid_selection`]), and mark the visit
    /// implicit (drag-initiated) so completing the gesture — copying, or a
    /// bare click abandoning the selection — resumes the live grid
    /// automatically. Returns the split's content rect, or `None` when the
    /// situation changed under the gesture (buffer no longer a terminal,
    /// split already in scrollback, layout gone) and nothing was done.
    fn drop_terminal_grid_into_selection_scrollback(
        &mut self,
        split_id: crate::model::event::LeafId,
        buffer_id: BufferId,
    ) -> Option<Rect> {
        if !self.active_window().is_terminal_buffer(buffer_id)
            || self
                .active_window()
                .split_terminal_scrollback(split_id, buffer_id)
        {
            return None;
        }
        let content_rect = self
            .active_layout()
            .split_areas
            .iter()
            .find(|(sid, bid, _, _, _, _)| *sid == split_id && *bid == buffer_id)
            .map(|(_, _, rect, _, _, _)| *rect)?;

        // Drop into read-only scrollback. The press already focused the
        // split, so the sync pins THIS split's viewport to the grid's row 0.
        self.active_window_mut()
            .set_split_terminal_scrollback(split_id, buffer_id, true);
        self.active_window_mut()
            .set_split_terminal_drag_scrollback(split_id, buffer_id, true);
        self.active_window_mut().sync_terminal_mode_flags();
        self.set_status_message(
            "Terminal mode disabled - read only (Ctrl+Space to resume)".to_string(),
        );
        Some(content_rect)
    }

    /// Resolve a screen position over a terminal scrollback view to a byte
    /// offset: grid row `r` is buffer line `top_line + r` and grid columns
    /// map 1:1 (wrap off, no gutter). Exact for any scroll position, with no
    /// dependency on render-cached view-line mappings.
    pub(super) fn terminal_grid_byte_at(
        &self,
        split_id: crate::model::event::LeafId,
        buffer_id: BufferId,
        content_rect: Rect,
        col: u16,
        row: u16,
    ) -> Option<usize> {
        let win = self.windows.get(&self.active_window)?;
        let (_, view_states) = win.buffers.splits()?;
        let vs = view_states.get(&split_id)?;
        let state = win.buffers.get(&buffer_id)?;
        let (top_line, _) = state.buffer.position_to_line_col(vs.viewport.top_byte);
        let grid_row = row.saturating_sub(content_rect.y) as usize;
        // Account for horizontal scroll (a pinned view starts at 0, but an
        // explicit scrollback view may have been scrolled right).
        let grid_col =
            col.saturating_sub(content_rect.x) as usize + vs.viewport.left_column as usize;
        let pos = state
            .buffer
            .line_col_to_position(top_line + grid_row, grid_col);
        Some(state.buffer.snap_to_char_boundary(pos))
    }
}
