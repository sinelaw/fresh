//! The split grid's behaviour.
//!
//! **There is no chrome component here any more.** Every surface a pane has —
//! its dividers, its tab strip with the split controls on it, both scrollbars,
//! its content, and the buffer-group grid a pane can hold inside that content
//! — is a node in the shell's tree (`view::shell::splits`), keyed by the pane
//! it belongs to. What is left in this file is what those nodes dispatch to:
//! the handlers themselves, which take a pane and a cell and do the work.
//!
//! Each of them used to open by asking every recorded rectangle in turn
//! whether it contained the point, because a `LayoutBox` is a rectangle and
//! the pane's identity had to be recovered from it. A node has that identity.
//! What the handlers still read back is geometry that is genuinely a record of
//! the last paint — where a scrollbar's thumb ended up, where the tab renderer
//! put each tab — and the pane's own content rectangle, which is read from the
//! node that defines it.

use crate::app::types::{HoverTarget, TabContextMenu};
use crate::app::BufferId;
use crate::input::keybindings::Action;
use crate::model::event::{CursorId, LeafId, SplitDirection};
use crate::view::ui::tabs::TabHit;
use anyhow::Result as AnyhowResult;
use fresh_i18n::t;

use super::Editor;

impl Editor {
    /// Double-click on a split's content rect: the Splits component's
    /// `chrome:editor` arm (moved from the old post-walk scan).
    pub(super) fn handle_split_double_click(
        &mut self,
        split_id: LeafId,
        buffer_id: BufferId,
        content_rect: ratatui::layout::Rect,
        col: u16,
        row: u16,
    ) -> AnyhowResult<()> {
        // Double-clicked on an editor split. A LIVE terminal grid has
        // no selection model of its own — select the word through the
        // same implicit-scrollback detour a drag uses (the first
        // press of the pair already focused the split; with
        // `mouse_drag_selects` off the grid stays inert). A terminal
        // in read-only scrollback is an ordinary buffer view: fall
        // through so double-click selects the word.
        if self.active_window().is_terminal_buffer(buffer_id)
            && !self
                .active_window()
                .split_terminal_scrollback(split_id, buffer_id)
        {
            if self.config.terminal.mouse_drag_selects {
                return self.begin_terminal_grid_word_selection(split_id, buffer_id, col, row);
            }
            self.active_window_mut().key_context = crate::input::keybindings::KeyContext::Terminal;
            return Ok(());
        }

        self.active_window_mut().key_context = crate::input::keybindings::KeyContext::Normal;

        // Position cursor at click location and select word
        self.handle_editor_double_click(col, row, split_id, buffer_id, content_rect)
    }

    /// Handle double-click in editor content area - selects the word under cursor
    fn handle_editor_double_click(
        &mut self,
        col: u16,
        row: u16,
        split_id: LeafId,
        buffer_id: BufferId,
        content_rect: ratatui::layout::Rect,
    ) -> AnyhowResult<()> {
        use crate::model::event::Event;

        // Fixed panels (toolbars, headers) are inert — no click focus,
        // no selection. Scrollable group panels still accept clicks even
        // when their cursor is hidden.
        if self.active_window().is_non_scrollable_buffer(buffer_id) {
            return Ok(());
        }

        // Focus this split
        self.focus_split(split_id, buffer_id);

        // Get cached view line mappings for this split
        let cached_mappings = self
            .active_layout()
            .view_line_mappings
            .get(&split_id)
            .cloned();

        // Get fallback from SplitViewState viewport
        let leaf_id = split_id;
        let fallback = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&leaf_id)
            .map(|vs| vs.viewport.top_byte())
            .unwrap_or(0);

        // Get compose width for this split
        let compose_width = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&leaf_id)
            .and_then(|vs| vs.compose_width);

        // Pull the bits we need out of the active window separately;
        // the per-step helper methods (`apply_event_to_buffer` etc.)
        // hide the disjoint sub-field borrowing.
        let gutter_width = self
            .active_window()
            .buffers
            .get(&buffer_id)
            .map(|s| s.margins.left_total_width() as u16)
            .unwrap_or(0);

        let Some(target_position) = crate::app::click_geometry::screen_to_buffer_position(
            col,
            row,
            content_rect,
            gutter_width,
            &cached_mappings,
            fallback,
            true, // Allow gutter clicks
            compose_width,
        ) else {
            return Ok(());
        };

        let primary_cursor_id = self
            .active_window()
            .buffers
            .splits()
            .and_then(|(_, vs)| vs.get(&leaf_id))
            .map(|vs| vs.cursors.primary_id())
            .unwrap_or(CursorId(0));
        let event = Event::MoveCursor {
            cursor_id: primary_cursor_id,
            old_position: 0,
            new_position: target_position,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: None,
            new_sticky_column: None,
        };

        if let Some(event_log) = self.active_window_mut().event_logs.get_mut(&buffer_id) {
            event_log.append(event.clone());
        }
        self.active_window_mut()
            .apply_event_to_buffer(buffer_id, leaf_id, &event);

        // Now select the word under cursor
        self.handle_action(Action::SelectWord)?;

        // Set up drag state so subsequent drag events extend selection word-by-word
        if let Some(cursor) = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&leaf_id)
            .map(|vs| vs.cursors.primary())
        {
            // Store both edges of the selected word so we can use the appropriate
            // anchor when dragging forward (use word start) vs backward (use word end).
            let sel_start = cursor.selection_start();
            let sel_end = cursor.selection_end();
            self.active_window_mut().mouse_state.dragging_text_selection = true;
            self.active_window_mut().mouse_state.drag_selection_split = Some(split_id);
            self.active_window_mut().mouse_state.drag_selection_anchor = Some(sel_start);
            self.active_window_mut().mouse_state.drag_selection_by_words = true;
            self.active_window_mut().mouse_state.drag_selection_word_end = Some(sel_end);
        }

        Ok(())
    }

    /// Triple-click on a split's content rect: the Splits component's
    /// `chrome:editor` arm (moved from the old hand-ordered ladder).
    pub(super) fn handle_split_triple_click(
        &mut self,
        split_id: LeafId,
        buffer_id: BufferId,
        content_rect: ratatui::layout::Rect,
        col: u16,
        row: u16,
    ) -> AnyhowResult<()> {
        // Live grid: select the line via the implicit-scrollback
        // detour (see double-click above); scrollback view: ordinary
        // buffer, select the line.
        if self.active_window().is_terminal_buffer(buffer_id)
            && !self
                .active_window()
                .split_terminal_scrollback(split_id, buffer_id)
        {
            if self.config.terminal.mouse_drag_selects {
                return self.begin_terminal_grid_line_selection(split_id, buffer_id, col, row);
            }
            return Ok(());
        }

        self.active_window_mut().key_context = crate::input::keybindings::KeyContext::Normal;

        // Use the same pattern as handle_editor_double_click:
        // first focus and position cursor, then select line
        self.handle_editor_triple_click(col, row, split_id, buffer_id, content_rect)
    }

    /// Handle triple-click in editor content area - selects the entire line under cursor
    fn handle_editor_triple_click(
        &mut self,
        col: u16,
        row: u16,
        split_id: LeafId,
        buffer_id: BufferId,
        content_rect: ratatui::layout::Rect,
    ) -> AnyhowResult<()> {
        use crate::model::event::Event;

        if self.active_window().is_non_scrollable_buffer(buffer_id) {
            return Ok(());
        }

        // Focus this split
        self.focus_split(split_id, buffer_id);

        // Get cached view line mappings for this split
        let cached_mappings = self
            .active_layout()
            .view_line_mappings
            .get(&split_id)
            .cloned();

        let leaf_id = split_id;
        let fallback = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&leaf_id)
            .map(|vs| vs.viewport.top_byte())
            .unwrap_or(0);

        // Get compose width for this split
        let compose_width = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&leaf_id)
            .and_then(|vs| vs.compose_width);

        // Pull the bits we need out of the active window separately;
        // the per-step helper methods (`apply_event_to_buffer` etc.)
        // hide the disjoint sub-field borrowing.
        let gutter_width = self
            .active_window()
            .buffers
            .get(&buffer_id)
            .map(|s| s.margins.left_total_width() as u16)
            .unwrap_or(0);

        let Some(target_position) = crate::app::click_geometry::screen_to_buffer_position(
            col,
            row,
            content_rect,
            gutter_width,
            &cached_mappings,
            fallback,
            true,
            compose_width,
        ) else {
            return Ok(());
        };

        let primary_cursor_id = self
            .active_window()
            .buffers
            .splits()
            .and_then(|(_, vs)| vs.get(&leaf_id))
            .map(|vs| vs.cursors.primary_id())
            .unwrap_or(CursorId(0));
        let event = Event::MoveCursor {
            cursor_id: primary_cursor_id,
            old_position: 0,
            new_position: target_position,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: None,
            new_sticky_column: None,
        };

        if let Some(event_log) = self.active_window_mut().event_logs.get_mut(&buffer_id) {
            event_log.append(event.clone());
        }
        self.active_window_mut()
            .apply_event_to_buffer(buffer_id, leaf_id, &event);

        // Now select the entire line
        self.handle_action(Action::SelectLine)?;

        Ok(())
    }

    pub(crate) fn handle_click_scrollbar(
        &mut self,
        pane: LeafId,
        col: u16,
        row: u16,
    ) -> Option<AnyhowResult<()>> {
        // **Which pane is the node's, and so is where its bar is.** What is
        // still looked up is the *thumb's* extent, which is a read of the
        // scroll state at paint time and is genuinely recorded.
        let (split_id, scrollbar_rect) = (pane, self.pane_vscroll_rect(pane)?);
        let buffer_id = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.pane_buffer(pane))?;
        let is_on_thumb = self.active_layout().split_areas.iter().find_map(
            |(split_id, .., thumb_start, thumb_end)| {
                (*split_id == pane).then(|| {
                    let relative_row = row.saturating_sub(scrollbar_rect.y) as usize;
                    relative_row >= *thumb_start && relative_row < *thumb_end
                })
            },
        )?;

        self.focus_split(split_id, buffer_id);
        // Grabbing the scrollbar of a drag-parked terminal scrollback view is
        // scrollback *reading* — convert the implicit visit to an explicit one
        // (no-op otherwise).
        self.active_window_mut()
            .set_split_terminal_drag_scrollback(split_id, buffer_id, false);
        if is_on_thumb {
            self.active_window_mut().mouse_state.dragging_scrollbar = Some(split_id);
            self.active_window_mut().mouse_state.drag_start_row = Some(row);
            if self.active_window().is_composite_buffer(buffer_id) {
                if let Some(vs) = self
                    .active_window()
                    .composite_view_states
                    .get(&(split_id, buffer_id))
                {
                    self.active_window_mut()
                        .mouse_state
                        .drag_start_composite_scroll_row = Some(vs.scroll_row);
                }
            } else {
                let snap = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(_, vs)| vs)
                    .expect("active window must have a populated split layout")
                    .get(&split_id)
                    .map(|vs| (vs.viewport.top_byte(), vs.viewport.top_view_line_offset()));
                if let Some((top_byte, top_view_line_offset)) = snap {
                    let ms = &mut self.active_window_mut().mouse_state;
                    ms.drag_start_top_byte = Some(top_byte);
                    ms.drag_start_view_line_offset = Some(top_view_line_offset);
                }
            }
        } else {
            self.active_window_mut().mouse_state.dragging_scrollbar = Some(split_id);
            if let Err(e) = self.active_window_mut().handle_scrollbar_jump(
                col,
                row,
                split_id,
                buffer_id,
                scrollbar_rect,
            ) {
                return Some(Err(e));
            }
            // The thumb jumped to the pointer, so the pointer is on the thumb
            // now — and the tree will not say so again until the pointer
            // moves. Written to the tree's field, since that is where this
            // bar's hover comes from and it would otherwise still read
            // `ScrollbarTrack` from the move that preceded the click.
            self.shell_hover = Some(HoverTarget::ScrollbarThumb(split_id));
        }
        Some(Ok(()))
    }

    pub(crate) fn handle_click_horizontal_scrollbar(
        &mut self,
        pane: LeafId,
        col: u16,
        _row: u16,
    ) -> Option<AnyhowResult<()>> {
        // The bar is the tree's and the buffer is the window's; the thumb's
        // extent and the content's width are reads of the scroll state at
        // paint time, so those stay recorded.
        let (split_id, hscrollbar_rect) = (pane, self.pane_hscroll_rect(pane)?);
        let buffer_id = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.pane_buffer(pane))?;
        let (max_content_width, is_on_thumb) = self
            .active_layout()
            .horizontal_scrollbar_areas
            .iter()
            .find_map(|(split_id, _, max_content_width, thumb_start, thumb_end)| {
                (*split_id == pane).then(|| {
                    let relative_col = col.saturating_sub(hscrollbar_rect.x) as usize;
                    let on_thumb = relative_col >= *thumb_start && relative_col < *thumb_end;
                    (*max_content_width, on_thumb)
                })
            })?;

        self.focus_split(split_id, buffer_id);
        self.active_window_mut()
            .mouse_state
            .dragging_horizontal_scrollbar = Some(split_id);
        if is_on_thumb {
            self.active_window_mut().mouse_state.drag_start_hcol = Some(col);
            if let Some(vs) = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(_, vs)| vs)
                .expect("active window must have a populated split layout")
                .get(&split_id)
            {
                self.active_window_mut().mouse_state.drag_start_left_column =
                    Some(vs.viewport.left_column);
            }
        } else {
            self.active_window_mut().mouse_state.drag_start_hcol = None;
            self.active_window_mut().mouse_state.drag_start_left_column = None;
            let relative_col = col.saturating_sub(hscrollbar_rect.x) as f64;
            let track_width = hscrollbar_rect.width as f64;
            let ratio = if track_width > 1.0 {
                (relative_col / (track_width - 1.0)).clamp(0.0, 1.0)
            } else {
                0.0
            };
            if let Some(vs) = self
                .windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_view_states_mut())
                .expect("active window must have a populated split layout")
                .get_mut(&split_id)
            {
                let visible_width = vs.viewport.width as usize;
                let max_scroll = max_content_width.saturating_sub(visible_width);
                let target_col = (ratio * max_scroll as f64).round() as usize;
                vs.viewport.left_column = target_col.min(max_scroll);
                vs.viewport.set_skip_ensure_visible();
            }
        }
        Some(Ok(()))
    }

    /// A left press on a pane's content: place the caret, select the word, or
    /// select the line — or toggle a fold, when the cell is a folded line's
    /// gutter indicator, which is checked first and was checked first when it
    /// lived pre-walk.
    ///
    /// The pane comes from the node. The content rectangle comes from that
    /// same node, read back from the laid-out tree: click-to-byte projects
    /// through the view pipeline and needs the extent. It used to come from
    /// whichever recorded rectangle happened to contain the point.
    pub(crate) fn press_pane_content(
        &mut self,
        pane: LeafId,
        col: u16,
        row: u16,
        clicks: u8,
        modifiers: crossterm::event::KeyModifiers,
    ) -> AnyhowResult<()> {
        // The pane's content is a live terminal that wants the mouse, or a
        // Ctrl+Click on a path it printed. Both are the content's, and both
        // come before placing a caret — the same order they had when they sat
        // between the tree and the legacy walk, back when a box did this.
        if let Some(ev) = self.shell_pointer_event.map(|(ev, _)| ev) {
            if let Some(r) = self.pane_content_takes_pointer(col, row, ev) {
                r?;
                return Ok(());
            }
        }
        if clicks >= 2 {
            if let Some((buffer_id, byte_pos)) = self.fold_toggle_line_at_screen_position(col, row)
            {
                self.active_window_mut()
                    .toggle_fold_at_byte(buffer_id, byte_pos);
                return Ok(());
            }
        }
        let (Some(buffer_id), Some(content_rect)) = (
            self.active_window().pane_buffer(pane),
            self.pane_content_rect(pane),
        ) else {
            return Ok(());
        };
        match clicks {
            1 => self.handle_editor_click(col, row, pane, buffer_id, content_rect, modifiers),
            2 => self.handle_split_double_click(pane, buffer_id, content_rect, col, row),
            _ => self.handle_split_triple_click(pane, buffer_id, content_rect, col, row),
        }
    }

    /// Where the shell laid this pane's content out.
    pub fn pane_content_rect(&self, pane: LeafId) -> Option<ratatui::layout::Rect> {
        self.pane_part_rect(crate::view::shell::splits::content_key(pane))
    }

    /// Where the shell laid this pane's vertical scrollbar, or `None` when the
    /// pane has no bar.
    ///
    /// **`None` is "no bar", which is what a zero-width record meant.**
    /// `pane_interior` places all three parts whether or not the pane has
    /// them and gives the missing ones a width of zero; `rect_of` drops a
    /// zero-size element. So the two spellings of "there is no scrollbar
    /// here" — a zero-width `Rect` in `split_areas` and a `None` from the
    /// tree — say the same thing, and every caller of these already had to
    /// handle it.
    pub fn pane_vscroll_rect(&self, pane: LeafId) -> Option<ratatui::layout::Rect> {
        self.pane_part_rect(crate::view::shell::splits::vscroll_key(pane))
    }

    /// The same, for the horizontal bar.
    pub fn pane_hscroll_rect(&self, pane: LeafId) -> Option<ratatui::layout::Rect> {
        self.pane_part_rect(crate::view::shell::splits::hscroll_key(pane))
    }

    fn pane_part_rect(&self, key: fresh_ui::Key) -> Option<ratatui::layout::Rect> {
        crate::view::shell::rect_of(
            self.shell_ui.as_ref()?,
            &key,
            ratatui::layout::Rect::new(
                0,
                0,
                self.active_chrome().last_frame.width,
                self.active_chrome().last_frame.height,
            ),
        )
    }

    /// The pane whose **content** covers a screen cell, and that content's
    /// rectangle.
    ///
    /// **One implementation, and the rectangle is the tree's.** Four places
    /// scanned `split_areas` for a content rect containing the cell, each
    /// writing the comparison out again and two of them wanting the rectangle
    /// they found on the way — a recorded list standing in for a layout the
    /// shell already owns.
    ///
    /// Deliberately containment and not `Ui::hit_test`: the question here is
    /// "which pane's content covers this cell", which is not "what would a
    /// click hit". A popup over the cell changes the second answer and must
    /// not change the first — the plugin `mouse_move` hook converts screen
    /// coordinates to content coordinates with it, and the LSP hover probe has
    /// its own popup guard.
    pub(crate) fn pane_content_at(
        &self,
        col: u16,
        row: u16,
    ) -> Option<(LeafId, ratatui::layout::Rect)> {
        let leaves = self
            .windows
            .get(&self.active_window)?
            .buffers
            .splits()
            .map(|(mgr, _)| mgr.visible_leaves())?;
        leaves.into_iter().find_map(|(pane, _)| {
            let r = self.pane_content_rect(pane)?;
            crate::app::chrome::in_rect(col, row, r).then_some((pane, r))
        })
    }

    /// The pane whose tab strip covers a screen cell.
    ///
    /// A pane with no strip has none — which is the whole point: the caller
    /// that guessed this row as "the content's, minus one" named the row above
    /// the pane for every such pane.
    pub(crate) fn pane_strip_at(&self, col: u16, row: u16) -> Option<LeafId> {
        let ui = self.shell_ui.as_ref()?;
        let frame = ratatui::layout::Rect::new(
            0,
            0,
            self.active_chrome().last_frame.width,
            self.active_chrome().last_frame.height,
        );
        let leaves = self
            .windows
            .get(&self.active_window)?
            .buffers
            .splits()
            .map(|(mgr, _)| mgr.visible_leaves())?;
        leaves.into_iter().find_map(|(pane, _)| {
            let strip = crate::view::shell::rect_of(
                ui,
                &crate::view::shell::splits::tabs_key(pane),
                frame,
            )?;
            crate::app::chrome::in_rect(col, row, strip).then_some(pane)
        })
    }

    /// Where the pane showing this buffer laid its content out.
    ///
    /// The three callers that wanted this scanned `split_areas` for an entry
    /// whose buffer matched and took the rectangle beside it — a lookup that
    /// answered "which pane" from the painter's record of the last frame when
    /// the split model already knows. The model says which pane, the tree says
    /// where it is.
    ///
    /// A buffer mounted in no visible pane has no rectangle, which is what an
    /// absent entry meant.
    pub fn pane_content_rect_for_buffer(
        &self,
        buffer_id: crate::app::BufferId,
    ) -> Option<ratatui::layout::Rect> {
        let pane = self
            .windows
            .get(&self.active_window)?
            .buffers
            .splits()
            .map(|(mgr, _)| mgr.visible_leaves())?
            .into_iter()
            .find_map(|(pane, bid)| (bid == buffer_id).then_some(pane))?;
        self.pane_content_rect(pane)
    }

    /// The pane a screen cell belongs to, counting its scrollbar column.
    ///
    /// The wider question than [`Self::pane_content_at`], and the one
    /// `Window::split_at_position` answered by scanning `split_areas` for
    /// either of the two rectangles it recorded per pane. Both are nodes.
    pub(crate) fn pane_at(&self, col: u16, row: u16) -> Option<LeafId> {
        if let Some((pane, _)) = self.pane_content_at(col, row) {
            return Some(pane);
        }
        let ui = self.shell_ui.as_ref()?;
        let frame = ratatui::layout::Rect::new(
            0,
            0,
            self.active_chrome().last_frame.width,
            self.active_chrome().last_frame.height,
        );
        let leaves = self
            .windows
            .get(&self.active_window)?
            .buffers
            .splits()
            .map(|(mgr, _)| mgr.visible_leaves())?;
        leaves.into_iter().find_map(|(pane, _)| {
            let bar = crate::view::shell::rect_of(
                ui,
                &crate::view::shell::splits::vscroll_key(pane),
                frame,
            )?;
            crate::app::chrome::in_rect(col, row, bar).then_some(pane)
        })
    }

    /// The terminal a screen cell is over, and the rectangle its grid occupies.
    ///
    /// The terminal's own mouse handling lives on `impl Window`, which cannot
    /// see the tree, so this is asked here and the answer travels down. It was
    /// `Window::get_terminal_content_area_at_position`, a third scan of
    /// `split_areas`.
    pub(crate) fn terminal_pane_at(
        &self,
        col: u16,
        row: u16,
    ) -> Option<(crate::app::BufferId, ratatui::layout::Rect)> {
        let (pane, rect) = self.pane_content_at(col, row)?;
        let win = self.windows.get(&self.active_window)?;
        let buffer_id = win.pane_buffer(pane)?;
        win.is_terminal_buffer(buffer_id)
            .then_some((buffer_id, rect))
    }

    /// Whether the pointer is on a pane's scrollbar thumb or its track.
    ///
    /// The pane comes from the node, **and so does the bar**; the thumb's
    /// extent is the recorded read of the scroll state, which is what makes
    /// this a lookup rather than a calculation.
    pub(crate) fn scrollbar_hover(&self, pane: LeafId, row: u16) -> Option<HoverTarget> {
        let bar = self.pane_vscroll_rect(pane)?;
        let (.., thumb_start, thumb_end) = self
            .active_layout()
            .split_areas
            .iter()
            .find(|(split_id, ..)| *split_id == pane)?;
        let rel = row.saturating_sub(bar.y) as usize;
        Some(match rel >= *thumb_start && rel < *thumb_end {
            true => HoverTarget::ScrollbarThumb(pane),
            false => HoverTarget::ScrollbarTrack(pane, rel as u16),
        })
    }

    /// What the pointer is on within a pane's tab strip.
    ///
    /// The strip is a node; its interior is the tab renderer's layout, whose
    /// per-tab columns come from measuring text and are therefore a genuine
    /// record of the last paint. The two buttons at the right end used to be
    /// answered here as well, ahead of the tabs, because they are drawn over
    /// the same row — they are nodes now, and hover their own way.
    ///
    /// A cell that is only the strip's ground — the bar behind the tabs, the
    /// scroll arrows, the "+" — names nothing, exactly as `chrome:tabs`
    /// declined those and let the point fall through.
    pub(crate) fn tab_strip_hover(&self, pane: LeafId, col: u16, row: u16) -> Option<HoverTarget> {
        match self
            .active_layout()
            .tab_layouts
            .get(&pane)?
            .hit_test(col, row)
        {
            Some(TabHit::CloseButton(target)) => Some(HoverTarget::TabCloseButton(target, pane)),
            Some(TabHit::TabName(target)) => Some(HoverTarget::TabName(target, pane)),
            _ => None,
        }
    }

    /// A right press on a tab raises its context menu; on the strip's ground
    /// it raises none and leaves any open one to the base surface's clear.
    ///
    /// Context menus only make sense for buffer tabs — groups are
    /// plugin-managed — which is what `as_buffer` is asking.
    pub(crate) fn open_tab_context_menu(&mut self, pane: LeafId, col: u16, row: u16) {
        let hit =
            self.active_layout()
                .tab_layouts
                .get(&pane)
                .and_then(|tab_layout| match tab_layout.hit_test(col, row) {
                    Some(TabHit::TabName(target) | TabHit::CloseButton(target)) => {
                        target.as_buffer()
                    }
                    _ => None,
                });
        self.active_window_mut().tab_context_menu =
            hit.map(|buffer_id| TabContextMenu::new(buffer_id, pane, col, row + 1));
    }

    /// The `×` on a pane's strip.
    ///
    /// Closing a split is not undoable, so the press offers "Close split" /
    /// "Cancel" just below the button rather than acting — which needs the
    /// button's own cell, and that is now the node's rectangle instead of the
    /// `(row, start_col)` the painter recorded beside it.
    pub(crate) fn close_split_button(&mut self, pane: LeafId) {
        let Some(btn) = self.split_control_rect(&crate::view::shell::splits::close_key(pane))
        else {
            return;
        };
        // One popup at a time.
        self.active_window_mut().close_context_menus();
        self.active_window_mut().close_split_menu = Some(crate::app::types::CloseSplitMenu::new(
            pane,
            btn.x,
            btn.y + 1,
        ));
    }

    /// The `□` / `⧉` beside it: maximize this pane, or restore it.
    pub(crate) fn maximize_split_button(&mut self, pane: LeafId) {
        // Move focus to the clicked split before maximizing. Otherwise
        // a click on a non-active split's button leaves the active
        // split (now hidden by the maximize) silently capturing
        // keystrokes. Skip when already maximized: the unmaximize
        // click can only land on the maximized split, which is
        // already the active one.
        let already_maximized = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr.is_maximized())
            .unwrap_or(false);
        if !already_maximized {
            if let Some(buffer_id) = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(mgr, _)| mgr)
                .expect("active window must have a populated split layout")
                .buffer_for_split(pane)
            {
                self.focus_split(pane, buffer_id);
            }
        }
        match self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .toggle_maximize_for(pane)
        {
            Ok(maximized) => {
                let msg = if maximized {
                    t!("split.maximized").to_string()
                } else {
                    t!("split.restored").to_string()
                };
                self.set_status_message(msg);
            }
            Err(e) => self.set_status_message(e),
        }
        // Maximize/restore changed every pane's geometry: reflow through
        // the single layout funnel, exactly as the keyboard/command
        // `toggle_maximize_split` does. Without this the mouse path left
        // every visible terminal at its pre-toggle PTY size and scroll-back
        // wrap column.
        // Maximize/restore changed every pane's geometry: reflow through
        // the single layout funnel, exactly as the keyboard/command
        // `toggle_maximize_split` does. Without this the mouse path left
        // every visible terminal at its pre-toggle PTY size and scroll-back
        // wrap column.
        self.relayout();
    }

    /// Where one of a pane's strip buttons is, read off the laid-out tree.
    fn split_control_rect(&self, key: &fresh_ui::Key) -> Option<ratatui::layout::Rect> {
        crate::view::shell::rect_of(
            self.shell_ui.as_ref()?,
            key,
            ratatui::layout::Rect::new(
                0,
                0,
                self.active_chrome().last_frame.width,
                self.active_chrome().last_frame.height,
            ),
        )
    }

    pub(crate) fn handle_click_tab_bar(
        &mut self,
        pane: LeafId,
        col: u16,
        row: u16,
    ) -> Option<AnyhowResult<()>> {
        let hit = self
            .active_layout()
            .tab_layouts
            .get(&pane)
            .and_then(|tab_layout| tab_layout.hit_test(col, row))?;
        let split_id = pane;
        tracing::trace!(?split_id, ?hit, col, row, "handle_click_tab_bar: hit");
        match hit {
            TabHit::CloseButton(target) => {
                match target {
                    crate::view::split::TabTarget::Buffer(buffer_id) => {
                        self.focus_split(split_id, buffer_id);
                        self.close_tab_in_split(buffer_id, split_id);
                    }
                    crate::view::split::TabTarget::Group(group_leaf) => {
                        self.close_buffer_group_by_leaf(group_leaf);
                    }
                }
                Some(Ok(()))
            }
            TabHit::TabName(target) => {
                let direction = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(_, vs)| vs)
                    .expect("active window must have a populated split layout")
                    .get(&split_id)
                    .map(|vs| {
                        let open = &vs.open_buffers;
                        let cur = vs.active_target();
                        let cur_idx = open.iter().position(|t| *t == cur);
                        let new_idx = open.iter().position(|t| *t == target);
                        match (cur_idx, new_idx) {
                            (Some(c), Some(n)) if n > c => 1,
                            (Some(c), Some(n)) if n < c => -1,
                            _ => 0,
                        }
                    })
                    .unwrap_or(0);
                if let Some(area) = self.pane_or_group_content_rect(split_id) {
                    self.active_window_mut().animate_tab_switch(area, direction);
                }
                match target {
                    crate::view::split::TabTarget::Buffer(buffer_id) => {
                        self.focus_split(split_id, buffer_id);
                        self.active_window_mut()
                            .promote_buffer_from_preview(buffer_id);
                        self.active_window_mut().mouse_state.dragging_tab = Some(
                            crate::app::types::TabDragState::new(buffer_id, split_id, (col, row)),
                        );
                    }
                    crate::view::split::TabTarget::Group(group_leaf) => {
                        self.activate_group_tab(split_id, group_leaf);
                    }
                }
                Some(Ok(()))
            }
            // The indicators and the wheel nudge the strip through one shared
            // helper, so a click and a wheel notch move it by the same step
            // and both stop at the last tab.
            TabHit::ScrollLeft => {
                self.active_window_mut().scroll_tab_strip(split_id, -1);
                Some(Ok(()))
            }
            TabHit::ScrollRight => {
                self.active_window_mut().scroll_tab_strip(split_id, 1);
                Some(Ok(()))
            }
            TabHit::NewTabButton => {
                // Open the "+" popup just below the button. Close any tab
                // context menu first so only one popup is visible.
                self.active_window_mut().tab_context_menu = None;
                self.active_window_mut().new_tab_menu =
                    Some(crate::app::types::NewTabMenu::new(split_id, col, row + 1));
                Some(Ok(()))
            }
            TabHit::BarBackground => None,
        }
    }
    /// Execute a "+" new-tab popup menu action.
    pub(super) fn execute_new_tab_menu_action(
        &mut self,
        item: crate::app::types::NewTabMenuItem,
        split_id: LeafId,
    ) -> AnyhowResult<()> {
        use crate::app::types::NewTabMenuItem;
        // Ensure the new buffer/terminal lands in the split whose "+" was
        // clicked: `open_terminal`/`new_buffer` act on the active split, so
        // focus that split first (via the buffer it currently shows).
        if let Some(buffer_id) = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .and_then(|(mgr, _)| mgr.buffer_for_split(split_id))
        {
            self.focus_split(split_id, buffer_id);
        }
        match item {
            NewTabMenuItem::NewTerminal => {
                self.open_terminal();
            }
            NewTabMenuItem::NewFile => {
                self.new_buffer();
            }
        }
        Ok(())
    }

    /// Execute a tab context menu action
    pub(super) fn execute_tab_context_menu_action(
        &mut self,
        item: crate::app::types::TabContextMenuItem,
        buffer_id: BufferId,
        leaf_id: LeafId,
    ) -> AnyhowResult<()> {
        use crate::app::types::TabContextMenuItem;
        match item {
            TabContextMenuItem::Close => {
                self.close_tab_in_split(buffer_id, leaf_id);
            }
            TabContextMenuItem::CloseOthers => {
                self.close_other_tabs_in_split(buffer_id, leaf_id);
            }
            TabContextMenuItem::CloseToRight => {
                self.close_tabs_to_right_in_split(buffer_id, leaf_id);
            }
            TabContextMenuItem::CloseToLeft => {
                self.close_tabs_to_left_in_split(buffer_id, leaf_id);
            }
            TabContextMenuItem::CloseAll => {
                self.close_all_tabs_in_split(leaf_id);
            }
            TabContextMenuItem::CopyRelativePath => {
                self.copy_buffer_path(buffer_id, true);
            }
            TabContextMenuItem::CopyFullPath => {
                self.copy_buffer_path(buffer_id, false);
            }
            TabContextMenuItem::ExtractToNewWorkspace => {
                self.extract_tab_to_new_workspace(buffer_id);
            }
        }

        Ok(())
    }

    /// Execute a close-split confirmation choice. "Cancel" is a no-op (the menu
    /// was already dismissed by the caller); "Close split" runs the actual
    /// close.
    pub(super) fn execute_close_split_menu_action(
        &mut self,
        item: crate::app::types::CloseSplitMenuItem,
        split_id: LeafId,
    ) {
        use crate::app::types::CloseSplitMenuItem;
        match item {
            CloseSplitMenuItem::Cancel => {}
            CloseSplitMenuItem::CloseSplit => self.close_split_confirmed(split_id),
        }
    }

    /// Close a split for real (after the confirmation popup). Mirrors the
    /// keyboard "Close Split" command: close the pane, forget its terminal
    /// scrollback modes, and refocus whichever split becomes active.
    fn close_split_confirmed(&mut self, split_id: LeafId) {
        if let Err(e) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .close_split(split_id)
        {
            self.set_status_message(
                t!("error.cannot_close_split", error = e.to_string()).to_string(),
            );
            return;
        }
        // Drop the closed split from every terminal's scrollback set.
        self.active_window_mut()
            .forget_split_terminal_modes(split_id);
        let new_active = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();
        if let Some(buffer_id) = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .buffer_for_split(new_active)
        {
            self.set_active_buffer(buffer_id);
        }
        // Closing a split gives its space back to the surviving panes — the
        // same reflow `close_active_split` runs. `set_active_buffer` above only
        // resizes terminals when the *newly focused* buffer is one, so the
        // other surviving panes need the funnel.
        self.relayout();
        self.set_status_message(t!("split.closed").to_string());
    }

    /// Vertical scrollbar drag: relative thumb drag or track jump on the
    /// grabbed split. Reached from `UiFact::PaneScrollbarDrag` — the bar
    /// captured the pointer on its press, so the move is its own.
    pub(crate) fn handle_vscrollbar_drag(&mut self, col: u16, row: u16) -> AnyhowResult<()> {
        if let Some(dragging_split_id) = self.active_window_mut().mouse_state.dragging_scrollbar {
            // The bar is where the tree put it, and the buffer is the one the
            // pane is showing — neither is a fact about the last paint. The
            // scan this replaces read both out of `split_areas`, one entry of
            // which is the pane being dragged.
            let bar = self.pane_vscroll_rect(dragging_split_id);
            let buffer = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.pane_buffer(dragging_split_id));
            if let (Some(bar), Some(buffer_id)) = (bar, buffer) {
                // A drag that started on the thumb moves relative to where it
                // was grabbed; one that started on the track jumps.
                match self.active_window().mouse_state.drag_start_row.is_some() {
                    true => self.active_window_mut().handle_scrollbar_drag_relative(
                        row,
                        dragging_split_id,
                        buffer_id,
                        bar,
                    )?,
                    false => self.active_window_mut().handle_scrollbar_jump(
                        col,
                        row,
                        dragging_split_id,
                        buffer_id,
                        bar,
                    )?,
                }
            }
        }
        Ok(())
    }

    /// Horizontal scrollbar drag: relative thumb drag or track jump on the
    /// grabbed split. Reached from `UiFact::PaneScrollbarDrag`, as above.
    pub(crate) fn handle_hscrollbar_drag(&mut self, col: u16, _row: u16) -> AnyhowResult<()> {
        if let Some(dragging_split_id) = self
            .active_window_mut()
            .mouse_state
            .dragging_horizontal_scrollbar
        {
            // The bar is the tree's. What is still snapshotted is the thumb
            // and the content width — reads of the scroll state at paint time
            // — cloned so the loop does not hold an immutable borrow on
            // `self` while it mutates `self.split_view_states`.
            let Some(bar) = self.pane_hscroll_rect(dragging_split_id) else {
                return Ok(());
            };
            let hscrollbar_areas = self.active_layout().horizontal_scrollbar_areas.clone();
            for (split_id, _buffer_id, max_content_width, thumb_start, thumb_end) in
                &hscrollbar_areas
            {
                if *split_id == dragging_split_id {
                    let hscrollbar_rect = &bar;
                    let track_width = hscrollbar_rect.width as f64;
                    if track_width <= 1.0 {
                        break;
                    }

                    if let (Some(drag_start_hcol), Some(drag_start_left_column)) = (
                        self.active_window_mut().mouse_state.drag_start_hcol,
                        self.active_window_mut().mouse_state.drag_start_left_column,
                    ) {
                        // Relative drag from thumb - move proportionally to mouse offset
                        // Use thumb size to compute the correct ratio so thumb tracks with mouse
                        let col_offset = (col as i32) - (drag_start_hcol as i32);
                        if let Some(view_state) = self
                            .windows
                            .get_mut(&self.active_window)
                            .and_then(|w| w.split_view_states_mut())
                            .expect("active window must have a populated split layout")
                            .get_mut(&dragging_split_id)
                        {
                            let visible_width = view_state.viewport.width as usize;
                            let max_scroll = max_content_width.saturating_sub(visible_width);
                            if max_scroll > 0 {
                                let thumb_size = thumb_end.saturating_sub(*thumb_start).max(1);
                                let track_travel = (track_width - thumb_size as f64).max(1.0);
                                let scroll_per_pixel = max_scroll as f64 / track_travel;
                                let scroll_offset =
                                    (col_offset as f64 * scroll_per_pixel).round() as i64;
                                let new_left =
                                    (drag_start_left_column as i64 + scroll_offset).max(0) as usize;
                                view_state.viewport.left_column = new_left.min(max_scroll);
                                view_state.viewport.set_skip_ensure_visible();
                            }
                        }
                    } else {
                        // Jump drag (started from track) - jump to absolute position
                        let relative_col = col.saturating_sub(hscrollbar_rect.x) as f64;
                        let ratio = (relative_col / (track_width - 1.0)).clamp(0.0, 1.0);

                        if let Some(view_state) = self
                            .windows
                            .get_mut(&self.active_window)
                            .and_then(|w| w.split_view_states_mut())
                            .expect("active window must have a populated split layout")
                            .get_mut(&dragging_split_id)
                        {
                            let visible_width = view_state.viewport.width as usize;
                            let max_scroll = max_content_width.saturating_sub(visible_width);
                            let target_col = (ratio * max_scroll as f64).round() as usize;
                            view_state.viewport.left_column = target_col.min(max_scroll);
                            view_state.viewport.set_skip_ensure_visible();
                        }
                    }

                    return Ok(());
                }
            }
        }
        Ok(())
    }

    /// Handle text selection drag - extends selection from anchor to current position
    pub(crate) fn handle_text_selection_drag(&mut self, col: u16, row: u16) -> AnyhowResult<()> {
        use crate::model::event::Event;
        use crate::primitives::word_navigation::{find_word_end, find_word_start};

        let Some(split_id) = self.active_window_mut().mouse_state.drag_selection_split else {
            return Ok(());
        };
        let Some(anchor_position) = self.active_window_mut().mouse_state.drag_selection_anchor
        else {
            return Ok(());
        };

        // Where the pane is, and which buffer it shows. The scan this replaces
        // read both from the painter's record of the last frame.
        let Some(content_rect) = self.pane_content_rect(split_id) else {
            return Ok(());
        };
        let Some(buffer_id) = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.pane_buffer(split_id))
        else {
            return Ok(());
        };

        // Get cached view line mappings for this split
        let cached_mappings = self
            .active_layout()
            .view_line_mappings
            .get(&split_id)
            .cloned();

        let leaf_id = split_id;

        // Get fallback from SplitViewState viewport
        let fallback = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&leaf_id)
            .map(|vs| vs.viewport.top_byte())
            .unwrap_or(0);

        // Get compose width for this split
        let compose_width = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&leaf_id)
            .and_then(|vs| vs.compose_width);

        // Calculate the target position and selection geometry by
        // reading buffer state directly, then dispatch the move via
        // Window helpers.
        let drag_by_words = self.active_window_mut().mouse_state.drag_selection_by_words;
        let drag_word_end = self.active_window_mut().mouse_state.drag_selection_word_end;

        // Terminal scrollback views are unwrapped and gutter-free, so a
        // screen cell maps to a byte exactly (viewport top line + row).
        // Resolve directly instead of through the render-cached view-line
        // mappings: drag events processed after the live-grid→scrollback
        // flip but before the next render would otherwise resolve against
        // mappings cached from a *previous* buffer view of this split and
        // land the selection head far from the pointer.
        let terminal_grid_target = if self.active_window().is_terminal_buffer(buffer_id)
            && self
                .active_window()
                .split_terminal_scrollback(leaf_id, buffer_id)
        {
            self.terminal_grid_byte_at(leaf_id, buffer_id, content_rect, col, row)
        } else {
            None
        };

        let Some((target_position, new_position, anchor_position, new_sticky_column)) = self
            .active_window()
            .buffers
            .get(&buffer_id)
            .and_then(|state| {
                let gutter_width = state.margins.left_total_width() as u16;
                let target_position = match terminal_grid_target {
                    Some(pos) => pos,
                    None => {
                        let target =
                            crate::app::click_geometry::screen_to_buffer_position_with_overshoot(
                                col,
                                row,
                                content_rect,
                                gutter_width,
                                &cached_mappings,
                                fallback,
                                true, // Allow gutter clicks for drag selection
                                compose_width,
                            )?;
                        // Pointer outside the text area: the row→line lookup
                        // can only name lines that are on screen, so it clamps
                        // to the first/last visible one and the selection head
                        // stops dead at the viewport edge. Carry the rows past
                        // the edge into lines past the edge, so the head keeps
                        // moving and the viewport follows it (issue #3006).
                        //
                        // Without this the drag only ever scrolls as a side
                        // effect of the scroll-off margin, so a configured
                        // `scroll_offset = 0` means dragging past the edge
                        // does nothing at all.
                        let rows_past_edge =
                            target.row_overshoot as isize - target.row_undershoot as isize;
                        crate::app::click_geometry::position_offset_by_lines(
                            &state.buffer,
                            target.position,
                            rows_past_edge,
                        )
                    }
                };
                let (new_position, anchor_pos) = if drag_by_words {
                    if target_position >= anchor_position {
                        (
                            find_word_end(&state.buffer, target_position),
                            anchor_position,
                        )
                    } else {
                        let word_end = drag_word_end.unwrap_or(anchor_position);
                        (find_word_start(&state.buffer, target_position), word_end)
                    }
                } else {
                    (target_position, anchor_position)
                };
                // Visual column, not byte column — see `visual_column_of`.
                let new_sticky_column =
                    crate::primitives::display_width::visual_column_of(&state.buffer, new_position);
                Some((target_position, new_position, anchor_pos, new_sticky_column))
            })
        else {
            return Ok(());
        };
        let _ = target_position;

        let (primary_cursor_id, old_position, old_anchor, old_sticky_column) = self
            .active_window()
            .buffers
            .splits()
            .and_then(|(_, vs)| vs.get(&leaf_id))
            .map(|vs| {
                let cursor = vs.cursors.primary();
                (
                    vs.cursors.primary_id(),
                    cursor.position,
                    cursor.anchor,
                    cursor.sticky_column,
                )
            })
            .unwrap_or((CursorId(0), 0, None, None));

        let event = Event::MoveCursor {
            cursor_id: primary_cursor_id,
            old_position,
            new_position,
            old_anchor,
            new_anchor: Some(anchor_position),
            old_sticky_column,
            new_sticky_column: new_sticky_column.or(old_sticky_column),
        };

        if let Some(event_log) = self.active_window_mut().event_logs.get_mut(&buffer_id) {
            event_log.append(event.clone());
        }
        // A drag is cursor motion, so it owns vertical placement from here on
        // — exactly like a key press, which clears this same flag in
        // `handle_key`. A wheel or scrollbar scroll sets `skip_ensure_visible`
        // so the render pass won't yank the viewport back to the cursor, and
        // nothing on the mouse path used to clear it again: after any scroll
        // by wheel or scrollbar, a drag-select moved the selection head but
        // the viewport stayed frozen, in *both* directions (issue #3006).
        if let Some(view_state) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .and_then(|states| states.get_mut(&leaf_id))
        {
            view_state.viewport.clear_skip_ensure_visible();
        }
        self.active_window_mut()
            .apply_event_to_buffer(buffer_id, leaf_id, &event);

        Ok(())
    }

    /// Handle separator drag for split resizing
    pub(crate) fn handle_separator_drag(
        &mut self,
        col: u16,
        row: u16,
        split_id: crate::model::event::ContainerId,
        direction: crate::model::event::SplitDirection,
    ) -> AnyhowResult<()> {
        let Some((start_col, start_row)) = self.active_window_mut().mouse_state.drag_start_position
        else {
            return Ok(());
        };
        let Some(start_ratio) = self.active_window_mut().mouse_state.drag_start_ratio else {
            return Ok(());
        };
        let Some(editor_area) = self.active_layout().last_editor_content_area else {
            return Ok(());
        };

        // Calculate the delta in screen space
        let (delta, total_size) = match direction {
            SplitDirection::Horizontal => {
                // For horizontal splits, we move the separator up/down (row changes)
                let delta = row as i32 - start_row as i32;
                let total = editor_area.height as i32;
                (delta, total)
            }
            SplitDirection::Vertical => {
                // For vertical splits, we move the separator left/right (col changes)
                let delta = col as i32 - start_col as i32;
                let total = editor_area.width as i32;
                (delta, total)
            }
        };

        // Convert screen delta to ratio delta
        // The ratio represents the fraction of space the first split gets
        if total_size > 0 {
            let ratio_delta = delta as f32 / total_size as f32;
            // Store the raw fraction; the absolute minimum-pane-size guard is
            // enforced at layout time, so dragging the separator toward the
            // edge stops when the sibling would drop below the minimum size
            // rather than at a fixed 10%/90%.
            let new_ratio = (start_ratio + ratio_delta).clamp(0.0, 1.0);

            // Update the split ratio. The container may live in the main
            // split tree or inside a stashed Grouped subtree (buffer group
            // panels like the theme editor); try the main tree first and
            // fall back to the grouped subtrees.
            if self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(mgr, _)| mgr)
                .expect("active window must have a populated split layout")
                .get_ratio(split_id.into())
                .is_some()
            {
                // Guarded by the `get_ratio(..).is_some()` check above, so
                // this id resolves to a resizable Split; the bool result is
                // not actionable here (the drag can only target a container).
                let _resized = self
                    .windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_manager_mut())
                    .expect("active window must have a populated split layout")
                    .set_ratio(split_id, new_ratio);
            } else {
                self.set_grouped_split_ratio(split_id, new_ratio);
            }
            // Reflow live as the separator moves so terminals track the
            // split sizes during the drag, not just on release.
            self.relayout();
        }

        Ok(())
    }
}
