//! The split grid: widget-panel content surfaces, separators,
//! close/maximize buttons, tab bars, v/h scrollbars, and the editor
//! content rects.

use crate::app::types::HoverTarget;
use crate::app::BufferId;
use crate::input::keybindings::Action;
use crate::model::event::{CursorId, LeafId, SplitDirection};
use crate::view::ui::tabs::TabHit;
use crate::widgets::LayoutBox;
use anyhow::Result as AnyhowResult;
use rust_i18n::t;

use super::{in_rect, ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct Splits;

impl ChromeComponent for Splits {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        for (_, buffer_id, content_rect, ..) in &ed.active_layout().split_areas {
            if !ed.widget_registry.panels_for_buffer(*buffer_id).is_empty() {
                t.rect("chrome:split_widget_panel", 120, *content_rect);
            }
        }
        for (_, direction, sep_x, sep_y, sep_len) in &ed.active_layout().separator_areas {
            let (w, h) = match direction {
                SplitDirection::Horizontal => (*sep_len as u32, 1),
                SplitDirection::Vertical => (1, *sep_len as u32),
            };
            let mut b = LayoutBox::plain(
                "chrome:split_separators",
                *sep_y as u32,
                *sep_x as u32,
                w,
                h,
            );
            b.z = 80;
            t.push(b);
        }
        for (_, btn_row, start, end) in &ed.active_layout().close_split_areas {
            let mut b = LayoutBox::plain(
                "chrome:split_buttons",
                *btn_row as u32,
                *start as u32,
                end.saturating_sub(*start) as u32,
                1,
            );
            b.z = 70;
            t.push(b);
        }
        for (_, btn_row, start, end) in &ed.active_layout().maximize_split_areas {
            let mut b = LayoutBox::plain(
                "chrome:split_buttons",
                *btn_row as u32,
                *start as u32,
                end.saturating_sub(*start) as u32,
                1,
            );
            b.z = 70;
            t.push(b);
        }
        for (_, tl) in &ed.active_layout().tab_layouts {
            t.rect("chrome:tabs", 60, tl.bar_area);
        }
        for (_, _, _, scrollbar_rect, _, _) in &ed.active_layout().split_areas {
            t.rect("chrome:scrollbars", 50, *scrollbar_rect);
        }
        for (_, _, r, _, _, _) in &ed.active_layout().horizontal_scrollbar_areas {
            t.rect("chrome:h_scrollbar", 50, *r);
        }
        for (_, _, content_rect, ..) in &ed.active_layout().split_areas {
            t.rect("chrome:editor", 10, *content_rect);
        }
        // Right-click-only act-then-continue guard at the very top
        // band: a right-click ANYWHERE clears the "+" new-tab menu and
        // the close-split confirmation before routing — even when a
        // higher surface then consumes the click (the old pre-walk
        // clear's semantics, kept exactly).
        t.full("chrome:tab_menu_clear_guard", 200);
    }

    fn hover(&self, ed: &mut Editor, bx: &LayoutBox, col: u16, row: u16) -> Option<HoverTarget> {
        match bx.kind {
            "chrome:split_separators" => {
                for (split_id, direction, sep_x, sep_y, sep_length) in
                    &ed.active_layout().separator_areas
                {
                    let is_on_separator = match direction {
                        SplitDirection::Horizontal => {
                            row == *sep_y && col >= *sep_x && col < sep_x + sep_length
                        }
                        SplitDirection::Vertical => {
                            col == *sep_x && row >= *sep_y && row < sep_y + sep_length
                        }
                    };
                    if is_on_separator {
                        return Some(HoverTarget::SplitSeparator(*split_id, *direction));
                    }
                }
                None
            }
            "chrome:split_buttons" => {
                // Split control buttons sit on top of the tab row.
                for (split_id, btn_row, start_col, end_col) in &ed.active_layout().close_split_areas
                {
                    if row == *btn_row && col >= *start_col && col < *end_col {
                        return Some(HoverTarget::CloseSplitButton(*split_id));
                    }
                }
                for (split_id, btn_row, start_col, end_col) in
                    &ed.active_layout().maximize_split_areas
                {
                    if row == *btn_row && col >= *start_col && col < *end_col {
                        return Some(HoverTarget::MaximizeSplitButton(*split_id));
                    }
                }
                None
            }
            "chrome:tabs" => {
                for (split_id, tab_layout) in &ed.active_layout().tab_layouts {
                    match tab_layout.hit_test(col, row) {
                        Some(TabHit::CloseButton(target)) => {
                            return Some(HoverTarget::TabCloseButton(target, *split_id));
                        }
                        Some(TabHit::TabName(target)) => {
                            return Some(HoverTarget::TabName(target, *split_id));
                        }
                        Some(TabHit::ScrollLeft)
                        | Some(TabHit::ScrollRight)
                        | Some(TabHit::BarBackground)
                        | Some(TabHit::NewTabButton)
                        | None => {}
                    }
                }
                None
            }
            "chrome:scrollbars" => {
                for (split_id, _buffer_id, _content_rect, scrollbar_rect, thumb_start, thumb_end) in
                    &ed.active_layout().split_areas
                {
                    if in_rect(col, row, *scrollbar_rect) {
                        let relative_row = row.saturating_sub(scrollbar_rect.y) as usize;
                        let is_on_thumb = relative_row >= *thumb_start && relative_row < *thumb_end;
                        if is_on_thumb {
                            return Some(HoverTarget::ScrollbarThumb(*split_id));
                        } else {
                            return Some(HoverTarget::ScrollbarTrack(
                                *split_id,
                                relative_row as u16,
                            ));
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    fn on_pointer(
        &self,
        ed: &mut Editor,
        bx: &LayoutBox,
        ev: &super::ChromePointer,
    ) -> anyhow::Result<super::Disposition> {
        use super::{Disposition, PointerPress};
        match ev.press {
            PointerPress::Left => {}
            // A right-click anywhere dismisses the left-click-only
            // popups (the "+" new-tab menu and the close-split
            // confirmation) and keeps routing — the pre-walk clear
            // expressed as a top-band act-then-continue guard.
            PointerPress::Right => {
                if bx.kind == "chrome:tab_menu_clear_guard" {
                    ed.active_window_mut().new_tab_menu = None;
                    ed.active_window_mut().close_split_menu = None;
                    return Ok(Disposition::PassAfter);
                }
                return Ok(Disposition::Pass);
            }
            // Double = word select, triple = line select, on the split
            // under the pointer (moved from the old post-walk scan /
            // hand-ordered ladder — a popup's opaque box above this
            // band now blocks them by construction).
            PointerPress::Double | PointerPress::Triple => {
                if bx.kind != "chrome:editor" {
                    return Ok(Disposition::Pass);
                }
                // A double/triple press on a folded line's gutter
                // indicator toggles the fold instead of selecting.
                // Checked before word/line select (its historical
                // pre-walk position), and INSIDE the walk since the
                // move from `handle_mouse`'s pre-band: a popup's
                // opaque box or the overlay prompt's double-click
                // swallow above this band now blocks it by
                // construction, like its select siblings.
                if let Some((buffer_id, byte_pos)) =
                    ed.fold_toggle_line_at_screen_position(ev.col, ev.row)
                {
                    ed.active_window_mut()
                        .toggle_fold_at_byte(buffer_id, byte_pos);
                    return Ok(Disposition::Consumed);
                }
                let areas: Vec<_> = ed
                    .active_layout()
                    .split_areas
                    .iter()
                    .map(|(split_id, buffer_id, content_rect, _, _, _)| {
                        (*split_id, *buffer_id, *content_rect)
                    })
                    .collect();
                for (split_id, buffer_id, content_rect) in areas {
                    if in_rect(ev.col, ev.row, content_rect) {
                        if ev.press == PointerPress::Double {
                            ed.handle_split_double_click(
                                split_id,
                                buffer_id,
                                content_rect,
                                ev.col,
                                ev.row,
                            )?;
                        } else {
                            ed.handle_split_triple_click(
                                split_id,
                                buffer_id,
                                content_rect,
                                ev.col,
                                ev.row,
                            )?;
                        }
                        return Ok(Disposition::Consumed);
                    }
                }
                return Ok(Disposition::Pass);
            }
        }
        let consumed = match bx.kind {
            "chrome:scrollbars" => ed.handle_click_scrollbar(ev.col, ev.row),
            "chrome:h_scrollbar" => ed.handle_click_horizontal_scrollbar(ev.col, ev.row),
            "chrome:split_separators" => ed.handle_click_split_separator(ev.col, ev.row),
            "chrome:split_buttons" => ed.handle_click_split_controls(ev.col, ev.row),
            "chrome:tabs" => ed.handle_click_tab_bar(ev.col, ev.row),
            "chrome:editor" => {
                let areas: Vec<_> = ed
                    .active_layout()
                    .split_areas
                    .iter()
                    .map(|(split_id, buffer_id, content_rect, _, _, _)| {
                        (*split_id, *buffer_id, *content_rect)
                    })
                    .collect();
                for (split_id, buffer_id, content_rect) in areas {
                    if in_rect(ev.col, ev.row, content_rect) {
                        ed.handle_editor_click(
                            ev.col,
                            ev.row,
                            split_id,
                            buffer_id,
                            content_rect,
                            ev.modifiers,
                        )?;
                        return Ok(Disposition::Consumed);
                    }
                }
                None
            }
            _ => None,
        };
        if let Some(r) = consumed {
            r?;
            return Ok(Disposition::Consumed);
        }
        Ok(Disposition::Pass)
    }

    fn on_wheel(
        &self,
        ed: &mut Editor,
        bx: &LayoutBox,
        col: u16,
        row: u16,
        delta: i32,
    ) -> anyhow::Result<super::Disposition> {
        use super::Disposition;
        match bx.kind {
            "chrome:split_widget_panel" => {
                if ed.handle_split_widget_panel_wheel(col, row, delta) {
                    Ok(Disposition::Consumed)
                } else {
                    Ok(Disposition::Pass)
                }
            }
            // A vertical wheel over a horizontal tab strip pans it: up
            // walks toward the first tab, down toward the last.
            "chrome:tabs" => {
                let Some(split_id) = ed.active_window().tab_bar_split_at(col, row) else {
                    return Ok(Disposition::Pass);
                };
                ed.dismiss_transient_popups();
                ed.active_window().wheel_plugin_hook(col, row, delta);
                ed.active_window_mut().scroll_tab_strip(split_id, delta);
                Ok(Disposition::Consumed)
            }
            // A split pane, hit in its content rect or scrollbar
            // gutter (moved from the old central `wheel_surface_at`
            // fork — the surface's wheel lives with the surface).
            "chrome:editor" | "chrome:scrollbars" | "chrome:h_scrollbar" => {
                let Some((split_id, buffer_id)) = ed.active_window().split_at_position(col, row)
                else {
                    return Ok(Disposition::Pass);
                };
                // Only a wheel over a pane changes that terminal's
                // live/scrollback state; panning the tab strip or the
                // explorer leaves a live terminal streaming.
                if ed.active_window().focused_terminal_live() {
                    ed.enter_terminal_scrollback();
                } else {
                    ed.active_window_mut()
                        .set_split_terminal_drag_scrollback(split_id, buffer_id, false);
                }
                ed.dismiss_transient_popups();
                ed.active_window().wheel_plugin_hook(col, row, delta);
                ed.active_window_mut()
                    .scroll_split_surface(split_id, buffer_id, delta);
                Ok(Disposition::Consumed)
            }
            _ => Ok(Disposition::Pass),
        }
    }

    fn on_hwheel(
        &self,
        ed: &mut Editor,
        bx: &LayoutBox,
        col: u16,
        row: u16,
        delta: i32,
    ) -> anyhow::Result<super::Disposition> {
        use super::Disposition;
        match bx.kind {
            // A horizontal wheel over the tab strip pans it the same
            // way the vertical wheel does.
            "chrome:tabs" => {
                let Some(split_id) = ed.active_window().tab_bar_split_at(col, row) else {
                    return Ok(Disposition::Pass);
                };
                ed.active_window_mut().scroll_tab_strip(split_id, delta);
                Ok(Disposition::Consumed)
            }
            "chrome:editor" | "chrome:scrollbars" | "chrome:h_scrollbar" => {
                let Some((split_id, buffer_id)) = ed.active_window().split_at_position(col, row)
                else {
                    return Ok(Disposition::Pass);
                };
                ed.active_window_mut()
                    .pan_split_horizontal(split_id, buffer_id, delta)?;
                Ok(Disposition::Consumed)
            }
            _ => Ok(Disposition::Pass),
        }
    }
}

/// Behavior owned by this component (moved from mouse_input.rs —
/// the handlers its arms dispatch to).
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

    pub(super) fn handle_click_scrollbar(
        &mut self,
        col: u16,
        row: u16,
    ) -> Option<AnyhowResult<()>> {
        let (split_id, buffer_id, scrollbar_rect, is_on_thumb) =
            self.active_layout().split_areas.iter().find_map(
                |(split_id, buffer_id, _content, scrollbar_rect, thumb_start, thumb_end)| {
                    if in_rect(col, row, *scrollbar_rect) {
                        let relative_row = row.saturating_sub(scrollbar_rect.y) as usize;
                        let on_thumb = relative_row >= *thumb_start && relative_row < *thumb_end;
                        Some((*split_id, *buffer_id, *scrollbar_rect, on_thumb))
                    } else {
                        None
                    }
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
            self.active_window_mut().mouse_state.hover_target =
                Some(HoverTarget::ScrollbarThumb(split_id));
        }
        Some(Ok(()))
    }

    pub(super) fn handle_click_horizontal_scrollbar(
        &mut self,
        col: u16,
        row: u16,
    ) -> Option<AnyhowResult<()>> {
        let (split_id, buffer_id, hscrollbar_rect, max_content_width, is_on_thumb) = self
            .active_layout()
            .horizontal_scrollbar_areas
            .iter()
            .find_map(
                |(
                    split_id,
                    buffer_id,
                    hscrollbar_rect,
                    max_content_width,
                    thumb_start,
                    thumb_end,
                )| {
                    if col >= hscrollbar_rect.x
                        && col < hscrollbar_rect.x + hscrollbar_rect.width
                        && row >= hscrollbar_rect.y
                        && row < hscrollbar_rect.y + hscrollbar_rect.height
                    {
                        let relative_col = col.saturating_sub(hscrollbar_rect.x) as usize;
                        let on_thumb = relative_col >= *thumb_start && relative_col < *thumb_end;
                        Some((
                            *split_id,
                            *buffer_id,
                            *hscrollbar_rect,
                            *max_content_width,
                            on_thumb,
                        ))
                    } else {
                        None
                    }
                },
            )?;

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

    pub(super) fn handle_click_split_separator(
        &mut self,
        col: u16,
        row: u16,
    ) -> Option<AnyhowResult<()>> {
        let separator_areas = self.active_layout().separator_areas.clone();
        for (split_id, direction, sep_x, sep_y, sep_length) in &separator_areas {
            let is_on_separator = match direction {
                SplitDirection::Horizontal => {
                    row == *sep_y && col >= *sep_x && col < sep_x + sep_length
                }
                SplitDirection::Vertical => {
                    col == *sep_x && row >= *sep_y && row < sep_y + sep_length
                }
            };
            if is_on_separator {
                self.active_window_mut().mouse_state.dragging_separator =
                    Some((*split_id, *direction));
                self.active_window_mut().mouse_state.drag_start_position = Some((col, row));
                let ratio = self
                    .split_manager_mut()
                    .get_ratio((*split_id).into())
                    .or_else(|| self.grouped_split_ratio(*split_id));
                if let Some(ratio) = ratio {
                    self.active_window_mut().mouse_state.drag_start_ratio = Some(ratio);
                }
                return Some(Ok(()));
            }
        }
        None
    }

    pub(super) fn handle_click_split_controls(
        &mut self,
        col: u16,
        row: u16,
    ) -> Option<AnyhowResult<()>> {
        let close_split_hit = self
            .active_layout()
            .close_split_areas
            .iter()
            .find(|(_, btn_row, start_col, end_col)| {
                row == *btn_row && col >= *start_col && col < *end_col
            })
            .map(|(split_id, btn_row, start_col, _)| (*split_id, *btn_row, *start_col));
        if let Some((split_id, btn_row, start_col)) = close_split_hit {
            // Closing a split isn't undoable, so don't act on the raw click —
            // pop a small confirmation just below the `×` button offering
            // "Close split" / "Cancel". Dismiss any other native menu first so
            // only one popup is visible.
            self.active_window_mut().close_context_menus();
            self.active_window_mut().close_split_menu = Some(
                crate::app::types::CloseSplitMenu::new(split_id, start_col, btn_row + 1),
            );
            return Some(Ok(()));
        }

        let maximize_target = self
            .active_layout()
            .maximize_split_areas
            .iter()
            .find(|(_, btn_row, start_col, end_col)| {
                row == *btn_row && col >= *start_col && col < *end_col
            })
            .map(|(split_id, _, _, _)| *split_id);
        if let Some(target) = maximize_target {
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
                    .buffer_for_split(target)
                {
                    self.focus_split(target, buffer_id);
                }
            }
            match self
                .windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_manager_mut())
                .expect("active window must have a populated split layout")
                .toggle_maximize_for(target)
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
            self.relayout();
            return Some(Ok(()));
        }

        None
    }

    pub(super) fn handle_click_tab_bar(&mut self, col: u16, row: u16) -> Option<AnyhowResult<()>> {
        let tab_hit = self
            .active_layout()
            .tab_layouts
            .iter()
            .find_map(|(split_id, tab_layout)| {
                tab_layout.hit_test(col, row).map(|h| (*split_id, h))
            });
        let (split_id, hit) = tab_hit?;
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
                self.active_window_mut()
                    .animate_tab_switch(split_id, direction);
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
}
