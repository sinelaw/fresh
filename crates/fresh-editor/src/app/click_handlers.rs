//! Click and scroll-position helpers on `Editor`.
//!
//! - `move_cursor_to_visible_area` and `calculate_max_scroll_position`:
//!   small helpers that fix up cursor position after scroll-driven moves
//!   so the user keeps a visible cursor.
//! - `fold_toggle_line_at_screen_position`: maps a click in the gutter to
//!   the byte to fold/unfold (uses the pure helper from
//!   `super::click_geometry`).
//! - `handle_editor_click`: dispatches mouse clicks to gutter / scrollbar
//!   / caret placement. A plain click places the caret and collapses to
//!   one cursor; Shift/Ctrl extend the selection. No click adds a cursor.
//!
//! (`handle_file_explorer_click` lives with its component in
//! `chrome/file_explorer.rs`.)

use anyhow::Result as AnyhowResult;

use crate::input::keybindings::Action;
use crate::model::event::BufferId;
use crate::services::plugins::hooks::HookArgs;

use super::Editor;

impl Editor {
    // `move_cursor_to_visible_area` and `calculate_max_scroll_position`
    // live on `impl Window` — call them via
    // `self.active_window_mut().move_cursor_to_visible_area(...)` and
    // `Window::calculate_max_scroll_position(buffer, viewport_height)`.

    pub(super) fn fold_toggle_line_at_screen_position(
        &self,
        col: u16,
        row: u16,
    ) -> Option<(BufferId, usize)> {
        // Which pane covers the cell, and where its content is — one
        // question the shell answers from the tree, rather than a scan of
        // the painter's record repeating the containment test by hand.
        let Some((split_id, content_rect)) = self.pane_content_at(col, row) else {
            return None;
        };
        let Some(buffer_id) = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.pane_buffer(split_id))
        else {
            return None;
        };
        let (split_id, buffer_id, content_rect) = (&split_id, &buffer_id, &content_rect);
        // Neither a terminal grid nor a composite view has fold gutters.
        if self.active_window().is_terminal_buffer(*buffer_id)
            || self.active_window().is_composite_buffer(*buffer_id)
        {
            return None;
        }

        let (gutter_width, collapsed_header_bytes) = {
            let state = self
                .windows
                .get(&self.active_window)
                .map(|w| &w.buffers)
                .expect("active window present")
                .get(buffer_id)?;
            let headers = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(_, vs)| vs)
                .expect("active window must have a populated split layout")
                .get(split_id)
                .map(|vs| {
                    vs.folds
                        .collapsed_header_bytes(&state.buffer, &state.marker_list)
                })
                .unwrap_or_default();
            (state.margins.left_total_width() as u16, headers)
        };

        let cached_mappings = self
            .active_layout()
            .view_line_mappings
            .get(split_id)
            .cloned();
        let fallback = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(split_id)
            .map(|vs| vs.viewport.top_byte())
            .unwrap_or(0);
        let compose_width = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(split_id)
            .and_then(|vs| vs.compose_width);

        let target_position = super::click_geometry::screen_to_buffer_position(
            col,
            row,
            *content_rect,
            gutter_width,
            &cached_mappings,
            fallback,
            true,
            compose_width,
        )?;

        let adjusted_rect =
            super::click_geometry::adjust_content_rect_for_compose(*content_rect, compose_width);
        let content_col = col.saturating_sub(adjusted_rect.x);
        let state = self
            .windows
            .get(&self.active_window)
            .map(|w| &w.buffers)
            .expect("active window present")
            .get(buffer_id)?;
        let fold_indicators_visible = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(split_id)
            .map(|vs| vs.fold_indicators_visible())
            .unwrap_or(true);
        if let Some(byte_pos) = super::click_geometry::fold_toggle_byte_from_position(
            state,
            &collapsed_header_bytes,
            target_position,
            content_col,
            gutter_width,
            fold_indicators_visible,
        ) {
            return Some((*buffer_id, byte_pos));
        }

        None
    }

    /// Handle click in editor content area
    pub(super) fn handle_editor_click(
        &mut self,
        col: u16,
        row: u16,
        split_id: crate::model::event::LeafId,
        buffer_id: BufferId,
        content_rect: ratatui::layout::Rect,
        modifiers: crossterm::event::KeyModifiers,
    ) -> AnyhowResult<()> {
        use crate::model::event::{CursorId, Event};
        use crossterm::event::KeyModifiers;

        // Build modifiers string for plugins
        let modifiers_str = if modifiers.contains(KeyModifiers::SHIFT) {
            "shift".to_string()
        } else {
            String::new()
        };

        // Compute buffer-local row/col once. Both the widget hit-test
        // and the mouse_click hook need them, and the cost (a single
        // `screen_to_buffer_position` call) is non-trivial — share the
        // result.
        // **A described pane has no screen-to-line projection, and must say
        // so rather than answer.**
        //
        // Every other pointer probe is gated on whether the tree describes the
        // panel — the floating and dock right press, the hover walk, the dock
        // and floating left press. This one was not, and the projection it
        // reads is not merely stale for a described pane: the text pass that
        // fills `view_line_mappings` does not run there, so the map is written
        // *empty* and `screen_to_buffer_position` falls back to the viewport's
        // top byte. Every click in the pane therefore resolved to the same
        // line and column — the first row of the panel — and the widget
        // hit-test below happily matched whatever control sits there. A press
        // that lands on a widget is stopped by the node, so what reached here
        // was exactly the case the projection cannot answer.
        //
        // `None` is what both readers below already expect for "no position",
        // and it is what the `mouse_click` hook's `Option` fields mean. A
        // number that is wrong for every click but one is worse than no
        // number.
        let described_pane = self.pane_panel_is_described(buffer_id);
        let (mc_buffer_row, mc_buffer_col) = if described_pane {
            (None, None)
        } else {
            let cached_mappings = self
                .active_layout()
                .view_line_mappings
                .get(&split_id)
                .cloned();
            let fallback = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(_, vs)| vs)
                .expect("active window must have a populated split layout")
                .get(&split_id)
                .map(|vs| vs.viewport.top_byte())
                .unwrap_or(0);
            let compose_width = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(_, vs)| vs)
                .expect("active window must have a populated split layout")
                .get(&split_id)
                .and_then(|vs| vs.compose_width);
            let gutter_width = self
                .buffers()
                .get(&buffer_id)
                .map(|s| s.margins.left_total_width() as u16)
                .unwrap_or(0);
            let target = super::click_geometry::screen_to_buffer_position(
                col,
                row,
                content_rect,
                gutter_width,
                &cached_mappings,
                fallback,
                true,
                compose_width,
            );
            match target {
                Some(byte_pos) => {
                    let state = self
                        .windows
                        .get(&self.active_window)
                        .map(|w| &w.buffers)
                        .expect("active window present")
                        .get(&buffer_id);
                    if let Some(s) = state {
                        let (line, col_b) = s.buffer.position_to_line_col(byte_pos);
                        (
                            Some(line.min(u32::MAX as usize) as u32),
                            Some(col_b.min(u32::MAX as usize) as u32),
                        )
                    } else {
                        (None, None)
                    }
                }
                None => (None, None),
            }
        };

        // A press on a widget never reaches here: a mounted panel's widgets
        // are nodes in the tree, and a node answers its own press and stops
        // it (`view::shell::widgets`). What arrives is a press the panel's
        // nodes declined, and the buffer's own readers below get it.

        // A line that points somewhere (`editor.setLineTargets`) opens its
        // target on click. Checked before the plugin hook so a declarative
        // index behaves the same whether or not anything is listening — its
        // author is typically a script that has already exited.
        #[cfg(feature = "plugins")]
        if let Some(brow) = mc_buffer_row {
            if let Some(target) = self.line_target_at(buffer_id, brow as usize) {
                self.follow_line_target(target, split_id);
                return Ok(());
            }
        }

        // Dispatch MouseClick hook to plugins
        // Plugins can handle clicks on their virtual buffers
        if self
            .plugin_manager
            .read()
            .unwrap()
            .has_hook_handlers("mouse_click")
        {
            self.plugin_manager.read().unwrap().run_hook(
                "mouse_click",
                HookArgs::MouseClick {
                    column: col,
                    row,
                    button: "left".to_string(),
                    modifiers: modifiers_str,
                    content_x: content_rect.x,
                    content_y: content_rect.y,
                    buffer_id: Some(buffer_id.0 as u64),
                    buffer_row: mc_buffer_row,
                    buffer_col: mc_buffer_col,
                },
            );
        }

        // Fixed buffer-group panels (toolbars/headers/footers) aren't
        // interactive targets: focusing them would let arrow keys move an
        // invisible cursor and scroll the pinned content. Swallow the click
        // after the plugin hook has had a chance to observe it. Scrollable
        // group panels still accept the click (focus routes to them) even
        // when their cursor is hidden.
        //
        // A widget-panel buffer can also be non-scrollable (it owns its own
        // scroll window, e.g. Search & Replace), but it IS an interactive
        // target — its click must still route focus to the split so
        // keyboard nav works afterward.
        if self.active_window().is_non_scrollable_buffer(buffer_id) {
            if self.widget_registry.panels_for_buffer(buffer_id).is_empty() {
                return Ok(());
            }
            // Widget panel: take the focus, then stop. The panel owns every
            // row it draws, and its nodes already answered any click that
            // landed on a control. A click that missed one — a
            // `labeledSection` border, the padding under a short list — must
            // not fall through to cursor placement: the buffer's cursor is
            // hidden, but the viewport still follows it, so the click scrolls
            // the panel's own header and buttons out of view with no way to
            // scroll them back.
            self.focus_split(split_id, buffer_id);
            return Ok(());
        }

        // Focus this split (handles terminal mode exit, tab state, etc.)
        self.focus_split(split_id, buffer_id);

        // Handle composite buffer clicks specially
        if self.active_window().is_composite_buffer(buffer_id) {
            return self.handle_composite_click(col, row, split_id, buffer_id, content_rect);
        }

        // A bare press on a terminal parked in implicit (drag-initiated)
        // scrollback abandons the selection gesture: resume the live grid and
        // fall through to the live-grid branch below, which re-records the
        // press as a potential selection origin — so the pane keeps behaving
        // like the live grid it appears to be (click focuses, click-then-drag
        // starts a fresh selection). Shift/Ctrl-clicks extend the selection
        // instead (the gesture continues), and explicit scrollback visits are
        // never resumed by a click.
        if self.active_window().is_terminal_buffer(buffer_id)
            && self
                .active_window()
                .split_terminal_drag_scrollback(split_id, buffer_id)
            && !modifiers.contains(KeyModifiers::SHIFT)
            && !modifiers.contains(KeyModifiers::CONTROL)
        {
            self.enter_terminal_mode();
        }

        // Live terminal grid: the grid overlay covers a stale buffer view, so
        // positioning a cursor at the click would land in invisible text — and
        // a bare click must keep the terminal live (click-to-focus-and-type).
        // Record the click as a potential selection origin instead: if a drag
        // follows, `begin_terminal_grid_selection` drops this split into
        // read-only scrollback (whose view is pixel-identical to the grid,
        // see `sync_terminal_to_buffer`) and starts a real text selection
        // anchored here.
        if self.active_window().is_terminal_buffer(buffer_id)
            && !self
                .active_window()
                .split_terminal_scrollback(split_id, buffer_id)
        {
            // `terminal.mouse_drag_selects = false` keeps the live grid
            // inert under drags: the click still focuses (above), no
            // selection origin is recorded.
            if self.config.terminal.mouse_drag_selects {
                self.active_window_mut().mouse_state.terminal_drag_pending =
                    Some((split_id, buffer_id, col, row));
            }
            return Ok(());
        }

        // Ensure key context is Normal for non-terminal buffers
        // This handles the edge case where split/buffer don't change but we clicked from FileExplorer
        if !self.active_window().is_terminal_buffer(buffer_id) {
            self.active_window_mut().key_context = crate::input::keybindings::KeyContext::Normal;
        }

        // Get cached view line mappings for this split (before mutable borrow of buffers)
        let cached_mappings = self
            .active_layout()
            .view_line_mappings
            .get(&split_id)
            .cloned();

        // Get fallback from SplitViewState viewport
        let fallback = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&split_id)
            .map(|vs| vs.viewport.top_byte())
            .unwrap_or(0);

        // Get compose width for this split (adjusts content rect for centered layout)
        let compose_width = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&split_id)
            .and_then(|vs| vs.compose_width);

        // Shift-click extends the selection; Ctrl-click too, since some
        // terminals intercept Shift+click.
        let extend_selection =
            modifiers.contains(KeyModifiers::SHIFT) || modifiers.contains(KeyModifiers::CONTROL);

        // Calculate clicked position in buffer
        let (toggle_fold_byte, onclick_action, click_target, cursor_snapshot) = if let Some(state) =
            self.windows
                .get(&self.active_window)
                .map(|w| &w.buffers)
                .expect("active window present")
                .get(&buffer_id)
        {
            let gutter_width = state.margins.left_total_width() as u16;

            let Some(click_target) =
                super::click_geometry::screen_to_buffer_position_with_overshoot(
                    col,
                    row,
                    content_rect,
                    gutter_width,
                    &cached_mappings,
                    fallback,
                    true, // Allow gutter clicks - position cursor at start of line
                    compose_width,
                )
            else {
                return Ok(());
            };
            let target_position = click_target.position;

            // Toggle fold on gutter click if this line is foldable/collapsed
            let adjusted_rect =
                super::click_geometry::adjust_content_rect_for_compose(content_rect, compose_width);
            let content_col = col.saturating_sub(adjusted_rect.x);
            let collapsed_header_bytes = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(_, vs)| vs)
                .expect("active window must have a populated split layout")
                .get(&split_id)
                .map(|vs| {
                    vs.folds
                        .collapsed_header_bytes(&state.buffer, &state.marker_list)
                })
                .unwrap_or_default();
            let fold_indicators_visible = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(_, vs)| vs)
                .expect("active window must have a populated split layout")
                .get(&split_id)
                .map(|vs| vs.fold_indicators_visible())
                .unwrap_or(true);
            let toggle_fold_byte = super::click_geometry::fold_toggle_byte_from_position(
                state,
                &collapsed_header_bytes,
                target_position,
                content_col,
                gutter_width,
                fold_indicators_visible,
            );

            let cursor_snapshot = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(_, vs)| vs)
                .expect("active window must have a populated split layout")
                .get(&split_id)
                .map(|vs| {
                    // A plain click ends multi-cursor editing: every other
                    // cursor goes and the one kept moves to the click — as
                    // in VS Code, Sublime and Zed. Before this the click
                    // moved the primary and kept the rest, which may be
                    // out of the viewport, and the next keystroke edited
                    // every one of them (#3125). Shift/Ctrl-click extends
                    // the primary's selection and leaves the set alone.
                    //
                    // The survivor is the lowest id, as `Esc`
                    // (`RemoveSecondaryCursors`) keeps it, not the primary:
                    // the add-cursor commands allocate the next id from the
                    // cursor count, so a lone survivor with a higher id
                    // would be overwritten by the next `Ctrl+Alt+Down`.
                    let kept_id = if extend_selection {
                        vs.cursors.primary_id()
                    } else {
                        vs.cursors
                            .ids()
                            .into_iter()
                            .min_by_key(|id| id.0)
                            .unwrap_or_else(|| vs.cursors.primary_id())
                    };
                    let cursor = vs
                        .cursors
                        .get(kept_id)
                        .copied()
                        .unwrap_or(*vs.cursors.primary());
                    // Sorted by id so the logged batch, and which cursor an
                    // undo re-adds last, do not depend on hash order.
                    let mut removed: Vec<(CursorId, usize, Option<usize>)> = if extend_selection {
                        Vec::new()
                    } else {
                        vs.cursors
                            .iter()
                            .filter(|(id, _)| *id != kept_id)
                            .map(|(id, c)| (id, c.position, c.anchor))
                            .collect()
                    };
                    removed.sort_by_key(|(id, _, _)| id.0);
                    (
                        kept_id,
                        cursor.position,
                        cursor.anchor,
                        cursor.sticky_column,
                        cursor.deselect_on_move,
                        removed,
                    )
                })
                .unwrap_or((CursorId(0), 0, None, None, true, Vec::new()));

            // Check for onClick text property at this position
            // This enables clickable UI elements in virtual buffers
            let onclick_action = state
                .text_properties
                .get_at(target_position)
                .iter()
                .find_map(|prop| {
                    prop.get("onClick")
                        .and_then(|v| v.as_str())
                        .map(|s| s.to_string())
                });

            (
                toggle_fold_byte,
                onclick_action,
                click_target,
                cursor_snapshot,
            )
        } else {
            return Ok(());
        };
        let mut target_position = click_target.position;
        let click_overshoot = click_target.col_overshoot;

        // Vertical virtual space: a click on the rows below the last line
        // (with the buffer's end on screen — the last cached row must be the
        // buffer's last display line) parks the cursor on a virtual line at
        // the clicked column, regardless of the last line's width. Only when
        // virtual space is fully on and the click doesn't extend a selection.
        let virtual_lines_below = self
            .buffers()
            .get(&buffer_id)
            .filter(|state| {
                click_target.row_overshoot > 0
                    && !extend_selection
                    && state.buffer_settings.virtual_space.cursor_beyond_eol()
                    && cached_mappings
                        .as_ref()
                        .and_then(|m| m.last())
                        .is_some_and(|m| m.line_end_byte == state.buffer.len())
            })
            .map(|_| click_target.row_overshoot)
            .unwrap_or(0);
        if virtual_lines_below > 0 {
            // The virtual line hangs off the end of the buffer; the byte
            // position resolved against the last real row (possibly
            // mid-line) doesn't apply.
            target_position = self
                .buffers()
                .get(&buffer_id)
                .map(|state| state.buffer.len())
                .unwrap_or(target_position);
        }

        if toggle_fold_byte.is_some() {
            self.active_window_mut()
                .toggle_fold_at_byte(buffer_id, target_position);
            return Ok(());
        }

        let (
            kept_cursor_id,
            old_position,
            old_anchor,
            old_sticky_column,
            deselect_on_move,
            removed_cursors,
        ) = cursor_snapshot;

        if let Some(action_name) = onclick_action {
            // Execute the action associated with this clickable element
            tracing::debug!(
                "onClick triggered at position {}: action={}",
                target_position,
                action_name
            );
            let empty_args = std::collections::HashMap::new();
            if let Some(action) = Action::from_str(&action_name, &empty_args) {
                return self.handle_action(action);
            }
            return Ok(());
        }

        // Move cursor to clicked position (respect shift for selection)
        let new_anchor = if extend_selection {
            Some(old_anchor.unwrap_or(old_position))
        } else if deselect_on_move {
            None
        } else {
            old_anchor
        };

        // The goal column for later vertical movement is a *visual* column
        // (wide-char aware), not the byte column `offset_to_position` returns.
        // With virtual space on, a click past the end of a line keeps the
        // clicked column: the byte position clips to the content end and the
        // sticky column carries the overshoot. On a virtual line below the
        // buffer end the line is empty, so the clicked column is measured
        // from the line start (viewport column + horizontal scroll).
        let left_col = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&split_id)
            .map(|vs| vs.viewport.left_column)
            .unwrap_or(0);
        let new_sticky_column = self.buffers().get(&buffer_id).and_then(|state| {
            if virtual_lines_below > 0 {
                return Some(left_col + click_target.text_col);
            }
            if click_overshoot > 0
                && !extend_selection
                && state.buffer_settings.virtual_space.cursor_beyond_eol()
            {
                if let Some(width) = crate::model::virtual_space::line_width_at_content_end(
                    &state.buffer,
                    target_position,
                ) {
                    return Some(width + click_overshoot);
                }
            }
            crate::primitives::display_width::visual_column_of(&state.buffer, target_position)
        });

        let move_event = Event::MoveCursor {
            cursor_id: kept_cursor_id,
            old_position,
            new_position: target_position,
            old_anchor,
            new_anchor,
            old_sticky_column,
            new_sticky_column,
        };

        // The removals (see the cursor snapshot above) and the move are one
        // `Batch`. Removing a cursor is a write action, so unlike the lone
        // `MoveCursor` a click used to log, a collapsing click is an undo
        // step — `Ctrl+Z` brings the cursors back — and it truncates redo,
        // exactly as `Esc` does today.
        let mut events: Vec<Event> = removed_cursors
            .into_iter()
            .map(|(cursor_id, position, anchor)| Event::RemoveCursor {
                cursor_id,
                position,
                anchor,
            })
            .collect();
        let event = if events.is_empty() {
            move_event
        } else {
            events.push(move_event);
            Event::Batch {
                events,
                description: "Click".to_string(),
            }
        };

        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
        // Position history follows the move, not the removals.
        let moved = match &event {
            Event::Batch { events, .. } => events.last().expect("the batch ends with the move"),
            other => other,
        };
        self.track_cursor_movement(moved);

        // Park the cursor on the clicked virtual line (transient state, not
        // carried by the MoveCursor event — see Cursor::virtual_lines_below).
        if virtual_lines_below > 0 {
            self.active_cursors_mut().primary_mut().virtual_lines_below = virtual_lines_below;
        }

        // Start text selection drag for potential mouse drag
        self.active_window_mut().mouse_state.dragging_text_selection = true;
        self.active_window_mut().mouse_state.drag_selection_split = Some(split_id);
        self.active_window_mut().mouse_state.drag_selection_anchor =
            Some(new_anchor.unwrap_or(target_position));

        Ok(())
    }
}
