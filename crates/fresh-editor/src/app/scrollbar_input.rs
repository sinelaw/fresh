//! Mouse-driven scrollbar input on `Editor`.
//!
//! Mouse-wheel scrolling, horizontal panning, and the click/drag handlers
//! for both the regular vertical scrollbar and the composite-buffer
//! scrollbar (used in unified diff views). Pure scrollbar math lives in
//! `super::scrollbar_math`; these methods do the side-effecting work of
//! mutating viewports and split state.

use anyhow::Result as AnyhowResult;

use crate::model::event::{BufferId, LeafId};

use super::Editor;

impl Editor {
    /// Handle mouse wheel scroll event
    pub(super) fn handle_mouse_scroll(
        &mut self,
        col: u16,
        row: u16,
        delta: i32,
    ) -> AnyhowResult<()> {
        // Notify plugins of mouse scroll so they can handle it for virtual buffers
        let buffer_id = self.active_buffer();
        self.plugin_manager.run_hook(
            "mouse_scroll",
            fresh_core::hooks::HookArgs::MouseScroll {
                buffer_id,
                delta,
                col,
                row,
            },
        );

        // Check if scroll is over the file explorer
        if let Some(explorer_area) = self.cached_layout.file_explorer_area {
            if col >= explorer_area.x
                && col < explorer_area.x + explorer_area.width
                && row >= explorer_area.y
                && row < explorer_area.y + explorer_area.height
            {
                // Scroll the file explorer
                if let Some(explorer) = &mut self.file_explorer {
                    let count = explorer.visible_count();
                    if count == 0 {
                        return Ok(());
                    }

                    // Get current selected index
                    let current_index = explorer.get_selected_index().unwrap_or(0);

                    // Calculate new index based on scroll delta
                    let new_index = if delta < 0 {
                        // Scroll up (negative delta)
                        current_index.saturating_sub(delta.unsigned_abs() as usize)
                    } else {
                        // Scroll down (positive delta)
                        (current_index + delta as usize).min(count - 1)
                    };

                    // Set the new selection
                    if let Some(node_id) = explorer.get_node_at_index(new_index) {
                        explorer.set_selected(Some(node_id));
                        explorer.update_scroll_for_selection();
                    }
                }
                return Ok(());
            }
        }

        // Scroll the split under the mouse pointer (not necessarily the focused split).
        // Fall back to the active split if the pointer isn't over any split area.
        let (target_split, buffer_id) = self
            .split_at_position(col, row)
            .unwrap_or_else(|| (self.split_manager.active_split(), self.active_buffer()));

        // Panels marked non-scrollable (buffer-group toolbars/headers/footers
        // default to this) swallow the wheel event — their content is pinned
        // so scrolling would just shift the visible rows by one line.
        if self.is_non_scrollable_buffer(buffer_id) {
            return Ok(());
        }

        // Check if this is a composite buffer - if so, use composite scroll
        if self.is_composite_buffer(buffer_id) {
            let max_row = self
                .composite_buffers
                .get(&buffer_id)
                .map(|c| c.row_count().saturating_sub(1))
                .unwrap_or(0);
            if let Some(view_state) = self
                .composite_view_states
                .get_mut(&(target_split, buffer_id))
            {
                view_state.scroll(delta as isize, max_row);
                tracing::trace!(
                    "handle_mouse_scroll (composite): delta={}, scroll_row={}",
                    delta,
                    view_state.scroll_row
                );
            }
            return Ok(());
        }

        // Get view_transform tokens from SplitViewState (if any)
        let view_transform_tokens = self
            .split_view_states
            .get(&target_split)
            .and_then(|vs| vs.view_transform.as_ref())
            .map(|vt| vt.tokens.clone());

        // Get mutable references to both buffer state and view state
        let state = self.buffers.get_mut(&buffer_id);
        let view_state = self.split_view_states.get_mut(&target_split);

        if let (Some(state), Some(view_state)) = (state, view_state) {
            // Collect plugin soft-break positions BEFORE re-borrowing the buffer
            // so the viewport's visual-row math stays in lock-step with the
            // renderer (e.g. markdown_compose adds hanging-indent breaks via
            // addSoftBreak; without these the mouse wheel was either "absorbed"
            // at long-wrap item boundaries or got clamped short of EOF,
            // leaving the bottom half empty).
            let soft_breaks = state.collect_soft_break_positions();
            let buffer = &mut state.buffer;
            let top_byte_before = view_state.viewport.top_byte;
            if let Some(tokens) = view_transform_tokens {
                // Use view-aware scrolling with the transform's tokens
                use crate::view::ui::view_pipeline::ViewLineIterator;
                let tab_size = self.config.editor.tab_size;
                let view_lines: Vec<_> =
                    ViewLineIterator::new(&tokens, false, false, tab_size, false).collect();
                view_state
                    .viewport
                    .scroll_view_lines(&view_lines, delta as isize);
            } else {
                // No view transform - use traditional buffer-based scrolling.
                if delta < 0 {
                    // Scroll up
                    let lines_to_scroll = delta.unsigned_abs() as usize;
                    view_state
                        .viewport
                        .scroll_up(buffer, &soft_breaks, lines_to_scroll);
                } else {
                    // Scroll down
                    let lines_to_scroll = delta as usize;
                    view_state
                        .viewport
                        .scroll_down(buffer, &soft_breaks, lines_to_scroll);
                }
            }
            // Skip ensure_visible so the scroll position isn't undone during render
            view_state.viewport.set_skip_ensure_visible();

            if let Some(folds) = view_state.keyed_states.get(&buffer_id).map(|bs| &bs.folds) {
                if !folds.is_empty() {
                    let top_line = buffer.get_line_number(view_state.viewport.top_byte);
                    if let Some(range) = folds
                        .resolved_ranges(buffer, &state.marker_list)
                        .iter()
                        .find(|r| top_line >= r.start_line && top_line <= r.end_line)
                    {
                        let target_line = if delta >= 0 {
                            range.end_line.saturating_add(1)
                        } else {
                            range.header_line
                        };
                        let target_byte = buffer
                            .line_start_offset(target_line)
                            .unwrap_or_else(|| buffer.len());
                        view_state.viewport.top_byte = target_byte;
                        view_state.viewport.top_view_line_offset = 0;
                    }
                }
            }
            tracing::trace!(
                "handle_mouse_scroll: delta={}, top_byte {} -> {}",
                delta,
                top_byte_before,
                view_state.viewport.top_byte
            );
        }

        Ok(())
    }

    /// Handle horizontal scroll (Shift+ScrollWheel or native ScrollLeft/ScrollRight)
    pub(super) fn handle_horizontal_scroll(
        &mut self,
        col: u16,
        row: u16,
        delta: i32,
    ) -> AnyhowResult<()> {
        let (target_split, buffer_id) = self
            .split_at_position(col, row)
            .unwrap_or_else(|| (self.split_manager.active_split(), self.active_buffer()));

        if self.is_non_scrollable_buffer(buffer_id) {
            return Ok(());
        }

        if let Some(view_state) = self.split_view_states.get_mut(&target_split) {
            // Line wrap makes horizontal scroll a no-op.
            if view_state.viewport.line_wrap_enabled {
                return Ok(());
            }

            let columns_to_scroll = delta.unsigned_abs() as usize;
            let viewport = &mut view_state.viewport;
            if delta < 0 {
                viewport.left_column = viewport.left_column.saturating_sub(columns_to_scroll);
            } else {
                // No max_line_length_seen clamp: that value is stale between
                // renders (often 0 before any h-scroll), pinning this at 0
                // even when long lines exist. Overshoot clips at render.
                viewport.left_column = viewport.left_column.saturating_add(columns_to_scroll);
            }
            viewport.set_skip_ensure_visible();
        }

        Ok(())
    }

    /// Handle scrollbar drag with relative movement (when dragging from thumb)
    pub(super) fn handle_scrollbar_drag_relative(
        &mut self,
        row: u16,
        split_id: LeafId,
        buffer_id: BufferId,
        scrollbar_rect: ratatui::layout::Rect,
    ) -> AnyhowResult<()> {
        let drag_start_row = match self.mouse_state.drag_start_row {
            Some(r) => r,
            None => return Ok(()), // No drag start, shouldn't happen
        };

        // Handle composite buffers - use row-based scrolling
        if self.is_composite_buffer(buffer_id) {
            return self.handle_composite_scrollbar_drag_relative(
                row,
                drag_start_row,
                split_id,
                buffer_id,
                scrollbar_rect,
            );
        }

        let drag_start_top_byte = match self.mouse_state.drag_start_top_byte {
            Some(b) => b,
            None => return Ok(()), // No drag start, shouldn't happen
        };

        let drag_start_view_line_offset = self.mouse_state.drag_start_view_line_offset.unwrap_or(0);

        // Calculate the offset in rows (still used for large files)
        let row_offset = (row as i32) - (drag_start_row as i32);

        // Get viewport height from SplitViewState
        let viewport_height = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.viewport.height as usize)
            .unwrap_or(10);

        // Check if line wrapping is enabled
        let line_wrap_enabled = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.viewport.line_wrap_enabled)
            .unwrap_or(false);

        let viewport_width = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.viewport.width as usize)
            .unwrap_or(80);

        // Get the buffer state and calculate target position using RELATIVE movement
        // Returns (byte_position, view_line_offset) for proper positioning within wrapped lines
        let scroll_position = if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let scrollbar_height = scrollbar_rect.height as usize;
            if scrollbar_height == 0 {
                return Ok(());
            }

            let buffer_len = state.buffer.len();
            let large_file_threshold = self.config.editor.large_file_threshold_bytes as usize;

            // Use relative movement: calculate scroll change based on row_offset from drag start
            if buffer_len <= large_file_threshold {
                // When line wrapping is enabled, use visual row calculations
                if line_wrap_enabled {
                    let pipeline_inputs_ver = crate::view::line_wrap_cache::pipeline_inputs_version(
                        state.buffer.version(),
                        state.soft_breaks.version(),
                        state.conceals.version(),
                    );
                    super::scrollbar_math::scrollbar_drag_relative_visual(
                        &mut state.buffer,
                        row,
                        scrollbar_rect.y,
                        scrollbar_height,
                        drag_start_row,
                        drag_start_top_byte,
                        drag_start_view_line_offset,
                        viewport_height,
                        viewport_width,
                        &mut state.line_wrap_cache,
                        pipeline_inputs_ver,
                    )
                } else {
                    // Small file without line wrap: thumb follows mouse
                    let total_lines = if buffer_len > 0 {
                        state.buffer.get_line_number(buffer_len.saturating_sub(1)) + 1
                    } else {
                        1
                    };

                    let max_scroll_line = total_lines.saturating_sub(viewport_height);

                    if max_scroll_line == 0 || scrollbar_height <= 1 {
                        // File fits in viewport, no scrolling
                        (0, 0)
                    } else {
                        // Find the starting line number from drag_start_top_byte
                        let start_line = state.buffer.get_line_number(drag_start_top_byte);

                        // Calculate thumb size (same formula as scrollbar rendering)
                        let thumb_size_raw = (viewport_height as f64 / total_lines as f64
                            * scrollbar_height as f64)
                            .ceil() as usize;
                        let max_thumb_size = (scrollbar_height as f64 * 0.8).floor() as usize;
                        let thumb_size = thumb_size_raw
                            .max(1)
                            .min(max_thumb_size)
                            .min(scrollbar_height);

                        // Calculate max thumb start position (same as scrollbar rendering)
                        let max_thumb_start = scrollbar_height.saturating_sub(thumb_size);

                        if max_thumb_start == 0 {
                            // Thumb fills the track, no dragging possible
                            (0, 0)
                        } else {
                            // Calculate where the thumb was at drag start
                            let start_scroll_ratio =
                                start_line.min(max_scroll_line) as f64 / max_scroll_line as f64;
                            let thumb_row_at_start = scrollbar_rect.y as f64
                                + start_scroll_ratio * max_thumb_start as f64;

                            // Calculate click offset (where on thumb we clicked)
                            let click_offset = drag_start_row as f64 - thumb_row_at_start;

                            // Target thumb position based on current mouse position
                            let target_thumb_row = row as f64 - click_offset;

                            // Map target thumb position to scroll ratio
                            let target_scroll_ratio = ((target_thumb_row
                                - scrollbar_rect.y as f64)
                                / max_thumb_start as f64)
                                .clamp(0.0, 1.0);

                            // Map scroll ratio to target line
                            let target_line =
                                (target_scroll_ratio * max_scroll_line as f64).round() as usize;
                            let target_line = target_line.min(max_scroll_line);

                            // Find byte position of target line
                            let target_byte = state
                                .buffer
                                .line_start_offset(target_line)
                                .unwrap_or(drag_start_top_byte);

                            (target_byte, 0)
                        }
                    }
                }
            } else {
                // Large file: use byte-based relative movement
                let bytes_per_pixel = buffer_len as f64 / scrollbar_height as f64;
                let byte_offset = (row_offset as f64 * bytes_per_pixel) as i64;

                let new_top_byte = if byte_offset >= 0 {
                    drag_start_top_byte.saturating_add(byte_offset as usize)
                } else {
                    drag_start_top_byte.saturating_sub((-byte_offset) as usize)
                };

                // Clamp to valid range using byte-based max (avoid iterating entire buffer)
                let new_top_byte = new_top_byte.min(buffer_len.saturating_sub(1));

                // Find the line start for this byte position
                let iter = state.buffer.line_iterator(new_top_byte, 80);
                (iter.current_position(), 0)
            }
        } else {
            return Ok(());
        };

        // Set viewport top to this position in SplitViewState
        if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
            view_state.viewport.top_byte = scroll_position.0;
            view_state.viewport.top_view_line_offset = scroll_position.1;
            // Skip ensure_visible so the scroll position isn't undone during render
            view_state.viewport.set_skip_ensure_visible();
        }

        // Move cursor to be visible in the new viewport (after releasing the state borrow)
        self.move_cursor_to_visible_area(split_id, buffer_id);

        Ok(())
    }

    /// Handle scrollbar jump (clicking on track or absolute positioning)
    pub(super) fn handle_scrollbar_jump(
        &mut self,
        _col: u16,
        row: u16,
        split_id: LeafId,
        buffer_id: BufferId,
        scrollbar_rect: ratatui::layout::Rect,
    ) -> AnyhowResult<()> {
        // Calculate which line to scroll to based on mouse position
        let scrollbar_height = scrollbar_rect.height as usize;
        if scrollbar_height == 0 {
            return Ok(());
        }

        // Get relative position in scrollbar (0.0 to 1.0)
        // Divide by (height - 1) to map first row to 0.0 and last row to 1.0
        let relative_row = row.saturating_sub(scrollbar_rect.y);
        let ratio = if scrollbar_height > 1 {
            ((relative_row as f64) / ((scrollbar_height - 1) as f64)).clamp(0.0, 1.0)
        } else {
            0.0
        };

        // Handle composite buffers - use row-based scrolling
        if self.is_composite_buffer(buffer_id) {
            return self.handle_composite_scrollbar_jump(
                ratio,
                split_id,
                buffer_id,
                scrollbar_rect,
            );
        }

        // Get viewport height from SplitViewState
        let viewport_height = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.viewport.height as usize)
            .unwrap_or(10);

        // Check if line wrapping is enabled
        let line_wrap_enabled = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.viewport.line_wrap_enabled)
            .unwrap_or(false);

        let viewport_width = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.viewport.width as usize)
            .unwrap_or(80);

        // Get the buffer state and calculate scroll position
        // Returns (byte_position, view_line_offset) for proper positioning within wrapped lines
        let scroll_position = if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let buffer_len = state.buffer.len();
            let large_file_threshold = self.config.editor.large_file_threshold_bytes as usize;

            // For small files, use precise line-based calculations
            // For large files, fall back to byte-based estimation
            if buffer_len <= large_file_threshold {
                // When line wrapping is enabled, use visual row calculations
                if line_wrap_enabled {
                    // calculate_scrollbar_jump_visual already handles max scroll limiting
                    // and returns both byte position and view line offset
                    let pipeline_inputs_ver = crate::view::line_wrap_cache::pipeline_inputs_version(
                        state.buffer.version(),
                        state.soft_breaks.version(),
                        state.conceals.version(),
                    );
                    super::scrollbar_math::scrollbar_jump_visual(
                        &mut state.buffer,
                        ratio,
                        viewport_height,
                        viewport_width,
                        &mut state.line_wrap_cache,
                        pipeline_inputs_ver,
                    )
                } else {
                    // Small file without line wrap: use line-based calculation for precision
                    let total_lines = if buffer_len > 0 {
                        state.buffer.get_line_number(buffer_len.saturating_sub(1)) + 1
                    } else {
                        1
                    };

                    let max_scroll_line = total_lines.saturating_sub(viewport_height);

                    let target_byte = if max_scroll_line == 0 {
                        // File fits in viewport, no scrolling
                        0
                    } else {
                        // Map ratio to target line
                        let target_line = (ratio * max_scroll_line as f64).round() as usize;
                        let target_line = target_line.min(max_scroll_line);

                        // Find byte position of target line
                        // We need to iterate 'target_line' times to skip past lines 0..target_line-1,
                        // then one more time to get the position of line 'target_line'
                        let mut iter = state.buffer.line_iterator(0, 80);
                        let mut line_byte = 0;

                        for _ in 0..target_line {
                            if let Some((pos, _content)) = iter.next_line() {
                                line_byte = pos;
                            } else {
                                break;
                            }
                        }

                        // Get the position of the target line
                        if let Some((pos, _)) = iter.next_line() {
                            pos
                        } else {
                            line_byte // Reached end of buffer
                        }
                    };

                    // Find the line start for this byte position
                    let iter = state.buffer.line_iterator(target_byte, 80);
                    let line_start = iter.current_position();

                    // Apply scroll limiting
                    let max_top_byte =
                        Self::calculate_max_scroll_position(&mut state.buffer, viewport_height);
                    (line_start.min(max_top_byte), 0)
                }
            } else {
                // Large file: use byte-based estimation (original logic)
                let target_byte = (buffer_len as f64 * ratio) as usize;
                let target_byte = target_byte.min(buffer_len.saturating_sub(1));

                // Find the line start for this byte position
                let iter = state.buffer.line_iterator(target_byte, 80);
                let line_start = iter.current_position();

                (line_start.min(buffer_len.saturating_sub(1)), 0)
            }
        } else {
            return Ok(());
        };

        // Set viewport top to this position in SplitViewState
        if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
            view_state.viewport.top_byte = scroll_position.0;
            view_state.viewport.top_view_line_offset = scroll_position.1;
            // Skip ensure_visible so the scroll position isn't undone during render
            view_state.viewport.set_skip_ensure_visible();
        }

        // Move cursor to be visible in the new viewport (after releasing the state borrow)
        self.move_cursor_to_visible_area(split_id, buffer_id);

        Ok(())
    }

    /// Handle scrollbar jump (click on track) for composite buffers.
    /// Maps the click ratio to a row-based scroll position.
    fn handle_composite_scrollbar_jump(
        &mut self,
        ratio: f64,
        split_id: LeafId,
        buffer_id: BufferId,
        scrollbar_rect: ratatui::layout::Rect,
    ) -> AnyhowResult<()> {
        let total_rows = self
            .composite_buffers
            .get(&buffer_id)
            .map(|c| c.row_count())
            .unwrap_or(0);
        let content_height = scrollbar_rect.height.saturating_sub(1) as usize;
        let max_scroll_row = total_rows.saturating_sub(content_height);
        let target_row = (ratio * max_scroll_row as f64).round() as usize;
        let target_row = target_row.min(max_scroll_row);

        if let Some(view_state) = self.composite_view_states.get_mut(&(split_id, buffer_id)) {
            view_state.set_scroll_row(target_row, max_scroll_row);
        }
        Ok(())
    }

    /// Handle scrollbar thumb drag for composite buffers.
    /// Uses relative movement from the drag start position.
    fn handle_composite_scrollbar_drag_relative(
        &mut self,
        row: u16,
        drag_start_row: u16,
        split_id: LeafId,
        buffer_id: BufferId,
        scrollbar_rect: ratatui::layout::Rect,
    ) -> AnyhowResult<()> {
        let drag_start_scroll_row = match self.mouse_state.drag_start_composite_scroll_row {
            Some(r) => r,
            None => return Ok(()),
        };

        let total_rows = self
            .composite_buffers
            .get(&buffer_id)
            .map(|c| c.row_count())
            .unwrap_or(0);
        let content_height = scrollbar_rect.height.saturating_sub(1) as usize;
        let max_scroll_row = total_rows.saturating_sub(content_height);

        if max_scroll_row == 0 {
            return Ok(());
        }

        let scrollbar_height = scrollbar_rect.height as usize;
        if scrollbar_height <= 1 {
            return Ok(());
        }

        // Calculate thumb size (same formula as render_composite_scrollbar)
        let thumb_size_raw =
            (content_height as f64 / total_rows as f64 * scrollbar_height as f64).ceil() as usize;
        let max_thumb_size = (scrollbar_height as f64 * 0.8).floor() as usize;
        let thumb_size = thumb_size_raw
            .max(1)
            .min(max_thumb_size)
            .min(scrollbar_height);
        let max_thumb_start = scrollbar_height.saturating_sub(thumb_size);

        if max_thumb_start == 0 {
            return Ok(());
        }

        // Calculate where the thumb was at drag start
        let start_scroll_ratio =
            drag_start_scroll_row.min(max_scroll_row) as f64 / max_scroll_row as f64;
        let thumb_row_at_start =
            scrollbar_rect.y as f64 + start_scroll_ratio * max_thumb_start as f64;

        // Calculate click offset (where on thumb we clicked)
        let click_offset = drag_start_row as f64 - thumb_row_at_start;

        // Target thumb position based on current mouse position
        let target_thumb_row = row as f64 - click_offset;

        // Map target thumb position to scroll ratio
        let target_scroll_ratio =
            ((target_thumb_row - scrollbar_rect.y as f64) / max_thumb_start as f64).clamp(0.0, 1.0);

        // Map scroll ratio to target row
        let target_row = (target_scroll_ratio * max_scroll_row as f64).round() as usize;
        let target_row = target_row.min(max_scroll_row);

        if let Some(view_state) = self.composite_view_states.get_mut(&(split_id, buffer_id)) {
            view_state.set_scroll_row(target_row, max_scroll_row);
        }
        Ok(())
    }
}
