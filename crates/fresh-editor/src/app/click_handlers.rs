//! Click and scroll-position helpers on `Editor`.
//!
//! - `move_cursor_to_visible_area` and `calculate_max_scroll_position`:
//!   small helpers that fix up cursor position after scroll-driven moves
//!   so the user keeps a visible cursor.
//! - `fold_toggle_line_at_screen_position`: maps a click in the gutter to
//!   the byte to fold/unfold (uses the pure helper from
//!   `super::click_geometry`).
//! - `handle_editor_click`: dispatches mouse clicks to gutter / scrollbar
//!   / cursor placement / multi-cursor add depending on modifiers.
//! - `handle_file_explorer_click`: file-browser entry selection and
//!   expand/collapse.

use anyhow::Result as AnyhowResult;

use crate::input::keybindings::Action;
use crate::model::event::{BufferId, LeafId};
use crate::services::plugins::hooks::HookArgs;

use super::Editor;

impl Editor {
    /// Move the cursor to a visible position within the current viewport
    /// This is called after scrollbar operations to ensure the cursor is in view
    pub(super) fn move_cursor_to_visible_area(&mut self, split_id: LeafId, buffer_id: BufferId) {
        // Get viewport info from SplitViewState
        let (top_byte, viewport_height) =
            if let Some(view_state) = self.split_view_states.get(&split_id) {
                (
                    view_state.viewport.top_byte,
                    view_state.viewport.height as usize,
                )
            } else {
                return;
            };

        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let buffer_len = state.buffer.len();

            // Find the bottom byte of the viewport
            // We iterate through viewport_height lines starting from top_byte
            let mut iter = state.buffer.line_iterator(top_byte, 80);
            let mut bottom_byte = buffer_len;

            // Consume viewport_height lines to find where the visible area ends
            for _ in 0..viewport_height {
                if let Some((pos, line)) = iter.next_line() {
                    // The bottom of this line is at pos + line.len()
                    bottom_byte = pos + line.len();
                } else {
                    // Reached end of buffer
                    bottom_byte = buffer_len;
                    break;
                }
            }

            // Check if cursor is outside visible range and move it if needed
            if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                let cursor_pos = view_state.cursors.primary().position;
                if cursor_pos < top_byte || cursor_pos > bottom_byte {
                    // Move cursor to the top of the viewport
                    let cursor = view_state.cursors.primary_mut();
                    cursor.position = top_byte;
                    // Keep the existing sticky_column value so vertical navigation preserves column
                }
            }
        }
    }

    /// Calculate the maximum allowed scroll position
    /// Ensures the last line is always at the bottom unless the buffer is smaller than viewport
    pub(super) fn calculate_max_scroll_position(
        buffer: &mut crate::model::buffer::Buffer,
        viewport_height: usize,
    ) -> usize {
        if viewport_height == 0 {
            return 0;
        }

        let buffer_len = buffer.len();
        if buffer_len == 0 {
            return 0;
        }

        // Count total lines in buffer
        let mut line_count = 0;
        let mut iter = buffer.line_iterator(0, 80);
        while iter.next_line().is_some() {
            line_count += 1;
        }

        // If buffer has fewer lines than viewport, can't scroll at all
        if line_count <= viewport_height {
            return 0;
        }

        // Calculate how many lines from the start we can scroll
        // We want to be able to scroll so that the last line is at the bottom
        let scrollable_lines = line_count.saturating_sub(viewport_height);

        // Find the byte position of the line at scrollable_lines offset
        let mut iter = buffer.line_iterator(0, 80);
        let mut current_line = 0;
        let mut max_byte_pos = 0;

        while current_line < scrollable_lines {
            if let Some((pos, _content)) = iter.next_line() {
                max_byte_pos = pos;
                current_line += 1;
            } else {
                break;
            }
        }

        max_byte_pos
    }

    pub(super) fn fold_toggle_line_at_screen_position(
        &self,
        col: u16,
        row: u16,
    ) -> Option<(BufferId, usize)> {
        for (split_id, buffer_id, content_rect, _scrollbar_rect, _thumb_start, _thumb_end) in
            &self.cached_layout.split_areas
        {
            if col < content_rect.x
                || col >= content_rect.x + content_rect.width
                || row < content_rect.y
                || row >= content_rect.y + content_rect.height
            {
                continue;
            }

            if self.is_terminal_buffer(*buffer_id) || self.is_composite_buffer(*buffer_id) {
                continue;
            }

            let (gutter_width, collapsed_header_bytes) = {
                let state = self.buffers.get(buffer_id)?;
                let headers = self
                    .split_view_states
                    .get(split_id)
                    .map(|vs| {
                        vs.folds
                            .collapsed_header_bytes(&state.buffer, &state.marker_list)
                    })
                    .unwrap_or_default();
                (state.margins.left_total_width() as u16, headers)
            };

            let cached_mappings = self.cached_layout.view_line_mappings.get(split_id).cloned();
            let fallback = self
                .split_view_states
                .get(split_id)
                .map(|vs| vs.viewport.top_byte)
                .unwrap_or(0);
            let compose_width = self
                .split_view_states
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

            let adjusted_rect = super::click_geometry::adjust_content_rect_for_compose(
                *content_rect,
                compose_width,
            );
            let content_col = col.saturating_sub(adjusted_rect.x);
            let state = self.buffers.get(buffer_id)?;
            if let Some(byte_pos) = super::click_geometry::fold_toggle_byte_from_position(
                state,
                &collapsed_header_bytes,
                target_position,
                content_col,
                gutter_width,
            ) {
                return Some((*buffer_id, byte_pos));
            }
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

        // Dispatch MouseClick hook to plugins
        // Plugins can handle clicks on their virtual buffers
        if self.plugin_manager.has_hook_handlers("mouse_click") {
            // Compute buffer-local row/col so plugins can react to clicks
            // on specific rows (e.g. clicking a file header, or a comment
            // in the comments navigation panel) without redoing the math.
            let (hook_buffer_row, hook_buffer_col) = {
                let cached_mappings = self
                    .cached_layout
                    .view_line_mappings
                    .get(&split_id)
                    .cloned();
                let fallback = self
                    .split_view_states
                    .get(&split_id)
                    .map(|vs| vs.viewport.top_byte)
                    .unwrap_or(0);
                let compose_width = self
                    .split_view_states
                    .get(&split_id)
                    .and_then(|vs| vs.compose_width);
                let gutter_width = self
                    .buffers
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
                        let state = self.buffers.get(&buffer_id);
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
            self.plugin_manager.run_hook(
                "mouse_click",
                HookArgs::MouseClick {
                    column: col,
                    row,
                    button: "left".to_string(),
                    modifiers: modifiers_str,
                    content_x: content_rect.x,
                    content_y: content_rect.y,
                    buffer_id: Some(buffer_id.0 as u64),
                    buffer_row: hook_buffer_row,
                    buffer_col: hook_buffer_col,
                },
            );
        }

        // Fixed buffer-group panels (toolbars/headers/footers) aren't
        // interactive targets: focusing them would let arrow keys move an
        // invisible cursor and scroll the pinned content. Swallow the click
        // after the plugin hook has had a chance to observe it. Scrollable
        // group panels still accept the click (focus routes to them) even
        // when their cursor is hidden.
        if self.is_non_scrollable_buffer(buffer_id) {
            return Ok(());
        }

        // Focus this split (handles terminal mode exit, tab state, etc.)
        self.focus_split(split_id, buffer_id);

        // Handle composite buffer clicks specially
        if self.is_composite_buffer(buffer_id) {
            return self.handle_composite_click(col, row, split_id, buffer_id, content_rect);
        }

        // Ensure key context is Normal for non-terminal buffers
        // This handles the edge case where split/buffer don't change but we clicked from FileExplorer
        if !self.is_terminal_buffer(buffer_id) {
            self.key_context = crate::input::keybindings::KeyContext::Normal;
        }

        // Get cached view line mappings for this split (before mutable borrow of buffers)
        let cached_mappings = self
            .cached_layout
            .view_line_mappings
            .get(&split_id)
            .cloned();

        // Get fallback from SplitViewState viewport
        let fallback = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.viewport.top_byte)
            .unwrap_or(0);

        // Get compose width for this split (adjusts content rect for centered layout)
        let compose_width = self
            .split_view_states
            .get(&split_id)
            .and_then(|vs| vs.compose_width);

        // Calculate clicked position in buffer
        let (toggle_fold_byte, onclick_action, target_position, cursor_snapshot) =
            if let Some(state) = self.buffers.get(&buffer_id) {
                let gutter_width = state.margins.left_total_width() as u16;

                let Some(target_position) = super::click_geometry::screen_to_buffer_position(
                    col,
                    row,
                    content_rect,
                    gutter_width,
                    &cached_mappings,
                    fallback,
                    true, // Allow gutter clicks - position cursor at start of line
                    compose_width,
                ) else {
                    return Ok(());
                };

                // Toggle fold on gutter click if this line is foldable/collapsed
                let adjusted_rect = super::click_geometry::adjust_content_rect_for_compose(
                    content_rect,
                    compose_width,
                );
                let content_col = col.saturating_sub(adjusted_rect.x);
                let collapsed_header_bytes = self
                    .split_view_states
                    .get(&split_id)
                    .map(|vs| {
                        vs.folds
                            .collapsed_header_bytes(&state.buffer, &state.marker_list)
                    })
                    .unwrap_or_default();
                let toggle_fold_byte = super::click_geometry::fold_toggle_byte_from_position(
                    state,
                    &collapsed_header_bytes,
                    target_position,
                    content_col,
                    gutter_width,
                );

                let cursor_snapshot = self
                    .split_view_states
                    .get(&split_id)
                    .map(|vs| {
                        let cursor = vs.cursors.primary();
                        (
                            vs.cursors.primary_id(),
                            cursor.position,
                            cursor.anchor,
                            cursor.sticky_column,
                            cursor.deselect_on_move,
                        )
                    })
                    .unwrap_or((CursorId(0), 0, None, 0, true));

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
                    target_position,
                    cursor_snapshot,
                )
            } else {
                return Ok(());
            };

        if toggle_fold_byte.is_some() {
            self.toggle_fold_at_byte(buffer_id, target_position);
            return Ok(());
        }

        let (primary_cursor_id, old_position, old_anchor, old_sticky_column, deselect_on_move) =
            cursor_snapshot;

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
        // Both modifiers supported since some terminals intercept shift+click.
        let extend_selection =
            modifiers.contains(KeyModifiers::SHIFT) || modifiers.contains(KeyModifiers::CONTROL);
        let new_anchor = if extend_selection {
            Some(old_anchor.unwrap_or(old_position))
        } else if deselect_on_move {
            None
        } else {
            old_anchor
        };

        let new_sticky_column = self
            .buffers
            .get(&buffer_id)
            .and_then(|state| state.buffer.offset_to_position(target_position))
            .map(|pos| pos.column)
            .unwrap_or(0);

        let event = Event::MoveCursor {
            cursor_id: primary_cursor_id,
            old_position,
            new_position: target_position,
            old_anchor,
            new_anchor,
            old_sticky_column,
            new_sticky_column,
        };

        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
        self.track_cursor_movement(&event);

        // Start text selection drag for potential mouse drag
        self.mouse_state.dragging_text_selection = true;
        self.mouse_state.drag_selection_split = Some(split_id);
        self.mouse_state.drag_selection_anchor = Some(new_anchor.unwrap_or(target_position));

        Ok(())
    }

    /// Handle click in file explorer
    pub(super) fn handle_file_explorer_click(
        &mut self,
        col: u16,
        row: u16,
        explorer_area: ratatui::layout::Rect,
    ) -> AnyhowResult<()> {
        // Check if click is on the title bar (first row)
        if row == explorer_area.y {
            // Check if click is on close button (× at right side of title bar)
            // Close button is at position: explorer_area.x + explorer_area.width - 3 to -1
            let close_button_x = explorer_area.x + explorer_area.width.saturating_sub(3);
            if col >= close_button_x && col < explorer_area.x + explorer_area.width {
                self.toggle_file_explorer();
                return Ok(());
            }
        }

        // Focus file explorer
        self.key_context = crate::input::keybindings::KeyContext::FileExplorer;

        // Calculate which item was clicked (accounting for border and title)
        // The file explorer has a 1-line border at top and bottom
        let relative_row = row.saturating_sub(explorer_area.y + 1); // +1 for top border

        if let Some(ref mut explorer) = self.file_explorer {
            let display_nodes = explorer.get_display_nodes();
            let scroll_offset = explorer.get_scroll_offset();
            let clicked_index = (relative_row as usize) + scroll_offset;

            if clicked_index < display_nodes.len() {
                let (node_id, _indent) = display_nodes[clicked_index];

                // Select this node
                explorer.set_selected(Some(node_id));

                // Check if it's a file or directory
                let node = explorer.tree().get_node(node_id);
                if let Some(node) = node {
                    if node.is_dir() {
                        // Toggle expand/collapse using the existing method
                        self.file_explorer_toggle_expand();
                    } else if node.is_file() {
                        // Open the file but keep focus on file explorer (single click).
                        // Double-click or Enter will focus the editor and promote to
                        // a permanent tab. Single-click opens in "preview" mode so a
                        // string of exploratory clicks doesn't accumulate tabs.
                        let path = node.entry.path.clone();
                        let name = node.entry.name.clone();
                        match self.open_file_preview(&path) {
                            Ok(_) => {
                                self.set_status_message(
                                    rust_i18n::t!("explorer.opened_file", name = &name).to_string(),
                                );
                            }
                            Err(e) => {
                                // Check if this is a large file encoding confirmation error
                                if let Some(confirmation) = e.downcast_ref::<
                                    crate::model::buffer::LargeFileEncodingConfirmation,
                                >() {
                                    self.start_large_file_encoding_confirmation(confirmation);
                                } else {
                                    self.set_status_message(
                                        rust_i18n::t!("file.error_opening", error = e.to_string())
                                            .to_string(),
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }
}
