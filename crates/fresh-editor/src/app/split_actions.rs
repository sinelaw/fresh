//! Split/pane management for the Editor.
//!
//! This module contains all methods related to managing editor splits:
//! - Creating horizontal/vertical splits
//! - Closing splits
//! - Navigating between splits
//! - Managing per-split view states (cursors, viewport)
//! - Split size adjustment and maximize

use rust_i18n::t;

use crate::model::event::{BufferId, ContainerId, Event, LeafId, SplitDirection, SplitId};
use crate::view::folding::CollapsedFoldLineRange;
use crate::view::split::SplitViewState;

use super::Editor;

impl Editor {
    /// Split the current pane horizontally
    pub fn split_pane_horizontal(&mut self) {
        self.split_pane_impl(crate::model::event::SplitDirection::Horizontal);
    }

    /// Split the current pane vertically
    pub fn split_pane_vertical(&mut self) {
        self.split_pane_impl(crate::model::event::SplitDirection::Vertical);
    }

    /// Common split creation logic
    fn split_pane_impl(&mut self, direction: crate::model::event::SplitDirection) {
        // Splitting the layout is a commitment gesture for any preview tab:
        // the user is setting up their working environment around it. Promote
        // before touching the split tree so the invariant "preview is anchored
        // to a single split" stays consistent across the operation.
        self.promote_current_preview();

        let current_buffer_id = self.active_buffer();
        let active_split = self.split_manager.active_split();

        // Copy keyed states from source split so the new split inherits per-buffer state
        let source_keyed_states = self.split_view_states.get(&active_split).map(|vs| {
            vs.keyed_states
                .iter()
                .filter(|(&buf_id, _)| buf_id != current_buffer_id)
                .map(|(&buf_id, buf_state)| {
                    let folds = self
                        .buffers
                        .get(&buf_id)
                        .map(|state| {
                            buf_state
                                .folds
                                .collapsed_line_ranges(&state.buffer, &state.marker_list)
                        })
                        .unwrap_or_default();
                    (buf_id, buf_state.clone(), folds)
                })
                .collect::<Vec<(
                    BufferId,
                    crate::view::split::BufferViewState,
                    Vec<CollapsedFoldLineRange>,
                )>>()
        });

        match self
            .split_manager
            .split_active(direction, current_buffer_id, 0.5)
        {
            Ok(new_split_id) => {
                let mut view_state = SplitViewState::with_buffer(
                    self.terminal_width,
                    self.terminal_height,
                    current_buffer_id,
                );
                view_state.apply_config_defaults(
                    self.config.editor.line_numbers,
                    self.config.editor.highlight_current_line,
                    self.resolve_line_wrap_for_buffer(current_buffer_id),
                    self.config.editor.wrap_indent,
                    self.resolve_wrap_column_for_buffer(current_buffer_id),
                    self.config.editor.rulers.clone(),
                );

                // Copy keyed states from source split for OTHER buffers (not the active one).
                // The active buffer gets a fresh cursor in the new split.
                if let Some(source) = source_keyed_states {
                    for (buf_id, mut buf_state, folds) in source {
                        if let Some(state) = self.buffers.get_mut(&buf_id) {
                            buf_state.folds.clear(&mut state.marker_list);
                            for fold in folds {
                                let start_line = fold.header_line.saturating_add(1);
                                let end_line = fold.end_line;
                                if start_line > end_line {
                                    continue;
                                }
                                let Some(start_byte) = state.buffer.line_start_offset(start_line)
                                else {
                                    continue;
                                };
                                let end_byte = state
                                    .buffer
                                    .line_start_offset(end_line.saturating_add(1))
                                    .unwrap_or_else(|| state.buffer.len());
                                buf_state.folds.add(
                                    &mut state.marker_list,
                                    start_byte,
                                    end_byte,
                                    fold.placeholder.clone(),
                                );
                            }
                        }
                        view_state.keyed_states.insert(buf_id, buf_state);
                    }
                }

                self.split_view_states.insert(new_split_id, view_state);
                let msg = match direction {
                    crate::model::event::SplitDirection::Horizontal => t!("split.horizontal"),
                    crate::model::event::SplitDirection::Vertical => t!("split.vertical"),
                };
                self.set_status_message(msg.to_string());
            }
            Err(e) => {
                self.set_status_message(t!("split.error", error = e.to_string()).to_string());
            }
        }
    }

    /// Close the active split
    pub fn close_active_split(&mut self) {
        // Closing a split rearranges tab ownership (remaining tabs migrate
        // to the new active split). Promote any preview first so it doesn't
        // end up orphaned in a split that no longer exists, or silently
        // migrated to an unrelated pane.
        self.promote_current_preview();

        let closing_split = self.split_manager.active_split();

        // Get the tabs from the split we're closing before we close it
        let closing_split_tabs = self
            .split_view_states
            .get(&closing_split)
            .map(|vs| vs.open_buffers.clone())
            .unwrap_or_default();

        match self.split_manager.close_split(closing_split) {
            Ok(_) => {
                // Clean up the view state for the closed split
                self.split_view_states.remove(&closing_split);

                // Get the new active split after closing
                let new_active_split = self.split_manager.active_split();

                // Transfer tabs from closed split to the new active split
                if let Some(view_state) = self.split_view_states.get_mut(&new_active_split) {
                    for target in closing_split_tabs {
                        // Only add if not already in the split's tabs
                        if !view_state.open_buffers.contains(&target) {
                            view_state.open_buffers.push(target);
                        }
                    }
                }

                // NOTE: active_buffer is now derived from split_manager, no sync needed

                self.set_status_message(t!("split.closed").to_string());
            }
            Err(e) => {
                self.set_status_message(
                    t!("split.cannot_close", error = e.to_string()).to_string(),
                );
            }
        }
    }

    /// Switch to next split
    pub fn next_split(&mut self) {
        self.switch_split(true);
        self.set_status_message(t!("split.next").to_string());
    }

    /// Switch to previous split
    pub fn prev_split(&mut self) {
        self.switch_split(false);
        self.set_status_message(t!("split.prev").to_string());
    }

    /// Common split switching logic
    fn switch_split(&mut self, next: bool) {
        // Capture what was active before the switch so we can mirror the
        // mouse-click path in `focus_split`: leaving a terminal buffer must
        // stop routing keyboard input to it. The terminal's visible pane
        // keeps rendering live because `render_terminal_splits` ignores
        // `terminal_mode` whenever the terminal isn't the active buffer.
        let previous_buffer = self.active_buffer();

        if next {
            self.split_manager.next_split();
        } else {
            self.split_manager.prev_split();
        }

        // Ensure the active tab is visible in the newly active split
        let split_id = self.split_manager.active_split();
        // Moving focus to a different split commits the preview — walking
        // away is commitment. Matches the rule applied in `focus_split`.
        self.promote_preview_if_not_in_split(split_id);
        self.ensure_active_tab_visible(split_id, self.active_buffer(), self.effective_tabs_width());

        let buffer_id = self.active_buffer();

        // Leaving a terminal buffer: stop capturing keyboard for the
        // terminal. Symmetric with the mouse-click path in `focus_split`.
        if self.terminal_mode
            && self.is_terminal_buffer(previous_buffer)
            && !self.is_terminal_buffer(buffer_id)
        {
            self.terminal_mode = false;
            self.key_context = crate::input::keybindings::KeyContext::Normal;
        }

        // Emit buffer_activated hook for plugins
        self.plugin_manager.run_hook(
            "buffer_activated",
            crate::services::plugins::hooks::HookArgs::BufferActivated { buffer_id },
        );

        // Enter terminal mode if switching to a terminal split
        if self.is_terminal_buffer(buffer_id) {
            self.terminal_mode = true;
            self.key_context = crate::input::keybindings::KeyContext::Terminal;
        }
    }

    /// Adjust cursors in other splits that share the same buffer after an edit
    pub(crate) fn adjust_other_split_cursors_for_event(&mut self, event: &Event) {
        // Handle BulkEdit - cursors are managed by the event
        if let Event::BulkEdit { new_cursors, .. } = event {
            // Get the current buffer and split
            let current_buffer_id = self.active_buffer();
            let current_split_id = self.split_manager.active_split();

            // Find all other splits that share the same buffer
            let splits_for_buffer = self.split_manager.splits_for_buffer(current_buffer_id);

            // Get buffer length to clamp cursor positions
            let buffer_len = self
                .buffers
                .get(&current_buffer_id)
                .map(|s| s.buffer.len())
                .unwrap_or(0);

            // Reset cursors in each other split to primary cursor position
            for split_id in splits_for_buffer {
                if split_id == current_split_id {
                    continue;
                }

                if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                    // Use the primary cursor position from the event
                    if let Some((_, pos, _)) = new_cursors.first() {
                        let new_pos = (*pos).min(buffer_len);
                        view_state.cursors.primary_mut().position = new_pos;
                        view_state.cursors.primary_mut().anchor = None;
                    }
                }
            }
            return;
        }

        // Find the edit parameters from the event
        let adjustments = match event {
            Event::Insert { position, text, .. } => {
                vec![(*position, 0, text.len())]
            }
            Event::Delete { range, .. } => {
                vec![(range.start, range.len(), 0)]
            }
            Event::Batch { events, .. } => {
                // Collect all edits from the batch
                events
                    .iter()
                    .filter_map(|e| match e {
                        Event::Insert { position, text, .. } => Some((*position, 0, text.len())),
                        Event::Delete { range, .. } => Some((range.start, range.len(), 0)),
                        _ => None,
                    })
                    .collect()
            }
            _ => vec![],
        };

        if adjustments.is_empty() {
            return;
        }

        // Get the current buffer and split
        let current_buffer_id = self.active_buffer();
        let current_split_id = self.split_manager.active_split();

        // Find all other splits that share the same buffer
        let splits_for_buffer = self.split_manager.splits_for_buffer(current_buffer_id);

        // Adjust cursors in each other split's view state
        for split_id in splits_for_buffer {
            if split_id == current_split_id {
                continue; // Skip the current split (already adjusted by BufferState::apply)
            }

            if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                for (edit_pos, old_len, new_len) in &adjustments {
                    view_state
                        .cursors
                        .adjust_for_edit(*edit_pos, *old_len, *new_len);
                }
            }
        }
    }

    /// Adjust the size of the active split
    pub fn adjust_split_size(&mut self, delta: f32) {
        let active_split = self.split_manager.active_split();
        if let Some(container) = self.split_manager.parent_container_of(active_split) {
            self.split_manager.adjust_ratio(container, delta);

            let percent = (delta * 100.0) as i32;
            self.set_status_message(t!("split.size_adjusted", percent = percent).to_string());
            // Resize visible terminals to match new split dimensions
            self.resize_visible_terminals();
        }
    }

    /// Toggle maximize state for the active split
    pub fn toggle_maximize_split(&mut self) {
        match self.split_manager.toggle_maximize() {
            Ok(maximized) => {
                if maximized {
                    self.set_status_message(t!("split.maximized").to_string());
                } else {
                    self.set_status_message(t!("split.restored").to_string());
                }
                // Resize visible terminals to match new split dimensions
                self.resize_visible_terminals();
            }
            Err(e) => self.set_status_message(e),
        }
    }

    /// Get cached separator areas for testing
    /// Returns (split_id, direction, x, y, length) tuples
    pub fn get_separator_areas(&self) -> &[(ContainerId, SplitDirection, u16, u16, u16)] {
        &self.cached_layout.separator_areas
    }

    /// Get cached tab layouts for testing
    pub fn get_tab_layouts(
        &self,
    ) -> &std::collections::HashMap<LeafId, crate::view::ui::tabs::TabLayout> {
        &self.cached_layout.tab_layouts
    }

    /// Get cached split content areas for testing
    /// Returns (split_id, buffer_id, content_rect, scrollbar_rect, thumb_start, thumb_end) tuples
    pub fn get_split_areas(
        &self,
    ) -> &[(
        LeafId,
        BufferId,
        ratatui::layout::Rect,
        ratatui::layout::Rect,
        usize,
        usize,
    )] {
        &self.cached_layout.split_areas
    }

    /// Get the ratio of a specific split (for testing).
    ///
    /// Looks in the main split tree first, then falls back to splits
    /// that live inside stashed Grouped subtrees (buffer-group panels).
    pub fn get_split_ratio(&self, split_id: SplitId) -> Option<f32> {
        self.split_manager
            .get_ratio(split_id)
            .or_else(|| self.grouped_split_ratio(crate::model::event::ContainerId(split_id)))
    }

    /// Get the active split ID (for testing)
    pub fn get_active_split(&self) -> LeafId {
        self.split_manager.active_split()
    }

    /// Get the buffer ID for a split (for testing)
    pub fn get_split_buffer(&self, split_id: SplitId) -> Option<BufferId> {
        self.split_manager.get_buffer_id(split_id)
    }

    /// Get the open buffers (tabs) in a split (for testing)
    pub fn get_split_tabs(&self, split_id: LeafId) -> Vec<BufferId> {
        self.split_view_states
            .get(&split_id)
            .map(|vs| vs.buffer_tab_ids_vec())
            .unwrap_or_default()
    }

    /// Get the number of splits (for testing)
    pub fn get_split_count(&self) -> usize {
        self.split_manager.root().count_leaves()
    }

    /// Compute the drop zone for a tab drag at a given position (for testing)
    pub fn compute_drop_zone(
        &self,
        col: u16,
        row: u16,
        source_split_id: LeafId,
    ) -> Option<super::types::TabDropZone> {
        self.compute_tab_drop_zone(col, row, source_split_id)
    }
}
