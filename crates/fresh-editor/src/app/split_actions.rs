//! Split/pane management for the Editor.
//!
//! This module contains all methods related to managing editor splits:
//! - Creating horizontal/vertical splits
//! - Closing splits
//! - Navigating between splits
//! - Managing per-split view states (cursors, viewport)
//! - Split size adjustment and maximize

use rust_i18n::t;

use crate::model::event::{BufferId, Event, SplitDirection, SplitId};
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
        let current_buffer_id = self.active_buffer();
        let active_split = self.split_manager.active_split();

        // Copy keyed states from source split so the new split inherits per-buffer state
        let source_keyed_states = self
            .split_view_states
            .get(&active_split)
            .map(|vs| vs.keyed_states.clone());

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
                view_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
                view_state.rulers = self.config.editor.rulers.clone();
                view_state.show_line_numbers = self.config.editor.line_numbers;

                // Copy keyed states from source split for OTHER buffers (not the active one).
                // The active buffer gets a fresh cursor in the new split.
                if let Some(source) = source_keyed_states {
                    for (buf_id, buf_state) in source {
                        if buf_id != current_buffer_id {
                            view_state.keyed_states.insert(buf_id, buf_state);
                        }
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
                    for buffer_id in closing_split_tabs {
                        // Only add if not already in the split's tabs
                        if !view_state.open_buffers.contains(&buffer_id) {
                            view_state.open_buffers.push(buffer_id);
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
        if next {
            self.split_manager.next_split();
        } else {
            self.split_manager.prev_split();
        }

        // Ensure the active tab is visible in the newly active split
        let split_id = self.split_manager.active_split();
        self.ensure_active_tab_visible(split_id, self.active_buffer(), self.effective_tabs_width());

        let buffer_id = self.active_buffer();

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
        if let Err(e) = self.split_manager.adjust_ratio(active_split, delta) {
            self.set_status_message(t!("split.cannot_adjust", error = e).to_string());
        } else {
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
    pub fn get_separator_areas(&self) -> &[(SplitId, SplitDirection, u16, u16, u16)] {
        &self.cached_layout.separator_areas
    }

    /// Get cached tab layouts for testing
    pub fn get_tab_layouts(
        &self,
    ) -> &std::collections::HashMap<SplitId, crate::view::ui::tabs::TabLayout> {
        &self.cached_layout.tab_layouts
    }

    /// Get cached split content areas for testing
    /// Returns (split_id, buffer_id, content_rect, scrollbar_rect, thumb_start, thumb_end) tuples
    pub fn get_split_areas(
        &self,
    ) -> &[(
        SplitId,
        BufferId,
        ratatui::layout::Rect,
        ratatui::layout::Rect,
        usize,
        usize,
    )] {
        &self.cached_layout.split_areas
    }

    /// Get the ratio of a specific split (for testing)
    pub fn get_split_ratio(&self, split_id: SplitId) -> Option<f32> {
        self.split_manager.get_ratio(split_id)
    }

    /// Get the active split ID (for testing)
    pub fn get_active_split(&self) -> SplitId {
        self.split_manager.active_split()
    }

    /// Get the buffer ID for a split (for testing)
    pub fn get_split_buffer(&self, split_id: SplitId) -> Option<BufferId> {
        self.split_manager.get_buffer_id(split_id)
    }

    /// Get the open buffers (tabs) in a split (for testing)
    pub fn get_split_tabs(&self, split_id: SplitId) -> Vec<BufferId> {
        self.split_view_states
            .get(&split_id)
            .map(|vs| vs.open_buffers.clone())
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
        source_split_id: SplitId,
    ) -> Option<super::types::TabDropZone> {
        self.compute_tab_drop_zone(col, row, source_split_id)
    }
}
