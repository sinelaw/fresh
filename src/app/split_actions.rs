//! Split/pane management for the Editor.
//!
//! This module contains all methods related to managing editor splits:
//! - Creating horizontal/vertical splits
//! - Closing splits
//! - Navigating between splits
//! - Managing per-split view states (cursors, viewport)
//! - Split size adjustment and maximize

use crate::model::event::{Event, SplitDirection, SplitId};
use crate::view::split::SplitViewState;

use super::Editor;

impl Editor {
    /// Split the current pane horizontally
    pub fn split_pane_horizontal(&mut self) {
        // Save current split's view state before creating a new one
        self.save_current_split_view_state();

        // Share the current buffer with the new split (Emacs-style)
        let current_buffer_id = self.active_buffer();

        // Split the pane
        match self.split_manager.split_active(
            crate::model::event::SplitDirection::Horizontal,
            current_buffer_id,
            0.5,
        ) {
            Ok(new_split_id) => {
                // Create independent view state for the new split with the current buffer
                let mut view_state = SplitViewState::with_buffer(
                    self.terminal_width,
                    self.terminal_height,
                    current_buffer_id,
                );
                view_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
                self.split_view_states.insert(new_split_id, view_state);
                // Restore the new split's view state to the buffer
                self.restore_current_split_view_state();
                self.set_status_message("Split pane horizontally".to_string());
            }
            Err(e) => {
                self.set_status_message(format!("Error splitting pane: {}", e));
            }
        }
    }

    /// Split the current pane vertically
    pub fn split_pane_vertical(&mut self) {
        // Save current split's view state before creating a new one
        self.save_current_split_view_state();

        // Share the current buffer with the new split (Emacs-style)
        let current_buffer_id = self.active_buffer();

        // Split the pane
        match self.split_manager.split_active(
            crate::model::event::SplitDirection::Vertical,
            current_buffer_id,
            0.5,
        ) {
            Ok(new_split_id) => {
                // Create independent view state for the new split with the current buffer
                let mut view_state = SplitViewState::with_buffer(
                    self.terminal_width,
                    self.terminal_height,
                    current_buffer_id,
                );
                view_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
                self.split_view_states.insert(new_split_id, view_state);
                // Restore the new split's view state to the buffer
                self.restore_current_split_view_state();
                self.set_status_message("Split pane vertically".to_string());
            }
            Err(e) => {
                self.set_status_message(format!("Error splitting pane: {}", e));
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

                // Sync the view state to editor state
                self.sync_split_view_state_to_editor_state();

                self.set_status_message("Closed split".to_string());
            }
            Err(e) => {
                self.set_status_message(format!("Cannot close split: {}", e));
            }
        }
    }

    /// Switch to next split
    pub fn next_split(&mut self) {
        self.switch_split(true);
        self.set_status_message("Switched to next split".to_string());
    }

    /// Switch to previous split
    pub fn prev_split(&mut self) {
        self.switch_split(false);
        self.set_status_message("Switched to previous split".to_string());
    }

    /// Common split switching logic
    fn switch_split(&mut self, next: bool) {
        self.save_current_split_view_state();
        if next {
            self.split_manager.next_split();
        } else {
            self.split_manager.prev_split();
        }
        self.restore_current_split_view_state();
        // Enter terminal mode if switching to a terminal split
        if self.is_terminal_buffer(self.active_buffer()) {
            self.terminal_mode = true;
            self.key_context = crate::input::keybindings::KeyContext::Terminal;
        }
    }

    /// Save the current split's cursor state (viewport is owned by SplitViewState)
    pub(crate) fn save_current_split_view_state(&mut self) {
        let split_id = self.split_manager.active_split();
        if let Some(buffer_state) = self.buffers.get(&self.active_buffer()) {
            if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                view_state.cursors = buffer_state.cursors.clone();
                // Note: viewport is now owned by SplitViewState, no sync needed
            }
        }
    }

    /// Restore the current split's cursor state (viewport is owned by SplitViewState)
    pub(crate) fn restore_current_split_view_state(&mut self) {
        let split_id = self.split_manager.active_split();
        // NOTE: active_buffer is now derived from split_manager, no sync needed
        // Restore cursor from split view state (viewport stays in SplitViewState)
        self.sync_split_view_state_to_editor_state();
        // Ensure the active tab is visible in the newly active split
        // Use effective_tabs_width() to account for file explorer taking 30% of width
        self.ensure_active_tab_visible(split_id, self.active_buffer(), self.effective_tabs_width());
    }

    /// Sync SplitViewState's cursors to EditorState
    /// Called when switching splits to restore the split's cursor state
    /// Note: Viewport is now owned by SplitViewState, not synced to EditorState
    pub(crate) fn sync_split_view_state_to_editor_state(&mut self) {
        let split_id = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get(&split_id) {
            if let Some(buffer_state) = self.buffers.get_mut(&self.active_buffer()) {
                buffer_state.cursors = view_state.cursors.clone();
                // Note: viewport is now owned by SplitViewState, no sync needed
            }
        }
    }

    /// Adjust cursors in other splits that share the same buffer after an edit
    pub(crate) fn adjust_other_split_cursors_for_event(&mut self, event: &Event) {
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
            self.set_status_message(format!("Cannot adjust split size: {}", e));
        } else {
            self.set_status_message(format!("Adjusted split size by {:.0}%", delta * 100.0));
            // Resize visible terminals to match new split dimensions
            self.resize_visible_terminals();
        }
    }

    /// Toggle maximize state for the active split
    pub fn toggle_maximize_split(&mut self) {
        match self.split_manager.toggle_maximize() {
            Ok(maximized) => {
                if maximized {
                    self.set_status_message("Maximized split".to_string());
                } else {
                    self.set_status_message("Restored all splits".to_string());
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

    /// Get the ratio of a specific split (for testing)
    pub fn get_split_ratio(&self, split_id: SplitId) -> Option<f32> {
        self.split_manager.get_ratio(split_id)
    }

    /// Sync EditorState's cursors back to SplitViewState
    ///
    /// This keeps SplitViewState's cursor state in sync with EditorState after
    /// events are applied. This is necessary because cursor events (cursor
    /// movements, edits) still update EditorState.cursors directly.
    /// Note: Viewport is now owned by SplitViewState, no sync needed.
    pub(crate) fn sync_editor_state_to_split_view_state(&mut self) {
        let split_id = self.split_manager.active_split();
        if let Some(buffer_state) = self.buffers.get(&self.active_buffer()) {
            if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                view_state.cursors = buffer_state.cursors.clone();
                // Note: viewport is now owned by SplitViewState, no sync needed
            }
        }
    }
}
