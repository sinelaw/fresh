//! Split/pane management for the Editor.
//!
//! This module contains all methods related to managing editor splits:
//! - Creating horizontal/vertical splits
//! - Closing splits
//! - Navigating between splits
//! - Managing per-split view states (cursors, viewport)
//! - Split size adjustment and maximize

use rust_i18n::t;

use crate::model::event::{BufferId, ContainerId, LeafId, SplitDirection, SplitId};
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
        self.active_window_mut().promote_current_preview();

        let current_buffer_id = self.active_buffer();
        let active_split = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();

        // Copy keyed states from source split so the new split inherits per-buffer state
        let source_keyed_states = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&active_split)
            .map(|vs| {
                vs.keyed_states
                    .iter()
                    .filter(|(&buf_id, _)| buf_id != current_buffer_id)
                    .map(|(&buf_id, buf_state)| {
                        let folds = self
                            .buffers()
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
            .split_manager_mut()
            .split_active(direction, current_buffer_id, 0.5)
        {
            Ok(new_split_id) => {
                let mut view_state = SplitViewState::with_buffer(
                    self.terminal_width,
                    self.terminal_height,
                    current_buffer_id,
                );
                view_state.apply_config_defaults(crate::view::split::ViewConfigDefaults {
                    line_numbers: self.config.editor.line_numbers,
                    highlight_current_line: self.config.editor.highlight_current_line,
                    line_wrap: self
                        .active_window()
                        .resolve_line_wrap_for_buffer(current_buffer_id),
                    wrap_indent: self.config.editor.wrap_indent,
                    wrap_column: self
                        .active_window()
                        .resolve_wrap_column_for_buffer(current_buffer_id),
                    rulers: self.config.editor.rulers.clone(),
                    scroll_offset: self.config.editor.scroll_offset,
                });

                // Copy keyed states from source split for OTHER buffers (not the active one).
                // The active buffer gets a fresh cursor in the new split.
                if let Some(source) = source_keyed_states {
                    for (buf_id, mut buf_state, folds) in source {
                        if let Some(state) = self
                            .windows
                            .get_mut(&self.active_window)
                            .map(|w| &mut w.buffers)
                            .expect("active window present")
                            .get_mut(&buf_id)
                        {
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

                self.windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_view_states_mut())
                    .expect("active window must have a populated split layout")
                    .insert(new_split_id, view_state);
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

        // A new split changes every sibling pane's width/height. Reflow
        // through the single layout funnel so existing terminals shrink to
        // their new pane immediately, instead of waiting for the next
        // unrelated resize trigger.
        self.relayout();
    }

    /// Close the active split
    pub fn close_active_split(&mut self) {
        // Closing a split rearranges tab ownership (remaining tabs migrate
        // to the new active split). Promote any preview first so it doesn't
        // end up orphaned in a split that no longer exists, or silently
        // migrated to an unrelated pane.
        self.active_window_mut().promote_current_preview();

        let closing_split = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();

        // Get the tabs from the split we're closing before we close it
        let closing_split_tabs = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&closing_split)
            .map(|vs| vs.open_buffers.clone())
            .unwrap_or_default();

        match self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .close_split(closing_split)
        {
            Ok(_) => {
                // Clean up the view state for the closed split
                self.windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_view_states_mut())
                    .expect("active window must have a populated split layout")
                    .remove(&closing_split);

                // Get the new active split after closing
                let new_active_split = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .active_split();

                // Transfer tabs from closed split to the new active split
                if let Some(view_state) = self
                    .windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_view_states_mut())
                    .expect("active window must have a populated split layout")
                    .get_mut(&new_active_split)
                {
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

        // Focus snapped to the surviving split through the split manager,
        // bypassing the buffer-focus path — restore terminal mode so a
        // re-focused terminal keeps the mode it remembers (issue #2485).
        self.sync_terminal_mode_to_active_buffer();

        // Closing a split gives its space back to the surviving panes.
        // Reflow through the single layout funnel so their terminals grow
        // into the reclaimed area.
        self.relayout();
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
        // `next_split`/`prev_split` auto-unmaximize so the newly-active
        // split is visible (issue #1961). Detect that here so terminal
        // PTYs can be resized to match the restored layout.
        let was_maximized = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr.is_maximized())
            .unwrap_or(false);

        if next {
            self.windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_manager_mut())
                .expect("active window must have a populated split layout")
                .next_split();
        } else {
            self.windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_manager_mut())
                .expect("active window must have a populated split layout")
                .prev_split();
        }

        if was_maximized {
            self.relayout();
        }

        // Ensure the active tab is visible in the newly active split
        let split_id = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();
        // Moving focus to a different split commits the preview — walking
        // away is commitment. Matches the rule applied in `focus_split`.
        self.active_window_mut()
            .promote_preview_if_not_in_split(split_id);
        let buffer = self.active_buffer();
        let tabs_width = self.active_window().effective_tabs_width();
        self.active_window_mut()
            .ensure_active_tab_visible(split_id, buffer, tabs_width);

        let buffer_id = self.active_buffer();

        // Bring terminal mode in line with the newly focused split: a
        // terminal resumes the live/scrollback mode it remembers, a
        // non-terminal clears terminal mode. Single restore authority.
        self.sync_terminal_mode_to_active_buffer();

        // Emit buffer_activated hook for plugins
        self.plugin_manager.read().unwrap().run_hook(
            "buffer_activated",
            crate::services::plugins::hooks::HookArgs::BufferActivated { buffer_id },
        );
    }

    /// Adjust the size of the active split
    pub fn adjust_split_size(&mut self, delta: f32) {
        let active_split = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();
        if let Some(container) = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .parent_container_of(active_split)
        {
            self.windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_manager_mut())
                .expect("active window must have a populated split layout")
                .adjust_ratio(container, delta);

            let percent = (delta * 100.0) as i32;
            self.set_status_message(t!("split.size_adjusted", percent = percent).to_string());
            // Split ratios changed: reflow through the single layout funnel.
            self.relayout();
        }
    }

    /// Toggle maximize state for the active split
    pub fn toggle_maximize_split(&mut self) {
        match self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .toggle_maximize()
        {
            Ok(maximized) => {
                if maximized {
                    self.set_status_message(t!("split.maximized").to_string());
                } else {
                    self.set_status_message(t!("split.restored").to_string());
                }
                // Maximize/restore changed the split sizes: reflow via funnel.
                self.relayout();
            }
            Err(e) => self.set_status_message(e),
        }
    }

    /// Get cached separator areas for testing
    /// Returns (split_id, direction, x, y, length) tuples
    pub fn get_separator_areas(&self) -> &[(ContainerId, SplitDirection, u16, u16, u16)] {
        &self.active_layout().separator_areas
    }

    /// Get cached tab layouts for testing
    pub fn get_tab_layouts(
        &self,
    ) -> &std::collections::HashMap<LeafId, crate::view::ui::tabs::TabLayout> {
        &self.active_layout().tab_layouts
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
        &self.active_layout().split_areas
    }

    /// Get the ratio of a specific split (for testing).
    ///
    /// Looks in the main split tree first, then falls back to splits
    /// that live inside stashed Grouped subtrees (buffer-group panels).
    pub fn get_split_ratio(&self, split_id: SplitId) -> Option<f32> {
        self.windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .get_ratio(split_id)
            .or_else(|| self.grouped_split_ratio(crate::model::event::ContainerId(split_id)))
    }

    /// Get the active split ID (for testing)
    pub fn get_active_split(&self) -> LeafId {
        self.windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split()
    }

    /// Get the buffer ID for a split (for testing)
    pub fn get_split_buffer(&self, split_id: SplitId) -> Option<BufferId> {
        self.windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .get_buffer_id(split_id)
    }

    /// Get the open buffers (tabs) in a split (for testing)
    pub fn get_split_tabs(&self, split_id: LeafId) -> Vec<BufferId> {
        self.windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&split_id)
            .map(|vs| vs.buffer_tab_ids_vec())
            .unwrap_or_default()
    }

    /// Get the number of splits (for testing)
    pub fn get_split_count(&self) -> usize {
        self.windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .root()
            .count_leaves()
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
