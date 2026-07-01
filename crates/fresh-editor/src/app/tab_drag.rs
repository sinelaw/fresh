//! Tab drag-and-drop functionality
//!
//! This module handles tab drag operations including:
//! - Detecting drop zones during drag
//! - Reordering tabs within a split
//! - Moving tabs between splits
//! - Creating new splits from dropped tabs

use super::types::TabDropZone;
use super::Editor;
use crate::model::event::{BufferId, LeafId, SplitDirection};
use crate::view::ui::tabs::TabHit;
use anyhow::Result as AnyhowResult;
use rust_i18n::t;

impl Editor {
    /// Handle tab drag - update position and compute drop zone
    pub(super) fn handle_tab_drag(&mut self, col: u16, row: u16) -> AnyhowResult<()> {
        // Update current position and check if we're dragging
        let (is_dragging, source_split_id) =
            if let Some(ref mut drag_state) = self.active_window_mut().mouse_state.dragging_tab {
                drag_state.current_position = (col, row);
                (drag_state.is_dragging(), drag_state.source_split_id)
            } else {
                return Ok(());
            };

        // Only compute drop zone if we've moved past threshold
        if !is_dragging {
            if let Some(ref mut drag_state) = self.active_window_mut().mouse_state.dragging_tab {
                drag_state.drop_zone = None;
            }
            return Ok(());
        }

        // Compute the drop zone based on mouse position
        let drop_zone = self.compute_tab_drop_zone(col, row, source_split_id);
        if let Some(ref mut drag_state) = self.active_window_mut().mouse_state.dragging_tab {
            drag_state.drop_zone = drop_zone;
        }

        Ok(())
    }

    /// Compute the drop zone for a tab being dragged
    pub(super) fn compute_tab_drop_zone(
        &self,
        col: u16,
        row: u16,
        source_split_id: LeafId,
    ) -> Option<TabDropZone> {
        // First check if we're over a tab bar (for reordering/moving to another split)
        for (split_id, tab_layout) in &self.active_layout().tab_layouts {
            if matches!(
                tab_layout.hit_test(col, row),
                Some(TabHit::TabName(_) | TabHit::CloseButton(_))
            ) {
                // Find the index where this tab would be inserted
                let insert_idx = self.find_tab_insert_index(*split_id, col);
                return Some(TabDropZone::TabBar(*split_id, insert_idx));
            }
        }

        // Check if we're in the tab row area of any split (for moving to end of tab bar)
        for (split_id, _buffer_id, content_rect, _scrollbar_rect, _thumb_start, _thumb_end) in
            &self.active_layout().split_areas
        {
            // The tab row is typically at content_rect.y - 1 (assuming 1 row for tabs)
            let tab_row = content_rect.y.saturating_sub(1);
            if row == tab_row && col >= content_rect.x && col < content_rect.x + content_rect.width
            {
                return Some(TabDropZone::TabBar(*split_id, None));
            }
        }

        // Check if we're over a split content area for edge-based splitting
        for (split_id, _buffer_id, content_rect, _scrollbar_rect, _thumb_start, _thumb_end) in
            &self.active_layout().split_areas
        {
            if col >= content_rect.x
                && col < content_rect.x + content_rect.width
                && row >= content_rect.y
                && row < content_rect.y + content_rect.height
            {
                // Calculate the edge zones (each edge takes 25% of the dimension)
                let width = content_rect.width as f32;
                let height = content_rect.height as f32;
                let edge_threshold_x = (width * 0.25).max(3.0) as u16;
                let edge_threshold_y = (height * 0.25).max(2.0) as u16;

                let rel_x = col - content_rect.x;
                let rel_y = row - content_rect.y;

                // Determine which zone we're in (priority: edges, then center)
                // Left edge
                if rel_x < edge_threshold_x {
                    return Some(TabDropZone::SplitLeft(*split_id));
                }
                // Right edge
                if rel_x >= content_rect.width - edge_threshold_x {
                    return Some(TabDropZone::SplitRight(*split_id));
                }
                // Top edge
                if rel_y < edge_threshold_y {
                    return Some(TabDropZone::SplitTop(*split_id));
                }
                // Bottom edge
                if rel_y >= content_rect.height - edge_threshold_y {
                    return Some(TabDropZone::SplitBottom(*split_id));
                }

                // Center - only allow if different from source split
                if *split_id != source_split_id {
                    return Some(TabDropZone::SplitCenter(*split_id));
                }
            }
        }

        None
    }

    /// Find the index where a tab should be inserted based on mouse x position
    fn find_tab_insert_index(&self, split_id: LeafId, col: u16) -> Option<usize> {
        // Get the tab layout for this split
        let tab_layout = self.active_layout().tab_layouts.get(&split_id)?;

        if tab_layout.tabs.is_empty() {
            return Some(0);
        }

        // Find the tab we're over and determine if we're in the left or right half
        for (idx, tab_hit) in tab_layout.tabs.iter().enumerate() {
            let start_col = tab_hit.tab_area.x;
            let end_col = start_col + tab_hit.tab_area.width;
            if col >= start_col && col < end_col {
                let mid = (start_col + end_col) / 2;
                if col < mid {
                    return Some(idx);
                } else {
                    return Some(idx + 1);
                }
            }
        }

        // If past all tabs, insert at end
        Some(tab_layout.tabs.len())
    }

    /// Execute a tab drop action
    pub(super) fn execute_tab_drop(
        &mut self,
        buffer_id: BufferId,
        source_split_id: LeafId,
        drop_zone: TabDropZone,
    ) {
        // Dropping a tab (reorder, move to another split, or create a new
        // split from it) is an unambiguous commitment gesture — promote any
        // preview first so a drag never leaves behind stale preview state
        // anchored to a split that has changed underneath it.
        self.active_window_mut().promote_current_preview();

        match drop_zone {
            TabDropZone::TabBar(target_split_id, insert_idx) => {
                if target_split_id == source_split_id {
                    // Reordering within the same split
                    self.reorder_tab_in_split(buffer_id, source_split_id, insert_idx);
                } else {
                    // Moving to a different split's tab bar
                    self.move_tab_to_split(buffer_id, source_split_id, target_split_id, insert_idx);
                }
            }
            TabDropZone::SplitLeft(target_split_id) => {
                self.create_split_from_tab(
                    buffer_id,
                    source_split_id,
                    target_split_id,
                    SplitDirection::Vertical,
                    true, // new split on left
                );
            }
            TabDropZone::SplitRight(target_split_id) => {
                self.create_split_from_tab(
                    buffer_id,
                    source_split_id,
                    target_split_id,
                    SplitDirection::Vertical,
                    false, // new split on right
                );
            }
            TabDropZone::SplitTop(target_split_id) => {
                self.create_split_from_tab(
                    buffer_id,
                    source_split_id,
                    target_split_id,
                    SplitDirection::Horizontal,
                    true, // new split on top
                );
            }
            TabDropZone::SplitBottom(target_split_id) => {
                self.create_split_from_tab(
                    buffer_id,
                    source_split_id,
                    target_split_id,
                    SplitDirection::Horizontal,
                    false, // new split on bottom
                );
            }
            TabDropZone::SplitCenter(target_split_id) => {
                // Move tab to target split's tab bar (at end)
                self.move_tab_to_split(buffer_id, source_split_id, target_split_id, None);
            }
        }
    }

    /// Reorder a tab within the same split
    fn reorder_tab_in_split(
        &mut self,
        buffer_id: BufferId,
        split_id: LeafId,
        insert_idx: Option<usize>,
    ) {
        use crate::view::split::TabTarget;
        if let Some(view_state) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .get_mut(&split_id)
        {
            let target = TabTarget::Buffer(buffer_id);
            // Find current position of the buffer
            if let Some(current_idx) = view_state.open_buffers.iter().position(|t| *t == target) {
                // Remove from current position
                view_state.open_buffers.remove(current_idx);

                // Insert at new position
                let target_idx = insert_idx.unwrap_or(view_state.open_buffers.len());
                // Adjust for the removal if necessary
                let adjusted_idx = if current_idx < target_idx {
                    target_idx.saturating_sub(1)
                } else {
                    target_idx
                };
                let final_idx = adjusted_idx.min(view_state.open_buffers.len());
                view_state.open_buffers.insert(final_idx, target);
            }
        }
    }

    /// Move a tab from one split to another
    fn move_tab_to_split(
        &mut self,
        buffer_id: BufferId,
        source_split_id: LeafId,
        target_split_id: LeafId,
        insert_idx: Option<usize>,
    ) {
        use crate::view::split::TabTarget;
        // Check if source split will be empty after removing this buffer
        let source_becomes_empty = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&source_split_id)
            .map(|vs| vs.open_buffers.len() == 1 && vs.has_buffer(buffer_id))
            .unwrap_or(false);

        // If the source leaf is being absorbed (its last tab is moving
        // out) and it carries a SplitRole, the role transfers to the
        // target leaf so the user can physically relocate the dock by
        // dragging out its only tab. This implements the
        // "role follows the window" rule from
        // docs/internal/tui-editor-layout-design.md Section 2.
        let role_to_transfer = if source_becomes_empty {
            self.windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_manager_mut())
                .expect("active window must have a populated split layout")
                .root()
                .find(source_split_id.into())
                .and_then(|n| n.role())
        } else {
            None
        };

        // Compute decisions UP FRONT so we can hold a single mutable
        // borrow on the source view state without re-borrowing windows.
        let active_id = self.active_window;
        let source_showed_buffer =
            self.split_manager().get_buffer_id(source_split_id.into()) == Some(buffer_id);
        let mut next_buffer_for_source: Option<BufferId> = None;
        // Remove from source split's tab bar
        if let Some((mgr, vs)) = self
            .windows
            .get_mut(&active_id)
            .and_then(|w| w.buffers.splits_mut())
        {
            if let Some(source_view_state) = vs.get_mut(&source_split_id) {
                source_view_state
                    .open_buffers
                    .retain(|t| *t != TabTarget::Buffer(buffer_id));

                if source_showed_buffer {
                    next_buffer_for_source = source_view_state.buffer_tab_ids().next();
                }
            }
            if let Some(next_buffer) = next_buffer_for_source {
                mgr.set_split_buffer(source_split_id, next_buffer);
            }
        }

        // Add to target split's tab bar
        if let Some(target_view_state) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .get_mut(&target_split_id)
        {
            // Don't add duplicate
            if !target_view_state.has_buffer(buffer_id) {
                let idx = insert_idx.unwrap_or(target_view_state.open_buffers.len());
                let final_idx = idx.min(target_view_state.open_buffers.len());
                target_view_state
                    .open_buffers
                    .insert(final_idx, TabTarget::Buffer(buffer_id));
            }
        }

        // Focus the target split and switch to the dropped buffer.
        //
        // Use `set_pane_buffer` (not raw `set_split_buffer`) so the target
        // split's `SplitViewState` gets a `keyed_states` entry for the
        // dropped buffer. Without this, dragging a tab into a split that
        // never hosted that buffer (e.g. a search-replace results tab
        // dragged out of the utility dock onto another split) leaves the
        // target SVS missing the entry. `set_active_buffer` below would
        // then early-return because the split tree already reports
        // `buffer_id` as active, so it never reaches `set_pane_buffer`
        // either — and the next keystroke panics in
        // `apply_event_to_state` on `keyed_states.get_mut(...).unwrap()`.
        self.active_window_mut()
            .set_pane_buffer(target_split_id, buffer_id);
        self.windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .set_active_split(target_split_id);
        self.set_active_buffer(buffer_id);

        // If source split is now empty, close it
        if source_becomes_empty {
            self.windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_view_states_mut())
                .expect("active window must have a populated split layout")
                .remove(&source_split_id);
            if let Err(e) = self
                .windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_manager_mut())
                .expect("active window must have a populated split layout")
                .close_split(source_split_id)
            {
                tracing::warn!("Failed to close empty split: {}", e);
            }
            // Transfer the absorbed leaf's role to the destination so
            // utility-dock placement follows the window the user just
            // moved into.
            if let Some(role) = role_to_transfer {
                self.windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_manager_mut())
                    .expect("active window must have a populated split layout")
                    .clear_role(role);
                self.windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_manager_mut())
                    .expect("active window must have a populated split layout")
                    .set_leaf_role(target_split_id, Some(role));
                tracing::info!(
                    "Transferred role {:?} from absorbed leaf {:?} to {:?}",
                    role,
                    source_split_id,
                    target_split_id
                );
            }
            self.set_status_message(t!("status.moved_tab_split_closed").to_string());
        } else {
            self.set_status_message(t!("status.moved_tab").to_string());
        }
    }

    /// Create a new split from a dropped tab
    fn create_split_from_tab(
        &mut self,
        buffer_id: BufferId,
        source_split_id: LeafId,
        target_split_id: LeafId,
        direction: SplitDirection,
        _new_split_first: bool, // If true, new split is placed first (left/top) - TODO: implement
    ) {
        use crate::view::split::TabTarget;
        // Check if source split will be empty after removing this buffer
        let source_becomes_empty = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&source_split_id)
            .map(|vs| vs.open_buffers.len() == 1 && vs.has_buffer(buffer_id))
            .unwrap_or(false);

        // Decide buffer-switching UP FRONT under an immutable borrow,
        // then take one mutable borrow on the active window's splits to
        // perform the actual edits without re-borrowing windows.
        let active_id = self.active_window;
        let source_showed_buffer =
            self.split_manager().get_buffer_id(source_split_id.into()) == Some(buffer_id);
        let mut next_buffer_for_source: Option<BufferId> = None;
        let source_had_buffer = if let Some((mgr, vs)) = self
            .windows
            .get_mut(&active_id)
            .and_then(|w| w.buffers.splits_mut())
        {
            let had = if let Some(source_view_state) = vs.get_mut(&source_split_id) {
                let had = source_view_state.has_buffer(buffer_id);
                source_view_state
                    .open_buffers
                    .retain(|t| *t != TabTarget::Buffer(buffer_id));

                if source_showed_buffer {
                    next_buffer_for_source = source_view_state.buffer_tab_ids().next();
                }
                had
            } else {
                false
            };
            if let Some(next_buffer) = next_buffer_for_source {
                mgr.set_split_buffer(source_split_id, next_buffer);
            }
            had
        } else {
            false
        };

        if !source_had_buffer {
            return;
        }

        // Create new split - we need to split the target split
        // First, temporarily set the target split as active
        let original_active = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();
        self.windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .set_active_split(target_split_id);

        // Determine the ratio (new split gets 50%)
        let ratio = 0.5;

        // Create the split
        match self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .split_active(direction, buffer_id, ratio)
        {
            Ok(new_split_id) => {
                // Initialize the new split's view state
                let (width, height) = (self.terminal_width, self.terminal_height);
                let mut new_view_state =
                    crate::view::split::SplitViewState::with_buffer(width, height, buffer_id);
                new_view_state.apply_config_defaults(crate::view::split::ViewConfigDefaults {
                    line_numbers: self.config.editor.line_numbers,
                    highlight_current_line: self.config.editor.highlight_current_line,
                    line_wrap: self.active_window().resolve_line_wrap_for_buffer(buffer_id),
                    wrap_indent: self.config.editor.wrap_indent,
                    wrap_column: self
                        .active_window()
                        .resolve_wrap_column_for_buffer(buffer_id),
                    rulers: self.config.editor.rulers.clone(),
                    scroll_offset: self.config.editor.scroll_offset,
                });

                // Copy cursor position from source split's view state
                if let Some(source_vs) = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(_, vs)| vs)
                    .expect("active window must have a populated split layout")
                    .get(&source_split_id)
                {
                    new_view_state.cursors = source_vs.cursors.clone();
                }

                self.windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_view_states_mut())
                    .expect("active window must have a populated split layout")
                    .insert(new_split_id, new_view_state);

                // If new_split_first is true, we need to swap the children
                // This requires modifying the split manager's tree structure
                // For now, we accept the default behavior (new split is second)
                // TODO: Implement swap_split_children for new_split_first=true

                // If source split is now empty, close it
                if source_becomes_empty {
                    self.windows
                        .get_mut(&self.active_window)
                        .and_then(|w| w.split_view_states_mut())
                        .expect("active window must have a populated split layout")
                        .remove(&source_split_id);
                    if let Err(e) = self
                        .windows
                        .get_mut(&self.active_window)
                        .and_then(|w| w.split_manager_mut())
                        .expect("active window must have a populated split layout")
                        .close_split(source_split_id)
                    {
                        tracing::warn!("Failed to close empty split: {}", e);
                    }
                }

                // Focus the new split
                self.windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_manager_mut())
                    .expect("active window must have a populated split layout")
                    .set_active_split(new_split_id);
                self.set_active_buffer(buffer_id);

                self.set_status_message(t!("status.created_new_split").to_string());
            }
            Err(e) => {
                // Restore active split on error
                self.windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_manager_mut())
                    .expect("active window must have a populated split layout")
                    .set_active_split(original_active);
                self.set_status_message(
                    t!("error.split_failed", error = e.to_string()).to_string(),
                );
            }
        }
    }
}
