//! Scroll-sync orchestrators on `Editor`.
//!
//! - `ensure_active_tab_visible` — adjusts a split's tab-bar scroll offset
//!   so the active tab is on screen.
//! - `sync_scroll_groups` — when splits share a scroll group (e.g. for
//!   side-by-side diffs), keep their viewports in lockstep.
//! - `pre_sync_ensure_visible` — pre-sync hook that ensures the active
//!   split's cursor is on screen so the scroll-group sync uses a valid
//!   anchor.

use crate::model::event::{BufferId, LeafId, SplitId};

use super::Editor;

impl Editor {
    /// Ensure the active tab in a split is visible by adjusting its scroll offset.
    /// This function recalculates the required scroll_offset based on the active tab's position
    /// and the available width, and updates the SplitViewState.
    pub(super) fn ensure_active_tab_visible(
        &mut self,
        split_id: LeafId,
        active_buffer: BufferId,
        available_width: u16,
    ) {
        tracing::debug!(
            "ensure_active_tab_visible called: split={:?}, buffer={:?}, width={}",
            split_id,
            active_buffer,
            available_width
        );
        let Some(view_state) = self.split_view_states.get_mut(&split_id) else {
            tracing::debug!("  -> no view_state for split");
            return;
        };

        let split_buffers = view_state.open_buffers.clone();
        // Collect group names from the stashed Grouped subtrees.
        let group_names: std::collections::HashMap<LeafId, String> = self
            .grouped_subtrees
            .iter()
            .filter_map(|(leaf_id, node)| {
                if let crate::view::split::SplitNode::Grouped { name, .. } = node {
                    Some((*leaf_id, name.clone()))
                } else {
                    None
                }
            })
            .collect();

        // Use the shared function to calculate tab widths (same as render_for_split)
        let (tab_widths, rendered_targets) = crate::view::ui::tabs::calculate_tab_widths(
            &split_buffers,
            &self.buffers,
            &self.buffer_metadata,
            &self.composite_buffers,
            &group_names,
        );

        let total_tabs_width: usize = tab_widths.iter().sum();
        let max_visible_width = available_width as usize;

        // Determine the active target from the SplitViewState marker.
        let active_target = view_state.active_target();
        // If the caller passed an explicit buffer_id and the split doesn't
        // have a group marked active, use that buffer as the target.
        let active_target = if matches!(active_target, crate::view::split::TabTarget::Buffer(_)) {
            crate::view::split::TabTarget::Buffer(active_buffer)
        } else {
            active_target
        };

        // Find the active tab index among rendered targets
        // Note: tab_widths includes separators, so we need to map tab index to width index
        let active_tab_index = rendered_targets.iter().position(|t| *t == active_target);

        // Map buffer index to width index (accounting for separators)
        // Widths are: [sep?, tab0, sep, tab1, sep, tab2, ...]
        // First tab has no separator before it, subsequent tabs have separator before
        let active_width_index = active_tab_index.map(|buf_idx| {
            if buf_idx == 0 {
                0
            } else {
                // Each tab after the first has a separator before it
                // So tab N is at position 2*N (sep before tab1 is at 1, tab1 at 2, sep before tab2 at 3, tab2 at 4, etc.)
                // Wait, the structure is: [tab0, sep, tab1, sep, tab2]
                // So tab N (0-indexed) is at position 2*N
                buf_idx * 2
            }
        });

        // Calculate offset to bring active tab into view
        let old_offset = view_state.tab_scroll_offset;
        let new_scroll_offset = if let Some(idx) = active_width_index {
            crate::view::ui::tabs::scroll_to_show_tab(
                &tab_widths,
                idx,
                view_state.tab_scroll_offset,
                max_visible_width,
            )
        } else {
            view_state
                .tab_scroll_offset
                .min(total_tabs_width.saturating_sub(max_visible_width))
        };

        tracing::debug!(
            "  -> offset: {} -> {} (idx={:?}, max_width={}, total={})",
            old_offset,
            new_scroll_offset,
            active_width_index,
            max_visible_width,
            total_tabs_width
        );
        view_state.tab_scroll_offset = new_scroll_offset;
    }

    /// Synchronize viewports for all scroll sync groups
    ///
    /// This syncs the inactive split's viewport to match the active split's position.
    /// By deriving from the active split's actual viewport, we capture all viewport
    /// changes regardless of source (scroll events, cursor movements, etc.).
    pub(super) fn sync_scroll_groups(&mut self) {
        let active_split = self.split_manager.active_split();
        let group_count = self.scroll_sync_manager.groups().len();

        if group_count > 0 {
            tracing::debug!(
                "sync_scroll_groups: active_split={:?}, {} groups",
                active_split,
                group_count
            );
        }

        // Collect sync info: for each group where active split participates,
        // get the active split's current line position
        let sync_info: Vec<_> = self
            .scroll_sync_manager
            .groups()
            .iter()
            .filter_map(|group| {
                tracing::debug!(
                    "sync_scroll_groups: checking group {}, left={:?}, right={:?}",
                    group.id,
                    group.left_split,
                    group.right_split
                );

                if !group.contains_split(active_split.into()) {
                    tracing::debug!(
                        "sync_scroll_groups: active split {:?} not in group",
                        active_split
                    );
                    return None;
                }

                // Get active split's current viewport top_byte
                let active_top_byte = self
                    .split_view_states
                    .get(&active_split)?
                    .viewport
                    .top_byte;

                // Get active split's buffer to convert bytes → line
                let active_buffer_id = self.split_manager.buffer_for_split(active_split)?;
                let buffer_state = self.buffers.get(&active_buffer_id)?;
                let buffer_len = buffer_state.buffer.len();
                let active_line = buffer_state.buffer.get_line_number(active_top_byte);

                tracing::debug!(
                    "sync_scroll_groups: active_split={:?}, buffer_id={:?}, top_byte={}, buffer_len={}, active_line={}",
                    active_split,
                    active_buffer_id,
                    active_top_byte,
                    buffer_len,
                    active_line
                );

                // Determine the other split and compute its target line
                let (other_split, other_line) = if group.is_left_split(active_split.into()) {
                    // Active is left, sync right
                    (group.right_split, group.left_to_right_line(active_line))
                } else {
                    // Active is right, sync left
                    (group.left_split, group.right_to_left_line(active_line))
                };

                tracing::debug!(
                    "sync_scroll_groups: syncing other_split={:?} to line {}",
                    other_split,
                    other_line
                );

                Some((other_split, other_line))
            })
            .collect();

        // Apply sync to other splits
        for (other_split, target_line) in sync_info {
            let other_leaf = LeafId(other_split);
            if let Some(buffer_id) = self.split_manager.buffer_for_split(other_leaf) {
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    let buffer = &mut state.buffer;
                    if let Some(view_state) = self.split_view_states.get_mut(&other_leaf) {
                        view_state.viewport.scroll_to(buffer, target_line);
                    }
                }
            }
        }

        // Same-buffer scroll sync: when two splits show the same buffer (e.g., source
        // vs compose mode), sync the inactive split's viewport to match the active
        // split's scroll position.  Gated on the user-togglable scroll sync flag.
        //
        // We copy top_byte directly for the general case.  At the bottom edge the
        // two splits may disagree because compose mode has soft-break virtual lines.
        // Rather than computing the correct position here (where view lines aren't
        // available), we set a flag and let `render_buffer_in_split` fix it up using
        // the same view-line-based logic that `ensure_visible_in_layout` uses.
        let active_buffer_id = if self.same_buffer_scroll_sync {
            self.split_manager.buffer_for_split(active_split)
        } else {
            None
        };
        if let Some(active_buf_id) = active_buffer_id {
            let active_top_byte = self
                .split_view_states
                .get(&active_split)
                .map(|vs| vs.viewport.top_byte);
            let active_viewport_height = self
                .split_view_states
                .get(&active_split)
                .map(|vs| vs.viewport.visible_line_count())
                .unwrap_or(0);

            if let Some(top_byte) = active_top_byte {
                // Find other splits showing the same buffer (not in an explicit sync group)
                let other_splits: Vec<_> = self
                    .split_view_states
                    .keys()
                    .filter(|&&s| {
                        s != active_split
                            && self.split_manager.buffer_for_split(s) == Some(active_buf_id)
                            && !self.scroll_sync_manager.is_split_synced(s.into())
                    })
                    .copied()
                    .collect();

                if !other_splits.is_empty() {
                    // Detect whether the active split is at the bottom of the
                    // buffer (remaining lines fit within the viewport).
                    let at_bottom = if let Some(state) = self.buffers.get_mut(&active_buf_id) {
                        let mut iter = state.buffer.line_iterator(top_byte, 80);
                        let mut lines_remaining = 0;
                        while iter.next_line().is_some() {
                            lines_remaining += 1;
                            if lines_remaining > active_viewport_height {
                                break;
                            }
                        }
                        lines_remaining <= active_viewport_height
                    } else {
                        false
                    };

                    for other_split in other_splits {
                        if let Some(view_state) = self.split_view_states.get_mut(&other_split) {
                            view_state.viewport.top_byte = top_byte;
                            // At the bottom edge, tell the render pass to
                            // adjust using view lines (soft-break-aware).
                            view_state.viewport.sync_scroll_to_end = at_bottom;
                        }
                    }
                }
            }
        }
    }

    /// Pre-sync ensure_visible for scroll sync groups
    ///
    /// When the active split is in a scroll sync group, we need to update its viewport
    /// BEFORE sync_scroll_groups runs. This ensures cursor movements like 'G' (go to end)
    /// properly sync to the other split.
    ///
    /// After updating the active split's viewport, we mark the OTHER splits in the group
    /// to skip ensure_visible so the sync position isn't undone during rendering.
    pub(super) fn pre_sync_ensure_visible(&mut self, active_split: LeafId) {
        // Check if active split is in any scroll sync group
        let group_info = self
            .scroll_sync_manager
            .find_group_for_split(active_split.into())
            .map(|g| (g.left_split, g.right_split));

        if let Some((left_split, right_split)) = group_info {
            // Get the active split's buffer and update its viewport
            if let Some(buffer_id) = self.split_manager.buffer_for_split(active_split) {
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
                        // Update viewport to show cursor
                        view_state.ensure_cursor_visible(&mut state.buffer, &state.marker_list);

                        tracing::debug!(
                            "pre_sync_ensure_visible: updated active split {:?} viewport, top_byte={}",
                            active_split,
                            view_state.viewport.top_byte
                        );
                    }
                }
            }

            // Mark the OTHER split to skip ensure_visible so the sync position isn't undone
            let active_sid: SplitId = active_split.into();
            let other_split: SplitId = if active_sid == left_split {
                right_split
            } else {
                left_split
            };

            if let Some(view_state) = self.split_view_states.get_mut(&LeafId(other_split)) {
                view_state.viewport.set_skip_ensure_visible();
                tracing::debug!(
                    "pre_sync_ensure_visible: marked other split {:?} to skip ensure_visible",
                    other_split
                );
            }
        }

        // Same-buffer scroll sync: also mark other splits showing the same buffer
        // to skip ensure_visible, so our sync_scroll_groups position isn't undone.
        if !self.same_buffer_scroll_sync {
            // Scroll sync disabled — don't interfere with other splits.
        } else if let Some(active_buf_id) = self.split_manager.buffer_for_split(active_split) {
            let other_same_buffer_splits: Vec<_> = self
                .split_view_states
                .keys()
                .filter(|&&s| {
                    s != active_split
                        && self.split_manager.buffer_for_split(s) == Some(active_buf_id)
                        && !self.scroll_sync_manager.is_split_synced(s.into())
                })
                .copied()
                .collect();

            for other_split in other_same_buffer_splits {
                if let Some(view_state) = self.split_view_states.get_mut(&other_split) {
                    view_state.viewport.set_skip_ensure_visible();
                }
            }
        }
    }
}
