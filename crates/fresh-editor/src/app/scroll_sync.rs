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

impl crate::app::window::Window {
    /// Ensure the active tab in a split is visible by adjusting its
    /// scroll offset. Pure window-state mutation: split tree +
    /// view_states + buffer_metadata + composite_buffers + grouped_subtrees
    /// all live on `Window`.
    pub fn ensure_active_tab_visible(
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
        let metadata = &self.buffer_metadata;
        let composites = &self.composite_buffers;

        self.buffers.with_all_mut(|buffer_map, _mgr, vs_map| {
            let Some(view_state) = vs_map.get_mut(&split_id) else {
                return;
            };
            let split_buffers = view_state.open_buffers.clone();
            let (tab_widths, rendered_targets) = crate::view::ui::tabs::calculate_tab_widths(
                &split_buffers,
                buffer_map,
                metadata,
                composites,
                &group_names,
            );

            let total_tabs_width: usize = tab_widths.iter().sum();
            // Reserve the pinned "+" button's column when the tabs overflow, so
            // the active tab stays fully visible and never slips under it.
            let max_visible_width = crate::view::ui::tabs::tabs_render_width(
                total_tabs_width,
                available_width as usize,
            );

            let active_target = view_state.active_target();
            let active_target = if matches!(active_target, crate::view::split::TabTarget::Buffer(_))
            {
                crate::view::split::TabTarget::Buffer(active_buffer)
            } else {
                active_target
            };

            let active_tab_index = rendered_targets.iter().position(|t| *t == active_target);
            let active_width_index =
                active_tab_index.map(|buf_idx| if buf_idx == 0 { 0 } else { buf_idx * 2 });

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
        });
    }

    /// Synchronize viewports for all scroll-sync groups in this window.
    ///
    /// For each registered group containing the active split, derive the
    /// active split's top line from its viewport and project it onto the
    /// paired split via the group's mapping. Then, when same-buffer
    /// scroll sync is enabled, also mirror the active split's `top_byte`
    /// onto every other split that shows the same buffer (and isn't in
    /// an explicit sync group). The bottom-edge case sets
    /// `sync_scroll_to_end` so the render pass does the
    /// soft-break-aware fix-up using view lines.
    pub(super) fn sync_scroll_groups(&mut self) {
        let (mgr, vs_map) = self
            .buffers
            .splits()
            .expect("window must have a populated split layout");
        let active_split = mgr.active_split();
        let group_count = self.scroll_sync_manager.groups().len();

        if group_count > 0 {
            tracing::debug!(
                "sync_scroll_groups: active_split={:?}, {} groups",
                active_split,
                group_count
            );
        }

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

                let active_top_byte = vs_map.get(&active_split)?.viewport.top_byte;
                let active_buffer_id = mgr.buffer_for_split(active_split)?;
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

                let (other_split, other_line) = if group.is_left_split(active_split.into()) {
                    (group.right_split, group.left_to_right_line(active_line))
                } else {
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

        for (other_split, target_line) in sync_info {
            let other_leaf = LeafId(other_split);
            let buffer_id = self
                .buffers
                .splits()
                .expect("window must have a populated split layout")
                .0
                .buffer_for_split(other_leaf);
            if let Some(buffer_id) = buffer_id {
                self.scroll_split_viewport_to(buffer_id, other_leaf, target_line, false);
            }
        }

        let active_buffer_id = if self.same_buffer_scroll_sync {
            self.buffers
                .splits()
                .expect("window must have a populated split layout")
                .0
                .buffer_for_split(active_split)
        } else {
            None
        };
        if let Some(active_buf_id) = active_buffer_id {
            let (mgr, vs_map) = self
                .buffers
                .splits()
                .expect("window must have a populated split layout");
            let active_top_byte = vs_map.get(&active_split).map(|vs| vs.viewport.top_byte);
            let active_viewport_height = vs_map
                .get(&active_split)
                .map(|vs| vs.viewport.visible_line_count())
                .unwrap_or(0);

            if let Some(top_byte) = active_top_byte {
                let other_splits: Vec<_> = vs_map
                    .keys()
                    .filter(|&&s| {
                        s != active_split
                            && mgr.buffer_for_split(s) == Some(active_buf_id)
                            && !self.scroll_sync_manager.is_split_synced(s.into())
                    })
                    .copied()
                    .collect();

                if !other_splits.is_empty() {
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

                    let (_, vs_map_mut) = self
                        .buffers
                        .splits_mut()
                        .expect("window must have a populated split layout");
                    for other_split in other_splits {
                        if let Some(view_state) = vs_map_mut.get_mut(&other_split) {
                            view_state.viewport.top_byte = top_byte;
                            view_state.viewport.sync_scroll_to_end = at_bottom;
                        }
                    }
                }
            }
        }
    }

    /// Pre-sync ensure-visible hook for scroll-sync groups in this window.
    ///
    /// When the active split is in a sync group we update its viewport
    /// here (before `sync_scroll_groups`) so commands like `G` produce
    /// the right scroll position that gets mirrored. The other split in
    /// the group is then marked to skip `ensure_visible` during render
    /// so the sync isn't undone. Same-buffer sync mirrors the same
    /// "skip" mark across the other splits showing the same buffer.
    pub(super) fn pre_sync_ensure_visible(&mut self, active_split: LeafId) {
        let group_info = self
            .scroll_sync_manager
            .find_group_for_split(active_split.into())
            .map(|g| (g.left_split, g.right_split));

        if let Some((left_split, right_split)) = group_info {
            let buffer_id = self
                .buffers
                .splits()
                .expect("window must have a populated split layout")
                .0
                .buffer_for_split(active_split);
            if let Some(buffer_id) = buffer_id {
                self.ensure_cursor_visible_for_split(buffer_id, active_split);
            }

            let active_sid: SplitId = active_split.into();
            let other_split: SplitId = if active_sid == left_split {
                right_split
            } else {
                left_split
            };

            if let Some((_, vs_map)) = self.buffers.splits_mut() {
                if let Some(view_state) = vs_map.get_mut(&LeafId(other_split)) {
                    view_state.viewport.set_skip_ensure_visible();
                    tracing::debug!(
                        "pre_sync_ensure_visible: marked other split {:?} to skip ensure_visible",
                        other_split
                    );
                }
            }
        }

        if !self.same_buffer_scroll_sync {
            return;
        }
        let active_buf_id = match self
            .buffers
            .splits()
            .expect("window must have a populated split layout")
            .0
            .buffer_for_split(active_split)
        {
            Some(b) => b,
            None => return,
        };

        let other_same_buffer_splits: Vec<_> = {
            let (mgr, vs_map) = self
                .buffers
                .splits()
                .expect("window must have a populated split layout");
            vs_map
                .keys()
                .filter(|&&s| {
                    s != active_split
                        && mgr.buffer_for_split(s) == Some(active_buf_id)
                        && !self.scroll_sync_manager.is_split_synced(s.into())
                })
                .copied()
                .collect()
        };

        if let Some((_, vs_map)) = self.buffers.splits_mut() {
            for other_split in other_same_buffer_splits {
                if let Some(view_state) = vs_map.get_mut(&other_split) {
                    view_state.viewport.set_skip_ensure_visible();
                }
            }
        }
    }
}
