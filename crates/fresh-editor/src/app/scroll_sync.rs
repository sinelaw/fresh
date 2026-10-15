//! Scroll-sync orchestrators on `Editor`.
//!
//! - `sync_scroll_groups` — when splits share a scroll group (e.g. for
//!   side-by-side diffs), keep their viewports in lockstep.
//! - `pre_sync_ensure_visible` — pre-sync hook that ensures the active
//!   split's cursor is on screen so the scroll-group sync uses a valid
//!   anchor.

use crate::model::event::{LeafId, SplitId};

impl crate::app::window::Window {
    /// Synchronize viewports for all scroll-sync groups in this window.
    ///
    /// For each registered group containing the active split, derive the
    /// active split's top line from its viewport and project it onto the
    /// paired split via the group's mapping. (Horizontal scroll is copied
    /// after layout instead, see `sync_left_column_from`.) Then, when same-buffer
    /// scroll sync is enabled, also mirror the active split's `top_byte`
    /// onto every other split that shows the same buffer (and isn't in
    /// an explicit sync group). The bottom-edge case sets
    /// `sync_scroll_to_end` so the render pass does the
    /// soft-break-aware fix-up using view lines.
    pub(super) fn sync_scroll_groups(&mut self) {
        // The pane the user is in: a shown group's focused panel, not the
        // pane showing the group. Panels are not in the split tree, so a
        // pane's buffer is read off its view state (`shown_buffer`).
        let active_split = self.effective_active_split();
        let vs_map = self
            .buffers
            .split_view_states()
            .expect("window must have a populated split layout");
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

                let active_vs = vs_map.get(&active_split)?;
                let active_top_byte = active_vs.buffer_tab_state().viewport.top_byte();
                let active_buffer_id = active_vs.shown_buffer()?;
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
                .split_view_states()
                .and_then(|vs_map| vs_map.get(&other_leaf))
                .and_then(|vs| vs.shown_buffer());
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
            let active_top_byte = vs_map
                .get(&active_split)
                .map(|vs| vs.buffer_tab_state().viewport.top_byte());
            let active_viewport_height = vs_map
                .get(&active_split)
                .map(|vs| vs.buffer_tab_state().viewport.visible_line_count())
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
                            view_state
                                .buffer_tab_state_mut()
                                .viewport
                                .set_top_byte(top_byte);
                            view_state
                                .buffer_tab_state_mut()
                                .viewport
                                .sync_scroll_to_end = at_bottom;
                        }
                    }
                }
            }
        }
    }

    /// Carry `source`'s horizontal scroll to its partner in every
    /// scroll-sync group it is in. Returns whether a partner moved.
    ///
    /// Vertical sync maps lines through the group's anchors; horizontal sync
    /// is a plain column copy, and only between panes that pan horizontally
    /// at all (`shares_horizontal_scroll`). A wrapped or compose pane neither drives
    /// its partner back to column 0 nor gets shifted off screen by it.
    /// Same-buffer scroll sync stays vertical only.
    ///
    /// Called after a frame's layout for the focused pane (whose column is
    /// only final then: the cursor's visual column needs the laid-out rows),
    /// and on a wheel pan for the pane under the pointer.
    pub(super) fn sync_left_column_from(&mut self, source: LeafId) -> bool {
        let source_id: SplitId = source.into();
        let Some(left_column) = self
            .buffers
            .split_view_states()
            .and_then(|vs_map| vs_map.get(&source))
            .map(|vs| vs.buffer_tab_state())
            .filter(|state| state.shares_horizontal_scroll())
            .map(|state| state.viewport.left_column)
        else {
            return false;
        };
        let partners: Vec<LeafId> = self
            .scroll_sync_manager
            .groups()
            .iter()
            .filter(|group| group.contains_split(source_id))
            .map(|group| {
                LeafId(if group.is_left_split(source_id) {
                    group.right_split
                } else {
                    group.left_split
                })
            })
            .collect();
        let Some(vs_map) = self.buffers.split_view_states_mut() else {
            return false;
        };
        let mut moved = false;
        for partner in partners {
            let Some(state) = vs_map.get_mut(&partner).map(|vs| vs.buffer_tab_state_mut()) else {
                continue;
            };
            if state.shares_horizontal_scroll() && state.viewport.left_column != left_column {
                state.viewport.left_column = left_column;
                state.viewport.set_skip_ensure_visible();
                moved = true;
            }
        }
        moved
    }

    /// After a frame's layout, carry the focused pane's column to its
    /// partners. Returns whether one more frame is owed.
    ///
    /// The focused pane's column is only final after layout: the cursor's
    /// column is a visual one (tabs, wide characters, inlay hints), which
    /// only the laid-out rows know. Its partners were laid out from their old
    /// column, so a move owes a frame.
    ///
    /// Only a column not synced yet is copied. A partner that cannot scroll
    /// as far (a file of shorter lines) settles short of it; copying the same
    /// column again would push it back out, its settle would pull it in, and
    /// each frame would owe the next.
    pub(crate) fn sync_left_column_after_layout(&mut self) -> bool {
        let focused = self.effective_active_split();
        let Some(column) = self
            .buffers
            .split_view_states()
            .and_then(|vs_map| vs_map.get(&focused))
            .map(|vs| vs.buffer_tab_state().viewport.left_column)
        else {
            return false;
        };
        if self.last_horizontal_sync == Some((focused, column)) {
            return false;
        }
        self.last_horizontal_sync = Some((focused, column));
        self.sync_left_column_from(focused)
    }

    /// Drop every scroll-sync group with a pane that has closed.
    ///
    /// A group dies with either of its panes. Panes close on several paths
    /// (close split, tab drag, buffer-group close), all of which drop the
    /// pane's view state, so the check is on that rather than at each path.
    /// It runs at the start of every frame's sync, and before a group is
    /// created: a plugin closing a pane and pairing again under the same id
    /// does both in one batch of commands, with no frame between.
    pub(crate) fn prune_scroll_sync_groups(&mut self) {
        if let Some(vs_map) = self.buffers.split_view_states() {
            self.scroll_sync_manager
                .retain_live_groups(|split| vs_map.contains_key(&LeafId(split)));
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
    ///
    /// First it drops groups whose pane has closed
    /// (`prune_scroll_sync_groups`), so nothing below reads one.
    pub(super) fn pre_sync_ensure_visible(&mut self, active_split: LeafId) {
        self.prune_scroll_sync_groups();

        let group_info = self
            .scroll_sync_manager
            .find_group_for_split(active_split.into())
            .map(|g| (g.left_split, g.right_split));

        if let Some((left_split, right_split)) = group_info {
            // Off the view state: a group's panel is not in the split tree.
            let buffer_id = self
                .buffers
                .split_view_states()
                .and_then(|vs_map| vs_map.get(&active_split))
                .and_then(|vs| vs.shown_buffer());
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
                    view_state
                        .buffer_tab_state_mut()
                        .viewport
                        .set_skip_ensure_visible();
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
                    view_state
                        .buffer_tab_state_mut()
                        .viewport
                        .set_skip_ensure_visible();
                }
            }
        }
    }
}
