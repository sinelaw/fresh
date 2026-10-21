//! Scroll-sync orchestrators on `Editor`.
//!
//! - `sync_scroll_groups` — when splits share a scroll group (e.g. for
//!   side-by-side diffs), keep their viewports in lockstep.
//! - `pre_sync_ensure_visible` — pre-sync hook that ensures the active
//!   split's cursor is on screen so the scroll-group sync uses a valid
//!   anchor.

use crate::model::event::{LeafId, SplitId};

/// A pane's rows, as [`crate::app::window::Window::pane_rows`] reads them.
struct PaneRows {
    geometry: crate::view::wrap_index::WrapIndexGeometry,
    top: u32,
    height: u32,
    max_top: u32,
}

impl crate::app::window::Window {
    /// Synchronize viewports for all scroll-sync groups in this window.
    ///
    /// For each registered group containing the active split, derive the
    /// active split's top line from its viewport and project it onto the
    /// paired split via the group's mapping — or, for a group whose two panes
    /// show the same buffer, in visual rows (`sync_same_buffer_rows`). The
    /// active split is the focused one unless the pointer scrolled another
    /// since the last sync (`scroll_sync_driver`). (Horizontal scroll is copied
    /// after layout instead, see `sync_left_column_from`.) Then, when same-buffer
    /// scroll sync is enabled, also mirror the active split's `top_byte`
    /// onto every other split that shows the same buffer (and isn't in
    /// an explicit sync group). The bottom-edge case sets
    /// `sync_scroll_to_end` so the render pass does the
    /// soft-break-aware fix-up using view lines.
    pub(super) fn sync_scroll_groups(&mut self) {
        // The pane the user is in: a shown group's focused panel, not the
        // pane showing the group. Panels are not in the split tree, so a
        // pane's buffer is read off its view state (`shown_buffer`). A pane
        // the pointer scrolled since the last sync drives this one instead.
        let focused = self.effective_active_split();
        let active_split = self
            .scroll_sync_driver
            .take()
            .filter(|leaf| {
                self.buffers
                    .split_view_states()
                    .is_some_and(|vs_map| vs_map.contains_key(leaf))
            })
            .unwrap_or(focused);
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

                let same_buffer = vs_map
                    .get(&LeafId(other_split))
                    .and_then(|vs| vs.shown_buffer())
                    == Some(active_buffer_id);
                Some((group.id, other_split, other_line, same_buffer))
            })
            .collect();

        for (group_id, other_split, target_line, same_buffer) in sync_info {
            let other_leaf = LeafId(other_split);
            let buffer_id = self
                .buffers
                .split_view_states()
                .and_then(|vs_map| vs_map.get(&other_leaf))
                .and_then(|vs| vs.shown_buffer());
            let Some(buffer_id) = buffer_id else {
                continue;
            };
            if same_buffer
                && self.sync_same_buffer_rows(group_id, buffer_id, active_split, other_leaf)
            {
                continue;
            }
            self.scroll_split_viewport_to(buffer_id, other_leaf, target_line, false);
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

    /// Sync a group whose two panes show the same buffer — markdown's source
    /// beside its rendered preview — in visual rows. Returns `false` when the
    /// rows cannot be had (a buffer beyond the wrap index's size ceilings, a
    /// pane not laid out yet), leaving the caller's line mapping to do it.
    ///
    /// The line mapping a diff's anchors call for is the identity here, and it
    /// is not enough: the two panes lay the same lines out in different rows
    /// (the preview frames tables, spaces list items, wraps paragraphs), so
    /// "same top line" keeps only the top row together, lets everything below
    /// drift apart, and moves the partner by whole lines of uneven height. Rows
    /// carry the position inside a line too, so the partner moves smoothly, and
    /// a row is lined up wherever the window is (`scroll_sync::synced_top`).
    fn sync_same_buffer_rows(
        &mut self,
        group_id: crate::view::scroll_sync::ScrollSyncGroupId,
        buffer_id: crate::model::event::BufferId,
        active: LeafId,
        other: LeafId,
    ) -> bool {
        use crate::view::scroll_sync::{inverse_synced_top, map_row, synced_top, RowSyncState};

        let Some(left_is_active) = self
            .scroll_sync_manager
            .get_group_mut(group_id)
            .map(|g| g.is_left_split(active.into()))
        else {
            return false;
        };
        let (Some(a), Some(o)) = (
            self.pane_rows(buffer_id, active),
            self.pane_rows(buffer_id, other),
        ) else {
            return false;
        };
        let last = self
            .scroll_sync_manager
            .get_group_mut(group_id)
            .and_then(|g| g.row_sync);
        let record = |mgr: &mut crate::view::scroll_sync::ScrollSyncManager, other_top: u32| {
            if let Some(group) = mgr.get_group_mut(group_id) {
                group.row_sync = Some(RowSyncState {
                    active: active.into(),
                    active_top: a.top,
                    other_top,
                });
            }
        };

        // The focus moved to the other pane and nothing has scrolled since: the
        // pair is where the last sync put it, and stays there.
        if let Some(last) = last {
            if last.active != SplitId::from(active) && a.top == last.other_top {
                record(&mut self.scroll_sync_manager, o.top);
                return true;
            }
        }

        let Some(state) = self.buffers.get(&buffer_id) else {
            return false;
        };
        let (Some(index_a), Some(index_o)) = (
            state.wrap_indices.get(&a.geometry),
            state.wrap_indices.get(&o.geometry),
        ) else {
            return false;
        };
        // One curve relates the pair, driven from the group's left pane (the
        // source): the right pane's top for each left top. Scrolling the right
        // pane inverts it, so the pair sits on the same curve whichever pane
        // the user scrolls.
        let (left, right, index_l, index_r) = if left_is_active {
            (&a, &o, index_a, index_o)
        } else {
            (&o, &a, index_o, index_a)
        };
        let curve = |top: u32| {
            synced_top(top, left.max_top, left.height, right.max_top, |row| {
                map_row(index_l, index_r, &state.buffer, row)
            })
        };
        let mut target = if left_is_active {
            curve(a.top)
        } else if a.top == 0 {
            0
        } else if a.top >= a.max_top {
            o.max_top
        } else {
            inverse_synced_top(a.top, o.max_top, curve)
        };
        // Never against the scroll: the driver went down, so the partner does
        // not go up, and the other way round. The curve is monotonic, but the
        // aligned row slides with the window and a line laid out shorter in the
        // partner could still pull it back a row.
        if let Some(last) = last.filter(|l| l.active == SplitId::from(active)) {
            if a.top > last.active_top {
                target = target.max(last.other_top.min(o.max_top));
            } else if a.top < last.active_top {
                target = target.min(last.other_top);
            }
        }
        let addr = index_o.byte_of_row(&state.buffer, target);
        let line_start = state.buffer.line_start_offset(addr.line).unwrap_or(0);

        if let Some(vs) = self
            .buffers
            .split_view_states_mut()
            .and_then(|vs_map| vs_map.get_mut(&other))
        {
            let viewport = &mut vs.buffer_tab_state_mut().viewport;
            viewport.set_top_byte(line_start);
            viewport.set_top_view_line_offset(addr.row_in_line);
            viewport.set_skip_ensure_visible();
        }
        record(&mut self.scroll_sync_manager, target);
        true
    }

    /// `leaf`'s rows as the frame will lay them out: the wrap index for its
    /// geometry (built here if stale, as the frame's reconcile would), and its
    /// top row, height and furthest top row in that index. `None` beyond the
    /// index's size ceilings, or for a pane not laid out yet.
    fn pane_rows(
        &mut self,
        buffer_id: crate::model::event::BufferId,
        leaf: LeafId,
    ) -> Option<PaneRows> {
        use crate::view::ui::split_rendering::scrollbar::{
            MAX_WRAP_SCROLLBAR_BYTES, MAX_WRAP_SCROLLBAR_LINES,
        };
        self.buffers
            .with_buffer_and_split(buffer_id, leaf, |state, vs| {
                let within_bounds = !state.buffer.is_large_file()
                    && state.buffer.len() <= MAX_WRAP_SCROLLBAR_BYTES
                    && state
                        .buffer
                        .line_count()
                        .is_some_and(|lc| lc <= MAX_WRAP_SCROLLBAR_LINES);
                let height = vs.viewport.visible_line_count() as u32;
                if !within_bounds || height == 0 {
                    return None;
                }
                let fold_ranges = state.fold_ranges(&vs.folds);
                let geometry = crate::view::ui::split_rendering::wrap_index_geometry_for(
                    &vs.viewport,
                    &state.buffer,
                    vs.viewport.line_wrap_enabled,
                    &vs.view_mode,
                    crate::view::wrap_index::fold_signature(&fold_ranges),
                );
                let inputs = state.pipeline_inputs();
                let line_ending = state.buffer.line_ending();
                let decorations = state.index_decorations(geometry.view_mode, fold_ranges, &[]);
                let index = state.wrap_indices.entry(geometry);
                index.ensure_built(
                    &mut state.buffer,
                    geometry,
                    inputs,
                    line_ending,
                    &decorations,
                );
                let top_line = state.buffer.get_line_number(vs.viewport.top_byte());
                let max_top = index.total_rows().saturating_sub(height);
                let top = (index.line_first_row(top_line)
                    + vs.viewport.top_view_line_offset() as u32)
                    .min(max_top);
                Some(PaneRows {
                    geometry,
                    top,
                    height,
                    max_top,
                })
            })
            .flatten()
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
