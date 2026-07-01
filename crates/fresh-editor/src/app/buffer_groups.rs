//! Buffer group management.
//!
//! A buffer group presents multiple splits/buffers as a single tab.
//! Each panel is a real buffer with its own viewport and scrollbar.
//! The group appears as one entry in the tab bar and buffer list.

use crate::app::types::BufferGroupId;
#[cfg(feature = "plugins")]
use crate::app::types::{BufferGroup, GroupLayoutNode};
#[cfg(feature = "plugins")]
use crate::model::event::SplitDirection;
use crate::model::event::{BufferId, LeafId};
#[cfg(feature = "plugins")]
use crate::view::split::SplitViewState;
#[cfg(feature = "plugins")]
use fresh_core::api::BufferGroupResult;
#[cfg(feature = "plugins")]
use std::collections::HashMap;

/// Layout description deserialized from plugin JSON.
#[cfg(feature = "plugins")]
#[derive(Debug, serde::Deserialize)]
#[serde(tag = "type")]
enum LayoutDesc {
    #[serde(rename = "scrollable")]
    Scrollable {
        id: String,
        /// Whether this panel responds to scroll events. Defaults to true
        /// for scrollable panels.
        scrollable: Option<bool>,
    },
    #[serde(rename = "fixed")]
    Fixed {
        id: String,
        height: u16,
        /// Whether this panel responds to scroll events. Defaults to false
        /// for fixed-height panels — their content is pinned to the panel
        /// size, so mouse-wheel scroll is a no-op and no scrollbar is drawn.
        /// Callers can override by passing `"scrollable": true`.
        scrollable: Option<bool>,
    },
    #[serde(rename = "split")]
    Split {
        direction: String, // "h" or "v"
        ratio: f32,
        first: Box<LayoutDesc>,
        second: Box<LayoutDesc>,
    },
}

impl super::Editor {
    /// Create a buffer group from a layout description.
    ///
    /// Builds a `SplitNode::Grouped` wrapping the panel layout and stores
    /// it in `grouped_subtrees`, then adds a `TabTarget::Group(group_leaf_id)`
    /// entry to the current split's tab bar. The main split tree is NOT
    /// modified — the group's subtree is dispatched to at render time when
    /// the current split's active target is this group.
    #[cfg(feature = "plugins")]
    pub(super) fn create_buffer_group(
        &mut self,
        name: String,
        mode: String,
        layout_json: String,
    ) -> Result<BufferGroupResult, String> {
        use crate::view::split::{SplitNode, TabTarget};

        // Parse layout
        let desc: LayoutDesc =
            serde_json::from_str(&layout_json).map_err(|e| format!("Invalid layout: {}", e))?;

        // Allocate group ID
        let group_id = BufferGroupId(self.active_window_mut().next_buffer_group_id);
        self.active_window_mut().next_buffer_group_id += 1;

        // Build buffers for each leaf in the layout
        let mut panel_buffers: HashMap<String, BufferId> = HashMap::new();
        let mut panel_splits: HashMap<String, LeafId> = HashMap::new();
        let layout = self.build_group_layout(&desc, &mode, &mut panel_buffers)?;

        // Build the inner split tree for the group
        let inner_tree = self.build_split_tree(&layout, &mut panel_splits)?;

        // Determine the active inner leaf (first scrollable panel, fallback to any leaf)
        let active_inner_leaf = find_first_scrollable_leaf(&layout, &panel_splits)
            .or_else(|| panel_splits.values().next().copied())
            .ok_or("No panels in layout")?;

        // Allocate a LeafId for the Grouped node itself. This is what the
        // tab bar uses to reference this group (`TabTarget::Group(group_leaf_id)`).
        let group_leaf_id = LeafId(
            self.windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_manager_mut())
                .expect("active window must have a populated split layout")
                .allocate_split_id(),
        );

        // Build the Grouped SplitNode and stash it in the side map.
        let grouped_node = SplitNode::Grouped {
            split_id: group_leaf_id,
            name: name.clone(),
            layout: Box::new(inner_tree),
            active_inner_leaf,
        };
        self.active_window_mut()
            .grouped_subtrees
            .insert(group_leaf_id, grouped_node);

        // Create SplitViewState for each inner panel leaf
        let (tw, th) = (self.terminal_width, self.terminal_height);
        for (panel_name, leaf_id) in &panel_splits {
            let buffer_id = *panel_buffers
                .get(panel_name)
                .ok_or(format!("Panel '{}' has no buffer", panel_name))?;
            let mut vs = SplitViewState::with_buffer(tw, th, buffer_id);
            // All panels inside a group suppress chrome — the parent split's
            // tab bar is the only tab bar shown.
            vs.suppress_chrome = true;
            vs.hide_tilde = true;
            if let Some(bs) = vs.keyed_states.get_mut(&buffer_id) {
                bs.show_line_numbers = false;
                bs.highlight_current_line = false;
            }
            self.windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_view_states_mut())
                .expect("active window must have a populated split layout")
                .insert(*leaf_id, vs);
        }

        // Mark all panel buffers as hidden from tabs so they don't appear
        // in quick-switch or the buffer list.
        for buffer_id in panel_buffers.values() {
            if let Some(meta) = self.active_window_mut().buffer_metadata.get_mut(buffer_id) {
                meta.hidden_from_tabs = true;
            }
        }

        // Remove panel buffers from every OTHER split's open_buffers AND
        // keyed_states. create_virtual_buffer adds them to the active split
        // when each was created; leaving them there makes the outer split
        // carry a stale cursor entry for the panel buffer, which later
        // collides with the panel's own view state in any lookup that
        // scans split_view_states by buffer id.
        let hidden_panel_ids: Vec<BufferId> = panel_buffers.values().copied().collect();
        let panel_leaf_ids: std::collections::HashSet<LeafId> =
            panel_splits.values().copied().collect();
        for (leaf_id, vs) in self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .iter_mut()
        {
            if panel_leaf_ids.contains(leaf_id) {
                // The panel's own view state needs its buffer.
                continue;
            }
            vs.open_buffers.retain(|t| match t {
                TabTarget::Buffer(b) => !hidden_panel_ids.contains(b),
                TabTarget::Group(_) => true,
            });
            vs.keyed_states
                .retain(|bid, _| !hidden_panel_ids.contains(bid));
        }

        // Add the group as a tab in the CURRENT split's tab bar and make it
        // the active tab. (The main split tree is untouched — the group's
        // layout lives in `grouped_subtrees` and is dispatched at render time.)
        let current_split_id = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();
        if let Some(current_vs) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .get_mut(&current_split_id)
        {
            current_vs.add_group(group_leaf_id);
            current_vs.set_active_group_tab(group_leaf_id);
            current_vs.focused_group_leaf = Some(active_inner_leaf);
        }

        // Register the group metadata
        let group = BufferGroup {
            id: group_id,
            name: name.clone(),
            mode,
            layout,
            panel_buffers: panel_buffers.clone(),
            panel_splits,
            representative_split: Some(group_leaf_id),
        };

        // Register reverse mapping
        for buffer_id in panel_buffers.values() {
            self.active_window_mut()
                .buffer_to_group
                .insert(*buffer_id, group_id);
        }

        self.active_window_mut()
            .buffer_groups
            .insert(group_id, group);

        // Build result
        let panels: HashMap<String, u64> = panel_buffers
            .iter()
            .map(|(name, bid)| (name.clone(), bid.0 as u64))
            .collect();

        Ok(BufferGroupResult {
            group_id: group_id.0 as u64,
            panels,
        })
    }

    /// Build a SplitNode tree directly from a GroupLayoutNode.
    /// Populates panel_splits with leaf_id for each panel.
    #[cfg(feature = "plugins")]
    fn build_split_tree(
        &mut self,
        node: &GroupLayoutNode,
        panel_splits: &mut HashMap<String, crate::model::event::LeafId>,
    ) -> Result<crate::view::split::SplitNode, String> {
        use crate::model::event::LeafId;
        use crate::view::split::SplitNode;

        match node {
            GroupLayoutNode::Scrollable {
                id,
                buffer_id: Some(bid),
                ..
            }
            | GroupLayoutNode::Fixed {
                id,
                buffer_id: Some(bid),
                ..
            } => {
                let split_id = self
                    .windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_manager_mut())
                    .expect("active window must have a populated split layout")
                    .allocate_split_id();
                panel_splits.insert(id.clone(), LeafId(split_id));
                Ok(SplitNode::leaf(*bid, split_id))
            }
            GroupLayoutNode::Scrollable {
                buffer_id: None, ..
            }
            | GroupLayoutNode::Fixed {
                buffer_id: None, ..
            } => Err("Layout leaf has no buffer_id".to_string()),
            GroupLayoutNode::Split {
                direction,
                ratio,
                first,
                second,
            } => {
                let first_node = self.build_split_tree(first, panel_splits)?;
                let second_node = self.build_split_tree(second, panel_splits)?;
                let split_id = self
                    .windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_manager_mut())
                    .expect("active window must have a populated split layout")
                    .allocate_split_id();
                let mut split =
                    SplitNode::split(*direction, first_node, second_node, *ratio, split_id);
                // Apply fixed sizes from children
                let fixed_first_size = fixed_height_of(first);
                let fixed_second_size = fixed_height_of(second);
                if let SplitNode::Split {
                    fixed_first,
                    fixed_second,
                    ..
                } = &mut split
                {
                    *fixed_first = fixed_first_size;
                    *fixed_second = fixed_second_size;
                }
                Ok(split)
            }
        }
    }

    /// Build a GroupLayoutNode from a LayoutDesc, creating buffers for each leaf.
    #[cfg(feature = "plugins")]
    fn build_group_layout(
        &mut self,
        desc: &LayoutDesc,
        mode: &str,
        panel_buffers: &mut HashMap<String, BufferId>,
    ) -> Result<GroupLayoutNode, String> {
        match desc {
            LayoutDesc::Scrollable { id, scrollable } => {
                let scrollable = scrollable.unwrap_or(true);
                let buffer_id = self.active_window_mut().create_virtual_buffer(
                    format!("*{}*", id),
                    mode.to_string(),
                    true,
                );
                if let Some(state) = self
                    .windows
                    .get_mut(&self.active_window)
                    .map(|w| &mut w.buffers)
                    .expect("active window present")
                    .get_mut(&buffer_id)
                {
                    state.show_cursors = false;
                    state.editing_disabled = true;
                    state.scrollable = scrollable;
                    state.margins.configure_for_line_numbers(false);
                }
                panel_buffers.insert(id.clone(), buffer_id);
                Ok(GroupLayoutNode::Scrollable {
                    id: id.clone(),
                    buffer_id: Some(buffer_id),
                    split_id: None,
                })
            }
            LayoutDesc::Fixed {
                id,
                height,
                scrollable,
            } => {
                let scrollable = scrollable.unwrap_or(false);
                let buffer_id = self.active_window_mut().create_virtual_buffer(
                    format!("*{}*", id),
                    mode.to_string(),
                    true,
                );
                if let Some(state) = self
                    .windows
                    .get_mut(&self.active_window)
                    .map(|w| &mut w.buffers)
                    .expect("active window present")
                    .get_mut(&buffer_id)
                {
                    state.show_cursors = false;
                    state.editing_disabled = true;
                    state.scrollable = scrollable;
                    state.margins.configure_for_line_numbers(false);
                }
                panel_buffers.insert(id.clone(), buffer_id);
                Ok(GroupLayoutNode::Fixed {
                    id: id.clone(),
                    height: *height,
                    buffer_id: Some(buffer_id),
                    split_id: None,
                })
            }
            LayoutDesc::Split {
                direction,
                ratio,
                first,
                second,
            } => {
                let dir = if direction == "h" {
                    SplitDirection::Vertical // "h" = horizontal layout = vertical split line
                } else {
                    SplitDirection::Horizontal
                };
                let first_node = self.build_group_layout(first, mode, panel_buffers)?;
                let second_node = self.build_group_layout(second, mode, panel_buffers)?;
                Ok(GroupLayoutNode::Split {
                    direction: dir,
                    ratio: *ratio,
                    first: Box::new(first_node),
                    second: Box::new(second_node),
                })
            }
        }
    }

    /// Set content on a panel within a buffer group.
    #[cfg(feature = "plugins")]
    pub(super) fn set_panel_content(
        &mut self,
        group_id: usize,
        panel_name: String,
        entries: Vec<fresh_core::text_property::TextPropertyEntry>,
    ) {
        let bg_id = BufferGroupId(group_id);
        let buffer_id = self
            .active_window_mut()
            .buffer_groups
            .get(&bg_id)
            .and_then(|g| g.panel_buffers.get(&panel_name).copied());

        if let Some(buffer_id) = buffer_id {
            if let Err(e) = self.set_virtual_buffer_content(buffer_id, entries) {
                tracing::error!("Failed to set panel '{}' content: {}", panel_name, e);
            }
        } else {
            tracing::warn!("Panel '{}' not found in group {}", panel_name, group_id);
        }
    }

    /// Close a buffer group — remove the Grouped subtree, close all panel
    /// buffers, and remove the group tab from any split's tab bar.
    pub(super) fn close_buffer_group(&mut self, group_id: usize) {
        use crate::view::split::TabTarget;
        let bg_id = BufferGroupId(group_id);
        if let Some(group) = self.active_window_mut().buffer_groups.remove(&bg_id) {
            // Remove reverse mappings
            for buffer_id in group.panel_buffers.values() {
                self.active_window_mut().buffer_to_group.remove(buffer_id);
            }

            // Find the group_leaf_id (it's the `representative_split` now).
            if let Some(group_leaf_id) = group.representative_split {
                // Remove the Grouped subtree from the side map
                self.active_window_mut()
                    .grouped_subtrees
                    .remove(&group_leaf_id);
                // Remove the group tab from all splits' tab bars and clear
                // any active/focused group markers that point at this group.
                for vs in self
                    .windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_view_states_mut())
                    .expect("active window must have a populated split layout")
                    .values_mut()
                {
                    vs.open_buffers
                        .retain(|t| *t != TabTarget::Group(group_leaf_id));
                    vs.remove_group_from_history(group_leaf_id);
                    if vs.active_group_tab == Some(group_leaf_id) {
                        vs.active_group_tab = None;
                    }
                    if let Some(focused) = vs.focused_group_leaf {
                        if group.panel_splits.values().any(|&l| l == focused) {
                            vs.focused_group_leaf = None;
                        }
                    }
                }
            }

            // Clean up SplitViewState for inner panel leaves
            for split_id in group.panel_splits.values() {
                self.windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_view_states_mut())
                    .expect("active window must have a populated split layout")
                    .remove(split_id);
            }

            // Close all panel buffers
            for buffer_id in group.panel_buffers.values() {
                if let Err(e) = self.close_buffer(*buffer_id) {
                    tracing::warn!("Failed to close panel buffer {:?}: {}", buffer_id, e);
                }
            }

            // Ensure the active split now has a valid active_target.
            // If it was the group's tab, switch to the first available buffer tab.
            let active_split = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(mgr, _)| mgr)
                .expect("active window must have a populated split layout")
                .active_split();
            if let Some(vs) = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(_, vs)| vs)
                .expect("active window must have a populated split layout")
                .get(&active_split)
            {
                if let Some(first_buf) = vs.buffer_tab_ids().next() {
                    let _ = first_buf; // active_buffer is per-leaf; already set
                }
            }
        }
    }

    /// Focus a specific panel in a buffer group.
    ///
    /// If the panel's inner leaf is not in the main split tree (side-map
    /// approach), this activates the group tab on whichever split hosts it
    /// and marks the panel's leaf as the focused inner leaf.
    #[cfg(feature = "plugins")]
    pub(super) fn focus_panel(&mut self, group_id: usize, panel_name: String) {
        let bg_id = BufferGroupId(group_id);
        let (group_leaf_id, inner_leaf) = match self.active_window_mut().buffer_groups.get(&bg_id) {
            Some(group) => {
                let Some(&inner) = group.panel_splits.get(&panel_name) else {
                    return;
                };
                let Some(leaf) = group.representative_split else {
                    return;
                };
                (leaf, inner)
            }
            None => return,
        };

        // Find the host split whose open_buffers contains this group tab.
        let host_split = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .iter()
            .find(|(_, vs)| vs.has_group(group_leaf_id))
            .map(|(sid, _)| *sid);

        if let Some(host_split) = host_split {
            // Ensure the host split is the active one.
            self.windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_manager_mut())
                .expect("active window must have a populated split layout")
                .set_active_split(host_split);
            if let Some(vs) = self
                .windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_view_states_mut())
                .expect("active window must have a populated split layout")
                .get_mut(&host_split)
            {
                vs.active_group_tab = Some(group_leaf_id);
                vs.focused_group_leaf = Some(inner_leaf);
            }
            // Persist the choice on the SplitNode so a tab-away/back round
            // trip restores the same panel — `activate_group_tab` reads
            // this field when re-focusing the group.
            if let Some(crate::view::split::SplitNode::Grouped {
                active_inner_leaf, ..
            }) = self
                .active_window_mut()
                .grouped_subtrees
                .get_mut(&group_leaf_id)
            {
                *active_inner_leaf = inner_leaf;
            }
            // Transfer focus away from File Explorer (or any other context)
            // to the editor, since we're explicitly focusing a panel.
            self.active_window_mut().key_context = crate::input::keybindings::KeyContext::Normal;
        }
    }

    /// Re-point a buffer group's panel at a different buffer id.
    ///
    /// Updates two places: `group.panel_buffers[panel_name]` (the
    /// authoritative name → buffer mapping for the group) and the
    /// panel split's `SplitViewState.active_buffer` (which buffer the
    /// panel actually renders). Marks the split's layout dirty so the
    /// next render sees the swap.
    ///
    /// Designed for streaming plugins that allocate one file-backed
    /// buffer per item and re-target the panel on navigation, instead
    /// of mutating a single shared buffer's contents.
    ///
    /// Returns `true` on success, `false` if the group, panel, or
    /// buffer was missing.
    #[cfg(feature = "plugins")]
    pub(super) fn set_buffer_group_panel_buffer(
        &mut self,
        group_id: usize,
        panel_name: String,
        new_buffer_id: BufferId,
    ) -> bool {
        let bg_id = BufferGroupId(group_id);

        // Validate the buffer exists before touching anything.
        let buffer_exists = self
            .windows
            .get(&self.active_window)
            .map(|w| &w.buffers)
            .map(|b| b.get(&new_buffer_id).is_some())
            .unwrap_or(false);
        if !buffer_exists {
            tracing::warn!(
                "setBufferGroupPanelBuffer: buffer {:?} not found",
                new_buffer_id
            );
            return false;
        }

        // Look up the panel's inner leaf id, and the prior buffer id
        // we're replacing.
        let (panel_leaf, prior_buffer_id) =
            match self.active_window_mut().buffer_groups.get_mut(&bg_id) {
                Some(group) => {
                    let Some(&leaf) = group.panel_splits.get(&panel_name) else {
                        tracing::warn!(
                            "setBufferGroupPanelBuffer: panel '{}' missing in group {}",
                            panel_name,
                            group_id
                        );
                        return false;
                    };
                    let prior = group
                        .panel_buffers
                        .insert(panel_name.clone(), new_buffer_id);
                    (leaf, prior)
                }
                None => {
                    tracing::warn!("setBufferGroupPanelBuffer: group {} not found", group_id);
                    return false;
                }
            };

        // Maintain the reverse mapping `buffer_to_group` so the new
        // buffer is recognised as part of this group everywhere mode
        // resolution / close handling looks it up. The prior panel
        // buffer is only de-registered if it isn't still pointed at
        // by some other panel in the same group (rare, but possible
        // with custom layouts).
        if let Some(prior) = prior_buffer_id {
            let still_panel = self
                .active_window()
                .buffer_groups
                .get(&bg_id)
                .map(|g| g.panel_buffers.values().any(|b| *b == prior))
                .unwrap_or(false);
            if !still_panel {
                self.active_window_mut().buffer_to_group.remove(&prior);
            }
        }
        self.active_window_mut()
            .buffer_to_group
            .insert(new_buffer_id, bg_id);

        // The buffer needs the same per-buffer presentation flags
        // that `build_group_layout` applies to virtual panel buffers
        // (scrollable, no line-number margins, editing disabled).
        // Without these, a freshly-attached file-backed buffer
        // renders with the wrong margins/wrap and overflows the
        // panel's allotted width.
        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&new_buffer_id)
        {
            state.scrollable = true;
            state.editing_disabled = true;
            state.margins.configure_for_line_numbers(false);
        }

        // Walk the grouped subtree and update the SplitNode::Leaf's
        // `buffer_id`. The renderer reads this — not the
        // SplitViewState — when collecting which buffer to draw in
        // each panel rect (see `get_leaves_with_rects`). Without
        // this, retargeting only updates focus state and the panel
        // keeps drawing the prior (now-empty) buffer.
        for node in self.active_window_mut().grouped_subtrees.values_mut() {
            if let Some(crate::view::split::SplitNode::Leaf { buffer_id, .. }) =
                node.find_mut(panel_leaf.into())
            {
                *buffer_id = new_buffer_id;
                break;
            }
        }

        // Update the panel split's view state: ensure a per-buffer
        // state entry for the new id BEFORE swapping active_buffer
        // (otherwise the next `active_state()` panics because the
        // freshly-set active_buffer has no keyed_states entry yet).
        let line_wrap = self
            .active_window()
            .resolve_line_wrap_for_buffer(new_buffer_id);
        let wrap_column = self
            .active_window()
            .resolve_wrap_column_for_buffer(new_buffer_id);
        let cfg = self.config.editor.clone();
        if let Some(vs) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .get_mut(&panel_leaf)
        {
            // 1) Allocate the keyed state for the new buffer first.
            //    This call internally reads `active_state()` to copy
            //    viewport dims; calling it while active_buffer is
            //    still the prior id is safe.
            {
                let buf_state = vs.ensure_buffer_state(new_buffer_id);
                buf_state.apply_config_defaults(
                    cfg.line_numbers,
                    cfg.highlight_current_line,
                    line_wrap,
                    cfg.wrap_indent,
                    wrap_column,
                    cfg.rulers,
                    cfg.scroll_offset,
                );
                // Match the panel-buffer presentation set in
                // `build_group_layout` (no line numbers, no current-
                // line highlight inside grouped panels).
                buf_state.show_line_numbers = false;
                buf_state.highlight_current_line = false;
            }
            // 2) Now flip the active pointer.
            vs.active_buffer = new_buffer_id;
            vs.layout_dirty = true;
        }

        // Mark the new buffer as hidden from tabs (panel buffers
        // shouldn't show in quick-switch) — matches create-time logic.
        if let Some(meta) = self
            .active_window_mut()
            .buffer_metadata
            .get_mut(&new_buffer_id)
        {
            meta.hidden_from_tabs = true;
        }

        tracing::info!(
            "setBufferGroupPanelBuffer: group {} panel '{}' {:?} -> {:?}",
            group_id,
            panel_name,
            prior_buffer_id,
            new_buffer_id
        );
        true
    }

    /// Activate a group tab by its Grouped-node LeafId in the given split.
    /// Records the group as the split's active tab so the group's layout
    /// becomes visible in that split's content area, and moves keyboard
    /// focus to the group's active inner leaf. If `split_id` is not the
    /// currently active split (e.g. the user clicked a group tab in a
    /// non-focused pane), focus is transferred to it — tab clicks are
    /// commitment gestures pointing at the clicked pane.
    pub(crate) fn activate_group_tab(&mut self, split_id: LeafId, group_leaf: LeafId) {
        // Find the inner active leaf and its buffer from the stored Grouped node.
        let Some(crate::view::split::SplitNode::Grouped {
            active_inner_leaf, ..
        }) = self.active_window().grouped_subtrees.get(&group_leaf)
        else {
            return;
        };
        let inner_leaf = *active_inner_leaf;

        // If activating a group tab in a non-focused split, transfer focus
        // to that split first so subsequent keyboard input routes to the
        // group's inner panel rather than the previously-active pane. This
        // mirrors how clicking a buffer tab in another split moves focus.
        if self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split()
            != split_id
        {
            self.active_window_mut()
                .promote_preview_if_not_in_split(split_id);
            if self.active_window_mut().key_context
                == crate::input::keybindings::KeyContext::FileExplorer
            {
                self.active_window_mut().key_context =
                    crate::input::keybindings::KeyContext::Normal;
            }
            self.windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_manager_mut())
                .expect("active window must have a populated split layout")
                .set_active_split(split_id);
        }

        // Record the group as the active-tab and focused inner leaf for
        // this split. The inner leaf is NOT in the main split tree — it
        // only exists inside the stashed Grouped subtree — so focus is
        // routed via `focused_group_leaf` rather than `focus_split`.
        if let Some(vs) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .get_mut(&split_id)
        {
            vs.active_group_tab = Some(group_leaf);
            vs.focused_group_leaf = Some(inner_leaf);
        }
    }

    /// Look up the ratio of a split container that lives inside one of the
    /// stashed Grouped subtrees (i.e. not in the main split tree). Returns
    /// `None` if no grouped subtree contains this container.
    pub(crate) fn grouped_split_ratio(
        &self,
        container: crate::model::event::ContainerId,
    ) -> Option<f32> {
        self.active_window().grouped_split_ratio(container)
    }

    /// Set the ratio of a split container that lives inside a stashed
    /// Grouped subtree. Returns `true` if the container was found and
    /// updated.
    pub(crate) fn set_grouped_split_ratio(
        &mut self,
        container: crate::model::event::ContainerId,
        new_ratio: f32,
    ) -> bool {
        self.active_window_mut()
            .set_grouped_split_ratio(container, new_ratio)
    }

    /// Close a buffer group by its Grouped-node LeafId (used by tab close button).
    pub(crate) fn close_buffer_group_by_leaf(&mut self, group_leaf: LeafId) {
        // Find the BufferGroupId whose stored representative_split matches
        // this Grouped node's LeafId.
        let bg_id_opt = self
            .active_window_mut()
            .buffer_groups
            .iter()
            .find(|(_, g)| g.representative_split == Some(group_leaf))
            .map(|(id, _)| id.0);

        if let Some(bg_id) = bg_id_opt {
            self.close_buffer_group(bg_id);
        }
    }
}

impl crate::app::window::Window {
    /// Look up the ratio of a split container that lives inside one of the
    /// stashed Grouped subtrees (i.e. not in the main split tree). Returns
    /// `None` if no grouped subtree contains this container.
    pub fn grouped_split_ratio(&self, container: crate::model::event::ContainerId) -> Option<f32> {
        use crate::view::split::SplitNode;
        for node in self.grouped_subtrees.values() {
            if let Some(SplitNode::Split { ratio, .. }) = node.find(container.into()) {
                return Some(*ratio);
            }
        }
        None
    }

    /// Set the ratio of a split container that lives inside a stashed
    /// Grouped subtree. Returns `true` if the container was found and
    /// updated.
    pub fn set_grouped_split_ratio(
        &mut self,
        container: crate::model::event::ContainerId,
        new_ratio: f32,
    ) -> bool {
        use crate::view::split::SplitNode;
        for node in self.grouped_subtrees.values_mut() {
            if let Some(SplitNode::Split { ratio, .. }) = node.find_mut(container.into()) {
                *ratio = new_ratio.clamp(0.1, 0.9);
                return true;
            }
        }
        false
    }

    /// Whether the given buffer is marked non-scrollable. Buffer-group
    /// panels can set `scrollable: false` (and Fixed panels default to
    /// it) so the mouse wheel is a no-op and no scrollbar is drawn.
    pub fn is_non_scrollable_buffer(&self, buffer_id: BufferId) -> bool {
        self.buffers.get(&buffer_id).is_some_and(|s| !s.scrollable)
    }
}

/// Get the fixed height of a layout node if it's a Fixed leaf.
#[cfg(feature = "plugins")]
fn fixed_height_of(node: &GroupLayoutNode) -> Option<u16> {
    match node {
        GroupLayoutNode::Fixed { height, .. } => Some(*height),
        _ => None,
    }
}

// `is_non_scrollable_buffer` moved to `impl Window` above. Editor
// callers reach it via `self.active_window().is_non_scrollable_buffer(...)`.

/// Find the first scrollable leaf in the layout tree.
#[cfg(feature = "plugins")]
fn find_first_scrollable_name(node: &GroupLayoutNode) -> Option<String> {
    match node {
        GroupLayoutNode::Scrollable { id, .. } => Some(id.clone()),
        GroupLayoutNode::Fixed { .. } => None,
        GroupLayoutNode::Split { first, second, .. } => {
            find_first_scrollable_name(first).or_else(|| find_first_scrollable_name(second))
        }
    }
}

/// Find the first scrollable leaf's LeafId from the panel_splits map.
#[cfg(feature = "plugins")]
fn find_first_scrollable_leaf(
    node: &GroupLayoutNode,
    panel_splits: &HashMap<String, LeafId>,
) -> Option<LeafId> {
    find_first_scrollable_name(node).and_then(|name| panel_splits.get(&name).copied())
}
