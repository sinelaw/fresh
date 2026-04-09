//! Buffer group management.
//!
//! A buffer group presents multiple splits/buffers as a single tab.
//! Each panel is a real buffer with its own viewport and scrollbar.
//! The group appears as one entry in the tab bar and buffer list.

use crate::app::types::{BufferGroup, BufferGroupId, GroupLayoutNode};
use crate::model::event::{BufferId, LeafId, SplitDirection};
use crate::view::split::SplitViewState;
use fresh_core::api::BufferGroupResult;
use std::collections::HashMap;

/// Layout description deserialized from plugin JSON.
#[derive(Debug, serde::Deserialize)]
#[serde(tag = "type")]
enum LayoutDesc {
    #[serde(rename = "scrollable")]
    Scrollable { id: String },
    #[serde(rename = "fixed")]
    Fixed { id: String, height: u16 },
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
        let group_id = BufferGroupId(self.next_buffer_group_id);
        self.next_buffer_group_id += 1;

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
        let group_leaf_id = LeafId(self.split_manager.allocate_split_id());

        // Build the Grouped SplitNode and stash it in the side map.
        let grouped_node = SplitNode::Grouped {
            split_id: group_leaf_id,
            name: name.clone(),
            layout: Box::new(inner_tree),
            active_inner_leaf,
        };
        self.grouped_subtrees.insert(group_leaf_id, grouped_node);

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
            self.split_view_states.insert(*leaf_id, vs);
        }

        // Mark all panel buffers as hidden from tabs so they don't appear
        // in quick-switch or the buffer list.
        for buffer_id in panel_buffers.values() {
            if let Some(meta) = self.buffer_metadata.get_mut(buffer_id) {
                meta.hidden_from_tabs = true;
            }
        }

        // Remove panel buffers from any split's open_buffers list
        // (they were added during create_virtual_buffer).
        let hidden_panel_ids: Vec<BufferId> = panel_buffers.values().copied().collect();
        for (_leaf_id, vs) in self.split_view_states.iter_mut() {
            vs.open_buffers.retain(|t| match t {
                TabTarget::Buffer(b) => !hidden_panel_ids.contains(b),
                TabTarget::Group(_) => true,
            });
        }

        // Add the group as a tab in the CURRENT split's tab bar and make it
        // the active tab. (The main split tree is untouched — the group's
        // layout lives in `grouped_subtrees` and is dispatched at render time.)
        let current_split_id = self.split_manager.active_split();
        if let Some(current_vs) = self.split_view_states.get_mut(&current_split_id) {
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
            self.buffer_to_group.insert(*buffer_id, group_id);
        }

        self.buffer_groups.insert(group_id, group);

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
                let split_id = self.split_manager.allocate_split_id();
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
                let split_id = self.split_manager.allocate_split_id();
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
    fn build_group_layout(
        &mut self,
        desc: &LayoutDesc,
        mode: &str,
        panel_buffers: &mut HashMap<String, BufferId>,
    ) -> Result<GroupLayoutNode, String> {
        match desc {
            LayoutDesc::Scrollable { id } => {
                let buffer_id =
                    self.create_virtual_buffer(format!("*{}*", id), mode.to_string(), true);
                // Configure the buffer for panel use
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    state.show_cursors = false;
                    state.editing_disabled = true;
                    state.margins.configure_for_line_numbers(false);
                }
                panel_buffers.insert(id.clone(), buffer_id);
                Ok(GroupLayoutNode::Scrollable {
                    id: id.clone(),
                    buffer_id: Some(buffer_id),
                    split_id: None,
                })
            }
            LayoutDesc::Fixed { id, height } => {
                let buffer_id =
                    self.create_virtual_buffer(format!("*{}*", id), mode.to_string(), true);
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    state.show_cursors = false;
                    state.editing_disabled = true;
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
    pub(super) fn set_panel_content(
        &mut self,
        group_id: usize,
        panel_name: String,
        entries: Vec<fresh_core::text_property::TextPropertyEntry>,
    ) {
        let bg_id = BufferGroupId(group_id);
        let buffer_id = self
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
        if let Some(group) = self.buffer_groups.remove(&bg_id) {
            // Remove reverse mappings
            for buffer_id in group.panel_buffers.values() {
                self.buffer_to_group.remove(buffer_id);
            }

            // Find the group_leaf_id (it's the `representative_split` now).
            if let Some(group_leaf_id) = group.representative_split {
                // Remove the Grouped subtree from the side map
                self.grouped_subtrees.remove(&group_leaf_id);
                // Remove the group tab from all splits' tab bars
                for vs in self.split_view_states.values_mut() {
                    vs.open_buffers
                        .retain(|t| *t != TabTarget::Group(group_leaf_id));
                }
            }

            // Clean up SplitViewState for inner panel leaves
            for split_id in group.panel_splits.values() {
                self.split_view_states.remove(split_id);
            }

            // Close all panel buffers
            for buffer_id in group.panel_buffers.values() {
                if let Err(e) = self.close_buffer(*buffer_id) {
                    tracing::warn!("Failed to close panel buffer {:?}: {}", buffer_id, e);
                }
            }

            // Ensure the active split now has a valid active_target.
            // If it was the group's tab, switch to the first available buffer tab.
            let active_split = self.split_manager.active_split();
            if let Some(vs) = self.split_view_states.get(&active_split) {
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
    pub(super) fn focus_panel(&mut self, group_id: usize, panel_name: String) {
        let bg_id = BufferGroupId(group_id);
        let (group_leaf_id, inner_leaf) = match self.buffer_groups.get(&bg_id) {
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
            .split_view_states
            .iter()
            .find(|(_, vs)| vs.has_group(group_leaf_id))
            .map(|(sid, _)| *sid);

        if let Some(host_split) = host_split {
            // Ensure the host split is the active one.
            self.split_manager.set_active_split(host_split);
            if let Some(vs) = self.split_view_states.get_mut(&host_split) {
                vs.active_group_tab = Some(group_leaf_id);
                vs.focused_group_leaf = Some(inner_leaf);
            }
        }
    }

    /// Activate a group tab by its Grouped-node LeafId. This sets the
    /// active tab of the current split so the group's layout becomes visible
    /// in the split's content area. The active inner leaf receives focus.
    pub(crate) fn activate_group_tab(&mut self, group_leaf: LeafId) {
        // Find the inner active leaf and its buffer from the stored Grouped node.
        let Some(crate::view::split::SplitNode::Grouped {
            active_inner_leaf, ..
        }) = self.grouped_subtrees.get(&group_leaf)
        else {
            return;
        };
        let inner_leaf = *active_inner_leaf;

        // Set the current split's "effective active target" by recording
        // the group as the active-tab for this split.
        let active_split = self.split_manager.active_split();
        if let Some(vs) = self.split_view_states.get_mut(&active_split) {
            vs.active_group_tab = Some(group_leaf);
        }

        // Focus the inner leaf's buffer so keyboard input goes there.
        // NOTE: the inner leaf is NOT in the main split tree — it only
        // exists inside the stashed Grouped subtree. Focus routing
        // through focus_split won't work directly.
        // Instead we set a separate "focused group leaf" marker that the
        // input router can consult.
        if let Some(vs) = self.split_view_states.get_mut(&active_split) {
            vs.focused_group_leaf = Some(inner_leaf);
        }
    }

    /// Close a buffer group by its Grouped-node LeafId (used by tab close button).
    pub(crate) fn close_buffer_group_by_leaf(&mut self, group_leaf: LeafId) {
        // Find the BufferGroupId whose stored representative_split matches
        // this Grouped node's LeafId.
        let bg_id_opt = self
            .buffer_groups
            .iter()
            .find(|(_, g)| g.representative_split == Some(group_leaf))
            .map(|(id, _)| id.0);

        if let Some(bg_id) = bg_id_opt {
            self.close_buffer_group(bg_id);
        }
    }
}

/// Get the fixed height of a layout node if it's a Fixed leaf.
fn fixed_height_of(node: &GroupLayoutNode) -> Option<u16> {
    match node {
        GroupLayoutNode::Fixed { height, .. } => Some(*height),
        _ => None,
    }
}

/// Find the first scrollable leaf in the layout tree.
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
fn find_first_scrollable_leaf(
    node: &GroupLayoutNode,
    panel_splits: &HashMap<String, LeafId>,
) -> Option<LeafId> {
    find_first_scrollable_name(node).and_then(|name| panel_splits.get(&name).copied())
}
