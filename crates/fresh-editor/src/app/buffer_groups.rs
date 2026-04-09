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
    pub(super) fn create_buffer_group(
        &mut self,
        name: String,
        mode: String,
        layout_json: String,
    ) -> Result<BufferGroupResult, String> {
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

        // Build the split tree directly from the layout
        let split_tree = self.build_split_tree(&layout, &mut panel_splits)?;

        // Determine the active leaf (first scrollable panel, fallback to any leaf)
        let active_leaf = find_first_scrollable_leaf(&layout, &panel_splits)
            .or_else(|| panel_splits.values().next().copied())
            .ok_or("No panels in layout")?;

        // Replace the split manager's root with our tree
        self.split_manager.replace_root(split_tree, active_leaf);

        // Create SplitViewState for each panel split
        let (tw, th) = (self.terminal_width, self.terminal_height);
        for (panel_name, leaf_id) in &panel_splits {
            let buffer_id = *panel_buffers
                .get(panel_name)
                .ok_or(format!("Panel '{}' has no buffer", panel_name))?;
            let mut vs = SplitViewState::with_buffer(tw, th, buffer_id);
            vs.suppress_chrome = true;
            vs.hide_tilde = true;
            if let Some(bs) = vs.keyed_states.get_mut(&buffer_id) {
                bs.show_line_numbers = false;
                bs.highlight_current_line = false;
            }
            self.split_view_states.insert(*leaf_id, vs);
        }

        // Use the first scrollable panel as the representative
        let first_buffer_id = *panel_buffers
            .get(&find_first_scrollable_name(&layout).unwrap_or_default())
            .or_else(|| panel_buffers.values().next())
            .ok_or("No panels")?;

        // Mark all panel buffers as hidden from tabs
        for buffer_id in panel_buffers.values() {
            if let Some(meta) = self.buffer_metadata.get_mut(buffer_id) {
                meta.hidden_from_tabs = true;
            }
        }

        // The first panel's buffer serves as the "representative" tab entry.
        // Show it in the tab bar with the group name.
        if let Some(meta) = self.buffer_metadata.get_mut(&first_buffer_id) {
            meta.hidden_from_tabs = false;
            meta.display_name = name.clone();
        }

        let representative_split = Some(active_leaf);

        // Register the group
        let group = BufferGroup {
            id: group_id,
            name: name.clone(),
            mode,
            layout,
            panel_buffers: panel_buffers.clone(),
            panel_splits,
            representative_split,
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

    /// Close a buffer group — close all splits and buffers.
    pub(super) fn close_buffer_group(&mut self, group_id: usize) {
        let bg_id = BufferGroupId(group_id);
        if let Some(group) = self.buffer_groups.remove(&bg_id) {
            // Remove reverse mappings
            for buffer_id in group.panel_buffers.values() {
                self.buffer_to_group.remove(buffer_id);
            }

            // Close all splits (close the splits, buffers get cleaned up)
            for (_, split_id) in &group.panel_splits {
                let _ = self.split_manager.close_split(*split_id);
            }

            // Close all buffers
            for (_, buffer_id) in &group.panel_buffers {
                let _ = self.close_buffer(*buffer_id);
            }
        }
    }

    /// Focus a specific panel in a buffer group.
    pub(super) fn focus_panel(&mut self, group_id: usize, panel_name: String) {
        let bg_id = BufferGroupId(group_id);
        if let Some(group) = self.buffer_groups.get(&bg_id) {
            if let Some(&split_id) = group.panel_splits.get(&panel_name) {
                if let Some(&buffer_id) = group.panel_buffers.get(&panel_name) {
                    self.focus_split(split_id, buffer_id);
                }
            }
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
