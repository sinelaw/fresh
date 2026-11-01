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
        /// An existing buffer for the panel to show, instead of a new
        /// virtual one. See `BufferGroup::borrowed_buffers`.
        #[serde(rename = "bufferId")]
        buffer_id: Option<usize>,
        /// A file for the panel to show: the buffer that has it open, else
        /// the group opens it, without a tab. See
        /// `BufferGroup::opened_buffers`.
        path: Option<String>,
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
        /// As for `Scrollable`.
        #[serde(rename = "bufferId")]
        buffer_id: Option<usize>,
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
        let mut borrowed = std::collections::HashSet::new();
        let mut opened = std::collections::HashSet::new();
        let layout =
            self.build_group_layout(&desc, &mode, &mut panel_buffers, &mut borrowed, &mut opened)?;

        // Build the inner split tree for the group
        let inner_tree = self.build_split_tree(&layout, &mut panel_splits)?;

        // Determine the active inner leaf (first scrollable panel, fallback to any leaf)
        let active_inner_leaf = find_first_scrollable_leaf(&layout, &panel_splits)
            .or_else(|| panel_splits.values().next().copied())
            .ok_or("No panels in layout")?;

        // Allocate a LeafId for the Grouped node itself. This is what the
        // tab bar uses to reference this group (`TabTarget::Group(group_leaf_id)`).
        let group_leaf_id = LeafId(
            self.active_window_mut()
                .split_manager_mut()
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

        // Create SplitViewState for each inner panel leaf. A panel on a
        // borrowed buffer opens where the pane that will hold the group was
        // in it, as `return_to_borrowed_buffer` hands it back on close.
        let host_split = self.active_window().split_manager().active_split();
        for (panel_name, leaf_id) in &panel_splits {
            let buffer_id = *panel_buffers
                .get(panel_name)
                .ok_or(format!("Panel '{}' has no buffer", panel_name))?;
            let is_borrowed = borrowed.contains(&buffer_id);
            let mut vs = self.panel_view_state(buffer_id, is_borrowed);
            let host_view = is_borrowed
                .then(|| {
                    let host = self.active_window().split_view_states().get(&host_split)?;
                    let bs = host.buffer_state(buffer_id)?;
                    Some((bs.cursors.clone(), bs.viewport.anchor))
                })
                .flatten();
            if let (Some((cursors, anchor)), Some(bs)) = (host_view, vs.buffer_state_mut(buffer_id))
            {
                bs.cursors = cursors;
                bs.viewport.anchor = anchor;
            }
            self.active_window_mut()
                .split_view_states_mut()
                .insert(*leaf_id, vs);
        }

        // Everything below is for the buffers the group owns. A borrowed
        // buffer stays an ordinary buffer: in its tabs, in quick-switch, and
        // routed and keyed as usual.
        let owned_buffers: Vec<BufferId> = panel_buffers
            .values()
            .copied()
            .filter(|b| !borrowed.contains(b))
            .collect();

        // Mark the owned panel buffers as hidden from tabs so they don't
        // appear in quick-switch or the buffer list.
        for buffer_id in &owned_buffers {
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
        let hidden_panel_ids = &owned_buffers;
        let panel_leaf_ids: std::collections::HashSet<LeafId> =
            panel_splits.values().copied().collect();
        for (leaf_id, vs) in self.active_window_mut().split_view_states_mut().iter_mut() {
            if panel_leaf_ids.contains(leaf_id) {
                // The panel's own view state needs its buffer.
                continue;
            }
            vs.open_buffers.retain(|t| match t {
                TabTarget::Buffer(b) => !hidden_panel_ids.contains(b),
                TabTarget::Group(_) => true,
            });
            vs.retain_buffer_states(|bid| !hidden_panel_ids.contains(&bid));
        }

        // Add the group as a tab in the CURRENT split's tab bar and make it
        // the active tab. (The main split tree is untouched — the group's
        // layout lives in `grouped_subtrees` and is dispatched at render time.)
        let current_split_id = self.active_window().split_manager().active_split();
        if let Some(current_vs) = self
            .active_window_mut()
            .split_view_states_mut()
            .get_mut(&current_split_id)
        {
            current_vs.add_group(group_leaf_id);
            current_vs.show_group(group_leaf_id, active_inner_leaf);
        }

        // Register the group metadata
        let group = BufferGroup {
            id: group_id,
            name: name.clone(),
            mode,
            layout,
            panel_buffers: panel_buffers.clone(),
            panel_splits: panel_splits.clone(),
            hidden_panels: std::collections::HashSet::new(),
            borrowed_buffers: borrowed,
            opened_buffers: opened,
            representative_split: Some(group_leaf_id),
            title: None,
        };

        // Register reverse mapping, which routes `showBuffer` to the group
        // and gives the buffer the group's mode
        for buffer_id in &owned_buffers {
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

        let panel_splits = panel_splits
            .iter()
            .map(|(name, leaf)| (name.clone(), leaf.0 .0 as u64))
            .collect();

        Ok(BufferGroupResult {
            group_id: group_id.0 as u64,
            panels,
            panel_splits,
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
                    .active_window_mut()
                    .split_manager_mut()
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
                    .active_window_mut()
                    .split_manager_mut()
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
        borrowed: &mut std::collections::HashSet<BufferId>,
        opened: &mut std::collections::HashSet<BufferId>,
    ) -> Result<GroupLayoutNode, String> {
        match desc {
            LayoutDesc::Scrollable {
                id,
                path: Some(path),
                buffer_id: None,
                ..
            } => {
                let buffer_id = self.open_panel_file(id, path, opened)?;
                panel_buffers.insert(id.clone(), buffer_id);
                borrowed.insert(buffer_id);
                Ok(GroupLayoutNode::Scrollable {
                    id: id.clone(),
                    buffer_id: Some(buffer_id),
                    split_id: None,
                })
            }
            LayoutDesc::Scrollable {
                id,
                buffer_id: Some(existing),
                ..
            } => {
                let buffer_id = self.borrow_panel_buffer(id, *existing, panel_buffers, borrowed)?;
                Ok(GroupLayoutNode::Scrollable {
                    id: id.clone(),
                    buffer_id: Some(buffer_id),
                    split_id: None,
                })
            }
            LayoutDesc::Fixed {
                id,
                height,
                buffer_id: Some(existing),
                ..
            } => {
                let buffer_id = self.borrow_panel_buffer(id, *existing, panel_buffers, borrowed)?;
                Ok(GroupLayoutNode::Fixed {
                    id: id.clone(),
                    height: *height,
                    buffer_id: Some(buffer_id),
                    split_id: None,
                })
            }
            LayoutDesc::Scrollable {
                id,
                scrollable,
                buffer_id: None,
                path: None,
            } => {
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
                buffer_id: None,
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
                let first_node =
                    self.build_group_layout(first, mode, panel_buffers, borrowed, opened)?;
                let second_node =
                    self.build_group_layout(second, mode, panel_buffers, borrowed, opened)?;
                Ok(GroupLayoutNode::Split {
                    direction: dir,
                    ratio: *ratio,
                    first: Box::new(first_node),
                    second: Box::new(second_node),
                })
            }
        }
    }

    /// Record that panel `id` shows the existing buffer `existing`, which the
    /// group borrows rather than owns: its flags are left as they are.
    #[cfg(feature = "plugins")]
    fn borrow_panel_buffer(
        &self,
        id: &str,
        existing: usize,
        panel_buffers: &mut HashMap<String, BufferId>,
        borrowed: &mut std::collections::HashSet<BufferId>,
    ) -> Result<BufferId, String> {
        let buffer_id = BufferId(existing);
        if self.active_window().buffers.get(&buffer_id).is_none() {
            return Err(format!("Panel '{}': no buffer {}", id, existing));
        }
        panel_buffers.insert(id.to_string(), buffer_id);
        borrowed.insert(buffer_id);
        Ok(buffer_id)
    }

    /// The buffer a `path` leaf shows: the one that already has the file
    /// open, which the group then borrows like a `bufferId` leaf, or one the
    /// group opens itself, with no tab, recorded in `opened`.
    #[cfg(feature = "plugins")]
    fn open_panel_file(
        &mut self,
        id: &str,
        path: &str,
        opened: &mut std::collections::HashSet<BufferId>,
    ) -> Result<BufferId, String> {
        let path = std::path::PathBuf::from(path);
        let path = if path.is_relative() {
            self.working_dir().join(path)
        } else {
            path
        };
        let canonical = std::fs::canonicalize(&path).unwrap_or_else(|_| path.clone());
        let already_open = self
            .active_window()
            .buffers
            .iter()
            .find(|(_, state)| {
                state.buffer.file_path().is_some_and(|p| {
                    p == path || std::fs::canonicalize(p).is_ok_and(|p| p == canonical)
                })
            })
            .map(|(bid, _)| *bid);
        if let Some(buffer_id) = already_open {
            return Ok(buffer_id);
        }
        let buffer_id = self
            .open_file_no_focus(&path)
            .map_err(|e| format!("Panel '{id}': cannot open {}: {e}", path.display()))?;
        // `open_file_no_focus` gives the buffer a tab in the preferred pane;
        // the group's panel is the only place it is shown.
        for view_state in self
            .active_window_mut()
            .split_view_states_mut()
            .values_mut()
        {
            view_state.remove_buffer(buffer_id);
        }
        opened.insert(buffer_id);
        Ok(buffer_id)
    }

    /// The view state a panel starts with. Every panel leaves the tab bar to
    /// the pane showing the group. A panel on a buffer the group owns also
    /// drops the editor's chrome, as the plugin lays its content out itself;
    /// one on a borrowed buffer is set up like any pane opening that buffer.
    #[cfg(feature = "plugins")]
    fn panel_view_state(&self, buffer_id: BufferId, borrowed: bool) -> SplitViewState {
        let mut vs =
            SplitViewState::with_buffer(self.terminal_width, self.terminal_height, buffer_id);
        vs.suppress_chrome = true;
        vs.hide_tilde = true;
        if borrowed {
            let window = self.active_window();
            let cfg = &self.config.editor;
            if let Some(bs) = vs.buffer_state_mut(buffer_id) {
                bs.apply_config_defaults(crate::view::split::ViewConfigDefaults {
                    line_numbers: cfg.line_numbers,
                    highlight_current_line: cfg.highlight_current_line,
                    line_wrap: window.resolve_line_wrap_for_buffer(buffer_id),
                    wrap_indent: cfg.wrap_indent,
                    wrap_column: window.resolve_wrap_column_for_buffer(buffer_id),
                    rulers: cfg.rulers.clone(),
                    scroll_offset: cfg.scroll_offset,
                });
            }
        } else if let Some(bs) = vs.buffer_state_mut(buffer_id) {
            bs.show_line_numbers = false;
            bs.highlight_current_line = false;
            // Panel content is composed by the plugin at a width it
            // already knows (it truncates or pre-wraps its own rows), so
            // soft-wrapping it again just re-flows lines the plugin laid
            // out — a diff row spilling onto three screen rows, say. Panels
            // never wrap; the horizontal scrollbar handles overflow.
            bs.viewport.line_wrap_enabled = false;
        }
        vs
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
        self.close_buffer_group_leaving(group_id, None);
    }

    /// [`Self::close_buffer_group`], leaving `leaving` alone: the buffer
    /// whose own close (or move to another window) is closing the group,
    /// which must not be closed or given a tab from inside that.
    fn close_buffer_group_leaving(&mut self, group_id: usize, leaving: Option<BufferId>) {
        let bg_id = BufferGroupId(group_id);
        if let Some(group) = self.active_window_mut().buffer_groups.remove(&bg_id) {
            // The pane showing the group's tab, where a buffer the group
            // opened gets a tab of its own if it has unsaved changes.
            let host = group.representative_split.and_then(|group_leaf| {
                self.active_window()
                    .split_view_states()
                    .iter()
                    .find(|(_, vs)| {
                        vs.open_buffers.iter().any(
                            |t| matches!(t, crate::view::split::TabTarget::Group(g) if *g == group_leaf),
                        )
                    })
                    .map(|(leaf, _)| *leaf)
            });
            // Remove reverse mappings — only the group's own buffers have one
            for buffer_id in group.panel_buffers.values() {
                if !group.borrowed_buffers.contains(buffer_id) {
                    self.active_window_mut().buffer_to_group.remove(buffer_id);
                }
            }

            // Find the group_leaf_id (it's the `representative_split` now).
            if let Some(group_leaf_id) = group.representative_split {
                self.active_window_mut()
                    .return_to_borrowed_buffer(group_leaf_id, &group.borrowed_buffers);
                // Remove the Grouped subtree from the side map
                self.active_window_mut()
                    .grouped_subtrees
                    .remove(&group_leaf_id);
                // Remove the group tab from all splits' tab bars and clear
                // any active/focused group markers that point at this group.
                for vs in self
                    .active_window_mut()
                    .split_view_states_mut()
                    .values_mut()
                {
                    // Leaves the group too, if the pane was showing it —
                    // and with it the focused panel, which only exists
                    // inside a shown group.
                    vs.remove_group(group_leaf_id);
                    vs.remove_group_from_history(group_leaf_id);
                }
            }

            // Clean up SplitViewState for inner panel leaves
            for split_id in group.panel_splits.values() {
                self.active_window_mut()
                    .split_view_states_mut()
                    .remove(split_id);
            }

            // Close the panel buffers the group owns. Borrowed ones stay
            // open: they were the user's before the group showed them.
            for buffer_id in group.panel_buffers.values() {
                if group.borrowed_buffers.contains(buffer_id) {
                    continue;
                }
                if let Err(e) = self.close_buffer(*buffer_id) {
                    tracing::warn!("Failed to close panel buffer {:?}: {}", buffer_id, e);
                }
            }

            // The files the group opened: one with no changes goes with the
            // group; one with unsaved changes stays, in a tab of the pane
            // that showed the group, so its edits are where the user can see
            // and save or discard them. (A file another pane has a tab of is
            // left as it is.)
            let mut opened: Vec<BufferId> = group
                .opened_buffers
                .iter()
                .copied()
                .filter(|b| Some(*b) != leaving)
                .collect();
            opened.sort_by_key(|b| b.0);
            let (mut to_close, mut to_keep) = (Vec::new(), Vec::new());
            for buffer_id in opened {
                let has_tab = self
                    .active_window()
                    .split_view_states()
                    .values()
                    .any(|vs| vs.has_buffer(buffer_id));
                let Some(modified) = self
                    .active_window()
                    .buffers
                    .get(&buffer_id)
                    .map(|state| state.buffer.is_modified())
                else {
                    continue;
                };
                if has_tab {
                    continue;
                }
                if modified {
                    to_keep.push(buffer_id);
                } else {
                    to_close.push(buffer_id);
                }
            }
            for &buffer_id in &to_keep {
                if let Some(vs) = host.and_then(|leaf| {
                    self.active_window_mut()
                        .split_view_states_mut()
                        .get_mut(&leaf)
                }) {
                    vs.add_buffer(buffer_id);
                    vs.show_buffer(buffer_id);
                }
            }
            // Out of the running as the replacement a closing buffer's pane
            // shows: otherwise closing one gives the next one a tab.
            for &buffer_id in &to_close {
                if let Some(meta) = self.active_window_mut().buffer_metadata.get_mut(&buffer_id) {
                    meta.hidden_from_tabs = true;
                }
            }
            for buffer_id in to_close {
                if let Err(e) = self.close_buffer(buffer_id) {
                    tracing::warn!("Failed to close file {:?} a group opened: {}", buffer_id, e);
                }
            }

            self.plugin_manager.read().unwrap().run_hook(
                "buffer_group_closed",
                fresh_core::hooks::HookArgs::BufferGroupClosed {
                    group_id: group_id as u64,
                    window_id: self.active_window.0,
                },
            );

            // Ensure the active split now has a valid active_target.
            // If it was the group's tab, switch to the first available buffer tab.
            let active_split = self.active_window().split_manager().active_split();
            if let Some(vs) = self.active_window().split_view_states().get(&active_split) {
                if let Some(first_buf) = vs.buffer_tab_ids().next() {
                    let _ = first_buf; // active_buffer is per-leaf; already set
                }
            }
        }
    }

    /// Close every group with a panel on `buffer_id` that it borrowed. Such a
    /// group goes with the buffer: it has nothing else to show there.
    pub(super) fn close_groups_borrowing(&mut self, buffer_id: BufferId) {
        let groups: Vec<usize> = self
            .active_window()
            .buffer_groups
            .values()
            .filter(|g| {
                g.borrowed_buffers.contains(&buffer_id)
                    && g.panel_buffers.values().any(|b| *b == buffer_id)
            })
            .map(|g| g.id.0)
            .collect();
        for group_id in groups {
            self.close_buffer_group_leaving(group_id, Some(buffer_id));
        }
    }

    /// Give group `group_id`'s tab the title `segments` (see
    /// `setBufferGroupTitle`). The group's name becomes the segments' text,
    /// for everything that names the group without marking it (the buffer
    /// switcher, say); the tab strip reads the segments themselves.
    #[cfg(feature = "plugins")]
    pub(super) fn set_buffer_group_title(
        &mut self,
        group_id: usize,
        segments: Vec<fresh_core::api::BufferGroupTitleSegment>,
    ) {
        use crate::view::split::SplitNode;

        let text: String = segments.iter().map(|s| s.text.as_str()).collect();
        let window = self.active_window_mut();
        let Some(group) = window.buffer_groups.get_mut(&BufferGroupId(group_id)) else {
            tracing::warn!("setBufferGroupTitle: group {} not found", group_id);
            return;
        };
        group.name = text.clone();
        group.title = Some(segments);
        let Some(leaf) = group.representative_split else {
            return;
        };
        if let Some(SplitNode::Grouped { name, .. }) = window.grouped_subtrees.get_mut(&leaf) {
            *name = text;
        }
    }

    /// Show or hide one panel of a buffer group.
    ///
    /// The group is not torn down: the panel's buffer, its content and its
    /// scroll position all survive being hidden, because only the group's
    /// inner split tree is rebuilt (from `group.layout`, which always
    /// describes every panel) with the hidden leaves left out. A split whose
    /// children are all hidden collapses to the visible side, so the
    /// remaining panels take over the freed space at their original ratios.
    ///
    /// Returns `false` — leaving the group untouched — when the group or
    /// panel is unknown, or when hiding this panel would leave the group
    /// with nothing to render.
    #[cfg(feature = "plugins")]
    pub(super) fn set_buffer_group_panel_visible(
        &mut self,
        group_id: usize,
        panel_name: &str,
        visible: bool,
    ) -> bool {
        use crate::view::split::SplitNode;

        let bg_id = BufferGroupId(group_id);
        let Some(group) = self.active_window().buffer_groups.get(&bg_id) else {
            tracing::warn!("setBufferGroupPanelVisible: group {} not found", group_id);
            return false;
        };
        if !group.panel_buffers.contains_key(panel_name) {
            tracing::warn!(
                "setBufferGroupPanelVisible: panel '{}' missing in group {}",
                panel_name,
                group_id
            );
            return false;
        }
        let already_hidden = group.hidden_panels.contains(panel_name);
        if already_hidden != visible {
            return true;
        }
        let Some(group_leaf_id) = group.representative_split else {
            return false;
        };
        let layout = group.layout.clone();
        let mut hidden = group.hidden_panels.clone();
        if visible {
            hidden.remove(panel_name);
        } else {
            hidden.insert(panel_name.to_string());
        }
        let existing_leaves = group.panel_splits.clone();
        let current_buffers = group.panel_buffers.clone();

        // Rebuild the tree first: if the request would empty the group,
        // nothing has been mutated yet and we can decline it outright.
        let mut panel_splits: HashMap<String, LeafId> = HashMap::new();
        let Some(inner_tree) = self.build_visible_split_tree(
            &layout,
            &hidden,
            &existing_leaves,
            &current_buffers,
            &mut panel_splits,
        ) else {
            tracing::warn!(
                "setBufferGroupPanelVisible: hiding '{}' would leave group {} empty",
                panel_name,
                group_id
            );
            return false;
        };

        // Every panel keeps its leaf id and view state, visible or not, so a
        // panel that comes back finds its scroll position where it left it.
        // `panel_splits` therefore stays the full map; only the tree shrinks.
        let mut all_splits = existing_leaves.clone();
        all_splits.extend(panel_splits.iter().map(|(k, v)| (k.clone(), *v)));
        let visible_leaves: std::collections::HashSet<LeafId> =
            panel_splits.values().copied().collect();

        // Panels shown for the first time need the same view state that
        // `create_buffer_group` gives every panel at create time.
        for (name, leaf_id) in &panel_splits {
            let has_state = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(_, vs)| vs.contains_key(leaf_id))
                .unwrap_or(false);
            if has_state {
                continue;
            }
            let Some(buffer_id) = self
                .active_window()
                .buffer_groups
                .get(&bg_id)
                .and_then(|g| g.panel_buffers.get(name).copied())
            else {
                continue;
            };
            let borrowed = self
                .active_window()
                .buffer_groups
                .get(&bg_id)
                .is_some_and(|g| g.borrowed_buffers.contains(&buffer_id));
            let vs = self.panel_view_state(buffer_id, borrowed);
            self.active_window_mut()
                .split_view_states_mut()
                .insert(*leaf_id, vs);
        }

        // Swap the tree in, keeping the group's focus on a leaf that is
        // still rendered.
        let mut fallback_leaf = None;
        if let Some(SplitNode::Grouped {
            layout: node_layout,
            active_inner_leaf,
            ..
        }) = self
            .active_window_mut()
            .grouped_subtrees
            .get_mut(&group_leaf_id)
        {
            **node_layout = inner_tree;
            if !visible_leaves.contains(active_inner_leaf) {
                if let Some(leaf) = node_layout.leaf_split_ids().first().copied() {
                    *active_inner_leaf = leaf;
                }
            }
            fallback_leaf = Some(*active_inner_leaf);
        }

        if let Some(group) = self.active_window_mut().buffer_groups.get_mut(&bg_id) {
            group.hidden_panels = hidden;
            group.panel_splits = all_splits;
        }

        for vs in self
            .active_window_mut()
            .split_view_states_mut()
            .values_mut()
        {
            if let (Some((_, focused)), Some(fallback)) = (vs.shown_group(), fallback_leaf) {
                if !visible_leaves.contains(&focused)
                    && existing_leaves.values().any(|l| *l == focused)
                {
                    vs.focus_group_panel(fallback);
                }
            }
        }
        self.plugin_render_requested = true;
        true
    }

    /// Build a group's inner split tree from its layout, skipping hidden
    /// panels and reusing each visible panel's existing leaf id (so its view
    /// state — and with it the scroll position — carries over). Returns
    /// `None` when every leaf in the subtree is hidden.
    #[cfg(feature = "plugins")]
    fn build_visible_split_tree(
        &mut self,
        node: &GroupLayoutNode,
        hidden: &std::collections::HashSet<String>,
        existing: &HashMap<String, LeafId>,
        current_buffers: &HashMap<String, BufferId>,
        panel_splits: &mut HashMap<String, LeafId>,
    ) -> Option<crate::view::split::SplitNode> {
        use crate::view::split::SplitNode;

        match node {
            GroupLayoutNode::Scrollable { id, buffer_id, .. }
            | GroupLayoutNode::Fixed { id, buffer_id, .. } => {
                if hidden.contains(id) {
                    return None;
                }
                // `layout` records the buffer each panel was *created*
                // with. A panel that has since been retargeted
                // (`setBufferGroupPanelBuffer` — the review diff swaps its
                // center panel to a composite for the side-by-side view)
                // must keep the buffer it holds now; rebuilding from the
                // layout's id would silently revert it.
                let buffer_id = current_buffers.get(id).copied().or(*buffer_id)?;
                let leaf_id = existing.get(id).copied().unwrap_or_else(|| {
                    LeafId(
                        self.active_window_mut()
                            .split_manager_mut()
                            .allocate_split_id(),
                    )
                });
                panel_splits.insert(id.clone(), leaf_id);
                Some(SplitNode::leaf(buffer_id, leaf_id.0))
            }
            GroupLayoutNode::Split {
                direction,
                ratio,
                first,
                second,
            } => {
                let first_node = self.build_visible_split_tree(
                    first,
                    hidden,
                    existing,
                    current_buffers,
                    panel_splits,
                );
                let second_node = self.build_visible_split_tree(
                    second,
                    hidden,
                    existing,
                    current_buffers,
                    panel_splits,
                );
                match (first_node, second_node) {
                    (Some(f), Some(s)) => {
                        let split_id = self
                            .active_window_mut()
                            .split_manager_mut()
                            .allocate_split_id();
                        let mut split = SplitNode::split(*direction, f, s, *ratio, split_id);
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
                        Some(split)
                    }
                    // One side hidden: the survivor takes the whole rect.
                    (Some(only), None) | (None, Some(only)) => Some(only),
                    (None, None) => None,
                }
            }
        }
    }

    /// Focus a specific panel in a buffer group.
    ///
    /// If the panel's inner leaf is not in the main split tree (side-map
    /// approach), this activates the group tab on whichever split hosts it
    /// and marks the panel's leaf as the focused inner leaf.
    ///
    /// Focusing a hidden panel is a no-op — there is no rect to put a cursor
    /// in, and routing keys at an unrendered panel would strand input.
    #[cfg(feature = "plugins")]
    pub(super) fn focus_panel(&mut self, group_id: usize, panel_name: String) {
        let bg_id = BufferGroupId(group_id);
        let (group_leaf_id, inner_leaf) = match self.active_window_mut().buffer_groups.get(&bg_id) {
            Some(group) => {
                if group.hidden_panels.contains(&panel_name) {
                    return;
                }
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
            .active_window()
            .split_view_states()
            .iter()
            .find(|(_, vs)| vs.has_group(group_leaf_id))
            .map(|(sid, _)| *sid);

        if let Some(host_split) = host_split {
            // Ensure the host split is the active one.
            self.active_window_mut()
                .split_manager_mut()
                .set_active_split(host_split);
            if let Some(vs) = self
                .active_window_mut()
                .split_view_states_mut()
                .get_mut(&host_split)
            {
                vs.show_group(group_leaf_id, inner_leaf);
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
    /// panel split's buffer tab, `SplitViewState::buffer_tab` (which buffer the
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
        // each panel rect (see `SplitNode::visible_leaves`). Without
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
            .active_window_mut()
            .split_view_states_mut()
            .get_mut(&panel_leaf)
        {
            // 1) Allocate the keyed state for the new buffer first.
            //    This call internally reads `active_state()` to copy
            //    viewport dims; calling it while active_buffer is
            //    still the prior id is safe.
            {
                let buf_state = vs.ensure_buffer_state(new_buffer_id);
                buf_state.apply_config_defaults(crate::view::split::ViewConfigDefaults {
                    line_numbers: cfg.line_numbers,
                    highlight_current_line: cfg.highlight_current_line,
                    line_wrap,
                    wrap_indent: cfg.wrap_indent,
                    wrap_column,
                    rulers: cfg.rulers,
                    scroll_offset: cfg.scroll_offset,
                });
                // Match the panel-buffer presentation set in
                // `build_group_layout` (no line numbers, no current-
                // line highlight, no soft wrap inside grouped panels).
                // The wrap flag must be cleared *after*
                // `apply_config_defaults`, which would otherwise stamp
                // the global `editor.line_wrap` (on by default) onto a
                // panel the plugin already laid out to the panel width.
                buf_state.show_line_numbers = false;
                buf_state.highlight_current_line = false;
                buf_state.viewport.line_wrap_enabled = false;
            }
            // 2) Now flip the active pointer.
            vs.set_buffer_tab(new_buffer_id);
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
        if self.active_window().split_manager().active_split() != split_id {
            self.active_window_mut()
                .promote_preview_if_not_in_split(split_id);
            if self.active_window_mut().key_context
                == crate::input::keybindings::KeyContext::FileExplorer
            {
                self.active_window_mut().key_context =
                    crate::input::keybindings::KeyContext::Normal;
            }
            self.active_window_mut()
                .split_manager_mut()
                .set_active_split(split_id);
        }

        // Record the group as the active-tab and focused inner leaf for
        // this split. The inner leaf is NOT in the main split tree — it
        // only exists inside the stashed Grouped subtree — so focus is
        // routed via the shown group's focused panel rather than
        // `focus_split`.
        if let Some(vs) = self
            .active_window_mut()
            .split_view_states_mut()
            .get_mut(&split_id)
        {
            vs.show_group(group_leaf, inner_leaf);
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
    /// Whether `leaf` is a group panel showing `buffer_id` as a buffer its
    /// group borrowed. Such a panel is no tab of the buffer: closing the
    /// buffer's last tab closes the buffer, and with it the group.
    pub(crate) fn is_borrowing_panel(&self, leaf: LeafId, buffer_id: BufferId) -> bool {
        self.buffer_groups.values().any(|g| {
            g.borrowed_buffers.contains(&buffer_id) && g.panel_splits.values().any(|l| *l == leaf)
        })
    }

    /// A pane showing group `group_leaf` is about to show its buffer tab
    /// again. When the group's focused panel shows a buffer the group
    /// borrowed, and the pane has a tab of that buffer, the pane goes back to
    /// it, where the user was in it: the panel's cursor and scroll. Otherwise
    /// closing a source/preview pair could land the pane on whatever tab was
    /// behind the group, or put the user back where they were before opening
    /// it.
    pub(crate) fn return_to_borrowed_buffer(
        &mut self,
        group_leaf: LeafId,
        borrowed: &std::collections::HashSet<BufferId>,
    ) {
        let Some(vs_map) = self.buffers.split_view_states() else {
            return;
        };
        let returns: Vec<_> = vs_map
            .iter()
            .filter_map(|(host, vs)| match vs.shown_group() {
                Some((group, panel)) if group == group_leaf => Some((*host, panel)),
                _ => None,
            })
            .filter_map(|(host, panel)| {
                let panel_vs = vs_map.get(&panel)?;
                let buffer = panel_vs.shown_buffer()?;
                let bs = panel_vs.buffer_state(buffer)?;
                let host_has_tab = vs_map.get(&host)?.has_buffer(buffer);
                (borrowed.contains(&buffer) && host_has_tab)
                    .then(|| (host, buffer, bs.cursors.clone(), bs.viewport.anchor))
            })
            .collect();
        for (host, buffer, cursors, anchor) in returns {
            self.set_pane_buffer(host, buffer);
            if let Some(bs) = self
                .buffers
                .split_view_states_mut()
                .and_then(|vs_map| vs_map.get_mut(&host))
                .and_then(|vs| vs.buffer_state_mut(buffer))
            {
                bs.cursors = cursors;
                bs.viewport.anchor = anchor;
            }
        }
    }

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
                // Raw-storage clamp only: the sibling is kept usable by the
                // layout-time min-pane-size guard (see `clamp_first_to_min`),
                // matching `SplitManager::set_ratio` — not a fixed percentage floor.
                *ratio = new_ratio.clamp(0.0, 1.0);
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
