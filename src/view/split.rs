/// Split view system for displaying multiple buffers simultaneously
///
/// Design Philosophy (following Emacs model):
/// - A split is a tree structure: either a leaf (single buffer) or a node (horizontal/vertical split)
/// - Each split has a fixed size (in percentage or absolute lines/columns)
/// - Splits can be nested arbitrarily deep
/// - Only one split is "active" at a time (receives input)
/// - Splits can display the same buffer multiple times (useful for viewing different parts)
///
/// Example split layouts:
/// ```text
/// ┌────────────────────┐      ┌──────────┬─────────┐
/// │                    │      │          │         │
/// │   Single buffer    │      │  Buffer  │ Buffer  │
/// │                    │      │    A     │    B    │
/// └────────────────────┘      └──────────┴─────────┘
///   (no split)                  (vertical split)
///
/// ┌────────────────────┐      ┌──────────┬─────────┐
/// │     Buffer A       │      │          │ Buffer C│
/// ├────────────────────┤      │  Buffer  ├─────────┤
/// │     Buffer B       │      │    A     │ Buffer D│
/// └────────────────────┘      └──────────┴─────────┘
///  (horizontal split)          (mixed splits)
/// ```
use crate::model::cursor::Cursors;
use crate::model::event::{BufferId, SplitDirection, SplitId};
use crate::view::ui::view_pipeline::Layout;
use crate::view::viewport::Viewport;
use crate::{services::plugins::api::ViewTransformPayload, state::ViewMode};
use ratatui::layout::Rect;
use serde::{Deserialize, Serialize};

/// A node in the split tree
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SplitNode {
    /// Leaf node: displays a single buffer
    Leaf {
        /// Which buffer to display
        buffer_id: BufferId,
        /// Unique ID for this split pane
        split_id: SplitId,
    },
    /// Internal node: contains two child splits
    Split {
        /// Direction of the split
        direction: SplitDirection,
        /// First child (top or left)
        first: Box<Self>,
        /// Second child (bottom or right)
        second: Box<Self>,
        /// Size ratio (0.0 to 1.0) - how much space the first child gets
        /// 0.5 = equal split, 0.3 = first gets 30%, etc.
        ratio: f32,
        /// Unique ID for this split container
        split_id: SplitId,
    },
}

/// Per-split view state (independent of buffer content)
///
/// Following the Emacs model where each window (split) has its own:
/// - Point (cursor position) - independent per split
/// - Window-start (scroll position) - independent per split
/// - Tabs (open buffers) - independent per split
///
/// This allows multiple splits to display the same buffer at different positions
/// with independent cursor and scroll positions, and each split has its own set of tabs.
#[derive(Debug, Clone)]
pub struct SplitViewState {
    /// Independent cursor set for this split (supports multi-cursor)
    pub cursors: Cursors,

    /// Independent scroll position for this split
    pub viewport: Viewport,

    /// List of buffer IDs open in this split's tab bar (in order)
    /// The currently displayed buffer is tracked in the SplitNode::Leaf
    pub open_buffers: Vec<BufferId>,

    /// Horizontal scroll offset for the tabs in this split
    pub tab_scroll_offset: usize,

    /// View mode (Source/Compose) per split
    pub view_mode: ViewMode,

    /// Optional compose width for centering/wrapping in this split
    pub compose_width: Option<u16>,

    /// Column guides for this split (e.g., tables)
    pub compose_column_guides: Option<Vec<u16>>,

    /// Previously configured line number visibility (restored when leaving Compose)
    pub compose_prev_line_numbers: Option<bool>,

    /// Optional view transform payload for this split/viewport
    pub view_transform: Option<ViewTransformPayload>,

    /// Computed layout for this view (from view_transform or base tokens)
    /// This is View state - each split has its own Layout
    pub layout: Option<Layout>,

    /// Whether the layout needs to be rebuilt (buffer changed, transform changed, etc.)
    pub layout_dirty: bool,

    /// Previously active buffer in this split (for "Switch to Previous Tab" command)
    pub previous_buffer: Option<BufferId>,

    /// Sync group ID for synchronized scrolling
    /// Splits with the same sync_group will scroll together
    pub sync_group: Option<u32>,

    /// When set, this split renders a composite view (e.g., side-by-side diff).
    /// The split's buffer_id is the focused source buffer, but rendering uses
    /// the composite layout. This makes the source buffer the "active buffer"
    /// so normal keybindings work directly.
    pub composite_view: Option<BufferId>,
}

impl SplitViewState {
    /// Create a new split view state with default cursor at position 0
    pub fn new(width: u16, height: u16) -> Self {
        Self {
            cursors: Cursors::new(),
            viewport: Viewport::new(width, height),
            open_buffers: Vec::new(),
            tab_scroll_offset: 0,
            view_mode: ViewMode::Source,
            compose_width: None,
            compose_column_guides: None,
            compose_prev_line_numbers: None,
            view_transform: None,
            layout: None,
            layout_dirty: true, // Start dirty so first operation builds layout
            previous_buffer: None,
            sync_group: None,
            composite_view: None,
        }
    }

    /// Create a new split view state with an initial buffer open
    pub fn with_buffer(width: u16, height: u16, buffer_id: BufferId) -> Self {
        Self {
            cursors: Cursors::new(),
            viewport: Viewport::new(width, height),
            open_buffers: vec![buffer_id],
            tab_scroll_offset: 0,
            view_mode: ViewMode::Source,
            compose_width: None,
            compose_column_guides: None,
            compose_prev_line_numbers: None,
            view_transform: None,
            layout: None,
            layout_dirty: true, // Start dirty so first operation builds layout
            previous_buffer: None,
            sync_group: None,
            composite_view: None,
        }
    }

    /// Mark layout as needing rebuild (call after buffer changes)
    pub fn invalidate_layout(&mut self) {
        self.layout_dirty = true;
    }

    /// Ensure layout is valid, rebuilding if needed.
    /// Returns the Layout - never returns None. Following VSCode's ViewModel pattern.
    ///
    /// # Arguments
    /// * `tokens` - ViewTokenWire array (from view_transform or built from buffer)
    /// * `source_range` - The byte range this layout covers
    /// * `tab_size` - Tab width for rendering
    pub fn ensure_layout(
        &mut self,
        tokens: &[crate::services::plugins::api::ViewTokenWire],
        source_range: std::ops::Range<usize>,
        tab_size: usize,
    ) -> &Layout {
        if self.layout.is_none() || self.layout_dirty {
            self.layout = Some(Layout::from_tokens(tokens, source_range, tab_size));
            self.layout_dirty = false;
        }
        self.layout.as_ref().unwrap()
    }

    /// Get the current layout if it exists and is valid
    pub fn get_layout(&self) -> Option<&Layout> {
        if self.layout_dirty {
            None
        } else {
            self.layout.as_ref()
        }
    }

    /// Add a buffer to this split's tabs (if not already present)
    pub fn add_buffer(&mut self, buffer_id: BufferId) {
        if !self.open_buffers.contains(&buffer_id) {
            self.open_buffers.push(buffer_id);
        }
    }

    /// Remove a buffer from this split's tabs
    pub fn remove_buffer(&mut self, buffer_id: BufferId) {
        self.open_buffers.retain(|&id| id != buffer_id);
    }

    /// Check if a buffer is open in this split
    pub fn has_buffer(&self, buffer_id: BufferId) -> bool {
        self.open_buffers.contains(&buffer_id)
    }
}

impl SplitNode {
    /// Create a new leaf node
    pub fn leaf(buffer_id: BufferId, split_id: SplitId) -> Self {
        Self::Leaf {
            buffer_id,
            split_id,
        }
    }

    /// Create a new split node with two children
    pub fn split(
        direction: SplitDirection,
        first: SplitNode,
        second: SplitNode,
        ratio: f32,
        split_id: SplitId,
    ) -> Self {
        SplitNode::Split {
            direction,
            first: Box::new(first),
            second: Box::new(second),
            ratio: ratio.clamp(0.1, 0.9), // Prevent extreme ratios
            split_id,
        }
    }

    /// Get the split ID for this node
    pub fn id(&self) -> SplitId {
        match self {
            Self::Leaf { split_id, .. } | Self::Split { split_id, .. } => *split_id,
        }
    }

    /// Get the buffer ID if this is a leaf node
    pub fn buffer_id(&self) -> Option<BufferId> {
        match self {
            Self::Leaf { buffer_id, .. } => Some(*buffer_id),
            Self::Split { .. } => None,
        }
    }

    /// Find a split by ID (returns mutable reference)
    pub fn find_mut(&mut self, target_id: SplitId) -> Option<&mut Self> {
        if self.id() == target_id {
            return Some(self);
        }

        match self {
            Self::Leaf { .. } => None,
            Self::Split { first, second, .. } => first
                .find_mut(target_id)
                .or_else(|| second.find_mut(target_id)),
        }
    }

    /// Find a split by ID (returns immutable reference)
    pub fn find(&self, target_id: SplitId) -> Option<&Self> {
        if self.id() == target_id {
            return Some(self);
        }

        match self {
            Self::Leaf { .. } => None,
            Self::Split { first, second, .. } => {
                first.find(target_id).or_else(|| second.find(target_id))
            }
        }
    }

    /// Get all leaf nodes (buffer views) with their rectangles
    pub fn get_leaves_with_rects(&self, rect: Rect) -> Vec<(SplitId, BufferId, Rect)> {
        match self {
            Self::Leaf {
                buffer_id,
                split_id,
            } => {
                vec![(*split_id, *buffer_id, rect)]
            }
            Self::Split {
                direction,
                first,
                second,
                ratio,
                ..
            } => {
                let (first_rect, second_rect) = split_rect(rect, *direction, *ratio);
                let mut leaves = first.get_leaves_with_rects(first_rect);
                leaves.extend(second.get_leaves_with_rects(second_rect));
                leaves
            }
        }
    }

    /// Get all split separator lines (for rendering borders)
    /// Returns (direction, x, y, length) tuples
    pub fn get_separators(&self, rect: Rect) -> Vec<(SplitDirection, u16, u16, u16)> {
        self.get_separators_with_ids(rect)
            .into_iter()
            .map(|(_, dir, x, y, len)| (dir, x, y, len))
            .collect()
    }

    /// Get all split separator lines with their split IDs (for mouse hit testing)
    /// Returns (split_id, direction, x, y, length) tuples
    pub fn get_separators_with_ids(
        &self,
        rect: Rect,
    ) -> Vec<(SplitId, SplitDirection, u16, u16, u16)> {
        match self {
            Self::Leaf { .. } => vec![],
            Self::Split {
                direction,
                first,
                second,
                ratio,
                split_id,
            } => {
                let (first_rect, second_rect) = split_rect(rect, *direction, *ratio);
                let mut separators = Vec::new();

                // Add separator for this split (in the 1-char gap between first and second)
                match direction {
                    SplitDirection::Horizontal => {
                        // Horizontal split: separator line is between first and second
                        // y position is at the end of first rect (the gap line)
                        separators.push((
                            *split_id,
                            SplitDirection::Horizontal,
                            rect.x,
                            first_rect.y + first_rect.height,
                            rect.width,
                        ));
                    }
                    SplitDirection::Vertical => {
                        // Vertical split: separator line is between first and second
                        // x position is at the end of first rect (the gap column)
                        separators.push((
                            *split_id,
                            SplitDirection::Vertical,
                            first_rect.x + first_rect.width,
                            rect.y,
                            rect.height,
                        ));
                    }
                }

                // Recursively get separators from children
                separators.extend(first.get_separators_with_ids(first_rect));
                separators.extend(second.get_separators_with_ids(second_rect));
                separators
            }
        }
    }

    /// Collect all split IDs in the tree
    pub fn all_split_ids(&self) -> Vec<SplitId> {
        let mut ids = vec![self.id()];
        match self {
            Self::Leaf { .. } => ids,
            Self::Split { first, second, .. } => {
                ids.extend(first.all_split_ids());
                ids.extend(second.all_split_ids());
                ids
            }
        }
    }

    /// Collect only leaf split IDs (visible buffer splits, not container nodes)
    pub fn leaf_split_ids(&self) -> Vec<SplitId> {
        match self {
            Self::Leaf { split_id, .. } => vec![*split_id],
            Self::Split { first, second, .. } => {
                let mut ids = first.leaf_split_ids();
                ids.extend(second.leaf_split_ids());
                ids
            }
        }
    }

    /// Count the number of leaf nodes (visible buffers)
    pub fn count_leaves(&self) -> usize {
        match self {
            Self::Leaf { .. } => 1,
            Self::Split { first, second, .. } => first.count_leaves() + second.count_leaves(),
        }
    }
}

/// Split a rectangle into two parts based on direction and ratio
/// Leaves 1 character space for the separator line between splits
fn split_rect(rect: Rect, direction: SplitDirection, ratio: f32) -> (Rect, Rect) {
    match direction {
        SplitDirection::Horizontal => {
            // Split into top and bottom, with 1 line for separator
            let total_height = rect.height.saturating_sub(1); // Reserve 1 line for separator
            let first_height = (total_height as f32 * ratio).round() as u16;
            let second_height = total_height.saturating_sub(first_height);

            let first = Rect {
                x: rect.x,
                y: rect.y,
                width: rect.width,
                height: first_height,
            };

            let second = Rect {
                x: rect.x,
                y: rect.y + first_height + 1, // +1 for separator
                width: rect.width,
                height: second_height,
            };

            (first, second)
        }
        SplitDirection::Vertical => {
            // Split into left and right, with 1 column for separator
            let total_width = rect.width.saturating_sub(1); // Reserve 1 column for separator
            let first_width = (total_width as f32 * ratio).round() as u16;
            let second_width = total_width.saturating_sub(first_width);

            let first = Rect {
                x: rect.x,
                y: rect.y,
                width: first_width,
                height: rect.height,
            };

            let second = Rect {
                x: rect.x + first_width + 1, // +1 for separator
                y: rect.y,
                width: second_width,
                height: rect.height,
            };

            (first, second)
        }
    }
}

/// Manager for the split view system
#[derive(Debug)]
pub struct SplitManager {
    /// Root of the split tree
    root: SplitNode,

    /// Currently active split (receives input)
    active_split: SplitId,

    /// Next split ID to assign
    next_split_id: usize,

    /// Currently maximized split (if any). When set, only this split is visible.
    maximized_split: Option<SplitId>,
}

impl SplitManager {
    /// Create a new split manager with a single buffer
    pub fn new(buffer_id: BufferId) -> Self {
        let split_id = SplitId(0);
        Self {
            root: SplitNode::leaf(buffer_id, split_id),
            active_split: split_id,
            next_split_id: 1,
            maximized_split: None,
        }
    }

    /// Get the root split node
    pub fn root(&self) -> &SplitNode {
        &self.root
    }

    /// Get the currently active split ID
    pub fn active_split(&self) -> SplitId {
        self.active_split
    }

    /// Set the active split
    pub fn set_active_split(&mut self, split_id: SplitId) -> bool {
        // Verify the split exists
        if self.root.find(split_id).is_some() {
            self.active_split = split_id;
            true
        } else {
            false
        }
    }

    /// Get the buffer ID of the active split (if it's a leaf)
    pub fn active_buffer_id(&self) -> Option<BufferId> {
        self.root
            .find(self.active_split)
            .and_then(|node| node.buffer_id())
    }

    /// Get the buffer ID for a specific split (if it's a leaf)
    pub fn get_buffer_id(&self, split_id: SplitId) -> Option<BufferId> {
        self.root.find(split_id).and_then(|node| node.buffer_id())
    }

    /// Update the buffer ID of the active split
    /// Returns true if successful (active split is a leaf), false otherwise
    pub fn set_active_buffer_id(&mut self, new_buffer_id: BufferId) -> bool {
        if let Some(SplitNode::Leaf { buffer_id, .. }) = self.root.find_mut(self.active_split) {
            *buffer_id = new_buffer_id;
            return true;
        }
        false
    }

    /// Update the buffer ID of a specific split
    /// Returns Ok(()) if successful, Err with message if split not found or not a leaf
    pub fn set_split_buffer(
        &mut self,
        split_id: SplitId,
        new_buffer_id: BufferId,
    ) -> Result<(), String> {
        if let Some(node) = self.root.find_mut(split_id) {
            if let SplitNode::Leaf { buffer_id, .. } = node {
                *buffer_id = new_buffer_id;
                return Ok(());
            }
            return Err(format!("Split {:?} is not a leaf", split_id));
        }
        Err(format!("Split {:?} not found", split_id))
    }

    /// Allocate a new split ID
    fn allocate_split_id(&mut self) -> SplitId {
        let id = SplitId(self.next_split_id);
        self.next_split_id += 1;
        id
    }

    /// Split the currently active pane
    pub fn split_active(
        &mut self,
        direction: SplitDirection,
        new_buffer_id: BufferId,
        ratio: f32,
    ) -> Result<SplitId, String> {
        let active_id = self.active_split;

        // Find the parent of the active split
        let result = self.replace_split_with_split(active_id, direction, new_buffer_id, ratio);

        if let Ok(new_split_id) = result {
            // Set the new split as active
            self.active_split = new_split_id;
            Ok(new_split_id)
        } else {
            result
        }
    }

    /// Replace a split with a new split container
    fn replace_split_with_split(
        &mut self,
        target_id: SplitId,
        direction: SplitDirection,
        new_buffer_id: BufferId,
        ratio: f32,
    ) -> Result<SplitId, String> {
        // Pre-allocate all IDs before any borrowing
        let temp_id = self.allocate_split_id();
        let new_split_id = self.allocate_split_id();
        let new_leaf_id = self.allocate_split_id();

        // Special case: if target is root, replace root
        if self.root.id() == target_id {
            let old_root =
                std::mem::replace(&mut self.root, SplitNode::leaf(new_buffer_id, temp_id));

            self.root = SplitNode::split(
                direction,
                old_root,
                SplitNode::leaf(new_buffer_id, new_leaf_id),
                ratio,
                new_split_id,
            );

            return Ok(new_leaf_id);
        }

        // Find and replace the target node
        if let Some(node) = self.root.find_mut(target_id) {
            let old_node = std::mem::replace(node, SplitNode::leaf(new_buffer_id, temp_id));

            *node = SplitNode::split(
                direction,
                old_node,
                SplitNode::leaf(new_buffer_id, new_leaf_id),
                ratio,
                new_split_id,
            );

            Ok(new_leaf_id)
        } else {
            Err(format!("Split {:?} not found", target_id))
        }
    }

    /// Close a split pane (if not the last one)
    pub fn close_split(&mut self, split_id: SplitId) -> Result<(), String> {
        // Can't close if it's the only split
        if self.root.count_leaves() <= 1 {
            return Err("Cannot close the last split".to_string());
        }

        // Can't close if it's the root and root is a leaf
        if self.root.id() == split_id && self.root.buffer_id().is_some() {
            return Err("Cannot close the only split".to_string());
        }

        // If the split being closed is maximized, unmaximize first
        if self.maximized_split == Some(split_id) {
            self.maximized_split = None;
        }

        // Find the parent of the split to close
        // This requires a parent-tracking traversal
        let result = self.remove_split_node(split_id);

        // If we closed the active split, update active_split to another split
        if result.is_ok() && self.active_split == split_id {
            let leaf_ids = self.root.leaf_split_ids();
            if let Some(&first_leaf) = leaf_ids.first() {
                self.active_split = first_leaf;
            }
        }

        result
    }

    /// Remove a split node from the tree
    fn remove_split_node(&mut self, target_id: SplitId) -> Result<(), String> {
        // Special case: removing root
        if self.root.id() == target_id {
            if let SplitNode::Split { first, .. } = &self.root {
                // Replace root with the other child
                // Choose first child arbitrarily
                self.root = (**first).clone();
                return Ok(());
            }
        }

        // Recursively find and remove
        Self::remove_child_static(&mut self.root, target_id)
    }

    /// Helper to remove a child from a split node (static to avoid borrow issues)
    fn remove_child_static(node: &mut SplitNode, target_id: SplitId) -> Result<(), String> {
        match node {
            SplitNode::Leaf { .. } => Err("Target not found".to_string()),
            SplitNode::Split { first, second, .. } => {
                // Check if either child is the target
                if first.id() == target_id {
                    // Replace this node with the second child
                    *node = (**second).clone();
                    Ok(())
                } else if second.id() == target_id {
                    // Replace this node with the first child
                    *node = (**first).clone();
                    Ok(())
                } else {
                    // Recurse into children
                    Self::remove_child_static(first, target_id)
                        .or_else(|_| Self::remove_child_static(second, target_id))
                }
            }
        }
    }

    /// Adjust the split ratio of a container
    pub fn adjust_ratio(&mut self, split_id: SplitId, delta: f32) -> Result<(), String> {
        if let Some(node) = self.root.find_mut(split_id) {
            if let SplitNode::Split { ratio, .. } = node {
                *ratio = (*ratio + delta).clamp(0.1, 0.9);
                Ok(())
            } else {
                Err("Target is not a split container".to_string())
            }
        } else {
            Err("Split not found".to_string())
        }
    }

    /// Get all visible buffer views with their rectangles
    pub fn get_visible_buffers(&self, viewport_rect: Rect) -> Vec<(SplitId, BufferId, Rect)> {
        // If a split is maximized, only show that split taking up the full viewport
        if let Some(maximized_id) = self.maximized_split {
            if let Some(node) = self.root.find(maximized_id) {
                if let Some(buffer_id) = node.buffer_id() {
                    return vec![(maximized_id, buffer_id, viewport_rect)];
                }
            }
            // Maximized split no longer exists, clear it and fall through
        }
        self.root.get_leaves_with_rects(viewport_rect)
    }

    /// Get all split separator positions for rendering borders
    /// Returns (direction, x, y, length) tuples
    pub fn get_separators(&self, viewport_rect: Rect) -> Vec<(SplitDirection, u16, u16, u16)> {
        // No separators when a split is maximized
        if self.maximized_split.is_some() {
            return vec![];
        }
        self.root.get_separators(viewport_rect)
    }

    /// Get all split separator positions with their split IDs (for mouse hit testing)
    /// Returns (split_id, direction, x, y, length) tuples
    pub fn get_separators_with_ids(
        &self,
        viewport_rect: Rect,
    ) -> Vec<(SplitId, SplitDirection, u16, u16, u16)> {
        // No separators when a split is maximized
        if self.maximized_split.is_some() {
            return vec![];
        }
        self.root.get_separators_with_ids(viewport_rect)
    }

    /// Get the current ratio of a split container
    pub fn get_ratio(&self, split_id: SplitId) -> Option<f32> {
        if let Some(SplitNode::Split { ratio, .. }) = self.root.find(split_id) {
            Some(*ratio)
        } else {
            None
        }
    }

    /// Set the exact ratio of a split container
    pub fn set_ratio(&mut self, split_id: SplitId, new_ratio: f32) -> Result<(), String> {
        if let Some(node) = self.root.find_mut(split_id) {
            if let SplitNode::Split { ratio, .. } = node {
                *ratio = new_ratio.clamp(0.1, 0.9);
                Ok(())
            } else {
                Err("Target is not a split container".to_string())
            }
        } else {
            Err("Split not found".to_string())
        }
    }

    /// Distribute all visible splits evenly
    /// This sets the ratios of all container splits so that leaf splits get equal space
    pub fn distribute_splits_evenly(&mut self) {
        Self::distribute_node_evenly(&mut self.root);
    }

    /// Recursively distribute a node's splits evenly
    /// Returns the number of leaves in this subtree
    fn distribute_node_evenly(node: &mut SplitNode) -> usize {
        match node {
            SplitNode::Leaf { .. } => 1,
            SplitNode::Split {
                first,
                second,
                ratio,
                ..
            } => {
                let first_leaves = Self::distribute_node_evenly(first);
                let second_leaves = Self::distribute_node_evenly(second);
                let total_leaves = first_leaves + second_leaves;

                // Set ratio so each leaf gets equal space
                // ratio = proportion for first pane
                *ratio = (first_leaves as f32 / total_leaves as f32).clamp(0.1, 0.9);

                total_leaves
            }
        }
    }

    /// Navigate to the next split (circular)
    pub fn next_split(&mut self) {
        let leaf_ids = self.root.leaf_split_ids();
        if let Some(pos) = leaf_ids.iter().position(|id| *id == self.active_split) {
            let next_pos = (pos + 1) % leaf_ids.len();
            self.active_split = leaf_ids[next_pos];
        }
    }

    /// Navigate to the previous split (circular)
    pub fn prev_split(&mut self) {
        let leaf_ids = self.root.leaf_split_ids();
        if let Some(pos) = leaf_ids.iter().position(|id| *id == self.active_split) {
            let prev_pos = if pos == 0 { leaf_ids.len() } else { pos } - 1;
            self.active_split = leaf_ids[prev_pos];
        }
    }

    /// Get all split IDs that display a specific buffer
    pub fn splits_for_buffer(&self, target_buffer_id: BufferId) -> Vec<SplitId> {
        self.root
            .get_leaves_with_rects(Rect {
                x: 0,
                y: 0,
                width: 1,
                height: 1,
            })
            .into_iter()
            .filter(|(_, buffer_id, _)| *buffer_id == target_buffer_id)
            .map(|(split_id, _, _)| split_id)
            .collect()
    }

    /// Get the buffer ID for a specific split
    pub fn buffer_for_split(&self, target_split_id: SplitId) -> Option<BufferId> {
        self.root
            .get_leaves_with_rects(Rect {
                x: 0,
                y: 0,
                width: 1,
                height: 1,
            })
            .into_iter()
            .find(|(split_id, _, _)| *split_id == target_split_id)
            .map(|(_, buffer_id, _)| buffer_id)
    }

    /// Maximize the active split (hide all other splits temporarily)
    /// Returns Ok(()) if successful, Err if there's only one split
    pub fn maximize_split(&mut self) -> Result<(), String> {
        // Can't maximize if there's only one split
        if self.root.count_leaves() <= 1 {
            return Err("Cannot maximize: only one split exists".to_string());
        }

        // Can't maximize if already maximized
        if self.maximized_split.is_some() {
            return Err("A split is already maximized".to_string());
        }

        // Maximize the active split
        self.maximized_split = Some(self.active_split);
        Ok(())
    }

    /// Unmaximize the currently maximized split (restore all splits)
    /// Returns Ok(()) if successful, Err if no split is maximized
    pub fn unmaximize_split(&mut self) -> Result<(), String> {
        if self.maximized_split.is_none() {
            return Err("No split is maximized".to_string());
        }

        self.maximized_split = None;
        Ok(())
    }

    /// Check if a split is currently maximized
    pub fn is_maximized(&self) -> bool {
        self.maximized_split.is_some()
    }

    /// Get the currently maximized split ID (if any)
    pub fn maximized_split(&self) -> Option<SplitId> {
        self.maximized_split
    }

    /// Toggle maximize state for the active split
    /// If maximized, unmaximize. If not maximized, maximize.
    /// Returns true if maximized, false if ununmaximized.
    pub fn toggle_maximize(&mut self) -> Result<bool, String> {
        if self.is_maximized() {
            self.unmaximize_split()?;
            Ok(false)
        } else {
            self.maximize_split()?;
            Ok(true)
        }
    }

    /// Get all leaf split IDs that belong to a specific sync group
    pub fn get_splits_in_group(
        &self,
        group_id: u32,
        view_states: &std::collections::HashMap<SplitId, SplitViewState>,
    ) -> Vec<SplitId> {
        self.root
            .leaf_split_ids()
            .into_iter()
            .filter(|id| {
                view_states
                    .get(id)
                    .and_then(|vs| vs.sync_group)
                    .is_some_and(|g| g == group_id)
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_split_manager() {
        let buffer_id = BufferId(0);
        let manager = SplitManager::new(buffer_id);

        assert_eq!(manager.active_buffer_id(), Some(buffer_id));
        assert_eq!(manager.root().count_leaves(), 1);
    }

    #[test]
    fn test_horizontal_split() {
        let buffer_a = BufferId(0);
        let buffer_b = BufferId(1);

        let mut manager = SplitManager::new(buffer_a);
        let result = manager.split_active(SplitDirection::Horizontal, buffer_b, 0.5);

        assert!(result.is_ok());
        assert_eq!(manager.root().count_leaves(), 2);
    }

    #[test]
    fn test_vertical_split() {
        let buffer_a = BufferId(0);
        let buffer_b = BufferId(1);

        let mut manager = SplitManager::new(buffer_a);
        let result = manager.split_active(SplitDirection::Vertical, buffer_b, 0.5);

        assert!(result.is_ok());
        assert_eq!(manager.root().count_leaves(), 2);
    }

    #[test]
    fn test_nested_splits() {
        let buffer_a = BufferId(0);
        let buffer_b = BufferId(1);
        let buffer_c = BufferId(2);

        let mut manager = SplitManager::new(buffer_a);

        // Split horizontally
        manager
            .split_active(SplitDirection::Horizontal, buffer_b, 0.5)
            .unwrap();

        // Split the second pane vertically
        manager
            .split_active(SplitDirection::Vertical, buffer_c, 0.5)
            .unwrap();

        assert_eq!(manager.root().count_leaves(), 3);
    }

    #[test]
    fn test_close_split() {
        let buffer_a = BufferId(0);
        let buffer_b = BufferId(1);

        let mut manager = SplitManager::new(buffer_a);
        let new_split = manager
            .split_active(SplitDirection::Horizontal, buffer_b, 0.5)
            .unwrap();

        assert_eq!(manager.root().count_leaves(), 2);

        // Close the new split
        let result = manager.close_split(new_split);
        assert!(result.is_ok());
        assert_eq!(manager.root().count_leaves(), 1);
    }

    #[test]
    fn test_cannot_close_last_split() {
        let buffer_a = BufferId(0);
        let mut manager = SplitManager::new(buffer_a);

        let result = manager.close_split(manager.active_split());
        assert!(result.is_err());
    }

    #[test]
    fn test_split_rect_horizontal() {
        let rect = Rect {
            x: 0,
            y: 0,
            width: 100,
            height: 100,
        };

        let (first, second) = split_rect(rect, SplitDirection::Horizontal, 0.5);

        // With 1 line reserved for separator: (100-1)/2 = 49.5 rounds to 50 and 49
        assert_eq!(first.height, 50);
        assert_eq!(second.height, 49);
        assert_eq!(first.width, 100);
        assert_eq!(second.width, 100);
        assert_eq!(first.y, 0);
        assert_eq!(second.y, 51); // first.y + first.height + 1 (separator)
    }

    #[test]
    fn test_split_rect_vertical() {
        let rect = Rect {
            x: 0,
            y: 0,
            width: 100,
            height: 100,
        };

        let (first, second) = split_rect(rect, SplitDirection::Vertical, 0.5);

        // With 1 column reserved for separator: (100-1)/2 = 49.5 rounds to 50 and 49
        assert_eq!(first.width, 50);
        assert_eq!(second.width, 49);
        assert_eq!(first.height, 100);
        assert_eq!(second.height, 100);
        assert_eq!(first.x, 0);
        assert_eq!(second.x, 51); // first.x + first.width + 1 (separator)
    }
}
