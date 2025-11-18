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
use crate::cursor::Cursors;
use crate::event::{BufferId, SplitDirection, SplitId};
use crate::viewport::Viewport;
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
        first: Box<SplitNode>,
        /// Second child (bottom or right)
        second: Box<SplitNode>,
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
}

impl SplitViewState {
    /// Create a new split view state with default cursor at position 0
    pub fn new(width: u16, height: u16) -> Self {
        Self {
            cursors: Cursors::new(),
            viewport: Viewport::new(width, height),
            open_buffers: Vec::new(),
        }
    }

    /// Create a new split view state with an initial buffer open
    pub fn with_buffer(width: u16, height: u16, buffer_id: BufferId) -> Self {
        Self {
            cursors: Cursors::new(),
            viewport: Viewport::new(width, height),
            open_buffers: vec![buffer_id],
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
        SplitNode::Leaf {
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
            SplitNode::Leaf { split_id, .. } => *split_id,
            SplitNode::Split { split_id, .. } => *split_id,
        }
    }

    /// Get the buffer ID if this is a leaf node
    pub fn buffer_id(&self) -> Option<BufferId> {
        match self {
            SplitNode::Leaf { buffer_id, .. } => Some(*buffer_id),
            SplitNode::Split { .. } => None,
        }
    }

    /// Find a split by ID (returns mutable reference)
    pub fn find_mut(&mut self, target_id: SplitId) -> Option<&mut SplitNode> {
        if self.id() == target_id {
            return Some(self);
        }

        match self {
            SplitNode::Leaf { .. } => None,
            SplitNode::Split { first, second, .. } => first
                .find_mut(target_id)
                .or_else(|| second.find_mut(target_id)),
        }
    }

    /// Find a split by ID (returns immutable reference)
    pub fn find(&self, target_id: SplitId) -> Option<&SplitNode> {
        if self.id() == target_id {
            return Some(self);
        }

        match self {
            SplitNode::Leaf { .. } => None,
            SplitNode::Split { first, second, .. } => {
                first.find(target_id).or_else(|| second.find(target_id))
            }
        }
    }

    /// Get all leaf nodes (buffer views) with their rectangles
    pub fn get_leaves_with_rects(&self, rect: Rect) -> Vec<(SplitId, BufferId, Rect)> {
        match self {
            SplitNode::Leaf {
                buffer_id,
                split_id,
            } => {
                vec![(*split_id, *buffer_id, rect)]
            }
            SplitNode::Split {
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
    pub fn get_separators_with_ids(&self, rect: Rect) -> Vec<(SplitId, SplitDirection, u16, u16, u16)> {
        match self {
            SplitNode::Leaf { .. } => vec![],
            SplitNode::Split {
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
            SplitNode::Leaf { .. } => ids,
            SplitNode::Split { first, second, .. } => {
                ids.extend(first.all_split_ids());
                ids.extend(second.all_split_ids());
                ids
            }
        }
    }

    /// Collect only leaf split IDs (visible buffer splits, not container nodes)
    pub fn leaf_split_ids(&self) -> Vec<SplitId> {
        match self {
            SplitNode::Leaf { split_id, .. } => vec![*split_id],
            SplitNode::Split { first, second, .. } => {
                let mut ids = first.leaf_split_ids();
                ids.extend(second.leaf_split_ids());
                ids
            }
        }
    }

    /// Count the number of leaf nodes (visible buffers)
    pub fn count_leaves(&self) -> usize {
        match self {
            SplitNode::Leaf { .. } => 1,
            SplitNode::Split { first, second, .. } => first.count_leaves() + second.count_leaves(),
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
}

impl SplitManager {
    /// Create a new split manager with a single buffer
    pub fn new(buffer_id: BufferId) -> Self {
        let split_id = SplitId(0);
        Self {
            root: SplitNode::leaf(buffer_id, split_id),
            active_split: split_id,
            next_split_id: 1,
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

    /// Update the buffer ID of the active split
    /// Returns true if successful (active split is a leaf), false otherwise
    pub fn set_active_buffer_id(&mut self, new_buffer_id: BufferId) -> bool {
        if let Some(node) = self.root.find_mut(self.active_split) {
            if let SplitNode::Leaf { buffer_id, .. } = node {
                *buffer_id = new_buffer_id;
                return true;
            }
        }
        false
    }

    /// Update the buffer ID of a specific split
    /// Returns Ok(()) if successful, Err with message if split not found or not a leaf
    pub fn set_split_buffer(&mut self, split_id: SplitId, new_buffer_id: BufferId) -> Result<(), String> {
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
        self.root.get_leaves_with_rects(viewport_rect)
    }

    /// Get all split separator positions for rendering borders
    /// Returns (direction, x, y, length) tuples
    pub fn get_separators(&self, viewport_rect: Rect) -> Vec<(SplitDirection, u16, u16, u16)> {
        self.root.get_separators(viewport_rect)
    }

    /// Get all split separator positions with their split IDs (for mouse hit testing)
    /// Returns (split_id, direction, x, y, length) tuples
    pub fn get_separators_with_ids(&self, viewport_rect: Rect) -> Vec<(SplitId, SplitDirection, u16, u16, u16)> {
        self.root.get_separators_with_ids(viewport_rect)
    }

    /// Get the current ratio of a split container
    pub fn get_ratio(&self, split_id: SplitId) -> Option<f32> {
        if let Some(node) = self.root.find(split_id) {
            if let SplitNode::Split { ratio, .. } = node {
                Some(*ratio)
            } else {
                None
            }
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
            let prev_pos = if pos == 0 { leaf_ids.len() - 1 } else { pos - 1 };
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
