/// Scroll synchronization for side-by-side diff views
///
/// This module implements marker-based sync anchors for synchronized scrolling
/// between two panes showing different versions of a file (e.g., old vs new in a diff).
///
/// Key design principles:
/// - Single source of truth: `scroll_line` is the authoritative position
/// - Sync anchors mark corresponding lines between buffers (e.g., hunk boundaries)
/// - Synchronization happens at render time, not via async commands
/// - No feedback loops because only one position is tracked
use crate::model::event::SplitId;
use serde::{Deserialize, Serialize};

/// A sync anchor linking corresponding line positions in two buffers
///
/// Anchors are placed at diff hunk boundaries where both buffers
/// have a known correspondence (e.g., start of context, end of hunk).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SyncAnchor {
    /// Line number in the left (primary) buffer
    pub left_line: usize,
    /// Line number in the right (secondary) buffer
    pub right_line: usize,
}

/// A unique identifier for a scroll sync group
pub type ScrollSyncGroupId = u32;

/// A group of two splits that scroll together with anchor-based synchronization
///
/// Unlike the simple sync_group which applies the same scroll delta to all splits,
/// this uses sync anchors to correctly handle buffers with different line counts.
#[derive(Debug, Clone)]
pub struct ScrollSyncGroup {
    /// Unique ID for this sync group
    pub id: ScrollSyncGroupId,
    /// The left (primary) split - scroll position is tracked in this split's line space
    pub left_split: SplitId,
    /// The right (secondary) split - position is derived from anchors
    pub right_split: SplitId,
    /// Single source of truth: scroll position in left buffer's line space
    /// Both splits derive their viewport position from this value
    pub scroll_line: usize,
    /// Sync anchors ordered by left_line
    /// These mark corresponding positions between the two buffers
    pub anchors: Vec<SyncAnchor>,
}

impl ScrollSyncGroup {
    /// Create a new scroll sync group
    pub fn new(id: ScrollSyncGroupId, left_split: SplitId, right_split: SplitId) -> Self {
        Self {
            id,
            left_split,
            right_split,
            scroll_line: 0,
            anchors: vec![SyncAnchor {
                left_line: 0,
                right_line: 0,
            }],
        }
    }

    /// Set the sync anchors (replacing any existing ones)
    /// Anchors should be sorted by left_line
    pub fn set_anchors(&mut self, anchors: Vec<SyncAnchor>) {
        self.anchors = anchors;
        // Ensure there's always at least the origin anchor
        if self.anchors.is_empty() {
            self.anchors.push(SyncAnchor {
                left_line: 0,
                right_line: 0,
            });
        }
    }

    /// Check if a split is part of this sync group
    pub fn contains_split(&self, split_id: SplitId) -> bool {
        self.left_split == split_id || self.right_split == split_id
    }

    /// Check if a split is the left (primary) split
    pub fn is_left_split(&self, split_id: SplitId) -> bool {
        self.left_split == split_id
    }

    /// Convert a line number from left buffer space to right buffer space
    pub fn left_to_right_line(&self, left_line: usize) -> usize {
        // Find the anchor just at or before left_line
        let anchor = self
            .anchors
            .iter()
            .rfind(|a| a.left_line <= left_line)
            .unwrap_or(&self.anchors[0]);

        // Calculate offset from anchor
        let offset = left_line.saturating_sub(anchor.left_line);

        // Apply offset to right side
        anchor.right_line.saturating_add(offset)
    }

    /// Convert a line number from right buffer space to left buffer space
    pub fn right_to_left_line(&self, right_line: usize) -> usize {
        // Find the anchor just at or before right_line in right buffer space
        let anchor = self
            .anchors
            .iter()
            .rfind(|a| a.right_line <= right_line)
            .unwrap_or(&self.anchors[0]);

        // Calculate offset from anchor in right space
        let offset = right_line.saturating_sub(anchor.right_line);

        // Apply offset to left side
        anchor.left_line.saturating_add(offset)
    }

    /// Update scroll position from a scroll delta on the given split
    /// Returns true if this group handled the scroll
    pub fn apply_scroll_delta(&mut self, split_id: SplitId, delta_lines: isize) -> bool {
        if !self.contains_split(split_id) {
            return false;
        }

        // Apply delta to scroll_line (which is always in left buffer space)
        let new_scroll = if delta_lines >= 0 {
            self.scroll_line.saturating_add(delta_lines as usize)
        } else {
            self.scroll_line.saturating_sub(delta_lines.unsigned_abs())
        };

        self.scroll_line = new_scroll;
        true
    }

    /// Set scroll position directly (used for SetViewport events)
    /// The line number should be in left buffer space
    pub fn set_scroll_line(&mut self, line: usize) {
        self.scroll_line = line;
    }

    /// Get the scroll line for the left split
    pub fn left_scroll_line(&self) -> usize {
        self.scroll_line
    }

    /// Get the scroll line for the right split (derived via anchors)
    pub fn right_scroll_line(&self) -> usize {
        self.left_to_right_line(self.scroll_line)
    }

    /// Get the scroll line for a specific split
    pub fn scroll_line_for_split(&self, split_id: SplitId) -> usize {
        if split_id == self.left_split {
            self.left_scroll_line()
        } else {
            self.right_scroll_line()
        }
    }
}

/// Manager for scroll sync groups
#[derive(Debug, Default)]
pub struct ScrollSyncManager {
    /// Active scroll sync groups
    groups: Vec<ScrollSyncGroup>,
    /// Next group ID to assign
    next_id: ScrollSyncGroupId,
}

impl ScrollSyncManager {
    /// Create a new scroll sync manager
    pub fn new() -> Self {
        Self {
            groups: Vec::new(),
            next_id: 1,
        }
    }

    /// Create a new scroll sync group and return its ID
    pub fn create_group(&mut self, left_split: SplitId, right_split: SplitId) -> ScrollSyncGroupId {
        let id = self.next_id;
        self.next_id += 1;

        let group = ScrollSyncGroup::new(id, left_split, right_split);
        self.groups.push(group);
        id
    }

    /// Create a scroll sync group with a plugin-provided ID
    /// Returns true if created successfully, false if ID already exists
    pub fn create_group_with_id(
        &mut self,
        id: ScrollSyncGroupId,
        left_split: SplitId,
        right_split: SplitId,
    ) -> bool {
        // Check if ID already exists
        if self.groups.iter().any(|g| g.id == id) {
            return false;
        }

        let group = ScrollSyncGroup::new(id, left_split, right_split);
        self.groups.push(group);
        true
    }

    /// Remove a scroll sync group by ID
    pub fn remove_group(&mut self, id: ScrollSyncGroupId) -> bool {
        if let Some(pos) = self.groups.iter().position(|g| g.id == id) {
            self.groups.remove(pos);
            true
        } else {
            false
        }
    }

    /// Remove all scroll sync groups containing a specific split
    pub fn remove_groups_for_split(&mut self, split_id: SplitId) {
        self.groups.retain(|g| !g.contains_split(split_id));
    }

    /// Get a mutable reference to a group by ID
    pub fn get_group_mut(&mut self, id: ScrollSyncGroupId) -> Option<&mut ScrollSyncGroup> {
        self.groups.iter_mut().find(|g| g.id == id)
    }

    /// Get a reference to a group by ID
    pub fn get_group(&self, id: ScrollSyncGroupId) -> Option<&ScrollSyncGroup> {
        self.groups.iter().find(|g| g.id == id)
    }

    /// Find the group containing a specific split
    pub fn find_group_for_split(&self, split_id: SplitId) -> Option<&ScrollSyncGroup> {
        self.groups.iter().find(|g| g.contains_split(split_id))
    }

    /// Find the group containing a specific split (mutable)
    pub fn find_group_for_split_mut(&mut self, split_id: SplitId) -> Option<&mut ScrollSyncGroup> {
        self.groups.iter_mut().find(|g| g.contains_split(split_id))
    }

    /// Check if a split is in any scroll sync group
    pub fn is_split_synced(&self, split_id: SplitId) -> bool {
        self.groups.iter().any(|g| g.contains_split(split_id))
    }

    /// Get all groups (for iteration during render)
    pub fn groups(&self) -> &[ScrollSyncGroup] {
        &self.groups
    }

    /// Apply scroll delta to the group containing the split
    /// Returns true if a group handled the scroll
    pub fn apply_scroll_delta(&mut self, split_id: SplitId, delta_lines: isize) -> bool {
        if let Some(group) = self.find_group_for_split_mut(split_id) {
            group.apply_scroll_delta(split_id, delta_lines);
            true
        } else {
            false
        }
    }

    /// Set anchors for a group
    pub fn set_anchors(&mut self, group_id: ScrollSyncGroupId, anchors: Vec<SyncAnchor>) {
        if let Some(group) = self.get_group_mut(group_id) {
            group.set_anchors(anchors);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_left_to_right_line_simple() {
        let mut group = ScrollSyncGroup::new(1, SplitId(1), SplitId(2));
        group.set_anchors(vec![
            SyncAnchor {
                left_line: 0,
                right_line: 0,
            },
            SyncAnchor {
                left_line: 10,
                right_line: 10,
            },
            SyncAnchor {
                left_line: 20,
                right_line: 25,
            }, // Right has 5 extra lines
        ]);

        // Before any anchors
        assert_eq!(group.left_to_right_line(0), 0);
        assert_eq!(group.left_to_right_line(5), 5);

        // After second anchor (1:1 mapping)
        assert_eq!(group.left_to_right_line(10), 10);
        assert_eq!(group.left_to_right_line(15), 15);

        // After third anchor (offset by 5)
        assert_eq!(group.left_to_right_line(20), 25);
        assert_eq!(group.left_to_right_line(25), 30);
    }

    #[test]
    fn test_right_to_left_line() {
        let mut group = ScrollSyncGroup::new(1, SplitId(1), SplitId(2));
        group.set_anchors(vec![
            SyncAnchor {
                left_line: 0,
                right_line: 0,
            },
            SyncAnchor {
                left_line: 10,
                right_line: 15,
            }, // Right has 5 extra lines
        ]);

        // Before anchor
        assert_eq!(group.right_to_left_line(0), 0);
        assert_eq!(group.right_to_left_line(5), 5);

        // After anchor
        assert_eq!(group.right_to_left_line(15), 10);
        assert_eq!(group.right_to_left_line(20), 15);
    }

    #[test]
    fn test_scroll_delta() {
        let mut group = ScrollSyncGroup::new(1, SplitId(1), SplitId(2));
        group.set_anchors(vec![
            SyncAnchor {
                left_line: 0,
                right_line: 0,
            },
            SyncAnchor {
                left_line: 50,
                right_line: 60,
            },
        ]);

        // Initial position
        assert_eq!(group.left_scroll_line(), 0);
        assert_eq!(group.right_scroll_line(), 0);

        // Scroll down 10 lines
        group.apply_scroll_delta(SplitId(1), 10);
        assert_eq!(group.left_scroll_line(), 10);
        assert_eq!(group.right_scroll_line(), 10);

        // Scroll to position past anchor
        group.set_scroll_line(55);
        assert_eq!(group.left_scroll_line(), 55);
        assert_eq!(group.right_scroll_line(), 65); // 60 + 5
    }
}
