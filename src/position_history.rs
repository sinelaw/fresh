/// Position history for go back/forward navigation like VS Code
///
/// This module tracks the user's position history across buffers,
/// allowing navigation back and forward through editing locations.
/// Similar to VS Code's Alt+Left/Alt+Right navigation.

use crate::event::BufferId;

/// A single entry in the position history
#[derive(Clone, Debug, PartialEq)]
pub struct PositionEntry {
    /// The buffer ID
    pub buffer_id: BufferId,

    /// The cursor position (byte offset)
    pub position: usize,

    /// Optional selection anchor
    pub anchor: Option<usize>,
}

impl PositionEntry {
    /// Create a new position entry
    pub fn new(buffer_id: BufferId, position: usize, anchor: Option<usize>) -> Self {
        Self {
            buffer_id,
            position,
            anchor,
        }
    }
}

/// Position history manager
///
/// This tracks navigation history across the editor, storing positions
/// the user has visited. It maintains a stack with a current index,
/// allowing back/forward navigation.
pub struct PositionHistory {
    /// Stack of position entries
    entries: Vec<PositionEntry>,

    /// Current index in the stack (where we are in history)
    /// Points to the current position
    current_index: Option<usize>,

    /// Maximum number of entries to keep
    max_entries: usize,
}

impl PositionHistory {
    /// Create a new position history with default max entries (100)
    pub fn new() -> Self {
        Self::with_capacity(100)
    }

    /// Create a new position history with specified max entries
    pub fn with_capacity(max_entries: usize) -> Self {
        Self {
            entries: Vec::new(),
            current_index: None,
            max_entries,
        }
    }

    /// Push a new position to the history
    ///
    /// This is called when the user makes a significant navigation:
    /// - Switching buffers
    /// - Large cursor movements (e.g., search, go-to-definition)
    /// - Opening a file
    ///
    /// If we're not at the end of history (user has gone back), this
    /// truncates the forward history and adds the new position.
    pub fn push(&mut self, entry: PositionEntry) {
        // If we're not at the end, truncate forward history FIRST
        // This ensures forward history is cleared even if the new entry is a duplicate
        if let Some(current_idx) = self.current_index {
            self.entries.truncate(current_idx + 1);
        }

        // Don't add duplicate consecutive entries
        if let Some(current_idx) = self.current_index {
            if current_idx < self.entries.len() {
                if self.entries[current_idx] == entry {
                    return;
                }
            }
        }

        // Add new entry
        self.entries.push(entry);

        // Limit size
        if self.entries.len() > self.max_entries {
            self.entries.remove(0);
        }

        // Update current index to point to the new entry
        self.current_index = Some(self.entries.len() - 1);
    }

    /// Navigate back in history
    ///
    /// Returns the previous position, or None if we're at the beginning
    /// of history. This does NOT include the current position - it moves
    /// to the previous entry.
    pub fn back(&mut self) -> Option<&PositionEntry> {
        if self.entries.is_empty() {
            return None;
        }

        match self.current_index {
            None => None,
            Some(0) => None, // Already at the beginning
            Some(idx) => {
                self.current_index = Some(idx - 1);
                Some(&self.entries[idx - 1])
            }
        }
    }

    /// Navigate forward in history
    ///
    /// Returns the next position, or None if we're at the end of history.
    pub fn forward(&mut self) -> Option<&PositionEntry> {
        if self.entries.is_empty() {
            return None;
        }

        match self.current_index {
            None => None,
            Some(idx) if idx >= self.entries.len() - 1 => None, // Already at the end
            Some(idx) => {
                self.current_index = Some(idx + 1);
                Some(&self.entries[idx + 1])
            }
        }
    }

    /// Check if we can go back
    pub fn can_go_back(&self) -> bool {
        match self.current_index {
            Some(idx) => idx > 0,
            None => false,
        }
    }

    /// Check if we can go forward
    pub fn can_go_forward(&self) -> bool {
        match self.current_index {
            Some(idx) => idx < self.entries.len() - 1,
            None => false,
        }
    }

    /// Get the current position entry
    pub fn current(&self) -> Option<&PositionEntry> {
        self.current_index
            .and_then(|idx| self.entries.get(idx))
    }

    /// Clear all history
    pub fn clear(&mut self) {
        self.entries.clear();
        self.current_index = None;
    }

    /// Get the number of entries in history
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Check if history is empty
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

impl Default for PositionHistory {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_entry(buffer_id: usize, position: usize) -> PositionEntry {
        PositionEntry::new(BufferId(buffer_id), position, None)
    }

    #[test]
    fn test_new_history_is_empty() {
        let history = PositionHistory::new();
        assert!(history.is_empty());
        assert_eq!(history.len(), 0);
        assert!(!history.can_go_back());
        assert!(!history.can_go_forward());
    }

    #[test]
    fn test_push_single_entry() {
        let mut history = PositionHistory::new();
        let entry = make_entry(1, 10);

        history.push(entry.clone());

        assert_eq!(history.len(), 1);
        assert_eq!(history.current(), Some(&entry));
        assert!(!history.can_go_back());
        assert!(!history.can_go_forward());
    }

    #[test]
    fn test_push_multiple_entries() {
        let mut history = PositionHistory::new();
        let entry1 = make_entry(1, 10);
        let entry2 = make_entry(1, 20);
        let entry3 = make_entry(2, 5);

        history.push(entry1.clone());
        history.push(entry2.clone());
        history.push(entry3.clone());

        assert_eq!(history.len(), 3);
        assert_eq!(history.current(), Some(&entry3));
        assert!(history.can_go_back());
        assert!(!history.can_go_forward());
    }

    #[test]
    fn test_back_navigation() {
        let mut history = PositionHistory::new();
        let entry1 = make_entry(1, 10);
        let entry2 = make_entry(1, 20);
        let entry3 = make_entry(2, 5);

        history.push(entry1.clone());
        history.push(entry2.clone());
        history.push(entry3.clone());

        // Go back once
        let back1 = history.back();
        assert_eq!(back1, Some(&entry2));
        assert_eq!(history.current(), Some(&entry2));
        assert!(history.can_go_back());
        assert!(history.can_go_forward());

        // Go back again
        let back2 = history.back();
        assert_eq!(back2, Some(&entry1));
        assert_eq!(history.current(), Some(&entry1));
        assert!(!history.can_go_back());
        assert!(history.can_go_forward());

        // Try to go back at beginning
        let back3 = history.back();
        assert_eq!(back3, None);
        assert_eq!(history.current(), Some(&entry1));
    }

    #[test]
    fn test_forward_navigation() {
        let mut history = PositionHistory::new();
        let entry1 = make_entry(1, 10);
        let entry2 = make_entry(1, 20);
        let entry3 = make_entry(2, 5);

        history.push(entry1.clone());
        history.push(entry2.clone());
        history.push(entry3.clone());

        // Go back twice
        history.back();
        history.back();
        assert_eq!(history.current(), Some(&entry1));

        // Go forward once
        let fwd1 = history.forward();
        assert_eq!(fwd1, Some(&entry2));
        assert_eq!(history.current(), Some(&entry2));

        // Go forward again
        let fwd2 = history.forward();
        assert_eq!(fwd2, Some(&entry3));
        assert_eq!(history.current(), Some(&entry3));

        // Try to go forward at end
        let fwd3 = history.forward();
        assert_eq!(fwd3, None);
        assert_eq!(history.current(), Some(&entry3));
    }

    #[test]
    fn test_push_truncates_forward_history() {
        let mut history = PositionHistory::new();
        let entry1 = make_entry(1, 10);
        let entry2 = make_entry(1, 20);
        let entry3 = make_entry(2, 5);
        let entry4 = make_entry(2, 15);

        history.push(entry1.clone());
        history.push(entry2.clone());
        history.push(entry3.clone());

        // Go back twice
        history.back();
        history.back();
        assert_eq!(history.current(), Some(&entry1));

        // Push new entry - should truncate forward history
        history.push(entry4.clone());

        assert_eq!(history.len(), 2);
        assert_eq!(history.current(), Some(&entry4));
        assert!(history.can_go_back());
        assert!(!history.can_go_forward());

        // Verify we can go back to entry1
        let back = history.back();
        assert_eq!(back, Some(&entry1));
    }

    #[test]
    fn test_duplicate_consecutive_entries_not_added() {
        let mut history = PositionHistory::new();
        let entry1 = make_entry(1, 10);

        history.push(entry1.clone());
        history.push(entry1.clone());
        history.push(entry1.clone());

        assert_eq!(history.len(), 1);
    }

    #[test]
    fn test_max_entries_limit() {
        let mut history = PositionHistory::with_capacity(3);

        for i in 0..5 {
            history.push(make_entry(1, i * 10));
        }

        assert_eq!(history.len(), 3);
        // Should have kept the last 3 entries (20, 30, 40)
        assert_eq!(history.current(), Some(&make_entry(1, 40)));

        history.back();
        assert_eq!(history.current(), Some(&make_entry(1, 30)));

        history.back();
        assert_eq!(history.current(), Some(&make_entry(1, 20)));
    }

    #[test]
    fn test_clear() {
        let mut history = PositionHistory::new();

        history.push(make_entry(1, 10));
        history.push(make_entry(1, 20));

        assert_eq!(history.len(), 2);

        history.clear();

        assert!(history.is_empty());
        assert_eq!(history.len(), 0);
        assert_eq!(history.current(), None);
        assert!(!history.can_go_back());
        assert!(!history.can_go_forward());
    }
}
