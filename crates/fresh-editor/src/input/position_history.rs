/// Position history for go back/forward navigation like VS Code
///
/// This module tracks the user's position history across buffers,
/// allowing navigation back and forward through editing locations.
/// Similar to VS Code's Alt+Left/Alt+Right navigation.
///
/// ## Architecture
///
/// Position history consumes MoveCursor events from the event log and coalesces
/// consecutive movements into single "jump" entries. This means:
/// - Many arrow key presses = one jump entry
/// - Each buffer switch = commits pending movement and adds new entry
/// - Idle period = commits pending movement
///
/// This matches VS Code's behavior where you can navigate back through your
/// editing trail, not through every single keystroke.
use crate::model::event::BufferId;

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

/// Pending movement that may be coalesced with subsequent movements
#[derive(Clone, Debug)]
struct PendingMovement {
    /// Starting position of this movement sequence
    start_entry: PositionEntry,
}

/// Distance threshold for considering a movement "large" (in bytes)
/// Movements larger than this will not be coalesced
const LARGE_JUMP_THRESHOLD: usize = 50;

/// Position history manager
///
/// This tracks navigation history across the editor, storing positions
/// the user has visited. It maintains a stack with a current index,
/// allowing back/forward navigation.
///
/// Movements are coalesced: consecutive MoveCursor events within a short
/// time period are treated as a single "jump" for navigation purposes.
pub struct PositionHistory {
    /// Stack of position entries
    entries: Vec<PositionEntry>,

    /// Current index in the stack (where we are in history)
    /// Points to the current position
    current_index: Option<usize>,

    /// Maximum number of entries to keep
    max_entries: usize,

    /// Pending movement that hasn't been committed yet
    /// Gets committed when: buffer switches, timeout expires, or significant event
    pending_movement: Option<PendingMovement>,
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
            pending_movement: None,
        }
    }

    /// Record a cursor movement event
    ///
    /// This is called for EVERY MoveCursor event. Consecutive small movements are coalesced
    /// into a single history entry. The movement is committed to history when:
    /// - Buffer changes
    /// - Large jump detected (> 50 bytes distance from pending start position)
    /// - User triggers back/forward navigation
    pub fn record_movement(&mut self, buffer_id: BufferId, position: usize, anchor: Option<usize>) {
        let entry = PositionEntry::new(buffer_id, position, anchor);

        if let Some(pending) = &mut self.pending_movement {
            // Check if this is a continuation of the current movement
            if pending.start_entry.buffer_id == buffer_id {
                // Calculate distance from the pending movement's start position
                let distance = position.abs_diff(pending.start_entry.position);

                // Check if this is a small movement that should be coalesced
                if distance <= LARGE_JUMP_THRESHOLD {
                    // Small movement - keep coalescing, don't commit yet
                    return;
                }
            }

            // Different buffer or large jump - commit the pending movement
            self.commit_pending_movement();
        }

        // Start a new pending movement
        self.pending_movement = Some(PendingMovement { start_entry: entry });
    }

    /// Commit any pending movement to history
    ///
    /// This is called when:
    /// - Switching buffers
    /// - Before navigating back/forward
    pub fn commit_pending_movement(&mut self) {
        if let Some(pending) = self.pending_movement.take() {
            // Always call push(), which handles both:
            // 1. Truncating forward history (if we're not at the end)
            // 2. Checking for duplicates before adding
            self.push(pending.start_entry);
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
            if current_idx < self.entries.len() && self.entries[current_idx] == entry {
                return;
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
    /// Commits any pending movement first, then returns the previous position.
    /// Returns None if we're at the beginning of history.
    pub fn back(&mut self) -> Option<&PositionEntry> {
        // Commit any pending movement before navigating
        self.commit_pending_movement();

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
        self.current_index.and_then(|idx| self.entries.get(idx))
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

    /// Get current index (for debugging)
    pub fn current_index(&self) -> Option<usize> {
        self.current_index
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
