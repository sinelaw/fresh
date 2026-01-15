use crate::model::event::CursorId;
use std::collections::HashMap;
use std::ops::Range;

/// Selection mode for cursors
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SelectionMode {
    /// Normal character-wise selection (stream)
    #[default]
    Normal,
    /// Block/rectangular selection (column-wise)
    Block,
}

/// Position in 2D coordinates (for block selection)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position2D {
    pub line: usize,
    pub column: usize,
}

/// A cursor in the buffer with optional selection
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Cursor {
    /// Primary position (where edits happen) - byte offset
    pub position: usize,

    /// Selection anchor (if any) for visual selection - byte offset
    pub anchor: Option<usize>,

    /// Desired column for vertical navigation
    /// When moving up/down, try to stay in this column
    pub sticky_column: usize,

    /// Selection mode (normal or block)
    pub selection_mode: SelectionMode,

    /// Block selection anchor position (line, column) for rectangular selections
    /// Only used when selection_mode is Block
    pub block_anchor: Option<Position2D>,

    /// Whether regular movement should clear the selection (default: true)
    /// When false (e.g., after set_mark in Emacs mode), movement preserves the anchor
    pub deselect_on_move: bool,
}

impl Cursor {
    /// Create a new cursor at a position
    pub fn new(position: usize) -> Self {
        Self {
            position,
            anchor: None,
            sticky_column: 0,
            selection_mode: SelectionMode::Normal,
            block_anchor: None,
            deselect_on_move: true, // Default: movement clears selection
        }
    }

    /// Create a cursor with a selection
    pub fn with_selection(start: usize, end: usize) -> Self {
        Self {
            position: end,
            anchor: Some(start),
            sticky_column: 0,
            selection_mode: SelectionMode::Normal,
            block_anchor: None,
            deselect_on_move: true, // Default: movement clears selection
        }
    }

    /// Is the cursor collapsed (no selection)?
    pub fn collapsed(&self) -> bool {
        self.anchor.is_none() && self.block_anchor.is_none()
    }

    /// Get the selection range, if any (for normal selection)
    pub fn selection_range(&self) -> Option<Range<usize>> {
        self.anchor.map(|anchor| {
            if anchor < self.position {
                anchor..self.position
            } else {
                self.position..anchor
            }
        })
    }

    /// Check if this cursor has a block selection
    pub fn has_block_selection(&self) -> bool {
        self.selection_mode == SelectionMode::Block && self.block_anchor.is_some()
    }

    /// Get the block selection bounds (start_line, start_col, end_line, end_col)
    /// Returns None if not in block selection mode
    pub fn block_selection_bounds(&self) -> Option<(usize, usize, usize, usize)> {
        if self.selection_mode != SelectionMode::Block {
            return None;
        }
        self.block_anchor.map(|anchor| {
            // We need current position as 2D coords, which requires buffer context
            // For now, just return the anchor info
            // The actual current position will be computed by the caller using the buffer
            (anchor.line, anchor.column, anchor.line, anchor.column)
        })
    }

    /// Get the start of the selection (min of position and anchor)
    pub fn selection_start(&self) -> usize {
        self.anchor.map_or(self.position, |a| a.min(self.position))
    }

    /// Get the end of the selection (max of position and anchor)
    pub fn selection_end(&self) -> usize {
        self.anchor.map_or(self.position, |a| a.max(self.position))
    }

    /// Clear the selection, keeping only the position
    pub fn clear_selection(&mut self) {
        self.anchor = None;
        self.block_anchor = None;
        self.selection_mode = SelectionMode::Normal;
    }

    /// Set the selection anchor
    pub fn set_anchor(&mut self, anchor: usize) {
        self.anchor = Some(anchor);
    }

    /// Start a block selection at the given 2D position
    pub fn start_block_selection(&mut self, line: usize, column: usize) {
        self.selection_mode = SelectionMode::Block;
        self.block_anchor = Some(Position2D { line, column });
        // Also set the byte anchor for compatibility
        // (will need to be set properly by caller with buffer context)
    }

    /// Clear block selection and return to normal mode
    pub fn clear_block_selection(&mut self) {
        self.selection_mode = SelectionMode::Normal;
        self.block_anchor = None;
    }

    /// Move to a position, optionally extending selection
    pub fn move_to(&mut self, position: usize, extend_selection: bool) {
        if extend_selection {
            if self.anchor.is_none() {
                self.anchor = Some(self.position);
            }
        } else {
            self.anchor = None;
            // Also clear block selection when not extending
            if !extend_selection && self.selection_mode == SelectionMode::Block {
                self.selection_mode = SelectionMode::Normal;
                self.block_anchor = None;
            }
        }
        self.position = position;
    }

    /// Adjust cursor position after an edit
    /// If an edit happens before the cursor, adjust position accordingly
    pub fn adjust_for_edit(&mut self, edit_pos: usize, old_len: usize, new_len: usize) {
        let delta = new_len as isize - old_len as isize;

        if edit_pos <= self.position {
            if edit_pos + old_len <= self.position {
                // Edit is completely before cursor
                self.position = (self.position as isize + delta).max(0) as usize;
            } else {
                // Edit overlaps cursor position - move to end of edit
                self.position = edit_pos + new_len;
            }
        }

        // Adjust anchor similarly
        if let Some(anchor) = self.anchor {
            if edit_pos <= anchor {
                if edit_pos + old_len <= anchor {
                    self.anchor = Some((anchor as isize + delta).max(0) as usize);
                } else {
                    self.anchor = Some(edit_pos + new_len);
                }
            }
        }
    }
}

/// Collection of cursors with multi-cursor support
#[derive(Debug, Clone)]
pub struct Cursors {
    /// Map from cursor ID to cursor
    cursors: HashMap<CursorId, Cursor>,

    /// Next available cursor ID
    next_id: usize,

    /// Primary cursor ID (the most recently added/active one)
    primary_id: CursorId,
}

impl Cursors {
    /// Create a new cursor collection with one cursor at position 0
    pub fn new() -> Self {
        let primary_id = CursorId(0);
        let mut cursors = HashMap::new();
        cursors.insert(primary_id, Cursor::new(0));

        Self {
            cursors,
            next_id: 1,
            primary_id,
        }
    }

    /// Get the primary cursor
    pub fn primary(&self) -> &Cursor {
        self.cursors
            .get(&self.primary_id)
            .expect("Primary cursor should always exist")
    }

    /// Get the primary cursor mutably
    pub fn primary_mut(&mut self) -> &mut Cursor {
        self.cursors
            .get_mut(&self.primary_id)
            .expect("Primary cursor should always exist")
    }

    /// Get the primary cursor ID
    pub fn primary_id(&self) -> CursorId {
        self.primary_id
    }

    /// Get a cursor by ID
    pub fn get(&self, id: CursorId) -> Option<&Cursor> {
        self.cursors.get(&id)
    }

    /// Get a cursor by ID mutably
    pub fn get_mut(&mut self, id: CursorId) -> Option<&mut Cursor> {
        self.cursors.get_mut(&id)
    }

    /// Add a new cursor and return its ID
    pub fn add(&mut self, cursor: Cursor) -> CursorId {
        let id = CursorId(self.next_id);
        self.next_id += 1;
        self.cursors.insert(id, cursor);
        self.primary_id = id; // New cursor becomes primary
        id
    }

    /// Insert a cursor with a specific ID (for undo/redo)
    pub fn insert_with_id(&mut self, id: CursorId, cursor: Cursor) {
        self.cursors.insert(id, cursor);
        self.primary_id = id; // New cursor becomes primary
                              // Update next_id if necessary to avoid collisions
        if id.0 >= self.next_id {
            self.next_id = id.0 + 1;
        }
    }

    /// Remove a cursor by ID
    pub fn remove(&mut self, id: CursorId) -> Option<Cursor> {
        // Can't remove the last cursor
        if self.cursors.len() <= 1 {
            return None;
        }

        let cursor = self.cursors.remove(&id);

        // If we removed the primary cursor, pick a new primary
        if id == self.primary_id {
            self.primary_id = *self
                .cursors
                .keys()
                .next()
                .expect("Should have at least one cursor remaining");
        }

        cursor
    }

    /// Remove all cursors except the first one (original cursor)
    /// When exiting multi-cursor mode, return to the original cursor position
    pub fn remove_secondary(&mut self) {
        // Find the cursor with the minimum ID (the original/first cursor)
        let first_id = *self
            .cursors
            .keys()
            .min_by_key(|id| id.0)
            .expect("Should have at least one cursor");
        let first_cursor = *self
            .cursors
            .get(&first_id)
            .expect("First cursor should exist");

        self.cursors.clear();
        self.cursors.insert(first_id, first_cursor);
        self.primary_id = first_id; // Update primary to be the first cursor
    }

    /// Get all cursor IDs
    pub fn ids(&self) -> Vec<CursorId> {
        self.cursors.keys().copied().collect()
    }

    /// Get all cursors as a slice
    pub fn iter(&self) -> impl Iterator<Item = (CursorId, &Cursor)> {
        self.cursors.iter().map(|(id, c)| (*id, c))
    }

    /// Get number of cursors
    pub fn count(&self) -> usize {
        self.cursors.len()
    }

    /// Apply a function to all cursors
    pub fn map<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut Cursor),
    {
        for cursor in self.cursors.values_mut() {
            f(cursor);
        }
    }

    /// Adjust all cursors after an edit
    pub fn adjust_for_edit(&mut self, edit_pos: usize, old_len: usize, new_len: usize) {
        for cursor in self.cursors.values_mut() {
            cursor.adjust_for_edit(edit_pos, old_len, new_len);
        }
    }

    /// Normalize cursors: merge overlapping selections, remove duplicates
    pub fn normalize(&mut self) {
        // Collect all cursors sorted by position
        let mut cursor_list: Vec<(CursorId, Cursor)> =
            self.cursors.iter().map(|(id, c)| (*id, *c)).collect();

        cursor_list.sort_by_key(|(_, c)| c.selection_start());

        // Remove exact duplicates
        cursor_list.dedup_by(|(_, a), (_, b)| a.position == b.position && a.anchor == b.anchor);

        // Rebuild cursors map
        self.cursors.clear();
        for (id, cursor) in cursor_list {
            self.cursors.insert(id, cursor);
        }

        // Ensure primary cursor still exists
        if !self.cursors.contains_key(&self.primary_id) {
            if let Some(id) = self.cursors.keys().next() {
                self.primary_id = *id;
            }
        }
    }

    /// Get all cursor positions (for rendering)
    pub fn positions(&self) -> Vec<usize> {
        self.cursors.values().map(|c| c.position).collect()
    }

    /// Get all selection ranges (for rendering)
    pub fn selections(&self) -> Vec<Range<usize>> {
        self.cursors
            .values()
            .filter_map(|c| c.selection_range())
            .collect()
    }
}

impl Default for Cursors {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cursor_new() {
        let cursor = Cursor::new(5);
        assert_eq!(cursor.position, 5);
        assert!(cursor.collapsed());
        assert_eq!(cursor.selection_range(), None);
    }

    #[test]
    fn test_cursor_with_selection() {
        let cursor = Cursor::with_selection(5, 10);
        assert_eq!(cursor.position, 10);
        assert!(!cursor.collapsed());
        assert_eq!(cursor.selection_range(), Some(5..10));
    }

    #[test]
    fn test_cursor_move_to() {
        let mut cursor = Cursor::new(5);
        cursor.move_to(10, false);
        assert_eq!(cursor.position, 10);
        assert!(cursor.collapsed());

        cursor.move_to(15, true);
        assert_eq!(cursor.position, 15);
        assert_eq!(cursor.selection_range(), Some(10..15));
    }

    #[test]
    fn test_cursor_adjust_for_edit() {
        let mut cursor = Cursor::new(10);

        // Edit before cursor
        cursor.adjust_for_edit(5, 0, 3);
        assert_eq!(cursor.position, 13);

        // Edit after cursor
        cursor.adjust_for_edit(20, 5, 2);
        assert_eq!(cursor.position, 13);
    }

    #[test]
    fn test_cursors_new() {
        let cursors = Cursors::new();
        assert_eq!(cursors.count(), 1);
        assert_eq!(cursors.primary().position, 0);
    }

    #[test]
    fn test_cursors_add_remove() {
        let mut cursors = Cursors::new();
        let id = cursors.add(Cursor::new(10));
        assert_eq!(cursors.count(), 2);
        assert_eq!(cursors.get(id).unwrap().position, 10);

        cursors.remove(id);
        assert_eq!(cursors.count(), 1);
    }

    #[test]
    fn test_cursors_remove_secondary() {
        let mut cursors = Cursors::new();
        cursors.add(Cursor::new(10));
        cursors.add(Cursor::new(20));
        assert_eq!(cursors.count(), 3);

        cursors.remove_secondary();
        assert_eq!(cursors.count(), 1);
    }

    #[test]
    fn test_cursors_normalize() {
        let mut cursors = Cursors::new();
        cursors.add(Cursor::new(10));
        cursors.add(Cursor::new(10)); // Duplicate

        cursors.normalize();
        assert_eq!(cursors.count(), 2); // Duplicates removed
    }
}
