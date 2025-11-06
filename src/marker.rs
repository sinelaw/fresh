/// Gap-based marker list for content-anchored positions
///
/// This module implements a marker system inspired by Emacs, where markers
/// automatically adjust their positions when text is inserted or deleted.
///
/// Instead of storing absolute positions that all need updating on every edit,
/// markers are stored in a sequential list with "gaps" (byte counts) between them.
/// When text is inserted, only the gap containing the insertion point needs updating.
///
/// Example:
/// ```text
/// Buffer: "Hello World"
///          ^     ^
///          m1    m2
///
/// Storage: [Gap(0), Marker(m1), Gap(6), Marker(m2), Gap(5)]
/// ```
///
/// After inserting "Beautiful " at position 6:
/// ```text
/// Buffer: "Hello Beautiful World"
///          ^                ^
///          m1               m2
///
/// Storage: [Gap(0), Marker(m1), Gap(16), Marker(m2), Gap(5)]
///          Only this gap size changed! ^^^
/// ```

use std::collections::HashMap;

/// Unique identifier for a marker
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MarkerId(u64);

/// Entry in the marker list - either a gap (content bytes) or a marker
#[derive(Debug, Clone, PartialEq)]
pub enum MarkerEntry {
    /// A gap representing N bytes of buffer content
    Gap(usize),

    /// A marker at this position
    Marker {
        id: MarkerId,
        /// Insertion affinity:
        /// - true (left): marker stays before text inserted at this position
        /// - false (right): marker moves after text inserted at this position
        left_affinity: bool,
    },
}

/// Sequential list of markers and gaps
///
/// Invariants:
/// - Always starts with a Gap (possibly 0-sized)
/// - Always ends with a Gap (possibly 0-sized)
/// - No two adjacent Gaps (they must be merged)
/// - marker_index accurately maps marker IDs to their position in entries
pub struct MarkerList {
    /// Markers and gaps in order
    entries: Vec<MarkerEntry>,

    /// Fast lookup: marker ID → index in entries vec
    marker_index: HashMap<MarkerId, usize>,

    /// Next marker ID to allocate
    next_id: u64,
}

impl MarkerList {
    /// Create a new empty marker list
    pub fn new() -> Self {
        Self {
            // Start with a single gap covering the entire buffer (initially empty)
            entries: vec![MarkerEntry::Gap(0)],
            marker_index: HashMap::new(),
            next_id: 0,
        }
    }

    /// Create a new marker at the given position
    ///
    /// # Arguments
    /// * `position` - Byte offset in the buffer
    /// * `left_affinity` - If true, marker stays before text inserted at this position
    ///
    /// # Returns
    /// The ID of the newly created marker
    pub fn create(&mut self, position: usize, left_affinity: bool) -> MarkerId {
        let id = MarkerId(self.next_id);
        self.next_id += 1;

        tracing::debug!(
            "Creating marker {:?} at position {} with {} affinity. Total entries: {}, first entry: {:?}",
            id,
            position,
            if left_affinity { "left" } else { "right" },
            self.entries.len(),
            self.entries.get(0)
        );

        // Find which gap contains this position
        let mut cumulative_pos = 0;
        let mut insert_idx = 0;
        let mut found = false;

        for (idx, entry) in self.entries.iter().enumerate() {
            match entry {
                MarkerEntry::Gap(size) => {
                    let gap_start = cumulative_pos;
                    let gap_end = cumulative_pos + size;

                    tracing::trace!(
                        "Checking gap at idx={}: start={}, end={}, size={}",
                        idx,
                        gap_start,
                        gap_end,
                        size
                    );

                    if position >= gap_start && position <= gap_end {
                        // Found the gap containing position
                        let offset_in_gap = position - cumulative_pos;

                        tracing::debug!(
                            "Found gap for position {}: gap_idx={}, gap_size={}, offset_in_gap={}",
                            position,
                            idx,
                            size,
                            offset_in_gap
                        );

                        // Split gap: [before, marker, after]
                        let gap_before = offset_in_gap;
                        let gap_after = size - offset_in_gap;

                        // Replace this gap with [gap_before, marker, gap_after]
                        self.entries[idx] = MarkerEntry::Gap(gap_before);
                        self.entries.insert(
                            idx + 1,
                            MarkerEntry::Marker { id, left_affinity },
                        );
                        self.entries.insert(idx + 2, MarkerEntry::Gap(gap_after));

                        insert_idx = idx + 1;
                        found = true;
                        break;
                    }

                    cumulative_pos = gap_end;
                }
                MarkerEntry::Marker { .. } => {
                    // Markers don't contribute to position
                }
            }
        }

        if !found {
            tracing::error!(
                "Failed to find gap for position {}! Cumulative pos reached: {}, total entries: {}",
                position,
                cumulative_pos,
                self.entries.len()
            );
        }

        // Update marker index (adjusting for insertions that shifted other markers)
        self.reindex_from(insert_idx);

        id
    }

    /// Delete a marker
    pub fn delete(&mut self, id: MarkerId) {
        if let Some(&idx) = self.marker_index.get(&id) {
            self.entries.remove(idx);
            self.marker_index.remove(&id);

            // Merge adjacent gaps
            self.merge_gaps_at(idx);

            // Reindex markers after this position
            self.reindex_from(idx);
        }
    }

    /// Get the current byte position of a marker
    ///
    /// This walks the gap list and sums up gaps before the marker.
    /// Cost: O(M) where M is the number of markers before this one.
    pub fn get_position(&self, id: MarkerId) -> Option<usize> {
        let marker_idx = self.marker_index.get(&id)?;

        // Calculate cumulative position up to this marker
        let mut pos = 0;
        for entry in &self.entries[..*marker_idx] {
            if let MarkerEntry::Gap(size) = entry {
                pos += size;
            }
        }

        Some(pos)
    }

    /// Adjust all markers for an insertion
    ///
    /// When text is inserted, we only need to find the gap containing the
    /// insertion point and increase its size. Markers before are unaffected,
    /// and markers after automatically "move" because the gap grew.
    ///
    /// # Arguments
    /// * `position` - Byte offset where text was inserted
    /// * `length` - Number of bytes inserted
    pub fn adjust_for_insert(&mut self, position: usize, length: usize) {
        if length == 0 {
            return;
        }

        tracing::debug!(
            "adjust_for_insert: position={}, length={}, entries_len={}, first_entry={:?}",
            position,
            length,
            self.entries.len(),
            self.entries.get(0)
        );

        // First pass: find which entry to update
        let mut cumulative_pos = 0;
        let mut target_idx = None;

        for (idx, entry) in self.entries.iter().enumerate() {
            match entry {
                MarkerEntry::Gap(size) => {
                    let gap_start = cumulative_pos;
                    let gap_end = cumulative_pos + *size;

                    if position >= gap_start && position < gap_end {
                        // Insertion is strictly inside this gap (not at boundary)
                        target_idx = Some(idx);
                        break;
                    } else if position == gap_end {
                        // At gap boundary - check if next entry is a marker
                        if idx + 1 < self.entries.len() {
                            if let MarkerEntry::Marker { left_affinity, .. } = self.entries[idx + 1] {
                                // There's a marker at this position
                                if left_affinity {
                                    // Left affinity: insertion goes after marker
                                    target_idx = Some(idx + 2);
                                } else {
                                    // Right affinity: insertion goes before marker
                                    target_idx = Some(idx);
                                }
                                break;
                            }
                        }
                        // No marker at boundary, insert at end of this gap
                        target_idx = Some(idx);
                        break;
                    }

                    cumulative_pos = gap_end;
                }

                MarkerEntry::Marker { .. } => {
                    // Markers don't contribute to cumulative position
                }
            }
        }

        // Second pass: update the target gap
        if let Some(idx) = target_idx {
            if let MarkerEntry::Gap(ref mut size) = self.entries[idx] {
                tracing::debug!("Adjusting gap at idx={} from {} to {}", idx, *size, *size + length);
                *size += length;
            }
        } else {
            tracing::error!(
                "adjust_for_insert: Could not find gap for position={}! cumulative_pos={}, entries_len={}",
                position,
                cumulative_pos,
                self.entries.len()
            );
        }
    }

    /// Adjust all markers for a deletion
    ///
    /// This is more complex than insertion because:
    /// 1. Multiple gaps might be affected
    /// 2. Markers inside the deleted range must be removed
    /// 3. Adjacent gaps might need merging
    ///
    /// # Arguments
    /// * `position` - Byte offset where deletion starts
    /// * `length` - Number of bytes deleted
    pub fn adjust_for_delete(&mut self, position: usize, length: usize) {
        if length == 0 {
            return;
        }

        let delete_end = position + length;

        // First pass: calculate positions and identify what to remove/update
        let mut cumulative_pos = 0;
        let mut actions = Vec::new();

        for (idx, entry) in self.entries.iter().enumerate() {
            match entry {
                MarkerEntry::Gap(size) => {
                    let gap_start = cumulative_pos;
                    let gap_end = cumulative_pos + *size;

                    if delete_end <= gap_start {
                        // Deletion is entirely before this gap - we're done
                        break;
                    } else if position < gap_end {
                        // Deletion overlaps this gap
                        let overlap_start = position.max(gap_start);
                        let overlap_end = delete_end.min(gap_end);
                        let overlap_size = overlap_end - overlap_start;

                        if overlap_size > 0 {
                            actions.push((idx, overlap_size));
                        }
                    }

                    cumulative_pos = gap_end;
                }

                MarkerEntry::Marker { id, .. } => {
                    if position <= cumulative_pos && cumulative_pos < delete_end {
                        // This marker is inside the deletion - mark for removal
                        actions.push((idx, 0)); // 0 size means "remove marker"
                        self.marker_index.remove(id);
                    }
                }
            }
        }

        // Second pass: apply actions in reverse order (so indices stay valid)
        for (idx, overlap_size) in actions.iter().rev() {
            if *overlap_size == 0 {
                // Remove marker
                self.entries.remove(*idx);
            } else {
                // Reduce gap size
                if let MarkerEntry::Gap(ref mut size) = self.entries[*idx] {
                    *size -= overlap_size;
                }
            }
        }

        // Merge any adjacent gaps that resulted from deletions
        self.merge_all_adjacent_gaps();

        // Rebuild marker index (positions have shifted)
        self.reindex_all();
    }

    /// Merge adjacent gaps starting at the given index
    fn merge_gaps_at(&mut self, idx: usize) {
        // Check if we can merge with previous gap
        if idx > 0 && idx < self.entries.len() {
            if let (MarkerEntry::Gap(prev_size), MarkerEntry::Gap(curr_size)) =
                (&self.entries[idx - 1], &self.entries[idx])
            {
                let merged_size = *prev_size + *curr_size;
                self.entries[idx - 1] = MarkerEntry::Gap(merged_size);
                self.entries.remove(idx);
                return;
            }
        }

        // Check if we can merge with next gap
        if idx + 1 < self.entries.len() {
            if let (MarkerEntry::Gap(curr_size), MarkerEntry::Gap(next_size)) =
                (&self.entries[idx], &self.entries[idx + 1])
            {
                let merged_size = *curr_size + *next_size;
                self.entries[idx] = MarkerEntry::Gap(merged_size);
                self.entries.remove(idx + 1);
            }
        }
    }

    /// Merge all adjacent gaps in the entire list
    fn merge_all_adjacent_gaps(&mut self) {
        let mut i = 0;
        while i + 1 < self.entries.len() {
            if let (MarkerEntry::Gap(size1), MarkerEntry::Gap(size2)) =
                (&self.entries[i], &self.entries[i + 1])
            {
                // Found adjacent gaps - merge them
                let merged_size = *size1 + *size2;
                self.entries[i] = MarkerEntry::Gap(merged_size);
                self.entries.remove(i + 1);
                // Don't increment i, check again in case there's another gap
            } else {
                i += 1;
            }
        }
    }

    /// Rebuild the entire marker index
    fn reindex_all(&mut self) {
        self.marker_index.clear();
        for (idx, entry) in self.entries.iter().enumerate() {
            if let MarkerEntry::Marker { id, .. } = entry {
                self.marker_index.insert(*id, idx);
            }
        }
    }

    /// Reindex markers from the given index onwards
    fn reindex_from(&mut self, start_idx: usize) {
        for (idx, entry) in self.entries[start_idx..].iter().enumerate() {
            if let MarkerEntry::Marker { id, .. } = entry {
                self.marker_index.insert(*id, start_idx + idx);
            }
        }
    }

    /// Get the total size of the buffer (sum of all gaps)
    pub fn buffer_size(&self) -> usize {
        self.entries
            .iter()
            .filter_map(|e| match e {
                MarkerEntry::Gap(size) => Some(*size),
                _ => None,
            })
            .sum()
    }

    /// Get the number of markers
    pub fn marker_count(&self) -> usize {
        self.marker_index.len()
    }

    /// Set the initial buffer size (for tests)
    /// This resets the marker list to contain a single gap of the given size
    #[cfg(test)]
    pub fn set_buffer_size(&mut self, size: usize) {
        self.entries = vec![MarkerEntry::Gap(size)];
        self.marker_index.clear();
        self.next_id = 0;
    }

    /// Iterate through entries (for testing and debugging)
    #[cfg(test)]
    pub fn entries(&self) -> &[MarkerEntry] {
        &self.entries
    }

    /// Check invariants (for testing)
    #[cfg(test)]
    pub fn check_invariants(&self) -> Result<(), String> {
        // Must start with a Gap
        if !matches!(self.entries.first(), Some(MarkerEntry::Gap(_))) {
            return Err("Must start with a Gap".to_string());
        }

        // Must end with a Gap
        if !matches!(self.entries.last(), Some(MarkerEntry::Gap(_))) {
            return Err("Must end with a Gap".to_string());
        }

        // No adjacent gaps
        for window in self.entries.windows(2) {
            if matches!(&window[0], MarkerEntry::Gap(_))
                && matches!(&window[1], MarkerEntry::Gap(_))
            {
                return Err(format!("Adjacent gaps found: {:?}", window));
            }
        }

        // Marker index is accurate
        for (&id, &idx) in &self.marker_index {
            if idx >= self.entries.len() {
                return Err(format!("Marker index out of bounds: {} >= {}", idx, self.entries.len()));
            }
            match &self.entries[idx] {
                MarkerEntry::Marker { id: entry_id, .. } if *entry_id == id => {}
                _ => {
                    return Err(format!(
                        "Marker index mismatch: {:?} at {} points to {:?}",
                        id, idx, self.entries[idx]
                    ));
                }
            }
        }

        Ok(())
    }
}

impl Default for MarkerList {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_marker_list() {
        let list = MarkerList::new();
        assert_eq!(list.entries.len(), 1);
        assert!(matches!(list.entries[0], MarkerEntry::Gap(0)));
        assert!(list.marker_index.is_empty());
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_create_marker_at_start() {
        let mut list = MarkerList::new();

        let m1 = list.create(0, true);
        assert_eq!(list.marker_count(), 1);
        assert_eq!(list.get_position(m1), Some(0));
        list.check_invariants().unwrap();

        // Should have: [Gap(0), Marker, Gap(0)]
        assert_eq!(list.entries.len(), 3);
    }

    #[test]
    fn test_create_multiple_markers() {
        let mut list = MarkerList::new();

        // Simulate a buffer of 20 bytes
        list.set_buffer_size(20);

        let m1 = list.create(5, true);
        let m2 = list.create(15, false);

        assert_eq!(list.get_position(m1), Some(5));
        assert_eq!(list.get_position(m2), Some(15));
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_insert_before_marker() {
        let mut list = MarkerList::new();
        list.set_buffer_size(20);

        let m1 = list.create(10, true);
        assert_eq!(list.get_position(m1), Some(10));

        // Insert 5 bytes before marker
        list.adjust_for_insert(5, 5);

        // Marker should have moved forward
        assert_eq!(list.get_position(m1), Some(15));
        assert_eq!(list.buffer_size(), 25);
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_insert_after_marker() {
        let mut list = MarkerList::new();
        list.set_buffer_size(20);

        let m1 = list.create(10, true);
        assert_eq!(list.get_position(m1), Some(10));

        // Insert 5 bytes after marker
        list.adjust_for_insert(15, 5);

        // Marker should stay at same position
        assert_eq!(list.get_position(m1), Some(10));
        assert_eq!(list.buffer_size(), 25);
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_insert_at_marker_left_affinity() {
        let mut list = MarkerList::new();
        list.set_buffer_size(20);

        // Left affinity: marker stays before inserted text
        let m1 = list.create(10, true);

        // Insert at marker position
        list.adjust_for_insert(10, 5);

        // Marker should stay at 10, insertion goes after
        assert_eq!(list.get_position(m1), Some(10));
        assert_eq!(list.buffer_size(), 25);
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_insert_at_marker_right_affinity() {
        let mut list = MarkerList::new();
        list.set_buffer_size(20);

        // Right affinity: marker moves after inserted text
        let m1 = list.create(10, false);

        // Insert at marker position
        list.adjust_for_insert(10, 5);

        // Marker should move to 15, insertion goes before
        assert_eq!(list.get_position(m1), Some(15));
        assert_eq!(list.buffer_size(), 25);
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_delete_before_marker() {
        let mut list = MarkerList::new();
        list.set_buffer_size(20);

        let m1 = list.create(15, true);
        assert_eq!(list.get_position(m1), Some(15));

        // Delete 5 bytes before marker (at position 5)
        list.adjust_for_delete(5, 5);

        // Marker should move backward
        assert_eq!(list.get_position(m1), Some(10));
        assert_eq!(list.buffer_size(), 15);
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_delete_after_marker() {
        let mut list = MarkerList::new();
        list.set_buffer_size(20);

        let m1 = list.create(10, true);
        assert_eq!(list.get_position(m1), Some(10));

        // Delete 5 bytes after marker (at position 15)
        list.adjust_for_delete(15, 5);

        // Marker should stay at same position
        assert_eq!(list.get_position(m1), Some(10));
        assert_eq!(list.buffer_size(), 15);
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_delete_marker() {
        let mut list = MarkerList::new();
        list.set_buffer_size(20);

        let m1 = list.create(10, true);

        // Delete the marker itself
        list.adjust_for_delete(10, 5);

        // Marker should be gone
        assert_eq!(list.get_position(m1), None);
        assert_eq!(list.marker_count(), 0);
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_delete_multiple_markers() {
        let mut list = MarkerList::new();
        list.set_buffer_size(30);

        let m1 = list.create(10, true);
        let m2 = list.create(15, true);
        let m3 = list.create(20, true);

        // Delete range covering m1 and m2
        list.adjust_for_delete(8, 10);

        // m1 and m2 should be gone, m3 should have moved
        assert_eq!(list.get_position(m1), None);
        assert_eq!(list.get_position(m2), None);
        assert_eq!(list.get_position(m3), Some(10)); // 20 - 10 = 10
        assert_eq!(list.marker_count(), 1);
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_complex_scenario() {
        let mut list = MarkerList::new();
        list.set_buffer_size(100);

        // Create markers at 10, 20, 30
        let m1 = list.create(10, true);
        let m2 = list.create(20, true);
        let m3 = list.create(30, true);

        // Insert at 15
        list.adjust_for_insert(15, 5);
        assert_eq!(list.get_position(m1), Some(10));
        assert_eq!(list.get_position(m2), Some(25)); // 20 + 5
        assert_eq!(list.get_position(m3), Some(35)); // 30 + 5

        // Delete at 12, length 8 (delete range [12, 20))
        // This removes part of the gap between m1 and m2, but not m2 itself
        list.adjust_for_delete(12, 8);
        assert_eq!(list.get_position(m1), Some(10));  // Before deletion
        assert_eq!(list.get_position(m2), Some(17)); // 25 - 8 = 17
        assert_eq!(list.get_position(m3), Some(27)); // 35 - 8 = 27

        list.check_invariants().unwrap();
    }

    #[test]
    fn test_marker_deletion_with_delete_method() {
        let mut list = MarkerList::new();
        list.set_buffer_size(20);

        let m1 = list.create(10, true);
        let m2 = list.create(15, false);

        // Delete m1
        list.delete(m1);

        assert_eq!(list.get_position(m1), None);
        assert_eq!(list.get_position(m2), Some(15));
        assert_eq!(list.marker_count(), 1);
        list.check_invariants().unwrap();
    }

    // Property-based tests
    #[cfg(test)]
    mod property_tests {
        use super::*;
        use proptest::prelude::*;

        /// Generate random edit operations
        #[derive(Debug, Clone)]
        enum EditOp {
            Insert { position: usize, length: usize },
            Delete { position: usize, length: usize },
        }

        fn arb_edit_op(max_buffer_size: usize) -> impl Strategy<Value = EditOp> {
            prop_oneof![
                (0..=max_buffer_size, 1..=50usize).prop_map(|(pos, len)| EditOp::Insert {
                    position: pos,
                    length: len
                }),
                (0..=max_buffer_size, 1..=20usize).prop_map(|(pos, len)| EditOp::Delete {
                    position: pos,
                    length: len
                }),
            ]
        }

        proptest! {
            /// Invariants should hold after any sequence of operations
            #[test]
            fn prop_invariants_hold(
                buffer_size in 10..1000usize,
                ops in prop::collection::vec(arb_edit_op(1000), 1..20)
            ) {
                let mut list = MarkerList::new();
                list.set_buffer_size(buffer_size);

                // Create some markers
                let markers: Vec<_> = (0..5)
                    .map(|i| list.create((i * buffer_size / 6).min(buffer_size), i % 2 == 0))
                    .collect();

                // Apply random operations
                for op in ops {
                    match op {
                        EditOp::Insert { position, length } => {
                            let pos = position.min(list.buffer_size());
                            list.adjust_for_insert(pos, length);
                        }
                        EditOp::Delete { position, length } => {
                            let pos = position.min(list.buffer_size());
                            let len = length.min(list.buffer_size().saturating_sub(pos));
                            if len > 0 {
                                list.adjust_for_delete(pos, len);
                            }
                        }
                    }

                    // Invariants must hold after every operation
                    list.check_invariants().unwrap();
                }

                // All remaining markers should have valid positions
                for marker in markers {
                    if let Some(pos) = list.get_position(marker) {
                        assert!(pos <= list.buffer_size());
                    }
                }
            }

            /// Marker positions should be in the same order after edits
            #[test]
            fn prop_marker_ordering_preserved(
                buffer_size in 100..500usize,
                ops in prop::collection::vec(arb_edit_op(500), 1..10)
            ) {
                let mut list = MarkerList::new();
                list.set_buffer_size(buffer_size);

                // Create markers in order
                let markers: Vec<_> = (0..5)
                    .map(|i| list.create(i * 20, true))
                    .collect();

                // Apply operations
                for op in ops {
                    match op {
                        EditOp::Insert { position, length } => {
                            let pos = position.min(list.buffer_size());
                            list.adjust_for_insert(pos, length);
                        }
                        EditOp::Delete { position, length } => {
                            let pos = position.min(list.buffer_size());
                            let len = length.min(list.buffer_size().saturating_sub(pos));
                            if len > 0 {
                                list.adjust_for_delete(pos, len);
                            }
                        }
                    }
                }

                // Get positions of surviving markers
                let positions: Vec<_> = markers
                    .iter()
                    .filter_map(|&m| list.get_position(m))
                    .collect();

                // Should still be in order (no inversions)
                for window in positions.windows(2) {
                    assert!(window[0] <= window[1], "Marker ordering violated: {:?}", positions);
                }
            }

            /// Buffer size should be sum of all gaps
            #[test]
            fn prop_buffer_size_is_sum_of_gaps(
                buffer_size in 10..500usize,
                ops in prop::collection::vec(arb_edit_op(500), 1..15)
            ) {
                let mut list = MarkerList::new();
                list.set_buffer_size(buffer_size);

                let mut expected_size = buffer_size;

                for op in ops {
                    match op {
                        EditOp::Insert { position, length } => {
                            let pos = position.min(list.buffer_size());
                            list.adjust_for_insert(pos, length);
                            expected_size += length;
                        }
                        EditOp::Delete { position, length } => {
                            let pos = position.min(list.buffer_size());
                            let len = length.min(list.buffer_size().saturating_sub(pos));
                            if len > 0 {
                                list.adjust_for_delete(pos, len);
                                expected_size -= len;
                            }
                        }
                    }

                    assert_eq!(list.buffer_size(), expected_size,
                        "Buffer size mismatch after {:?}", op);
                }
            }

            /// No adjacent gaps should exist after operations
            #[test]
            fn prop_no_adjacent_gaps(
                buffer_size in 50..300usize,
                ops in prop::collection::vec(arb_edit_op(300), 1..10)
            ) {
                let mut list = MarkerList::new();
                list.set_buffer_size(buffer_size);

                // Create some markers
                for i in 0..3 {
                    list.create((i * buffer_size / 4).min(buffer_size), true);
                }

                // Apply operations
                for op in ops {
                    match op {
                        EditOp::Insert { position, length } => {
                            let pos = position.min(list.buffer_size());
                            list.adjust_for_insert(pos, length);
                        }
                        EditOp::Delete { position, length } => {
                            let pos = position.min(list.buffer_size());
                            let len = length.min(list.buffer_size().saturating_sub(pos));
                            if len > 0 {
                                list.adjust_for_delete(pos, len);
                            }
                        }
                    }

                    // Check no adjacent gaps
                    for window in list.entries.windows(2) {
                        let has_adjacent_gaps = matches!(&window[0], MarkerEntry::Gap(_))
                            && matches!(&window[1], MarkerEntry::Gap(_));
                        assert!(!has_adjacent_gaps, "Found adjacent gaps: {:?}", window);
                    }
                }
            }
        }
    }
}
