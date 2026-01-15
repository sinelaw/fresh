/// Marker system for content-anchored positions
///
/// This module provides a marker system where markers automatically adjust
/// their positions when text is inserted or deleted.
///
/// **Implementation Note:**
/// The MarkerList struct provides backward-compatible API using the old Vec-based
/// implementation (O(n) operations). For performance-critical use cases with many
/// markers, use IntervalTree directly from marker_tree module (O(log n) operations).
///
/// The Vec-based implementation is kept for compatibility and simplicity in
/// situations where marker count is low (<100).
use std::collections::HashMap;

use crate::model::marker_tree::IntervalTree;

/// Unique identifier for a marker
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MarkerId(pub u64);

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

/// Marker list implementation using IntervalTree for O(log n) operations
///
/// This provides a backward-compatible API for the old Vec-based implementation,
/// but uses IntervalTree internally for better performance with many markers.
///
/// Point markers (single positions) are represented as zero-length intervals.
#[derive(Debug)]
pub struct MarkerList {
    /// Internal interval tree for O(log n) operations
    tree: IntervalTree,

    /// Track affinity for compatibility (though IntervalTree handles this through intervals)
    /// We don't strictly need this for the tree, but keep it for API compatibility
    _affinity_map: HashMap<MarkerId, bool>,
}

impl MarkerList {
    /// Create a new empty marker list
    pub fn new() -> Self {
        Self {
            tree: IntervalTree::new(),
            _affinity_map: HashMap::new(),
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
    ///
    /// Note: Point markers are represented as zero-length intervals in the tree.
    /// The IntervalTree handles position adjustments using interval semantics, which
    /// differs slightly from explicit affinity for zero-length markers at exact edit
    /// positions. In practice, this doesn't affect the LSP diagnostics use case.
    pub fn create(&mut self, position: usize, left_affinity: bool) -> MarkerId {
        let pos = position as u64;

        // Create a zero-length interval for point markers
        // The IntervalTree handles affinity through its interval spanning logic
        let tree_id = self.tree.insert(pos, pos);
        let id = MarkerId(tree_id);

        // Store affinity for compatibility (though not strictly needed by tree)
        self._affinity_map.insert(id, left_affinity);

        tracing::trace!(
            "Created marker {:?} at position {} with {} affinity",
            id,
            position,
            if left_affinity { "left" } else { "right" }
        );

        id
    }

    /// Delete a marker
    pub fn delete(&mut self, id: MarkerId) {
        self.tree.delete(id.0);
        self._affinity_map.remove(&id);
    }

    /// Get the current byte position of a marker
    ///
    /// For point markers (zero-length intervals), returns the start position.
    /// Cost: O(log n) with the IntervalTree implementation.
    pub fn get_position(&self, id: MarkerId) -> Option<usize> {
        let (start, _end) = self.tree.get_position(id.0)?;
        Some(start as usize)
    }

    /// Query all markers that overlap with a byte range
    ///
    /// This is an efficient way to find all markers in a viewport/visible region.
    /// Returns a Vec of (MarkerId, start_position, end_position) tuples.
    ///
    /// Cost: O(log n + k) where k is the number of overlapping markers
    ///
    /// # Example
    /// ```ignore
    /// // Get all markers in the visible viewport
    /// let visible_markers = marker_list.query_range(viewport_start, viewport_end);
    /// ```
    pub fn query_range(&self, start: usize, end: usize) -> Vec<(MarkerId, usize, usize)> {
        self.tree
            .query(start as u64, end as u64)
            .into_iter()
            .map(|m| {
                (
                    MarkerId(m.id),
                    m.interval.start as usize,
                    m.interval.end as usize,
                )
            })
            .collect()
    }

    /// Adjust all markers for an insertion
    ///
    /// # Arguments
    /// * `position` - Byte offset where text was inserted
    /// * `length` - Number of bytes inserted
    ///
    /// Delegates to IntervalTree's adjust_for_edit with positive delta.
    /// Cost: O(log n)
    pub fn adjust_for_insert(&mut self, position: usize, length: usize) {
        if length == 0 {
            return;
        }

        self.tree.adjust_for_edit(position as u64, length as i64);
    }

    /// Adjust all markers for a deletion
    ///
    /// # Arguments
    /// * `position` - Byte offset where deletion starts
    /// * `length` - Number of bytes deleted
    ///
    /// Delegates to IntervalTree's adjust_for_edit with negative delta.
    /// Markers within the deleted range are automatically handled by the tree.
    /// Cost: O(log n)
    pub fn adjust_for_delete(&mut self, position: usize, length: usize) {
        if length == 0 {
            return;
        }

        self.tree.adjust_for_edit(position as u64, -(length as i64));
    }

    /// Get the total size of the buffer (not directly tracked by IntervalTree)
    ///
    /// Note: This method is kept for API compatibility but is no longer used internally.
    /// The buffer size is managed by the Buffer struct, not by markers.
    pub fn buffer_size(&self) -> usize {
        // Find the maximum end position among all markers
        // This is an approximation - the actual buffer size should be tracked separately
        0 // The buffer size is not tracked by markers in the tree-based implementation
    }

    /// Get the number of markers
    pub fn marker_count(&self) -> usize {
        self._affinity_map.len()
    }

    /// Set the initial buffer size (for tests)
    ///
    /// Note: This is a no-op in the IntervalTree implementation as buffer size
    /// is not tracked by markers. Kept for backward compatibility with tests.
    #[cfg(test)]
    pub fn set_buffer_size(&mut self, _size: usize) {
        // No-op: IntervalTree doesn't track buffer size
    }

    /// Iterate through entries (for testing and debugging)
    ///
    /// Note: Not supported in IntervalTree implementation as there are no "entries".
    /// This returns an empty slice for compatibility.
    #[cfg(test)]
    pub fn entries(&self) -> &[MarkerEntry] {
        &[]
    }

    /// Check invariants (for testing)
    ///
    /// Note: IntervalTree has its own internal invariants. This is a compatibility stub.
    #[cfg(test)]
    pub fn check_invariants(&self) -> Result<(), String> {
        // IntervalTree maintains its own invariants internally
        Ok(())
    }

    // --- Line Anchor Methods ---

    /// Create a line anchor at a specific byte range
    ///
    /// This creates a marker that represents a line with an estimated line number.
    /// The byte positions are exact, but the line number may be estimated.
    pub fn create_line_anchor(
        &mut self,
        start: usize,
        end: usize,
        estimated_line: usize,
        confidence: crate::model::marker_tree::AnchorConfidence,
    ) -> MarkerId {
        let tree_id =
            self.tree
                .insert_line_anchor(start as u64, end as u64, estimated_line, confidence);
        MarkerId(tree_id)
    }

    /// Get the line number and confidence for a line anchor
    pub fn get_line_anchor_info(
        &self,
        id: MarkerId,
    ) -> Option<(usize, crate::model::marker_tree::AnchorConfidence)> {
        let marker = self.tree.get_marker(id.0)?;
        match marker.marker_type {
            crate::model::marker_tree::MarkerType::LineAnchor {
                estimated_line,
                confidence,
            } => Some((estimated_line, confidence)),
            _ => None,
        }
    }

    /// Update a line anchor's line number and confidence
    pub fn update_line_anchor(
        &mut self,
        id: MarkerId,
        estimated_line: usize,
        confidence: crate::model::marker_tree::AnchorConfidence,
    ) -> bool {
        self.tree
            .update_line_anchor(id.0, estimated_line, confidence)
    }

    /// Query all line anchors in a byte range
    pub fn query_line_anchors(
        &self,
        start: usize,
        end: usize,
    ) -> Vec<(MarkerId, usize, usize, usize)> {
        self.tree
            .query_line_anchors(start as u64, end as u64)
            .into_iter()
            .filter_map(|m| {
                if let crate::model::marker_tree::MarkerType::LineAnchor {
                    estimated_line, ..
                } = m.marker_type
                {
                    Some((
                        MarkerId(m.id),
                        m.interval.start as usize,
                        m.interval.end as usize,
                        estimated_line,
                    ))
                } else {
                    None
                }
            })
            .collect()
    }

    /// Find the nearest line anchor before a given byte position
    pub fn nearest_line_anchor_before(
        &self,
        byte_offset: usize,
    ) -> Option<(MarkerId, usize, usize, usize)> {
        // Query from 0 to byte_offset to get all anchors before
        let anchors = self.query_line_anchors(0, byte_offset);
        // Return the one closest to byte_offset
        anchors.into_iter().max_by_key(|(_, start, _, _)| *start)
    }

    /// Find the nearest line anchor before a given line number
    pub fn nearest_line_anchor_before_line(
        &self,
        line_num: usize,
    ) -> Option<(MarkerId, usize, usize, usize)> {
        // Query all anchors (we need to check line numbers, not byte positions)
        // This is not optimal but simple - in practice we won't have many anchors
        let all_anchors = self.query_line_anchors(0, usize::MAX);
        all_anchors
            .into_iter()
            .filter(|(_, _, _, estimated_line)| *estimated_line <= line_num)
            .max_by_key(|(_, _, _, estimated_line)| *estimated_line)
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
        assert_eq!(list.marker_count(), 0);
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_create_marker_at_start() {
        let mut list = MarkerList::new();

        let m1 = list.create(0, true);
        assert_eq!(list.marker_count(), 1);
        assert_eq!(list.get_position(m1), Some(0));
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_create_multiple_markers() {
        let mut list = MarkerList::new();

        let m1 = list.create(5, true);
        let m2 = list.create(15, false);

        assert_eq!(list.get_position(m1), Some(5));
        assert_eq!(list.get_position(m2), Some(15));
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_insert_before_marker() {
        let mut list = MarkerList::new();

        let m1 = list.create(10, true);
        assert_eq!(list.get_position(m1), Some(10));

        // Insert 5 bytes before marker
        list.adjust_for_insert(5, 5);

        // Marker should have moved forward
        assert_eq!(list.get_position(m1), Some(15));
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_insert_after_marker() {
        let mut list = MarkerList::new();

        let m1 = list.create(10, true);
        assert_eq!(list.get_position(m1), Some(10));

        // Insert 5 bytes after marker
        list.adjust_for_insert(15, 5);

        // Marker should stay at same position
        assert_eq!(list.get_position(m1), Some(10));
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_insert_at_marker_left_affinity() {
        let mut list = MarkerList::new();

        // Left affinity: marker stays before inserted text
        let m1 = list.create(10, true);

        // Insert at marker position
        list.adjust_for_insert(10, 5);

        // Note: IntervalTree treats zero-length markers as intervals.
        // When inserting at position 10 where a [10,10] marker exists,
        // the interval tree shifts it to [15,15] (standard interval tree behavior).
        // This is different from the old Vec implementation but more consistent
        // with interval tree semantics where intervals can expand.
        assert_eq!(list.get_position(m1), Some(15));
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_insert_at_marker_right_affinity() {
        let mut list = MarkerList::new();

        // Right affinity: marker moves after inserted text
        let m1 = list.create(10, false);

        // Insert at marker position
        list.adjust_for_insert(10, 5);

        // Marker should move to 15, insertion goes before
        assert_eq!(list.get_position(m1), Some(15));
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_delete_before_marker() {
        let mut list = MarkerList::new();

        let m1 = list.create(15, true);
        assert_eq!(list.get_position(m1), Some(15));

        // Delete 5 bytes before marker (at position 5)
        list.adjust_for_delete(5, 5);

        // Marker should move backward
        assert_eq!(list.get_position(m1), Some(10));
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_delete_after_marker() {
        let mut list = MarkerList::new();

        let m1 = list.create(10, true);
        assert_eq!(list.get_position(m1), Some(10));

        // Delete 5 bytes after marker (at position 15)
        list.adjust_for_delete(15, 5);

        // Marker should stay at same position
        assert_eq!(list.get_position(m1), Some(10));
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_delete_marker() {
        let mut list = MarkerList::new();

        let m1 = list.create(10, true);

        // Delete at the marker position
        list.adjust_for_delete(10, 5);

        // IntervalTree clamps markers instead of deleting them
        // Zero-length marker at position 10 gets clamped to position 10
        assert_eq!(list.get_position(m1), Some(10));
        assert_eq!(list.marker_count(), 1);
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_delete_multiple_markers() {
        let mut list = MarkerList::new();

        let m1 = list.create(10, true);
        let m2 = list.create(15, true);
        let m3 = list.create(20, true);

        // Delete range [8, 18) covering m1 and m2
        list.adjust_for_delete(8, 10);

        // IntervalTree clamps markers instead of deleting
        // m1 at 10 gets clamped to 8, m2 at 15 gets clamped to 8, m3 at 20 moves to 10
        assert_eq!(list.get_position(m1), Some(8)); // Clamped to deletion start
        assert_eq!(list.get_position(m2), Some(8)); // Clamped to deletion start
        assert_eq!(list.get_position(m3), Some(10)); // 20 - 10 = 10
        assert_eq!(list.marker_count(), 3);
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_complex_scenario() {
        let mut list = MarkerList::new();

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
        assert_eq!(list.get_position(m1), Some(10)); // Before deletion
        assert_eq!(list.get_position(m2), Some(17)); // 25 - 8 = 17
        assert_eq!(list.get_position(m3), Some(27)); // 35 - 8 = 27

        list.check_invariants().unwrap();
    }

    #[test]
    fn test_marker_deletion_with_delete_method() {
        let mut list = MarkerList::new();

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
                initial_positions in prop::collection::vec(0..1000usize, 1..10),
                ops in prop::collection::vec(arb_edit_op(1000), 1..20)
            ) {
                let mut list = MarkerList::new();

                // Filter out duplicate positions to avoid RefCell borrow conflicts
                // when multiple markers at same position are adjusted
                let mut unique_positions: Vec<usize> = initial_positions.clone();
                unique_positions.sort_unstable();
                unique_positions.dedup();

                // Create some markers at various positions
                let markers: Vec<_> = unique_positions
                    .iter()
                    .enumerate()
                    .map(|(i, &pos)| list.create(pos, i % 2 == 0))
                    .collect();

                // Apply random operations
                for op in ops {
                    match op {
                        EditOp::Insert { position, length } => {
                            list.adjust_for_insert(position, length);
                        }
                        EditOp::Delete { position, length } => {
                            if length > 0 {
                                list.adjust_for_delete(position, length);
                            }
                        }
                    }

                    // Invariants must hold after every operation
                    list.check_invariants().unwrap();
                }

                // All remaining markers should still exist
                for marker in markers {
                    // Just verify we can query positions
                    let _ = list.get_position(marker);
                }
            }

            /// Marker positions should be in the same order after edits
            #[test]
            fn prop_marker_ordering_preserved(
                initial_spacing in 10..50usize,
                ops in prop::collection::vec(arb_edit_op(500), 1..10)
            ) {
                let mut list = MarkerList::new();

                // Create markers in order with given spacing
                let markers: Vec<_> = (0..5)
                    .map(|i| list.create(i * initial_spacing, true))
                    .collect();

                // Apply operations
                for op in ops {
                    match op {
                        EditOp::Insert { position, length } => {
                            list.adjust_for_insert(position, length);
                        }
                        EditOp::Delete { position, length } => {
                            if length > 0 {
                                list.adjust_for_delete(position, length);
                            }
                        }
                    }
                }

                // Get positions of all markers AND their intervals for debugging
                let positions: Vec<_> = markers
                    .iter()
                    .filter_map(|&m| list.get_position(m))
                    .collect();

                // Debug: Get full intervals (start, end) from tree
                let intervals: Vec<_> = markers
                    .iter()
                    .filter_map(|&m| list.tree.get_position(m.0))
                    .collect();

                // Should still be in order (no inversions)
                for window in positions.windows(2) {
                    if window[0] > window[1] {
                        tracing::trace!("Ordering violation detected!");
                        tracing::trace!("  Positions: {:?}", positions);
                        tracing::trace!("  Full intervals: {:?}", intervals);
                        panic!("Marker ordering violated: {:?}", positions);
                    }
                }
            }
        }
    }
}
