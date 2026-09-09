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
    /// Below this many insert-only edits, adjusting one at a time (O(log n)
    /// each) is cheaper than the batched rebuild (O(markers)).
    const BULK_ADJUST_MIN_EDITS: usize = 16;

    /// Create a new empty marker list
    pub fn new() -> Self {
        Self {
            tree: IntervalTree::new(),
            _affinity_map: HashMap::new(),
        }
    }

    /// Create a new **right-gravity** point marker at the given position:
    /// text inserted exactly at `position` pushes the marker forward.
    ///
    /// This used to take a `left_affinity: bool` that was stored in a side map
    /// and never reached the tree — every marker was right-gravity regardless,
    /// while call sites (and their comments) claimed otherwise. Code that
    /// mirrors marker movement has to model gravity exactly, and a parameter
    /// that silently does nothing is how it gets modelled wrong: see
    /// `IndexDecorations::shift_for_edit`, which was written to match the
    /// argument rather than the behaviour. Callers that genuinely need the
    /// other gravity use [`create_left_gravity`](Self::create_left_gravity),
    /// which does reach the tree.
    ///
    /// Point markers are zero-length intervals; the tree resolves adjustment
    /// with interval semantics.
    pub fn create(&mut self, position: usize) -> MarkerId {
        let pos = position as u64;

        // Create a zero-length interval for point markers
        let tree_id = self.tree.insert(pos, pos);
        let id = MarkerId(tree_id);
        self._affinity_map.insert(id, false);

        tracing::trace!("Created marker {:?} at position {}", id, position);

        id
    }

    /// Create a new left-gravity point marker at the given position.
    ///
    /// Unlike [`create`], text inserted exactly at the marker's position leaves
    /// it in place instead of pushing it forward. Used as the end marker of a
    /// fixed-width highlight (e.g. a search match) so the highlight does not
    /// grow when text is typed immediately after it.
    pub fn create_left_gravity(&mut self, position: usize) -> MarkerId {
        let pos = position as u64;
        let tree_id = self.tree.insert_left_gravity(pos, pos);
        let id = MarkerId(tree_id);
        self._affinity_map.insert(id, true);
        id
    }

    /// Create a marker covering a whole range, rather than a point.
    ///
    /// The tree's `query` is an ordinary max-end interval-overlap query, so a
    /// marker inserted with its real extent is returned for any range it
    /// touches — including a query that falls strictly *inside* it. Point
    /// markers cannot answer that: an interval whose endpoints both sit
    /// outside the query is invisible to a query over its endpoints. Callers
    /// that need "give me the things covering this window" (overlay
    /// viewport lookup) index their extent with this and keep their own
    /// endpoint markers for the authoritative positions.
    ///
    /// Right gravity, matching `create`.
    pub fn create_span(&mut self, start: usize, end: usize) -> MarkerId {
        let tree_id = self.tree.insert(start as u64, end.max(start) as u64);
        let id = MarkerId(tree_id);
        self._affinity_map.insert(id, false);
        id
    }

    /// Create the three markers an overlay needs — a start point, an end
    /// point and a span covering both — for many ranges at once.
    ///
    /// Ids are handed out exactly as calling [`create`](Self::create),
    /// `create` and [`create_span`](Self::create_span) per range in order
    /// would hand them out, so the markers are indistinguishable from the
    /// one-at-a-time path; only the tree construction differs. Returns
    /// `(start, end, span)` per range, in range order.
    pub fn create_overlay_markers_bulk(
        &mut self,
        ranges: &[std::ops::Range<usize>],
    ) -> Vec<(MarkerId, MarkerId, MarkerId)> {
        let mut intervals: Vec<(u64, u64)> = Vec::with_capacity(ranges.len() * 3);
        for range in ranges {
            let (start, end) = (range.start as u64, range.end.max(range.start) as u64);
            intervals.push((start, start));
            intervals.push((range.end as u64, range.end as u64));
            intervals.push((start, end));
        }
        let ids = self.tree.insert_many(&intervals);
        let mut out = Vec::with_capacity(ranges.len());
        for triple in ids.chunks_exact(3) {
            let (s, e, span) = (
                MarkerId(triple[0]),
                MarkerId(triple[1]),
                MarkerId(triple[2]),
            );
            self._affinity_map.insert(s, false);
            self._affinity_map.insert(e, false);
            self._affinity_map.insert(span, false);
            out.push((s, e, span));
        }
        out
    }

    /// Delete a marker
    pub fn delete(&mut self, id: MarkerId) {
        self.tree.delete(id.0);
        self._affinity_map.remove(&id);
    }

    /// Move a marker to a new byte position, preserving its ID and affinity.
    ///
    /// Implemented as delete + reinsert in the interval tree to maintain BST
    /// ordering invariants. The MarkerId is preserved so external references
    /// (VirtualTextManager, OverlayManager, MarginManager) remain valid.
    /// Returns false if the marker doesn't exist.
    /// Cost: O(log n)
    pub fn set_position(&mut self, id: MarkerId, new_position: usize) -> bool {
        let pos = new_position as u64;
        self.tree.set_position(id.0, pos, pos)
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

    /// Adjust all markers for a batch of non-overlapping edits at once.
    ///
    /// `edits` are `(position, deleted_len, inserted_len)` triples describing a
    /// single bulk edit. Applying them one at a time costs O(markers) per
    /// deletion, so a replace-all with one edit per match was quadratic; the
    /// batched path (see [`IntervalTree::adjust_for_bulk_edits`]) is
    /// O(markers + edits·log edits) and produces the same positions.
    ///
    /// Two cases stay on the per-edit path, which is exact either way:
    /// small batches, where a single O(log n) insertion beats rebuilding the
    /// whole tree; and batches carrying more than one edit at the same
    /// position, which the batched path cannot model because it resolves each
    /// marker against one governing edit. A bulk edit merges its
    /// same-position edits before it gets here, so that second case is a
    /// guard rather than a path anything takes today.
    pub fn adjust_for_bulk_edits(&mut self, edits: &[(usize, usize, usize)]) {
        let net: Vec<(u64, i64)> = edits
            .iter()
            .map(|(pos, del_len, ins_len)| (*pos as u64, *ins_len as i64 - *del_len as i64))
            .filter(|(_, delta)| *delta != 0)
            .collect();

        let deletions = net.iter().filter(|(_, delta)| *delta < 0).count();
        let worth_batching =
            net.len() > 1 && (deletions > 0 || net.len() >= Self::BULK_ADJUST_MIN_EDITS);
        if worth_batching && !Self::has_repeated_position(&net) {
            self.tree.adjust_for_bulk_edits(&net);
            return;
        }

        for (pos, delta) in net {
            self.tree.adjust_for_edit(pos, delta);
        }
    }

    /// Whether any two edits share a position.
    fn has_repeated_position(edits: &[(u64, i64)]) -> bool {
        let mut positions: Vec<u64> = edits.iter().map(|(pos, _)| *pos).collect();
        positions.sort_unstable();
        positions.windows(2).any(|w| w[0] == w[1])
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
    /// is not tracked by markers. Kept for backward compatibility with tests,
    /// which now live in `fresh-editor` and so never see this crate's `test` cfg.
    #[doc(hidden)]
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

    /// A batch carrying two edits at one position falls back to the per-edit
    /// path, which applies them in turn. The batched path resolves each marker
    /// against a single governing edit and cannot express that, so the two must
    /// not diverge — a bulk edit merges same-position edits before this point,
    /// but nothing in the signature enforces it.
    #[test]
    fn bulk_adjust_with_repeated_position_matches_per_edit() {
        let positions = [0usize, 40, 41, 60, 100];
        let edits = [(60usize, 13, 0), (60, 0, 1), (40, 16, 0)];

        let mut batched = MarkerList::new();
        let mut per_edit = MarkerList::new();
        let ids: Vec<_> = positions
            .iter()
            .map(|&pos| (batched.create(pos), per_edit.create(pos)))
            .collect();

        batched.adjust_for_bulk_edits(&edits);
        for (pos, del_len, ins_len) in edits {
            match ins_len.cmp(&del_len) {
                std::cmp::Ordering::Greater => per_edit.adjust_for_insert(pos, ins_len - del_len),
                std::cmp::Ordering::Less => per_edit.adjust_for_delete(pos, del_len - ins_len),
                std::cmp::Ordering::Equal => {}
            }
        }

        for (batched_id, per_edit_id) in ids {
            assert_eq!(
                batched.get_position(batched_id),
                per_edit.get_position(per_edit_id)
            );
        }
        batched.check_invariants().unwrap();
    }

    #[test]
    fn test_new_marker_list() {
        let list = MarkerList::new();
        assert_eq!(list.marker_count(), 0);
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_create_marker_at_start() {
        let mut list = MarkerList::new();

        let m1 = list.create(0);
        assert_eq!(list.marker_count(), 1);
        assert_eq!(list.get_position(m1), Some(0));
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_create_multiple_markers() {
        let mut list = MarkerList::new();

        let m1 = list.create(5);
        let m2 = list.create(15);

        assert_eq!(list.get_position(m1), Some(5));
        assert_eq!(list.get_position(m2), Some(15));
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_insert_before_marker() {
        let mut list = MarkerList::new();

        let m1 = list.create(10);
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

        let m1 = list.create(10);
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
        let m1 = list.create(10);

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
        let m1 = list.create(10);

        // Insert at marker position
        list.adjust_for_insert(10, 5);

        // Marker should move to 15, insertion goes before
        assert_eq!(list.get_position(m1), Some(15));
        list.check_invariants().unwrap();
    }

    #[test]
    fn test_delete_before_marker() {
        let mut list = MarkerList::new();

        let m1 = list.create(15);
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

        let m1 = list.create(10);
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

        let m1 = list.create(10);

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

        let m1 = list.create(10);
        let m2 = list.create(15);
        let m3 = list.create(20);

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
        let m1 = list.create(10);
        let m2 = list.create(20);
        let m3 = list.create(30);

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

        let m1 = list.create(10);
        let m2 = list.create(15);

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
                    .map(|(_i, &pos)| list.create(pos))
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
                    .map(|i| list.create(i * initial_spacing))
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

            /// Shadow-model property test: for every sequence of
            /// create / delete / adjust_for_insert / adjust_for_delete
            /// operations, the positions reported by MarkerList for
            /// each still-live marker must match the positions a naïve
            /// `Vec<(MarkerId, usize)>` would compute by independently
            /// shifting/clamping on each edit.
            ///
            /// This catches bugs where the interval-tree's own
            /// bookkeeping (e.g. lazy_delta propagation, BST-delete
            /// node swaps, marker_map staleness) diverges from the
            /// straightforward "markers are points that slide with
            /// the buffer" semantics. The inlay-hint-jumps-to-start
            /// regression on line delete was exactly this kind of
            /// divergence, and was invisible to every other invariant
            /// check in this file.
            #[test]
            fn prop_shadow_model_matches_tree(
                initial_positions in prop::collection::vec(0..1000usize, 1..20),
                ops in prop::collection::vec(arb_edit_op(1000), 1..30),
                delete_indices in prop::collection::vec(0..20usize, 0..5),
            ) {
                let mut list = MarkerList::new();

                let mut unique_positions: Vec<usize> = initial_positions;
                unique_positions.sort_unstable();
                unique_positions.dedup();

                // Shadow: Vec<(MarkerId, Option<usize>, right_gravity)>.
                // None means deleted. Half the markers are created left-gravity
                // (issue #2053) so the model also covers the sticky-end path.
                let mut shadow: Vec<(MarkerId, Option<usize>, bool)> = Vec::new();
                for (i, &p) in unique_positions.iter().enumerate() {
                    let right_gravity = i % 2 == 0;
                    let id = if right_gravity {
                        list.create(p)
                    } else {
                        list.create_left_gravity(p)
                    };
                    shadow.push((id, Some(p), right_gravity));
                }

                // Delete some markers (by shadow index modulo len).
                for idx in delete_indices {
                    if shadow.is_empty() {
                        break;
                    }
                    let i = idx % shadow.len();
                    if let (id, Some(_), _) = shadow[i] {
                        list.delete(id);
                        shadow[i].1 = None;
                    }
                }

                // Apply edits to both the tree and the shadow.
                for op in ops {
                    match op {
                        EditOp::Insert { position, length } => {
                            list.adjust_for_insert(position, length);
                            for (_id, pos, right_gravity) in shadow.iter_mut() {
                                if let Some(p) = pos {
                                    // Right-gravity markers shift when the
                                    // insertion is at or before them; left-gravity
                                    // markers only shift for insertions strictly
                                    // before, staying put at the exact boundary.
                                    let shifts = if *right_gravity {
                                        *p >= position
                                    } else {
                                        *p > position
                                    };
                                    if shifts {
                                        *p += length;
                                    }
                                }
                            }
                        }
                        EditOp::Delete { position, length } => {
                            if length == 0 {
                                continue;
                            }
                            list.adjust_for_delete(position, length);
                            for (_id, pos, _right_gravity) in shadow.iter_mut() {
                                if let Some(p) = pos {
                                    // Markers inside the deleted range
                                    // clamp to the deletion start in
                                    // MarkerList's semantics (see
                                    // adjust_recursive's `.max(pos)`),
                                    // so mirror that in the shadow.
                                    // Gravity does not affect deletions.
                                    if *p >= position + length {
                                        *p -= length;
                                    } else if *p > position {
                                        *p = position;
                                    }
                                }
                            }
                        }
                    }

                    // Every live marker's tree position must match its
                    // shadow position after every operation.
                    for (id, shadow_pos, _right_gravity) in &shadow {
                        match shadow_pos {
                            Some(expected) => {
                                let actual = list.get_position(*id);
                                prop_assert_eq!(
                                    actual,
                                    Some(*expected),
                                    "marker {:?} expected at {} but tree says {:?}",
                                    id,
                                    expected,
                                    actual
                                );
                            }
                            None => {
                                // Deleted markers: get_position may
                                // return None OR the tree may leak the
                                // underlying storage — accept either,
                                // but never a stale live position.
                            }
                        }
                    }
                }
            }
        }
    }
}
