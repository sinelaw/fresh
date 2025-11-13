use std::sync::Arc;

/// Identifies which buffer a piece of text comes from
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BufferLocation {
    /// Data is in the original stored/persisted buffer
    Stored,
    /// Data is in the added/modified buffer
    Added,
}

/// A node in the piece tree (VSCode-style: only tracks bytes, not lines)
#[derive(Debug, Clone)]
pub enum PieceTreeNode {
    /// Internal node with left and right children
    Internal {
        left_bytes: usize,  // Total bytes in left subtree
        left: Arc<PieceTreeNode>,
        right: Arc<PieceTreeNode>,
    },
    /// Leaf node representing an actual piece
    Leaf {
        location: BufferLocation,  // Where this piece's data is
        offset: usize,             // Offset within the buffer
        bytes: usize,              // Number of bytes in this piece
    },
}

/// Information about a piece at a specific location
#[derive(Debug, Clone)]
pub struct PieceInfo {
    pub location: BufferLocation,       // Which buffer (Stored or Added)
    pub offset: usize,                  // Starting offset of this piece within that buffer
    pub bytes: usize,                   // Length of this piece in bytes
    pub offset_in_piece: Option<usize>, // For queries: how far into this piece the query point is
}

/// Result from finding a piece by byte offset
#[derive(Debug, Clone)]
struct OffsetFindResult {
    info: PieceInfo,
    bytes_before: usize,  // Total bytes in all pieces before this one
}

/// A cursor position in the document
#[derive(Debug, Clone)]
pub struct Cursor {
    pub byte_offset: usize,  // Absolute byte offset in document
    pub line: usize,         // Line number (0-indexed)
    pub col: usize,          // Column within line (byte offset)
}


// Line iteration can be implemented by:
// 1. Maintaining a cursor position (current piece + offset within piece)
// 2. For next_line(): scan forward in the current piece's buffer until '\n',
//    or move to the next piece if we reach the end
// 3. For prev_line(): scan backward similarly
// The iterator would need access to the actual buffer data (Stored/Added)
// which is managed externally, so this is deferred until buffer integration.

impl PieceTreeNode {
    /// Find the piece containing the given byte offset
    fn find_by_offset(&self, offset: usize) -> Option<OffsetFindResult> {
        match self {
            PieceTreeNode::Internal {
                left_bytes,
                left,
                right,
            } => {
                if offset < *left_bytes {
                    left.find_by_offset(offset)
                } else {
                    // Search in right subtree
                    right.find_by_offset(offset - left_bytes).map(|mut result| {
                        // Adjust bytes_before to account for left subtree
                        result.bytes_before += left_bytes;
                        result
                    })
                }
            }
            PieceTreeNode::Leaf {
                location,
                offset: piece_offset,
                bytes,
            } => {
                if offset < *bytes {
                    Some(OffsetFindResult {
                        info: PieceInfo {
                            location: *location,
                            offset: *piece_offset,
                            bytes: *bytes,
                            offset_in_piece: Some(offset),
                        },
                        bytes_before: 0,
                    })
                } else {
                    None
                }
            }
        }
    }

    /// Get total bytes in this node
    fn total_bytes(&self) -> usize {
        match self {
            PieceTreeNode::Internal { left_bytes, right, .. } => {
                left_bytes + right.total_bytes()
            }
            PieceTreeNode::Leaf { bytes, .. } => *bytes,
        }
    }

    /// Get the depth of this tree
    fn depth(&self) -> usize {
        match self {
            PieceTreeNode::Internal { left, right, .. } => {
                1 + left.depth().max(right.depth())
            }
            PieceTreeNode::Leaf { .. } => 1,
        }
    }

    /// Count the number of leaf nodes
    fn count_leaves(&self) -> usize {
        match self {
            PieceTreeNode::Internal { left, right, .. } => {
                left.count_leaves() + right.count_leaves()
            }
            PieceTreeNode::Leaf { .. } => 1,
        }
    }

    /// Collect all leaves in order
    fn collect_leaves(&self, leaves: &mut Vec<(BufferLocation, usize, usize)>) {
        match self {
            PieceTreeNode::Internal { left, right, .. } => {
                left.collect_leaves(leaves);
                right.collect_leaves(leaves);
            }
            PieceTreeNode::Leaf {
                location,
                offset,
                bytes,
            } => {
                leaves.push((*location, *offset, *bytes));
            }
        }
    }
}

/// The main piece table structure (VSCode-style: only tracks bytes)
pub struct PieceTree {
    root: Arc<PieceTreeNode>,
    total_bytes: usize,
}

impl PieceTree {
    /// Create a new piece table with a single initial piece
    pub fn new(location: BufferLocation, offset: usize, bytes: usize) -> Self {
        PieceTree {
            root: Arc::new(PieceTreeNode::Leaf {
                location,
                offset,
                bytes,
            }),
            total_bytes: bytes,
        }
    }

    /// Create an empty piece table
    pub fn empty() -> Self {
        PieceTree {
            root: Arc::new(PieceTreeNode::Leaf {
                location: BufferLocation::Stored,
                offset: 0,
                bytes: 0,
            }),
            total_bytes: 0,
        }
    }

    /// Build a balanced tree from a list of leaves
    fn build_balanced(leaves: &[(BufferLocation, usize, usize)]) -> Arc<PieceTreeNode> {
        if leaves.is_empty() {
            return Arc::new(PieceTreeNode::Leaf {
                location: BufferLocation::Stored,
                offset: 0,
                bytes: 0,
            });
        }

        if leaves.len() == 1 {
            let (location, offset, bytes) = leaves[0];
            return Arc::new(PieceTreeNode::Leaf {
                location,
                offset,
                bytes,
            });
        }

        // Split in the middle
        let mid = leaves.len() / 2;
        let left = Self::build_balanced(&leaves[..mid]);
        let right = Self::build_balanced(&leaves[mid..]);

        let left_bytes = left.total_bytes();

        Arc::new(PieceTreeNode::Internal {
            left_bytes,
            left,
            right,
        })
    }

    /// Rebuild the tree to be balanced
    fn rebalance(&mut self) {
        let mut leaves = Vec::new();
        self.root.collect_leaves(&mut leaves);
        self.root = Self::build_balanced(&leaves);
    }

    /// Check if rebalancing is needed and do it
    fn check_and_rebalance(&mut self) {
        let count = self.root.count_leaves();
        if count < 2 {
            return;
        }

        let depth = self.root.depth();
        let max_depth = 2 * (count as f64).log2().ceil() as usize;

        if depth > max_depth {
            self.rebalance();
        }
    }

    /// Find the piece at the given byte offset
    pub fn find_by_offset(&self, offset: usize) -> Option<PieceInfo> {
        if offset >= self.total_bytes {
            return None;
        }
        self.root.find_by_offset(offset).map(|result| result.info)
    }

    /// Create a cursor at the given byte offset
    /// Note: line/col calculation should be done by LineIndex
    pub fn cursor_at_offset(&self, offset: usize) -> Cursor {
        Cursor {
            byte_offset: offset.min(self.total_bytes),
            line: 0,
            col: 0,
        }
    }

    /// Insert text at the given offset
    /// Returns new cursor after the inserted text
    pub fn insert(
        &mut self,
        offset: usize,
        location: BufferLocation,
        buffer_offset: usize,
        bytes: usize,
    ) -> Cursor {
        if bytes == 0 {
            return self.cursor_at_offset(offset);
        }

        // Find the piece to split
        if let Some(_result) = self.root.find_by_offset(offset) {
            // Split the piece at the insertion point
            let mut leaves = Vec::new();
            self.collect_leaves_with_split(
                &self.root,
                0,
                offset,
                Some((location, buffer_offset, bytes)),
                &mut leaves,
            );

            self.root = Self::build_balanced(&leaves);
            self.total_bytes += bytes;

            self.check_and_rebalance();
        } else if offset == self.total_bytes {
            // Append at end
            let mut leaves = Vec::new();
            self.root.collect_leaves(&mut leaves);
            leaves.push((location, buffer_offset, bytes));

            self.root = Self::build_balanced(&leaves);
            self.total_bytes += bytes;

            self.check_and_rebalance();
        }

        self.cursor_at_offset(offset + bytes)
    }

    /// Helper to collect leaves while splitting at insertion point
    fn collect_leaves_with_split(
        &self,
        node: &Arc<PieceTreeNode>,
        current_offset: usize,
        split_offset: usize,
        insert: Option<(BufferLocation, usize, usize)>,
        leaves: &mut Vec<(BufferLocation, usize, usize)>,
    ) {
        match node.as_ref() {
            PieceTreeNode::Internal {
                left_bytes,
                left,
                right,
            } => {
                self.collect_leaves_with_split(left, current_offset, split_offset, insert, leaves);
                self.collect_leaves_with_split(
                    right,
                    current_offset + left_bytes,
                    split_offset,
                    insert,
                    leaves,
                );
            }
            PieceTreeNode::Leaf {
                location,
                offset,
                bytes,
            } => {
                let piece_end = current_offset + bytes;

                if split_offset > current_offset && split_offset < piece_end {
                    // Split this piece
                    let offset_in_piece = split_offset - current_offset;

                    // First part (before split)
                    if offset_in_piece > 0 {
                        leaves.push((
                            *location,
                            *offset,
                            offset_in_piece,
                        ));
                    }

                    // Inserted piece
                    if let Some((ins_loc, ins_off, ins_bytes)) = insert {
                        leaves.push((ins_loc, ins_off, ins_bytes));
                    }

                    // Second part (after split)
                    let remaining = bytes - offset_in_piece;
                    if remaining > 0 {
                        leaves.push((
                            *location,
                            offset + offset_in_piece,
                            remaining,
                        ));
                    }
                } else if split_offset == current_offset {
                    // Insert before this piece
                    if let Some((ins_loc, ins_off, ins_bytes)) = insert {
                        leaves.push((ins_loc, ins_off, ins_bytes));
                    }
                    leaves.push((*location, *offset, *bytes));
                } else {
                    // Don't split, just add the piece
                    leaves.push((*location, *offset, *bytes));
                }
            }
        }
    }

    /// Delete text starting at offset for the given number of bytes
    pub fn delete(&mut self, offset: usize, delete_bytes: usize) {
        if delete_bytes == 0 || offset >= self.total_bytes {
            return;
        }

        let delete_bytes = delete_bytes.min(self.total_bytes - offset);
        let end_offset = offset + delete_bytes;

        let mut leaves = Vec::new();
        self.collect_leaves_with_delete(&self.root, 0, offset, end_offset, &mut leaves);

        self.root = Self::build_balanced(&leaves);
        self.total_bytes -= delete_bytes;

        self.check_and_rebalance();
    }

    /// Helper to collect leaves while deleting a range
    fn collect_leaves_with_delete(
        &self,
        node: &Arc<PieceTreeNode>,
        current_offset: usize,
        delete_start: usize,
        delete_end: usize,
        leaves: &mut Vec<(BufferLocation, usize, usize)>,
    ) {
        match node.as_ref() {
            PieceTreeNode::Internal {
                left_bytes,
                left,
                right,
            } => {
                self.collect_leaves_with_delete(left, current_offset, delete_start, delete_end, leaves);
                self.collect_leaves_with_delete(
                    right,
                    current_offset + left_bytes,
                    delete_start,
                    delete_end,
                    leaves,
                );
            }
            PieceTreeNode::Leaf {
                location,
                offset,
                bytes,
            } => {
                let piece_start = current_offset;
                let piece_end = current_offset + bytes;

                // Piece completely before delete range
                if piece_end <= delete_start {
                    leaves.push((*location, *offset, *bytes));
                    return;
                }

                // Piece completely after delete range
                if piece_start >= delete_end {
                    leaves.push((*location, *offset, *bytes));
                    return;
                }

                // Piece partially or fully overlaps delete range
                // Keep part before delete range
                if piece_start < delete_start {
                    let keep_bytes = delete_start - piece_start;
                    leaves.push((
                        *location,
                        *offset,
                        keep_bytes,
                    ));
                }

                // Keep part after delete range
                if piece_end > delete_end {
                    let skip_bytes = delete_end - piece_start;
                    let keep_bytes = piece_end - delete_end;
                    leaves.push((
                        *location,
                        offset + skip_bytes,
                        keep_bytes,
                    ));
                }
            }
        }
    }

    /// Get the total number of bytes in the document
    pub fn total_bytes(&self) -> usize {
        self.total_bytes
    }

    /// Get tree statistics for debugging
    pub fn stats(&self) -> (usize, usize, usize) {
        let depth = self.root.depth();
        let leaves = self.root.count_leaves();
        (self.total_bytes, depth, leaves)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_empty() {
        let tree = PieceTree::empty();
        assert_eq!(tree.total_bytes(), 0);
    }

    #[test]
    fn test_create_with_initial_piece() {
        let tree = PieceTree::new(BufferLocation::Stored, 0, 100);
        assert_eq!(tree.total_bytes(), 100);
    }

    #[test]
    fn test_insert_at_end() {
        let mut tree = PieceTree::new(BufferLocation::Stored, 0, 100);
        tree.insert(100, BufferLocation::Added, 0, 50);
        assert_eq!(tree.total_bytes(), 150);
    }

    #[test]
    fn test_insert_in_middle() {
        let mut tree = PieceTree::new(BufferLocation::Stored, 0, 100);
        tree.insert(50, BufferLocation::Added, 0, 25);
        assert_eq!(tree.total_bytes(), 125);
        let (_, _, leaves) = tree.stats();
        assert_eq!(leaves, 3); // Original piece split + new piece
    }

    #[test]
    fn test_delete() {
        let mut tree = PieceTree::new(BufferLocation::Stored, 0, 100);
        tree.delete(25, 50);
        assert_eq!(tree.total_bytes(), 50);
    }

    #[test]
    fn test_delete_at_boundaries() {
        let mut tree = PieceTree::new(BufferLocation::Stored, 0, 100);

        // Delete from start
        tree.delete(0, 10);
        assert_eq!(tree.total_bytes(), 90);

        // Delete from end
        tree.delete(80, 10);
        assert_eq!(tree.total_bytes(), 80);
    }

    #[test]
    fn test_multiple_inserts_and_deletes() {
        let mut tree = PieceTree::new(BufferLocation::Stored, 0, 100);

        tree.insert(50, BufferLocation::Added, 0, 20);
        assert_eq!(tree.total_bytes(), 120);

        tree.delete(40, 30);
        assert_eq!(tree.total_bytes(), 90);

        tree.insert(0, BufferLocation::Added, 0, 10);
        assert_eq!(tree.total_bytes(), 100);
    }

    #[test]
    fn test_rebalancing_many_inserts() {
        let mut tree = PieceTree::new(BufferLocation::Stored, 0, 100);

        // Insert many times, which could create unbalanced tree
        for i in 0..20 {
            tree.insert(i * 5, BufferLocation::Added, 0, 1);
        }

        let (bytes, depth, leaves) = tree.stats();
        assert_eq!(bytes, 120);
        // Each insert splits pieces, so we expect many leaves
        // Exact count depends on implementation details, but should be > 20
        assert!(leaves > 20);
        assert!(leaves < 50); // Reasonable upper bound

        // Depth should be reasonable due to rebalancing
        let max_expected_depth = 2 * (leaves as f64).log2().ceil() as usize;
        assert!(depth <= max_expected_depth + 2, "Tree depth {} exceeds max {} for {} leaves", depth, max_expected_depth, leaves);
    }

    #[test]
    fn test_find_by_offset() {
        let tree = PieceTree::new(BufferLocation::Stored, 0, 100);

        let info = tree.find_by_offset(50).unwrap();
        assert_eq!(info.location, BufferLocation::Stored);
        assert_eq!(info.offset_in_piece, Some(50));

        // Out of bounds
        assert!(tree.find_by_offset(100).is_none());
    }

    #[test]
    fn test_find_after_inserts() {
        let mut tree = PieceTree::new(BufferLocation::Stored, 0, 100);
        tree.insert(50, BufferLocation::Added, 0, 25);

        // Should find in added section
        let info = tree.find_by_offset(50).unwrap();
        assert_eq!(info.location, BufferLocation::Added);
    }
}

#[cfg(test)]
mod property_tests {
    use super::*;
    use proptest::prelude::*;

    // Strategy to generate operations
    #[derive(Debug, Clone)]
    enum Operation {
        Insert { offset: usize, bytes: usize },
        Delete { offset: usize, bytes: usize },
    }

    // Generate a sequence of operations
    fn operation_strategy() -> impl Strategy<Value = Vec<Operation>> {
        prop::collection::vec(
            prop_oneof![
                (0usize..200, 1usize..50).prop_map(|(offset, bytes)| {
                    Operation::Insert { offset, bytes }
                }),
                (0usize..200, 1usize..50).prop_map(|(offset, bytes)| {
                    Operation::Delete { offset, bytes }
                }),
            ],
            0..50,
        )
    }

    proptest! {
        #[test]
        fn prop_total_bytes_consistency(operations in operation_strategy()) {
            let mut tree = PieceTree::new(BufferLocation::Stored, 0, 100);
            let mut expected_bytes = 100;

            for op in operations {
                match op {
                    Operation::Insert { offset, bytes } => {
                        let offset = offset.min(tree.total_bytes());
                        tree.insert(offset, BufferLocation::Added, 0, bytes);
                        expected_bytes += bytes;
                    }
                    Operation::Delete { offset, bytes } => {
                        if offset < tree.total_bytes() {
                            let actual_delete = bytes.min(tree.total_bytes() - offset);
                            tree.delete(offset, bytes);
                            expected_bytes -= actual_delete;
                        }
                    }
                }
            }

            prop_assert_eq!(tree.total_bytes(), expected_bytes);
        }

        #[test]
        fn prop_tree_never_negative_bytes(operations in operation_strategy()) {
            let mut tree = PieceTree::new(BufferLocation::Stored, 0, 100);

            for op in operations {
                match op {
                    Operation::Insert { offset, bytes } => {
                        let offset = offset.min(tree.total_bytes());
                        tree.insert(offset, BufferLocation::Added, 0, bytes);
                    }
                    Operation::Delete { offset, bytes } => {
                        tree.delete(offset, bytes);
                    }
                }

                // Tree should never have negative bytes (underflow would wrap to large number)
                prop_assert!(tree.total_bytes() < 10_000_000);
            }
        }

        #[test]
        fn prop_balanced_after_operations(operations in operation_strategy()) {
            let mut tree = PieceTree::new(BufferLocation::Stored, 0, 100);

            for op in operations {
                match op {
                    Operation::Insert { offset, bytes } => {
                        let offset = offset.min(tree.total_bytes());
                        tree.insert(offset, BufferLocation::Added, 0, bytes);
                    }
                    Operation::Delete { offset, bytes } => {
                        tree.delete(offset, bytes);
                    }
                }
            }

            let (_, depth, leaves) = tree.stats();
            if leaves > 1 {
                let max_depth = 2 * (leaves as f64).log2().ceil() as usize;
                prop_assert!(depth <= max_depth + 2, "Tree depth {} exceeds expected max {} for {} leaves", depth, max_depth, leaves);
            }
        }

        #[test]
        fn prop_insert_then_delete_equals_original(
            insert_offset in 0usize..100,
            insert_bytes in 1usize..50
        ) {
            let mut tree = PieceTree::new(BufferLocation::Stored, 0, 100);
            let original_bytes = tree.total_bytes();

            let insert_offset = insert_offset.min(tree.total_bytes());
            tree.insert(insert_offset, BufferLocation::Added, 0, insert_bytes);

            // Delete what we just inserted
            tree.delete(insert_offset, insert_bytes);

            prop_assert_eq!(tree.total_bytes(), original_bytes);
        }

        #[test]
        fn prop_find_offset_in_bounds(
            offset in 0usize..100
        ) {
            let tree = PieceTree::new(BufferLocation::Stored, 0, 100);

            let result = tree.find_by_offset(offset);
            prop_assert!(result.is_some());
        }

        #[test]
        fn prop_find_offset_out_of_bounds(
            offset in 100usize..1000
        ) {
            let tree = PieceTree::new(BufferLocation::Stored, 0, 100);

            let result = tree.find_by_offset(offset);
            prop_assert!(result.is_none());
        }

        #[test]
        fn prop_sequential_inserts_maintain_order(
            count in 1usize..20,
            insert_size in 1usize..10
        ) {
            let mut tree = PieceTree::new(BufferLocation::Stored, 0, 10);

            for _i in 0..count {
                tree.insert(tree.total_bytes(), BufferLocation::Added, 0, insert_size);
            }

            let expected_bytes = 10 + (count * insert_size);
            prop_assert_eq!(tree.total_bytes(), expected_bytes);
        }

        #[test]
        fn prop_delete_all_reaches_zero(
            delete_size in 1usize..10
        ) {
            let mut tree = PieceTree::new(BufferLocation::Stored, 0, 100);

            while tree.total_bytes() > 0 {
                let to_delete = delete_size.min(tree.total_bytes());
                tree.delete(0, to_delete);
            }

            prop_assert_eq!(tree.total_bytes(), 0);
        }
    }

    #[test]
    fn test_empty_delete() {
        let mut tree = PieceTree::new(BufferLocation::Stored, 0, 100);
        tree.delete(50, 0);
        assert_eq!(tree.total_bytes(), 100);
    }

    #[test]
    fn test_delete_beyond_end() {
        let mut tree = PieceTree::new(BufferLocation::Stored, 0, 100);
        tree.delete(50, 100); // Try to delete 100 bytes from offset 50
        assert_eq!(tree.total_bytes(), 50); // Should only delete 50 bytes
    }

    #[test]
    fn test_insert_zero_bytes() {
        let mut tree = PieceTree::new(BufferLocation::Stored, 0, 100);
        tree.insert(50, BufferLocation::Added, 0, 0);
        assert_eq!(tree.total_bytes(), 100);
    }
}
