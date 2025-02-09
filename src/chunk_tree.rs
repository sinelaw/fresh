//! A rope-like data structure implemented as a ternary tree for efficient text manipulation.
//!
//! # Overview
//! `ChunkTree` is an immutable, persistent data structure that represents text as a tree of chunks,
//! allowing for efficient insert and remove operations. It maintains data in a ternary tree structure
//! where each internal node has three children (left, middle, and right), and leaf nodes contain
//! the actual data. Being persistent means that operations create new versions of the tree while
//! preserving the original, making it suitable for scenarios requiring history or undo functionality.
//!
//! # Type Parameters
//! - `'a`: Lifetime parameter for the stored data
//! - `N`: Const generic parameter that defines the maximum size of leaf chunks
//!
//! # Examples
//! ```
//! use chunk_tree::ChunkTree;
//!
//! let tree = ChunkTree::<2>::new();
//! let tree = tree.insert(0, b"Hello");      // Creates a new tree, original remains unchanged
//! let tree = tree.insert(5, b" World!");    // Creates another new version
//! assert_eq!(tree.collect_bytes(), b"Hello World!");
//!
//! // Remove some content (creates new version)
//! let tree = tree.remove(5..11);
//! assert_eq!(tree.collect_bytes(), b"Hello!");
//!
//! // Sparse insert (creates gaps)
//! let tree = tree.insert(10, b"far away");  // Inserts with gaps
//! assert_eq!(tree.collect_bytes(b'_'), b"Hello!____far away");
//! ```
//!
//! # Implementation Details
//! The tree maintains the following invariants:
//! - Leaf nodes contain at most `N` bytes
//! - Internal nodes track the total size of their subtree
//! - All operations create new nodes instead of modifying existing ones
//! - Unchanged subtrees are shared between versions through Arc
//! - Gaps are represented explicitly using special Gap nodes
//!
//! # Sparse Operations
//! The structure supports sparse operations:
//! - Inserting beyond the current length creates gaps
//! - Gaps are filled with a specified byte value when collecting
//! - Removing beyond the current length is a no-op
//! - Gaps can be efficiently stored and manipulated
//!
//! # Performance
//! - Insert: O(log n)
//! - Remove: O(log n)
//! - Length query: O(1)
//! - Collection to contiguous bytes: O(n)
//! - Space efficiency: O(log n) additional space per modification
//! - Gap operations: O(1) for creation and removal
//!
//! # Memory Usage
//! The persistent nature of the structure means that modifications create new nodes
//! while reusing unmodified portions of the tree. This is achieved through Arc (Atomic
//! Reference Counting), which enables efficient sharing of unchanged subtrees between
//! different versions of the tree. Gaps are stored efficiently without allocating
//! actual space for the gap contents.
//! different versions of the tree.
use std::ops::Range;
use std::sync::Arc;

#[derive(Debug, Clone)]
enum ChunkTreeNode<'a, const N: usize> {
    Leaf {
        data: &'a [u8],
    },
    Gap {
        size: usize,
    },
    Internal {
        left: Arc<ChunkTreeNode<'a, N>>,
        mid: Arc<ChunkTreeNode<'a, N>>,
        right: Arc<ChunkTreeNode<'a, N>>,
        size: usize,
    },
}

impl<'a, const N: usize> ChunkTreeNode<'a, N> {
    fn from_slice(data: &'a [u8]) -> ChunkTreeNode<'a, N> {
        assert!(N > 0);
        if data.len() <= N {
            return ChunkTreeNode::Leaf { data };
        }

        let mid_index = data.len() / 2;
        let left = Self::from_slice(&data[..mid_index]);
        let right = Self::from_slice(&data[mid_index..]);
        let size = data.len();

        ChunkTreeNode::Internal {
            left: Arc::new(left),
            mid: Arc::new(ChunkTreeNode::empty()),
            right: Arc::new(right),
            size,
        }
    }

    fn len(&self) -> usize {
        match self {
            ChunkTreeNode::Leaf { data } => data.len(),
            ChunkTreeNode::Gap { size } => *size,
            ChunkTreeNode::Internal { size, .. } => *size,
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            ChunkTreeNode::Leaf { data } => data.is_empty(),
            ChunkTreeNode::Gap { size } => *size == 0,
            ChunkTreeNode::Internal { size, .. } => *size == 0,
        }
    }

    fn empty() -> ChunkTreeNode<'a, N> {
        ChunkTreeNode::Gap { size: 0 }
    }

    /// Inserts bytes in between existing data - growing the tree by data.len() bytes
    fn insert(&self, index: usize, data: &'a [u8]) -> ChunkTreeNode<'a, N> {
        match self {
            ChunkTreeNode::Leaf { data: leaf_data } => {
                let left = Self::from_slice(&leaf_data[..index]);
                let mid = Self::from_slice(data);
                let right = Self::from_slice(&leaf_data[index..]);

                ChunkTreeNode::Internal {
                    left: Arc::new(left),
                    mid: Arc::new(mid),
                    right: Arc::new(right),
                    size: leaf_data.len() + data.len(),
                }
            }
            ChunkTreeNode::Gap { size } => {
                let end_padding = size.saturating_sub(index);
                ChunkTreeNode::Internal {
                    left: Arc::new(ChunkTreeNode::Gap { size: index }),
                    mid: Arc::new(Self::from_slice(data)),
                    right: Arc::new(ChunkTreeNode::Gap { size: end_padding }),
                    size: index + data.len() + end_padding,
                }
            }
            ChunkTreeNode::Internal {
                left,
                mid,
                right,
                size: _,
            } => {
                let left_size = left.len();
                if index <= left_size {
                    let new_left = left.insert(index, data);
                    let new_size = new_left.len() + mid.len() + right.len();
                    ChunkTreeNode::Internal {
                        left: Arc::new(new_left),
                        mid: mid.clone(),
                        right: right.clone(),
                        size: new_size,
                    }
                } else if index <= left_size + mid.len() {
                    let new_mid = mid.insert(index - left_size, data);
                    let new_size = left_size + new_mid.len() + right.len();
                    ChunkTreeNode::Internal {
                        left: left.clone(),
                        mid: Arc::new(new_mid),
                        right: right.clone(),
                        size: new_size,
                    }
                } else if index <= left_size + mid.len() + right.len() {
                    let new_right = right.insert(index - left_size - mid.len(), data);
                    let new_size = left_size + mid.len() + new_right.len();
                    ChunkTreeNode::Internal {
                        left: left.clone(),
                        mid: mid.clone(),
                        right: Arc::new(new_right),
                        size: new_size,
                    }
                } else {
                    panic!("bug: sparse insert should have been handled above!")
                }
            }
        }
    }

    pub fn remove(&self, range: Range<usize>) -> ChunkTreeNode<'a, N> {
        if self.len() == 0 && range.is_empty() {
            return ChunkTreeNode::empty();
        }

        match self {
            ChunkTreeNode::Leaf { data } => ChunkTreeNode::Internal {
                left: Arc::new(Self::from_slice(&data[..range.start])),
                mid: Arc::new(Self::empty()),
                right: Arc::new(Self::from_slice(&data[range.end..])),
                size: data.len() - range.len(),
            },
            ChunkTreeNode::Gap { size } => {
                let new_size = if range.start >= *size {
                    *size
                } else {
                    let clamped_end = std::cmp::min(*size, range.end);
                    let removed_size = clamped_end - range.start;
                    *size - removed_size
                };
                assert!(
                    new_size <= *size,
                    "not satifisfied: new_size: {} <= size: {}",
                    new_size,
                    size
                );
                return ChunkTreeNode::Gap { size: new_size };
            }
            ChunkTreeNode::Internal {
                left,
                mid,
                right,
                size,
            } => {
                if range.start > *size {
                    return ChunkTreeNode::Internal {
                        left: left.clone(),
                        mid: mid.clone(),
                        right: right.clone(),
                        size: *size,
                    };
                }

                let new_left = if range.start < left.len() {
                    Arc::new(left.remove(Self::range_cap(&range, left.len())))
                } else {
                    left.clone()
                };

                let mid_range = Self::range_shift_left(&range, left.len());
                let new_mid = if mid_range.start < mid.len() {
                    Arc::new(mid.remove(Self::range_cap(&mid_range, mid.len())))
                } else {
                    mid.clone()
                };

                let right_range = Self::range_shift_left(&range, left.len() + mid.len());
                let new_right = if right_range.start < right.len() {
                    Arc::new(right.remove(Self::range_cap(&right_range, right.len())))
                } else {
                    right.clone()
                };

                let new_size = new_left.len() + new_mid.len() + new_right.len();

                assert!(*size >= new_size);
                assert_eq!(size - Self::range_cap(&range, *size).len(), new_size);

                ChunkTreeNode::Internal {
                    left: new_left,
                    mid: new_mid,
                    right: new_right,
                    size: new_size,
                }
            }
        }
    }

    fn range_shift_left(range: &Range<usize>, amount: usize) -> Range<usize> {
        (range.start.saturating_sub(amount))..(range.end.saturating_sub(amount))
    }

    fn range_cap(range: &Range<usize>, max: usize) -> Range<usize> {
        (std::cmp::min(range.start, max))..(std::cmp::min(range.end, max))
    }

    fn collect_bytes_into(&self, gap_value: u8, output: &mut Vec<u8>) {
        match self {
            ChunkTreeNode::Leaf { data } => output.extend_from_slice(data),
            ChunkTreeNode::Gap { size } => {
                for _ in 0..*size {
                    output.push(gap_value);
                }
            }
            ChunkTreeNode::Internal {
                left,
                mid,
                right,
                size: _,
            } => {
                left.collect_bytes_into(gap_value, output);
                mid.collect_bytes_into(gap_value, output);
                right.collect_bytes_into(gap_value, output);
            }
        }
    }
}
pub struct ChunkTreeIterator<'a, const N: usize> {
    stack: Vec<(&'a ChunkTreeNode<'a, N>, usize)>, // (node, next_child_index)
}

impl<'a, const N: usize> ChunkTreeNode<'a, N> {
    pub fn iter(&'a self) -> ChunkTreeIterator<'a, N> {
        let mut iter = ChunkTreeIterator { stack: Vec::new() };
        iter.stack.push((self, 0));
        iter
    }
}

#[derive(Debug, PartialEq)]
pub enum ChunkPiece<'a> {
    Data { data: &'a [u8] },
    Gap { size: usize },
}

impl<'a, const N: usize> Iterator for ChunkTreeIterator<'a, N> {
    type Item = ChunkPiece<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((node, child_idx)) = self.stack.pop() {
            if node.len() == 0 {
                // hide empty data / empty gaps
                continue;
            }
            match node {
                ChunkTreeNode::Leaf { data } => return Some(ChunkPiece::Data { data }),
                ChunkTreeNode::Gap { size } => return Some(ChunkPiece::Gap { size: *size }),
                ChunkTreeNode::Internal {
                    left, mid, right, ..
                } => match child_idx {
                    0 => {
                        self.stack.push((node, 1));
                        self.stack.push((left, 0));
                    }
                    1 => {
                        self.stack.push((node, 2));
                        self.stack.push((mid, 0));
                    }
                    2 => {
                        self.stack.push((right, 0));
                    }
                    _ => panic!("invalid child_idx: {:?}", child_idx),
                },
            }
        }
        None
    }
}
#[derive(Debug)]
pub struct ChunkTree<'a, const N: usize> {
    root: Arc<ChunkTreeNode<'a, N>>,
}

impl<'a, const N: usize> ChunkTree<'a, N> {
    /// Panics if N = 0
    pub fn new() -> ChunkTree<'a, N> {
        Self::from_slice(&[])
    }

    /// Creates a tree from (possibly empty) data
    pub fn from_slice(data: &'a [u8]) -> ChunkTree<'a, N> {
        ChunkTree {
            root: Arc::new(ChunkTreeNode::from_slice(data)),
        }
    }

    pub fn len(&self) -> usize {
        self.root.len()
    }

    pub fn is_empty(&self) -> bool {
        self.root.is_empty()
    }

    pub fn insert(&self, index: usize, data: &'a [u8]) -> ChunkTree<'a, N> {
        if index <= self.len() {
            ChunkTree {
                root: Arc::new(self.root.insert(index, data)),
            }
        } else {
            // sparse insert
            ChunkTree {
                root: Arc::new(ChunkTreeNode::Internal {
                    left: self.root.clone(),
                    mid: Arc::new(ChunkTreeNode::Gap {
                        size: index - self.len(),
                    }),
                    right: Arc::new(ChunkTreeNode::from_slice(data)),
                    size: index + data.len(),
                }),
            }
        }
    }

    pub fn remove(&self, range: Range<usize>) -> ChunkTree<'a, N> {
        if range.start < self.len() {
            ChunkTree {
                root: Arc::new(
                    self.root
                        .remove(range.start..(std::cmp::min(self.root.len(), range.end))),
                ),
            }
        } else {
            // sparse remove - do nothing
            ChunkTree {
                root: self.root.clone(),
            }
        }
    }

    pub fn collect_bytes(&self, gap_value: u8) -> Vec<u8> {
        let mut v = vec![];
        self.root.collect_bytes_into(gap_value, &mut v);
        v
    }

    pub fn collect_bytes_into(&self, gap_value: u8, output: &mut Vec<u8>) {
        self.root.collect_bytes_into(gap_value, output);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_tree() {
        let tree = ChunkTree::<2>::new();
        assert!(tree.is_empty());
        assert_eq!(tree.len(), 0);
        assert_eq!(tree.collect_bytes(0), vec![]);
    }

    #[test]
    fn test_empty_operations() {
        let tree = ChunkTree::<2>::from_slice(b"test");
        let tree = tree.remove(2..2); // Empty range
        assert_eq!(tree.collect_bytes(0), b"test");
    }

    #[test]
    fn test_from_slice() {
        let data = b"Hello World!";
        let tree = ChunkTree::<2>::from_slice(data);
        assert!(!tree.is_empty());
        assert_eq!(tree.len(), data.len());
        assert_eq!(tree.collect_bytes(0), b"Hello World!");
    }

    #[test]
    fn test_from_slice_big() {
        let data = b"Hello World!";
        let tree = ChunkTree::<20>::from_slice(data);
        assert!(!tree.is_empty());
        println!("tree: {:?}", tree);
        assert_eq!(tree.len(), data.len());
        assert_eq!(tree.collect_bytes(0), b"Hello World!");
    }

    #[test]
    fn test_insert_middle() {
        let tree = ChunkTree::<2>::from_slice(b"Hello World!");
        let tree = tree.insert(5, b" beautiful");
        assert_eq!(tree.collect_bytes(0), b"Hello beautiful World!");
    }

    #[test]
    fn test_insert_sparse_big() {
        let tree = ChunkTree::<20>::new();
        let tree = tree.insert(5, b"ahem, ahem");
        println!("tree: {:?}", tree);
        assert_eq!(tree.collect_bytes(b'_'), b"_____ahem, ahem");
    }

    #[test]
    fn test_insert_start() {
        let tree = ChunkTree::<2>::from_slice(b"World!");
        let tree = tree.insert(0, b"Hello ");
        assert_eq!(tree.collect_bytes(0), b"Hello World!");
    }

    #[test]
    fn test_insert_end() {
        let tree = ChunkTree::<2>::from_slice(b"Hello");
        let tree = tree.insert(5, b" World!");
        assert_eq!(tree.collect_bytes(0), b"Hello World!");
    }

    #[test]
    fn test_remove_middle() {
        let tree = ChunkTree::<2>::from_slice(b"Hello beautiful World!");
        let tree = tree.remove(5..15);
        assert_eq!(tree.collect_bytes(0), b"Hello World!");
    }

    #[test]
    fn test_remove_start() {
        let tree = ChunkTree::<2>::from_slice(b"Hello World!");
        let tree = tree.remove(0..6);
        assert_eq!(tree.collect_bytes(0), b"World!");
    }

    #[test]
    fn test_remove_end() {
        let tree = ChunkTree::<2>::from_slice(b"Hello World!");
        let tree = tree.remove(5..12);
        assert_eq!(tree.collect_bytes(0), b"Hello");
    }

    #[test]
    fn test_from_slice_big_chunk() {
        let data = b"Hello World!";
        let tree = ChunkTree::<15>::from_slice(data);
        assert!(!tree.is_empty());
        assert_eq!(tree.len(), data.len());
        assert_eq!(tree.collect_bytes(0), b"Hello World!");
    }

    #[test]
    fn test_insert_middle_big_chunk() {
        let tree = ChunkTree::<15>::from_slice(b"Hello World!");
        let tree = tree.insert(5, b" beautiful");
        assert_eq!(tree.collect_bytes(0), b"Hello beautiful World!");
    }

    #[test]
    fn test_insert_start_big_chunk() {
        let tree = ChunkTree::<15>::from_slice(b"World!");
        let tree = tree.insert(0, b"Hello ");
        assert_eq!(tree.collect_bytes(0), b"Hello World!");
    }

    #[test]
    fn test_insert_end_big_chunk() {
        let tree = ChunkTree::<15>::from_slice(b"Hello");
        let tree = tree.insert(5, b" World!");
        assert_eq!(tree.collect_bytes(0), b"Hello World!");
    }

    #[test]
    fn test_remove_middle_big_chunk() {
        let tree = ChunkTree::<15>::from_slice(b"Hello beautiful World!");
        let tree = tree.remove(5..15);
        assert_eq!(tree.collect_bytes(0), b"Hello World!");
    }

    #[test]
    fn test_remove_start_big_chunk() {
        let tree = ChunkTree::<15>::from_slice(b"Hello World!");
        let tree = tree.remove(0..6);
        assert_eq!(tree.collect_bytes(0), b"World!");
    }

    #[test]
    fn test_remove_end_big_chunk() {
        let tree = ChunkTree::<15>::from_slice(b"Hello World!");
        let tree = tree.remove(5..12);
        assert_eq!(tree.collect_bytes(0), b"Hello");
    }

    #[test]

    fn test_sparse_insert_small() {
        let tree = ChunkTree::<2>::from_slice(b"Hello");
        let tree = tree.insert(6, b" World!");
        assert_eq!(tree.len(), 13);
    }

    fn test_sparse_insert() {
        let tree = ChunkTree::<15>::from_slice(b"Hello");
        let tree = tree.insert(6, b" World!");
        assert_eq!(tree.len(), 13);
        assert_eq!(tree.collect_bytes(b'X'), b"HelloX World!");
    }

    fn test_sparse_insert_remove() {
        let tree = ChunkTree::<15>::from_slice(b"Hello");
        let tree = tree.insert(6, b" World!");
        assert_eq!(tree.len(), 13);
        assert_eq!(tree.collect_bytes(b'X'), b"HelloX World!");

        let tree = tree.remove(4..7);
        assert_eq!(tree.len(), 12);
        assert_eq!(tree.collect_bytes(b'X'), b"HellWorld!");
    }

    #[test]
    fn test_remove_beyond_end_small() {
        let tree = ChunkTree::<2>::from_slice(b"Hello");
        let tree = tree.remove(3..6);
        assert_eq!(tree.len(), 3);
        assert_eq!(tree.collect_bytes(0), b"Hel");
    }

    #[test]
    fn test_remove_beyond_end() {
        let tree = ChunkTree::<15>::from_slice(b"Hello");
        let tree = tree.remove(3..8);
        assert_eq!(tree.len(), 3);
        assert_eq!(tree.collect_bytes(0), b"Hel");
    }

    #[test]
    fn test_insert_all_ranges() {
        let initial = b"Hello World!";
        let tree = ChunkTree::<2>::from_slice(initial);
        for pos in 0..=initial.len() {
            for len in 0..=initial.len() {
                let data = ("0123456789abcdefgh"[0..len]).as_bytes();

                // Test insert
                let mut reference = Vec::from(&initial[..]);
                reference.splice(pos..pos, data.iter().cloned());
                let modified_tree = tree.insert(pos, &data);
                assert_eq!(modified_tree.collect_bytes(0), reference);
                if len > 0 {
                    assert_ne!(modified_tree.collect_bytes(0), tree.collect_bytes(0));
                } else {
                    assert_eq!(modified_tree.collect_bytes(0), tree.collect_bytes(0));
                }
            }
        }
    }

    #[test]
    fn test_remove_all_ranges() {
        let initial = b"Hello World!";
        let tree = ChunkTree::<2>::from_slice(initial);
        for pos in 0..initial.len() {
            for len in 0..=initial.len() {
                // Test remove
                let range = pos..std::cmp::min(pos + len, tree.len());
                let mut reference = Vec::from(&initial[..]);
                reference.splice(range.clone(), []);
                let modified_tree = tree.remove(range);
                assert_eq!(modified_tree.collect_bytes(0), reference);
                if len > 0 {
                    assert_ne!(modified_tree.collect_bytes(0), tree.collect_bytes(0));
                } else {
                    assert_eq!(modified_tree.collect_bytes(0), tree.collect_bytes(0));
                }
            }
        }
    }

    #[test]
    fn test_iterator() {
        // Empty tree
        let empty_tree = ChunkTreeNode::<2>::empty();
        let mut iter = empty_tree.iter();
        assert_eq!(iter.next(), None);

        // Simple leaf node
        let leaf = ChunkTreeNode::<2>::from_slice(b"abc");
        let mut iter = leaf.iter();
        assert_eq!(iter.next(), Some(ChunkPiece::Data { data: b"a" }));
        assert_eq!(iter.next(), Some(ChunkPiece::Data { data: b"bc" }));
        assert_eq!(iter.next(), None);

        // Gap node
        let gap: ChunkTreeNode<'_, 2> = ChunkTreeNode::Gap { size: 3 };
        let mut iter = gap.iter();
        assert_eq!(iter.next(), Some(ChunkPiece::Gap { size: 3 }));
        assert_eq!(iter.next(), None);

        // Complex tree with internal nodes
        let tree = ChunkTreeNode::<2>::from_slice(b"Hello");
        let tree = tree.insert(5, b" World!");

        let expected = vec![
            ChunkPiece::Data { data: b"He" },
            ChunkPiece::Data { data: b"l" },
            ChunkPiece::Data { data: b"lo" },
            ChunkPiece::Data { data: b" " },
            ChunkPiece::Data { data: b"Wo" },
            ChunkPiece::Data { data: b"rl" },
            ChunkPiece::Data { data: b"d!" },
        ];

        let actual: Vec<_> = tree.iter().collect();

        for (index, (a, e)) in actual.iter().zip(expected.iter()).enumerate() {
            println!("index: {}, actual: {:?}, expected: {:?}", index, a, e);
            assert_eq!(a, e);
        }
        println!("actual: {:?}", actual);
        assert_eq!(actual.len(), expected.len());
    }

    #[test]
    fn test_fill_sparse() {
        let tree = ChunkTree::<2>::new();
        let tree = tree.insert(1, b"the end");
        let tree = tree.insert(0, b"start");
        assert_eq!(tree.collect_bytes(b'_'), b"start_the end");
    }

    #[test]
    fn test_complex_sparse_operations() {
        let tree = ChunkTree::<30>::new();

        // Test sparse insert with large gap
        let tree = tree.insert(10, b"hello");
        assert_eq!(tree.len(), 15);
        assert_eq!(tree.collect_bytes(b'_'), b"__________hello");

        // Test sparse remove beyond end
        let tree = tree.remove(20..30);
        assert_eq!(tree.len(), 15);

        // Test removing gaps
        let tree = tree.remove(5..12);
        println!("tree: {:?}", tree);
        assert_eq!(tree.collect_bytes(b'_'), b"_____llo");

        // Test complex insert chain
        let tree = tree.insert(2, b"ABC");
        println!("tree: {:?}", tree);
        assert_eq!(tree.collect_bytes(b'_'), b"__ABC___llo");
        let tree = tree.insert(8, b"XYZ");
        assert_eq!(tree.collect_bytes(b'_'), b"__ABC___XYZllo");
    }

    #[test]
    fn test_internal_node_edge_cases() {
        let tree = ChunkTree::<2>::from_slice(b"abcdef");

        // Test internal node operations at boundaries
        let tree = tree.remove(0..2); // Remove from start
        let tree = tree.remove(2..4); // Remove from middle
        assert_eq!(tree.collect_bytes(b'_'), b"cd");

        // Test empty gap creation
        let tree = tree.insert(10, b"end");
        assert_eq!(tree.collect_bytes(b'_'), b"cd________end");
    }

    #[test]
    fn test_iterator_complex() {
        let tree = ChunkTree::<10>::new();
        println!("tree: {:?}", tree);
        let tree = tree.insert(5, b"middle");
        println!("tree: {:?}", tree);
        let tree = tree.insert(0, b"start");
        println!("tree: {:?}", tree);
        let tree = tree.insert(20, b"end");
        println!("tree: {:?}", tree);

        let pieces: Vec<ChunkPiece> = tree.root.iter().collect();
        assert!(pieces.len() > 0);

        // Verify the structure contains expected data and gaps
        let mut found_start = false;
        let mut found_middle = false;
        let mut found_end = false;

        for piece in pieces {
            match piece {
                ChunkPiece::Data { data } => {
                    let str = String::from_utf8_lossy(data);
                    println!("data: {:?}", str);
                    if data == b"start" {
                        found_start = true;
                    }
                    if data == b"middle" {
                        found_middle = true;
                    }
                    if data == b"end" {
                        found_end = true;
                    }
                }
                ChunkPiece::Gap { size: _ } => {}
            }
        }

        assert!(found_start);
        assert!(found_middle);
        assert!(found_end);
    }

    #[test]
    #[should_panic]
    fn test_zero_size_chunk() {
        let _tree = ChunkTree::<0>::new();
    }
}
