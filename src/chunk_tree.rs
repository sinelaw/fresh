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
    fn from_slice(data: &[u8]) -> ChunkTreeNode<N> {
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
            mid: Arc::new(ChunkTreeNode::Leaf { data: &[] }),
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

    fn insert(&'a self, index: usize, data: &'a [u8]) -> ChunkTreeNode<N> {
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
                let end_padding = size.saturating_sub(index + data.len());
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

    pub fn remove(&'a self, range: Range<usize>) -> ChunkTreeNode<N> {
        if self.len() == 0 && range.is_empty() {
            return ChunkTreeNode::Leaf { data: &[] };
        }

        match self {
            ChunkTreeNode::Leaf { data } => ChunkTreeNode::Internal {
                left: Arc::new(Self::from_slice(&data[..range.start])),
                mid: Arc::new(Self::from_slice(&[])),
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

#[derive(Debug)]
struct ChunkTree<'a, const N: usize> {
    root: Arc<ChunkTreeNode<'a, N>>,
}

impl<'a, const N: usize> ChunkTree<'a, N> {
    /// Panics if N = 0
    pub fn new() -> ChunkTree<'a, N> {
        Self::from_slice(&[])
    }

    /// Creates a tree from (possibly empty) data
    pub fn from_slice(data: &[u8]) -> ChunkTree<N> {
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

    pub fn insert(&'a self, index: usize, data: &'a [u8]) -> ChunkTree<N> {
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

    pub fn remove(&'a self, range: Range<usize>) -> ChunkTree<N> {
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
    fn test_from_slice() {
        let data = b"Hello World!";
        let tree = ChunkTree::<2>::from_slice(data);
        assert!(!tree.is_empty());
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
        let tree = tree.remove(3..6);
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
}
