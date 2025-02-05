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
//! ```
//!
//! # Implementation Details
//! The tree maintains the following invariants:
//! - Leaf nodes contain at most `N` bytes
//! - Internal nodes track the total size of their subtree
//! - All operations create new nodes instead of modifying existing ones
//! - Unchanged subtrees are shared between versions through Arc
//!
//! # Performance
//! - Insert: O(log n)
//! - Remove: O(log n)
//! - Length query: O(1)
//! - Collection to contiguous bytes: O(n)
//! - Space efficiency: O(log n) additional space per modification
//!
//! # Memory Usage
//! The persistent nature of the structure means that modifications create new nodes
//! while reusing unmodified portions of the tree. This is achieved through Arc (Atomic
//! Reference Counting), which enables efficient sharing of unchanged subtrees between
//! different versions of the tree.
use std::ops::Range;
use std::sync::Arc;

#[derive(Debug, Clone)]
enum ChunkTree<'a, const N: usize> {
    Leaf {
        data: &'a [u8],
    },
    Gap {
        size: usize,
    },
    Internal {
        left: Arc<ChunkTree<'a, N>>,
        mid: Arc<ChunkTree<'a, N>>,
        right: Arc<ChunkTree<'a, N>>,
        size: usize,
    },
}

impl<'a, const N: usize> ChunkTree<'a, N> {
    /// Panics if N = 0
    pub fn new() -> Arc<ChunkTree<'a, N>> {
        assert!(N > 0);
        Self::from_slice(&[])
    }

    /// Creates a tree from (possibly empty) data
    pub fn from_slice(data: &[u8]) -> Arc<ChunkTree<N>> {
        if data.len() <= N {
            return Arc::new(ChunkTree::Leaf { data });
        }

        let mid_index = data.len() / 2;
        let left = Self::from_slice(&data[..mid_index]);
        let right = Self::from_slice(&data[mid_index..]);
        let size = data.len();

        Arc::new(ChunkTree::Internal {
            left,
            mid: Arc::new(ChunkTree::Leaf { data: &[] }),
            right,
            size,
        })
    }

    pub fn len(&self) -> usize {
        match self {
            ChunkTree::Leaf { data } => data.len(),
            ChunkTree::Gap { size } => *size,
            ChunkTree::Internal { size, .. } => *size,
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            ChunkTree::Leaf { data } => data.is_empty(),
            ChunkTree::Gap { size } => *size == 0,
            ChunkTree::Internal { size, .. } => *size == 0,
        }
    }

    pub fn insert(&'a self, index: usize, data: &'a [u8]) -> Arc<ChunkTree<N>> {
        if index >= self.len() {
            // sparse insert
            return Arc::new(ChunkTree::Internal {
                left: Arc::new((*self).clone()),
                mid: Arc::new(ChunkTree::Gap {
                    size: index - self.len(),
                }),
                right: Self::from_slice(data),
                size: index + data.len(),
            });
        }

        match self {
            ChunkTree::Leaf { data: leaf_data } => {
                let left = Self::from_slice(&leaf_data[..index]);
                let mid = Self::from_slice(data);
                let right = Self::from_slice(&leaf_data[index..]);

                Arc::new(ChunkTree::Internal {
                    left,
                    mid,
                    right,
                    size: leaf_data.len() + data.len(),
                })
            }
            ChunkTree::Gap { size } => {
                let end_padding = size.saturating_sub(index + data.len());
                Arc::new(ChunkTree::Internal {
                    left: Arc::new(ChunkTree::Gap { size: index }),
                    mid: Self::from_slice(data),
                    right: Arc::new(ChunkTree::Gap { size: end_padding }),
                    size: index + data.len() + end_padding,
                })
            }
            ChunkTree::Internal {
                left,
                mid,
                right,
                size: _,
            } => {
                let left_size = left.len();
                if index <= left_size {
                    let new_left = left.insert(index, data);
                    let new_size = new_left.len() + mid.len() + right.len();
                    Arc::new(ChunkTree::Internal {
                        left: new_left,
                        mid: mid.clone(),
                        right: right.clone(),
                        size: new_size,
                    })
                } else if index <= left_size + mid.len() {
                    let new_mid = mid.insert(index - left_size, data);
                    let new_size = left_size + new_mid.len() + right.len();
                    Arc::new(ChunkTree::Internal {
                        left: left.clone(),
                        mid: new_mid,
                        right: right.clone(),
                        size: new_size,
                    })
                } else if index <= left_size + mid.len() + right.len() {
                    let new_right = right.insert(index - left_size - mid.len(), data);
                    let new_size = left_size + mid.len() + new_right.len();
                    Arc::new(ChunkTree::Internal {
                        left: left.clone(),
                        mid: mid.clone(),
                        right: new_right,
                        size: new_size,
                    })
                } else {
                    panic!("bug: sparse insert should have been handled above!")
                }
            }
        }
    }

    pub fn remove(&'a self, range: Range<usize>) -> Arc<ChunkTree<N>> {
        if self.len() == 0 && range.is_empty() {
            return Arc::new(ChunkTree::Leaf { data: &[] });
        }

        if range.start >= self.len() || range.end > self.len() {
            // sparse remove - do nothing
            return Arc::new(self.clone());
        }

        match self {
            ChunkTree::Leaf { data } => Arc::new(ChunkTree::Internal {
                left: Self::from_slice(&data[..range.start]),
                mid: Self::from_slice(&[]),
                right: Self::from_slice(&data[range.end..]),
                size: data.len() - range.len(),
            }),
            ChunkTree::Gap { size } => {
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
                return Arc::new(ChunkTree::Gap { size: new_size });
            }
            ChunkTree::Internal {
                left,
                mid,
                right,
                size,
            } => {
                if range.start > *size {
                    return Arc::new(ChunkTree::Internal {
                        left: left.clone(),
                        mid: mid.clone(),
                        right: right.clone(),
                        size: *size,
                    });
                }

                let new_left = if range.start < left.len() {
                    left.remove(Self::range_cap(&range, left.len()))
                } else {
                    left.clone()
                };

                let mid_range = Self::range_shift_left(&range, left.len());
                let new_mid = if mid_range.start < mid.len() {
                    mid.remove(Self::range_cap(&mid_range, mid.len()))
                } else {
                    mid.clone()
                };

                let right_range = Self::range_shift_left(&range, left.len() + mid.len());
                let new_right = if right_range.start < right.len() {
                    right.remove(Self::range_cap(&right_range, right.len()))
                } else {
                    right.clone()
                };

                let new_size = new_left.len() + new_mid.len() + new_right.len();

                assert!(*size >= new_size);
                assert_eq!(size - Self::range_cap(&range, *size).len(), new_size);

                Arc::new(ChunkTree::Internal {
                    left: new_left,
                    mid: new_mid,
                    right: new_right,
                    size: new_size,
                })
            }
        }
    }

    pub fn collect_bytes(&self, gap_value: u8) -> Vec<u8> {
        let mut v = vec![];
        self.collect_bytes_into(gap_value, &mut v);
        v
    }

    pub fn collect_bytes_into(&self, gap_value: u8, output: &mut Vec<u8>) {
        match self {
            ChunkTree::Leaf { data } => output.extend_from_slice(data),
            ChunkTree::Gap { size } => {
                for i in 0..*size {
                    output.push(gap_value);
                }
            }
            ChunkTree::Internal {
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

    fn range_shift_left(range: &Range<usize>, amount: usize) -> Range<usize> {
        (range.start.saturating_sub(amount))..(range.end.saturating_sub(amount))
    }

    fn range_cap(range: &Range<usize>, max: usize) -> Range<usize> {
        (std::cmp::min(range.start, max))..(std::cmp::min(range.end, max))
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

    #[test]
    fn test_remove_beyond_end_small() {
        let tree = ChunkTree::<2>::from_slice(b"Hello");
        let tree = tree.remove(3..6);
        assert_eq!(tree.len(), 5);
        assert_eq!(tree.collect_bytes(0), b"Hello");
    }

    #[test]
    fn test_remove_beyond_end() {
        let tree = ChunkTree::<15>::from_slice(b"Hello");
        let tree = tree.remove(3..6);
        assert_eq!(tree.len(), 5);
        assert_eq!(tree.collect_bytes(0), b"Hello");
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
