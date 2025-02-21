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
//! let tree = ChunkTree::new(2);
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

use crate::logs::log;

#[derive(Debug, Clone)]
enum ChunkTreeNode<'a> {
    Leaf {
        data: &'a [u8],
    },
    Gap {
        size: usize,
    },
    Internal {
        children: Vec<Arc<ChunkTreeNode<'a>>>,
        size: usize,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct ChunkTreeConfig {
    chunk_size: usize,
    max_children: usize,
}

impl ChunkTreeConfig {
    pub const fn new(chunk_size: usize, max_children: usize) -> ChunkTreeConfig {
        assert!(chunk_size > 0);
        assert!(max_children > 0);
        ChunkTreeConfig {
            chunk_size,
            max_children,
        }
    }
}

impl<'a> ChunkTreeNode<'a> {
    fn from_slice(data: &'a [u8], config: ChunkTreeConfig) -> ChunkTreeNode<'a> {
        if data.len() <= config.chunk_size {
            return ChunkTreeNode::Leaf { data };
        }

        let mid_index = data.len() / 2;
        let left = Self::from_slice(&data[..mid_index], config);
        let right = Self::from_slice(&data[mid_index..], config);
        let size = data.len();

        ChunkTreeNode::Internal {
            children: vec![Arc::new(left), Arc::new(right)],
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

    fn empty() -> ChunkTreeNode<'a> {
        ChunkTreeNode::Gap { size: 0 }
    }

    /// Concatenates two trees with optional gap
    fn append(
        &self,
        gap_size: usize,
        other: Arc<ChunkTreeNode<'a>>,
        _config: ChunkTreeConfig,
    ) -> ChunkTreeNode<'a> {
        let other_len = other.len();
        match self {
            ChunkTreeNode::Leaf { .. } => {
                let mut children = Vec::new();
                children.push(Arc::new(self.clone()));
                if gap_size > 0 {
                    children.push(Arc::new(ChunkTreeNode::Gap { size: gap_size }));
                }
                children.push(other);
                ChunkTreeNode::Internal {
                    children,
                    size: self.len() + gap_size + other_len,
                }
            }
            ChunkTreeNode::Gap { .. } => {
                let mut children = Vec::new();
                children.push(Arc::new(self.clone()));
                if gap_size > 0 {
                    children.push(Arc::new(ChunkTreeNode::Gap { size: gap_size }));
                }
                children.push(other);
                ChunkTreeNode::Internal {
                    children,
                    size: self.len() + gap_size + other_len,
                }
            }
            ChunkTreeNode::Internal { children, size } => {
                let mut new_children = children.clone();
                if gap_size > 0 {
                    new_children.push(Arc::new(ChunkTreeNode::Gap { size: gap_size }));
                }
                new_children.push(other);
                ChunkTreeNode::Internal {
                    children: new_children,
                    size: size + gap_size + other_len,
                }
            }
        }
    }

    /// Inserts bytes in between existing data - growing the tree by data.len() bytes
    ///
    /// panics if `index > self.len()` (sparse insert)
    /// panics if data.is_empty()
    fn insert(&self, index: usize, data: &'a [u8], config: ChunkTreeConfig) -> ChunkTreeNode<'a> {
        assert!(index <= self.len());
        assert!(!data.is_empty());
        match self {
            ChunkTreeNode::Leaf { data: leaf_data } => {
                let mut children = Vec::with_capacity(3);
                if index > 0 {
                    children.push(Arc::new(Self::from_slice(&leaf_data[..index], config)));
                }
                children.push(Arc::new(Self::from_slice(data, config)));
                if index < leaf_data.len() {
                    children.push(Arc::new(Self::from_slice(&leaf_data[index..], config)));
                }
                ChunkTreeNode::Internal {
                    children,
                    size: leaf_data.len() + data.len(),
                }
            }
            ChunkTreeNode::Gap { size } => {
                let mut children = Vec::with_capacity(3);
                if index > 0 {
                    children.push(Arc::new(ChunkTreeNode::Gap { size: index }));
                }
                children.push(Arc::new(Self::from_slice(data, config)));
                if index < *size {
                    children.push(Arc::new(ChunkTreeNode::Gap { size: size - index }));
                }
                ChunkTreeNode::Internal {
                    children,
                    size: size + data.len(),
                }
            }
            ChunkTreeNode::Internal { children, size } => {
                let mut current_pos = 0;
                let mut i = 0;
                // Find which child we should insert into
                for (idx, child) in children.iter().enumerate() {
                    if current_pos + child.len() >= index {
                        i = idx;
                        break;
                    }
                    current_pos += child.len();
                }

                if i >= children.len() {
                    panic!("bug: sparse insert should have been handled above!");
                }

                // Insert into the appropriate child
                let relative_index = index - current_pos;
                let mut new_children = children.clone();

                if relative_index == 0 {
                    // Insert at start of child - add as new node between children
                    new_children.insert(i, Arc::new(ChunkTreeNode::from_slice(data, config)));
                } else if relative_index == children[i].len() {
                    // Insert at end of child - add as new node between children
                    new_children.insert(i + 1, Arc::new(ChunkTreeNode::from_slice(data, config)));
                } else {
                    // Insert within child
                    let new_child = new_children[i].insert(relative_index, data, config);
                    new_children[i] = Arc::new(new_child);
                }

                ChunkTreeNode::Internal {
                    children: new_children,
                    size: size + data.len(),
                }
            }
        }
    }

    /// Removes a range from the tree
    ///
    /// panics if range.start or range.end > tree.len()
    /// panics if range.is_empty()
    pub fn remove(&self, range: Range<usize>, config: ChunkTreeConfig) -> ChunkTreeNode<'a> {
        assert!(range.start <= self.len());
        assert!(range.end <= self.len());
        assert!(!range.is_empty());

        if self.len() == 0 {
            return ChunkTreeNode::empty();
        }

        log!("range: {:?}", range);

        match self {
            ChunkTreeNode::Leaf { data } => ChunkTreeNode::Internal {
                children: vec![
                    Arc::new(Self::from_slice(&data[..range.start], config)),
                    Arc::new(Self::from_slice(&data[range.end..], config)),
                ],
                size: data.len() - range.len(),
            },
            ChunkTreeNode::Gap { size } => {
                return ChunkTreeNode::Gap {
                    size: *size - range.len(),
                };
            }
            ChunkTreeNode::Internal { children, size } => {
                let mut next_pos = 0;
                let mut new_children = Vec::new();
                let mut remaining_range = range.clone();
                // Iterate through children to find affected ranges
                for child in children.iter() {
                    let child_len = child.len();
                    let child_pos = next_pos;
                    next_pos += child_len;
                    let child_range_abs = child_pos..(child_pos + child_len);

                    if child_range_abs.is_empty() {
                        continue; // skip empty child
                    }
                    if child_range_abs.end <= remaining_range.start {
                        new_children.push(child.clone());
                        continue;
                    }
                    if child_range_abs.start >= remaining_range.end {
                        new_children.push(child.clone());
                        continue;
                    }

                    // Process child that intersects with range
                    let end = std::cmp::min(child_pos + child_len, remaining_range.end);
                    let remove_relative_range =
                        (remaining_range.start - child_pos)..(end - child_pos);
                    log!(
                        "remaining_range: {:?}, remove_relative_range: {:?}, child: {:?}",
                        remaining_range,
                        remove_relative_range,
                        child
                    );
                    let new_child = child.remove(remove_relative_range, config);
                    if !new_child.is_empty() {
                        new_children.push(Arc::new(new_child));
                    }
                    // Adjust remaining range
                    remaining_range.start = end;
                }

                ChunkTreeNode::Internal {
                    children: new_children,
                    size: size - range.len(),
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
            ChunkTreeNode::Internal { children, size: _ } => {
                for child in children {
                    child.collect_bytes_into(gap_value, output);
                }
            }
        }
    }
}
pub struct ChunkTreeIterator<'a> {
    stack: Vec<(&'a ChunkTreeNode<'a>, usize)>, // (nodeext_child_index)
}

impl<'a> ChunkTreeNode<'a> {
    pub fn iter(&'a self) -> ChunkTreeIterator<'a> {
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

impl<'a> Iterator for ChunkTreeIterator<'a> {
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
                ChunkTreeNode::Internal { children, .. } => {
                    if child_idx < children.len() {
                        if child_idx + 1 < children.len() {
                            self.stack.push((node, child_idx + 1));
                        }
                        self.stack.push((&children[child_idx], 0));
                    } else {
                        panic!("invalid child_idx: {:?}", child_idx);
                    }
                }
            }
        }
        None
    }
}
#[derive(Debug)]
pub struct ChunkTree<'a> {
    root: Arc<ChunkTreeNode<'a>>,
    config: ChunkTreeConfig,
}

impl<'a> ChunkTree<'a> {
    /// Panics if n = 0
    pub fn new(config: ChunkTreeConfig) -> ChunkTree<'a> {
        Self::from_slice(&[], config)
    }

    /// Creates a tree from (possibly empty) data
    pub fn from_slice(data: &'a [u8], config: ChunkTreeConfig) -> ChunkTree<'a> {
        ChunkTree {
            root: Arc::new(ChunkTreeNode::from_slice(data, config)),
            config,
        }
    }

    pub fn len(&self) -> usize {
        self.root.len()
    }

    pub fn is_empty(&self) -> bool {
        self.root.is_empty()
    }

    pub fn insert(&self, index: usize, data: &'a [u8]) -> ChunkTree<'a> {
        if data.len() == 0 {
            ChunkTree {
                root: self.root.clone(),
                config: self.config,
            }
        } else if index <= self.len() {
            ChunkTree {
                root: Arc::new(self.root.insert(index, data, self.config)),
                config: self.config,
            }
        } else {
            // sparse insert
            ChunkTree {
                root: Arc::new(self.root.append(
                    index - self.len(),
                    Arc::new(ChunkTreeNode::from_slice(&data, self.config)),
                    self.config,
                )),
                config: self.config,
            }
        }
    }

    pub fn remove(&self, range: Range<usize>) -> ChunkTree<'a> {
        if range.is_empty() || range.start >= self.len() {
            // empty or sparse remove - do nothing
            ChunkTree {
                root: self.root.clone(),
                config: self.config,
            }
        } else {
            ChunkTree {
                root: Arc::new(self.root.remove(
                    range.start..(std::cmp::min(self.root.len(), range.end)),
                    self.config,
                )),
                config: self.config,
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

    const SMALL_CONFIG: ChunkTreeConfig = ChunkTreeConfig::new(2, 2);

    #[test]
    fn test_empty_tree() {
        let tree = ChunkTree::new(SMALL_CONFIG);
        assert!(tree.is_empty());
        assert_eq!(tree.len(), 0);
        assert_eq!(tree.collect_bytes(0), vec![]);
    }

    #[test]
    fn test_empty_operations() {
        let tree = ChunkTree::from_slice(b"test", SMALL_CONFIG);
        let tree = tree.remove(2..2); // Empty range
        assert_eq!(tree.collect_bytes(0), b"test");
    }

    #[test]
    fn test_from_slice() {
        let data = b"Hello World!";
        let tree = ChunkTree::from_slice(data, SMALL_CONFIG);
        assert!(!tree.is_empty());
        assert_eq!(tree.len(), data.len());
        assert_eq!(tree.collect_bytes(0), b"Hello World!");
    }

    #[test]
    fn test_from_slice_big() {
        let data = b"Hello World!";
        let tree = ChunkTree::from_slice(data, ChunkTreeConfig::new(20, 20));
        assert!(!tree.is_empty());
        println!("tree: {:?}", tree);
        assert_eq!(tree.len(), data.len());
        assert_eq!(tree.collect_bytes(0), b"Hello World!");
    }

    #[test]
    fn test_insert_middle() {
        let tree = ChunkTree::from_slice(b"Hello World!", SMALL_CONFIG);
        let tree = tree.insert(5, b" beautiful");
        assert_eq!(tree.collect_bytes(0), b"Hello beautiful World!");
    }

    #[test]
    fn test_insert_sparse_big() {
        let tree = ChunkTree::new(ChunkTreeConfig::new(20, 20));
        let tree = tree.insert(5, b"ahem, ahem");
        println!("tree: {:?}", tree);
        assert_eq!(tree.collect_bytes(b'_'), b"_____ahem, ahem");
    }

    #[test]
    fn test_insert_start() {
        let tree = ChunkTree::from_slice(b"World!", SMALL_CONFIG);
        let tree = tree.insert(0, b"Hello ");
        assert_eq!(tree.collect_bytes(0), b"Hello World!");
    }

    #[test]
    fn test_insert_end() {
        let tree = ChunkTree::from_slice(b"Hello", SMALL_CONFIG);
        let tree = tree.insert(5, b" World!");
        assert_eq!(tree.collect_bytes(0), b"Hello World!");
    }

    #[test]
    fn test_remove_middle() {
        let tree = ChunkTree::from_slice(b"Hello beautiful World!", SMALL_CONFIG);
        let tree = tree.remove(5..15);
        assert_eq!(tree.collect_bytes(0), b"Hello World!");
    }

    #[test]
    fn test_remove_start() {
        let tree = ChunkTree::from_slice(b"Hello World!", SMALL_CONFIG);
        let tree = tree.remove(0..6);
        assert_eq!(tree.collect_bytes(0), b"World!");
    }

    #[test]
    fn test_remove_end() {
        let tree = ChunkTree::from_slice(b"Hello World!", SMALL_CONFIG);
        let tree = tree.remove(5..12);
        assert_eq!(tree.collect_bytes(0), b"Hello");
    }

    #[test]
    fn test_from_slice_big_chunk() {
        let data = b"Hello World!";
        let tree = ChunkTree::from_slice(data, ChunkTreeConfig::new(15, 5));
        assert!(!tree.is_empty());
        assert_eq!(tree.len(), data.len());
        assert_eq!(tree.collect_bytes(0), b"Hello World!");
    }

    #[test]
    fn test_insert_middle_big_chunk() {
        let tree = ChunkTree::from_slice(b"Hello World!", ChunkTreeConfig::new(15, 5));
        let tree = tree.insert(5, b" beautiful");
        assert_eq!(tree.collect_bytes(0), b"Hello beautiful World!");
    }

    #[test]
    fn test_insert_start_big_chunk() {
        let tree = ChunkTree::from_slice(
            b"World!",
            ChunkTreeConfig {
                chunk_size: 15,
                max_children: 5,
            },
        );
        let tree = tree.insert(0, b"Hello ");
        assert_eq!(tree.collect_bytes(0), b"Hello World!");
    }

    #[test]
    fn test_insert_end_big_chunk() {
        let tree = ChunkTree::from_slice(
            b"Hello",
            ChunkTreeConfig {
                chunk_size: 15,
                max_children: 5,
            },
        );
        let tree = tree.insert(5, b" World!");
        assert_eq!(tree.collect_bytes(0), b"Hello World!");
    }

    #[test]
    fn test_remove_middle_big_chunk() {
        let tree = ChunkTree::from_slice(
            b"Hello beautiful World!",
            ChunkTreeConfig {
                chunk_size: 15,
                max_children: 5,
            },
        );
        let tree = tree.remove(5..15);
        assert_eq!(tree.collect_bytes(0), b"Hello World!");
    }

    #[test]
    fn test_remove_start_big_chunk() {
        let tree = ChunkTree::from_slice(
            b"Hello World!",
            ChunkTreeConfig {
                chunk_size: 15,
                max_children: 5,
            },
        );
        let tree = tree.remove(0..6);
        assert_eq!(tree.collect_bytes(0), b"World!");
    }

    #[test]
    fn test_remove_end_big_chunk() {
        let tree = ChunkTree::from_slice(
            b"Hello World!",
            ChunkTreeConfig {
                chunk_size: 15,
                max_children: 5,
            },
        );
        let tree = tree.remove(5..12);
        assert_eq!(tree.collect_bytes(0), b"Hello");
    }

    #[test]
    fn test_sparse_insert_small() {
        let tree = ChunkTree::from_slice(b"Hello", SMALL_CONFIG);
        let tree = tree.insert(6, b" World!");
        assert_eq!(tree.len(), 13);
    }

    #[test]
    fn test_sparse_insert() {
        for chunk_size in 1..15 {
            for max_children in 1..10 {
                let tree = ChunkTree::from_slice(
                    b"Hello",
                    ChunkTreeConfig {
                        chunk_size,
                        max_children,
                    },
                );
                let tree = tree.insert(6, b" World!");
                assert_eq!(tree.len(), 13);
                assert_eq!(tree.collect_bytes(b'X'), b"HelloX World!");
            }
        }
    }

    #[test]
    fn test_sparse_insert_remove() {
        for chunk_size in 1..15 {
            for max_children in 1..10 {
                let tree = ChunkTree::from_slice(
                    b"Hello",
                    ChunkTreeConfig {
                        chunk_size,
                        max_children,
                    },
                );
                let tree = tree.insert(6, b" World!");
                assert_eq!(tree.len(), 13);
                assert_eq!(tree.collect_bytes(b'X'), b"HelloX World!");

                let tree = tree.remove(4..7);
                assert_eq!(tree.collect_bytes(b'X'), b"HellWorld!");
                assert_eq!(tree.len(), 10);
            }
        }
    }

    #[test]
    fn test_remove_beyond_end_small() {
        let tree = ChunkTree::from_slice(b"Hello", SMALL_CONFIG);
        let tree = tree.remove(3..6);
        assert_eq!(tree.len(), 3);
        assert_eq!(tree.collect_bytes(0), b"Hel");
    }

    #[test]
    fn test_remove_beyond_end() {
        let tree = ChunkTree::from_slice(
            b"Hello",
            ChunkTreeConfig {
                chunk_size: 15,
                max_children: 5,
            },
        );
        let tree = tree.remove(3..8);
        assert_eq!(tree.len(), 3);
        assert_eq!(tree.collect_bytes(0), b"Hel");
    }

    #[test]
    fn test_insert_all_ranges() {
        let initial = b"Hello World!";
        for chunk_size in 1..15 {
            for max_children in 1..10 {
                let tree = ChunkTree::from_slice(
                    initial,
                    ChunkTreeConfig {
                        chunk_size,
                        max_children,
                    },
                );
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
        }
    }

    #[test]
    fn test_remove_all_ranges() {
        let initial = b"Hello World!";
        for chunk_size in 1..15 {
            for max_children in 1..15 {
                let tree = ChunkTree::from_slice(
                    initial,
                    ChunkTreeConfig {
                        chunk_size,
                        max_children,
                    },
                );
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
    }
    #[test]
    fn test_iterator() {
        // Empty tree
        let empty_tree = ChunkTreeNode::empty();
        let mut iter = empty_tree.iter();
        assert_eq!(iter.next(), None);

        // Simple leaf node
        let leaf = ChunkTreeNode::from_slice(b"abc", SMALL_CONFIG);
        let mut iter = leaf.iter();
        assert_eq!(iter.next(), Some(ChunkPiece::Data { data: b"a" }));
        assert_eq!(iter.next(), Some(ChunkPiece::Data { data: b"bc" }));
        assert_eq!(iter.next(), None);

        // Gap node
        let gap: ChunkTreeNode<'_> = ChunkTreeNode::Gap { size: 3 };
        let mut iter = gap.iter();
        assert_eq!(iter.next(), Some(ChunkPiece::Gap { size: 3 }));
        assert_eq!(iter.next(), None);

        // Complex tree with internal nodes
        let tree = ChunkTreeNode::from_slice(b"Hello", SMALL_CONFIG);
        let tree = tree.insert(5, b" World!", SMALL_CONFIG);

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
        for chunk_size in 1..15 {
            for max_children in 1..15 {
                let tree = ChunkTree::new(ChunkTreeConfig {
                    chunk_size: chunk_size,
                    max_children: max_children,
                });
                let tree = tree.insert(1, b"the end");
                let tree = tree.insert(0, b"start");
                assert_eq!(tree.collect_bytes(b'_'), b"start_the end");
            }
        }
    }

    #[test]
    fn test_complex_sparse_operations() {
        for chunk_size in 1..30 {
            for max_children in 1..15 {
                let config = ChunkTreeConfig {
                    chunk_size,
                    max_children,
                };
                let tree = ChunkTree::new(config);

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
        }
    }

    #[test]
    fn test_internal_node_edge_cases() {
        let tree = ChunkTree::from_slice(b"abcdef", SMALL_CONFIG);

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
        let tree = ChunkTree::new(ChunkTreeConfig {
            chunk_size: 10,
            max_children: 10,
        });
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
        let _config = ChunkTreeConfig::new(0, 1);
    }
}
