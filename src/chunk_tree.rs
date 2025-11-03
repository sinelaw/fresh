//! A rope-like data structure implemented as a tree for efficient text manipulation.
//!
//! # Overview
//! `ChunkTree` is an immutable, persistent data structure that represents text as a tree of chunks,
//! allowing for efficient insert and remove operations. Each node in the tree can be a leaf containing
//! actual data, a gap representing empty space, or an internal node with multiple children.
//!
//! # Type Parameters
//! - `'a`: Lifetime parameter for the stored data
//!
//! # Configuration
//! The tree's behavior is controlled by `ChunkTreeConfig`:
//! - `chunk_size`: Maximum size of leaf chunks
//! - `max_children`: Maximum number of children for internal nodes
//!
//! # Examples
//! ```
//! use editor::chunk_tree::{ChunkTree, ChunkTreeConfig};
//!
//! let config = ChunkTreeConfig::new(4, 4);
//! let tree = ChunkTree::new(config);
//! let tree = tree.insert(0, b"Hello");      // Creates a new tree, original remains unchanged
//! let tree = tree.insert(5, b" World!");    // Creates another new version
//! assert_eq!(tree.collect_bytes(b'_'), b"Hello World!");
//!
//! // Remove some content (creates new version)
//! let tree = tree.remove(5..11);
//! assert_eq!(tree.collect_bytes(b'_'), b"Hello!");
//!
//! // Sparse insert (creates gaps)
//! let tree = tree.insert(10, b"far away");  // Inserts with gaps
//! assert_eq!(tree.collect_bytes(b'_'), b"Hello!____far away");
//! ```
//!
//! # Implementation Details
//! The tree maintains the following invariants:
//! - Leaf nodes contain arbitrary-sized byte slices up to chunk_size
//! - Gap nodes represent empty spaces efficiently
//! - Internal nodes contain multiple children and track total size
//! - All operations create new nodes instead of modifying existing ones
//! - Unchanged subtrees are shared between versions through Arc
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

// Removed logs module dependency

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
        assert!(max_children > 2);
        ChunkTreeConfig {
            chunk_size,
            max_children,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ChunkPiece<'a> {
    Data { data: &'a [u8] },
    Gap { size: usize },
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

    fn get(&self, index: usize) -> ChunkPiece<'a> {
        assert!(index < self.len());
        match self {
            ChunkTreeNode::Leaf { data } => ChunkPiece::Data {
                data: &data[index..],
            },
            ChunkTreeNode::Gap { size } => ChunkPiece::Gap { size: size - index },
            ChunkTreeNode::Internal { children, size: _ } => {
                let mut cur_offset = 0;
                for child in children {
                    let next_offset = cur_offset + child.len();
                    if index < next_offset {
                        return child.get(index - cur_offset);
                    }
                    cur_offset = next_offset;
                }
                panic!("out of range index should have been caught by assert above");
            }
        }
    }

    /// Concatenates two trees with optional gap
    fn append(
        &self,
        gap_size: usize,
        other: Arc<ChunkTreeNode<'a>>,
        config: ChunkTreeConfig,
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
                assert!(children.len() <= config.max_children);
                let mut new_children = children.clone();
                if gap_size > 0 {
                    new_children.push(Arc::new(ChunkTreeNode::Gap { size: gap_size }));
                }
                new_children.push(other);
                Self::build_internal_node(config, size + gap_size + other_len, new_children)
            }
        }
    }

    /// Fills gaps with given data starting at 'index'
    ///
    /// panics if `index > self.len()` or `index + data.len() > self.len()`
    /// panics if data.is_empty()
    fn fill(&self, index: usize, data: &'a [u8], config: ChunkTreeConfig) -> ChunkTreeNode<'a> {
        assert!(index <= self.len());
        assert!(index + data.len() <= self.len());
        assert!(!data.is_empty());

        match self {
            ChunkTreeNode::Leaf { data: leaf_data } => ChunkTreeNode::Leaf { data: leaf_data },
            ChunkTreeNode::Gap { size } => {
                let mut children = Vec::new();
                if index > 0 {
                    children.push(Arc::new(ChunkTreeNode::Gap { size: index }));
                }
                children.push(Arc::new(Self::from_slice(data, config)));
                let end = index + data.len();
                if end < *size {
                    children.push(Arc::new(ChunkTreeNode::Gap { size: size - end }));
                }
                ChunkTreeNode::Internal {
                    children,
                    size: *size,
                }
            }
            ChunkTreeNode::Internal { children, size } => {
                let mut current_pos = 0;

                let mut new_children = Vec::new();

                for child in children {
                    let child_len = child.len();
                    let child_pos = current_pos;

                    current_pos += child_len;

                    // Child before index
                    if child_pos + child_len <= index {
                        new_children.push(child.clone());
                        continue;
                    }
                    // Already finished filling up, rest of children left as-is
                    if child_pos >= index + data.len() {
                        new_children.push(child.clone());
                        continue;
                    }

                    // child overlaps fill range
                    let child_relative_index = index.saturating_sub(child_pos);
                    let data_index = child_pos.saturating_sub(index);
                    let data_end =
                        std::cmp::min(data.len(), data_index + child.len() - child_relative_index);
                    if data_index >= data_end {
                        new_children.push(child.clone());
                        continue;
                    }
                    let data_slice = &data[data_index..data_end];
                    let new_child = child.fill(child_relative_index, data_slice, config);
                    new_children.push(Arc::new(new_child));
                }

                ChunkTreeNode::Internal {
                    children: new_children,
                    size: *size,
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
                assert!(children.len() <= config.max_children);
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

                Self::build_internal_node(config, size + data.len(), new_children)
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

        if self.is_empty() {
            return ChunkTreeNode::empty();
        }

        // Debug logging removed
        // log!("range: {:?}", range);

        match self {
            ChunkTreeNode::Leaf { data } => ChunkTreeNode::Internal {
                children: vec![
                    Arc::new(Self::from_slice(&data[..range.start], config)),
                    Arc::new(Self::from_slice(&data[range.end..], config)),
                ],
                size: data.len() - range.len(),
            },
            ChunkTreeNode::Gap { size } => ChunkTreeNode::Gap {
                size: *size - range.len(),
            },
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
                    // Debug logging removed
                    // log!(
                    //     "remaining_range: {:?}, remove_relative_range: {:?}, child: {:?}",
                    //     remaining_range,
                    //     remove_relative_range,
                    //     child
                    // );
                    let new_child = child.remove(remove_relative_range, config);
                    if !new_child.is_empty() {
                        new_children.push(Arc::new(new_child));
                    }
                    // Adjust remaining range
                    remaining_range.start = end;
                }

                if new_children.len() == 1 {
                    return (*new_children[0]).clone();
                }

                ChunkTreeNode::Internal {
                    children: new_children,
                    size: size - range.len(),
                }
            }
        }
    }

    fn build_internal_node(
        config: ChunkTreeConfig,
        size: usize,
        new_children: Vec<Arc<ChunkTreeNode<'a>>>,
    ) -> ChunkTreeNode<'a> {
        if new_children.len() <= config.max_children {
            ChunkTreeNode::Internal {
                children: new_children,
                size,
            }
        } else {
            // need to split the children between two nodes
            let mid = new_children.len() / 2;
            let left_children = new_children[..mid].to_vec();
            let right_children = new_children[mid..].to_vec();
            let left_size: usize = left_children.iter().map(|c| c.len()).sum();
            let right_size: usize = right_children.iter().map(|c| c.len()).sum();
            assert!(left_size + right_size == size);
            ChunkTreeNode::Internal {
                children: vec![
                    Arc::new(ChunkTreeNode::Internal {
                        children: left_children,
                        size: left_size,
                    }),
                    Arc::new(ChunkTreeNode::Internal {
                        children: right_children,
                        size: right_size,
                    }),
                ],
                size,
            }
        }
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

impl<'a> Iterator for ChunkTreeIterator<'a> {
    type Item = ChunkPiece<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((node, child_idx)) = self.stack.pop() {
            if node.is_empty() {
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
                        panic!("invalid child_idx: {child_idx:?}");
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

    pub fn get(&self, index: usize) -> ChunkPiece<'a> {
        self.root.get(index)
    }

    /// Fills gaps with given data starting at 'index' (inserting if tree.len() is surpassed)
    pub fn fill(&self, index: usize, data: &'a [u8]) -> ChunkTree<'a> {
        if data.is_empty() {
            ChunkTree {
                root: self.root.clone(),
                config: self.config,
            }
        } else if index <= self.len() {
            let data_end = std::cmp::min(self.root.len() - index, data.len());
            let filled = self.root.fill(index, &data[..data_end], self.config);
            assert_eq!(filled.len(), self.root.len());
            let new_root = if index + data.len() <= self.root.len() {
                filled
            } else {
                filled.append(
                    0,
                    Arc::new(ChunkTreeNode::from_slice(
                        &data[(self.root.len() - index)..],
                        self.config,
                    )),
                    self.config,
                )
            };
            ChunkTree {
                root: Arc::new(new_root),
                config: self.config,
            }
        } else {
            // sparse fill
            ChunkTree {
                root: Arc::new(self.root.append(
                    index - self.len(),
                    Arc::new(ChunkTreeNode::from_slice(data, self.config)),
                    self.config,
                )),
                config: self.config,
            }
        }
    }

    pub fn insert(&self, index: usize, data: &'a [u8]) -> ChunkTree<'a> {
        if data.is_empty() {
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
                    Arc::new(ChunkTreeNode::from_slice(data, self.config)),
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

    /// Returns an iterator over chunks in the tree
    pub fn iter(&self) -> ChunkTreeIterator {
        self.root.iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SMALL_CONFIG: ChunkTreeConfig = ChunkTreeConfig::new(2, 3);

    #[test]
    fn test_empty_tree() {
        let tree = ChunkTree::new(SMALL_CONFIG);
        assert!(tree.is_empty());
        assert_eq!(tree.len(), 0);
        assert_eq!(tree.collect_bytes(0), Vec::<u8>::new());
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
        println!("tree: {tree:?}");
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
        println!("tree: {tree:?}");
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
    fn test_remove_all_piecewise() {
        let tree = ChunkTree::from_slice(b"Hello World!", SMALL_CONFIG);
        let tree = tree.remove(0..2);
        let tree = tree.remove(0..2);
        let tree = tree.remove(0..2);
        let tree = tree.remove(0..2);
        let tree = tree.remove(0..2);
        let tree = tree.remove(0..2);
        assert_eq!(tree.collect_bytes(0), b"");
        assert_eq!(tree.len(), 0);
    }

    #[test]
    fn test_remove_all() {
        let tree = ChunkTree::from_slice(b"Hello World!", SMALL_CONFIG);
        let tree = tree.remove(0..12);
        assert_eq!(tree.collect_bytes(0), b"");
        assert_eq!(tree.len(), 0);
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
        let tree = ChunkTree::from_slice(b"World!", ChunkTreeConfig::new(15, 5));
        let tree = tree.insert(0, b"Hello ");
        assert_eq!(tree.collect_bytes(0), b"Hello World!");
    }

    #[test]
    fn test_insert_end_big_chunk() {
        let tree = ChunkTree::from_slice(b"Hello", ChunkTreeConfig::new(15, 5));
        let tree = tree.insert(5, b" World!");
        assert_eq!(tree.collect_bytes(0), b"Hello World!");
    }

    #[test]
    fn test_remove_middle_big_chunk() {
        let tree = ChunkTree::from_slice(b"Hello beautiful World!", ChunkTreeConfig::new(15, 5));
        let tree = tree.remove(5..15);
        assert_eq!(tree.collect_bytes(0), b"Hello World!");
    }

    #[test]
    fn test_remove_start_big_chunk() {
        let tree = ChunkTree::from_slice(b"Hello World!", ChunkTreeConfig::new(15, 5));
        let tree = tree.remove(0..6);
        assert_eq!(tree.collect_bytes(0), b"World!");
    }

    #[test]
    fn test_remove_end_big_chunk() {
        let tree = ChunkTree::from_slice(b"Hello World!", ChunkTreeConfig::new(15, 5));
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
            for max_children in 3..10 {
                let tree =
                    ChunkTree::from_slice(b"Hello", ChunkTreeConfig::new(chunk_size, max_children));
                let tree = tree.insert(6, b" World!");
                assert_eq!(tree.len(), 13);
                assert_eq!(tree.collect_bytes(b'X'), b"HelloX World!");
            }
        }
    }

    #[test]
    fn test_sparse_insert_remove() {
        for chunk_size in 1..15 {
            for max_children in 3..10 {
                let tree =
                    ChunkTree::from_slice(b"Hello", ChunkTreeConfig::new(chunk_size, max_children));
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
        let tree = ChunkTree::from_slice(b"Hello", ChunkTreeConfig::new(15, 5));
        let tree = tree.remove(3..8);
        assert_eq!(tree.len(), 3);
        assert_eq!(tree.collect_bytes(0), b"Hel");
    }

    #[test]
    fn test_insert_all_ranges() {
        let initial = b"Hello World!";
        for chunk_size in 1..15 {
            for max_children in 3..10 {
                let tree =
                    ChunkTree::from_slice(initial, ChunkTreeConfig::new(chunk_size, max_children));
                for pos in 0..=initial.len() {
                    for len in 0..=initial.len() {
                        let data = &"0123456789abcdefgh".as_bytes()[0..len];

                        // Test insert
                        let mut reference = Vec::from(&initial[..]);
                        reference.splice(pos..pos, data.iter().cloned());
                        let modified_tree = tree.insert(pos, data);
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
            for max_children in 3..15 {
                let tree =
                    ChunkTree::from_slice(initial, ChunkTreeConfig::new(chunk_size, max_children));
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

        let expected = [
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
            println!("index: {index}, actual: {a:?}, expected: {e:?}");
            assert_eq!(a, e);
        }
        println!("actual: {actual:?}");
        assert_eq!(actual.len(), expected.len());
    }

    #[test]
    fn test_insert_sparse() {
        for chunk_size in 1..15 {
            for max_children in 3..15 {
                let tree = ChunkTree::new(ChunkTreeConfig::new(chunk_size, max_children));
                let tree = tree.insert(1, b"the end");
                let tree = tree.insert(0, b"start");
                assert_eq!(tree.collect_bytes(b'_'), b"start_the end");
            }
        }
    }

    #[test]
    fn test_complex_sparse_operations() {
        for chunk_size in 1..30 {
            for max_children in 3..15 {
                let config = ChunkTreeConfig::new(chunk_size, max_children);
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
                println!("tree: {tree:?}");
                assert_eq!(tree.collect_bytes(b'_'), b"_____llo");

                // Test complex insert chain
                let tree = tree.insert(2, b"ABC");
                println!("tree: {tree:?}");
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
        println!("tree: {tree:?}");
        let tree = tree.insert(5, b"middle");
        println!("tree: {tree:?}");
        let tree = tree.insert(0, b"start");
        println!("tree: {tree:?}");
        let tree = tree.insert(20, b"end");
        println!("tree: {tree:?}");

        let pieces: Vec<ChunkPiece> = tree.root.iter().collect();
        assert!(!pieces.is_empty());

        // Verify the structure contains expected data and gaps
        let mut found_start = false;
        let mut found_middle = false;
        let mut found_end = false;

        for piece in pieces {
            match piece {
                ChunkPiece::Data { data } => {
                    let str = String::from_utf8_lossy(data);
                    println!("data: {str:?}");
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

    #[test]
    fn test_get_empty() {
        let tree = ChunkTree::new(SMALL_CONFIG);

        // Empty tree should panic on get
        let result = std::panic::catch_unwind(|| {
            tree.get(0);
        });
        assert!(result.is_err());
    }

    #[test]
    fn test_get() {
        // Test get on simple tree
        let tree = ChunkTree::from_slice(b"Hello", SMALL_CONFIG);
        assert_eq!(tree.get(0), ChunkPiece::Data { data: b"He" });
        assert_eq!(tree.get(2), ChunkPiece::Data { data: b"l" });

        // Test get on tree with gaps
        let tree = tree.insert(7, b"World");
        assert_eq!(tree.get(5), ChunkPiece::Gap { size: 2 });
        assert_eq!(tree.get(7), ChunkPiece::Data { data: b"Wo" });

        // Test get on complex tree
        let tree = ChunkTree::new(ChunkTreeConfig::new(100, 3))
            .insert(0, b"start")
            .insert(10, b"middle")
            .insert(20, b"end");

        assert_eq!(tree.get(0), ChunkPiece::Data { data: b"start" });
        assert_eq!(tree.get(5), ChunkPiece::Gap { size: 5 });
        assert_eq!(tree.get(10), ChunkPiece::Data { data: b"middle" });
        assert_eq!(tree.get(16), ChunkPiece::Gap { size: 4 });
        assert_eq!(tree.get(20), ChunkPiece::Data { data: b"end" });

        // Out of bounds should panic
        let result = std::panic::catch_unwind(|| {
            tree.get(100);
        });
        assert!(result.is_err());
    }
    #[test]
    fn test_fill_basic() {
        let tree = ChunkTree::from_slice(b"abcdef", SMALL_CONFIG);
        let tree = tree.insert(10, b"xyz");
        assert_eq!(tree.collect_bytes(b'_'), b"abcdef____xyz");
        let tree = tree.fill(6, b"123");
        assert_eq!(tree.collect_bytes(b'_'), b"abcdef123_xyz");
    }

    #[test]
    fn test_fill_start_of_gap() {
        let tree = ChunkTree::from_slice(b"abc", SMALL_CONFIG);
        let tree = tree.insert(5, b"xyz");
        assert_eq!(tree.collect_bytes(b'_'), b"abc__xyz");
        let tree = tree.fill(3, b"12");
        assert_eq!(tree.collect_bytes(b'_'), b"abc12xyz");
    }

    #[test]
    fn test_fill_end_of_gap() {
        let tree = ChunkTree::from_slice(b"abc", SMALL_CONFIG);
        let tree = tree.insert(5, b"xyz");
        assert_eq!(tree.collect_bytes(b'_'), b"abc__xyz");
        let tree = tree.fill(4, b"12");
        assert_eq!(tree.collect_bytes(b'_'), b"abc_1xyz");
    }

    #[test]
    fn test_fill_entire_gap() {
        let tree = ChunkTree::from_slice(b"abc", SMALL_CONFIG);
        let tree = tree.insert(5, b"xyz");
        let tree = tree.fill(3, b"12");
        assert_eq!(tree.collect_bytes(b'_'), b"abc12xyz");
    }

    #[test]
    fn test_fill_multiple_gaps() {
        let tree = ChunkTree::new(SMALL_CONFIG)
            .insert(2, b"ab")
            .insert(6, b"cd")
            .insert(10, b"ef");
        assert_eq!(tree.collect_bytes(b'_'), b"__ab__cd__ef");
        let tree = tree.fill(0, b"123456789");
        assert_eq!(tree.collect_bytes(b'_'), b"12ab56cd9_ef");
    }

    #[test]
    #[should_panic]
    fn test_fill_sparse_index() {
        let tree = ChunkTree::from_slice(b"abc", SMALL_CONFIG);
        // Fill beyond length
        tree.fill(4, b"xyz");
        assert_eq!(tree.collect_bytes(b'_'), b"abc_xyz");
    }

    #[test]
    fn test_fill_beyond_end() {
        // Test invalid fills
        let tree = ChunkTree::from_slice(b"abc", SMALL_CONFIG);
        // Fill that would overflow length should panic
        let tree = tree.fill(2, b"toolong");
        assert_eq!(tree.collect_bytes(b'_'), b"abcoolong");
    }

    #[test]
    fn test_fill_empty() {
        let tree = ChunkTree::from_slice(b"abc", SMALL_CONFIG);
        let tree = tree.fill(0, b"");
        assert_eq!(tree.collect_bytes(b'_'), b"abc");
    }
}
