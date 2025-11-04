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
#[derive(Debug, Clone)]
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

    /// Create a byte-level iterator over a range
    /// This is much more efficient than calling get(i) repeatedly
    /// The iterator can be reversed with .rev() to iterate backwards
    pub fn bytes_range(&self, start: usize, end: usize) -> ByteRangeIterator {
        ByteRangeIterator::new(self, start, end)
    }

    /// Create a byte-level iterator starting from a given byte offset to EOF
    pub fn bytes_from(&self, start_offset: usize) -> ByteRangeIterator {
        ByteRangeIterator::new(self, start_offset, self.len())
    }

    /// Create a bidirectional byte iterator positioned at a specific offset
    /// Can navigate both forward (via next()) and backward (via next_back()) from this position
    /// This is efficient (O(log n)) as it only collects chunks as needed during iteration
    pub fn bytes_at(&self, position: usize) -> ByteIterator {
        ByteIterator::at_position(self, position)
    }
}

/// Iterator that yields individual bytes from the chunk tree over a range
/// Efficiently handles chunk boundaries by caching the current chunk
/// Implements DoubleEndedIterator so you can use .rev() to iterate backwards
pub struct ByteRangeIterator<'a> {
    // Cached chunks for efficient iteration
    chunks: Vec<(usize, &'a [u8])>, // (start_position, data)

    // Current iteration state
    front_chunk_idx: usize,
    front_offset: usize,

    back_chunk_idx: usize,
    back_offset: usize,
}

/// A cursor that maintains a position in the tree for efficient navigation
/// Tracks the path from root to current position, allowing O(1) local moves
struct TreeCursor<'a> {
    // Stack of (node, child_index, position_at_node_start)
    // This represents the path from root to current position
    stack: Vec<(&'a ChunkTreeNode<'a>, usize, usize)>,
    // Current absolute byte position in the tree
    position: usize,
    // Total tree length
    tree_len: usize,
}

impl<'a> TreeCursor<'a> {
    /// Create a cursor positioned at the given byte offset
    fn new(root: &'a ChunkTreeNode<'a>, position: usize) -> Self {
        let tree_len = root.len();
        let position = position.min(tree_len);

        let mut cursor = Self {
            stack: Vec::new(),
            position,
            tree_len,
        };

        // Build initial path to the position
        cursor.seek_to(root, position);
        cursor
    }

    /// Seek to a specific position by rebuilding the stack
    fn seek_to(&mut self, root: &'a ChunkTreeNode<'a>, target: usize) {
        self.stack.clear();
        self.position = target.min(self.tree_len);

        if self.position >= self.tree_len {
            return;
        }

        // Walk down the tree to find the target position
        let mut node = root;
        let mut pos_at_node_start = 0;

        loop {
            match node {
                ChunkTreeNode::Leaf { .. } | ChunkTreeNode::Gap { .. } => {
                    // Reached a leaf/gap, we're done
                    self.stack.push((node, 0, pos_at_node_start));
                    return;
                }
                ChunkTreeNode::Internal { children, .. } => {
                    let mut current_pos = pos_at_node_start;

                    for (child_idx, child) in children.iter().enumerate() {
                        let child_end = current_pos + child.len();

                        if target < child_end {
                            // Target is in this child
                            self.stack.push((node, child_idx, pos_at_node_start));
                            node = child.as_ref();
                            pos_at_node_start = current_pos;
                            break;
                        }

                        current_pos = child_end;
                    }
                }
            }
        }
    }

    /// Get the current byte without advancing
    fn current(&self) -> Option<u8> {
        if self.position >= self.tree_len {
            return None;
        }

        // Get the leaf node from the bottom of the stack
        if let Some((node, _, node_start)) = self.stack.last() {
            let offset = self.position - node_start;

            match node {
                ChunkTreeNode::Leaf { data } => {
                    if offset < data.len() {
                        return Some(data[offset]);
                    }
                }
                ChunkTreeNode::Gap { .. } => {
                    return Some(b' ');
                }
                ChunkTreeNode::Internal { .. } => {
                    // Should not happen - leaf should be at bottom
                }
            }
        }

        None
    }

    /// Move forward one byte
    fn advance(&mut self, root: &'a ChunkTreeNode<'a>) -> bool {
        if self.position >= self.tree_len {
            return false;
        }

        self.position += 1;

        if self.position >= self.tree_len {
            return false;
        }

        // Check if we're still in the current leaf/gap
        if let Some((node, _, node_start)) = self.stack.last() {
            let node_end = node_start + node.len();
            if self.position < node_end {
                // Still in same node
                return true;
            }
        }

        // Need to move to next chunk - rebuild path
        self.seek_to(root, self.position);
        true
    }

    /// Move backward one byte
    fn retreat(&mut self, root: &'a ChunkTreeNode<'a>) -> bool {
        if self.position == 0 {
            return false;
        }

        self.position -= 1;

        // Check if we're still in the current leaf/gap
        if let Some((_node, _, node_start)) = self.stack.last() {
            if self.position >= *node_start {
                // Still in same node
                return true;
            }
        }

        // Need to move to previous chunk - rebuild path
        self.seek_to(root, self.position);
        true
    }
}

/// Seekable, bidirectional byte iterator
/// Can be positioned at any byte offset and navigate forward/backward efficiently
pub struct ByteIterator<'a> {
    tree: &'a ChunkTree<'a>,
    cursor: TreeCursor<'a>,
}

impl<'a> ByteIterator<'a> {
    /// Create a new byte iterator positioned at the given offset
    fn at_position(tree: &'a ChunkTree<'a>, position: usize) -> Self {
        let cursor = TreeCursor::new(&tree.root, position);
        Self { tree, cursor }
    }

    /// Get the current position
    pub fn position(&self) -> usize {
        self.cursor.position
    }

    /// Get the next byte, advancing the position
    pub fn next(&mut self) -> Option<u8> {
        let byte = self.cursor.current()?;
        self.cursor.advance(&self.tree.root);
        Some(byte)
    }

    /// Get the previous byte, moving backward
    pub fn prev(&mut self) -> Option<u8> {
        if !self.cursor.retreat(&self.tree.root) {
            return None;
        }
        self.cursor.current()
    }

    /// Peek at the current byte without advancing
    pub fn peek(&self) -> Option<u8> {
        self.cursor.current()
    }

    /// Seek to a specific position
    pub fn seek(&mut self, position: usize) {
        self.cursor.seek_to(&self.tree.root, position);
    }
}

impl<'a> ByteRangeIterator<'a> {
    fn new(tree: &'a ChunkTree<'a>, start: usize, end: usize) -> Self {
        let start = start.min(tree.len());
        let end = end.min(tree.len());

        // Collect all chunks in the range using tree navigation
        let mut chunks = Vec::new();
        Self::collect_chunks_in_range(&tree.root, start, end, 0, &mut chunks);

        let back_chunk_idx = chunks.len().saturating_sub(1);
        let back_offset = chunks
            .get(back_chunk_idx)
            .map(|(_, data)| data.len())
            .unwrap_or(0);

        Self {
            chunks,
            front_chunk_idx: 0,
            front_offset: 0,
            back_chunk_idx,
            back_offset,
        }
    }

    /// Create an iterator positioned at a specific byte offset
    /// This collects all chunks in the file and positions the iterator at the given offset
    fn at_position(tree: &'a ChunkTree<'a>, position: usize) -> Self {
        let position = position.min(tree.len());

        // Collect all chunks in the entire file
        let mut chunks = Vec::new();
        Self::collect_chunks_in_range(&tree.root, 0, tree.len(), 0, &mut chunks);

        // Find which chunk contains the position and set up front/back cursors there
        let mut front_chunk_idx = 0;
        let mut front_offset = 0;

        for (idx, (chunk_start, chunk_data)) in chunks.iter().enumerate() {
            let chunk_end = chunk_start + chunk_data.len();
            if position >= *chunk_start && position < chunk_end {
                front_chunk_idx = idx;
                front_offset = position - chunk_start;
                break;
            }
            if position <= *chunk_start {
                front_chunk_idx = idx;
                front_offset = 0;
                break;
            }
        }

        // Back cursor starts at the end
        let back_chunk_idx = chunks.len().saturating_sub(1);
        let back_offset = chunks
            .get(back_chunk_idx)
            .map(|(_, data)| data.len())
            .unwrap_or(0);

        Self {
            chunks,
            front_chunk_idx,
            front_offset,
            back_chunk_idx,
            back_offset,
        }
    }

    /// Recursively collect chunks that overlap with the range [start, end)
    fn collect_chunks_in_range(
        node: &'a ChunkTreeNode<'a>,
        start: usize,
        end: usize,
        node_start: usize,
        chunks: &mut Vec<(usize, &'a [u8])>,
    ) {
        let node_end = node_start + node.len();

        // Skip entirely if this node is before the range
        if node_end <= start {
            return;
        }

        // Skip entirely if this node is after the range
        if node_start >= end {
            return;
        }

        match node {
            ChunkTreeNode::Leaf { data } => {
                // This leaf overlaps with the range
                let chunk_start_in_range = node_start.max(start);
                let chunk_end_in_range = node_end.min(end);

                let data_start = chunk_start_in_range - node_start;
                let data_end = chunk_end_in_range - node_start;

                chunks.push((chunk_start_in_range, &data[data_start..data_end]));
            }
            ChunkTreeNode::Gap { .. } => {
                // Gaps don't contain data, skip them
            }
            ChunkTreeNode::Internal { children, .. } => {
                // Recursively process children that might overlap
                let mut current_pos = node_start;

                for child in children {
                    let child_size = child.len();
                    let child_end = current_pos + child_size;

                    // Only recurse into children that might overlap with the range
                    if child_end > start && current_pos < end {
                        Self::collect_chunks_in_range(child, start, end, current_pos, chunks);
                    }

                    current_pos = child_end;

                    // Early exit if we've passed the range
                    if current_pos >= end {
                        break;
                    }
                }
            }
        }
    }
}

impl<'a> Iterator for ByteRangeIterator<'a> {
    type Item = (usize, u8); // (byte_position, byte_value)

    fn next(&mut self) -> Option<Self::Item> {
        // Check if we've exhausted the iterator
        if self.front_chunk_idx > self.back_chunk_idx {
            return None;
        }

        if self.front_chunk_idx == self.back_chunk_idx && self.front_offset >= self.back_offset {
            return None;
        }

        // Get current chunk
        let (chunk_start, chunk_data) = self.chunks.get(self.front_chunk_idx)?;

        if self.front_offset < chunk_data.len() {
            let byte = chunk_data[self.front_offset];
            let pos = chunk_start + self.front_offset;
            self.front_offset += 1;
            return Some((pos, byte));
        }

        // Move to next chunk
        self.front_chunk_idx += 1;
        self.front_offset = 0;
        self.next()
    }
}

impl<'a> DoubleEndedIterator for ByteRangeIterator<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        // Check if we've exhausted the iterator
        if self.front_chunk_idx > self.back_chunk_idx {
            return None;
        }

        if self.front_chunk_idx == self.back_chunk_idx && self.front_offset >= self.back_offset {
            return None;
        }

        // Get current chunk from back
        let (chunk_start, chunk_data) = self.chunks.get(self.back_chunk_idx)?;

        if self.back_offset > 0 {
            self.back_offset -= 1;
            let byte = chunk_data[self.back_offset];
            let pos = chunk_start + self.back_offset;
            return Some((pos, byte));
        }

        // Move to previous chunk
        if self.back_chunk_idx == 0 {
            return None;
        }
        self.back_chunk_idx -= 1;
        let (_, prev_chunk_data) = self.chunks.get(self.back_chunk_idx)?;
        self.back_offset = prev_chunk_data.len();
        self.next_back()
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

    #[test]
    fn test_byte_iterator_from_start() {
        let tree = ChunkTree::from_slice(b"Hello World!", SMALL_CONFIG);
        let bytes: Vec<u8> = tree.bytes_from(0).map(|(_, b)| b).collect();
        assert_eq!(bytes, b"Hello World!");
    }

    #[test]
    fn test_byte_iterator_from_middle() {
        let tree = ChunkTree::from_slice(b"Hello World!", SMALL_CONFIG);
        let bytes: Vec<u8> = tree.bytes_from(6).map(|(_, b)| b).collect();
        assert_eq!(bytes, b"World!");
    }

    #[test]
    fn test_byte_iterator_positions() {
        let tree = ChunkTree::from_slice(b"Hello", SMALL_CONFIG);
        let positions: Vec<(usize, u8)> = tree.bytes_from(2).collect();
        assert_eq!(positions, vec![(2, b'l'), (3, b'l'), (4, b'o')]);
    }

    #[test]
    fn test_bytes_range_performance_near_end() {
        // Create a large tree by inserting many small chunks
        // This simulates the real-world case where a 61MB file has many chunks
        let config = ChunkTreeConfig::new(4096, 16);
        let mut tree = ChunkTree::new(config);

        // Insert 10,000 chunks of 1KB each = ~10MB
        let chunk = vec![b'x'; 1024];
        for i in 0..10_000 {
            tree = tree.insert(i * 1024, &chunk);
        }

        let total_len = tree.len();

        // Now test creating a range iterator near the END of the tree
        // This should be fast (O(log n) tree traversal), not O(n) iteration
        let range_start = total_len.saturating_sub(100_000);
        let range_end = total_len;

        let iter = tree.bytes_range(range_start, range_end);

        // Verify the iterator actually works
        let count = iter.count();
        assert_eq!(count, range_end - range_start);
    }

    #[test]
    fn test_bytes_range_skips_chunks_correctly() {
        // Create a tree with multiple chunks
        let config = ChunkTreeConfig::new(10, 4);
        let tree = ChunkTree::from_slice(b"0123456789abcdefghijklmnopqrstuvwxyz", config);

        // Request a range in the middle - should only collect overlapping chunks
        let iter = tree.bytes_range(10, 20);
        let bytes: Vec<u8> = iter.map(|(_, b)| b).collect();

        assert_eq!(bytes, b"abcdefghij");
    }

    #[test]
    fn test_byte_iterator_forward() {
        let config = ChunkTreeConfig::new(4, 4);
        let tree = ChunkTree::from_slice(b"Hello World!", config);

        let mut iter = tree.bytes_at(0);

        assert_eq!(iter.position(), 0);
        assert_eq!(iter.next(), Some(b'H'));
        assert_eq!(iter.next(), Some(b'e'));
        assert_eq!(iter.next(), Some(b'l'));
        assert_eq!(iter.position(), 3);
    }

    #[test]
    fn test_byte_iterator_backward() {
        let config = ChunkTreeConfig::new(4, 4);
        let tree = ChunkTree::from_slice(b"Hello World!", config);

        let mut iter = tree.bytes_at(5);

        assert_eq!(iter.position(), 5);
        assert_eq!(iter.prev(), Some(b'o'));
        assert_eq!(iter.prev(), Some(b'l'));
        assert_eq!(iter.prev(), Some(b'l'));
        assert_eq!(iter.position(), 2);
    }

    #[test]
    fn test_byte_iterator_bidirectional() {
        let config = ChunkTreeConfig::new(4, 4);
        let tree = ChunkTree::from_slice(b"0123456789", config);

        let mut iter = tree.bytes_at(5);

        // Go forward
        assert_eq!(iter.next(), Some(b'5'));
        assert_eq!(iter.next(), Some(b'6'));
        assert_eq!(iter.position(), 7);

        // Go backward
        assert_eq!(iter.prev(), Some(b'6'));
        assert_eq!(iter.prev(), Some(b'5'));
        assert_eq!(iter.position(), 5);

        // Go forward again
        assert_eq!(iter.next(), Some(b'5'));
        assert_eq!(iter.position(), 6);
    }

    #[test]
    fn test_byte_iterator_seek() {
        let config = ChunkTreeConfig::new(4, 4);
        let tree = ChunkTree::from_slice(b"0123456789", config);

        let mut iter = tree.bytes_at(0);

        iter.seek(5);
        assert_eq!(iter.position(), 5);
        assert_eq!(iter.peek(), Some(b'5'));

        iter.seek(9);
        assert_eq!(iter.position(), 9);
        assert_eq!(iter.peek(), Some(b'9'));

        iter.seek(0);
        assert_eq!(iter.position(), 0);
        assert_eq!(iter.peek(), Some(b'0'));
    }

    #[test]
    fn test_byte_iterator_boundaries() {
        let config = ChunkTreeConfig::new(4, 4);
        let tree = ChunkTree::from_slice(b"ABC", config);

        let mut iter = tree.bytes_at(0);

        // At start, can't go backward
        assert_eq!(iter.prev(), None);
        assert_eq!(iter.position(), 0);

        // Move to end
        assert_eq!(iter.next(), Some(b'A'));
        assert_eq!(iter.next(), Some(b'B'));
        assert_eq!(iter.next(), Some(b'C'));

        // At end, can't go forward
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_byte_iterator_large_file() {
        // Create a larger tree
        let config = ChunkTreeConfig::new(1024, 8);
        let data = b"x".repeat(10000);
        let tree = ChunkTree::from_slice(&data, config);

        let mut iter = tree.bytes_at(5000);

        // Should be able to navigate efficiently
        assert_eq!(iter.position(), 5000);
        assert_eq!(iter.next(), Some(b'x'));
        assert_eq!(iter.prev(), Some(b'x'));
        assert_eq!(iter.position(), 5000);

        // Jump to different position
        iter.seek(9999);
        assert_eq!(iter.position(), 9999);
        assert_eq!(iter.peek(), Some(b'x'));
    }

    // Property-based tests
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn prop_byte_iterator_matches_slice(data in prop::collection::vec(any::<u8>(), 0..1000)) {
            let config = ChunkTreeConfig::new(16, 4);
            let tree = ChunkTree::from_slice(&data, config);

            // Collect all bytes using iterator
            let mut iter = tree.bytes_at(0);
            let mut collected = Vec::new();
            while let Some(byte) = iter.next() {
                collected.push(byte);
            }

            // Should match original data
            prop_assert_eq!(collected, data);
        }

        #[test]
        fn prop_byte_iterator_forward_backward_identity(
            data in prop::collection::vec(any::<u8>(), 1..1000),
            position in 0usize..1000
        ) {
            let config = ChunkTreeConfig::new(16, 4);
            let tree = ChunkTree::from_slice(&data, config);
            let position = position.min(data.len().saturating_sub(1));

            let mut iter = tree.bytes_at(position);

            // Move forward then backward should return to same position
            let original_pos = iter.position();
            let byte_at_pos = iter.peek();

            if let Some(forward_byte) = iter.next() {
                let backward_byte = iter.prev();
                prop_assert_eq!(Some(forward_byte), backward_byte);
                prop_assert_eq!(iter.position(), original_pos);
                prop_assert_eq!(iter.peek(), byte_at_pos);
            }
        }

        #[test]
        fn prop_byte_iterator_seek_correctness(
            data in prop::collection::vec(any::<u8>(), 1..1000),
            positions in prop::collection::vec(0usize..1000, 1..20)
        ) {
            let config = ChunkTreeConfig::new(16, 4);
            let tree = ChunkTree::from_slice(&data, config);

            let mut iter = tree.bytes_at(0);

            for pos in positions {
                let pos = pos.min(data.len().saturating_sub(1));
                iter.seek(pos);

                prop_assert_eq!(iter.position(), pos);
                if pos < data.len() {
                    prop_assert_eq!(iter.peek(), Some(data[pos]));
                }
            }
        }

        #[test]
        fn prop_byte_iterator_reverse_matches_reversed_data(
            data in prop::collection::vec(any::<u8>(), 1..1000)
        ) {
            let config = ChunkTreeConfig::new(16, 4);
            let tree = ChunkTree::from_slice(&data, config);

            // Collect bytes going backward from the end
            let mut iter = tree.bytes_at(data.len());
            let mut collected = Vec::new();
            while let Some(byte) = iter.prev() {
                collected.push(byte);
            }

            // Should match reversed data
            let mut expected = data.clone();
            expected.reverse();
            prop_assert_eq!(collected, expected);
        }

        #[test]
        fn prop_byte_iterator_chunk_boundaries(
            data in prop::collection::vec(any::<u8>(), 1..1000),
            chunk_size in 2usize..32
        ) {
            let config = ChunkTreeConfig::new(chunk_size, 4);
            let tree = ChunkTree::from_slice(&data, config);

            // Iterator should work correctly across chunk boundaries
            let mut iter = tree.bytes_at(0);
            let mut collected = Vec::new();
            while let Some(byte) = iter.next() {
                collected.push(byte);
            }

            prop_assert_eq!(collected, data);
        }

        #[test]
        fn prop_byte_iterator_position_tracking(
            data in prop::collection::vec(any::<u8>(), 1..500)
        ) {
            let config = ChunkTreeConfig::new(16, 4);
            let tree = ChunkTree::from_slice(&data, config);

            let mut iter = tree.bytes_at(0);
            let mut expected_pos = 0;

            while expected_pos < data.len() {
                prop_assert_eq!(iter.position(), expected_pos);
                iter.next();
                expected_pos += 1;
            }

            prop_assert_eq!(iter.position(), data.len());
        }
    }
}
