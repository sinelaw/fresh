use std::io::{self, Read, Seek, SeekFrom};
use std::path::PathBuf;
use std::sync::Arc;

/// A position in the document (line and column)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: usize,   // 0-indexed line number
    pub column: usize, // Byte offset within the line
}

/// Data storage for a buffer - either loaded in memory or unloaded (file reference)
#[derive(Debug, Clone)]
pub enum BufferData {
    /// Loaded in memory with optional line indexing
    Loaded {
        data: Vec<u8>,
        line_starts: Option<Vec<usize>>, // None = not indexed (large file mode)
    },
    /// Not yet loaded from file
    Unloaded {
        file_path: PathBuf,
        file_offset: usize, // Where in file this buffer starts
        bytes: usize,       // Length of this region
    },
}

/// A string buffer containing a chunk of text data and its line metadata
/// This is the fundamental storage unit - piece tree nodes reference these buffers
#[derive(Debug, Clone)]
pub struct StringBuffer {
    /// Unique identifier for this buffer
    pub id: usize,
    /// The buffer data - either loaded or unloaded
    pub data: BufferData,
}

impl StringBuffer {
    /// Create a new string buffer with line metadata (legacy constructor)
    /// Automatically computes line starts
    pub fn new(id: usize, data: Vec<u8>) -> Self {
        let line_starts = Self::compute_line_starts(&data);
        StringBuffer {
            id,
            data: BufferData::Loaded {
                data,
                line_starts: Some(line_starts),
            },
        }
    }

    /// Create a loaded buffer with optional line indexing
    pub fn new_loaded(id: usize, data: Vec<u8>, compute_lines: bool) -> Self {
        let line_starts = if compute_lines {
            Some(Self::compute_line_starts(&data))
        } else {
            None
        };
        StringBuffer {
            id,
            data: BufferData::Loaded { data, line_starts },
        }
    }

    /// Create buffer for file region (not yet loaded)
    pub fn new_unloaded(id: usize, file_path: PathBuf, file_offset: usize, bytes: usize) -> Self {
        StringBuffer {
            id,
            data: BufferData::Unloaded {
                file_path,
                file_offset,
                bytes,
            },
        }
    }

    /// Check if buffer is loaded
    pub fn is_loaded(&self) -> bool {
        matches!(self.data, BufferData::Loaded { .. })
    }

    /// Get data reference if loaded, None if unloaded
    ///
    /// NOTE: This is a low-level API. External code should use TextBuffer::get_text_range_mut()
    /// which provides automatic lazy loading. This method is pub(crate) to prevent misuse.
    pub(crate) fn get_data(&self) -> Option<&[u8]> {
        match &self.data {
            BufferData::Loaded { data, .. } => Some(data),
            BufferData::Unloaded { .. } => None,
        }
    }

    /// Get line starts if available
    pub fn get_line_starts(&self) -> Option<&[usize]> {
        match &self.data {
            BufferData::Loaded { line_starts, .. } => line_starts.as_deref(),
            BufferData::Unloaded { .. } => None,
        }
    }

    /// Load buffer data from file (for unloaded buffers)
    /// Returns error if buffer is not unloaded or if I/O fails
    pub fn load(&mut self) -> io::Result<()> {
        match &self.data {
            BufferData::Loaded { .. } => Ok(()), // Already loaded
            BufferData::Unloaded {
                file_path,
                file_offset,
                bytes,
            } => {
                // Load from file
                let mut file = std::fs::File::open(file_path)?;
                file.seek(SeekFrom::Start(*file_offset as u64))?;

                let mut buffer = vec![0u8; *bytes];
                file.read_exact(&mut buffer)?;

                // Replace with loaded data (no line indexing for lazy-loaded chunks)
                self.data = BufferData::Loaded {
                    data: buffer,
                    line_starts: None,
                };

                Ok(())
            }
        }
    }

    /// Create a new unloaded buffer representing a chunk of this buffer
    /// This is used for splitting large unloaded buffers into smaller chunks
    ///
    /// # Arguments
    /// * `new_id` - The ID for the new buffer
    /// * `chunk_offset` - Offset within this buffer where the chunk starts
    /// * `chunk_bytes` - Number of bytes in the chunk
    ///
    /// # Returns
    /// A new StringBuffer referencing the chunk, or None if this buffer is not unloaded
    /// or if the chunk range is invalid
    pub fn create_chunk_buffer(
        &self,
        new_id: usize,
        chunk_offset: usize,
        chunk_bytes: usize,
    ) -> Option<StringBuffer> {
        match &self.data {
            BufferData::Unloaded {
                file_path,
                file_offset,
                bytes,
            } => {
                // Validate chunk range
                if chunk_offset + chunk_bytes > *bytes {
                    return None;
                }

                Some(StringBuffer::new_unloaded(
                    new_id,
                    file_path.clone(),
                    file_offset + chunk_offset,
                    chunk_bytes,
                ))
            }
            BufferData::Loaded { .. } => None, // Can't create chunk from loaded buffer
        }
    }

    /// Compute line start offsets for a buffer
    fn compute_line_starts(data: &[u8]) -> Vec<usize> {
        let mut line_starts = vec![0];
        for (i, &byte) in data.iter().enumerate() {
            if byte == b'\n' {
                line_starts.push(i + 1);
            }
        }
        line_starts
    }

    /// Get the number of line feeds (newlines) in this buffer
    /// Returns None if line indexing was not computed or buffer is unloaded
    pub fn line_feed_count(&self) -> Option<usize> {
        match &self.data {
            BufferData::Loaded { line_starts, .. } => line_starts
                .as_ref()
                .map(|starts| starts.len().saturating_sub(1)),
            BufferData::Unloaded { .. } => None,
        }
    }

    /// Append data to this buffer and recompute line starts
    /// Returns the offset where the appended data starts
    /// Only works for loaded buffers with line starts
    pub fn append(&mut self, data_to_append: &[u8]) -> usize {
        match &mut self.data {
            BufferData::Loaded { data, line_starts } => {
                let start_offset = data.len();
                data.extend_from_slice(data_to_append);

                // Add new line starts if we're tracking them
                if let Some(ref mut line_starts) = line_starts {
                    for (i, &byte) in data_to_append.iter().enumerate() {
                        if byte == b'\n' {
                            line_starts.push(start_offset + i + 1);
                        }
                    }
                }

                start_offset
            }
            BufferData::Unloaded { .. } => {
                // Can't append to unloaded buffer
                0
            }
        }
    }
}

/// Identifies which buffer a piece of text comes from
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BufferLocation {
    /// Data is in the original stored/persisted buffer
    Stored(usize), // buffer_id
    /// Data is in the added/modified buffer
    Added(usize), // buffer_id
}

impl BufferLocation {
    /// Get the buffer ID
    pub fn buffer_id(&self) -> usize {
        match self {
            Self::Stored(id) | Self::Added(id) => *id,
        }
    }
}

/// A node in the piece tree with integrated line tracking
#[derive(Debug, Clone)]
pub enum PieceTreeNode {
    /// Internal node with left and right children
    Internal {
        left_bytes: usize,      // Total bytes in left subtree
        lf_left: Option<usize>, // Total line feeds in left subtree (None if unknown)
        left: Arc<PieceTreeNode>,
        right: Arc<PieceTreeNode>,
    },
    /// Leaf node representing an actual piece
    Leaf {
        location: BufferLocation, // Where this piece's data is (includes buffer_id)
        offset: usize,            // Offset within the buffer
        bytes: usize,             // Number of bytes in this piece
        line_feed_cnt: Option<usize>, // Number of line feeds in this piece (None if unknown)
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
    bytes_before: usize, // Total bytes in all pieces before this one
}

/// A cursor position in the document
#[derive(Debug, Clone)]
pub struct Cursor {
    pub byte_offset: usize, // Absolute byte offset in document
    pub line: usize,        // Line number (0-indexed)
    pub col: usize,         // Column within line (byte offset)
}

/// Represents the data for a leaf node in the piece tree
#[derive(Debug, Clone, Copy)]
pub struct LeafData {
    pub location: BufferLocation,
    pub offset: usize,
    pub bytes: usize,
    pub line_feed_cnt: Option<usize>,
}

impl LeafData {
    pub fn new(
        location: BufferLocation,
        offset: usize,
        bytes: usize,
        line_feed_cnt: Option<usize>,
    ) -> Self {
        LeafData {
            location,
            offset,
            bytes,
            line_feed_cnt,
        }
    }
}

/// Statistics about the piece tree structure
#[derive(Debug, Clone, Copy)]
pub struct TreeStats {
    pub total_bytes: usize,
    pub depth: usize,
    pub leaf_count: usize,
    pub line_feed_count: Option<usize>,
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
            Self::Internal {
                left_bytes,
                left,
                right,
                ..
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
            Self::Leaf {
                location,
                offset: piece_offset,
                bytes,
                ..
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
            Self::Internal {
                left_bytes, right, ..
            } => left_bytes + right.total_bytes(),
            Self::Leaf { bytes, .. } => *bytes,
        }
    }

    /// Get total line feeds in this node
    /// Returns None if any piece has unknown line count
    fn total_line_feeds(&self) -> Option<usize> {
        match self {
            Self::Internal { lf_left, right, .. } => match (lf_left, right.total_line_feeds()) {
                (Some(left), Some(right)) => Some(left + right),
                _ => None,
            },
            Self::Leaf { line_feed_cnt, .. } => *line_feed_cnt,
        }
    }

    /// Get the depth of this tree
    fn depth(&self) -> usize {
        match self {
            Self::Internal { left, right, .. } => 1 + left.depth().max(right.depth()),
            Self::Leaf { .. } => 1,
        }
    }

    /// Count the number of leaf nodes
    fn count_leaves(&self) -> usize {
        match self {
            Self::Internal { left, right, .. } => left.count_leaves() + right.count_leaves(),
            Self::Leaf { .. } => 1,
        }
    }

    /// Collect all leaves in order
    fn collect_leaves(&self, leaves: &mut Vec<LeafData>) {
        match self {
            Self::Internal { left, right, .. } => {
                left.collect_leaves(leaves);
                right.collect_leaves(leaves);
            }
            Self::Leaf {
                location,
                offset,
                bytes,
                line_feed_cnt,
            } => {
                leaves.push(LeafData::new(*location, *offset, *bytes, *line_feed_cnt));
            }
        }
    }

    /// Count line feeds in a byte range [start, end)
    /// current_offset: byte offset at the start of this node
    /// Returns None if any piece in the range has unknown line count
    fn count_lines_in_byte_range(
        &self,
        current_offset: usize,
        start: usize,
        end: usize,
    ) -> Option<usize> {
        match self {
            Self::Internal {
                left_bytes,
                left,
                right,
                ..
            } => {
                let left_end = current_offset + left_bytes;

                if end <= current_offset || start >= current_offset + self.total_bytes() {
                    Some(0) // Range is completely before or after this node
                } else if start <= current_offset && end >= current_offset + self.total_bytes() {
                    // Range completely contains this node
                    self.total_line_feeds()
                } else if end <= left_end {
                    // Range is completely in left subtree
                    left.count_lines_in_byte_range(current_offset, start, end)
                } else if start >= left_end {
                    // Range is completely in right subtree
                    right.count_lines_in_byte_range(left_end, start, end)
                } else {
                    // Range spans both subtrees
                    let left_count = left.count_lines_in_byte_range(current_offset, start, end)?;
                    let right_count = right.count_lines_in_byte_range(left_end, start, end)?;
                    Some(left_count + right_count)
                }
            }
            Self::Leaf {
                line_feed_cnt,
                bytes,
                ..
            } => {
                let node_end = current_offset + bytes;

                if end <= current_offset || start >= node_end {
                    Some(0) // No overlap
                } else if start <= current_offset && end >= node_end {
                    // Range completely contains this leaf
                    *line_feed_cnt
                } else {
                    // Partial overlap - for simplicity, return the full count
                    // (accurate counting would require scanning the buffer)
                    *line_feed_cnt
                }
            }
        }
    }

    /// Find byte offset for a given line/column position
    /// current_offset: byte offset at start of this node
    /// lines_before: number of complete lines before this node
    fn find_byte_offset_for_line(
        &self,
        current_offset: usize,
        lines_before: usize,
        target_line: usize,
        column: usize,
        buffers: &[StringBuffer],
    ) -> Option<usize> {
        match self {
            Self::Internal {
                left_bytes,
                lf_left,
                left,
                right,
            } => {
                // If line count is unknown, we can't do line-based navigation
                let lf_left = lf_left.as_ref()?;
                let lines_after_left = lines_before + lf_left;

                // When looking for line start (column == 0), we want the leftmost piece containing the line
                // So use <= instead of < to prefer going left when the line boundary is exactly at lines_after_left
                let go_left = if column == 0 {
                    target_line <= lines_after_left
                } else {
                    target_line < lines_after_left
                };

                if go_left {
                    // Target is in left subtree
                    let result = left.find_byte_offset_for_line(
                        current_offset,
                        lines_before,
                        target_line,
                        column,
                        buffers,
                    );
                    // If left returns None, try right as fallback (happens when line starts after a newline)
                    result.or_else(|| {
                        right.find_byte_offset_for_line(
                            current_offset + left_bytes,
                            lines_after_left,
                            target_line,
                            column,
                            buffers,
                        )
                    })
                } else {
                    // Target is in right subtree
                    right.find_byte_offset_for_line(
                        current_offset + left_bytes,
                        lines_after_left,
                        target_line,
                        column,
                        buffers,
                    )
                }
            }
            Self::Leaf {
                location,
                offset,
                bytes,
                line_feed_cnt,
            } => {
                // If line count is unknown, we can't do line-based navigation
                let line_feed_cnt = line_feed_cnt.as_ref()?;
                let lines_in_piece = lines_before + line_feed_cnt;

                // Special case: when looking for column==0 of line N where N == lines_in_piece,
                // the line might start in the NEXT piece if this piece ends with a newline.
                // Check if the last byte of this piece is a newline.
                if column == 0 && target_line == lines_in_piece && target_line > lines_before {
                    let buffer = buffers.get(location.buffer_id())?;
                    let data = buffer.get_data()?;
                    let last_byte_offset = offset + bytes - 1;
                    let last_byte = data.get(last_byte_offset)?;

                    if *last_byte == b'\n' {
                        // Piece ends with newline, so the next line starts in the next piece
                        return None;
                    }
                    // Otherwise, line starts within this piece after a newline
                }

                if target_line < lines_before || target_line > lines_in_piece {
                    // Target line not in this piece
                    return None;
                }

                // Get the buffer for this piece
                let buffer_id = location.buffer_id();
                let buffer = buffers.get(buffer_id)?;
                let line_starts = buffer.get_line_starts()?;

                // Find the line within the piece
                let line_in_piece = target_line - lines_before;

                // Get piece range in buffer
                let piece_start_in_buffer = *offset;
                let piece_end_in_buffer = offset + bytes;

                // Special case: first line of piece (line_in_piece == 0)
                let line_start_in_buffer = if line_in_piece == 0 {
                    // First line starts at piece start
                    piece_start_in_buffer
                } else {
                    // Find the Nth newline within this piece
                    // Count line_starts that fall within [piece_start, piece_end)
                    let mut lines_seen = 0;
                    let mut found_line_start = None;

                    for &line_start in line_starts.iter() {
                        // Line starts are positions of newlines + 1, or beginning of buffer (0)
                        // We want line_starts that are > piece_start and < piece_end
                        if line_start > piece_start_in_buffer && line_start < piece_end_in_buffer {
                            if lines_seen == line_in_piece - 1 {
                                // This is the start of our target line
                                found_line_start = Some(line_start);
                                break;
                            }
                            lines_seen += 1;
                        }
                    }

                    found_line_start?
                };

                // Add column offset
                let target_offset_in_buffer = line_start_in_buffer + column;

                // Convert to document offset
                let offset_in_piece = target_offset_in_buffer.saturating_sub(piece_start_in_buffer);
                Some(current_offset + offset_in_piece.min(*bytes))
            }
        }
    }
}

/// The main piece table structure with integrated line tracking
#[derive(Debug, Clone)]
pub struct PieceTree {
    root: Arc<PieceTreeNode>,
    total_bytes: usize,
}

impl PieceTree {
    /// Create a new piece table with a single initial piece
    pub fn new(
        location: BufferLocation,
        offset: usize,
        bytes: usize,
        line_feed_cnt: Option<usize>,
    ) -> Self {
        PieceTree {
            root: Arc::new(PieceTreeNode::Leaf {
                location,
                offset,
                bytes,
                line_feed_cnt,
            }),
            total_bytes: bytes,
        }
    }

    /// Create an empty piece table
    pub fn empty() -> Self {
        PieceTree {
            root: Arc::new(PieceTreeNode::Leaf {
                location: BufferLocation::Stored(0),
                offset: 0,
                bytes: 0,
                line_feed_cnt: Some(0), // Empty has zero line feeds (known)
            }),
            total_bytes: 0,
        }
    }

    /// Build a balanced tree from a list of leaves
    fn build_balanced(leaves: &[LeafData]) -> Arc<PieceTreeNode> {
        if leaves.is_empty() {
            return Arc::new(PieceTreeNode::Leaf {
                location: BufferLocation::Stored(0),
                offset: 0,
                bytes: 0,
                line_feed_cnt: Some(0), // Empty has zero line feeds (known)
            });
        }

        if leaves.len() == 1 {
            let leaf = leaves[0];
            return Arc::new(PieceTreeNode::Leaf {
                location: leaf.location,
                offset: leaf.offset,
                bytes: leaf.bytes,
                line_feed_cnt: leaf.line_feed_cnt,
            });
        }

        // Split in the middle
        let mid = leaves.len() / 2;
        let left = Self::build_balanced(&leaves[..mid]);
        let right = Self::build_balanced(&leaves[mid..]);

        let left_bytes = left.total_bytes();
        let lf_left = left.total_line_feeds();

        Arc::new(PieceTreeNode::Internal {
            left_bytes,
            lf_left,
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
    /// line_feed_cnt: number of line feeds in the inserted text (None if unknown/not computed)
    /// buffers: reference to the string buffers for computing line feeds during splits
    pub fn insert(
        &mut self,
        offset: usize,
        location: BufferLocation,
        buffer_offset: usize,
        bytes: usize,
        line_feed_cnt: Option<usize>,
        buffers: &[StringBuffer],
    ) -> Cursor {
        if bytes == 0 {
            return self.cursor_at_offset(offset);
        }

        // Find the piece to split
        if let Some(_result) = self.root.find_by_offset(offset) {
            // Split the piece at the insertion point
            let mut leaves = Vec::new();
            let insert_leaf = LeafData::new(location, buffer_offset, bytes, line_feed_cnt);
            self.collect_leaves_with_split(
                &self.root,
                0,
                offset,
                Some(insert_leaf),
                &mut leaves,
                buffers,
            );

            self.root = Self::build_balanced(&leaves);
            self.total_bytes += bytes;

            self.check_and_rebalance();
        } else if offset == self.total_bytes {
            // Append at end
            let mut leaves = Vec::new();
            self.root.collect_leaves(&mut leaves);
            leaves.push(LeafData::new(location, buffer_offset, bytes, line_feed_cnt));

            self.root = Self::build_balanced(&leaves);
            self.total_bytes += bytes;

            self.check_and_rebalance();
        }

        self.cursor_at_offset(offset + bytes)
    }

    /// Get a clone of the root node (shared via Arc)
    pub fn root(&self) -> Arc<PieceTreeNode> {
        Arc::clone(&self.root)
    }

    /// Insert text at the given position (line, column)
    /// Returns new cursor after the inserted text
    /// This performs a SINGLE tree traversal (more efficient than position_to_offset + insert)
    #[allow(clippy::too_many_arguments)]
    pub fn insert_at_position(
        &mut self,
        line: usize,
        column: usize,
        location: BufferLocation,
        buffer_offset: usize,
        bytes: usize,
        line_feed_cnt: usize,
        buffers: &[StringBuffer],
    ) -> Cursor {
        if bytes == 0 {
            let offset = self.position_to_offset(line, column, buffers);
            return self.cursor_at_offset(offset);
        }

        // Collect leaves while splitting at the position
        let mut leaves = Vec::new();
        let insert_leaf = LeafData::new(location, buffer_offset, bytes, Some(line_feed_cnt));

        self.collect_leaves_with_split_at_position(
            &self.root,
            0,
            0,
            line,
            column,
            Some(insert_leaf),
            &mut leaves,
            buffers,
        );

        self.root = Self::build_balanced(&leaves);
        self.total_bytes += bytes;
        self.check_and_rebalance();

        // Return cursor at position after insertion
        let offset = self.position_to_offset(line, column, buffers) + bytes;
        self.cursor_at_offset(offset)
    }

    /// Helper to collect leaves while splitting at a position (line, column)
    /// Similar to collect_leaves_with_split but works with positions instead of offsets
    #[allow(clippy::too_many_arguments)]
    fn collect_leaves_with_split_at_position(
        &self,
        node: &Arc<PieceTreeNode>,
        current_offset: usize,
        lines_before: usize,
        target_line: usize,
        target_column: usize,
        insert: Option<LeafData>,
        leaves: &mut Vec<LeafData>,
        buffers: &[StringBuffer],
    ) {
        match node.as_ref() {
            PieceTreeNode::Internal {
                left_bytes,
                lf_left,
                left,
                right,
            } => {
                // If line counts are unknown, we can't do position-based navigation
                let Some(lf_left) = lf_left else {
                    return;
                };
                let lines_after_left = lines_before + lf_left;

                // Determine if target position is in left or right subtree
                let go_left = if target_column == 0 {
                    target_line <= lines_after_left
                } else {
                    target_line < lines_after_left
                };

                if go_left {
                    // Target is in left subtree
                    self.collect_leaves_with_split_at_position(
                        left,
                        current_offset,
                        lines_before,
                        target_line,
                        target_column,
                        insert,
                        leaves,
                        buffers,
                    );
                    self.collect_leaves_with_split_at_position(
                        right,
                        current_offset + left_bytes,
                        lines_after_left,
                        target_line,
                        target_column,
                        None,
                        leaves,
                        buffers,
                    );
                } else {
                    // Target is in right subtree
                    self.collect_leaves_with_split_at_position(
                        left,
                        current_offset,
                        lines_before,
                        target_line,
                        target_column,
                        None,
                        leaves,
                        buffers,
                    );
                    self.collect_leaves_with_split_at_position(
                        right,
                        current_offset + left_bytes,
                        lines_after_left,
                        target_line,
                        target_column,
                        insert,
                        leaves,
                        buffers,
                    );
                }
            }
            PieceTreeNode::Leaf {
                location,
                offset,
                bytes,
                line_feed_cnt,
            } => {
                // If line counts are unknown, we can't do position-based navigation
                let Some(line_feed_cnt) = line_feed_cnt else {
                    return;
                };
                let lines_in_piece = lines_before + line_feed_cnt;

                // Check if this piece contains the target line
                if target_line >= lines_before && target_line <= lines_in_piece {
                    // Target line is in this piece
                    let buffer_id = location.buffer_id();
                    if let Some(buffer) = buffers.get(buffer_id) {
                        let line_in_piece = target_line - lines_before;

                        // Find the line start within the piece
                        let line_start_in_buffer = if line_in_piece == 0 {
                            *offset
                        } else {
                            // Find the Nth newline within this piece
                            let mut lines_seen = 0;
                            let mut found_line_start = *offset;

                            if let Some(line_starts) = buffer.get_line_starts() {
                                for &ls in line_starts.iter() {
                                    if ls > *offset && ls < *offset + *bytes {
                                        if lines_seen == line_in_piece - 1 {
                                            found_line_start = ls;
                                            break;
                                        }
                                        lines_seen += 1;
                                    }
                                }
                            }

                            found_line_start
                        };

                        // Calculate split offset within the piece
                        let column_offset = target_column.min(*bytes);
                        let split_in_buffer = line_start_in_buffer + column_offset;
                        let split_offset_in_piece =
                            split_in_buffer.saturating_sub(*offset).min(*bytes);

                        // Split the piece at this position
                        if split_offset_in_piece > 0 {
                            // First part (before split)
                            let lf_cnt = Self::compute_line_feeds_static(
                                buffers,
                                *location,
                                *offset,
                                split_offset_in_piece,
                            );
                            leaves.push(LeafData::new(
                                *location,
                                *offset,
                                split_offset_in_piece,
                                lf_cnt,
                            ));
                        }

                        // Inserted piece
                        if let Some(insert_leaf) = insert {
                            leaves.push(insert_leaf);
                        }

                        // Second part (after split)
                        let remaining = bytes.saturating_sub(split_offset_in_piece);
                        if remaining > 0 {
                            let lf_cnt = Self::compute_line_feeds_static(
                                buffers,
                                *location,
                                offset + split_offset_in_piece,
                                remaining,
                            );
                            leaves.push(LeafData::new(
                                *location,
                                offset + split_offset_in_piece,
                                remaining,
                                lf_cnt,
                            ));
                        }
                    } else {
                        // Buffer not found, just keep the piece as-is
                        leaves.push(LeafData::new(
                            *location,
                            *offset,
                            *bytes,
                            Some(*line_feed_cnt),
                        ));
                    }
                } else {
                    // Target line not in this piece, just keep it
                    leaves.push(LeafData::new(
                        *location,
                        *offset,
                        *bytes,
                        Some(*line_feed_cnt),
                    ));
                }
            }
        }
    }

    /// Helper to collect leaves while splitting at insertion point
    fn collect_leaves_with_split(
        &self,
        node: &Arc<PieceTreeNode>,
        current_offset: usize,
        split_offset: usize,
        insert: Option<LeafData>,
        leaves: &mut Vec<LeafData>,
        buffers: &[StringBuffer],
    ) {
        match node.as_ref() {
            PieceTreeNode::Internal {
                left_bytes,
                left,
                right,
                ..
            } => {
                // Only pass `insert` to the subtree containing the split point
                if split_offset < current_offset + left_bytes {
                    // Split is in left subtree
                    self.collect_leaves_with_split(
                        left,
                        current_offset,
                        split_offset,
                        insert,
                        leaves,
                        buffers,
                    );
                    self.collect_leaves_with_split(
                        right,
                        current_offset + left_bytes,
                        split_offset,
                        None,
                        leaves,
                        buffers,
                    );
                } else {
                    // Split is in right subtree (or at boundary)
                    self.collect_leaves_with_split(
                        left,
                        current_offset,
                        split_offset,
                        None,
                        leaves,
                        buffers,
                    );
                    self.collect_leaves_with_split(
                        right,
                        current_offset + left_bytes,
                        split_offset,
                        insert,
                        leaves,
                        buffers,
                    );
                }
            }
            PieceTreeNode::Leaf {
                location,
                offset,
                bytes,
                line_feed_cnt,
            } => {
                let piece_end = current_offset + bytes;

                if split_offset > current_offset && split_offset < piece_end {
                    // Split this piece - need to compute line feeds for each part
                    let offset_in_piece = split_offset - current_offset;

                    // First part (before split)
                    if offset_in_piece > 0 {
                        let lf_cnt = Self::compute_line_feeds_static(
                            buffers,
                            *location,
                            *offset,
                            offset_in_piece,
                        );
                        leaves.push(LeafData::new(*location, *offset, offset_in_piece, lf_cnt));
                    }

                    // Inserted piece
                    if let Some(insert_leaf) = insert {
                        leaves.push(insert_leaf);
                    }

                    // Second part (after split)
                    let remaining = bytes - offset_in_piece;
                    if remaining > 0 {
                        let lf_cnt = Self::compute_line_feeds_static(
                            buffers,
                            *location,
                            offset + offset_in_piece,
                            remaining,
                        );
                        leaves.push(LeafData::new(
                            *location,
                            offset + offset_in_piece,
                            remaining,
                            lf_cnt,
                        ));
                    }
                } else if split_offset == current_offset {
                    // Insert before this piece
                    if let Some(insert_leaf) = insert {
                        leaves.push(insert_leaf);
                    }
                    leaves.push(LeafData::new(*location, *offset, *bytes, *line_feed_cnt));
                } else {
                    // Don't split, just add the piece
                    leaves.push(LeafData::new(*location, *offset, *bytes, *line_feed_cnt));
                }
            }
        }
    }

    /// Helper to compute line feeds in a buffer range
    fn compute_line_feeds_static(
        buffers: &[StringBuffer],
        location: BufferLocation,
        offset: usize,
        bytes: usize,
    ) -> Option<usize> {
        let buffer_id = location.buffer_id();
        if let Some(buffer) = buffers.get(buffer_id) {
            if let Some(data) = buffer.get_data() {
                let end = (offset + bytes).min(data.len());
                Some(data[offset..end].iter().filter(|&&b| b == b'\n').count())
            } else {
                // Buffer is unloaded - return None
                None
            }
        } else {
            // Buffer not available - return None
            None
        }
    }

    /// Split a piece at the given offset without inserting anything
    /// This is useful for isolating a chunk of a large piece for partial loading
    ///
    /// If the offset is in the middle of a piece, that piece will be split into two pieces.
    /// If the offset is at a piece boundary, nothing changes.
    /// Does nothing if offset is 0 or >= total_bytes.
    pub fn split_at_offset(&mut self, offset: usize, buffers: &[StringBuffer]) {
        if offset == 0 || offset >= self.total_bytes {
            return;
        }

        // Check if we need to split (offset must be in middle of a piece)
        if let Some(_result) = self.root.find_by_offset(offset) {
            // Split the piece at the offset (with no insertion)
            let mut leaves = Vec::new();
            self.collect_leaves_with_split(&self.root, 0, offset, None, &mut leaves, buffers);

            self.root = Self::build_balanced(&leaves);
            self.check_and_rebalance();
        }
    }

    /// Replace buffer references in pieces
    /// This is used when creating chunk buffers from large unloaded buffers
    ///
    /// Finds all pieces that reference the old buffer at the specified offset/bytes
    /// and updates them to reference the new buffer at offset 0.
    pub fn replace_buffer_reference(
        &mut self,
        old_buffer_id: usize,
        old_buffer_offset: usize,
        old_buffer_bytes: usize,
        new_buffer_location: BufferLocation,
    ) {
        let mut leaves = Vec::new();
        self.root.collect_leaves(&mut leaves);

        // Find and update matching pieces
        let mut modified = false;
        for leaf in &mut leaves {
            if leaf.location.buffer_id() == old_buffer_id
                && leaf.offset == old_buffer_offset
                && leaf.bytes == old_buffer_bytes
            {
                leaf.location = new_buffer_location;
                leaf.offset = 0; // New buffer starts at 0
                modified = true;
            }
        }

        // Rebuild tree if we made changes
        if modified {
            self.root = Self::build_balanced(&leaves);
            self.check_and_rebalance();
        }
    }

    /// Delete text starting at offset for the given number of bytes
    pub fn delete(&mut self, offset: usize, delete_bytes: usize, buffers: &[StringBuffer]) {
        if delete_bytes == 0 || offset >= self.total_bytes {
            return;
        }

        let delete_bytes = delete_bytes.min(self.total_bytes - offset);
        let end_offset = offset + delete_bytes;

        let mut leaves = Vec::new();
        self.collect_leaves_with_delete(&self.root, 0, offset, end_offset, &mut leaves, buffers);

        self.root = Self::build_balanced(&leaves);
        self.total_bytes -= delete_bytes;

        self.check_and_rebalance();
    }

    /// Delete text in a range specified by positions (start_line, start_col) to (end_line, end_col)
    /// This performs a more efficient traversal than converting positions to offsets separately
    pub fn delete_position_range(
        &mut self,
        start_line: usize,
        start_column: usize,
        end_line: usize,
        end_column: usize,
        buffers: &[StringBuffer],
    ) {
        // Edge case: empty range
        if start_line == end_line && start_column == end_column {
            return;
        }

        // Find both positions in a single traversal and collect leaves
        let mut leaves = Vec::new();
        let mut delete_start_offset = None;
        let mut delete_end_offset = None;

        self.collect_leaves_with_position_delete(
            &self.root,
            0,
            0,
            start_line,
            start_column,
            end_line,
            end_column,
            &mut delete_start_offset,
            &mut delete_end_offset,
            &mut leaves,
            buffers,
        );

        // Calculate how many bytes were deleted
        if let (Some(start), Some(end)) = (delete_start_offset, delete_end_offset) {
            let deleted_bytes = end.saturating_sub(start);
            if deleted_bytes > 0 {
                self.root = Self::build_balanced(&leaves);
                self.total_bytes = self.total_bytes.saturating_sub(deleted_bytes);
                self.check_and_rebalance();
            }
        }
    }

    /// Helper to collect leaves while deleting a range specified by positions
    /// This finds both positions and performs the deletion in a single tree traversal
    #[allow(clippy::too_many_arguments)]
    fn collect_leaves_with_position_delete(
        &self,
        node: &Arc<PieceTreeNode>,
        current_offset: usize,
        lines_before: usize,
        start_line: usize,
        start_column: usize,
        end_line: usize,
        end_column: usize,
        delete_start_offset: &mut Option<usize>,
        delete_end_offset: &mut Option<usize>,
        leaves: &mut Vec<LeafData>,
        buffers: &[StringBuffer],
    ) {
        match node.as_ref() {
            PieceTreeNode::Internal {
                left_bytes,
                lf_left,
                left,
                right,
            } => {
                // If line counts are unknown, we can't do position-based navigation
                let Some(lf_left) = lf_left else {
                    return;
                };
                let lines_after_left = lines_before + lf_left;

                // Recursively process both subtrees
                self.collect_leaves_with_position_delete(
                    left,
                    current_offset,
                    lines_before,
                    start_line,
                    start_column,
                    end_line,
                    end_column,
                    delete_start_offset,
                    delete_end_offset,
                    leaves,
                    buffers,
                );
                self.collect_leaves_with_position_delete(
                    right,
                    current_offset + left_bytes,
                    lines_after_left,
                    start_line,
                    start_column,
                    end_line,
                    end_column,
                    delete_start_offset,
                    delete_end_offset,
                    leaves,
                    buffers,
                );
            }
            PieceTreeNode::Leaf {
                location,
                offset,
                bytes,
                line_feed_cnt,
            } => {
                // If line counts are unknown, we can't do position-based navigation
                let Some(line_feed_cnt) = line_feed_cnt else {
                    return;
                };
                let lines_in_piece = lines_before + line_feed_cnt;
                let piece_start = current_offset;
                let piece_end = current_offset + bytes;

                // Check if this piece contains the start position
                if start_line >= lines_before
                    && start_line <= lines_in_piece
                    && delete_start_offset.is_none()
                {
                    if let Some(buffer) = buffers.get(location.buffer_id()) {
                        let offset_in_piece = self.find_position_in_leaf(
                            lines_before,
                            start_line,
                            start_column,
                            *offset,
                            *bytes,
                            buffer,
                        );
                        *delete_start_offset = Some(piece_start + offset_in_piece);
                    }
                }

                // Check if this piece contains the end position
                if end_line >= lines_before
                    && end_line <= lines_in_piece
                    && delete_end_offset.is_none()
                {
                    if let Some(buffer) = buffers.get(location.buffer_id()) {
                        let offset_in_piece = self.find_position_in_leaf(
                            lines_before,
                            end_line,
                            end_column,
                            *offset,
                            *bytes,
                            buffer,
                        );
                        *delete_end_offset = Some(piece_start + offset_in_piece);
                    }
                }

                // Now determine what to keep
                let del_start = delete_start_offset.unwrap_or(usize::MAX);
                let del_end = delete_end_offset.unwrap_or(0);

                // Piece completely before delete range
                if piece_end <= del_start {
                    leaves.push(LeafData::new(
                        *location,
                        *offset,
                        *bytes,
                        Some(*line_feed_cnt),
                    ));
                    return;
                }

                // Piece completely after delete range (only if we've found end)
                if delete_end_offset.is_some() && piece_start >= del_end {
                    leaves.push(LeafData::new(
                        *location,
                        *offset,
                        *bytes,
                        Some(*line_feed_cnt),
                    ));
                    return;
                }

                // Piece overlaps with delete range
                // Keep part before delete start
                if piece_start < del_start && del_start < piece_end {
                    let keep_bytes = del_start - piece_start;
                    let lf_cnt =
                        Self::compute_line_feeds_static(buffers, *location, *offset, keep_bytes);
                    leaves.push(LeafData::new(*location, *offset, keep_bytes, lf_cnt));
                }

                // Keep part after delete end (if we know where end is)
                if delete_end_offset.is_some() && del_end > piece_start && del_end < piece_end {
                    let skip_bytes = del_end - piece_start;
                    let keep_bytes = piece_end - del_end;
                    let lf_cnt = Self::compute_line_feeds_static(
                        buffers,
                        *location,
                        offset + skip_bytes,
                        keep_bytes,
                    );
                    leaves.push(LeafData::new(
                        *location,
                        offset + skip_bytes,
                        keep_bytes,
                        lf_cnt,
                    ));
                }
            }
        }
    }

    /// Helper to find a position within a leaf piece
    /// Returns the offset within the piece (not the document offset)
    fn find_position_in_leaf(
        &self,
        lines_before: usize,
        target_line: usize,
        target_column: usize,
        piece_offset: usize,
        piece_bytes: usize,
        buffer: &StringBuffer,
    ) -> usize {
        let line_in_piece = target_line - lines_before;

        // Find the line start within the piece
        let line_start_in_buffer = if line_in_piece == 0 {
            piece_offset
        } else {
            // Find the Nth newline within this piece
            let mut lines_seen = 0;
            let mut found_line_start = piece_offset;

            if let Some(line_starts) = buffer.get_line_starts() {
                for &ls in line_starts.iter() {
                    if ls > piece_offset && ls < piece_offset + piece_bytes {
                        if lines_seen == line_in_piece - 1 {
                            found_line_start = ls;
                            break;
                        }
                        lines_seen += 1;
                    }
                }
            }

            found_line_start
        };

        // Calculate offset within the piece
        let column_offset = target_column.min(piece_bytes);
        let target_in_buffer = line_start_in_buffer + column_offset;
        target_in_buffer
            .saturating_sub(piece_offset)
            .min(piece_bytes)
    }

    /// Helper to collect leaves while deleting a range
    fn collect_leaves_with_delete(
        &self,
        node: &Arc<PieceTreeNode>,
        current_offset: usize,
        delete_start: usize,
        delete_end: usize,
        leaves: &mut Vec<LeafData>,
        buffers: &[StringBuffer],
    ) {
        match node.as_ref() {
            PieceTreeNode::Internal {
                left_bytes,
                left,
                right,
                ..
            } => {
                self.collect_leaves_with_delete(
                    left,
                    current_offset,
                    delete_start,
                    delete_end,
                    leaves,
                    buffers,
                );
                self.collect_leaves_with_delete(
                    right,
                    current_offset + left_bytes,
                    delete_start,
                    delete_end,
                    leaves,
                    buffers,
                );
            }
            PieceTreeNode::Leaf {
                location,
                offset,
                bytes,
                line_feed_cnt,
            } => {
                let piece_start = current_offset;
                let piece_end = current_offset + bytes;

                // Piece completely before delete range
                if piece_end <= delete_start {
                    leaves.push(LeafData::new(*location, *offset, *bytes, *line_feed_cnt));
                    return;
                }

                // Piece completely after delete range
                if piece_start >= delete_end {
                    leaves.push(LeafData::new(*location, *offset, *bytes, *line_feed_cnt));
                    return;
                }

                // Piece partially or fully overlaps delete range
                // Keep part before delete range
                if piece_start < delete_start {
                    let keep_bytes = delete_start - piece_start;
                    let lf_cnt =
                        Self::compute_line_feeds_static(buffers, *location, *offset, keep_bytes);
                    leaves.push(LeafData::new(*location, *offset, keep_bytes, lf_cnt));
                }

                // Keep part after delete range
                if piece_end > delete_end {
                    let skip_bytes = delete_end - piece_start;
                    let keep_bytes = piece_end - delete_end;
                    let lf_cnt = Self::compute_line_feeds_static(
                        buffers,
                        *location,
                        offset + skip_bytes,
                        keep_bytes,
                    );
                    leaves.push(LeafData::new(
                        *location,
                        offset + skip_bytes,
                        keep_bytes,
                        lf_cnt,
                    ));
                }
            }
        }
    }

    /// Get the total number of bytes in the document
    pub fn total_bytes(&self) -> usize {
        self.total_bytes
    }

    /// Get the total number of lines in the document
    /// Line count = line feeds + 1
    /// Returns None if any piece has unknown line count
    pub fn line_count(&self) -> Option<usize> {
        self.root.total_line_feeds().map(|lf| lf + 1)
    }

    /// Get tree statistics for debugging
    pub fn stats(&self) -> TreeStats {
        TreeStats {
            total_bytes: self.total_bytes,
            depth: self.root.depth(),
            leaf_count: self.root.count_leaves(),
            line_feed_count: self.root.total_line_feeds(),
        }
    }

    /// Get all leaves in order (for debugging)
    pub fn get_leaves(&self) -> Vec<LeafData> {
        let mut leaves = Vec::new();
        self.root.collect_leaves(&mut leaves);
        leaves
    }

    /// Convert byte offset to line/column position using tree's line metadata
    pub fn offset_to_position(
        &self,
        offset: usize,
        buffers: &[StringBuffer],
    ) -> Option<(usize, usize)> {
        if offset == 0 {
            return Some((0, 0));
        }

        let offset = offset.min(self.total_bytes);

        // Find the piece containing this offset
        if let Some(result) = self.root.find_by_offset(offset) {
            let piece_info = result.info;
            let bytes_before = result.bytes_before;

            // Count lines before this piece
            // If line count is unknown, return None - we can't reliably compute position
            let lines_before = match self.count_lines_before_offset(bytes_before) {
                Some(count) => count,
                None => {
                    // No line metadata available - cannot compute position reliably
                    return None;
                }
            };

            // Get the buffer for this piece
            let buffer_id = piece_info.location.buffer_id();
            if let Some(buffer) = buffers.get(buffer_id) {
                // Check if we have line starts available
                if let Some(line_starts) = buffer.get_line_starts() {
                    // Find position within the piece
                    let offset_in_piece = piece_info.offset_in_piece.unwrap_or(0);
                    let byte_offset_in_buffer = piece_info.offset + offset_in_piece;

                    // Find which line within the buffer
                    let line_in_buffer = line_starts
                        .binary_search(&byte_offset_in_buffer)
                        .unwrap_or_else(|i| i.saturating_sub(1));

                    // Find which line the piece starts at in the buffer
                    let piece_start_line = line_starts
                        .binary_search(&piece_info.offset)
                        .unwrap_or_else(|i| i.saturating_sub(1));

                    // Calculate line relative to piece start (not buffer start)
                    let line_in_piece = line_in_buffer - piece_start_line;

                    // Calculate the document line number
                    let doc_line = lines_before + line_in_piece;

                    // Calculate column
                    let column = if line_in_piece == 0 && bytes_before == 0 {
                        // Fast path: piece is at document start, so column is just offset within piece
                        offset_in_piece
                    } else if line_in_piece == 0 {
                        // We're on the first line of this piece, but the document line may have
                        // started before this piece (after modifications). Find the actual line start.
                        let line_start = self.position_to_offset(doc_line, 0, buffers);
                        offset.saturating_sub(line_start)
                    } else {
                        // Line starts within this piece
                        // Find where the line starts within the piece
                        let mut count = 0;
                        let mut line_start_in_buf = piece_info.offset;
                        for &ls in line_starts.iter() {
                            if ls > piece_info.offset && ls < piece_info.offset + piece_info.bytes {
                                count += 1;
                                if count == line_in_piece {
                                    line_start_in_buf = ls;
                                    break;
                                }
                            }
                        }
                        let line_start_offset_in_piece = line_start_in_buf - piece_info.offset;
                        offset_in_piece - line_start_offset_in_piece
                    };

                    return Some((doc_line, column));
                }
                // No line starts available - return None
            }
        }

        // Fallback: end of document
        // Only if we have line metadata
        match self.line_count() {
            Some(line_count) => {
                let last_line = line_count.saturating_sub(1);
                let line_start = self.position_to_offset(last_line, 0, buffers);
                let column = self.total_bytes.saturating_sub(line_start);
                Some((last_line, column))
            }
            None => {
                // No line metadata - cannot compute position
                None
            }
        }
    }

    /// Convert line/column position to byte offset using tree's line metadata
    pub fn position_to_offset(
        &self,
        line: usize,
        column: usize,
        buffers: &[StringBuffer],
    ) -> usize {
        if line == 0 && column == 0 {
            return 0;
        }

        // Traverse tree to find the piece containing the target line
        self.find_offset_for_line(line, column, buffers)
            .unwrap_or(self.total_bytes)
    }

    /// Helper: count line feeds before a given byte offset
    /// Returns None if any piece has unknown line count
    fn count_lines_before_offset(&self, byte_offset: usize) -> Option<usize> {
        self.count_lines_in_range(0, byte_offset)
    }

    /// Helper: count line feeds in a byte range
    /// Returns None if any piece has unknown line count
    fn count_lines_in_range(&self, start: usize, end: usize) -> Option<usize> {
        if start >= end {
            return Some(0);
        }

        self.root.count_lines_in_byte_range(0, start, end)
    }

    /// Helper: find byte offset for a given line/column
    fn find_offset_for_line(
        &self,
        target_line: usize,
        column: usize,
        buffers: &[StringBuffer],
    ) -> Option<usize> {
        self.root
            .find_byte_offset_for_line(0, 0, target_line, column, buffers)
    }

    /// Get the byte range for a specific line
    pub fn line_range(
        &self,
        line: usize,
        buffers: &[StringBuffer],
    ) -> Option<(usize, Option<usize>)> {
        // Check if line exists
        let line_count = self.line_count()?;
        if line >= line_count {
            return None;
        }

        let start = self.position_to_offset(line, 0, buffers);
        let end = if line + 1 < line_count {
            Some(self.position_to_offset(line + 1, 0, buffers))
        } else {
            None
        };
        Some((start, end))
    }

    /// Iterate through pieces overlapping a byte range
    /// Does ONE O(log n) tree traversal, then iterates sequentially
    pub fn iter_pieces_in_range(&self, start: usize, end: usize) -> PieceRangeIter {
        PieceRangeIter::new(&self.root, start, end)
    }

    /// Apply multiple edits in a single tree traversal + rebuild
    ///
    /// # Arguments
    /// * `edits` - Vec of (position, delete_len, insert_text), MUST be sorted descending by position
    /// * `buffers` - Reference to string buffers (for line feed computation)
    /// * `add_text_fn` - Function to add text to buffer, returns (BufferLocation, offset, bytes)
    ///
    /// # Complexity
    /// O(pieces + edits) instead of O(pieces × edits)
    ///
    /// # Returns
    /// The net change in total bytes
    pub fn apply_bulk_edits<F>(
        &mut self,
        edits: &[(usize, usize, &str)],
        buffers: &[StringBuffer],
        mut add_text_fn: F,
    ) -> isize
    where
        F: FnMut(&str) -> (BufferLocation, usize, usize, Option<usize>),
    {
        if edits.is_empty() {
            return 0;
        }

        // 1. Collect all split points (both start and end of each edit range)
        let mut split_points: Vec<usize> = Vec::with_capacity(edits.len() * 2);
        for (pos, del_len, _) in edits {
            split_points.push(*pos);
            if *del_len > 0 {
                let end = pos.saturating_add(*del_len).min(self.total_bytes);
                if end > *pos {
                    split_points.push(end);
                }
            }
        }
        split_points.sort_unstable();
        split_points.dedup();

        // 2. Collect all leaves, splitting at all required points
        let mut leaves = Vec::new();
        self.collect_leaves_with_multi_split(
            &self.root.clone(),
            0,
            &split_points,
            &mut leaves,
            buffers,
        );

        // 3. Build edit ranges for quick lookup (sorted descending by position)
        // Each edit: (start, end, insert_leaf)
        let mut edit_ranges: Vec<(usize, usize, Option<LeafData>)> =
            Vec::with_capacity(edits.len());
        for (pos, del_len, text) in edits {
            let del_end = pos.saturating_add(*del_len).min(self.total_bytes);
            let insert_leaf = if !text.is_empty() {
                let (location, offset, bytes, lf_cnt) = add_text_fn(text);
                Some(LeafData::new(location, offset, bytes, lf_cnt))
            } else {
                None
            };
            edit_ranges.push((*pos, del_end, insert_leaf));
        }

        // 4. Apply edits to leaves
        // Edits are sorted descending by position, so:
        //   edit_ranges[len-1] has smallest position, edit_ranges[0] has largest
        // We iterate leaves ascending, and for each leaf we:
        //   1. First add any inserts that belong BEFORE this leaf
        //   2. Then add the leaf (if not deleted)
        let mut new_leaves: Vec<LeafData> = Vec::with_capacity(leaves.len() + edits.len());
        let mut current_offset = 0;
        let mut edit_idx = edit_ranges.len(); // Points past the end; we access [edit_idx-1]

        for leaf in leaves {
            let leaf_start = current_offset;
            let leaf_end = current_offset + leaf.bytes;

            // First, add any inserts whose position is <= leaf_start
            // These inserts should appear BEFORE this leaf's content
            while edit_idx > 0 {
                let (edit_start, _edit_end, ref insert_leaf) = edit_ranges[edit_idx - 1];

                // If this edit's position is after where we are, stop
                if edit_start > leaf_start {
                    break;
                }

                // Insert belongs at or before leaf_start
                if let Some(insert) = insert_leaf {
                    new_leaves.push(*insert);
                }
                edit_idx -= 1;
            }

            // Check if this leaf overlaps with ANY edit's delete range
            // We check ALL edits, not just remaining ones, because edits
            // processed in the insert loop above may still have deletions
            let mut keep_leaf = true;

            for (edit_start, edit_end, _) in &edit_ranges {
                // If edit's delete range is entirely after this leaf, skip
                if *edit_start >= leaf_end {
                    continue;
                }

                // If edit has no deletion (edit_start == edit_end), skip
                if *edit_start == *edit_end {
                    continue;
                }

                // If edit's delete range is entirely before this leaf, skip
                if *edit_end <= leaf_start {
                    continue;
                }

                // Leaf overlaps with this edit's delete range - filter it out
                if leaf_start >= *edit_start && leaf_end <= *edit_end {
                    keep_leaf = false;
                    break;
                }
            }

            if keep_leaf {
                new_leaves.push(leaf);
            }

            current_offset = leaf_end;
        }

        // Handle any remaining inserts at the end of document
        while edit_idx > 0 {
            if let Some(insert) = &edit_ranges[edit_idx - 1].2 {
                new_leaves.push(*insert);
            }
            edit_idx -= 1;
        }

        // 5. Calculate byte delta
        let old_bytes = self.total_bytes;
        let mut new_bytes: usize = 0;
        for leaf in &new_leaves {
            new_bytes += leaf.bytes;
        }
        let delta = new_bytes as isize - old_bytes as isize;

        // 6. Single balanced tree rebuild
        self.root = Self::build_balanced(&new_leaves);
        self.total_bytes = new_bytes;

        delta
    }

    /// Collect leaves, splitting at multiple points in one traversal
    fn collect_leaves_with_multi_split(
        &self,
        node: &Arc<PieceTreeNode>,
        current_offset: usize,
        split_points: &[usize],
        leaves: &mut Vec<LeafData>,
        buffers: &[StringBuffer],
    ) {
        match node.as_ref() {
            PieceTreeNode::Internal {
                left_bytes,
                left,
                right,
                ..
            } => {
                // Recurse into both subtrees
                self.collect_leaves_with_multi_split(
                    left,
                    current_offset,
                    split_points,
                    leaves,
                    buffers,
                );
                self.collect_leaves_with_multi_split(
                    right,
                    current_offset + left_bytes,
                    split_points,
                    leaves,
                    buffers,
                );
            }
            PieceTreeNode::Leaf {
                location,
                offset,
                bytes,
                line_feed_cnt,
            } => {
                if *bytes == 0 {
                    return;
                }

                let piece_start = current_offset;
                let piece_end = current_offset + bytes;

                // Find split points within this piece
                let mut split_offsets: Vec<usize> = Vec::new();
                for &sp in split_points {
                    if sp > piece_start && sp < piece_end {
                        split_offsets.push(sp - piece_start);
                    }
                }

                if split_offsets.is_empty() {
                    // No splits needed, add entire piece
                    leaves.push(LeafData::new(*location, *offset, *bytes, *line_feed_cnt));
                } else {
                    // Split the piece at each point
                    split_offsets.sort_unstable();
                    split_offsets.dedup();

                    let mut prev_offset = 0;
                    for split_offset in split_offsets {
                        if split_offset > prev_offset {
                            let chunk_bytes = split_offset - prev_offset;
                            let lf_cnt = Self::compute_line_feeds_static(
                                buffers,
                                *location,
                                offset + prev_offset,
                                chunk_bytes,
                            );
                            leaves.push(LeafData::new(
                                *location,
                                offset + prev_offset,
                                chunk_bytes,
                                lf_cnt,
                            ));
                        }
                        prev_offset = split_offset;
                    }

                    // Add remaining part after last split
                    if prev_offset < *bytes {
                        let remaining = bytes - prev_offset;
                        let lf_cnt = Self::compute_line_feeds_static(
                            buffers,
                            *location,
                            offset + prev_offset,
                            remaining,
                        );
                        leaves.push(LeafData::new(
                            *location,
                            offset + prev_offset,
                            remaining,
                            lf_cnt,
                        ));
                    }
                }
            }
        }
    }
}

/// A view into a piece's data within the document
#[derive(Debug, Clone)]
pub struct PieceView {
    /// The location of this piece (which buffer it references)
    pub location: BufferLocation,
    /// Offset within the source buffer where this piece starts
    pub buffer_offset: usize,
    /// Number of bytes in this piece
    pub bytes: usize,
    /// Byte offset where this piece starts in the document
    pub doc_offset: usize,
    /// Number of line feeds in this piece (None if unknown for large files)
    pub line_feed_cnt: Option<usize>,
}

/// Iterator over pieces in a byte range
/// Performs ONE O(log n) traversal to collect pieces, then iterates in O(1) per piece
pub struct PieceRangeIter {
    pieces: Vec<PieceView>,
    current_index: usize,
}

impl PieceRangeIter {
    fn new(root: &Arc<PieceTreeNode>, start: usize, end: usize) -> Self {
        let mut pieces = Vec::new();
        Self::collect_pieces(root, 0, start, end, &mut pieces);
        PieceRangeIter {
            pieces,
            current_index: 0,
        }
    }

    /// Recursively collect all pieces that overlap [start, end)
    fn collect_pieces(
        node: &Arc<PieceTreeNode>,
        doc_offset: usize,
        range_start: usize,
        range_end: usize,
        pieces: &mut Vec<PieceView>,
    ) {
        match node.as_ref() {
            PieceTreeNode::Internal {
                left_bytes,
                left,
                right,
                ..
            } => {
                let left_end = doc_offset + left_bytes;

                // Check if left subtree overlaps with range
                if range_start < left_end {
                    Self::collect_pieces(left, doc_offset, range_start, range_end, pieces);
                }

                // Check if right subtree overlaps with range
                if range_end > left_end {
                    Self::collect_pieces(right, left_end, range_start, range_end, pieces);
                }
            }
            PieceTreeNode::Leaf {
                location,
                offset,
                bytes,
                line_feed_cnt,
            } => {
                let piece_end = doc_offset + bytes;

                // Check if this piece overlaps with the range
                if doc_offset < range_end && piece_end > range_start {
                    pieces.push(PieceView {
                        location: *location,
                        buffer_offset: *offset,
                        bytes: *bytes,
                        doc_offset,
                        line_feed_cnt: *line_feed_cnt,
                    });
                }
            }
        }
    }
}

impl Iterator for PieceRangeIter {
    type Item = PieceView;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_index < self.pieces.len() {
            let piece = self.pieces[self.current_index].clone();
            self.current_index += 1;
            Some(piece)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Helper to create test buffers
    fn test_buffers() -> Vec<StringBuffer> {
        vec![
            StringBuffer::new(0, vec![b'a'; 100]), // Buffer 0: 100 'a's
            StringBuffer::new(1, vec![b'b'; 50]),  // Buffer 1: 50 'b's
            StringBuffer::new(2, vec![b'c'; 25]),  // Buffer 2: 25 'c's
        ]
    }

    #[test]
    fn test_create_empty() {
        let tree = PieceTree::empty();
        assert_eq!(tree.total_bytes(), 0);
    }

    #[test]
    fn test_create_with_initial_piece() {
        let tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));
        assert_eq!(tree.total_bytes(), 100);
    }

    #[test]
    fn test_insert_at_end() {
        let buffers = test_buffers();
        let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));
        tree.insert(100, BufferLocation::Added(1), 0, 50, Some(0), &buffers);
        assert_eq!(tree.total_bytes(), 150);
    }

    #[test]
    fn test_insert_in_middle() {
        let buffers = test_buffers();
        let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));
        tree.insert(50, BufferLocation::Added(2), 0, 25, Some(0), &buffers);
        assert_eq!(tree.total_bytes(), 125);
        let stats = tree.stats();
        assert_eq!(stats.leaf_count, 3); // Original piece split + new piece
    }

    #[test]
    fn test_delete() {
        let buffers = test_buffers();
        let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));
        tree.delete(25, 50, &buffers);
        assert_eq!(tree.total_bytes(), 50);
    }

    #[test]
    fn test_delete_at_boundaries() {
        let buffers = test_buffers();
        let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));

        // Delete from start
        tree.delete(0, 10, &buffers);
        assert_eq!(tree.total_bytes(), 90);

        // Delete from end
        tree.delete(80, 10, &buffers);
        assert_eq!(tree.total_bytes(), 80);
    }

    #[test]
    fn test_multiple_inserts_and_deletes() {
        let buffers = test_buffers();
        let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));

        tree.insert(50, BufferLocation::Added(1), 0, 20, Some(0), &buffers);
        assert_eq!(tree.total_bytes(), 120);

        tree.delete(40, 30, &buffers);
        assert_eq!(tree.total_bytes(), 90);

        tree.insert(0, BufferLocation::Added(1), 20, 10, Some(0), &buffers);
        assert_eq!(tree.total_bytes(), 100);
    }

    #[test]
    fn test_rebalancing_many_inserts() {
        let buffers = test_buffers();
        let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));

        // Insert many times, which could create unbalanced tree
        for i in 0..20 {
            tree.insert(i * 5, BufferLocation::Added(1), i, 1, Some(0), &buffers);
        }

        let stats = tree.stats();
        assert_eq!(stats.total_bytes, 120);
        // Each insert splits pieces, so we expect many leaves
        // Exact count depends on implementation details, but should be > 20
        assert!(stats.leaf_count > 20);
        assert!(stats.leaf_count < 50); // Reasonable upper bound

        // Depth should be reasonable due to rebalancing
        let max_expected_depth = 2 * (stats.leaf_count as f64).log2().ceil() as usize;
        assert!(
            stats.depth <= max_expected_depth + 2,
            "Tree depth {} exceeds max {} for {} leaves",
            stats.depth,
            max_expected_depth,
            stats.leaf_count
        );
    }

    #[test]
    fn test_find_by_offset() {
        let tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));

        let info = tree.find_by_offset(50).unwrap();
        assert_eq!(info.location, BufferLocation::Stored(0));
        assert_eq!(info.offset_in_piece, Some(50));

        // Out of bounds
        assert!(tree.find_by_offset(100).is_none());
    }

    #[test]
    fn test_find_after_inserts() {
        let buffers = test_buffers();
        let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));
        tree.insert(50, BufferLocation::Added(1), 0, 25, Some(0), &buffers);

        // Should find in added section
        let info = tree.find_by_offset(50).unwrap();
        assert_eq!(info.location, BufferLocation::Added(1));
    }

    #[test]
    fn test_offset_to_position_column_after_modification() {
        // This test reproduces a bug where offset_to_position returns incorrect
        // column values after buffer modifications.
        //
        // Initial content: "fn foo(val: i32) {\n    val + 1\n}\n"
        // After deleting "val" and inserting "value" twice:
        // Buffer becomes: "fn foo(value: i32) {\n    value + 1\n}\n"
        //
        // Position 25 should be line 1, column 4 (the 'v' in second "value")
        // But the bug causes it to return column 0.

        // Create buffer with initial content
        let initial = b"fn foo(val: i32) {\n    val + 1\n}\n";
        let buffer = StringBuffer::new(0, initial.to_vec());
        let buffers = vec![buffer.clone()];

        let mut tree = PieceTree::new(
            BufferLocation::Stored(0),
            0,
            initial.len(),
            Some(initial.iter().filter(|&&b| b == b'\n').count()),
        );

        // Verify initial position works correctly
        // Position 23 = 'v' of second "val" on line 1 (after newline at pos 18)
        let pos = tree.offset_to_position(23, &buffers);
        assert_eq!(
            pos,
            Some((1, 4)),
            "Initial: position 23 should be line 1, column 4"
        );

        // Now simulate LSP rename operations:
        // 1. Delete "val" at position 23 (3 bytes)
        // 2. Insert "value" at position 23 (5 bytes)
        // 3. Delete "val" at position 7 (3 bytes)
        // 4. Insert "value" at position 7 (5 bytes)

        // First modification: delete "val" at position 23
        tree.delete(23, 3, &buffers);

        // Insert "value" - need a new buffer
        let value_buf = StringBuffer::new(1, b"value".to_vec());
        let buffers = vec![buffer.clone(), value_buf.clone()];
        tree.insert(23, BufferLocation::Added(1), 0, 5, Some(0), &buffers);

        // Second modification: delete "val" at position 7
        tree.delete(7, 3, &buffers);

        // Insert "value" - use another buffer
        let value_buf2 = StringBuffer::new(2, b"value".to_vec());
        let buffers = vec![buffer.clone(), value_buf.clone(), value_buf2];
        tree.insert(7, BufferLocation::Added(2), 0, 5, Some(0), &buffers);

        // Buffer is now: "fn foo(value: i32) {\n    value + 1\n}\n"
        // Line 0: "fn foo(value: i32) {\n" = 21 bytes (positions 0-20)
        // Line 1: "    value + 1\n" starts at position 21
        // Position 25 = 21 + 4 = line 1, column 4

        // This is where the bug manifests
        let pos = tree.offset_to_position(25, &buffers);
        assert_eq!(
            pos,
            Some((1, 4)),
            "After modification: position 25 should be line 1, column 4"
        );

        // Also test position 21 (start of line 1)
        let pos = tree.offset_to_position(21, &buffers);
        assert_eq!(pos, Some((1, 0)), "Position 21 should be line 1, column 0");
    }

    // ============== Tests for apply_bulk_edits ==============

    // Helper to pre-allocate buffers for bulk edit tests
    fn prepare_bulk_edit_buffers(
        buffers: &mut Vec<StringBuffer>,
        texts: &[&str],
    ) -> Vec<(BufferLocation, usize, usize, Option<usize>)> {
        let mut infos = Vec::new();
        for (i, text) in texts.iter().enumerate() {
            let id = buffers.len();
            let bytes = text.as_bytes().to_vec();
            let lf = bytes.iter().filter(|&&b| b == b'\n').count();
            let len = bytes.len();
            buffers.push(StringBuffer::new(id, bytes));
            infos.push((BufferLocation::Added(id), 0, len, Some(lf)));
            let _ = i; // suppress warning
        }
        infos
    }

    #[test]
    fn test_bulk_edit_single_insert() {
        let mut buffers = vec![StringBuffer::new(0, b"hello world".to_vec())];
        let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 11, Some(0));

        // Pre-allocate buffer for the insert
        let infos = prepare_bulk_edit_buffers(&mut buffers, &["!"]);

        // Insert "!" at position 11 (end)
        let edits: Vec<(usize, usize, &str)> = vec![(11, 0, "!")];
        let mut idx = 0;

        let delta = tree.apply_bulk_edits(&edits, &buffers, |_text| {
            let info = infos[idx];
            idx += 1;
            info
        });

        assert_eq!(delta, 1);
        assert_eq!(tree.total_bytes(), 12);
    }

    #[test]
    fn test_bulk_edit_single_delete() {
        let buffers = vec![StringBuffer::new(0, b"hello world".to_vec())];
        let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 11, Some(0));

        // Delete "world" (positions 6-11) - no insert, so no buffer needed
        let edits: Vec<(usize, usize, &str)> = vec![(6, 5, "")];

        let delta = tree.apply_bulk_edits(&edits, &buffers, |_text| {
            (BufferLocation::Added(1), 0, 0, Some(0))
        });

        assert_eq!(delta, -5);
        assert_eq!(tree.total_bytes(), 6);
    }

    #[test]
    fn test_bulk_edit_single_replace() {
        let mut buffers = vec![StringBuffer::new(0, b"hello world".to_vec())];
        let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 11, Some(0));

        // Pre-allocate buffer for the replacement
        let infos = prepare_bulk_edit_buffers(&mut buffers, &["rust"]);

        // Replace "world" with "rust"
        let edits: Vec<(usize, usize, &str)> = vec![(6, 5, "rust")];
        let mut idx = 0;

        let delta = tree.apply_bulk_edits(&edits, &buffers, |_text| {
            let info = infos[idx];
            idx += 1;
            info
        });

        assert_eq!(delta, -1); // "world" (5) -> "rust" (4)
        assert_eq!(tree.total_bytes(), 10);
    }

    #[test]
    fn test_bulk_edit_multiple_inserts_descending() {
        // Edits must be sorted descending by position
        let mut buffers = vec![StringBuffer::new(0, b"abc".to_vec())];
        let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 3, Some(0));

        // Pre-allocate buffers for 4 inserts
        let infos = prepare_bulk_edit_buffers(&mut buffers, &["X", "X", "X", "X"]);

        // Insert "X" at positions 3, 2, 1, 0 (descending order)
        let edits: Vec<(usize, usize, &str)> = vec![
            (3, 0, "X"), // Insert at end
            (2, 0, "X"), // Insert before 'c'
            (1, 0, "X"), // Insert before 'b'
            (0, 0, "X"), // Insert at start
        ];
        let mut idx = 0;

        let delta = tree.apply_bulk_edits(&edits, &buffers, |_text| {
            let info = infos[idx];
            idx += 1;
            info
        });

        assert_eq!(delta, 4);
        assert_eq!(tree.total_bytes(), 7); // "XaXbXcX"
    }

    #[test]
    fn test_bulk_edit_multiple_deletes_descending() {
        let buffers = vec![StringBuffer::new(0, b"abcdefgh".to_vec())];
        let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 8, Some(0));

        // Delete chars at positions 6, 4, 2, 0 (descending order) - no inserts
        let edits: Vec<(usize, usize, &str)> = vec![
            (6, 1, ""), // Delete 'g'
            (4, 1, ""), // Delete 'e'
            (2, 1, ""), // Delete 'c'
            (0, 1, ""), // Delete 'a'
        ];

        let delta = tree.apply_bulk_edits(&edits, &buffers, |_| {
            (BufferLocation::Added(1), 0, 0, Some(0))
        });

        assert_eq!(delta, -4);
        assert_eq!(tree.total_bytes(), 4); // "bdfh"
    }

    #[test]
    fn test_bulk_edit_empty_edits() {
        let buffers = vec![StringBuffer::new(0, b"hello".to_vec())];
        let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 5, Some(0));

        let edits: Vec<(usize, usize, &str)> = vec![];

        let delta = tree.apply_bulk_edits(&edits, &buffers, |_| {
            (BufferLocation::Added(1), 0, 0, Some(0))
        });

        assert_eq!(delta, 0);
        assert_eq!(tree.total_bytes(), 5);
    }

    #[test]
    fn test_bulk_edit_consistency_check() {
        // Test that piece sum equals total_bytes after bulk edit
        let mut buffers = vec![StringBuffer::new(0, b"0123456789".to_vec())];
        let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 10, Some(0));

        // Pre-allocate buffers for the inserts
        let infos = prepare_bulk_edit_buffers(&mut buffers, &["XX", "Y", "ZZZ"]);

        // Multiple mixed operations
        let edits: Vec<(usize, usize, &str)> = vec![
            (8, 1, "XX"),  // Replace '8' with 'XX'
            (5, 2, "Y"),   // Replace '56' with 'Y'
            (2, 0, "ZZZ"), // Insert 'ZZZ' at position 2
        ];
        let mut idx = 0;

        tree.apply_bulk_edits(&edits, &buffers, |_text| {
            let info = infos[idx];
            idx += 1;
            info
        });

        // Verify consistency
        let leaves = tree.get_leaves();
        let sum: usize = leaves.iter().map(|l| l.bytes).sum();
        assert_eq!(
            sum,
            tree.total_bytes(),
            "Piece sum {} != total_bytes {}",
            sum,
            tree.total_bytes()
        );
    }

    #[test]
    fn test_bulk_edit_vs_sequential_equivalence() {
        // Verify that bulk edit produces same result as sequential edits
        let original_content = b"The quick brown fox";

        // Setup for bulk edit
        let mut buffers1 = vec![StringBuffer::new(0, original_content.to_vec())];
        let mut tree1 = PieceTree::new(BufferLocation::Stored(0), 0, 19, Some(0));

        // Pre-allocate buffers for the replacements
        let infos = prepare_bulk_edit_buffers(&mut buffers1, &["red", "slow"]);

        // Setup for sequential edit
        let mut buffers2 = vec![StringBuffer::new(0, original_content.to_vec())];
        let mut tree2 = PieceTree::new(BufferLocation::Stored(0), 0, 19, Some(0));
        let mut next_id2 = 1;

        // Edits: Replace "quick" with "slow", "brown" with "red"
        // Positions in original: quick=4-9, brown=10-15
        // Must be sorted descending
        let edits: Vec<(usize, usize, &str)> = vec![
            (10, 5, "red"), // Replace "brown" at 10
            (4, 5, "slow"), // Replace "quick" at 4
        ];
        let mut idx = 0;

        // Apply bulk edit
        tree1.apply_bulk_edits(&edits, &buffers1, |_text| {
            let info = infos[idx];
            idx += 1;
            info
        });

        // Apply sequential edits (in descending order to match)
        // First: replace "brown" at 10
        tree2.delete(10, 5, &buffers2);
        buffers2.push(StringBuffer::new(next_id2, b"red".to_vec()));
        tree2.insert(
            10,
            BufferLocation::Added(next_id2),
            0,
            3,
            Some(0),
            &buffers2,
        );
        next_id2 += 1;

        // Second: replace "quick" at 4
        tree2.delete(4, 5, &buffers2);
        buffers2.push(StringBuffer::new(next_id2, b"slow".to_vec()));
        tree2.insert(4, BufferLocation::Added(next_id2), 0, 4, Some(0), &buffers2);

        assert_eq!(
            tree1.total_bytes(),
            tree2.total_bytes(),
            "Bulk edit total_bytes {} != sequential {}",
            tree1.total_bytes(),
            tree2.total_bytes()
        );
    }
}

#[cfg(test)]
mod property_tests {
    use super::*;
    use proptest::prelude::*;

    // Helper to create test buffers - using larger buffers for property tests
    fn test_buffers_large() -> Vec<StringBuffer> {
        vec![
            StringBuffer::new(0, vec![b'a'; 10000]), // Large buffer
            StringBuffer::new(1, vec![b'b'; 10000]),
        ]
    }

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
                (0usize..200, 1usize..50)
                    .prop_map(|(offset, bytes)| { Operation::Insert { offset, bytes } }),
                (0usize..200, 1usize..50)
                    .prop_map(|(offset, bytes)| { Operation::Delete { offset, bytes } }),
            ],
            0..50,
        )
    }

    // More aggressive operation strategy that creates more internal nodes
    fn aggressive_operation_strategy() -> impl Strategy<Value = Vec<Operation>> {
        prop::collection::vec(
            prop_oneof![
                // More inserts, smaller chunks to create more splits
                3 => (0usize..100, 1usize..20).prop_map(|(offset, bytes)| {
                    Operation::Insert { offset, bytes }
                }),
                // Some deletes
                1 => (0usize..100, 1usize..30).prop_map(|(offset, bytes)| {
                    Operation::Delete { offset, bytes }
                }),
            ],
            10..30, // More operations to force tree growth
        )
    }

    proptest! {
        #[test]
        fn prop_total_bytes_consistency(operations in operation_strategy()) {
            let buffers = test_buffers_large();
            let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));
            let mut expected_bytes = 100;

            for op in operations {
                match op {
                    Operation::Insert { offset, bytes } => {
                        let offset = offset.min(tree.total_bytes());
                        let buffer_len = buffers[1].get_data().map(|d| d.len()).unwrap_or(0);
                        let bytes = bytes.min(buffer_len);
                        tree.insert(offset, BufferLocation::Added(1), 0, bytes, Some(0), &buffers);
                        expected_bytes += bytes;
                    }
                    Operation::Delete { offset, bytes } => {
                        if offset < tree.total_bytes() {
                            let actual_delete = bytes.min(tree.total_bytes() - offset);
                            tree.delete(offset, bytes, &buffers);
                            expected_bytes -= actual_delete;
                        }
                    }
                }
            }

            prop_assert_eq!(tree.total_bytes(), expected_bytes);
        }

        #[test]
        fn prop_tree_never_negative_bytes(operations in operation_strategy()) {
            let buffers = test_buffers_large();
            let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));

            for op in operations {
                match op {
                    Operation::Insert { offset, bytes } => {
                        let offset = offset.min(tree.total_bytes());
                        let buffer_len = buffers[1].get_data().map(|d| d.len()).unwrap_or(0);
                        let bytes = bytes.min(buffer_len);
                        tree.insert(offset, BufferLocation::Added(1), 0, bytes, Some(0), &buffers);
                    }
                    Operation::Delete { offset, bytes } => {
                        tree.delete(offset, bytes, &buffers);
                    }
                }

                // Tree should never have negative bytes (underflow would wrap to large number)
                prop_assert!(tree.total_bytes() < 10_000_000);
            }
        }

        #[test]
        fn prop_balanced_after_operations(operations in operation_strategy()) {
            let buffers = test_buffers_large();
            let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));

            for op in operations {
                match op {
                    Operation::Insert { offset, bytes } => {
                        let offset = offset.min(tree.total_bytes());
                        let buffer_len = buffers[1].get_data().map(|d| d.len()).unwrap_or(0);
                        let bytes = bytes.min(buffer_len);
                        tree.insert(offset, BufferLocation::Added(1), 0, bytes, Some(0), &buffers);
                    }
                    Operation::Delete { offset, bytes } => {
                        tree.delete(offset, bytes, &buffers);
                    }
                }
            }

            let stats = tree.stats();
            if stats.leaf_count > 1 {
                let max_depth = 2 * (stats.leaf_count as f64).log2().ceil() as usize;
                prop_assert!(stats.depth <= max_depth + 2, "Tree depth {} exceeds expected max {} for {} leaves", stats.depth, max_depth, stats.leaf_count);
            }
        }

        #[test]
        fn prop_insert_then_delete_equals_original(
            insert_offset in 0usize..100,
            insert_bytes in 1usize..50
        ) {
            let buffers = test_buffers_large();
            let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));
            let original_bytes = tree.total_bytes();

            let insert_offset = insert_offset.min(tree.total_bytes());
            let buffer_len = buffers[1].get_data().map(|d| d.len()).unwrap_or(0);
            let insert_bytes = insert_bytes.min(buffer_len);
            tree.insert(insert_offset, BufferLocation::Added(1), 0, insert_bytes, Some(0), &buffers);

            // Delete what we just inserted
            tree.delete(insert_offset, insert_bytes, &buffers);

            prop_assert_eq!(tree.total_bytes(), original_bytes);
        }

        #[test]
        fn prop_find_offset_in_bounds(
            offset in 0usize..100
        ) {
            let tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));

            let result = tree.find_by_offset(offset);
            prop_assert!(result.is_some());
        }

        #[test]
        fn prop_find_offset_out_of_bounds(
            offset in 100usize..1000
        ) {
            let tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));

            let result = tree.find_by_offset(offset);
            prop_assert!(result.is_none());
        }

        #[test]
        fn prop_sequential_inserts_maintain_order(
            count in 1usize..20,
            insert_size in 1usize..10
        ) {
            let buffers = test_buffers_large();
            let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 10, Some(0));

            for _i in 0..count {
                let buffer_len = buffers[1].get_data().map(|d| d.len()).unwrap_or(0);
                let insert_size = insert_size.min(buffer_len);
                tree.insert(tree.total_bytes(), BufferLocation::Added(1), 0, insert_size, Some(0), &buffers);
            }

            let expected_bytes = 10 + (count * insert_size);
            prop_assert_eq!(tree.total_bytes(), expected_bytes);
        }

        #[test]
        fn prop_delete_all_reaches_zero(
            delete_size in 1usize..10
        ) {
            let buffers = test_buffers_large();
            let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));

            while tree.total_bytes() > 0 {
                let to_delete = delete_size.min(tree.total_bytes());
                tree.delete(0, to_delete, &buffers);
            }

            prop_assert_eq!(tree.total_bytes(), 0);
        }
    }

    #[test]
    fn test_empty_delete() {
        let buffers = test_buffers_large();
        let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));
        tree.delete(50, 0, &buffers);
        assert_eq!(tree.total_bytes(), 100);
    }

    // ============== Property tests for apply_bulk_edits ==============

    // Strategy to generate bulk edit operations
    #[derive(Debug, Clone)]
    struct BulkEditOp {
        position: usize,
        delete_len: usize,
        insert_text: String,
    }

    fn bulk_edit_strategy() -> impl Strategy<Value = Vec<BulkEditOp>> {
        prop::collection::vec(
            (0usize..100, 0usize..20, "[a-zA-Z0-9]{0,10}").prop_map(
                |(position, delete_len, insert_text)| BulkEditOp {
                    position,
                    delete_len,
                    insert_text,
                },
            ),
            1..20,
        )
    }

    // Helper to pre-allocate buffers for property tests
    fn preallocate_buffers(
        buffers: &mut Vec<StringBuffer>,
        texts: &[String],
    ) -> Vec<(BufferLocation, usize, usize, Option<usize>)> {
        let mut infos = Vec::new();
        for text in texts {
            let id = buffers.len();
            let bytes = text.as_bytes().to_vec();
            let lf = bytes.iter().filter(|&&b| b == b'\n').count();
            let len = bytes.len();
            buffers.push(StringBuffer::new(id, bytes));
            infos.push((BufferLocation::Added(id), 0, len, Some(lf)));
        }
        infos
    }

    proptest! {
        /// Property: apply_bulk_edits maintains tree consistency
        /// Sum of piece lengths must equal total_bytes after bulk edit
        #[test]
        fn prop_bulk_edit_tree_consistency(ops in bulk_edit_strategy()) {
            let mut buffers = vec![StringBuffer::new(0, b"x".repeat(100).to_vec())];
            let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));

            // Sort edits by position descending (required by apply_bulk_edits)
            let mut ops = ops;
            ops.sort_by(|a, b| b.position.cmp(&a.position));

            // Pre-allocate all buffers
            let texts: Vec<String> = ops.iter().map(|op| op.insert_text.clone()).collect();
            let infos = preallocate_buffers(&mut buffers, &texts);

            // Clamp positions and delete lengths to valid ranges
            let edits: Vec<(usize, usize, &str)> = ops.iter()
                .map(|op| {
                    let pos = op.position.min(tree.total_bytes());
                    let del = op.delete_len.min(tree.total_bytes().saturating_sub(pos));
                    (pos, del, op.insert_text.as_str())
                })
                .collect();

            let mut idx = 0;
            tree.apply_bulk_edits(&edits, &buffers, |_text| {
                let info = infos[idx];
                idx += 1;
                info
            });

            // INVARIANT: Sum of all piece lengths must equal total_bytes
            let leaves = tree.get_leaves();
            let sum_of_pieces: usize = leaves.iter().map(|leaf| leaf.bytes).sum();
            prop_assert_eq!(
                sum_of_pieces,
                tree.total_bytes(),
                "After bulk edit: sum of pieces ({}) != total_bytes ({})",
                sum_of_pieces,
                tree.total_bytes()
            );
        }

        /// Property: apply_bulk_edits returns correct delta
        #[test]
        fn prop_bulk_edit_correct_delta(ops in bulk_edit_strategy()) {
            let mut buffers = vec![StringBuffer::new(0, b"x".repeat(100).to_vec())];
            let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));

            let original_bytes = tree.total_bytes();

            // Sort edits by position descending
            let mut ops = ops;
            ops.sort_by(|a, b| b.position.cmp(&a.position));

            // Pre-allocate all buffers
            let texts: Vec<String> = ops.iter().map(|op| op.insert_text.clone()).collect();
            let infos = preallocate_buffers(&mut buffers, &texts);

            let edits: Vec<(usize, usize, &str)> = ops.iter()
                .map(|op| {
                    let pos = op.position.min(tree.total_bytes());
                    let del = op.delete_len.min(tree.total_bytes().saturating_sub(pos));
                    (pos, del, op.insert_text.as_str())
                })
                .collect();

            let mut idx = 0;
            let delta = tree.apply_bulk_edits(&edits, &buffers, |_text| {
                let info = infos[idx];
                idx += 1;
                info
            });

            let actual_change = tree.total_bytes() as isize - original_bytes as isize;
            prop_assert_eq!(
                delta,
                actual_change,
                "Returned delta ({}) != actual change ({})",
                delta,
                actual_change
            );
        }

        /// Property: bulk edit with only inserts increases size correctly
        #[test]
        fn prop_bulk_edit_inserts_only(
            positions in prop::collection::vec(0usize..50, 1..10),
            insert_len in 1usize..10
        ) {
            let mut buffers = vec![StringBuffer::new(0, b"x".repeat(50).to_vec())];
            let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 50, Some(0));

            let insert_text = "a".repeat(insert_len);
            let original_bytes = tree.total_bytes();

            // Sort positions descending
            let mut positions = positions;
            positions.sort_by(|a, b| b.cmp(a));
            positions.dedup();

            // Pre-allocate all buffers
            let texts: Vec<String> = positions.iter().map(|_| insert_text.clone()).collect();
            let infos = preallocate_buffers(&mut buffers, &texts);

            let edits: Vec<(usize, usize, &str)> = positions
                .iter()
                .map(|&pos| (pos.min(tree.total_bytes()), 0, insert_text.as_str()))
                .collect();

            let mut idx = 0;
            tree.apply_bulk_edits(&edits, &buffers, |_text| {
                let info = infos[idx];
                idx += 1;
                info
            });

            let expected_bytes = original_bytes + edits.len() * insert_len;
            prop_assert_eq!(
                tree.total_bytes(),
                expected_bytes,
                "After {} inserts of {} bytes each: expected {} bytes, got {}",
                edits.len(),
                insert_len,
                expected_bytes,
                tree.total_bytes()
            );
        }

        /// Property: bulk edit with only deletes decreases size correctly
        #[test]
        fn prop_bulk_edit_deletes_only(
            ops in prop::collection::vec((0usize..80, 1usize..5), 1..10)
        ) {
            let buffers = vec![StringBuffer::new(0, b"x".repeat(100).to_vec())];
            let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));

            // Sort by position descending
            let mut ops = ops;
            ops.sort_by(|a, b| b.0.cmp(&a.0));

            // Remove overlapping deletes
            let mut edits: Vec<(usize, usize, &str)> = Vec::new();
            let mut last_affected_pos = tree.total_bytes();
            for (pos, del_len) in ops {
                if pos < last_affected_pos {
                    let actual_del = del_len.min(last_affected_pos - pos);
                    if actual_del > 0 {
                        edits.push((pos, actual_del, ""));
                        last_affected_pos = pos;
                    }
                }
            }

            let expected_delete: usize = edits.iter().map(|(_, d, _)| d).sum();

            tree.apply_bulk_edits(&edits, &buffers, |_| {
                (BufferLocation::Added(1), 0, 0, Some(0))
            });

            let expected_bytes = 100 - expected_delete;
            prop_assert_eq!(
                tree.total_bytes(),
                expected_bytes,
                "After deleting {} bytes: expected {} bytes, got {}",
                expected_delete,
                expected_bytes,
                tree.total_bytes()
            );
        }
    }

    proptest! {
        /// Property: Sum of all piece lengths must equal total_bytes
        /// This catches bugs like duplicate piece insertion
        #[test]
        fn prop_tree_consistency_piece_sum(operations in operation_strategy()) {
            let buffers = test_buffers_large();
            let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));

            for op in operations {
                match op {
                    Operation::Insert { offset, bytes } => {
                        let offset = offset.min(tree.total_bytes());
                        let buffer_len = buffers[1].get_data().map(|d| d.len()).unwrap_or(0);
                        let bytes = bytes.min(buffer_len);
                        tree.insert(offset, BufferLocation::Added(1), 0, bytes, Some(0), &buffers);
                    }
                    Operation::Delete { offset, bytes } => {
                        tree.delete(offset, bytes, &buffers);
                    }
                }

                // INVARIANT: Sum of all piece lengths must equal total_bytes
                let leaves = tree.get_leaves();
                let sum_of_pieces: usize = leaves.iter().map(|leaf| leaf.bytes).sum();
                prop_assert_eq!(
                    sum_of_pieces,
                    tree.total_bytes(),
                    "Tree inconsistency: sum of piece lengths ({}) != total_bytes ({})",
                    sum_of_pieces,
                    tree.total_bytes()
                );
            }
        }

        /// Property: Line feed count consistency
        /// Sum of all piece line_feed_cnt must equal tree's total line feeds
        #[test]
        fn prop_tree_consistency_line_feeds(operations in operation_strategy()) {
            let buffers = test_buffers_large();
            let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));

            for op in operations {
                match op {
                    Operation::Insert { offset, bytes } => {
                        let offset = offset.min(tree.total_bytes());
                        let buffer_len = buffers[1].get_data().map(|d| d.len()).unwrap_or(0);
                        let bytes = bytes.min(buffer_len);
                        tree.insert(offset, BufferLocation::Added(1), 0, bytes, Some(0), &buffers);
                    }
                    Operation::Delete { offset, bytes } => {
                        tree.delete(offset, bytes, &buffers);
                    }
                }

                // INVARIANT: Sum of all piece line feeds must equal tree's total
                let leaves = tree.get_leaves();
                let sum_of_line_feeds: Option<usize> = leaves.iter()
                    .try_fold(0, |acc, leaf| leaf.line_feed_cnt.map(|cnt| acc + cnt));
                let stats = tree.stats();
                prop_assert_eq!(
                    sum_of_line_feeds,
                    stats.line_feed_count,
                    "Line feed inconsistency: sum of piece line feeds ({:?}) != tree total ({:?})",
                    sum_of_line_feeds,
                    stats.line_feed_count
                );
            }
        }

        /// Aggressive consistency test designed to catch the duplicate piece insertion bug
        /// Uses more operations with smaller inserts to force internal node creation and splits
        #[test]
        fn prop_tree_consistency_aggressive(operations in aggressive_operation_strategy()) {
            let buffers = test_buffers_large();
            let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));

            // Prime the tree with several inserts to create internal nodes first
            // This increases the likelihood of hitting the bug scenario
            for i in 0..5 {
                let offset = (i * 17) % (tree.total_bytes().max(1));
                tree.insert(offset, BufferLocation::Added(1), i * 100, 10, Some(0), &buffers);
            }

            // Verify we have internal nodes
            prop_assert!(tree.stats().depth > 1, "Priming should create internal nodes");

            for (i, op) in operations.iter().enumerate() {
                match *op {
                    Operation::Insert { offset, bytes } => {
                        let offset = offset.min(tree.total_bytes());
                        let buffer_len = buffers[1].get_data().map(|d| d.len()).unwrap_or(0);
                        let bytes = bytes.min(buffer_len);
                        tree.insert(offset, BufferLocation::Added(1), 0, bytes, Some(0), &buffers);
                    }
                    Operation::Delete { offset, bytes } => {
                        tree.delete(offset, bytes, &buffers);
                    }
                }

                // CRITICAL INVARIANT: Sum of all piece lengths must equal total_bytes
                // This catches the duplicate piece insertion bug
                let leaves = tree.get_leaves();
                let sum_of_pieces: usize = leaves.iter().map(|leaf| leaf.bytes).sum();
                prop_assert_eq!(
                    sum_of_pieces,
                    tree.total_bytes(),
                    "Operation {}: Tree inconsistency after {:?}.\n\
                     Sum of piece lengths ({}) != total_bytes ({}).\n\
                     Tree depth: {}, leaves: {}.\n\
                     Pieces: {:?}",
                    i, op, sum_of_pieces, tree.total_bytes(),
                    tree.stats().depth, tree.stats().leaf_count,
                    leaves
                );
            }
        }
    }

    #[test]
    fn test_delete_beyond_end() {
        let buffers = test_buffers_large();
        let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));
        tree.delete(50, 100, &buffers); // Try to delete 100 bytes from offset 50
        assert_eq!(tree.total_bytes(), 50); // Should only delete 50 bytes
    }

    #[test]
    fn test_insert_zero_bytes() {
        let buffers = test_buffers_large();
        let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));
        tree.insert(50, BufferLocation::Added(1), 0, 0, Some(0), &buffers);
        assert_eq!(tree.total_bytes(), 100);
    }

    #[test]
    fn test_tree_consistency_after_insert() {
        // Regression test: verify tree consistency after each operation
        // This test creates enough inserts to force internal nodes, which is where the bug manifests
        let buffers = test_buffers_large();
        let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));

        // Do several inserts to create internal nodes and splits
        for i in 0..10 {
            let offset = (i * 13) % (tree.total_bytes().max(1)); // Varying offsets
            tree.insert(
                offset,
                BufferLocation::Added(1),
                i * 10,
                5,
                Some(0),
                &buffers,
            );

            // INVARIANT: sum of piece lengths must equal total_bytes
            let leaves = tree.get_leaves();
            let sum: usize = leaves.iter().map(|l| l.bytes).sum();
            assert_eq!(
                sum,
                tree.total_bytes(),
                "After insert {}: sum of pieces ({}) != total_bytes ({}).\nLeaves: {:?}",
                i,
                sum,
                tree.total_bytes(),
                leaves
            );
        }

        // Verify we actually created internal nodes
        let stats = tree.stats();
        assert!(
            stats.depth > 1,
            "Test should create internal nodes, but depth is {}",
            stats.depth
        );
    }

    #[test]
    fn test_duplicate_piece_bug_exact_scenario() {
        // This replicates the exact scenario that exposed the duplicate insertion bug
        let mut buffers = vec![StringBuffer::new(0, b"initial\ntext".to_vec())];
        let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 12, Some(1));

        // Delete all - creates an empty piece
        tree.delete(0, 12, &buffers);

        // Check tree consistency after delete
        let leaves = tree.get_leaves();
        let sum: usize = leaves.iter().map(|l| l.bytes).sum();
        assert_eq!(
            sum,
            tree.total_bytes(),
            "After delete: sum={}, total={}",
            sum,
            tree.total_bytes()
        );

        // Insert 'a' at position 0
        buffers.push(StringBuffer::new(1, b"a".to_vec()));
        tree.insert(0, BufferLocation::Added(1), 0, 1, Some(0), &buffers);

        // Check consistency
        let leaves = tree.get_leaves();
        let sum: usize = leaves.iter().map(|l| l.bytes).sum();
        assert_eq!(
            sum,
            tree.total_bytes(),
            "After first insert: sum={}, total={}. Leaves: {:?}",
            sum,
            tree.total_bytes(),
            leaves
        );

        // Insert 'b' at position 0 - this should trigger the bug with buggy code
        buffers.push(StringBuffer::new(2, b"b".to_vec()));
        tree.insert(0, BufferLocation::Added(2), 0, 1, Some(0), &buffers);

        // Check consistency - this will fail with the bug
        let leaves = tree.get_leaves();
        let sum: usize = leaves.iter().map(|l| l.bytes).sum();
        assert_eq!(
            sum,
            tree.total_bytes(),
            "After second insert: sum={}, total={}. Leaves: {:?}",
            sum,
            tree.total_bytes(),
            leaves
        );
    }

    // Property tests for PieceRangeIter
    proptest! {
        #[test]
        fn test_piece_iter_covers_exact_range(
            ops in aggressive_operation_strategy(),
            start in 0usize..100,
            len in 1usize..50
        ) {
            let mut buffers = vec![StringBuffer::new(0, b"x".repeat(100).to_vec())];
            let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));

            // Apply operations to build up tree
            for op in ops.iter() {
                match op {
                    Operation::Insert { offset, bytes } => {
                        let offset = (*offset).min(tree.total_bytes());
                        buffers.push(StringBuffer::new(buffers.len(), b"a".repeat(*bytes).to_vec()));
                        tree.insert(offset, BufferLocation::Added(buffers.len() - 1), 0, *bytes, Some(0), &buffers);
                    }
                    Operation::Delete { offset, bytes } => {
                        let offset = (*offset).min(tree.total_bytes());
                        let bytes = (*bytes).min(tree.total_bytes().saturating_sub(offset));
                        if bytes > 0 {
                            tree.delete(offset, bytes, &buffers);
                        }
                    }
                }
            }

            let total_bytes = tree.total_bytes();
            if total_bytes == 0 {
                return Ok(());
            }

            let start = start.min(total_bytes.saturating_sub(1));
            let end = (start + len).min(total_bytes);

            // Collect pieces using iterator
            let pieces: Vec<_> = tree.iter_pieces_in_range(start, end).collect();

            // Verify coverage: pieces should cover [start, end)
            if !pieces.is_empty() {
                let first_piece_start = pieces[0].doc_offset;
                let last_piece = &pieces[pieces.len() - 1];
                let last_piece_end = last_piece.doc_offset + last_piece.bytes;

                // First piece should start at or before requested start
                prop_assert!(first_piece_start <= start,
                    "First piece starts at {}, but requested start is {}", first_piece_start, start);

                // Last piece should end at or after requested end
                prop_assert!(last_piece_end >= end,
                    "Last piece ends at {}, but requested end is {}", last_piece_end, end);
            }
        }

        #[test]
        fn test_piece_iter_no_gaps(ops in aggressive_operation_strategy()) {
            let mut buffers = vec![StringBuffer::new(0, b"x".repeat(100).to_vec())];
            let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));

            for op in ops {
                match op {
                    Operation::Insert { offset, bytes } => {
                        let offset = offset.min(tree.total_bytes());
                        buffers.push(StringBuffer::new(buffers.len(), b"a".repeat(bytes).to_vec()));
                        tree.insert(offset, BufferLocation::Added(buffers.len() - 1), 0, bytes, Some(0), &buffers);
                    }
                    Operation::Delete { offset, bytes } => {
                        let offset = offset.min(tree.total_bytes());
                        let bytes = bytes.min(tree.total_bytes().saturating_sub(offset));
                        if bytes > 0 {
                            tree.delete(offset, bytes, &buffers);
                        }
                    }
                }
            }

            let total_bytes = tree.total_bytes();
            if total_bytes == 0 {
                return Ok(());
            }

            // Iterate over entire document
            let pieces: Vec<_> = tree.iter_pieces_in_range(0, total_bytes).collect();

            // Verify no gaps: each piece should start where previous one ended
            for i in 1..pieces.len() {
                let prev_end = pieces[i - 1].doc_offset + pieces[i - 1].bytes;
                let curr_start = pieces[i].doc_offset;
                prop_assert_eq!(prev_end, curr_start,
                    "Gap between piece {} (ends at {}) and piece {} (starts at {})",
                    i - 1, prev_end, i, curr_start);
            }
        }

        #[test]
        fn test_piece_iter_total_bytes_matches(ops in aggressive_operation_strategy()) {
            let mut buffers = vec![StringBuffer::new(0, b"x".repeat(100).to_vec())];
            let mut tree = PieceTree::new(BufferLocation::Stored(0), 0, 100, Some(0));

            for op in ops {
                match op {
                    Operation::Insert { offset, bytes } => {
                        let offset = offset.min(tree.total_bytes());
                        buffers.push(StringBuffer::new(buffers.len(), b"a".repeat(bytes).to_vec()));
                        tree.insert(offset, BufferLocation::Added(buffers.len() - 1), 0, bytes, Some(0), &buffers);
                    }
                    Operation::Delete { offset, bytes } => {
                        let offset = offset.min(tree.total_bytes());
                        let bytes = bytes.min(tree.total_bytes().saturating_sub(offset));
                        if bytes > 0 {
                            tree.delete(offset, bytes, &buffers);
                        }
                    }
                }
            }

            let total_bytes = tree.total_bytes();
            if total_bytes == 0 {
                return Ok(());
            }

            // Sum of piece bytes should equal total bytes
            let pieces: Vec<_> = tree.iter_pieces_in_range(0, total_bytes).collect();
            let sum_bytes: usize = pieces.iter().map(|p| p.bytes).sum();
            prop_assert_eq!(sum_bytes, total_bytes,
                "Sum of piece bytes ({}) doesn't match total_bytes ({})", sum_bytes, total_bytes);
        }

        /// Property test that verifies offset_to_position returns correct line/column
        /// after buffer modifications. This catches the bug where column calculation
        /// was incorrect after insertions/deletions.
        #[test]
        fn prop_offset_to_position_correct_after_modifications(
            ops in prop::collection::vec(
                prop_oneof![
                    // Insert with newlines
                    (0usize..50, prop::collection::vec(
                        prop_oneof![
                            Just(b'a'),
                            Just(b'\n'),
                        ],
                        1..20
                    )).prop_map(|(offset, bytes)| (offset, bytes, true)),
                    // Delete
                    (0usize..50, 1usize..10).prop_map(|(offset, _bytes)| (offset, vec![], false)),
                ],
                5..20
            ),
            test_offsets in prop::collection::vec(0usize..100, 3..10)
        ) {
            // Start with content that has newlines
            let initial = b"Hello\nWorld\nTest\n";
            let mut content = initial.to_vec();

            let mut buffers = vec![StringBuffer::new(0, initial.to_vec())];
            let newline_count = initial.iter().filter(|&&b| b == b'\n').count();
            let mut tree = PieceTree::new(
                BufferLocation::Stored(0),
                0,
                initial.len(),
                Some(newline_count),
            );

            // Apply operations, tracking actual content
            for (offset, bytes, is_insert) in ops {
                if is_insert && !bytes.is_empty() {
                    let offset = offset.min(content.len());
                    let newlines = bytes.iter().filter(|&&b| b == b'\n').count();

                    // Add buffer and insert into tree
                    buffers.push(StringBuffer::new(buffers.len(), bytes.clone()));
                    tree.insert(
                        offset,
                        BufferLocation::Added(buffers.len() - 1),
                        0,
                        bytes.len(),
                        Some(newlines),
                        &buffers,
                    );

                    // Update actual content
                    content.splice(offset..offset, bytes);
                } else if !is_insert {
                    // Delete operation - offset is first element, bytes length is implied
                    let offset = offset.min(content.len());
                    let delete_len = 5.min(content.len().saturating_sub(offset)); // Use fixed small delete
                    if delete_len > 0 {
                        tree.delete(offset, delete_len, &buffers);
                        content.drain(offset..offset + delete_len);
                    }
                }
            }

            // Helper to compute ground truth line/column from content
            let compute_position = |content: &[u8], offset: usize| -> (usize, usize) {
                let offset = offset.min(content.len());
                let mut line = 0;
                let mut col = 0;
                for (i, &byte) in content.iter().enumerate() {
                    if i == offset {
                        break;
                    }
                    if byte == b'\n' {
                        line += 1;
                        col = 0;
                    } else {
                        col += 1;
                    }
                }
                (line, col)
            };

            // Test various offsets
            for offset in test_offsets {
                let offset = offset.min(content.len());
                if offset == 0 {
                    continue; // Skip 0, it's a special case that always works
                }

                let expected = compute_position(&content, offset);
                let actual = tree.offset_to_position(offset, &buffers);

                prop_assert_eq!(
                    actual,
                    Some(expected),
                    "offset_to_position({}) returned {:?}, expected {:?}. Content len: {}",
                    offset,
                    actual,
                    expected,
                    content.len()
                );
            }
        }
    }
}
