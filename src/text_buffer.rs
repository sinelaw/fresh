/// Text buffer that uses PieceTree with integrated line tracking
/// Architecture where the tree is the single source of truth for text and line information
use crate::piece_tree::{
    BufferLocation, Cursor, PieceInfo, PieceRangeIter, PieceTree, Position, StringBuffer, TreeStats,
};
use anyhow::{Context, Result};
use regex::bytes::Regex;
use std::io::{self, Read, Write};
use std::ops::Range;
use std::path::{Path, PathBuf};

// Large file support configuration
/// Default threshold for considering a file "large" (100 MB)
pub const DEFAULT_LARGE_FILE_THRESHOLD: usize = 100 * 1024 * 1024;

/// Chunk size to load when lazy loading (1 MB)
pub const LOAD_CHUNK_SIZE: usize = 1024 * 1024;

/// Chunk alignment for lazy loading (64 KB)
pub const CHUNK_ALIGNMENT: usize = 64 * 1024;

/// Represents a line number (simplified for new implementation)
/// Legacy enum kept for backwards compatibility - always Absolute now
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LineNumber {
    /// Absolute line number - this is the actual line number in the file
    Absolute(usize),
    /// Relative line number (deprecated - now same as Absolute)
    Relative {
        line: usize,
        from_cached_line: usize,
    },
}

impl LineNumber {
    /// Get the line number value
    pub fn value(&self) -> usize {
        match self {
            LineNumber::Absolute(line) => *line,
            LineNumber::Relative { line, .. } => *line,
        }
    }

    /// Check if this is an absolute line number
    pub fn is_absolute(&self) -> bool {
        matches!(self, LineNumber::Absolute(_))
    }

    /// Check if this is a relative line number
    pub fn is_relative(&self) -> bool {
        matches!(self, LineNumber::Relative { .. })
    }

    /// Format the line number for display
    pub fn format(&self) -> String {
        match self {
            LineNumber::Absolute(line) => format!("{}", line + 1),
            LineNumber::Relative { line, .. } => format!("~{}", line + 1),
        }
    }
}

/// A text buffer that manages document content using a piece table
/// with integrated line tracking
pub struct TextBuffer {
    /// The piece tree for efficient text manipulation with integrated line tracking
    piece_tree: PieceTree,

    /// List of string buffers containing chunks of text data
    /// Index 0 is typically the original/stored buffer
    /// Additional buffers are added for modifications
    buffers: Vec<StringBuffer>,

    /// Next buffer ID to assign
    next_buffer_id: usize,

    /// Optional file path for persistence
    file_path: Option<PathBuf>,

    /// Has the buffer been modified since last save?
    modified: bool,

    /// Is this a large file (no line indexing, lazy loading enabled)?
    large_file: bool,
}

impl TextBuffer {
    /// Create a new text buffer (with large_file_threshold for backwards compatibility)
    /// Note: large_file_threshold is ignored in the new implementation
    pub fn new(_large_file_threshold: usize) -> Self {
        TextBuffer {
            piece_tree: PieceTree::empty(),
            buffers: vec![StringBuffer::new(0, Vec::new())],
            next_buffer_id: 1,
            file_path: None,
            modified: false,
            large_file: false,
        }
    }

    /// Create a text buffer from initial content
    pub fn from_bytes(content: Vec<u8>) -> Self {
        let bytes = content.len();

        // Create initial StringBuffer with ID 0
        let buffer = StringBuffer::new(0, content);
        let line_feed_cnt = buffer.line_feed_count();

        TextBuffer {
            piece_tree: if bytes > 0 {
                PieceTree::new(BufferLocation::Stored(0), 0, bytes, line_feed_cnt)
            } else {
                PieceTree::empty()
            },
            buffers: vec![buffer],
            next_buffer_id: 1,
            file_path: None,
            modified: false,
            large_file: false,
        }
    }

    /// Create a text buffer from a string
    pub fn from_str(s: &str, _large_file_threshold: usize) -> Self {
        Self::from_bytes(s.as_bytes().to_vec())
    }

    /// Create an empty text buffer
    pub fn empty() -> Self {
        TextBuffer {
            piece_tree: PieceTree::empty(),
            buffers: vec![StringBuffer::new(0, Vec::new())],
            next_buffer_id: 1,
            file_path: None,
            modified: false,
            large_file: false,
        }
    }

    /// Load a text buffer from a file
    pub fn load_from_file<P: AsRef<Path>>(
        path: P,
        large_file_threshold: usize,
    ) -> io::Result<Self> {
        let path = path.as_ref();

        // Get file size to determine loading strategy
        let metadata = std::fs::metadata(path)?;
        let file_size = metadata.len() as usize;

        // Use threshold parameter or default
        let threshold = if large_file_threshold > 0 {
            large_file_threshold
        } else {
            DEFAULT_LARGE_FILE_THRESHOLD
        };

        // Choose loading strategy based on file size
        if file_size >= threshold {
            Self::load_large_file(path, file_size)
        } else {
            Self::load_small_file(path)
        }
    }

    /// Load a small file with full eager loading and line indexing
    fn load_small_file<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let path = path.as_ref();
        let mut file = std::fs::File::open(path)?;
        let mut contents = Vec::new();
        file.read_to_end(&mut contents)?;

        let mut buffer = Self::from_bytes(contents);
        buffer.file_path = Some(path.to_path_buf());
        buffer.modified = false;
        buffer.large_file = false;
        Ok(buffer)
    }

    /// Load a large file with unloaded buffer (no line indexing, lazy loading)
    fn load_large_file<P: AsRef<Path>>(path: P, file_size: usize) -> io::Result<Self> {
        use crate::piece_tree::{BufferData, BufferLocation};

        let path = path.as_ref();

        // Create an unloaded buffer that references the entire file
        let buffer = StringBuffer {
            id: 0,
            data: BufferData::Unloaded {
                file_path: path.to_path_buf(),
                file_offset: 0,
                bytes: file_size,
            },
        };

        // Create piece tree with a single piece covering the whole file
        // No line feed count (None) since we're not computing line indexing
        let piece_tree = if file_size > 0 {
            PieceTree::new(BufferLocation::Stored(0), 0, file_size, None)
        } else {
            PieceTree::empty()
        };

        Ok(TextBuffer {
            piece_tree,
            buffers: vec![buffer],
            next_buffer_id: 1,
            file_path: Some(path.to_path_buf()),
            modified: false,
            large_file: true,
        })
    }

    /// Save the buffer to its associated file
    pub fn save(&mut self) -> io::Result<()> {
        if let Some(path) = &self.file_path {
            self.save_to_file(path.clone())
        } else {
            Err(io::Error::new(
                io::ErrorKind::NotFound,
                "No file path associated with buffer",
            ))
        }
    }

    /// Save the buffer to a specific file
    pub fn save_to_file<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()> {
        let mut file = std::fs::File::create(path.as_ref())?;
        let content = self.get_all_text();
        file.write_all(&content)?;
        self.file_path = Some(path.as_ref().to_path_buf());
        self.modified = false;
        Ok(())
    }

    /// Get the total number of bytes in the document
    pub fn total_bytes(&self) -> usize {
        self.piece_tree.total_bytes()
    }

    /// Get the total number of lines in the document
    /// Uses the piece tree's integrated line tracking
    /// Returns None if line count is unknown (e.g., for large files without line indexing)
    pub fn line_count(&self) -> Option<usize> {
        self.piece_tree.line_count()
    }

    /// Convert a byte offset to a line/column position
    pub fn offset_to_position(&self, offset: usize) -> Position {
        let (line, column) = self.piece_tree.offset_to_position(offset, &self.buffers);
        Position { line, column }
    }

    /// Convert a line/column position to a byte offset
    pub fn position_to_offset(&self, position: Position) -> usize {
        self.piece_tree
            .position_to_offset(position.line, position.column, &self.buffers)
    }

    /// Insert text at the given byte offset
    pub fn insert_bytes(&mut self, offset: usize, text: Vec<u8>) -> Cursor {
        if text.is_empty() {
            return self.piece_tree.cursor_at_offset(offset);
        }

        // Mark as modified
        self.modified = true;

        // Count line feeds in the text to insert
        let line_feed_cnt = Some(text.iter().filter(|&&b| b == b'\n').count());

        // Optimization: try to append to existing buffer if insertion is at piece boundary
        let (buffer_location, buffer_offset, text_len) =
            if let Some(append_info) = self.try_append_to_existing_buffer(offset, &text) {
                append_info
            } else {
                // Create a new StringBuffer for this insertion
                let buffer_id = self.next_buffer_id;
                self.next_buffer_id += 1;
                let buffer = StringBuffer::new(buffer_id, text.clone());
                self.buffers.push(buffer);
                (BufferLocation::Added(buffer_id), 0, text.len())
            };

        // Update piece tree (need to pass buffers reference)
        self.piece_tree.insert(
            offset,
            buffer_location,
            buffer_offset,
            text_len,
            line_feed_cnt,
            &self.buffers,
        )
    }

    /// Try to append to an existing buffer if insertion point aligns with buffer end
    /// Returns (BufferLocation, buffer_offset, text_len) if append succeeds, None otherwise
    fn try_append_to_existing_buffer(
        &mut self,
        offset: usize,
        text: &[u8],
    ) -> Option<(BufferLocation, usize, usize)> {
        // Only optimize for non-empty insertions after existing content
        if text.is_empty() || offset == 0 {
            return None;
        }

        // Mark as modified
        self.modified = true;

        // Find the piece containing the byte just before the insertion point
        // This avoids the saturating_sub issue
        let piece_info = self.piece_tree.find_by_offset(offset - 1)?;

        // Check if insertion is exactly at the end of this piece
        // offset_in_piece tells us where (offset-1) is within the piece
        // For insertion to be at piece end, (offset-1) must be the last byte
        let offset_in_piece = piece_info.offset_in_piece?;
        if offset_in_piece + 1 != piece_info.bytes {
            return None; // Not at the end of the piece
        }

        // Only append to "Added" buffers (not original Stored buffers)
        if !matches!(piece_info.location, BufferLocation::Added(_)) {
            return None;
        }

        let buffer_id = piece_info.location.buffer_id();
        let buffer = self.buffers.get_mut(buffer_id)?;

        // Check if buffer is loaded
        let buffer_len = buffer.get_data()?.len();

        // Check if this piece ends exactly at the end of its buffer
        if piece_info.offset + piece_info.bytes != buffer_len {
            return None;
        }

        // Perfect! Append to this buffer
        let append_offset = buffer.append(text);

        Some((piece_info.location, append_offset, text.len()))
    }

    /// Insert text (from &str) at the given byte offset
    pub fn insert(&mut self, offset: usize, text: &str) {
        self.insert_bytes(offset, text.as_bytes().to_vec());
    }

    /// Insert text at a line/column position
    /// This now uses the optimized piece_tree.insert_at_position() for a single traversal
    pub fn insert_at_position(&mut self, position: Position, text: Vec<u8>) -> Cursor {
        if text.is_empty() {
            let offset = self.position_to_offset(position);
            return self.piece_tree.cursor_at_offset(offset);
        }

        // Mark as modified
        self.modified = true;

        // Count line feeds in the text to insert
        let line_feed_cnt = text.iter().filter(|&&b| b == b'\n').count();

        // Create a new StringBuffer for this insertion
        let buffer_id = self.next_buffer_id;
        self.next_buffer_id += 1;
        let buffer = StringBuffer::new(buffer_id, text.clone());
        self.buffers.push(buffer);

        // Use the optimized position-based insertion (single traversal)
        self.piece_tree.insert_at_position(
            position.line,
            position.column,
            BufferLocation::Added(buffer_id),
            0,
            text.len(),
            line_feed_cnt,
            &self.buffers,
        )
    }

    /// Delete text starting at the given byte offset
    pub fn delete_bytes(&mut self, offset: usize, bytes: usize) {
        if bytes == 0 || offset >= self.total_bytes() {
            return;
        }

        // Update piece tree
        self.piece_tree.delete(offset, bytes, &self.buffers);

        // Mark as modified
        self.modified = true;
    }

    /// Delete text in a range
    pub fn delete(&mut self, range: Range<usize>) {
        if range.end > range.start {
            self.delete_bytes(range.start, range.end - range.start);
        }
    }

    /// Delete text in a line/column range
    /// This now uses the optimized piece_tree.delete_position_range() for a single traversal
    pub fn delete_range(&mut self, start: Position, end: Position) {
        // Use the optimized position-based deletion
        self.piece_tree.delete_position_range(
            start.line,
            start.column,
            end.line,
            end.column,
            &self.buffers,
        );
        self.modified = true;
    }

    /// Get text from a byte offset range
    /// This now uses the optimized piece_tree.iter_pieces_in_range() for a single traversal
    /// Get text from a byte offset range (read-only)
    /// Returns None if any buffer in the range is unloaded
    /// For guaranteed complete data with lazy loading, use get_text_range_mut() instead
    pub fn get_text_range(&self, offset: usize, bytes: usize) -> Option<Vec<u8>> {
        if bytes == 0 {
            return Some(Vec::new());
        }

        let mut result = Vec::with_capacity(bytes);
        let end_offset = offset + bytes;
        let mut collected = 0;

        // Use the efficient piece iterator (single O(log n) traversal + O(N) iteration)
        for piece_view in self.piece_tree.iter_pieces_in_range(offset, end_offset) {
            let buffer_id = piece_view.location.buffer_id();
            if let Some(buffer) = self.buffers.get(buffer_id) {
                // Calculate the range to read from this piece
                let piece_start_in_doc = piece_view.doc_offset;
                let piece_end_in_doc = piece_view.doc_offset + piece_view.bytes;

                // Clip to the requested range
                let read_start = offset.max(piece_start_in_doc);
                let read_end = end_offset.min(piece_end_in_doc);

                if read_end > read_start {
                    let offset_in_piece = read_start - piece_start_in_doc;
                    let bytes_to_read = read_end - read_start;

                    let buffer_start = piece_view.buffer_offset + offset_in_piece;
                    let buffer_end = buffer_start + bytes_to_read;

                    // Return None if buffer is unloaded (type-safe)
                    let data = buffer.get_data()?;

                    if buffer_end <= data.len() {
                        result.extend_from_slice(&data[buffer_start..buffer_end]);
                        collected += bytes_to_read;

                        if collected >= bytes {
                            break;
                        }
                    }
                }
            }
        }

        Some(result)
    }

    /// Get text from a byte offset range with lazy loading
    /// This will load unloaded chunks on-demand and always returns complete data
    ///
    /// Returns an error if loading fails or if data cannot be read for any reason.
    ///
    /// NOTE: Currently loads entire buffers on-demand. Future optimization would split
    /// large pieces and load only LOAD_CHUNK_SIZE chunks at a time.
    pub fn get_text_range_mut(&mut self, offset: usize, bytes: usize) -> Result<Vec<u8>> {
        if bytes == 0 {
            return Ok(Vec::new());
        }

        let mut result = Vec::with_capacity(bytes);
        let end_offset = offset + bytes;
        let mut current_offset = offset;

        // Keep iterating until we've collected all requested bytes
        while current_offset < end_offset {
            let mut made_progress = false;
            let mut restarted_iteration = false;

            // Use the efficient piece iterator (single O(log n) traversal + O(N) iteration)
            for piece_view in self.piece_tree.iter_pieces_in_range(current_offset, end_offset) {
                let buffer_id = piece_view.location.buffer_id();

                // Check if buffer needs loading
                let needs_loading = self
                    .buffers
                    .get(buffer_id)
                    .map(|b| !b.is_loaded())
                    .unwrap_or(false);

                if needs_loading {
                    // Check if piece is too large for full loading
                    if piece_view.bytes > LOAD_CHUNK_SIZE {
                        // Split large piece into chunks
                        let piece_start_in_doc = piece_view.doc_offset;
                        let offset_in_piece = current_offset.saturating_sub(piece_start_in_doc);

                        // Calculate chunk boundaries aligned to CHUNK_ALIGNMENT
                        let chunk_start_in_buffer =
                            (piece_view.buffer_offset + offset_in_piece) / CHUNK_ALIGNMENT
                                * CHUNK_ALIGNMENT;
                        let chunk_bytes = LOAD_CHUNK_SIZE.min(
                            (piece_view.buffer_offset + piece_view.bytes)
                                .saturating_sub(chunk_start_in_buffer),
                        );

                        // Calculate document offsets for splitting
                        let chunk_start_offset_in_piece =
                            chunk_start_in_buffer.saturating_sub(piece_view.buffer_offset);
                        let split_start_in_doc = piece_start_in_doc + chunk_start_offset_in_piece;
                        let split_end_in_doc = split_start_in_doc + chunk_bytes;

                        // Split the piece to isolate the chunk
                        if chunk_start_offset_in_piece > 0 {
                            self.piece_tree.split_at_offset(split_start_in_doc, &self.buffers);
                        }
                        if split_end_in_doc < piece_start_in_doc + piece_view.bytes {
                            self.piece_tree.split_at_offset(split_end_in_doc, &self.buffers);
                        }

                        // Create a new buffer for this chunk
                        let chunk_buffer = self
                            .buffers
                            .get(buffer_id)
                            .context("Buffer not found")?
                            .create_chunk_buffer(
                                self.next_buffer_id,
                                chunk_start_in_buffer,
                                chunk_bytes,
                            )
                            .context("Failed to create chunk buffer")?;

                        self.next_buffer_id += 1;
                        let new_buffer_id = chunk_buffer.id;
                        self.buffers.push(chunk_buffer);

                        // Update the piece to reference the new chunk buffer
                        self.piece_tree.replace_buffer_reference(
                            buffer_id,
                            piece_view.buffer_offset + chunk_start_offset_in_piece,
                            chunk_bytes,
                            BufferLocation::Added(new_buffer_id),
                        );

                        // Load the chunk buffer
                        self.buffers
                            .get_mut(new_buffer_id)
                            .context("Chunk buffer not found")?
                            .load()
                            .context("Failed to load chunk")?;

                        // Restart iteration with the modified tree
                        restarted_iteration = true;
                        break;
                    } else {
                        // Piece is small enough, load the entire buffer
                        self.buffers
                            .get_mut(buffer_id)
                            .context("Buffer not found")?
                            .load()
                            .context("Failed to load buffer")?;
                    }
                }

                // Calculate the range to read from this piece
                let piece_start_in_doc = piece_view.doc_offset;
                let piece_end_in_doc = piece_view.doc_offset + piece_view.bytes;

                // Clip to the requested range
                let read_start = current_offset.max(piece_start_in_doc);
                let read_end = end_offset.min(piece_end_in_doc);

                if read_end > read_start {
                    let offset_in_piece = read_start - piece_start_in_doc;
                    let bytes_to_read = read_end - read_start;

                    let buffer_start = piece_view.buffer_offset + offset_in_piece;
                    let buffer_end = buffer_start + bytes_to_read;

                    // Buffer should be loaded now
                    let buffer = self.buffers.get(buffer_id).context("Buffer not found")?;
                    let data = buffer
                        .get_data()
                        .context("Buffer data unavailable after load")?;

                    anyhow::ensure!(
                        buffer_end <= data.len(),
                        "Buffer range out of bounds: requested {}..{}, buffer size {}",
                        buffer_start,
                        buffer_end,
                        data.len()
                    );

                    result.extend_from_slice(&data[buffer_start..buffer_end]);
                    current_offset = read_end;
                    made_progress = true;
                }
            }

            // If we didn't make progress and didn't restart iteration, this is an error
            anyhow::ensure!(
                made_progress || restarted_iteration,
                "Failed to read data at offset {}: no progress made",
                current_offset
            );
        }

        Ok(result)
    }

    /// Prepare a viewport for rendering
    ///
    /// This is called before rendering with &mut access to pre-load all data
    /// that will be needed for the viewport. It estimates the number of bytes
    /// needed based on the line count and pre-loads them.
    ///
    /// # Arguments
    /// * `start_offset` - The byte offset where the viewport starts
    /// * `line_count` - The number of lines to prepare (estimate)
    ///
    /// # Returns
    /// Ok(()) if preparation succeeded, Err if loading failed
    pub fn prepare_viewport(&mut self, start_offset: usize, line_count: usize) -> Result<()> {
        // Estimate how many bytes we need (pessimistic assumption)
        // Average line length is typically 80-100 bytes, but we use 200 to be safe
        let estimated_bytes = line_count.saturating_mul(200);

        // Cap the estimate at the remaining bytes in the document
        let remaining_bytes = self.total_bytes().saturating_sub(start_offset);
        let bytes_to_load = estimated_bytes.min(remaining_bytes);

        // Pre-load with full chunk-splitting support
        // This may load more than we need, but ensures all data is available
        self.get_text_range_mut(start_offset, bytes_to_load)?;

        Ok(())
    }

    /// Get all text as a single Vec<u8>
    /// Returns empty vector if any buffers are unloaded
    pub fn get_all_text(&self) -> Vec<u8> {
        self.get_text_range(0, self.total_bytes()).unwrap_or_default()
    }

    /// Get all text as a String
    pub fn get_all_text_string(&self) -> String {
        String::from_utf8_lossy(&self.get_all_text()).into_owned()
    }

    /// Get text from a byte range as bytes
    /// Returns empty vector if any buffers are unloaded
    ///
    /// Note: For new code, prefer using DocumentModel methods which provide
    /// better error handling and support the document model abstraction.
    pub fn slice_bytes(&self, range: Range<usize>) -> Vec<u8> {
        self.get_text_range(range.start, range.end.saturating_sub(range.start))
            .unwrap_or_default()
    }

    /// Get all text as a String
    pub fn to_string(&self) -> String {
        self.get_all_text_string()
    }

    /// Get the total number of bytes
    pub fn len(&self) -> usize {
        self.total_bytes()
    }

    /// Check if the buffer is empty
    pub fn is_empty(&self) -> bool {
        self.total_bytes() == 0
    }

    /// Get the file path associated with this buffer
    pub fn file_path(&self) -> Option<&Path> {
        self.file_path.as_deref()
    }

    /// Set the file path for this buffer
    pub fn set_file_path(&mut self, path: PathBuf) {
        self.file_path = Some(path);
    }

    /// Check if the buffer has been modified since last save
    pub fn is_modified(&self) -> bool {
        self.modified
    }

    /// Clear the modified flag (after save)
    pub fn clear_modified(&mut self) {
        self.modified = false;
    }

    /// Get text for a specific line
    pub fn get_line(&self, line: usize) -> Option<Vec<u8>> {
        let (start, end) = self.piece_tree.line_range(line, &self.buffers)?;

        let bytes = if let Some(end_offset) = end {
            end_offset.saturating_sub(start)
        } else {
            self.total_bytes().saturating_sub(start)
        };

        self.get_text_range(start, bytes)
    }

    /// Get the byte offset where a line starts
    pub fn line_start_offset(&self, line: usize) -> Option<usize> {
        let (start, _) = self.piece_tree.line_range(line, &self.buffers)?;
        Some(start)
    }

    /// Get piece information at a byte offset
    pub fn piece_info_at_offset(&self, offset: usize) -> Option<PieceInfo> {
        self.piece_tree.find_by_offset(offset)
    }

    /// Get tree statistics for debugging
    pub fn stats(&self) -> TreeStats {
        self.piece_tree.stats()
    }

    // Search and Replace Operations

    /// Find the next occurrence of a pattern, with wrap-around
    pub fn find_next(&self, pattern: &str, start_pos: usize) -> Option<usize> {
        if pattern.is_empty() {
            return None;
        }

        let pattern_bytes = pattern.as_bytes();
        let buffer_len = self.len();

        // Search from start_pos to end
        if start_pos < buffer_len {
            if let Some(offset) = self.find_pattern(start_pos, buffer_len, pattern_bytes) {
                return Some(offset);
            }
        }

        // Wrap around: search from beginning to start_pos
        if start_pos > 0 {
            if let Some(offset) = self.find_pattern(0, start_pos, pattern_bytes) {
                return Some(offset);
            }
        }

        None
    }

    /// Find the next occurrence of a pattern within an optional range
    /// If range is None, searches the entire buffer with wrap-around (same as find_next)
    /// If range is Some, searches only within that range without wrap-around
    pub fn find_next_in_range(
        &self,
        pattern: &str,
        start_pos: usize,
        range: Option<Range<usize>>,
    ) -> Option<usize> {
        if pattern.is_empty() {
            return None;
        }

        if let Some(search_range) = range {
            // Search within range only, no wrap-around
            let pattern_bytes = pattern.as_bytes();
            let search_start = start_pos.max(search_range.start);
            let search_end = search_range.end.min(self.len());

            if search_start < search_end {
                self.find_pattern(search_start, search_end, pattern_bytes)
            } else {
                None
            }
        } else {
            // No range specified, use normal find_next with wrap-around
            self.find_next(pattern, start_pos)
        }
    }

    /// Find pattern in a byte range using overlapping chunks
    fn find_pattern(&self, start: usize, end: usize, pattern: &[u8]) -> Option<usize> {
        if pattern.is_empty() || start >= end {
            return None;
        }

        const CHUNK_SIZE: usize = 65536; // 64KB chunks
        let overlap = pattern.len().saturating_sub(1).max(1);

        // Use the overlapping chunks iterator for efficient streaming search
        let chunks = OverlappingChunks::new(self, start, end, CHUNK_SIZE, overlap);

        for chunk in chunks {
            // Search the entire chunk buffer
            if let Some(pos) = Self::find_in_bytes(&chunk.buffer, pattern) {
                let match_end = pos + pattern.len();
                // Only report if match ENDS in or after the valid zone
                // This ensures patterns spanning boundaries are found exactly once
                if match_end > chunk.valid_start {
                    let absolute_pos = chunk.absolute_pos + pos;
                    // Verify the match doesn't extend beyond our search range
                    if absolute_pos + pattern.len() <= end {
                        return Some(absolute_pos);
                    }
                }
            }
        }

        None
    }

    /// Simple byte pattern search using naive algorithm
    fn find_in_bytes(haystack: &[u8], needle: &[u8]) -> Option<usize> {
        if needle.is_empty() || needle.len() > haystack.len() {
            return None;
        }

        for i in 0..=haystack.len() - needle.len() {
            if &haystack[i..i + needle.len()] == needle {
                return Some(i);
            }
        }

        None
    }

    /// Find the next occurrence of a regex pattern, with wrap-around
    pub fn find_next_regex(&self, regex: &Regex, start_pos: usize) -> Option<usize> {
        let buffer_len = self.len();

        // Search from start_pos to end
        if start_pos < buffer_len {
            if let Some(offset) = self.find_regex(start_pos, buffer_len, regex) {
                return Some(offset);
            }
        }

        // Wrap around: search from beginning to start_pos
        if start_pos > 0 {
            if let Some(offset) = self.find_regex(0, start_pos, regex) {
                return Some(offset);
            }
        }

        None
    }

    /// Find the next occurrence of a regex pattern within an optional range
    pub fn find_next_regex_in_range(
        &self,
        regex: &Regex,
        start_pos: usize,
        range: Option<Range<usize>>,
    ) -> Option<usize> {
        if let Some(search_range) = range {
            let search_start = start_pos.max(search_range.start);
            let search_end = search_range.end.min(self.len());

            if search_start < search_end {
                self.find_regex(search_start, search_end, regex)
            } else {
                None
            }
        } else {
            self.find_next_regex(regex, start_pos)
        }
    }

    /// Find regex pattern in a byte range using overlapping chunks
    fn find_regex(&self, start: usize, end: usize, regex: &Regex) -> Option<usize> {
        if start >= end {
            return None;
        }

        const CHUNK_SIZE: usize = 1048576; // 1MB chunks
        const OVERLAP: usize = 4096; // 4KB overlap for regex

        // Use the overlapping chunks iterator for efficient streaming search
        // This fixes the critical bug where regex patterns spanning chunk boundaries were missed
        let chunks = OverlappingChunks::new(self, start, end, CHUNK_SIZE, OVERLAP);

        for chunk in chunks {
            // Search the entire chunk buffer
            if let Some(mat) = regex.find(&chunk.buffer) {
                let match_end = mat.end();
                // Only report if match ENDS in or after the valid zone
                // This ensures patterns spanning boundaries are found exactly once
                if match_end > chunk.valid_start {
                    let absolute_pos = chunk.absolute_pos + mat.start();
                    // Verify the match doesn't extend beyond our search range
                    let match_len = mat.end() - mat.start();
                    if absolute_pos + match_len <= end {
                        return Some(absolute_pos);
                    }
                }
            }
        }

        None
    }

    /// Replace a range with replacement text
    pub fn replace_range(&mut self, range: Range<usize>, replacement: &str) -> bool {
        if range.start >= self.len() {
            return false;
        }

        let end = range.end.min(self.len());
        if end > range.start {
            self.delete_bytes(range.start, end - range.start);
        }

        if !replacement.is_empty() {
            self.insert(range.start, replacement);
        }

        true
    }

    /// Find and replace the next occurrence of a pattern
    pub fn replace_next(
        &mut self,
        pattern: &str,
        replacement: &str,
        start_pos: usize,
        range: Option<Range<usize>>,
    ) -> Option<usize> {
        if let Some(pos) = self.find_next_in_range(pattern, start_pos, range.clone()) {
            self.replace_range(pos..pos + pattern.len(), replacement);
            Some(pos)
        } else {
            None
        }
    }

    /// Replace all occurrences of a pattern with replacement text
    pub fn replace_all(&mut self, pattern: &str, replacement: &str) -> usize {
        if pattern.is_empty() {
            return 0;
        }

        let mut count = 0;
        let mut pos = 0;

        // Keep searching and replacing
        // Note: we search forward from last replacement to handle growth/shrinkage
        loop {
            // Find next occurrence (no wrap-around for replace_all)
            if let Some(found_pos) = self.find_next_in_range(pattern, pos, Some(0..self.len())) {
                self.replace_range(found_pos..found_pos + pattern.len(), replacement);
                count += 1;

                // Move past the replacement
                pos = found_pos + replacement.len();

                // If we're at or past the end, stop
                if pos >= self.len() {
                    break;
                }
            } else {
                break;
            }
        }

        count
    }

    /// Replace all occurrences of a regex pattern with replacement text
    pub fn replace_all_regex(&mut self, regex: &Regex, replacement: &str) -> Result<usize> {
        let mut count = 0;
        let mut pos = 0;

        loop {
            if let Some(found_pos) = self.find_next_regex_in_range(regex, pos, Some(0..self.len()))
            {
                // Get the match to find its length
                let text = self
                    .get_text_range_mut(found_pos, self.len() - found_pos)
                    .context("Failed to read text for regex match")?;

                if let Some(mat) = regex.find(&text) {
                    self.replace_range(found_pos..found_pos + mat.len(), replacement);
                    count += 1;
                    pos = found_pos + replacement.len();

                    if pos >= self.len() {
                        break;
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(count)
    }

    // LSP Support (UTF-16 conversions)

    /// Convert byte position to (line, column) in bytes
    pub fn position_to_line_col(&self, byte_pos: usize) -> (usize, usize) {
        let pos = self.offset_to_position(byte_pos);
        (pos.line, pos.column)
    }

    /// Convert (line, character) to byte position - 0-indexed
    /// character is in BYTES, not UTF-16 code units
    /// Optimized to use single line_range() call instead of two
    pub fn line_col_to_position(&self, line: usize, character: usize) -> usize {
        if let Some((start, end)) = self.piece_tree.line_range(line, &self.buffers) {
            // Calculate line length from the range
            let line_len = if let Some(end_offset) = end {
                end_offset.saturating_sub(start)
            } else {
                self.total_bytes().saturating_sub(start)
            };
            let byte_offset = character.min(line_len);
            start + byte_offset
        } else {
            // Line doesn't exist, return end of buffer
            self.len()
        }
    }

    /// Convert byte position to LSP position (line, UTF-16 code units)
    /// LSP protocol uses UTF-16 code units for character offsets
    pub fn position_to_lsp_position(&self, byte_pos: usize) -> (usize, usize) {
        let pos = self.offset_to_position(byte_pos);
        let line = pos.line;
        let column_bytes = pos.column;

        // Get the line content
        if let Some(line_bytes) = self.get_line(line) {
            // Convert byte offset to UTF-16 code units
            let text_before = &line_bytes[..column_bytes.min(line_bytes.len())];
            let text_str = String::from_utf8_lossy(text_before);
            let utf16_offset = text_str.encode_utf16().count();
            (line, utf16_offset)
        } else {
            (line, 0)
        }
    }

    /// Convert LSP position (line, UTF-16 code units) to byte position
    /// LSP uses UTF-16 code units for character offsets, not bytes
    /// Optimized to use single line_range() call instead of two
    pub fn lsp_position_to_byte(&self, line: usize, utf16_offset: usize) -> usize {
        if let Some((line_start, end)) = self.piece_tree.line_range(line, &self.buffers) {
            // Calculate line length and get line content
            let line_len = if let Some(end_offset) = end {
                end_offset.saturating_sub(line_start)
            } else {
                self.total_bytes().saturating_sub(line_start)
            };

            if line_len > 0 {
                // If data is unloaded, return line_start as fallback
                let Some(line_bytes) = self.get_text_range(line_start, line_len) else {
                    return line_start;
                };
                let line_str = String::from_utf8_lossy(&line_bytes);

                // Convert UTF-16 offset to byte offset
                let mut utf16_count = 0;
                let mut byte_offset = 0;

                for ch in line_str.chars() {
                    if utf16_count >= utf16_offset {
                        break;
                    }
                    utf16_count += ch.len_utf16();
                    byte_offset += ch.len_utf8();
                }

                line_start + byte_offset
            } else {
                line_start
            }
        } else {
            // Line doesn't exist, return end of buffer
            self.len()
        }
    }

    // Navigation helpers

    /// Find the previous character boundary (UTF-8 aware)
    pub fn prev_char_boundary(&self, pos: usize) -> usize {
        if pos == 0 {
            return 0;
        }

        // Get a few bytes before pos to find the character boundary
        let start = pos.saturating_sub(4);
        let Some(bytes) = self.get_text_range(start, pos - start) else {
            // Data unloaded, return pos as fallback
            return pos;
        };

        // Walk backwards to find a UTF-8 leading byte
        for i in (0..bytes.len()).rev() {
            let byte = bytes[i];
            // Check if this is a UTF-8 leading byte (not a continuation byte)
            if (byte & 0b1100_0000) != 0b1000_0000 {
                return start + i;
            }
        }

        // Fallback
        pos.saturating_sub(1)
    }

    /// Find the next character boundary (UTF-8 aware)
    pub fn next_char_boundary(&self, pos: usize) -> usize {
        let len = self.len();
        if pos >= len {
            return len;
        }

        // Get a few bytes after pos to find the character boundary
        let end = (pos + 5).min(len);
        let Some(bytes) = self.get_text_range(pos, end - pos) else {
            // Data unloaded, return pos as fallback
            return pos;
        };

        // Start from index 1 (we want the NEXT boundary)
        for i in 1..bytes.len() {
            let byte = bytes[i];
            // Check if this is a UTF-8 leading byte (not a continuation byte)
            if (byte & 0b1100_0000) != 0b1000_0000 {
                return pos + i;
            }
        }

        // If we got here, we're at the end or found no boundary in the range
        end
    }

    /// Find the previous word boundary
    pub fn prev_word_boundary(&self, pos: usize) -> usize {
        if pos == 0 {
            return 0;
        }

        // Get some text before pos
        let start = pos.saturating_sub(256).max(0);
        let Some(bytes) = self.get_text_range(start, pos - start) else {
            // Data unloaded, return pos as fallback
            return pos;
        };
        let text = String::from_utf8_lossy(&bytes);

        let mut found_word_char = false;
        let chars: Vec<char> = text.chars().collect();

        for i in (0..chars.len()).rev() {
            let ch = chars[i];
            let is_word_char = ch.is_alphanumeric() || ch == '_';

            if found_word_char && !is_word_char {
                // We've transitioned from word to non-word
                // Calculate the byte position
                let byte_offset: usize = chars[0..=i].iter().map(|c| c.len_utf8()).sum();
                return start + byte_offset;
            }

            if is_word_char {
                found_word_char = true;
            }
        }

        0
    }

    /// Find the next word boundary
    pub fn next_word_boundary(&self, pos: usize) -> usize {
        let len = self.len();
        if pos >= len {
            return len;
        }

        // Get some text after pos
        let end = (pos + 256).min(len);
        let Some(bytes) = self.get_text_range(pos, end - pos) else {
            // Data unloaded, return pos as fallback
            return pos;
        };
        let text = String::from_utf8_lossy(&bytes);

        let mut found_word_char = false;
        let mut byte_offset = 0;

        for ch in text.chars() {
            let is_word_char = ch.is_alphanumeric() || ch == '_';

            if found_word_char && !is_word_char {
                // We've transitioned from word to non-word
                return pos + byte_offset;
            }

            if is_word_char {
                found_word_char = true;
            }

            byte_offset += ch.len_utf8();
        }

        len
    }

    /// Create a line iterator starting at the given byte position
    pub fn line_iterator(&mut self, byte_pos: usize) -> LineIterator<'_> {
        // Ensure data around byte_pos is loaded for lazy-loaded files
        // Load a reasonable amount of data before and after the position
        let load_before = 10000; // ~100 lines before
        let load_after = 10000;  // ~100 lines after
        let start = byte_pos.saturating_sub(load_before);
        let length = load_before + load_after;

        // Silently load data - this ensures offset_to_position works correctly
        let _ = self.get_text_range_mut(start, length);

        LineIterator::new(self, byte_pos)
    }

    /// Get a reference to piece tree for internal use (package-private)
    pub(crate) fn piece_tree_ref(&self) -> &PieceTree {
        &self.piece_tree
    }

    /// Get a reference to buffers for internal use (package-private)
    pub(crate) fn buffers_ref(&self) -> &[StringBuffer] {
        &self.buffers
    }

    // Legacy API methods for backwards compatibility

    /// Get the line number for a given byte offset
    /// Always returns absolute line numbers (no estimation needed with new implementation)
    pub fn get_line_number(&mut self, byte_offset: usize) -> usize {
        // Ensure data at this offset is loaded for lazy-loaded files
        // Load a small amount of data around the offset
        let load_radius = 1000; // ~10 lines
        let start = byte_offset.saturating_sub(load_radius);
        let length = load_radius * 2;

        // Silently load data - this ensures offset_to_position works correctly
        let _ = self.get_text_range_mut(start, length);

        self.offset_to_position(byte_offset).line
    }

    /// Populate line cache (no-op in new implementation - kept for compatibility)
    /// The new LineIndex implementation doesn't need pre-population
    pub fn populate_line_cache(&mut self, _start_byte: usize, _line_count: usize) -> usize {
        // No-op: LineIndex maintains all line starts automatically
        0
    }

    /// Get cached byte offset for line (compatibility method)
    pub fn get_cached_byte_offset_for_line(&self, line_number: usize) -> Option<usize> {
        self.line_start_offset(line_number)
    }

    /// Invalidate line cache from offset (no-op in new implementation)
    pub fn invalidate_line_cache_from(&mut self, _byte_offset: usize) {
        // No-op: LineIndex updates automatically
    }

    /// Handle line cache insertion (no-op in new implementation)
    pub fn handle_line_cache_insertion(&mut self, _byte_offset: usize, _bytes_inserted: usize) {
        // No-op: LineIndex updates automatically during insert
    }

    /// Handle line cache deletion (no-op in new implementation)
    pub fn handle_line_cache_deletion(&mut self, _byte_offset: usize, _bytes_deleted: usize) {
        // No-op: LineIndex updates automatically during delete
    }

    /// Clear line cache (no-op in new implementation)
    pub fn clear_line_cache(&mut self) {
        // No-op: LineIndex can't be cleared
    }

    // Test helper methods

    /// Create a buffer from a string for testing
    #[cfg(test)]
    pub fn from_str_test(s: &str) -> Self {
        Self::from_bytes(s.as_bytes().to_vec())
    }

    /// Create a new empty buffer for testing
    #[cfg(test)]
    pub fn new_test() -> Self {
        Self::empty()
    }
}

/// Type alias for backwards compatibility
pub type Buffer = TextBuffer;

// Re-export LineIterator from the line_iterator module
pub use crate::line_iterator::LineIterator;

// ============================================================================
// Overlapping Chunks Iterator for Efficient Search
// ============================================================================

/// Information about a chunk of data for pattern matching
#[derive(Debug)]
pub struct ChunkInfo {
    /// The buffer containing this chunk's data (includes overlap from previous chunk)
    pub buffer: Vec<u8>,

    /// Absolute position in the document where this buffer starts
    pub absolute_pos: usize,

    /// Offset within buffer where "new" data starts (valid match zone)
    /// Matches starting before this offset were already checked in the previous chunk
    pub valid_start: usize,
}

/// Iterator that yields overlapping chunks for pattern matching
///
/// This iterator implements the VSCode/Sublime approach: pull overlapping chunks
/// from the underlying piece tree and use standard search algorithms on them.
///
/// # Algorithm
///
/// ```text
/// Chunk 1: [------------ valid -----------]
/// Chunk 2:      [overlap][---- valid ----]
/// Chunk 3:                   [overlap][-- valid --]
///
/// Only matches starting in the "valid" zone are reported to avoid duplicates.
/// ```
///
/// # Example
///
/// ```ignore
/// let chunks = OverlappingChunks::new(&text_buffer, start, end, 4096, pattern.len()-1);
/// for chunk in chunks {
///     // Search only starting from chunk.valid_start
///     if let Some(pos) = search(&chunk.buffer[chunk.valid_start..]) {
///         let absolute_pos = chunk.absolute_pos + chunk.valid_start + pos;
///         return Some(absolute_pos);
///     }
/// }
/// ```
pub struct OverlappingChunks<'a> {
    piece_iter: PieceRangeIter,
    buffers: &'a [StringBuffer],

    // Reusable chunk buffer that we fill from pieces
    buffer: Vec<u8>,
    buffer_absolute_pos: usize,

    // Current state
    current_pos: usize,
    end_pos: usize,

    // Configuration
    chunk_size: usize,
    overlap: usize,

    // Track first chunk special case
    first_chunk: bool,

    // Cached piece data for incremental reading
    current_piece_data: Option<Vec<u8>>,
    current_piece_offset: usize,
}

impl<'a> OverlappingChunks<'a> {
    /// Create a new overlapping chunks iterator
    ///
    /// # Arguments
    ///
    /// * `text_buffer` - The text buffer to iterate over
    /// * `start` - Start position in the document
    /// * `end` - End position in the document (exclusive)
    /// * `chunk_size` - Target size for each chunk (excluding overlap)
    /// * `overlap` - Number of bytes to overlap between chunks
    ///
    /// # Recommendations
    ///
    /// * For literal string search: `chunk_size=65536, overlap=pattern.len()-1`
    /// * For regex search: `chunk_size=1048576, overlap=4096`
    pub fn new(
        text_buffer: &'a TextBuffer,
        start: usize,
        end: usize,
        chunk_size: usize,
        overlap: usize,
    ) -> Self {
        let piece_iter = text_buffer.piece_tree.iter_pieces_in_range(start, end);

        Self {
            piece_iter,
            buffers: &text_buffer.buffers,
            buffer: Vec::with_capacity(chunk_size + overlap),
            buffer_absolute_pos: start,
            current_pos: start,
            end_pos: end,
            chunk_size,
            overlap,
            first_chunk: true,
            current_piece_data: None,
            current_piece_offset: 0,
        }
    }

    /// Read one byte from the piece iterator
    fn read_byte(&mut self) -> Option<u8> {
        loop {
            // If we have cached piece data, read from it
            if let Some(ref data) = self.current_piece_data {
                if self.current_piece_offset < data.len() {
                    let byte = data[self.current_piece_offset];
                    self.current_piece_offset += 1;
                    self.current_pos += 1;
                    return Some(byte);
                } else {
                    // Exhausted current piece, move to next
                    self.current_piece_data = None;
                    self.current_piece_offset = 0;
                }
            }

            // Get next piece
            if let Some(piece_view) = self.piece_iter.next() {
                let buffer_id = piece_view.location.buffer_id();
                if let Some(buffer) = self.buffers.get(buffer_id) {
                    // Extract the relevant slice from this piece
                    let piece_start_in_doc = piece_view.doc_offset;
                    let piece_end_in_doc = piece_view.doc_offset + piece_view.bytes;

                    // Clip to our search range
                    let read_start = self.current_pos.max(piece_start_in_doc);
                    let read_end = self.end_pos.min(piece_end_in_doc);

                    if read_end > read_start {
                        let offset_in_piece = read_start - piece_start_in_doc;
                        let bytes_to_read = read_end - read_start;

                        let buffer_start = piece_view.buffer_offset + offset_in_piece;
                        let buffer_end = buffer_start + bytes_to_read;

                        if let Some(data) = buffer.get_data() {
                            if buffer_end <= data.len() {
                                // Cache this piece's data
                                self.current_piece_data = Some(
                                    data[buffer_start..buffer_end].to_vec()
                                );
                                self.current_piece_offset = 0;
                                continue;
                            }
                        }
                    }
                }
            }

            // No more data
            return None;
        }
    }

    /// Fill the buffer with the next chunk of data
    fn fill_next_chunk(&mut self) -> bool {
        if self.first_chunk {
            // First chunk: fill up to chunk_size
            self.first_chunk = false;
            while self.buffer.len() < self.chunk_size && self.current_pos < self.end_pos {
                if let Some(byte) = self.read_byte() {
                    self.buffer.push(byte);
                } else {
                    break;
                }
            }
            !self.buffer.is_empty()
        } else {
            // Subsequent chunks: keep overlap, fill chunk_size NEW bytes
            if self.current_pos >= self.end_pos {
                return false;
            }

            // Keep overlap bytes at the end
            if self.buffer.len() > self.overlap {
                let drain_amount = self.buffer.len() - self.overlap;
                self.buffer.drain(0..drain_amount);
                self.buffer_absolute_pos += drain_amount;
            }

            // Fill chunk_size NEW bytes (in addition to overlap)
            let before_len = self.buffer.len();
            let target_len = self.overlap + self.chunk_size;
            while self.buffer.len() < target_len && self.current_pos < self.end_pos {
                if let Some(byte) = self.read_byte() {
                    self.buffer.push(byte);
                } else {
                    break;
                }
            }

            // Return true if we added new data
            self.buffer.len() > before_len
        }
    }
}

impl<'a> Iterator for OverlappingChunks<'a> {
    type Item = ChunkInfo;

    fn next(&mut self) -> Option<Self::Item> {
        // Track if this is the first chunk before filling
        let is_first = self.buffer_absolute_pos == self.current_pos;

        if !self.fill_next_chunk() {
            return None;
        }

        // First chunk: all data is valid (no overlap from previous)
        // Subsequent chunks: overlap bytes are not valid (already checked)
        let valid_start = if is_first {
            0
        } else {
            self.overlap.min(self.buffer.len())
        };

        Some(ChunkInfo {
            buffer: self.buffer.clone(),
            absolute_pos: self.buffer_absolute_pos,
            valid_start,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_buffer() {
        let buffer = TextBuffer::empty();
        assert_eq!(buffer.total_bytes(), 0);
        assert_eq!(buffer.line_count(), Some(1)); // Empty doc has 1 line
    }

    #[test]
    fn test_line_positions_multiline() {
        let buffer = TextBuffer::from_bytes(b"Hello\nNew Line\nWorld!".to_vec());

        // Check line count
        assert_eq!(buffer.line_count(), Some(3));

        // Check line starts
        assert_eq!(buffer.line_start_offset(0), Some(0)); // "Hello\n" starts at 0
        assert_eq!(buffer.line_start_offset(1), Some(6)); // "New Line\n" starts at 6
        assert_eq!(buffer.line_start_offset(2), Some(15)); // "World!" starts at 15

        // Check offset_to_position
        assert_eq!(buffer.offset_to_position(0).line, 0); // Start of "Hello"
        assert_eq!(buffer.offset_to_position(5).line, 0); // End of "Hello" (before \n)
        assert_eq!(buffer.offset_to_position(6).line, 1); // Start of "New Line"
        assert_eq!(buffer.offset_to_position(14).line, 1); // End of "New Line" (before \n)
        assert_eq!(buffer.offset_to_position(15).line, 2); // Start of "World!"

        // Check line_col_to_position
        assert_eq!(buffer.line_col_to_position(0, 5), 5); // End of line 0
        assert_eq!(buffer.line_col_to_position(1, 0), 6); // Start of line 1
        assert_eq!(buffer.line_col_to_position(1, 8), 14); // End of line 1
        assert_eq!(buffer.line_col_to_position(2, 0), 15); // Start of line 2
    }

    #[test]
    fn test_new_from_content() {
        let buffer = TextBuffer::from_bytes(b"hello\nworld".to_vec());
        assert_eq!(buffer.total_bytes(), 11);
        assert_eq!(buffer.line_count(), Some(2));
    }

    #[test]
    fn test_get_all_text() {
        let buffer = TextBuffer::from_bytes(b"hello\nworld".to_vec());
        assert_eq!(buffer.get_all_text(), b"hello\nworld");
    }

    #[test]
    fn test_insert_at_start() {
        let mut buffer = TextBuffer::from_bytes(b"world".to_vec());
        buffer.insert_bytes(0, b"hello ".to_vec());

        assert_eq!(buffer.get_all_text(), b"hello world");
        assert_eq!(buffer.total_bytes(), 11);
    }

    #[test]
    fn test_insert_in_middle() {
        let mut buffer = TextBuffer::from_bytes(b"helloworld".to_vec());
        buffer.insert_bytes(5, b" ".to_vec());

        assert_eq!(buffer.get_all_text(), b"hello world");
        assert_eq!(buffer.total_bytes(), 11);
    }

    #[test]
    fn test_insert_at_end() {
        let mut buffer = TextBuffer::from_bytes(b"hello".to_vec());
        buffer.insert_bytes(5, b" world".to_vec());

        assert_eq!(buffer.get_all_text(), b"hello world");
        assert_eq!(buffer.total_bytes(), 11);
    }

    #[test]
    fn test_insert_with_newlines() {
        let mut buffer = TextBuffer::from_bytes(b"hello".to_vec());
        buffer.insert_bytes(5, b"\nworld\ntest".to_vec());

        assert_eq!(buffer.get_all_text(), b"hello\nworld\ntest");
        assert_eq!(buffer.line_count(), Some(3));
    }

    #[test]
    fn test_delete_from_start() {
        let mut buffer = TextBuffer::from_bytes(b"hello world".to_vec());
        buffer.delete_bytes(0, 6);

        assert_eq!(buffer.get_all_text(), b"world");
        assert_eq!(buffer.total_bytes(), 5);
    }

    #[test]
    fn test_delete_from_middle() {
        let mut buffer = TextBuffer::from_bytes(b"hello world".to_vec());
        buffer.delete_bytes(5, 1);

        assert_eq!(buffer.get_all_text(), b"helloworld");
        assert_eq!(buffer.total_bytes(), 10);
    }

    #[test]
    fn test_delete_from_end() {
        let mut buffer = TextBuffer::from_bytes(b"hello world".to_vec());
        buffer.delete_bytes(6, 5);

        assert_eq!(buffer.get_all_text(), b"hello ");
        assert_eq!(buffer.total_bytes(), 6);
    }

    #[test]
    fn test_delete_with_newlines() {
        let mut buffer = TextBuffer::from_bytes(b"hello\nworld\ntest".to_vec());
        buffer.delete_bytes(5, 7); // Delete "\nworld\n"

        assert_eq!(buffer.get_all_text(), b"hellotest");
        assert_eq!(buffer.line_count(), Some(1));
    }

    #[test]
    fn test_offset_position_conversions() {
        let buffer = TextBuffer::from_bytes(b"hello\nworld\ntest".to_vec());

        let pos = buffer.offset_to_position(0);
        assert_eq!(pos, Position { line: 0, column: 0 });

        let pos = buffer.offset_to_position(6);
        assert_eq!(pos, Position { line: 1, column: 0 });

        let offset = buffer.position_to_offset(Position { line: 1, column: 0 });
        assert_eq!(offset, 6);
    }

    #[test]
    fn test_insert_at_position() {
        let mut buffer = TextBuffer::from_bytes(b"hello\nworld".to_vec());
        buffer.insert_at_position(Position { line: 1, column: 0 }, b"beautiful ".to_vec());

        assert_eq!(buffer.get_all_text(), b"hello\nbeautiful world");
    }

    #[test]
    fn test_delete_range() {
        let mut buffer = TextBuffer::from_bytes(b"hello\nworld\ntest".to_vec());

        let start = Position { line: 0, column: 5 };
        let end = Position { line: 2, column: 0 };
        buffer.delete_range(start, end);

        assert_eq!(buffer.get_all_text(), b"hellotest");
    }

    #[test]
    fn test_get_line() {
        let buffer = TextBuffer::from_bytes(b"hello\nworld\ntest".to_vec());

        assert_eq!(buffer.get_line(0), Some(b"hello\n".to_vec()));
        assert_eq!(buffer.get_line(1), Some(b"world\n".to_vec()));
        assert_eq!(buffer.get_line(2), Some(b"test".to_vec()));
        assert_eq!(buffer.get_line(3), None);
    }

    #[test]
    fn test_multiple_operations() {
        let mut buffer = TextBuffer::from_bytes(b"line1\nline2\nline3".to_vec());

        buffer.insert_bytes(0, b"start\n".to_vec());
        assert_eq!(buffer.line_count(), Some(4));

        buffer.delete_bytes(6, 6); // Delete "line1\n"
        assert_eq!(buffer.line_count(), Some(3));

        buffer.insert_bytes(6, b"new\n".to_vec());
        assert_eq!(buffer.line_count(), Some(4));

        let text = buffer.get_all_text();
        assert_eq!(text, b"start\nnew\nline2\nline3");
    }

    #[test]
    fn test_get_text_range() {
        let buffer = TextBuffer::from_bytes(b"hello world".to_vec());

        assert_eq!(buffer.get_text_range(0, 5), Some(b"hello".to_vec()));
        assert_eq!(buffer.get_text_range(6, 5), Some(b"world".to_vec()));
        assert_eq!(buffer.get_text_range(0, 11), Some(b"hello world".to_vec()));
    }

    #[test]
    fn test_empty_operations() {
        let mut buffer = TextBuffer::from_bytes(b"hello".to_vec());

        buffer.insert_bytes(2, Vec::new());
        assert_eq!(buffer.get_all_text(), b"hello");

        buffer.delete_bytes(2, 0);
        assert_eq!(buffer.get_all_text(), b"hello");
    }

    #[test]
    fn test_sequential_inserts_at_beginning() {
        // Regression test for piece tree duplicate insertion bug
        let mut buffer = TextBuffer::from_bytes(b"initial\ntext".to_vec());

        // Delete all
        buffer.delete_bytes(0, 12);
        assert_eq!(buffer.get_all_text(), b"");

        // Insert 'a' at 0
        buffer.insert_bytes(0, vec![b'a']);
        assert_eq!(buffer.get_all_text(), b"a");

        // Insert 'b' at 0 (should give "ba")
        buffer.insert_bytes(0, vec![b'b']);
        assert_eq!(buffer.get_all_text(), b"ba");
    }

    // ===== Phase 1-3: Large File Support Tests =====

    mod large_file_support {
        use super::*;
        use crate::piece_tree::StringBuffer;
        use std::fs::File;
        use std::io::Write;
        use tempfile::TempDir;

        // Phase 1: Option<usize> Type Safety Tests

        #[test]
        fn test_line_feed_count_is_some_for_loaded_buffer() {
            let buffer = StringBuffer::new(0, b"hello\nworld\ntest".to_vec());
            assert_eq!(buffer.line_feed_count(), Some(2));
        }

        #[test]
        fn test_line_feed_count_is_none_for_unloaded_buffer() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("test.txt");

            let buffer = StringBuffer::new_unloaded(0, file_path, 0, 100);
            assert_eq!(buffer.line_feed_count(), None);
        }

        #[test]
        fn test_line_count_is_some_for_small_buffer() {
            let buffer = TextBuffer::from_bytes(b"hello\nworld\ntest".to_vec());
            assert_eq!(buffer.line_count(), Some(3));
        }

        #[test]
        fn test_piece_tree_works_with_none_line_count() {
            // Create a buffer with no line count information
            let buffer = StringBuffer::new_loaded(0, b"hello\nworld".to_vec(), false);
            assert_eq!(buffer.line_feed_count(), None);

            // Create piece tree without line feed count
            use crate::piece_tree::{BufferLocation, PieceTree};
            let tree = PieceTree::new(BufferLocation::Stored(0), 0, 11, None);

            // line_count should return None
            assert_eq!(tree.line_count(), None);
        }

        // Phase 2: BufferData Enum Tests

        #[test]
        fn test_buffer_data_loaded_variant() {
            let data = b"hello world".to_vec();
            let buffer = StringBuffer::new_loaded(0, data.clone(), true);

            assert!(buffer.is_loaded());
            assert_eq!(buffer.get_data(), Some(&data[..]));
            assert!(buffer.get_line_starts().is_some());
        }

        #[test]
        fn test_buffer_data_loaded_without_line_starts() {
            let data = b"hello\nworld".to_vec();
            let buffer = StringBuffer::new_loaded(0, data.clone(), false);

            assert!(buffer.is_loaded());
            assert_eq!(buffer.get_data(), Some(&data[..]));
            assert_eq!(buffer.get_line_starts(), None); // No line indexing
        }

        #[test]
        fn test_buffer_data_unloaded_variant() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("test.txt");

            let buffer = StringBuffer::new_unloaded(0, file_path.clone(), 0, 100);

            assert!(!buffer.is_loaded());
            assert_eq!(buffer.get_data(), None);
            assert_eq!(buffer.get_line_starts(), None);
        }

        #[test]
        fn test_buffer_load_method() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("test.txt");

            // Create test file
            let test_data = b"hello world";
            File::create(&file_path)
                .unwrap()
                .write_all(test_data)
                .unwrap();

            // Create unloaded buffer
            let mut buffer = StringBuffer::new_unloaded(0, file_path, 0, test_data.len());
            assert!(!buffer.is_loaded());

            // Load the buffer
            buffer.load().unwrap();

            // Now it should be loaded
            assert!(buffer.is_loaded());
            assert_eq!(buffer.get_data(), Some(&test_data[..]));
        }

        #[test]
        fn test_string_buffer_new_vs_new_loaded() {
            let data = b"hello\nworld".to_vec();

            // StringBuffer::new should compute line starts
            let buf1 = StringBuffer::new(0, data.clone());
            assert!(buf1.is_loaded());
            assert!(buf1.get_line_starts().is_some());
            assert_eq!(buf1.line_feed_count(), Some(1));

            // StringBuffer::new_loaded with compute_lines=false should not
            let buf2 = StringBuffer::new_loaded(0, data.clone(), false);
            assert!(buf2.is_loaded());
            assert_eq!(buf2.get_line_starts(), None);
            assert_eq!(buf2.line_feed_count(), None);
        }

        // Phase 3: Large File Detection Tests

        #[test]
        fn test_load_small_file_eager_loading() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("small.txt");

            // Create a small file (10 bytes < 100MB threshold)
            let test_data = b"hello\ntest";
            File::create(&file_path)
                .unwrap()
                .write_all(test_data)
                .unwrap();

            // Load with default threshold
            let buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();

            // Should be eagerly loaded (not large_file mode)
            assert!(!buffer.large_file);
            assert_eq!(buffer.total_bytes(), test_data.len());
            assert_eq!(buffer.line_count(), Some(2)); // Has line indexing
            assert_eq!(buffer.get_all_text(), test_data);

            // The buffer should be loaded
            assert!(buffer.buffers[0].is_loaded());
        }

        #[test]
        fn test_load_large_file_lazy_loading() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("large.txt");

            // Create a "large" file by using a small threshold
            let test_data = b"hello\nworld\ntest";
            File::create(&file_path)
                .unwrap()
                .write_all(test_data)
                .unwrap();

            // Load with threshold of 10 bytes (file is 17 bytes, so it's "large")
            let buffer = TextBuffer::load_from_file(&file_path, 10).unwrap();

            // Should be in large_file mode
            assert!(buffer.large_file);
            assert_eq!(buffer.total_bytes(), test_data.len());

            // Should NOT have line indexing
            assert_eq!(buffer.line_count(), None);

            // The buffer should be unloaded
            assert!(!buffer.buffers[0].is_loaded());
            assert_eq!(buffer.buffers[0].get_data(), None);
        }

        #[test]
        fn test_large_file_threshold_boundary() {
            let temp_dir = TempDir::new().unwrap();

            // Test exactly at threshold
            let file_path = temp_dir.path().join("at_threshold.txt");
            let test_data = vec![b'x'; 100];
            File::create(&file_path)
                .unwrap()
                .write_all(&test_data)
                .unwrap();

            // Load with threshold of 100 bytes - should be large file (>= threshold)
            let buffer = TextBuffer::load_from_file(&file_path, 100).unwrap();
            assert!(buffer.large_file);

            // Test just below threshold
            let file_path2 = temp_dir.path().join("below_threshold.txt");
            let test_data2 = vec![b'x'; 99];
            File::create(&file_path2)
                .unwrap()
                .write_all(&test_data2)
                .unwrap();

            // Load with threshold of 100 bytes - should be small file (< threshold)
            let buffer2 = TextBuffer::load_from_file(&file_path2, 100).unwrap();
            assert!(!buffer2.large_file);
        }

        #[test]
        fn test_large_file_default_threshold() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("test.txt");

            // Create a small file
            File::create(&file_path)
                .unwrap()
                .write_all(b"hello")
                .unwrap();

            // Load with threshold 0 - should use DEFAULT_LARGE_FILE_THRESHOLD
            let buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();

            // 5 bytes < 100MB, so should not be large file
            assert!(!buffer.large_file);
        }

        #[test]
        fn test_large_file_has_correct_piece_tree_structure() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("large.txt");

            let test_data = b"hello world";
            File::create(&file_path)
                .unwrap()
                .write_all(test_data)
                .unwrap();

            // Load as large file
            let buffer = TextBuffer::load_from_file(&file_path, 5).unwrap();

            // Should have correct total bytes
            assert_eq!(buffer.total_bytes(), test_data.len());

            // Should have 1 buffer
            assert_eq!(buffer.buffers.len(), 1);

            // Buffer should be unloaded
            assert!(!buffer.buffers[0].is_loaded());
        }

        #[test]
        fn test_empty_large_file() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("empty.txt");

            // Create an empty file
            File::create(&file_path).unwrap();

            // Load as large file
            let buffer = TextBuffer::load_from_file(&file_path, 0).unwrap();

            // Empty file is handled gracefully
            assert_eq!(buffer.total_bytes(), 0);
            assert!(buffer.is_empty());
        }

        #[test]
        fn test_large_file_basic_api_operations() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("large_test.txt");

            // Create a test file with known content
            let test_data = b"line1\nline2\nline3\nline4\n";
            File::create(&file_path)
                .unwrap()
                .write_all(test_data)
                .unwrap();

            // Load as large file (use small threshold to trigger large file mode)
            let mut buffer = TextBuffer::load_from_file(&file_path, 10).unwrap();

            // Verify it's in large file mode
            assert!(buffer.large_file);
            assert_eq!(buffer.line_count(), None); // No line indexing

            // Test basic access functions
            assert_eq!(buffer.total_bytes(), test_data.len());
            assert!(!buffer.is_empty());
            assert_eq!(buffer.len(), test_data.len());

            // Test reading operations using get_text_range_mut (lazy loads on demand)
            let range_result = buffer.get_text_range_mut(0, 5).unwrap();
            assert_eq!(range_result, b"line1");

            let range_result2 = buffer.get_text_range_mut(6, 5).unwrap();
            assert_eq!(range_result2, b"line2");

            // Test get_all_text (via get_text_range after lazy loading)
            let all_text = buffer.get_all_text();
            assert_eq!(all_text, test_data);

            // Test slice_bytes method
            assert_eq!(buffer.slice_bytes(0..5), b"line1");

            // Test basic editing operations
            // Insert at offset 0
            buffer.insert_bytes(0, b"prefix_".to_vec());
            assert_eq!(buffer.total_bytes(), test_data.len() + 7);
            assert!(buffer.is_modified());

            // Verify the insertion worked
            let text_after_insert = buffer.get_all_text();
            assert_eq!(&text_after_insert[0..7], b"prefix_");
            assert_eq!(&text_after_insert[7..12], b"line1");

            // Delete some bytes
            buffer.delete_bytes(0, 7);
            assert_eq!(buffer.total_bytes(), test_data.len());

            // Verify deletion worked - should be back to original
            let text_after_delete = buffer.get_all_text();
            assert_eq!(text_after_delete, test_data);

            // Insert at end
            let end_offset = buffer.total_bytes();
            buffer.insert_bytes(end_offset, b"suffix".to_vec());
            assert_eq!(buffer.total_bytes(), test_data.len() + 6);

            // Verify end insertion
            let final_text = buffer.get_all_text();
            assert!(final_text.ends_with(b"suffix"));
            assert_eq!(&final_text[0..test_data.len()], test_data);

            // Test offset_to_position
            // Note: Without line indexing, position tracking is limited
            // but byte-level operations still work
            let pos = buffer.offset_to_position(0);
            assert_eq!(pos.column, 0);

            // Test position_to_offset
            let offset = buffer.position_to_offset(Position { line: 0, column: 0 });
            assert_eq!(offset, 0);

            // Test replace operations
            let replace_result = buffer.replace_range(0..5, "START");
            assert!(replace_result);

            let text_after_replace = buffer.get_all_text();
            assert!(text_after_replace.starts_with(b"START"));
        }

        #[test]
        fn test_large_file_chunk_based_loading() {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("huge.txt");

            // Create a file larger than LOAD_CHUNK_SIZE (1MB)
            // We'll create a 3MB file with a repeating pattern so we can verify chunks
            let chunk_size = LOAD_CHUNK_SIZE; // 1MB
            let file_size = chunk_size * 3; // 3MB

            // Pattern: "AAAA...AAAA" (1MB of A's), "BBBB...BBBB" (1MB of B's), "CCCC...CCCC" (1MB of C's)
            let mut file = File::create(&file_path).unwrap();
            file.write_all(&vec![b'A'; chunk_size]).unwrap();
            file.write_all(&vec![b'B'; chunk_size]).unwrap();
            file.write_all(&vec![b'C'; chunk_size]).unwrap();
            file.flush().unwrap();

            // Load as large file (use threshold of 1 byte to ensure large file mode)
            let mut buffer = TextBuffer::load_from_file(&file_path, 1).unwrap();

            // Verify it's in large file mode
            assert!(buffer.large_file);
            assert_eq!(buffer.total_bytes(), file_size);

            // Buffer should be unloaded initially
            assert!(!buffer.buffers[0].is_loaded());

            // Read from the first chunk (should load only first 1MB)
            let first_chunk_data = buffer.get_text_range_mut(0, 1024).unwrap();
            assert_eq!(first_chunk_data.len(), 1024);
            assert!(first_chunk_data.iter().all(|&b| b == b'A'));

            // Read from the middle chunk (offset = 1MB, should load second 1MB)
            let second_chunk_data = buffer.get_text_range_mut(chunk_size, 1024).unwrap();
            assert_eq!(second_chunk_data.len(), 1024);
            assert!(second_chunk_data.iter().all(|&b| b == b'B'));

            // Read from the last chunk (offset = 2MB, should load third 1MB)
            let third_chunk_data = buffer.get_text_range_mut(chunk_size * 2, 1024).unwrap();
            assert_eq!(third_chunk_data.len(), 1024);
            assert!(third_chunk_data.iter().all(|&b| b == b'C'));

            // Verify we can read across chunk boundaries
            // Read from middle of first chunk to middle of second chunk
            let cross_chunk_offset = chunk_size - 512;
            let cross_chunk_data = buffer.get_text_range_mut(cross_chunk_offset, 1024).unwrap();
            assert_eq!(cross_chunk_data.len(), 1024);
            // First 512 bytes should be 'A', next 512 bytes should be 'B'
            assert!(cross_chunk_data[..512].iter().all(|&b| b == b'A'));
            assert!(cross_chunk_data[512..].iter().all(|&b| b == b'B'));

            // After chunk-based loading, verify the piece tree has been split
            // The number of buffers should be greater than 1 (original + chunks)
            assert!(buffer.buffers.len() > 1,
                "Expected multiple buffers after chunk-based loading, got {}",
                buffer.buffers.len());

            // Test that editing still works after chunk-based loading
            buffer.insert_bytes(0, b"PREFIX".to_vec());
            assert_eq!(buffer.total_bytes(), file_size + 6);

            let after_insert = buffer.get_text_range_mut(0, 6).unwrap();
            assert_eq!(after_insert, b"PREFIX");

            // Verify the original data is still there after the prefix
            let after_prefix = buffer.get_text_range_mut(6, 10).unwrap();
            assert!(after_prefix.iter().all(|&b| b == b'A'));

            // Most importantly: validate the entire buffer content matches the original file
            // Create a fresh buffer to read the original file
            let mut buffer2 = TextBuffer::load_from_file(&file_path, 1).unwrap();

            // Read the entire file in chunks and verify each chunk
            let chunk_read_size = 64 * 1024; // Read in 64KB chunks for efficiency
            let mut offset = 0;
            while offset < file_size {
                let bytes_to_read = chunk_read_size.min(file_size - offset);
                let chunk_data = buffer2.get_text_range_mut(offset, bytes_to_read).unwrap();

                // Determine which section of the file we're reading
                let first_mb_end = chunk_size;
                let second_mb_end = chunk_size * 2;

                // Validate the data based on which MB section we're in
                for (i, &byte) in chunk_data.iter().enumerate() {
                    let file_offset = offset + i;
                    let expected = if file_offset < first_mb_end {
                        b'A'
                    } else if file_offset < second_mb_end {
                        b'B'
                    } else {
                        b'C'
                    };
                    assert_eq!(byte, expected,
                        "Mismatch at file offset {}: expected {}, got {}",
                        file_offset, expected as char, byte as char);
                }

                offset += bytes_to_read;
            }
        }
    }
}

#[cfg(test)]
mod property_tests {
    use super::*;
    use proptest::prelude::*;

    // Generate text with some newlines
    fn text_with_newlines() -> impl Strategy<Value = Vec<u8>> {
        prop::collection::vec(
            prop_oneof![(b'a'..=b'z').prop_map(|c| c), Just(b'\n'),],
            0..100,
        )
    }

    // Strategy to generate operations
    #[derive(Debug, Clone)]
    enum Operation {
        Insert { offset: usize, text: Vec<u8> },
        Delete { offset: usize, bytes: usize },
    }

    fn operation_strategy() -> impl Strategy<Value = Vec<Operation>> {
        prop::collection::vec(
            prop_oneof![
                (0usize..200, text_with_newlines())
                    .prop_map(|(offset, text)| { Operation::Insert { offset, text } }),
                (0usize..200, 1usize..50)
                    .prop_map(|(offset, bytes)| { Operation::Delete { offset, bytes } }),
            ],
            0..50,
        )
    }

    proptest! {
        #[test]
        fn prop_line_count_consistent(text in text_with_newlines()) {
            let buffer = TextBuffer::from_bytes(text.clone());

            let newline_count = text.iter().filter(|&&b| b == b'\n').count();
            prop_assert_eq!(buffer.line_count(), Some(newline_count + 1));
        }

        #[test]
        fn prop_get_all_text_matches_original(text in text_with_newlines()) {
            let buffer = TextBuffer::from_bytes(text.clone());
            prop_assert_eq!(buffer.get_all_text(), text);
        }

        #[test]
        fn prop_insert_increases_size(
            text in text_with_newlines(),
            offset in 0usize..100,
            insert_text in text_with_newlines()
        ) {
            let mut buffer = TextBuffer::from_bytes(text);
            let initial_bytes = buffer.total_bytes();

            let offset = offset.min(buffer.total_bytes());
            buffer.insert_bytes(offset, insert_text.clone());

            prop_assert_eq!(buffer.total_bytes(), initial_bytes + insert_text.len());
        }

        #[test]
        fn prop_delete_decreases_size(
            text in text_with_newlines(),
            offset in 0usize..100,
            delete_bytes in 1usize..50
        ) {
            if text.is_empty() {
                return Ok(());
            }

            let mut buffer = TextBuffer::from_bytes(text);
            let initial_bytes = buffer.total_bytes();

            let offset = offset.min(buffer.total_bytes());
            let delete_bytes = delete_bytes.min(buffer.total_bytes() - offset);

            if delete_bytes == 0 {
                return Ok(());
            }

            buffer.delete_bytes(offset, delete_bytes);

            prop_assert_eq!(buffer.total_bytes(), initial_bytes - delete_bytes);
        }

        #[test]
        fn prop_insert_then_delete_restores_original(
            text in text_with_newlines(),
            offset in 0usize..100,
            insert_text in text_with_newlines()
        ) {
            let mut buffer = TextBuffer::from_bytes(text.clone());

            let offset = offset.min(buffer.total_bytes());
            buffer.insert_bytes(offset, insert_text.clone());
            buffer.delete_bytes(offset, insert_text.len());

            prop_assert_eq!(buffer.get_all_text(), text);
        }

        #[test]
        fn prop_offset_position_roundtrip(text in text_with_newlines()) {
            let buffer = TextBuffer::from_bytes(text.clone());

            for offset in 0..text.len() {
                let pos = buffer.offset_to_position(offset);
                let back = buffer.position_to_offset(pos);
                prop_assert_eq!(back, offset, "Failed roundtrip for offset {}", offset);
            }
        }

        #[test]
        fn prop_get_text_range_valid(
            text in text_with_newlines(),
            offset in 0usize..100,
            length in 1usize..50
        ) {
            if text.is_empty() {
                return Ok(());
            }

            let buffer = TextBuffer::from_bytes(text.clone());
            let offset = offset.min(buffer.total_bytes());
            let length = length.min(buffer.total_bytes() - offset);

            if length == 0 {
                return Ok(());
            }

            let result = buffer.get_text_range(offset, length);
            prop_assert_eq!(result, Some(text[offset..offset + length].to_vec()));
        }

        #[test]
        fn prop_operations_maintain_consistency(operations in operation_strategy()) {
            let mut buffer = TextBuffer::from_bytes(b"initial\ntext".to_vec());
            let mut expected_text = b"initial\ntext".to_vec();

            for op in operations {
                match op {
                    Operation::Insert { offset, text } => {
                        let offset = offset.min(buffer.total_bytes());
                        buffer.insert_bytes(offset, text.clone());

                        // Update expected
                        let offset = offset.min(expected_text.len());
                        expected_text.splice(offset..offset, text);
                    }
                    Operation::Delete { offset, bytes } => {
                        if offset < buffer.total_bytes() {
                            let bytes = bytes.min(buffer.total_bytes() - offset);
                            buffer.delete_bytes(offset, bytes);

                            // Update expected
                            if offset < expected_text.len() {
                                let bytes = bytes.min(expected_text.len() - offset);
                                expected_text.drain(offset..offset + bytes);
                            }
                        }
                    }
                }
            }

            prop_assert_eq!(buffer.get_all_text(), expected_text);
        }

        #[test]
        fn prop_line_count_never_zero(operations in operation_strategy()) {
            let mut buffer = TextBuffer::from_bytes(b"test".to_vec());

            for op in operations {
                match op {
                    Operation::Insert { offset, text } => {
                        let offset = offset.min(buffer.total_bytes());
                        buffer.insert_bytes(offset, text);
                    }
                    Operation::Delete { offset, bytes } => {
                        buffer.delete_bytes(offset, bytes);
                    }
                }

                // Document always has at least 1 line
                prop_assert!(buffer.line_count().unwrap_or(1) >= 1);
            }
        }

        #[test]
        fn prop_total_bytes_never_negative(operations in operation_strategy()) {
            let mut buffer = TextBuffer::from_bytes(b"test".to_vec());

            for op in operations {
                match op {
                    Operation::Insert { offset, text } => {
                        let offset = offset.min(buffer.total_bytes());
                        buffer.insert_bytes(offset, text);
                    }
                    Operation::Delete { offset, bytes } => {
                        buffer.delete_bytes(offset, bytes);
                    }
                }

                // Bytes should never overflow
                prop_assert!(buffer.total_bytes() < 10_000_000);
            }
        }

        #[test]
        fn prop_piece_tree_and_line_index_stay_synced(operations in operation_strategy()) {
            let mut buffer = TextBuffer::from_bytes(b"line1\nline2\nline3".to_vec());

            for op in operations {
                match op {
                    Operation::Insert { offset, text } => {
                        let offset = offset.min(buffer.total_bytes());
                        buffer.insert_bytes(offset, text);
                    }
                    Operation::Delete { offset, bytes } => {
                        buffer.delete_bytes(offset, bytes);
                    }
                }

                // Verify we can still convert between offsets and positions
                if buffer.total_bytes() > 0 {
                    let mid_offset = buffer.total_bytes() / 2;
                    let pos = buffer.offset_to_position(mid_offset);
                    let back = buffer.position_to_offset(pos);

                    // Should be able to roundtrip
                    prop_assert!(back <= buffer.total_bytes());
                }
            }
        }
    }
}
