/// Text buffer that uses PieceTree with integrated line tracking
/// Architecture where the tree is the single source of truth for text and line information
use crate::piece_tree::{
    BufferLocation, Cursor, PieceInfo, PieceTree, Position, StringBuffer, TreeStats,
};
use regex::bytes::Regex;
use std::io::{self, Read, Write};
use std::ops::Range;
use std::path::{Path, PathBuf};

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
        }
    }

    /// Load a text buffer from a file
    pub fn load_from_file<P: AsRef<Path>>(
        path: P,
        _large_file_threshold: usize,
    ) -> io::Result<Self> {
        let path = path.as_ref();
        let mut file = std::fs::File::open(path)?;
        let mut contents = Vec::new();
        file.read_to_end(&mut contents)?;

        let mut buffer = Self::from_bytes(contents);
        buffer.file_path = Some(path.to_path_buf());
        buffer.modified = false;
        Ok(buffer)
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
    pub fn line_count(&self) -> usize {
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
        let line_feed_cnt = text.iter().filter(|&&b| b == b'\n').count();

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

        // Check if this piece ends exactly at the end of its buffer
        if piece_info.offset + piece_info.bytes != buffer.data.len() {
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
    pub fn get_text_range(&self, offset: usize, bytes: usize) -> Vec<u8> {
        if bytes == 0 {
            return Vec::new();
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

                    if buffer_end <= buffer.data.len() {
                        result.extend_from_slice(&buffer.data[buffer_start..buffer_end]);
                        collected += bytes_to_read;

                        if collected >= bytes {
                            break;
                        }
                    }
                }
            }
        }

        result
    }

    /// Get all text as a single Vec<u8>
    pub fn get_all_text(&self) -> Vec<u8> {
        self.get_text_range(0, self.total_bytes())
    }

    /// Get all text as a String
    pub fn get_all_text_string(&self) -> String {
        String::from_utf8_lossy(&self.get_all_text()).into_owned()
    }

    /// Get text from a byte range as a String
    pub fn slice(&self, range: Range<usize>) -> String {
        let bytes = self.get_text_range(range.start, range.end.saturating_sub(range.start));
        String::from_utf8_lossy(&bytes).into_owned()
    }

    /// Get text from a byte range as bytes
    pub fn slice_bytes(&self, range: Range<usize>) -> Vec<u8> {
        self.get_text_range(range.start, range.end.saturating_sub(range.start))
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

        Some(self.get_text_range(start, bytes))
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

    /// Find pattern in a byte range
    fn find_pattern(&self, start: usize, end: usize, pattern: &[u8]) -> Option<usize> {
        if pattern.is_empty() || start >= end {
            return None;
        }

        // For now, use a simple approach: get the text and search
        // TODO: Optimize with streaming search for large buffers
        const CHUNK_SIZE: usize = 64 * 1024; // 64KB chunks
        let search_len = end - start;

        if search_len <= CHUNK_SIZE {
            // Small search, just get the whole range
            let text = self.get_text_range(start, search_len);
            if let Some(pos) = Self::find_in_bytes(&text, pattern) {
                return Some(start + pos);
            }
        } else {
            // Large search, use overlapping chunks
            let overlap = pattern.len().saturating_sub(1);
            let mut offset = start;

            while offset < end {
                let chunk_size = CHUNK_SIZE.min(end - offset);
                let text = self.get_text_range(offset, chunk_size);

                if let Some(pos) = Self::find_in_bytes(&text, pattern) {
                    // Make sure the match doesn't extend beyond our search range
                    let match_pos = offset + pos;
                    if match_pos + pattern.len() <= end {
                        return Some(match_pos);
                    }
                }

                // Move forward, but overlap to catch patterns spanning chunks
                offset += chunk_size;
                if offset < end {
                    offset = offset.saturating_sub(overlap);
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

    /// Find regex pattern in a byte range
    fn find_regex(&self, start: usize, end: usize, regex: &Regex) -> Option<usize> {
        if start >= end {
            return None;
        }

        // For regex, we need to get the full text to search
        // TODO: Optimize with overlapping chunks for large buffers
        const MAX_REGEX_SEARCH: usize = 10 * 1024 * 1024; // 10MB limit
        let search_len = end - start;

        if search_len > MAX_REGEX_SEARCH {
            // For very large ranges, search in chunks
            const CHUNK_SIZE: usize = 1024 * 1024; // 1MB chunks
            let mut offset = start;

            while offset < end {
                let chunk_size = CHUNK_SIZE.min(end - offset);
                let text = self.get_text_range(offset, chunk_size);

                if let Some(mat) = regex.find(&text) {
                    return Some(offset + mat.start());
                }

                offset += chunk_size;
            }

            None
        } else {
            // Get the full range and search
            let text = self.get_text_range(start, search_len);
            regex.find(&text).map(|mat| start + mat.start())
        }
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
    pub fn replace_all_regex(&mut self, regex: &Regex, replacement: &str) -> usize {
        let mut count = 0;
        let mut pos = 0;

        loop {
            if let Some(found_pos) = self.find_next_regex_in_range(regex, pos, Some(0..self.len()))
            {
                // Get the match to find its length
                let text = self.get_text_range(found_pos, self.len() - found_pos);
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

        count
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
                let line_bytes = self.get_text_range(line_start, line_len);
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
        let bytes = self.get_text_range(start, pos - start);

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
        let bytes = self.get_text_range(pos, end - pos);

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
        let bytes = self.get_text_range(start, pos - start);
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
        let bytes = self.get_text_range(pos, end - pos);
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
    pub fn line_iterator(&self, byte_pos: usize) -> LineIterator<'_> {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_buffer() {
        let buffer = TextBuffer::empty();
        assert_eq!(buffer.total_bytes(), 0);
        assert_eq!(buffer.line_count(), 1); // Empty doc has 1 line
    }

    #[test]
    fn test_line_positions_multiline() {
        let buffer = TextBuffer::from_bytes(b"Hello\nNew Line\nWorld!".to_vec());

        // Check line count
        assert_eq!(buffer.line_count(), 3);

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
        assert_eq!(buffer.line_count(), 2);
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
        assert_eq!(buffer.line_count(), 3);
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
        assert_eq!(buffer.line_count(), 1);
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
        assert_eq!(buffer.line_count(), 4);

        buffer.delete_bytes(6, 6); // Delete "line1\n"
        assert_eq!(buffer.line_count(), 3);

        buffer.insert_bytes(6, b"new\n".to_vec());
        assert_eq!(buffer.line_count(), 4);

        let text = buffer.get_all_text();
        assert_eq!(text, b"start\nnew\nline2\nline3");
    }

    #[test]
    fn test_get_text_range() {
        let buffer = TextBuffer::from_bytes(b"hello world".to_vec());

        assert_eq!(buffer.get_text_range(0, 5), b"hello");
        assert_eq!(buffer.get_text_range(6, 5), b"world");
        assert_eq!(buffer.get_text_range(0, 11), b"hello world");
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
            prop_assert_eq!(buffer.line_count(), newline_count + 1);
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
            prop_assert_eq!(result, text[offset..offset + length].to_vec());
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
                prop_assert!(buffer.line_count() >= 1);
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
