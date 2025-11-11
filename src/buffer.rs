use crate::chunk_tree::{ChunkTree, ChunkTreeConfig};
use crate::chunked_search::OverlappingChunks;
use crate::line_cache::{LineCache, LineInfo};
use crate::persistence::ChunkTreePersistence;
use crate::virtual_buffer::VirtualBuffer;
use regex::bytes::Regex;
use std::io::{self, Read, Write};
use std::ops::Range;
use std::path::{Path, PathBuf};

/// Default configuration for ChunkTree
// Chunk size of 4KB provides good balance between memory usage and performance
// For a 61MB file, this creates ~15K leaf nodes instead of 1M with 64-byte chunks
const DEFAULT_CONFIG: ChunkTreeConfig = ChunkTreeConfig::new(4096, 128);

// For large jumps (> 100KB), use estimation instead of iterating
// This prevents hanging when jumping to the end of large files
const ESTIMATION_THRESHOLD: usize = 10_000_000; // 10 MB

/// Represents a line number that may be absolute (known/cached) or relative (estimated)
/// NOTE: This enum is kept for backward compatibility but will eventually be removed
/// as we transition fully to iterator-based APIs
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LineNumber {
    /// Absolute line number - this is the actual line number in the file
    Absolute(usize),

    /// Relative/estimated line number - calculated from last known cached position
    Relative {
        /// The estimated line number
        line: usize,
        /// The last cached line number we used as a base
        from_cached_line: usize,
    },
}

impl LineNumber {
    /// Get the line number value regardless of whether it's absolute or relative
    pub fn value(&self) -> usize {
        match self {
            LineNumber::Absolute(line) => *line,
            LineNumber::Relative { line, .. } => *line,
        }
    }

    /// Check if this is an absolute (cached) line number
    pub fn is_absolute(&self) -> bool {
        matches!(self, LineNumber::Absolute(_))
    }

    /// Check if this is a relative (estimated) line number
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

/// A text buffer backed by VirtualBuffer
pub struct Buffer {
    /// The underlying virtual buffer
    virtual_buffer: VirtualBuffer,

    /// Optional file path for persistence
    file_path: Option<PathBuf>,

    /// Has the buffer been modified since last save?
    modified: bool,

    /// Cache of line number to byte offset mappings
    line_cache: LineCache,
}

impl Buffer {
    /// Create a new empty buffer
    pub fn new() -> Self {
        let persistence = Box::new(ChunkTreePersistence::new(DEFAULT_CONFIG));
        Self {
            virtual_buffer: VirtualBuffer::new(persistence),
            file_path: None,
            modified: false,
            line_cache: LineCache::new(),
        }
    }

    /// Create a buffer from a string
    pub fn from_str(s: &str) -> Self {
        // Leak the string to get 'static lifetime for ChunkTree
        let leaked: &'static [u8] = Box::leak(s.as_bytes().to_vec().into_boxed_slice());
        let tree = ChunkTree::from_slice(leaked, DEFAULT_CONFIG);
        let persistence = Box::new(ChunkTreePersistence::from_tree(tree));

        Self {
            virtual_buffer: VirtualBuffer::new(persistence),
            file_path: None,
            modified: false,
            line_cache: LineCache::new(),
        }
    }

    /// Load a buffer from a file
    pub fn load_from_file<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let path = path.as_ref();
        let mut file = std::fs::File::open(path)?;
        let mut contents = Vec::new();
        file.read_to_end(&mut contents)?;

        // Leak for 'static lifetime
        let leaked: &'static [u8] = Box::leak(contents.into_boxed_slice());
        let tree = ChunkTree::from_slice(leaked, DEFAULT_CONFIG);
        let persistence = Box::new(ChunkTreePersistence::from_tree(tree));

        Ok(Self {
            virtual_buffer: VirtualBuffer::new(persistence),
            file_path: Some(path.to_path_buf()),
            modified: false,
            line_cache: LineCache::new(),
        })
    }

    /// Save the buffer to its associated file
    pub fn save(&mut self) -> io::Result<()> {
        if let Some(path) = &self.file_path {
            self.save_to_file(path.clone())
        } else {
            Err(io::Error::new(
                io::ErrorKind::NotFound,
                "No file path set for buffer",
            ))
        }
    }

    /// Save the buffer to a specific file
    pub fn save_to_file<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()> {
        let contents = self.virtual_buffer.read(0, self.virtual_buffer.len())?;
        let mut file = std::fs::File::create(path.as_ref())?;
        file.write_all(&contents)?;
        self.modified = false;
        self.file_path = Some(path.as_ref().to_path_buf());
        Ok(())
    }

    /// Insert text at a position
    pub fn insert(&mut self, pos: usize, text: &str) {
        if text.is_empty() {
            return;
        }
        let _ = self.virtual_buffer.insert(pos, text.as_bytes());
        self.modified = true;

        // Update line cache with insertion
        let newlines = text.matches('\n').count();
        self.handle_line_cache_insertion(pos, text.len(), newlines);
    }

    /// Delete a range of bytes
    pub fn delete(&mut self, range: Range<usize>) {
        if range.is_empty() {
            return;
        }

        // Get the text being deleted to count newlines
        let deleted_text = self.slice(range.clone());
        let newlines = deleted_text.matches('\n').count();

        let _ = self.virtual_buffer.delete(range.clone());
        self.modified = true;

        // Update line cache with deletion
        self.handle_line_cache_deletion(range.start, range.len(), newlines);
    }

    /// Get a slice of the buffer as a string
    pub fn slice(&self, range: Range<usize>) -> String {
        let len = range.end.saturating_sub(range.start);
        let bytes = self
            .virtual_buffer
            .read(range.start, len)
            .unwrap_or_default();
        String::from_utf8_lossy(&bytes).to_string()
    }

    /// Get a slice of the buffer as bytes
    pub fn slice_bytes(&self, range: Range<usize>) -> Vec<u8> {
        let len = range.end.saturating_sub(range.start);
        self.virtual_buffer
            .read(range.start, len)
            .unwrap_or_default()
    }

    /// Get the entire buffer as a string
    pub fn to_string(&self) -> String {
        self.slice(0..self.len())
    }

    /// Get the length of the buffer in bytes
    pub fn len(&self) -> usize {
        self.virtual_buffer.len()
    }

    /// Check if the buffer is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Get the file path associated with this buffer
    pub fn file_path(&self) -> Option<&Path> {
        self.file_path.as_deref()
    }

    /// Set the file path
    pub fn set_file_path(&mut self, path: PathBuf) {
        self.file_path = Some(path);
    }

    /// Is the buffer modified?
    pub fn is_modified(&self) -> bool {
        self.modified
    }

    /// Mark the buffer as unmodified
    pub fn clear_modified(&mut self) {
        self.modified = false;
    }

    /// Create a line iterator starting from a byte position
    /// The iterator automatically finds the start of the line containing byte_pos
    pub fn line_iterator(&self, byte_pos: usize) -> LineIterator {
        LineIterator::new(&self.virtual_buffer, byte_pos)
    }

    /// Find the next occurrence of a pattern starting from a given position
    /// Returns the byte offset of the match, or None if not found
    /// Uses streaming iteration to avoid materializing the entire buffer
    pub fn find_next(&self, pattern: &str, start_pos: usize) -> Option<usize> {
        if pattern.is_empty() {
            return None;
        }

        let pattern_bytes = pattern.as_bytes();
        let buffer_len = self.len();

        // Search from start_pos to end
        if start_pos < buffer_len {
            if let Some(offset) = self.find_pattern_streaming(start_pos, buffer_len, pattern_bytes)
            {
                return Some(offset);
            }
        }

        // Wrap around: search from beginning to start_pos
        if start_pos > 0 {
            if let Some(offset) = self.find_pattern_streaming(0, start_pos, pattern_bytes) {
                return Some(offset);
            }
        }

        None
    }

    /// Find the next occurrence of a pattern within an optional range
    /// If range is None, searches the entire buffer with wrap-around (same as find_next)
    /// If range is Some, searches only within that range without wrap-around
    /// Returns the byte offset of the match, or None if not found
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
                self.find_pattern_streaming(search_start, search_end, pattern_bytes)
            } else {
                None
            }
        } else {
            // No range specified, use normal find_next with wrap-around
            self.find_next(pattern, start_pos)
        }
    }

    /// Streaming pattern search from start to end position using overlapping chunks
    /// Uses the VSCode-style buffered iteration approach with standard string search
    fn find_pattern_streaming(&self, start: usize, end: usize, pattern: &[u8]) -> Option<usize> {
        if pattern.is_empty() || start >= end {
            return None;
        }

        const CHUNK_SIZE: usize = 4096;
        let overlap = pattern.len().saturating_sub(1);
        let iter = self.virtual_buffer.iter_at(start);
        let chunks = OverlappingChunks::new(iter, start, end, CHUNK_SIZE, overlap);

        for chunk in chunks {
            // Search the entire buffer to find patterns spanning boundaries
            if let Some(offset) = Self::find_pattern(&chunk.buffer, pattern) {
                let match_end = offset + pattern.len();
                // Only accept matches that END in or after the valid zone
                // This ensures patterns spanning chunk boundaries are found exactly once
                if match_end > chunk.valid_start {
                    let match_pos = chunk.absolute_pos + offset;
                    if match_pos + pattern.len() <= end {
                        return Some(match_pos);
                    }
                }
            }
        }

        None
    }

    /// Find all occurrences of a pattern in a single pass through the buffer
    /// This is much more efficient than calling find_pattern_streaming repeatedly
    fn find_all_matches(&self, start: usize, end: usize, pattern: &[u8]) -> Vec<usize> {
        if pattern.is_empty() || start >= end {
            return Vec::new();
        }

        const CHUNK_SIZE: usize = 4096;
        let overlap = pattern.len().saturating_sub(1);
        let iter = self.virtual_buffer.iter_at(start);
        let chunks = OverlappingChunks::new(iter, start, end, CHUNK_SIZE, overlap);

        let mut matches = Vec::new();

        for chunk in chunks {
            // Search entire buffer including overlap, but only accept matches
            // that END in or after the valid zone to avoid duplicates
            let mut pos = 0;

            while pos + pattern.len() <= chunk.buffer.len() {
                if &chunk.buffer[pos..pos + pattern.len()] == pattern {
                    let match_end = pos + pattern.len();
                    // Only accept matches that END in or after the valid zone
                    // This ensures patterns spanning chunk boundaries are found exactly once
                    if match_end > chunk.valid_start {
                        let match_pos = chunk.absolute_pos + pos;
                        if match_pos + pattern.len() <= end {
                            matches.push(match_pos);
                            // Skip past this match (no overlapping matches)
                            pos += pattern.len();
                        } else {
                            break;
                        }
                    } else {
                        pos += 1;
                    }
                } else {
                    pos += 1;
                }
            }
        }

        matches
    }

    /// Find the next occurrence of a regex pattern starting from a given position
    /// Returns the byte offset of the match, or None if not found
    /// Uses streaming iteration with overlapping chunks to support patterns spanning boundaries
    ///
    /// # Note
    /// The regex engine will only find patterns that fit within the chunk + overlap size.
    /// Default overlap is 4KB, so patterns longer than this may not be found across boundaries.
    pub fn find_next_regex(&self, regex: &Regex, start_pos: usize) -> Option<usize> {
        let buffer_len = self.len();

        // Search from start_pos to end
        if start_pos < buffer_len {
            if let Some(offset) = self.find_regex_streaming(start_pos, buffer_len, regex) {
                return Some(offset);
            }
        }

        // Wrap around: search from beginning to start_pos
        if start_pos > 0 {
            if let Some(offset) = self.find_regex_streaming(0, start_pos, regex) {
                return Some(offset);
            }
        }

        None
    }

    /// Find the next occurrence of a regex pattern within an optional range
    /// If range is None, searches the entire buffer with wrap-around (same as find_next_regex)
    /// If range is Some, searches only within that range without wrap-around
    /// Returns the byte offset of the match, or None if not found
    pub fn find_next_regex_in_range(
        &self,
        regex: &Regex,
        start_pos: usize,
        range: Option<Range<usize>>,
    ) -> Option<usize> {
        if let Some(search_range) = range {
            // Search within range only, no wrap-around
            let search_start = start_pos.max(search_range.start);
            let search_end = search_range.end.min(self.len());

            if search_start < search_end {
                self.find_regex_streaming(search_start, search_end, regex)
            } else {
                None
            }
        } else {
            // No range specified, use normal find_next_regex with wrap-around
            self.find_next_regex(regex, start_pos)
        }
    }

    /// Streaming regex search from start to end position using overlapping chunks
    fn find_regex_streaming(&self, start: usize, end: usize, regex: &Regex) -> Option<usize> {
        if start >= end {
            return None;
        }

        const CHUNK_SIZE: usize = 64 * 1024; // 64KB chunks for regex
        const OVERLAP: usize = 4096; // 4KB overlap to catch patterns spanning boundaries

        let iter = self.virtual_buffer.iter_at(start);
        let chunks = OverlappingChunks::new(iter, start, end, CHUNK_SIZE, OVERLAP);

        for chunk in chunks {
            // Search for all matches in this chunk
            for mat in regex.find_iter(&chunk.buffer) {
                let match_end = mat.start() + mat.len();
                // Only report matches that END in or after the valid zone
                // This ensures patterns spanning chunk boundaries are found exactly once
                if match_end > chunk.valid_start {
                    let match_pos = chunk.absolute_pos + mat.start();
                    if match_pos + mat.len() <= end {
                        return Some(match_pos);
                    }
                }
            }
        }

        None
    }

    /// Replace a specific range of bytes with new text
    /// Returns true if the replacement was successful
    pub fn replace_range(&mut self, range: Range<usize>, replacement: &str) -> bool {
        if range.start > self.len() || range.end > self.len() || range.start > range.end {
            return false;
        }

        // Delete the old text and insert the new text
        if !range.is_empty() {
            self.delete(range.clone());
        }
        if !replacement.is_empty() {
            self.insert(range.start, replacement);
        }

        true
    }

    /// Find the next occurrence of a pattern and replace it
    /// Returns the position of the replacement, or None if pattern not found
    pub fn replace_next(
        &mut self,
        pattern: &str,
        replacement: &str,
        start_pos: usize,
    ) -> Option<usize> {
        if let Some(pos) = self.find_next(pattern, start_pos) {
            let end = pos + pattern.len();
            self.replace_range(pos..end, replacement);
            Some(pos)
        } else {
            None
        }
    }

    /// Replace all occurrences of a pattern with replacement text
    /// Returns the number of replacements made
    pub fn replace_all(&mut self, pattern: &str, replacement: &str) -> usize {
        if pattern.is_empty() {
            return 0;
        }

        let pattern_bytes = pattern.as_bytes();
        let buffer_len = self.len();

        // Find all matches in a single efficient pass through the buffer
        let matches = self.find_all_matches(0, buffer_len, pattern_bytes);

        // Apply replacements in reverse order to preserve positions
        let replacements = matches.len();
        for match_pos in matches.into_iter().rev() {
            let end = match_pos + pattern.len();
            self.replace_range(match_pos..end, replacement);
        }

        replacements
    }

    /// Replace all occurrences of a regex pattern with replacement text
    /// This uses the standard (non-bytes) Regex to support capture groups in replacement string
    /// For searching, it converts buffer content to string which may be less efficient for huge files
    /// Returns the number of replacements made
    ///
    /// # Arguments
    /// * `regex` - A standard regex::Regex (NOT regex::bytes::Regex)
    /// * `replacement` - Replacement string, can include capture groups like $1, $2, etc.
    ///                   Use ${1} syntax when followed by non-whitespace: e.g., "${1}_${2}"
    pub fn replace_all_regex(&mut self, regex: &regex::Regex, replacement: &str) -> usize {
        // For replace with capture groups, we need to materialize the buffer
        // This is less efficient but necessary for proper capture group expansion
        let content = self.to_string();
        let mut replacements = 0;
        let mut matches = Vec::new();

        // Find all matches with their captures
        for captures in regex.captures_iter(&content) {
            if let Some(full_match) = captures.get(0) {
                // Expand the replacement string with capture groups
                let mut expanded = String::new();
                captures.expand(replacement, &mut expanded);
                matches.push((full_match.start(), full_match.end(), expanded));
                replacements += 1;
            }
        }

        // Apply replacements in reverse order to preserve positions
        for (start, end, replacement_text) in matches.into_iter().rev() {
            self.replace_range(start..end, &replacement_text);
        }

        replacements
    }

    /// Helper: Find pattern in haystack using naive string search
    fn find_pattern(haystack: &[u8], needle: &[u8]) -> Option<usize> {
        if needle.is_empty() || haystack.len() < needle.len() {
            return None;
        }

        (0..=(haystack.len() - needle.len())).find(|&i| &haystack[i..i + needle.len()] == needle)
    }

    // Utility methods for character and word boundaries

    /// Find the previous character boundary (UTF-8 aware)
    pub fn prev_char_boundary(&self, pos: usize) -> usize {
        if pos == 0 {
            return 0;
        }

        let mut byte_iter = self.virtual_buffer.iter_at(pos.saturating_sub(1));

        // Move backward until we find a UTF-8 character boundary
        for _ in 0..4 {
            if byte_iter.position() == 0 {
                return 0;
            }

            if let Some(byte) = byte_iter.peek() {
                // Check if this is a UTF-8 leading byte (not a continuation byte)
                if (byte & 0b1100_0000) != 0b1000_0000 {
                    return byte_iter.position();
                }
            }

            byte_iter.prev();
        }

        // Fallback to just moving back one byte
        pos.saturating_sub(1)
    }

    /// Find the next character boundary (UTF-8 aware)
    pub fn next_char_boundary(&self, pos: usize) -> usize {
        let len = self.len();
        if pos >= len {
            return len;
        }

        let mut byte_iter = self.virtual_buffer.iter_at(pos + 1);

        // Move forward until we find a UTF-8 character boundary
        for _ in 0..4 {
            if byte_iter.position() >= len {
                return len;
            }

            if let Some(byte) = byte_iter.peek() {
                // Check if this is a UTF-8 leading byte (not a continuation byte)
                if (byte & 0b1100_0000) != 0b1000_0000 {
                    return byte_iter.position();
                }
            }

            if byte_iter.next().is_none() {
                return len;
            }
        }

        // Fallback
        (pos + 1).min(len)
    }

    /// Find the previous word boundary
    pub fn prev_word_boundary(&self, pos: usize) -> usize {
        if pos == 0 {
            return 0;
        }

        let mut byte_iter = self.virtual_buffer.iter_at(pos.saturating_sub(1));
        let mut found_word_char = false;

        while byte_iter.position() > 0 {
            if let Some(byte) = byte_iter.peek() {
                let ch = byte as char;
                let is_word_char = ch.is_alphanumeric() || ch == '_';

                if found_word_char && !is_word_char {
                    // We've transitioned from word to non-word
                    return byte_iter.position() + 1;
                }

                if is_word_char {
                    found_word_char = true;
                }
            }

            byte_iter.prev();
        }

        0
    }

    /// Find the next word boundary
    pub fn next_word_boundary(&self, pos: usize) -> usize {
        let len = self.len();
        if pos >= len {
            return len;
        }

        let mut byte_iter = self.virtual_buffer.iter_at(pos);
        let mut found_word_char = false;

        while byte_iter.position() < len {
            if let Some(byte) = byte_iter.next() {
                let ch = byte as char;
                let is_word_char = ch.is_alphanumeric() || ch == '_';

                if found_word_char && !is_word_char {
                    // We've transitioned from word to non-word
                    return byte_iter.position();
                }

                if is_word_char {
                    found_word_char = true;
                }
            } else {
                break;
            }
        }

        len
    }

    // LineCache API - The ONLY way to get line number information

    /// Get the line number for a given byte offset.
    /// This will populate the cache if needed by iterating from the nearest known point.
    pub fn get_line_number(&mut self, byte_offset: usize) -> usize {
        // Check if already cached
        if let Some(info) = self.line_cache.entries.get(&byte_offset) {
            return info.line_number;
        }

        // Find nearest cached entry before this offset
        let (start_byte, start_line) =
            if let Some(info) = self.line_cache.get_nearest_before(byte_offset) {
                (info.byte_offset, info.line_number)
            } else {
                (0, 0) // Start from beginning
            };

        let distance = byte_offset.saturating_sub(start_byte);

        if distance > ESTIMATION_THRESHOLD {
            // Estimate line number based on average line length (80 chars)
            let estimated_lines = distance / 80;
            let estimated_line_number = start_line + estimated_lines;

            // Cache this estimate so subsequent calls near this location are fast
            self.line_cache.entries.insert(
                byte_offset,
                LineInfo {
                    line_number: estimated_line_number,
                    byte_offset,
                },
            );

            return estimated_line_number;
        }

        // For small jumps, iterate to get exact line number
        let mut iter = self.line_iterator(start_byte);
        let mut current_line = start_line;

        // Cache the starting position if not already cached
        self.line_cache
            .entries
            .entry(start_byte)
            .or_insert(LineInfo {
                line_number: start_line,
                byte_offset: start_byte,
            });

        while let Some((line_byte, _)) = iter.next() {
            if line_byte > byte_offset {
                break;
            }

            // Cache this line
            self.line_cache.entries.insert(
                line_byte,
                LineInfo {
                    line_number: current_line,
                    byte_offset: line_byte,
                },
            );

            if line_byte == byte_offset {
                return current_line;
            }

            current_line += 1;
        }

        // If we get here, byte_offset is beyond what we found
        current_line.saturating_sub(1)
    }

    /// Populate the line cache for a range of lines starting from a byte offset.
    /// This is useful for pre-populating the viewport area.
    /// Returns the line number of the starting byte offset.
    pub fn populate_line_cache(&mut self, start_byte: usize, line_count: usize) -> usize {
        let start_line = self.get_line_number(start_byte);

        // Now iterate forward to populate more lines
        let mut iter = self.line_iterator(start_byte);
        let mut current_line = start_line;
        let mut lines_added = 0;

        while let Some((line_byte, _)) = iter.next() {
            if lines_added >= line_count {
                break;
            }

            // Cache this line if not already cached
            self.line_cache
                .entries
                .entry(line_byte)
                .or_insert_with(|| LineInfo {
                    line_number: current_line,
                    byte_offset: line_byte,
                });

            current_line += 1;
            lines_added += 1;
        }

        start_line
    }

    /// Get the byte offset for a line number if it's cached.
    /// Returns None if the line is not in the cache.
    /// This is a read-only operation that doesn't trigger cache population.
    pub fn get_cached_byte_offset_for_line(&self, line_number: usize) -> Option<usize> {
        self.line_cache
            .entries
            .iter()
            .find(|(_, info)| info.line_number == line_number)
            .map(|(_, info)| info.byte_offset)
    }

    /// Invalidate line cache from a byte offset onwards.
    /// This should be called after any edit operation.
    pub fn invalidate_line_cache_from(&mut self, byte_offset: usize) {
        self.line_cache.invalidate_from(byte_offset);
    }

    /// Handle an insertion in the line cache.
    /// Call this after inserting text to update cached line info.
    pub fn handle_line_cache_insertion(
        &mut self,
        insert_byte: usize,
        inserted_bytes: usize,
        inserted_newlines: usize,
    ) {
        self.line_cache
            .handle_insertion(insert_byte, inserted_bytes, inserted_newlines);
    }

    /// Handle a deletion in the line cache.
    /// Call this after deleting text to update cached line info.
    pub fn handle_line_cache_deletion(
        &mut self,
        delete_start: usize,
        deleted_bytes: usize,
        deleted_newlines: usize,
    ) {
        self.line_cache
            .handle_deletion(delete_start, deleted_bytes, deleted_newlines);
    }

    /// Clear the entire line cache (useful when reloading a file).
    pub fn clear_line_cache(&mut self) {
        self.line_cache.clear();
    }

    /// Convert byte position to (line, character) - 0-indexed
    /// Returns (line_number, character_offset_within_line)
    /// Note: character is in BYTES, not UTF-16 code units (use position_to_lsp_position for LSP)
    pub fn position_to_line_col(&self, byte_pos: usize) -> (usize, usize) {
        let mut iter = self.line_iterator(0);
        let mut line_number = 0;

        while let Some((line_start, line_content)) = iter.next() {
            let line_end = line_start + line_content.len();

            if byte_pos >= line_start && byte_pos <= line_end {
                // Found the line containing byte_pos
                let character = byte_pos - line_start;
                return (line_number, character);
            }

            line_number += 1;
        }

        // If position is beyond the end, return the last line
        if line_number > 0 {
            line_number -= 1;
        }
        (line_number, 0)
    }

    /// Convert byte position to LSP position (line, UTF-16 code units) - 0-indexed
    /// LSP uses UTF-16 code units for character offsets, not bytes
    /// Returns (line_number, utf16_code_unit_offset)
    ///
    /// This uses the line cache for O(log n + k) performance where k is the distance
    /// from the nearest cached line, instead of O(n) from the start of the file.
    pub fn position_to_lsp_position(&self, byte_pos: usize) -> (usize, usize) {
        // Find the nearest cached line at or before byte_pos for fast lookup
        let (start_byte, start_line) = if let Some(info) = self.line_cache.get_nearest_before(byte_pos) {
            (info.byte_offset, info.line_number)
        } else {
            // No cache entry, start from beginning (rare case)
            (0, 0)
        };

        // Start iterating from the nearest cached position instead of 0
        let mut iter = self.line_iterator(start_byte);
        let mut line_number = start_line;
        let mut last_line_info: Option<(usize, String)> = None;

        while let Some((line_start, line_content)) = iter.next() {
            let line_end = line_start + line_content.len();

            // Position belongs to this line if it's within [line_start, line_end)
            if byte_pos >= line_start && byte_pos < line_end {
                // Found the line containing byte_pos
                let byte_offset = byte_pos - line_start;

                // Convert byte offset to UTF-16 code units
                // Take the substring from start of line to our byte position
                let text_before = &line_content[..byte_offset.min(line_content.len())];

                // Count UTF-16 code units
                let utf16_offset = text_before.encode_utf16().count();

                return (line_number, utf16_offset);
            }

            last_line_info = Some((line_start, line_content));
            line_number += 1;
        }

        // Handle EOF case: if byte_pos is exactly at the end of the buffer,
        // return position at the end of the last line
        if let Some((line_start, line_content)) = last_line_info {
            let line_end = line_start + line_content.len();
            if byte_pos == line_end {
                // Position is exactly at EOF - return end of last line
                let utf16_offset = line_content.encode_utf16().count();
                return (line_number - 1, utf16_offset);
            }
        }

        // Position is beyond EOF - return start of last line (or 0,0 if no lines)
        if line_number > 0 {
            line_number -= 1;
        }
        (line_number, 0)
    }

    /// Convert (line, character) to byte position - 0-indexed
    /// Returns byte position (clamps to end of buffer if line doesn't exist)
    /// Note: character is in BYTES, not UTF-16 code units (use lsp_position_to_byte for LSP)
    pub fn line_col_to_position(&self, line: usize, character: usize) -> usize {
        let mut iter = self.line_iterator(0);
        let mut current_line = 0;

        while current_line < line {
            if iter.next().is_none() {
                // Line doesn't exist, return end of buffer
                return self.len();
            }
            current_line += 1;
        }

        // Get the start of the target line
        if let Some((line_start, line_content)) = iter.next() {
            let byte_offset = character.min(line_content.len());
            line_start + byte_offset
        } else {
            // Line doesn't exist, return end of buffer
            self.len()
        }
    }

    /// Convert LSP position (line, UTF-16 code units) to byte position - 0-indexed
    /// LSP uses UTF-16 code units for character offsets, not bytes
    /// Returns byte position (clamps to end of buffer/line if out of bounds)
    pub fn lsp_position_to_byte(&self, line: usize, utf16_offset: usize) -> usize {
        let mut iter = self.line_iterator(0);
        let mut current_line = 0;

        while current_line < line {
            if iter.next().is_none() {
                // Line doesn't exist, return end of buffer
                return self.len();
            }
            current_line += 1;
        }

        // Get the start of the target line
        if let Some((line_start, line_content)) = iter.next() {
            // Convert UTF-16 offset to byte offset
            // We need to count UTF-16 code units until we reach the target offset
            let mut current_utf16 = 0;
            let mut byte_offset = 0;

            for ch in line_content.chars() {
                if current_utf16 >= utf16_offset {
                    break;
                }
                // Each char can be 1 or 2 UTF-16 code units
                current_utf16 += ch.len_utf16();
                byte_offset += ch.len_utf8();
            }

            // Clamp to line length
            line_start + byte_offset.min(line_content.len())
        } else {
            // Line doesn't exist, return end of buffer
            self.len()
        }
    }
}

impl Default for Buffer {
    fn default() -> Self {
        Self::new()
    }
}

/// Bidirectional cursor-based line iterator.
///
/// Semantics: `next()` reads forward and advances cursor, `prev()` reads backward and retreats cursor.
/// Unlike `DoubleEndedIterator`, calling `next()` then `prev()` returns the same line twice.
pub struct LineIterator {
    byte_iter: crate::virtual_buffer::ByteIterator,
}

impl LineIterator {
    /// Create a line iterator at any byte position.
    /// Automatically positions cursor at the start of the line containing byte_pos.
    pub fn new(vbuf: &VirtualBuffer, byte_pos: usize) -> Self {
        let mut byte_iter = vbuf.iter_at(byte_pos.min(vbuf.len()));

        // Scan backward to find line start (newline or position 0)
        while byte_iter.position() > 0 {
            byte_iter.prev();
            if let Some(b'\n') = byte_iter.peek() {
                byte_iter.next(); // Move past newline to line start
                break;
            }
        }

        Self { byte_iter }
    }

    /// Read next line forward and advance cursor.
    /// Returns (line_start_byte, line_content). Line content includes newline if present.
    pub fn next(&mut self) -> Option<(usize, String)> {
        let line_start = self.byte_iter.position();
        let buffer_len = self.byte_iter.buffer_len();

        if line_start >= buffer_len {
            return None;
        }

        let mut content = Vec::new();

        // Read until newline or EOF
        loop {
            match self.byte_iter.next() {
                Some(b'\n') => {
                    content.push(b'\n');
                    break;
                }
                Some(byte) => {
                    content.push(byte);
                }
                None => break,
            }
        }

        Some((line_start, String::from_utf8_lossy(&content).to_string()))
    }

    /// Read previous line backward and retreat cursor.
    /// Returns (line_start_byte, line_content). Line content includes newline if present.
    pub fn prev(&mut self) -> Option<(usize, String)> {
        let current_pos = self.byte_iter.position();

        if current_pos == 0 {
            return None;
        }

        // Move back one position to get to the previous line's last character (or newline)
        self.byte_iter.seek(current_pos.saturating_sub(1));

        // Scan backward to find start of the previous line
        while self.byte_iter.position() > 0 {
            self.byte_iter.prev();
            if let Some(b'\n') = self.byte_iter.peek() {
                self.byte_iter.next(); // Move past newline to line start
                break;
            }
        }

        // Read forward to get line content
        let line_start = self.byte_iter.position();
        let mut content = Vec::new();

        loop {
            match self.byte_iter.next() {
                Some(b'\n') => {
                    content.push(b'\n');
                    break;
                }
                Some(byte) => {
                    content.push(byte);
                }
                None => break,
            }
        }

        // Reset to line start for next operation
        self.byte_iter.seek(line_start);

        Some((line_start, String::from_utf8_lossy(&content).to_string()))
    }

    /// Get the current byte position
    pub fn current_position(&self) -> usize {
        self.byte_iter.position()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_buffer_basic() {
        let buffer = Buffer::from_str("hello world");
        assert_eq!(buffer.len(), 11);
        assert_eq!(buffer.to_string(), "hello world");
    }

    #[test]
    fn test_buffer_insert() {
        let mut buffer = Buffer::from_str("hello world");
        buffer.insert(5, " beautiful");
        assert_eq!(buffer.to_string(), "hello beautiful world");
        assert!(buffer.is_modified());
    }

    #[test]
    fn test_buffer_delete() {
        let mut buffer = Buffer::from_str("hello world");
        buffer.delete(5..11);
        assert_eq!(buffer.to_string(), "hello");
    }

    #[test]
    fn test_line_iterator() {
        let buffer = Buffer::from_str("line1\nline2\nline3");
        let mut iter = buffer.line_iterator(0);

        let (start, content) = iter.next().unwrap();
        assert_eq!(start, 0);
        assert_eq!(content, "line1\n");

        let (start, content) = iter.next().unwrap();
        assert_eq!(start, 6);
        assert_eq!(content, "line2\n");

        let (start, content) = iter.next().unwrap();
        assert_eq!(start, 12);
        assert_eq!(content, "line3");

        assert!(iter.next().is_none());
    }

    #[test]
    fn test_line_iterator_from_middle() {
        let buffer = Buffer::from_str("line1\nline2\nline3");
        let mut iter = buffer.line_iterator(8); // Middle of "line2"

        // Should start from beginning of line2
        let (start, content) = iter.next().unwrap();
        assert_eq!(start, 6);
        assert_eq!(content, "line2\n");
    }

    #[test]
    fn test_buffer_slice() {
        let buffer = Buffer::from_str("hello world");
        assert_eq!(buffer.slice(0..5), "hello");
        assert_eq!(buffer.slice(6..11), "world");
    }

    #[test]
    fn test_buffer_find_next() {
        let buffer = Buffer::from_str("hello world hello");
        assert_eq!(buffer.find_next("hello", 0), Some(0));
        assert_eq!(buffer.find_next("hello", 1), Some(12));
        assert_eq!(buffer.find_next("hello", 13), Some(0)); // Wraps around
        assert_eq!(buffer.find_next("xyz", 0), None);
    }

    #[test]
    fn test_lsp_position_to_byte_rust_rename_scenario() {
        // Reproduce the exact scenario from the bug report:
        // fn main() {
        //     let log_line = "hello world";
        //     println!("{}", log_line);
        //     let result = log_line.len();
        // }
        let code = "fn main() {\n    let log_line = \"hello world\";\n    println!(\"{}\", log_line);\n    let result = log_line.len();\n}\n";
        let buffer = Buffer::from_str(code);

        // Test the 3 edits from rust-analyzer (as shown in VSCode logs):

        // Edit 1: line 1, character 8-16 (the declaration)
        let start_pos = buffer.lsp_position_to_byte(1, 8);
        let end_pos = buffer.lsp_position_to_byte(1, 16);
        let text = buffer.slice(start_pos..end_pos);
        assert_eq!(
            text, "log_line",
            "Edit 1: declaration should be 'log_line' at line 1, chars 8-16"
        );

        // Edit 2: line 2, character 19-27 (the println! argument)
        let start_pos = buffer.lsp_position_to_byte(2, 19);
        let end_pos = buffer.lsp_position_to_byte(2, 27);
        let text = buffer.slice(start_pos..end_pos);
        assert_eq!(
            text, "log_line",
            "Edit 2: println! arg should be 'log_line' at line 2, chars 19-27"
        );

        // Edit 3: line 3, character 17-25 (the .len() call)
        let start_pos = buffer.lsp_position_to_byte(3, 17);
        let end_pos = buffer.lsp_position_to_byte(3, 25);
        let text = buffer.slice(start_pos..end_pos);
        assert_eq!(
            text, "log_line",
            "Edit 3: .len() call should be 'log_line' at line 3, chars 17-25"
        );
    }

    #[test]
    fn test_buffer_find_next_regex() {
        let buffer = Buffer::from_str("hello world hello");

        // Simple literal pattern
        let regex = Regex::new("hello").unwrap();
        assert_eq!(buffer.find_next_regex(&regex, 0), Some(0));
        assert_eq!(buffer.find_next_regex(&regex, 1), Some(12));
        assert_eq!(buffer.find_next_regex(&regex, 13), Some(0)); // Wraps around

        // Pattern with \s+
        let regex = Regex::new(r"hello\s+world").unwrap();
        assert_eq!(buffer.find_next_regex(&regex, 0), Some(0));

        // No match
        let regex = Regex::new("xyz").unwrap();
        assert_eq!(buffer.find_next_regex(&regex, 0), None);
    }

    #[test]
    fn test_buffer_find_regex_across_chunks() {
        // Create a buffer larger than chunk size to test cross-chunk matching
        let content = "x".repeat(5000) + "hello world" + &"y".repeat(5000);
        let buffer = Buffer::from_str(&content);

        let regex = Regex::new(r"hello\s+world").unwrap();
        let match_pos = buffer.find_next_regex(&regex, 0);
        assert!(match_pos.is_some());
        assert_eq!(match_pos.unwrap(), 5000);
    }

    #[test]
    fn test_buffer_find_regex_unicode() {
        let buffer = Buffer::from_str("hello 世界 world");

        // Unicode character class
        let regex = Regex::new(r"\p{Han}+").unwrap();
        let match_pos = buffer.find_next_regex(&regex, 0);
        assert!(match_pos.is_some());

        // The position should be after "hello "
        let pos = match_pos.unwrap();
        assert_eq!(&buffer.slice(pos..pos + 6), "世界");
    }

    #[test]
    fn test_line_iterator_next_then_prev() {
        // Correct semantics for cursor-based bidirectional iterator:
        // If items are [a, b, c] and cursor is between a and b:
        // - next() returns b, cursor moves between b and c
        // - prev() returns b again, cursor moves back between a and b
        //
        // This is like a bidirectional cursor where:
        // - next() reads forward and advances
        // - prev() reads backward and retreats

        let buffer = Buffer::from_str("Line 1\nLine 2\nLine 3");
        let mut iter = buffer.line_iterator(10); // Middle of Line 2

        // Cursor is at Line 2
        // next() should return Line 2 and advance past it
        let (line_start, line_content) = iter.next().unwrap();
        assert_eq!(line_start, 7);
        assert_eq!(line_content, "Line 2\n");

        // Cursor is now after Line 2 (before Line 3)
        // prev() should return Line 2 again and move cursor back before Line 2
        let (prev_line_start, prev_line_content) = iter.prev().unwrap();
        assert_eq!(prev_line_start, 7, "prev() should return same Line 2");
        assert_eq!(prev_line_content, "Line 2\n");

        // Cursor is now before Line 2
        // prev() again should return Line 1
        let (prev_line_start, prev_line_content) = iter.prev().unwrap();
        assert_eq!(prev_line_start, 0);
        assert_eq!(prev_line_content, "Line 1\n");

        // Test 2: From last line (no trailing newline) - this was the failing case!
        let mut iter = buffer.line_iterator(20);
        let (line_start, line_content) = iter.next().unwrap();
        assert_eq!(line_start, 14);
        assert_eq!(line_content, "Line 3");

        // Cursor should be at EOF (position 20)
        // prev() should return Line 3 again
        let (prev_line_start, prev_line_content) = iter.prev().unwrap();
        assert_eq!(prev_line_start, 14, "prev() should return Line 3 again");
        assert_eq!(prev_line_content, "Line 3");

        // prev() again should return Line 2
        let (prev_line_start, prev_line_content) = iter.prev().unwrap();
        assert_eq!(prev_line_start, 7, "second prev() should return Line 2");
        assert_eq!(prev_line_content, "Line 2\n");
    }

    // ============================================================================
    // Property-based tests for streaming search
    // ============================================================================

    mod proptests {
        use super::*;
        use proptest::prelude::*;

        proptest! {
            /// Property: Streaming search finds same results as naive string search
            #[test]
            fn prop_find_next_matches_naive(
                prefix in "[a-zA-Z0-9 ]{0,200}",
                pattern in "[a-z]{3,15}",
                suffix in "[a-zA-Z0-9 ]{0,200}",
            ) {
                let content = format!("{}{}{}", prefix, pattern, suffix);
                let buffer = Buffer::from_str(&content);

                // Naive search
                let naive_result = content.find(&pattern);

                // Streaming search
                let streaming_result = buffer.find_next(&pattern, 0);

                prop_assert_eq!(streaming_result, naive_result,
                    "Streaming search should match naive search for pattern '{}'", pattern);
            }

            /// Property: Multiple searches find all occurrences in order
            #[test]
            fn prop_find_next_finds_all_occurrences(
                pattern in "[a-z]{3,8}",
                separator in "[0-9 ]{3,10}",
                repetitions in 2usize..8,
            ) {
                // Create content with known pattern positions
                let parts: Vec<String> = (0..repetitions)
                    .map(|_| pattern.clone())
                    .collect();
                let content = parts.join(&separator);
                let buffer = Buffer::from_str(&content);

                // Collect all matches with naive search
                let mut naive_positions = Vec::new();
                let mut pos = 0;
                while let Some(found_at) = content[pos..].find(&pattern) {
                    let absolute_pos = pos + found_at;
                    naive_positions.push(absolute_pos);
                    pos = absolute_pos + 1;
                }

                // Collect all matches with streaming search
                let mut streaming_positions = Vec::new();
                let mut search_from = 0;
                while let Some(found_at) = buffer.find_next(&pattern, search_from) {
                    // If found position is before search start, we've wrapped around - stop
                    if found_at < search_from {
                        break;
                    }
                    streaming_positions.push(found_at);
                    search_from = found_at + 1;
                }

                prop_assert_eq!(streaming_positions, naive_positions,
                    "Should find all {} occurrences at same positions", repetitions);
            }

            /// Property: Search works with patterns at various positions
            #[test]
            fn prop_find_next_at_any_position(
                before in "[a-z]{0,100}",
                pattern in "[a-z]{5,20}",
                after in "[a-z]{0,100}",
                start_offset in 0usize..50,
            ) {
                let content = format!("{}{}{}", before, pattern, after);
                let buffer = Buffer::from_str(&content);

                let start_pos = start_offset.min(content.len());

                // find_next does wrap-around search, so we need to match that behavior
                let expected_pos = if let Some(offset) = content[start_pos..].find(&pattern) {
                    // Found from start_pos to end
                    Some(start_pos + offset)
                } else if start_pos > 0 {
                    // Wrap around: search from beginning to start_pos
                    content[..start_pos].find(&pattern)
                } else {
                    None
                };

                let result = buffer.find_next(&pattern, start_pos);
                prop_assert_eq!(result, expected_pos,
                    "Streaming search should match naive wrap-around search from start={}", start_pos);
            }

            /// Property: Empty and single-character patterns
            #[test]
            fn prop_find_edge_cases(
                content in "[a-z]{10,100}",
            ) {
                let buffer = Buffer::from_str(&content);

                // Empty pattern should return None
                prop_assert_eq!(buffer.find_next("", 0), None, "Empty pattern should not match");

                // Single character should work
                if !content.is_empty() {
                    let first_char = &content[0..1];
                    let result = buffer.find_next(first_char, 0);
                    prop_assert_eq!(result, Some(0), "First character should be found at position 0");
                }
            }

            /// Property: Wrap-around search works correctly
            #[test]
            fn prop_find_next_wraps_around(
                before_pattern in "[a-z]{20,50}",
                pattern in "[a-z]{5,10}",
                after_pattern in "[a-z]{20,50}",
            ) {
                let content = format!("{}{}{}{}", pattern, before_pattern, pattern, after_pattern);
                let buffer = Buffer::from_str(&content);

                // Start search after first occurrence
                let start_pos = pattern.len() + 1;

                // Should find second occurrence
                let second_occurrence = pattern.len() + before_pattern.len();
                let result = buffer.find_next(&pattern, start_pos);
                prop_assert_eq!(result, Some(second_occurrence),
                    "Should find second occurrence at {}", second_occurrence);

                // Start search after both occurrences - should wrap to first
                let start_after_both = content.len() - 1;
                let wrapped_result = buffer.find_next(&pattern, start_after_both);
                prop_assert_eq!(wrapped_result, Some(0),
                    "Should wrap around and find first occurrence");
            }

            /// Property: Regex search matches standard regex behavior
            #[test]
            fn prop_regex_matches_standard(
                prefix in "[a-z]{0,100}",
                digits in "[0-9]{3,8}",
                suffix in "[a-z]{0,100}",
            ) {
                let content = format!("{}{}{}", prefix, digits, suffix);
                let buffer = Buffer::from_str(&content);

                let regex = Regex::new(r"\d+").unwrap();

                // Standard regex search
                let standard_result = regex.find(content.as_bytes()).map(|m| m.start());

                // Streaming regex search
                let streaming_result = buffer.find_next_regex(&regex, 0);

                prop_assert_eq!(streaming_result, standard_result,
                    "Streaming regex should match standard regex");
            }

            /// Property: Regex finds patterns spanning chunks
            #[test]
            fn prop_regex_finds_across_chunks(
                pattern_text in "[a-z]{10,30}",
                chunk_boundary in 5usize..25,
            ) {
                // Create content where pattern will span chunk boundary
                // (assuming CHUNK_SIZE of 64KB in find_regex_streaming)
                let prefix = "x".repeat(chunk_boundary);
                let content = format!("{}{}", prefix, pattern_text);
                let buffer = Buffer::from_str(&content);

                // Create regex that matches our pattern
                let regex_str = regex::escape(&pattern_text);
                let regex = Regex::new(&regex_str).unwrap();

                let result = buffer.find_next_regex(&regex, 0);
                prop_assert_eq!(result, Some(chunk_boundary),
                    "Should find pattern at position {}", chunk_boundary);
            }

            /// Property: Regex with Unicode works correctly
            #[test]
            fn prop_regex_unicode(
                prefix in "[a-z]{0,50}",
                unicode_chars in "[\\u{4E00}-\\u{9FFF}]{2,5}", // Chinese characters
                suffix in "[a-z]{0,50}",
            ) {
                let content = format!("{}{}{}", prefix, unicode_chars, suffix);
                let buffer = Buffer::from_str(&content);

                // Match any Han character
                let regex = Regex::new(r"\p{Han}+").unwrap();

                let result = buffer.find_next_regex(&regex, 0);
                prop_assert!(result.is_some(), "Should find Unicode characters");

                if let Some(pos) = result {
                    prop_assert_eq!(pos, prefix.len(),
                        "Should find Unicode at correct position");
                }
            }

            /// Property: Large buffer search works without materialization
            #[test]
            fn prop_large_buffer_search(
                pattern in "[a-z]{5,10}",
                position_in_kb in 1usize..50,
            ) {
                // Create a large buffer (up to ~50KB)
                let position_bytes = position_in_kb * 1024;
                let prefix = "a".repeat(position_bytes);
                let suffix = "b".repeat(5000);
                let content = format!("{}{}{}", prefix, pattern, suffix);
                let buffer = Buffer::from_str(&content);

                let result = buffer.find_next(&pattern, 0);
                prop_assert_eq!(result, Some(position_bytes),
                    "Should find pattern in large buffer at position {}", position_bytes);
            }

            // ============================================================================
            // Property-based tests for replace
            // ============================================================================

            /// Property: replace_all matches naive replace for simple patterns
            #[test]
            fn prop_replace_all_matches_naive(
                prefix in "[a-z]{0,50}",
                pattern in "[a-z]{3,10}",
                suffix in "[a-z]{0,50}",
                replacement in "[A-Z]{3,10}",
            ) {
                let content = format!("{}{}{}", prefix, pattern, suffix);
                let mut buffer = Buffer::from_str(&content);

                // Naive replace using standard library
                let naive_result = content.replace(&pattern, &replacement);

                // Buffer replace_all
                let count = buffer.replace_all(&pattern, &replacement);
                let buffer_result = buffer.to_string();

                prop_assert_eq!(buffer_result, naive_result,
                    "replace_all should match naive replace");

                // Count should match number of occurrences
                let expected_count = content.matches(&pattern).count();
                prop_assert_eq!(count, expected_count,
                    "Should report correct replacement count");
            }

            /// Property: replace_all finds all occurrences
            #[test]
            fn prop_replace_all_finds_all_occurrences(
                pattern in "[a-z]{3,8}",
                separator in "[0-9]{2,5}",
                repetitions in 2usize..10,
                replacement in "[A-Z]{2,6}",
            ) {
                // Create content with multiple occurrences
                let parts: Vec<String> = (0..repetitions)
                    .map(|_| pattern.clone())
                    .collect();
                let content = parts.join(&separator);
                let mut buffer = Buffer::from_str(&content);

                let count = buffer.replace_all(&pattern, &replacement);
                let result = buffer.to_string();

                // Should have replaced all occurrences
                prop_assert!(!result.contains(&pattern),
                    "All occurrences of '{}' should be replaced", pattern);

                prop_assert_eq!(count, repetitions,
                    "Should replace all {} occurrences", repetitions);

                // Count replacement in result
                let replacement_count = result.matches(&replacement).count();
                prop_assert_eq!(replacement_count, repetitions,
                    "Result should contain {} instances of replacement", repetitions);
            }

            /// Property: replace_all handles size changes correctly
            #[test]
            fn prop_replace_all_size_changes(
                pattern in "[a-z]{2,5}",
                replacement_size_diff in -3isize..6,
                occurrences in 1usize..8,
            ) {
                // Create replacement string with different size
                let replacement_len = (pattern.len() as isize + replacement_size_diff).max(0) as usize;
                let replacement = "X".repeat(replacement_len);

                // Create content with known occurrences
                let content = vec![pattern.clone(); occurrences].join(" ");
                let mut buffer = Buffer::from_str(&content);

                let original_len = content.len();
                let count = buffer.replace_all(&pattern, &replacement);
                let result_len = buffer.len();

                // Check count
                prop_assert_eq!(count, occurrences,
                    "Should replace all {} occurrences", occurrences);

                // Check size calculation - actual size change per replacement
                let actual_size_diff = replacement.len() as isize - pattern.len() as isize;
                let expected_len = (original_len as isize
                    + (occurrences as isize * actual_size_diff)).max(0) as usize;
                prop_assert_eq!(result_len, expected_len,
                    "Buffer length should change correctly");
            }

            /// Property: replace_range preserves surrounding content
            #[test]
            fn prop_replace_range_preserves_surroundings(
                prefix in "[a-z]{10,50}",
                middle in "[a-z]{5,20}",
                suffix in "[a-z]{10,50}",
                replacement in "[A-Z]{5,25}",
            ) {
                let content = format!("{}{}{}", prefix, middle, suffix);
                let mut buffer = Buffer::from_str(&content);

                let start = prefix.len();
                let end = start + middle.len();

                let success = buffer.replace_range(start..end, &replacement);
                prop_assert!(success, "replace_range should succeed");

                let result = buffer.to_string();
                let expected = format!("{}{}{}", prefix, replacement, suffix);

                prop_assert_eq!(result, expected,
                    "Should replace only the middle section");
            }

            /// Property: replace_next finds and replaces first occurrence
            #[test]
            fn prop_replace_next_finds_first(
                prefix in "[a-z]{0,30}",
                pattern in "[a-z]{4,10}",
                middle in "[0-9]{5,15}",
                suffix in "[a-z]{0,30}",
                replacement in "[A-Z]{3,8}",
            ) {
                // Ensure pattern appears at least once
                let content = format!("{}{}{}{}", prefix, pattern, middle, suffix);
                let mut buffer = Buffer::from_str(&content);

                let result = buffer.replace_next(&pattern, &replacement, 0);

                if content.contains(&pattern) {
                    prop_assert!(result.is_some(), "Should find pattern");

                    let pos = result.unwrap();
                    let buffer_content = buffer.to_string();

                    // Should contain replacement
                    prop_assert!(buffer_content.contains(&replacement),
                        "Result should contain replacement");

                    // First occurrence should be replaced
                    let expected_pos = content.find(&pattern).unwrap();
                    prop_assert_eq!(pos, expected_pos,
                        "Should replace at first occurrence position");
                }
            }

            /// Property: replace_all with overlapping patterns
            #[test]
            fn prop_replace_all_overlapping_patterns(
                repetitions in 3usize..10,
                replacement in "[A-Z]{1,3}",
            ) {
                // Create overlapping pattern like "aaa" with base char 'a'
                let base_char = 'a';
                let pattern = base_char.to_string().repeat(2);
                let content = base_char.to_string().repeat(repetitions);
                let mut buffer = Buffer::from_str(&content);

                let count = buffer.replace_all(&pattern, &replacement);
                let result = buffer.to_string();

                // With overlapping patterns, should match standard library behavior
                let naive_result = content.replace(&pattern, &replacement);
                prop_assert_eq!(result, naive_result,
                    "Should handle overlapping patterns like standard library");

                // Standard library does non-overlapping replacement
                let expected_count = repetitions / 2;
                prop_assert_eq!(count, expected_count,
                    "Should replace {} non-overlapping occurrences", expected_count);
            }

            /// Property: replace_all empty pattern should not modify buffer
            #[test]
            fn prop_replace_all_empty_pattern_noop(
                content in "[a-zA-Z0-9 ]{10,100}",
                replacement in "[A-Z]{5,15}",
            ) {
                let mut buffer = Buffer::from_str(&content);
                let original = buffer.to_string();

                let count = buffer.replace_all("", &replacement);
                let result = buffer.to_string();

                prop_assert_eq!(count, 0, "Should not replace anything");
                prop_assert_eq!(result, original, "Buffer should be unchanged");
            }

            /// Property: replace_all with pattern not found should not modify buffer
            #[test]
            fn prop_replace_all_pattern_not_found_noop(
                content in "[a-z]{20,100}",
                replacement in "[A-Z]{5,15}",
            ) {
                // Use a pattern that definitely won't be in lowercase content
                let pattern = "XXXYYY";
                let mut buffer = Buffer::from_str(&content);
                let original = buffer.to_string();

                let count = buffer.replace_all(pattern, &replacement);
                let result = buffer.to_string();

                prop_assert_eq!(count, 0, "Should not replace anything");
                prop_assert_eq!(result, original, "Buffer should be unchanged");
            }

            /// Property: Multiple replace_next calls replace all occurrences
            #[test]
            fn prop_replace_next_multiple_replaces_all(
                pattern in "[a-z]{3,6}",
                separator in "[0-9]{3,8}",
                occurrences in 2usize..6,
                replacement in "[A-Z]{3,6}",
            ) {
                // Create content with known number of occurrences
                let parts: Vec<String> = (0..occurrences)
                    .map(|_| pattern.clone())
                    .collect();
                let content = parts.join(&separator);
                let mut buffer = Buffer::from_str(&content);

                let mut replaced_count = 0;
                let mut search_pos = 0;

                // Keep replacing until no more matches
                while let Some(_pos) = buffer.replace_next(&pattern, &replacement, search_pos) {
                    replaced_count += 1;
                    search_pos = 0; // Start from beginning each time

                    // Safety: limit iterations
                    if replaced_count > occurrences * 2 {
                        break;
                    }
                }

                prop_assert_eq!(replaced_count, occurrences,
                    "Should replace all {} occurrences with multiple calls", occurrences);

                let result = buffer.to_string();
                prop_assert!(!result.contains(&pattern),
                    "All occurrences should be replaced");
            }

            /// Property: replace_all with large buffer works correctly
            #[test]
            fn prop_replace_all_large_buffer(
                pattern in "[a-z]{4,8}",
                lines in 100usize..500,
                replacement in "[A-Z]{3,7}",
            ) {
                // Create larger buffer with pattern on every line
                let content = (0..lines)
                    .map(|_| format!("prefix {} suffix\n", pattern))
                    .collect::<String>();
                let mut buffer = Buffer::from_str(&content);

                let count = buffer.replace_all(&pattern, &replacement);

                prop_assert_eq!(count, lines,
                    "Should replace pattern on all {} lines", lines);

                let result = buffer.to_string();
                prop_assert!(!result.contains(&pattern),
                    "Large buffer should have all patterns replaced");
            }
        }
    }

    // ============================================================================
    // Replace tests
    // ============================================================================

    #[test]
    fn test_replace_range() {
        let mut buffer = Buffer::from_str("hello world");
        assert!(buffer.replace_range(6..11, "rust"));
        assert_eq!(buffer.to_string(), "hello rust");
        assert!(buffer.is_modified());
    }

    #[test]
    fn test_replace_range_empty() {
        let mut buffer = Buffer::from_str("hello world");
        assert!(buffer.replace_range(5..5, " beautiful"));
        assert_eq!(buffer.to_string(), "hello beautiful world");
    }

    #[test]
    fn test_replace_range_with_empty() {
        let mut buffer = Buffer::from_str("hello world");
        assert!(buffer.replace_range(5..11, ""));
        assert_eq!(buffer.to_string(), "hello");
    }

    #[test]
    fn test_replace_range_invalid() {
        let mut buffer = Buffer::from_str("hello");
        assert!(!buffer.replace_range(0..100, "text")); // End out of bounds
        assert!(!buffer.replace_range(10..15, "text")); // Start out of bounds
        assert!(!buffer.replace_range(5..3, "text")); // Start > end
    }

    #[test]
    fn test_replace_next() {
        let mut buffer = Buffer::from_str("hello world hello");
        assert_eq!(buffer.replace_next("hello", "hi", 0), Some(0));
        assert_eq!(buffer.to_string(), "hi world hello");

        assert_eq!(buffer.replace_next("hello", "hi", 0), Some(9));
        assert_eq!(buffer.to_string(), "hi world hi");
    }

    #[test]
    fn test_replace_next_not_found() {
        let mut buffer = Buffer::from_str("hello world");
        assert_eq!(buffer.replace_next("xyz", "abc", 0), None);
        assert_eq!(buffer.to_string(), "hello world");
    }

    #[test]
    fn test_replace_all_simple() {
        let mut buffer = Buffer::from_str("hello world hello");
        let count = buffer.replace_all("hello", "hi");
        assert_eq!(count, 2);
        assert_eq!(buffer.to_string(), "hi world hi");
    }

    #[test]
    fn test_replace_all_overlapping() {
        let mut buffer = Buffer::from_str("aaaa");
        let count = buffer.replace_all("aa", "b");
        assert_eq!(count, 2);
        assert_eq!(buffer.to_string(), "bb");
    }

    #[test]
    fn test_replace_all_empty_pattern() {
        let mut buffer = Buffer::from_str("hello");
        let count = buffer.replace_all("", "x");
        assert_eq!(count, 0);
        assert_eq!(buffer.to_string(), "hello");
    }

    #[test]
    fn test_replace_all_no_matches() {
        let mut buffer = Buffer::from_str("hello world");
        let count = buffer.replace_all("xyz", "abc");
        assert_eq!(count, 0);
        assert_eq!(buffer.to_string(), "hello world");
    }

    #[test]
    fn test_replace_all_size_change() {
        let mut buffer = Buffer::from_str("a b c d e");
        let count = buffer.replace_all(" ", "---");
        assert_eq!(count, 4);
        assert_eq!(buffer.to_string(), "a---b---c---d---e");
    }

    #[test]
    fn test_replace_all_multiline() {
        let mut buffer = Buffer::from_str("line1\nline2\nline3");
        let count = buffer.replace_all("line", "LINE");
        assert_eq!(count, 3);
        assert_eq!(buffer.to_string(), "LINE1\nLINE2\nLINE3");
    }

    #[test]
    fn test_replace_all_regex_simple() {
        use regex::Regex;
        let mut buffer = Buffer::from_str("hello world hello");
        let regex = Regex::new(r"hello").unwrap();
        let count = buffer.replace_all_regex(&regex, "hi");
        assert_eq!(count, 2);
        assert_eq!(buffer.to_string(), "hi world hi");
    }

    #[test]
    fn test_replace_all_regex_with_captures() {
        use regex::Regex;
        let mut buffer = Buffer::from_str("foo123 bar456 baz789");
        let regex = Regex::new(r"([a-z]+)(\d+)").unwrap();
        // Note: Use ${1} syntax when capture group is followed by non-whitespace
        let count = buffer.replace_all_regex(&regex, "${1}_${2}");
        assert_eq!(count, 3);
        assert_eq!(buffer.to_string(), "foo_123 bar_456 baz_789");
    }

    #[test]
    fn test_replace_all_regex_word_boundaries() {
        use regex::Regex;
        let mut buffer = Buffer::from_str("the theory is theoretical");
        let regex = Regex::new(r"\bthe\b").unwrap();
        let count = buffer.replace_all_regex(&regex, "a");
        assert_eq!(count, 1); // Only "the" at the beginning
        assert_eq!(buffer.to_string(), "a theory is theoretical");
    }

    #[test]
    fn test_replace_all_regex_no_matches() {
        use regex::Regex;
        let mut buffer = Buffer::from_str("hello world");
        let regex = Regex::new(r"\d+").unwrap();
        let count = buffer.replace_all_regex(&regex, "NUM");
        assert_eq!(count, 0);
        assert_eq!(buffer.to_string(), "hello world");
    }

    #[test]
    fn test_replace_all_large_file() {
        // Test with a larger buffer to ensure streaming works
        let content = "test\n".repeat(1000); // 5000 bytes
        let mut buffer = Buffer::from_str(&content);
        let count = buffer.replace_all("test", "TEST");
        assert_eq!(count, 1000);
        assert_eq!(buffer.to_string(), "TEST\n".repeat(1000));
    }
}
