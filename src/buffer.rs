use crate::chunk_tree::{ChunkTree, ChunkTreeConfig};
use crate::persistence::ChunkTreePersistence;
use crate::virtual_buffer::VirtualBuffer;
use std::io::{self, Read, Write};
use std::ops::Range;
use std::path::{Path, PathBuf};

/// Default configuration for ChunkTree
const DEFAULT_CONFIG: ChunkTreeConfig = ChunkTreeConfig::new(64, 128);

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
}

impl Buffer {
    /// Create a new empty buffer
    pub fn new() -> Self {
        let persistence = Box::new(ChunkTreePersistence::new(DEFAULT_CONFIG));
        Self {
            virtual_buffer: VirtualBuffer::new(persistence),
            file_path: None,
            modified: false,
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
    }

    /// Delete a range of bytes
    pub fn delete(&mut self, range: Range<usize>) {
        if range.is_empty() {
            return;
        }
        let _ = self.virtual_buffer.delete(range);
        self.modified = true;
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
    pub fn find_next(&self, pattern: &str, start_pos: usize) -> Option<usize> {
        if pattern.is_empty() {
            return None;
        }

        let text = self.to_string();
        let bytes = text.as_bytes();
        let pattern_bytes = pattern.as_bytes();

        // Search from start_pos to end
        if start_pos < bytes.len() {
            if let Some(offset) = Self::find_pattern(&bytes[start_pos..], pattern_bytes) {
                return Some(start_pos + offset);
            }
        }

        // Wrap around: search from beginning to start_pos
        if start_pos > 0 {
            if let Some(offset) = Self::find_pattern(&bytes[..start_pos], pattern_bytes) {
                return Some(offset);
            }
        }

        None
    }

    /// Helper: Find pattern in haystack using naive string search
    fn find_pattern(haystack: &[u8], needle: &[u8]) -> Option<usize> {
        if needle.is_empty() || haystack.len() < needle.len() {
            return None;
        }

        for i in 0..=(haystack.len() - needle.len()) {
            if &haystack[i..i + needle.len()] == needle {
                return Some(i);
            }
        }

        None
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

        // Step 1: Move back past newlines at current position
        self.byte_iter.seek(current_pos.saturating_sub(1));
        while self.byte_iter.position() > 0 {
            if let Some(b'\n') = self.byte_iter.peek() {
                if self.byte_iter.prev().is_none() {
                    break;
                }
            } else {
                break;
            }
        }

        // Step 2: Scan backward to find start of this line
        while self.byte_iter.position() > 0 {
            self.byte_iter.prev();
            if let Some(b'\n') = self.byte_iter.peek() {
                self.byte_iter.next(); // Move past newline
                break;
            }
        }

        // Step 3: Read forward to get line content
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
}
