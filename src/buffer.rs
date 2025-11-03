use crate::chunk_tree::{ChunkTree, ChunkTreeConfig};
use std::io::{self, Read, Write};
use std::ops::Range;
use std::path::{Path, PathBuf};

/// Default configuration for ChunkTree
const DEFAULT_CONFIG: ChunkTreeConfig = ChunkTreeConfig::new(64, 128);

/// A text buffer backed by ChunkTree with cached line boundaries
pub struct Buffer {
    /// The underlying text storage (rope structure)
    /// Uses 'static lifetime with leaked strings for simplicity
    content: ChunkTree<'static>,

    /// Cached line boundaries for fast line<->byte conversion
    line_cache: LineCache,

    /// Optional file path for persistence
    file_path: Option<PathBuf>,

    /// Has the buffer been modified since last save?
    modified: bool,
}

/// Cache of line start positions for fast lookups
struct LineCache {
    /// Byte offset where each line starts
    /// line_starts[0] = 0 (first line starts at byte 0)
    /// line_starts[1] = byte offset where line 1 starts
    line_starts: Vec<usize>,

    /// Is the cache currently valid?
    valid: bool,
}

impl LineCache {
    fn new() -> Self {
        Self {
            line_starts: vec![0],
            valid: true,
        }
    }

    fn invalidate(&mut self) {
        self.valid = false;
    }

    fn is_valid(&self) -> bool {
        self.valid
    }

    /// Rebuild the line cache from text
    fn rebuild(&mut self, text: &[u8]) {
        self.line_starts.clear();
        self.line_starts.push(0);

        for (i, &byte) in text.iter().enumerate() {
            if byte == b'\n' {
                self.line_starts.push(i + 1);
            }
        }

        self.valid = true;
    }

    /// Get the number of lines
    fn line_count(&self) -> usize {
        self.line_starts.len()
    }

    /// Get the byte offset where a line starts
    fn line_to_byte(&self, line: usize) -> Option<usize> {
        self.line_starts.get(line).copied()
    }

    /// Get the line number for a byte offset
    fn byte_to_line(&self, byte: usize) -> usize {
        match self.line_starts.binary_search(&byte) {
            Ok(line) => line,
            Err(line) => line.saturating_sub(1),
        }
    }
}

impl Buffer {
    /// Create a new empty buffer
    pub fn new() -> Self {
        Self {
            content: ChunkTree::new(DEFAULT_CONFIG),
            line_cache: LineCache::new(),
            file_path: None,
            modified: false,
        }
    }

    /// Create a buffer from a string
    pub fn from_str(s: &str) -> Self {
        // Leak the string to get 'static lifetime
        // This is a trade-off for simplicity - we won't reclaim this memory
        let leaked: &'static [u8] = Box::leak(s.as_bytes().to_vec().into_boxed_slice());
        let content = ChunkTree::from_slice(leaked, DEFAULT_CONFIG);

        let mut line_cache = LineCache::new();
        line_cache.rebuild(leaked);

        Self {
            content,
            line_cache,
            file_path: None,
            modified: false,
        }
    }

    /// Load a buffer from a file
    pub fn load_from_file<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let path = path.as_ref();
        let mut file = std::fs::File::open(path)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;

        let mut buffer = Self::from_str(&contents);
        buffer.file_path = Some(path.to_path_buf());
        buffer.modified = false;

        Ok(buffer)
    }

    /// Save the buffer to its file path
    pub fn save(&mut self) -> io::Result<()> {
        if let Some(path) = self.file_path.clone() {
            self.save_to_file(path)?;
            self.modified = false;
            Ok(())
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "No file path set for buffer",
            ))
        }
    }

    /// Save the buffer to a specific file
    pub fn save_to_file<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()> {
        let content = self.to_string();
        let mut file = std::fs::File::create(path.as_ref())?;
        file.write_all(content.as_bytes())?;

        self.file_path = Some(path.as_ref().to_path_buf());
        self.modified = false;

        Ok(())
    }

    /// Insert text at a byte position
    pub fn insert(&mut self, pos: usize, text: &str) {
        if text.is_empty() {
            return;
        }

        // Leak the text to get 'static lifetime
        let leaked: &'static [u8] = Box::leak(text.as_bytes().to_vec().into_boxed_slice());
        self.content = self.content.insert(pos, leaked);
        self.line_cache.invalidate();
        self.modified = true;
    }

    /// Delete a range of bytes
    pub fn delete(&mut self, range: Range<usize>) {
        if range.start >= range.end {
            return;
        }

        self.content = self.content.remove(range);
        self.line_cache.invalidate();
        self.modified = true;
    }

    /// Get a slice of text as a string
    pub fn slice(&self, range: Range<usize>) -> String {
        let bytes = self.slice_bytes(range);
        String::from_utf8_lossy(&bytes).to_string()
    }

    /// Get a slice of text as bytes
    pub fn slice_bytes(&self, range: Range<usize>) -> Vec<u8> {
        if range.start >= self.len() {
            return vec![];
        }

        let end = range.end.min(self.len());
        let mut result = Vec::new();

        for i in range.start..end {
            let piece = self.content.get(i);
            match piece {
                crate::chunk_tree::ChunkPiece::Data { data } => {
                    if !data.is_empty() {
                        result.push(data[0]);
                    }
                }
                crate::chunk_tree::ChunkPiece::Gap { .. } => {
                    // Gap - fill with space for now
                    result.push(b' ');
                }
            }
        }

        result
    }

    /// Get the entire buffer as a string
    pub fn to_string(&self) -> String {
        let bytes = self.content.collect_bytes(b' ');
        String::from_utf8_lossy(&bytes).to_string()
    }

    /// Get the total length in bytes
    pub fn len(&self) -> usize {
        self.content.len()
    }

    /// Is the buffer empty?
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Ensure the line cache is valid
    fn ensure_line_cache(&mut self) {
        if !self.line_cache.is_valid() {
            let bytes = self.content.collect_bytes(b' ');
            self.line_cache.rebuild(&bytes);
        }
    }

    /// Convert a line number to a byte offset
    pub fn line_to_byte(&mut self, line: usize) -> usize {
        self.ensure_line_cache();
        self.line_cache.line_to_byte(line).unwrap_or(self.len())
    }

    /// Convert a byte offset to a line number
    pub fn byte_to_line(&mut self, byte: usize) -> usize {
        self.ensure_line_cache();
        self.line_cache.byte_to_line(byte.min(self.len()))
    }

    /// Get the number of lines in the buffer
    pub fn line_count(&mut self) -> usize {
        self.ensure_line_cache();
        self.line_cache.line_count()
    }

    /// Get the content of a specific line
    pub fn line_content(&mut self, line: usize) -> String {
        self.ensure_line_cache();
        let start = self.line_cache.line_to_byte(line).unwrap_or(self.len());
        let end = self
            .line_cache
            .line_to_byte(line + 1)
            .unwrap_or(self.len());

        let mut content = self.slice(start..end);
        // Remove trailing newline if present
        if content.ends_with('\n') {
            content.pop();
        }
        content
    }

    /// Get multiple lines as strings
    pub fn lines_in_range(&mut self, start_line: usize, count: usize) -> Vec<String> {
        let mut lines = Vec::new();
        for line in start_line..(start_line + count) {
            if line >= self.line_count() {
                break;
            }
            lines.push(self.line_content(line));
        }
        lines
    }

    /// Find the previous UTF-8 character boundary before the given position
    pub fn prev_char_boundary(&self, pos: usize) -> usize {
        if pos == 0 {
            return 0;
        }

        let bytes = self.slice_bytes(pos.saturating_sub(4)..pos);

        for i in (0..bytes.len()).rev() {
            if (bytes[i] & 0b1100_0000) != 0b1000_0000 {
                return (pos.saturating_sub(4) + i).min(pos - 1);
            }
        }

        pos.saturating_sub(1)
    }

    /// Find the next UTF-8 character boundary after the given position
    pub fn next_char_boundary(&self, pos: usize) -> usize {
        let len = self.len();

        if pos >= len {
            return len;
        }

        let bytes = self.slice_bytes(pos..(pos + 4).min(len));

        for (i, &byte) in bytes.iter().enumerate() {
            if i > 0 && (byte & 0b1100_0000) != 0b1000_0000 {
                return pos + i;
            }
        }

        (pos + 1).min(len)
    }

    /// Find the start of the word before the given position
    pub fn prev_word_boundary(&self, pos: usize) -> usize {
        if pos == 0 {
            return 0;
        }

        let text = self.to_string();
        let chars: Vec<char> = text.chars().collect();

        // Convert byte pos to char pos
        let mut byte_count = 0;
        let mut char_pos = 0;
        for (i, c) in chars.iter().enumerate() {
            if byte_count >= pos {
                char_pos = i;
                break;
            }
            byte_count += c.len_utf8();
        }

        if char_pos == 0 {
            return 0;
        }

        // Skip whitespace backward
        let mut i = char_pos.saturating_sub(1);
        while i > 0 && chars[i].is_whitespace() {
            i -= 1;
        }

        // Skip word characters backward
        while i > 0 && !chars[i].is_whitespace() {
            i -= 1;
        }

        if i > 0 || chars.get(0).map_or(false, |c| c.is_whitespace()) {
            i += 1;
        }

        // Convert back to byte position
        chars[..i].iter().map(|c| c.len_utf8()).sum()
    }

    /// Find the end of the word after the given position
    pub fn next_word_boundary(&self, pos: usize) -> usize {
        let text = self.to_string();
        let chars: Vec<char> = text.chars().collect();

        // Convert byte pos to char pos
        let mut byte_count = 0;
        let mut char_pos = 0;
        for (i, c) in chars.iter().enumerate() {
            if byte_count >= pos {
                char_pos = i;
                break;
            }
            byte_count += c.len_utf8();
        }

        if char_pos >= chars.len() {
            return text.len();
        }

        // Skip word characters forward
        let mut i = char_pos;
        while i < chars.len() && !chars[i].is_whitespace() {
            i += 1;
        }

        // Skip whitespace forward
        while i < chars.len() && chars[i].is_whitespace() {
            i += 1;
        }

        // Convert back to byte position
        chars[..i].iter().map(|c| c.len_utf8()).sum()
    }

    /// Get the file path
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
}

impl Default for Buffer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_buffer_new() {
        let buffer = Buffer::new();
        assert_eq!(buffer.len(), 0);
        assert!(buffer.is_empty());
    }

    #[test]
    fn test_buffer_from_str() {
        let mut buffer = Buffer::from_str("hello\nworld");
        assert_eq!(buffer.len(), 11);
        assert_eq!(buffer.line_count(), 2);
        assert_eq!(buffer.line_content(0), "hello");
        assert_eq!(buffer.line_content(1), "world");
    }

    #[test]
    fn test_buffer_insert() {
        let mut buffer = Buffer::from_str("hello");
        buffer.insert(5, " world");
        assert_eq!(buffer.to_string(), "hello world");
        assert!(buffer.is_modified());
    }

    #[test]
    fn test_buffer_delete() {
        let mut buffer = Buffer::from_str("hello world");
        buffer.delete(5..11);
        assert_eq!(buffer.to_string(), "hello");
        assert!(buffer.is_modified());
    }

    #[test]
    fn test_line_to_byte() {
        let mut buffer = Buffer::from_str("line0\nline1\nline2");
        assert_eq!(buffer.line_to_byte(0), 0);
        assert_eq!(buffer.line_to_byte(1), 6);
        assert_eq!(buffer.line_to_byte(2), 12);
    }

    #[test]
    fn test_byte_to_line() {
        let mut buffer = Buffer::from_str("line0\nline1\nline2");
        assert_eq!(buffer.byte_to_line(0), 0);
        assert_eq!(buffer.byte_to_line(5), 0);
        assert_eq!(buffer.byte_to_line(6), 1);
        assert_eq!(buffer.byte_to_line(12), 2);
    }

    #[test]
    fn test_line_cache_invalidation() {
        let mut buffer = Buffer::from_str("line1\nline2");
        assert_eq!(buffer.line_count(), 2);

        buffer.insert(6, "inserted\n");
        assert_eq!(buffer.line_count(), 3);
        assert_eq!(buffer.line_content(1), "inserted");
    }

    #[test]
    fn test_char_boundaries() {
        let buffer = Buffer::from_str("hello");
        assert_eq!(buffer.prev_char_boundary(5), 4);
        assert_eq!(buffer.next_char_boundary(0), 1);
    }

    #[test]
    fn test_word_boundaries() {
        let buffer = Buffer::from_str("hello world foo");
        assert_eq!(buffer.next_word_boundary(0), 6);
        assert_eq!(buffer.next_word_boundary(6), 12);
        assert_eq!(buffer.prev_word_boundary(11), 6);
        assert_eq!(buffer.prev_word_boundary(5), 0);
    }

    #[test]
    fn test_save_load() {
        let temp_dir = tempfile::tempdir().unwrap();
        let file_path = temp_dir.path().join("test.txt");

        let mut buffer = Buffer::from_str("test content");
        buffer.save_to_file(&file_path).unwrap();

        let loaded = Buffer::load_from_file(&file_path).unwrap();
        assert_eq!(loaded.to_string(), "test content");
        assert!(!loaded.is_modified());
    }
}
