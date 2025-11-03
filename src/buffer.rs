use crate::chunk_tree::{ChunkTree, ChunkTreeConfig};
use std::cell::RefCell;
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
    /// Uses RefCell for interior mutability - allows cache updates through &self
    line_cache: RefCell<LineCache>,

    /// Optional file path for persistence
    file_path: Option<PathBuf>,

    /// Has the buffer been modified since last save?
    modified: bool,
}

/// Cache of line start positions for fast lookups
/// Can be partially built (lazy) - scans regions on-demand
struct LineCache {
    /// Byte offset where each line starts
    /// line_starts[0] = 0 (first line starts at byte 0)
    /// line_starts[1] = byte offset where line 1 starts
    line_starts: Vec<usize>,

    /// Is the cache currently valid?
    valid: bool,

    /// Is this a full scan or partial?
    /// If false, line_starts may be incomplete and needs extension
    fully_scanned: bool,

    /// Last byte position we've scanned up to
    scanned_up_to: usize,
}

impl LineCache {
    fn new() -> Self {
        Self {
            line_starts: vec![0],
            valid: true,
            fully_scanned: true,
            scanned_up_to: 0,
        }
    }

    fn invalidate(&mut self) {
        self.valid = false;
        self.fully_scanned = false;
        self.scanned_up_to = 0;
    }

    fn is_valid(&self) -> bool {
        self.valid
    }

    /// Rebuild the line cache from text (full scan)
    fn rebuild(&mut self, text: &[u8]) {
        self.line_starts.clear();
        self.line_starts.push(0);

        for (i, &byte) in text.iter().enumerate() {
            if byte == b'\n' {
                self.line_starts.push(i + 1);
            }
        }

        self.valid = true;
        self.fully_scanned = true;
        self.scanned_up_to = text.len();
    }

    /// Extend the line cache up to at least the given byte position
    /// Only scans the portion that hasn't been scanned yet
    fn ensure_scanned_to(&mut self, text: &[u8], min_byte_pos: usize) {
        if self.fully_scanned || min_byte_pos <= self.scanned_up_to {
            return; // Already scanned enough
        }

        let scan_from = self.scanned_up_to;
        let scan_to = min_byte_pos.min(text.len());

        // Scan from where we left off to the target position
        for i in scan_from..scan_to {
            if text[i] == b'\n' {
                self.line_starts.push(i + 1);
            }
        }

        self.scanned_up_to = scan_to;
        self.fully_scanned = scan_to >= text.len();
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
            line_cache: RefCell::new(LineCache::new()),
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
            line_cache: RefCell::new(line_cache),
            file_path: None,
            modified: false,
        }
    }

    /// Load a buffer from a file
    /// Uses chunked reading to avoid loading the entire file into memory at once
    pub fn load_from_file<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let path = path.as_ref();
        let file = std::fs::File::open(path)?;
        let metadata = file.metadata()?;
        let file_size = metadata.len() as usize;

        // For small files, use the fast path
        if file_size < 1024 * 1024 {
            // < 1MB, read all at once
            let mut file = file;
            let mut contents = String::new();
            file.read_to_string(&mut contents)?;

            let mut buffer = Self::from_str(&contents);
            buffer.file_path = Some(path.to_path_buf());
            buffer.modified = false;
            return Ok(buffer);
        }

        // For large files, read in chunks
        const CHUNK_SIZE: usize = 64 * 1024; // 64KB chunks
        let mut content = ChunkTree::new(DEFAULT_CONFIG);
        let mut reader = std::io::BufReader::with_capacity(CHUNK_SIZE, file);
        let mut chunk_buf = vec![0u8; CHUNK_SIZE];

        loop {
            let bytes_read = reader.read(&mut chunk_buf)?;
            if bytes_read == 0 {
                break;
            }

            // Leak the chunk to get 'static lifetime
            let leaked: &'static [u8] =
                Box::leak(chunk_buf[..bytes_read].to_vec().into_boxed_slice());
            content = content.insert(content.len(), leaked);
        }

        // Line cache starts invalid - will be built lazily on first access
        let line_cache = RefCell::new(LineCache::new());
        line_cache.borrow_mut().invalidate();

        Ok(Self {
            content,
            line_cache,
            file_path: Some(path.to_path_buf()),
            modified: false,
        })
    }

    /// Save the buffer to its file path
    pub fn save(&mut self) -> io::Result<()> {
        if let Some(path) = self.file_path.clone() {
            self.save_to_file(path)?;
            self.modified = false;
            Ok(())
        } else {
            Err(io::Error::other("No file path set for buffer"))
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
        self.line_cache.borrow_mut().invalidate();
        self.modified = true;
    }

    /// Delete a range of bytes
    pub fn delete(&mut self, range: Range<usize>) {
        if range.start >= range.end {
            return;
        }

        self.content = self.content.remove(range);
        self.line_cache.borrow_mut().invalidate();
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

    /// Ensure the line cache is valid UP TO a certain byte position
    /// This allows lazy scanning - only scan what's needed
    fn ensure_line_cache_to(&self, min_byte_pos: usize) {
        use std::time::Instant;
        let start = Instant::now();

        let mut cache = self.line_cache.borrow_mut();

        if cache.fully_scanned {
            return; // Already have everything
        }

        if !cache.valid {
            // First time - start scanning from beginning
            cache.line_starts.clear();
            cache.line_starts.push(0);
            cache.scanned_up_to = 0;
            cache.valid = true;
        }

        // If we've already scanned far enough, we're done
        if min_byte_pos <= cache.scanned_up_to {
            return;
        }

        // Scan incrementally from where we left off using chunk iterator
        let scan_from = cache.scanned_up_to;
        let scan_to = min_byte_pos.min(self.len());
        tracing::debug!(scan_from, scan_to, file_len = self.len(), "Scanning line cache");

        let mut current_pos = 0;
        let mut chunks_visited = 0;
        let mut chunks_skipped = 0;
        let mut chunks_scanned = 0;
        for piece in self.content.iter() {
            chunks_visited += 1;
            match piece {
                crate::chunk_tree::ChunkPiece::Data { data } => {
                    let chunk_end = current_pos + data.len();

                    // Skip chunks entirely before our scan range
                    if chunk_end <= scan_from {
                        chunks_skipped += 1;
                        current_pos = chunk_end;
                        continue;
                    }

                    // Stop if we've scanned past our target
                    if current_pos >= scan_to {
                        break;
                    }

                    // Scan this chunk
                    chunks_scanned += 1;
                    let start_in_chunk = scan_from.saturating_sub(current_pos);
                    let end_in_chunk = (scan_to - current_pos).min(data.len());

                    for i in start_in_chunk..end_in_chunk {
                        if data[i] == b'\n' {
                            cache.line_starts.push(current_pos + i + 1);
                        }
                    }

                    cache.scanned_up_to = current_pos + end_in_chunk;

                    if cache.scanned_up_to >= scan_to {
                        cache.fully_scanned = cache.scanned_up_to >= self.len();
                        return;
                    }

                    current_pos = chunk_end;
                }
                crate::chunk_tree::ChunkPiece::Gap { size } => {
                    let gap_end = current_pos + size;

                    // Skip gaps entirely before our scan range
                    if gap_end <= scan_from {
                        chunks_skipped += 1;
                        current_pos = gap_end;
                        continue;
                    }

                    // Stop if we've scanned past our target
                    if current_pos >= scan_to {
                        break;
                    }

                    current_pos = gap_end;
                    cache.scanned_up_to = cache.scanned_up_to.max(current_pos.min(scan_to));
                }
            }
        }

        cache.fully_scanned = cache.scanned_up_to >= self.len();
        let elapsed = start.elapsed();
        tracing::debug!(
            chunks_visited,
            chunks_skipped,
            chunks_scanned,
            elapsed_ms = elapsed.as_millis(),
            "Line cache scan complete"
        );
    }

    /// Ensure the line cache is fully valid
    /// For large files, this is expensive (~1.2s for 61MB in debug mode)
    fn ensure_line_cache(&self) {
        self.ensure_line_cache_to(self.len());
    }

    /// Convert a line number to a byte offset
    /// For small line numbers, this only scans a small portion of the file
    pub fn line_to_byte(&self, line: usize) -> usize {
        tracing::trace!(line, "line_to_byte called");

        // Quick check: if we already have this line cached, return it
        {
            let cache = self.line_cache.borrow();
            if cache.valid && line < cache.line_count() {
                if let Some(byte_pos) = cache.line_to_byte(line) {
                    tracing::trace!(line, byte_pos, "Found in cache");
                    return byte_pos;
                }
            }
            tracing::debug!(
                line,
                cache_lines = cache.line_count(),
                scanned_up_to = cache.scanned_up_to,
                "Line not in cache, need to scan"
            );
        }

        // We need to scan further - scan incrementally in chunks until we find it
        // Start with a reasonable chunk size (e.g., 64KB at a time)
        const SCAN_CHUNK_SIZE: usize = 64 * 1024; // 64KB

        let mut iteration = 0;
        loop {
            iteration += 1;
            let scan_target = {
                let cache = self.line_cache.borrow();

                // Check if we already have the line
                if let Some(byte_pos) = cache.line_to_byte(line) {
                    tracing::debug!(line, byte_pos, iteration, "Found after scanning");
                    return byte_pos;
                }

                // If we've fully scanned and still don't have it, the line doesn't exist
                if cache.fully_scanned {
                    tracing::warn!(line, "Line not found after full scan");
                    return self.len();
                }

                // Scan another chunk
                let target = cache.scanned_up_to + SCAN_CHUNK_SIZE;
                tracing::debug!(line, iteration, from = cache.scanned_up_to, to = target, "Need to scan more for line");
                target
            };

            self.ensure_line_cache_to(scan_target);
        }
    }

    /// Get line number for a byte position without forcing a scan
    /// Returns an estimate by counting newlines from last known position
    /// This is much faster for large files but less accurate
    pub fn byte_to_line_lazy(&self, byte: usize) -> usize {
        let byte = byte.min(self.len());
        let cache = self.line_cache.borrow();

        // If we've already scanned past this point, use the cached value
        if cache.fully_scanned || byte <= cache.scanned_up_to {
            cache.byte_to_line(byte)
        } else {
            // Estimate by counting from last known position
            let last_known_line = cache.line_starts.len().saturating_sub(1);
            let last_known_byte = cache.line_starts.last().copied().unwrap_or(0);

            // Drop the borrow before counting newlines (which needs to borrow content)
            drop(cache);

            // Count newlines from last known position to target
            let additional_lines = self.count_newlines_in_range(last_known_byte, byte);
            last_known_line + additional_lines
        }
    }

    /// Get an approximate or cached line count without forcing a full scan
    /// Returns None if the full scan hasn't been done yet
    pub fn approximate_line_count(&self) -> Option<usize> {
        let cache = self.line_cache.borrow();
        if cache.fully_scanned {
            Some(cache.line_count())
        } else {
            None
        }
    }

    /// Check if we're at or past the end of the file (by bytes)
    pub fn is_at_eof(&self, byte_pos: usize) -> bool {
        byte_pos >= self.len()
    }

    /// Find the start of the line containing the given byte position
    /// Works backwards from byte_pos, no line number conversion needed
    pub fn find_line_start_at_byte(&self, byte_pos: usize) -> usize {
        let byte_pos = byte_pos.min(self.len());

        // Search backwards for newline
        for i in (0..byte_pos).rev() {
            let piece = self.content.get(i);
            if let crate::chunk_tree::ChunkPiece::Data { data } = piece {
                if !data.is_empty() && data[0] == b'\n' {
                    return i + 1; // Return position after the newline
                }
            }
        }
        0 // Start of file
    }

    /// Find the end of the line containing the given byte position
    /// Works forwards from byte_pos, no line number conversion needed
    /// Returns position just before the newline (or EOF)
    pub fn find_line_end_at_byte(&self, byte_pos: usize) -> usize {
        let byte_pos = byte_pos.min(self.len());
        let file_len = self.len();

        // Search forwards for newline
        for i in byte_pos..file_len {
            let piece = self.content.get(i);
            if let crate::chunk_tree::ChunkPiece::Data { data } = piece {
                if !data.is_empty() && data[0] == b'\n' {
                    return i; // Return position of the newline
                }
            }
        }
        file_len // End of file
    }

    /// Find the start of the previous line from the given byte position
    /// Returns None if already on the first line
    pub fn find_prev_line_start_from_byte(&self, byte_pos: usize) -> Option<usize> {
        let line_start = self.find_line_start_at_byte(byte_pos);
        if line_start == 0 {
            return None; // Already on first line
        }

        // Go back one character (to the newline of previous line) and find that line's start
        Some(self.find_line_start_at_byte(line_start - 1))
    }

    /// Find the start of the next line from the given byte position
    /// Returns None if on the last line
    pub fn find_next_line_start_from_byte(&self, byte_pos: usize) -> Option<usize> {
        let line_end = self.find_line_end_at_byte(byte_pos);
        if line_end >= self.len() {
            return None; // On last line
        }

        // Move past the newline to get to next line start
        Some(line_end + 1)
    }

    /// Check if a line is the last line in the file (no full scan)
    /// Returns true if the next line would be at or past EOF
    pub fn is_last_line(&self, line: usize) -> bool {
        let next_line_start = self.line_to_byte(line + 1);
        next_line_start >= self.len()
    }

    /// Get the byte position of the end of a line (no full scan)
    /// This works even if we haven't scanned the entire file
    /// Returns the byte position just before the newline (or EOF)
    pub fn line_end_byte(&self, line: usize) -> usize {
        let next_line_start = self.line_to_byte(line + 1);
        if next_line_start >= self.len() {
            // Last line - return EOF
            self.len()
        } else {
            // Not last line - return position before newline
            next_line_start.saturating_sub(1)
        }
    }

    /// Get the byte position of the end of a line INCLUDING the newline (no full scan)
    /// This is used for operations that want to include the newline character
    pub fn line_end_byte_with_newline(&self, line: usize) -> usize {
        let next_line_start = self.line_to_byte(line + 1);
        if next_line_start >= self.len() {
            self.len()
        } else {
            next_line_start
        }
    }

    /// Get line number for display purposes
    /// Returns either:
    /// - LineNumber::Absolute(n) if we have scanned up to this line
    /// - LineNumber::Relative(offset) if we haven't scanned this far yet
    pub fn display_line_number(&self, byte_pos: usize) -> LineNumber {
        let cache = self.line_cache.borrow();

        // If we've scanned past this position, we know the absolute line number
        if cache.fully_scanned || byte_pos <= cache.scanned_up_to {
            LineNumber::Absolute(cache.byte_to_line(byte_pos))
        } else {
            // We haven't scanned this far yet - return relative to last known line
            let last_known_byte = cache.line_starts.last().copied().unwrap_or(0);

            // Count newlines from last known position (locally, without full scan)
            let relative_offset = self.count_newlines_in_range(last_known_byte, byte_pos);
            LineNumber::Relative(relative_offset)
        }
    }

    /// Count newlines in a byte range without caching
    /// This is used for relative line numbers
    fn count_newlines_in_range(&self, start: usize, end: usize) -> usize {
        if start >= end || start >= self.len() {
            return 0;
        }

        let mut count = 0;
        let actual_end = end.min(self.len());

        for i in start..actual_end {
            let piece = self.content.get(i);
            if let crate::chunk_tree::ChunkPiece::Data { data } = piece {
                if !data.is_empty() && data[0] == b'\n' {
                    count += 1;
                }
            }
        }

        count
    }
}

/// Represents a line number for display purposes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LineNumber {
    /// Absolute line number (we've scanned this far)
    Absolute(usize),
    /// Relative offset from viewport start (haven't scanned this far)
    Relative(usize),
}

impl LineNumber {
    /// Format for display (e.g., "42" or "+5")
    pub fn format(&self) -> String {
        match self {
            LineNumber::Absolute(n) => format!("{}", n + 1), // 1-indexed for display
            LineNumber::Relative(offset) => format!("+{}", offset),
        }
    }
}

impl Buffer {
    /// Get line content starting from a byte position (no full scan needed)
    /// Scans forward from byte_pos to the next newline
    pub fn line_content_at_byte(&self, byte_pos: usize) -> String {
        if byte_pos >= self.len() {
            return String::new();
        }

        // Find the start of the line (scan backward to previous newline or start)
        let mut line_start = byte_pos;
        while line_start > 0 {
            let piece = self.content.get(line_start - 1);
            if let crate::chunk_tree::ChunkPiece::Data { data } = piece {
                if !data.is_empty() && data[0] == b'\n' {
                    break;
                }
            }
            line_start -= 1;
        }

        // Find the end of the line (scan forward to next newline or end)
        let mut line_end = byte_pos;
        while line_end < self.len() {
            let piece = self.content.get(line_end);
            if let crate::chunk_tree::ChunkPiece::Data { data } = piece {
                if !data.is_empty() && data[0] == b'\n' {
                    break;
                }
            }
            line_end += 1;
        }

        self.slice(line_start..line_end)
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

        if i > 0 || chars.first().is_some_and(|c| c.is_whitespace()) {
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
}

impl Default for Buffer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::NamedTempFile;

    // Test-only helper methods that trigger full scans
    impl Buffer {
        /// Get line count - triggers full scan, only for tests
        pub(in crate::buffer) fn test_line_count(&self) -> usize {
            self.ensure_line_cache();
            self.line_cache.borrow().line_count()
        }

        /// Get line content - triggers full scan, only for tests
        pub(in crate::buffer) fn test_line_content(&self, line: usize) -> String {
            self.ensure_line_cache();
            let cache = self.line_cache.borrow();
            let start = cache.line_to_byte(line).unwrap_or(self.len());
            let end = cache.line_to_byte(line + 1).unwrap_or(self.len());

            let mut content = self.slice(start..end);
            // Remove trailing newline if present
            if content.ends_with('\n') {
                content.pop();
            }
            content
        }

        /// Convert byte offset to line number - triggers scan, only for tests
        pub(in crate::buffer) fn byte_to_line(&self, byte: usize) -> usize {
            let byte = byte.min(self.len());
            self.ensure_line_cache_to(byte);
            self.line_cache.borrow().byte_to_line(byte)
        }

        /// Find line end from byte position - triggers scan, only for tests
        pub(in crate::buffer) fn find_line_end_from_byte(&self, byte_pos: usize) -> usize {
            let line = self.byte_to_line(byte_pos);
            self.line_end_byte(line)
        }

        /// Find line end with newline from byte position - triggers scan, only for tests
        pub(in crate::buffer) fn find_line_end_with_newline_from_byte(&self, byte_pos: usize) -> usize {
            let line = self.byte_to_line(byte_pos);
            self.line_end_byte_with_newline(line)
        }

        /// Check if byte position is on last line - triggers scan, only for tests
        pub(in crate::buffer) fn is_on_last_line(&self, byte_pos: usize) -> bool {
            let line = self.byte_to_line(byte_pos);
            self.is_last_line(line)
        }
    }

    // Property-based tests using proptest
    #[cfg(test)]
    mod property_tests {
        use super::*;
        use proptest::prelude::*;

        proptest! {
            /// Insert then delete should restore original state
            #[test]
            fn insert_delete_inverse(
                initial in ".{0,100}",
                pos in 0usize..100,
                text in ".{1,50}"
            ) {
                let mut buffer = Buffer::from_str(&initial);
                let original = buffer.to_string();

                // Clamp position to valid range
                let pos = pos.min(buffer.len());

                // Insert
                buffer.insert(pos, &text);

                // Verify it was inserted
                assert_eq!(buffer.len(), original.len() + text.len());

                // Delete what we inserted
                buffer.delete(pos..pos + text.len());

                // Should be back to original
                assert_eq!(buffer.to_string(), original);
            }

            /// Line cache should be consistent with byte positions
            #[test]
            fn line_cache_consistency(text in ".{0,200}\n*.{0,200}") {
                let buffer = Buffer::from_str(&text);
                let line_count = buffer.test_line_count();

                // For each line, byte_to_line(line_to_byte(n)) should equal n
                for line_num in 0..line_count {
                    let byte_offset = buffer.line_to_byte(line_num);
                    let recovered_line = buffer.byte_to_line(byte_offset);
                    assert_eq!(recovered_line, line_num,
                        "Line {line_num} -> byte {byte_offset} -> line {recovered_line} (should be {line_num})");
                }
            }

            /// Content length should always match input length
            #[test]
            fn content_length_invariant(text in ".{0,500}") {
                let buffer = Buffer::from_str(&text);
                assert_eq!(buffer.len(), text.len());
            }

            /// Deleting text should never increase line count
            #[test]
            fn delete_monotonic_lines(
                text in ".{10,200}",
                start in 0usize..100,
                end in 0usize..100
            ) {
                let mut buffer = Buffer::from_str(&text);
                let original_lines = buffer.test_line_count();

                let start = start.min(buffer.len());
                let end = end.min(buffer.len());
                let range = start.min(end)..start.max(end);

                if !range.is_empty() {
                    buffer.delete(range);
                    assert!(buffer.test_line_count() <= original_lines,
                        "Delete increased line count: {} -> {}",
                        original_lines, buffer.test_line_count());
                }
            }

            /// Save then load should preserve content
            #[test]
            fn save_load_roundtrip(text in ".{0,1000}") {
                let mut buffer = Buffer::from_str(&text);

                // Save to temp file
                let temp_file = NamedTempFile::new().unwrap();
                buffer.set_file_path(temp_file.path().to_path_buf());
                buffer.save().unwrap();

                // Load it back
                let loaded = Buffer::load_from_file(temp_file.path()).unwrap();

                assert_eq!(loaded.to_string(), text);
            }
        }
    }

    #[test]
    fn test_buffer_new() {
        let buffer = Buffer::new();
        assert_eq!(buffer.len(), 0);
        assert!(buffer.is_empty());
    }

    #[test]
    fn test_buffer_from_str() {
        let buffer = Buffer::from_str("hello\nworld");
        assert_eq!(buffer.len(), 11);
        assert_eq!(buffer.test_line_count(), 2);
        assert_eq!(buffer.test_line_content(0), "hello");
        assert_eq!(buffer.test_line_content(1), "world");
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
        let buffer = Buffer::from_str("line0\nline1\nline2");
        assert_eq!(buffer.line_to_byte(0), 0);
        assert_eq!(buffer.line_to_byte(1), 6);
        assert_eq!(buffer.line_to_byte(2), 12);
    }

    #[test]
    fn test_byte_to_line() {
        let buffer = Buffer::from_str("line0\nline1\nline2");
        assert_eq!(buffer.byte_to_line_lazy(0), 0);
        assert_eq!(buffer.byte_to_line_lazy(5), 0);
        assert_eq!(buffer.byte_to_line_lazy(6), 1);
        assert_eq!(buffer.byte_to_line_lazy(12), 2);
    }

    #[test]
    fn test_line_cache_invalidation() {
        let mut buffer = Buffer::from_str("line1\nline2");
        assert_eq!(buffer.test_line_count(), 2);

        buffer.insert(6, "inserted\n");
        assert_eq!(buffer.test_line_count(), 3);
        assert_eq!(buffer.test_line_content(1), "inserted");
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

    #[test]
    #[ignore] // Run with: cargo test test_load_big_file -- --ignored --nocapture
    fn test_load_big_file() {
        use std::time::Instant;

        println!("\n=== Testing BIG.txt loading ===");

        let start = Instant::now();
        let buffer = Buffer::load_from_file("tests/BIG.txt").unwrap();
        let load_time = start.elapsed();
        println!("✓ File loaded in: {:?}", load_time);

        let start = Instant::now();
        let len = buffer.len();
        let len_time = start.elapsed();
        println!("✓ Length ({} bytes) in: {:?}", len, len_time);

        let start = Instant::now();
        let line_count = buffer.test_line_count();
        let count_time = start.elapsed();
        println!("✓ Line count ({} lines) in: {:?}", line_count, count_time);

        let start = Instant::now();
        let first_line = buffer.test_line_content(0);
        let first_line_time = start.elapsed();
        println!("✓ First line content in: {:?}", first_line_time);
        println!("  First line: {:?}", &first_line[..first_line.len().min(50)]);

        println!("\nTotal time: {:?}", load_time + len_time + count_time + first_line_time);
    }

    #[test]
    #[ignore] // Run with: cargo test test_load_big_file_instant -- --ignored --nocapture
    fn test_load_big_file_instant() {
        use std::time::Instant;

        println!("\n=== Testing BIG.txt INSTANT loading (no line_count) ===");

        let start = Instant::now();
        let buffer = Buffer::load_from_file("tests/BIG.txt").unwrap();
        let load_time = start.elapsed();
        println!("✓ File loaded in: {:?}", load_time);

        // Test that we can get display line numbers WITHOUT triggering full scan
        let start = Instant::now();
        let display_num_0 = buffer.display_line_number(0);
        let display_num_100 = buffer.display_line_number(100);
        let display_time = start.elapsed();
        println!("✓ Display line numbers in: {:?}", display_time);
        println!("  Line 0: {}", display_num_0.format());
        println!("  Byte 100: {}", display_num_100.format());

        // Check that we haven't scanned the full file
        let approx_count = buffer.approximate_line_count();
        println!("✓ Approximate line count: {:?} (None = not scanned)", approx_count);

        let start = Instant::now();
        let first_line = buffer.line_content_at_byte(0);
        let first_line_time = start.elapsed();
        println!("✓ First line content (no scan) in: {:?}", first_line_time);
        println!("  First line: {:?}", &first_line[..first_line.len().min(50)]);

        println!("\nTotal time (INSTANT): {:?}", load_time + display_time + first_line_time);
        println!("Expected: < 500ms on fast machine (vs ~1.7s with full line_count scan)");

        // The key assertion: we should NOT have scanned the full file
        assert!(approx_count.is_none(),
            "File should not be fully scanned yet, but approximate_line_count returned {:?}", approx_count);
    }
}
