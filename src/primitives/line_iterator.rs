use crate::model::buffer::TextBuffer;

/// Iterator over lines in a TextBuffer with bidirectional support
/// Uses piece iterator for efficient sequential scanning (ONE O(log n) initialization)
///
/// # Performance Characteristics
///
/// Line tracking is now always computed when chunks are loaded:
/// - **All loaded chunks**: `line_starts = Vec<usize>` → exact line metadata available
/// - **Unloaded chunks**: Only metadata unavailable until first access
///
/// ## Current Performance:
/// - **Forward iteration (`next()`)**: ✅ Efficient O(1) amortized per line using piece iterator
/// - **Backward iteration (`prev()`)**: ✅ O(log n) using piece tree line indexing
/// - **Initialization (`new()`)**: ✅ O(log n) using offset_to_position
///
/// ## Design:
/// - Loaded chunks are always indexed (10% memory overhead per chunk)
/// - Cursor vicinity is always loaded and indexed → 100% accurate navigation
/// - Forward scanning with lazy loading handles long lines efficiently
/// - Backward navigation uses piece tree's line_range() lookup
///
/// The `estimated_line_length` parameter is still used for forward scanning to estimate
/// initial chunk sizes, but line boundaries are always accurate after data is loaded.
/// Maximum bytes to return per "line" to prevent memory exhaustion from huge single-line files.
/// Lines longer than this are split into multiple chunks, each treated as a separate "line".
/// This is generous enough for any practical line while preventing OOM from 10MB+ lines.
const MAX_LINE_BYTES: usize = 100_000;

pub struct LineIterator<'a> {
    buffer: &'a mut TextBuffer,
    /// Current byte position in the document (points to start of current line)
    current_pos: usize,
    buffer_len: usize,
    /// Estimated average line length in bytes (for large file estimation)
    estimated_line_length: usize,
    /// Whether we still need to emit a synthetic empty line at EOF
    /// (set when starting at EOF after a trailing newline or when a newline-ending
    /// line exhausts the buffer during forward iteration)
    pending_trailing_empty_line: bool,
}

impl<'a> LineIterator<'a> {
    /// Scan backward from byte_pos to find the start of the line
    /// chunk_size: suggested chunk size for loading (used as performance hint only)
    fn find_line_start_backward(
        buffer: &mut TextBuffer,
        byte_pos: usize,
        chunk_size: usize,
    ) -> usize {
        if byte_pos == 0 {
            return 0;
        }

        // Scan backward in chunks until we find a newline or reach position 0
        // The chunk_size is just a hint for performance - we MUST find the actual line start
        let mut search_end = byte_pos;

        loop {
            let scan_start = search_end.saturating_sub(chunk_size);
            let scan_len = search_end - scan_start;

            // Load the chunk we need to scan
            if let Ok(chunk) = buffer.get_text_range_mut(scan_start, scan_len) {
                // Scan backward through the chunk to find the last newline
                for i in (0..chunk.len()).rev() {
                    if chunk[i] == b'\n' {
                        // Found newline - line starts at the next byte
                        return scan_start + i + 1;
                    }
                }
            }

            // No newline found in this chunk
            if scan_start == 0 {
                // Reached the start of the buffer - line starts at 0
                return 0;
            }

            // Continue searching from earlier position
            search_end = scan_start;
        }
    }

    pub(crate) fn new(
        buffer: &'a mut TextBuffer,
        byte_pos: usize,
        estimated_line_length: usize,
    ) -> Self {
        let buffer_len = buffer.len();
        let byte_pos = byte_pos.min(buffer_len);

        // Find the start of the line containing byte_pos
        let line_start = if byte_pos == 0 {
            0
        } else {
            // CRITICAL: Pre-load the chunk containing byte_pos to ensure offset_to_position works
            // Handle EOF case where byte_pos might equal buffer_len
            let pos_to_load = if byte_pos >= buffer_len {
                buffer_len.saturating_sub(1)
            } else {
                byte_pos
            };

            if pos_to_load < buffer_len {
                let _ = buffer.get_text_range_mut(pos_to_load, 1);
            }

            // Scan backward from byte_pos to find the start of the line
            // We scan backward looking for a newline character
            // NOTE: We previously tried to use offset_to_position() but it has bugs with column calculation
            Self::find_line_start_backward(buffer, byte_pos, estimated_line_length)
        };

        let mut pending_trailing_empty_line = false;
        if buffer_len > 0 && byte_pos == buffer_len {
            if let Ok(bytes) = buffer.get_text_range_mut(buffer_len - 1, 1) {
                if bytes.first() == Some(&b'\n') {
                    pending_trailing_empty_line = true;
                }
            }
        }

        LineIterator {
            buffer,
            current_pos: line_start,
            buffer_len,
            estimated_line_length,
            pending_trailing_empty_line,
        }
    }

    /// Get the next line (moving forward)
    /// Uses lazy loading to handle unloaded buffers transparently
    pub fn next_line(&mut self) -> Option<(usize, String)> {
        if self.pending_trailing_empty_line {
            self.pending_trailing_empty_line = false;
            let line_start = self.buffer_len;
            return Some((line_start, String::new()));
        }

        if self.current_pos >= self.buffer_len {
            return None;
        }

        let line_start = self.current_pos;

        // Estimate line length for chunk loading (typically lines are < 200 bytes)
        // We load more than average to handle long lines without multiple loads
        let estimated_max_line_length = self.estimated_line_length * 3;
        let bytes_to_scan = estimated_max_line_length.min(self.buffer_len - self.current_pos);

        // Use get_text_range_mut() which handles lazy loading automatically
        // This never scans the entire file - only loads the chunk needed for this line
        let chunk = match self
            .buffer
            .get_text_range_mut(self.current_pos, bytes_to_scan)
        {
            Ok(data) => data,
            Err(e) => {
                tracing::error!(
                    "LineIterator: Failed to load chunk at offset {}: {}",
                    self.current_pos,
                    e
                );
                return None;
            }
        };

        // Scan for newline in the loaded chunk
        let mut line_len = 0;
        let mut found_newline = false;
        for &byte in chunk.iter() {
            line_len += 1;
            if byte == b'\n' {
                found_newline = true;
                break;
            }
        }

        // If we didn't find a newline and didn't reach EOF, the line is longer than our estimate
        // Load more data iteratively (rare case for very long lines)
        // BUT: limit to MAX_LINE_BYTES to prevent memory exhaustion from huge lines
        if !found_newline && self.current_pos + line_len < self.buffer_len {
            // Line is longer than expected, keep loading until we find newline, EOF, or hit limit
            let mut extended_chunk = chunk;
            while !found_newline
                && self.current_pos + extended_chunk.len() < self.buffer_len
                && extended_chunk.len() < MAX_LINE_BYTES
            {
                let additional_bytes = estimated_max_line_length
                    .min(self.buffer_len - self.current_pos - extended_chunk.len())
                    .min(MAX_LINE_BYTES - extended_chunk.len()); // Don't exceed limit
                match self
                    .buffer
                    .get_text_range_mut(self.current_pos + extended_chunk.len(), additional_bytes)
                {
                    Ok(mut more_data) => {
                        let start_len = extended_chunk.len();
                        extended_chunk.append(&mut more_data);

                        // Scan the newly added portion
                        for &byte in extended_chunk[start_len..].iter() {
                            line_len += 1;
                            if byte == b'\n' {
                                found_newline = true;
                                break;
                            }
                            // Also stop if we've hit the limit
                            if line_len >= MAX_LINE_BYTES {
                                break;
                            }
                        }
                    }
                    Err(e) => {
                        tracing::error!("LineIterator: Failed to extend chunk: {}", e);
                        break;
                    }
                }
            }

            // Clamp line_len to MAX_LINE_BYTES (safety limit for huge single-line files)
            line_len = line_len.min(MAX_LINE_BYTES).min(extended_chunk.len());

            // Use the extended chunk
            let line_bytes = &extended_chunk[..line_len];
            self.current_pos += line_len;
            self.schedule_trailing_empty_line(line_bytes);
            let line_string = String::from_utf8_lossy(line_bytes).into_owned();
            return Some((line_start, line_string));
        }

        // Normal case: found newline or reached EOF within initial chunk
        let line_bytes = &chunk[..line_len];
        self.current_pos += line_len;
        self.schedule_trailing_empty_line(line_bytes);
        let line_string = String::from_utf8_lossy(line_bytes).into_owned();
        Some((line_start, line_string))
    }

    /// Get the previous line (moving backward)
    /// Uses direct byte scanning which works even with unloaded chunks
    pub fn prev(&mut self) -> Option<(usize, String)> {
        if self.current_pos == 0 {
            return None;
        }

        // current_pos is the start of the current line
        // Scan backward from current_pos-1 to find the end of the previous line
        if self.current_pos == 0 {
            return None;
        }

        // Load a reasonable chunk backward for scanning
        let scan_distance = self.estimated_line_length * 3;
        let scan_start = self.current_pos.saturating_sub(scan_distance);
        let scan_len = self.current_pos - scan_start;

        // Load the data we need to scan
        let chunk = match self.buffer.get_text_range_mut(scan_start, scan_len) {
            Ok(data) => data,
            Err(e) => {
                tracing::error!(
                    "LineIterator::prev(): Failed to load chunk at {}: {}",
                    scan_start,
                    e
                );
                return None;
            }
        };

        // Scan backward to find the last newline (end of previous line)
        let mut prev_line_end = None;
        for i in (0..chunk.len()).rev() {
            if chunk[i] == b'\n' {
                prev_line_end = Some(scan_start + i);
                break;
            }
        }

        let prev_line_end = prev_line_end?;

        // Now find the start of the previous line by scanning backward from prev_line_end
        let prev_line_start = if prev_line_end == 0 {
            0
        } else {
            Self::find_line_start_backward(self.buffer, prev_line_end, scan_distance)
        };

        // Load the previous line content
        let prev_line_len = prev_line_end - prev_line_start + 1; // +1 to include the newline
        let line_bytes = match self
            .buffer
            .get_text_range_mut(prev_line_start, prev_line_len)
        {
            Ok(data) => data,
            Err(e) => {
                tracing::error!(
                    "LineIterator::prev(): Failed to load line at {}: {}",
                    prev_line_start,
                    e
                );
                return None;
            }
        };

        let line_string = String::from_utf8_lossy(&line_bytes).into_owned();
        self.current_pos = prev_line_start;
        Some((prev_line_start, line_string))
    }

    /// Get the current position in the buffer (byte offset of current line start)
    pub fn current_position(&self) -> usize {
        self.current_pos
    }

    fn schedule_trailing_empty_line(&mut self, line_bytes: &[u8]) {
        if line_bytes.ends_with(b"\n") && self.current_pos == self.buffer_len {
            self.pending_trailing_empty_line = true;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_iterator_new_at_line_start() {
        let mut buffer = TextBuffer::from_bytes(b"Hello\nWorld\nTest".to_vec());

        // Test iterator at position 0 (start of line 0)
        let iter = buffer.line_iterator(0, 80);
        assert_eq!(iter.current_position(), 0, "Should be at start of line 0");

        // Test iterator at position 6 (start of line 1, after \n)
        let iter = buffer.line_iterator(6, 80);
        assert_eq!(iter.current_position(), 6, "Should be at start of line 1");

        // Test iterator at position 12 (start of line 2, after second \n)
        let iter = buffer.line_iterator(12, 80);
        assert_eq!(iter.current_position(), 12, "Should be at start of line 2");
    }

    #[test]
    fn test_line_iterator_new_in_middle_of_line() {
        let mut buffer = TextBuffer::from_bytes(b"Hello\nWorld\nTest".to_vec());

        // Test iterator at position 3 (middle of "Hello")
        let iter = buffer.line_iterator(3, 80);
        assert_eq!(iter.current_position(), 0, "Should find start of line 0");

        // Test iterator at position 9 (middle of "World")
        let iter = buffer.line_iterator(9, 80);
        assert_eq!(iter.current_position(), 6, "Should find start of line 1");

        // Test iterator at position 14 (middle of "Test")
        let iter = buffer.line_iterator(14, 80);
        assert_eq!(iter.current_position(), 12, "Should find start of line 2");
    }

    #[test]
    fn test_line_iterator_next() {
        let mut buffer = TextBuffer::from_bytes(b"Hello\nWorld\nTest".to_vec());
        let mut iter = buffer.line_iterator(0, 80);

        // First line
        let (pos, content) = iter.next_line().expect("Should have first line");
        assert_eq!(pos, 0);
        assert_eq!(content, "Hello\n");

        // Second line
        let (pos, content) = iter.next_line().expect("Should have second line");
        assert_eq!(pos, 6);
        assert_eq!(content, "World\n");

        // Third line
        let (pos, content) = iter.next_line().expect("Should have third line");
        assert_eq!(pos, 12);
        assert_eq!(content, "Test");

        // No more lines
        assert!(iter.next_line().is_none());
    }

    #[test]
    fn test_line_iterator_from_middle_position() {
        let mut buffer = TextBuffer::from_bytes(b"Hello\nWorld\nTest".to_vec());

        // Start from position 9 (middle of "World")
        let mut iter = buffer.line_iterator(9, 80);
        assert_eq!(
            iter.current_position(),
            6,
            "Should be at start of line containing position 9"
        );

        // First next() should return current line
        let (pos, content) = iter.next_line().expect("Should have current line");
        assert_eq!(pos, 6);
        assert_eq!(content, "World\n");

        // Second next() should return next line
        let (pos, content) = iter.next_line().expect("Should have next line");
        assert_eq!(pos, 12);
        assert_eq!(content, "Test");
    }

    #[test]
    fn test_line_iterator_offset_to_position_consistency() {
        let mut buffer = TextBuffer::from_bytes(b"Hello\nWorld".to_vec());

        // For each position, verify that offset_to_position returns correct values
        let expected = vec![
            (0, 0, 0),  // H
            (1, 0, 1),  // e
            (2, 0, 2),  // l
            (3, 0, 3),  // l
            (4, 0, 4),  // o
            (5, 0, 5),  // \n
            (6, 1, 0),  // W
            (7, 1, 1),  // o
            (8, 1, 2),  // r
            (9, 1, 3),  // l
            (10, 1, 4), // d
        ];

        for (offset, expected_line, expected_col) in expected {
            let pos = buffer
                .offset_to_position(offset)
                .expect(&format!("Should have position for offset {}", offset));
            assert_eq!(pos.line, expected_line, "Wrong line for offset {}", offset);
            assert_eq!(
                pos.column, expected_col,
                "Wrong column for offset {}",
                offset
            );

            // Verify LineIterator uses this correctly
            let iter = buffer.line_iterator(offset, 80);
            let expected_line_start = if expected_line == 0 { 0 } else { 6 };
            assert_eq!(
                iter.current_position(),
                expected_line_start,
                "LineIterator at offset {} should be at line start {}",
                offset,
                expected_line_start
            );
        }
    }

    #[test]
    fn test_line_iterator_prev() {
        let mut buffer = TextBuffer::from_bytes(b"Line1\nLine2\nLine3".to_vec());

        // Start at line 2
        let mut iter = buffer.line_iterator(12, 80);

        // Go back to line 1
        let (pos, content) = iter.prev().expect("Should have previous line");
        assert_eq!(pos, 6);
        assert_eq!(content, "Line2\n");

        // Go back to line 0
        let (pos, content) = iter.prev().expect("Should have previous line");
        assert_eq!(pos, 0);
        assert_eq!(content, "Line1\n");

        // No more previous lines
        assert!(iter.prev().is_none());
    }

    #[test]
    fn test_line_iterator_single_line() {
        let mut buffer = TextBuffer::from_bytes(b"Only one line".to_vec());
        let mut iter = buffer.line_iterator(0, 80);

        let (pos, content) = iter.next_line().expect("Should have the line");
        assert_eq!(pos, 0);
        assert_eq!(content, "Only one line");

        assert!(iter.next_line().is_none());
        assert!(iter.prev().is_none());
    }

    #[test]
    fn test_line_iterator_empty_lines() {
        let mut buffer = TextBuffer::from_bytes(b"Line1\n\nLine3".to_vec());
        let mut iter = buffer.line_iterator(0, 80);

        let (pos, content) = iter.next_line().expect("First line");
        assert_eq!(pos, 0);
        assert_eq!(content, "Line1\n");

        let (pos, content) = iter.next_line().expect("Empty line");
        assert_eq!(pos, 6);
        assert_eq!(content, "\n");

        let (pos, content) = iter.next_line().expect("Third line");
        assert_eq!(pos, 7);
        assert_eq!(content, "Line3");
    }

    #[test]
    fn test_line_iterator_trailing_newline_emits_empty_line() {
        let mut buffer = TextBuffer::from_bytes(b"Hello world\n".to_vec());
        let mut iter = buffer.line_iterator(0, 80);

        let (pos, content) = iter.next_line().expect("First line");
        assert_eq!(pos, 0);
        assert_eq!(content, "Hello world\n");

        let (pos, content) = iter
            .next_line()
            .expect("Should emit empty line for trailing newline");
        assert_eq!(pos, "Hello world\n".len());
        assert_eq!(content, "");

        assert!(iter.next_line().is_none(), "No more lines expected");
    }

    #[test]
    fn test_line_iterator_trailing_newline_starting_at_eof() {
        let mut buffer = TextBuffer::from_bytes(b"Hello world\n".to_vec());
        let buffer_len = buffer.len();
        let mut iter = buffer.line_iterator(buffer_len, 80);

        let (pos, content) = iter
            .next_line()
            .expect("Should emit empty line at EOF when starting there");
        assert_eq!(pos, buffer_len);
        assert_eq!(content, "");

        assert!(iter.next_line().is_none(), "No more lines expected");
    }

    /// BUG REPRODUCTION: Line longer than estimated_line_length
    /// When a line is longer than the estimated_line_length passed to line_iterator(),
    /// the LineIterator::new() constructor fails to find the actual line start.
    ///
    /// This causes Home/End key navigation to fail on long lines.
    #[test]
    fn test_line_iterator_long_line_exceeds_estimate() {
        // Create a line that's 200 bytes long (much longer than typical estimate)
        let long_line = "x".repeat(200);
        let content = format!("{}\n", long_line);
        let mut buffer = TextBuffer::from_bytes(content.as_bytes().to_vec());

        // Use a small estimated_line_length (50 bytes) - smaller than actual line
        let estimated_line_length = 50;

        // Position cursor at the END of the long line (position 200, before the \n)
        let cursor_at_end = 200;

        // Create iterator from end of line - this should find position 0 as line start
        let iter = buffer.line_iterator(cursor_at_end, estimated_line_length);

        // BUG: iter.current_position() returns 150 (200 - 50) instead of 0
        // because find_line_start_backward only scans back 50 bytes
        assert_eq!(
            iter.current_position(),
            0,
            "LineIterator should find actual line start (0), not estimation boundary ({})",
            cursor_at_end - estimated_line_length
        );

        // Test with cursor in the middle too
        let cursor_in_middle = 100;
        let iter = buffer.line_iterator(cursor_in_middle, estimated_line_length);
        assert_eq!(
            iter.current_position(),
            0,
            "LineIterator should find line start regardless of cursor position"
        );
    }

    /// BUG REPRODUCTION: Multiple lines where one exceeds estimate
    /// Tests that line iteration works correctly even when one line is very long
    #[test]
    fn test_line_iterator_mixed_line_lengths() {
        // Short line, very long line, short line
        let long_line = "L".repeat(300);
        let content = format!("Short1\n{}\nShort2\n", long_line);
        let mut buffer = TextBuffer::from_bytes(content.as_bytes().to_vec());

        let estimated_line_length = 50;

        // Position cursor at end of long line (position 7 + 300 = 307)
        let cursor_pos = 307;

        let iter = buffer.line_iterator(cursor_pos, estimated_line_length);

        // Should find position 7 (start of long line), not 257 (307 - 50)
        assert_eq!(
            iter.current_position(),
            7,
            "Should find start of long line at position 7, not estimation boundary"
        );
    }

    /// Test that LineIterator correctly handles CRLF line endings
    /// Each line should have the correct byte offset, accounting for 2 bytes per line ending
    #[test]
    fn test_line_iterator_crlf() {
        // CRLF content: "abc\r\ndef\r\nghi\r\n"
        // Bytes: a=0, b=1, c=2, \r=3, \n=4, d=5, e=6, f=7, \r=8, \n=9, g=10, h=11, i=12, \r=13, \n=14
        let content = b"abc\r\ndef\r\nghi\r\n";
        let buffer_len = content.len();
        let mut buffer = TextBuffer::from_bytes(content.to_vec());

        let mut iter = buffer.line_iterator(0, 80);

        // First line: starts at 0, content is "abc\r\n"
        let (pos, line_content) = iter.next_line().expect("Should have first line");
        assert_eq!(pos, 0, "First line should start at byte 0");
        assert_eq!(line_content, "abc\r\n", "First line content");

        // Second line: starts at 5 (after "abc\r\n"), content is "def\r\n"
        let (pos, line_content) = iter.next_line().expect("Should have second line");
        assert_eq!(pos, 5, "Second line should start at byte 5 (after CRLF)");
        assert_eq!(line_content, "def\r\n", "Second line content");

        // Third line: starts at 10 (after "abc\r\ndef\r\n"), content is "ghi\r\n"
        let (pos, line_content) = iter.next_line().expect("Should have third line");
        assert_eq!(
            pos, 10,
            "Third line should start at byte 10 (after two CRLFs)"
        );
        assert_eq!(line_content, "ghi\r\n", "Third line content");

        // Trailing CRLF means there's an empty synthetic line at EOF
        let (pos, line_content) = iter
            .next_line()
            .expect("Should emit empty line after trailing CRLF");
        assert_eq!(pos, buffer_len, "Empty line should start at EOF");
        assert_eq!(line_content, "", "Empty line content");

        assert!(iter.next_line().is_none(), "Should have no more lines");
    }

    /// Test that line_start values are correct for CRLF files when starting from middle
    #[test]
    fn test_line_iterator_crlf_from_middle() {
        // CRLF content: "abc\r\ndef\r\nghi"
        // Bytes: a=0, b=1, c=2, \r=3, \n=4, d=5, e=6, f=7, \r=8, \n=9, g=10, h=11, i=12
        let content = b"abc\r\ndef\r\nghi";
        let mut buffer = TextBuffer::from_bytes(content.to_vec());

        // Start iterator from middle of second line (byte 6 = 'e')
        let iter = buffer.line_iterator(6, 80);
        assert_eq!(
            iter.current_position(),
            5,
            "Iterator at byte 6 should find line start at byte 5"
        );

        // Start iterator from the \r of first line (byte 3)
        let iter = buffer.line_iterator(3, 80);
        assert_eq!(
            iter.current_position(),
            0,
            "Iterator at byte 3 (\\r) should find line start at byte 0"
        );

        // Start iterator from the \n of first line (byte 4)
        let iter = buffer.line_iterator(4, 80);
        assert_eq!(
            iter.current_position(),
            0,
            "Iterator at byte 4 (\\n) should find line start at byte 0"
        );

        // Start iterator from first char of third line (byte 10 = 'g')
        let iter = buffer.line_iterator(10, 80);
        assert_eq!(
            iter.current_position(),
            10,
            "Iterator at byte 10 should be at line start already"
        );
    }

    /// Test that large single-line files are chunked correctly and all data is preserved.
    /// This verifies the MAX_LINE_BYTES limit works correctly with sequential data.
    #[test]
    fn test_line_iterator_large_single_line_chunked_correctly() {
        // Create content with sequential markers: "[00001][00002][00003]..."
        // Each marker is 7 bytes, so we can verify order and completeness
        let num_markers = 20_000; // ~140KB of data, spans multiple chunks
        let content: String = (1..=num_markers).map(|i| format!("[{:05}]", i)).collect();

        let content_bytes = content.as_bytes().to_vec();
        let content_len = content_bytes.len();
        let mut buffer = TextBuffer::from_bytes(content_bytes);

        // Iterate and collect all chunks
        let mut iter = buffer.line_iterator(0, 200);
        let mut all_content = String::new();
        let mut chunk_count = 0;
        let mut chunk_sizes = Vec::new();

        while let Some((pos, chunk)) = iter.next_line() {
            // Verify chunk starts at expected position
            assert_eq!(
                pos,
                all_content.len(),
                "Chunk {} should start at byte {}",
                chunk_count,
                all_content.len()
            );

            // Verify chunk is within MAX_LINE_BYTES limit
            assert!(
                chunk.len() <= super::MAX_LINE_BYTES,
                "Chunk {} exceeds MAX_LINE_BYTES: {} > {}",
                chunk_count,
                chunk.len(),
                super::MAX_LINE_BYTES
            );

            chunk_sizes.push(chunk.len());
            all_content.push_str(&chunk);
            chunk_count += 1;
        }

        // Verify all content was retrieved
        assert_eq!(
            all_content.len(),
            content_len,
            "Total content length should match original"
        );
        assert_eq!(
            all_content, content,
            "Reconstructed content should match original"
        );

        // With 140KB of data and 100KB limit, should have 2 chunks
        assert!(
            chunk_count >= 2,
            "Should have multiple chunks for {}KB content (got {})",
            content_len / 1024,
            chunk_count
        );

        // Verify sequential markers are all present and in order
        for i in 1..=num_markers {
            let marker = format!("[{:05}]", i);
            assert!(
                all_content.contains(&marker),
                "Missing marker {} in reconstructed content",
                marker
            );
        }

        // Verify markers are in correct order by checking a sample
        let pos_1000 = all_content.find("[01000]").unwrap();
        let pos_2000 = all_content.find("[02000]").unwrap();
        let pos_10000 = all_content.find("[10000]").unwrap();
        assert!(
            pos_1000 < pos_2000 && pos_2000 < pos_10000,
            "Markers should be in sequential order"
        );
    }
}
