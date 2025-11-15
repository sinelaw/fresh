use crate::text_buffer::TextBuffer;

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
pub struct LineIterator<'a> {
    buffer: &'a mut TextBuffer,
    /// Current byte position in the document (points to start of current line)
    current_pos: usize,
    buffer_len: usize,
    /// Estimated average line length in bytes (for large file estimation)
    estimated_line_length: usize,
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

        LineIterator {
            buffer,
            current_pos: line_start,
            buffer_len,
            estimated_line_length,
        }
    }

    /// Get the next line (moving forward)
    /// Uses lazy loading to handle unloaded buffers transparently
    pub fn next(&mut self) -> Option<(usize, String)> {
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
        if !found_newline && self.current_pos + line_len < self.buffer_len {
            // Line is longer than expected, keep loading until we find newline or EOF
            let mut extended_chunk = chunk;
            while !found_newline && self.current_pos + extended_chunk.len() < self.buffer_len {
                let additional_bytes = estimated_max_line_length
                    .min(self.buffer_len - self.current_pos - extended_chunk.len());
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
                        }
                    }
                    Err(e) => {
                        tracing::error!("LineIterator: Failed to extend chunk: {}", e);
                        break;
                    }
                }
            }

            // Use the extended chunk
            let line_bytes = &extended_chunk[..line_len];
            self.current_pos += line_len;
            let line_string = String::from_utf8_lossy(line_bytes).into_owned();
            return Some((line_start, line_string));
        }

        // Normal case: found newline or reached EOF within initial chunk
        let line_bytes = &chunk[..line_len];
        self.current_pos += line_len;
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
        let (pos, content) = iter.next().expect("Should have first line");
        assert_eq!(pos, 0);
        assert_eq!(content, "Hello\n");

        // Second line
        let (pos, content) = iter.next().expect("Should have second line");
        assert_eq!(pos, 6);
        assert_eq!(content, "World\n");

        // Third line
        let (pos, content) = iter.next().expect("Should have third line");
        assert_eq!(pos, 12);
        assert_eq!(content, "Test");

        // No more lines
        assert!(iter.next().is_none());
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
        let (pos, content) = iter.next().expect("Should have current line");
        assert_eq!(pos, 6);
        assert_eq!(content, "World\n");

        // Second next() should return next line
        let (pos, content) = iter.next().expect("Should have next line");
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

        let (pos, content) = iter.next().expect("Should have the line");
        assert_eq!(pos, 0);
        assert_eq!(content, "Only one line");

        assert!(iter.next().is_none());
        assert!(iter.prev().is_none());
    }

    #[test]
    fn test_line_iterator_empty_lines() {
        let mut buffer = TextBuffer::from_bytes(b"Line1\n\nLine3".to_vec());
        let mut iter = buffer.line_iterator(0, 80);

        let (pos, content) = iter.next().expect("First line");
        assert_eq!(pos, 0);
        assert_eq!(content, "Line1\n");

        let (pos, content) = iter.next().expect("Empty line");
        assert_eq!(pos, 6);
        assert_eq!(content, "\n");

        let (pos, content) = iter.next().expect("Third line");
        assert_eq!(pos, 7);
        assert_eq!(content, "Line3");
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
}
