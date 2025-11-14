use crate::piece_tree::Position;
use crate::text_buffer::TextBuffer;

/// Iterator over lines in a TextBuffer with bidirectional support
/// Uses piece iterator for efficient sequential scanning (ONE O(log n) initialization)
///
/// TODO: For huge file mode (>100MB), reconsider this design:
/// - Current implementation scans byte-by-byte when line metadata unavailable
/// - This is inefficient: get_text_range(pos, 1) per byte = many piece tree lookups
/// - Alternative approaches:
///   1. Bulk load chunks (e.g., 64KB) and scan for newlines in memory
///   2. Use a different iterator that works purely with byte ranges
///   3. Cache loaded chunks to avoid redundant loads during backward scans
///   4. For huge files, consider streaming/windowed approach instead of random access
pub struct LineIterator<'a> {
    buffer: &'a TextBuffer,
    /// Current byte position in the document (points to start of current line)
    current_pos: usize,
    buffer_len: usize,
}

impl<'a> LineIterator<'a> {
    pub(crate) fn new(buffer: &'a TextBuffer, byte_pos: usize) -> Self {
        let buffer_len = buffer.len();
        let byte_pos = byte_pos.min(buffer_len);

        // Find the start of the line containing byte_pos
        let line_start = if byte_pos == 0 {
            0
        } else {
            // Try using offset_to_position first (fast if line metadata is available)
            match buffer.offset_to_position(byte_pos) {
                Some(pos) => buffer.position_to_offset(Position {
                    line: pos.line,
                    column: 0,
                }),
                None => {
                    // Line metadata not available - scan backwards to find newline
                    // This handles large files with lazy loading
                    // TODO: This is inefficient for huge files - see struct documentation

                    // Scan backwards from byte_pos to find the previous newline
                    let mut scan_pos = byte_pos;
                    while scan_pos > 0 {
                        scan_pos -= 1;
                        // Get one byte at this position
                        if let Some(bytes) = buffer.get_text_range(scan_pos, 1) {
                            if !bytes.is_empty() && bytes[0] == b'\n' {
                                // Found newline - line starts at next byte
                                let line_start = scan_pos + 1;
                                return LineIterator {
                                    buffer,
                                    current_pos: line_start,
                                    buffer_len,
                                };
                            }
                        }
                        // Don't scan too far back - limit to reasonable line length
                        if byte_pos - scan_pos > 10000 {
                            break;
                        }
                    }
                    // Either hit start of file or gave up - start from here
                    scan_pos
                }
            }
        };

        LineIterator {
            buffer,
            current_pos: line_start,
            buffer_len,
        }
    }

    /// Get the next line (moving forward)
    /// Uses piece iterator for efficient sequential scanning
    pub fn next(&mut self) -> Option<(usize, String)> {
        if self.current_pos >= self.buffer_len {
            return None;
        }

        let line_start = self.current_pos;

        // Use piece iterator to scan for newline - amortized O(1) per line
        let pieces = self
            .buffer
            .piece_tree_ref()
            .iter_pieces_in_range(self.current_pos, self.buffer_len);

        let mut line_bytes = Vec::new();
        let mut found_newline = false;
        let mut bytes_scanned = 0;

        for piece in pieces {
            let buffer = &self.buffer.buffers_ref()[piece.location.buffer_id()];

            // Calculate where to start reading within this piece
            let start_offset_in_doc = piece.doc_offset.max(self.current_pos);
            let offset_in_piece = start_offset_in_doc - piece.doc_offset;
            let start_in_buffer = piece.buffer_offset + offset_in_piece;
            let bytes_to_read = piece.bytes - offset_in_piece;

            let buffer_data = match buffer.get_data() {
                Some(data) => data,
                None => continue, // Buffer not loaded, skip
            };
            let piece_data = &buffer_data[start_in_buffer..start_in_buffer + bytes_to_read];

            // Scan this piece for newline
            for &byte in piece_data.iter() {
                line_bytes.push(byte);
                bytes_scanned += 1;

                if byte == b'\n' {
                    found_newline = true;
                    break;
                }
            }

            if found_newline {
                break;
            }
        }

        // Move to next line
        self.current_pos += bytes_scanned;

        let line_string = String::from_utf8_lossy(&line_bytes).into_owned();
        Some((line_start, line_string))
    }

    /// Get the previous line (moving backward)
    /// Falls back to piece tree lookup for backwards navigation
    pub fn prev(&mut self) -> Option<(usize, String)> {
        if self.current_pos == 0 {
            return None;
        }

        // Convert current position to line number, then get previous line
        let current_line = match self.buffer.offset_to_position(self.current_pos) {
            Some(pos) => pos.line,
            None => {
                // Can't determine line - scan backwards byte-by-byte
                if self.current_pos == 0 {
                    return None;
                }
                // Move back one byte and try to find previous line boundary
                self.current_pos -= 1;
                while self.current_pos > 0 {
                    if let Some(bytes) = self.buffer.get_text_range(self.current_pos, 1) {
                        if !bytes.is_empty() && bytes[0] == b'\n' {
                            // Found a newline - scan back to find the start of THIS line
                            let line_end = self.current_pos;
                            self.current_pos = self.current_pos.saturating_sub(1);
                            while self.current_pos > 0 {
                                if let Some(bytes) =
                                    self.buffer.get_text_range(self.current_pos - 1, 1)
                                {
                                    if !bytes.is_empty() && bytes[0] == b'\n' {
                                        // Found start of line
                                        break;
                                    }
                                }
                                self.current_pos -= 1;
                            }
                            // Get the line content
                            if let Some(line_bytes) = self
                                .buffer
                                .get_text_range(self.current_pos, line_end - self.current_pos + 1)
                            {
                                return Some((
                                    self.current_pos,
                                    String::from_utf8_lossy(&line_bytes).into_owned(),
                                ));
                            }
                        }
                    }
                    self.current_pos -= 1;
                }
                return None;
            }
        };

        if current_line == 0 {
            return None;
        }

        let prev_line = current_line - 1;

        // Get the previous line's range
        let (line_start, line_end) = self
            .buffer
            .piece_tree_ref()
            .line_range(prev_line, self.buffer.buffers_ref())?;

        let line_len =
            line_end.map_or_else(|| self.buffer_len - line_start, |end| end - line_start);

        // Use piece iterator to get line content
        let mut line_bytes = Vec::new();
        for piece in self
            .buffer
            .piece_tree_ref()
            .iter_pieces_in_range(line_start, line_start + line_len)
        {
            let buffer = &self.buffer.buffers_ref()[piece.location.buffer_id()];

            // Calculate which part of this piece overlaps with our line
            let piece_line_start = line_start.max(piece.doc_offset);
            let piece_line_end = (line_start + line_len).min(piece.doc_offset + piece.bytes);

            let offset_in_piece = piece_line_start - piece.doc_offset;
            let len_in_piece = piece_line_end - piece_line_start;

            if let Some(buffer_data) = buffer.get_data() {
                let start_in_buffer = piece.buffer_offset + offset_in_piece;
                let data = &buffer_data[start_in_buffer..start_in_buffer + len_in_piece];
                line_bytes.extend_from_slice(data);
            }
        }

        self.current_pos = line_start;
        let line_string = String::from_utf8_lossy(&line_bytes).into_owned();
        Some((line_start, line_string))
    }

    /// Get the current position in the buffer (byte offset of current line start)
    pub fn current_position(&self) -> usize {
        self.current_pos
    }
}
