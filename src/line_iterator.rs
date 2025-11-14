use crate::piece_tree::Position;
use crate::text_buffer::TextBuffer;

/// Iterator over lines in a TextBuffer with bidirectional support
/// Uses piece iterator for efficient sequential scanning (ONE O(log n) initialization)
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

        // Find the start of the line containing byte_pos using piece tree - ONE O(log n)
        let line_start = if byte_pos == 0 {
            0
        } else {
            // Use offset_to_position to find line number, then position_to_offset to get line start
            let pos = buffer.offset_to_position(byte_pos);
            eprintln!("DEBUG LineIterator::new: byte_pos={}, offset_to_position returned ({}, {})",
                byte_pos, pos.line, pos.column);
            let line_start = buffer.position_to_offset(Position {
                line: pos.line,
                column: 0,
            });
            eprintln!("DEBUG LineIterator::new: position_to_offset({}, 0) returned {}",
                pos.line, line_start);
            line_start
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
        let current_pos = self.buffer.offset_to_position(self.current_pos);
        let current_line = current_pos.line;

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
