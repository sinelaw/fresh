use crate::piece_tree::Position;
use crate::text_buffer::TextBuffer;

/// Iterator over lines in a TextBuffer with bidirectional support
/// Uses piece iterator for efficient sequential scanning (ONE O(log n) initialization)
///
/// # Performance Characteristics After Piece Tree Refactoring
///
/// The recent refactoring integrated line tracking into the piece tree via `BufferData::Loaded { line_starts: Option<Vec<usize>> }`:
/// - **Small files (< 1MB)**: `line_starts = Some(vec)` → exact line metadata available
/// - **Large files (≥ 1MB)**: `line_starts = None` → no line metadata (for performance)
///
/// ## Current Performance:
/// - **Forward iteration (`next()`)**: ✅ Efficient O(1) amortized per line using piece iterator
/// - **Initialization (`new()`)**: ⚠️ Fast path when line metadata exists, but falls back to
///   O(n * log n) byte-by-byte backward scan when metadata unavailable (lines 43-64)
/// - **Backward iteration (`prev()`)**: ⚠️ Same issue as `new()` (lines 144-182)
///
/// ## Problem:
/// When `offset_to_position()` returns `None` (large files without line metadata), we scan
/// backwards byte-by-byte with `get_text_range(pos, 1)`. Each call does a piece tree lookup
/// O(log n), so scanning N bytes = O(N * log n) total.
///
/// ## Implemented Solution:
/// **Estimation-based approach** for large files:
/// - Uses configurable `estimated_line_length` (default: 80 bytes, set in EditorConfig)
/// - Estimates line positions as `byte_offset / estimated_line_length`
/// - Accepts imprecision for large files in exchange for O(1) performance
/// - Small files still use exact line metadata from piece tree
///
/// Users can adjust `estimated_line_length` in config to match their typical file structure:
/// - Code with short lines: ~60-80 bytes
/// - Code with long lines: ~100-120 bytes
/// - Prose/documentation: ~80-100 bytes
pub struct LineIterator<'a> {
    buffer: &'a TextBuffer,
    /// Current byte position in the document (points to start of current line)
    current_pos: usize,
    buffer_len: usize,
    /// Estimated average line length in bytes (for large file estimation)
    estimated_line_length: usize,
}

impl<'a> LineIterator<'a> {
    pub(crate) fn new(buffer: &'a TextBuffer, byte_pos: usize, estimated_line_length: usize) -> Self {
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
                    // Large file without line metadata - estimate line start
                    // Uses configured estimated_line_length (default: 80 bytes)
                    // This avoids expensive O(N * log n) byte-by-byte backward scanning
                    let estimated_line = byte_pos / estimated_line_length;
                    let estimated_start = estimated_line * estimated_line_length;

                    tracing::trace!(
                        "LineIterator: Large file mode - estimating line start at byte {} for requested position {} (using avg line length: {})",
                        estimated_start,
                        byte_pos,
                        estimated_line_length
                    );

                    // Clamp to valid range
                    estimated_start.min(byte_pos)
                }
            }
        };

        LineIterator {
            buffer,
            current_pos: line_start,
            buffer_len,
            estimated_line_length,
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
                // Large file without line metadata - estimate line number using configured avg line length
                if self.current_pos == 0 {
                    return None;
                }

                let estimated_current_line = self.current_pos / self.estimated_line_length;
                if estimated_current_line == 0 {
                    // Already at first line (estimated)
                    return None;
                }

                // Estimate previous line position
                let estimated_prev_line = estimated_current_line.saturating_sub(1);
                let estimated_prev_start = estimated_prev_line * self.estimated_line_length;

                tracing::trace!(
                    "LineIterator::prev: Large file mode - estimating prev line {} at byte {} (current at {}, using avg line length: {})",
                    estimated_prev_line,
                    estimated_prev_start,
                    self.current_pos,
                    self.estimated_line_length
                );

                // Move iterator to estimated position
                self.current_pos = estimated_prev_start;

                // Read approximate line (might be partial or span multiple lines, but that's okay for large files)
                // We'll read estimated_line_length bytes forward to get the "line"
                if let Some(bytes) = self.buffer.get_text_range(estimated_prev_start, self.estimated_line_length) {
                    let line_string = String::from_utf8_lossy(&bytes).into_owned();
                    return Some((estimated_prev_start, line_string));
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
