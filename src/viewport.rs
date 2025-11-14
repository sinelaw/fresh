use crate::cursor::Cursor;
use crate::line_wrapping::{char_position_to_segment, wrap_line, WrapConfig};
use crate::text_buffer::Buffer;
/// The viewport - what portion of the buffer is visible
#[derive(Debug, Clone)]
pub struct Viewport {
    /// Byte position of the first visible line
    /// **This is the authoritative source of truth for all viewport operations**
    /// The line number for this byte is obtained from Buffer's LineCache
    pub top_byte: usize,

    /// Left column offset (horizontal scroll position)
    pub left_column: usize,

    /// Terminal dimensions
    pub width: u16,
    pub height: u16,

    /// Scroll offset (lines to keep visible above/below cursor)
    pub scroll_offset: usize,

    /// Horizontal scroll offset (columns to keep visible left/right of cursor)
    pub horizontal_scroll_offset: usize,

    /// Whether line wrapping is enabled
    /// When true, horizontal scrolling is disabled
    pub line_wrap_enabled: bool,

    /// Whether viewport needs synchronization with cursor positions
    /// When true, ensure_visible needs to be called before rendering
    /// This allows batching multiple cursor movements into a single viewport update
    needs_sync: bool,
}

impl Viewport {
    /// Create a new viewport
    pub fn new(width: u16, height: u16) -> Self {
        Self {
            top_byte: 0,
            left_column: 0,
            width,
            height,
            scroll_offset: 3,
            horizontal_scroll_offset: 5,
            line_wrap_enabled: false,
            needs_sync: false,
        }
    }

    /// Set the scroll offset
    pub fn set_scroll_offset(&mut self, offset: usize) {
        self.scroll_offset = offset;
    }

    /// Update terminal dimensions
    pub fn resize(&mut self, width: u16, height: u16) {
        self.width = width;
        self.height = height;
    }

    /// Get the number of visible lines
    pub fn visible_line_count(&self) -> usize {
        self.height as usize
    }

    /// Calculate the gutter width based on buffer length
    /// Format: "[indicator]{:>N} │ " where N is the number of digits for line numbers
    /// - Indicator column: 1 char (space, or symbols like ●/✗/⚠)
    /// - Line numbers: N digits (min 4), right-aligned
    /// - Separator: " │ " = 3 chars (space, box char, space)
    /// Total width = 1 + N + 3 = N + 4 (where N >= 4 minimum, so min 8 total)
    /// This is a heuristic that assumes approximately 80 chars per line
    pub fn gutter_width(&self, buffer: &Buffer) -> usize {
        let buffer_len = buffer.len();
        let estimated_lines = (buffer_len / 80).max(1);
        let digits = if estimated_lines == 0 {
            1
        } else {
            ((estimated_lines as f64).log10().floor() as usize) + 1
        };
        // 1 (indicator) + minimum 4 digits for readability + 3 (" │ ")
        1 + digits.max(4) + 3
    }

    /// Scroll up by N lines (byte-based)
    /// LineCache automatically tracks line numbers
    pub fn scroll_up(&mut self, buffer: &Buffer, lines: usize) {
        let mut iter = buffer.line_iterator(self.top_byte);
        for _ in 0..lines {
            if iter.prev().is_none() {
                break;
            }
        }
        self.set_top_byte_with_limit(buffer, iter.current_position());
    }

    /// Scroll down by N lines (byte-based)
    /// LineCache automatically tracks line numbers
    pub fn scroll_down(&mut self, buffer: &Buffer, lines: usize) {
        let mut iter = buffer.line_iterator(self.top_byte);
        for _ in 0..lines {
            if iter.next().is_none() {
                break;
            }
        }
        self.set_top_byte_with_limit(buffer, iter.current_position());
    }

    /// Set top_byte with automatic scroll limit enforcement
    /// This prevents scrolling past the end of the buffer by ensuring
    /// the viewport can be filled from the proposed position
    fn set_top_byte_with_limit(&mut self, buffer: &Buffer, proposed_top_byte: usize) {
        tracing::trace!(
            "DEBUG set_top_byte_with_limit: proposed_top_byte={}",
            proposed_top_byte
        );

        let viewport_height = self.visible_line_count();
        if viewport_height == 0 {
            self.top_byte = proposed_top_byte;
            return;
        }

        let buffer_len = buffer.len();
        if buffer_len == 0 {
            self.top_byte = 0;
            return;
        }

        // Try to iterate viewport_height lines from proposed_top_byte
        // If we can't reach viewport_height lines before hitting EOF,
        // then we need to adjust backward
        let mut iter = buffer.line_iterator(proposed_top_byte);
        let mut lines_visible = 0;

        while let Some((_, _)) = iter.next() {
            lines_visible += 1;
            if lines_visible >= viewport_height {
                // We have a full viewport of content, use proposed position
                tracing::trace!(
                    "DEBUG: Full viewport available, setting top_byte={}",
                    proposed_top_byte
                );
                self.top_byte = proposed_top_byte;
                return;
            }
        }

        tracing::trace!(
            "DEBUG: After iteration, lines_visible={}, viewport_height={}",
            lines_visible,
            viewport_height
        );

        // Check if buffer ends with newline (which creates a phantom empty line)
        let buffer_ends_with_newline = buffer_len > 0 && {
            let last_byte_slice = buffer.slice_bytes(buffer_len - 1..buffer_len);
            !last_byte_slice.is_empty() && last_byte_slice[0] == b'\n'
        };

        tracing::trace!(
            "DEBUG: buffer_ends_with_newline={}",
            buffer_ends_with_newline
        );

        // Account for the phantom line if buffer ends with newline
        if buffer_ends_with_newline {
            lines_visible += 1;
            tracing::trace!(
                "DEBUG: After adding phantom line, lines_visible={}",
                lines_visible
            );
        }

        // If we have enough lines to fill the viewport, we're good
        if lines_visible >= viewport_height {
            tracing::trace!(
                "DEBUG: Enough lines to fill viewport, setting top_byte={}",
                proposed_top_byte
            );
            self.top_byte = proposed_top_byte;
            return;
        }

        // We don't have enough lines to fill the viewport from proposed_top_byte
        // Calculate how many lines we're short and scroll back
        let lines_short = viewport_height - lines_visible;
        tracing::trace!("DEBUG: lines_short={}, scrolling back", lines_short);

        let mut backtrack_iter = buffer.line_iterator(proposed_top_byte);
        tracing::trace!(
            "DEBUG: Backtracking from byte {}",
            backtrack_iter.current_position()
        );
        for i in 0..lines_short {
            let pos_before = backtrack_iter.current_position();
            if backtrack_iter.prev().is_none() {
                tracing::trace!(
                    "DEBUG: Hit beginning of buffer at backtrack iteration {}",
                    i
                );
                break; // Hit the beginning of the buffer
            }
            let pos_after = backtrack_iter.current_position();
            tracing::trace!(
                "DEBUG: Backtrack iteration {}: {} -> {}",
                i,
                pos_before,
                pos_after
            );
        }

        let final_top_byte = backtrack_iter.current_position();
        tracing::trace!(
            "DEBUG: After backtracking, setting top_byte={}",
            final_top_byte
        );
        self.top_byte = final_top_byte;
    }

    /// Scroll to a specific line (byte-based)
    /// This seeks from the beginning to find the byte position of the line
    pub fn scroll_to(&mut self, buffer: &Buffer, line: usize) {
        // Seek from the beginning to find the byte position for this line
        let mut iter = buffer.line_iterator(0);
        let mut current_line = 0;

        while current_line < line {
            if let Some((line_start, _)) = iter.next() {
                if current_line + 1 == line {
                    self.set_top_byte_with_limit(buffer, line_start);
                    return;
                }
                current_line += 1;
            } else {
                // Reached end of buffer before target line
                break;
            }
        }

        // If we didn't find the line, stay at the last valid position
        self.set_top_byte_with_limit(buffer, iter.current_position());
    }

    /// Mark viewport as needing synchronization with cursor positions
    /// This defers the actual viewport update until sync_with_cursor is called
    pub fn mark_needs_sync(&mut self) {
        self.needs_sync = true;
    }

    /// Check if viewport needs synchronization
    pub fn needs_sync(&self) -> bool {
        self.needs_sync
    }

    /// Synchronize viewport with cursor position (deferred ensure_visible)
    /// This should be called before rendering to batch multiple cursor movements
    pub fn sync_with_cursor(&mut self, buffer: &mut Buffer, cursor: &Cursor) {
        if self.needs_sync {
            self.ensure_visible(buffer, cursor);
            self.needs_sync = false;
        }
    }

    /// Ensure a cursor is visible, scrolling if necessary (smart scroll)
    /// This now uses ONLY the LineCache - no manual line counting
    pub fn ensure_visible(&mut self, buffer: &mut Buffer, cursor: &Cursor) {
        eprintln!("DEBUG ensure_visible: cursor.position={}, top_byte={}", cursor.position, self.top_byte);

        // For large files with lazy loading, ensure data around cursor is loaded
        // before we try to calculate line numbers and iterate
        let viewport_lines = self.visible_line_count().max(1);
        eprintln!("DEBUG prepare_viewport: cursor.position={}, viewport_lines={}", cursor.position, viewport_lines);

        // Load data BEFORE the cursor (for cases like jumping to EOF)
        // Estimate bytes per line conservatively
        let estimated_viewport_bytes = viewport_lines * 200;
        let load_start = cursor.position.saturating_sub(estimated_viewport_bytes);
        let load_length = estimated_viewport_bytes.min(cursor.position - load_start) + viewport_lines * 200;

        eprintln!("DEBUG loading range: {}..{}", load_start, load_start + load_length);
        if let Err(e) = buffer.prepare_viewport(load_start, viewport_lines * 2) {
            tracing::warn!("Failed to prepare viewport around cursor at {}: {}", cursor.position, e);
            eprintln!("ERROR prepare_viewport failed: {}", e);
            // Continue anyway - we'll work with whatever data is available
        }

        // Find the start of the line containing the cursor using iterator
        let cursor_iter = buffer.line_iterator(cursor.position);
        let cursor_line_start = cursor_iter.current_position();
        eprintln!("DEBUG cursor_line_start={}", cursor_line_start);

        // Get line numbers from the cache
        let top_line_number = buffer.get_line_number(self.top_byte);
        let cursor_line_number = buffer.get_line_number(cursor_line_start);
        eprintln!("DEBUG top_line_number={}, cursor_line_number={}", top_line_number, cursor_line_number);

        // Check if cursor line is visible
        let visible_count = self.visible_line_count();
        let lines_from_top = cursor_line_number.saturating_sub(top_line_number);

        tracing::trace!(
            "DEBUG ensure_visible: cursor_pos={}, cursor_line_start={}, top_byte={}, top_line={}, cursor_line={}, visible_count={}, lines_from_top={}",
            cursor.position, cursor_line_start, self.top_byte, top_line_number, cursor_line_number, visible_count, lines_from_top
        );

        // Scroll if cursor is beyond the visible area
        // Must also check cursor is not above viewport (saturating_sub would make it appear at line 0)
        let cursor_is_visible =
            cursor_line_number >= top_line_number && lines_from_top < visible_count;

        tracing::trace!(
            "DEBUG ensure_visible: cursor_is_visible={}",
            cursor_is_visible
        );

        // If cursor is not visible, scroll to make it visible
        if !cursor_is_visible {
            tracing::trace!("DEBUG: Scrolling to make cursor visible!");

            // Position cursor at center of viewport when jumping
            let target_line_from_top = self.visible_line_count() / 2;
            tracing::trace!("DEBUG: target_line_from_top={}", target_line_from_top);

            // Move backwards from cursor to find the new top_byte
            let mut iter = buffer.line_iterator(cursor_line_start);
            tracing::trace!(
                "DEBUG: Starting iteration from cursor_line_start={}, iter.current_position()={}",
                cursor_line_start,
                iter.current_position()
            );

            for i in 0..target_line_from_top {
                if iter.prev().is_none() {
                    tracing::trace!("DEBUG: Hit beginning of buffer at iteration {}", i);
                    break; // Hit beginning of buffer
                }
                tracing::trace!(
                    "DEBUG: After prev() iteration {}: iter.current_position()={}",
                    i,
                    iter.current_position()
                );
            }

            let new_top_byte = iter.current_position();
            tracing::trace!(
                "DEBUG: Calling set_top_byte_with_limit with new_top_byte={}",
                new_top_byte
            );
            self.set_top_byte_with_limit(buffer, new_top_byte);
            tracing::trace!(
                "DEBUG: After set_top_byte_with_limit, self.top_byte={}",
                self.top_byte
            );
        }

        // Horizontal scrolling - skip if line wrapping is enabled
        // When wrapping is enabled, all columns are always visible via wrapping
        if !self.line_wrap_enabled {
            let cursor_column = cursor.position.saturating_sub(cursor_line_start);

            // Get the line content to know its length (for limiting horizontal scroll)
            let mut line_iter = buffer.line_iterator(cursor_line_start);
            let line_length = if let Some((_start, content)) = line_iter.next() {
                // Line length without the newline character
                content.trim_end_matches('\n').len()
            } else {
                0
            };

            self.ensure_column_visible(cursor_column, line_length, buffer);
        } else {
            // With line wrapping enabled, reset any horizontal scroll
            self.left_column = 0;
        }
    }

    /// Ensure a line is visible with scroll offset applied
    /// This is a legacy method kept for backward compatibility with tests
    /// In practice, use ensure_visible() which works directly with cursors and bytes
    pub fn ensure_line_visible(&mut self, buffer: &Buffer, line: usize) {
        // Seek to the target line to get its byte position
        let mut seek_iter = buffer.line_iterator(0);
        let mut current_line = 0;
        let mut target_line_byte = 0;

        while current_line < line {
            if let Some((line_start, _)) = seek_iter.next() {
                if current_line + 1 == line {
                    target_line_byte = line_start;
                    break;
                }
                current_line += 1;
            } else {
                // Reached end of buffer before target line
                return;
            }
        }

        // Check if the line is already visible by iterating from top_byte
        let visible_count = self.visible_line_count();
        let mut iter = buffer.line_iterator(self.top_byte);
        let mut lines_from_top = 0;
        let mut target_is_visible = false;

        while let Some((line_byte, _)) = iter.next() {
            if line_byte == target_line_byte {
                target_is_visible = lines_from_top < visible_count;
                break;
            }
            lines_from_top += 1;
            if lines_from_top >= visible_count {
                break;
            }
        }

        // If not visible, scroll to show it with scroll offset
        if !target_is_visible {
            let effective_offset = self.scroll_offset.min(visible_count / 2);
            let target_line_from_top = effective_offset;

            // Move backwards from target to find new top_byte
            let mut iter = buffer.line_iterator(target_line_byte);
            for _ in 0..target_line_from_top {
                if iter.prev().is_none() {
                    break;
                }
            }
            self.set_top_byte_with_limit(buffer, iter.current_position());
        }
    }

    /// Ensure a column is visible with horizontal scroll offset applied
    ///
    /// # Arguments
    /// * `column` - The column position within the line (0-indexed)
    /// * `line_length` - The length of the line content (without newline)
    /// * `buffer` - The buffer (for calculating gutter width)
    pub fn ensure_column_visible(&mut self, column: usize, line_length: usize, buffer: &Buffer) {
        // Calculate visible width (accounting for line numbers gutter which is dynamic)
        let gutter_width = self.gutter_width(buffer);
        // Also account for scrollbar (always present, takes 1 column)
        let scrollbar_width = 1;
        let visible_width = (self.width as usize)
            .saturating_sub(gutter_width)
            .saturating_sub(scrollbar_width);

        if visible_width == 0 {
            return; // Terminal too narrow
        }

        // If viewport is too small for scroll offset, use what we can
        let effective_offset = self.horizontal_scroll_offset.min(visible_width / 2);

        // Calculate the ideal left and right boundaries with scroll offset
        let ideal_left = self.left_column + effective_offset;
        let ideal_right = self.left_column + visible_width.saturating_sub(effective_offset);

        if column < ideal_left {
            // Cursor is to the left of the ideal zone - scroll left
            self.left_column = column.saturating_sub(effective_offset);
        } else if column >= ideal_right {
            // Cursor is to the right of the ideal zone - scroll right
            // Place cursor at (visible_width - effective_offset - 1) to keep it in valid range [0, visible_width-1]
            let target_position = visible_width
                .saturating_sub(effective_offset)
                .saturating_sub(1);
            self.left_column = column.saturating_sub(target_position);
        }

        // BUGFIX: Limit left_column to ensure content is always visible
        // Don't scroll past the point where the end of the line would be off-screen to the left
        // This prevents the viewport from scrolling into "empty space" past the line content
        if line_length > 0 {
            // Calculate the maximum left_column that still shows some content
            // Account for cursor potentially being one position past the line content (at position line_length)
            // If the line is shorter than visible width, left_column should be 0
            // Otherwise, allow scrolling enough to show position line_length at the last visible column
            let max_left_column = line_length.saturating_sub(visible_width.saturating_sub(1));

            // Limit left_column to max_left_column
            if self.left_column > max_left_column {
                self.left_column = max_left_column;
            }
        }
    }

    /// Ensure multiple cursors are visible (smart scroll for multi-cursor)
    /// Prioritizes keeping the primary cursor visible
    pub fn ensure_cursors_visible(
        &mut self,
        buffer: &mut Buffer,
        cursors: &[(usize, &Cursor)], // (priority, cursor) - lower priority number = higher priority
    ) {
        if cursors.is_empty() {
            return;
        }

        // Sort cursors by priority (primary cursor first)
        let mut sorted_cursors: Vec<_> = cursors.to_vec();
        sorted_cursors.sort_by_key(|(priority, _)| *priority);

        // Get byte positions for all cursors (at line starts)
        let cursor_line_bytes: Vec<usize> = sorted_cursors
            .iter()
            .map(|(_, cursor)| {
                let iter = buffer.line_iterator(cursor.position);
                iter.current_position()
            })
            .collect();

        // Count how many lines span between min and max cursors
        let min_byte = *cursor_line_bytes.iter().min().unwrap();
        let max_byte = *cursor_line_bytes.iter().max().unwrap();

        // Count lines between min and max using iterator
        let mut iter = buffer.line_iterator(min_byte);
        let mut line_span = 0;
        while let Some((line_byte, _)) = iter.next() {
            if line_byte >= max_byte {
                break;
            }
            line_span += 1;
        }

        let visible_count = self.visible_line_count();

        // If all cursors fit in the viewport, center them
        if line_span < visible_count {
            let lines_to_go_back = visible_count / 2;
            let mut iter = buffer.line_iterator(min_byte);
            for _ in 0..lines_to_go_back {
                if iter.prev().is_none() {
                    break;
                }
            }
            self.set_top_byte_with_limit(buffer, iter.current_position());
        } else {
            // Can't fit all cursors, ensure primary is visible
            let primary_cursor = sorted_cursors[0].1;
            self.ensure_visible(buffer, primary_cursor);
        }
    }

    /// Get the cursor screen position (x, y) which is (col, row) for rendering
    /// This returns the position relative to the viewport, accounting for horizontal scrolling
    ///
    /// NOTE: This function is kept for popup positioning and multi-cursor display,
    /// but is NO LONGER used for primary cursor rendering, which now happens during
    /// the line rendering loop in split_rendering.rs to eliminate duplicate line iteration.
    pub fn cursor_screen_position(&self, buffer: &mut Buffer, cursor: &Cursor) -> (u16, u16) {
        // Find line start using iterator
        let cursor_iter = buffer.line_iterator(cursor.position);
        let line_start = cursor_iter.current_position();
        let column = cursor.position.saturating_sub(line_start);

        // Count lines from top_byte to cursor to get screen row
        let mut iter = buffer.line_iterator(self.top_byte);
        let mut screen_row = 0;

        while let Some((line_byte, _)) = iter.next() {
            if line_byte >= line_start {
                break;
            }
            screen_row += 1;
        }

        // Calculate screen column and additional wrapped rows if line wrapping is enabled
        let (screen_col, additional_rows) = if self.line_wrap_enabled {
            // Use new clean wrapping implementation
            let gutter_width = self.gutter_width(buffer);
            let config = WrapConfig::new(self.width as usize, gutter_width, true);

            // Get the line text for wrapping
            let mut line_iter = buffer.line_iterator(line_start);
            let line_text = if let Some((_start, content)) = line_iter.next() {
                // Remove trailing newline if present
                content.trim_end_matches('\n').to_string()
            } else {
                String::new()
            };

            // Wrap the line
            let segments = wrap_line(&line_text, &config);

            // Find which segment the cursor is in
            let (segment_idx, col_in_segment) = char_position_to_segment(column, &segments);

            (col_in_segment as u16, segment_idx)
        } else {
            // No wrapping - account for horizontal scrolling
            let screen_col = column.saturating_sub(self.left_column) as u16;
            (screen_col, 0)
        };

        // Return (x, y) which is (col, row)
        // Add the additional wrapped rows to the screen row
        (screen_col, (screen_row + additional_rows) as u16)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cursor::Cursor;
    use crate::text_buffer::Buffer;

    #[test]
    fn test_viewport_new() {
        let vp = Viewport::new(80, 24);
        assert_eq!(vp.width, 80);
        assert_eq!(vp.height, 24);
        assert_eq!(vp.top_byte, 0);
    }

    #[test]
    fn test_scroll_up_down() {
        // Create a buffer with more lines than the viewport to make scrolling possible
        let mut content = String::new();
        for i in 1..=50 {
            if i > 1 {
                content.push('\n');
            }
            content.push_str(&format!("line{}", i));
        }
        let buffer = Buffer::from_str_test(&content);
        let mut vp = Viewport::new(80, 24);

        vp.scroll_down(&buffer, 10);
        // Check that we scrolled down (top_byte should be > 0)
        assert!(vp.top_byte > 0);

        let prev_top = vp.top_byte;
        vp.scroll_up(&buffer, 5);
        // Check that we scrolled up (top_byte should be less than before)
        assert!(vp.top_byte < prev_top);

        vp.scroll_up(&buffer, 100);
        assert_eq!(vp.top_byte, 0); // Can't scroll past 0
    }

    #[test]
    fn test_ensure_line_visible() {
        let buffer = Buffer::from_str_test("line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\nline11\nline12\nline13\nline14\nline15\nline16\nline17\nline18\nline19\nline20\nline21\nline22\nline23\nline24\nline25\nline26\nline27\nline28\nline29\nline30\nline31\nline32\nline33\nline34\nline35\nline36\nline37\nline38\nline39\nline40\nline41\nline42\nline43\nline44\nline45\nline46\nline47\nline48\nline49\nline50\nline51");
        let mut vp = Viewport::new(80, 24);
        vp.scroll_offset = 3;

        // Line within scroll offset should adjust viewport
        vp.ensure_line_visible(&buffer, 2);
        // top_byte should be close to the beginning since line 2 is near the top
        assert!(vp.top_byte < 100);

        // Line far below should scroll down
        vp.ensure_line_visible(&buffer, 50);
        assert!(vp.top_byte > 0);
        // Verify the line is now visible by checking we can iterate to it
        let mut iter = buffer.line_iterator(vp.top_byte);
        let mut found = false;
        for _ in 0..vp.visible_line_count() {
            if iter.next().is_none() {
                break;
            }
            found = true;
        }
        assert!(found);
    }

    #[test]
    fn test_ensure_visible_with_cursor() {
        let mut buffer = Buffer::from_str_test("line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\nline11\nline12\nline13\nline14\nline15\nline16\nline17\nline18\nline19\nline20");
        let mut vp = Viewport::new(80, 10);

        // Find byte position of line 15 using iterator
        let mut iter = buffer.line_iterator(0);
        let mut cursor_pos = 0;
        for i in 0..15 {
            if let Some((line_start, _)) = iter.next() {
                if i == 14 {
                    cursor_pos = line_start;
                    break;
                }
            }
        }

        let cursor = Cursor::new(cursor_pos);
        vp.ensure_visible(&mut buffer, &cursor);

        // Verify cursor is now visible by checking we scrolled appropriately
        assert!(vp.top_byte > 0);
    }

    #[test]
    fn test_cursor_screen_position() {
        let mut buffer = Buffer::from_str_test("line1\nline2\nline3");
        let vp = Viewport::new(80, 24);

        let cursor = Cursor::new(6); // Start of line 1 ("line2")
        let (x, y) = vp.cursor_screen_position(&mut buffer, &cursor);
        // x is column (horizontal), y is row (vertical)
        assert_eq!(x, 0); // Column 0 (start of line)
        assert_eq!(y, 1); // Row 1 (second line, since top_line is 0)
    }

    #[test]
    fn test_ensure_visible_cursor_above_viewport() {
        // Create buffer with many lines
        let mut buffer = Buffer::from_str_test("line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\nline11\nline12\nline13\nline14\nline15\nline16\nline17\nline18\nline19\nline20");
        let mut vp = Viewport::new(80, 10); // 10 lines visible

        // Scroll down to show lines 10-19 (top_byte at line 10)
        // scroll_to uses 1-based line numbers, so line 10 = argument 10
        vp.scroll_to(&buffer, 10);
        let old_top_byte = vp.top_byte;

        // Verify we scrolled to around line 10
        let top_line = buffer.get_line_number(vp.top_byte);
        assert!(
            top_line >= 9,
            "Should have scrolled down to at least line 10"
        );

        // Now move cursor to line 5 (above the viewport)
        let mut iter = buffer.line_iterator(0);
        let mut line_5_byte = 0;
        for i in 0..5 {
            if let Some((line_start, _)) = iter.next() {
                if i == 4 {
                    line_5_byte = line_start;
                    break;
                }
            }
        }
        let cursor = Cursor::new(line_5_byte);

        // Before fix, this should fail because ensure_visible doesn't detect cursor is above viewport
        vp.ensure_visible(&mut buffer, &cursor);

        // Verify that viewport scrolled up to make cursor visible
        // The viewport should now be positioned so cursor (line 5) is visible
        let new_top_line = buffer.get_line_number(vp.top_byte);
        let cursor_line = buffer.get_line_number(line_5_byte);
        assert!(
            cursor_line >= new_top_line,
            "Cursor line should be at or below top of viewport"
        );
        assert!(
            new_top_line < top_line,
            "Viewport should have scrolled up from line {}",
            top_line
        );

        // Verify cursor is within visible area
        let lines_from_top = cursor_line.saturating_sub(new_top_line);
        assert!(
            lines_from_top < vp.visible_line_count(),
            "Cursor should be within visible area"
        );

        // Verify cursor is centered (or close to center)
        let expected_center = vp.visible_line_count() / 2;
        assert!(
            lines_from_top >= expected_center - 1 && lines_from_top <= expected_center + 1,
            "Cursor should be centered in viewport, expected around {}, got {}",
            expected_center,
            lines_from_top
        );
    }

    #[test]
    fn test_ensure_visible_cursor_below_viewport_centers() {
        // Create buffer with many lines
        let mut buffer = Buffer::from_str_test("line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\nline11\nline12\nline13\nline14\nline15\nline16\nline17\nline18\nline19\nline20");
        let mut vp = Viewport::new(80, 10); // 10 lines visible

        // Start at top (line 1 visible)
        assert_eq!(vp.top_byte, 0);

        // Move cursor to line 15 (below viewport)
        let mut iter = buffer.line_iterator(0);
        let mut line_15_byte = 0;
        for i in 0..15 {
            if let Some((line_start, _)) = iter.next() {
                if i == 14 {
                    line_15_byte = line_start;
                    break;
                }
            }
        }
        let cursor = Cursor::new(line_15_byte);

        vp.ensure_visible(&mut buffer, &cursor);

        // Verify cursor is centered
        let new_top_line = buffer.get_line_number(vp.top_byte);
        let cursor_line = buffer.get_line_number(line_15_byte);
        let lines_from_top = cursor_line.saturating_sub(new_top_line);

        let expected_center = vp.visible_line_count() / 2;
        assert!(
            lines_from_top >= expected_center - 1 && lines_from_top <= expected_center + 1,
            "Cursor should be centered in viewport when jumping down, expected around {}, got {}",
            expected_center,
            lines_from_top
        );
    }
}
