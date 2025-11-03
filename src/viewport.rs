use crate::buffer::{Buffer, LineNumber};
use crate::cursor::Cursor;

/// The viewport - what portion of the buffer is visible
#[derive(Debug, Clone)]
pub struct Viewport {
    /// Byte position of the first visible line
    /// **This is the authoritative source of truth for all viewport operations**
    pub top_byte: usize,

    /// Line number of first visible line (for display/debugging only)
    /// May be Absolute (known) or Relative (estimated)
    /// **Never use this for logic - always use top_byte**
    pub top_line: LineNumber,

    /// Left column offset (horizontal scroll position)
    pub left_column: usize,

    /// Terminal dimensions
    pub width: u16,
    pub height: u16,

    /// Scroll offset (lines to keep visible above/below cursor)
    pub scroll_offset: usize,

    /// Horizontal scroll offset (columns to keep visible left/right of cursor)
    pub horizontal_scroll_offset: usize,
}

impl Viewport {
    /// Create a new viewport
    pub fn new(width: u16, height: u16) -> Self {
        Self {
            top_byte: 0,
            top_line: LineNumber::Absolute(0),
            left_column: 0,
            width,
            height,
            scroll_offset: 3,
            horizontal_scroll_offset: 5,
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

    /// Get the bottom line (exclusive) - uses top_line which may be estimated
    pub fn bottom_line(&self) -> usize {
        self.top_line.value() + self.visible_line_count()
    }

    /// Get the range of visible lines - returns line numbers for compatibility
    /// Note: This uses top_line which may be estimated for large files
    pub fn visible_range(&self) -> std::ops::Range<usize> {
        self.top_line.value()..self.bottom_line()
    }

    /// Calculate the gutter width based on the maximum visible line number
    /// Format is: "{:>N} │ " where N is the number of digits
    /// Total width = N + 3 (for " │ ")
    pub fn gutter_width(&self) -> usize {
        let max_line = self.bottom_line();
        let digits = if max_line == 0 {
            1
        } else {
            // Calculate number of digits needed
            // Line numbers are 1-indexed for display, so add 1
            ((max_line as f64).log10().floor() as usize) + 1
        };
        // Minimum 4 digits for readability, plus " │ " = 3 chars
        digits.max(4) + 3
    }

    /// Check if a line is visible
    pub fn is_line_visible(&self, line: usize) -> bool {
        line >= self.top_line.value() && line < self.bottom_line()
    }

    /// Scroll up by N lines (byte-based)
    pub fn scroll_up(&mut self, buffer: &Buffer, lines: usize) {
        let mut iter = buffer.line_iterator(self.top_byte);
        for _ in 0..lines {
            if iter.prev().is_none() {
                break;
            }
        }
        self.top_byte = iter.current_position();
        self.top_line = buffer.byte_to_line_lazy(self.top_byte);
    }

    /// Scroll down by N lines (byte-based)
    pub fn scroll_down(&mut self, buffer: &Buffer, lines: usize) {
        let mut iter = buffer.line_iterator(self.top_byte);
        for _ in 0..lines {
            if iter.next().is_none() {
                break;
            }
        }
        self.top_byte = iter.current_position();
        self.top_line = buffer.byte_to_line_lazy(self.top_byte);
    }

    /// Scroll to a specific line (byte-based)
    /// This will scroll to the byte position of that line if cached, or estimate
    pub fn scroll_to(&mut self, buffer: &Buffer, line: usize) {
        // Try to get byte position for this line
        let byte_pos = buffer.line_to_byte(line);
        self.top_byte = byte_pos;
        self.top_line = buffer.byte_to_line_lazy(self.top_byte);
    }

    /// Ensure a cursor is visible, scrolling if necessary (smart scroll)
    pub fn ensure_visible(&mut self, buffer: &mut Buffer, cursor: &Cursor) {
        // Find the start of the line containing the cursor
        let cursor_line_start = buffer.find_line_start_at_byte(cursor.position);

        // Check if cursor line is visible by iterating from top_byte
        let visible_count = self.visible_line_count();
        let mut iter = buffer.line_iterator(self.top_byte);
        let mut lines_from_top = 0;
        let mut cursor_is_visible = false;

        while let Some((line_byte, _)) = iter.next() {
            if line_byte == cursor_line_start {
                cursor_is_visible = lines_from_top < visible_count;
                break;
            }
            lines_from_top += 1;
            if lines_from_top >= visible_count {
                break;
            }
        }

        // If cursor is not visible, scroll to make it visible
        if !cursor_is_visible {
            // Position cursor in the middle of the viewport with scroll offset
            let target_line_from_top = (visible_count / 2).min(self.scroll_offset);

            // Move backwards from cursor to find the new top_byte
            let mut iter = buffer.line_iterator(cursor_line_start);
            for _ in 0..target_line_from_top {
                if iter.prev().is_none() {
                    break;
                }
            }
            self.top_byte = iter.current_position();
        }

        // Update top_line for display/debugging (may be absolute or relative)
        self.top_line = buffer.byte_to_line_lazy(self.top_byte);

        // Horizontal scrolling
        let cursor_column = cursor.position.saturating_sub(cursor_line_start);
        self.ensure_column_visible(cursor_column);
    }

    /// Ensure a line is visible with scroll offset applied
    /// This is a legacy method kept for backward compatibility with tests
    /// In practice, use ensure_visible() which works directly with cursors and bytes
    pub fn ensure_line_visible(&mut self, buffer: &Buffer, line: usize) {
        // Convert line number to byte position
        let target_byte = buffer.line_to_byte(line);

        // Check if the line is already visible by iterating from top_byte
        let visible_count = self.visible_line_count();
        let mut iter = buffer.line_iterator(self.top_byte);
        let mut lines_from_top = 0;
        let mut target_is_visible = false;

        // Find the actual line start for the target
        let target_line_byte = buffer.find_line_start_at_byte(target_byte);

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
            self.top_byte = iter.current_position();
        }

        // Update top_line for display
        self.top_line = buffer.byte_to_line_lazy(self.top_byte);
    }

    /// Ensure a column is visible with horizontal scroll offset applied
    pub fn ensure_column_visible(&mut self, column: usize) {
        // Calculate visible width (accounting for line numbers gutter which is dynamic)
        let gutter_width = self.gutter_width();
        let visible_width = (self.width as usize).saturating_sub(gutter_width);

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
            self.left_column = column.saturating_sub(visible_width.saturating_sub(effective_offset));
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
        let mut sorted_cursors: Vec<_> = cursors.iter().copied().collect();
        sorted_cursors.sort_by_key(|(priority, _)| *priority);

        // Get byte positions for all cursors (at line starts)
        let cursor_line_bytes: Vec<usize> = sorted_cursors
            .iter()
            .map(|(_, cursor)| buffer.find_line_start_at_byte(cursor.position))
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
            self.top_byte = iter.current_position();
        } else {
            // Can't fit all cursors, ensure primary is visible
            let primary_cursor = sorted_cursors[0].1;
            self.ensure_visible(buffer, primary_cursor);
        }

        // Update top_line for display
        self.top_line = buffer.byte_to_line_lazy(self.top_byte);
    }

    /// Convert a screen row to a buffer line number
    pub fn screen_row_to_line(&self, row: u16) -> usize {
        self.top_line.value() + row as usize
    }

    /// Convert a buffer line number to a screen row (if visible)
    pub fn line_to_screen_row(&self, line: usize) -> Option<u16> {
        if self.is_line_visible(line) {
            Some((line - self.top_line.value()) as u16)
        } else {
            None
        }
    }

    /// Get the cursor screen position (x, y) which is (col, row) for rendering
    /// This returns the position relative to the viewport, accounting for horizontal scrolling
    pub fn cursor_screen_position(&self, buffer: &mut Buffer, cursor: &Cursor) -> (u16, u16) {
        let line_number = buffer.byte_to_line_lazy(cursor.position);
        let line = line_number.value();

        // Use byte-based approach for finding line start to avoid conversion
        let line_start = buffer.find_line_start_at_byte(cursor.position);
        let column = cursor.position.saturating_sub(line_start);

        let screen_row = line.saturating_sub(self.top_line.value()) as u16;
        // Account for horizontal scrolling - subtract left_column offset
        let screen_col = column.saturating_sub(self.left_column) as u16;

        // Return (x, y) which is (col, row)
        (screen_col, screen_row)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::buffer::Buffer;
    use crate::cursor::Cursor;

    #[test]
    fn test_viewport_new() {
        let vp = Viewport::new(80, 24);
        assert_eq!(vp.width, 80);
        assert_eq!(vp.height, 24);
        assert_eq!(vp.top_line.value(), 0);
        assert_eq!(vp.top_byte, 0);
    }

    #[test]
    fn test_visible_range() {
        let vp = Viewport::new(80, 24);
        assert_eq!(vp.visible_range(), 0..24);
    }

    #[test]
    fn test_scroll_up_down() {
        let mut buffer = Buffer::from_str("line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\nline11\nline12\nline13\nline14\nline15");
        let mut vp = Viewport::new(80, 24);

        vp.scroll_down(&buffer, 10);
        assert_eq!(vp.top_line.value(), 10);

        vp.scroll_up(&buffer, 5);
        assert_eq!(vp.top_line.value(), 5);

        vp.scroll_up(&buffer, 10);
        assert_eq!(vp.top_line.value(), 0); // Can't scroll past 0
    }

    #[test]
    fn test_ensure_line_visible() {
        let buffer = Buffer::from_str("line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\nline11\nline12\nline13\nline14\nline15\nline16\nline17\nline18\nline19\nline20\nline21\nline22\nline23\nline24\nline25\nline26\nline27\nline28\nline29\nline30\nline31\nline32\nline33\nline34\nline35\nline36\nline37\nline38\nline39\nline40\nline41\nline42\nline43\nline44\nline45\nline46\nline47\nline48\nline49\nline50\nline51");
        let mut vp = Viewport::new(80, 24);
        vp.scroll_offset = 3;

        // Line within scroll offset should adjust viewport
        vp.ensure_line_visible(&buffer, 2);
        assert!(vp.top_line.value() <= 2);

        // Line far below should scroll down
        vp.ensure_line_visible(&buffer, 50);
        assert!(vp.top_line.value() > 0);
        assert!(vp.is_line_visible(50));
    }

    #[test]
    fn test_ensure_visible_with_cursor() {
        let mut buffer = Buffer::from_str("line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\nline11\nline12\nline13\nline14\nline15\nline16\nline17\nline18\nline19\nline20");
        let mut vp = Viewport::new(80, 10);

        // Cursor at line 15 should scroll viewport
        let cursor_pos = buffer.line_to_byte(15);
        let cursor = Cursor::new(cursor_pos);
        vp.ensure_visible(&mut buffer, &cursor);

        let cursor_line_number = buffer.byte_to_line_lazy(cursor_pos);
        assert!(vp.is_line_visible(cursor_line_number.value()));
    }

    #[test]
    fn test_cursor_screen_position() {
        let mut buffer = Buffer::from_str("line1\nline2\nline3");
        let vp = Viewport::new(80, 24);

        let cursor = Cursor::new(6); // Start of line 1 ("line2")
        let (x, y) = vp.cursor_screen_position(&mut buffer, &cursor);
        // x is column (horizontal), y is row (vertical)
        assert_eq!(x, 0); // Column 0 (start of line)
        assert_eq!(y, 1); // Row 1 (second line, since top_line is 0)
    }
}
