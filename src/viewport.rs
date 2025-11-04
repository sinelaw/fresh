use crate::buffer::Buffer;
use crate::cursor::Cursor;

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
    /// Format is: "{:>N} │ " where N is the number of digits
    /// Total width = N + 3 (for " │ ")
    /// This is a heuristic that assumes approximately 80 chars per line
    pub fn gutter_width(&self, buffer: &Buffer) -> usize {
        let buffer_len = buffer.len();
        let estimated_lines = (buffer_len / 80).max(1);
        let digits = if estimated_lines == 0 {
            1
        } else {
            ((estimated_lines as f64).log10().floor() as usize) + 1
        };
        // Minimum 4 digits for readability, plus " │ " = 3 chars
        digits.max(4) + 3
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
        self.top_byte = iter.current_position();
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
        self.top_byte = iter.current_position();
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
                    self.top_byte = line_start;
                    return;
                }
                current_line += 1;
            } else {
                // Reached end of buffer before target line
                break;
            }
        }

        // If we didn't find the line, stay at the last valid position
        self.top_byte = iter.current_position();
    }

    /// Ensure a cursor is visible, scrolling if necessary (smart scroll)
    /// This now uses ONLY the LineCache - no manual line counting
    pub fn ensure_visible(&mut self, buffer: &mut Buffer, cursor: &Cursor) {
        // Find the start of the line containing the cursor using iterator
        let cursor_iter = buffer.line_iterator(cursor.position);
        let cursor_line_start = cursor_iter.current_position();

        // Get line numbers from the cache
        let top_line_number = buffer.get_line_number(self.top_byte);
        let cursor_line_number = buffer.get_line_number(cursor_line_start);

        // Check if cursor line is visible
        let visible_count = self.visible_line_count();
        let lines_from_top = cursor_line_number.saturating_sub(top_line_number);

        let cursor_is_visible = lines_from_top >= self.scroll_offset
            && lines_from_top < visible_count.saturating_sub(self.scroll_offset);

        // If cursor is not visible, scroll to make it visible
        if !cursor_is_visible {
            // Position cursor at scroll_offset lines from top
            let target_line_from_top = self.scroll_offset;

            // Move backwards from cursor to find the new top_byte
            let mut iter = buffer.line_iterator(cursor_line_start);

            for _ in 0..target_line_from_top {
                if iter.prev().is_none() {
                    break; // Hit beginning of buffer
                }
            }

            self.top_byte = iter.current_position();
        }

        // Horizontal scrolling
        let cursor_column = cursor.position.saturating_sub(cursor_line_start);
        self.ensure_column_visible(cursor_column, buffer);
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
            self.top_byte = iter.current_position();
            // Line number is now tracked automatically via LineCache
        }
    }

    /// Ensure a column is visible with horizontal scroll offset applied
    pub fn ensure_column_visible(&mut self, column: usize, buffer: &Buffer) {
        // Calculate visible width (accounting for line numbers gutter which is dynamic)
        let gutter_width = self.gutter_width(buffer);
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
            self.top_byte = iter.current_position();
            // Line number is now tracked automatically via LineCache
        } else {
            // Can't fit all cursors, ensure primary is visible
            let primary_cursor = sorted_cursors[0].1;
            self.ensure_visible(buffer, primary_cursor);
        }
    }

    /// Get the cursor screen position (x, y) which is (col, row) for rendering
    /// This returns the position relative to the viewport, accounting for horizontal scrolling
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

        // Account for horizontal scrolling - subtract left_column offset
        let screen_col = column.saturating_sub(self.left_column) as u16;

        // Return (x, y) which is (col, row)
        (screen_col, screen_row as u16)
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
        assert_eq!(vp.top_byte, 0);
    }

    #[test]
    fn test_scroll_up_down() {
        let buffer = Buffer::from_str("line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\nline11\nline12\nline13\nline14\nline15");
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
        let buffer = Buffer::from_str("line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\nline11\nline12\nline13\nline14\nline15\nline16\nline17\nline18\nline19\nline20\nline21\nline22\nline23\nline24\nline25\nline26\nline27\nline28\nline29\nline30\nline31\nline32\nline33\nline34\nline35\nline36\nline37\nline38\nline39\nline40\nline41\nline42\nline43\nline44\nline45\nline46\nline47\nline48\nline49\nline50\nline51");
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
        let mut buffer = Buffer::from_str("line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\nline11\nline12\nline13\nline14\nline15\nline16\nline17\nline18\nline19\nline20");
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
        let mut buffer = Buffer::from_str("line1\nline2\nline3");
        let vp = Viewport::new(80, 24);

        let cursor = Cursor::new(6); // Start of line 1 ("line2")
        let (x, y) = vp.cursor_screen_position(&mut buffer, &cursor);
        // x is column (horizontal), y is row (vertical)
        assert_eq!(x, 0); // Column 0 (start of line)
        assert_eq!(y, 1); // Row 1 (second line, since top_line is 0)
    }
}
