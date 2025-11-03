use crate::buffer::Buffer;
use crate::cursor::Cursor;

/// The viewport - what portion of the buffer is visible
#[derive(Debug, Clone)]
pub struct Viewport {
    /// Top-left corner (line number of first visible line)
    pub top_line: usize,

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
            top_line: 0,
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

    /// Get the bottom line (exclusive)
    pub fn bottom_line(&self) -> usize {
        self.top_line + self.visible_line_count()
    }

    /// Get the range of visible lines
    pub fn visible_range(&self) -> std::ops::Range<usize> {
        self.top_line..self.bottom_line()
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
        line >= self.top_line && line < self.bottom_line()
    }

    /// Scroll up by N lines
    pub fn scroll_up(&mut self, lines: usize) {
        self.top_line = self.top_line.saturating_sub(lines);
    }

    /// Scroll down by N lines
    pub fn scroll_down(&mut self, lines: usize, max_line: usize) {
        self.top_line = (self.top_line + lines).min(max_line.saturating_sub(1));
    }

    /// Scroll to a specific line
    pub fn scroll_to(&mut self, line: usize, max_line: usize) {
        self.top_line = line.min(max_line.saturating_sub(1));
    }

    /// Ensure a cursor is visible, scrolling if necessary (smart scroll)
    pub fn ensure_visible(&mut self, buffer: &mut Buffer, cursor: &Cursor) {
        // Vertical scrolling
        // Use lazy line lookup to avoid forcing full scans on large files
        let cursor_line = buffer.byte_to_line_lazy(cursor.position);
        // Use approximate line count if available, otherwise use a very large number
        // This is safe because ensure_line_visible will naturally clamp when scrolling
        let total_lines = buffer.approximate_line_count().unwrap_or(usize::MAX);
        self.ensure_line_visible(cursor_line, total_lines);

        // Horizontal scrolling
        let line_start = buffer.line_to_byte(cursor_line);
        let cursor_column = cursor.position.saturating_sub(line_start);
        self.ensure_column_visible(cursor_column);
    }

    /// Ensure a line is visible with scroll offset applied
    pub fn ensure_line_visible(&mut self, line: usize, total_lines: usize) {
        let visible_count = self.visible_line_count();

        // If viewport is too small for scroll offset, use what we can
        let effective_offset = self.scroll_offset.min(visible_count / 2);

        // Calculate the ideal top and bottom boundaries with scroll offset
        let ideal_top = self.top_line + effective_offset;
        let ideal_bottom = self.top_line + visible_count.saturating_sub(effective_offset);

        if line < ideal_top {
            // Cursor is above the ideal zone - scroll up
            self.top_line = line.saturating_sub(effective_offset);
        } else if line >= ideal_bottom {
            // Cursor is below the ideal zone - scroll down
            self.top_line = line
                .saturating_sub(visible_count.saturating_sub(effective_offset))
                .min(total_lines.saturating_sub(1));
        }

        // Ensure we don't scroll past the end
        if self.top_line + visible_count > total_lines {
            self.top_line = total_lines.saturating_sub(visible_count);
        }
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

        // Convert cursor positions to line numbers
        let mut cursor_lines: Vec<(usize, usize)> = cursors
            .iter()
            .map(|(priority, cursor)| (*priority, buffer.byte_to_line_lazy(cursor.position)))
            .collect();

        // Sort by priority (primary cursor first)
        cursor_lines.sort_by_key(|(priority, _)| *priority);

        // Try to fit as many cursors as possible, prioritizing primary
        let visible_count = self.visible_line_count();
        let min_line = cursor_lines.iter().map(|(_, line)| *line).min().unwrap();
        let max_line = cursor_lines.iter().map(|(_, line)| *line).max().unwrap();

        // If all cursors fit in the viewport, center them
        if max_line - min_line < visible_count {
            let center = (min_line + max_line) / 2;
            let new_top = center.saturating_sub(visible_count / 2);
            let approx_total = buffer.approximate_line_count().unwrap_or(usize::MAX);
            self.top_line = new_top.min(approx_total.saturating_sub(visible_count));
        } else {
            // Can't fit all cursors, ensure primary is visible with scroll offset
            let (_, primary_line) = cursor_lines[0];
            let approx_total = buffer.approximate_line_count().unwrap_or(usize::MAX);
            self.ensure_line_visible(primary_line, approx_total);
        }
    }

    /// Convert a screen row to a buffer line number
    pub fn screen_row_to_line(&self, row: u16) -> usize {
        self.top_line + row as usize
    }

    /// Convert a buffer line number to a screen row (if visible)
    pub fn line_to_screen_row(&self, line: usize) -> Option<u16> {
        if self.is_line_visible(line) {
            Some((line - self.top_line) as u16)
        } else {
            None
        }
    }

    /// Get the cursor screen position (x, y) which is (col, row) for rendering
    /// This returns the position relative to the viewport, accounting for horizontal scrolling
    pub fn cursor_screen_position(&self, buffer: &mut Buffer, cursor: &Cursor) -> (u16, u16) {
        let line = buffer.byte_to_line_lazy(cursor.position);
        let line_start = buffer.line_to_byte(line);
        let column = cursor.position.saturating_sub(line_start);

        let screen_row = line.saturating_sub(self.top_line) as u16;
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
        assert_eq!(vp.top_line, 0);
    }

    #[test]
    fn test_visible_range() {
        let vp = Viewport::new(80, 24);
        assert_eq!(vp.visible_range(), 0..24);
    }

    #[test]
    fn test_scroll_up_down() {
        let mut vp = Viewport::new(80, 24);
        vp.scroll_down(10, 100);
        assert_eq!(vp.top_line, 10);

        vp.scroll_up(5);
        assert_eq!(vp.top_line, 5);

        vp.scroll_up(10);
        assert_eq!(vp.top_line, 0); // Can't scroll past 0
    }

    #[test]
    fn test_ensure_line_visible() {
        let mut vp = Viewport::new(80, 24);
        vp.scroll_offset = 3;

        // Line within scroll offset should adjust viewport
        vp.ensure_line_visible(2, 100);
        assert!(vp.top_line < 2);

        // Line far below should scroll down
        vp.ensure_line_visible(50, 100);
        assert!(vp.top_line > 0);
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

        let cursor_line = buffer.byte_to_line_lazy(cursor_pos);
        assert!(vp.is_line_visible(cursor_line));
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
