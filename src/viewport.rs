use crate::buffer::Buffer;
use crate::cursor::Cursor;
use crate::line_wrapping::{char_position_to_segment, wrap_line, WrapConfig};
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

        // Apply scroll limiting
        self.apply_scroll_limit(buffer);
    }

    /// Apply scroll limiting to prevent scrolling past the end of the buffer
    /// Ensures the last line is always at the bottom unless the buffer is smaller than viewport
    pub fn apply_scroll_limit(&mut self, buffer: &Buffer) {
        let viewport_height = self.visible_line_count();
        if viewport_height == 0 {
            return;
        }

        let buffer_len = buffer.len();
        if buffer_len == 0 {
            self.top_byte = 0;
            return;
        }

        // Count total lines in buffer
        let mut line_count = 0;
        let mut iter = buffer.line_iterator(0);
        while iter.next().is_some() {
            line_count += 1;
        }

        // If buffer has fewer lines than viewport, scroll to top
        if line_count <= viewport_height {
            self.top_byte = 0;
            return;
        }

        // Calculate how many lines from the start we can scroll
        // We want to be able to scroll so that the last line is at the bottom
        let scrollable_lines = line_count.saturating_sub(viewport_height);

        // Find the byte position of the line at scrollable_lines offset
        let mut iter = buffer.line_iterator(0);
        let mut current_line = 0;
        let mut max_byte_pos = 0;

        while current_line < scrollable_lines {
            if let Some((pos, _content)) = iter.next() {
                max_byte_pos = pos;
                current_line += 1;
            } else {
                break;
            }
        }

        // Clamp top_byte to not exceed max_byte_pos
        if self.top_byte > max_byte_pos {
            self.top_byte = max_byte_pos;
        }
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

        // Note: Don't apply scroll limiting here - we want to allow the viewport
        // to scroll to show the cursor even if it's at/beyond the end of the buffer.
        // Scroll limiting is only applied for explicit scroll commands (scroll_down).

        // Horizontal scrolling - skip if line wrapping is enabled
        // When wrapping is enabled, all columns are always visible via wrapping
        if !self.line_wrap_enabled {
            let cursor_column = cursor.position.saturating_sub(cursor_line_start);
            self.ensure_column_visible(cursor_column, buffer);
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
            self.top_byte = iter.current_position();
            // Line number is now tracked automatically via LineCache

            // Apply scroll limiting
            self.apply_scroll_limit(buffer);
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
            self.left_column =
                column.saturating_sub(visible_width.saturating_sub(effective_offset));
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
        // Create a buffer with more lines than the viewport to make scrolling possible
        let mut content = String::new();
        for i in 1..=50 {
            if i > 1 {
                content.push('\n');
            }
            content.push_str(&format!("line{}", i));
        }
        let buffer = Buffer::from_str(&content);
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
