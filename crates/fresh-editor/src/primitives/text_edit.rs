//! Reusable multiline text editing state
//!
//! This module provides `TextEdit`, a multiline text editor with:
//! - Cursor navigation (arrows, home/end)
//! - Selection support (Shift+arrows, Ctrl+A)
//! - Insert/delete operations
//! - Word navigation (Ctrl+arrows)
//!
//! Single-line editing is a special case (one line, newlines disallowed).

use crate::primitives::word_navigation::{find_word_end_bytes, find_word_start_bytes};

/// Compute the flat UTF-8 byte offset of `(row, col)` into the
/// `\n`-joined view of `lines`. Used by both `flat_cursor_byte` and
/// `selection_flat_range` so the two stay in lock-step.
fn flat_offset(lines: &[String], row: usize, col: usize) -> usize {
    let mut offset = 0;
    for (i, line) in lines.iter().enumerate() {
        if i == row {
            return offset + col.min(line.len());
        }
        offset += line.len() + 1;
    }
    offset.saturating_sub(1)
}

/// Multiline text editing state
#[derive(Debug, Clone)]
pub struct TextEdit {
    /// Lines of text
    pub lines: Vec<String>,
    /// Current cursor row (0-indexed)
    pub cursor_row: usize,
    /// Current cursor column (0-indexed, in bytes)
    pub cursor_col: usize,
    /// Selection anchor position (row, col) - for Shift+Arrow selection
    pub selection_anchor: Option<(usize, usize)>,
    /// Whether to allow multiline (newlines)
    pub multiline: bool,
}

impl Default for TextEdit {
    fn default() -> Self {
        Self::new()
    }
}

impl TextEdit {
    /// Create a new empty text edit (multiline by default)
    pub fn new() -> Self {
        Self {
            lines: vec![String::new()],
            cursor_row: 0,
            cursor_col: 0,
            selection_anchor: None,
            multiline: true,
        }
    }

    /// Create a single-line text edit
    pub fn single_line() -> Self {
        Self {
            lines: vec![String::new()],
            cursor_row: 0,
            cursor_col: 0,
            selection_anchor: None,
            multiline: false,
        }
    }

    /// Create from initial text
    pub fn with_text(text: &str) -> Self {
        let lines: Vec<String> = text.lines().map(String::from).collect();
        let lines = if lines.is_empty() {
            vec![String::new()]
        } else {
            lines
        };
        Self {
            lines,
            cursor_row: 0,
            cursor_col: 0,
            selection_anchor: None,
            multiline: true,
        }
    }

    /// Create single-line from initial text (takes first line only)
    pub fn single_line_with_text(text: &str) -> Self {
        let first_line = text.lines().next().unwrap_or("").to_string();
        Self {
            lines: vec![first_line],
            cursor_row: 0,
            cursor_col: 0,
            selection_anchor: None,
            multiline: false,
        }
    }

    /// Get the full text value
    pub fn value(&self) -> String {
        self.lines.join("\n")
    }

    /// Set the text value, resetting cursor to start
    pub fn set_value(&mut self, text: &str) {
        if self.multiline {
            self.lines = text.lines().map(String::from).collect();
            if self.lines.is_empty() {
                self.lines.push(String::new());
            }
        } else {
            self.lines = vec![text.lines().next().unwrap_or("").to_string()];
        }
        self.cursor_row = 0;
        self.cursor_col = 0;
        self.selection_anchor = None;
    }

    /// Get the current line
    pub fn current_line(&self) -> &str {
        self.lines
            .get(self.cursor_row)
            .map(|s| s.as_str())
            .unwrap_or("")
    }

    /// Get number of lines
    pub fn line_count(&self) -> usize {
        self.lines.len()
    }

    /// Flat UTF-8 byte offset of the cursor in `value()`. Each newline
    /// separator between lines counts as one byte. Useful as a bridge
    /// to renderers / IPC payloads that speak in linear byte offsets.
    pub fn flat_cursor_byte(&self) -> usize {
        let mut offset = 0;
        for (i, line) in self.lines.iter().enumerate() {
            if i == self.cursor_row {
                return offset + self.cursor_col.min(line.len());
            }
            offset += line.len() + 1; // +1 for the joining '\n'
        }
        // cursor_row past the end: clamp to value length.
        self.value().len()
    }

    /// Move the cursor to the position designated by a flat UTF-8 byte
    /// offset into `value()`. Clears any active selection (matching the
    /// other non-selecting cursor-move methods). Clamps to the value's
    /// length and snaps down to the nearest grapheme-cluster boundary, so
    /// an externally computed offset (e.g. from a mouse click) can never
    /// land the caret between the codepoints of a single cluster — Thai
    /// combining marks, emoji ZWJ sequences, flags — matching how
    /// `move_left`/`move_right` traverse the value.
    pub fn set_cursor_from_flat(&mut self, byte: usize) {
        self.clear_selection();
        let total = self.value().len();
        let mut remaining = byte.min(total);
        for (i, line) in self.lines.iter().enumerate() {
            if remaining <= line.len() {
                self.cursor_row = i;
                self.cursor_col = crate::primitives::grapheme::snap_to_grapheme_boundary(
                    line, remaining,
                );
                return;
            }
            remaining -= line.len() + 1; // step past the line and its trailing '\n'
        }
        self.cursor_row = self.lines.len().saturating_sub(1);
        self.cursor_col = self.lines.last().map(|l| l.len()).unwrap_or(0);
    }

    /// Selection range expressed as `(start_flat_byte, end_flat_byte)`
    /// into `value()`. Returns `None` when there's no active selection.
    /// Useful for renderers that overlay a background highlight on a
    /// flat-string view of the value.
    pub fn selection_flat_range(&self) -> Option<(usize, usize)> {
        let ((sr, sc), (er, ec)) = self.selection_range()?;
        Some((
            flat_offset(&self.lines, sr, sc),
            flat_offset(&self.lines, er, ec),
        ))
    }

    // ========================================================================
    // Cursor movement (clears selection)
    // ========================================================================

    /// Move cursor left
    pub fn move_left(&mut self) {
        self.clear_selection();
        self.move_left_internal();
    }

    fn move_left_internal(&mut self) {
        if self.cursor_col > 0 {
            // Move to the previous grapheme-cluster boundary, so a
            // single Left crosses a multi-codepoint cluster (Thai
            // combining marks, emoji ZWJ sequences) instead of landing
            // between its codepoints — matching the buffer's movement.
            let line = &self.lines[self.cursor_row];
            self.cursor_col =
                crate::primitives::grapheme::prev_grapheme_boundary(line, self.cursor_col);
        } else if self.cursor_row > 0 && self.multiline {
            self.cursor_row -= 1;
            self.cursor_col = self.lines[self.cursor_row].len();
        }
    }

    /// Move cursor right
    pub fn move_right(&mut self) {
        self.clear_selection();
        self.move_right_internal();
    }

    fn move_right_internal(&mut self) {
        let line_len = self
            .lines
            .get(self.cursor_row)
            .map(|l| l.len())
            .unwrap_or(0);
        if self.cursor_col < line_len {
            // Move to the next grapheme-cluster boundary (see
            // `move_left_internal`).
            let line = &self.lines[self.cursor_row];
            self.cursor_col =
                crate::primitives::grapheme::next_grapheme_boundary(line, self.cursor_col);
        } else if self.cursor_row + 1 < self.lines.len() && self.multiline {
            self.cursor_row += 1;
            self.cursor_col = 0;
        }
    }

    /// Move cursor up
    pub fn move_up(&mut self) {
        self.clear_selection();
        self.move_up_internal();
    }

    fn move_up_internal(&mut self) {
        if self.cursor_row > 0 {
            self.cursor_row -= 1;
            let line_len = self.lines[self.cursor_row].len();
            self.cursor_col = self.cursor_col.min(line_len);
        }
    }

    /// Move cursor down
    pub fn move_down(&mut self) {
        self.clear_selection();
        self.move_down_internal();
    }

    fn move_down_internal(&mut self) {
        if self.cursor_row + 1 < self.lines.len() {
            self.cursor_row += 1;
            let line_len = self.lines[self.cursor_row].len();
            self.cursor_col = self.cursor_col.min(line_len);
        }
    }

    /// Move to start of line
    pub fn move_home(&mut self) {
        self.clear_selection();
        self.cursor_col = 0;
    }

    /// Move to end of line
    pub fn move_end(&mut self) {
        self.clear_selection();
        self.cursor_col = self
            .lines
            .get(self.cursor_row)
            .map(|l| l.len())
            .unwrap_or(0);
    }

    /// Move to start of previous word
    pub fn move_word_left(&mut self) {
        self.clear_selection();
        self.move_word_left_internal();
    }

    fn move_word_left_internal(&mut self) {
        let line = &self.lines[self.cursor_row];
        if self.cursor_col > 0 {
            let new_col = find_word_start_bytes(line.as_bytes(), self.cursor_col);
            if new_col < self.cursor_col {
                self.cursor_col = new_col;
                return;
            }
        }
        // At start of line, move to end of previous line
        if self.cursor_row > 0 && self.multiline {
            self.cursor_row -= 1;
            self.cursor_col = self.lines[self.cursor_row].len();
        }
    }

    /// Move to start of next word
    pub fn move_word_right(&mut self) {
        self.clear_selection();
        self.move_word_right_internal();
    }

    fn move_word_right_internal(&mut self) {
        let line = &self.lines[self.cursor_row];
        if self.cursor_col < line.len() {
            let new_col = find_word_end_bytes(line.as_bytes(), self.cursor_col);
            if new_col > self.cursor_col {
                self.cursor_col = new_col;
                return;
            }
        }
        // At end of line, move to start of next line
        if self.cursor_row + 1 < self.lines.len() && self.multiline {
            self.cursor_row += 1;
            self.cursor_col = 0;
        }
    }

    // ========================================================================
    // Selection support
    // ========================================================================

    /// Check if there's an active selection
    pub fn has_selection(&self) -> bool {
        if let Some((anchor_row, anchor_col)) = self.selection_anchor {
            anchor_row != self.cursor_row || anchor_col != self.cursor_col
        } else {
            false
        }
    }

    /// Get selection range as ((start_row, start_col), (end_row, end_col))
    /// where start is before end in document order
    pub fn selection_range(&self) -> Option<((usize, usize), (usize, usize))> {
        let (anchor_row, anchor_col) = self.selection_anchor?;
        if anchor_row == self.cursor_row && anchor_col == self.cursor_col {
            return None;
        }

        let (start, end) = if anchor_row < self.cursor_row
            || (anchor_row == self.cursor_row && anchor_col < self.cursor_col)
        {
            ((anchor_row, anchor_col), (self.cursor_row, self.cursor_col))
        } else {
            ((self.cursor_row, self.cursor_col), (anchor_row, anchor_col))
        };
        Some((start, end))
    }

    /// Get selected text
    pub fn selected_text(&self) -> Option<String> {
        let ((start_row, start_col), (end_row, end_col)) = self.selection_range()?;

        if start_row == end_row {
            let line = &self.lines[start_row];
            let end_col = end_col.min(line.len());
            let start_col = start_col.min(end_col);
            Some(line[start_col..end_col].to_string())
        } else {
            let mut result = String::new();
            // First line from start_col to end
            let first_line = &self.lines[start_row];
            result.push_str(&first_line[start_col.min(first_line.len())..]);
            result.push('\n');
            // Middle lines (full)
            for row in (start_row + 1)..end_row {
                result.push_str(&self.lines[row]);
                result.push('\n');
            }
            // Last line from start to end_col
            let last_line = &self.lines[end_row];
            result.push_str(&last_line[..end_col.min(last_line.len())]);
            Some(result)
        }
    }

    /// Delete selection and return the deleted text
    pub fn delete_selection(&mut self) -> Option<String> {
        let ((start_row, start_col), (end_row, end_col)) = self.selection_range()?;
        let deleted = self.selected_text()?;

        if start_row == end_row {
            let line = &mut self.lines[start_row];
            let end_col = end_col.min(line.len());
            let start_col = start_col.min(end_col);
            line.drain(start_col..end_col);
        } else {
            let end_col = end_col.min(self.lines[end_row].len());
            let after_end = self.lines[end_row][end_col..].to_string();
            self.lines[start_row].truncate(start_col);
            self.lines[start_row].push_str(&after_end);
            // Remove the lines in between
            for _ in (start_row + 1)..=end_row {
                self.lines.remove(start_row + 1);
            }
        }

        self.cursor_row = start_row;
        self.cursor_col = start_col;
        self.selection_anchor = None;
        Some(deleted)
    }

    /// Clear selection without deleting text
    pub fn clear_selection(&mut self) {
        self.selection_anchor = None;
    }

    /// Start or extend selection
    fn ensure_anchor(&mut self) {
        if self.selection_anchor.is_none() {
            self.selection_anchor = Some((self.cursor_row, self.cursor_col));
        }
    }

    /// Move cursor left with selection (Shift+Left)
    pub fn move_left_selecting(&mut self) {
        self.ensure_anchor();
        self.move_left_internal();
    }

    /// Move cursor right with selection (Shift+Right)
    pub fn move_right_selecting(&mut self) {
        self.ensure_anchor();
        self.move_right_internal();
    }

    /// Move cursor up with selection (Shift+Up)
    pub fn move_up_selecting(&mut self) {
        self.ensure_anchor();
        self.move_up_internal();
    }

    /// Move cursor down with selection (Shift+Down)
    pub fn move_down_selecting(&mut self) {
        self.ensure_anchor();
        self.move_down_internal();
    }

    /// Move to start of line with selection (Shift+Home)
    pub fn move_home_selecting(&mut self) {
        self.ensure_anchor();
        self.cursor_col = 0;
    }

    /// Move to end of line with selection (Shift+End)
    pub fn move_end_selecting(&mut self) {
        self.ensure_anchor();
        self.cursor_col = self
            .lines
            .get(self.cursor_row)
            .map(|l| l.len())
            .unwrap_or(0);
    }

    /// Move word left with selection (Ctrl+Shift+Left)
    pub fn move_word_left_selecting(&mut self) {
        self.ensure_anchor();
        self.move_word_left_internal();
    }

    /// Move word right with selection (Ctrl+Shift+Right)
    pub fn move_word_right_selecting(&mut self) {
        self.ensure_anchor();
        self.move_word_right_internal();
    }

    /// Select all text (Ctrl+A)
    pub fn select_all(&mut self) {
        self.selection_anchor = Some((0, 0));
        self.cursor_row = self.lines.len().saturating_sub(1);
        self.cursor_col = self.lines.last().map(|l| l.len()).unwrap_or(0);
    }

    // ========================================================================
    // Editing operations
    // ========================================================================

    /// Insert a character at cursor position
    pub fn insert_char(&mut self, c: char) {
        // Delete selection first if any
        if self.has_selection() {
            self.delete_selection();
        }

        if c == '\n' && self.multiline {
            // Split line at cursor
            let current_line = &self.lines[self.cursor_row];
            let col = self.cursor_col.min(current_line.len());
            let (before, after) = current_line.split_at(col);
            let before = before.to_string();
            let after = after.to_string();
            self.lines[self.cursor_row] = before;
            self.lines.insert(self.cursor_row + 1, after);
            self.cursor_row += 1;
            self.cursor_col = 0;
        } else if c != '\n' && self.cursor_row < self.lines.len() {
            let line = &mut self.lines[self.cursor_row];
            let col = self.cursor_col.min(line.len());
            line.insert(col, c);
            self.cursor_col = col + c.len_utf8();
        }
        // Ignore newline in single-line mode
    }

    /// Insert a string at cursor position.
    ///
    /// In single-line mode, embedded `\n` are flattened to a space
    /// (`' '`) — pasting `"foo\nbar"` into a single-line input
    /// yields `"foo bar"`. Dropping the newline silently would
    /// concatenate the words (`"foobar"`), which is the wrong
    /// thing for the common "paste a CSV line into a search
    /// field" workflow. The space preserves token boundaries and
    /// matches what every other GUI text input does on paste.
    pub fn insert_str(&mut self, text: &str) {
        if self.has_selection() {
            self.delete_selection();
        }
        for c in text.chars() {
            if c == '\n' && !self.multiline {
                self.insert_char(' ');
                continue;
            }
            self.insert_char(c);
        }
    }

    /// Delete character before cursor (backspace)
    pub fn backspace(&mut self) {
        if self.has_selection() {
            self.delete_selection();
            return;
        }

        if self.cursor_col > 0 {
            let line = &mut self.lines[self.cursor_row];
            // Find previous char boundary
            let mut del_start = self.cursor_col - 1;
            while del_start > 0 && !line.is_char_boundary(del_start) {
                del_start -= 1;
            }
            line.drain(del_start..self.cursor_col);
            self.cursor_col = del_start;
        } else if self.cursor_row > 0 && self.multiline {
            // Join with previous line
            let current_line = self.lines.remove(self.cursor_row);
            self.cursor_row -= 1;
            self.cursor_col = self.lines[self.cursor_row].len();
            self.lines[self.cursor_row].push_str(&current_line);
        }
    }

    /// Delete character at cursor (delete key)
    pub fn delete(&mut self) {
        if self.has_selection() {
            self.delete_selection();
            return;
        }

        let line_len = self
            .lines
            .get(self.cursor_row)
            .map(|l| l.len())
            .unwrap_or(0);
        if self.cursor_col < line_len {
            let line = &mut self.lines[self.cursor_row];
            // Find next char boundary
            let mut del_end = self.cursor_col + 1;
            while del_end < line.len() && !line.is_char_boundary(del_end) {
                del_end += 1;
            }
            line.drain(self.cursor_col..del_end);
        } else if self.cursor_row + 1 < self.lines.len() && self.multiline {
            // Join with next line
            let next_line = self.lines.remove(self.cursor_row + 1);
            self.lines[self.cursor_row].push_str(&next_line);
        }
    }

    /// Delete from cursor to end of word (Ctrl+Delete)
    pub fn delete_word_forward(&mut self) {
        if self.has_selection() {
            self.delete_selection();
            return;
        }

        let line = &self.lines[self.cursor_row];
        let word_end = find_word_end_bytes(line.as_bytes(), self.cursor_col);
        if word_end > self.cursor_col {
            let line = &mut self.lines[self.cursor_row];
            line.drain(self.cursor_col..word_end);
        } else if self.cursor_row + 1 < self.lines.len() && self.multiline {
            // At end of line, join with next
            let next_line = self.lines.remove(self.cursor_row + 1);
            self.lines[self.cursor_row].push_str(&next_line);
        }
    }

    /// Delete from start of word to cursor (Ctrl+Backspace)
    pub fn delete_word_backward(&mut self) {
        if self.has_selection() {
            self.delete_selection();
            return;
        }

        let line = &self.lines[self.cursor_row];
        let word_start = find_word_start_bytes(line.as_bytes(), self.cursor_col);
        if word_start < self.cursor_col {
            let line = &mut self.lines[self.cursor_row];
            line.drain(word_start..self.cursor_col);
            self.cursor_col = word_start;
        } else if self.cursor_row > 0 && self.multiline {
            // At start of line, join with previous
            let current_line = self.lines.remove(self.cursor_row);
            self.cursor_row -= 1;
            self.cursor_col = self.lines[self.cursor_row].len();
            self.lines[self.cursor_row].push_str(&current_line);
        }
    }

    /// Delete from cursor to end of line (Ctrl+K)
    pub fn delete_to_end(&mut self) {
        if self.has_selection() {
            self.delete_selection();
            return;
        }

        if let Some(line) = self.lines.get_mut(self.cursor_row) {
            line.truncate(self.cursor_col);
        }
    }

    /// Clear all text
    pub fn clear(&mut self) {
        self.lines = vec![String::new()];
        self.cursor_row = 0;
        self.cursor_col = 0;
        self.selection_anchor = None;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_line_basic() {
        let mut edit = TextEdit::single_line();
        edit.insert_str("hello world");
        assert_eq!(edit.value(), "hello world");
        assert_eq!(edit.cursor_col, 11);
    }

    #[test]
    fn test_single_line_flattens_newlines_to_spaces() {
        // Pasting a multi-line string into a single-line input
        // should flatten newlines to spaces, not concatenate
        // (which would lose token boundaries).
        let mut edit = TextEdit::single_line();
        edit.insert_str("hello\nworld");
        assert_eq!(edit.value(), "hello world");
        assert_eq!(edit.line_count(), 1);
    }

    #[test]
    fn test_multiline_basic() {
        let mut edit = TextEdit::new();
        edit.insert_str("hello\nworld");
        assert_eq!(edit.value(), "hello\nworld");
        assert_eq!(edit.line_count(), 2);
        assert_eq!(edit.cursor_row, 1);
        assert_eq!(edit.cursor_col, 5);
    }

    #[test]
    fn test_selection_single_line() {
        let mut edit = TextEdit::single_line_with_text("hello world");
        edit.cursor_col = 6; // After "hello "

        edit.move_right_selecting();
        edit.move_right_selecting();
        edit.move_right_selecting();
        edit.move_right_selecting();
        edit.move_right_selecting();

        assert!(edit.has_selection());
        assert_eq!(edit.selected_text(), Some("world".to_string()));
    }

    #[test]
    fn test_selection_multiline() {
        let mut edit = TextEdit::with_text("line1\nline2\nline3");
        edit.cursor_row = 0;
        edit.cursor_col = 3; // After "lin"

        // Select to middle of line 2
        edit.move_down_selecting();
        edit.move_right_selecting();
        edit.move_right_selecting();

        assert!(edit.has_selection());
        let selected = edit.selected_text().unwrap();
        assert_eq!(selected, "e1\nline2");
    }

    #[test]
    fn test_delete_selection() {
        let mut edit = TextEdit::with_text("hello world");
        edit.cursor_col = 0;

        // Select "hello "
        for _ in 0..6 {
            edit.move_right_selecting();
        }

        let deleted = edit.delete_selection();
        assert_eq!(deleted, Some("hello ".to_string()));
        assert_eq!(edit.value(), "world");
        assert_eq!(edit.cursor_col, 0);
    }

    #[test]
    fn test_backspace_with_selection() {
        let mut edit = TextEdit::with_text("hello world");
        edit.select_all();
        edit.backspace();
        assert_eq!(edit.value(), "");
    }

    #[test]
    fn test_insert_replaces_selection() {
        let mut edit = TextEdit::with_text("hello world");
        edit.select_all();
        edit.insert_str("goodbye");
        assert_eq!(edit.value(), "goodbye");
    }

    #[test]
    fn test_flat_cursor_byte_round_trips_multiline() {
        let mut edit = TextEdit::with_text("ab\ncde\nf");
        // Cursor on line 1 col 2 (between 'd' and 'e').
        edit.cursor_row = 1;
        edit.cursor_col = 2;
        let flat = edit.flat_cursor_byte();
        // "ab\n" = 3 bytes, plus 2 into line 1 = 5.
        assert_eq!(flat, 5);
        // Round-trip: set from flat 5 should land at (1, 2).
        let mut edit2 = TextEdit::with_text("ab\ncde\nf");
        edit2.set_cursor_from_flat(5);
        assert_eq!((edit2.cursor_row, edit2.cursor_col), (1, 2));
    }

    #[test]
    fn test_set_cursor_from_flat_clamps_past_end() {
        let mut edit = TextEdit::with_text("abc\nde");
        edit.set_cursor_from_flat(999);
        assert_eq!((edit.cursor_row, edit.cursor_col), (1, 2));
    }

    #[test]
    fn test_set_cursor_from_flat_snaps_to_char_boundary() {
        // "é" is 2 bytes. Flat offset 1 lands mid-multibyte — snap to 0.
        let mut edit = TextEdit::single_line_with_text("é");
        edit.set_cursor_from_flat(1);
        assert_eq!(edit.cursor_col, 0);
    }

    #[test]
    fn test_set_cursor_from_flat_snaps_to_grapheme_boundary() {
        // Decomposed "é" = 'e' + combining acute (U+0301, 2 bytes) is one
        // grapheme cluster spanning 3 bytes. Byte 1 is a valid *char*
        // boundary (between 'e' and the mark) but lands mid-cluster; the
        // caret must snap back to the cluster start (0), not sit inside it.
        let mut edit = TextEdit::single_line_with_text("e\u{301}");
        edit.set_cursor_from_flat(1);
        assert_eq!(edit.cursor_col, 0);
        // Byte 3 (past the whole cluster) is the end boundary — kept as-is.
        edit.set_cursor_from_flat(3);
        assert_eq!(edit.cursor_col, 3);

        // Thai "ที่" is a single 9-byte cluster; any interior offset snaps
        // to its start, and the trailing 'x' stays reachable at its start.
        let mut thai = TextEdit::single_line_with_text("ที่x");
        thai.set_cursor_from_flat(6);
        assert_eq!(thai.cursor_col, 0);
        thai.set_cursor_from_flat(9);
        assert_eq!(thai.cursor_col, 9);
    }

    #[test]
    fn test_selection_flat_range_spans_newline() {
        let mut edit = TextEdit::with_text("ab\ncd");
        edit.cursor_row = 0;
        edit.cursor_col = 1;
        edit.move_right_selecting(); // (0,2)
        edit.move_right_selecting(); // (1,0) — crossed the newline
        edit.move_right_selecting(); // (1,1)
                                     // Anchor was at (0,1) → flat 1. Cursor at (1,1) → flat 4.
        assert_eq!(edit.selection_flat_range(), Some((1, 4)));
    }

    #[test]
    fn test_word_navigation() {
        let mut edit = TextEdit::single_line_with_text("one two three");
        edit.cursor_col = 0;

        edit.move_word_right();
        assert_eq!(edit.cursor_col, 3); // After "one"

        edit.move_word_right();
        assert_eq!(edit.cursor_col, 7); // After "two"

        edit.move_word_left();
        assert_eq!(edit.cursor_col, 4); // Start of "two"
    }
}
