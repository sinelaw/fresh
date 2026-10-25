//! The rows of a pane that draws one row per line, plus one per plugin
//! virtual line: an unwrapped pane.
//!
//! The wrap index ([`crate::view::wrap_index::WrapIndex`]) knows the rows of
//! any pane, but it is built only under its size ceilings, and a pane with
//! wrapping off needs none of what it measures: its rows are its lines with
//! the virtual lines slotted in. The virtual lines' marker trees count their
//! markers, so where a line's rows fall is two tree queries — nothing is
//! collected, kept or invalidated, and it holds for a file of any size. A
//! side-by-side comparison needs it: its filler rows are virtual lines, and
//! both scrolling a pane and keeping the two panes level work in rows that
//! count them.

use crate::model::buffer::Buffer;
use crate::state::EditorState;
use crate::view::virtual_text::{VirtualTextManager, VirtualTextPosition};

/// Where the rows of an unwrapped pane fall. Within a line the rows go: the
/// virtual lines drawn above it, its text, the virtual lines drawn below it —
/// the order the renderer draws them, and the order a viewport's
/// `top_view_line_offset` counts.
///
/// A view of the buffer and its virtual lines as they are: it borrows them,
/// so it cannot outlive an edit.
///
/// Only for a pane in source view. One in page view lays its lines out with
/// what a composing plugin adds — soft breaks that split a line into rows,
/// conceals that join lines into one — which this doesn't count; its rows are
/// the wrap index's, or the line walk's.
#[derive(Clone, Copy)]
pub struct FlatRows<'a> {
    buffer: &'a Buffer,
    lines: &'a VirtualTextManager,
    line_count: usize,
}

impl<'a> FlatRows<'a> {
    /// The rows of `state`'s buffer as an unwrapped pane draws them. `None`
    /// for a pane in page view (`page_view`; see the type) and for a buffer
    /// without line data (large-file mode).
    pub fn new(state: &'a EditorState, page_view: bool) -> Option<Self> {
        if page_view {
            return None;
        }
        Self::over(&state.buffer, &state.virtual_texts)
    }

    fn over(buffer: &'a Buffer, lines: &'a VirtualTextManager) -> Option<Self> {
        if buffer.is_large_file() {
            return None;
        }
        Some(Self {
            buffer,
            lines,
            line_count: buffer.line_count()?.max(1),
        })
    }

    /// The byte `line`'s anchors start at; past the last line, the end of the
    /// buffer. A virtual line anchored there is drawn by no line — the
    /// renderer takes a line's anchors from its own bytes, and the wrap index
    /// likewise stops at the end — so it isn't counted either.
    fn start_of(&self, line: usize) -> usize {
        let end = self.buffer.len();
        if line >= self.line_count {
            return end;
        }
        self.buffer.line_start_offset(line).unwrap_or(end).min(end)
    }

    /// The virtual lines drawn before `position`: a source pane draws no
    /// compose-only ones.
    fn lines_before(&self, position: usize) -> usize {
        self.lines.lines_before(position, false)
    }

    /// Whether any virtual line is drawn: without one, rows are lines.
    pub fn has_virtual_lines(&self) -> bool {
        self.total_rows() > self.line_count
    }

    pub fn total_rows(&self) -> usize {
        self.line_count + self.lines_before(self.start_of(self.line_count))
    }

    /// The row `line`'s first row (its first virtual line above, or its text)
    /// is drawn on.
    pub fn first_row(&self, line: usize) -> usize {
        let line = line.min(self.line_count);
        line + self.lines_before(self.start_of(line))
    }

    /// The row `line`'s text is drawn on.
    pub fn text_row(&self, line: usize) -> usize {
        let above = self.lines.lines_in(
            VirtualTextPosition::LineAbove,
            false,
            self.start_of(line),
            self.start_of(line + 1),
        );
        self.first_row(line) + above
    }

    /// The line `row` belongs to, and how many rows into it `row` is.
    pub fn locate(&self, row: usize) -> (usize, usize) {
        // The last line whose first row is at or before `row`.
        let (mut lo, mut hi) = (0, self.line_count);
        while hi - lo > 1 {
            let mid = lo + (hi - lo) / 2;
            if self.first_row(mid) <= row {
                lo = mid;
            } else {
                hi = mid;
            }
        }
        let first = self.first_row(lo);
        let rows_in_line = self.first_row(lo + 1) - first;
        (lo, (row - first).min(rows_in_line.saturating_sub(1)))
    }

    /// The furthest a pane `height` rows tall scrolls: its last page.
    pub fn max_top(&self, height: usize) -> usize {
        self.total_rows().saturating_sub(height)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::virtual_text::VirtualTextNamespace;
    use ratatui::style::Style;

    /// Ten lines `0\n` … `9\n`: line `n` starts at byte `2n`.
    fn ten_lines() -> Buffer {
        Buffer::from_str_test(&(0..10).map(|i| format!("{i}\n")).collect::<String>())
    }

    fn add(lines: &mut VirtualTextManager, at: usize, placement: VirtualTextPosition, n: usize) {
        for i in 0..n {
            lines.add_line(
                at,
                String::new(),
                Style::default(),
                placement,
                VirtualTextNamespace::from_string("t".into()),
                i as i32,
            );
        }
    }

    #[test]
    fn rows_are_lines_without_virtual_lines() {
        let buffer = ten_lines();
        let lines = VirtualTextManager::new();
        let r = FlatRows::over(&buffer, &lines).unwrap();
        // The text's final newline leaves an empty eleventh line.
        assert_eq!(r.total_rows(), 11);
        assert_eq!(r.first_row(4), 4);
        assert_eq!(r.text_row(4), 4);
        assert_eq!(r.locate(4), (4, 0));
        assert_eq!(r.max_top(3), 8);
        assert!(!r.has_virtual_lines());
    }

    #[test]
    fn virtual_lines_below_a_line_follow_its_text() {
        let buffer = ten_lines();
        let mut lines = VirtualTextManager::new();
        // Three filler rows below line 2, anchored at its newline.
        add(&mut lines, 5, VirtualTextPosition::LineBelow, 3);
        let r = FlatRows::over(&buffer, &lines).unwrap();
        assert_eq!(r.total_rows(), 14);
        assert_eq!(r.text_row(2), 2);
        assert_eq!(r.first_row(3), 6);
        assert_eq!(r.locate(2), (2, 0));
        assert_eq!(r.locate(4), (2, 2));
        assert_eq!(r.locate(5), (2, 3));
        assert_eq!(r.locate(6), (3, 0));
        // Past the end: the last line's last row.
        assert_eq!(r.locate(100), (10, 0));
    }

    #[test]
    fn virtual_lines_above_a_line_precede_its_text() {
        let buffer = ten_lines();
        let mut lines = VirtualTextManager::new();
        // Two rows above line 0 (a change at the very top of a file).
        add(&mut lines, 0, VirtualTextPosition::LineAbove, 2);
        let r = FlatRows::over(&buffer, &lines).unwrap();
        assert_eq!(r.first_row(0), 0);
        assert_eq!(r.text_row(0), 2);
        assert_eq!(r.first_row(1), 3);
        assert_eq!(r.locate(1), (0, 1));
    }

    /// A source pane doesn't draw a composing plugin's compose-only lines,
    /// and a pane in page view isn't one this counts at all.
    #[test]
    fn compose_only_lines_are_not_counted() {
        let buffer = ten_lines();
        let mut lines = VirtualTextManager::new();
        lines.add_line_with_theme_keys(
            2,
            String::new(),
            Style::default(),
            None,
            None,
            VirtualTextPosition::LineAbove,
            VirtualTextNamespace::from_string("t".into()),
            0,
            None,
            None,
            Vec::new(),
            true,
        );
        let rows = FlatRows::over(&buffer, &lines).unwrap();
        assert_eq!(rows.text_row(1), 1);
        assert_eq!(rows.total_rows(), 11);
        assert!(!rows.has_virtual_lines());
    }

    /// A virtual line anchored at the very end of the buffer is drawn by no
    /// line, so it adds no row.
    #[test]
    fn an_anchor_at_the_end_of_the_buffer_adds_no_row() {
        let buffer = ten_lines();
        let mut lines = VirtualTextManager::new();
        add(&mut lines, buffer.len(), VirtualTextPosition::LineBelow, 2);
        let r = FlatRows::over(&buffer, &lines).unwrap();
        assert_eq!(r.total_rows(), 11);
        assert!(!r.has_virtual_lines());
    }

    /// Edits move the rows with the text: the count follows the anchors
    /// through the virtual lines' own marker trees, with nothing to rebuild.
    #[test]
    fn rows_follow_edits() {
        let mut buffer = ten_lines();
        let mut lines = VirtualTextManager::new();
        add(&mut lines, 5, VirtualTextPosition::LineBelow, 3);
        // Two new lines at the very top: the fillers now hang off line 4.
        lines.adjust_for_insert(0, 4);
        buffer.insert(0, "a\nb\n");
        let r = FlatRows::over(&buffer, &lines).unwrap();
        assert_eq!(r.first_row(5), 8);
        assert_eq!(r.locate(6), (4, 2));
    }
}
