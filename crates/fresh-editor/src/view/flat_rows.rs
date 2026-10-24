//! The rows of a pane that draws one row per line, plus one per plugin
//! virtual line: an unwrapped pane.
//!
//! The wrap index ([`crate::view::wrap_index::WrapIndex`]) knows the rows of
//! any pane, but it is built only under its size ceilings, and a pane with
//! wrapping off needs none of what it measures: its rows are its lines with
//! the virtual lines slotted in. That is arithmetic over the virtual lines'
//! anchors, so it holds for a file of any size. A side-by-side comparison
//! needs it: its filler rows are virtual lines, and both scrolling a pane and
//! keeping the two panes level work in rows that count them.

use crate::model::buffer::Buffer;
use crate::state::EditorState;

/// Where the rows of an unwrapped pane fall. Within a line the rows go: the
/// virtual lines drawn above it, its text, the virtual lines drawn below it —
/// the order the renderer draws them, and the order a viewport's
/// `top_view_line_offset` counts.
#[derive(Debug, Clone)]
pub struct FlatRows {
    line_count: usize,
    /// The line each virtual line is anchored in, ascending.
    virtual_lines: Vec<usize>,
    /// The line of each virtual line drawn above its line, ascending.
    above: Vec<usize>,
}

impl FlatRows {
    /// The rows of `state`'s buffer as a pane in or out of page view
    /// (`is_compose`) draws them unwrapped. `None` for a buffer without line
    /// data (large-file mode).
    pub fn new(state: &EditorState, is_compose: bool) -> Option<Self> {
        let buffer = &state.buffer;
        let lines = if state.virtual_texts.is_empty() {
            Vec::new()
        } else {
            state
                .virtual_texts
                .query_lines_in_range(&state.marker_list, 0, buffer.len() + 1)
                .into_iter()
                .filter(|(_, vt)| is_compose || !vt.compose_only)
                .map(|(pos, vt)| {
                    let above =
                        vt.position == crate::view::virtual_text::VirtualTextPosition::LineAbove;
                    (pos, above)
                })
                .collect()
        };
        Self::from_anchors(buffer, lines)
    }

    /// The rows of `buffer` with virtual lines anchored at the byte positions
    /// `virtual_lines` (ascending), as the scroll paths collect them
    /// (`EditorState::collect_virtual_line_positions`). Which side of its line
    /// each is drawn on doesn't change which line a row is in, so this is
    /// enough to scroll by; only [`Self::text_row`] needs [`Self::new`].
    pub fn from_positions(buffer: &Buffer, virtual_lines: &[usize]) -> Option<Self> {
        Self::from_anchors(buffer, virtual_lines.iter().map(|p| (*p, false)).collect())
    }

    /// `anchors`: each virtual line's byte position, ascending, and whether
    /// it is drawn above its line.
    fn from_anchors(buffer: &Buffer, anchors: Vec<(usize, bool)>) -> Option<Self> {
        if buffer.is_large_file() {
            return None;
        }
        let line_count = buffer.line_count()?.max(1);
        let last_line = line_count - 1;
        let mut virtual_lines = Vec::with_capacity(anchors.len());
        let mut above = Vec::new();
        for (pos, is_above) in anchors {
            // A later byte is never an earlier line, so these stay ascending.
            let line = buffer.get_line_number(pos).min(last_line);
            virtual_lines.push(line);
            if is_above {
                above.push(line);
            }
        }
        Some(Self {
            line_count,
            virtual_lines,
            above,
        })
    }

    /// Whether any virtual line is drawn: without one, rows are lines.
    pub fn has_virtual_lines(&self) -> bool {
        !self.virtual_lines.is_empty()
    }

    pub fn total_rows(&self) -> usize {
        self.line_count + self.virtual_lines.len()
    }

    /// The row `line`'s first row (its first virtual line above, or its text)
    /// is drawn on.
    pub fn first_row(&self, line: usize) -> usize {
        let line = line.min(self.line_count);
        line + self.virtual_lines.partition_point(|l| *l < line)
    }

    /// The row `line`'s text is drawn on.
    pub fn text_row(&self, line: usize) -> usize {
        let above =
            self.above.partition_point(|l| *l <= line) - self.above.partition_point(|l| *l < line);
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
        let rows_in_line = self.first_row(lo + 1) - self.first_row(lo);
        (
            lo,
            (row - self.first_row(lo)).min(rows_in_line.saturating_sub(1)),
        )
    }

    /// The furthest a pane `height` rows tall scrolls: its last page.
    pub fn max_top(&self, height: usize) -> usize {
        self.total_rows().saturating_sub(height)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rows(line_count: usize, virtual_lines: &[usize], above: &[usize]) -> FlatRows {
        FlatRows {
            line_count,
            virtual_lines: virtual_lines.to_vec(),
            above: above.to_vec(),
        }
    }

    #[test]
    fn rows_are_lines_without_virtual_lines() {
        let r = rows(10, &[], &[]);
        assert_eq!(r.total_rows(), 10);
        assert_eq!(r.first_row(4), 4);
        assert_eq!(r.text_row(4), 4);
        assert_eq!(r.locate(4), (4, 0));
        assert_eq!(r.max_top(3), 7);
    }

    #[test]
    fn virtual_lines_below_a_line_follow_its_text() {
        // Three filler rows below line 2.
        let r = rows(10, &[2, 2, 2], &[]);
        assert_eq!(r.total_rows(), 13);
        assert_eq!(r.text_row(2), 2);
        assert_eq!(r.first_row(3), 6);
        assert_eq!(r.locate(2), (2, 0));
        assert_eq!(r.locate(4), (2, 2));
        assert_eq!(r.locate(5), (2, 3));
        assert_eq!(r.locate(6), (3, 0));
        // Past the end: the last line's last row.
        assert_eq!(r.locate(100), (9, 0));
    }

    #[test]
    fn virtual_lines_above_a_line_precede_its_text() {
        // Two rows above line 0 (a change at the very top of a file).
        let r = rows(5, &[0, 0], &[0, 0]);
        assert_eq!(r.first_row(0), 0);
        assert_eq!(r.text_row(0), 2);
        assert_eq!(r.first_row(1), 3);
        assert_eq!(r.locate(1), (0, 1));
    }
}
