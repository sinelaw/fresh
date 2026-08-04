//! Continuation-cell normalization for double-width glyphs.
//!
//! A grapheme two columns wide occupies two buffer cells: the glyph lands in
//! the first, and ratatui reserves the second by `reset()`ing it — which
//! leaves its symbol as a plain space. Nothing is supposed to print that
//! space: `Buffer::diff` is expected to suppress the continuation cell,
//! because the terminal already advanced two columns for the glyph itself.
//!
//! That suppression is unreliable. For a cluster that is double-width by
//! virtue of a variation selector — `❤\u{FE0F}`, `1\u{FE0F}\u{20E3}` — the
//! diff emits the continuation cell as well, so the backend prints the glyph
//! (terminal advances 2) *and* a space (advances 1). From there the terminal
//! sits one column right of where ratatui believes it is, and because the
//! backend only re-positions on a non-consecutive cell, every later write on
//! that row lands one column off. The columns those drifted writes step over
//! are never repainted, so they keep whatever the previous frame left there —
//! stray characters scattered through the row until something forces a full
//! repaint (issue #2877, seen while scrolling a buffer up and down).
//!
//! Emptying the continuation symbol makes the backend print nothing for it,
//! which is exactly the effect ratatui already gets for the clusters whose
//! continuation cell `diff` does drop. The glyph still paints both columns,
//! so the cell's own style is irrelevant — it is never drawn.

use crate::primitives::display_width::str_width;
use ratatui::buffer::Buffer;

/// Blank the symbol of every cell that only exists to reserve columns for the
/// double-width glyph in front of it.
///
/// Runs over the finished frame, so it covers every layer — splits, dock,
/// modals, animations — and every frontend that paints through `Editor::render`.
pub fn normalize_wide_glyph_padding(buf: &mut Buffer) {
    let area = *buf.area();
    if area.is_empty() {
        return;
    }

    for y in area.top()..area.bottom() {
        let mut x = area.left();
        while x < area.right() {
            let width = str_width(buf[(x, y)].symbol());
            // `width` is 0 for a cell we already blanked on an earlier pass
            // (or one ratatui left empty itself); step over it as a single
            // column so the scan always advances.
            let advance = width.max(1) as u16;

            // Cells 1..width of the glyph are pure padding: the glyph covers
            // those columns on screen, so printing anything for them shifts
            // the terminal past where the buffer says the row is.
            for pad in 1..advance {
                let Some(px) = x.checked_add(pad) else { break };
                if px >= area.right() {
                    break;
                }
                buf[(px, y)].set_symbol("");
            }

            x = x.saturating_add(advance);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::layout::Rect;
    use ratatui::text::Line;
    use ratatui::widgets::Widget;

    /// Walk a diff the way `CrosstermBackend::draw` does — it only emits a
    /// cursor move when the next cell is not adjacent to the last one — and
    /// return the column the terminal actually ends on versus the column the
    /// buffer says the row ends on. Terminal advance is measured with
    /// `str_width`, which is what real terminals do for these clusters.
    fn terminal_vs_buffer_end(prev: &Buffer, next: &Buffer) -> (usize, usize) {
        let updates = prev.diff(next);
        let mut term_col = 0usize;
        let mut last_x: Option<u16> = None;
        for (x, _, cell) in updates.iter() {
            if last_x != Some(x.wrapping_sub(1)) {
                term_col = *x as usize;
            }
            term_col += str_width(cell.symbol());
            last_x = Some(*x);
        }
        let buffer_col = updates
            .last()
            .map(|(x, _, cell)| *x as usize + str_width(cell.symbol()).max(1))
            .unwrap_or(0);
        (term_col, buffer_col)
    }

    fn render_row(text: &str, width: u16) -> Buffer {
        let area = Rect::new(0, 0, width, 1);
        let mut buf = Buffer::empty(area);
        Line::from(text.to_string()).render(area, &mut buf);
        buf
    }

    #[test]
    fn variation_selector_glyph_does_not_drift_the_terminal() {
        // `❤\u{FE0F}` is the cluster from issue #2877: two columns wide, but
        // the diff keeps its continuation cell, so the row drifts right.
        let prev = render_row(&"X".repeat(16), 16);
        let mut next = render_row(&"\u{2764}\u{FE0F}".repeat(3), 16);

        let (term, buffer) = terminal_vs_buffer_end(&prev, &next);
        assert_ne!(
            term, buffer,
            "precondition: without the fix the terminal must drift past the buffer"
        );

        normalize_wide_glyph_padding(&mut next);
        let (term, buffer) = terminal_vs_buffer_end(&prev, &next);
        assert_eq!(
            term, buffer,
            "terminal must end on the column the buffer says the row ends on"
        );
    }

    #[test]
    fn keycap_glyph_does_not_drift_the_terminal() {
        let prev = render_row(&"X".repeat(16), 16);
        let mut next = render_row(&"1\u{FE0F}\u{20E3}".repeat(3), 16);

        normalize_wide_glyph_padding(&mut next);
        let (term, buffer) = terminal_vs_buffer_end(&prev, &next);
        assert_eq!(term, buffer);
    }

    /// Clusters whose continuation cell `diff` already drops must keep
    /// behaving — the pass blanks a symbol nothing was going to print.
    #[test]
    fn already_correct_glyphs_stay_correct() {
        for unit in ["\u{1F600}", "\u{6F22}", "\u{1F1EF}\u{1F1F5}"] {
            let prev = render_row(&"X".repeat(16), 16);
            let mut next = render_row(&unit.repeat(3), 16);

            let (before_term, before_buffer) = terminal_vs_buffer_end(&prev, &next);
            assert_eq!(before_term, before_buffer, "{unit:?} was already aligned");

            normalize_wide_glyph_padding(&mut next);
            let (term, buffer) = terminal_vs_buffer_end(&prev, &next);
            assert_eq!(term, buffer, "{unit:?} must stay aligned");
        }
    }

    /// Narrow content must come through the pass untouched.
    #[test]
    fn narrow_cells_are_untouched() {
        let mut buf = render_row("int value8 = compute(8);", 30);
        let before: Vec<String> = (0..30).map(|x| buf[(x, 0)].symbol().to_string()).collect();
        normalize_wide_glyph_padding(&mut buf);
        let after: Vec<String> = (0..30).map(|x| buf[(x, 0)].symbol().to_string()).collect();
        assert_eq!(before, after);
    }

    /// A double-width glyph in the last column has no continuation cell to
    /// blank; the pass must not run off the end of the row.
    #[test]
    fn wide_glyph_in_final_column_is_safe() {
        let area = Rect::new(0, 0, 3, 1);
        let mut buf = Buffer::empty(area);
        buf[(2, 0)].set_symbol("\u{2764}\u{FE0F}");
        normalize_wide_glyph_padding(&mut buf);
        assert_eq!(buf[(2, 0)].symbol(), "\u{2764}\u{FE0F}");
    }
}
