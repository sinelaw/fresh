//! Display-width policy: which columns a row of text occupies.
//!
//! Layout measures text with `unicode-width`, so a run of `你好` reserves four
//! cells. Every backend that paints a [`Draw::Lines`](crate::Draw) must then
//! *advance* by the same widths, or a wide glyph is painted into one cell and
//! everything after it lands two cells left of where layout put it — which is
//! exactly what happened while each backend stepped one column per `char`.
//! This module is the one statement of the policy, walked by every backend:
//! the library's own reference backend, the interactive example, and the
//! editor's ratatui fold.
//!
//! # The policy
//!
//! * **The unit is the extended grapheme cluster** (UAX #29), not the `char`.
//!   A base letter and its combining marks are one cluster and paint into one
//!   cell; an emoji ZWJ sequence (`👨‍👩‍👧`) is one cluster and paints into two
//!   cells, not six. Per-`char` widths cannot express either: `unicode-width`
//!   measures a *string* as the sum of its clusters, so painting by cluster is
//!   what keeps paint in step with measure.
//! * **A cluster's width is `unicode-width`'s width of the cluster**: one cell
//!   for most text, two for East Asian wide and fullwidth glyphs and for emoji
//!   presentation, zero for a cluster that is nothing but marks.
//! * **A zero-width cluster consumes no column and is not painted.** A lone
//!   combining mark, a zero-width joiner or space, a variation selector with no
//!   base — there is no cell for it to go in. (Inside a cluster, a mark is part
//!   of the glyph in its base's cell.)
//! * **A wide cluster occupies its first column and blanks the rest.** The
//!   cells after the first are *continuation* cells: they show nothing of their
//!   own, and a backend whose cell model has a glyph per cell must write a
//!   blank into them so that whatever was there before does not show through
//!   (ratatui's `Cell::set_symbol` does not do this for the caller).
//! * **A wide cluster that would straddle the clip edge is not painted.** Half
//!   a glyph cannot be drawn. Its visible columns are painted as blanks in the
//!   item's style, so nothing stale shows through the hole, and the walk still
//!   advances by the cluster's full width so what follows is unaffected.
//! * **Control characters paint as `unicode-width` measures them.** `\t` and
//!   `\r` are one cell each to `unicode-width` 0.2, so they are one cell here;
//!   a backend that cannot show them shows a blank. Rows never contain `\n`:
//!   shaping splits on it before anything reaches a backend.
//!
//! # What this does not decide
//!
//! Where a *run* boundary splits a cluster — a base letter in one styled piece
//! and its mark in the next — each piece is measured and painted on its own,
//! so the mark becomes a zero-width cluster of its own and is dropped. The
//! same is true of an emoji sequence cut by a style boundary. Both are
//! defects of the description rather than of the painter, and no backend
//! attempts to repair them.

use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

/// One painted cluster: where it starts, what it is, and how many columns it
/// takes. `width` is never zero.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Glyph<'a> {
    /// The column the cluster starts in.
    pub x: i32,
    /// The cluster's text — one or more `char`s, painted as one symbol into
    /// the cell at `x`. A backend that stores a `char` per cell keeps the
    /// first and loses the marks; that is its limitation, not the policy's.
    pub text: &'a str,
    /// Columns the cluster covers, from `x`. Cells `x + 1 .. x + width` are
    /// continuation cells (see the module docs).
    pub width: u16,
}

impl Glyph<'_> {
    /// The column just past this cluster.
    pub fn end(&self) -> i32 {
        self.x + self.width as i32
    }
}

/// The display width of `s`, in cells — what layout measures a run at, and
/// what [`glyphs`] advances by in total.
///
/// `unicode-width` defines a string's width as the sum of its extended
/// grapheme clusters' widths (and [`glyphs`] is tested against that), so this
/// needs no segmentation of its own.
pub fn width(s: &str) -> u16 {
    UnicodeWidthStr::width(s).min(u16::MAX as usize) as u16
}

/// The clusters of `line` and the column each starts in, beginning at `x0`.
///
/// Zero-width clusters are dropped, per the policy above; every yielded glyph
/// has `width >= 1`, and the glyphs are contiguous: each starts where the one
/// before it ended.
pub fn glyphs(line: &str, x0: i32) -> impl Iterator<Item = Glyph<'_>> {
    let mut x = x0;
    line.graphemes(true).filter_map(move |g| {
        let w = width(g);
        if w == 0 {
            return None;
        }
        let at = x;
        x += w as i32;
        Some(Glyph {
            x: at,
            text: g,
            width: w,
        })
    })
}

/// [`glyphs`], cut to the columns `lo..hi`.
///
/// A cluster wholly inside the range is yielded as it is. One wholly outside
/// is dropped. A *wide* cluster that straddles either edge is yielded as one
/// blank (`" "`, width 1) per column of it that is inside the range, so the
/// caller paints the visible half of the hole rather than half a glyph — and
/// every glyph this yields can be painted without a further clip test on its
/// columns.
pub fn glyphs_in(line: &str, x0: i32, lo: i32, hi: i32) -> impl Iterator<Item = Glyph<'_>> {
    // A wide glyph straddling an edge becomes up to `width - 1` blanks, so the
    // walk carries a small queue of what it has yet to hand out.
    let mut pending: std::collections::VecDeque<Glyph<'_>> = std::collections::VecDeque::new();
    let mut inner = glyphs(line, x0);
    // Stops at the first cluster past the range: nothing after it is inside.
    let mut done = false;
    std::iter::from_fn(move || loop {
        if let Some(g) = pending.pop_front() {
            return Some(g);
        }
        if done {
            return None;
        }
        let g = inner.next()?;
        if g.x >= hi {
            done = true;
            return None;
        }
        if g.end() <= lo {
            continue;
        }
        if g.x >= lo && g.end() <= hi {
            return Some(g);
        }
        // Straddling. Only a wide glyph can, and only its visible columns are
        // painted — as blanks.
        for col in g.x.max(lo)..g.end().min(hi) {
            pending.push_back(Glyph {
                x: col,
                text: " ",
                width: 1,
            });
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn cols(line: &str) -> Vec<(i32, &str, u16)> {
        glyphs(line, 0).map(|g| (g.x, g.text, g.width)).collect()
    }

    #[test]
    fn ascii_is_one_column_per_char() {
        assert_eq!(cols("fn"), vec![(0, "f", 1), (1, "n", 1)]);
    }

    #[test]
    fn a_wide_glyph_takes_two_columns_and_the_next_starts_after_it() {
        assert_eq!(cols("你好"), vec![(0, "你", 2), (2, "好", 2)]);
        assert_eq!(cols("你a"), vec![(0, "你", 2), (2, "a", 1)]);
    }

    #[test]
    fn a_combining_mark_rides_in_its_base_cell() {
        // e + U+0301: one cluster, one cell, the mark inside it.
        assert_eq!(cols("e\u{301}x"), vec![(0, "e\u{301}", 1), (1, "x", 1)]);
    }

    #[test]
    fn a_lone_mark_consumes_no_column_and_is_not_painted() {
        assert_eq!(cols("\u{301}x"), vec![(0, "x", 1)]);
        assert_eq!(cols("\u{200d}"), vec![]);
        assert_eq!(cols("a\u{200b}b"), vec![(0, "a", 1), (1, "b", 1)]);
    }

    #[test]
    fn an_emoji_sequence_is_one_two_column_glyph() {
        let fam = "👨\u{200d}👩\u{200d}👧";
        assert_eq!(cols(fam), vec![(0, fam, 2)]);
        // Emoji presentation via VS16 is two cells; the bare heart is one.
        assert_eq!(cols("❤\u{fe0f}"), vec![(0, "❤\u{fe0f}", 2)]);
        assert_eq!(cols("❤"), vec![(0, "❤", 1)]);
    }

    #[test]
    fn the_walk_advances_by_exactly_the_measured_width() {
        for s in [
            "你好",
            "fn 你好 fn",
            "e\u{301}",
            "👨\u{200d}👩\u{200d}👧!",
            "🇯🇵",
            "ｆｕｌｌ",
            "\t",
            "",
        ] {
            let end = glyphs(s, 3).last().map(|g| g.end()).unwrap_or(3);
            assert_eq!(end - 3, width(s) as i32, "{s:?}");
        }
    }

    #[test]
    fn clipping_keeps_whole_glyphs_and_blanks_a_straddler() {
        let v: Vec<_> = glyphs_in("你好", 0, 0, 4).collect();
        assert_eq!(v.len(), 2);
        // Only three columns: `好` straddles the edge and its visible column
        // is a blank.
        let v: Vec<_> = glyphs_in("你好", 0, 0, 3)
            .map(|g| (g.x, g.text, g.width))
            .collect();
        assert_eq!(v, vec![(0, "你", 2), (2, " ", 1)]);
        // Straddling the left edge likewise.
        let v: Vec<_> = glyphs_in("你好", 0, 1, 4)
            .map(|g| (g.x, g.text, g.width))
            .collect();
        assert_eq!(v, vec![(1, " ", 1), (2, "好", 2)]);
        // Wholly outside: nothing.
        assert_eq!(glyphs_in("你好", 0, 4, 8).count(), 0);
        assert_eq!(glyphs_in("abc", 5, 0, 5).count(), 0);
    }
}
