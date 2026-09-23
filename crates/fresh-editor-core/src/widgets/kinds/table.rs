//! **A table**: a `Tree` whose nodes carry cells (`Tree::columns`,
//! `TreeNode::cells`).
//!
//! A plugin used to lay its columns out itself: measure every cell, guess the
//! width the list would get from the terminal's width and the panel's share of
//! it, squeeze the widest column until the guess fit, elide each cell, and pad
//! it into one pre-rendered string per row. Every one of those steps is a
//! question about a width the plugin cannot see, so the columns came out wrong
//! whenever the real width differed from the guess — paths cut at both ends.
//!
//! The rules live here, as pure functions of the cells and the width layout
//! gives the rows:
//!
//! - [`natural_widths`]: each column as wide as its widest cell, capped by the
//!   column's `max_width`;
//! - [`fit`]: too wide for the room, the widest column gives a column at a
//!   time (so a long path goes before a short name), never below a floor;
//! - [`elide`]: a cell too wide for its column is cut at the end its column
//!   names, with `…` marking the cut;
//! - [`row_entry`] / [`header_entry`]: a row of cells, and the titles over
//!   them, laid out on those widths.

use fresh_core::api::{Elide, OverlayOptions, TableCell, TableColumn};
use fresh_core::text_property::{InlineOverlay, OffsetUnit, TextPropertyEntry};

use crate::primitives::display_width::str_width;

/// Blank columns between one column and the next.
pub const GAP: u32 = 2;

/// The narrowest a column is squeezed to when the table must fit.
pub const MIN_COL: u32 = 6;

/// Each column's natural width: its widest cell (or its title), capped by the
/// column's `max_width`.
pub fn natural_widths<'a>(
    columns: &[TableColumn],
    rows: impl IntoIterator<Item = &'a [TableCell]>,
) -> Vec<u32> {
    let mut w: Vec<u32> = columns.iter().map(|c| str_width(&c.title) as u32).collect();
    for cells in rows {
        for (i, cell) in cells.iter().enumerate().take(columns.len()) {
            w[i] = w[i].max(str_width(&cell.text) as u32);
        }
    }
    for (i, c) in columns.iter().enumerate() {
        if c.max_width > 0 {
            w[i] = w[i].min(c.max_width);
        }
    }
    w
}

/// Fit `natural` into `room` display columns, gaps included: the widest
/// column gives one column at a time until the table fits or every column is
/// at the floor.
pub fn fit(natural: &[u32], room: u32) -> Vec<u32> {
    let mut w = natural.to_vec();
    let gaps = GAP * (w.len().saturating_sub(1) as u32);
    let used = |w: &[u32]| gaps + w.iter().sum::<u32>();
    while used(&w) > room {
        let Some((i, &widest)) = w
            .iter()
            .enumerate()
            .max_by_key(|(i, v)| (**v, usize::MAX - i))
        else {
            break;
        };
        if widest <= MIN_COL {
            break;
        }
        w[i] -= 1;
    }
    w
}

/// `text` cut to `width` display columns, the cut marked with `…` at the end
/// `keep` names: `Elide::Head` drops the head (a path keeps its tail),
/// anything else drops the tail (a name keeps its head).
pub fn elide(text: &str, width: u32, keep: Elide) -> String {
    let width = width as usize;
    if str_width(text) <= width {
        return text.to_string();
    }
    if width <= 1 {
        return "…".to_string();
    }
    let chars: Vec<char> = text.chars().collect();
    let mut taken = String::new();
    match keep {
        Elide::Head => {
            for ch in chars.iter().rev() {
                let next = format!("{ch}{taken}");
                if str_width(&next) + 1 > width {
                    break;
                }
                taken = next;
            }
            format!("…{taken}")
        }
        _ => {
            for ch in &chars {
                taken.push(*ch);
                if str_width(&taken) + 1 > width {
                    taken.pop();
                    break;
                }
            }
            format!("{taken}…")
        }
    }
}

/// Append `text` padded to `width` columns, styled with `style`.
fn push_cell(
    e: &mut TextPropertyEntry,
    text: &str,
    width: u32,
    style: Option<&OverlayOptions>,
    pad_after: u32,
) {
    let start = e.text.len();
    e.text.push_str(text);
    let end = e.text.len();
    let pad = (width as usize).saturating_sub(str_width(text)) + pad_after as usize;
    e.text.push_str(&" ".repeat(pad));
    if let Some(style) = style {
        e.inline_overlays.push(InlineOverlay {
            start,
            end,
            style: style.clone(),
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }
}

/// One row: each cell in its column, elided at the column's end and padded
/// to its width, a gap between columns.
pub fn row_entry(
    columns: &[TableColumn],
    cells: &[TableCell],
    widths: &[u32],
) -> TextPropertyEntry {
    let mut e = TextPropertyEntry::text("");
    let n = widths.len();
    for (i, w) in widths.iter().enumerate() {
        let cell = cells.get(i);
        let keep = columns.get(i).map(|c| c.elide).unwrap_or_default();
        let text = elide(cell.map(|c| c.text.as_str()).unwrap_or(""), *w, keep);
        let gap = if i + 1 < n { GAP } else { 0 };
        push_cell(&mut e, &text, *w, cell.and_then(|c| c.style.as_ref()), gap);
    }
    e
}

/// The header row: each title over its column, in the table's header ink.
pub fn header_entry(columns: &[TableColumn], widths: &[u32]) -> TextPropertyEntry {
    let style = OverlayOptions {
        fg: Some(fresh_core::api::OverlayColorSpec::theme_key(
            "ui.menu_disabled_fg",
        )),
        bold: true,
        ..Default::default()
    };
    let mut e = TextPropertyEntry::text("");
    let n = widths.len();
    for (i, w) in widths.iter().enumerate() {
        let title = elide(
            columns.get(i).map(|c| c.title.as_str()).unwrap_or(""),
            *w,
            Elide::Tail,
        );
        let gap = if i + 1 < n { GAP } else { 0 };
        push_cell(&mut e, &title, *w, Some(&style), gap);
    }
    e
}

#[cfg(test)]
mod tests {
    use super::*;

    fn col(title: &str, elide: Elide, max: u32) -> TableColumn {
        TableColumn {
            title: title.into(),
            elide,
            max_width: max,
        }
    }

    fn cells(v: &[&str]) -> Vec<TableCell> {
        v.iter()
            .map(|t| TableCell {
                text: (*t).into(),
                style: None,
            })
            .collect()
    }

    #[test]
    fn a_path_keeps_its_tail_and_a_name_its_head() {
        assert_eq!(
            elide("/home/user/src/project", 10, Elide::Head),
            "…c/project"
        );
        assert_eq!(elide("a long session name", 10, Elide::Tail), "a long se…");
        assert_eq!(elide("short", 10, Elide::Head), "short");
    }

    #[test]
    fn columns_are_as_wide_as_their_widest_cell_capped() {
        let cols = [col("Name", Elide::Tail, 0), col("Dir", Elide::Head, 10)];
        let a = cells(&["alpha", "/a/very/long/directory/path"]);
        let b = cells(&["a much longer name", "/b"]);
        let w = natural_widths(&cols, [a.as_slice(), b.as_slice()]);
        assert_eq!(w, vec![18, 10]);
    }

    /// Too wide: the widest column gives first, so the long path is cut
    /// before the short name is.
    #[test]
    fn the_widest_column_gives_first() {
        let w = fit(&[10, 40, 8], 50);
        assert_eq!(w.iter().sum::<u32>() + 2 * GAP, 50);
        assert_eq!(w[0], 10);
        assert_eq!(w[2], 8);
        assert_eq!(w[1], 50 - 10 - 8 - 2 * GAP);
    }

    #[test]
    fn a_row_lays_its_cells_on_the_widths() {
        let cols = [col("Name", Elide::Tail, 0), col("Dir", Elide::Head, 0)];
        let e = row_entry(&cols, &cells(&["alpha", "/src/x"]), &[6, 6]);
        assert_eq!(e.text, "alpha   /src/x");
        let e = row_entry(&cols, &cells(&["alphabetic", "/src/project"]), &[6, 6]);
        assert_eq!(e.text, "alpha…  …oject");
        let h = header_entry(&cols, &[6, 6]);
        assert_eq!(h.text, "Name    Dir   ");
    }
}
