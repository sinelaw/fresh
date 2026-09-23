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
//! Now each cell is a node of the tree. What is left here is the one fact that
//! is data rather than layout: each column's **natural width** over *every*
//! row, measured from the cells — a table's rows are a windowed list, which
//! builds only the rows on screen, so a width measured by layout would change
//! as the list scrolled. The shell hands these to `fresh_ui::Columns`, and
//! layout fits them to the room each row really has (the widest column gives
//! first, down to [`MIN_COL`]) and cuts each cell at the end its column names
//! (`fresh_ui::Elide`).

use fresh_core::api::{TableCell, TableColumn};

use crate::primitives::display_width::str_width;

/// Blank columns between one column and the next.
pub const GAP: u16 = 2;

/// The narrowest a column is squeezed to when the table must fit.
pub const MIN_COL: u16 = 6;

/// Each column's natural width: its widest cell (or its title), capped by the
/// column's `max_width`.
pub fn natural_widths<'a>(
    columns: &[TableColumn],
    rows: impl IntoIterator<Item = &'a [TableCell]>,
) -> Vec<u16> {
    let clamp = |w: usize| w.min(u16::MAX as usize) as u16;
    let mut w: Vec<u16> = columns.iter().map(|c| clamp(str_width(&c.title))).collect();
    for cells in rows {
        for (i, cell) in cells.iter().enumerate().take(columns.len()) {
            w[i] = w[i].max(clamp(str_width(&cell.text)));
        }
    }
    for (i, c) in columns.iter().enumerate() {
        if c.max_width > 0 {
            w[i] = w[i].min(clamp(c.max_width as usize));
        }
    }
    w
}

#[cfg(test)]
mod tests {
    use super::*;
    use fresh_core::api::Elide;

    fn col(title: &str, max: u32) -> TableColumn {
        TableColumn {
            title: title.into(),
            elide: Elide::Tail,
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

    /// Over every row — not only the ones a window would build — so a
    /// column does not change width as the list scrolls.
    #[test]
    fn columns_are_as_wide_as_their_widest_cell_capped() {
        let cols = [col("Name", 0), col("Dir", 10)];
        let a = cells(&["alpha", "/a/very/long/directory/path"]);
        let b = cells(&["a much longer name", "/b"]);
        let w = natural_widths(&cols, [a.as_slice(), b.as_slice()]);
        assert_eq!(w, vec![18, 10]);
    }

    #[test]
    fn a_title_wider_than_its_cells_sets_the_width() {
        let cols = [col("Directory", 0)];
        let a = cells(&["/p"]);
        assert_eq!(natural_widths(&cols, [a.as_slice()]), vec![9]);
    }
}
