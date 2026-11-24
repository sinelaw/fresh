//! The last step between a rendered frame and the terminal.
//!
//! ratatui's `Terminal::flush` diffs the new frame against the previous one
//! and hands the changed cells to the backend, which writes each cell and only
//! moves the cursor explicitly when a cell is not the right-hand neighbour of
//! the one written before it. Everything else is left to the terminal's own
//! cursor advance — so every write after a grapheme cluster lands where the
//! *terminal* thinks that cluster ended, not where ratatui thinks it did.
//!
//! The two disagree for multi-codepoint clusters. An emoji presentation
//! sequence such as `❤️` (U+2764 U+FE0F), a keycap (`1️⃣`) or a flag is two
//! columns wide by Unicode's reckoning and to tmux, kitty, WezTerm and iTerm2,
//! but one column to terminals that size it by its base codepoint (xterm,
//! older VTE, the `vt100` crate). ratatui's diff makes this worse for VS16
//! clusters: to clear stale content it emits the cluster's trailing column as
//! a separate cell *right after* the cluster. That trailing cell is the
//! cluster's right-hand neighbour, so no cursor move is written; on a terminal
//! that advanced two columns it lands one column too far right, and every
//! following cell on the row drifts with it. Cells the diff considers
//! unchanged are never rewritten, so the stale characters stay on screen
//! until a full repaint (issue #2877).
//!
//! [`ClusterSafeBackend`] wraps the real backend and reorders and splits the
//! diff so that no write ever depends on how far the terminal advanced over a
//! cluster whose width it may size differently:
//!
//! * A wide cell's trailing columns are written **before** the cell itself.
//!   The cluster is then drawn last, at an explicitly positioned cursor, and
//!   covers those columns on a terminal that draws it wide, or leaves them
//!   correctly cleared on one that draws it narrow.
//! * After any multi-codepoint cluster the next write is explicitly
//!   positioned, even when it is the cluster's neighbour.
//!
//! This is the same reordering ratatui itself adopted upstream
//! (ratatui/ratatui#2686), applied at the backend boundary so it covers every
//! backend Fresh draws through — the local terminal and the session server's
//! capture backend alike — and does not wait on a ratatui release.

use ratatui::backend::{Backend, ClearType, WindowSize};
use ratatui::buffer::Cell;
use ratatui::layout::{Position, Size};
use unicode_width::UnicodeWidthStr;

/// A [`Backend`] wrapper that makes the frame's cell writes independent of
/// how wide the terminal draws each grapheme cluster. See the module docs.
#[derive(Debug)]
pub struct ClusterSafeBackend<B> {
    inner: B,
}

impl<B> ClusterSafeBackend<B> {
    pub fn new(inner: B) -> Self {
        Self { inner }
    }

    pub fn inner(&self) -> &B {
        &self.inner
    }

    pub fn inner_mut(&mut self) -> &mut B {
        &mut self.inner
    }
}

/// The number of columns the frame reserves for `cell` — the same measure
/// ratatui's `Buffer::diff` uses to skip the columns a wide cell covers.
fn reserved_width(cell: &Cell) -> u16 {
    u16::try_from(cell.symbol().width()).unwrap_or(u16::MAX)
}

/// Whether terminals may disagree about how far `cell` advances the cursor.
///
/// A single codepoint has one width every terminal agrees on (the ambiguous
/// East Asian widths aside, which are a terminal setting, not a property of
/// the text). A cluster of several — an emoji presentation sequence, a
/// keycap, a flag, a ZWJ sequence, a base with combining marks — is sized
/// differently by different terminals.
fn advance_is_uncertain(cell: &Cell) -> bool {
    cell.symbol().chars().nth(1).is_some()
}

/// Reorder a frame's updates so that every wide cell's trailing columns are
/// written before the cell itself, and split them into runs at every point
/// where the write after a cluster would otherwise rely on the terminal's
/// cursor advance. Each run is drawn with a fresh cursor position.
fn plan_runs<'a>(updates: Vec<(u16, u16, &'a Cell)>) -> Vec<Vec<(u16, u16, &'a Cell)>> {
    let mut runs: Vec<Vec<(u16, u16, &'a Cell)>> = Vec::new();
    let mut run: Vec<(u16, u16, &'a Cell)> = Vec::with_capacity(updates.len());
    let mut i = 0;
    while i < updates.len() {
        let (x, y, cell) = updates[i];
        let width = reserved_width(cell);
        // The trailing columns the diff emitted for this cell, if any: the
        // updates that immediately follow it on the same row, inside the
        // columns it covers.
        let mut trailing_end = i + 1;
        while let Some(&(tx, ty, _)) = updates.get(trailing_end) {
            if ty == y && tx > x && u32::from(tx) < u32::from(x) + u32::from(width) {
                trailing_end += 1;
            } else {
                break;
            }
        }
        run.extend_from_slice(&updates[i + 1..trailing_end]);
        run.push((x, y, cell));
        i = trailing_end;

        // The next write must not rely on the terminal's advance over this
        // cell. When it is a neighbour the inner backend would skip the
        // cursor move, so end the run here; any other position already gets
        // an explicit move.
        if advance_is_uncertain(cell) {
            if let Some(&(nx, ny, _)) = updates.get(i) {
                if ny == y && u32::from(nx) == u32::from(x) + 1 {
                    runs.push(std::mem::take(&mut run));
                }
            }
        }
    }
    if !run.is_empty() {
        runs.push(run);
    }
    runs
}

impl<B: Backend> Backend for ClusterSafeBackend<B> {
    type Error = B::Error;

    fn draw<'a, I>(&mut self, content: I) -> Result<(), Self::Error>
    where
        I: Iterator<Item = (u16, u16, &'a Cell)>,
    {
        let updates: Vec<_> = content.collect();
        if updates.is_empty() {
            // Keep an unchanged frame byte-empty: the session server skips
            // broadcasting frames that wrote nothing.
            return Ok(());
        }
        for run in plan_runs(updates) {
            self.inner.draw(run.into_iter())?;
        }
        Ok(())
    }

    fn append_lines(&mut self, n: u16) -> Result<(), Self::Error> {
        self.inner.append_lines(n)
    }

    fn hide_cursor(&mut self) -> Result<(), Self::Error> {
        self.inner.hide_cursor()
    }

    fn show_cursor(&mut self) -> Result<(), Self::Error> {
        self.inner.show_cursor()
    }

    fn get_cursor_position(&mut self) -> Result<Position, Self::Error> {
        self.inner.get_cursor_position()
    }

    fn set_cursor_position<P: Into<Position>>(&mut self, position: P) -> Result<(), Self::Error> {
        self.inner.set_cursor_position(position)
    }

    fn clear(&mut self) -> Result<(), Self::Error> {
        self.inner.clear()
    }

    fn clear_region(&mut self, clear_type: ClearType) -> Result<(), Self::Error> {
        self.inner.clear_region(clear_type)
    }

    fn size(&self) -> Result<Size, Self::Error> {
        self.inner.size()
    }

    fn window_size(&mut self) -> Result<WindowSize, Self::Error> {
        self.inner.window_size()
    }

    fn flush(&mut self) -> Result<(), Self::Error> {
        self.inner.flush()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn cell(symbol: &str) -> Cell {
        let mut cell = Cell::default();
        cell.set_symbol(symbol);
        cell
    }

    /// `(x, symbol)` of each write, run by run.
    fn plan(updates: &[(u16, &Cell)]) -> Vec<Vec<(u16, String)>> {
        plan_runs(updates.iter().map(|&(x, c)| (x, 0, c)).collect())
            .into_iter()
            .map(|run| {
                run.into_iter()
                    .map(|(x, _, c)| (x, c.symbol().to_string()))
                    .collect()
            })
            .collect()
    }

    #[test]
    fn wide_cluster_is_written_after_its_trailing_column() {
        let heart = cell("\u{2764}\u{FE0F}");
        let blank = cell(" ");
        let a = cell("a");
        // The order ratatui's diff emits: cluster, its trailing column, next.
        let runs = plan(&[(0, &a), (1, &heart), (2, &blank), (3, &a)]);
        assert_eq!(
            runs,
            vec![vec![
                (0, "a".to_string()),
                (2, " ".to_string()),
                (1, "\u{2764}\u{FE0F}".to_string()),
                // Not the cluster's neighbour, so the inner backend moves the
                // cursor here explicitly.
                (3, "a".to_string()),
            ]]
        );
    }

    #[test]
    fn write_after_narrow_multi_codepoint_cluster_starts_a_new_run() {
        let accented = cell("e\u{0301}");
        let b = cell("b");
        let runs = plan(&[(0, &accented), (1, &b), (2, &b)]);
        assert_eq!(
            runs,
            vec![
                vec![(0, "e\u{0301}".to_string())],
                vec![(1, "b".to_string()), (2, "b".to_string())],
            ]
        );
    }

    #[test]
    fn single_codepoint_cells_stay_in_one_run_in_order() {
        let a = cell("a");
        let han = cell("\u{6F22}");
        let runs = plan(&[(0, &a), (1, &han), (3, &a), (4, &a)]);
        assert_eq!(
            runs,
            vec![vec![
                (0, "a".to_string()),
                (1, "\u{6F22}".to_string()),
                (3, "a".to_string()),
                (4, "a".to_string()),
            ]]
        );
    }

    #[test]
    fn trailing_columns_on_another_row_are_not_reordered() {
        let heart = cell("\u{2764}\u{FE0F}");
        let a = cell("a");
        let runs = plan_runs(vec![(9, 0, &heart), (0, 1, &a)]);
        let order: Vec<_> = runs.concat().iter().map(|&(x, y, _)| (x, y)).collect();
        assert_eq!(order, vec![(9, 0), (0, 1)]);
    }
}
