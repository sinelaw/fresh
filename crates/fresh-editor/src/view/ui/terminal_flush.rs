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
//! sequence such as `❤️` (U+2764 U+FE0F) or a keycap (`1️⃣`) is two columns
//! wide by Unicode's reckoning and to tmux, kitty, WezTerm and iTerm2, but one
//! column to terminals that add up the widths of its codepoints (xterm, older
//! VTE, the `vt100` crate), to which VS16 is zero-width. ratatui's diff makes
//! this worse for VS16 clusters: to clear stale content it emits the
//! cluster's trailing column as a separate cell *right after* the cluster.
//! That trailing cell is the cluster's right-hand neighbour, so no cursor
//! move is written; on a terminal
//! that advanced two columns it lands one column too far right, and every
//! following cell on the row drifts with it. Cells the diff considers
//! unchanged are never rewritten, so the stale characters stay on screen
//! until a full repaint (issue #2877).
//!
//! [`ClusterSafeBackend`] wraps the real backend and reorders and splits the
//! diff so that no write ever depends on how far the terminal advanced over a
//! cluster whose width it may size differently:
//!
//! * A wide cell's trailing columns, when the diff sends them (it does for a
//!   VS16 cluster whose trailing column changed), are written **before** the
//!   cell itself. The cluster is then drawn last, at an explicitly positioned
//!   cursor, and covers those columns on a terminal that draws it wide, or
//!   leaves them cleared on one that draws it narrow. The diff never sends a
//!   trailing column for other wide clusters (flags, ZWJ sequences), so a
//!   terminal that draws one narrower than Unicode says can still show a
//!   stale character beside it; that is outside what a backend can fix.
//! * After any multi-codepoint cluster the next write is explicitly
//!   positioned, even when it is the cluster's neighbour.
//!
//! This is the same reordering ratatui itself adopted upstream
//! (ratatui/ratatui#2686), applied at the backend boundary so it covers every
//! backend Fresh draws through — the local terminal and the session server's
//! capture backend alike — and does not wait on a ratatui release.
//!
//! # Hyperlinks
//!
//! A terminal hyperlink (OSC 8) is an escape sequence around the linked text,
//! so the only way to get one through ratatui is inside a cell's symbol —
//! [`set_cell_hyperlink`]. That used to break the frame diff (#1227): the
//! diff measures a cell by its symbol, and a symbol carrying a URL measures
//! dozens of columns, so the cells after it were skipped. Each linked cell
//! therefore declares a width with `CellDiffOption::ForcedWidth`, and this
//! backend and every other reader that needs the drawn text
//! ([`visible_symbol`]) measure what it shows. Because the URL is part of
//! the symbol, a cell whose link appears, changes or disappears differs from
//! the previous frame's and is rewritten, like any other change.

use ratatui::backend::{Backend, ClearType, WindowSize};
use ratatui::buffer::{Cell, CellDiffOption, CellWidth};
use ratatui::layout::{Position, Size};
use std::num::NonZeroU16;
use unicode_width::UnicodeWidthStr;

/// Opens an OSC 8 hyperlink; followed by the link parameters, `;`, the URI
/// and [`OSC8_END`].
const OSC8_START: &str = "\x1b]8;";
/// Terminates an OSC 8 sequence (BEL; every terminal that supports OSC 8
/// accepts it, as does tmux's passthrough).
const OSC8_END: char = '\x07';
/// Closes the active hyperlink.
const OSC8_CLOSE: &str = "\x1b]8;;\x07";

/// Make `cell` a hyperlink to `url` when drawn on a terminal.
///
/// The URL is sanitised before it goes near the terminal: OSC 8 allows only
/// bytes 0x21–0x7E in a URI, and a document is untrusted input, so anything
/// else — control characters that could end the sequence early and inject
/// others, spaces, non-ASCII — is percent-encoded.
///
/// Cells that show nothing of their own (the trailing columns of a wide
/// character) and cells that are already links are left alone. `id` groups
/// every cell of one link — across the rows it wraps over — so terminals
/// highlight it as a whole on hover.
///
/// The cell is declared one column wide even when its glyph is wider. The
/// diff then still visits a wide glyph's trailing columns as cells of their
/// own and rewrites them when they change — the clearing it does for an
/// unlinked emoji, which a forced width of two would skip, leaving a stale
/// character beside a linked `❤️` on terminals that draw it wide.
/// [`ClusterSafeBackend`] measures the glyph itself and writes those
/// trailing cells before it.
pub fn set_cell_hyperlink(cell: &mut Cell, url: &str, id: &str) {
    let symbol = cell.symbol();
    if symbol.starts_with(OSC8_START) || symbol.width() == 0 {
        return;
    }
    let mut linked = String::with_capacity(symbol.len() + url.len() + id.len() + 16);
    linked.push_str(OSC8_START);
    linked.push_str("id=");
    push_uri_safe(&mut linked, id);
    linked.push(';');
    push_uri_safe(&mut linked, url);
    linked.push(OSC8_END);
    linked.push_str(symbol);
    linked.push_str(OSC8_CLOSE);
    cell.set_symbol(&linked);
    cell.set_diff_option(CellDiffOption::ForcedWidth(NonZeroU16::MIN));
}

/// Append `text` with every byte outside 0x21–0x7E percent-encoded. `;` and
/// `:` stay — they are legal in both the URI and (`:`-separated) params.
fn push_uri_safe(out: &mut String, text: &str) {
    for byte in text.bytes() {
        if (0x21..=0x7e).contains(&byte) {
            out.push(byte as char);
        } else {
            out.push_str(&format!("%{byte:02X}"));
        }
    }
}

/// The text a cell draws: its symbol without the hyperlink sequence
/// [`set_cell_hyperlink`] wrapped it in.
pub fn visible_symbol(symbol: &str) -> &str {
    let Some(rest) = symbol.strip_prefix(OSC8_START) else {
        return symbol;
    };
    let Some((_, inner)) = rest.split_once(OSC8_END) else {
        return symbol;
    };
    inner.strip_suffix(OSC8_CLOSE).unwrap_or(inner)
}

/// The hyperlink target of a cell [`set_cell_hyperlink`] linked.
pub fn cell_hyperlink(symbol: &str) -> Option<&str> {
    let rest = symbol.strip_prefix(OSC8_START)?;
    let (params_and_uri, _) = rest.split_once(OSC8_END)?;
    params_and_uri.split_once(';').map(|(_, uri)| uri)
}

/// Clear the screen and make the next draw repaint every cell, without
/// asking the terminal where its cursor is.
///
/// Since ratatui 0.30.1, `Terminal::clear` saves and restores the cursor
/// around the clear, and saving it is a cursor-position report request: a
/// blocking round trip to the terminal that fails after two seconds when the
/// terminal does not answer (a PTY nobody reads, a slow remote link). Fresh
/// positions the cursor itself on every draw, so it has nothing to restore.
/// `Terminal::resize` to the terminal's current size clears and resets the
/// back buffer the same way, with no query for a full-screen viewport.
pub fn clear_for_full_repaint<B: Backend>(
    terminal: &mut ratatui::Terminal<B>,
) -> Result<(), B::Error> {
    let area = terminal.size()?.into();
    terminal.resize(area)
}

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

/// The number of columns `cell` covers on screen.
///
/// Normally the measure ratatui's `Buffer::diff` uses (`cell_width`, which
/// honours a forced width). A cell can outlive its forced width — a widget
/// drawn over a hyperlink keeps the link cell's diff option while replacing
/// its symbol — so the drawn text's own width wins when it is wider.
fn reserved_width(cell: &Cell) -> u16 {
    let drawn = u16::try_from(visible_symbol(cell.symbol()).width()).unwrap_or(u16::MAX);
    cell.cell_width().max(drawn)
}

/// Whether terminals may disagree about how far `cell` advances the cursor.
///
/// A single codepoint has one width every terminal agrees on (the ambiguous
/// East Asian widths aside, which are a terminal setting, not a property of
/// the text). A cluster of several — an emoji presentation sequence, a
/// keycap, a flag, a ZWJ sequence, a base with combining marks — is sized
/// differently by different terminals.
fn advance_is_uncertain(cell: &Cell) -> bool {
    visible_symbol(cell.symbol()).chars().nth(1).is_some()
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

/// The OSC 8 opener of a linked cell's symbol (`ESC ] 8 ; params ; uri BEL`).
fn hyperlink_opener(symbol: &str) -> Option<&str> {
    let rest = symbol.strip_prefix(OSC8_START)?;
    let end = rest.find(OSC8_END)?;
    Some(&symbol[..OSC8_START.len() + end + OSC8_END.len_utf8()])
}

/// Within a run, write a link once per stretch of neighbouring cells rather
/// than once per cell.
///
/// Every linked cell carries the whole sequence (opener, URL, glyph, closer)
/// so that it stands on its own in the frame diff. Written that way, a
/// 300-character URL wrapping over a few rows costs some 100 KB per full
/// repaint — felt over SSH and by session-server clients. Cells written one
/// after another with no cursor move in between (the inner backend moves the
/// cursor only between non-neighbours) can share one opener and one closer:
/// the first keeps its opener, the last its closer, and the cells between are
/// written as their glyphs, still inside the link. Returns the replacement
/// for each cell of `run`, `None` where the cell is written as it is.
fn merge_hyperlinks(run: &[(u16, u16, &Cell)]) -> Vec<Option<Cell>> {
    let openers: Vec<Option<&str>> = run
        .iter()
        .map(|(_, _, cell)| hyperlink_opener(cell.symbol()))
        .collect();
    // Whether cell `i` continues cell `i - 1`'s link with no cursor move.
    let continues = |i: usize| -> bool {
        i > 0
            && openers[i].is_some()
            && openers[i] == openers[i - 1]
            && run[i].1 == run[i - 1].1
            && u32::from(run[i].0) == u32::from(run[i - 1].0) + 1
    };
    (0..run.len())
        .map(|i| {
            let opener = openers[i]?;
            let keep_opener = !continues(i);
            let keep_closer = !(i + 1 < run.len() && continues(i + 1));
            if keep_opener && keep_closer {
                return None;
            }
            let symbol = run[i].2.symbol();
            let mut merged = run[i].2.clone();
            let mut text = String::with_capacity(symbol.len());
            if keep_opener {
                text.push_str(opener);
            }
            text.push_str(visible_symbol(symbol));
            if keep_closer {
                text.push_str(OSC8_CLOSE);
            }
            merged.set_symbol(&text);
            Some(merged)
        })
        .collect()
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
            let merged = merge_hyperlinks(&run);
            self.inner.draw(
                run.iter()
                    .zip(&merged)
                    .map(|(&(x, y, cell), merged)| (x, y, merged.as_ref().unwrap_or(cell))),
            )?;
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

#[cfg(test)]
mod hyperlink_tests {
    use super::*;
    use ratatui::buffer::Buffer;
    use ratatui::layout::Rect;

    const URL: &str = "https://example.com/a/long/path";

    fn row(text: &str, width: u16) -> Buffer {
        let mut buf = Buffer::empty(Rect::new(0, 0, width, 1));
        buf.set_string(0, 0, text, ratatui::style::Style::default());
        buf
    }

    fn link(buf: &mut Buffer, cols: std::ops::Range<u16>) {
        for x in cols {
            set_cell_hyperlink(&mut buf[(x, 0)], URL, "id1");
        }
    }

    /// The text a terminal shows after replaying the diff of `frames`, one
    /// frame after another, onto a terminal that starts blank — each update
    /// overwrites its cell; wide cells cover the column after them.
    fn replay(frames: &[Buffer]) -> (String, Vec<Option<String>>) {
        let area = frames[0].area;
        let mut screen = Buffer::empty(area);
        let mut previous = Buffer::empty(area);
        for frame in frames {
            for (x, y, cell) in previous.diff(frame) {
                screen[(x, y)] = cell.clone();
            }
            previous = frame.clone();
        }
        let mut text = String::new();
        let mut links = Vec::new();
        let mut x = 0;
        while x < area.width {
            let symbol = screen[(x, 0)].symbol();
            let shown = visible_symbol(symbol);
            text.push_str(shown);
            links.push(cell_hyperlink(symbol).map(str::to_string));
            x += u16::try_from(shown.width()).unwrap_or(1).max(1);
        }
        (text.trim_end().to_string(), links)
    }

    #[test]
    fn linked_cell_shows_its_text_and_targets_the_url() {
        let mut buf = row("ab", 4);
        link(&mut buf, 0..1);
        let symbol = buf[(0, 0)].symbol();
        assert_eq!(visible_symbol(symbol), "a");
        assert_eq!(cell_hyperlink(symbol), Some(URL));
        assert_eq!(buf[(0, 0)].cell_width(), 1);
        assert_eq!(visible_symbol(buf[(1, 0)].symbol()), "b");
        assert_eq!(cell_hyperlink(buf[(1, 0)].symbol()), None);
    }

    #[test]
    fn url_cannot_inject_escape_sequences() {
        let mut buf = row("a", 1);
        set_cell_hyperlink(&mut buf[(0, 0)], "https://x/\u{7}\u{1b}[2J é", "i\u{1b}d");
        let symbol = buf[(0, 0)].symbol();
        assert_eq!(cell_hyperlink(symbol), Some("https://x/%07%1B[2J%20%C3%A9"));
        // One OSC 8 opener, one closer, and no other escape or BEL.
        assert_eq!(symbol.matches('\u{1b}').count(), 2, "{symbol:?}");
        assert_eq!(symbol.matches('\u{7}').count(), 2, "{symbol:?}");
        assert_eq!(visible_symbol(symbol), "a");
    }

    #[test]
    fn empty_cells_are_not_linked_and_links_do_not_nest() {
        let mut buf = row("ab", 2);
        buf[(1, 0)].set_symbol("");
        link(&mut buf, 0..2);
        assert_eq!(cell_hyperlink(buf[(0, 0)].symbol()), Some(URL));
        assert_eq!(
            buf[(1, 0)].symbol(),
            "",
            "a cell drawing nothing stays as it is"
        );
        let before = buf[(0, 0)].symbol().to_string();
        link(&mut buf, 0..1);
        assert_eq!(buf[(0, 0)].symbol(), before);
    }

    /// The case that kept hyperlinks disabled (#1227): a concealed link
    /// whose markup is revealed on the next frame. The URL inside the cell
    /// symbol used to make the diff skip the cells after each linked one,
    /// so the revealed text came out garbled.
    #[test]
    fn diff_between_renders_shows_every_cell() {
        let mut concealed = row("Quick Install", 40);
        link(&mut concealed, 0..13);
        let full = "[Quick Install](#installation)";
        let mut revealed = row(full, 40);
        link(&mut revealed, 1..14);

        let (text, links) = replay(&[concealed.clone(), revealed.clone()]);
        assert_eq!(text, full);
        assert_eq!(links[0], None);
        assert!(links[1..14].iter().all(|l| l.as_deref() == Some(URL)));
        assert_eq!(links[14], None, "`]` is not part of the link");

        // And back: the link moves left again and the markup disappears.
        let (text, links) = replay(&[concealed.clone(), revealed, concealed]);
        assert_eq!(text, "Quick Install");
        assert!(links[..13].iter().all(|l| l.as_deref() == Some(URL)));
        assert!(links[13..].iter().all(Option::is_none));
    }

    #[test]
    fn link_appearing_or_disappearing_on_unchanged_text_is_redrawn() {
        let plain = row("see docs", 10);
        let mut linked = plain.clone();
        link(&mut linked, 4..8);
        let (_, links) = replay(&[plain.clone(), linked.clone()]);
        assert!(links[4..8].iter().all(|l| l.as_deref() == Some(URL)));
        let (text, links) = replay(&[plain, linked, row("see docs", 10)]);
        assert_eq!(text, "see docs");
        assert!(links.iter().all(Option::is_none));
    }

    #[test]
    fn neighbouring_linked_cells_share_one_opener_and_closer() {
        let mut buf = row("ab cd", 5);
        link(&mut buf, 0..2);
        link(&mut buf, 3..5);
        let updates: Vec<_> = Buffer::empty(buf.area).diff(&buf);
        let mut written = String::new();
        for run in plan_runs(updates) {
            let merged = merge_hyperlinks(&run);
            for (&(_, _, cell), merged) in run.iter().zip(&merged) {
                written.push_str(merged.as_ref().unwrap_or(cell).symbol());
            }
        }
        let open = "\x1b]8;id=id1;https://example.com/a/long/path\x07";
        let close = "\x1b]8;;\x07";
        assert_eq!(
            written,
            // The blank at column 2 is unchanged, so not written: two stretches.
            format!("{open}ab{close}{open}cd{close}"),
            "one link per stretch of neighbouring cells"
        );
    }

    #[test]
    fn wide_linked_glyph_keeps_its_trailing_column_in_the_diff() {
        // A linked emoji replacing ASCII: the column it covers changed, and
        // the diff must still say so, or a terminal that does not clear it
        // keeps showing the old character there.
        let before = row("abcd", 4);
        let mut after = row("\u{2764}\u{FE0F}cd", 4);
        link(&mut after, 0..1);
        let updated: Vec<u16> = before.diff(&after).iter().map(|&(x, _, _)| x).collect();
        assert_eq!(updated, vec![0, 1]);
        // And the backend writes the covered column before the emoji.
        let updates = before.diff(&after);
        let order: Vec<u16> = plan_runs(updates)
            .concat()
            .iter()
            .map(|&(x, _, _)| x)
            .collect();
        assert_eq!(order, vec![1, 0]);
    }
}
