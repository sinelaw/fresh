//! Minimal vt100-shaped adapter over [rio-vt](https://crates.io/crates/rio-vt),
//! used by the terminal-output tests and the startup-measurement binary: feed
//! bytes with `process`, then read the parsed grid back through `screen()`.
//!
//! Only the small slice of the old `vt100` surface the tests relied on is
//! reproduced (`process`, `screen().cell()/.contents()/.cursor_position()/
//! .hide_cursor()`), so call sites are unchanged apart from the type path.

use rio_vt::ansi::CursorShape;
use rio_vt::crosswords::formatter::FormatOptions;
use rio_vt::crosswords::grid::row::Row;
use rio_vt::crosswords::pos::Column;
use rio_vt::crosswords::square::{Square, Wide};
use rio_vt::crosswords::{Crosswords, CrosswordsSize, Mode};
use rio_vt::event::{VoidListener, WindowId};
use rio_vt::performer::handler::Processor;

/// A terminal parser: bytes in, grid state out.
pub struct Parser {
    term: Crosswords<VoidListener>,
    processor: Processor,
}

impl Parser {
    pub fn new(rows: u16, cols: u16, _scrollback: usize) -> Self {
        Self {
            term: Crosswords::new(
                CrosswordsSize::new(cols as usize, rows as usize),
                CursorShape::Block,
                VoidListener,
                WindowId::from(0),
                0,
                0,
            ),
            processor: Processor::default(),
        }
    }

    pub fn process(&mut self, bytes: &[u8]) {
        self.processor.advance(&mut self.term, bytes);
    }

    pub fn screen(&self) -> Screen {
        Screen {
            columns: self.term.columns(),
            cursor: {
                let pos = self.term.cursor().pos;
                (pos.row.0 as u16, pos.col.0 as u16)
            },
            cursor_hidden: !self.term.mode().contains(Mode::SHOW_CURSOR),
            plain: self.term.format(FormatOptions::plain()),
            rows: self.term.visible_rows(),
        }
    }
}

/// A snapshot of the visible grid.
pub struct Screen {
    columns: usize,
    cursor: (u16, u16),
    cursor_hidden: bool,
    plain: String,
    rows: Vec<Row<Square>>,
}

impl Screen {
    /// Visible screen as plain UTF-8 text.
    pub fn contents(&self) -> String {
        self.plain.clone()
    }

    /// The cell at `(row, col)`, or `None` when out of bounds.
    pub fn cell(&self, row: u16, col: u16) -> Option<Cell> {
        let grid_row = self.rows.get(row as usize)?;
        if col as usize >= self.columns {
            return None;
        }
        let square = grid_row[Column(col as usize)];
        Some(Cell {
            ch: square.c(),
            spacer: matches!(square.wide(), Wide::Spacer),
        })
    }

    /// Cursor position as `(row, col)`.
    pub fn cursor_position(&self) -> (u16, u16) {
        self.cursor
    }

    /// Whether the hardware cursor is hidden (`\x1b[?25l`).
    pub fn hide_cursor(&self) -> bool {
        self.cursor_hidden
    }
}

/// A single grid cell.
pub struct Cell {
    ch: char,
    spacer: bool,
}

impl Cell {
    /// Cell text: empty for the continuation half of a wide glyph, a space
    /// for an untouched cell, otherwise the glyph.
    pub fn contents(&self) -> String {
        if self.spacer {
            String::new()
        } else if self.ch == '\0' {
            " ".to_string()
        } else {
            self.ch.to_string()
        }
    }
}
