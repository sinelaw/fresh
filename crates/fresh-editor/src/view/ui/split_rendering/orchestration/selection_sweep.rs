//! Per-cell selection lookup with a linear-range sweep + per-line
//! block-rect refresh.
//!
//! Mirrors the shape of `OverlayActiveSet` but for the two selection
//! sources `SelectionContext` carries:
//!
//! * **Linear ranges** (`SelectionContext::ranges`) — sorted by
//!   `start`. The cell loop scans the buffer monotonically, so a
//!   single `range_cursor` advances past ranges whose `end <= bp`;
//!   anything from there with `start <= bp` is a candidate.
//! * **Block (rectangular) selections** (`SelectionContext::block_rects`)
//!   — entries are `(start_line, start_col, end_line, end_col)` with
//!   columns in per-line byte indices. Sorted by `start_line`. We
//!   maintain an active set of rects whose `[start_line, end_line]`
//!   includes the current visible line, refreshed once per row as
//!   `gutter_num` advances; each cell tests its own byte column in the
//!   line against the rect's column span.
//!
//! Net: the cell loop just calls `contains(byte_pos, line_column)`.

use std::ops::Range;

pub(super) struct SelectionActiveSet<'a> {
    ranges: &'a [Range<usize>],
    blocks: &'a [(usize, usize, usize, usize)],

    /// Advances monotonically through `ranges`; never revisits.
    range_cursor: usize,

    /// Indices into `blocks` whose line span covers the current row.
    /// Refreshed by `enter_line`.
    active_block: Vec<usize>,
    /// Next rect (in `blocks`) to consider for admission.
    block_next_idx: usize,
    /// Last gutter line `enter_line` ran for; subsequent calls with
    /// the same line short-circuit.
    block_last_line: Option<usize>,
}

impl<'a> SelectionActiveSet<'a> {
    pub(super) fn new(
        ranges: &'a [Range<usize>],
        blocks: &'a [(usize, usize, usize, usize)],
    ) -> Self {
        Self {
            ranges,
            blocks,
            range_cursor: 0,
            active_block: Vec::new(),
            block_next_idx: 0,
            block_last_line: None,
        }
    }

    /// Refresh the active block-rect set for `gutter_num`. Idempotent
    /// on the same line — the cell loop can call this either per-row
    /// or per-cell; per-row is preferred to skip the inner gate.
    pub(super) fn enter_line(&mut self, gutter_num: usize) {
        if self.block_last_line == Some(gutter_num) {
            return;
        }
        // Drop rects whose `end_line` is now behind us.
        self.active_block
            .retain(|&i| self.blocks[i].2 >= gutter_num);
        // Admit any rects whose `start_line` has caught up.
        while self.block_next_idx < self.blocks.len() {
            let (start_line, _, _, _) = self.blocks[self.block_next_idx];
            if start_line > gutter_num {
                break;
            }
            if self.blocks[self.block_next_idx].2 >= gutter_num {
                self.active_block.push(self.block_next_idx);
            }
            self.block_next_idx += 1;
        }
        self.block_last_line = Some(gutter_num);
    }

    /// Is this cell inside a *linear* selection range (block rects
    /// excluded)?
    ///
    /// The selected-line-break column keys off this rather than
    /// [`contains`](Self::contains): a block selection's rect can cover the
    /// column a newline sits in without the line break itself being part of
    /// the selection — copying the block wouldn't take it.
    ///
    /// Safe to call after [`contains`](Self::contains) for the same cell:
    /// `range_cursor` only ever advances past ranges that ended before `bp`.
    pub(super) fn contains_linear(&mut self, buffer_byte: usize) -> bool {
        self.advance_ranges_to(buffer_byte);
        self.ranges[self.range_cursor..]
            .iter()
            .take_while(|r| r.start <= buffer_byte)
            .any(|r| r.end > buffer_byte)
    }

    /// Drop ranges that ended before `bp` — the cell loop scans the buffer
    /// monotonically, so they can never match again.
    fn advance_ranges_to(&mut self, bp: usize) {
        while self.range_cursor < self.ranges.len() && self.ranges[self.range_cursor].end <= bp {
            self.range_cursor += 1;
        }
    }

    /// Is this cell inside any selection?
    ///
    /// `buffer_byte` is the absolute byte position (used by the
    /// linear-range sweep). `None` for cells with no source byte
    /// (ANSI / virtual cells), which are in no selection of either
    /// kind: such a cell has no column in the file, so `line_column`
    /// is `None` too and the block test below declines it.
    ///
    /// That last part is a deliberate change, not only a narrower
    /// spelling: soft-wrap indent padding, fold placeholders and
    /// plugin-injected inline text used to be swept into a block rect
    /// whose column span happened to reach their *view-row* index. A
    /// rectangle cannot cover a cell that is not in the file — a block
    /// copy takes none of it — so they are outside it now.
    ///
    /// `line_column` is the cell's byte offset **within its logical
    /// source line** — the unit `block_rects` states its column bounds
    /// in (`cursor.position - line_start`). `None` for a cell that maps
    /// to no source byte, which no rectangle can cover.
    ///
    /// It used to be the cell's index into the *view* row, which counts a
    /// tab as its whole expansion: with one leading tab and `tab_size 4`
    /// the rectangle painted four cells where the cursor stood at column
    /// 3, i.e. `tab_size - 1` cells to its left (issue #3148). The cursor
    /// is the one telling the truth about what a block copy takes, and a
    /// column in the line is what both of them now count.
    pub(super) fn contains(
        &mut self,
        buffer_byte: Option<usize>,
        line_column: Option<usize>,
    ) -> bool {
        let linear = match buffer_byte {
            Some(bp) => self.contains_linear(bp),
            None => false,
        };
        let block = line_column.is_some_and(|col| {
            self.active_block.iter().any(|&i| {
                let (_, start_col, _, end_col) = self.blocks[i];
                col >= start_col && col <= end_col
            })
        });
        linear || block
    }
}
