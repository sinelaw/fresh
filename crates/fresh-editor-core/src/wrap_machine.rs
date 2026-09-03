//! The one wrap rule.
//!
//! Where a visual row ends is decided here and nowhere else. Two drivers read
//! the result:
//!
//! - the renderer wants the token stream with `Break` tokens spliced in —
//!   `view::ui::split_rendering::transforms::apply_wrapping_transform` (in `fresh-editor`);
//! - the row index wants only the boundaries — [`WrapOutput::rows`].
//!
//! Because both read the same run, "the scrollbar disagrees with the renderer"
//! stops being a class of bug that can exist. Before this module the same
//! decision was re-implemented in `apply_wrapping_transform`,
//! `apply_grid_wrapping_transform`, `wrap_str_to_width`,
//! `count_visual_rows_for_text`, `count_visual_rows_for_text_grid`, and
//! `for_each_grid_row_start`, kept in agreement by convention plus a pair of
//! cross-checking tests.
//!
//! [`RowCarry`] is the complete resume state at a row boundary. That
//! completeness is what lets a run start at any row rather than at byte 0 of the
//! logical line — the basis for rendering a viewport deep inside a long line
//! without building everything above it, and for repairing a row index after an
//! edit instead of rebuilding it.

use crate::primitives::ansi::AnsiParser;
use crate::primitives::{ansi, display_width, visual_layout};
use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};
use unicode_segmentation::UnicodeSegmentation;

use visual_layout::WRAP_MAX_LOOKBACK as MAX_LOOKBACK;

/// Minimum content width for continuation lines when hanging indent is active.
const MIN_CONTINUATION_CONTENT_WIDTH: usize = 10;

/// Stand-in for "never wrap", used when the pane is too narrow to wrap in.
const NO_WRAP: usize = usize::MAX / 4;

/// How rows end.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WrapRule {
    /// Soft wrap: word boundaries, hanging indent, gutter on the first row.
    Word {
        content_width: usize,
        gutter_width: usize,
        hanging_indent: bool,
    },
    /// Terminal scroll-back (fresh#2649): exact column boundaries, no gutter, no
    /// indent, ANSI-aware, parser reset per logical line.
    Grid { cols: usize },
    /// Soft wrap off: a safety chop every `chars` characters, bounding memory on
    /// a pathological line. This is the forced `Break` that `build_base_tokens`
    /// used to inject inline; owning it here means wrap-off row structure is
    /// described by the same code path as wrap-on.
    Chop { chars: usize },
}

impl WrapRule {
    pub fn available_width(&self) -> usize {
        match *self {
            WrapRule::Word {
                content_width,
                gutter_width,
                ..
            } => content_width.saturating_sub(gutter_width),
            WrapRule::Grid { cols } => cols,
            WrapRule::Chop { chars } => chars,
        }
    }

    /// Below this width the transform emits no breaks at all — a one-column pane
    /// would otherwise produce one `Break` per character.
    pub fn is_degenerate(&self) -> bool {
        matches!(self, WrapRule::Word { .. }) && self.available_width() < 2
    }

    fn hanging_indent(&self) -> bool {
        matches!(
            self,
            WrapRule::Word {
                hanging_indent: true,
                ..
            }
        )
    }
}

/// Everything that crosses a row boundary.
///
/// Completeness argument for [`WrapRule::Word`]: tab widths restart from the
/// row's own starting column, the word-boundary lookback never reaches behind
/// the row start, and the back-up-to-prior-space path is bounded by the current
/// row — so `line_indent` and `on_continuation` are the only state a resumed run
/// needs. `Grid` additionally carries the ANSI parser state, since a split
/// escape sequence would otherwise become visible. `Chop` carries its character
/// counter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct RowCarry {
    pub line_indent: usize,
    pub on_continuation: bool,
    pub ansi_in_escape: bool,
    pub chars_in_row: usize,
}

/// One visual row produced by a run.
#[derive(Debug, Clone, Copy)]
pub struct RowInfo {
    /// First source-bearing byte on the row — exactly what a row index stores as
    /// a row start. `None` for a row made entirely of injected content (a
    /// hanging indent with nothing after it, a soft break's newline and indent).
    pub source_byte: Option<usize>,
    /// State to resume this row with.
    pub carry: RowCarry,
    /// Index of the row's first token in [`WrapOutput::tokens`].
    pub token_start: usize,
    /// Index one past the row's last token.
    pub token_end: usize,
}

/// Result of one run.
#[derive(Debug, Default)]
pub struct WrapOutput {
    pub tokens: Vec<ViewTokenWire>,
    pub rows: Vec<RowInfo>,
}

/// Tokens of the row currently being built.
///
/// Held rather than emitted immediately because the Space-overflow path (issue
/// #1363) retroactively moves the row's trailing word onto the next row.
/// Bounded by one row's worth of tokens.
#[derive(Default)]
struct RowAccum {
    tokens: Vec<ViewTokenWire>,
    source_byte: Option<usize>,
}

impl RowAccum {
    fn push(&mut self, token: ViewTokenWire) {
        if self.source_byte.is_none() {
            self.source_byte = token.source_offset;
        }
        self.tokens.push(token);
    }
}

/// Decides row boundaries for a token stream under one [`WrapRule`].
pub struct WrapMachine {
    rule: WrapRule,
    line_indent: usize,
    on_continuation: bool,
    measuring_indent: bool,
    chars_in_row: usize,
    col: usize,
    ansi: AnsiParser,
    out: Vec<ViewTokenWire>,
    rows: Vec<RowInfo>,
    row: RowAccum,
    row_token_start: usize,
    row_carry: RowCarry,
}

impl WrapMachine {
    pub fn new(rule: WrapRule) -> Self {
        Self::resume(rule, RowCarry::default())
    }

    /// Start a run mid-line, from a previous row boundary's carry.
    pub fn resume(rule: WrapRule, carry: RowCarry) -> Self {
        let mut ansi = AnsiParser::new();
        ansi.set_in_escape(carry.ansi_in_escape);
        let mut machine = Self {
            rule,
            line_indent: carry.line_indent,
            on_continuation: carry.on_continuation,
            // A resumed run starts mid-line, so the indent has already been
            // measured; only a run starting at a logical line start measures.
            measuring_indent: rule.hanging_indent() && !carry.on_continuation,
            chars_in_row: carry.chars_in_row,
            col: if carry.on_continuation {
                carry.line_indent
            } else {
                0
            },
            ansi,
            out: Vec::new(),
            rows: Vec::new(),
            row: RowAccum::default(),
            row_token_start: 0,
            row_carry: carry,
        };
        if carry.on_continuation && carry.line_indent > 0 {
            // A continuation row opens with its hanging indent. The carry knows
            // the width, so a resumed run reconstructs it rather than needing it
            // fed back in — which is what keeps a resume addressable by a plain
            // source byte.
            machine.row.push(indent_token(carry.line_indent));
        }
        machine
    }

    /// Wrap a whole token stream in one call.
    pub fn run(tokens: Vec<ViewTokenWire>, rule: WrapRule) -> WrapOutput {
        Self::run_from(tokens, rule, RowCarry::default())
    }

    /// Wrap a token stream, resuming from `carry`.
    pub fn run_from(tokens: Vec<ViewTokenWire>, rule: WrapRule, carry: RowCarry) -> WrapOutput {
        let mut machine = Self::resume(rule, carry);
        for token in tokens {
            machine.feed(token);
        }
        machine.finish()
    }

    /// Rows sealed so far. Lets a caller stop feeding once it has enough.
    pub fn rows_so_far(&self) -> &[RowInfo] {
        &self.rows
    }

    /// Output tokens of the rows sealed so far.
    pub fn tokens_so_far(&self) -> &[ViewTokenWire] {
        &self.out
    }

    /// State to resume the row currently being built.
    pub fn carry(&self) -> RowCarry {
        RowCarry {
            line_indent: self.line_indent,
            on_continuation: self.on_continuation,
            ansi_in_escape: self.ansi.in_escape(),
            chars_in_row: self.chars_in_row,
        }
    }

    pub fn feed(&mut self, token: ViewTokenWire) {
        match self.rule {
            WrapRule::Grid { .. } => self.feed_grid(token),
            WrapRule::Chop { .. } => self.feed_chop(token),
            WrapRule::Word { .. } => self.feed_word(token),
        }
    }

    pub fn finish(mut self) -> WrapOutput {
        self.close_row(true);
        WrapOutput {
            tokens: self.out,
            rows: self.rows,
        }
    }

    // -- row bookkeeping -----------------------------------------------------

    fn close_row(&mut self, final_row: bool) {
        // A stream ending exactly at a break has already had its last row
        // recorded; don't invent an empty one.
        if final_row && self.row.tokens.is_empty() && !self.rows.is_empty() {
            return;
        }
        self.out.append(&mut self.row.tokens);
        self.rows.push(RowInfo {
            source_byte: self.row.source_byte,
            carry: self.row_carry,
            token_start: self.row_token_start,
            token_end: self.out.len(),
        });
        self.row = RowAccum::default();
        self.row_token_start = self.out.len();
        self.row_carry = self.carry();
    }

    /// End the row with a `Break`, then re-emit the hanging indent.
    fn emit_break(&mut self, with_indent: bool) {
        self.row.push(break_token());
        self.on_continuation = true;
        self.chars_in_row = 0;
        self.close_row(false);
        self.col = 0;
        if with_indent && self.line_indent > 0 {
            self.row.push(indent_token(self.line_indent));
            self.col = self.line_indent;
        }
    }

    // -- Word rule -----------------------------------------------------------

    fn feed_word(&mut self, token: ViewTokenWire) {
        let eff = if self.rule.is_degenerate() {
            NO_WRAP
        } else {
            self.rule.available_width()
        };

        match &token.kind {
            ViewTokenWireKind::Newline => {
                self.row.push(token);
                self.close_row(false);
                self.col = 0;
                self.line_indent = 0;
                self.measuring_indent = self.rule.hanging_indent();
                self.on_continuation = false;
                self.row_carry = self.carry();
            }
            ViewTokenWireKind::Break => self.emit_break(true),
            ViewTokenWireKind::Space => {
                if self.measuring_indent {
                    self.line_indent += 1;
                    if self.line_indent + MIN_CONTINUATION_CONTENT_WIDTH > eff {
                        self.line_indent = 0;
                    }
                }
                if self.col + 1 > eff {
                    self.space_overflow(eff);
                }
                self.row.push(token);
                self.col += 1;
            }
            ViewTokenWireKind::BinaryByte(_) => {
                self.measuring_indent = false;
                // `col > line_indent` — the row has content. Without this guard a
                // `<XX>` escape (4 columns) on a pane narrower than 4 breaks at
                // column 0 and strands an empty leading row. The grid path has
                // always guarded with `col > 0`; this makes the word path agree.
                if self.col > self.line_indent && self.col + 4 > eff {
                    self.emit_break(true);
                }
                self.row.push(token);
                self.col += 4;
            }
            ViewTokenWireKind::Text(_) => self.feed_word_text(token, eff),
        }
    }

    fn feed_word_text(&mut self, token: ViewTokenWire, eff: usize) {
        let text = match &token.kind {
            ViewTokenWireKind::Text(s) => s.clone(),
            _ => return,
        };
        if self.measuring_indent {
            self.measure_indent(&text, eff);
        }

        let text_w = visual_layout::visual_width(&text, self.col);

        // Break before a token that overflows, when either it fits on a fresh
        // row (classic word wrap) or the row already carries enough content that
        // ending here beats pushing one straggler grapheme to reach `eff`.
        //
        // `col > line_indent` rather than `col > 0`: on a continuation row
        // holding only its hanging indent, breaking would emit an identical
        // empty row and make no progress.
        let fresh_capacity = eff.saturating_sub(self.line_indent);
        let row_floor = eff.saturating_sub(MAX_LOOKBACK).max(eff / 2);
        if self.col > self.line_indent
            && self.col + text_w > eff
            && (text_w <= fresh_capacity || self.col >= row_floor)
        {
            self.emit_break(true);
        }

        let text_w = visual_layout::visual_width(&text, self.col);
        if self.col + text_w > eff && !ansi::contains_ansi_codes(&text) {
            self.split_text(&token, &text, eff);
        } else {
            self.row.push(token);
            self.col += text_w;
        }
    }

    /// Accumulate the logical line's leading whitespace into `line_indent`.
    fn measure_indent(&mut self, text: &str, eff: usize) {
        let mut ws_chars = 0usize;
        let mut ws_width = 0usize;
        for c in text.chars() {
            if c == ' ' {
                ws_width += 1;
                ws_chars += 1;
            } else if c == '\t' {
                let col = self.line_indent + ws_width;
                ws_width += visual_layout::tab_expansion_width(col);
                ws_chars += 1;
            } else {
                break;
            }
        }
        self.line_indent += ws_width;
        if ws_chars != text.chars().count() {
            self.measuring_indent = false;
        }
        // Dropped to zero when it would leave a continuation row unusably
        // narrow — the clamp that also guarantees the split loop makes progress.
        if self.line_indent + MIN_CONTINUATION_CONTENT_WIDTH > eff {
            self.line_indent = 0;
        }
    }

    /// Grapheme-split a token too wide for the current row.
    ///
    /// Each chunk prefers to end at a UAX #29 word boundary within the lookback
    /// window, falling back to the hard column cap so progress is guaranteed.
    fn split_text(&mut self, token: &ViewTokenWire, text: &str, eff: usize) {
        let graphemes: Vec<(usize, &str)> = text.grapheme_indices(true).collect();
        let word_bounds: Vec<usize> = text.split_word_bound_indices().map(|(b, _)| b).collect();
        let mut wb_lo = 0usize;
        let mut idx = 0usize;

        while idx < graphemes.len() {
            let remaining = eff.saturating_sub(self.col);
            if remaining == 0 {
                self.emit_break(true);
                continue;
            }

            let row_has_content = self.col > self.line_indent;
            let mut chunk_w = 0usize;
            let mut chunk_n = 0usize;
            let mut col = self.col;
            for &(_, g) in &graphemes[idx..] {
                let gw = grapheme_width(g, col);
                // `chunk_n > 0 || row_has_content`: the pre-existing guard was
                // `chunk_n > 0` alone, which let a chunk's first cluster through
                // even when it did not fit — a double-width glyph with one
                // column left overflowed the row and the renderer clipped it.
                if chunk_w + gw > remaining && (chunk_n > 0 || row_has_content) {
                    break;
                }
                chunk_w += gw;
                chunk_n += 1;
                col += gw;
            }

            if chunk_n == 0 {
                // Nothing fits in what is left of the row. End the row; force a
                // cluster only when the row is empty, i.e. the cluster is wider
                // than an entire row and has nowhere else to go.
                if row_has_content {
                    self.emit_break(true);
                    continue;
                }
                chunk_n = 1;
                chunk_w = grapheme_width(graphemes[idx].1, self.col);
            }

            let mut force_break = false;
            if chunk_n > 1 {
                if let Some(shrunk) = self.prefer_word_boundary(
                    &graphemes,
                    &word_bounds,
                    &mut wb_lo,
                    idx,
                    chunk_n,
                    eff,
                    text,
                ) {
                    chunk_n = shrunk;
                    let mut col = self.col;
                    chunk_w = 0;
                    for &(_, g) in &graphemes[idx..idx + chunk_n] {
                        let w = grapheme_width(g, col);
                        chunk_w += w;
                        col += w;
                    }
                    force_break = true;
                }
            }

            let start_byte = graphemes[idx].0;
            let end_byte = if idx + chunk_n < graphemes.len() {
                graphemes[idx + chunk_n].0
            } else {
                text.len()
            };
            self.row.push(ViewTokenWire {
                source_offset: token.source_offset.map(|b| b + start_byte),
                kind: ViewTokenWireKind::Text(text[start_byte..end_byte].to_string()),
                style: token.style.clone(),
            });
            self.col += chunk_w;
            idx += chunk_n;

            // Break only when the boundary preference demands it. Breaking
            // eagerly on `col >= eff` here — as the pre-existing code did — is
            // not what the non-split path does, so a row filled exactly by a
            // split ended immediately while one filled exactly by a whole token
            // did not, and a trailing newline landed on a different row in the
            // two cases. That asymmetry also breaks resume.
            if force_break {
                self.emit_break(true);
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn prefer_word_boundary(
        &self,
        graphemes: &[(usize, &str)],
        word_bounds: &[usize],
        wb_lo: &mut usize,
        idx: usize,
        chunk_n: usize,
        eff: usize,
        text: &str,
    ) -> Option<usize> {
        let slice_start = graphemes[idx].0;
        let slice_end_hard = if idx + chunk_n < graphemes.len() {
            graphemes[idx + chunk_n].0
        } else {
            text.len()
        };
        let row_floor = eff.saturating_sub(MAX_LOOKBACK).max(eff / 2);
        let floor_from_cursor = row_floor.saturating_sub(self.col);
        let floor_byte = if floor_from_cursor < chunk_n {
            graphemes[idx + floor_from_cursor].0
        } else {
            slice_end_hard
        };

        // Walk the precomputed boundary list as a monotonic cursor: amortised
        // O(1) per chunk, so a single very long token stays O(n) overall.
        while *wb_lo < word_bounds.len() && word_bounds[*wb_lo] <= slice_start {
            *wb_lo += 1;
        }
        let mut wb_hi = *wb_lo;
        while wb_hi < word_bounds.len() && word_bounds[wb_hi] <= slice_end_hard {
            wb_hi += 1;
        }
        let mut best = word_bounds[*wb_lo..wb_hi]
            .iter()
            .rev()
            .copied()
            .find(|&b| b >= floor_byte);
        // The text end counts as a virtual boundary, so a chunk ending exactly
        // there isn't shrunk to an earlier one (which would leak characters onto
        // the next row).
        let end_byte = text.len();
        if end_byte > slice_start && end_byte >= floor_byte && end_byte <= slice_end_hard {
            best = Some(match best {
                Some(b) => b.max(end_byte),
                None => end_byte,
            });
        }

        let target = best?;
        let new_count = graphemes[idx..].iter().position(|(b, _)| *b == target)?;
        (new_count > 0 && new_count < chunk_n).then_some(new_count)
    }

    /// Issue #1363: back up over the trailing word instead of stranding a space.
    fn space_overflow(&mut self, eff: usize) {
        let Some((tail_start, tail_width)) = self.back_up_plan(eff) else {
            self.emit_break(true);
            return;
        };
        let tail: Vec<ViewTokenWire> = self.row.tokens.split_off(tail_start);
        // The cached source byte may have belonged to the tail.
        self.row.source_byte = self.row.tokens.iter().find_map(|t| t.source_offset);
        self.emit_break(true);
        for token in tail {
            self.row.push(token);
        }
        self.col += tail_width;
    }

    fn back_up_plan(&self, eff: usize) -> Option<(usize, usize)> {
        let toks = &self.row.tokens;
        let mut space_idx = None;
        for (i, t) in toks.iter().enumerate().rev() {
            match t.kind {
                ViewTokenWireKind::Break | ViewTokenWireKind::Newline => return None,
                ViewTokenWireKind::Space => {
                    space_idx = Some(i);
                    break;
                }
                _ => continue,
            }
        }
        let space_idx = space_idx?;
        let tail_start = space_idx + 1;
        if tail_start >= toks.len() {
            return None;
        }
        // Backing up must not leave a row consisting solely of a Space.
        if !toks[..space_idx]
            .iter()
            .any(|t| !matches!(t.kind, ViewTokenWireKind::Space))
        {
            return None;
        }
        let mut col = self.line_indent;
        for t in &toks[tail_start..] {
            match &t.kind {
                ViewTokenWireKind::Text(s) => col += visual_layout::visual_width(s, col),
                ViewTokenWireKind::Space => col += 1,
                ViewTokenWireKind::BinaryByte(_) => col += 4,
                _ => return None,
            }
        }
        let tail_width = col.saturating_sub(self.line_indent);
        (self.line_indent + tail_width <= eff).then_some((tail_start, tail_width))
    }

    // -- Grid rule -----------------------------------------------------------

    fn feed_grid(&mut self, token: ViewTokenWire) {
        let cols = self.rule.available_width();
        if cols == 0 {
            self.row.push(token);
            return;
        }
        match &token.kind {
            ViewTokenWireKind::Newline => {
                self.row.push(token);
                self.close_row(false);
                self.col = 0;
                // Escapes never span logical lines in captured scroll-back.
                self.ansi.reset();
                self.row_carry = self.carry();
            }
            ViewTokenWireKind::Break => {
                self.row.push(token);
                self.close_row(false);
                self.col = 0;
            }
            ViewTokenWireKind::Space => {
                if self.col > 0 && self.col + 1 > cols {
                    self.emit_break(false);
                }
                self.row.push(token);
                self.col += 1;
            }
            ViewTokenWireKind::BinaryByte(_) => {
                if self.col > 0 && self.col + 4 > cols {
                    self.emit_break(false);
                }
                self.row.push(token);
                self.col += 4;
            }
            ViewTokenWireKind::Text(_) => self.feed_grid_text(token, cols),
        }
    }

    fn feed_grid_text(&mut self, token: ViewTokenWire, cols: usize) {
        let text = match &token.kind {
            ViewTokenWireKind::Text(s) => s.clone(),
            _ => return,
        };
        let mut seg_start = 0usize;
        for (byte_offset, grapheme) in text.grapheme_indices(true) {
            let mut chars = grapheme.chars();
            let first = chars.next().unwrap_or('\0');
            if self.ansi.parse_char(first).is_none() {
                for ch in chars {
                    let _ = self.ansi.parse_char(ch);
                }
                continue;
            }
            let mut width = grapheme_width(grapheme, self.col);
            if self.col > 0 && self.col + width > cols {
                if byte_offset > seg_start {
                    self.row.push(ViewTokenWire {
                        source_offset: token.source_offset.map(|s| s + seg_start),
                        kind: ViewTokenWireKind::Text(text[seg_start..byte_offset].to_string()),
                        style: token.style.clone(),
                    });
                    seg_start = byte_offset;
                }
                self.emit_break(false);
                // A tab's width depends on the column it starts at, so it must
                // be re-measured once the break moves it to column 0. Measuring
                // before the break and applying after it — as the pre-existing
                // grid path did — has the wrap and the renderer disagreeing
                // about that tab's width.
                width = grapheme_width(grapheme, self.col);
            }
            self.col += width;
        }
        if seg_start == 0 {
            self.row.push(token);
        } else if seg_start < text.len() {
            self.row.push(ViewTokenWire {
                source_offset: token.source_offset.map(|s| s + seg_start),
                kind: ViewTokenWireKind::Text(text[seg_start..].to_string()),
                style: token.style,
            });
        }
    }

    // -- Chop rule -----------------------------------------------------------

    fn feed_chop(&mut self, token: ViewTokenWire) {
        let limit = self.rule.available_width();
        match &token.kind {
            ViewTokenWireKind::Newline => {
                self.row.push(token);
                self.close_row(false);
                self.chars_in_row = 0;
                self.row_carry = self.carry();
            }
            ViewTokenWireKind::Break => {
                self.row.push(token);
                self.close_row(false);
                self.chars_in_row = 0;
            }
            ViewTokenWireKind::Text(_) => self.feed_chop_text(token, limit),
            _ => {
                if self.chars_in_row >= limit {
                    self.emit_break(false);
                }
                self.row.push(token);
                self.chars_in_row += 1;
            }
        }
    }

    fn feed_chop_text(&mut self, token: ViewTokenWire, limit: usize) {
        let text = match &token.kind {
            ViewTokenWireKind::Text(s) => s.clone(),
            _ => return,
        };
        let mut buf = String::new();
        let mut buf_start = 0usize;
        let mut emitted = 0usize;
        for ch in text.chars() {
            if self.chars_in_row >= limit {
                if !buf.is_empty() {
                    self.row.push(ViewTokenWire {
                        source_offset: token.source_offset.map(|s| s + buf_start),
                        kind: ViewTokenWireKind::Text(std::mem::take(&mut buf)),
                        style: token.style.clone(),
                    });
                }
                self.emit_break(false);
                buf_start = emitted;
            }
            buf.push(ch);
            emitted += ch.len_utf8();
            self.chars_in_row += 1;
        }
        if !buf.is_empty() {
            self.row.push(ViewTokenWire {
                source_offset: token.source_offset.map(|s| s + buf_start),
                kind: ViewTokenWireKind::Text(buf),
                style: token.style,
            });
        }
    }
}

fn break_token() -> ViewTokenWire {
    ViewTokenWire {
        source_offset: None,
        kind: ViewTokenWireKind::Break,
        style: None,
    }
}

fn indent_token(width: usize) -> ViewTokenWire {
    ViewTokenWire {
        source_offset: None,
        kind: ViewTokenWireKind::Text(" ".repeat(width)),
        style: None,
    }
}

fn grapheme_width(grapheme: &str, col: usize) -> usize {
    if grapheme == "\t" {
        visual_layout::tab_expansion_width(col)
    } else {
        display_width::str_width(grapheme)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn text(s: &str, offset: usize) -> ViewTokenWire {
        ViewTokenWire {
            source_offset: Some(offset),
            kind: ViewTokenWireKind::Text(s.to_string()),
            style: None,
        }
    }

    fn space(offset: usize) -> ViewTokenWire {
        ViewTokenWire {
            source_offset: Some(offset),
            kind: ViewTokenWireKind::Space,
            style: None,
        }
    }

    /// Tokenize `s` the way `build_base_tokens` does: words coalesced, spaces
    /// separate. The `Space` tokens matter — the back-up path only fires on them.
    fn tokenize(s: &str) -> Vec<ViewTokenWire> {
        let mut out = Vec::new();
        let mut word = String::new();
        let mut word_start = 0usize;
        for (i, ch) in s.char_indices() {
            if ch == ' ' {
                if !word.is_empty() {
                    out.push(text(&std::mem::take(&mut word), word_start));
                }
                out.push(space(i));
            } else {
                if word.is_empty() {
                    word_start = i;
                }
                word.push(ch);
            }
        }
        if !word.is_empty() {
            out.push(text(&word, word_start));
        }
        out
    }

    /// Sub-stream starting at `byte`, splitting a `Text` token if needed —
    /// what a resumed run is fed.
    fn tokens_from(tokens: &[ViewTokenWire], byte: usize) -> Vec<ViewTokenWire> {
        let mut out = Vec::new();
        let mut started = false;
        for t in tokens {
            if started {
                out.push(t.clone());
                continue;
            }
            let Some(off) = t.source_offset else { continue };
            if off >= byte {
                started = true;
                out.push(t.clone());
                continue;
            }
            if let ViewTokenWireKind::Text(s) = &t.kind {
                if off + s.len() > byte {
                    started = true;
                    out.push(text(&s[byte - off..], byte));
                }
            }
        }
        out
    }

    fn word_rule(width: usize) -> WrapRule {
        WrapRule::Word {
            content_width: width,
            gutter_width: 0,
            hanging_indent: false,
        }
    }

    /// The property the whole architecture rests on: a run resumed at any row
    /// boundary, with that row's carry, reproduces the rest of the original run.
    ///
    /// Resume is defined over the *source* stream, not over wrapped output —
    /// wrapping is not idempotent, and both callers that resume (a renderer
    /// starting at a viewport anchor, a row index repairing after an edit) slice
    /// the source stream.
    #[test]
    fn resume_at_any_boundary_reproduces_the_run() {
        let inputs = [
            "alpha beta gamma delta epsilon zeta eta theta",
            "supercalifragilisticexpialidocious and more words after it",
            "日本語のテキストです and some latin too",
            "a b c d e f g h i j k l m n o p q r s t u v",
            "        indented text that wraps across several rows here",
        ];
        for input in inputs {
            for width in [6usize, 11, 17, 24] {
                let rule = word_rule(width);
                let source = tokenize(input);
                let full = WrapMachine::run(source.clone(), rule);
                for (i, row) in full.rows.iter().enumerate() {
                    let Some(byte) = row.source_byte else {
                        continue;
                    };
                    let resumed =
                        WrapMachine::run_from(tokens_from(&source, byte), rule, row.carry);
                    let got: Vec<Option<usize>> =
                        resumed.rows.iter().map(|r| r.source_byte).collect();
                    let want: Vec<Option<usize>> =
                        full.rows[i..].iter().map(|r| r.source_byte).collect();
                    assert_eq!(
                        got, want,
                        "resume at row {i} (byte {byte}) diverged for {input:?} at width {width}"
                    );
                }
            }
        }
    }

    /// Rows never exceed the rule's width. The pre-existing char-split let a
    /// chunk's first cluster through regardless of the space left, so a
    /// double-width glyph with one column remaining overflowed and was clipped.
    #[test]
    fn double_width_glyphs_do_not_overflow_the_row() {
        for width in [4usize, 7, 13, 20] {
            let out = WrapMachine::run(tokenize(&"日本語テキスト".repeat(3)), word_rule(width));
            let mut col = 0usize;
            for token in &out.tokens {
                match &token.kind {
                    ViewTokenWireKind::Break => col = 0,
                    ViewTokenWireKind::Text(s) => {
                        col += visual_layout::visual_width(s, col);
                        assert!(
                            col <= width.max(2),
                            "row reached {col} columns at width {width}"
                        );
                    }
                    ViewTokenWireKind::Space => col += 1,
                    _ => {}
                }
            }
        }
    }

    /// A `<XX>` escape on a pane narrower than its 4 columns must not break at
    /// column 0 and strand an empty leading row.
    #[test]
    fn control_byte_on_a_narrow_pane_does_not_emit_an_empty_row() {
        let tokens = vec![ViewTokenWire {
            source_offset: Some(0),
            kind: ViewTokenWireKind::BinaryByte(0x07),
            style: None,
        }];
        let out = WrapMachine::run(tokens, word_rule(2));
        assert_eq!(out.rows.len(), 1, "expected one row, got {:?}", out.rows);
        assert!(!matches!(out.tokens[0].kind, ViewTokenWireKind::Break));
    }

    /// Issue #1363: a continuation row starts with content, not a stranded space.
    #[test]
    fn space_overflow_backs_up_over_the_word() {
        let out = WrapMachine::run(tokenize("aaaa bbbb cccc"), word_rule(12));
        let after_break = out
            .tokens
            .iter()
            .skip_while(|t| !matches!(t.kind, ViewTokenWireKind::Break))
            .nth(1);
        assert!(
            !matches!(after_break.map(|t| &t.kind), Some(ViewTokenWireKind::Space)),
            "continuation row starts with a stranded space: {:?}",
            out.tokens
        );
    }

    /// A word wider than the pane is split rather than looping forever, and no
    /// character is lost.
    #[test]
    fn unbreakable_word_is_split_without_loss() {
        let out = WrapMachine::run(tokenize(&"x".repeat(50)), word_rule(10));
        let joined: String = out
            .tokens
            .iter()
            .filter_map(|t| match &t.kind {
                ViewTokenWireKind::Text(s) => Some(s.as_str()),
                _ => None,
            })
            .collect();
        assert_eq!(joined, "x".repeat(50));
        assert!(out.rows.len() >= 5);
    }

    /// The grid rule breaks at exact columns, mid-word.
    #[test]
    fn grid_breaks_at_exact_columns() {
        let out = WrapMachine::run(tokenize("abcdefghij"), WrapRule::Grid { cols: 4 });
        let chunks: Vec<&str> = out
            .tokens
            .iter()
            .filter_map(|t| match &t.kind {
                ViewTokenWireKind::Text(s) => Some(s.as_str()),
                _ => None,
            })
            .collect();
        assert_eq!(chunks, vec!["abcd", "efgh", "ij"]);
    }

    /// Wrap-off rows are the `MAX_SAFE_LINE_WIDTH` chop, expressed as a rule.
    #[test]
    fn chop_breaks_every_n_characters() {
        let out = WrapMachine::run(tokenize(&"y".repeat(25)), WrapRule::Chop { chars: 10 });
        let lens: Vec<usize> = out
            .tokens
            .iter()
            .filter_map(|t| match &t.kind {
                ViewTokenWireKind::Text(s) => Some(s.chars().count()),
                _ => None,
            })
            .collect();
        assert_eq!(lens, vec![10, 10, 5]);
    }

    /// A pane too narrow to wrap in emits no breaks at all.
    #[test]
    fn degenerate_width_emits_no_breaks() {
        let rule = word_rule(1);
        assert!(rule.is_degenerate());
        let out = WrapMachine::run(tokenize("hello world"), rule);
        assert!(!out
            .tokens
            .iter()
            .any(|t| matches!(t.kind, ViewTokenWireKind::Break)));
    }

    /// Every row records the first source byte drawn on it — what a row index
    /// stores as a row start — and those advance monotonically.
    #[test]
    fn row_source_bytes_are_monotonic() {
        let out = WrapMachine::run(
            tokenize("alpha beta gamma delta epsilon zeta eta theta iota"),
            word_rule(12),
        );
        let seen: Vec<usize> = out.rows.iter().filter_map(|r| r.source_byte).collect();
        let mut sorted = seen.clone();
        sorted.sort_unstable();
        assert_eq!(seen, sorted);
    }
}
