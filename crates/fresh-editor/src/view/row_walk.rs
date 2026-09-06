//! Visual rows of a buffer the wrap index cannot cover, walked from a byte.
//!
//! Such a buffer used to be placed by *logical line start plus a row count into
//! that line*, which on a file that is one enormous line puts the whole scroll
//! position in the count — and the count is re-derived from a `LineIterator`
//! read that stops after `MAX_LINE_BYTES`, so past 100 KB it saturated and the
//! viewport stopped following the cursor (issue #1806). Walking from a byte
//! costs the rows asked for wherever it starts.
//!
//! Two decisions hold the rest of the module together. Row boundaries come from
//! feeding [`build_base_tokens`] — the renderer's own stream, read from the same
//! byte, with the same collapsed-fold ranges — to [`WrapMachine`], so the walk
//! agrees with the screen by construction; a parallel wrap implementation is the
//! drift this replaces. And backward walks re-derive the grid from a bounded
//! back-off rather than from the line start, trading sub-row exactness for a
//! cost that does not grow with depth (see [`row_start_before`]).
//!
//! Every entry point therefore takes the `folds` the frame is drawing, in the
//! shape `build_base_tokens` wants: pass what `fold_skip_set` produced, not a
//! subset and not `&[]`. A walk given no folds counts rows inside collapsed
//! regions that the screen does not draw, which lands the viewport top inside a
//! fold.
//!
//! What the walk still cannot see is the plugin transform between the token
//! build and the wrap — soft breaks, conceals and virtual lines. Those inject
//! rows with no source byte of their own, so a walk cannot place them. The
//! row-counted path models them as a count (`count_visual_rows_for_line`'s
//! `extra_virtual_rows`); doing the same here would mean reproducing the
//! transform, and gating the anchored path on them instead would put the
//! viewport and the renderer in different coordinates, which is worse. They are
//! rare on the buffers this module serves — soft breaks are Compose-mode only —
//! and [`Viewport::top_visual_row_source_byte`] already declines to answer where
//! they intersect its span.

use crate::model::buffer::Buffer;
use crate::view::ui::split_rendering::base_tokens::build_base_tokens;
use crate::view::wrap_machine::{RowCarry, WrapMachine, WrapRule};
use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};

/// Characters allowed per column of a requested row, before slack.
///
/// `build_base_tokens` takes a *character* budget, and a row is bounded in
/// *columns*, so this is the assumed ratio between them — a first guess, not a
/// bound. Zero-width characters have no ratio: a base with four combining marks
/// is five characters in one column. A walk that comes up short for that reason
/// is retried on a larger budget rather than reported as the end of the buffer,
/// which is what [`row_starts_from`] loops for.
const CHARS_PER_COLUMN_ESTIMATE: usize = 4;

/// How far back a line start is looked for before giving up.
///
/// `Buffer::line_iterator` finds one by scanning backwards to the previous
/// newline with no bound at all, which on the files this module exists for is a
/// scan of the whole file per call — the very cost the module removes. Past
/// this, a row is treated as a continuation with no hanging indent: a line this
/// long has its indent clamped away by `MIN_CONTINUATION_CONTENT_WIDTH` on any
/// ordinary pane, and paying a megabyte-scale scan a keystroke to discover
/// otherwise is not a trade worth making.
const LINE_START_SEARCH_BYTES: usize = 64 * 1024;

/// Start of the line containing `byte`, if one is within
/// [`LINE_START_SEARCH_BYTES`].
///
/// `Some(0)` when the search reaches the start of the buffer, so "no newline
/// anywhere above" is distinguished from "gave up looking".
fn bounded_line_start(buffer: &mut Buffer, byte: usize) -> Option<usize> {
    const CHUNK: usize = 4096;
    let mut end = byte.min(buffer.len());
    if end == 0 {
        // The start of the buffer is a line start; there is nothing to search.
        return Some(0);
    }
    let floor = end.saturating_sub(LINE_START_SEARCH_BYTES);
    while end > floor {
        let start = end.saturating_sub(CHUNK).max(floor);
        let chunk = buffer.slice_bytes(start..end);
        if let Some(i) = chunk.iter().rposition(|b| *b == b'\n') {
            return Some(start + i + 1);
        }
        if start == 0 {
            return Some(0);
        }
        end = start;
    }
    None
}

/// The carry a row starting at `byte` resumes with.
///
/// A row boundary holds only continuation state and the hanging indent:
/// `chars_in_row` is zero there by definition, and an ANSI escape cannot
/// straddle one.
///
/// The indent comes from [`WrapMachine`] itself rather than being measured
/// here. It is a row-*width* input — `feed_word_text` sizes a row as
/// `available_width - line_indent` — so a second implementation of
/// `measure_indent` would have to reproduce its tab-stop arithmetic and its
/// `MIN_CONTINUATION_CONTENT_WIDTH` clamp exactly or rows walked mid-line would
/// come out a different width from rows walked from the line's start. Feeding
/// the machine the line's opening text and taking its carry cannot drift.
pub fn carry_at(buffer: &mut Buffer, byte: usize, rule: WrapRule) -> RowCarry {
    // Whether this row continues a line is settled by the byte before it, so it
    // costs one byte and is never in doubt. Deriving it from a *search* for the
    // line start instead made a search that gave up indistinguishable from a
    // real continuation — and byte 0, where there is nothing to search, came
    // back as one.
    if byte == 0 || !continues_a_line(buffer, byte) {
        return RowCarry::default();
    }
    // The indent is a refinement on top, and the only part that needs the line's
    // start — so it is also the only part allowed to give up: an unknown indent
    // draws a continuation row flush, which is cosmetic, where an unknown
    // continuation flag would be structural.
    //
    // Ask the rule before searching. A rule with no hanging indent has no use
    // for the answer, and the search is the expensive half of this function.
    let hanging = matches!(
        rule,
        WrapRule::Word {
            hanging_indent: true,
            ..
        }
    );
    RowCarry {
        line_indent: if hanging {
            bounded_line_start(buffer, byte).map_or(0, |line_start| {
                measured_line_indent(buffer, line_start, rule)
            })
        } else {
            0
        },
        on_continuation: true,
        ansi_in_escape: false,
        chars_in_row: 0,
    }
}

/// Whether `byte` sits inside a logical line rather than at its start.
fn continues_a_line(buffer: &mut Buffer, byte: usize) -> bool {
    buffer.slice_bytes(byte.saturating_sub(1)..byte).first() != Some(&b'\n')
}

/// The `line_indent` [`WrapMachine`] arrives at for the line opening at
/// `line_start`, obtained by running it over that opening.
///
/// Read bounded: whitespace past this is content, not indentation, and the
/// machine stops measuring at the first non-space anyway.
fn measured_line_indent(buffer: &mut Buffer, line_start: usize, rule: WrapRule) -> usize {
    if !matches!(
        rule,
        WrapRule::Word {
            hanging_indent: true,
            ..
        }
    ) {
        return 0;
    }
    // Indentation is spaces and tabs, one byte each and at least one column
    // each, so a row's width of bytes settles it: anything longer is an indent
    // the machine clamps to zero for leaving no room to continue in.
    let end = line_start
        .saturating_add(rule.available_width().saturating_add(1))
        .min(buffer.len());
    let opening = String::from_utf8_lossy(&buffer.slice_bytes(line_start..end)).into_owned();
    let mut machine = WrapMachine::resume(rule, RowCarry::default());
    machine.feed(ViewTokenWire {
        source_offset: Some(line_start),
        kind: ViewTokenWireKind::Text(opening),
        style: None,
    });
    machine.carry().line_indent
}

/// Byte at which each of the next rows starts, beginning with `from` itself.
///
/// `from` must be a row start; what comes back is then the rows a render
/// anchored there draws.
pub fn row_starts_from(
    buffer: &mut Buffer,
    from: usize,
    rule: WrapRule,
    max_rows: usize,
    folds: &[std::ops::Range<usize>],
) -> Vec<usize> {
    if max_rows == 0 || from >= buffer.len() {
        return vec![from];
    }
    let width = rule.available_width().max(1);
    let mut budget = max_rows
        .saturating_mul(width)
        .saturating_mul(CHARS_PER_COLUMN_ESTIMATE);

    // Grow the budget while growing it still buys rows. A walk short of the rows
    // asked for, with buffer left beyond what the budget could have read, was cut
    // by the budget rather than by the buffer — and a caller reads a short walk
    // as "the file ends here" and stops scrolling, so the two have to be told
    // apart.
    //
    // No attempt limit is needed: a round must strictly add rows to continue and
    // rows are capped at `max_rows`, so it ends in at most that many rounds, and
    // in practice the first. A round that buys nothing means something other than
    // the budget is stopping the walk — where a fixed count would keep going,
    // quadrupling toward the size of the file.
    let mut starts = walk_rows(buffer, from, rule, max_rows, budget, folds);
    while starts.len() < max_rows && from.saturating_add(budget) < buffer.len() {
        let Some(larger) = budget.checked_mul(4) else {
            break;
        };
        let grown = walk_rows(buffer, from, rule, max_rows, larger, folds);
        if grown.len() <= starts.len() {
            break;
        }
        budget = larger;
        starts = grown;
    }
    starts
}

/// One pass of [`row_starts_from`], reading at most `budget` bytes.
fn walk_rows(
    buffer: &mut Buffer,
    from: usize,
    rule: WrapRule,
    max_rows: usize,
    budget: usize,
    folds: &[std::ops::Range<usize>],
) -> Vec<usize> {
    let carry = carry_at(buffer, from, rule);
    let is_binary = buffer.is_binary();
    let line_ending = buffer.line_ending();
    let estimated = buffer.estimated_line_length().max(1);
    let tokens = build_base_tokens(
        buffer,
        from,
        estimated,
        max_rows.saturating_add(4),
        is_binary,
        line_ending,
        folds,
        Some(budget),
        // Read from `from` itself rather than its line start: that walk back is
        // the cost this module removes.
        true,
    );
    let output = WrapMachine::run_from(tokens, rule, carry);

    let mut starts = Vec::with_capacity(max_rows);
    starts.push(from);
    for row in output.rows.iter().skip(1) {
        if starts.len() >= max_rows {
            break;
        }
        // Rows of purely injected content own no byte, so a viewport anchored
        // on one could not describe itself: step over them.
        if let Some(byte) = row.source_byte {
            if byte > *starts.last().unwrap_or(&from) {
                starts.push(byte);
            }
        }
    }
    starts
}

/// The row start `rows` rows after `from`, or the last row of the buffer.
pub fn row_start_after(
    buffer: &mut Buffer,
    from: usize,
    rule: WrapRule,
    rows: usize,
    folds: &[std::ops::Range<usize>],
) -> usize {
    let starts = row_starts_from(buffer, from, rule, rows.saturating_add(1), folds);
    starts.last().copied().unwrap_or(from)
}

/// How many rows down from `from` the byte `target` is drawn.
///
/// `None` when `target` is above `from` or past `max_rows`: the caller's cue
/// that no bounded answer exists, so the viewport must move rather than check.
pub fn rows_between(
    buffer: &mut Buffer,
    from: usize,
    target: usize,
    rule: WrapRule,
    max_rows: usize,
    folds: &[std::ops::Range<usize>],
) -> Option<usize> {
    if target < from {
        return None;
    }
    let starts = row_starts_from(buffer, from, rule, max_rows.saturating_add(1), folds);
    let mut found = None;
    for (row, start) in starts.iter().enumerate() {
        if *start <= target {
            found = Some(row);
        } else {
            break;
        }
    }
    // The last row of a truncated walk only *looks* like the answer: a target
    // past its end is not known to be on it.
    match found {
        Some(row) if row + 1 < starts.len() => Some(row),
        Some(row) if starts.len() <= max_rows => Some(row),
        _ => None,
    }
}

/// Whether this buffer's visual rows are addressed by byte — true exactly when
/// no wrap index can exist, so `ViewAnchor::byte` is the first visible row's own
/// start rather than a line start.
///
/// This is the negation of `reconcile::place_pane`'s `indexable`, deliberately
/// stated in the same one term: a large file has no line data, so nothing can
/// build or cache an index for it, and `place_pane` can never take its
/// `has_index` path. Anything else keeps the row-numbered path, *including* a
/// buffer past the wrap scrollbar's ceilings — `place_pane` still treats one as
/// indexable when an index is already cached for the geometry, and `WrapIndices`
/// only evicts by LRU, so a buffer that crosses a ceiling keeps its index and
/// keeps being placed by row. Widening this to the ceilings would put those
/// buffers in two coordinates at once: written mid-line here, read back through
/// `line_first_row` there.
///
/// Keyed on the buffer, not on a frame, so the coordinate cannot change between
/// a scroll and the render of it.
pub fn addresses_rows_by_byte(buffer: &Buffer, line_wrap_enabled: bool) -> bool {
    line_wrap_enabled && buffer.is_large_file()
}

/// A row start `rows_back` rows above `byte`.
///
/// Backwards has no resume state to start from, so this backs off a bounded
/// distance and wraps forward. Sound rather than exact, for two reasons: the
/// back-off never starts before the logical line, so line tops give the
/// canonical grid; and a frame draws the rows a walk from *its own* anchor
/// produces, so a re-derived grid is never measured against an older one. The
/// price is that a back-off landing mid-row shifts the grid by less than a row
/// — invisible on screen, self-consistent after. Exactness here would want a
/// row-start trail kept by the viewport or the index.
pub fn row_start_before(
    buffer: &mut Buffer,
    byte: usize,
    rows_back: usize,
    rule: WrapRule,
    folds: &[std::ops::Range<usize>],
) -> usize {
    if byte == 0 {
        return byte;
    }
    let width = rule.available_width().max(1);
    // A row holds at most `width` characters but can hold as little as one, so
    // this is a guess that grows until the window holds the rows asked for.
    let mut reach = rows_back.saturating_add(2).saturating_mul(width);

    // Reach further while reaching further still finds more rows. Bounded without
    // a count: the reach quadruples, so it arrives at the start of the buffer in
    // a logarithmic number of rounds, and a round that finds no more rows than
    // the last ends it.
    let mut best_rows = 0usize;
    loop {
        let from = walk_start_before(buffer, byte, reach);
        let starts = row_starts_up_to(buffer, from, byte, rule, folds);
        if let Some(index) = starts.len().checked_sub(rows_back + 1) {
            return starts[index];
        }
        if from == 0 || from >= byte || starts.len() <= best_rows {
            // The start of the buffer, or a reach that bought nothing.
            return starts.first().copied().unwrap_or(from);
        }
        best_rows = starts.len();
        let Some(further) = reach.checked_mul(4) else {
            return starts.first().copied().unwrap_or(from);
        };
        reach = further;
    }
}

/// Where a backward walk should begin to reach `reach` bytes above `byte`.
///
/// Prefers a line start, which carries fresh and so gives the canonical grid.
/// A line too long to reach one — the case this module exists for — starts
/// mid-line and takes the shift documented on [`row_start_before`].
fn walk_start_before(buffer: &mut Buffer, byte: usize, reach: usize) -> usize {
    let back = byte.saturating_sub(reach);
    match bounded_line_start(buffer, back) {
        Some(line_start) if back.saturating_sub(line_start) <= reach => line_start,
        _ => back,
    }
}

/// Every row start from `from` through the row containing `to`.
///
/// Chunked so a window of many short rows is covered without asking for them
/// all up front.
fn row_starts_up_to(
    buffer: &mut Buffer,
    from: usize,
    to: usize,
    rule: WrapRule,
    folds: &[std::ops::Range<usize>],
) -> Vec<usize> {
    const CHUNK_ROWS: usize = 64;
    let mut collected: Vec<usize> = Vec::new();
    let mut at = from;
    loop {
        let chunk = row_starts_from(buffer, at, rule, CHUNK_ROWS, folds);
        let mut advanced = false;
        for start in &chunk {
            if *start > to {
                return collected;
            }
            if collected.last() != Some(start) {
                collected.push(*start);
                advanced = true;
            }
        }
        match chunk.last() {
            Some(last) if advanced && *last > at => at = *last,
            _ => return collected,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Most tests here are about the walk itself, with nothing folded away.
    const NO_FOLDS: &[std::ops::Range<usize>] = &[];

    fn word_rule(width: usize) -> WrapRule {
        WrapRule::Word {
            content_width: width,
            gutter_width: 0,
            hanging_indent: false,
        }
    }

    /// The minified-JSON shape these buffers usually hold.
    fn json_line(values: usize) -> String {
        let mut s = String::from("[");
        for i in 0..values {
            if i > 0 {
                s.push(',');
            }
            s.push_str(&i.to_string());
        }
        s.push(']');
        s
    }

    /// The property the anchored viewport rests on. Without it an anchor would
    /// describe rows the renderer does not draw.
    #[test]
    fn a_walk_from_any_row_start_reproduces_the_run() {
        let mut buffer = Buffer::from_str_test(&json_line(8_000));
        let rule = word_rule(97);

        let whole = row_starts_from(&mut buffer, 0, rule, 400, NO_FOLDS);
        assert!(
            whole.len() > 100,
            "expected a line of many rows, got {}",
            whole.len()
        );

        for start_row in [1usize, 7, 42, 99, whole.len() - 12] {
            let resumed = row_starts_from(&mut buffer, whole[start_row], rule, 10, NO_FOLDS);
            let expected = &whole[start_row..(start_row + 10).min(whole.len())];
            assert_eq!(
                resumed, expected,
                "a walk from row {start_row} (byte {}) diverged from the run",
                whole[start_row]
            );
        }
    }

    /// A walk deep inside a line returns the same rows the full run has there,
    /// and all of the rows it was asked for.
    ///
    /// This does *not* assert the cost property the module is for — an
    /// in-memory test buffer has nothing to read lazily, so there is no work to
    /// observe. `large_file_open_bounded`'s `Buffer::resident_bytes` assertions
    /// are what hold that; this holds the answer being right at depth, which is
    /// what a bounded walk has to get right to be worth anything.
    #[test]
    fn a_deep_walk_agrees_with_the_full_run() {
        let mut buffer = Buffer::from_str_test(&json_line(60_000));
        let rule = word_rule(97);
        let whole = row_starts_from(&mut buffer, 0, rule, 4000, NO_FOLDS);
        let deep_row = whole.len() - 20;
        let deep = whole[deep_row];
        assert!(
            deep > 300_000,
            "the sample row should be far past MAX_LINE_BYTES, got {deep}"
        );

        let near_top = row_starts_from(&mut buffer, whole[2], rule, 8, NO_FOLDS);
        assert_eq!(near_top, &whole[2..10], "a walk near the top diverged");

        let far_down = row_starts_from(&mut buffer, deep, rule, 8, NO_FOLDS);
        assert_eq!(
            far_down,
            &whole[deep_row..deep_row + 8],
            "a walk 300 KB into the line diverged from the run"
        );
    }

    /// The retry recovers rows a first budget could not reach.
    ///
    /// The budget assumes [`CHARS_PER_COLUMN_ESTIMATE`] characters per column,
    /// which zero-width marks break: a base plus five combining marks is six
    /// characters in one column. A walk short for that reason must retry, not
    /// report the end of the buffer — otherwise a page-down over such text
    /// silently moves a fraction of a page.
    #[test]
    fn a_walk_over_zero_width_text_still_returns_the_rows_asked_for() {
        // Six characters, one column: well past the four the budget assumes.
        let dense = "a\u{0301}\u{0302}\u{0303}\u{0304}\u{0305}".repeat(20_000);
        let mut buffer = Buffer::from_str_test(&dense);
        let rule = word_rule(73);

        let starts = row_starts_from(&mut buffer, 0, rule, 40, NO_FOLDS);
        assert_eq!(
            starts.len(),
            40,
            "a budget-bound walk was reported as the end of the buffer"
        );
        for pair in starts.windows(2) {
            assert!(pair[1] > pair[0], "row starts must ascend, got {pair:?}");
        }
    }

    /// The line-start lookup gives up rather than scanning to byte 0.
    ///
    /// `carry_at` and `walk_start_before` both need the enclosing line's start.
    /// `Buffer::line_iterator` finds one by scanning back to the previous
    /// newline with no bound, so on a file that is one line it scans from the
    /// walk's position to byte 0 — per call, on every scroll. That is the cost
    /// this module exists to remove, so the bound is asserted directly rather
    /// than inferred from a clock.
    #[test]
    fn the_line_start_lookup_is_bounded() {
        let mut buffer = Buffer::from_str_test(&json_line(400_000));
        assert!(
            buffer.len() > 2_000_000,
            "expected a multi-megabyte line, got {}",
            buffer.len()
        );

        // Byte 0 is a line start, not a failed search — getting this wrong makes
        // the first row of every buffer resume as a continuation row.
        assert_eq!(bounded_line_start(&mut buffer, 0), Some(0));

        // Within reach of the line's start: found exactly.
        assert_eq!(bounded_line_start(&mut buffer, 4_096), Some(0));

        // Megabytes in, with no newline anywhere above: give up instead of
        // walking back to 0.
        assert_eq!(bounded_line_start(&mut buffer, 2_000_000), None);

        // And a real line start just inside the window is still found.
        let mut lines =
            Buffer::from_str_test(&format!("{}\n{}", "x".repeat(1_000), json_line(200_000)));
        assert_eq!(bounded_line_start(&mut lines, 1_500), Some(1_001));
    }

    /// Whether a row continues a line is decided by the byte before it, so it is
    /// exact everywhere — including where the line-start lookup gives up, and at
    /// byte 0 where there is nothing to look for.
    ///
    /// Deriving it from the lookup instead made a search that gave up
    /// indistinguishable from a real continuation, and byte 0 came back as one:
    /// the first row of every buffer drawn as a continuation, which is a blank
    /// gutter and no fold marker.
    #[test]
    fn continuation_is_decided_by_the_preceding_byte() {
        let rule = word_rule(97);

        // Byte 0: the start of the buffer starts a line.
        let mut lines = Buffer::from_str_test("alpha\nbeta\ngamma");
        assert!(!carry_at(&mut lines, 0, rule).on_continuation);

        // Just past a newline: also a line start.
        assert!(!carry_at(&mut lines, 6, rule).on_continuation);

        // Inside a line: a continuation.
        assert!(carry_at(&mut lines, 8, rule).on_continuation);

        // Megabytes into one line, where the line-start lookup gives up: still
        // exactly a continuation, because the byte before it says so.
        let mut huge = Buffer::from_str_test(&json_line(400_000));
        assert_eq!(bounded_line_start(&mut huge, 2_000_000), None);
        let carry = carry_at(&mut huge, 2_000_000, rule);
        assert!(carry.on_continuation);
        assert_eq!(carry.chars_in_row, 0);
    }

    /// A walk steps over a collapsed fold, because the frame does.
    ///
    /// Given no folds the walk counts rows inside the collapsed region, so a
    /// scroll of N rows lands the viewport top *inside* a fold — a top the
    /// renderer never draws. Given the frame's folds, the same walk reaches the
    /// far side.
    #[test]
    fn a_walk_steps_over_a_collapsed_fold() {
        let mut content = String::from("header\n");
        let fold_start = content.len();
        for i in 0..500 {
            content.push_str(&format!("hidden body line {i}\n"));
        }
        let fold_end = content.len();
        for i in 0..50 {
            content.push_str(&format!("after {i}\n"));
        }
        let mut buffer = Buffer::from_str_test(&content);
        let rule = word_rule(97);
        let folds = [fold_start..fold_end];

        // Three rows down from the top, with the body collapsed, is past it.
        let folded = row_start_after(&mut buffer, 0, rule, 3, &folds);
        assert!(
            folded >= fold_end,
            "a walk of 3 rows over a collapsed 500-line fold landed at byte \
             {folded}, inside the fold ({fold_start}..{fold_end}) — the frame \
             draws no such row"
        );

        // The control: without the folds it walks into the hidden body, which
        // is exactly the bug this argument prevents.
        let unfolded = row_start_after(&mut buffer, 0, rule, 3, NO_FOLDS);
        assert!(
            unfolded < fold_end,
            "expected the unfolded walk to stay inside the body, got {unfolded}"
        );
    }

    /// A row-counted top converts to the byte of the row it names.
    ///
    /// A viewport can reach the anchored path still holding a line start plus a
    /// count of rows into that line. Zeroing the count and keeping the line
    /// start would scroll the view back up by that many rows without the reader
    /// asking; walking the count forward is the same position in the other
    /// coordinate.
    #[test]
    fn a_row_count_into_a_line_converts_to_that_row_s_byte() {
        let mut buffer = Buffer::from_str_test(&json_line(8_000));
        let rule = word_rule(97);
        let rows = row_starts_from(&mut buffer, 0, rule, 40, NO_FOLDS);

        // "line start, 7 rows down" names the same row as its byte does.
        assert_eq!(row_start_after(&mut buffer, 0, rule, 7, NO_FOLDS), rows[7]);
        assert_eq!(row_start_after(&mut buffer, 0, rule, 0, NO_FOLDS), rows[0]);
    }

    /// The read boundary is not a row boundary — what the old row counting
    /// could not manage (issue #1806).
    #[test]
    fn a_walk_crosses_the_read_piece_boundary() {
        let mut buffer = Buffer::from_str_test(&json_line(30_000));
        let rule = word_rule(97);
        let whole = row_starts_from(&mut buffer, 0, rule, 3000, NO_FOLDS);
        assert!(
            whole
                .iter()
                .any(|b| *b > crate::primitives::line_iterator::MAX_LINE_BYTES),
            "the walk stopped at the read piece boundary: last row {:?}",
            whole.last()
        );
        for pair in whole.windows(2) {
            assert!(pair[1] > pair[0], "row starts must ascend, got {pair:?}");
        }
    }

    #[test]
    fn rows_between_answers_within_its_bound_and_declines_past_it() {
        let mut buffer = Buffer::from_str_test(&json_line(8_000));
        let rule = word_rule(97);
        let whole = row_starts_from(&mut buffer, 0, rule, 400, NO_FOLDS);

        assert_eq!(
            rows_between(&mut buffer, whole[3], whole[3], rule, 10, NO_FOLDS),
            Some(0)
        );
        assert_eq!(
            rows_between(&mut buffer, whole[3], whole[9], rule, 10, NO_FOLDS),
            Some(6)
        );
        // A byte inside a row belongs to that row.
        assert_eq!(
            rows_between(&mut buffer, whole[3], whole[9] + 1, rule, 10, NO_FOLDS),
            Some(6)
        );
        // Above the walk's start, and beyond its reach: no bounded answer.
        assert_eq!(
            rows_between(&mut buffer, whole[9], whole[3], rule, 10, NO_FOLDS),
            None
        );
        assert_eq!(
            rows_between(&mut buffer, whole[3], whole[80], rule, 10, NO_FOLDS),
            None
        );
    }
}
