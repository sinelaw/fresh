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
//! byte — to [`WrapMachine`], so the walk agrees with the screen by
//! construction; a parallel wrap implementation is the drift this replaces. And
//! backward walks re-derive the grid from a bounded back-off rather than from
//! the line start, trading sub-row exactness for a cost that does not grow with
//! depth (see [`row_start_before`]).

use crate::model::buffer::Buffer;
use crate::view::ui::split_rendering::base_tokens::build_base_tokens;
use crate::view::wrap_machine::{RowCarry, WrapMachine, WrapRule};

/// Bytes read per row asked for, before slack.
///
/// A row holds at most `available_width` characters and a character is at most
/// four bytes, so this cannot cut a row short. Over-reading a little is far
/// cheaper than a second pass.
const BYTES_PER_ROW_ESTIMATE: usize = 4;

/// Slack bytes added to every read, so a walk for a single row on a narrow pane
/// still has a row's worth of text to wrap.
const READ_SLACK_BYTES: usize = 1024;

/// The carry a row starting at `byte` resumes with.
///
/// A row boundary holds only continuation state and the hanging indent:
/// `chars_in_row` is zero there by definition, and an ANSI escape cannot
/// straddle one.
pub fn carry_at(buffer: &mut Buffer, byte: usize, rule: WrapRule) -> RowCarry {
    let line_start = buffer.line_iterator(byte, 80).current_position();
    if byte <= line_start {
        return RowCarry::default();
    }
    let hanging = matches!(
        rule,
        WrapRule::Word {
            hanging_indent: true,
            ..
        }
    );
    RowCarry {
        line_indent: if hanging {
            leading_indent_width(buffer, line_start)
        } else {
            0
        },
        on_continuation: true,
        ansi_in_escape: false,
        chars_in_row: 0,
    }
}

/// Width of the hanging indent a continuation row resumes at. Read bounded:
/// whitespace past this is content, not indentation.
fn leading_indent_width(buffer: &mut Buffer, line_start: usize) -> usize {
    const MAX_INDENT_BYTES: usize = 256;
    let end = line_start
        .saturating_add(MAX_INDENT_BYTES)
        .min(buffer.len());
    let bytes = buffer.slice_bytes(line_start..end);
    let mut width = 0usize;
    for byte in bytes {
        match byte {
            b' ' => width += 1,
            b'\t' => width += 4,
            _ => break,
        }
    }
    width
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
) -> Vec<usize> {
    if max_rows == 0 || from >= buffer.len() {
        return vec![from];
    }
    let carry = carry_at(buffer, from, rule);
    let width = rule.available_width().max(1);
    let budget = max_rows
        .saturating_mul(width)
        .saturating_mul(BYTES_PER_ROW_ESTIMATE)
        .saturating_add(READ_SLACK_BYTES);
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
        &[],
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
pub fn row_start_after(buffer: &mut Buffer, from: usize, rule: WrapRule, rows: usize) -> usize {
    let starts = row_starts_from(buffer, from, rule, rows.saturating_add(1));
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
) -> Option<usize> {
    if target < from {
        return None;
    }
    let starts = row_starts_from(buffer, from, rule, max_rows.saturating_add(1));
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
/// no wrap index can cover it, so `ViewAnchor::byte` is the first visible row's
/// own start rather than a line start.
///
/// Must agree with the indexability test in `reconcile::place_pane`, or a
/// viewport would be placed in one coordinate and rendered in the other. Keyed
/// on the buffer, not on a frame, so the coordinate cannot change between a
/// scroll and the render of it.
pub fn addresses_rows_by_byte(buffer: &Buffer, line_wrap_enabled: bool) -> bool {
    use crate::view::ui::split_rendering::scrollbar::{
        MAX_WRAP_SCROLLBAR_BYTES, MAX_WRAP_SCROLLBAR_LINES,
    };
    line_wrap_enabled
        && (buffer.is_large_file()
            || buffer.len() > MAX_WRAP_SCROLLBAR_BYTES
            || buffer
                .line_count()
                .is_none_or(|lines| lines > MAX_WRAP_SCROLLBAR_LINES))
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
) -> usize {
    if byte == 0 {
        return byte;
    }
    let width = rule.available_width().max(1);
    // A row holds at most `width` characters but can hold as little as one, so
    // this is a guess that grows until the window holds the rows asked for.
    let mut reach = rows_back
        .saturating_add(2)
        .saturating_mul(width)
        .saturating_add(READ_SLACK_BYTES);

    for _ in 0..MAX_BACKOFF_ATTEMPTS {
        let from = walk_start_before(buffer, byte, reach);
        let starts = row_starts_up_to(buffer, from, byte, rule);
        if let Some(index) = starts.len().checked_sub(rows_back + 1) {
            return starts[index];
        }
        if from == 0 || from >= byte {
            // Nothing further back to reach for.
            return starts.first().copied().unwrap_or(from);
        }
        reach = reach.saturating_mul(4);
    }
    walk_start_before(buffer, byte, reach)
}

/// Each attempt quadruples the window, so this covers four orders of magnitude
/// of rows-per-byte before settling for what it found.
const MAX_BACKOFF_ATTEMPTS: usize = 4;

/// Where a backward walk should begin to reach `reach` bytes above `byte`.
///
/// Prefers a line start, which carries fresh and so gives the canonical grid.
/// A line too long to reach one — the case this module exists for — starts
/// mid-line and takes the shift documented on [`row_start_before`].
fn walk_start_before(buffer: &mut Buffer, byte: usize, reach: usize) -> usize {
    let back = byte.saturating_sub(reach);
    let enclosing_line = buffer.line_iterator(back, 80).current_position();
    if back.saturating_sub(enclosing_line) <= reach {
        enclosing_line
    } else {
        back
    }
}

/// Every row start from `from` through the row containing `to`.
///
/// Chunked so a window of many short rows is covered without asking for them
/// all up front.
fn row_starts_up_to(buffer: &mut Buffer, from: usize, to: usize, rule: WrapRule) -> Vec<usize> {
    const CHUNK_ROWS: usize = 64;
    let mut collected: Vec<usize> = Vec::new();
    let mut at = from;
    loop {
        let chunk = row_starts_from(buffer, at, rule, CHUNK_ROWS);
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

        let whole = row_starts_from(&mut buffer, 0, rule, 400);
        assert!(
            whole.len() > 100,
            "expected a line of many rows, got {}",
            whole.len()
        );

        for start_row in [1usize, 7, 42, 99, whole.len() - 12] {
            let resumed = row_starts_from(&mut buffer, whole[start_row], rule, 10);
            let expected = &whole[start_row..(start_row + 10).min(whole.len())];
            assert_eq!(
                resumed, expected,
                "a walk from row {start_row} (byte {}) diverged from the run",
                whole[start_row]
            );
        }
    }

    /// Walking costs the rows asked for, not the distance from the line start.
    #[test]
    fn a_walk_reads_the_rows_asked_for_wherever_it_starts() {
        let mut buffer = Buffer::from_str_test(&json_line(60_000));
        let rule = word_rule(97);
        let whole = row_starts_from(&mut buffer, 0, rule, 4000);
        let deep = *whole.last().unwrap();
        assert!(
            deep > 300_000,
            "the sample row should be far past MAX_LINE_BYTES, got {deep}"
        );

        let near_top = row_starts_from(&mut buffer, whole[2], rule, 8);
        let far_down = row_starts_from(&mut buffer, deep, rule, 8);
        assert_eq!(near_top.len(), 8);
        assert!(
            !far_down.is_empty() && far_down[0] == deep,
            "a walk starts on the row it was given"
        );
    }

    /// The read boundary is not a row boundary — what the old row counting
    /// could not manage (issue #1806).
    #[test]
    fn a_walk_crosses_the_read_piece_boundary() {
        let mut buffer = Buffer::from_str_test(&json_line(30_000));
        let rule = word_rule(97);
        let whole = row_starts_from(&mut buffer, 0, rule, 3000);
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
        let whole = row_starts_from(&mut buffer, 0, rule, 400);

        assert_eq!(
            rows_between(&mut buffer, whole[3], whole[3], rule, 10),
            Some(0)
        );
        assert_eq!(
            rows_between(&mut buffer, whole[3], whole[9], rule, 10),
            Some(6)
        );
        // A byte inside a row belongs to that row.
        assert_eq!(
            rows_between(&mut buffer, whole[3], whole[9] + 1, rule, 10),
            Some(6)
        );
        // Above the walk's start, and beyond its reach: no bounded answer.
        assert_eq!(
            rows_between(&mut buffer, whole[9], whole[3], rule, 10),
            None
        );
        assert_eq!(
            rows_between(&mut buffer, whole[3], whole[80], rule, 10),
            None
        );
    }
}
