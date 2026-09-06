//! Build base tokens (`ViewTokenWire`) from a buffer for the view pipeline.
//!
//! These helpers are self-contained: they take a `&mut Buffer`, a byte range,
//! and a few typed parameters, and produce a flat list of tokens. No shared
//! render-time "mega struct" is required.

use super::MAX_SAFE_LINE_WIDTH;
use crate::model::buffer::{Buffer, LineEnding};
use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};

/// Build tokens from a text buffer starting at `top_byte`, stopping roughly
/// after `visible_count` visual lines. Honors CRLF / LF line endings and
/// renders unsafe control characters as `BinaryByte` tokens.
///
/// `char_budget` is a second, independent stop condition: tokenising ends once
/// this many characters have been emitted, whatever `lines_seen` says. The
/// line budget alone is not enough on a file whose logical lines are far wider
/// than the screen — `lines_seen` advances once per line the reader yields, and
/// it yields a long line in [`MAX_LINE_BYTES`] pieces, so a 50-row viewport
/// would ask for 54 "lines" and tokenise megabytes to fill rows that hold a few
/// thousand characters. Callers that know the width a row wraps at pass roughly
/// `rows × width`; pass `None` to bound by source lines only.
///
/// `start_mid_line` means `top_byte` is a *visual row* start rather than a
/// logical line start — the caller obtained it from the wrap index — so the read
/// begins there instead of scanning back. That is the difference between reading
/// a viewport and reading everything above it.
#[allow(clippy::too_many_arguments)]
pub(crate) fn build_base_tokens(
    buffer: &mut Buffer,
    top_byte: usize,
    estimated_line_length: usize,
    visible_count: usize,
    is_binary: bool,
    line_ending: LineEnding,
    fold_skip: &[std::ops::Range<usize>],
    char_budget: Option<usize>,
    start_mid_line: bool,
) -> Vec<ViewTokenWire> {
    let mut tokens = Vec::new();

    // For binary files, read raw bytes directly to preserve byte values
    // (LineIterator uses String::from_utf8_lossy which loses high bytes)
    if is_binary {
        return build_base_tokens_binary(buffer, top_byte, estimated_line_length, visible_count);
    }

    let max_lines = visible_count.saturating_add(4);
    // Never stop before a single row's worth of text exists, however small the
    // caller's estimate turns out to be.
    let char_budget = char_budget.map(|b| b.max(MAX_SAFE_LINE_WIDTH.min(1024)));
    let mut chars_seen = 0usize;
    let mut lines_seen = 0usize;
    let buffer_len = buffer.len();
    // Don't clamp `cursor` to buffer_len: `LineIterator::new` clamps
    // internally and uses a backward scan to locate the line containing
    // `top_byte`, so a `top_byte >= buffer_len` (post-scroll past EOF on a
    // single very long line) still produces tokens for that final line.
    let mut cursor = top_byte;
    let mut fold_idx = 0usize;
    // Fast-forward past folds already ending at/before the cursor.
    while fold_idx < fold_skip.len() && fold_skip[fold_idx].end <= cursor {
        fold_idx += 1;
    }
    // If the cursor landed inside a fold, jump past it before reading anything.
    if let Some(r) = fold_skip.get(fold_idx) {
        if r.start <= cursor && cursor < r.end {
            cursor = r.end;
            fold_idx += 1;
        }
    }

    // Outer loop: one iteration per visible segment between folds. A fresh
    // `LineIterator` is constructed per segment so source bytes covered by
    // a collapsed fold are never read, never decoded, and never tokenised.
    'segments: loop {
        if lines_seen >= max_lines {
            break;
        }
        let next_fold_start = fold_skip.get(fold_idx).map(|r| r.start);
        let segment_end = next_fold_start.unwrap_or(buffer_len);
        // Zero-length segment between adjacent folds (or fold starting
        // exactly at cursor): jump past the fold and try again. Only fires
        // when there's actually a fold ahead — without one, segment_end
        // is `buffer_len`, but `cursor >= buffer_len` is fine: `LineIterator`
        // handles the past-EOF case via internal clamping.
        if next_fold_start.is_some() && cursor >= segment_end {
            let r = &fold_skip[fold_idx];
            cursor = r.end;
            fold_idx += 1;
            continue;
        }

        let mut iter = if start_mid_line && cursor == top_byte {
            buffer.line_iterator_from_mid_line(cursor, estimated_line_length)
        } else {
            buffer.line_iterator(cursor, estimated_line_length)
        };
        if let Some(budget) = char_budget {
            // Bytes, not characters — UTF-8 runs to 4 bytes per character — so
            // the first piece the iterator yields always covers the whole
            // budget and the loop below never asks for a second one. Without
            // this the iterator reads and UTF-8-decodes 100 KB of a long line
            // to hand back text we stop consuming after a few thousand chars.
            iter = iter.with_max_line_bytes(budget.saturating_mul(4).saturating_add(1024));
        }
        while lines_seen < max_lines {
            let Some((line_start, line_content)) = iter.next_line() else {
                break 'segments;
            };
            // Stop the inner loop when the next line crosses into the
            // upcoming fold. Without a fold ahead, `next_fold_start` is
            // `None` and we keep tokenising until the iterator reports EOF
            // — preserving the trailing-empty-line behaviour at buffer end.
            if next_fold_start.is_some_and(|s| line_start >= s) {
                break;
            }
            let mut byte_offset = 0usize;
            let content_bytes = line_content.as_bytes();
            let mut skip_next_lf = false; // Track if we should skip \n after \r in CRLF
            for ch in line_content.chars() {
                // Stop once the viewport's rows are covered. Unlike the
                // `lines_seen` bound this fires inside a single long line,
                // which is the only place it can fire at all.
                if char_budget.is_some_and(|budget| chars_seen >= budget) {
                    break 'segments;
                }
                chars_seen += 1;

                let ch_len = ch.len_utf8();
                let source_offset = Some(line_start + byte_offset);

                match ch {
                    '\r' => {
                        // In CRLF mode with \r\n: emit Newline at \r position, skip the \n.
                        // In LF/Unix files, ANY \r is unusual and should be shown as <0D>.
                        let is_crlf_file = line_ending == LineEnding::CRLF;
                        let next_byte = content_bytes.get(byte_offset + 1);
                        if is_crlf_file && next_byte == Some(&b'\n') {
                            tokens.push(ViewTokenWire {
                                source_offset,
                                kind: ViewTokenWireKind::Newline,
                                style: None,
                            });
                            skip_next_lf = true;
                            byte_offset += ch_len;
                            continue;
                        }
                        tokens.push(ViewTokenWire {
                            source_offset,
                            kind: ViewTokenWireKind::BinaryByte(ch as u8),
                            style: None,
                        });
                    }
                    '\n' if skip_next_lf => {
                        skip_next_lf = false;
                        byte_offset += ch_len;
                        continue;
                    }
                    '\n' => {
                        tokens.push(ViewTokenWire {
                            source_offset,
                            kind: ViewTokenWireKind::Newline,
                            style: None,
                        });
                    }
                    ' ' => {
                        tokens.push(ViewTokenWire {
                            source_offset,
                            kind: ViewTokenWireKind::Space,
                            style: None,
                        });
                    }
                    '\t' => {
                        tokens.push(ViewTokenWire {
                            source_offset,
                            kind: ViewTokenWireKind::Text(ch.to_string()),
                            style: None,
                        });
                    }
                    _ if is_control_char(ch) => {
                        tokens.push(ViewTokenWire {
                            source_offset,
                            kind: ViewTokenWireKind::BinaryByte(ch as u8),
                            style: None,
                        });
                    }
                    _ => {
                        if let Some(last) = tokens.last_mut() {
                            if let ViewTokenWireKind::Text(ref mut s) = last.kind {
                                let expected_offset = last.source_offset.map(|o| o + s.len());
                                if expected_offset == Some(line_start + byte_offset) {
                                    s.push(ch);
                                    byte_offset += ch_len;
                                    continue;
                                }
                            }
                        }
                        tokens.push(ViewTokenWire {
                            source_offset,
                            kind: ViewTokenWireKind::Text(ch.to_string()),
                            style: None,
                        });
                    }
                }
                byte_offset += ch_len;
            }
            lines_seen += 1;
        }

        if lines_seen >= max_lines {
            break;
        }
        // Jump past the fold at fold_idx (which drove segment_end). If we
        // ran out of folds, we've finished the last segment.
        if let Some(r) = fold_skip.get(fold_idx) {
            cursor = r.end;
            fold_idx += 1;
        } else {
            break;
        }
    }

    if tokens.is_empty() {
        tokens.push(ViewTokenWire {
            source_offset: Some(top_byte),
            kind: ViewTokenWireKind::Text(String::new()),
            style: None,
        });
    }

    tokens
}

/// Build tokens for binary files by reading raw bytes directly.
/// This preserves byte values >= 0x80 that would be lost by `String::from_utf8_lossy`.
pub(super) fn build_base_tokens_binary(
    buffer: &mut Buffer,
    top_byte: usize,
    estimated_line_length: usize,
    visible_count: usize,
) -> Vec<ViewTokenWire> {
    let mut tokens = Vec::new();
    let max_lines = visible_count.saturating_add(4);
    let buffer_len = buffer.len();

    if top_byte >= buffer_len {
        tokens.push(ViewTokenWire {
            source_offset: Some(top_byte),
            kind: ViewTokenWireKind::Text(String::new()),
            style: None,
        });
        return tokens;
    }

    let estimated_bytes = estimated_line_length * max_lines * 2;
    let bytes_to_read = estimated_bytes.min(buffer_len - top_byte);

    let raw_bytes = buffer.slice_bytes(top_byte..top_byte + bytes_to_read);

    let mut byte_offset = 0usize;
    let mut lines_seen = 0usize;
    let mut current_text = String::new();
    let mut current_text_start: Option<usize> = None;

    let flush_text =
        |tokens: &mut Vec<ViewTokenWire>, text: &mut String, start: &mut Option<usize>| {
            if !text.is_empty() {
                tokens.push(ViewTokenWire {
                    source_offset: *start,
                    kind: ViewTokenWireKind::Text(std::mem::take(text)),
                    style: None,
                });
                *start = None;
            }
        };

    while byte_offset < raw_bytes.len() && lines_seen < max_lines {
        let b = raw_bytes[byte_offset];
        let source_offset = top_byte + byte_offset;

        match b {
            b'\n' => {
                flush_text(&mut tokens, &mut current_text, &mut current_text_start);
                tokens.push(ViewTokenWire {
                    source_offset: Some(source_offset),
                    kind: ViewTokenWireKind::Newline,
                    style: None,
                });
                lines_seen += 1;
            }
            b' ' => {
                flush_text(&mut tokens, &mut current_text, &mut current_text_start);
                tokens.push(ViewTokenWire {
                    source_offset: Some(source_offset),
                    kind: ViewTokenWireKind::Space,
                    style: None,
                });
            }
            _ => {
                if is_binary_unprintable(b) {
                    flush_text(&mut tokens, &mut current_text, &mut current_text_start);
                    tokens.push(ViewTokenWire {
                        source_offset: Some(source_offset),
                        kind: ViewTokenWireKind::BinaryByte(b),
                        style: None,
                    });
                } else {
                    if current_text_start.is_none() {
                        current_text_start = Some(source_offset);
                    }
                    current_text.push(b as char);
                }
            }
        }
        byte_offset += 1;
    }

    flush_text(&mut tokens, &mut current_text, &mut current_text_start);

    if tokens.is_empty() {
        tokens.push(ViewTokenWire {
            source_offset: Some(top_byte),
            kind: ViewTokenWireKind::Text(String::new()),
            style: None,
        });
    }

    tokens
}

/// Check if a byte should be displayed as `<XX>` in binary mode.
/// Returns true for:
/// - Control characters (0x00-0x1F) except tab and newline
/// - DEL (0x7F)
/// - High bytes (0x80-0xFF) which are not valid single-byte UTF-8
fn is_binary_unprintable(b: u8) -> bool {
    if b == 0x09 || b == 0x0A {
        return false;
    }
    if b < 0x20 {
        return true;
    }
    if b == 0x7F {
        return true;
    }
    if b >= 0x80 {
        return true;
    }
    false
}

/// Check if a character is a control character that should be rendered as `<XX>`.
/// This applies to ALL files (binary and non-binary) to prevent terminal corruption.
fn is_control_char(ch: char) -> bool {
    let code = ch as u32;
    if code >= 128 {
        return false;
    }
    let b = code as u8;
    // Allow: tab (0x09), newline (0x0A), ESC (0x1B - for ANSI sequences)
    if b == 0x09 || b == 0x0A || b == 0x1B {
        return false;
    }
    b < 0x20 || b == 0x7F
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::filesystem::{FileSystem, StdFileSystem};
    use std::sync::Arc;

    fn test_fs() -> Arc<dyn FileSystem + Send + Sync> {
        Arc::new(StdFileSystem)
    }

    /// Number of source characters the token stream covers.
    fn char_count(tokens: &[ViewTokenWire]) -> usize {
        tokens
            .iter()
            .map(|t| match &t.kind {
                ViewTokenWireKind::Text(s) => s.chars().count(),
                ViewTokenWireKind::Break => 0,
                _ => 1,
            })
            .sum()
    }

    /// The line budget alone cannot bound a file that is one enormous line:
    /// `lines_seen` advances once per read piece, so a 50-row viewport pulls in
    /// megabytes. The character budget is what actually stops the read.
    #[test]
    fn char_budget_bounds_a_single_enormous_line() {
        let content = "x".repeat(500_000);
        let mut buffer = Buffer::from_bytes(content.into_bytes(), test_fs());

        let unbudgeted = build_base_tokens(
            &mut buffer,
            0,
            80,
            50,
            false,
            LineEnding::LF,
            &[],
            None,
            false,
        );
        assert!(
            char_count(&unbudgeted) > 100_000,
            "expected the unbudgeted read to run away, got {} chars",
            char_count(&unbudgeted)
        );

        // 50 rows at a 200-column terminal.
        let budgeted = build_base_tokens(
            &mut buffer,
            0,
            80,
            50,
            false,
            LineEnding::LF,
            &[],
            Some(50 * 200),
            false,
        );
        let budgeted_chars = char_count(&budgeted);
        assert!(
            budgeted_chars >= 50 * 200,
            "budget must still cover every visible row, got {budgeted_chars} chars"
        );
        assert!(
            budgeted_chars < 20_000,
            "budget should stop near its bound, got {budgeted_chars} chars"
        );
    }

    /// On ordinary content the character budget never binds — the line bound
    /// is reached first — so the token stream is byte-for-byte what it was.
    #[test]
    fn char_budget_leaves_ordinary_lines_untouched() {
        let content = (0..200)
            .map(|i| format!("line {i} with some text"))
            .collect::<Vec<_>>()
            .join("\n");
        let mut buffer = Buffer::from_bytes(content.into_bytes(), test_fs());

        let unbudgeted = build_base_tokens(
            &mut buffer,
            0,
            80,
            30,
            false,
            LineEnding::LF,
            &[],
            None,
            false,
        );
        let budgeted = build_base_tokens(
            &mut buffer,
            0,
            80,
            30,
            false,
            LineEnding::LF,
            &[],
            Some(30 * 200),
            false,
        );

        assert_eq!(char_count(&unbudgeted), char_count(&budgeted));
        assert_eq!(unbudgeted.len(), budgeted.len());
    }

    /// A budget far smaller than one row still yields something to render —
    /// the floor keeps a degenerate viewport from producing an empty frame.
    #[test]
    fn char_budget_has_a_floor() {
        let content = "y".repeat(10_000);
        let mut buffer = Buffer::from_bytes(content.into_bytes(), test_fs());

        let tokens = build_base_tokens(
            &mut buffer,
            0,
            80,
            1,
            false,
            LineEnding::LF,
            &[],
            Some(0),
            false,
        );
        assert!(
            char_count(&tokens) >= 1024,
            "floor should apply to tiny budgets"
        );
    }

    /// Tokens must be identical whether the budget is reached or absent, up to
    /// the point where the budgeted stream stops — a budget may truncate, but
    /// it must never change what it does emit.
    #[test]
    fn char_budget_truncates_without_reshaping() {
        let content = format!("{}\nsecond line\nthird line\n", "z".repeat(50_000));
        let mut buffer = Buffer::from_bytes(content.into_bytes(), test_fs());

        let unbudgeted = build_base_tokens(
            &mut buffer,
            0,
            80,
            40,
            false,
            LineEnding::LF,
            &[],
            None,
            false,
        );
        let budgeted = build_base_tokens(
            &mut buffer,
            0,
            80,
            40,
            false,
            LineEnding::LF,
            &[],
            Some(4_000),
            false,
        );

        assert!(budgeted.len() <= unbudgeted.len());
        // Every token but the last (which the budget may cut mid-run) matches.
        for (i, tok) in budgeted.iter().take(budgeted.len() - 1).enumerate() {
            assert_eq!(
                tok.source_offset, unbudgeted[i].source_offset,
                "token {i} moved"
            );
        }
    }
}

/// Base tokens for exactly one logical line, without its terminating newline.
///
/// This is what feeds [`crate::view::wrap_index::WrapIndex`]. It goes through
/// the same tokenizer the renderer uses rather than wrapping raw line text in a
/// synthetic `Text` token: the count-only helpers that did the latter never
/// produced `Space` tokens, so the space-overflow back-up (issue #1363) could
/// not fire in them and their row counts could disagree with what was drawn.
/// [`build_line_tokens`], but starting at `from_byte` instead of the line's
/// start.
///
/// Repair needs only the tail from its resume row, and reading the prefix to
/// throw it away is the difference between O(rows) and O(line): on a 500 KB
/// single-line file the untargeted version tokenised and allocated the whole
/// line for every character typed. `from_byte` must be a visual-row start the
/// caller got from the index, which is what makes skipping the backward scan
/// sound.
pub(crate) fn build_line_tokens_from(
    buffer: &mut Buffer,
    line: usize,
    line_ending: LineEnding,
    fold_skip: &[std::ops::Range<usize>],
    from_byte: Option<usize>,
) -> Vec<ViewTokenWire> {
    use crate::primitives::line_iterator::MAX_LINE_BYTES;

    let line_start = buffer.line_start_offset(line).unwrap_or(0);
    let buffer_len = buffer.len();
    let line_end = buffer
        .line_start_offset(line + 1)
        .unwrap_or(buffer_len)
        .min(buffer_len);
    let estimated = buffer.estimated_line_length().max(1);

    // The whole line, however long: this feeds the wrap index, and an index
    // that has seen only part of a line reports a row count for the part. A
    // scrollbar reading that maps the whole track onto the prefix.
    //
    // `build_base_tokens`'s budget counts *units*, and a single long line
    // spends one per `MAX_LINE_BYTES` piece the reader yields. The second term
    // is left over from a forced break the character loop used to inject every
    // `MAX_SAFE_LINE_WIDTH` characters; it stays because over-asking is free
    // here and under-asking truncates a line the index is about to measure.
    let start = from_byte.filter(|b| *b > line_start && *b < line_end);
    let read_from = start.unwrap_or(line_start);
    let line_bytes = line_end.saturating_sub(read_from);
    let units = line_bytes / MAX_SAFE_LINE_WIDTH + line_bytes / MAX_LINE_BYTES + 2;
    let mut tokens = build_base_tokens(
        buffer,
        read_from,
        estimated,
        units,
        false,
        line_ending,
        fold_skip,
        None,
        start.is_some(),
    );
    // The read runs past the line end; keep only this line, and drop its
    // terminator — a newline belongs to the line break, not to a row.
    tokens.retain(|t| t.source_offset.is_none_or(|o| o < line_end));
    while matches!(
        tokens.last().map(|t| &t.kind),
        Some(ViewTokenWireKind::Newline)
    ) {
        tokens.pop();
    }
    tokens
}
