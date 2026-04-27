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
pub(crate) fn build_base_tokens(
    buffer: &mut Buffer,
    top_byte: usize,
    estimated_line_length: usize,
    visible_count: usize,
    is_binary: bool,
    line_ending: LineEnding,
    fold_skip: &[std::ops::Range<usize>],
) -> Vec<ViewTokenWire> {
    let mut tokens = Vec::new();

    // For binary files, read raw bytes directly to preserve byte values
    // (LineIterator uses String::from_utf8_lossy which loses high bytes)
    if is_binary {
        return build_base_tokens_binary(buffer, top_byte, estimated_line_length, visible_count);
    }

    let max_lines = visible_count.saturating_add(4);
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

        let mut iter = buffer.line_iterator(cursor, estimated_line_length);
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
            let mut chars_this_line = 0usize; // Track chars to enforce MAX_SAFE_LINE_WIDTH
            for ch in line_content.chars() {
                // Limit characters per line to prevent memory exhaustion from huge lines.
                // Insert a Break token to force wrapping at safe intervals.
                if chars_this_line >= MAX_SAFE_LINE_WIDTH {
                    tokens.push(ViewTokenWire {
                        source_offset: None,
                        kind: ViewTokenWireKind::Break,
                        style: None,
                    });
                    chars_this_line = 0;
                    lines_seen += 1;
                    if lines_seen >= max_lines {
                        break;
                    }
                }
                chars_this_line += 1;

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
