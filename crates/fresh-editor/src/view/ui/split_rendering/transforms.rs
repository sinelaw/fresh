//! Token / line stream transforms used by the view pipeline.
//!
//! This module contains four independent passes:
//! - `apply_wrapping_transform` — hard + soft wrap by display width
//! - `apply_soft_breaks` — inject breaks at plugin-requested positions
//! - `apply_conceal_ranges` — conceal or replace byte ranges in Text tokens
//! - `inject_virtual_lines` — inject `LineAbove` / `LineBelow` virtual text
//!
//! None of these depend on any shared render-time "mega struct".

use super::style::{create_wrapped_virtual_lines, token_style_from_ratatui};
use crate::primitives::{ansi, display_width, visual_layout};
use crate::state::EditorState;
use crate::view::theme::Theme;
use crate::view::ui::view_pipeline::ViewLine;
use crate::view::virtual_text::{VirtualTextNamespace, VirtualTextPosition};
use fresh_core::api::{ViewTokenStyle, ViewTokenWire, ViewTokenWireKind};
use std::collections::{HashMap, HashSet};

/// Wrap tokens to fit within `content_width` columns (accounting for a
/// leading gutter on the first visual line). Emits `Break` tokens where
/// lines should wrap, optionally with a hanging indent for continuation
/// lines.
///
/// The wrap algorithm:
///   1. Inter-token breaks (classic word-wrap) kick in when a Text
///      token wouldn't fit on the current row.
///   2. When a Text token is too wide to fit even on a fresh row,
///      char-wrap (grapheme-split) fills the remaining columns.
///   3. Inside the grapheme-split path, each chunk prefers to end at a
///      UAX #29 word boundary within a lookback window defined by
///      `WRAP_MAX_LOOKBACK`.  This turns e.g.
///      `dialog.getButton(...).setOnClickListener` into
///      `dialog.getButton(...)` / `.setOnClickListener` rather than a
///      mid-identifier char-split.
///   4. Falls back to the grapheme-level hard cap when no word
///      boundary qualifies — guaranteeing forward progress and, as a
///      post-condition, that no row is ever emitted wider than
///      `eff_width`.
///
/// Inter-word Space-overflow handling (issue #1363): when a `Space`
/// token at the wrap boundary would push past `eff_width`, the wrap
/// looks back for the most recent inter-word `Space` on the current
/// row and (when the tokens after it fit on a fresh continuation row)
/// moves them to the new row.  This keeps continuation rows starting
/// with content rather than a stranded leading space, without letting
/// any character drift past `eff_width`.
///
/// The grapheme-split + word-boundary algorithm in step 3/4 mirrors the
/// standalone [`crate::primitives::visual_layout::wrap_str_to_width`]
/// helper — used by the virtual-line path
/// (`split_rendering::style::create_wrapped_virtual_lines`).  Both share
/// the same `WRAP_MAX_LOOKBACK` constant; the agreement test
/// `wrap_str_to_width_matches_apply_wrapping_transform` in
/// `transforms` keeps them honest on simple inputs.  This keeps virtual
/// lines and source lines wrapping at the same boundaries even though
/// the orchestration (token carry-over, hanging indent, tabs, ANSI) is
/// only handled here.
/// Result of [`back_up_to_prior_space`]: how to split the current row's
/// tokens so the row ends just after the prior inter-word `Space` and
/// the tail becomes the start of the continuation row.
struct BackUpPlan {
    /// Index into `wrapped` where the tail begins.  Tokens at
    /// `[tail_start..]` move to the continuation row.
    tail_start: usize,
    /// Total visual width of the tail when placed at the start of a
    /// fresh row (accounting for hanging indent and tab expansion
    /// relative to the new starting column).  Used to advance
    /// `current_line_width` after re-emitting the tail.
    tail_width: usize,
}

/// Scan back through `wrapped` from the end of the current row to find
/// the most recent `Space` token on this row.  Returns a [`BackUpPlan`]
/// when a viable back-up point exists, i.e.:
///   1. There's a `Space` before any `Break` / `Newline` (so we don't
///      cross a row boundary), AND
///   2. The Space isn't the row's first content token (backing up would
///      otherwise leave the row empty), AND
///   3. The tail tokens fit within `eff_width` columns when re-emitted
///      at the start of a fresh continuation row (accounting for
///      `line_indent`).
///
/// Returns `None` if no such point exists — the caller falls back to
/// the old "emit Break, push current token on new row" behaviour.
fn back_up_to_prior_space(
    wrapped: &[ViewTokenWire],
    line_indent: usize,
    eff_width: usize,
) -> Option<BackUpPlan> {
    use crate::primitives::visual_layout;

    // Walk back from the end until we find a `Space`.  Stop at
    // `Break`/`Newline` — they bound the current row.
    let mut space_idx: Option<usize> = None;
    for (i, t) in wrapped.iter().enumerate().rev() {
        match &t.kind {
            ViewTokenWireKind::Break | ViewTokenWireKind::Newline => return None,
            ViewTokenWireKind::Space => {
                space_idx = Some(i);
                break;
            }
            _ => continue,
        }
    }
    let space_idx = space_idx?;

    // Tail starts right after the Space.  If the tail is empty, there's
    // nothing to move — the caller hit a Space-after-Space pattern and
    // we have no word to back over.
    let tail_start = space_idx + 1;
    if tail_start >= wrapped.len() {
        return None;
    }

    // Require at least one non-Space token BEFORE this Space, otherwise
    // backing up would leave a row consisting solely of a Space.
    if !wrapped[..space_idx]
        .iter()
        .any(|t| !matches!(t.kind, ViewTokenWireKind::Space))
    {
        return None;
    }

    // Compute the tail's visual width as it would appear on a fresh
    // continuation row starting at column `line_indent`.  Tab widths
    // depend on the starting column, so we re-derive them here.
    let mut col = line_indent;
    for t in &wrapped[tail_start..] {
        match &t.kind {
            ViewTokenWireKind::Text(s) => col += visual_layout::visual_width(s, col),
            ViewTokenWireKind::Space => col += 1,
            ViewTokenWireKind::BinaryByte(_) => col += 4,
            // Break / Newline shouldn't appear inside a single row's
            // post-Space tail; if they do, the back-up is unsafe.
            _ => return None,
        }
    }
    let tail_width = col.saturating_sub(line_indent);
    if line_indent + tail_width > eff_width {
        return None;
    }
    Some(BackUpPlan {
        tail_start,
        tail_width,
    })
}

pub(crate) fn apply_wrapping_transform(
    tokens: Vec<ViewTokenWire>,
    content_width: usize,
    gutter_width: usize,
    hanging_indent: bool,
) -> Vec<ViewTokenWire> {
    use visual_layout::visual_width;
    // Single source of truth for the lookback window — keeps the
    // word-boundary preference here in sync with the standalone
    // `wrap_str_to_width` helper used by the virtual-line path.
    use visual_layout::WRAP_MAX_LOOKBACK as MAX_LOOKBACK;

    /// Minimum content width for continuation lines when hanging indent is active.
    const MIN_CONTINUATION_CONTENT_WIDTH: usize = 10;

    // Calculate available width (accounting for gutter on first line only)
    let available_width = content_width.saturating_sub(gutter_width);

    // Guard against zero or very small available width which would produce
    // one Break per character, causing pathological memory usage.
    if available_width < 2 {
        return tokens;
    }

    let mut wrapped = Vec::new();
    let mut current_line_width: usize = 0;

    // Hanging indent state: the visual indent width for the current logical line.
    let mut line_indent: usize = 0;
    let mut measuring_indent = hanging_indent;
    let mut on_continuation = false;

    /// Effective width for the current segment.
    ///
    /// Always returns `available_width` because hanging indent is already
    /// accounted for by the indent text emitted into `current_line_width`
    /// via `emit_break_with_indent`. Subtracting `line_indent` here would
    /// double-count it.
    #[inline]
    fn effective_width(
        available_width: usize,
        _line_indent: usize,
        _on_continuation: bool,
    ) -> usize {
        available_width
    }

    /// Emit a Break token followed by hanging indent spaces.
    fn emit_break_with_indent(
        wrapped: &mut Vec<ViewTokenWire>,
        current_line_width: &mut usize,
        indent_string: &str,
    ) {
        wrapped.push(ViewTokenWire {
            source_offset: None,
            kind: ViewTokenWireKind::Break,
            style: None,
        });
        *current_line_width = 0;
        if !indent_string.is_empty() {
            wrapped.push(ViewTokenWire {
                source_offset: None,
                kind: ViewTokenWireKind::Text(indent_string.to_string()),
                style: None,
            });
            *current_line_width = indent_string.len();
        }
    }

    // Pre-computed indent string, updated only when line_indent changes.
    let mut cached_indent_string = String::new();
    let mut cached_indent_len: usize = 0;

    for token in tokens {
        match &token.kind {
            ViewTokenWireKind::Newline => {
                wrapped.push(token);
                current_line_width = 0;
                line_indent = 0;
                cached_indent_string.clear();
                cached_indent_len = 0;
                measuring_indent = hanging_indent;
                on_continuation = false;
            }
            ViewTokenWireKind::Text(text) => {
                if measuring_indent {
                    let mut ws_char_count = 0usize;
                    let mut ws_visual_width = 0usize;
                    for c in text.chars() {
                        if c == ' ' {
                            ws_visual_width += 1;
                            ws_char_count += 1;
                        } else if c == '\t' {
                            let tab_stop = 4;
                            let col = line_indent + ws_visual_width;
                            ws_visual_width += tab_stop - (col % tab_stop);
                            ws_char_count += 1;
                        } else {
                            break;
                        }
                    }
                    if ws_char_count == text.chars().count() {
                        line_indent += ws_visual_width;
                    } else {
                        line_indent += ws_visual_width;
                        measuring_indent = false;
                    }
                    if line_indent + MIN_CONTINUATION_CONTENT_WIDTH > available_width {
                        line_indent = 0;
                    }
                    if line_indent != cached_indent_len {
                        cached_indent_string = " ".repeat(line_indent);
                        cached_indent_len = line_indent;
                    }
                }

                let eff_width = effective_width(available_width, line_indent, on_continuation);
                let text_visual_width = visual_width(text, current_line_width);

                // Break before the token whenever it overflows the
                // current row AND either
                //   (a) the token will fit on a fresh continuation line
                //       (classic word-wrap), or
                //   (b) the row already holds at least `row_floor`
                //       columns of content.  Every Text token begins at
                //       a UAX #29 word boundary (tokens are split on
                //       spaces by the tokenizer), so ending the current
                //       row here lands on a boundary — which beats
                //       pushing one straggler grapheme mid-word just to
                //       reach `eff_width`.  When the row is still below
                //       the floor, don't break: the grapheme-split path
                //       below will fill the remaining columns.
                let fresh_line_capacity = eff_width.saturating_sub(line_indent);
                let row_floor = eff_width.saturating_sub(MAX_LOOKBACK).max(eff_width / 2);
                if current_line_width > 0
                    && current_line_width + text_visual_width > eff_width
                    && (text_visual_width <= fresh_line_capacity || current_line_width >= row_floor)
                {
                    on_continuation = true;
                    emit_break_with_indent(
                        &mut wrapped,
                        &mut current_line_width,
                        &cached_indent_string,
                    );
                }

                let eff_width = effective_width(available_width, line_indent, on_continuation);
                let text_visual_width = visual_width(text, current_line_width);

                // Char-split whenever the token still won't fit on the
                // current line — including the post-break case where the
                // hanging indent alone already leaves no room for the whole
                // token.  Using `current_line_width + text_visual_width`
                // (rather than just `text_visual_width`) is what keeps the
                // transform from writing past `eff_width`.
                if current_line_width + text_visual_width > eff_width
                    && !ansi::contains_ansi_codes(text)
                {
                    use unicode_segmentation::UnicodeSegmentation;

                    let graphemes: Vec<(usize, &str)> = text.grapheme_indices(true).collect();
                    let mut grapheme_idx = 0;
                    let source_base = token.source_offset;

                    // Pre-compute UAX #29 word boundary byte offsets ONCE
                    // for the entire text.  Each chunk later filters this
                    // sorted list down to its window (slice_start ..=
                    // slice_end_hard) and walks it as a monotonic cursor.
                    //
                    // Without this, the per-chunk loop below called
                    // `text.split_word_bound_indices()` afresh — an O(n)
                    // scan from byte 0 of the WHOLE text on every chunk.
                    // For a single very long token wrapping into thousands
                    // of chunks, that's O(n²) per token.  Lifting the
                    // boundary list out of the loop and walking it with a
                    // monotonic cursor brings the loop's amortised cost
                    // back to O(n).
                    let word_bounds: Vec<usize> =
                        text.split_word_bound_indices().map(|(b, _)| b).collect();
                    // Cursor: word_bounds[wb_lo..] are all > most recent
                    // chunk's slice_start.  Advanced monotonically as
                    // chunks progress.
                    let mut wb_lo: usize = 0;

                    while grapheme_idx < graphemes.len() {
                        let eff_width =
                            effective_width(available_width, line_indent, on_continuation);
                        let remaining_width = eff_width.saturating_sub(current_line_width);
                        if remaining_width == 0 {
                            // No room left on the current line — emit a
                            // break and retry.  `line_indent` is clamped
                            // above to leave at least
                            // MIN_CONTINUATION_CONTENT_WIDTH of usable
                            // space on every continuation, so this can't
                            // loop forever.  The earlier "force one
                            // grapheme" fallback here wrote past
                            // `eff_width`, which the renderer then
                            // clipped — the source of the missing chars
                            // seen when a Text token landed on a line
                            // whose hanging indent had consumed all of
                            // `eff_width`.
                            on_continuation = true;
                            emit_break_with_indent(
                                &mut wrapped,
                                &mut current_line_width,
                                &cached_indent_string,
                            );
                            continue;
                        }

                        let mut chunk_visual_width = 0;
                        let mut chunk_grapheme_count = 0;
                        let mut col = current_line_width;

                        for &(_byte_offset, grapheme) in &graphemes[grapheme_idx..] {
                            let g_width = if grapheme == "\t" {
                                visual_layout::tab_expansion_width(col)
                            } else {
                                display_width::str_width(grapheme)
                            };

                            if chunk_visual_width + g_width > remaining_width
                                && chunk_grapheme_count > 0
                            {
                                break;
                            }

                            chunk_visual_width += g_width;
                            chunk_grapheme_count += 1;
                            col += g_width;
                        }

                        if chunk_grapheme_count == 0 {
                            chunk_grapheme_count = 1;
                            let grapheme = graphemes[grapheme_idx].1;
                            chunk_visual_width = if grapheme == "\t" {
                                visual_layout::tab_expansion_width(current_line_width)
                            } else {
                                display_width::str_width(grapheme)
                            };
                        }

                        // Prefer a UAX #29 word boundary as the split
                        // point.  Word boundaries depend on context
                        // (e.g. an 'A' followed by '_' is part of the
                        // same word segment), so we compute them on
                        // the FULL token text and then constrain to
                        // the window `[floor, hard_cap]` relative to
                        // the current grapheme cursor.  The floor is
                        // computed ROW-relative (not chunk-relative)
                        // because the invariant is about where the
                        // visual row ends — if the row already has
                        // content from earlier tokens or an indent,
                        // the chunk's minimum length is proportionally
                        // smaller.  If a qualified boundary exists,
                        // shrink the chunk to end there AND force a
                        // break immediately after push — otherwise
                        // the next iteration would refill the space
                        // we freed and undo the shrink.  Falls back to
                        // the hard cap when no boundary qualifies.
                        let mut force_break_after_push = false;
                        if chunk_grapheme_count > 1 {
                            let slice_start = graphemes[grapheme_idx].0;
                            let slice_end_hard =
                                if grapheme_idx + chunk_grapheme_count < graphemes.len() {
                                    graphemes[grapheme_idx + chunk_grapheme_count].0
                                } else {
                                    text.len()
                                };
                            let row_floor =
                                eff_width.saturating_sub(MAX_LOOKBACK).max(eff_width / 2);
                            let chunk_floor_from_cursor =
                                row_floor.saturating_sub(current_line_width);
                            let floor_byte = if chunk_floor_from_cursor < chunk_grapheme_count {
                                graphemes[grapheme_idx + chunk_floor_from_cursor].0
                            } else {
                                slice_end_hard
                            };

                            // Walk the precomputed `word_bounds` list as
                            // a monotonic cursor.  `wb_lo` advances past
                            // entries already <= slice_start; from there
                            // we look forward only until we cross
                            // slice_end_hard.  Amortised O(1) per chunk,
                            // O(N) total — replacing the previous
                            // per-chunk full-text rescan that made the
                            // whole loop O(n²) on a single very long
                            // token.
                            //
                            // We still treat `text.len()` as a virtual
                            // boundary so a chunk that happens to end
                            // exactly at the text end isn't shrunk to
                            // an earlier boundary (leaking chars onto
                            // the next row).
                            while wb_lo < word_bounds.len() && word_bounds[wb_lo] <= slice_start {
                                wb_lo += 1;
                            }
                            let mut wb_hi = wb_lo;
                            while wb_hi < word_bounds.len() && word_bounds[wb_hi] <= slice_end_hard
                            {
                                wb_hi += 1;
                            }
                            // Within `word_bounds[wb_lo..wb_hi]`, find the
                            // largest entry >= floor_byte.  We scan from
                            // the back since we want the MAX qualifier.
                            let mut best_target_byte = word_bounds[wb_lo..wb_hi]
                                .iter()
                                .rev()
                                .copied()
                                .find(|&b| b >= floor_byte);
                            // Consider `text.len()` as a virtual
                            // boundary if it's within the window.
                            let end_byte = text.len();
                            if best_target_byte.is_none()
                                && end_byte > slice_start
                                && end_byte >= floor_byte
                                && end_byte <= slice_end_hard
                            {
                                best_target_byte = Some(end_byte);
                            } else if let Some(b) = best_target_byte {
                                // We found one in the precomputed list,
                                // but text.len() (if eligible) might be
                                // larger.  next_back semantics in the
                                // original picked the maximum across
                                // both sources.
                                if end_byte <= slice_end_hard
                                    && end_byte >= floor_byte
                                    && end_byte > b
                                {
                                    best_target_byte = Some(end_byte);
                                }
                            }

                            if let Some(target_byte) = best_target_byte {
                                let new_count = graphemes[grapheme_idx..]
                                    .iter()
                                    .position(|(b, _)| *b == target_byte)
                                    .unwrap_or(chunk_grapheme_count);
                                if new_count > 0 && new_count < chunk_grapheme_count {
                                    chunk_grapheme_count = new_count;
                                    let mut col = current_line_width;
                                    chunk_visual_width = 0;
                                    for &(_b, g) in
                                        &graphemes[grapheme_idx..grapheme_idx + new_count]
                                    {
                                        let w = if g == "\t" {
                                            visual_layout::tab_expansion_width(col)
                                        } else {
                                            display_width::str_width(g)
                                        };
                                        chunk_visual_width += w;
                                        col += w;
                                    }
                                    force_break_after_push = true;
                                }
                            }
                        }

                        let chunk_start_byte = graphemes[grapheme_idx].0;
                        let chunk_end_byte =
                            if grapheme_idx + chunk_grapheme_count < graphemes.len() {
                                graphemes[grapheme_idx + chunk_grapheme_count].0
                            } else {
                                text.len()
                            };
                        let chunk = text[chunk_start_byte..chunk_end_byte].to_string();
                        let chunk_source = source_base.map(|b| b + chunk_start_byte);

                        wrapped.push(ViewTokenWire {
                            source_offset: chunk_source,
                            kind: ViewTokenWireKind::Text(chunk),
                            style: token.style.clone(),
                        });

                        current_line_width += chunk_visual_width;
                        grapheme_idx += chunk_grapheme_count;

                        let eff_width =
                            effective_width(available_width, line_indent, on_continuation);
                        if force_break_after_push || current_line_width >= eff_width {
                            on_continuation = true;
                            emit_break_with_indent(
                                &mut wrapped,
                                &mut current_line_width,
                                &cached_indent_string,
                            );
                        }
                    }
                } else {
                    wrapped.push(token);
                    current_line_width += text_visual_width;
                }
            }
            ViewTokenWireKind::Space => {
                if measuring_indent {
                    line_indent += 1;
                    if line_indent + MIN_CONTINUATION_CONTENT_WIDTH > available_width {
                        line_indent = 0;
                    }
                }

                let eff_width = effective_width(available_width, line_indent, on_continuation);
                if current_line_width + 1 > eff_width {
                    // Space-overflow: emitting a Break here would
                    // strand this Space as a leading space at the
                    // start of the continuation row.  Try backing up
                    // to before the most recent word on the current
                    // row instead — that word moves to the
                    // continuation row, and the row break lands at
                    // the prior inter-word Space (which sits at a
                    // column well within `eff_width`).  See issue
                    // #1363.  Falls back to the old "Break, push
                    // Space on new row" path when no back-up point
                    // exists (single-word row from char-split, or
                    // the Space is the row's first token).
                    let back_up = back_up_to_prior_space(&wrapped, line_indent, eff_width);
                    if let Some(BackUpPlan {
                        tail_start,
                        tail_width,
                    }) = back_up
                    {
                        let tail: Vec<ViewTokenWire> = wrapped.drain(tail_start..).collect();
                        on_continuation = true;
                        emit_break_with_indent(
                            &mut wrapped,
                            &mut current_line_width,
                            &cached_indent_string,
                        );
                        // Tail tokens are guaranteed to fit on a
                        // fresh continuation row (we computed
                        // `tail_width` against `eff_width`); push
                        // them back and advance the column by the
                        // precomputed width.
                        wrapped.extend(tail);
                        current_line_width += tail_width;
                    } else {
                        on_continuation = true;
                        emit_break_with_indent(
                            &mut wrapped,
                            &mut current_line_width,
                            &cached_indent_string,
                        );
                    }
                }
                wrapped.push(token);
                current_line_width += 1;
            }
            ViewTokenWireKind::Break => {
                wrapped.push(token);
                current_line_width = 0;
                on_continuation = true;
                if line_indent > 0 {
                    wrapped.push(ViewTokenWire {
                        source_offset: None,
                        kind: ViewTokenWireKind::Text(" ".repeat(line_indent)),
                        style: None,
                    });
                    current_line_width = line_indent;
                }
            }
            ViewTokenWireKind::BinaryByte(_) => {
                if measuring_indent {
                    measuring_indent = false;
                }

                let eff_width = effective_width(available_width, line_indent, on_continuation);
                let byte_display_width = 4;
                if current_line_width + byte_display_width > eff_width {
                    on_continuation = true;
                    emit_break_with_indent(
                        &mut wrapped,
                        &mut current_line_width,
                        &cached_indent_string,
                    );
                }
                wrapped.push(token);
                current_line_width += byte_display_width;
            }
        }
    }

    wrapped
}

/// Apply soft breaks to a token stream.
///
/// Walks tokens with a sorted break list `[(position, indent)]`. When a
/// token's `source_offset` matches a break position:
/// - For Space tokens: replace with Newline + indent Spaces
/// - For other tokens: insert Newline + indent Spaces before the token
///
/// Tokens without source_offset (injected/virtual) pass through unchanged.
pub(crate) fn apply_soft_breaks(
    tokens: Vec<ViewTokenWire>,
    soft_breaks: &[(usize, u16)],
) -> Vec<ViewTokenWire> {
    if soft_breaks.is_empty() {
        return tokens;
    }

    let mut output = Vec::with_capacity(tokens.len() + soft_breaks.len() * 2);
    let mut break_idx = 0;

    for token in tokens {
        let offset = match token.source_offset {
            Some(o) => o,
            None => {
                output.push(token);
                continue;
            }
        };

        while break_idx < soft_breaks.len() && soft_breaks[break_idx].0 < offset {
            break_idx += 1;
        }

        if break_idx < soft_breaks.len() && soft_breaks[break_idx].0 == offset {
            let indent = soft_breaks[break_idx].1;
            break_idx += 1;

            match &token.kind {
                ViewTokenWireKind::Space => {
                    output.push(ViewTokenWire {
                        source_offset: None,
                        kind: ViewTokenWireKind::Newline,
                        style: None,
                    });
                    for _ in 0..indent {
                        output.push(ViewTokenWire {
                            source_offset: None,
                            kind: ViewTokenWireKind::Space,
                            style: None,
                        });
                    }
                }
                _ => {
                    output.push(ViewTokenWire {
                        source_offset: None,
                        kind: ViewTokenWireKind::Newline,
                        style: None,
                    });
                    for _ in 0..indent {
                        output.push(ViewTokenWire {
                            source_offset: None,
                            kind: ViewTokenWireKind::Space,
                            style: None,
                        });
                    }
                    output.push(token);
                }
            }
        } else {
            output.push(token);
        }
    }

    output
}

/// Apply conceal ranges to a token stream.
///
/// Handles partial token overlap: if a Text token spans bytes that are
/// partially concealed, the token is split at conceal boundaries. Non-text
/// tokens (Space, Newline) are treated as single-byte.
///
/// Tokens without source_offset (injected/virtual) always pass through.
pub(crate) fn apply_conceal_ranges(
    tokens: Vec<ViewTokenWire>,
    conceal_ranges: &[(std::ops::Range<usize>, Option<&str>)],
) -> Vec<ViewTokenWire> {
    if conceal_ranges.is_empty() {
        return tokens;
    }

    let mut output = Vec::with_capacity(tokens.len());
    let mut emitted_replacements: HashSet<usize> = HashSet::new();

    // Sort a parallel index by `range.start` so the concealment lookup can
    // be a monotonic cursor instead of a per-byte linear scan. Conceals
    // rarely overlap (typically markdown syntax markers); the cursor walks
    // the sorted list as tokens advance through source bytes.
    let mut sorted: Vec<usize> = (0..conceal_ranges.len()).collect();
    sorted.sort_by_key(|&i| conceal_ranges[i].0.start);
    let mut conceal_cursor: usize = 0;

    // Advance `conceal_cursor` past ranges ending before `byte_offset`,
    // then check if the current range contains `byte_offset`. Returns the
    // *original* conceal index (so `emitted_replacements` keys stay
    // stable). Monotonic: caller must invoke with non-decreasing
    // `byte_offset` within the token stream.
    #[inline]
    fn is_concealed(
        conceal_ranges: &[(std::ops::Range<usize>, Option<&str>)],
        sorted: &[usize],
        cursor: &mut usize,
        byte_offset: usize,
    ) -> Option<usize> {
        while *cursor < sorted.len() && conceal_ranges[sorted[*cursor]].0.end <= byte_offset {
            *cursor += 1;
        }
        let orig_idx = sorted.get(*cursor).copied()?;
        let range = &conceal_ranges[orig_idx].0;
        (range.start <= byte_offset && byte_offset < range.end).then_some(orig_idx)
    }

    for token in tokens {
        let offset = match token.source_offset {
            Some(o) => o,
            None => {
                output.push(token);
                continue;
            }
        };

        match &token.kind {
            ViewTokenWireKind::Text(text) => {
                let mut current_byte = offset;
                let mut visible_start: Option<usize> = None;
                let mut visible_chars = String::new();

                for ch in text.chars() {
                    let ch_len = ch.len_utf8();

                    if let Some(cidx) =
                        is_concealed(conceal_ranges, &sorted, &mut conceal_cursor, current_byte)
                    {
                        if !visible_chars.is_empty() {
                            output.push(ViewTokenWire {
                                source_offset: visible_start,
                                kind: ViewTokenWireKind::Text(std::mem::take(&mut visible_chars)),
                                style: token.style.clone(),
                            });
                            visible_start = None;
                        }

                        // Emit replacement text once per conceal range.
                        // Split into first-char (with source_offset for cursor/click
                        // positioning) and remaining chars (with None source_offset).
                        if let Some(repl) = conceal_ranges[cidx].1 {
                            if !emitted_replacements.contains(&cidx) {
                                emitted_replacements.insert(cidx);
                                if !repl.is_empty() {
                                    let mut chars = repl.chars();
                                    if let Some(first_ch) = chars.next() {
                                        output.push(ViewTokenWire {
                                            source_offset: Some(conceal_ranges[cidx].0.start),
                                            kind: ViewTokenWireKind::Text(first_ch.to_string()),
                                            style: None,
                                        });
                                        let rest: String = chars.collect();
                                        if !rest.is_empty() {
                                            output.push(ViewTokenWire {
                                                source_offset: None,
                                                kind: ViewTokenWireKind::Text(rest),
                                                style: None,
                                            });
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        if visible_start.is_none() {
                            visible_start = Some(current_byte);
                        }
                        visible_chars.push(ch);
                    }

                    current_byte += ch_len;
                }

                if !visible_chars.is_empty() {
                    output.push(ViewTokenWire {
                        source_offset: visible_start,
                        kind: ViewTokenWireKind::Text(visible_chars),
                        style: token.style.clone(),
                    });
                }
            }
            ViewTokenWireKind::Space | ViewTokenWireKind::Newline | ViewTokenWireKind::Break => {
                if let Some(cidx) =
                    is_concealed(conceal_ranges, &sorted, &mut conceal_cursor, offset)
                {
                    // Concealed single-byte token.  If the conceal
                    // range carries a `replacement`, we still need
                    // to emit it — the Text branch above does this
                    // via `emitted_replacements`, and dropping the
                    // token here without doing the same was a real
                    // bug: e.g. flash plugin labels overlay the
                    // next char after each match, and when that
                    // next char is a space the renderer used to
                    // eat the cell entirely (label letter never
                    // shown, surrounding text shifted left).
                    if let Some(repl) = conceal_ranges[cidx].1 {
                        if !emitted_replacements.contains(&cidx) {
                            emitted_replacements.insert(cidx);
                            if !repl.is_empty() {
                                let mut chars = repl.chars();
                                if let Some(first_ch) = chars.next() {
                                    output.push(ViewTokenWire {
                                        source_offset: Some(conceal_ranges[cidx].0.start),
                                        kind: ViewTokenWireKind::Text(first_ch.to_string()),
                                        style: None,
                                    });
                                    let rest: String = chars.collect();
                                    if !rest.is_empty() {
                                        output.push(ViewTokenWire {
                                            source_offset: None,
                                            kind: ViewTokenWireKind::Text(rest),
                                            style: None,
                                        });
                                    }
                                }
                            }
                        }
                    }
                    // null replacement = hide the byte range; nothing to emit.
                } else {
                    output.push(token);
                }
            }
            ViewTokenWireKind::BinaryByte(_) => {
                if is_concealed(conceal_ranges, &sorted, &mut conceal_cursor, offset).is_some() {
                    // Skip concealed binary byte
                } else {
                    output.push(token);
                }
            }
        }
    }

    output
}

/// Inject `LineAbove` / `LineBelow` virtual lines into the view line stream.
///
/// `wrap_width` is the viewport's effective content width when soft-wrap is
/// enabled, allowing a virtual line whose text exceeds the row width to be
/// split across multiple visual rows (matching how source lines behave under
/// `line_wrap = true`). Pass `None` to keep virtual lines on a single row.
pub(super) fn inject_virtual_lines(
    source_lines: Vec<ViewLine>,
    state: &EditorState,
    theme: &Theme,
    wrap_width: Option<usize>,
    is_compose: bool,
) -> Vec<ViewLine> {
    // Get viewport byte range from source lines.
    // Use the last line that has source bytes (not a trailing empty line
    // which the iterator may emit at the buffer end).
    let viewport_start = source_lines
        .first()
        .and_then(|l| l.char_source_bytes.iter().find_map(|m| *m))
        .unwrap_or(0);
    let viewport_end = source_lines
        .iter()
        .rev()
        .find_map(|l| l.char_source_bytes.iter().rev().find_map(|m| *m))
        .map(|b| b + 1)
        .unwrap_or(viewport_start);

    let mut virtual_lines =
        state
            .virtual_texts
            .query_lines_in_range(&state.marker_list, viewport_start, viewport_end);

    // markdown_compose's table-border virtual lines (`md-tb`) frame the composed
    // table and belong only to a Compose-mode split. Virtual lines live on the
    // buffer, so in a Source-mode split sharing a buffer with a composing sibling
    // they would otherwise draw a frame around the raw source. Drop that
    // compose-only namespace here — mirroring the `md-syntax` conceal gate in
    // `view_data.rs` and the `md-emphasis` overlay gate in `overlays.rs`. Every
    // other virtual-line namespace (git blame, diff, …) renders in both modes.
    if !is_compose {
        let md_border_ns = VirtualTextNamespace::from_string("md-tb".to_string());
        virtual_lines.retain(|(_, vt)| vt.namespace.as_ref() != Some(&md_border_ns));
    }

    if virtual_lines.is_empty() {
        return source_lines;
    }

    let mut result = Vec::with_capacity(source_lines.len() + virtual_lines.len());

    for source_line in source_lines {
        let line_start_byte = source_line.char_source_bytes.iter().find_map(|m| *m);
        let line_end_byte = source_line
            .char_source_bytes
            .iter()
            .rev()
            .find_map(|m| *m)
            .map(|b| b + 1);

        if let (Some(start), Some(end)) = (line_start_byte, line_end_byte) {
            for (anchor_pos, vtext) in &virtual_lines {
                if *anchor_pos >= start
                    && *anchor_pos < end
                    && vtext.position == VirtualTextPosition::LineAbove
                {
                    let glyph = vtext.gutter_glyph.as_ref().map(|g| {
                        (
                            g.clone(),
                            vtext.gutter_color.unwrap_or(theme.line_number_fg),
                        )
                    });
                    result.extend(create_wrapped_virtual_lines(
                        &vtext.text,
                        vtext.resolved_style(theme),
                        wrap_width,
                        glyph,
                        &vtext.text_overlays,
                    ));
                }
            }
        }

        result.push(source_line.clone());

        if let (Some(start), Some(end)) = (line_start_byte, line_end_byte) {
            for (anchor_pos, vtext) in &virtual_lines {
                if *anchor_pos >= start
                    && *anchor_pos < end
                    && vtext.position == VirtualTextPosition::LineBelow
                {
                    let glyph = vtext.gutter_glyph.as_ref().map(|g| {
                        (
                            g.clone(),
                            vtext.gutter_color.unwrap_or(theme.line_number_fg),
                        )
                    });
                    result.extend(create_wrapped_virtual_lines(
                        &vtext.text,
                        vtext.resolved_style(theme),
                        wrap_width,
                        glyph,
                        &vtext.text_overlays,
                    ));
                }
            }
        }
    }

    result
}

/// One inline inlay-hint cell to splice into the token stream, already
/// padded to match the legacy render-time spacing and resolved to a wire
/// style.
struct InlineHintCell {
    text: String,
    style: Option<ViewTokenStyle>,
}

/// Splice inline virtual text (`BeforeChar` / `AfterChar` inlay hints) into
/// the token stream as styled `source_offset: None` Text cells, **before**
/// wrapping.
///
/// This is the heart of the canonical layout model: by turning hints into
/// real cells up front, their display width participates in line wrapping,
/// in the per-character visual-column map ([`ViewLine`]), and therefore in
/// horizontal scrolling and cursor math — all from a single source of
/// truth. Previously hints were drawn only at render time, invisible to
/// wrapping and h-scroll, which dropped wrapped characters (the hint width
/// pushed real text past the row edge) and clipped the end of hinted lines
/// when scrolling.
///
/// Padding mirrors the old render-time injection exactly so output is
/// unchanged except for the bug fix:
///   - `BeforeChar`: `"{text} "`, or `" {text} "` when anchored on a
///     newline (an end-of-line hint).
///   - `AfterChar`:  `" {text}"`.
///
/// `theme` is `Some` on the draw path (so hint colours resolve) and `None`
/// on the wrap-cache / scroll-math path, where only cell *width* matters and
/// the output is never drawn.
pub fn splice_inline_virtual_text(
    tokens: Vec<ViewTokenWire>,
    state: &EditorState,
    theme: Option<&Theme>,
    start: usize,
    end: usize,
) -> Vec<ViewTokenWire> {
    let inline = state
        .virtual_texts
        .query_inline_in_range(&state.marker_list, start, end);
    if inline.is_empty() {
        return tokens;
    }

    // Group by anchor byte, preserving the query's (position, priority)
    // order. `before` stores the raw hint text — its leading-space padding
    // depends on whether the anchor cell is a newline, decided while
    // walking the token stream below.
    let mut before: HashMap<usize, Vec<(String, Option<ViewTokenStyle>)>> = HashMap::new();
    let mut after: HashMap<usize, Vec<InlineHintCell>> = HashMap::new();
    for (pos, vtext) in inline {
        let style = theme.map(|t| token_style_from_ratatui(vtext.resolved_style(t)));
        match vtext.position {
            VirtualTextPosition::BeforeChar => {
                before
                    .entry(pos)
                    .or_default()
                    .push((vtext.text.clone(), style));
            }
            VirtualTextPosition::AfterChar => {
                after.entry(pos).or_default().push(InlineHintCell {
                    text: format!(" {}", vtext.text),
                    style,
                });
            }
            // Line-level positions are handled by `inject_virtual_lines`.
            _ => {}
        }
    }

    let virt = |text: String, style: Option<ViewTokenStyle>| ViewTokenWire {
        source_offset: None,
        kind: ViewTokenWireKind::Text(text),
        style,
    };

    let mut out: Vec<ViewTokenWire> = Vec::with_capacity(tokens.len());
    for token in tokens {
        let src = token.source_offset;
        match (&token.kind, src) {
            (ViewTokenWireKind::Text(s), Some(token_start)) => {
                // Split the (possibly coalesced) Text token at each hint
                // anchor so before/after cells land in the right place and
                // source-byte mapping stays exact for the surrounding text.
                let mut seg = String::new();
                let mut seg_start = token_start;
                let mut byte_idx = 0usize;
                for ch in s.chars() {
                    let anchor = token_start + byte_idx;
                    if let Some(hints) = before.get(&anchor) {
                        if !seg.is_empty() {
                            out.push(ViewTokenWire {
                                source_offset: Some(seg_start),
                                kind: ViewTokenWireKind::Text(std::mem::take(&mut seg)),
                                style: token.style.clone(),
                            });
                        }
                        seg_start = anchor;
                        for (text, style) in hints {
                            out.push(virt(format!("{text} "), style.clone()));
                        }
                    }
                    seg.push(ch);
                    byte_idx += ch.len_utf8();
                    if let Some(hints) = after.get(&anchor) {
                        out.push(ViewTokenWire {
                            source_offset: Some(seg_start),
                            kind: ViewTokenWireKind::Text(std::mem::take(&mut seg)),
                            style: token.style.clone(),
                        });
                        seg_start = token_start + byte_idx;
                        for hint in hints {
                            out.push(virt(hint.text.clone(), hint.style.clone()));
                        }
                    }
                }
                if !seg.is_empty() {
                    out.push(ViewTokenWire {
                        source_offset: Some(seg_start),
                        kind: ViewTokenWireKind::Text(seg),
                        style: token.style.clone(),
                    });
                }
            }
            (kind, Some(anchor)) => {
                // Atomic source cell (Newline / Space / BinaryByte): hints
                // anchor around the whole cell. A `BeforeChar` hint on a
                // newline is an end-of-line hint and gets a leading space.
                let anchor_is_newline = matches!(kind, ViewTokenWireKind::Newline);
                if let Some(hints) = before.get(&anchor) {
                    for (text, style) in hints {
                        let padded = if anchor_is_newline {
                            format!(" {text} ")
                        } else {
                            format!("{text} ")
                        };
                        out.push(virt(padded, style.clone()));
                    }
                }
                let after_hints = after.get(&anchor);
                out.push(token);
                if let Some(hints) = after_hints {
                    for hint in hints {
                        out.push(virt(hint.text.clone(), hint.style.clone()));
                    }
                }
            }
            // Injected tokens (Break, or any `source_offset: None`) carry no
            // anchor and pass through untouched.
            _ => out.push(token),
        }
    }

    out
}
