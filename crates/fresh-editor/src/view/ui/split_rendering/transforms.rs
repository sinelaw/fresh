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
use crate::state::EditorState;
use crate::view::theme::Theme;
use crate::view::ui::view_pipeline::ViewLine;
use crate::view::virtual_text::{VirtualTextNamespace, VirtualTextPosition};
use crate::view::wrap_machine::{WrapMachine, WrapRule};
use fresh_core::api::{ViewTokenStyle, ViewTokenWire, ViewTokenWireKind};
use std::collections::{HashMap, HashSet};

/// Wrap tokens to fit within `content_width` columns (accounting for a leading
/// gutter on the first visual line), emitting `Break` tokens where rows end.
///
/// A thin driver over [`WrapMachine`], which owns the wrap rule. Splitting the
/// decision (there) from the token splicing (here) is what lets the row index
/// read the same run for its boundaries — see the `wrap_machine` module docs.
pub(crate) fn apply_wrapping_transform(
    tokens: Vec<ViewTokenWire>,
    content_width: usize,
    gutter_width: usize,
    hanging_indent: bool,
) -> Vec<ViewTokenWire> {
    apply_wrapping_transform_from(tokens, content_width, gutter_width, hanging_indent, None)
}

/// As [`apply_wrapping_transform`], resuming from a previous row's carry.
///
/// `carry` is `Some` when the caller starts mid-line — the renderer beginning at
/// a viewport anchor rather than at the logical line's start. Resuming with the
/// row's carry is what makes that produce the same rows the line-start build
/// would have; see `wrap_machine::RowCarry`.
pub(crate) fn apply_wrapping_transform_from(
    tokens: Vec<ViewTokenWire>,
    content_width: usize,
    gutter_width: usize,
    hanging_indent: bool,
    carry: Option<crate::view::wrap_machine::RowCarry>,
) -> Vec<ViewTokenWire> {
    let rule = WrapRule::Word {
        content_width,
        gutter_width,
        hanging_indent,
    };
    // Guard against a pane too narrow to wrap in, which would otherwise produce
    // one `Break` per character.
    if rule.is_degenerate() {
        return tokens;
    }
    match carry {
        Some(carry) => WrapMachine::run_from(tokens, rule, carry).tokens,
        None => WrapMachine::run(tokens, rule).tokens,
    }
}

/// Terminal-grid wrap: break at **exact column boundaries** every `cols`
/// columns, the way the live PTY grid lays out its rows (fresh#2649).
///
/// Differs from [`apply_wrapping_transform`] in the rule, not the plumbing: no
/// word-boundary preference, no gutter or hanging indent, and ANSI-aware so
/// zero-width escapes never trigger a break. Both drive the same machine, so the
/// renderer and the scroll math cannot disagree about where a grid row ends —
/// the divergence that made scroll-back stick.
pub(crate) fn apply_grid_wrapping_transform(
    tokens: Vec<ViewTokenWire>,
    cols: usize,
) -> Vec<ViewTokenWire> {
    if cols == 0 {
        return tokens;
    }
    WrapMachine::run(tokens, WrapRule::Grid { cols }).tokens
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
