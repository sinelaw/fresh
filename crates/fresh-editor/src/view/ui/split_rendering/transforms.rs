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
use crate::primitives::display_width::str_width;
use crate::state::EditorState;
use crate::view::compose_only::is_compose_only_virtual_text;
use crate::view::soft_break::SoftBreakRender;
use crate::view::theme::Theme;
use crate::view::ui::view_pipeline::{LineStart, ViewLine};
use crate::view::virtual_text::VirtualTextPosition;
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
/// Walks tokens with a sorted break list. A break at a position:
/// - on a Space token: replaces it with Newline + the continuation indent
/// - anywhere else: inserts Newline + the continuation indent before that byte,
///   splitting the Text token it lands inside if it is not already at a token
///   boundary
///
/// That last case is what lets a caller break a run with no space in it — a
/// bare URL, a long path — at a chosen column. The characters of a Text token
/// all share the token's start `source_offset`, so a mid-token break used to
/// match nothing and silently do nothing, leaving the run to the renderer's own
/// wrap, which folds it to column zero with none of the caller's framing or
/// indentation. Splitting mirrors what `apply_conceal_ranges` already does for
/// ranges landing inside a token; each piece keeps the token's style, so
/// highlighting survives the break.
///
/// The continuation indent is `indent` columns wide. A break carrying a
/// [`SoftBreakRender::prefix`] draws that glyph run at the head of those
/// columns and pads the rest with Spaces; without one the whole indent is
/// Spaces, as it always was. The prefix never widens the row — the manager
/// grew `indent` to fit it at insert time — so the columns a measurement-only
/// caller computes from `indent` alone still match what gets drawn.
///
/// Tokens without source_offset (injected/virtual) pass through unchanged.
pub(crate) fn apply_soft_breaks(
    tokens: Vec<ViewTokenWire>,
    soft_breaks: &[SoftBreakRender],
) -> Vec<ViewTokenWire> {
    if soft_breaks.is_empty() {
        return tokens;
    }

    let mut output = Vec::with_capacity(tokens.len() + soft_breaks.len() * 2);
    let mut break_idx = 0;

    // Newline + the break's continuation indent: a prefix at its head when the
    // break carries one, spaces for the rest of the columns.
    fn push_break(output: &mut Vec<ViewTokenWire>, brk: &SoftBreakRender) {
        output.push(ViewTokenWire {
            source_offset: None,
            kind: ViewTokenWireKind::Newline,
            style: None,
        });
        let mut spaces = brk.indent as usize;
        if let Some((text, style)) = &brk.prefix {
            spaces = spaces.saturating_sub(str_width(text));
            output.push(ViewTokenWire {
                source_offset: None,
                kind: ViewTokenWireKind::Text(text.clone()),
                style: style.map(token_style_from_ratatui),
            });
        }
        for _ in 0..spaces {
            output.push(ViewTokenWire {
                source_offset: None,
                kind: ViewTokenWireKind::Space,
                style: None,
            });
        }
    }

    for token in tokens {
        let offset = match token.source_offset {
            Some(o) => o,
            None => {
                output.push(token);
                continue;
            }
        };

        while break_idx < soft_breaks.len() && soft_breaks[break_idx].position < offset {
            break_idx += 1;
        }

        // A Text token can span several break positions; walk its characters
        // and cut at each. The common case (no break inside) falls through the
        // loop and re-emits the token whole.
        if let ViewTokenWireKind::Text(str_tok) = &token.kind {
            let token_end = offset + str_tok.len();
            if soft_breaks[break_idx..]
                .first()
                .is_some_and(|b| b.position < token_end)
            {
                let mut seg = String::new();
                let mut seg_start = offset;
                let mut byte_idx = 0usize;
                for ch in str_tok.chars() {
                    let pos = offset + byte_idx;
                    if break_idx < soft_breaks.len() && soft_breaks[break_idx].position == pos {
                        if !seg.is_empty() {
                            output.push(ViewTokenWire {
                                source_offset: Some(seg_start),
                                kind: ViewTokenWireKind::Text(std::mem::take(&mut seg)),
                                style: token.style.clone(),
                            });
                        }
                        push_break(&mut output, &soft_breaks[break_idx]);
                        seg_start = pos;
                        break_idx += 1;
                    }
                    seg.push(ch);
                    byte_idx += ch.len_utf8();
                }
                if !seg.is_empty() {
                    output.push(ViewTokenWire {
                        source_offset: Some(seg_start),
                        kind: ViewTokenWireKind::Text(seg),
                        style: token.style.clone(),
                    });
                }
                continue;
            }
        }

        if break_idx < soft_breaks.len() && soft_breaks[break_idx].position == offset {
            let brk = &soft_breaks[break_idx];
            break_idx += 1;

            match &token.kind {
                // The space *is* the break: it is consumed by the row end.
                ViewTokenWireKind::Space => push_break(&mut output, brk),
                _ => {
                    push_break(&mut output, brk);
                    output.push(token);
                }
            }
        } else {
            output.push(token);
        }
    }

    output
}

/// How many times the join estimate is allowed to re-measure. Each pass grows
/// the window, which can bring more joins into view; two passes settle every
/// realistic document (a paragraph's joins are found by the first), and the
/// bound keeps a pathological one — a file that is one joined block — from
/// walking the buffer.
const JOIN_ESTIMATE_PASSES: usize = 2;

/// Grow the visible-line estimate by the line breaks the viewport's conceals
/// swallow.
///
/// A conceal whose range covers a newline renders the line below it as part of
/// the line above — the mechanism compose mode reflows a paragraph with. N
/// source lines then fill fewer than N rows, so a token build sized in source
/// lines stops above the viewport's bottom and the frame ends in EOF tildes
/// with the document still going. This counts the swallowed breaks and asks
/// for that many lines more, re-measuring because the extra lines can carry
/// joins of their own.
///
/// Returns `visible_count` unchanged for a non-composing split or an
/// unconcealed buffer: every other conceal in the editor stays inside one
/// line, so nothing else can pay for this query.
pub(crate) fn join_adjusted_visible_count(
    buffer: &crate::model::buffer::Buffer,
    conceals: &crate::view::conceal::ConcealManager,
    marker_list: &crate::model::marker::MarkerList,
    cursor_positions: &[usize],
    top_byte: usize,
    visible_count: usize,
    is_compose: bool,
) -> usize {
    if !is_compose || conceals.is_empty() || visible_count == 0 {
        return visible_count;
    }
    let start_line = buffer.get_line_number(top_byte);
    let mut total = visible_count;
    for _ in 0..JOIN_ESTIMATE_PASSES {
        let end_byte = buffer
            .line_start_offset(start_line + total)
            .unwrap_or_else(|| buffer.len());
        if end_byte <= top_byte {
            return total;
        }
        let swallowed: usize = conceals
            .query_viewport(top_byte, end_byte, marker_list, cursor_positions)
            .iter()
            .map(|(range, _)| {
                let from = buffer.get_line_number(range.start.max(top_byte));
                let to = buffer.get_line_number(range.end.min(end_byte));
                to.saturating_sub(from)
            })
            .sum();
        let grown = visible_count.saturating_add(swallowed);
        if grown <= total {
            return total;
        }
        total = grown;
    }
    total
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

/// Whether this visual row continues the source line above it rather than
/// starting one of its own.
///
/// Two different things cut a source line into rows, and neither ends it: the
/// wrap machine's `Break`, and a plugin *soft break* — which reaches the token
/// stream as an injected `Newline` (`source_offset: None`, see
/// `apply_soft_breaks`) and is how compose mode folds a table cell or a framed
/// code line. Only a newline that came from a source byte ends the line.
///
/// Injected newlines from virtual *lines* are not a case here: those are added
/// by `inject_virtual_lines` itself, downstream of the rows it reads.
fn continues_previous_line(line: &ViewLine) -> bool {
    matches!(
        line.line_start,
        LineStart::AfterBreak | LineStart::AfterInjectedNewline
    )
}

/// End byte of the logical source line whose first visual row is `idx` —
/// the end of its last continuation row, so a `LineAbove` anchored anywhere
/// in a wrapped line still resolves against the whole line.
fn logical_end(source_lines: &[ViewLine], idx: usize) -> Option<usize> {
    let mut end = None;
    for (offset, line) in source_lines[idx..].iter().enumerate() {
        if offset > 0 && !continues_previous_line(line) {
            break;
        }
        if let Some(b) = line.char_source_bytes.iter().rev().find_map(|m| *m) {
            end = Some(b + 1);
        }
    }
    end
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

    // markdown_compose's frame lines — the table border (`md-tb`) and the
    // spacer rows between list items (`md-ls`) — belong only to a Compose-mode
    // split. Virtual lines live on the buffer, so in a Source-mode split
    // sharing a buffer with a composing sibling they would otherwise draw a
    // frame around the raw source. Drop the compose-only ones here — mirroring
    // the `md-syntax` conceal gate in `view_data.rs` and the `md-emphasis`
    // overlay gate in `overlays.rs`. Every other virtual-line namespace (git
    // blame, diff, …) renders in both modes.
    if !is_compose {
        virtual_lines.retain(|(_, vt)| !is_compose_only_virtual_text(vt));
    }

    if virtual_lines.is_empty() {
        return source_lines;
    }

    let mut result = Vec::with_capacity(source_lines.len() + virtual_lines.len());

    // `source_lines` are *visual* rows: a wrapped source line arrives as a
    // first row plus one `AfterBreak` continuation per fold. A virtual line
    // belongs above or below the whole source line, not above or below the
    // fold its anchor byte happens to land in — so both passes run against
    // the logical line's byte range, and emit at its first / last visual row.
    //
    // Placing a `LineBelow` at the anchor's own row is what put compose
    // mode's `└─┴─┘` table border directly under the *first* visual row of a
    // wrapped final row, leaving the rest of that row's text below a frame
    // that had already closed.
    let logical_start_of: Vec<Option<usize>> = {
        let mut starts = Vec::with_capacity(source_lines.len());
        let mut current: Option<usize> = None;
        for line in &source_lines {
            let row_start = line.char_source_bytes.iter().find_map(|m| *m);
            if !continues_previous_line(line) {
                current = row_start;
            }
            // A continuation row inherits its line's start; a first row that
            // maps to no source byte (a virtual line already in the stream)
            // contributes none.
            starts.push(current.or(row_start));
        }
        starts
    };

    for (idx, source_line) in source_lines.iter().enumerate() {
        let logical_start = logical_start_of[idx];
        let line_end_byte = source_line
            .char_source_bytes
            .iter()
            .rev()
            .find_map(|m| *m)
            .map(|b| b + 1);
        let is_first_row = !continues_previous_line(source_line);
        let is_last_row = source_lines
            .get(idx + 1)
            .is_none_or(|next| !continues_previous_line(next));

        if is_first_row {
            if let (Some(start), Some(end)) = (logical_start, logical_end(&source_lines, idx)) {
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
        }

        result.push(source_line.clone());

        if is_last_row {
            if let (Some(start), Some(end)) = (logical_start, line_end_byte) {
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

/// One inline hint, resolved: everything the splice needs and no borrow of
/// editor state.
#[derive(Debug, Clone)]
pub struct InlineHint {
    /// Source byte the hint is anchored to.
    pub anchor: usize,
    pub text: String,
    pub position: VirtualTextPosition,
    /// How the underlying marker moves when text is inserted at `anchor`.
    ///
    /// The wrap index keeps its own copy of these anchors and shifts them
    /// itself (`IndexDecorations::shift_for_edit`); without the gravity it
    /// would have to assume one, and a snapshot that disagrees with the live
    /// marker makes the index lay out a line the renderer never draws.
    pub gravity: crate::view::virtual_text::MarkerGravity,
    /// `None` when the caller passed no theme — the scroll-math and index
    /// paths, where only the cell's *width* matters and nothing is drawn.
    pub style: Option<ViewTokenStyle>,
}

/// Resolve the inline hints anchored in `start..end`.
///
/// The state-dependent half of the splice, split out so the transform itself is
/// pure. `theme` is `Some` on the draw path (so hint colours resolve) and `None`
/// wherever the output is measured but never drawn.
///
/// `include_compose_only` is `false` only on the *draw* path of a Source-mode
/// split, where markdown_compose's code-block side rails must be dropped the
/// same way [`inject_virtual_lines`] drops its frame lines: the rails are
/// emitted whenever any split composes the buffer, and a source pane has to
/// show the code literally.
///
/// Every *measuring* caller passes `true`, rails included, because the row
/// counts they feed are not per split. Scroll math asks for its index under a
/// fixed `CacheViewMode::Source` label that says nothing about any split's
/// actual mode (`scrollbar_math::scroll_geometry`), so gating on it there takes
/// compose's own decorations out of the row count of a *composing* buffer and
/// the scrollbar stops short of the end of the document.
pub fn resolve_inline_hints(
    state: &EditorState,
    theme: Option<&Theme>,
    start: usize,
    end: usize,
    include_compose_only: bool,
) -> Vec<InlineHint> {
    state
        .virtual_texts
        .query_inline_in_range(&state.marker_list, start, end)
        .into_iter()
        .filter(|(_, vtext)| include_compose_only || !is_compose_only_virtual_text(vtext))
        .map(|(anchor, vtext)| InlineHint {
            anchor,
            text: vtext.text.clone(),
            position: vtext.position,
            gravity: vtext.gravity,
            style: theme.map(|t| token_style_from_ratatui(vtext.resolved_style(t))),
        })
        .collect()
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
/// Takes hints already resolved by [`resolve_inline_hints`] rather than
/// `&EditorState`, which is what lets the wrap index call it: the index builds
/// behind `&mut Buffer`, and a `&EditorState` alongside that does not borrow.
/// It also puts this transform on the same footing as the others in this
/// module — a pure function of tokens and decorations.
pub fn splice_inline_virtual_text(
    tokens: Vec<ViewTokenWire>,
    hints: &[InlineHint],
) -> Vec<ViewTokenWire> {
    if hints.is_empty() {
        return tokens;
    }

    // Group by anchor byte, preserving the resolver's (position, priority)
    // order. `before` stores the raw hint text — its leading-space padding
    // depends on whether the anchor cell is a newline, decided while
    // walking the token stream below.
    let mut before: HashMap<usize, Vec<(String, Option<ViewTokenStyle>)>> = HashMap::new();
    let mut after: HashMap<usize, Vec<InlineHintCell>> = HashMap::new();
    for hint in hints {
        match hint.position {
            VirtualTextPosition::BeforeChar => {
                before
                    .entry(hint.anchor)
                    .or_default()
                    .push((hint.text.clone(), hint.style.clone()));
            }
            VirtualTextPosition::AfterChar => {
                after.entry(hint.anchor).or_default().push(InlineHintCell {
                    text: format!(" {}", hint.text),
                    style: hint.style.clone(),
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
    // Whether nothing of this line's own source has been emitted yet. Only
    // used to recognise a newline that *is* the whole line — an empty line —
    // whose hints have no neighbouring text to be separated from.
    let mut at_line_start = true;
    for token in tokens {
        let src = token.source_offset;
        let is_newline = matches!(token.kind, ViewTokenWireKind::Newline);
        let line_start_cell = at_line_start;
        at_line_start = is_newline;
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
                // newline is an end-of-line hint and gets a leading space to
                // hold it off the text it trails.
                //
                // Unless the newline *is* the line: on an empty line there is
                // no text on either side, so the padding separates the hint
                // from nothing and merely indents it. Decorations that draw a
                // column — markdown compose's code-block side rails — then sit
                // one column inside their own frame on exactly the blank rows.
                let anchor_is_newline = matches!(kind, ViewTokenWireKind::Newline);
                let empty_line = anchor_is_newline && line_start_cell;
                if let Some(hints) = before.get(&anchor) {
                    for (text, style) in hints {
                        let padded = match (anchor_is_newline, empty_line) {
                            (_, true) => text.clone(),
                            (true, false) => format!(" {text} "),
                            (false, _) => format!("{text} "),
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

#[cfg(test)]
mod soft_break_tests {
    use super::*;

    /// One `Text` token per source byte, the shape `build_base_tokens`
    /// produces for a plain ASCII line.
    fn chars(text: &str) -> Vec<ViewTokenWire> {
        text.char_indices()
            .map(|(i, c)| ViewTokenWire {
                source_offset: Some(i),
                kind: if c == ' ' {
                    ViewTokenWireKind::Space
                } else {
                    ViewTokenWireKind::Text(c.to_string())
                },
                style: None,
            })
            .collect()
    }

    /// The columns emitted between the injected Newline and the next source
    /// token — the continuation row's indent, as text.
    fn continuation_indent(tokens: &[ViewTokenWire]) -> String {
        let newline = tokens
            .iter()
            .position(|t| matches!(t.kind, ViewTokenWireKind::Newline))
            .expect("no soft break was injected");
        tokens[newline + 1..]
            .iter()
            .take_while(|t| t.source_offset.is_none())
            .map(|t| match &t.kind {
                ViewTokenWireKind::Text(s) => s.clone(),
                ViewTokenWireKind::Space => " ".to_string(),
                _ => String::new(),
            })
            .collect()
    }

    #[test]
    fn a_plain_break_indents_with_spaces() {
        let out = apply_soft_breaks(chars("one two"), &[SoftBreakRender::plain(3, 4)]);
        assert_eq!(continuation_indent(&out), "    ");
    }

    /// The prefix is drawn *inside* the indent, not in addition to it: the
    /// continuation row is still `indent` columns wide, which is what keeps
    /// the quoted text in the column the first row put it in.
    #[test]
    fn a_prefix_is_drawn_inside_the_indent() {
        let brk = SoftBreakRender {
            position: 3,
            indent: 4,
            prefix: Some(("▌".to_string(), None)),
        };
        let out = apply_soft_breaks(chars("one two"), &[brk]);
        assert_eq!(continuation_indent(&out), "▌   ");
    }

    /// A prefix exactly filling the indent leaves no padding behind it.
    #[test]
    fn a_prefix_filling_the_indent_emits_no_padding() {
        let brk = SoftBreakRender {
            position: 3,
            indent: 2,
            prefix: Some(("▌ ".to_string(), None)),
        };
        let out = apply_soft_breaks(chars("one two"), &[brk]);
        assert_eq!(continuation_indent(&out), "▌ ");
    }

    /// A break landing on a non-Space token keeps that token, unlike the
    /// Space case where the break consumes it.
    #[test]
    fn a_break_on_a_non_space_token_keeps_the_token() {
        let brk = SoftBreakRender {
            position: 4,
            indent: 2,
            prefix: Some(("▌ ".to_string(), None)),
        };
        let out = apply_soft_breaks(chars("one two"), &[brk]);
        assert_eq!(continuation_indent(&out), "▌ ");
        let tail: String = out
            .iter()
            .filter_map(|t| match (&t.kind, t.source_offset) {
                (ViewTokenWireKind::Text(s), Some(_)) => Some(s.as_str()),
                _ => None,
            })
            .collect();
        assert_eq!(tail, "onetwo", "no source character may be dropped");
    }
}
