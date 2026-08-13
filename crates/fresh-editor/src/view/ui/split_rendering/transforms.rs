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
use crate::view::ui::view_pipeline::{LineStart, ViewLine};
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
/// Walks tokens with a sorted break list `[(position, indent)]`. A break at a
/// position:
/// - on a Space token: replaces it with Newline + indent Spaces
/// - anywhere else: inserts Newline + indent Spaces before that byte, splitting
///   the Text token it lands inside if it is not already at a token boundary
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

    // Newline + `indent` spaces, the row break itself.
    fn push_break(output: &mut Vec<ViewTokenWire>, indent: u16) {
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

        // A Text token can span several break positions; walk its characters
        // and cut at each. The common case (no break inside) falls through the
        // loop and re-emits the token whole.
        if let ViewTokenWireKind::Text(s) = &token.kind {
            let token_end = offset + s.len();
            if soft_breaks[break_idx..]
                .first()
                .is_some_and(|(pos, _)| *pos < token_end)
            {
                let mut seg = String::new();
                let mut seg_start = offset;
                let mut byte_idx = 0usize;
                for ch in s.chars() {
                    let pos = offset + byte_idx;
                    if break_idx < soft_breaks.len() && soft_breaks[break_idx].0 == pos {
                        if !seg.is_empty() {
                            output.push(ViewTokenWire {
                                source_offset: Some(seg_start),
                                kind: ViewTokenWireKind::Text(std::mem::take(&mut seg)),
                                style: token.style.clone(),
                            });
                        }
                        push_break(&mut output, soft_breaks[break_idx].1);
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

        if break_idx < soft_breaks.len() && soft_breaks[break_idx].0 == offset {
            let indent = soft_breaks[break_idx].1;
            break_idx += 1;

            match &token.kind {
                // The space *is* the break: it is consumed by the row end.
                ViewTokenWireKind::Space => push_break(&mut output, indent),
                _ => {
                    push_break(&mut output, indent);
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
    /// `None` when the caller passed no theme — the scroll-math and index
    /// paths, where only the cell's *width* matters and nothing is drawn.
    pub style: Option<ViewTokenStyle>,
}

/// Resolve the inline hints anchored in `start..end`.
///
/// The state-dependent half of the splice, split out so the transform itself is
/// pure. `theme` is `Some` on the draw path (so hint colours resolve) and `None`
/// wherever the output is measured but never drawn.
pub fn resolve_inline_hints(
    state: &EditorState,
    theme: Option<&Theme>,
    start: usize,
    end: usize,
) -> Vec<InlineHint> {
    state
        .virtual_texts
        .query_inline_in_range(&state.marker_list, start, end)
        .into_iter()
        .map(|(anchor, vtext)| InlineHint {
            anchor,
            text: vtext.text.clone(),
            position: vtext.position,
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
