//! Orchestration of the view pipeline: build tokens, transform them, and
//! produce a `Vec<ViewLine>` ready for rendering.
//!
//! This module combines the per-pass helpers from `base_tokens`,
//! `transforms`, `folding`, and `style` — its only dependencies are the
//! (also self-contained) sibling modules and a few editor state types.

use super::base_tokens::build_base_tokens;
use super::folding::{apply_folding, fold_adjusted_visible_count, fold_skip_set};
use super::style::fold_placeholder_style;
use super::transforms::{
    apply_conceal_ranges, apply_grid_wrapping_transform, apply_soft_breaks,
    apply_wrapping_transform_from, inject_virtual_lines, resolve_inline_hints,
    splice_inline_virtual_text,
};
use super::MAX_SAFE_LINE_WIDTH;
use crate::state::{EditorState, ViewMode};
use crate::view::folding::FoldManager;
use crate::view::theme::Theme;
use crate::view::ui::view_pipeline::{LineStart, ViewLine, ViewLineIterator};
use crate::view::viewport::Viewport;
use fresh_core::api::ViewTransformPayload;

/// markdown_compose's conceal namespace (`md-syntax`): the cell-separator /
/// emphasis-marker conceals that turn raw `|`/`**` into the composed table. Only
/// valid in a Compose-mode split; suppressed in Source mode (see the conceal
/// pass below). Mirrors the `md-emphasis` overlay gate in `overlays.rs`.
fn md_syntax_namespace() -> fresh_core::overlay::OverlayNamespace {
    fresh_core::overlay::OverlayNamespace::from_string("md-syntax".to_string())
}

/// Processed view data containing display lines from the view pipeline.
pub(super) struct ViewData {
    /// Display lines with all token information preserved.
    pub lines: Vec<ViewLine>,
    /// Index in `lines` of the viewport's first *drawn* row.
    ///
    /// Rows before it were built only because the wrap could not be resumed
    /// exactly at the anchor — a row opening with injected content the carry
    /// cannot reconstruct forces a walk back to the nearest resumable row (see
    /// `WrapIndex::resumable_row_at_or_before`). On a plain long line this is
    /// zero and the build starts exactly at the viewport.
    ///
    /// Before the build was anchored this was always `top_view_line_offset`,
    /// because `lines` began at the logical line's first row and the renderer
    /// discarded everything above the viewport — the O(scroll-depth) cost this
    /// replaces.
    pub first_drawn: usize,
}

/// Where the build starts, when the caller could resolve a resumable row.
///
/// Without one the build begins at `viewport.top_byte()` — the logical line's
/// start — and every row above the viewport is built and thrown away.
#[derive(Debug, Clone, Copy)]
pub(super) struct BuildAnchor {
    /// Byte of the first row to build; certified to be a visual-row start.
    pub byte: usize,
    /// Wrap state to resume that row with.
    pub carry: crate::view::wrap_machine::RowCarry,
    /// Rows built before the viewport's first drawn row.
    pub skip: usize,
}

/// Width at which one visual row wraps.
///
/// Wrapping is always applied for safety, but with different thresholds. When
/// line_wrap is on: wrap at viewport width (or `wrap_column` if set). When
/// line_wrap is off: wrap at `MAX_SAFE_LINE_WIDTH` to prevent memory
/// exhaustion from extremely long lines.
///
/// When wrapping is on, reserve the last content column so the end-of-line
/// cursor never lands on top of the vertical scrollbar. The cursor sits one
/// column past the last rendered character, so a row that fills
/// `content_width` exactly would place the EOL cursor on the scrollbar track
/// (which is drawn in the column immediately to the right of the content
/// area). `saturating_sub` keeps this safe at very small widths where the
/// guard inside `apply_wrapping_transform` will short-circuit anyway.
fn effective_wrap_width(
    viewport: &Viewport,
    line_wrap_enabled: bool,
    content_width: usize,
) -> usize {
    if !line_wrap_enabled {
        return MAX_SAFE_LINE_WIDTH;
    }
    if viewport.grid_wrap {
        // Terminal-grid wrap (fresh#2649): wrap at exactly the capture-time
        // PTY column count. No EOL-cursor column is reserved and no clamp to
        // the content width — the grid is one column wider than the
        // scroll-back content area (the live view reclaims the scrollbar
        // column), and clamping or reserving would re-wrap every full-width
        // grid row one cell early, reflowing the whole view on entry. Full
        // rows render with their last cell under the scrollbar, exactly like
        // the non-wrapped exit frame always has.
        return viewport.grid_cols();
    }
    let base = if let Some(col) = viewport.wrap_column {
        col.min(content_width)
    } else {
        content_width
    };
    base.saturating_sub(1).max(1)
}

/// Character budget for [`build_base_tokens`], or `None` to bound the read by
/// source lines alone.
///
/// Only meaningful under soft wrap. There, a logical line occupies
/// `ceil(width / effective_width)` rows, so the rows the renderer can possibly
/// draw are covered by about `rows × effective_width` characters — a few
/// thousand, against the ~540,000 an unbudgeted read pulls in on a
/// single-line file (`lines_seen` advances only once per `MAX_SAFE_LINE_WIDTH`
/// characters, so the line bound never fires inside one long line).
///
/// With wrapping off there is no budget to give: a long line is chopped into
/// rows of `MAX_SAFE_LINE_WIDTH` columns each, so covering the viewport's rows
/// genuinely costs `rows × MAX_SAFE_LINE_WIDTH` characters.
fn base_char_budget(
    line_wrap_enabled: bool,
    effective_width: usize,
    adjusted_visible_count: usize,
    cursor_positions: &[usize],
    rows_before_window: usize,
    start_byte: usize,
) -> Option<usize> {
    if !line_wrap_enabled {
        return None;
    }

    // Rows the build must cover: the window, plus whatever precedes it. With an
    // anchor that prefix is the walk-back to a resumable row — usually zero.
    // Without one it is every row of the logical line above the viewport.
    let rows = rows_before_window
        .saturating_add(adjusted_visible_count)
        .saturating_add(4);

    // Characters per row and columns per row are not the same number: a
    // double-width glyph fills two columns, a combining mark or ZWJ fills
    // none, and a tab fills up to `tab_size`. Only the zero-width direction
    // can make a row consume *more* characters than columns, so pad
    // generously rather than trying to be exact — over-reading a little is
    // free next to the 50× the budget saves.
    let mut budget = rows
        .saturating_mul(effective_width.max(1))
        .saturating_mul(2)
        .saturating_add(1024);

    // The scroll math (`ensure_visible_in_layout`) locates the cursor by
    // searching the rows this build produces, so the build must reach the
    // cursor even when it sits past the budgeted window — otherwise a cursor
    // moved far down a long wrapped line would never scroll into view.
    if let Some(furthest) = cursor_positions
        .iter()
        .copied()
        .filter(|&pos| pos >= start_byte)
        .max()
    {
        budget = budget.max(
            (furthest - start_byte)
                .saturating_add(effective_width.saturating_mul(2))
                .saturating_add(1024),
        );
    }

    Some(budget)
}

/// Run the entire view pipeline for the current viewport:
/// base tokens → (optional plugin transform) → soft breaks → conceal →
/// wrapping → [`ViewLine`] conversion → virtual lines → folding.
///
/// `cursor_positions` are the rendering split's cursor byte positions,
/// used to evaluate cursor-dependent conceal / soft-break activation
/// rules (e.g. markdown_compose revealing the markup under the cursor).
/// Pass `&[]` for cursor-less consumers (previews) — they render the
/// canonical "no cursor anywhere" form.
#[allow(clippy::too_many_arguments)]
pub(super) fn build_view_data(
    state: &mut EditorState,
    viewport: &Viewport,
    view_transform: Option<ViewTransformPayload>,
    estimated_line_length: usize,
    visible_count: usize,
    line_wrap_enabled: bool,
    content_width: usize,
    gutter_width: usize,
    view_mode: &ViewMode,
    folds: &FoldManager,
    theme: &Theme,
    cursor_positions: &[usize],
    anchor: Option<BuildAnchor>,
) -> ViewData {
    let adjusted_visible_count = fold_adjusted_visible_count(
        &state.buffer,
        &state.marker_list,
        folds,
        viewport.top_byte(),
        visible_count,
    );

    let is_binary = state.buffer.is_binary();
    let line_ending = state.buffer.line_ending();

    // Compute fold skip set once — reused by base token build (to avoid
    // reading/tokenising hidden ranges) and by ViewLineIterator (defence in
    // depth for any tokens produced by plugin view transforms).
    let fold_skip = fold_skip_set(&state.buffer, &state.marker_list, folds);

    // Width one visual row wraps at. Computed here — before the token build
    // rather than just before `apply_wrapping_transform` — because it also
    // sizes the token build's character budget (see `base_char_budget`).
    let effective_width = effective_wrap_width(viewport, line_wrap_enabled, content_width);

    // Build base token stream from source, skipping any source-byte range
    // that falls inside a collapsed fold.
    // With an anchor the build starts at the viewport's own row rather than at
    // the logical line's start, so nothing above the window is built at all —
    // and the budget needs to cover only the window, not the prefix leading to
    // it. Without one, both fall back to the line-start behaviour.
    let (start_byte, resume_carry, rows_before_window) = match anchor {
        Some(a) => (a.byte, Some(a.carry), a.skip),
        None => (viewport.top_byte(), None, viewport.top_view_line_offset()),
    };
    let base_tokens = build_base_tokens(
        &mut state.buffer,
        start_byte,
        estimated_line_length,
        adjusted_visible_count,
        is_binary,
        line_ending,
        &fold_skip,
        base_char_budget(
            line_wrap_enabled,
            effective_width,
            adjusted_visible_count,
            cursor_positions,
            rows_before_window,
            start_byte,
        ),
        anchor.is_some(),
    );

    // Use plugin transform if available, otherwise use base tokens
    let has_view_transform = view_transform.is_some();
    let mut tokens = view_transform.map(|vt| vt.tokens).unwrap_or(base_tokens);

    // Apply soft breaks — marker-based line wrapping that survives edits
    // without flicker. Only apply in Compose mode; Source mode shows the raw
    // unwrapped text.
    let is_compose = matches!(view_mode, ViewMode::PageView);
    if is_compose && !state.soft_breaks.is_empty() {
        let viewport_end = tokens
            .iter()
            .filter_map(|t| t.source_offset)
            .next_back()
            .unwrap_or(viewport.top_byte())
            + 1;
        let soft_breaks = state.soft_breaks.query_viewport(
            viewport.top_byte(),
            viewport_end,
            &state.marker_list,
            cursor_positions,
        );
        if !soft_breaks.is_empty() {
            tokens = apply_soft_breaks(tokens, &soft_breaks);
        }
    }

    // Apply conceal ranges — filter or replace tokens that fall within
    // concealed byte ranges. A blanket `is_compose` gate used to live here but
    // was removed because other plugins (flash) legitimately conceal in source
    // mode. Conceals are buffer-global, though, so markdown_compose's compose
    // cell conceals (`md-syntax`) would otherwise render in a Source-mode split
    // whenever a *sibling* split composes the same buffer (the plugin emits
    // them whenever `isComposingInAnySplit`). So suppress only that compose-only
    // namespace in Source mode — mirroring the `md-emphasis` overlay gate in
    // `overlays.rs` — while every other namespace (flash, etc.) still renders.
    if !state.conceals.is_empty() {
        let viewport_end = tokens
            .iter()
            .filter_map(|t| t.source_offset)
            .next_back()
            .unwrap_or(viewport.top_byte())
            + 1;
        let exclude_ns = (!is_compose).then(md_syntax_namespace);
        let conceal_ranges = state.conceals.query_viewport_excluding(
            viewport.top_byte(),
            viewport_end,
            &state.marker_list,
            exclude_ns.as_ref(),
            cursor_positions,
        );
        if !conceal_ranges.is_empty() {
            tokens = apply_conceal_ranges(tokens, &conceal_ranges);
        }
    }

    // Wrapping is applied below at `effective_width` (computed before the
    // token build, above).
    let hanging_indent = line_wrap_enabled && viewport.wrap_indent && !viewport.grid_wrap;

    // Splice inline virtual text (inlay hints) into the stream BEFORE
    // wrapping so its display width participates in wrap boundaries, the
    // per-character visual-column map, and horizontal scrolling. Done here
    // (not at draw time) so wrapping, ViewLine, cursor, and scroll all see
    // one canonical cell layout. `theme` is passed so hint colours resolve.
    if !state.virtual_texts.is_empty() {
        // Exclusive end of the byte range covered by these tokens. Each
        // token's `source_offset` is its START, so we must add the token's
        // byte length — otherwise a hint anchored inside the last (often
        // coalesced) token would fall outside the query range and never be
        // spliced.
        let viewport_end = tokens
            .iter()
            .filter_map(|t| {
                let start = t.source_offset?;
                let len = match &t.kind {
                    fresh_core::api::ViewTokenWireKind::Text(s) => s.len(),
                    _ => 1,
                };
                Some(start + len)
            })
            .max()
            .unwrap_or(viewport.top_byte());
        tokens = splice_inline_virtual_text(
            tokens,
            &resolve_inline_hints(state, Some(theme), viewport.top_byte(), viewport_end),
        );
    }

    tokens = if line_wrap_enabled && viewport.grid_wrap {
        // Exact-column grid wrap — must stay row-for-row identical to the
        // scroll math's `for_each_grid_row_start` (fresh#2649 symptom 2).
        apply_grid_wrapping_transform(tokens, effective_width)
    } else {
        apply_wrapping_transform_from(
            tokens,
            effective_width,
            gutter_width,
            hanging_indent,
            // Resuming at the anchor's carry is what makes a mid-line start
            // produce the same rows the line-start build would have.
            resume_carry,
        )
    };

    // Convert tokens to display lines using the view pipeline.
    let is_binary = state.buffer.is_binary();
    let ansi_aware = !is_binary;
    let at_buffer_end = if has_view_transform {
        // View transforms supply their own token streams; the trailing
        // empty line logic doesn't apply to them.
        false
    } else {
        let max_source_offset = tokens
            .iter()
            .filter_map(|t| t.source_offset)
            .max()
            .unwrap_or(0);
        max_source_offset + 2 >= state.buffer.len()
    };
    // Skip folded source ranges at the iterator level. Most folded content
    // is already absent from `tokens` (pre-skipped in `build_base_tokens`);
    // this handles plugin view transforms whose token stream predates the
    // skip.
    // `Beginning` claims the stream starts at a logical line, which the gutter
    // reads as "print this line's number". An anchored build starts at the
    // viewport's own row instead, and on a wrapped line that row is a
    // continuation — the anchor's carry is what knows the difference.
    let first_line_start = match anchor {
        Some(a) if a.carry.on_continuation => LineStart::AfterBreak,
        _ => LineStart::Beginning,
    };
    let source_lines: Vec<ViewLine> = ViewLineIterator::new(
        &tokens,
        is_binary,
        ansi_aware,
        state.buffer_settings.tab_size,
        at_buffer_end,
    )
    .starting_at(first_line_start)
    .with_fold_skip(&fold_skip)
    .collect();

    // Inject virtual lines (LineAbove/LineBelow) from VirtualTextManager.
    // When soft-wrap is enabled, pass the same per-row content width that
    // `apply_wrapping_transform` uses for source lines (effective width
    // less the gutter) so long virtual-line text wraps consistently
    // instead of being truncated past the viewport edge.
    let virtual_line_wrap_width = if line_wrap_enabled {
        let avail = effective_width.saturating_sub(gutter_width);
        // Mirror the `available_width < 2` bail-out in
        // `apply_wrapping_transform`: at degenerate widths (a tiny
        // `wrap_column`, a narrow split pane, or an unusually wide gutter)
        // source lines render unwrapped, so virtual/diff lines must too.
        // Clamping up to `1` here instead would feed `wrap_str_to_width`
        // a width of 1 and split every grapheme onto its own row — the
        // single-letter wrapping of diff hunks reported in #2177.
        (avail >= 2).then_some(avail)
    } else {
        None
    };
    let lines = inject_virtual_lines(
        source_lines,
        state,
        theme,
        virtual_line_wrap_width,
        is_compose,
    );
    let placeholder_style = fold_placeholder_style(theme);
    let lines = apply_folding(
        lines,
        &state.buffer,
        &state.marker_list,
        folds,
        &placeholder_style,
    );

    ViewData {
        lines,
        first_drawn: rows_before_window,
    }
}
