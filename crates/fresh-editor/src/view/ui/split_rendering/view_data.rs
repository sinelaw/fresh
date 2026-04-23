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
    apply_conceal_ranges, apply_soft_breaks, apply_wrapping_transform, inject_virtual_lines,
};
use super::MAX_SAFE_LINE_WIDTH;
use crate::state::{EditorState, ViewMode};
use crate::view::folding::FoldManager;
use crate::view::theme::Theme;
use crate::view::ui::view_pipeline::{ViewLine, ViewLineIterator};
use crate::view::viewport::Viewport;
use fresh_core::api::ViewTransformPayload;

/// Processed view data containing display lines from the view pipeline.
pub(super) struct ViewData {
    /// Display lines with all token information preserved.
    pub lines: Vec<ViewLine>,
}

/// Run the entire view pipeline for the current viewport:
/// base tokens → (optional plugin transform) → soft breaks → conceal →
/// wrapping → [`ViewLine`] conversion → virtual lines → folding.
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
) -> ViewData {
    let adjusted_visible_count = fold_adjusted_visible_count(
        &state.buffer,
        &state.marker_list,
        folds,
        viewport.top_byte,
        visible_count,
    );

    let is_binary = state.buffer.is_binary();
    let line_ending = state.buffer.line_ending();

    // Compute fold skip set once — reused by base token build (to avoid
    // reading/tokenising hidden ranges) and by ViewLineIterator (defence in
    // depth for any tokens produced by plugin view transforms).
    let fold_skip = fold_skip_set(&state.buffer, &state.marker_list, folds);

    // Build base token stream from source, skipping any source-byte range
    // that falls inside a collapsed fold.
    let base_tokens = build_base_tokens(
        &mut state.buffer,
        viewport.top_byte,
        estimated_line_length,
        adjusted_visible_count,
        is_binary,
        line_ending,
        &fold_skip,
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
            .unwrap_or(viewport.top_byte)
            + 1;
        let soft_breaks =
            state
                .soft_breaks
                .query_viewport(viewport.top_byte, viewport_end, &state.marker_list);
        if !soft_breaks.is_empty() {
            tokens = apply_soft_breaks(tokens, &soft_breaks);
        }
    }

    // Apply conceal ranges - filter/replace tokens that fall within concealed
    // byte ranges. Only apply in Compose mode; Source mode shows the raw
    // markdown syntax.
    if is_compose && !state.conceals.is_empty() {
        let viewport_end = tokens
            .iter()
            .filter_map(|t| t.source_offset)
            .next_back()
            .unwrap_or(viewport.top_byte)
            + 1;
        let conceal_ranges =
            state
                .conceals
                .query_viewport(viewport.top_byte, viewport_end, &state.marker_list);
        if !conceal_ranges.is_empty() {
            tokens = apply_conceal_ranges(tokens, &conceal_ranges);
        }
    }

    // Apply wrapping transform - always enabled for safety, but with
    // different thresholds. When line_wrap is on: wrap at viewport width (or
    // wrap_column if set). When line_wrap is off: wrap at
    // MAX_SAFE_LINE_WIDTH to prevent memory exhaustion from extremely long
    // lines.
    //
    // When wrapping is on, reserve the last content column so the
    // end-of-line cursor never lands on top of the vertical scrollbar.
    // The cursor sits one column past the last rendered character, so
    // a row that fills `content_width` exactly would place the EOL
    // cursor on the scrollbar track (which is drawn in the column
    // immediately to the right of the content area).  `saturating_sub`
    // keeps this safe at very small widths where the guard inside
    // `apply_wrapping_transform` will short-circuit anyway.
    let effective_width = if line_wrap_enabled {
        let base = if let Some(col) = viewport.wrap_column {
            col.min(content_width)
        } else {
            content_width
        };
        base.saturating_sub(1).max(1)
    } else {
        MAX_SAFE_LINE_WIDTH
    };
    let hanging_indent = line_wrap_enabled && viewport.wrap_indent;
    tokens = apply_wrapping_transform(tokens, effective_width, gutter_width, hanging_indent);

    // The renderer-side writeback to `LineWrapCache` is temporarily
    // disabled as part of the cache-value-type refactor (row count ->
    // Arc<Vec<ViewLine>>).  Phase 3 of the plan re-enables it by
    // slicing the full-frame ViewLines per logical line and storing
    // those Arc<Vec<ViewLine>>s.
    //
    // While disabled, the cache is filled only by miss-handler
    // computations in scroll-math (and the viewport-local count
    // cache).  No correctness impact — just a temporary perf
    // regression for first-render cache warmth.
    let _ = (viewport, view_mode, has_view_transform, line_wrap_enabled);

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
    let source_lines: Vec<ViewLine> = ViewLineIterator::new(
        &tokens,
        is_binary,
        ansi_aware,
        state.buffer_settings.tab_size,
        at_buffer_end,
    )
    .with_fold_skip(&fold_skip)
    .collect();

    // Inject virtual lines (LineAbove/LineBelow) from VirtualTextManager.
    let lines = inject_virtual_lines(source_lines, state, theme);
    let placeholder_style = fold_placeholder_style(theme);
    let lines = apply_folding(
        lines,
        &state.buffer,
        &state.marker_list,
        folds,
        &placeholder_style,
    );

    ViewData { lines }
}
