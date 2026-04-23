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

    // Writeback to the line-wrap cache.
    //
    // Walk the wrapped token stream and, for each logical line that
    // started in this render's visible window, store its visual-row
    // count under the LineWrapKey matching what scroll math will query.
    // This populates the cache from the renderer's side of the pipeline
    // so that subsequent scroll-math queries for the same lines are
    // O(1) cache hits instead of re-running `apply_wrapping_transform`.
    //
    // Skipped when:
    //   - A plugin view_transform is active (its token stream doesn't
    //     come from raw line text via `build_base_tokens`, so the
    //     scroll-math miss handler cannot reproduce it from a one-
    //     line input — cache entries would mismatch).
    //   - Line wrap is off (1 row per logical line is trivial; no
    //     benefit from caching).
    if !has_view_transform && line_wrap_enabled {
        use crate::view::line_wrap_cache::{pipeline_inputs_version, CacheViewMode, LineWrapKey};
        use fresh_core::api::ViewTokenWireKind;

        // Scroll math uses `CacheViewMode::Source` as its writer
        // convention because it runs without access to the view mode.
        // The renderer writes under the actual view mode; both coexist
        // in the cache keyed by the `view_mode` field. To make the
        // renderer's entries visible to scroll-math reads, we also
        // write a Source-keyed twin entry. The two entries have the
        // same value (compute once); this just makes both lookup
        // shapes hit.
        let cache_view_mode = if matches!(view_mode, ViewMode::PageView) {
            CacheViewMode::Compose
        } else {
            CacheViewMode::Source
        };
        let pipeline_inputs_ver = pipeline_inputs_version(
            state.buffer.version(),
            state.soft_breaks.version(),
            state.conceals.version(),
        );
        let make_key = |line_start: usize, mode: CacheViewMode| LineWrapKey {
            pipeline_inputs_version: pipeline_inputs_ver,
            view_mode: mode,
            line_start,
            effective_width: effective_width as u32,
            gutter_width: gutter_width as u16,
            wrap_column: viewport.wrap_column.map(|c| c as u32),
            hanging_indent,
            line_wrap_enabled: true,
        };

        // Walk tokens, accumulating non-empty-row counts between
        // Newlines. A token with `source_offset` anchors the current
        // logical line to a `line_start` byte. When we close a line
        // (via Newline), we write the count.
        let mut current_line_start: Option<usize> = None;
        let mut rows_in_line: u32 = 0;
        let mut row_has_content = false;
        for t in &tokens {
            if current_line_start.is_none() {
                if let Some(off) = t.source_offset {
                    // The renderer's tokens can include injected tokens
                    // without source_offset (indents, etc.). Anchor on
                    // the first real source position seen in this line.
                    current_line_start = Some(off);
                }
            }
            match &t.kind {
                ViewTokenWireKind::Newline => {
                    if let Some(line_start) = current_line_start {
                        if row_has_content {
                            rows_in_line += 1;
                        }
                        let count = rows_in_line.max(1);
                        state
                            .line_wrap_cache
                            .put(make_key(line_start, cache_view_mode), count);
                        state
                            .line_wrap_cache
                            .put(make_key(line_start, CacheViewMode::Source), count);
                    }
                    current_line_start = None;
                    rows_in_line = 0;
                    row_has_content = false;
                }
                ViewTokenWireKind::Break => {
                    if row_has_content {
                        rows_in_line += 1;
                    }
                    row_has_content = false;
                }
                ViewTokenWireKind::Text(s) => {
                    if !s.is_empty() {
                        row_has_content = true;
                    }
                }
                ViewTokenWireKind::Space | ViewTokenWireKind::BinaryByte(_) => {
                    row_has_content = true;
                }
            }
        }
        // Tail line with no trailing Newline (last line of the buffer).
        if let Some(line_start) = current_line_start {
            if row_has_content {
                rows_in_line += 1;
            }
            let count = rows_in_line.max(1);
            state
                .line_wrap_cache
                .put(make_key(line_start, cache_view_mode), count);
            state
                .line_wrap_cache
                .put(make_key(line_start, CacheViewMode::Source), count);
        }
    }

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
