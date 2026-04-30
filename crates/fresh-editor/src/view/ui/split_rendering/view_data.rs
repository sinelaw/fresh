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

    // Apply conceal ranges — filter or replace tokens that fall
    // within concealed byte ranges.  This used to be gated on
    // `is_compose` so markdown source mode would always show raw
    // syntax, but the gate was redundant: every plugin that adds
    // source-mode conceals already self-checks the buffer's view
    // mode (see e.g. `markdown_compose.ts`'s `isComposing`).  Other
    // plugins (flash) legitimately want overlay-style cell
    // substitution in source mode and were broken by the gate —
    // their `addConceal` calls landed in state but never rendered.
    if !state.conceals.is_empty() {
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

    // Writeback to the line-wrap cache.
    //
    // We have the full pipeline's output (`source_lines`) for the
    // visible window.  Slice it by logical line and store each slice
    // as an `Arc<Vec<ViewLine>>` under the key the scroll-math /
    // cursor-nav readers will query.  This means subsequent queries
    // for a line the renderer just visited are O(1) cache hits.
    //
    // Skipped when:
    //   - A plugin view_transform is active.  Its token stream doesn't
    //     come from raw line text via `build_base_tokens`, so the miss
    //     handler cannot reproduce it from a one-line input — cached
    //     entries would mismatch a cache-miss recompute.
    //   - Line wrap is off.  Every logical line is one visual row;
    //     caching the trivial answer provides no benefit.
    //   - Folds or virtual-text injection are active.  Those
    //     post-processing steps run AFTER this writeback and can add
    //     lines / reshape rows that a per-line miss-handler recompute
    //     wouldn't see — keep the cache to pre-fold, pre-virtual-text
    //     reality so the two writers agree.  (The renderer still uses
    //     the folded / injected output for drawing; the cache just
    //     reflects what `compute_line_layout` would produce for the
    //     same line.)
    if !has_view_transform
        && line_wrap_enabled
        && fold_skip.is_empty()
        && state.virtual_texts.is_empty()
    {
        use crate::view::line_wrap_cache::{pipeline_inputs_version, CacheViewMode, LineWrapKey};
        use crate::view::ui::view_pipeline::LineStart;
        use std::sync::Arc;

        let cache_view_mode = if matches!(view_mode, ViewMode::PageView) {
            CacheViewMode::Compose
        } else {
            CacheViewMode::Source
        };
        let pipeline_inputs_ver = pipeline_inputs_version(
            state.buffer.version(),
            state.soft_breaks.version(),
            state.conceals.version(),
            state.virtual_texts.version(),
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

        // Walk `source_lines` grouping consecutive rows that belong to
        // the same logical line.  A new logical line begins when we
        // see `LineStart::Beginning` (only on row 0 of the window) or
        // `LineStart::AfterSourceNewline`.  `AfterBreak` rows are
        // wrap continuations — same logical line.
        // `AfterInjectedNewline` is for plugin-injected breaks; we
        // conservatively don't publish those runs (their line_start
        // byte is ambiguous).
        //
        // The first row's `source_start_byte` anchors the group to a
        // buffer byte; if it's `None` (e.g. injected content), skip
        // the whole group.
        let mut i = 0;
        while i < source_lines.len() {
            let first = &source_lines[i];
            let is_group_start = match first.line_start {
                LineStart::Beginning | LineStart::AfterSourceNewline => true,
                LineStart::AfterInjectedNewline | LineStart::AfterBreak => false,
            };
            if !is_group_start {
                i += 1;
                continue;
            }
            let Some(line_start_byte) = first.source_start_byte else {
                i += 1;
                continue;
            };
            // Find the end of this logical line's group.
            let mut j = i + 1;
            let mut has_injected = false;
            while j < source_lines.len() {
                match source_lines[j].line_start {
                    LineStart::AfterBreak => {
                        j += 1;
                    }
                    LineStart::AfterInjectedNewline => {
                        has_injected = true;
                        break;
                    }
                    LineStart::Beginning | LineStart::AfterSourceNewline => {
                        break;
                    }
                }
            }
            if !has_injected {
                // Slice `source_lines[i..j]` corresponds to one logical
                // line with no plugin-injected reshaping.  Store it.
                let slice: Vec<ViewLine> = source_lines[i..j].to_vec();
                let arc = Arc::new(slice);
                state
                    .line_wrap_cache
                    .put(make_key(line_start_byte, cache_view_mode), arc.clone());
                // Also write under `Source` so scroll math (which
                // queries with its `Source` convention) hits the
                // same entry.  Same value, same Arc — no deep copy.
                if !matches!(cache_view_mode, CacheViewMode::Source) {
                    state
                        .line_wrap_cache
                        .put(make_key(line_start_byte, CacheViewMode::Source), arc);
                }
            }
            i = j;
        }
    }

    // Inject virtual lines (LineAbove/LineBelow) from VirtualTextManager.
    // When soft-wrap is enabled, pass the same per-row content width that
    // `apply_wrapping_transform` uses for source lines (effective width
    // less the gutter) so long virtual-line text wraps consistently
    // instead of being truncated past the viewport edge.
    let virtual_line_wrap_width = if line_wrap_enabled {
        Some(effective_width.saturating_sub(gutter_width).max(1))
    } else {
        None
    };
    let lines = inject_virtual_lines(source_lines, state, theme, virtual_line_wrap_width);
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
