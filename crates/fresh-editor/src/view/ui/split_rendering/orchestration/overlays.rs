//! Producers for [`SelectionContext`] and [`DecorationContext`].
//!
//! These two functions are the only places that write the shared carriers;
//! every consumer is another module inside `orchestration/`.

use super::super::folding::{diff_indicators_for_viewport, fold_indicators_for_viewport};
use super::super::style::inline_diagnostic_style;
use super::contexts::{DecorationContext, SelectionContext};
use crate::model::cursor::{Cursors, SelectionMode};
use crate::state::{EditorState, ViewMode};
use crate::view::folding::FoldManager;
use crate::view::theme::Theme;
use crate::view::ui::view_pipeline::ViewLine;
use ratatui::style::Style;
use std::collections::{HashMap, HashSet};
use std::ops::Range;

/// Build the [`SelectionContext`] for the current set of cursors.
pub(crate) fn selection_context(state: &EditorState, cursors: &Cursors) -> SelectionContext {
    // When cursors are hidden, suppress all visual selection feedback.
    if !state.show_cursors {
        return SelectionContext {
            ranges: Vec::new(),
            block_rects: Vec::new(),
            cursor_positions: Vec::new(),
            primary_cursor_position: cursors.primary().position,
        };
    }

    let ranges: Vec<Range<usize>> = cursors
        .iter()
        .filter_map(|(_, cursor)| {
            // Don't include normal selection for cursors in block selection mode;
            // block selections are rendered separately via block_rects.
            if cursor.selection_mode == SelectionMode::Block {
                None
            } else {
                cursor.selection_range()
            }
        })
        .collect();

    let block_rects: Vec<(usize, usize, usize, usize)> = cursors
        .iter()
        .filter_map(|(_, cursor)| {
            if cursor.selection_mode == SelectionMode::Block {
                if let Some(anchor) = cursor.block_anchor {
                    let cur_line = state.buffer.get_line_number(cursor.position);
                    let cur_line_start = state.buffer.line_start_offset(cur_line).unwrap_or(0);
                    let cur_col = cursor.position.saturating_sub(cur_line_start);

                    Some((
                        anchor.line.min(cur_line),
                        anchor.column.min(cur_col),
                        anchor.line.max(cur_line),
                        anchor.column.max(cur_col),
                    ))
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect();

    let cursor_positions: Vec<usize> = cursors.iter().map(|(_, cursor)| cursor.position).collect();

    SelectionContext {
        ranges,
        block_rects,
        cursor_positions,
        primary_cursor_position: cursors.primary().position,
    }
}

/// Build the [`DecorationContext`] for the current viewport: syntax
/// highlights, LSP overlays, diagnostics, virtual text, line indicators
/// (git gutter + diff-since-saved), and fold indicators.
#[allow(clippy::too_many_arguments)]
pub(crate) fn decoration_context(
    state: &mut EditorState,
    viewport_start: usize,
    viewport_end: usize,
    primary_cursor_position: usize,
    folds: &FoldManager,
    theme: &Theme,
    highlight_context_bytes: usize,
    view_mode: &ViewMode,
    diagnostics_inline_text: bool,
    view_lines: &[ViewLine],
) -> DecorationContext {
    use crate::view::folding::indent_folding;

    // Extend highlighting range by ~1 viewport size before/after for better
    // context. Helps tree-sitter parse multi-line constructs that span
    // viewport boundaries.
    let viewport_size = viewport_end.saturating_sub(viewport_start);
    let highlight_start = viewport_start.saturating_sub(viewport_size);
    let highlight_end = viewport_end
        .saturating_add(viewport_size)
        .min(state.buffer.len());

    let highlight_spans = state.highlighter.highlight_viewport(
        &state.buffer,
        highlight_start,
        highlight_end,
        theme,
        highlight_context_bytes,
    );

    // Update reference highlight overlays (debounced; creates overlays that
    // auto-adjust).
    state.reference_highlight_overlay.update(
        &state.buffer,
        &mut state.overlays,
        &mut state.marker_list,
        &mut state.reference_highlighter,
        primary_cursor_position,
        viewport_start,
        viewport_end,
        highlight_context_bytes,
        theme.semantic_highlight_bg,
    );

    // Update bracket highlight overlays.
    state.bracket_highlight_overlay.update(
        &state.buffer,
        &mut state.overlays,
        &mut state.marker_list,
        primary_cursor_position,
    );

    // Semantic tokens are stored as overlays so their ranges track edits.
    // Convert them into highlight spans for the render pipeline.
    let is_compose = matches!(view_mode, ViewMode::PageView);
    let md_emphasis_ns =
        fresh_core::overlay::OverlayNamespace::from_string("md-emphasis".to_string());
    let mut semantic_token_spans = Vec::new();
    let mut viewport_overlays = Vec::new();
    for (overlay, range) in
        state
            .overlays
            .query_viewport(viewport_start, viewport_end, &state.marker_list)
    {
        if crate::services::lsp::semantic_tokens::is_semantic_token_overlay(overlay) {
            if let crate::view::overlay::OverlayFace::Foreground { color } = &overlay.face {
                semantic_token_spans.push(crate::primitives::highlighter::HighlightSpan {
                    range,
                    color: *color,
                    category: None,
                });
            }
            continue;
        }

        // Skip markdown compose overlays in Source mode — they should only
        // render in the Compose-mode split.
        if !is_compose && overlay.namespace.as_ref() == Some(&md_emphasis_ns) {
            continue;
        }

        viewport_overlays.push((overlay.clone(), range));
    }

    // Sort overlays by priority (ascending) so higher priority overlays are
    // applied last in the rendering loop.
    viewport_overlays.sort_by_key(|(overlay, _)| overlay.priority);

    // Use the lsp-diagnostic namespace to identify diagnostic overlays.
    let diagnostic_ns = crate::services::lsp::diagnostics::lsp_diagnostic_namespace();
    let diagnostic_lines: HashSet<usize> = viewport_overlays
        .iter()
        .filter_map(|(overlay, range)| {
            if overlay.namespace.as_ref() == Some(&diagnostic_ns) {
                return Some(indent_folding::find_line_start_byte(
                    &state.buffer,
                    range.start,
                ));
            }
            None
        })
        .collect();

    // Build inline diagnostic text map; highest priority wins per line.
    let diagnostic_inline_texts: HashMap<usize, (String, Style)> = if diagnostics_inline_text {
        let mut by_line: HashMap<usize, (String, Style, i32)> = HashMap::new();
        for (overlay, range) in &viewport_overlays {
            if overlay.namespace.as_ref() != Some(&diagnostic_ns) {
                continue;
            }
            if let Some(ref message) = overlay.message {
                let line_start = indent_folding::find_line_start_byte(&state.buffer, range.start);
                let priority = overlay.priority;
                let dominated = by_line
                    .get(&line_start)
                    .is_some_and(|(_, _, existing_pri)| *existing_pri >= priority);
                if !dominated {
                    let style = inline_diagnostic_style(priority, theme);
                    let first_line = message.lines().next().unwrap_or(message);
                    by_line.insert(line_start, (first_line.to_string(), style, priority));
                }
            }
        }
        by_line
            .into_iter()
            .map(|(k, (msg, style, _))| (k, (msg, style)))
            .collect()
    } else {
        HashMap::new()
    };

    let virtual_text_lookup: HashMap<usize, Vec<crate::view::virtual_text::VirtualText>> = state
        .virtual_texts
        .build_lookup(&state.marker_list, viewport_start, viewport_end)
        .into_iter()
        .map(|(position, texts)| (position, texts.into_iter().cloned().collect()))
        .collect();

    // Pre-compute line indicators for the viewport.
    let mut line_indicators =
        state
            .margins
            .get_indicators_for_viewport(viewport_start, viewport_end, |byte_offset| {
                indent_folding::find_line_start_byte(&state.buffer, byte_offset)
            });

    // Merge native diff-since-saved indicators (cornflower blue │ for unsaved edits).
    // These have priority 5, lower than git gutter (10).
    let diff_indicators = diff_indicators_for_viewport(state, viewport_start, viewport_end);
    for (key, diff_ind) in diff_indicators {
        line_indicators.entry(key).or_insert(diff_ind);
    }

    let fold_indicators = fold_indicators_for_viewport(state, folds, view_lines);

    DecorationContext {
        highlight_spans,
        semantic_token_spans,
        viewport_overlays,
        virtual_text_lookup,
        diagnostic_lines,
        diagnostic_inline_texts,
        line_indicators,
        fold_indicators,
    }
}
