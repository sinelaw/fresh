//! Folding and diff-gutter indicators.
//!
//! This module is self-contained: it exposes pure functions that take typed
//! inputs (buffer, marker list, fold manager, view lines, viewport range) and
//! produce typed outputs (transformed view lines, indicator maps). It owns
//! the `FoldIndicator` type since that is only a data carrier for its
//! indicator map, consumed via `DecorationContext` later in the pipeline.

use super::style::append_fold_placeholder;
use crate::model::buffer::Buffer;
use crate::model::marker::MarkerList;
use crate::state::EditorState;
use crate::view::folding::FoldManager;
use crate::view::margin::LineIndicator;
use crate::view::ui::view_pipeline::ViewLine;
use fresh_core::api::ViewTokenStyle;
use ratatui::style::Color;
use std::collections::{BTreeMap, HashSet};

/// Fold indicator for a single line (closed vs. open).
#[derive(Clone, Copy, Debug)]
pub(super) struct FoldIndicator {
    pub collapsed: bool,
}

/// When folds are active, grow the visible-line estimate so that hidden lines
/// beyond the viewport top are still included in the token-build pass.
pub(super) fn fold_adjusted_visible_count(
    buffer: &Buffer,
    marker_list: &MarkerList,
    folds: &FoldManager,
    top_byte: usize,
    visible_count: usize,
) -> usize {
    if folds.is_empty() {
        return visible_count;
    }

    let start_line = buffer.get_line_number(top_byte);
    let mut total = visible_count;

    let mut ranges = folds.resolved_ranges(buffer, marker_list);
    if ranges.is_empty() {
        return visible_count;
    }
    ranges.sort_by_key(|range| range.header_line);

    let mut min_header_line = start_line;
    if let Some(containing_end) = ranges
        .iter()
        .filter(|range| start_line >= range.start_line && start_line <= range.end_line)
        .map(|range| range.end_line)
        .max()
    {
        let hidden_remaining = containing_end.saturating_sub(start_line).saturating_add(1);
        total = total.saturating_add(hidden_remaining);
        min_header_line = containing_end.saturating_add(1);
    }

    let mut end_line = start_line.saturating_add(total);

    for range in ranges {
        if range.header_line < min_header_line {
            continue;
        }
        if range.header_line > end_line {
            break;
        }
        let hidden = range
            .end_line
            .saturating_sub(range.start_line)
            .saturating_add(1);
        total = total.saturating_add(hidden);
        end_line = start_line.saturating_add(total);
    }

    total
}

/// Build the sorted, non-overlapping list of source-byte ranges for the
/// currently-collapsed folds. Feed into
/// [`ViewLineIterator::with_fold_skip`] so hidden content is never
/// materialised as a ViewLine.
pub(super) fn fold_skip_set(
    buffer: &Buffer,
    marker_list: &MarkerList,
    folds: &FoldManager,
) -> Vec<std::ops::Range<usize>> {
    if folds.is_empty() {
        return Vec::new();
    }
    let mut ranges: Vec<std::ops::Range<usize>> = folds
        .resolved_ranges(buffer, marker_list)
        .into_iter()
        .map(|r| r.start_byte..r.end_byte)
        .collect();
    ranges.sort_by_key(|r| r.start);
    ranges
}

/// Append placeholder text to the last visual segment of each collapsed
/// fold's header line. Hidden-line filtering is handled upstream by the
/// iterator's fold-skip sweep (see [`fold_skip_set`]), so this pass only
/// mutates header ViewLines — it no longer drops any.
pub(super) fn apply_folding(
    lines: Vec<ViewLine>,
    buffer: &Buffer,
    marker_list: &MarkerList,
    folds: &FoldManager,
    placeholder_style: &ViewTokenStyle,
) -> Vec<ViewLine> {
    if folds.is_empty() {
        return lines;
    }

    let collapsed_header_bytes = folds.collapsed_header_bytes(buffer, marker_list);
    if collapsed_header_bytes.is_empty() {
        return lines;
    }

    // Pre-compute: for each line, what is the source byte of the next line?
    let mut next_source_byte: Vec<Option<usize>> = vec![None; lines.len()];
    let mut next_byte: Option<usize> = None;
    for (idx, line) in lines.iter().enumerate().rev() {
        next_source_byte[idx] = next_byte;
        if let Some(byte) = view_line_source_byte(line) {
            next_byte = Some(byte);
        }
    }

    let mut out = lines;
    for idx in 0..out.len() {
        let Some(byte) = view_line_source_byte(&out[idx]) else {
            continue;
        };
        let Some(placeholder) = collapsed_header_bytes.get(&byte) else {
            continue;
        };
        // Only append placeholder on the last visual segment of the line.
        if next_source_byte[idx] == Some(byte) {
            continue;
        }
        let raw_text = placeholder
            .as_deref()
            .filter(|s| !s.trim().is_empty())
            .unwrap_or("...");
        let text = if raw_text.starts_with(' ') {
            raw_text.to_string()
        } else {
            format!(" {}", raw_text)
        };
        append_fold_placeholder(&mut out[idx], &text, placeholder_style);
    }

    out
}

/// Get the source byte offset of a view line (first `Some` in char_source_bytes).
fn view_line_source_byte(line: &ViewLine) -> Option<usize> {
    line.char_source_bytes.iter().find_map(|m| *m)
}

/// Build the per-line fold indicator map for the current viewport.
pub(super) fn fold_indicators_for_viewport(
    state: &EditorState,
    folds: &FoldManager,
    view_lines: &[ViewLine],
) -> BTreeMap<usize, FoldIndicator> {
    let mut indicators = BTreeMap::new();

    // Collapsed headers from marker-based folds — always keyed by header_byte
    for range in folds.resolved_ranges(&state.buffer, &state.marker_list) {
        indicators.insert(range.header_byte, FoldIndicator { collapsed: true });
    }

    if !state.folding_ranges.is_empty() {
        // Use LSP-provided folding ranges.
        // Filter to only ranges that start on one of our visible view lines.
        let visible_starts: HashSet<usize> = view_lines
            .iter()
            .filter_map(|l| l.source_start_byte)
            .collect();

        let resolved = state
            .folding_ranges
            .resolved(&state.buffer, &state.marker_list);
        for range in &resolved {
            let start_line = range.start_line as usize;
            let end_line = range.end_line as usize;
            if end_line <= start_line {
                continue;
            }
            if let Some(line_byte) = state.buffer.line_start_offset(start_line) {
                if visible_starts.contains(&line_byte) {
                    indicators
                        .entry(line_byte)
                        .or_insert(FoldIndicator { collapsed: false });
                }
            }
        }
    } else {
        // Indent-based fold detection on viewport bytes — key by absolute byte offset
        use crate::view::folding::indent_folding;
        let tab_size = state.buffer_settings.tab_size;
        let max_lookahead = crate::config::INDENT_FOLD_INDICATOR_MAX_SCAN;

        for (i, view_line) in view_lines.iter().enumerate() {
            if let Some(line_start_byte) = view_line.source_start_byte {
                if view_line.line_start.is_continuation() {
                    continue;
                }

                let mut subsequent_lines = Vec::new();
                let lookahead_limit = (i + 1 + max_lookahead).min(view_lines.len());
                for j in i..lookahead_limit {
                    subsequent_lines.push(view_lines[j].text.as_bytes());
                }

                if indent_folding::is_line_foldable_in_bytes(&subsequent_lines, tab_size) {
                    indicators
                        .entry(line_start_byte)
                        .or_insert(FoldIndicator { collapsed: false });
                }
            }
        }
    }

    indicators
}

/// Compute diff-since-saved indicators for the viewport.
///
/// Calls `diff_since_saved()` to get byte ranges that differ from the saved
/// version, intersects them with the viewport, and scans for line starts to
/// produce per-line indicators.
pub(super) fn diff_indicators_for_viewport(
    state: &EditorState,
    viewport_start: usize,
    viewport_end: usize,
) -> BTreeMap<usize, LineIndicator> {
    use crate::view::folding::indent_folding;
    let diff = state.buffer.diff_since_saved();
    if diff.equal || diff.byte_ranges.is_empty() {
        return BTreeMap::new();
    }

    let mut indicators = BTreeMap::new();
    let indicator = LineIndicator::new(
        "│",
        Color::Rgb(100, 149, 237), // Cornflower blue
        5,                         // Lower priority than git gutter (10)
    );

    let bytes = state.buffer.slice_bytes(viewport_start..viewport_end);
    if bytes.is_empty() {
        return indicators;
    }

    for range in &diff.byte_ranges {
        let lo = range.start.max(viewport_start);
        let hi = range.end.min(viewport_end);
        if lo >= hi {
            continue;
        }

        let line_start = indent_folding::find_line_start_byte(&state.buffer, lo);
        if line_start >= viewport_start && line_start < viewport_end {
            indicators
                .entry(line_start)
                .or_insert_with(|| indicator.clone());
        }

        let rel_lo = lo - viewport_start;
        let rel_hi = (hi - viewport_start).min(bytes.len());
        for (i, &byte) in bytes[rel_lo..rel_hi].iter().enumerate() {
            if byte == b'\n' {
                let next_line_start = viewport_start + rel_lo + i + 1;
                if next_line_start < viewport_end {
                    indicators
                        .entry(next_line_start)
                        .or_insert_with(|| indicator.clone());
                }
            }
        }
    }

    indicators
}
