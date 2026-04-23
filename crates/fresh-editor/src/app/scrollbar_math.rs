//! Pure visual-row-aware scrollbar math.
//!
//! Two functions:
//!
//! - [`scrollbar_jump_visual`] — convert a click ratio on the scrollbar
//!   track into a `(line_byte, view_line_offset)` scroll target.
//! - [`scrollbar_drag_relative_visual`] — follow the thumb under the mouse
//!   as the user drags, preserving the click offset on the thumb so the
//!   cursor stays pinned to the same spot on the thumb.
//!
//! Both take a `&mut Buffer` only because `Buffer::line_iterator` requires
//! mutable access for lazy-load internals; neither touches `Editor`.

use crate::model::buffer::Buffer;
use crate::primitives::line_wrapping::WrapConfig;
use crate::state::EditorState;
use crate::view::line_wrap_cache::{compute_line_layout, CacheViewMode, WrapGeometry};

/// Width estimate of the gutter, used to build the wrap config. Kept in
/// sync with the real gutter sizing in the render path (indicator + digits
/// + separator) — see `Viewport::gutter_width`, which uses the same
/// formula with `MIN_LINE_NUMBER_DIGITS` as the floor. If this diverges
/// from the renderer's gutter width, scroll math counts visual rows at a
/// different text-area width than the renderer actually uses, so
/// `max_scroll_row` ends up too high or too low and the scroll stops
/// short of the bottom (or past it) when line wrap is enabled.
fn estimated_gutter_width(buffer: &Buffer) -> usize {
    let line_count = buffer.line_count().unwrap_or(1);
    let digits = (line_count as f64).log10().floor() as usize + 1;
    1 + digits.max(crate::view::margin::MIN_LINE_NUMBER_DIGITS) + 3
}

/// Build a map of `(line_start_byte, visual_row_offset_within_line)` for
/// every visual row in the buffer, and the total row count.
///
/// Per-line row count goes through `LineWrapCache`:
///   - cache hit → O(1) `Arc` clone;
///   - cache miss → `compute_line_layout` runs the full per-line
///     pipeline (same as the renderer) and stores the result for future
///     drag/jump events.
///
/// Scroll math only needs the row count (`entry.len()`), but the cache
/// holds the full `Vec<ViewLine>` so any other consumer that queries
/// the same key (e.g. future cursor-nav migration) gets a consistent
/// value — see `docs/internal/line-wrap-cache-plan.md`.
fn build_visual_row_map(
    state: &mut EditorState,
    viewport_width: usize,
    pipeline_inputs_ver: u64,
) -> (Vec<(usize, usize)>, usize) {
    let gutter_width = estimated_gutter_width(&state.buffer);
    let wrap_config = WrapConfig::new(viewport_width, gutter_width, true, true);
    let effective_width = wrap_config
        .first_line_width
        .saturating_add(gutter_width)
        .max(2);
    let geom = WrapGeometry {
        effective_width,
        gutter_width,
        hanging_indent: wrap_config.hanging_indent,
        wrap_column: None,
        line_wrap_enabled: true,
        // Scrollbar-math runs without access to the view mode. The
        // renderer's writeback populates keys under the active mode;
        // here we use Source as a fixed convention and cache hits pick
        // up entries the renderer wrote under the same convention.
        view_mode: CacheViewMode::Source,
    };

    // First pass: collect line boundaries while the buffer iterator
    // holds a mutable borrow. We release the borrow before the second
    // pass so `compute_line_layout` can re-borrow `&mut state`.
    let line_ranges: Vec<(usize, usize)> = {
        let mut iter = state.buffer.line_iterator(0, 80);
        let mut out = Vec::new();
        while let Some((line_start, _content)) = iter.next_line() {
            let end = iter.current_position();
            out.push((line_start, end));
        }
        out
    };

    // Second pass: query cache per line, filling on miss via the
    // per-line mini-pipeline. Both borrows of `state` are disjoint from
    // the iterator borrow above (which has been dropped).
    let mut total_visual_rows = 0;
    let mut visual_row_positions: Vec<(usize, usize)> = Vec::new();
    for (line_start, line_end) in line_ranges {
        let key = geom.key(line_start, pipeline_inputs_ver);
        let entry = if let Some(cached) = state.line_wrap_cache.get(&key) {
            cached
        } else {
            let layout = compute_line_layout(state, line_start, line_end, &geom);
            let arc = std::sync::Arc::new(layout);
            state.line_wrap_cache.put(key, arc.clone());
            arc
        };
        let rows = entry.len();
        for offset in 0..rows {
            visual_row_positions.push((line_start, offset));
        }
        total_visual_rows += rows;
    }

    (visual_row_positions, total_visual_rows)
}

/// Pick the `(byte, offset)` at `target_row`, falling back to the last
/// valid row if the index is out of range.
fn position_at(visual_row_positions: &[(usize, usize)], target_row: usize) -> (usize, usize) {
    if target_row < visual_row_positions.len() {
        visual_row_positions[target_row]
    } else {
        visual_row_positions.last().copied().unwrap_or((0, 0))
    }
}

/// Calculate scroll position for a visual-row-aware scrollbar *jump*.
///
/// Returns `(byte_position, view_line_offset)` — the start of the line
/// and the wrap-segment offset inside that line.
#[allow(clippy::too_many_arguments)]
pub(crate) fn scrollbar_jump_visual(
    state: &mut EditorState,
    ratio: f64,
    viewport_height: usize,
    viewport_width: usize,
    pipeline_inputs_ver: u64,
) -> (usize, usize) {
    if state.buffer.len() == 0 || viewport_height == 0 {
        return (0, 0);
    }

    let (visual_row_positions, total_visual_rows) =
        build_visual_row_map(state, viewport_width, pipeline_inputs_ver);
    if total_visual_rows == 0 {
        return (0, 0);
    }

    let max_scroll_row = total_visual_rows.saturating_sub(viewport_height);
    if max_scroll_row == 0 {
        // Content fits in viewport, no scrolling needed
        return (0, 0);
    }

    let target_row = (ratio * max_scroll_row as f64).round() as usize;
    let target_row = target_row.min(max_scroll_row);

    position_at(&visual_row_positions, target_row)
}

/// Calculate scroll position for a visual-row-aware scrollbar *drag*.
///
/// The thumb follows the mouse position, accounting for where on the thumb
/// the user originally clicked.
#[allow(clippy::too_many_arguments)]
pub(crate) fn scrollbar_drag_relative_visual(
    state: &mut EditorState,
    current_row: u16,
    scrollbar_y: u16,
    scrollbar_height: usize,
    drag_start_row: u16,
    drag_start_top_byte: usize,
    drag_start_view_line_offset: usize,
    viewport_height: usize,
    viewport_width: usize,
    pipeline_inputs_ver: u64,
) -> (usize, usize) {
    if state.buffer.len() == 0 || viewport_height == 0 || scrollbar_height <= 1 {
        return (0, 0);
    }

    let (visual_row_positions, total_visual_rows) =
        build_visual_row_map(state, viewport_width, pipeline_inputs_ver);
    if total_visual_rows == 0 {
        return (0, 0);
    }

    let max_scroll_row = total_visual_rows.saturating_sub(viewport_height);
    if max_scroll_row == 0 {
        return (0, 0);
    }

    // Find the visual row corresponding to drag_start_top_byte + view_line_offset.
    // First find the line start, then add the offset for wrapped lines.
    let line_start_visual_row = visual_row_positions
        .iter()
        .position(|(byte, _)| *byte >= drag_start_top_byte)
        .unwrap_or(0);
    let start_visual_row =
        (line_start_visual_row + drag_start_view_line_offset).min(max_scroll_row);

    // Thumb size — same formula as the scrollbar renderer.
    let thumb_size_raw = (viewport_height as f64 / total_visual_rows as f64
        * scrollbar_height as f64)
        .ceil() as usize;
    let max_thumb_size = (scrollbar_height as f64 * 0.8).floor() as usize;
    let thumb_size = thumb_size_raw
        .max(1)
        .min(max_thumb_size)
        .min(scrollbar_height);

    let max_thumb_start = scrollbar_height.saturating_sub(thumb_size);

    // Where the thumb was at drag start, in scrollbar coordinates.
    let start_scroll_ratio = start_visual_row as f64 / max_scroll_row as f64;
    let thumb_row_at_start = scrollbar_y as f64 + start_scroll_ratio * max_thumb_start as f64;

    // Offset within the thumb where the user clicked.
    let click_offset = drag_start_row as f64 - thumb_row_at_start;

    // Target thumb position from current mouse position.
    let target_thumb_row = current_row as f64 - click_offset;

    // Inverse of the thumb_start formula.
    let target_scroll_ratio = if max_thumb_start > 0 {
        ((target_thumb_row - scrollbar_y as f64) / max_thumb_start as f64).clamp(0.0, 1.0)
    } else {
        0.0
    };

    let target_row = (target_scroll_ratio * max_scroll_row as f64).round() as usize;
    let target_row = target_row.min(max_scroll_row);

    position_at(&visual_row_positions, target_row)
}
