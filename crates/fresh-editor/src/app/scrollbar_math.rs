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
//! Both run in O(log N_lines) per call by reading from
//! [`VisualRowIndex`](crate::view::visual_row_index::VisualRowIndex) —
//! the whole-buffer prefix-sum index over per-line visual row counts.
//! No per-event O(N_lines) walk, no per-event flat row→byte vector.
//! On a cold index the first call walks the buffer once to build the
//! index; subsequent calls (the steady state during a drag) are pure
//! lookups.

use crate::model::buffer::Buffer;
use crate::primitives::line_wrapping::WrapConfig;
use crate::state::EditorState;
use crate::view::line_wrap_cache::CacheViewMode;
use crate::view::visual_row_index::{ensure_built, VisualRowIndexKey};

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

/// Build the `VisualRowIndexKey` scroll math uses for these viewport
/// dimensions, then ensure the per-state index is populated for it.
/// Subsequent calls during the same drag with unchanged geometry are
/// O(1) — the matching key is detected and the build is skipped.
fn ensure_index(state: &mut EditorState, viewport_width: usize, pipeline_inputs_ver: u64) {
    let gutter_width = estimated_gutter_width(&state.buffer);
    let wrap_config = WrapConfig::new(viewport_width, gutter_width, true, true);
    let effective_width = wrap_config
        .first_line_width
        .saturating_add(gutter_width)
        .max(2);
    let key = VisualRowIndexKey {
        pipeline_inputs_version: pipeline_inputs_ver,
        // Scrollbar-math runs without access to the view mode. The
        // renderer's writeback populates keys under the active mode;
        // here we use Source as a fixed convention and cache hits pick
        // up entries the renderer wrote under the same convention.
        view_mode: CacheViewMode::Source,
        effective_width: effective_width as u32,
        gutter_width: gutter_width as u16,
        wrap_column: None,
        hanging_indent: wrap_config.hanging_indent,
        line_wrap_enabled: true,
    };
    ensure_built(state, &key);
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

    ensure_index(state, viewport_width, pipeline_inputs_ver);
    let total_visual_rows = state.visual_row_index.total_rows() as usize;
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

    let (_line_idx, line_start, offset) = state.visual_row_index.position_at_row(target_row as u32);
    (line_start, offset)
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

    ensure_index(state, viewport_width, pipeline_inputs_ver);
    let total_visual_rows = state.visual_row_index.total_rows() as usize;
    if total_visual_rows == 0 {
        return (0, 0);
    }

    let max_scroll_row = total_visual_rows.saturating_sub(viewport_height);
    if max_scroll_row == 0 {
        return (0, 0);
    }

    // Visual row of the drag start: first row of the line containing
    // `drag_start_top_byte`, plus the wrap-segment offset within that line.
    let (drag_line_idx, _) = state.visual_row_index.line_for_byte(drag_start_top_byte);
    let line_first_row = state.visual_row_index.line_first_row(drag_line_idx) as usize;
    let start_visual_row = (line_first_row + drag_start_view_line_offset).min(max_scroll_row);

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

    let (_line_idx, line_start, offset) = state.visual_row_index.position_at_row(target_row as u32);
    (line_start, offset)
}
