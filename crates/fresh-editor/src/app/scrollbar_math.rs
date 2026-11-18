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
//! Both take the pane's [`WrapIndexGeometry`] from
//! [`wrap_index_geometry_for`](crate::view::ui::split_rendering::wrap_index_geometry_for),
//! the key the renderer and every other scroll path use, so a click or drag
//! counts the rows the thumb was drawn for.
//!
//! Both run in O(log N_lines) per call by reading from
//! [`WrapIndex`](crate::view::wrap_index::WrapIndex) —
//! the whole-buffer prefix-sum index over per-line visual row counts.
//! No per-event O(N_lines) walk, no per-event flat row→byte vector.
//! On a cold index the first call walks the buffer once to build the
//! index; subsequent calls (the steady state during a drag) are pure
//! lookups.

use crate::state::EditorState;
use crate::view::wrap_index::WrapIndexGeometry;

/// Total visual rows under `geometry`, building the index if needed.
fn total_rows(
    state: &mut EditorState,
    geometry: WrapIndexGeometry,
    inputs: crate::view::line_wrap_cache::PipelineInputs,
    fold_ranges: Vec<std::ops::Range<usize>>,
) -> usize {
    let line_ending = state.buffer.line_ending();
    // Resolved before `entry` takes `&mut state`.
    let decorations = state.index_decorations(geometry.view_mode, fold_ranges, &[]);
    let index = state.wrap_indices.entry(geometry);
    index.ensure_built(
        &mut state.buffer,
        geometry,
        inputs,
        line_ending,
        &decorations,
    );
    index.total_rows() as usize
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
    geometry: WrapIndexGeometry,
    pipeline_inputs_ver: crate::view::line_wrap_cache::PipelineInputs,
    fold_ranges: Vec<std::ops::Range<usize>>,
) -> (usize, usize) {
    if state.buffer.is_empty() || viewport_height == 0 {
        return (0, 0);
    }

    let total_visual_rows = total_rows(state, geometry, pipeline_inputs_ver, fold_ranges);
    if total_visual_rows == 0 {
        return (0, 0);
    }

    let max_scroll_row = total_visual_rows.saturating_sub(viewport_height);
    if max_scroll_row == 0 {
        // Content fits in viewport, no scrolling needed
        return (0, 0);
    }

    let target_row = ((ratio * max_scroll_row as f64).round() as usize).min(max_scroll_row);

    let Some(index) = state.wrap_indices.get(&geometry) else {
        return (0, 0);
    };
    let addr = index.byte_of_row(&state.buffer, target_row as u32);
    // The viewport still addresses a logical line plus a wrap-segment offset;
    // once it anchors on a byte this is just `addr.byte`.
    let line_start = state.buffer.line_start_offset(addr.line).unwrap_or(0);
    (line_start, addr.row_in_line)
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
    geometry: WrapIndexGeometry,
    pipeline_inputs_ver: crate::view::line_wrap_cache::PipelineInputs,
    fold_ranges: Vec<std::ops::Range<usize>>,
) -> (usize, usize) {
    if state.buffer.is_empty() || viewport_height == 0 || scrollbar_height <= 1 {
        return (0, 0);
    }

    let total_visual_rows = total_rows(state, geometry, pipeline_inputs_ver, fold_ranges);
    if total_visual_rows == 0 {
        return (0, 0);
    }

    let max_scroll_row = total_visual_rows.saturating_sub(viewport_height);
    if max_scroll_row == 0 {
        return (0, 0);
    }

    // Visual row of the drag start: first row of the line containing
    // `drag_start_top_byte`, plus the wrap-segment offset within that line.
    let Some(index) = state.wrap_indices.get(&geometry) else {
        return (0, 0);
    };
    let drag_line_idx = state.buffer.get_line_number(drag_start_top_byte);
    let line_first_row = index.line_first_row(drag_line_idx) as usize;
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

    let target_row =
        ((target_scroll_ratio * max_scroll_row as f64).round() as usize).min(max_scroll_row);

    let addr = index.byte_of_row(&state.buffer, target_row as u32);
    let line_start = state.buffer.line_start_offset(addr.line).unwrap_or(0);
    (line_start, addr.row_in_line)
}
