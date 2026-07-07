//! Scrollbar computation and rendering (vertical, horizontal, composite).
//!
//! These helpers take the editor `State`, viewport, and a few typed
//! parameters. They have no dependency on any shared render-time "mega
//! struct".

use crate::state::EditorState;
use crate::view::theme::Theme;
use crate::view::viewport::Viewport;
use ratatui::layout::Rect;
use ratatui::style::Style;
use ratatui::widgets::Paragraph;
use ratatui::widgets::Widget;

/// Above either bound, the exact wrapped-row scrollbar is skipped in favour of
/// the cheap logical-line approximation. The exact counts require word-wrapping
/// every line (`ensure_built`, O(all-lines)) and the index is rebuilt whenever
/// the buffer version changes — i.e. on every edit — so on a large wrapped
/// buffer each keystroke re-walks the whole buffer and stalls the UI
/// (fresh#2610). Both bounds are well under the 10 MB large-file threshold
/// because the per-line word-wrap cost is much higher than a raw byte scan.
/// Line count is the loop-iteration driver; the byte bound catches buffers with
/// few but very long lines.
const MAX_WRAP_SCROLLBAR_LINES: usize = 5_000;
const MAX_WRAP_SCROLLBAR_BYTES: usize = 2 * 1024 * 1024;

/// Compute scrollbar line counts: `(total_lines, top_line)`.
///
/// For large files the counts are reported as `(0, 0)` — the caller uses a
/// constant-size thumb in that case. When line wrapping is enabled, counts are
/// in visual rows instead of logical lines — except on a large wrapped buffer,
/// where the exact visual-row count is too expensive to recompute per edit and
/// we fall back to the logical-line approximation (see the constants above).
pub(super) fn scrollbar_line_counts(
    state: &mut EditorState,
    viewport: &Viewport,
    large_file_threshold_bytes: u64,
    buffer_len: usize,
) -> (usize, usize) {
    if buffer_len > large_file_threshold_bytes as usize {
        return (0, 0);
    }

    let total_lines = if buffer_len > 0 {
        state.buffer.get_line_number(buffer_len.saturating_sub(1)) + 1
    } else {
        1
    };

    if viewport.line_wrap_enabled
        && total_lines <= MAX_WRAP_SCROLLBAR_LINES
        && buffer_len <= MAX_WRAP_SCROLLBAR_BYTES
    {
        return scrollbar_visual_row_counts(state, viewport, buffer_len);
    }

    let top_line = if viewport.top_byte < buffer_len {
        state.buffer.get_line_number(viewport.top_byte)
    } else {
        0
    };

    (total_lines, top_line)
}

/// Calculate scrollbar position based on visual rows (for line-wrapped content).
/// Returns `(total_visual_rows, top_visual_row)`.
///
/// Both numbers come from the per-state [`VisualRowIndex`] in O(log N_lines).
/// The index is built lazily and reused across frames whenever its key
/// (pipeline-input version + geometry) is unchanged — so a steady-state
/// scroll where only `top_byte` moves never re-walks the buffer.
///
/// [`VisualRowIndex`]: crate::view::visual_row_index::VisualRowIndex
pub(super) fn scrollbar_visual_row_counts(
    state: &mut EditorState,
    viewport: &Viewport,
    buffer_len: usize,
) -> (usize, usize) {
    use crate::primitives::line_wrapping::WrapConfig;
    use crate::view::line_wrap_cache::{pipeline_inputs_version, CacheViewMode};
    use crate::view::visual_row_index::{ensure_built, VisualRowIndexKey};

    if buffer_len == 0 {
        return (1, 0);
    }

    let gutter_width = viewport.gutter_width(&state.buffer);
    let wrap_config = WrapConfig::new(
        viewport.width as usize,
        gutter_width,
        true,
        viewport.wrap_indent,
    );
    let effective_width = wrap_config
        .first_line_width
        .saturating_add(gutter_width)
        .max(2);
    let hanging_indent = wrap_config.hanging_indent;
    let pipeline_inputs_ver = pipeline_inputs_version(
        state.buffer.version(),
        state.soft_breaks.version(),
        state.conceals.version(),
        state.virtual_texts.version(),
    );

    let key = VisualRowIndexKey {
        pipeline_inputs_version: pipeline_inputs_ver,
        view_mode: CacheViewMode::Source,
        effective_width: effective_width as u32,
        gutter_width: gutter_width as u16,
        wrap_column: None,
        hanging_indent,
        line_wrap_enabled: viewport.line_wrap_enabled,
    };
    ensure_built(state, &key);

    let total_visual_rows = state.visual_row_index.total_rows() as usize;
    let total_visual_rows = total_visual_rows.max(1);

    // Top visual row: first row of the line containing `top_byte`,
    // plus the wrap-segment offset within that line.
    let (line_idx, _) = state.visual_row_index.line_for_byte(viewport.top_byte);
    let top_first_row = state.visual_row_index.line_first_row(line_idx) as usize;
    let top_visual_row =
        (top_first_row + viewport.top_view_line_offset).min(total_visual_rows.saturating_sub(1));

    (total_visual_rows, top_visual_row)
}

/// Compute the maximum line length encountered so far (in display columns).
/// Only scans the currently visible lines (plus a small margin) and updates
/// the running maximum stored in the viewport.
pub(super) fn compute_max_line_length(state: &mut EditorState, viewport: &mut Viewport) -> usize {
    let buffer_len = state.buffer.len();
    let visible_width = viewport.width as usize;

    if buffer_len == 0 {
        return viewport.max_line_length_seen.max(visible_width);
    }

    let visible_lines = viewport.height as usize + 5;
    let mut lines_scanned = 0usize;
    let mut iter = state.buffer.line_iterator(viewport.top_byte, 80);
    loop {
        if lines_scanned >= visible_lines {
            break;
        }
        match iter.next_line() {
            Some((_byte_offset, content)) => {
                let display_len = content.len();
                if display_len > viewport.max_line_length_seen {
                    viewport.max_line_length_seen = display_len;
                }
                lines_scanned += 1;
            }
            None => break,
        }
    }

    viewport.max_line_length_seen.max(visible_width)
}

/// Render a scrollbar for a split.
/// Returns (thumb_start, thumb_end) positions for mouse hit testing.
#[allow(clippy::too_many_arguments)]
pub(super) fn render_scrollbar(
    buf: &mut ratatui::buffer::Buffer,
    state: &EditorState,
    viewport: &Viewport,
    scrollbar_rect: Rect,
    _is_active: bool,
    theme: &Theme,
    large_file_threshold_bytes: u64,
    total_lines: usize,
    top_line: usize,
) -> (usize, usize) {
    let height = scrollbar_rect.height as usize;
    if height == 0 {
        return (0, 0);
    }

    let buffer_len = state.buffer.len();
    let viewport_top = viewport.top_byte;
    let viewport_height_lines = height;

    let (thumb_start, thumb_size) = if buffer_len > large_file_threshold_bytes as usize {
        let thumb_start = if buffer_len > 0 {
            ((viewport_top as f64 / buffer_len as f64) * height as f64) as usize
        } else {
            0
        };
        (thumb_start, 1)
    } else {
        let thumb_size_raw = if total_lines > 0 {
            ((viewport_height_lines as f64 / total_lines as f64) * height as f64).ceil() as usize
        } else {
            1
        };

        let max_scroll_line = total_lines.saturating_sub(viewport_height_lines);

        let thumb_size = if max_scroll_line == 0 {
            height
        } else {
            let max_thumb_size = (height as f64 * 0.8).floor() as usize;
            thumb_size_raw.max(1).min(max_thumb_size).min(height)
        };

        let thumb_start = if max_scroll_line > 0 {
            let scroll_ratio = top_line.min(max_scroll_line) as f64 / max_scroll_line as f64;
            let max_thumb_start = height.saturating_sub(thumb_size);
            (scroll_ratio * max_thumb_start as f64) as usize
        } else {
            0
        };

        (thumb_start, thumb_size)
    };

    let thumb_end = thumb_start + thumb_size;

    let track_color = theme.scrollbar_track_fg;
    let thumb_color = theme.scrollbar_thumb_fg;

    for row in 0..height {
        let cell_area = Rect::new(scrollbar_rect.x, scrollbar_rect.y + row as u16, 1, 1);

        let style = if row >= thumb_start && row < thumb_end {
            Style::default().bg(thumb_color)
        } else {
            Style::default().bg(track_color)
        };

        let paragraph = Paragraph::new(" ").style(style);
        paragraph.render(cell_area, buf);
    }

    (thumb_start, thumb_end)
}

/// Render a horizontal scrollbar for a split.
/// `max_content_width` should be the actual max line length
/// (from [`compute_max_line_length`]).
/// Returns (thumb_start_col, thumb_end_col) for mouse hit testing.
pub(super) fn render_horizontal_scrollbar(
    buf: &mut ratatui::buffer::Buffer,
    viewport: &Viewport,
    hscrollbar_rect: Rect,
    _is_active: bool,
    theme: &Theme,
    max_content_width: usize,
) -> (usize, usize) {
    let width = hscrollbar_rect.width as usize;
    if width == 0 || hscrollbar_rect.height == 0 {
        return (0, 0);
    }

    let track_color = theme.scrollbar_track_fg;

    if viewport.line_wrap_enabled {
        for col in 0..width {
            let cell_area = Rect::new(hscrollbar_rect.x + col as u16, hscrollbar_rect.y, 1, 1);
            let paragraph = Paragraph::new(" ").style(Style::default().bg(track_color));
            paragraph.render(cell_area, buf);
        }
        return (0, width);
    }

    let visible_width = viewport.width as usize;
    let left_column = viewport.left_column;

    let max_scroll = max_content_width.saturating_sub(visible_width);

    let (thumb_start, thumb_size) = if max_scroll == 0 {
        (0, width)
    } else {
        let thumb_size_raw =
            ((visible_width as f64 / max_content_width as f64) * width as f64).ceil() as usize;
        let thumb_size = thumb_size_raw.max(2).min(width);

        let scroll_ratio = left_column.min(max_scroll) as f64 / max_scroll as f64;
        let max_thumb_start = width.saturating_sub(thumb_size);
        let thumb_start = (scroll_ratio * max_thumb_start as f64).round() as usize;

        (thumb_start, thumb_size)
    };

    let thumb_end = thumb_start + thumb_size;

    let thumb_color = theme.scrollbar_thumb_fg;

    for col in 0..width {
        let cell_area = Rect::new(hscrollbar_rect.x + col as u16, hscrollbar_rect.y, 1, 1);

        let style = if col >= thumb_start && col < thumb_end {
            Style::default().bg(thumb_color)
        } else {
            Style::default().bg(track_color)
        };

        let paragraph = Paragraph::new(" ").style(style);
        paragraph.render(cell_area, buf);
    }

    (thumb_start, thumb_end)
}

/// Render a scrollbar for composite buffer views.
pub(super) fn render_composite_scrollbar(
    buf: &mut ratatui::buffer::Buffer,
    scrollbar_rect: Rect,
    total_rows: usize,
    scroll_row: usize,
    viewport_height: usize,
    _is_active: bool,
    theme: &Theme,
) -> (usize, usize) {
    let height = scrollbar_rect.height as usize;
    if height == 0 || total_rows == 0 {
        return (0, 0);
    }

    let thumb_size_raw = if total_rows > 0 {
        ((viewport_height as f64 / total_rows as f64) * height as f64).ceil() as usize
    } else {
        1
    };

    let max_scroll = total_rows.saturating_sub(viewport_height);

    let thumb_size = if max_scroll == 0 {
        height
    } else {
        let max_thumb_size = (height as f64 * 0.8).floor() as usize;
        thumb_size_raw.max(1).min(max_thumb_size).min(height)
    };

    let thumb_start = if max_scroll > 0 {
        let scroll_ratio = scroll_row.min(max_scroll) as f64 / max_scroll as f64;
        let max_thumb_start = height.saturating_sub(thumb_size);
        (scroll_ratio * max_thumb_start as f64) as usize
    } else {
        0
    };

    let thumb_end = thumb_start + thumb_size;

    let track_color = theme.scrollbar_track_fg;
    let thumb_color = theme.scrollbar_thumb_fg;

    for row in 0..height {
        let cell_area = Rect::new(scrollbar_rect.x, scrollbar_rect.y + row as u16, 1, 1);

        let style = if row >= thumb_start && row < thumb_end {
            Style::default().bg(thumb_color)
        } else {
            Style::default().bg(track_color)
        };

        let paragraph = Paragraph::new(" ").style(style);
        paragraph.render(cell_area, buf);
    }

    (thumb_start, thumb_end)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    fn state_with_wrapping_lines(n: usize) -> EditorState {
        let fs: Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> =
            Arc::new(crate::model::filesystem::StdFileSystem);
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            fs,
        );
        // Each line is long enough to wrap to several visual rows at width 40.
        let line = "the quick brown fox jumps over the lazy dog and keeps going\n";
        let mut text = String::with_capacity(n * line.len());
        for _ in 0..n {
            text.push_str(line);
        }
        state.buffer.insert(0, &text);
        state
    }

    fn narrow_wrapped_viewport() -> Viewport {
        let mut vp = Viewport::new(40, 24);
        vp.line_wrap_enabled = true;
        vp
    }

    /// Small wrapped buffers keep the exact visual-row scrollbar: total counts
    /// wrapped rows (more than logical lines) and the index is built.
    #[test]
    fn small_wrapped_buffer_uses_exact_visual_rows() {
        let mut state = state_with_wrapping_lines(100);
        let vp = narrow_wrapped_viewport();
        let buffer_len = state.buffer.len();
        let (total, _) = scrollbar_line_counts(
            &mut state,
            &vp,
            crate::config::LARGE_FILE_THRESHOLD_BYTES,
            buffer_len,
        );
        assert!(
            total > 100,
            "small wrapped buffer should report wrapped-row count (>100), got {total}"
        );
        assert!(
            state.visual_row_index.line_count() > 0,
            "small wrapped buffer should build the visual-row index"
        );
    }

    /// Large wrapped buffers fall back to the logical-line approximation so the
    /// O(all-lines) visual-row scan never runs (and so never re-runs per edit):
    /// total equals the logical line count and the index is left unbuilt
    /// (fresh#2610).
    #[test]
    fn large_wrapped_buffer_skips_visual_row_scan() {
        let n = MAX_WRAP_SCROLLBAR_LINES + 1;
        let mut state = state_with_wrapping_lines(n);
        let vp = narrow_wrapped_viewport();
        let buffer_len = state.buffer.len();
        assert!(
            buffer_len <= MAX_WRAP_SCROLLBAR_BYTES,
            "test buffer should trip the line bound, not the byte bound"
        );
        let (total, _) = scrollbar_line_counts(
            &mut state,
            &vp,
            crate::config::LARGE_FILE_THRESHOLD_BYTES,
            buffer_len,
        );
        assert_eq!(
            total, n,
            "large wrapped buffer should use the logical-line count, not wrapped rows"
        );
        assert_eq!(
            state.visual_row_index.line_count(),
            0,
            "large wrapped buffer must not build the O(all-lines) visual-row index"
        );
    }
}
