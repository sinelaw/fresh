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
use ratatui::Frame;

/// Compute scrollbar line counts: `(total_lines, top_line)`.
///
/// For large files the counts are reported as `(0, 0)` — the caller uses a
/// constant-size thumb in that case. When line wrapping is enabled, counts
/// are in visual rows instead of logical lines.
pub(super) fn scrollbar_line_counts(
    state: &mut EditorState,
    viewport: &Viewport,
    large_file_threshold_bytes: u64,
    buffer_len: usize,
) -> (usize, usize) {
    if buffer_len > large_file_threshold_bytes as usize {
        return (0, 0);
    }

    if viewport.line_wrap_enabled {
        return scrollbar_visual_row_counts(state, viewport, buffer_len);
    }

    let total_lines = if buffer_len > 0 {
        state.buffer.get_line_number(buffer_len.saturating_sub(1)) + 1
    } else {
        1
    };

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
/// Uses a cache to avoid re-wrapping every line on each frame. The cache is
/// invalidated when the buffer version, viewport width, or wrap settings
/// change. When only top_byte changes (scrolling), the cached
/// `total_visual_rows` is reused and only the `top_visual_row` is recomputed.
pub(super) fn scrollbar_visual_row_counts(
    state: &mut EditorState,
    viewport: &Viewport,
    buffer_len: usize,
) -> (usize, usize) {
    use crate::primitives::line_wrapping::WrapConfig;
    use crate::view::line_wrap_cache::{layout_for_plain_text, pipeline_inputs_version, CacheViewMode, LineWrapKey};
    use std::sync::Arc;

    if buffer_len == 0 {
        return (1, 0);
    }

    let buf_version = state.buffer.version();
    let cache = &state.scrollbar_row_cache;

    let cache_fully_valid = cache.valid
        && cache.buffer_version == buf_version
        && cache.viewport_width == viewport.width
        && cache.wrap_indent == viewport.wrap_indent
        && cache.top_byte == viewport.top_byte
        && cache.top_view_line_offset == viewport.top_view_line_offset;

    if cache_fully_valid {
        return (cache.total_visual_rows, cache.top_visual_row);
    }

    let total_rows_valid = cache.valid
        && cache.buffer_version == buf_version
        && cache.viewport_width == viewport.width
        && cache.wrap_indent == viewport.wrap_indent;

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
    let tab_size = state.buffer_settings.tab_size;
    let pipeline_inputs_ver = pipeline_inputs_version(
        state.buffer.version(),
        state.soft_breaks.version(),
        state.conceals.version(),
    );
    // Build the LineWrapKey template used for each line — only
    // `line_start` varies across lines.
    let make_key = |line_start: usize| LineWrapKey {
        pipeline_inputs_version: pipeline_inputs_ver,
        view_mode: CacheViewMode::Source,
        line_start,
        effective_width: effective_width as u32,
        gutter_width: gutter_width as u16,
        wrap_column: None,
        hanging_indent,
        line_wrap_enabled: viewport.line_wrap_enabled,
    };

    let line_count = state
        .buffer
        .line_count()
        .unwrap_or_else(|| (buffer_len / state.buffer.estimated_line_length()).max(1));

    // Helper: per-line row count.  Goes through the shared
    // LineWrapCache so the result matches the renderer's word-boundary
    // wrap.  On miss, runs layout_for_plain_text (no soft-breaks /
    // conceals — matching the OLD wrap_line behavior here; we keep
    // that gap documented rather than quietly widening it).
    fn row_count_for_line(
        state: &mut EditorState,
        line_idx: usize,
        buffer_len: usize,
        effective_width: usize,
        gutter_width: usize,
        hanging_indent: bool,
        tab_size: usize,
        make_key: &impl Fn(usize) -> LineWrapKey,
    ) -> (usize, usize) {
        // Returns (line_start, row_count).
        let line_start = state
            .buffer
            .line_start_offset(line_idx)
            .unwrap_or(buffer_len);
        let key = make_key(line_start);
        if let Some(cached) = state.line_wrap_cache.get(&key) {
            return (line_start, cached.len().max(1));
        }
        let Some(bytes) = state.buffer.get_line(line_idx) else {
            return (line_start, 0);
        };
        let line_content = String::from_utf8_lossy(&bytes)
            .trim_end_matches('\n')
            .trim_end_matches('\r')
            .to_string();
        let layout = layout_for_plain_text(
            &line_content,
            effective_width,
            gutter_width,
            hanging_indent,
            tab_size,
        );
        let rows = layout.len().max(1);
        state.line_wrap_cache.put(key, Arc::new(layout));
        (line_start, rows)
    }

    let mut total_visual_rows = 0;
    let mut top_visual_row = 0;
    let mut found_top = false;

    if total_rows_valid {
        total_visual_rows = cache.total_visual_rows;
        let mut rows_before_top = 0;
        for line_idx in 0..line_count {
            let (line_start, rows) = row_count_for_line(
                state,
                line_idx,
                buffer_len,
                effective_width,
                gutter_width,
                hanging_indent,
                tab_size,
                &make_key,
            );
            if line_start >= viewport.top_byte {
                top_visual_row = rows_before_top + viewport.top_view_line_offset;
                found_top = true;
                break;
            }
            if rows == 0 {
                break;
            }
            rows_before_top += rows;
        }

        if !found_top {
            top_visual_row = total_visual_rows.saturating_sub(1);
        }
    } else {
        for line_idx in 0..line_count {
            let (line_start, rows) = row_count_for_line(
                state,
                line_idx,
                buffer_len,
                effective_width,
                gutter_width,
                hanging_indent,
                tab_size,
                &make_key,
            );
            if !found_top && line_start >= viewport.top_byte {
                top_visual_row = total_visual_rows + viewport.top_view_line_offset;
                found_top = true;
            }
            if rows == 0 {
                break;
            }
            total_visual_rows += rows;
        }

        if !found_top {
            top_visual_row = total_visual_rows.saturating_sub(1);
        }

        total_visual_rows = total_visual_rows.max(1);
    }

    state.scrollbar_row_cache = crate::state::ScrollbarRowCache {
        buffer_version: buf_version,
        viewport_width: viewport.width,
        wrap_indent: viewport.wrap_indent,
        total_visual_rows,
        top_byte: viewport.top_byte,
        top_visual_row,
        top_view_line_offset: viewport.top_view_line_offset,
        valid: true,
    };

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
    frame: &mut Frame,
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
        frame.render_widget(paragraph, cell_area);
    }

    (thumb_start, thumb_end)
}

/// Render a horizontal scrollbar for a split.
/// `max_content_width` should be the actual max line length
/// (from [`compute_max_line_length`]).
/// Returns (thumb_start_col, thumb_end_col) for mouse hit testing.
pub(super) fn render_horizontal_scrollbar(
    frame: &mut Frame,
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
            frame.render_widget(paragraph, cell_area);
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
        frame.render_widget(paragraph, cell_area);
    }

    (thumb_start, thumb_end)
}

/// Render a scrollbar for composite buffer views.
pub(super) fn render_composite_scrollbar(
    frame: &mut Frame,
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
        frame.render_widget(paragraph, cell_area);
    }

    (thumb_start, thumb_end)
}
