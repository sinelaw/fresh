//! Scrollbar computation and rendering (vertical, horizontal, composite).
//!
//! These helpers take the editor `State`, viewport, and a few typed
//! parameters. They have no dependency on any shared render-time "mega
//! struct".

use crate::state::EditorState;
use crate::view::scrollbar_marker::{self, MarkerBasis, MarkerCell};
use crate::view::theme::Theme;
use crate::view::viewport::Viewport;
use ratatui::layout::Rect;
use ratatui::style::{Color, Style};
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

/// Compute scrollbar line counts: `(total_lines, top_line)`, plus the
/// coordinate basis those counts are expressed in.
///
/// For large files the counts are reported as `(0, 0)` — the caller uses a
/// constant-size thumb in that case. When line wrapping is enabled, counts are
/// in visual rows instead of logical lines — except on a large wrapped buffer,
/// where the exact visual-row count is too expensive to recompute per edit and
/// we fall back to the logical-line approximation (see the constants above).
///
/// The returned [`MarkerBasis`] is what plugin scrollbar markers are projected
/// through. Deriving it here, from the same branch that picks the thumb's
/// counts, is what keeps markers and thumb in the same coordinate space by
/// construction rather than by convention.
pub(super) fn scrollbar_line_counts(
    state: &mut EditorState,
    viewport: &Viewport,
    large_file_threshold_bytes: u64,
    buffer_len: usize,
) -> (usize, usize, MarkerBasis) {
    if buffer_len > large_file_threshold_bytes as usize {
        // No line coordinate exists here — on a file this size `line_count()`
        // is typically `None` until the incremental scan runs. Bytes are exact
        // and O(1), so markers stay correct on the very first frame.
        return (
            0,
            0,
            MarkerBasis::Bytes {
                total: buffer_len as u64,
            },
        );
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
        let (total_rows, top_row) = scrollbar_visual_row_counts(state, viewport, buffer_len);
        return (
            total_rows,
            top_row,
            MarkerBasis::VisualRows {
                total: total_rows as u64,
            },
        );
    }

    let top_line = if viewport.top_byte < buffer_len {
        state.buffer.get_line_number(viewport.top_byte)
    } else {
        0
    };

    (
        total_lines,
        top_line,
        MarkerBasis::LogicalLines {
            total: total_lines as u64,
        },
    )
}

/// Project this buffer's plugin scrollbar markers onto a `track_height`-tall
/// column, reusing the cached projection when nothing relevant changed.
///
/// Cost per frame is one key comparison in the steady state; a rebuild is
/// O(M) in the marker count with an O(1)–O(log n) coordinate lookup each, and
/// carries no term proportional to file size. The `basis` argument comes from
/// [`scrollbar_line_counts`], so the projection always uses the same
/// coordinate space as the thumb it is drawn beside.
pub(super) fn project_scrollbar_markers(
    state: &mut EditorState,
    basis: MarkerBasis,
    track_height: usize,
) -> Vec<Option<MarkerCell>> {
    if state.scrollbar_markers.is_empty() || track_height == 0 {
        return Vec::new();
    }

    let content_version = state.buffer.version();

    // Split the borrows: the projection reads the manager and the coordinate
    // source while writing the bucket cache.
    let mut buckets = std::mem::take(&mut state.scrollbar_marker_buckets);
    let cells = match basis {
        MarkerBasis::Bytes { .. } => scrollbar_marker::project(
            &state.scrollbar_markers,
            &mut buckets,
            basis,
            track_height,
            content_version,
            |byte| byte as u64,
        )
        .to_vec(),
        MarkerBasis::LogicalLines { .. } => {
            // `get_line_number` needs `&mut Buffer`; markers are read from a
            // snapshot first so the two borrows don't overlap.
            let resolved = state.scrollbar_markers.resolved();
            let mut lines = std::collections::HashMap::with_capacity(resolved.len() * 2);
            for m in &resolved {
                lines
                    .entry(m.start)
                    .or_insert_with(|| state.buffer.get_line_number(m.start) as u64);
                if let Some(e) = m.end {
                    lines
                        .entry(e)
                        .or_insert_with(|| state.buffer.get_line_number(e) as u64);
                }
            }
            scrollbar_marker::project(
                &state.scrollbar_markers,
                &mut buckets,
                basis,
                track_height,
                content_version,
                |byte| lines.get(&byte).copied().unwrap_or(0),
            )
            .to_vec()
        }
        MarkerBasis::VisualRows { .. } => {
            // The wrap index was built by `scrollbar_visual_row_counts` for
            // this same frame and geometry; this path only reads it. Markers
            // project onto a line's *first* row so a marker on a wrapped line
            // lands at the line's start, not inside it.
            let index = state
                .wrap_indices
                .most_recent()
                .expect("visual-row basis implies a built wrap index");
            let buffer = &state.buffer;
            scrollbar_marker::project(
                &state.scrollbar_markers,
                &mut buckets,
                basis,
                track_height,
                content_version,
                |byte| index.line_first_row(buffer.get_line_number(byte)) as u64,
            )
            .to_vec()
        }
    };
    state.scrollbar_marker_buckets = buckets;
    cells
}

/// Calculate scrollbar position based on visual rows (for line-wrapped content).
/// Returns `(total_visual_rows, top_visual_row)`.
///
/// Both numbers come from the per-state [`WrapIndex`] in O(log N_lines).
/// The index is built lazily and reused across frames whenever its key
/// (pipeline-input version + geometry) is unchanged — so a steady-state
/// scroll where only `top_byte` moves never re-walks the buffer.
///
/// [`WrapIndex`]: crate::view::wrap_index::WrapIndex
pub(super) fn scrollbar_visual_row_counts(
    state: &mut EditorState,
    viewport: &Viewport,
    buffer_len: usize,
) -> (usize, usize) {
    use crate::primitives::line_wrapping::WrapConfig;
    use crate::view::line_wrap_cache::{pipeline_inputs_version, CacheViewMode};
    use crate::view::ui::split_rendering::MAX_SAFE_LINE_WIDTH;
    use crate::view::wrap_index::WrapIndexGeometry;
    use crate::view::wrap_machine::WrapRule;

    if buffer_len == 0 {
        return (1, 0);
    }

    // Terminal-grid wrap (fresh#2649): count exact-column rows at the grid
    // width — same row model as the renderer and the viewport scroll math.
    let (effective_width, gutter_width, hanging_indent) = if viewport.grid_wrap {
        (viewport.grid_cols(), 0usize, false)
    } else {
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
        (effective_width, gutter_width, wrap_config.hanging_indent)
    };
    let pipeline_inputs_ver = pipeline_inputs_version(
        state.buffer.version(),
        state.soft_breaks.version(),
        state.conceals.version(),
        state.virtual_texts.version(),
    );

    // The wrap index answers both numbers directly: `total_rows` is O(1) off the
    // Fenwick tree and `row_of_byte` is a binary search. The path this replaced
    // re-wrapped every logical line on every keystroke, because its key folded in
    // the buffer version — 16.9% of a frame on a single-line file.
    let geometry = WrapIndexGeometry {
        rule: if viewport.grid_wrap {
            WrapRule::Grid {
                cols: effective_width,
            }
        } else if viewport.line_wrap_enabled {
            WrapRule::Word {
                content_width: effective_width,
                gutter_width,
                hanging_indent,
            }
        } else {
            // Without soft wrap every logical line is one visual row until the
            // safety chop, which no practical line reaches.
            WrapRule::Chop {
                chars: MAX_SAFE_LINE_WIDTH,
            }
        },
        view_mode: CacheViewMode::Source,
    };

    // Snapshot virtual-line anchors so the per-line lookup borrows this list
    // rather than `state`, whose buffer the build holds mutably.
    let virtual_positions: Vec<usize> = if state.virtual_texts.is_empty() {
        Vec::new()
    } else {
        let mut v: Vec<usize> = state
            .virtual_texts
            .query_lines_in_range(&state.marker_list, 0, buffer_len + 1)
            .into_iter()
            .map(|(pos, _)| pos)
            .collect();
        v.sort_unstable();
        v
    };
    let virtual_rows = |start: usize, end: usize| -> u32 {
        let lo = virtual_positions.partition_point(|p| *p < start);
        let hi = virtual_positions.partition_point(|p| *p < end);
        (hi - lo) as u32
    };

    let line_ending = state.buffer.line_ending();
    let index = state.wrap_indices.entry(geometry);
    index.ensure_built(
        &mut state.buffer,
        geometry,
        pipeline_inputs_ver,
        line_ending,
        &virtual_rows,
    );

    let total_visual_rows = index.total_rows() as usize;
    let top_visual_row = index.row_of_byte(&state.buffer, viewport.top_byte) as usize
        + viewport.top_view_line_offset;
    let top_visual_row = top_visual_row.min(total_visual_rows.saturating_sub(1));

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

/// Resolve a marker's colour spec against the live theme.
///
/// Theme keys are resolved here, at paint time, rather than when the plugin
/// sets the marker — so markers follow a theme switch with no invalidation.
fn resolve_marker_color(spec: &fresh_core::api::OverlayColorSpec, theme: &Theme) -> Color {
    match spec {
        fresh_core::api::OverlayColorSpec::Rgb(r, g, b) => Color::Rgb(*r, *g, *b),
        fresh_core::api::OverlayColorSpec::ThemeKey(key) => {
            crate::view::theme::named_color_from_str(key)
                .or_else(|| theme.resolve_theme_key(key))
                .unwrap_or(Color::Reset)
        }
    }
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
    markers: &[Option<MarkerCell>],
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

        let bg = if row >= thumb_start && row < thumb_end {
            thumb_color
        } else {
            track_color
        };

        // A marker paints a half-block glyph in its colour over the track or
        // thumb background. The scrollbar is a single column
        // (`split_rendering::layout`), so a solid fill would have to choose
        // between showing the mark and showing the scroll position; the half
        // block shows both in the same cell.
        let paragraph = match markers.get(row).and_then(|m| m.as_ref()) {
            Some(marker) => Paragraph::new(MARKER_GLYPH).style(
                Style::default()
                    .fg(resolve_marker_color(&marker.color, theme))
                    .bg(bg),
            ),
            None => Paragraph::new(" ").style(Style::default().bg(bg)),
        };
        paragraph.render(cell_area, buf);
    }

    (thumb_start, thumb_end)
}

/// Glyph used for a plugin scrollbar marker: a left half block, so the
/// marker colour and the underlying track/thumb background are both visible.
pub(super) const MARKER_GLYPH: &str = "▌";

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
        let (total, _, basis) = scrollbar_line_counts(
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
            !state.wrap_indices.is_empty(),
            "small wrapped buffer should build a wrap index for this geometry"
        );
        assert!(
            matches!(basis, MarkerBasis::VisualRows { .. }),
            "markers must follow the thumb onto the wrapped-row basis, got {basis:?}"
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
        let (total, _, basis) = scrollbar_line_counts(
            &mut state,
            &vp,
            crate::config::LARGE_FILE_THRESHOLD_BYTES,
            buffer_len,
        );
        assert_eq!(
            total, n,
            "large wrapped buffer should use the logical-line count, not wrapped rows"
        );
        assert!(
            state.wrap_indices.is_empty(),
            "large wrapped buffer must not build the O(all-lines) wrap index"
        );
        assert!(
            matches!(basis, MarkerBasis::LogicalLines { total } if total == n as u64),
            "markers must follow the thumb onto the logical-line basis, got {basis:?}"
        );
    }

    /// The marker basis is derived in the same branch that picks the thumb's
    /// counts, so the two can never end up in different coordinate spaces.
    #[test]
    fn marker_basis_matches_the_thumb_basis_in_every_regime() {
        // Exact wrapped rows.
        let mut state = state_with_wrapping_lines(100);
        let vp = narrow_wrapped_viewport();
        let len = state.buffer.len();
        let (total, _, basis) = scrollbar_line_counts(
            &mut state,
            &vp,
            crate::config::LARGE_FILE_THRESHOLD_BYTES,
            len,
        );
        assert_eq!(
            basis,
            MarkerBasis::VisualRows {
                total: total as u64
            }
        );

        // Logical lines (wrap off).
        let mut state = state_with_wrapping_lines(100);
        let vp = Viewport::new(40, 24);
        let len = state.buffer.len();
        let (total, _, basis) = scrollbar_line_counts(
            &mut state,
            &vp,
            crate::config::LARGE_FILE_THRESHOLD_BYTES,
            len,
        );
        assert_eq!(
            basis,
            MarkerBasis::LogicalLines {
                total: total as u64
            }
        );

        // Bytes: over the large-file threshold, where no line coordinate is
        // guaranteed to exist. A tiny threshold stands in for a huge file.
        let mut state = state_with_wrapping_lines(100);
        let len = state.buffer.len();
        let (total, top, basis) = scrollbar_line_counts(&mut state, &vp, 16, len);
        assert_eq!((total, top), (0, 0), "large files use the constant thumb");
        assert_eq!(basis, MarkerBasis::Bytes { total: len as u64 });
    }

    /// The byte regime must not consult line APIs at all — that is what lets
    /// markers render on the first frame of a huge, unscanned file.
    #[test]
    fn byte_regime_projection_does_not_build_the_visual_row_index() {
        use crate::view::scrollbar_marker::ResolvedMarker;

        let mut state = state_with_wrapping_lines(100);
        let len = state.buffer.len();
        state.scrollbar_markers.set_markers(
            "test",
            vec![ResolvedMarker {
                start: len / 2,
                end: None,
                color: fresh_core::api::OverlayColorSpec::Rgb(1, 2, 3),
                priority: 0,
            }],
        );

        let cells =
            project_scrollbar_markers(&mut state, MarkerBasis::Bytes { total: len as u64 }, 20);
        assert!(
            cells.iter().any(|c| c.is_some()),
            "the marker should land somewhere on the track"
        );
        assert!(
            state.wrap_indices.is_empty(),
            "byte-basis projection must not build a wrap index"
        );
    }

    /// A buffer with no markers must not allocate or project anything.
    #[test]
    fn no_markers_means_no_projection_work() {
        let mut state = state_with_wrapping_lines(10);
        let cells = project_scrollbar_markers(&mut state, MarkerBasis::Bytes { total: 100 }, 20);
        assert!(cells.is_empty());
    }
}
