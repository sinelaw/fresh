//! Scrollbar computation and rendering (vertical, horizontal, composite).
//!
//! These helpers take the editor `State`, viewport, and a few typed
//! parameters. They have no dependency on any shared render-time "mega
//! struct".

use crate::state::EditorState;
use crate::view::scrollbar_marker::{self, MarkerBasis};
use crate::view::theme::Theme;
use crate::view::viewport::Viewport;
use ratatui::style::Color;

/// Above either bound, the exact wrapped-row scrollbar is skipped in favour of
/// the cheap logical-line approximation. The exact counts require word-wrapping
/// every line (`ensure_built`, O(all-lines)) and the index is rebuilt whenever
/// the buffer version changes — i.e. on every edit — so on a large wrapped
/// buffer each keystroke re-walks the whole buffer and stalls the UI
/// (fresh#2610). Both bounds are well under the 10 MB large-file threshold
/// because the per-line word-wrap cost is much higher than a raw byte scan.
/// Line count is the loop-iteration driver; the byte bound catches buffers with
/// few but very long lines.
pub(crate) const MAX_WRAP_SCROLLBAR_LINES: usize = 5_000;
pub(crate) const MAX_WRAP_SCROLLBAR_BYTES: usize = 2 * 1024 * 1024;

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
pub(crate) fn scrollbar_line_counts(
    state: &mut EditorState,
    viewport: &Viewport,
    large_file_threshold_bytes: u64,
    buffer_len: usize,
    fold_ranges: Vec<std::ops::Range<usize>>,
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
        let (total_rows, top_row) =
            scrollbar_visual_row_counts(state, viewport, buffer_len, fold_ranges);
        return (
            total_rows,
            top_row,
            MarkerBasis::VisualRows {
                total: total_rows as u64,
            },
        );
    }

    let top_line = if viewport.top_byte() < buffer_len {
        state.buffer.get_line_number(viewport.top_byte())
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

/// The editor's own contribution to the track: one mark per unsaved-change
/// range, in the gutter bar's colour and priority.
///
/// `diff_since_saved` already returns whole-buffer byte ranges — the gutter
/// only clips them to the viewport afterwards — so the scrollbar can show
/// changes that are scrolled off screen without any extra diffing beyond the
/// call itself. Returns `None` when the buffer matches what was saved, which
/// is the `!modified` fast path inside the diff.
fn unsaved_change_marks(
    state: &EditorState,
    track_height: usize,
) -> Option<scrollbar_marker::CoreMarks> {
    use crate::view::ui::split_rendering::folding::{UNSAVED_CHANGE_FG, UNSAVED_CHANGE_PRIORITY};

    let diff = state.buffer.diff_since_saved();
    if diff.equal || diff.byte_ranges.is_empty() {
        return None;
    }
    let ranges = coalesce_within_a_cell(diff.byte_ranges, state.buffer.len(), track_height);

    let Color::Rgb(r, g, b) = UNSAVED_CHANGE_FG else {
        // The constant is an RGB literal; a non-RGB variant would have no
        // `OverlayColorSpec` spelling and is not reachable.
        return None;
    };

    Some(scrollbar_marker::CoreMarks {
        ranges,
        color: fresh_core::api::OverlayColorSpec::Rgb(r, g, b),
        priority: UNSAVED_CHANGE_PRIORITY,
    })
}

/// Merge ranges separated by less than one track cell's worth of bytes.
///
/// `diff_since_saved` can return thousands of ranges — a replace-all across a
/// large file, before saving, produces one per hit. Projecting each costs two
/// coordinate lookups, and `get_line_number` is a O(log n) tree descent
/// (~0.5 µs measured), so 2 000 raw ranges would put milliseconds on the
/// *typing* path to draw marks that a ≤200-cell track cannot tell apart.
///
/// Merging first bounds the projection at roughly two lookups per track row,
/// whatever the edit count, and cannot lose a mark: a merged group spans every
/// range inside it, so the streak painted is a superset of the true one, never
/// a subset. That makes it a resolution choice rather than a silent cap.
///
/// Input from the diff is ascending and disjoint; out-of-order input would
/// merely merge less, never mis-paint.
fn coalesce_within_a_cell(
    ranges: Vec<std::ops::Range<usize>>,
    buffer_len: usize,
    track_height: usize,
) -> Vec<std::ops::Range<usize>> {
    let cell_bytes = (buffer_len / track_height.max(1)).max(1);
    let mut out: Vec<std::ops::Range<usize>> = Vec::new();
    for r in ranges {
        match out.last_mut() {
            Some(last) if r.start <= last.end.saturating_add(cell_bytes) => {
                last.end = last.end.max(r.end);
            }
            _ => out.push(r),
        }
    }
    out
}

/// The buffer's scrollbar marks — the plugin markers and the editor's own
/// unsaved-change ranges — resolved to rows of `basis`, for a bar to bucket
/// onto its own track (design §3.7.6, L8).
///
/// The half of a projection that needs the buffer, cached: one key
/// comparison in the steady state, a rebuild only when the markers, the
/// content or the save state changed. The track is not part of the key —
/// that is the point — so a resize costs no rebuild, and the bar's leaf
/// buckets the same rows onto whatever track layout gives it
/// (`scrollbar_marker::bucket`).
pub(crate) fn resolve_scrollbar_marks(
    state: &mut EditorState,
    basis: MarkerBasis,
) -> std::rc::Rc<[scrollbar_marker::RowMark]> {
    if state.scrollbar_markers.is_empty() && !state.buffer.is_modified() {
        return std::rc::Rc::from(Vec::new());
    }
    let key = scrollbar_marker::ProjectionKey::new(
        &state.scrollbar_markers,
        state.buffer.version(),
        state.buffer.save_state_version(),
        basis,
    );
    if let Some(rows) = state.scrollbar_marker_buckets.cached_rows(&key) {
        return rows;
    }
    // The core ranges coalesce within a cell of the track; with no track
    // here they coalesce within a row of the basis, which loses nothing a
    // track can show.
    let core = unsaved_change_marks(state, basis.total().max(1) as usize);
    let rows: Vec<scrollbar_marker::RowMark> = match basis {
        MarkerBasis::Bytes { .. } => {
            scrollbar_marker::resolve_rows(&state.scrollbar_markers, core.as_ref(), basis, |b| {
                b as u64
            })
        }
        MarkerBasis::LogicalLines { .. } => {
            let resolved = state.scrollbar_markers.resolved();
            let core_bytes = core
                .iter()
                .flat_map(|c| c.endpoints())
                .flat_map(|(s, e)| [s, e]);
            let marker_bytes = resolved
                .iter()
                .flat_map(|m| std::iter::once(m.start).chain(m.end));
            let mut lines = std::collections::HashMap::with_capacity(resolved.len() * 2);
            for byte in core_bytes.chain(marker_bytes).collect::<Vec<_>>() {
                lines
                    .entry(byte)
                    .or_insert_with(|| state.buffer.get_line_number(byte) as u64);
            }
            scrollbar_marker::resolve_rows(&state.scrollbar_markers, core.as_ref(), basis, |b| {
                lines.get(&b).copied().unwrap_or(0)
            })
        }
        MarkerBasis::VisualRows { .. } => {
            let index = state
                .wrap_indices
                .most_recent()
                .expect("visual-row basis implies a built wrap index");
            let buffer = &state.buffer;
            scrollbar_marker::resolve_rows(&state.scrollbar_markers, core.as_ref(), basis, |b| {
                index.line_first_row(buffer.get_line_number(b)) as u64
            })
        }
    };
    let rows: std::rc::Rc<[scrollbar_marker::RowMark]> = std::rc::Rc::from(rows);
    // This is the projection's rebuild — the walk over every mark that
    // `ProjectionStats` counts — so the scroll-perf tests measure this path.
    state
        .scrollbar_marker_buckets
        .note_rebuild(state.scrollbar_markers.len() as u64);
    state.scrollbar_marker_buckets.cache_rows(key, rows.clone());
    rows
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
    fold_ranges: Vec<std::ops::Range<usize>>,
) -> (usize, usize) {
    use crate::primitives::line_wrapping::WrapConfig;
    use crate::view::line_wrap_cache::CacheViewMode;
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
    let inputs = state.pipeline_inputs();

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
        fold_signature: crate::view::wrap_index::fold_signature(&fold_ranges),
    };

    let line_ending = state.buffer.line_ending();
    // Decorations — virtual-line anchors included — resolved into one owned
    // snapshot before `entry` takes `&mut state`.
    let decorations = state.index_decorations(geometry.view_mode, fold_ranges, &[]);
    let index = state.wrap_indices.entry(geometry);
    index.ensure_built(
        &mut state.buffer,
        geometry,
        inputs,
        line_ending,
        &decorations,
    );

    let total_visual_rows = index.total_rows() as usize;
    let top_visual_row = index.row_of_byte(&state.buffer, viewport.top_byte()) as usize
        + viewport.top_view_line_offset();
    let top_visual_row = top_visual_row.min(total_visual_rows.saturating_sub(1));

    (total_visual_rows, top_visual_row)
}

/// Compute the maximum line length encountered so far (in display columns).
/// Only scans the currently visible lines (plus a small margin) and updates
/// the running maximum stored in the viewport.
pub(crate) fn compute_max_line_length(state: &mut EditorState, viewport: &mut Viewport) -> usize {
    let buffer_len = state.buffer.len();
    let visible_width = viewport.width as usize;

    if buffer_len == 0 {
        return viewport.max_line_length_seen.max(visible_width);
    }

    let visible_lines = viewport.height as usize + 5;
    let mut lines_scanned = 0usize;
    let mut iter = state.buffer.line_iterator(viewport.top_byte(), 80);
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
pub(crate) fn resolve_marker_color(
    spec: &fresh_core::api::OverlayColorSpec,
    theme: &Theme,
) -> Color {
    match spec {
        fresh_core::api::OverlayColorSpec::Rgb(r, g, b) => Color::Rgb(*r, *g, *b),
        fresh_core::api::OverlayColorSpec::ThemeKey(key) => {
            crate::view::theme::named_color_from_str(key)
                .or_else(|| theme.resolve_theme_key(key))
                .unwrap_or(Color::Reset)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    /// The marks resolved and bucketed onto a track — what the bar's leaf
    /// paints from, in one call.
    fn project(
        state: &mut EditorState,
        basis: MarkerBasis,
        track_height: usize,
    ) -> Vec<Option<scrollbar_marker::MarkerCell>> {
        let rows = resolve_scrollbar_marks(state, basis);
        scrollbar_marker::bucket(&rows, basis.total(), track_height)
    }

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
        // Stand in for a file just opened from disk: content present, nothing
        // unsaved. Without this the buffer would carry an unsaved-change diff
        // covering all of it, and every projection here would start with the
        // editor's own marks already on the track.
        state.buffer.mark_saved_snapshot();
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
            Vec::new(),
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
            Vec::new(),
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
            Vec::new(),
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
            Vec::new(),
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
        let (total, top, basis) = scrollbar_line_counts(&mut state, &vp, 16, len, Vec::new());
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

        let cells = project(&mut state, MarkerBasis::Bytes { total: len as u64 }, 20);
        assert!(
            cells.iter().any(|c| c.is_some()),
            "the marker should land somewhere on the track"
        );
        assert!(
            state.wrap_indices.is_empty(),
            "byte-basis projection must not build a wrap index"
        );
    }

    /// A saved buffer with no markers must not allocate or project anything.
    #[test]
    fn no_markers_means_no_projection_work() {
        let mut state = state_with_wrapping_lines(10);
        let cells = project(&mut state, MarkerBasis::Bytes { total: 100 }, 20);
        assert!(cells.iter().all(Option::is_none));
    }

    /// ...but an unsaved edit is a mark source of its own, so the same buffer
    /// with no plugin markers at all still paints. This is what makes the
    /// gutter's blue bar visible for changes scrolled off screen.
    #[test]
    fn an_unsaved_edit_projects_without_any_plugin_markers() {
        let mut state = state_with_wrapping_lines(10);
        let len = state.buffer.len();
        state.buffer.insert(len / 2, "an unsaved edit");

        let total = state.buffer.len() as u64;
        let cells = project(&mut state, MarkerBasis::Bytes { total }, 20);
        let marked: Vec<usize> = cells
            .iter()
            .enumerate()
            .filter(|(_, c)| c.is_some())
            .map(|(i, _)| i)
            .collect();
        // The structure diff reports leaf-granular ranges, so an insertion can
        // cover a little more than the typed bytes — the mark is a streak
        // around the edit, not necessarily a single row.
        assert!(
            !marked.is_empty() && marked.iter().all(|r| (8..=11).contains(r)),
            "the edit is halfway through the buffer, so it marks the middle of \
             a 20-row track; got {marked:?}"
        );

        // Saving it away clears the marks without any plugin involvement.
        state.buffer.mark_saved_snapshot();
        let cells = project(&mut state, MarkerBasis::Bytes { total }, 20);
        assert!(
            cells.iter().all(Option::is_none),
            "a saved buffer leaves the track clean"
        );
    }

    /// Thousands of scattered unsaved edits must not put thousands of
    /// coordinate lookups on the typing path: ranges within one track cell of
    /// each other merge first, so the projection stays bounded by the track.
    #[test]
    fn dense_unsaved_edits_coalesce_to_the_track_resolution() {
        let ranges: Vec<std::ops::Range<usize>> =
            (0..2_000).map(|k| (k * 50)..(k * 50 + 1)).collect();
        // 100 000 bytes over a 20-row track: one cell is 5 000 bytes, so the
        // 2 000 single-byte ranges collapse to one group per cell.
        let merged = coalesce_within_a_cell(ranges, 100_000, 20);
        assert!(
            merged.len() <= 21,
            "expected at most one group per track row, got {}",
            merged.len()
        );
        assert_eq!(merged.first().unwrap().start, 0);
        assert_eq!(
            merged.last().unwrap().end,
            1_999 * 50 + 1,
            "merging must still cover the last edit"
        );
    }
}
