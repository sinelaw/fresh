//! Pre-frame reconciliation of a text pane.
//!
//! Everything the formatter ([`super::render_buffer::compute_buffer_layout`])
//! used to *write* while a frame was being painted lives here, and runs
//! before the frame's description is painted — once per visible pane, in
//! paint order, with the rectangle the pane will be painted at:
//!
//! 1. the viewport's size and the per-split mirrors (`compose_width`,
//!    `show_line_numbers`), then the byte-oriented `ensure_visible`
//!    ([`sync_viewport_to_content`]);
//! 2. the horizontal-scrollbar clamp against the last known longest line;
//! 3. the buffer's shared margin state, written from this pane's resolved
//!    gutter ([`resolve_gutter_layout`]);
//! 4. the wrap index for the pane's geometry (`ensure_built`) and the
//!    row-space vertical placement (`ensure_visible_in_rows`), which sets
//!    `row_pass_owns_placement` for the byte pass of the next frame;
//! 5. the same-buffer scroll-sync's scroll-to-end, in row space.
//!
//! With these out of the way the formatter is a read of `(state, viewport,
//! rect)` and builds its rows exactly once. The two placement decisions that
//! genuinely need the built rows — the cursor's visual column for horizontal
//! scroll, and the longest visible line for the horizontal scrollbar's bound —
//! are computed by the formatter *as values* and stored by [`settle_pane`]
//! after the pane has painted, which is when the writing form used to store
//! them.
//!
//! **Order is behaviour.** The steps above run in the order the paint path
//! ran them, so every flag that one step sets and a later one reads
//! (`row_pass_owns_placement`, the resize-sync and ensure-visible skips) is
//! read at the same point in the frame it was before.

use super::super::layout::sync_viewport_to_content;
use super::super::scrollbar::{
    compute_max_line_length, MAX_WRAP_SCROLLBAR_BYTES, MAX_WRAP_SCROLLBAR_LINES,
};
use super::render_buffer::{resolve_gutter_layout, wrap_index_geometry_for};
use crate::model::cursor::Cursors;
use crate::state::{EditorState, ViewMode};
use crate::view::folding::FoldManager;
use crate::view::split::BufferViewState;
use crate::view::viewport::{CursorLineExpansion, Viewport};
use ratatui::layout::Rect;

/// What a pane's reconcile needs beyond the view state.
#[derive(Clone, Copy, Debug)]
pub(crate) struct ReconcileInputs {
    /// The rectangle the pane's text is formatted into — the pane's content
    /// rect, after its tab strip and scrollbars.
    pub content_rect: Rect,
    /// A pinned (non-scrollable) panel owns its own content window, so its
    /// buffer viewport stays anchored at the top.
    pub pin_to_top: bool,
    /// Whether the horizontal scrollbar is shown, which is what gates the
    /// `left_column` clamps against the longest line seen.
    pub show_horizontal_scrollbar: bool,
}

/// Reconcile one pane's view state for the frame about to be painted.
///
/// `view` is the split's state for the buffer it shows; `state` is that
/// buffer. Runs the byte pass, the scrollbar clamp, and then [`place_pane`].
pub(crate) fn reconcile_pane(
    state: &mut EditorState,
    view: &mut BufferViewState,
    inputs: ReconcileInputs,
) {
    let _span = tracing::trace_span!("reconcile_pane").entered();
    let BufferViewState {
        viewport,
        cursors,
        folds,
        view_mode,
        compose_width,
        show_line_numbers,
        ..
    } = view;

    // Resolve hidden fold byte ranges so ensure_visible can skip folded
    // lines when counting distance to the cursor.
    let hidden_ranges: Vec<(usize, usize)> = folds
        .resolved_ranges(&state.buffer, &state.marker_list)
        .into_iter()
        .map(|r| (r.start_byte, r.end_byte))
        .collect();

    {
        let _span = tracing::trace_span!("sync_viewport_to_content").entered();
        sync_viewport_to_content(
            viewport,
            &mut state.buffer,
            cursors,
            inputs.content_rect,
            &hidden_ranges,
            *compose_width,
            *show_line_numbers,
            inputs.pin_to_top,
            state.buffer_settings.virtual_space,
        );
    }

    // Use a previously discovered bound so an extra wheel step cannot
    // paint a blank frame before the post-paint scan refreshes it.
    if inputs.show_horizontal_scrollbar
        && !viewport.line_wrap_enabled
        && viewport.max_line_length_seen > 0
    {
        let visible_width = viewport.width as usize;
        let max_scroll = viewport
            .max_line_length_seen
            .max(visible_width)
            .saturating_sub(visible_width);
        viewport.left_column = viewport.left_column.min(max_scroll);
    }

    place_pane(
        state,
        viewport,
        cursors,
        folds,
        view_mode,
        *compose_width,
        *show_line_numbers,
        inputs.content_rect,
    );
}

/// The formatter's half of the reconcile: margins, the wrap index, row-space
/// placement, and the scroll-to-end sync. Everything `compute_buffer_layout`
/// wrote before it built its rows, in the order it wrote it.
///
/// Callers that never ran the byte pass (the overlay preview's phantom leaf)
/// call this directly; everything else comes through [`reconcile_pane`].
#[allow(clippy::too_many_arguments)]
pub(crate) fn place_pane(
    state: &mut EditorState,
    viewport: &mut Viewport,
    cursors: &Cursors,
    folds: &FoldManager,
    view_mode: &ViewMode,
    compose_width: Option<u16>,
    show_line_numbers: bool,
    area: Rect,
) {
    let _span = tracing::trace_span!("place_pane").entered();
    super::super::instrument::count_pane_placement();
    let line_wrap = viewport.line_wrap_enabled;

    // The buffer's shared margin state is what the readers between frames
    // (mouse mapping, `left_total_width`) consult. It holds the last
    // reconciled pane's gutter, as it used to hold the last painted pane's;
    // the frame itself reads each pane's own resolved gutter.
    let gutter = resolve_gutter_layout(
        &state.margins,
        show_line_numbers,
        view_mode,
        area,
        compose_width,
        gutter_estimated_lines(state),
    );
    state.margins.left_config = gutter.margin;

    // This split's cursor byte positions, for cursor-dependent conceal /
    // soft-break activation (evaluated per frame, per split — cursor
    // movement changes what's active without any marker churn).
    let cursor_positions = cursors.positions();

    // Decide the scroll before anything is built. In row space the wrap index
    // answers "which row is the cursor on" directly, so the common case — a
    // cursor that has drifted into the scroll margin — is settled with no rows
    // built at all.
    //
    // Build the index if it is stale, then place. With repair keeping the
    // version current across text edits, a stale index here means a
    // decoration batch arrived — compose's `lines_changed` round-trip — and
    // re-placing against the fresh rows is exactly the re-place-on-arrival
    // trigger the model requires (`test_arrival_without_replacement_loses_
    // the_cursor`): nothing else re-runs placement. Bounded by the
    // scrollbar's size ceilings so a huge file never builds an index just to
    // place the viewport. Folds are in the geometry key, so index rows *are*
    // drawn rows.
    let fold_ranges = state.fold_ranges(folds);
    let geometry = wrap_index_geometry_for(
        viewport,
        &state.buffer,
        line_wrap,
        view_mode,
        crate::view::wrap_index::fold_signature(&fold_ranges),
    );
    let inputs = state.pipeline_inputs();
    let cursor_byte = cursors.primary().position;
    let buffer_len = state.buffer.len();
    // Large-file mode has no line data at all — the gutter is byte-based and
    // `line_count` is byte arithmetic, so an index built here would be one
    // meaningless line and placement against it pins the viewport at the
    // top. The byte pass owns those buffers outright. `line_count()` (an
    // Option, no scan) replaces the earlier `get_line_number(len-1)`, which
    // forced exactly the scan large-file mode exists to avoid.
    let indexable = !state.buffer.is_large_file();
    let within_bounds = indexable
        && buffer_len <= MAX_WRAP_SCROLLBAR_BYTES
        && state
            .buffer
            .line_count()
            .is_some_and(|lc| lc <= MAX_WRAP_SCROLLBAR_LINES);
    let has_index = if within_bounds || (indexable && state.wrap_indices.get(&geometry).is_some()) {
        let line_ending = state.buffer.line_ending();
        // Decorations — virtual-line anchors included — are resolved into
        // one owned snapshot before `entry` takes `&mut state`.
        let decorations = state.index_decorations(geometry.view_mode, fold_ranges.clone(), &[]);
        let index = state.wrap_indices.entry(geometry);
        index.ensure_built(
            &mut state.buffer,
            geometry,
            inputs,
            line_ending,
            &decorations,
        );

        // Cursor-line expansion: the frame draws the cursor's line
        // cursor-aware, so placement must target the row the cursor is
        // *drawn* on and clamp against the rows that will actually exist.
        // Activation scopes are line-local, so this one line is the only
        // possible divergence; everything else stays canonical. Mirrors the
        // model's `EditorModel.ensure_cursor_visible`.
        let cursor_line = state.buffer.get_line_number(cursor_byte);
        let cl_start = state.buffer.line_start_offset(cursor_line).unwrap_or(0);
        let cl_end = state
            .buffer
            .line_start_offset(cursor_line + 1)
            .unwrap_or_else(|| state.buffer.len());
        let divergent = state
            .conceals
            .earliest_cursor_divergence(cl_start, cl_end, &state.marker_list, &cursor_positions)
            .is_some()
            || state
                .soft_breaks
                .earliest_cursor_divergence(cl_start, cl_end, &state.marker_list, &cursor_positions)
                .is_some();
        let expansion = if divergent {
            let canonical = state
                .wrap_indices
                .get(&geometry)
                .and_then(|i| i.line_wrap(cursor_line))
                .filter(|lw| !lw.hidden)
                .map(|lw| lw.wrap_rows());
            let first_row = state
                .wrap_indices
                .get(&geometry)
                .map(|i| i.row_of_byte(&state.buffer, cl_start));
            match (canonical, first_row) {
                (Some(canonical_rows), Some(first_row)) => {
                    let aware = state.index_decorations(
                        geometry.view_mode,
                        state.fold_ranges(folds),
                        &cursor_positions,
                    );
                    let starts = crate::view::wrap_index::line_drawn_row_starts(
                        &mut state.buffer,
                        cursor_line,
                        geometry.rule,
                        line_ending,
                        &aware,
                    );
                    let rel = cursor_byte.saturating_sub(cl_start) as u32;
                    let within = starts.partition_point(|s| *s <= rel).saturating_sub(1);
                    Some(CursorLineExpansion {
                        line_start: cl_start,
                        first_row,
                        canonical_rows: canonical_rows as usize,
                        drawn_rows: starts.len().max(1),
                        cursor_row_drawn: first_row + within as u32,
                    })
                }
                _ => None,
            }
        } else {
            None
        };

        if let Some(index) = state.wrap_indices.get(&geometry) {
            viewport.ensure_visible_in_rows(index, &state.buffer, cursor_byte, expansion.as_ref());
            viewport.row_pass_owns_placement = true;
            true
        } else {
            false
        }
    } else {
        // Beyond the size ceilings with no index built: the byte-oriented
        // pass is the only vertical authority there is.
        viewport.row_pass_owns_placement = false;
        false
    };

    // Same-buffer scroll sync: the sync flagged this viewport to show the end
    // of the document. Decided in row space now, where the formatter used to
    // build the rows to count them and then build them again from the
    // answer.
    if viewport.sync_scroll_to_end {
        viewport.sync_scroll_to_end = false;
        if has_index {
            if let Some(index) = state.wrap_indices.get(&geometry) {
                viewport.scroll_to_end_in_rows(index, &state.buffer);
            }
        } else {
            viewport.scroll_to_end_unindexed(&mut state.buffer);
        }
    }
}

/// Store what the pane's paint decided from its built rows, and refresh the
/// horizontal scrollbar's bound from the lines that were on screen.
///
/// `left_column` is the column the frame was drawn with
/// (`Viewport::layout_column_scroll`); it is stored now, after the paint,
/// exactly when the writing form used to store it. The resize-sync skip is
/// consumed here for the same reason: the layout pass was what consumed it.
///
/// The longest-line scan and its clamp come last because that is where they
/// were: the frame paints with the column the cursor asked for, and the
/// scan's (byte-measured) bound corrects it only for the readers between
/// frames and the next frame's own clamp.
pub(crate) fn settle_pane(
    state: &mut EditorState,
    viewport: &mut Viewport,
    left_column: usize,
    show_horizontal_scrollbar: bool,
) -> usize {
    viewport.left_column = left_column;
    let _ = viewport.should_skip_resize_sync();

    if show_horizontal_scrollbar && !viewport.line_wrap_enabled {
        let mcw = compute_max_line_length(state, viewport);
        // Clamp left_column so content can't scroll past the end of the
        // longest line.
        let visible_width = viewport.width as usize;
        let max_scroll = mcw.saturating_sub(visible_width);
        if viewport.left_column > max_scroll {
            viewport.left_column = max_scroll;
        }
        mcw
    } else {
        0
    }
}

/// The line count the gutter is sized for: the buffer's line count, or its
/// byte length when the gutter shows byte offsets (large-file mode).
pub(crate) fn gutter_estimated_lines(state: &EditorState) -> usize {
    match state.buffer.line_count() {
        Some(lines) => lines,
        // In byte offset mode the gutter shows byte offsets, so size it for
        // the largest byte offset (the file size).
        None => state.buffer.len().max(1),
    }
}
