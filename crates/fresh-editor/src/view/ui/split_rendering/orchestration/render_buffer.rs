//! Per-buffer render orchestration.
//!
//! Three functions compose here:
//! - [`compute_buffer_layout`] — pure layout phase (no drawing). A read of
//!   the pane's state, viewport and rect; the writes that used to precede
//!   the build (placement, margins, the wrap index) run before the frame,
//!   in [`super::reconcile`].
//! - [`draw_buffer_in_split`] — drawing phase from a `BufferLayoutOutput`.
//! - [`render_buffer_in_split`] — the two phases combined, the API used by
//!   the top-level `render_content`.

use super::super::folding::fold_adjusted_visible_count;
use super::super::gutter::render_compose_margins;
use super::super::layout::{
    calculate_compose_layout, calculate_view_anchor, calculate_viewport_end, visible_source_span,
    ComposeLayout,
};
use super::super::post_pass::{
    apply_background_to_lines, render_column_guides, tint_columns_in_lines,
};
use super::super::view_data::build_view_data;
use super::super::view_data::BuildAnchor;
use super::contexts::SelectionContext;
use super::overlays::{decoration_context, selection_context};
use super::render_line::{render_view_lines, LastLineEnd, LineRenderInput, LineRenderOutput};
use crate::app::types::{CellThemeInfo, ViewLineMapping};
use crate::config::IndentationGuideMode;
use crate::model::cursor::Cursors;
use crate::primitives::ansi_background::AnsiBackground;
use crate::state::{EditorState, ViewMode};
use crate::view::bracket_highlight_overlay::BracketHighlightSettings;
use crate::view::folding::FoldManager;
use crate::view::theme::Theme;
use crate::view::viewport::Viewport;
use ratatui::layout::Rect;
use ratatui::style::{Color, Modifier, Style};
use ratatui::widgets::Widget;
use ratatui::widgets::{Block, Borders, Clear, Paragraph};

/// Output of the pure layout computation phase of buffer rendering.
///
/// Contains everything the drawing phase needs to produce the final frame.
pub(crate) struct BufferLayoutOutput {
    pub view_line_mappings: Vec<ViewLineMapping>,
    pub render_output: LineRenderOutput,
    pub render_area: Rect,
    pub compose_layout: ComposeLayout,
    pub effective_editor_bg: Color,
    pub view_mode: ViewMode,
    /// The horizontal scroll the rows were laid out with — the viewport's
    /// column after this frame's cursor-column check, which the pane's paint
    /// stores back once it has drawn.
    pub left_column: usize,
    pub gutter_width: usize,
    pub buffer_ends_with_newline: bool,
    pub selection: SelectionContext,
}

/// The gutter one pane draws this frame.
///
/// Resolved from the pane's own line-number setting and view mode without
/// writing the buffer's shared `MarginManager`: the margin state is per
/// buffer, the setting is per split, and two panes on one buffer can want two
/// gutters in one frame.
pub(crate) struct GutterLayout {
    /// The left margin as the row renderer should draw it.
    pub margin: crate::view::margin::MarginConfig,
    /// `margin.total_width()`, after the compose-mode reclaim below.
    pub width: usize,
    /// The compose layout, with the desk margin narrowed by the gutter when
    /// there is room for it.
    pub compose: ComposeLayout,
}

/// Resolve [`GutterLayout`] for a pane. Pure: the same inputs give the same
/// gutter whether this runs in the pre-frame reconcile or in the formatter.
pub(crate) fn resolve_gutter_layout(
    margins: &crate::view::margin::MarginManager,
    show_line_numbers: bool,
    view_mode: &ViewMode,
    area: Rect,
    compose_width: Option<u16>,
    estimated_lines: usize,
    diff_gutter_width: Option<usize>,
) -> GutterLayout {
    let mut margin = margins.resolved_left_config(show_line_numbers, estimated_lines);
    if let Some(width) = diff_gutter_width {
        // A diff stream numbers its rows from their hunk headers, whatever
        // the pane's line-number setting says.
        margin.enabled = true;
        margin.show_separator = true;
        margin.width = width;
    } else if !show_line_numbers && !matches!(view_mode, ViewMode::PageView) {
        // The diagnostic/indicator gutter is kept when line numbers are off only in
        // compose mode, where the render below reclaims its width from the desk
        // margin (issue #2146). In normal editor mode, line-numbers-off means no
        // gutter at all — otherwise the 1-col indicator slot would eat into the
        // text width and shift content right.
        margin.enabled = false;
        margin.width = 0;
    }
    let mut width = margin.total_width();

    let mut compose = calculate_compose_layout(area, view_mode, compose_width);
    // In compose mode the gutter (diagnostic / indicator slot) is drawn in the
    // reclaimed desk margin so it does not shrink the centered text width
    // (issue #2146). Only do this when there is enough desk margin to give up;
    // if the paper already fills the area, drop the gutter instead of eating
    // into the text so table/wrap layout stays intact.
    if matches!(view_mode, ViewMode::PageView) && width > 0 {
        let g = width as u16;
        if compose.left_pad >= g {
            compose.left_pad -= g;
            let ra = compose.render_area;
            compose.render_area = Rect::new(ra.x - g, ra.y, ra.width + g, ra.height);
        } else {
            margin.enabled = false;
            margin.width = 0;
            width = 0;
        }
    }
    GutterLayout {
        margin,
        width,
        compose,
    }
}

/// Resolve the cursor position for the common "past end of buffer" edge
/// case. Returns the input `current_cursor` unchanged if it is already
/// `Some(_)` or the primary cursor isn't at buffer end.
pub(crate) fn resolve_cursor_fallback(
    current_cursor: Option<(u16, u16)>,
    primary_cursor_position: usize,
    buffer_len: usize,
    buffer_ends_with_newline: bool,
    last_line_end: Option<LastLineEnd>,
    lines_rendered: usize,
    gutter_width: usize,
) -> Option<(u16, u16)> {
    if current_cursor.is_some() || primary_cursor_position != buffer_len {
        return current_cursor;
    }

    if buffer_ends_with_newline {
        if let Some(end) = last_line_end {
            // When the last rendered line was the newline-terminated content
            // line, the cursor belongs on the implicit empty line one row
            // below. But when the trailing empty line was already emitted by
            // the ViewLineIterator (terminated_with_newline == false), the
            // cursor belongs on that rendered row itself.
            let y = if end.terminated_with_newline {
                end.pos.1.saturating_add(1)
            } else {
                end.pos.1
            };
            return Some((gutter_width as u16, y));
        }
        return Some((gutter_width as u16, lines_rendered as u16));
    }

    last_line_end.map(|end| end.pos)
}

/// Pure layout computation for a buffer in a split pane.
/// No frame/drawing involved — produces a `BufferLayoutOutput` that the
/// drawing phase can consume.
///
/// **A read of `(state, viewport, rect)`.** The viewport has been placed and
/// the buffer's margins and wrap index brought up to date by
/// [`super::reconcile`] before this runs; nothing here writes the viewport,
/// the folds or the margins, and the rows are built exactly once. `state` is
/// still `&mut` for the reads that fill caches as they go — the buffer's
/// lazy chunk loads under `line_iterator`, the highlighter, the overlay and
/// marker resolution in `decoration_context` — none of which is placement.
#[allow(clippy::too_many_arguments)]
pub(crate) fn compute_buffer_layout(
    state: &mut EditorState,
    cursors: &Cursors,
    viewport: &Viewport,
    folds: &FoldManager,
    area: Rect,
    is_active: bool,
    theme: &Theme,
    lsp_waiting: bool,
    view_mode: ViewMode,
    compose_width: Option<u16>,
    estimated_line_length: usize,
    highlight_context_bytes: usize,
    relative_line_numbers: bool,
    use_terminal_bg: bool,
    session_mode: bool,
    software_cursor_only: bool,
    show_line_numbers: bool,
    highlight_current_line: bool,
    fold_indicators_visible: bool,
    diagnostics_inline_text: bool,
    show_tilde: bool,
    indentation_guide: IndentationGuideMode,
    indentation_guide_glyph: &str,
    rainbow_indentation: bool,
    bracket_highlight: BracketHighlightSettings,
    cell_theme_map: Option<(&mut Vec<CellThemeInfo>, u16)>,
) -> BufferLayoutOutput {
    let _span = tracing::trace_span!("compute_buffer_layout").entered();
    crate::view::ui::split_rendering::instrument::count_buffer_layout();

    // Compute effective editor background: terminal default or theme-defined
    let effective_editor_bg = if use_terminal_bg {
        Color::Reset
    } else {
        theme.editor_bg
    };

    let line_wrap = viewport.line_wrap_enabled;

    let overlay_count = state.overlays.all().len();
    if overlay_count > 0 {
        tracing::trace!("render_content: {} overlays present", overlay_count);
    }

    let visible_count = viewport.visible_line_count();

    let buffer_len = state.buffer.len();
    let byte_offset_mode = state.buffer.line_count().is_none();
    let estimated_lines = if byte_offset_mode {
        // In byte offset mode, gutter shows byte offsets, so size the gutter
        // for the largest byte offset (file size)
        buffer_len.max(1)
    } else {
        state.buffer.line_count().unwrap_or(1)
    };
    let gutter = resolve_gutter_layout(
        &state.margins,
        show_line_numbers,
        &view_mode,
        area,
        compose_width,
        estimated_lines,
        state.diff_gutter.as_ref().map(|g| g.width()),
    );
    let GutterLayout {
        margin,
        width: gutter_width,
        compose: compose_layout,
    } = gutter;
    let render_area = compose_layout.render_area;

    // This split's cursor byte positions, for cursor-dependent conceal /
    // soft-break activation (evaluated per render, per split — cursor
    // movement changes what's active without any marker churn).
    let cursor_positions = cursors.positions();

    // Where the build starts. The viewport was placed before this frame by
    // `reconcile::place_pane`, which built the wrap index for this geometry
    // (when the buffer is within the index's size ceilings) and decided the
    // scroll in row space; here the same index — read, never built — says
    // which row the viewport's top is and where the wrap can be resumed.
    // Without an index the build starts at the logical line, as it always
    // did for buffers beyond the ceilings.
    let build_anchor: Option<BuildAnchor> = {
        let fold_ranges = state.fold_ranges(folds);
        let geometry = wrap_index_geometry_for(
            viewport,
            &state.buffer,
            line_wrap,
            &view_mode,
            crate::view::wrap_index::fold_signature(&fold_ranges),
        );
        // The reconcile builds the index for every indexable buffer within
        // the ceilings; a pane formatted without one is a pane reconciled
        // against a different geometry, or not at all.
        debug_assert!(
            state.wrap_indices.get(&geometry).is_some()
                || state.buffer.is_large_file()
                || state.buffer.len()
                    > crate::view::ui::split_rendering::scrollbar::MAX_WRAP_SCROLLBAR_BYTES
                || state.buffer.line_count().is_none_or(|lc| {
                    lc > crate::view::ui::split_rendering::scrollbar::MAX_WRAP_SCROLLBAR_LINES
                }),
            "compute_buffer_layout ran without a reconciled wrap index for its geometry"
        );
        if state.wrap_indices.get(&geometry).is_none()
            && crate::view::row_walk::addresses_rows_by_byte(&state.buffer, line_wrap)
        {
            // No index covers this buffer, so the top *is* the first visible
            // row and building from it draws the screen and nothing else.
            // Falling through to no anchor would build from the logical line —
            // every row from byte 0 on a one-line file, all but a screenful
            // discarded.
            let byte = viewport.top_byte();
            let carry = crate::view::row_walk::carry_at(&mut state.buffer, byte, geometry.rule);
            Some(BuildAnchor {
                byte,
                carry,
                skip: 0,
            })
        } else {
            state
                .wrap_indices
                .get(&geometry)
                .and_then(|index| resolve_build_anchor(index, state, viewport, &cursor_positions))
        }
    };

    let view_data = {
        let _span = tracing::trace_span!("build_view_data").entered();
        build_view_data(
            state,
            viewport,
            estimated_line_length,
            visible_count,
            line_wrap,
            render_area.width as usize,
            gutter_width,
            &view_mode,
            folds,
            theme,
            &cursor_positions,
            build_anchor,
        )
    };

    // Horizontal placement from the rows that were built. Vertical placement
    // was settled in row space by the reconcile, so this never moves
    // `top_byte` and the rows never need rebuilding after it. The column is a
    // value here — the frame is drawn with it, and the pane's paint stores
    // it afterwards (`reconcile::settle_pane`).
    let primary = *cursors.primary();
    let left_column = viewport.layout_column_scroll(
        &view_data.lines,
        &primary,
        render_area.width as usize,
        gutter_width,
    );

    let view_anchor = calculate_view_anchor(&view_data.lines, viewport.top_byte());

    let selection = selection_context(state, cursors);

    tracing::trace!(
        "Rendering buffer with {} cursors at positions: {:?}, primary at {}, is_active: {}, buffer_len: {}",
        selection.cursor_positions.len(),
        selection.cursor_positions,
        selection.primary_cursor_position,
        is_active,
        state.buffer.len()
    );

    if !selection.cursor_positions.is_empty()
        && !selection
            .cursor_positions
            .contains(&selection.primary_cursor_position)
    {
        tracing::warn!(
            "Primary cursor position {} not found in cursor_positions list: {:?}",
            selection.primary_cursor_position,
            selection.cursor_positions
        );
    }

    let adjusted_visible_count = fold_adjusted_visible_count(
        &state.buffer,
        &state.marker_list,
        folds,
        viewport.top_byte(),
        visible_count,
    );

    // Populate line cache to ensure chunks are loaded for rendering.
    let _ = state
        .buffer
        .populate_line_cache(viewport.top_byte(), adjusted_visible_count);

    // `calculate_viewport_end` walks *logical lines* from `top_byte` and
    // clamps each to one screen row's worth of columns — the right model for
    // horizontal scrolling, where a long line shows one row's window of
    // itself. Under soft wrap neither half holds: the drawn rows can all
    // belong to one logical line and can start `top_view_line_offset`
    // segments into it, so the byte window to decorate is the one the rows
    // themselves cover. Without this, scrolling into a long wrapped line
    // leaves every row past the first few undecorated — no syntax colours,
    // no overlays — because the request never moved off the line's start
    // (issue #2843).
    //
    // The rows already answer it under wrap, so the line walk only runs when
    // they can't — the unwrapped path, or drawn rows that carry no source
    // bytes at all.
    let wrapped_span = line_wrap.then(|| {
        let first_drawn = view_data.first_drawn.min(view_data.lines.len());
        let drawn = &view_data.lines[first_drawn..];
        let drawn = &drawn[..drawn.len().min(adjusted_visible_count)];
        visible_source_span(drawn)
    });
    let (viewport_start, viewport_end) = match wrapped_span.flatten() {
        Some(span) => span,
        None => {
            let viewport_start = viewport.top_byte();
            let viewport_end = calculate_viewport_end(
                state,
                viewport_start,
                estimated_line_length,
                adjusted_visible_count,
                left_column,
                render_area.width as usize,
            );
            (viewport_start, viewport_end)
        }
    };

    let decorations = decoration_context(
        state,
        viewport_start,
        viewport_end,
        selection.primary_cursor_position,
        selection.primary_selection.clone(),
        folds,
        theme,
        highlight_context_bytes,
        &view_mode,
        diagnostics_inline_text,
        bracket_highlight,
        &view_data.lines,
        fold_indicators_visible,
    );

    let calculated_offset = view_data.first_drawn;

    tracing::trace!(
        top_byte = viewport.top_byte(),
        top_view_line_offset = viewport.top_view_line_offset(),
        calculated_offset,
        view_data_lines = view_data.lines.len(),
        "view line offset calculation"
    );
    let (view_lines_to_render, adjusted_view_anchor) =
        if calculated_offset > 0 && calculated_offset < view_data.lines.len() {
            let sliced = &view_data.lines[calculated_offset..];
            let adjusted_anchor = calculate_view_anchor(sliced, viewport.top_byte());
            (sliced, adjusted_anchor)
        } else {
            (&view_data.lines[..], view_anchor)
        };

    // Use provided cell theme map or a temporary dummy
    let mut dummy_map = Vec::new();
    let (map_ref, sw) = match cell_theme_map {
        Some((map, w)) => (map, w),
        None => (&mut dummy_map, 0u16),
    };

    let render_output = render_view_lines(LineRenderInput {
        state,
        margin: &margin,
        theme,
        view_lines: view_lines_to_render,
        view_anchor: adjusted_view_anchor,
        render_area,
        gutter_width,
        selection: &selection,
        decorations: &decorations,
        visible_line_count: visible_count,
        lsp_waiting,
        is_active,
        line_wrap,
        estimated_lines,
        left_column,
        relative_line_numbers,
        session_mode,
        software_cursor_only,
        show_line_numbers,
        byte_offset_mode,
        show_tilde,
        effective_editor_bg,
        highlight_current_line,
        indentation_guide,
        indentation_guide_glyph,
        rainbow_indentation,
        cell_theme_map: map_ref,
        screen_width: sw,
    });

    let view_line_mappings = render_output.view_line_mappings.clone();

    let buffer_ends_with_newline = if !state.buffer.is_empty() {
        let last_char = state.get_text_range(state.buffer.len() - 1, state.buffer.len());
        last_char == "\n"
    } else {
        false
    };

    BufferLayoutOutput {
        view_line_mappings,
        render_output,
        render_area,
        compose_layout,
        effective_editor_bg,
        view_mode,
        left_column,
        gutter_width,
        buffer_ends_with_newline,
        selection,
    }
}

/// Where the pane's caret is, on screen, from a layout the content pass
/// produced: the row and cell the line pass placed it at, or the end-of-buffer
/// fallback, floated onto its virtual line or past the line end in virtual
/// space, clamped to the rows drawn. `None` for a layout that placed no
/// cursor at all.
///
/// **The one derivation of the caret's cell.** The pane's leaf places the
/// display list's cursor from it, the popup anchored to the caret reads it
/// back off the leaf, and the paint that draws a software cursor takes the
/// same value — none of them measures the rows again.
pub(crate) fn caret_cell(layout: &BufferLayoutOutput, buffer_len: usize) -> Option<(u16, u16)> {
    let render_area = layout.render_area;
    let gutter_width = layout.gutter_width;
    let cursor_from_line_pass = layout.render_output.cursor.is_some();
    let cursor = resolve_cursor_fallback(
        layout.render_output.cursor,
        layout.selection.primary_cursor_position,
        buffer_len,
        layout.buffer_ends_with_newline,
        layout.render_output.last_line_end,
        layout.render_output.content_lines_rendered,
        gutter_width,
    );
    // Virtual space: both the per-line pass and the EOF fallback park the
    // cursor at the buffer end's real position. Float it onto its virtual
    // line (vertical) — or, when the fallback produced it (the per-line
    // pass already applies horizontal shifts itself), out past the line
    // end (horizontal). Clamped to the render area.
    let cursor = cursor.map(|(cx, cy)| {
        let selection = &layout.selection;
        let max_x = render_area.width.saturating_sub(1);
        let max_y = render_area.height.saturating_sub(1);
        if selection.primary_virtual_lines > 0 {
            let x = gutter_width as u16
                + selection
                    .primary_virtual_line_col
                    .saturating_sub(layout.left_column) as u16;
            let y = cy.saturating_add(selection.primary_virtual_lines as u16);
            (x.min(max_x), y.min(max_y))
        } else if !cursor_from_line_pass && selection.primary_virtual_cols > 0 {
            ((cx + selection.primary_virtual_cols as u16).min(max_x), cy)
        } else {
            (cx, cy)
        }
    });
    cursor.map(|(cx, cy)| {
        let screen_x = render_area.x.saturating_add(cx);
        let max_y = render_area.height.saturating_sub(1);
        let screen_y = render_area.y.saturating_add(cy.min(max_y));
        (screen_x, screen_y)
    })
}

/// Draw a buffer into a frame using pre-computed layout output.
///
/// `caret` is the pane's caret on screen ([`caret_cell`]), when the pane
/// shows one: what the software cursor and the column highlight follow. The
/// hardware cursor is not this paint's — the pane's leaf placed it in the
/// display list from the same cell.
#[allow(clippy::too_many_arguments)]
pub(crate) fn draw_buffer_in_split(
    buf: &mut ratatui::buffer::Buffer,
    layout_output: BufferLayoutOutput,
    area: Rect,
    theme: &Theme,
    ansi_background: Option<&AnsiBackground>,
    background_fade: f32,
    software_cursor_only: bool,
    rulers: &[usize],
    compose_column_guides: Option<Vec<u16>>,
    highlight_current_column: bool,
    caret: Option<(u16, u16)>,
) {
    let render_area = layout_output.render_area;
    let effective_editor_bg = layout_output.effective_editor_bg;
    let gutter_width = layout_output.gutter_width;
    let starting_line_num = 0; // used only for background offset

    render_compose_margins(
        buf,
        area,
        &layout_output.compose_layout,
        &layout_output.view_mode,
        theme,
        effective_editor_bg,
    );

    let mut lines = layout_output.render_output.lines;
    let background_x_offset = layout_output.left_column;

    if let Some(bg) = ansi_background {
        apply_background_to_lines(
            &mut lines,
            render_area.width,
            bg,
            effective_editor_bg,
            theme.editor_fg,
            background_fade,
            background_x_offset,
            starting_line_num,
        );
    }

    // The caret, local to the rows: what the column highlight follows. Read
    // before the rows are drawn, because the tints below are part of them.
    let cursor = caret.map(|(sx, sy)| {
        (
            sx.saturating_sub(render_area.x),
            sy.saturating_sub(render_area.y),
        )
    });

    // **The column tints are runs in the rows, applied before they are drawn**
    // (L12). They used to be two passes back over the painted cells; the rows
    // carry them now, so nothing rewrites a cell the pane has already written
    // and the ruler's "which cell holds this column" is answered by the line's
    // own widths instead of by measuring what was painted beside it.
    //
    // Rulers span the pane's full height, including the rows below the last
    // line of text (#2631), so the row list is padded out to the pane before
    // they are applied — a row that renders nothing still shows the guide.
    let ruler_columns: Vec<usize> = rulers
        .iter()
        .filter_map(|c| {
            // 1-based display columns, as the "Add Ruler" prompt takes them.
            let col = c.checked_sub(1)?;
            let scrolled = col.checked_sub(layout_output.left_column)?;
            let at = gutter_width.checked_add(scrolled)?;
            (at < render_area.width as usize).then_some(at)
        })
        .collect();
    if !ruler_columns.is_empty() {
        let height = render_area.height as usize;
        if lines.len() < height {
            lines.resize_with(height, || ratatui::text::Line::from(""));
        }
        tint_columns_in_lines(
            &mut lines,
            &ruler_columns,
            theme.ruler_bg,
            theme.editor_fg,
            height,
        );
    }

    // The cursor column takes the current line's tint, over the rendered rows
    // only — an empty pane below the text has no line for it to follow. A
    // column inside the gutter is not the content's and is left alone.
    if highlight_current_column {
        if let Some((cx, _)) = cursor {
            if (cx as usize) >= gutter_width {
                tint_columns_in_lines(
                    &mut lines,
                    &[cx as usize],
                    theme.current_line_bg,
                    theme.editor_fg,
                    layout_output.render_output.content_lines_rendered,
                );
            }
        }
    }

    Clear.render(render_area, buf);
    let editor_block = Block::default()
        .borders(Borders::NONE)
        .style(Style::default().bg(effective_editor_bg));
    Paragraph::new(lines)
        .block(editor_block)
        .render(render_area, buf);

    // Render compose column guides
    if let Some(guides) = compose_column_guides {
        let guide_style = Style::default()
            .fg(theme.line_number_fg)
            .add_modifier(Modifier::DIM);
        render_column_guides(
            buf,
            &guides,
            guide_style,
            render_area,
            gutter_width,
            layout_output.render_output.content_lines_rendered,
            0,
        );
    }

    if let Some((screen_x, screen_y)) = caret {
        // When software_cursor_only the backend has no hardware cursor, so
        // ensure the cell at the cursor position always has REVERSED style.
        if software_cursor_only {
            let area = buf.area;
            if screen_x < area.x + area.width && screen_y < area.y + area.height {
                let cell = &mut buf[(screen_x, screen_y)];
                if !cell.modifier.contains(Modifier::REVERSED) {
                    cell.set_char(' ');
                    cell.fg = theme.editor_fg;
                    cell.bg = theme.editor_bg;
                    cell.modifier.insert(Modifier::REVERSED);
                }
            }
        }
    }
}

/// Where the build should start for this viewport, if the index can say.
///
/// The anchor is the viewport's own first row, walked back to the nearest row
/// the wrap can be resumed at — usually the same row, since every row of a plain
/// long line is resumable. `None` when the viewport is already at a logical line
/// start (nothing to save) or when the walk-back would cover the whole prefix
/// anyway.
fn resolve_build_anchor(
    index: &crate::view::wrap_index::WrapIndex,
    state: &EditorState,
    viewport: &Viewport,
    cursors: &[usize],
) -> Option<BuildAnchor> {
    let buffer = &state.buffer;
    if viewport.top_view_line_offset() == 0 {
        return None;
    }
    let top_line = buffer.get_line_number(viewport.top_byte());
    let line_start = buffer.line_start_offset(top_line).unwrap_or(0);
    let mut anchor_row = index.line_first_row(top_line) + viewport.top_view_line_offset() as u32;
    let mut stable_skip = 0u32;

    // The model's `_stable_anchor`. The index is canonical (no cursors), but
    // the frame is cursor-aware: a conceal or soft break whose activation
    // scope currently holds a cursor is applied differently on screen than in
    // the index. If such a decoration sits *above* the anchor inside the same
    // logical line, the canonical carry at the anchor does not describe the
    // stream the frame will draw from it — the rows drift, the cursor lands
    // rows away from where placement put it, and minimal placement then
    // correctly refuses to move (fresh#1574's stall). Backing the anchor up to
    // the divergence point and skipping the canonical row delta stitches the
    // two coordinate systems at a byte where they still agree.
    let anchor_byte = index.byte_of_row(buffer, anchor_row).byte;
    let divergence = [
        state.conceals.earliest_cursor_divergence(
            line_start,
            anchor_byte,
            &state.marker_list,
            cursors,
        ),
        state.soft_breaks.earliest_cursor_divergence(
            line_start,
            anchor_byte,
            &state.marker_list,
            cursors,
        ),
    ]
    .into_iter()
    .flatten()
    .min();
    if let Some(div) = divergence {
        let div_row = index.row_of_byte(buffer, div);
        let delta = anchor_row.saturating_sub(div_row);
        // Capped like the model: a divergence further back than a couple of
        // screens is cheaper to handle by not anchoring at all.
        if delta > 0 && (delta as usize) <= 2 * viewport.visible_line_count() {
            anchor_row = div_row;
            stable_skip = delta;
        } else if delta > 0 {
            return None;
        }
    }

    let (start_row, walk_back) = index.resumable_row_at_or_before(buffer, anchor_row);
    let addr = index.byte_of_row(buffer, start_row);
    // The stable anchor deliberately lands above `top_byte`; anything before
    // the top line's own start would mean resuming across a line boundary,
    // where the walk-back has failed — build unanchored instead.
    if addr.is_virtual || addr.byte < line_start {
        return None;
    }
    Some(BuildAnchor {
        byte: addr.byte,
        carry: addr.carry,
        skip: (walk_back + stable_skip) as usize,
    })
}

/// Geometry the wrap index is keyed by for this split.
///
/// Must match what `scrollbar_line_counts` builds, or the render path would look
/// up an entry that is never populated and silently fall back to the layout pass.
pub(crate) fn wrap_index_geometry_for(
    viewport: &Viewport,
    buffer: &crate::model::buffer::Buffer,
    line_wrap: bool,
    view_mode: &ViewMode,
    fold_signature: u64,
) -> crate::view::wrap_index::WrapIndexGeometry {
    use crate::primitives::line_wrapping::WrapConfig;
    use crate::view::line_wrap_cache::CacheViewMode;
    use crate::view::wrap_machine::WrapRule;

    let rule = if viewport.grid_wrap {
        WrapRule::Grid {
            cols: viewport.grid_cols().max(1),
        }
    } else if line_wrap {
        let gutter_width = viewport.gutter_width(buffer);
        let wrap_config = WrapConfig::new(
            viewport.width as usize,
            gutter_width,
            true,
            viewport.wrap_indent,
        );
        WrapRule::Word {
            content_width: wrap_config
                .first_line_width
                .saturating_add(gutter_width)
                .max(2),
            gutter_width,
            hanging_indent: wrap_config.hanging_indent,
        }
    } else {
        WrapRule::Chop {
            chars: crate::view::ui::split_rendering::MAX_SAFE_LINE_WIDTH,
        }
    };
    crate::view::wrap_index::WrapIndexGeometry {
        rule,
        view_mode: if matches!(view_mode, ViewMode::PageView) {
            CacheViewMode::Compose
        } else {
            CacheViewMode::Source
        },
        fold_signature,
    }
}
