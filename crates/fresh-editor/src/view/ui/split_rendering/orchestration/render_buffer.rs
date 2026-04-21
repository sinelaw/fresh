//! Per-buffer render orchestration.
//!
//! Three functions compose here:
//! - [`compute_buffer_layout`] — pure layout phase (no drawing).
//! - [`draw_buffer_in_split`] — drawing phase from a `BufferLayoutOutput`.
//! - [`render_buffer_in_split`] — the two phases combined, the API used by
//!   the top-level `render_content`.

use super::super::folding::fold_adjusted_visible_count;
use super::super::gutter::render_compose_margins;
use super::super::layout::{
    calculate_compose_layout, calculate_view_anchor, calculate_viewport_end, ComposeLayout,
};
use super::super::post_pass::{apply_background_to_lines, render_column_guides, render_ruler_bg};
use super::super::view_data::build_view_data;
use super::contexts::SelectionContext;
use super::overlays::{decoration_context, selection_context};
use super::render_line::{render_view_lines, LastLineEnd, LineRenderInput, LineRenderOutput};
use crate::app::types::{CellThemeInfo, ViewLineMapping};
use crate::model::cursor::Cursors;
use crate::model::event::{BufferId, EventLog};
use crate::primitives::ansi_background::AnsiBackground;
use crate::state::{EditorState, ViewMode};
use crate::view::folding::FoldManager;
use crate::view::theme::Theme;
use crate::view::viewport::Viewport;
use fresh_core::api::ViewTransformPayload;
use ratatui::layout::Rect;
use ratatui::style::{Color, Modifier, Style};
use ratatui::widgets::{Block, Borders, Clear, Paragraph};
use ratatui::Frame;

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
    pub left_column: usize,
    pub gutter_width: usize,
    pub buffer_ends_with_newline: bool,
    pub selection: SelectionContext,
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
#[allow(clippy::too_many_arguments)]
pub(crate) fn compute_buffer_layout(
    state: &mut EditorState,
    cursors: &Cursors,
    viewport: &mut Viewport,
    folds: &mut FoldManager,
    area: Rect,
    is_active: bool,
    theme: &Theme,
    lsp_waiting: bool,
    view_mode: ViewMode,
    compose_width: Option<u16>,
    view_transform: Option<ViewTransformPayload>,
    estimated_line_length: usize,
    highlight_context_bytes: usize,
    relative_line_numbers: bool,
    use_terminal_bg: bool,
    session_mode: bool,
    software_cursor_only: bool,
    show_line_numbers: bool,
    highlight_current_line: bool,
    diagnostics_inline_text: bool,
    show_tilde: bool,
    cell_theme_map: Option<(&mut Vec<CellThemeInfo>, u16)>,
) -> BufferLayoutOutput {
    let _span = tracing::trace_span!("compute_buffer_layout").entered();

    // Configure shared margin layout for this split's line number setting.
    state.margins.configure_for_line_numbers(show_line_numbers);

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
    state
        .margins
        .update_width_for_buffer(estimated_lines, show_line_numbers);
    let gutter_width = state.margins.left_total_width();

    let compose_layout = calculate_compose_layout(area, &view_mode, compose_width);
    let render_area = compose_layout.render_area;

    // Clone view_transform so we can reuse it if scrolling triggers a rebuild
    let view_transform_for_rebuild = view_transform.clone();

    let view_data = {
        let _span = tracing::trace_span!("build_view_data").entered();
        build_view_data(
            state,
            viewport,
            view_transform,
            estimated_line_length,
            visible_count,
            line_wrap,
            render_area.width as usize,
            gutter_width,
            &view_mode,
            folds,
            theme,
        )
    };

    // Same-buffer scroll sync: if the sync code flagged this viewport to
    // scroll to the end, apply it now using the view lines we just built.
    let sync_scrolled = if viewport.sync_scroll_to_end {
        viewport.sync_scroll_to_end = false;
        viewport.scroll_to_end_of_view(&view_data.lines)
    } else {
        false
    };

    // If the sync adjustment changed top_byte, rebuild view_data before
    // ensure_visible_in_layout runs (so it sees the correct view lines).
    let (view_data, view_transform_for_rebuild) = if sync_scrolled {
        viewport.top_view_line_offset = 0;
        let rebuilt = build_view_data(
            state,
            viewport,
            view_transform_for_rebuild,
            estimated_line_length,
            visible_count,
            line_wrap,
            render_area.width as usize,
            gutter_width,
            &view_mode,
            folds,
            theme,
        );
        viewport.scroll_to_end_of_view(&rebuilt.lines);
        (rebuilt, None)
    } else {
        (view_data, Some(view_transform_for_rebuild))
    };

    // Ensure cursor is visible using Layout-aware check (handles virtual lines)
    let primary = *cursors.primary();
    let top_byte_before_scroll = viewport.top_byte;
    let scrolled = viewport.ensure_visible_in_layout(&view_data.lines, &primary, gutter_width);

    // If we scrolled AND `top_byte` changed, rebuild view_data from the new
    // top_byte (the old view_data no longer matches what's visible).  We
    // also reset `top_view_line_offset` to 0 and re-run the layout-aware
    // check so that the offset is correct for the rebuilt view_data — the
    // absolute indices from the old view_data don't map directly to the
    // new one.
    //
    // When `top_byte` did NOT change (e.g. `snap_to_logical_line_start`
    // kept `top_byte` at the current logical line's start and only
    // shifted `top_view_line_offset` to a wrap-segment offset), the
    // existing view_data already matches and
    // `top_view_line_offset` is authoritative — resetting it here would
    // erase the scroll that `ensure_visible_in_layout` just applied
    // (issue #1574, Up-arrow jumpy variant: cy 5→7 at step 13 of the
    // width-sweep).
    let view_data = if scrolled && viewport.top_byte != top_byte_before_scroll {
        if let Some(vt) = view_transform_for_rebuild {
            viewport.top_view_line_offset = 0;
            let rebuilt = build_view_data(
                state,
                viewport,
                vt,
                estimated_line_length,
                visible_count,
                line_wrap,
                render_area.width as usize,
                gutter_width,
                &view_mode,
                folds,
                theme,
            );
            let _ = viewport.ensure_visible_in_layout(&rebuilt.lines, &primary, gutter_width);
            rebuilt
        } else {
            view_data
        }
    } else {
        view_data
    };

    let view_anchor = calculate_view_anchor(&view_data.lines, viewport.top_byte);

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
        viewport.top_byte,
        visible_count,
    );

    // Populate line cache to ensure chunks are loaded for rendering.
    let _ = state
        .buffer
        .populate_line_cache(viewport.top_byte, adjusted_visible_count);

    let viewport_start = viewport.top_byte;
    let viewport_end = calculate_viewport_end(
        state,
        viewport_start,
        estimated_line_length,
        adjusted_visible_count,
    );

    let decorations = decoration_context(
        state,
        viewport_start,
        viewport_end,
        selection.primary_cursor_position,
        folds,
        theme,
        highlight_context_bytes,
        &view_mode,
        diagnostics_inline_text,
        &view_data.lines,
    );

    let calculated_offset = viewport.top_view_line_offset;

    tracing::trace!(
        top_byte = viewport.top_byte,
        top_view_line_offset = viewport.top_view_line_offset,
        calculated_offset,
        view_data_lines = view_data.lines.len(),
        "view line offset calculation"
    );
    let (view_lines_to_render, adjusted_view_anchor) =
        if calculated_offset > 0 && calculated_offset < view_data.lines.len() {
            let sliced = &view_data.lines[calculated_offset..];
            let adjusted_anchor = calculate_view_anchor(sliced, viewport.top_byte);
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
        left_column: viewport.left_column,
        relative_line_numbers,
        session_mode,
        software_cursor_only,
        show_line_numbers,
        byte_offset_mode,
        show_tilde,
        highlight_current_line,
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
        left_column: viewport.left_column,
        gutter_width,
        buffer_ends_with_newline,
        selection,
    }
}

/// Draw a buffer into a frame using pre-computed layout output.
#[allow(clippy::too_many_arguments)]
pub(crate) fn draw_buffer_in_split(
    frame: &mut Frame,
    state: &EditorState,
    cursors: &Cursors,
    layout_output: BufferLayoutOutput,
    event_log: Option<&mut EventLog>,
    area: Rect,
    is_active: bool,
    theme: &Theme,
    ansi_background: Option<&AnsiBackground>,
    background_fade: f32,
    hide_cursor: bool,
    software_cursor_only: bool,
    rulers: &[usize],
    compose_column_guides: Option<Vec<u16>>,
    pending_hardware_cursor: &mut Option<(u16, u16)>,
) {
    let render_area = layout_output.render_area;
    let effective_editor_bg = layout_output.effective_editor_bg;
    let gutter_width = layout_output.gutter_width;
    let starting_line_num = 0; // used only for background offset

    render_compose_margins(
        frame,
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

    frame.render_widget(Clear, render_area);
    let editor_block = Block::default()
        .borders(Borders::NONE)
        .style(Style::default().bg(effective_editor_bg));
    frame.render_widget(Paragraph::new(lines).block(editor_block), render_area);

    let cursor = resolve_cursor_fallback(
        layout_output.render_output.cursor,
        layout_output.selection.primary_cursor_position,
        state.buffer.len(),
        layout_output.buffer_ends_with_newline,
        layout_output.render_output.last_line_end,
        layout_output.render_output.content_lines_rendered,
        gutter_width,
    );

    let cursor_screen_pos = if is_active && state.show_cursors && !hide_cursor {
        cursor.map(|(cx, cy)| {
            let screen_x = render_area.x.saturating_add(cx);
            let max_y = render_area.height.saturating_sub(1);
            let screen_y = render_area.y.saturating_add(cy.min(max_y));
            (screen_x, screen_y)
        })
    } else {
        None
    };

    // Render config-based vertical rulers
    if !rulers.is_empty() {
        let ruler_cols: Vec<u16> = rulers.iter().map(|&r| r as u16).collect();
        render_ruler_bg(
            frame,
            &ruler_cols,
            theme.ruler_bg,
            render_area,
            gutter_width,
            layout_output.render_output.content_lines_rendered,
            layout_output.left_column,
        );
    }

    // Render compose column guides
    if let Some(guides) = compose_column_guides {
        let guide_style = Style::default()
            .fg(theme.line_number_fg)
            .add_modifier(Modifier::DIM);
        render_column_guides(
            frame,
            &guides,
            guide_style,
            render_area,
            gutter_width,
            layout_output.render_output.content_lines_rendered,
            0,
        );
    }

    if let Some((screen_x, screen_y)) = cursor_screen_pos {
        // Record the hardware cursor position instead of committing it to
        // the frame now. `render.rs` decides at the end of the render pass
        // whether to show the cursor — if a popup later overlays this cell
        // it suppresses the cursor so the hardware caret does not bleed
        // through the popup.
        *pending_hardware_cursor = Some((screen_x, screen_y));

        // When software_cursor_only the backend has no hardware cursor, so
        // ensure the cell at the cursor position always has REVERSED style.
        if software_cursor_only {
            let buf = frame.buffer_mut();
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

        if let Some(event_log) = event_log {
            let cursor_pos = cursors.primary().position;
            let buffer_len = state.buffer.len();
            event_log.log_render_state(cursor_pos, screen_x, screen_y, buffer_len);
        }
    }
}

/// Render a single buffer in a split pane (convenience wrapper).
/// Calls [`compute_buffer_layout`] then [`draw_buffer_in_split`].
/// Returns the view line mappings for mouse click handling.
#[allow(clippy::too_many_arguments)]
pub(crate) fn render_buffer_in_split(
    frame: &mut Frame,
    state: &mut EditorState,
    cursors: &Cursors,
    viewport: &mut Viewport,
    folds: &mut FoldManager,
    event_log: Option<&mut EventLog>,
    area: Rect,
    is_active: bool,
    theme: &Theme,
    ansi_background: Option<&AnsiBackground>,
    background_fade: f32,
    lsp_waiting: bool,
    view_mode: ViewMode,
    compose_width: Option<u16>,
    compose_column_guides: Option<Vec<u16>>,
    view_transform: Option<ViewTransformPayload>,
    estimated_line_length: usize,
    highlight_context_bytes: usize,
    _buffer_id: BufferId,
    hide_cursor: bool,
    relative_line_numbers: bool,
    use_terminal_bg: bool,
    session_mode: bool,
    software_cursor_only: bool,
    rulers: &[usize],
    show_line_numbers: bool,
    highlight_current_line: bool,
    diagnostics_inline_text: bool,
    show_tilde: bool,
    cell_theme_map: &mut Vec<CellThemeInfo>,
    screen_width: u16,
    pending_hardware_cursor: &mut Option<(u16, u16)>,
) -> Vec<ViewLineMapping> {
    let layout_output = compute_buffer_layout(
        state,
        cursors,
        viewport,
        folds,
        area,
        is_active,
        theme,
        lsp_waiting,
        view_mode.clone(),
        compose_width,
        view_transform,
        estimated_line_length,
        highlight_context_bytes,
        relative_line_numbers,
        use_terminal_bg,
        session_mode,
        software_cursor_only,
        show_line_numbers,
        highlight_current_line,
        diagnostics_inline_text,
        show_tilde,
        Some((cell_theme_map, screen_width)),
    );

    let view_line_mappings = layout_output.view_line_mappings.clone();

    draw_buffer_in_split(
        frame,
        state,
        cursors,
        layout_output,
        event_log,
        area,
        is_active,
        theme,
        ansi_background,
        background_fade,
        hide_cursor,
        software_cursor_only,
        rulers,
        compose_column_guides,
        pending_hardware_cursor,
    );

    view_line_mappings
}
