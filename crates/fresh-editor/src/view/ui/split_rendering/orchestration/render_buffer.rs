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
    calculate_compose_layout, calculate_view_anchor, calculate_viewport_end, visible_source_span,
    ComposeLayout,
};
use super::super::post_pass::{
    apply_background_to_lines, render_column_guides, render_cursor_column_bg, render_ruler_bg,
};
use super::super::view_data::build_view_data;
use super::super::view_data::BuildAnchor;
use super::contexts::SelectionContext;
use super::overlays::{decoration_context, selection_context};
use super::render_line::{render_view_lines, LastLineEnd, LineRenderInput, LineRenderOutput};
use crate::app::types::{CellThemeInfo, ViewLineMapping};
use crate::config::IndentationGuideMode;
use crate::model::cursor::Cursors;
use crate::model::event::{BufferId, EventLog};
use crate::primitives::ansi_background::AnsiBackground;
use crate::state::{EditorState, ViewMode};
use crate::view::bracket_highlight_overlay::BracketHighlightSettings;
use crate::view::folding::FoldManager;
use crate::view::theme::Theme;
use crate::view::viewport::Viewport;
use fresh_core::api::ViewTransformPayload;
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
    indentation_guide: IndentationGuideMode,
    indentation_guide_glyph: &str,
    rainbow_indentation: bool,
    bracket_highlight: BracketHighlightSettings,
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
    // The diagnostic/indicator gutter is kept when line numbers are off only in
    // compose mode, where the render below reclaims its width from the desk
    // margin (issue #2146). In normal editor mode, line-numbers-off means no
    // gutter at all — otherwise the 1-col indicator slot would eat into the
    // text width and shift content right.
    if !show_line_numbers && !matches!(view_mode, ViewMode::PageView) {
        state.margins.left_config.enabled = false;
        state.margins.left_config.width = 0;
    }
    let mut gutter_width = state.margins.left_total_width();

    let mut compose_layout = calculate_compose_layout(area, &view_mode, compose_width);
    // In compose mode the gutter (diagnostic / indicator slot) is drawn in the
    // reclaimed desk margin so it does not shrink the centered text width
    // (issue #2146). Only do this when there is enough desk margin to give up;
    // if the paper already fills the area, drop the gutter instead of eating
    // into the text so table/wrap layout stays intact.
    if matches!(view_mode, ViewMode::PageView) && gutter_width > 0 {
        let g = gutter_width as u16;
        if compose_layout.left_pad >= g {
            compose_layout.left_pad -= g;
            let ra = compose_layout.render_area;
            compose_layout.render_area = Rect::new(ra.x - g, ra.y, ra.width + g, ra.height);
        } else {
            state.margins.left_config.enabled = false;
            state.margins.left_config.width = 0;
            gutter_width = 0;
        }
    }
    let render_area = compose_layout.render_area;

    // Clone view_transform so we can reuse it if scrolling triggers a rebuild
    let view_transform_for_rebuild = view_transform.clone();

    // This split's cursor byte positions, for cursor-dependent conceal /
    // soft-break activation (evaluated per render, per split — cursor
    // movement changes what's active without any marker churn).
    let cursor_positions = cursors.positions();

    // Decide the scroll *before* building. The layout-based pass below can only
    // run on materialised rows, so it makes the frame build rows to discover it
    // needs to scroll and then rebuild because it did. In row space the wrap
    // index answers "which row is the cursor on" directly, so the common case —
    // a cursor that has drifted into the scroll margin — is settled with no rows
    // built at all, and the layout pass then finds nothing left to do.
    //
    // Only when the index is already built for this geometry: building it here
    // would trade one O(buffer) pass for another.
    //
    // Not when a fold is collapsed. The index maps bytes to rows as if nothing
    // were hidden — it has no fold model — so with a fold active its row numbers
    // are not the rows being drawn. Deciding the scroll from them puts the
    // cursor in the wrong place, and because an anchored build also switches
    // off the layout pass's vertical phases, nothing downstream can correct it.
    let mut build_anchor: Option<BuildAnchor> = None;
    // Whether the row-space pass ran — which is what decides who owns vertical
    // placement, and is *not* the same question as whether an anchor resolved.
    // The pass lands on a logical line start often enough (it is where a scroll
    // up naturally stops), and there `top_view_line_offset` is 0, so no anchor
    // is needed. Reading that as "the row pass did not run" lets the layout pass
    // apply the margin a second time on top of a viewport that already
    // satisfies it, over-scrolling by the margin and leaving the cursor outside
    // it — after which the next key press finds nothing to do and scrolling
    // stalls (fresh#1574's up-arrow invariant).
    let mut rows_settled = false;
    if view_transform.is_none()
        && !state.wrap_indices.is_empty()
        && folds.is_empty()
        && state.wrap_index_models_layout()
    {
        let geometry = wrap_index_geometry_for(viewport, &state.buffer, line_wrap, &view_mode);
        let inputs_version = crate::view::line_wrap_cache::pipeline_inputs_version(
            state.buffer.version(),
            state.soft_breaks.version(),
            state.conceals.version(),
            state.virtual_texts.version(),
        );
        let cursor_byte = cursors.primary().position;
        let ready = state
            .wrap_indices
            .get(&geometry)
            .is_some_and(|index| index.is_built_for(&geometry, inputs_version));
        if ready {
            if let Some(index) = state.wrap_indices.get(&geometry) {
                viewport.ensure_visible_in_rows(index, &state.buffer, cursor_byte);
                rows_settled = true;
            }
            // Resolve the anchor *after* the scroll decision, so the build
            // starts where the frame will actually draw.
            build_anchor = state
                .wrap_indices
                .get(&geometry)
                .and_then(|index| resolve_build_anchor(index, &state.buffer, viewport));
        }
    }

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
            &cursor_positions,
            build_anchor,
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
            &cursor_positions,
            // A sync scroll moved the viewport; the anchor described where it
            // used to be.
            None,
        );
        viewport.scroll_to_end_of_view(&rebuilt.lines);
        (rebuilt, None)
    } else {
        (view_data, Some(view_transform_for_rebuild))
    };

    // Ensure cursor is visible using Layout-aware check (handles virtual lines)
    let primary = *cursors.primary();
    let top_byte_before_scroll = viewport.top_byte;
    let scrolled =
        viewport.ensure_visible_in_layout(&view_data.lines, &primary, gutter_width, rows_settled);

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
                &cursor_positions,
                // Same: the viewport moved, so the old anchor no longer applies.
                None,
            );
            // The rebuild is unanchored, so the layout pass owns the offsets again.
            let _ =
                viewport.ensure_visible_in_layout(&rebuilt.lines, &primary, gutter_width, false);
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
            let viewport_start = viewport.top_byte;
            let viewport_end = calculate_viewport_end(
                state,
                viewport_start,
                estimated_line_length,
                adjusted_visible_count,
                viewport.left_column,
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
        folds,
        theme,
        highlight_context_bytes,
        &view_mode,
        diagnostics_inline_text,
        bracket_highlight,
        &view_data.lines,
    );

    let calculated_offset = view_data.first_drawn;

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
        left_column: viewport.left_column,
        gutter_width,
        buffer_ends_with_newline,
        selection,
    }
}

/// Draw a buffer into a frame using pre-computed layout output.
#[allow(clippy::too_many_arguments)]
pub(crate) fn draw_buffer_in_split(
    buf: &mut ratatui::buffer::Buffer,
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
    highlight_current_column: bool,
    pending_hardware_cursor: &mut Option<(u16, u16)>,
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

    Clear.render(render_area, buf);
    let editor_block = Block::default()
        .borders(Borders::NONE)
        .style(Style::default().bg(effective_editor_bg));
    Paragraph::new(lines)
        .block(editor_block)
        .render(render_area, buf);

    let cursor_from_line_pass = layout_output.render_output.cursor.is_some();
    let cursor = resolve_cursor_fallback(
        layout_output.render_output.cursor,
        layout_output.selection.primary_cursor_position,
        state.buffer.len(),
        layout_output.buffer_ends_with_newline,
        layout_output.render_output.last_line_end,
        layout_output.render_output.content_lines_rendered,
        gutter_width,
    );
    // Virtual space: both the per-line pass and the EOF fallback park the
    // cursor at the buffer end's real position. Float it onto its virtual
    // line (vertical) — or, when the fallback produced it (the per-line
    // pass already applies horizontal shifts itself), out past the line
    // end (horizontal). Clamped to the render area.
    let cursor = cursor.map(|(cx, cy)| {
        let selection = &layout_output.selection;
        let max_x = render_area.width.saturating_sub(1);
        let max_y = render_area.height.saturating_sub(1);
        if selection.primary_virtual_lines > 0 {
            let x = gutter_width as u16
                + selection
                    .primary_virtual_line_col
                    .saturating_sub(layout_output.left_column) as u16;
            let y = cy.saturating_add(selection.primary_virtual_lines as u16);
            (x.min(max_x), y.min(max_y))
        } else if !cursor_from_line_pass && selection.primary_virtual_cols > 0 {
            ((cx + selection.primary_virtual_cols as u16).min(max_x), cy)
        } else {
            (cx, cy)
        }
    });

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

    // Render config-based vertical rulers. Span the full editor height rather
    // than stopping at the last text line: the ruler is a column guide, so it
    // must stay visible through the empty area below the buffer (matching VS
    // Code / Zed). Bounding it to `content_lines_rendered` made a short buffer
    // show the ruler only on written lines, leaving the rest of the pane blank
    // and the guide looking truncated (#2631).
    if !rulers.is_empty() {
        let ruler_cols: Vec<u16> = rulers.iter().map(|&r| r as u16).collect();
        render_ruler_bg(
            buf,
            &ruler_cols,
            theme.ruler_bg,
            render_area,
            gutter_width,
            render_area.height as usize,
            layout_output.left_column,
        );
    }

    // Highlight the cursor column (same bg tint as the current line) when
    // `highlight_current_column` is enabled and the split is active.
    if highlight_current_column && is_active && !hide_cursor {
        if let Some((cx, _)) = cursor {
            // `cx` already accounts for the gutter offset from render_area.x,
            // so skip highlighting if it falls inside the gutter.
            if (cx as usize) >= gutter_width {
                render_cursor_column_bg(
                    buf,
                    render_area,
                    cx,
                    theme.current_line_bg,
                    layout_output.render_output.content_lines_rendered,
                );
            }
        }
    }

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
    buf: &mut ratatui::buffer::Buffer,
    state: &mut EditorState,
    cursors: &Cursors,
    viewport: &mut Viewport,
    folds: &mut FoldManager,
    event_log: Option<&mut EventLog>,
    area: Rect,
    is_active: bool,
    style: crate::view::ui::RenderStyle<'_>,
    lsp_waiting: bool,
    view_mode: ViewMode,
    compose_width: Option<u16>,
    compose_column_guides: Option<Vec<u16>>,
    view_transform: Option<ViewTransformPayload>,
    _buffer_id: BufferId,
    hide_cursor: bool,
    session_mode: bool,
    rulers: &[usize],
    show_line_numbers: bool,
    highlight_current_line: bool,
    show_tilde: bool,
    highlight_current_column: bool,
    cell_theme_map: &mut Vec<CellThemeInfo>,
    screen_width: u16,
    pending_hardware_cursor: &mut Option<(u16, u16)>,
) -> Vec<ViewLineMapping> {
    // The style group provides theme + the appearance flags; unpack into the
    // locals the body already uses by name. The cfg fields this painter
    // doesn't read are ignored.
    let crate::view::ui::RenderStyle {
        theme,
        ansi_background,
        cfg,
    } = style;
    let crate::view::ui::EditorRenderConfig {
        background_fade,
        estimated_line_length,
        highlight_context_bytes,
        relative_line_numbers,
        use_terminal_bg,
        software_cursor_only,
        diagnostics_inline_text,
        indentation_guide,
        indentation_guide_glyph,
        rainbow_indentation,
        bracket_highlight,
        ..
    } = cfg;
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
        indentation_guide,
        indentation_guide_glyph,
        rainbow_indentation,
        bracket_highlight,
        Some((cell_theme_map, screen_width)),
    );

    let view_line_mappings = layout_output.view_line_mappings.clone();

    draw_buffer_in_split(
        buf,
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
        highlight_current_column,
        pending_hardware_cursor,
    );

    view_line_mappings
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
    buffer: &crate::model::buffer::Buffer,
    viewport: &Viewport,
) -> Option<BuildAnchor> {
    if viewport.top_view_line_offset == 0 {
        return None;
    }
    let top_line = buffer.get_line_number(viewport.top_byte);
    let anchor_row = index.line_first_row(top_line) + viewport.top_view_line_offset as u32;
    let (start_row, walk_back) = index.resumable_row_at_or_before(buffer, anchor_row);
    let addr = index.byte_of_row(buffer, start_row);
    if addr.is_virtual || addr.byte < viewport.top_byte {
        return None;
    }
    Some(BuildAnchor {
        byte: addr.byte,
        carry: addr.carry,
        skip: walk_back as usize,
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
    }
}
