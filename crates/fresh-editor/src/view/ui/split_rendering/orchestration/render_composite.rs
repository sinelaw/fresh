//! Composite buffer rendering (side-by-side view of multiple source buffers).
//!
//! Reuses the view pipeline (`build_view_data`) per pane and draws each
//! aligned row with syntax highlighting, selection, and inline diff
//! highlights.

use super::super::spans::{compute_inline_diff, span_color_at};
use super::super::view_data::build_view_data;
use crate::model::composite_buffer::{AlignedRow, CompositeBuffer};
use crate::model::event::BufferId;
use crate::primitives::display_width::char_width;
use crate::state::{EditorState, ViewMode};
use crate::view::composite_view::CompositeViewState;
use crate::view::folding::FoldManager;
use crate::view::theme::Theme;
use crate::view::ui::view_pipeline::{should_show_line_number, ViewLine};
use ratatui::layout::Rect;
use ratatui::style::{Color, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::Widget;
use ratatui::widgets::{Clear, Paragraph};
use std::collections::HashMap;
use std::ops::Range;

/// Render a composite buffer (side-by-side view of multiple source buffers).
/// Uses `ViewLine`s for proper syntax highlighting, ANSI handling, etc.
#[allow(clippy::too_many_arguments)]
pub(crate) fn render_composite_buffer(
    buf: &mut ratatui::buffer::Buffer,
    area: Rect,
    composite: &CompositeBuffer,
    buffers: &mut HashMap<BufferId, EditorState>,
    theme: &Theme,
    is_active: bool,
    view_state: &mut CompositeViewState,
    use_terminal_bg: bool,
    show_tilde: bool,
) {
    // Effective editor background: terminal default or theme-defined.
    let effective_editor_bg = if use_terminal_bg {
        Color::Reset
    } else {
        theme.editor_bg
    };

    // Clear the area first.
    Clear.render(area, buf);

    if composite.sources.is_empty() {
        return;
    }

    let layout = compute_pane_layout(composite, area.width);
    // Store computed pane widths in view state for cursor movement calculations.
    view_state.pane_widths = layout.widths.clone();

    render_pane_headers(
        buf,
        area,
        composite,
        &layout,
        view_state.focused_pane,
        theme,
    );

    // Content area (below the one-row headers).
    let header_height = 1u16;
    let content_y = area.y + header_height;
    let content_height = area.height.saturating_sub(header_height);
    let visible_rows = content_height as usize;
    let scroll_row = view_state.scroll_row;
    let total_rows = composite.alignment.rows.len();

    // Build each pane's ViewLines + syntax highlighting once for the whole
    // visible range, then paint every aligned row from that prepared data.
    let pane_render_data = build_pane_render_data(
        composite,
        buffers,
        &layout,
        scroll_row,
        visible_rows,
        content_height,
        theme,
    );

    for view_row in 0..visible_rows {
        let display_row = scroll_row + view_row;
        let row_y = content_y + view_row as u16;
        if display_row >= total_rows {
            render_past_end_row(buf, area.x, row_y, &layout, show_tilde, theme);
            continue;
        }
        render_aligned_row(
            buf,
            area.x,
            row_y,
            display_row,
            composite,
            buffers,
            &layout,
            &pane_render_data,
            view_state,
            is_active,
            effective_editor_bg,
            theme,
        );
    }
}

/// Pane geometry for a composite buffer: the content width of each pane plus
/// the width and visibility of the vertical separators drawn between them.
struct PaneLayout {
    widths: Vec<u16>,
    separator_width: u16,
    show_separator: bool,
}

impl PaneLayout {
    fn pane_count(&self) -> usize {
        self.widths.len()
    }
}

/// Compute the [`PaneLayout`] from the composite's layout spec and the
/// available width. Only called once at least one source pane exists.
fn compute_pane_layout(composite: &CompositeBuffer, area_width: u16) -> PaneLayout {
    use crate::model::composite_buffer::CompositeLayout;

    let pane_count = composite.sources.len();
    let show_separator = match &composite.layout {
        CompositeLayout::SideBySide { show_separator, .. } => *show_separator,
        _ => false,
    };
    let separator_width = if show_separator { 1 } else { 0 };
    let total_separators = (pane_count.saturating_sub(1)) as u16 * separator_width;
    let available_width = area_width.saturating_sub(total_separators);

    let widths: Vec<u16> = match &composite.layout {
        CompositeLayout::SideBySide { ratios, .. } => {
            let default_ratio = 1.0 / pane_count as f32;
            ratios
                .iter()
                .chain(std::iter::repeat(&default_ratio))
                .take(pane_count)
                .map(|r| (available_width as f32 * r).round() as u16)
                .collect()
        }
        _ => {
            let pane_width = available_width / pane_count as u16;
            vec![pane_width; pane_count]
        }
    };

    PaneLayout {
        widths,
        separator_width,
        show_separator,
    }
}

/// Render the one-row header bar for each pane (the source label), with the
/// focused pane drawn in the active-tab style.
fn render_pane_headers(
    buf: &mut ratatui::buffer::Buffer,
    area: Rect,
    composite: &CompositeBuffer,
    layout: &PaneLayout,
    focused_pane: usize,
    theme: &Theme,
) {
    let header_height = 1u16;
    let mut x_offset = area.x;
    for (idx, (source, &width)) in composite.sources.iter().zip(&layout.widths).enumerate() {
        let header_area = Rect::new(x_offset, area.y, width, header_height);
        let header_style = if idx == focused_pane {
            Style::default()
                .fg(theme.tab_active_fg)
                .bg(theme.tab_active_bg)
        } else {
            Style::default()
                .fg(theme.tab_inactive_fg)
                .bg(theme.tab_inactive_bg)
        };

        let header_text = format!(" {} ", source.label);
        let header = Paragraph::new(header_text).style(header_style);
        header.render(header_area, buf);

        x_offset += width + layout.separator_width;
    }
}

/// Prepared rendering data for one pane: the laid-out [`ViewLine`]s, a map
/// from source line number to its index in `lines`, and the pane's syntax
/// highlight spans.
struct PaneRenderData {
    lines: Vec<ViewLine>,
    line_to_view_line: HashMap<usize, usize>,
    highlight_spans: Vec<crate::primitives::highlighter::HighlightSpan>,
}

/// Build the [`PaneRenderData`] for every pane over the currently visible
/// range, yielding `None` for a pane with no visible source lines (or whose
/// source buffer is gone).
fn build_pane_render_data(
    composite: &CompositeBuffer,
    buffers: &mut HashMap<BufferId, EditorState>,
    layout: &PaneLayout,
    scroll_row: usize,
    visible_rows: usize,
    content_height: u16,
    theme: &Theme,
) -> Vec<Option<PaneRenderData>> {
    let alignment = &composite.alignment;
    let mut pane_render_data: Vec<Option<PaneRenderData>> = Vec::new();

    for (pane_idx, source) in composite.sources.iter().enumerate() {
        let Some(source_state) = buffers.get_mut(&source.buffer_id) else {
            pane_render_data.push(None);
            continue;
        };

        let visible_lines: Vec<usize> = alignment
            .rows
            .iter()
            .skip(scroll_row)
            .take(visible_rows)
            .filter_map(|row| row.get_pane_line(pane_idx))
            .map(|r| r.line)
            .collect();

        let (Some(first_line), Some(last_line)) = (
            visible_lines.iter().copied().min(),
            visible_lines.iter().copied().max(),
        ) else {
            pane_render_data.push(None);
            continue;
        };

        let top_byte = source_state
            .buffer
            .line_start_offset(first_line)
            .unwrap_or(0);
        let end_byte = source_state
            .buffer
            .line_start_offset(last_line + 1)
            .unwrap_or(source_state.buffer.len());

        let highlight_spans = source_state.highlighter.highlight_viewport(
            &source_state.buffer,
            top_byte,
            end_byte,
            theme,
            1024, // highlight_context_bytes
        );

        let pane_width = layout.widths.get(pane_idx).copied().unwrap_or(80);
        let mut viewport = crate::view::viewport::Viewport::new(pane_width, content_height);
        viewport.top_byte = top_byte;
        viewport.line_wrap_enabled = false;

        let pane_width = layout.widths.get(pane_idx).copied().unwrap_or(80) as usize;
        let gutter_width = 4; // Line number width
        let content_width = pane_width.saturating_sub(gutter_width);

        let lines_needed = last_line - first_line + 10;
        let empty_folds = FoldManager::new();
        let view_data = build_view_data(
            source_state,
            &viewport,
            None,         // No view transform
            80,           // estimated_line_length
            lines_needed, // visible_count - enough to cover the range
            false,        // line_wrap_enabled
            content_width,
            gutter_width,
            &ViewMode::Source, // Composite view uses source mode
            &empty_folds,
            theme,
            &[], // composite panes render cursor-blind
        );

        let mut line_to_view_line: HashMap<usize, usize> = HashMap::new();
        let mut current_line = first_line;
        for (idx, view_line) in view_data.lines.iter().enumerate() {
            if should_show_line_number(view_line) {
                line_to_view_line.insert(current_line, idx);
                current_line += 1;
            }
        }

        pane_render_data.push(Some(PaneRenderData {
            lines: view_data.lines,
            line_to_view_line,
            highlight_spans,
        }));
    }

    pane_render_data
}

/// Paint the panes of a row that lies past the end of the aligned content:
/// blank cells, with a leading `~` per pane when `show_tilde` is set.
fn render_past_end_row(
    buf: &mut ratatui::buffer::Buffer,
    area_x: u16,
    row_y: u16,
    layout: &PaneLayout,
    show_tilde: bool,
    theme: &Theme,
) {
    let mut x = area_x;
    for &width in &layout.widths {
        let eof_area = Rect::new(x, row_y, width, 1);
        let pad_width = width as usize;
        let text = if show_tilde && pad_width > 0 {
            format!("~{}", " ".repeat(pad_width.saturating_sub(1)))
        } else {
            " ".repeat(pad_width)
        };
        let eof = Paragraph::new(text).style(
            Style::default()
                .fg(theme.line_number_fg)
                .bg(theme.after_eof_bg),
        );
        eof.render(eof_area, buf);
        x += width + layout.separator_width;
    }
}

/// Compute the per-pane inline-diff highlight ranges for a modification row
/// (an empty range list for every other row type, and for any pane whose
/// line content is unavailable).
fn compute_modification_inline_diffs(
    aligned_row: &AlignedRow,
    composite: &CompositeBuffer,
    buffers: &HashMap<BufferId, EditorState>,
) -> Vec<Vec<Range<usize>>> {
    use crate::model::composite_buffer::RowType;

    if aligned_row.row_type != RowType::Modification {
        return vec![Vec::new(); composite.sources.len()];
    }

    let line_contents: Vec<Option<String>> = composite
        .sources
        .iter()
        .enumerate()
        .map(|(pane_idx, source)| {
            let line_ref = aligned_row.get_pane_line(pane_idx)?;
            let source_state = buffers.get(&source.buffer_id)?;
            source_state
                .buffer
                .get_line(line_ref.line)
                .map(|line| String::from_utf8_lossy(&line).to_string())
        })
        .collect();

    if let [Some(old_text), Some(new_text), ..] = line_contents.as_slice() {
        let (old_ranges, new_ranges) = compute_inline_diff(old_text, new_text);
        return vec![old_ranges, new_ranges];
    }
    vec![Vec::new(); composite.sources.len()]
}

/// Paint one aligned content row: every pane cell plus the separators between
/// them, at `row_y`.
#[allow(clippy::too_many_arguments)]
fn render_aligned_row(
    buf: &mut ratatui::buffer::Buffer,
    area_x: u16,
    row_y: u16,
    display_row: usize,
    composite: &CompositeBuffer,
    buffers: &HashMap<BufferId, EditorState>,
    layout: &PaneLayout,
    pane_render_data: &[Option<PaneRenderData>],
    view_state: &CompositeViewState,
    is_active: bool,
    effective_editor_bg: Color,
    theme: &Theme,
) {
    use crate::model::composite_buffer::RowType;

    let aligned_row = &composite.alignment.rows[display_row];
    // Only paint the cursor row / cursor cell when this composite panel
    // actually holds keyboard focus — otherwise the OLD/NEW cursor lingers
    // visibly after Tab moves focus to the file list or comments panel.
    let is_cursor_row = display_row == view_state.cursor_row && is_active;
    let selection_cols = view_state.selection_column_range(display_row);

    // Row background based on diff type (selection is character-level).
    let row_bg = match aligned_row.row_type {
        RowType::Addition => Some(theme.diff_add_bg),
        RowType::Deletion => Some(theme.diff_remove_bg),
        RowType::Modification => Some(theme.diff_modify_bg),
        RowType::HunkHeader => Some(theme.current_line_bg),
        RowType::Context => None,
    };

    let inline_diffs = compute_modification_inline_diffs(aligned_row, composite, buffers);

    let mut x_offset = area_x;
    for (pane_idx, &width) in layout.widths.iter().enumerate() {
        let pane_area = Rect::new(x_offset, row_y, width, 1);
        render_row_pane(
            buf,
            pane_area,
            pane_idx,
            width,
            aligned_row,
            pane_render_data,
            view_state,
            &inline_diffs,
            selection_cols,
            is_cursor_row,
            row_bg,
            effective_editor_bg,
            theme,
        );
        x_offset += width;

        if layout.show_separator && pane_idx < layout.pane_count() - 1 {
            let sep_area = Rect::new(x_offset, row_y, layout.separator_width, 1);
            let sep = Paragraph::new("│").style(
                Style::default()
                    .fg(theme.split_separator_fg)
                    .bg(theme.editor_bg),
            );
            sep.render(sep_area, buf);
            x_offset += layout.separator_width;
        }
    }
}

/// Paint a single pane cell for one aligned row: either highlighted source
/// content or a blank gap/padding cell (carrying the focused-pane cursor).
#[allow(clippy::too_many_arguments)]
fn render_row_pane(
    buf: &mut ratatui::buffer::Buffer,
    pane_area: Rect,
    pane_idx: usize,
    width: u16,
    aligned_row: &AlignedRow,
    pane_render_data: &[Option<PaneRenderData>],
    view_state: &CompositeViewState,
    inline_diffs: &[Vec<Range<usize>>],
    selection_cols: Option<(usize, usize)>,
    is_cursor_row: bool,
    row_bg: Option<Color>,
    effective_editor_bg: Color,
    theme: &Theme,
) {
    use crate::model::composite_buffer::RowType;

    let left_column = view_state
        .get_pane_viewport(pane_idx)
        .map(|v| v.left_column)
        .unwrap_or(0);
    let is_focused_pane = pane_idx == view_state.focused_pane;
    let gutter_width = 4usize;
    let max_content_width = width.saturating_sub(gutter_width as u16) as usize;

    let Some(source_line_ref) = aligned_row.get_pane_line(pane_idx) else {
        // No content for this pane (padding/gap line).
        let pane_has_selection = is_focused_pane
            && selection_cols
                .map(|(start, end)| start == 0 && end == usize::MAX)
                .unwrap_or(false);

        let bg = if pane_has_selection {
            theme.selection_bg
        } else if is_cursor_row && is_focused_pane {
            theme.current_line_bg
        } else {
            row_bg.unwrap_or(effective_editor_bg)
        };

        if is_cursor_row && is_focused_pane && view_state.cursor_column == 0 {
            let style = Style::default().fg(theme.line_number_fg).bg(bg);
            let cursor_style = Style::default().fg(theme.editor_bg).bg(theme.editor_fg);
            let padding = " ".repeat(max_content_width.saturating_sub(1));
            let line = Line::from(vec![
                Span::styled("    ", style),
                Span::styled(" ", cursor_style),
                Span::styled(padding, Style::default().bg(bg)),
            ]);
            Paragraph::new(line).render(pane_area, buf);
        } else {
            let gap_style = Style::default().bg(bg);
            let empty_content = " ".repeat(width as usize);
            Paragraph::new(empty_content)
                .style(gap_style)
                .render(pane_area, buf);
        }
        return;
    };

    let pane_data = pane_render_data.get(pane_idx).and_then(|opt| opt.as_ref());
    let view_line_opt = pane_data.and_then(|data| {
        data.line_to_view_line
            .get(&source_line_ref.line)
            .and_then(|&idx| data.lines.get(idx))
    });
    let highlight_spans = pane_data
        .map(|data| data.highlight_spans.as_slice())
        .unwrap_or(&[]);

    // Cursor-row highlight applies only on the focused pane.
    let bg = if is_cursor_row && is_focused_pane {
        theme.current_line_bg
    } else {
        row_bg.unwrap_or(effective_editor_bg)
    };

    let pane_selection_cols = if is_focused_pane {
        selection_cols
    } else {
        None
    };

    let line_num = format!("{:>3} ", source_line_ref.line + 1);
    let line_num_style = Style::default().fg(theme.line_number_fg).bg(bg);

    let inline_ranges = inline_diffs.get(pane_idx).cloned().unwrap_or_default();

    let highlight_bg = match aligned_row.row_type {
        RowType::Deletion => Some(theme.diff_remove_highlight_bg),
        RowType::Addition => Some(theme.diff_add_highlight_bg),
        RowType::Modification => {
            if pane_idx == 0 {
                Some(theme.diff_remove_highlight_bg)
            } else {
                Some(theme.diff_add_highlight_bg)
            }
        }
        _ => None,
    };

    let mut spans = vec![Span::styled(line_num, line_num_style)];

    if let Some(view_line) = view_line_opt {
        render_view_line_content(
            &mut spans,
            view_line,
            highlight_spans,
            left_column,
            max_content_width,
            bg,
            theme,
            is_cursor_row && is_focused_pane,
            view_state.cursor_column,
            &inline_ranges,
            highlight_bg,
            pane_selection_cols,
        );
    } else {
        // Unreachable in practice; fall back to a padded blank row.
        tracing::warn!(
            "ViewLine missing for composite buffer: pane={}, line={}, pane_data={}",
            pane_idx,
            source_line_ref.line,
            pane_data.is_some()
        );
        let base_style = Style::default().fg(theme.editor_fg).bg(bg);
        let padding = " ".repeat(max_content_width);
        spans.push(Span::styled(padding, base_style));
    }

    Paragraph::new(Line::from(spans)).render(pane_area, buf);
}

/// Render ViewLine content with syntax highlighting to spans.
#[allow(clippy::too_many_arguments)]
fn render_view_line_content(
    spans: &mut Vec<Span<'static>>,
    view_line: &ViewLine,
    highlight_spans: &[crate::primitives::highlighter::HighlightSpan],
    left_column: usize,
    max_width: usize,
    bg: Color,
    theme: &Theme,
    show_cursor: bool,
    cursor_column: usize,
    inline_ranges: &[Range<usize>],
    highlight_bg: Option<Color>,
    selection_cols: Option<(usize, usize)>,
) {
    let text = &view_line.text;
    let char_source_bytes = &view_line.char_source_bytes;

    let chars: Vec<char> = text.chars().collect();
    let mut col = 0usize;
    let mut rendered = 0usize;
    let mut current_span_text = String::new();
    let mut current_style: Option<Style> = None;
    let mut hl_cursor = 0usize;

    for (char_idx, ch) in chars.iter().enumerate() {
        let cw = char_width(*ch);

        // Skip characters before left_column
        if col < left_column {
            col += cw;
            continue;
        }

        if rendered >= max_width {
            break;
        }

        let byte_pos = char_source_bytes.get(char_idx).and_then(|b| *b);

        let highlight_color =
            byte_pos.and_then(|bp| span_color_at(highlight_spans, &mut hl_cursor, bp));

        let in_inline_range = inline_ranges.iter().any(|r| r.contains(&char_idx));

        let in_selection = selection_cols
            .map(|(start, end)| col >= start && col < end)
            .unwrap_or(false);

        let char_bg = if in_selection {
            theme.selection_bg
        } else if in_inline_range {
            highlight_bg.unwrap_or(bg)
        } else {
            bg
        };

        let char_style = if let Some(color) = highlight_color {
            Style::default().fg(color).bg(char_bg)
        } else {
            Style::default().fg(theme.editor_fg).bg(char_bg)
        };

        let final_style = if show_cursor && col == cursor_column {
            Style::default().fg(theme.editor_bg).bg(theme.editor_fg)
        } else {
            char_style
        };

        if let Some(style) = current_style {
            if style != final_style && !current_span_text.is_empty() {
                spans.push(Span::styled(std::mem::take(&mut current_span_text), style));
            }
        }

        current_style = Some(final_style);
        current_span_text.push(*ch);
        col += cw;
        rendered += cw;
    }

    if !current_span_text.is_empty() {
        if let Some(style) = current_style {
            spans.push(Span::styled(current_span_text, style));
        }
    }

    if rendered < max_width {
        let padding_len = max_width - rendered;
        let cursor_visual = cursor_column.saturating_sub(left_column);

        if show_cursor && cursor_visual >= rendered && cursor_visual < max_width {
            let cursor_offset = cursor_visual - rendered;
            let cursor_style = Style::default().fg(theme.editor_bg).bg(theme.editor_fg);
            let normal_style = Style::default().bg(bg);

            if cursor_offset > 0 {
                spans.push(Span::styled(" ".repeat(cursor_offset), normal_style));
            }
            spans.push(Span::styled(" ", cursor_style));
            let remaining = padding_len.saturating_sub(cursor_offset + 1);
            if remaining > 0 {
                spans.push(Span::styled(" ".repeat(remaining), normal_style));
            }
        } else {
            spans.push(Span::styled(
                " ".repeat(padding_len),
                Style::default().bg(bg),
            ));
        }
    }
}
