//! Post-row work: implicit trailing empty line + EOF tilde fill.
//!
//! Together these two passes own everything that runs *after* the
//! main per-row loop in `render_view_lines` finishes:
//!
//! * `render_implicit_trailing_line` — when the last actual content
//!   line ended with a newline, surface the implicit empty line that
//!   follows (so the cursor can sit there and the gutter shows the
//!   right number). Has a fallback that adds the `ViewLineMapping`
//!   entry even when there's no screen room left, so visual-line
//!   navigation can still reach the trailing line.
//! * `fill_eof_rows` — pad the bottom of the viewport with `~`
//!   markers / `after_eof_bg` shading so post-EOF space is visually
//!   distinct from buffer content (see issues #779, #458, ratatui
//!   #1606).

use super::super::super::style::dim_color_for_tilde;
use super::super::contexts::DecorationContext;
use super::LastLineEnd;
use crate::app::types::ViewLineMapping;
use crate::state::EditorState;
use crate::view::theme::Theme;
use ratatui::layout::Rect;
use ratatui::style::{Color, Style};
use ratatui::text::{Line, Span};

/// Mutable accumulators that the post-row passes append into.
pub(super) struct PostRowAccumulator<'a> {
    pub lines: &'a mut Vec<Line<'static>>,
    pub view_line_mappings: &'a mut Vec<ViewLineMapping>,
    pub lines_rendered: &'a mut usize,
    pub cursor_screen_x: &'a mut u16,
    pub cursor_screen_y: &'a mut u16,
    pub have_cursor: &'a mut bool,
}

/// Read-only inputs threaded from `render_view_lines`.
pub(super) struct PostRowContext<'a> {
    pub state: &'a EditorState,
    pub theme: &'a Theme,
    pub render_area: Rect,
    pub gutter_width: usize,
    pub decorations: &'a DecorationContext,
    pub cursor_line_start_byte: usize,
    pub primary_cursor_position: usize,
    /// Virtual-space columns of the primary cursor (0 when not in
    /// virtual space or the mode is off).
    pub primary_virtual_cols: usize,
    pub byte_offset_mode: bool,
    pub show_line_numbers: bool,
    pub highlight_current_line: bool,
    pub is_active: bool,
    pub last_gutter_num: Option<usize>,
    pub visible_line_count: usize,
    pub trailing_empty_line_rendered: bool,
}

/// Render the implicit empty line that sits after the buffer's final
/// newline (and the fallback `ViewLineMapping` for it).
///
/// No-op when the last rendered line didn't terminate with a newline,
/// when the per-row loop already produced a trailing empty line, or
/// when the buffer doesn't end with a newline at all.
pub(super) fn render_implicit_trailing_line(
    last_line_end: Option<&LastLineEnd>,
    ctx: &PostRowContext<'_>,
    acc: &mut PostRowAccumulator<'_>,
) {
    let Some(end) = last_line_end else {
        return;
    };
    if !end.terminated_with_newline {
        return;
    }

    if *acc.lines_rendered < ctx.visible_line_count && !ctx.trailing_empty_line_rendered {
        render_implicit_line_into(end, ctx, acc);
    }

    // Even when there's no screen room, ensure a `ViewLineMapping`
    // exists for the trailing line so `move_visual_line` can still
    // step into it (which triggers a viewport scroll on next render).
    ensure_trailing_mapping(ctx, acc);
}

fn render_implicit_line_into(
    _end: &LastLineEnd,
    ctx: &PostRowContext<'_>,
    acc: &mut PostRowAccumulator<'_>,
) {
    let mut implicit_line_spans = Vec::new();
    let implicit_line_byte = ctx.state.buffer.len();
    let implicit_gutter_num = if ctx.byte_offset_mode {
        implicit_line_byte
    } else {
        ctx.last_gutter_num.map_or(0, |n| n + 1)
    };

    let implicit_is_cursor_line = implicit_line_byte == ctx.cursor_line_start_byte;
    let implicit_cursor_bg =
        if implicit_is_cursor_line && ctx.highlight_current_line && ctx.is_active {
            Some(ctx.theme.current_line_bg)
        } else {
            None
        };

    if ctx.state.margins.left_config.enabled {
        push_left_margin(
            &mut implicit_line_spans,
            ctx,
            implicit_line_byte,
            implicit_gutter_num,
            implicit_cursor_bg,
        );
    }

    // Fill remaining width with current_line_bg for cursor line.
    if let Some(bg) = implicit_cursor_bg {
        let gutter_w = if ctx.state.margins.left_config.enabled {
            ctx.state.margins.left_total_width()
        } else {
            0
        };
        let content_width = ctx.render_area.width.saturating_sub(gutter_w as u16) as usize;
        if content_width > 0 {
            implicit_line_spans.push(Span::styled(
                " ".repeat(content_width),
                Style::default().bg(bg),
            ));
        }
    }

    let implicit_y = acc.lines.len() as u16;
    acc.lines.push(Line::from(implicit_line_spans));
    *acc.lines_rendered += 1;

    // Implicit line has no content — empty mapping (gutter offset is
    // applied by `screen_to_buffer_position`).
    acc.view_line_mappings.push(ViewLineMapping {
        char_source_bytes: Vec::new(),
        visual_to_char: Vec::new(),
        line_end_byte: ctx.state.buffer.len(),
        is_plugin_virtual: false,
    });

    // NOTE: We intentionally do NOT update last_line_end here; the
    // implicit empty line is a visual display aid, not actual content.

    // Cursor at EOF (after the newline) lands on this implicit line — at
    // its virtual column when in virtual space.
    if ctx.primary_cursor_position == ctx.state.buffer.len() && !*acc.have_cursor {
        *acc.cursor_screen_x = ctx.gutter_width as u16 + ctx.primary_virtual_cols as u16;
        *acc.cursor_screen_y = implicit_y;
        *acc.have_cursor = true;
    }
}

fn push_left_margin(
    spans: &mut Vec<Span<'static>>,
    ctx: &PostRowContext<'_>,
    implicit_line_byte: usize,
    implicit_gutter_num: usize,
    implicit_cursor_bg: Option<Color>,
) {
    // Diagnostic indicator column.
    if ctx
        .decorations
        .diagnostic_lines
        .contains(&implicit_line_byte)
    {
        let mut style = Style::default().fg(Color::Red);
        if let Some(bg) = implicit_cursor_bg {
            style = style.bg(bg);
        }
        spans.push(Span::styled("●", style));
    } else {
        let mut style = Style::default();
        if let Some(bg) = implicit_cursor_bg {
            style = style.bg(bg);
        }
        spans.push(Span::styled(" ", style));
    }

    // Line number (or byte offset in byte_offset_mode).
    let rendered_text = if ctx.byte_offset_mode && ctx.show_line_numbers {
        format!(
            "{:>width$}",
            implicit_gutter_num,
            width = ctx.state.margins.left_config.width
        )
    } else {
        let estimated_lines =
            ctx.state.buffer.line_count().unwrap_or(
                (ctx.state.buffer.len() / ctx.state.buffer.estimated_line_length()).max(1),
            );
        let margin_content = ctx.state.margins.render_line(
            implicit_gutter_num,
            crate::view::margin::MarginPosition::Left,
            estimated_lines,
            ctx.show_line_numbers,
        );
        margin_content.render(ctx.state.margins.left_config.width).0
    };
    let mut margin_style = Style::default().fg(ctx.theme.line_number_fg);
    if let Some(bg) = implicit_cursor_bg {
        margin_style = margin_style.bg(bg);
    }
    spans.push(Span::styled(rendered_text, margin_style));

    if ctx.state.margins.left_config.show_separator {
        let mut sep_style = Style::default().fg(ctx.theme.line_number_fg);
        if let Some(bg) = implicit_cursor_bg {
            sep_style = sep_style.bg(bg);
        }
        spans.push(Span::styled(
            ctx.state.margins.left_config.separator.to_string(),
            sep_style,
        ));
    }
}

fn ensure_trailing_mapping(ctx: &PostRowContext<'_>, acc: &mut PostRowAccumulator<'_>) {
    let last_mapped_byte = acc
        .view_line_mappings
        .last()
        .map(|m| m.line_end_byte)
        .unwrap_or(0);
    let near_buffer_end = last_mapped_byte + 2 >= ctx.state.buffer.len();
    let already_mapped = acc.view_line_mappings.last().is_some_and(|m| {
        m.char_source_bytes.is_empty() && m.line_end_byte == ctx.state.buffer.len()
    });
    if near_buffer_end && !already_mapped {
        acc.view_line_mappings.push(ViewLineMapping {
            char_source_bytes: Vec::new(),
            visual_to_char: Vec::new(),
            line_end_byte: ctx.state.buffer.len(),
            is_plugin_virtual: false,
        });
    }
}

/// Pad the bottom of the viewport with `~` (when `show_tilde`) and
/// `theme.after_eof_bg` shading. Issues #779, #458 explain why we
/// don't use `Modifier::DIM` here, and ratatui #1606 explains why
/// we always emit a styled span (vs leaving the row blank).
pub(super) fn fill_eof_rows(
    lines: &mut Vec<Line<'static>>,
    theme: &Theme,
    render_area: Rect,
    show_tilde: bool,
) {
    let eof_fg = dim_color_for_tilde(theme.line_number_fg);
    let eof_style = Style::default().fg(eof_fg).bg(theme.after_eof_bg);
    while lines.len() < render_area.height as usize {
        let width = render_area.width as usize;
        let eof_line = if show_tilde && width > 0 {
            format!("~{}", " ".repeat(width.saturating_sub(1)))
        } else {
            " ".repeat(width)
        };
        lines.push(Line::styled(eof_line, eof_style));
    }
}
