//! Gutter rendering: left-margin line numbers / indicators / separators and
//! the compose-mode "paper" margins.
//!
//! The `LeftMarginContext` is deliberately `pub(super)` and documented as
//! internal-only: only the two call sites inside the split-rendering module
//! use it; it is never re-exported outside the module tree.

use super::folding::FoldIndicator;
use super::layout::ComposeLayout;
use super::spans::push_span_with_map;
use crate::state::{EditorState, ViewMode};
use crate::view::margin::{LineIndicator, MarginPosition};
use crate::view::theme::Theme;
use ratatui::layout::Rect;
use ratatui::style::{Color, Style};
use ratatui::text::Span;
use ratatui::widgets::Block;
use ratatui::Frame;
use std::collections::{BTreeMap, HashSet};

/// Context for rendering the left margin (line numbers, indicators, separator).
///
/// This type is private to the split-rendering module — it is used only by
/// `render_left_margin` and the orchestration code that prepares its input.
pub(super) struct LeftMarginContext<'a> {
    pub state: &'a EditorState,
    pub theme: &'a Theme,
    pub is_continuation: bool,
    /// Line-start byte offset for fold/diagnostic/indicator lookups (None for continuations).
    pub line_start_byte: Option<usize>,
    /// Display line number or byte offset for the gutter.
    pub gutter_num: usize,
    pub estimated_lines: usize,
    pub diagnostic_lines: &'a HashSet<usize>,
    pub line_indicators: &'a BTreeMap<usize, LineIndicator>,
    pub fold_indicators: &'a BTreeMap<usize, FoldIndicator>,
    pub cursor_line_start_byte: usize,
    pub cursor_line_number: usize,
    pub relative_line_numbers: bool,
    pub show_line_numbers: bool,
    pub byte_offset_mode: bool,
    pub highlight_current_line: bool,
    pub is_active: bool,
}

/// Render the left margin (indicators + line numbers + separator) to the
/// provided span / column-map buffers.
pub(super) fn render_left_margin(
    ctx: &LeftMarginContext,
    line_spans: &mut Vec<Span<'static>>,
    line_view_map: &mut Vec<Option<usize>>,
) {
    if !ctx.state.margins.left_config.enabled {
        return;
    }

    let lookup_key = ctx.line_start_byte;
    // Pre-compute indicator bg for cursor line highlighting
    let indicator_is_cursor_line = lookup_key.is_some_and(|k| k == ctx.cursor_line_start_byte);
    let indicator_bg = if indicator_is_cursor_line && ctx.highlight_current_line && ctx.is_active {
        Some(ctx.theme.current_line_bg)
    } else {
        None
    };

    // For continuation lines, don't show any indicators
    if ctx.is_continuation {
        let mut style = Style::default();
        if let Some(bg) = indicator_bg {
            style = style.bg(bg);
        }
        push_span_with_map(line_spans, line_view_map, " ".to_string(), style, None);
    } else if lookup_key.is_some_and(|k| ctx.diagnostic_lines.contains(&k)) {
        // Diagnostic indicators have highest priority
        let mut style = Style::default().fg(Color::Red);
        if let Some(bg) = indicator_bg {
            style = style.bg(bg);
        }
        push_span_with_map(line_spans, line_view_map, "●".to_string(), style, None);
    } else if lookup_key.is_some_and(|k| {
        ctx.fold_indicators.contains_key(&k) && !ctx.line_indicators.contains_key(&k)
    }) {
        let fold = ctx.fold_indicators.get(&lookup_key.unwrap()).unwrap();
        let symbol = if fold.collapsed { "▸" } else { "▾" };
        let mut style = Style::default().fg(ctx.theme.line_number_fg);
        if let Some(bg) = indicator_bg {
            style = style.bg(bg);
        }
        push_span_with_map(line_spans, line_view_map, symbol.to_string(), style, None);
    } else if let Some(indicator) = lookup_key.and_then(|k| ctx.line_indicators.get(&k)) {
        let mut style = Style::default().fg(indicator.color);
        if let Some(bg) = indicator_bg {
            style = style.bg(bg);
        }
        push_span_with_map(
            line_spans,
            line_view_map,
            indicator.symbol.clone(),
            style,
            None,
        );
    } else {
        let mut style = Style::default();
        if let Some(bg) = indicator_bg {
            style = style.bg(bg);
        }
        push_span_with_map(line_spans, line_view_map, " ".to_string(), style, None);
    }

    let is_cursor_line = lookup_key.is_some_and(|k| k == ctx.cursor_line_start_byte);
    let use_cursor_line_bg = is_cursor_line && ctx.highlight_current_line && ctx.is_active;

    if ctx.is_continuation {
        let blank = " ".repeat(ctx.state.margins.left_config.width);
        let mut style = Style::default().fg(ctx.theme.line_number_fg);
        if use_cursor_line_bg {
            style = style.bg(ctx.theme.current_line_bg);
        }
        push_span_with_map(line_spans, line_view_map, blank, style, None);
    } else if ctx.byte_offset_mode && ctx.show_line_numbers {
        let rendered_text = format!(
            "{:>width$}",
            ctx.gutter_num,
            width = ctx.state.margins.left_config.width
        );
        let mut margin_style = if is_cursor_line {
            Style::default().fg(ctx.theme.editor_fg)
        } else {
            Style::default().fg(ctx.theme.line_number_fg)
        };
        if use_cursor_line_bg {
            margin_style = margin_style.bg(ctx.theme.current_line_bg);
        }
        push_span_with_map(line_spans, line_view_map, rendered_text, margin_style, None);
    } else if ctx.relative_line_numbers {
        let display_num = if is_cursor_line {
            ctx.gutter_num + 1
        } else {
            ctx.gutter_num.abs_diff(ctx.cursor_line_number)
        };
        let rendered_text = format!(
            "{:>width$}",
            display_num,
            width = ctx.state.margins.left_config.width
        );
        let mut margin_style = if is_cursor_line {
            Style::default().fg(ctx.theme.editor_fg)
        } else {
            Style::default().fg(ctx.theme.line_number_fg)
        };
        if use_cursor_line_bg {
            margin_style = margin_style.bg(ctx.theme.current_line_bg);
        }
        push_span_with_map(line_spans, line_view_map, rendered_text, margin_style, None);
    } else {
        let margin_content = ctx.state.margins.render_line(
            ctx.gutter_num,
            MarginPosition::Left,
            ctx.estimated_lines,
            ctx.show_line_numbers,
        );
        let (rendered_text, style_opt) = margin_content.render(ctx.state.margins.left_config.width);

        let mut margin_style =
            style_opt.unwrap_or_else(|| Style::default().fg(ctx.theme.line_number_fg));
        if use_cursor_line_bg {
            margin_style = margin_style.bg(ctx.theme.current_line_bg);
        }

        push_span_with_map(line_spans, line_view_map, rendered_text, margin_style, None);
    }

    if ctx.state.margins.left_config.show_separator {
        let mut separator_style = Style::default().fg(ctx.theme.line_number_fg);
        if use_cursor_line_bg {
            separator_style = separator_style.bg(ctx.theme.current_line_bg);
        }
        push_span_with_map(
            line_spans,
            line_view_map,
            ctx.state.margins.left_config.separator.clone(),
            separator_style,
            None,
        );
    }
}

/// Paper-on-desk compose-mode margins flanking the content area.
pub(super) fn render_compose_margins(
    frame: &mut Frame,
    area: Rect,
    layout: &ComposeLayout,
    _view_mode: &ViewMode,
    theme: &Theme,
    effective_editor_bg: Color,
) {
    if layout.left_pad == 0 && layout.right_pad == 0 {
        return;
    }

    // Paper-on-desk effect: outer "desk" margin with inner "paper edge".
    // Layout: [desk][paper edge][content][paper edge][desk]
    const PAPER_EDGE_WIDTH: u16 = 1;

    let desk_style = Style::default().bg(theme.compose_margin_bg);
    let paper_style = Style::default().bg(effective_editor_bg);

    if layout.left_pad > 0 {
        let paper_edge = PAPER_EDGE_WIDTH.min(layout.left_pad);
        let desk_width = layout.left_pad.saturating_sub(paper_edge);

        if desk_width > 0 {
            let desk_rect = Rect::new(area.x, area.y, desk_width, area.height);
            frame.render_widget(Block::default().style(desk_style), desk_rect);
        }

        if paper_edge > 0 {
            let paper_rect = Rect::new(area.x + desk_width, area.y, paper_edge, area.height);
            frame.render_widget(Block::default().style(paper_style), paper_rect);
        }
    }

    if layout.right_pad > 0 {
        let paper_edge = PAPER_EDGE_WIDTH.min(layout.right_pad);
        let desk_width = layout.right_pad.saturating_sub(paper_edge);
        let right_start = area.x + layout.left_pad + layout.render_area.width;

        if paper_edge > 0 {
            let paper_rect = Rect::new(right_start, area.y, paper_edge, area.height);
            frame.render_widget(Block::default().style(paper_style), paper_rect);
        }

        if desk_width > 0 {
            let desk_rect = Rect::new(right_start + paper_edge, area.y, desk_width, area.height);
            frame.render_widget(Block::default().style(desk_style), desk_rect);
        }
    }
}
