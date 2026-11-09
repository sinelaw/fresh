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
use crate::view::theme::ThemeStyle;
use ratatui::layout::Rect;
use ratatui::style::{Color, Style};
use ratatui::text::Span;
use ratatui::widgets::Block;
use ratatui::widgets::Widget;
use std::collections::{BTreeMap, HashSet};

/// Context for rendering the left margin (line numbers, indicators, separator).
///
/// This type is private to the split-rendering module — it is used only by
/// `render_left_margin` and the orchestration code that prepares its input.
pub(super) struct LeftMarginContext<'a> {
    pub state: &'a EditorState,
    /// The pane's resolved left margin; see `LineRenderInput::margin`.
    pub margin: &'a crate::view::margin::MarginConfig,
    pub theme: &'a Theme,
    pub is_continuation: bool,
    /// Line-start byte offset for fold/indicator lookups (None for continuations).
    pub line_start_byte: Option<usize>,
    /// Byte the row's diagnostics are keyed off. Same as `line_start_byte`
    /// except on the single row of an empty buffer, which has no source byte
    /// of its own but still sits at byte 0 and can still carry a diagnostic
    /// (issue #3484).
    pub diagnostic_anchor_byte: Option<usize>,
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
    /// Optional glyph + color rendered into the indicator slot for
    /// virtual lines (`ViewLine::virtual_gutter_glyph`). Source lines
    /// pass `None` and fall back to the existing
    /// diagnostic/fold/line-indicator lookup chain.
    pub virtual_gutter_glyph: Option<&'a (String, Color)>,
    /// Whether the pane hands its background to the terminal
    /// (`editor.use_terminal_bg`): the gutter then has no shade of its own
    /// either, or it would be an opaque band beside transparent text.
    pub terminal_bg: bool,
}

/// How every gutter cell is painted, decided in one place.
///
/// Both gutter painters — [`render_left_margin`] for buffer rows and the
/// implicit row after a trailing newline — ask this, so a theme key means the
/// same thing on every row:
///
/// - `editor.line_number_bg` is the gutter's ground. It is painted only when
///   the theme gives the gutter a shade of its own (one different from
///   `editor.bg`), and only while the gutter shows line numbers; otherwise
///   the gutter shows whatever the text area shows, which keeps
///   `use_terminal_bg` and per-buffer backgrounds intact (under
///   `use_terminal_bg` it is not painted at all). Without numbers
///   the gutter is just its marker column, and a shade there is a stray
///   one-cell stripe beside the text (Markdown's rendered mode hides the
///   numbers but keeps that column).
/// - The cursor's row wears `editor.current_line_bg` instead, across the
///   whole gutter, when the current line is highlighted.
/// - `editor.current_line_number_fg` styles the cursor's line number when the
///   theme names it; otherwise that number keeps its historical look.
/// - `editor.gutter_separator_fg` draws the separator. Whether there is one
///   is the theme's `chrome.gutter_separator`, settled into the margin's
///   width before the frame (`MarginManager::set_gutter_separator`).
pub(super) struct GutterPaint<'a> {
    theme: &'a Theme,
    /// Whether the gutter may wear its own shade: it holds line numbers, which
    /// the shade marks off, and the pane paints its own background.
    shaded: bool,
}

impl<'a> GutterPaint<'a> {
    /// `numbered`: the gutter shows line numbers; `terminal_bg`: the pane
    /// hands its background to the terminal.
    pub(super) fn new(theme: &'a Theme, numbered: bool, terminal_bg: bool) -> Self {
        Self {
            theme,
            shaded: numbered && !terminal_bg,
        }
    }

    /// The ground of a gutter cell. `highlighted` is whether this row is the
    /// cursor's row *and* the current line is being highlighted.
    pub(super) fn ground(&self, highlighted: bool) -> Style {
        if highlighted {
            Style::default().theme_bg(self.theme, "editor.current_line_bg")
        } else if self.shaded && self.theme.line_number_bg != self.theme.editor_bg {
            Style::default().theme_bg(self.theme, "editor.line_number_bg")
        } else {
            Style::default()
        }
    }

    /// `style` on this row's ground.
    pub(super) fn on_ground(&self, style: Style, highlighted: bool) -> Style {
        style.patch(self.ground(highlighted))
    }

    /// A line number's ink. `cursor_line` is whether it numbers the cursor's
    /// line; `emphasized` is whether that number has historically been drawn
    /// in `editor.fg` (relative numbers, byte offsets, diff numbering) rather
    /// than in the gutter's dim color.
    pub(super) fn number(&self, cursor_line: bool, emphasized: bool) -> Style {
        if cursor_line && self.theme.current_line_number_fg.is_some() {
            Style::default().theme_fg(self.theme, "editor.current_line_number_fg")
        } else if cursor_line && emphasized {
            Style::default().theme_fg(self.theme, "editor.fg")
        } else {
            Style::default().theme_fg(self.theme, "editor.line_number_fg")
        }
    }

    /// The separator's text and style.
    pub(super) fn separator(&self, separator: &str, highlighted: bool) -> (String, Style) {
        let ink = Style::default().theme_fg(self.theme, "editor.gutter_separator_fg");
        (separator.to_string(), self.on_ground(ink, highlighted))
    }

    /// The gutter's diagnostic marker.
    pub(super) fn diagnostic(&self, highlighted: bool) -> Style {
        self.on_ground(
            Style::default().theme_fg(self.theme, "diagnostic.error_fg"),
            highlighted,
        )
    }
}

/// Render the left margin (indicators + line numbers + separator) to the
/// provided span / column-map buffers.
/// The indicator column's text, exactly one cell wide.
///
/// The margin is measured with one cell for it (`MarginConfig::total_width`),
/// and the cursor and the mouse are mapped through that width, while its
/// glyph comes from the theme (`chrome.fold_*`, `diagnostic_marker`) or a
/// plugin. A wider glyph (an emoji) would push the row's numbers and text
/// one column right of every other row; an empty one, one column left. Its
/// first grapheme is kept when that is one cell, else the cell is blank.
pub(super) fn one_cell(glyph: String) -> String {
    use unicode_segmentation::UnicodeSegmentation;
    use unicode_width::UnicodeWidthStr;
    match glyph.graphemes(true).next() {
        Some(first) if first.width() == 1 => {
            if first.len() == glyph.len() {
                glyph
            } else {
                first.to_string()
            }
        }
        _ => " ".to_string(),
    }
}

pub(super) fn render_left_margin(
    ctx: &LeftMarginContext,
    line_spans: &mut Vec<Span<'static>>,
    line_view_map: &mut Vec<Option<usize>>,
) {
    if !ctx.margin.enabled {
        return;
    }

    let paint = GutterPaint::new(ctx.theme, ctx.show_line_numbers, ctx.terminal_bg);
    let lookup_key = ctx.line_start_byte;
    let is_cursor_line = lookup_key.is_some_and(|k| k == ctx.cursor_line_start_byte);
    let highlighted = is_cursor_line && ctx.highlight_current_line && ctx.is_active;

    // Virtual lines whose source `VirtualText` carries a `gutter_glyph`
    // get their own indicator (e.g. live-diff's "-" on a deletion
    // line). This wins over the "continuation = blank" rule below so
    // the glyph actually appears in the gutter; subsequent wrapped
    // rows of the same virtual line set `virtual_gutter_glyph` to
    // `None`, so they keep the blank fallback.
    let (indicator, indicator_style) = if let Some((glyph, color)) = ctx.virtual_gutter_glyph {
        (glyph.clone(), Style::default().fg(*color))
    } else if ctx.is_continuation {
        // For continuation lines, don't show any indicators
        (" ".to_string(), Style::default())
    } else if ctx
        .diagnostic_anchor_byte
        .is_some_and(|k| ctx.diagnostic_lines.contains(&k))
    {
        // Diagnostic indicators have highest priority
        (
            ctx.theme.chrome.glyphs().diagnostic_marker.clone(),
            paint.diagnostic(false),
        )
    } else if ctx.state.diff_gutter.is_none()
        && lookup_key.is_some_and(|k| {
            ctx.fold_indicators.contains_key(&k) && !ctx.line_indicators.contains_key(&k)
        })
    {
        // A diff stream draws its own collapse glyphs, in the text, on
        // the rows that can be collapsed; a margin glyph beside them
        // would be a second, differently-placed answer to the same
        // question.
        let fold = ctx.fold_indicators.get(&lookup_key.unwrap()).unwrap();
        let chrome = ctx.theme.chrome.glyphs();
        let symbol = if fold.collapsed {
            &chrome.fold_collapsed
        } else {
            &chrome.fold_expanded
        };
        (
            symbol.clone(),
            Style::default().theme_fg(ctx.theme, "editor.line_number_fg"),
        )
    } else if let Some(indicator) = lookup_key.and_then(|k| ctx.line_indicators.get(&k)) {
        (
            indicator.symbol.clone(),
            Style::default().fg(indicator.color),
        )
    } else {
        (" ".to_string(), Style::default())
    };
    push_span_with_map(
        line_spans,
        line_view_map,
        one_cell(indicator),
        paint.on_ground(indicator_style, highlighted),
        None,
    );

    let (rendered_text, number_style) = if ctx.is_continuation {
        (
            " ".repeat(ctx.margin.width),
            Style::default().theme_fg(ctx.theme, "editor.line_number_fg"),
        )
    } else if let Some(gutter) = ctx.state.diff_gutter.as_ref() {
        // A diff stream numbers its rows from their hunk headers, not from
        // their position in the buffer.
        (
            gutter.render(ctx.gutter_num),
            paint.number(is_cursor_line, true),
        )
    } else if ctx.byte_offset_mode && ctx.show_line_numbers {
        (
            format!("{:>width$}", ctx.gutter_num, width = ctx.margin.width),
            paint.number(is_cursor_line, true),
        )
    } else if ctx.relative_line_numbers {
        let display_num = if is_cursor_line {
            ctx.gutter_num + 1
        } else {
            ctx.gutter_num.abs_diff(ctx.cursor_line_number)
        };
        (
            format!("{:>width$}", display_num, width = ctx.margin.width),
            paint.number(is_cursor_line, true),
        )
    } else {
        let margin_content = ctx.state.margins.render_line(
            ctx.gutter_num,
            MarginPosition::Left,
            ctx.estimated_lines,
            ctx.show_line_numbers,
        );
        let (rendered_text, style_opt) = margin_content.render(ctx.margin.width);
        // A margin annotation's own style wins over the gutter's.
        (
            rendered_text,
            style_opt.unwrap_or_else(|| paint.number(is_cursor_line, false)),
        )
    };
    push_span_with_map(
        line_spans,
        line_view_map,
        rendered_text,
        paint.on_ground(number_style, highlighted),
        None,
    );

    if ctx.margin.show_separator {
        let (text, style) = paint.separator(&ctx.margin.separator, highlighted);
        push_span_with_map(line_spans, line_view_map, text, style, None);
    }
}

/// Paper-on-desk compose-mode margins flanking the content area.
pub(super) fn render_compose_margins(
    buf: &mut ratatui::buffer::Buffer,
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

    let desk_style = Style::default().theme_bg(theme, "ui.compose_margin_bg");
    let paper_style = Style::default().bg(effective_editor_bg);

    if layout.left_pad > 0 {
        let paper_edge = PAPER_EDGE_WIDTH.min(layout.left_pad);
        let desk_width = layout.left_pad.saturating_sub(paper_edge);

        if desk_width > 0 {
            let desk_rect = Rect::new(area.x, area.y, desk_width, area.height);
            Block::default().style(desk_style).render(desk_rect, buf);
        }

        if paper_edge > 0 {
            let paper_rect = Rect::new(area.x + desk_width, area.y, paper_edge, area.height);
            Block::default().style(paper_style).render(paper_rect, buf);
        }
    }

    if layout.right_pad > 0 {
        let paper_edge = PAPER_EDGE_WIDTH.min(layout.right_pad);
        let desk_width = layout.right_pad.saturating_sub(paper_edge);
        let right_start = area.x + layout.left_pad + layout.render_area.width;

        if paper_edge > 0 {
            let paper_rect = Rect::new(right_start, area.y, paper_edge, area.height);
            Block::default().style(paper_style).render(paper_rect, buf);
        }

        if desk_width > 0 {
            let desk_rect = Rect::new(right_start + paper_edge, area.y, desk_width, area.height);
            Block::default().style(desk_style).render(desk_rect, buf);
        }
    }
}
