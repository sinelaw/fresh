//! Split pane layout and buffer rendering

use std::collections::BTreeMap;

use crate::app::types::ViewLineMapping;
use crate::app::BufferMetadata;
use crate::model::buffer::Buffer;
use crate::model::cursor::SelectionMode;
use crate::model::event::{BufferId, EventLog, SplitDirection};
use crate::primitives::ansi::AnsiParser;
use crate::primitives::ansi_background::AnsiBackground;
use crate::primitives::display_width::char_width;
use crate::state::{EditorState, ViewMode};
use crate::view::split::SplitManager;
use crate::view::ui::tabs::TabsRenderer;
use crate::view::ui::view_pipeline::{
    should_show_line_number, LineStart, ViewLine, ViewLineIterator,
};
use crate::view::virtual_text::VirtualTextPosition;
use fresh_core::api::ViewTransformPayload;
use ratatui::layout::Rect;
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Paragraph};
use ratatui::Frame;
use std::collections::{HashMap, HashSet};
use std::ops::Range;

/// Maximum line width before forced wrapping is applied, even when line wrapping is disabled.
/// This prevents memory exhaustion when opening files with extremely long lines (e.g., 10MB
/// single-line JSON files). Lines exceeding this width are wrapped into multiple visual lines,
/// each bounded to this width. 10,000 columns is far wider than any monitor while keeping
/// memory usage reasonable (~80KB per ViewLine instead of hundreds of MB).
const MAX_SAFE_LINE_WIDTH: usize = 10_000;

/// Compute character-level diff between two strings, returning ranges of changed characters.
/// Returns a tuple of (old_changed_ranges, new_changed_ranges) where each range indicates
/// character indices that differ between the strings.
fn compute_inline_diff(old_text: &str, new_text: &str) -> (Vec<Range<usize>>, Vec<Range<usize>>) {
    let old_chars: Vec<char> = old_text.chars().collect();
    let new_chars: Vec<char> = new_text.chars().collect();

    let mut old_ranges = Vec::new();
    let mut new_ranges = Vec::new();

    // Find common prefix
    let prefix_len = old_chars
        .iter()
        .zip(new_chars.iter())
        .take_while(|(a, b)| a == b)
        .count();

    // Find common suffix (from the non-prefix part)
    let old_remaining = old_chars.len() - prefix_len;
    let new_remaining = new_chars.len() - prefix_len;
    let suffix_len = old_chars
        .iter()
        .rev()
        .zip(new_chars.iter().rev())
        .take(old_remaining.min(new_remaining))
        .take_while(|(a, b)| a == b)
        .count();

    // The changed range is between prefix and suffix
    let old_start = prefix_len;
    let old_end = old_chars.len().saturating_sub(suffix_len);
    let new_start = prefix_len;
    let new_end = new_chars.len().saturating_sub(suffix_len);

    if old_start < old_end {
        old_ranges.push(old_start..old_end);
    }
    if new_start < new_end {
        new_ranges.push(new_start..new_end);
    }

    (old_ranges, new_ranges)
}

fn push_span_with_map(
    spans: &mut Vec<Span<'static>>,
    map: &mut Vec<Option<usize>>,
    text: String,
    style: Style,
    source: Option<usize>,
) {
    if text.is_empty() {
        return;
    }
    // Push one map entry per visual column (not per character)
    // Double-width characters (CJK, emoji) need 2 entries
    // Zero-width characters (like \u{200b}) get 0 entries - they don't occupy screen space
    for ch in text.chars() {
        let width = char_width(ch);
        for _ in 0..width {
            map.push(source);
        }
    }
    spans.push(Span::styled(text, style));
}

/// Debug tag style - dim/muted color to distinguish from actual content
fn debug_tag_style() -> Style {
    Style::default()
        .fg(Color::DarkGray)
        .add_modifier(Modifier::DIM)
}

/// Compute a dimmed version of a color for EOF tilde lines.
/// This replaces using Modifier::DIM which can bleed through to overlays.
fn dim_color_for_tilde(color: Color) -> Color {
    match color {
        Color::Rgb(r, g, b) => {
            // Reduce brightness by ~50% (similar to DIM modifier effect)
            Color::Rgb(r / 2, g / 2, b / 2)
        }
        Color::Indexed(idx) => {
            // For indexed colors, map to a reasonable dim equivalent
            // Standard colors 0-7: use corresponding bright versions dimmed
            // Bright colors 8-15: dim them down
            // Grayscale and cube colors: just use a dark gray
            if idx < 16 {
                Color::Rgb(50, 50, 50) // Dark gray for basic colors
            } else {
                Color::Rgb(40, 40, 40) // Slightly darker for extended colors
            }
        }
        // Map named colors to dimmed RGB equivalents
        Color::Black => Color::Rgb(15, 15, 15),
        Color::White => Color::Rgb(128, 128, 128),
        Color::Red => Color::Rgb(100, 30, 30),
        Color::Green => Color::Rgb(30, 100, 30),
        Color::Yellow => Color::Rgb(100, 100, 30),
        Color::Blue => Color::Rgb(30, 30, 100),
        Color::Magenta => Color::Rgb(100, 30, 100),
        Color::Cyan => Color::Rgb(30, 100, 100),
        Color::Gray => Color::Rgb(64, 64, 64),
        Color::DarkGray => Color::Rgb(40, 40, 40),
        Color::LightRed => Color::Rgb(128, 50, 50),
        Color::LightGreen => Color::Rgb(50, 128, 50),
        Color::LightYellow => Color::Rgb(128, 128, 50),
        Color::LightBlue => Color::Rgb(50, 50, 128),
        Color::LightMagenta => Color::Rgb(128, 50, 128),
        Color::LightCyan => Color::Rgb(50, 128, 128),
        Color::Reset => Color::Rgb(50, 50, 50),
    }
}

/// Accumulator for building spans - collects characters with the same style
/// into a single span, flushing when style changes. This is important for
/// proper rendering of combining characters (like Thai diacritics) which
/// must be in the same string as their base character.
struct SpanAccumulator {
    text: String,
    style: Style,
    first_source: Option<usize>,
}

impl SpanAccumulator {
    fn new() -> Self {
        Self {
            text: String::new(),
            style: Style::default(),
            first_source: None,
        }
    }

    /// Add a character to the accumulator. If the style matches, append to current span.
    /// If style differs, flush the current span first and start a new one.
    fn push(
        &mut self,
        ch: char,
        style: Style,
        source: Option<usize>,
        spans: &mut Vec<Span<'static>>,
        map: &mut Vec<Option<usize>>,
    ) {
        // If we have accumulated text and the style changed, flush first
        if !self.text.is_empty() && style != self.style {
            self.flush(spans, map);
        }

        // Start new accumulation if empty
        if self.text.is_empty() {
            self.style = style;
            self.first_source = source;
        }

        self.text.push(ch);

        // Update map for this character's visual width
        let width = char_width(ch);
        for _ in 0..width {
            map.push(source);
        }
    }

    /// Flush accumulated text as a span
    fn flush(&mut self, spans: &mut Vec<Span<'static>>, _map: &mut Vec<Option<usize>>) {
        if !self.text.is_empty() {
            spans.push(Span::styled(std::mem::take(&mut self.text), self.style));
            self.first_source = None;
        }
    }
}

/// Push a debug tag span (no map entries since these aren't real content)
fn push_debug_tag(spans: &mut Vec<Span<'static>>, map: &mut Vec<Option<usize>>, text: String) {
    if text.is_empty() {
        return;
    }
    // Debug tags don't map to source positions - they're visual-only
    for ch in text.chars() {
        let width = char_width(ch);
        for _ in 0..width {
            map.push(None);
        }
    }
    spans.push(Span::styled(text, debug_tag_style()));
}

/// Context for tracking active spans in debug mode
#[derive(Default)]
struct DebugSpanTracker {
    /// Currently active highlight span (byte range)
    active_highlight: Option<Range<usize>>,
    /// Currently active overlay spans (byte ranges)
    active_overlays: Vec<Range<usize>>,
}

impl DebugSpanTracker {
    /// Get opening tags for spans that start at this byte position
    fn get_opening_tags(
        &mut self,
        byte_pos: Option<usize>,
        highlight_spans: &[crate::primitives::highlighter::HighlightSpan],
        viewport_overlays: &[(crate::view::overlay::Overlay, Range<usize>)],
    ) -> Vec<String> {
        let mut tags = Vec::new();

        if let Some(bp) = byte_pos {
            // Check if we're entering a new highlight span
            if let Some(span) = highlight_spans.iter().find(|s| s.range.start == bp) {
                tags.push(format!("<hl:{}-{}>", span.range.start, span.range.end));
                self.active_highlight = Some(span.range.clone());
            }

            // Check if we're entering new overlay spans
            for (overlay, range) in viewport_overlays.iter() {
                if range.start == bp {
                    let overlay_type = match &overlay.face {
                        crate::view::overlay::OverlayFace::Underline { .. } => "ul",
                        crate::view::overlay::OverlayFace::Background { .. } => "bg",
                        crate::view::overlay::OverlayFace::Foreground { .. } => "fg",
                        crate::view::overlay::OverlayFace::Style { .. } => "st",
                        crate::view::overlay::OverlayFace::ThemedStyle { .. } => "ts",
                    };
                    tags.push(format!("<{}:{}-{}>", overlay_type, range.start, range.end));
                    self.active_overlays.push(range.clone());
                }
            }
        }

        tags
    }

    /// Get closing tags for spans that end at this byte position
    fn get_closing_tags(&mut self, byte_pos: Option<usize>) -> Vec<String> {
        let mut tags = Vec::new();

        if let Some(bp) = byte_pos {
            // Check if we're exiting the active highlight span
            if let Some(ref range) = self.active_highlight {
                if bp >= range.end {
                    tags.push("</hl>".to_string());
                    self.active_highlight = None;
                }
            }

            // Check if we're exiting any overlay spans
            let mut closed_indices = Vec::new();
            for (i, range) in self.active_overlays.iter().enumerate() {
                if bp >= range.end {
                    tags.push("</ov>".to_string());
                    closed_indices.push(i);
                }
            }
            // Remove closed overlays (in reverse order to preserve indices)
            for i in closed_indices.into_iter().rev() {
                self.active_overlays.remove(i);
            }
        }

        tags
    }
}

/// Processed view data containing display lines from the view pipeline
struct ViewData {
    /// Display lines with all token information preserved
    lines: Vec<ViewLine>,
}

struct ViewAnchor {
    start_line_idx: usize,
    start_line_skip: usize,
}

struct ComposeLayout {
    render_area: Rect,
    left_pad: u16,
    right_pad: u16,
}

struct SelectionContext {
    ranges: Vec<Range<usize>>,
    block_rects: Vec<(usize, usize, usize, usize)>,
    cursor_positions: Vec<usize>,
    primary_cursor_position: usize,
}

struct DecorationContext {
    highlight_spans: Vec<crate::primitives::highlighter::HighlightSpan>,
    semantic_token_spans: Vec<crate::primitives::highlighter::HighlightSpan>,
    viewport_overlays: Vec<(crate::view::overlay::Overlay, Range<usize>)>,
    virtual_text_lookup: HashMap<usize, Vec<crate::view::virtual_text::VirtualText>>,
    diagnostic_lines: HashSet<usize>,
    /// Line indicators indexed by line number (highest priority indicator per line)
    line_indicators: BTreeMap<usize, crate::view::margin::LineIndicator>,
}

struct LineRenderOutput {
    lines: Vec<Line<'static>>,
    cursor: Option<(u16, u16)>,
    last_line_end: Option<LastLineEnd>,
    content_lines_rendered: usize,
    view_line_mappings: Vec<ViewLineMapping>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct LastLineEnd {
    pos: (u16, u16),
    terminated_with_newline: bool,
}

struct SplitLayout {
    tabs_rect: Rect,
    content_rect: Rect,
    scrollbar_rect: Rect,
    horizontal_scrollbar_rect: Rect,
}

struct ViewPreferences {
    view_mode: ViewMode,
    compose_width: Option<u16>,
    compose_column_guides: Option<Vec<u16>>,
    view_transform: Option<ViewTransformPayload>,
    rulers: Vec<usize>,
    /// Per-split line number override (None = use buffer default)
    show_line_numbers: Option<bool>,
}

struct LineRenderInput<'a> {
    state: &'a EditorState,
    theme: &'a crate::view::theme::Theme,
    /// Display lines from the view pipeline (each line has its own mappings, styles, etc.)
    view_lines: &'a [ViewLine],
    view_anchor: ViewAnchor,
    render_area: Rect,
    gutter_width: usize,
    selection: &'a SelectionContext,
    decorations: &'a DecorationContext,
    starting_line_num: usize,
    visible_line_count: usize,
    lsp_waiting: bool,
    is_active: bool,
    line_wrap: bool,
    estimated_lines: usize,
    /// Left column offset for horizontal scrolling
    left_column: usize,
    /// Whether to show relative line numbers (distance from cursor)
    relative_line_numbers: bool,
    /// Session mode: use hardware cursor only, skip REVERSED style for software cursor
    session_mode: bool,
}

/// Context for computing the style of a single character
struct CharStyleContext<'a> {
    byte_pos: Option<usize>,
    token_style: Option<&'a fresh_core::api::ViewTokenStyle>,
    ansi_style: Style,
    is_cursor: bool,
    is_selected: bool,
    theme: &'a crate::view::theme::Theme,
    highlight_spans: &'a [crate::primitives::highlighter::HighlightSpan],
    semantic_token_spans: &'a [crate::primitives::highlighter::HighlightSpan],
    viewport_overlays: &'a [(crate::view::overlay::Overlay, Range<usize>)],
    primary_cursor_position: usize,
    is_active: bool,
    /// Session mode: use hardware cursor only, skip REVERSED style for software cursor
    session_mode: bool,
}

/// Output from compute_char_style
struct CharStyleOutput {
    style: Style,
    is_secondary_cursor: bool,
}

/// Context for rendering the left margin (line numbers, indicators, separator)
struct LeftMarginContext<'a> {
    state: &'a EditorState,
    theme: &'a crate::view::theme::Theme,
    is_continuation: bool,
    current_source_line_num: usize,
    estimated_lines: usize,
    diagnostic_lines: &'a HashSet<usize>,
    /// Pre-computed line indicators (line_num -> indicator)
    line_indicators: &'a BTreeMap<usize, crate::view::margin::LineIndicator>,
    /// Line number where the primary cursor is located (for relative line numbers)
    cursor_line: usize,
    /// Whether to show relative line numbers
    relative_line_numbers: bool,
}

/// Render the left margin (indicators + line numbers + separator) to line_spans
fn render_left_margin(
    ctx: &LeftMarginContext,
    line_spans: &mut Vec<Span<'static>>,
    line_view_map: &mut Vec<Option<usize>>,
) {
    if !ctx.state.margins.left_config.enabled {
        return;
    }

    // For continuation lines, don't show any indicators
    if ctx.is_continuation {
        push_span_with_map(
            line_spans,
            line_view_map,
            " ".to_string(),
            Style::default(),
            None,
        );
    } else if ctx.diagnostic_lines.contains(&ctx.current_source_line_num) {
        // Diagnostic indicators have highest priority
        push_span_with_map(
            line_spans,
            line_view_map,
            "●".to_string(),
            Style::default().fg(ratatui::style::Color::Red),
            None,
        );
    } else if let Some(indicator) = ctx.line_indicators.get(&ctx.current_source_line_num) {
        // Show line indicator (git gutter, breakpoints, etc.)
        push_span_with_map(
            line_spans,
            line_view_map,
            indicator.symbol.clone(),
            Style::default().fg(indicator.color),
            None,
        );
    } else {
        // Show space (no indicator)
        push_span_with_map(
            line_spans,
            line_view_map,
            " ".to_string(),
            Style::default(),
            None,
        );
    }

    // Render line number (right-aligned) or blank for continuations
    if ctx.is_continuation {
        // For wrapped continuation lines, render blank space
        let blank = " ".repeat(ctx.state.margins.left_config.width);
        push_span_with_map(
            line_spans,
            line_view_map,
            blank,
            Style::default().fg(ctx.theme.line_number_fg),
            None,
        );
    } else if ctx.relative_line_numbers {
        // Relative line numbers: show distance from cursor, or absolute for cursor line
        let display_num = if ctx.current_source_line_num == ctx.cursor_line {
            // Show absolute line number for the cursor line (1-indexed)
            ctx.current_source_line_num + 1
        } else {
            // Show relative distance for other lines
            ctx.current_source_line_num.abs_diff(ctx.cursor_line)
        };
        let rendered_text = format!(
            "{:>width$}",
            display_num,
            width = ctx.state.margins.left_config.width
        );
        // Use brighter color for the cursor line
        let margin_style = if ctx.current_source_line_num == ctx.cursor_line {
            Style::default().fg(ctx.theme.editor_fg)
        } else {
            Style::default().fg(ctx.theme.line_number_fg)
        };
        push_span_with_map(line_spans, line_view_map, rendered_text, margin_style, None);
    } else {
        let margin_content = ctx.state.margins.render_line(
            ctx.current_source_line_num,
            crate::view::margin::MarginPosition::Left,
            ctx.estimated_lines,
        );
        let (rendered_text, style_opt) = margin_content.render(ctx.state.margins.left_config.width);

        // Use custom style if provided, otherwise use default theme color
        let margin_style =
            style_opt.unwrap_or_else(|| Style::default().fg(ctx.theme.line_number_fg));

        push_span_with_map(line_spans, line_view_map, rendered_text, margin_style, None);
    }

    // Render separator
    if ctx.state.margins.left_config.show_separator {
        let separator_style = Style::default().fg(ctx.theme.line_number_fg);
        push_span_with_map(
            line_spans,
            line_view_map,
            ctx.state.margins.left_config.separator.clone(),
            separator_style,
            None,
        );
    }
}

/// Compute the style for a character by layering: token -> ANSI -> syntax -> semantic -> overlays -> selection -> cursor
fn compute_char_style(ctx: &CharStyleContext) -> CharStyleOutput {
    use crate::view::overlay::OverlayFace;

    // Find highlight color for this byte position
    let highlight_color = ctx.byte_pos.and_then(|bp| {
        ctx.highlight_spans
            .iter()
            .find(|span| span.range.contains(&bp))
            .map(|span| span.color)
    });

    // Find overlays for this byte position
    let overlays: Vec<&crate::view::overlay::Overlay> = if let Some(bp) = ctx.byte_pos {
        ctx.viewport_overlays
            .iter()
            .filter(|(_, range)| range.contains(&bp))
            .map(|(overlay, _)| overlay)
            .collect()
    } else {
        Vec::new()
    };

    // Start with token style if present (for injected content like annotation headers)
    // Otherwise use ANSI/syntax/theme default
    let mut style = if let Some(ts) = ctx.token_style {
        let mut s = Style::default();
        if let Some((r, g, b)) = ts.fg {
            s = s.fg(ratatui::style::Color::Rgb(r, g, b));
        } else {
            s = s.fg(ctx.theme.editor_fg);
        }
        if let Some((r, g, b)) = ts.bg {
            s = s.bg(ratatui::style::Color::Rgb(r, g, b));
        }
        if ts.bold {
            s = s.add_modifier(Modifier::BOLD);
        }
        if ts.italic {
            s = s.add_modifier(Modifier::ITALIC);
        }
        s
    } else if ctx.ansi_style.fg.is_some()
        || ctx.ansi_style.bg.is_some()
        || !ctx.ansi_style.add_modifier.is_empty()
    {
        // Apply ANSI styling from escape codes
        let mut s = Style::default();
        if let Some(fg) = ctx.ansi_style.fg {
            s = s.fg(fg);
        } else {
            s = s.fg(ctx.theme.editor_fg);
        }
        if let Some(bg) = ctx.ansi_style.bg {
            s = s.bg(bg);
        }
        s = s.add_modifier(ctx.ansi_style.add_modifier);
        s
    } else if let Some(color) = highlight_color {
        // Apply syntax highlighting
        Style::default().fg(color)
    } else {
        // Default color from theme
        Style::default().fg(ctx.theme.editor_fg)
    };

    // If we have ANSI style but also syntax highlighting, syntax takes precedence for color
    // (unless ANSI has explicit color which we already applied above)
    if let Some(color) = highlight_color {
        if ctx.ansi_style.fg.is_none()
            && (ctx.ansi_style.bg.is_some() || !ctx.ansi_style.add_modifier.is_empty())
        {
            style = style.fg(color);
        }
    }

    // Note: Reference highlighting (word under cursor) is now handled via overlays
    // in the "Apply overlay styles" section below

    // Apply LSP semantic token foreground color when no custom token style is set.
    if ctx.token_style.is_none() {
        if let Some(bp) = ctx.byte_pos {
            if let Some(token_span) = ctx
                .semantic_token_spans
                .iter()
                .find(|span| span.range.contains(&bp))
            {
                style = style.fg(token_span.color);
            }
        }
    }

    // Apply overlay styles
    for overlay in &overlays {
        match &overlay.face {
            OverlayFace::Underline {
                color,
                style: _underline_style,
            } => {
                style = style.add_modifier(Modifier::UNDERLINED).fg(*color);
            }
            OverlayFace::Background { color } => {
                style = style.bg(*color);
            }
            OverlayFace::Foreground { color } => {
                style = style.fg(*color);
            }
            OverlayFace::Style {
                style: overlay_style,
            } => {
                style = style.patch(*overlay_style);
            }
            OverlayFace::ThemedStyle {
                fallback_style,
                fg_theme,
                bg_theme,
            } => {
                let mut themed_style = *fallback_style;
                if let Some(fg_key) = fg_theme {
                    if let Some(color) = ctx.theme.resolve_theme_key(fg_key) {
                        themed_style = themed_style.fg(color);
                    }
                }
                if let Some(bg_key) = bg_theme {
                    if let Some(color) = ctx.theme.resolve_theme_key(bg_key) {
                        themed_style = themed_style.bg(color);
                    }
                }
                style = style.patch(themed_style);
            }
        }
    }

    // Apply selection highlighting
    if ctx.is_selected {
        style = Style::default()
            .fg(ctx.theme.editor_fg)
            .bg(ctx.theme.selection_bg);
    }

    // Apply cursor styling - make all cursors visible with reversed colors
    // For active splits: apply REVERSED to ensure character under cursor is visible
    // (especially important for block cursors where white-on-white would be invisible)
    // For inactive splits: use a less pronounced background color (no hardware cursor)
    let is_secondary_cursor = ctx.is_cursor && ctx.byte_pos != Some(ctx.primary_cursor_position);
    if ctx.is_active {
        if ctx.is_cursor {
            if ctx.session_mode {
                // Session mode: rely on hardware cursor for primary cursor, no REVERSED
                // (REVERSED + hardware cursor = double inversion = invisible cursor)
                // Secondary cursors still need visual indicator via REVERSED
                if is_secondary_cursor {
                    style = style.add_modifier(Modifier::REVERSED);
                }
            } else {
                // Normal mode: apply REVERSED to all cursor positions (primary and secondary)
                // This ensures the character under the cursor is always visible
                style = style.add_modifier(Modifier::REVERSED);
            }
        }
    } else if ctx.is_cursor {
        style = style.fg(ctx.theme.editor_fg).bg(ctx.theme.inactive_cursor);
    }

    CharStyleOutput {
        style,
        is_secondary_cursor,
    }
}

/// Renders split panes and their content
pub struct SplitRenderer;

impl SplitRenderer {
    /// Render the main content area with all splits
    ///
    /// # Arguments
    /// * `frame` - The ratatui frame to render to
    /// * `area` - The rectangular area to render in
    /// * `split_manager` - The split manager
    /// * `buffers` - All open buffers
    /// * `buffer_metadata` - Metadata for buffers (contains display names)
    /// * `event_logs` - Event logs for each buffer
    /// * `theme` - The active theme for colors
    /// * `lsp_waiting` - Whether LSP is waiting
    /// * `large_file_threshold_bytes` - Threshold for using constant scrollbar thumb size
    /// * `line_wrap` - Whether line wrapping is enabled
    /// * `estimated_line_length` - Estimated average line length for large file line estimation
    /// * `hide_cursor` - Whether to hide the hardware cursor (e.g., when menu is open)
    ///
    /// # Returns
    /// * Vec of (split_id, buffer_id, content_rect, scrollbar_rect, thumb_start, thumb_end) for mouse handling
    #[allow(clippy::too_many_arguments)]
    #[allow(clippy::type_complexity)]
    pub fn render_content(
        frame: &mut Frame,
        area: Rect,
        split_manager: &SplitManager,
        buffers: &mut HashMap<BufferId, EditorState>,
        buffer_metadata: &HashMap<BufferId, BufferMetadata>,
        event_logs: &mut HashMap<BufferId, EventLog>,
        composite_buffers: &HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,
        composite_view_states: &mut HashMap<
            (crate::model::event::SplitId, BufferId),
            crate::view::composite_view::CompositeViewState,
        >,
        theme: &crate::view::theme::Theme,
        ansi_background: Option<&AnsiBackground>,
        background_fade: f32,
        lsp_waiting: bool,
        large_file_threshold_bytes: u64,
        _line_wrap: bool,
        estimated_line_length: usize,
        highlight_context_bytes: usize,
        mut split_view_states: Option<
            &mut HashMap<crate::model::event::SplitId, crate::view::split::SplitViewState>,
        >,
        hide_cursor: bool,
        hovered_tab: Option<(BufferId, crate::model::event::SplitId, bool)>, // (buffer_id, split_id, is_close_button)
        hovered_close_split: Option<crate::model::event::SplitId>,
        hovered_maximize_split: Option<crate::model::event::SplitId>,
        is_maximized: bool,
        relative_line_numbers: bool,
        tab_bar_visible: bool,
        use_terminal_bg: bool,
        session_mode: bool,
        show_vertical_scrollbar: bool,
        show_horizontal_scrollbar: bool,
    ) -> (
        Vec<(
            crate::model::event::SplitId,
            BufferId,
            Rect,
            Rect,
            usize,
            usize,
        )>,
        HashMap<crate::model::event::SplitId, crate::view::ui::tabs::TabLayout>, // tab layouts per split
        Vec<(crate::model::event::SplitId, u16, u16, u16)>, // close split button areas
        Vec<(crate::model::event::SplitId, u16, u16, u16)>, // maximize split button areas
        HashMap<crate::model::event::SplitId, Vec<ViewLineMapping>>, // view line mappings for mouse clicks
        Vec<(
            crate::model::event::SplitId,
            BufferId,
            Rect,
            usize,
            usize,
            usize,
        )>, // horizontal scrollbar areas (rect + max_content_width + thumb_start + thumb_end)
    ) {
        let _span = tracing::trace_span!("render_content").entered();

        // Get all visible splits with their areas
        let visible_buffers = split_manager.get_visible_buffers(area);
        let active_split_id = split_manager.active_split();
        let has_multiple_splits = visible_buffers.len() > 1;

        // Collect areas for mouse handling
        let mut split_areas = Vec::new();
        let mut horizontal_scrollbar_areas: Vec<(
            crate::model::event::SplitId,
            BufferId,
            Rect,
            usize,
            usize,
            usize,
        )> = Vec::new();
        let mut tab_layouts: HashMap<
            crate::model::event::SplitId,
            crate::view::ui::tabs::TabLayout,
        > = HashMap::new();
        let mut close_split_areas = Vec::new();
        let mut maximize_split_areas = Vec::new();
        let mut view_line_mappings: HashMap<crate::model::event::SplitId, Vec<ViewLineMapping>> =
            HashMap::new();

        // Render each split
        for (split_id, buffer_id, split_area) in visible_buffers {
            let is_active = split_id == active_split_id;

            let layout = Self::split_layout(
                split_area,
                tab_bar_visible,
                show_vertical_scrollbar,
                show_horizontal_scrollbar,
            );
            let (split_buffers, tab_scroll_offset) =
                Self::split_buffers_for_tabs(split_view_states.as_deref(), split_id, buffer_id);

            // Determine hover state for this split's tabs
            let tab_hover_for_split = hovered_tab.and_then(|(hover_buf, hover_split, is_close)| {
                if hover_split == split_id {
                    Some((hover_buf, is_close))
                } else {
                    None
                }
            });

            // Only render tabs and split control buttons when tab bar is visible
            if tab_bar_visible {
                // Render tabs for this split and collect hit areas
                let tab_layout = TabsRenderer::render_for_split(
                    frame,
                    layout.tabs_rect,
                    &split_buffers,
                    buffers,
                    buffer_metadata,
                    composite_buffers,
                    buffer_id, // The currently displayed buffer in this split
                    theme,
                    is_active,
                    tab_scroll_offset,
                    tab_hover_for_split,
                );

                // Store the tab layout for this split
                tab_layouts.insert(split_id, tab_layout);
                let tab_row = layout.tabs_rect.y;

                // Render split control buttons at the right side of tabs row
                // Show maximize/unmaximize button when: multiple splits exist OR we're currently maximized
                // Show close button when: multiple splits exist AND we're not maximized
                let show_maximize_btn = has_multiple_splits || is_maximized;
                let show_close_btn = has_multiple_splits && !is_maximized;

                if show_maximize_btn || show_close_btn {
                    // Calculate button positions from right edge
                    // Layout: [maximize] [space] [close] |
                    let mut btn_x = layout.tabs_rect.x + layout.tabs_rect.width.saturating_sub(2);

                    // Render close button first (rightmost) if visible
                    if show_close_btn {
                        let is_hovered = hovered_close_split == Some(split_id);
                        let close_fg = if is_hovered {
                            theme.tab_close_hover_fg
                        } else {
                            theme.line_number_fg
                        };
                        let close_button = Paragraph::new("×")
                            .style(Style::default().fg(close_fg).bg(theme.tab_separator_bg));
                        let close_area = Rect::new(btn_x, tab_row, 1, 1);
                        frame.render_widget(close_button, close_area);
                        close_split_areas.push((split_id, tab_row, btn_x, btn_x + 1));
                        btn_x = btn_x.saturating_sub(2); // Move left with 1 space for next button
                    }

                    // Render maximize/unmaximize button
                    if show_maximize_btn {
                        let is_hovered = hovered_maximize_split == Some(split_id);
                        let max_fg = if is_hovered {
                            theme.tab_close_hover_fg
                        } else {
                            theme.line_number_fg
                        };
                        // Use □ for maximize, ⧉ for unmaximize (restore)
                        let icon = if is_maximized { "⧉" } else { "□" };
                        let max_button = Paragraph::new(icon)
                            .style(Style::default().fg(max_fg).bg(theme.tab_separator_bg));
                        let max_area = Rect::new(btn_x, tab_row, 1, 1);
                        frame.render_widget(max_button, max_area);
                        maximize_split_areas.push((split_id, tab_row, btn_x, btn_x + 1));
                    }
                }
            }

            // Get references separately to avoid double borrow
            let state_opt = buffers.get_mut(&buffer_id);
            let event_log_opt = event_logs.get_mut(&buffer_id);

            if let Some(state) = state_opt {
                // Check if this is a composite buffer - render differently
                if state.is_composite_buffer {
                    if let Some(composite) = composite_buffers.get(&buffer_id) {
                        // Update SplitViewState viewport to match actual rendered area
                        // This ensures cursor movement uses correct viewport height after resize
                        if let Some(ref mut svs) = split_view_states {
                            if let Some(split_vs) = svs.get_mut(&split_id) {
                                if split_vs.viewport.width != layout.content_rect.width
                                    || split_vs.viewport.height != layout.content_rect.height
                                {
                                    split_vs.viewport.resize(
                                        layout.content_rect.width,
                                        layout.content_rect.height,
                                    );
                                }
                            }
                        }

                        // Get or create composite view state
                        let pane_count = composite.pane_count();
                        let view_state = composite_view_states
                            .entry((split_id, buffer_id))
                            .or_insert_with(|| {
                                crate::view::composite_view::CompositeViewState::new(
                                    buffer_id, pane_count,
                                )
                            });
                        // Render composite buffer with side-by-side panes
                        Self::render_composite_buffer(
                            frame,
                            layout.content_rect,
                            composite,
                            buffers,
                            theme,
                            is_active,
                            view_state,
                            use_terminal_bg,
                        );

                        // Render scrollbar for composite buffer
                        let total_rows = composite.row_count();
                        let content_height = layout.content_rect.height.saturating_sub(1) as usize; // -1 for header
                        let (thumb_start, thumb_end) = if show_vertical_scrollbar {
                            Self::render_composite_scrollbar(
                                frame,
                                layout.scrollbar_rect,
                                total_rows,
                                view_state.scroll_row,
                                content_height,
                                is_active,
                            )
                        } else {
                            (0, 0)
                        };

                        // Store the areas for mouse handling
                        split_areas.push((
                            split_id,
                            buffer_id,
                            layout.content_rect,
                            layout.scrollbar_rect,
                            thumb_start,
                            thumb_end,
                        ));
                        if show_horizontal_scrollbar {
                            horizontal_scrollbar_areas.push((
                                split_id,
                                buffer_id,
                                layout.horizontal_scrollbar_rect,
                                0, // composite buffers don't horizontal-scroll
                                0,
                                0,
                            ));
                        }
                    }
                    view_line_mappings.insert(split_id, Vec::new());
                    continue;
                }

                // Get viewport from SplitViewState (authoritative source)
                // We need to get it mutably for sync operations
                // Use as_deref() to get Option<&HashMap> for read-only operations
                let view_state_opt = split_view_states
                    .as_deref()
                    .and_then(|vs| vs.get(&split_id));
                let viewport_clone =
                    view_state_opt
                        .map(|vs| vs.viewport.clone())
                        .unwrap_or_else(|| {
                            crate::view::viewport::Viewport::new(
                                layout.content_rect.width,
                                layout.content_rect.height,
                            )
                        });
                let mut viewport = viewport_clone;

                // Get cursors from the split's view state
                let default_cursors = crate::model::cursor::Cursors::new();
                let split_cursors = split_view_states
                    .as_deref()
                    .and_then(|vs| vs.get(&split_id))
                    .map(|vs| &vs.cursors)
                    .unwrap_or(&default_cursors);
                Self::sync_viewport_to_content(
                    &mut viewport,
                    &mut state.buffer,
                    split_cursors,
                    layout.content_rect,
                );
                let view_prefs =
                    Self::resolve_view_preferences(state, split_view_states.as_deref(), split_id);

                let split_view_mappings = Self::render_buffer_in_split(
                    frame,
                    state,
                    split_cursors,
                    &mut viewport,
                    event_log_opt,
                    layout.content_rect,
                    is_active,
                    theme,
                    ansi_background,
                    background_fade,
                    lsp_waiting,
                    view_prefs.view_mode,
                    view_prefs.compose_width,
                    view_prefs.compose_column_guides,
                    view_prefs.view_transform,
                    estimated_line_length,
                    highlight_context_bytes,
                    buffer_id,
                    hide_cursor,
                    relative_line_numbers,
                    use_terminal_bg,
                    session_mode,
                    &view_prefs.rulers,
                    view_prefs.show_line_numbers,
                );

                // Store view line mappings for mouse click handling
                view_line_mappings.insert(split_id, split_view_mappings);

                // For small files, count actual lines for accurate scrollbar
                // For large files, we'll use a constant thumb size
                let buffer_len = state.buffer.len();
                let (total_lines, top_line) = Self::scrollbar_line_counts(
                    state,
                    &viewport,
                    large_file_threshold_bytes,
                    buffer_len,
                );

                // Render vertical scrollbar for this split and get thumb position
                let (thumb_start, thumb_end) = if show_vertical_scrollbar {
                    Self::render_scrollbar(
                        frame,
                        state,
                        &viewport,
                        layout.scrollbar_rect,
                        is_active,
                        theme,
                        large_file_threshold_bytes,
                        total_lines,
                        top_line,
                    )
                } else {
                    (0, 0)
                };

                // Compute the actual max line length for horizontal scrollbar
                let max_content_width = if show_horizontal_scrollbar && !viewport.line_wrap_enabled
                {
                    let mcw = Self::compute_max_line_length(state, &mut viewport);
                    // Clamp left_column so content can't scroll past the end of the longest line
                    let visible_width = viewport.width as usize;
                    let max_scroll = mcw.saturating_sub(visible_width);
                    if viewport.left_column > max_scroll {
                        viewport.left_column = max_scroll;
                    }
                    mcw
                } else {
                    0
                };

                // Render horizontal scrollbar for this split
                let (hthumb_start, hthumb_end) = if show_horizontal_scrollbar {
                    Self::render_horizontal_scrollbar(
                        frame,
                        &viewport,
                        layout.horizontal_scrollbar_rect,
                        is_active,
                        max_content_width,
                    )
                } else {
                    (0, 0)
                };

                // Write back updated viewport to SplitViewState
                // This is crucial for cursor visibility tracking (ensure_visible_in_layout updates)
                // NOTE: We do NOT clear skip_ensure_visible here - it should persist across
                // renders until something actually needs cursor visibility check
                if let Some(view_states) = split_view_states.as_deref_mut() {
                    if let Some(view_state) = view_states.get_mut(&split_id) {
                        tracing::trace!(
                            "Writing back viewport: top_byte={}, skip_ensure_visible={}",
                            viewport.top_byte,
                            viewport.should_skip_ensure_visible()
                        );
                        view_state.viewport = viewport.clone();
                    }
                }

                // Store the areas for mouse handling
                split_areas.push((
                    split_id,
                    buffer_id,
                    layout.content_rect,
                    layout.scrollbar_rect,
                    thumb_start,
                    thumb_end,
                ));
                if show_horizontal_scrollbar {
                    horizontal_scrollbar_areas.push((
                        split_id,
                        buffer_id,
                        layout.horizontal_scrollbar_rect,
                        max_content_width,
                        hthumb_start,
                        hthumb_end,
                    ));
                }
            }
        }

        // Render split separators
        let separators = split_manager.get_separators(area);
        for (direction, x, y, length) in separators {
            Self::render_separator(frame, direction, x, y, length, theme);
        }

        (
            split_areas,
            tab_layouts,
            close_split_areas,
            maximize_split_areas,
            view_line_mappings,
            horizontal_scrollbar_areas,
        )
    }

    /// Render a split separator line
    fn render_separator(
        frame: &mut Frame,
        direction: SplitDirection,
        x: u16,
        y: u16,
        length: u16,
        theme: &crate::view::theme::Theme,
    ) {
        match direction {
            SplitDirection::Horizontal => {
                // Draw horizontal line
                let line_area = Rect::new(x, y, length, 1);
                let line_text = "─".repeat(length as usize);
                let paragraph =
                    Paragraph::new(line_text).style(Style::default().fg(theme.split_separator_fg));
                frame.render_widget(paragraph, line_area);
            }
            SplitDirection::Vertical => {
                // Draw vertical line
                for offset in 0..length {
                    let cell_area = Rect::new(x, y + offset, 1, 1);
                    let paragraph =
                        Paragraph::new("│").style(Style::default().fg(theme.split_separator_fg));
                    frame.render_widget(paragraph, cell_area);
                }
            }
        }
    }

    /// Render a composite buffer (side-by-side view of multiple source buffers)
    /// Uses ViewLines for proper syntax highlighting, ANSI handling, etc.
    fn render_composite_buffer(
        frame: &mut Frame,
        area: Rect,
        composite: &crate::model::composite_buffer::CompositeBuffer,
        buffers: &mut HashMap<BufferId, EditorState>,
        theme: &crate::view::theme::Theme,
        _is_active: bool,
        view_state: &mut crate::view::composite_view::CompositeViewState,
        use_terminal_bg: bool,
    ) {
        use crate::model::composite_buffer::{CompositeLayout, RowType};

        // Compute effective editor background: terminal default or theme-defined
        let effective_editor_bg = if use_terminal_bg {
            ratatui::style::Color::Reset
        } else {
            theme.editor_bg
        };

        let scroll_row = view_state.scroll_row;
        let cursor_row = view_state.cursor_row;

        // Clear the area first
        frame.render_widget(Clear, area);

        // Calculate pane widths based on layout
        let pane_count = composite.sources.len();
        if pane_count == 0 {
            return;
        }

        // Extract show_separator from layout
        let show_separator = match &composite.layout {
            CompositeLayout::SideBySide { show_separator, .. } => *show_separator,
            _ => false,
        };

        // Calculate pane areas
        let separator_width = if show_separator { 1 } else { 0 };
        let total_separators = (pane_count.saturating_sub(1)) as u16 * separator_width;
        let available_width = area.width.saturating_sub(total_separators);

        let pane_widths: Vec<u16> = match &composite.layout {
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
                // Equal widths for stacked/unified layouts
                let pane_width = available_width / pane_count as u16;
                vec![pane_width; pane_count]
            }
        };

        // Store computed pane widths in view state for cursor movement calculations
        view_state.pane_widths = pane_widths.clone();

        // Render headers first
        let header_height = 1u16;
        let mut x_offset = area.x;
        for (idx, (source, &width)) in composite.sources.iter().zip(&pane_widths).enumerate() {
            let header_area = Rect::new(x_offset, area.y, width, header_height);
            let is_focused = idx == view_state.focused_pane;

            let header_style = if is_focused {
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
            frame.render_widget(header, header_area);

            x_offset += width + separator_width;
        }

        // Content area (below headers)
        let content_y = area.y + header_height;
        let content_height = area.height.saturating_sub(header_height);
        let visible_rows = content_height as usize;

        // Render aligned rows
        let alignment = &composite.alignment;
        let total_rows = alignment.rows.len();

        // Build ViewData and get syntax highlighting for each pane
        // Store: (ViewLines, line->ViewLine mapping, highlight spans)
        struct PaneRenderData {
            lines: Vec<ViewLine>,
            line_to_view_line: HashMap<usize, usize>,
            highlight_spans: Vec<crate::primitives::highlighter::HighlightSpan>,
        }

        let mut pane_render_data: Vec<Option<PaneRenderData>> = Vec::new();

        for (pane_idx, source) in composite.sources.iter().enumerate() {
            if let Some(source_state) = buffers.get_mut(&source.buffer_id) {
                // Find the first and last source lines we need for this pane
                let visible_lines: Vec<usize> = alignment
                    .rows
                    .iter()
                    .skip(scroll_row)
                    .take(visible_rows)
                    .filter_map(|row| row.get_pane_line(pane_idx))
                    .map(|r| r.line)
                    .collect();

                let first_line = visible_lines.iter().copied().min();
                let last_line = visible_lines.iter().copied().max();

                if let (Some(first_line), Some(last_line)) = (first_line, last_line) {
                    // Get byte range for highlighting
                    let top_byte = source_state
                        .buffer
                        .line_start_offset(first_line)
                        .unwrap_or(0);
                    let end_byte = source_state
                        .buffer
                        .line_start_offset(last_line + 1)
                        .unwrap_or(source_state.buffer.len());

                    // Get syntax highlighting spans from the highlighter
                    let highlight_spans = source_state.highlighter.highlight_viewport(
                        &source_state.buffer,
                        top_byte,
                        end_byte,
                        theme,
                        1024, // highlight_context_bytes
                    );

                    // Create a temporary viewport for building view data
                    let pane_width = pane_widths.get(pane_idx).copied().unwrap_or(80);
                    let mut viewport =
                        crate::view::viewport::Viewport::new(pane_width, content_height);
                    viewport.top_byte = top_byte;
                    viewport.line_wrap_enabled = false;

                    let pane_width = pane_widths.get(pane_idx).copied().unwrap_or(80) as usize;
                    let gutter_width = 4; // Line number width
                    let content_width = pane_width.saturating_sub(gutter_width);

                    // Build ViewData for this pane
                    // Need enough lines to cover from first_line to last_line
                    let lines_needed = last_line - first_line + 10;
                    let view_data = Self::build_view_data(
                        source_state,
                        &viewport,
                        None,         // No view transform
                        80,           // estimated_line_length
                        lines_needed, // visible_count - enough to cover the range
                        false,        // line_wrap_enabled
                        content_width,
                        gutter_width,
                        &ViewMode::Source, // Composite view uses source mode
                    );

                    // Build source_line -> ViewLine index mapping
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
                } else {
                    pane_render_data.push(None);
                }
            } else {
                pane_render_data.push(None);
            }
        }

        // Now render aligned rows using ViewLines
        for view_row in 0..visible_rows {
            let display_row = scroll_row + view_row;
            if display_row >= total_rows {
                // Fill with tildes for empty rows
                let mut x = area.x;
                for &width in &pane_widths {
                    let tilde_area = Rect::new(x, content_y + view_row as u16, width, 1);
                    let tilde =
                        Paragraph::new("~").style(Style::default().fg(theme.line_number_fg));
                    frame.render_widget(tilde, tilde_area);
                    x += width + separator_width;
                }
                continue;
            }

            let aligned_row = &alignment.rows[display_row];
            let is_cursor_row = display_row == cursor_row;
            // Get selection column range for this row (if any)
            let selection_cols = view_state.selection_column_range(display_row);

            // Determine row background based on type (selection is now character-level)
            let row_bg = match aligned_row.row_type {
                RowType::Addition => Some(theme.diff_add_bg),
                RowType::Deletion => Some(theme.diff_remove_bg),
                RowType::Modification => Some(theme.diff_modify_bg),
                RowType::HunkHeader => Some(theme.current_line_bg),
                RowType::Context => None,
            };

            // Compute inline diff for modified rows (to highlight changed words/characters)
            let inline_diffs: Vec<Vec<Range<usize>>> = if aligned_row.row_type
                == RowType::Modification
            {
                // Get line content from both panes
                let mut line_contents: Vec<Option<String>> = Vec::new();
                for (pane_idx, source) in composite.sources.iter().enumerate() {
                    if let Some(line_ref) = aligned_row.get_pane_line(pane_idx) {
                        if let Some(source_state) = buffers.get(&source.buffer_id) {
                            line_contents.push(
                                source_state
                                    .buffer
                                    .get_line(line_ref.line)
                                    .map(|line| String::from_utf8_lossy(&line).to_string()),
                            );
                        } else {
                            line_contents.push(None);
                        }
                    } else {
                        line_contents.push(None);
                    }
                }

                // Compute inline diff between panes (typically old vs new)
                if line_contents.len() >= 2 {
                    if let (Some(old_text), Some(new_text)) = (&line_contents[0], &line_contents[1])
                    {
                        let (old_ranges, new_ranges) = compute_inline_diff(old_text, new_text);
                        vec![old_ranges, new_ranges]
                    } else {
                        vec![Vec::new(); composite.sources.len()]
                    }
                } else {
                    vec![Vec::new(); composite.sources.len()]
                }
            } else {
                // For non-modification rows, no inline highlighting
                vec![Vec::new(); composite.sources.len()]
            };

            // Render each pane for this row
            let mut x_offset = area.x;
            for (pane_idx, (_source, &width)) in
                composite.sources.iter().zip(&pane_widths).enumerate()
            {
                let pane_area = Rect::new(x_offset, content_y + view_row as u16, width, 1);

                // Get horizontal scroll offset for this pane
                let left_column = view_state
                    .get_pane_viewport(pane_idx)
                    .map(|v| v.left_column)
                    .unwrap_or(0);

                // Get source line for this pane
                let source_line_opt = aligned_row.get_pane_line(pane_idx);

                if let Some(source_line_ref) = source_line_opt {
                    // Try to get ViewLine and highlight spans from pre-built data
                    let pane_data = pane_render_data.get(pane_idx).and_then(|opt| opt.as_ref());
                    let view_line_opt = pane_data.and_then(|data| {
                        data.line_to_view_line
                            .get(&source_line_ref.line)
                            .and_then(|&idx| data.lines.get(idx))
                    });
                    let highlight_spans = pane_data
                        .map(|data| data.highlight_spans.as_slice())
                        .unwrap_or(&[]);

                    let gutter_width = 4usize;
                    let max_content_width = width.saturating_sub(gutter_width as u16) as usize;

                    let is_focused_pane = pane_idx == view_state.focused_pane;

                    // Determine background - cursor row highlight only on focused pane
                    // Selection is now character-level, handled in render_view_line_content
                    let bg = if is_cursor_row && is_focused_pane {
                        theme.current_line_bg
                    } else {
                        row_bg.unwrap_or(effective_editor_bg)
                    };

                    // Selection range for this row (only for focused pane)
                    let pane_selection_cols = if is_focused_pane {
                        selection_cols
                    } else {
                        None
                    };

                    // Line number
                    let line_num = format!("{:>3} ", source_line_ref.line + 1);
                    let line_num_style = Style::default().fg(theme.line_number_fg).bg(bg);

                    let is_cursor_pane = is_focused_pane;
                    let cursor_column = view_state.cursor_column;

                    // Get inline diff ranges for this pane
                    let inline_ranges = inline_diffs.get(pane_idx).cloned().unwrap_or_default();

                    // Determine highlight color for changed portions (brighter than line bg)
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

                    // Build spans using ViewLine if available (for syntax highlighting)
                    let mut spans = vec![Span::styled(line_num, line_num_style)];

                    if let Some(view_line) = view_line_opt {
                        // Use ViewLine for syntax-highlighted content
                        Self::render_view_line_content(
                            &mut spans,
                            view_line,
                            highlight_spans,
                            left_column,
                            max_content_width,
                            bg,
                            theme,
                            is_cursor_row && is_cursor_pane,
                            cursor_column,
                            &inline_ranges,
                            highlight_bg,
                            pane_selection_cols,
                        );
                    } else {
                        // This branch should be unreachable:
                        // - visible_lines is collected from the same range we iterate over
                        // - If source_line_ref exists, that line was in visible_lines
                        // - So pane_render_data exists and the line should be in the mapping
                        // - With line_wrap disabled, each source line = one ViewLine
                        tracing::warn!(
                            "ViewLine missing for composite buffer: pane={}, line={}, pane_data={}",
                            pane_idx,
                            source_line_ref.line,
                            pane_data.is_some()
                        );
                        // Graceful degradation: render empty content with background
                        let base_style = Style::default().fg(theme.editor_fg).bg(bg);
                        let padding = " ".repeat(max_content_width);
                        spans.push(Span::styled(padding, base_style));
                    }

                    let line = Line::from(spans);
                    let para = Paragraph::new(line);
                    frame.render_widget(para, pane_area);
                } else {
                    // No content for this pane (padding/gap line)
                    let is_focused_pane = pane_idx == view_state.focused_pane;
                    // For empty lines in focused pane, show selection if entire line is selected
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
                    let style = Style::default().fg(theme.line_number_fg).bg(bg);

                    // Check if cursor should be shown on this empty line
                    let is_cursor_pane = pane_idx == view_state.focused_pane;
                    if is_cursor_row && is_cursor_pane && view_state.cursor_column == 0 {
                        // Show cursor on empty line
                        let cursor_style = Style::default().fg(theme.editor_bg).bg(theme.editor_fg);
                        let gutter_width = 4usize;
                        let max_content_width = width.saturating_sub(gutter_width as u16) as usize;
                        let padding = " ".repeat(max_content_width.saturating_sub(1));
                        let line = Line::from(vec![
                            Span::styled("    ", style),
                            Span::styled(" ", cursor_style),
                            Span::styled(padding, Style::default().bg(bg)),
                        ]);
                        let para = Paragraph::new(line);
                        frame.render_widget(para, pane_area);
                    } else {
                        // Empty gap line with diff background
                        let gap_style = Style::default().bg(bg);
                        let empty_content = " ".repeat(width as usize);
                        let para = Paragraph::new(empty_content).style(gap_style);
                        frame.render_widget(para, pane_area);
                    }
                }

                x_offset += width;

                // Render separator
                if show_separator && pane_idx < pane_count - 1 {
                    let sep_area =
                        Rect::new(x_offset, content_y + view_row as u16, separator_width, 1);
                    let sep =
                        Paragraph::new("│").style(Style::default().fg(theme.split_separator_fg));
                    frame.render_widget(sep, sep_area);
                    x_offset += separator_width;
                }
            }
        }
    }

    /// Render ViewLine content with syntax highlighting to spans
    #[allow(clippy::too_many_arguments)]
    fn render_view_line_content(
        spans: &mut Vec<Span<'static>>,
        view_line: &ViewLine,
        highlight_spans: &[crate::primitives::highlighter::HighlightSpan],
        left_column: usize,
        max_width: usize,
        bg: Color,
        theme: &crate::view::theme::Theme,
        show_cursor: bool,
        cursor_column: usize,
        inline_ranges: &[Range<usize>],
        highlight_bg: Option<Color>,
        selection_cols: Option<(usize, usize)>, // (start_col, end_col) for selection
    ) {
        let text = &view_line.text;
        let char_source_bytes = &view_line.char_source_bytes;

        // Apply horizontal scroll and collect visible characters with styles
        let chars: Vec<char> = text.chars().collect();
        let mut col = 0usize;
        let mut rendered = 0usize;
        let mut current_span_text = String::new();
        let mut current_style: Option<Style> = None;

        for (char_idx, ch) in chars.iter().enumerate() {
            let char_width = char_width(*ch);

            // Skip characters before left_column
            if col < left_column {
                col += char_width;
                continue;
            }

            // Stop if we've rendered enough
            if rendered >= max_width {
                break;
            }

            // Get source byte position for this character
            let byte_pos = char_source_bytes.get(char_idx).and_then(|b| *b);

            // Get syntax highlight color from highlight_spans
            let highlight_color = byte_pos.and_then(|bp| {
                highlight_spans
                    .iter()
                    .find(|span| span.range.contains(&bp))
                    .map(|span| span.color)
            });

            // Check if this character is in an inline diff range
            let in_inline_range = inline_ranges.iter().any(|r| r.contains(&char_idx));

            // Check if this character is in selection range
            let in_selection = selection_cols
                .map(|(start, end)| col >= start && col < end)
                .unwrap_or(false);

            // Determine background: selection > inline diff > normal
            let char_bg = if in_selection {
                theme.selection_bg
            } else if in_inline_range {
                highlight_bg.unwrap_or(bg)
            } else {
                bg
            };

            // Build character style
            let char_style = if let Some(color) = highlight_color {
                Style::default().fg(color).bg(char_bg)
            } else {
                Style::default().fg(theme.editor_fg).bg(char_bg)
            };

            // Handle cursor - cursor_column is absolute position, compare directly with col
            let final_style = if show_cursor && col == cursor_column {
                // Invert colors for cursor
                Style::default().fg(theme.editor_bg).bg(theme.editor_fg)
            } else {
                char_style
            };

            // Accumulate or flush spans based on style changes
            if let Some(style) = current_style {
                if style != final_style && !current_span_text.is_empty() {
                    spans.push(Span::styled(std::mem::take(&mut current_span_text), style));
                }
            }

            current_style = Some(final_style);
            current_span_text.push(*ch);
            col += char_width;
            rendered += char_width;
        }

        // Flush remaining span
        if !current_span_text.is_empty() {
            if let Some(style) = current_style {
                spans.push(Span::styled(current_span_text, style));
            }
        }

        // Pad to fill width
        if rendered < max_width {
            let padding_len = max_width - rendered;
            // cursor_column is absolute, convert to visual position for padding check
            let cursor_visual = cursor_column.saturating_sub(left_column);

            // Check if cursor is in the padding area (past end of line content)
            if show_cursor && cursor_visual >= rendered && cursor_visual < max_width {
                // Cursor is in padding area - render cursor as single char
                let cursor_offset = cursor_visual - rendered;
                let cursor_style = Style::default().fg(theme.editor_bg).bg(theme.editor_fg);
                let normal_style = Style::default().bg(bg);

                // Pre-cursor padding (if cursor is not at start of padding)
                if cursor_offset > 0 {
                    spans.push(Span::styled(" ".repeat(cursor_offset), normal_style));
                }
                // Single-char cursor
                spans.push(Span::styled(" ", cursor_style));
                // Post-cursor padding
                let remaining = padding_len.saturating_sub(cursor_offset + 1);
                if remaining > 0 {
                    spans.push(Span::styled(" ".repeat(remaining), normal_style));
                }
            } else {
                // No cursor in padding - just fill with background
                spans.push(Span::styled(
                    " ".repeat(padding_len),
                    Style::default().bg(bg),
                ));
            }
        }
    }

    /// Render a scrollbar for composite buffer views
    fn render_composite_scrollbar(
        frame: &mut Frame,
        scrollbar_rect: Rect,
        total_rows: usize,
        scroll_row: usize,
        viewport_height: usize,
        is_active: bool,
    ) -> (usize, usize) {
        let height = scrollbar_rect.height as usize;
        if height == 0 || total_rows == 0 {
            return (0, 0);
        }

        // Calculate thumb size based on viewport ratio to total document
        let thumb_size_raw = if total_rows > 0 {
            ((viewport_height as f64 / total_rows as f64) * height as f64).ceil() as usize
        } else {
            1
        };

        // Maximum scroll position
        let max_scroll = total_rows.saturating_sub(viewport_height);

        // When content fits in viewport, fill entire scrollbar
        let thumb_size = if max_scroll == 0 {
            height
        } else {
            // Cap thumb size: minimum 1, maximum 80% of scrollbar height
            let max_thumb_size = (height as f64 * 0.8).floor() as usize;
            thumb_size_raw.max(1).min(max_thumb_size).min(height)
        };

        // Calculate thumb position
        let thumb_start = if max_scroll > 0 {
            let scroll_ratio = scroll_row.min(max_scroll) as f64 / max_scroll as f64;
            let max_thumb_start = height.saturating_sub(thumb_size);
            (scroll_ratio * max_thumb_start as f64) as usize
        } else {
            0
        };

        let thumb_end = thumb_start + thumb_size;

        // Choose colors based on whether split is active
        let track_color = if is_active {
            Color::DarkGray
        } else {
            Color::Black
        };
        let thumb_color = if is_active {
            Color::Gray
        } else {
            Color::DarkGray
        };

        // Render as background fills
        for row in 0..height {
            let cell_area = Rect::new(scrollbar_rect.x, scrollbar_rect.y + row as u16, 1, 1);

            let style = if row >= thumb_start && row < thumb_end {
                Style::default().bg(thumb_color)
            } else {
                Style::default().bg(track_color)
            };

            let paragraph = Paragraph::new(" ").style(style);
            frame.render_widget(paragraph, cell_area);
        }

        (thumb_start, thumb_end)
    }

    fn split_layout(
        split_area: Rect,
        tab_bar_visible: bool,
        show_vertical_scrollbar: bool,
        show_horizontal_scrollbar: bool,
    ) -> SplitLayout {
        let tabs_height = if tab_bar_visible { 1u16 } else { 0u16 };
        let scrollbar_width = if show_vertical_scrollbar { 1u16 } else { 0u16 };
        let hscrollbar_height = if show_horizontal_scrollbar {
            1u16
        } else {
            0u16
        };

        let tabs_rect = Rect::new(split_area.x, split_area.y, split_area.width, tabs_height);
        let content_rect = Rect::new(
            split_area.x,
            split_area.y + tabs_height,
            split_area.width.saturating_sub(scrollbar_width),
            split_area
                .height
                .saturating_sub(tabs_height)
                .saturating_sub(hscrollbar_height),
        );
        let scrollbar_rect = Rect::new(
            split_area.x + split_area.width.saturating_sub(scrollbar_width),
            split_area.y + tabs_height,
            scrollbar_width,
            split_area
                .height
                .saturating_sub(tabs_height)
                .saturating_sub(hscrollbar_height),
        );
        let horizontal_scrollbar_rect = Rect::new(
            split_area.x,
            split_area.y + split_area.height.saturating_sub(hscrollbar_height),
            split_area.width.saturating_sub(scrollbar_width),
            hscrollbar_height,
        );

        SplitLayout {
            tabs_rect,
            content_rect,
            scrollbar_rect,
            horizontal_scrollbar_rect,
        }
    }

    fn split_buffers_for_tabs(
        split_view_states: Option<
            &HashMap<crate::model::event::SplitId, crate::view::split::SplitViewState>,
        >,
        split_id: crate::model::event::SplitId,
        buffer_id: BufferId,
    ) -> (Vec<BufferId>, usize) {
        if let Some(view_states) = split_view_states {
            if let Some(view_state) = view_states.get(&split_id) {
                return (
                    view_state.open_buffers.clone(),
                    view_state.tab_scroll_offset,
                );
            }
        }
        (vec![buffer_id], 0)
    }

    fn sync_viewport_to_content(
        viewport: &mut crate::view::viewport::Viewport,
        buffer: &mut crate::model::buffer::Buffer,
        cursors: &crate::model::cursor::Cursors,
        content_rect: Rect,
    ) {
        let size_changed =
            viewport.width != content_rect.width || viewport.height != content_rect.height;

        if size_changed {
            viewport.resize(content_rect.width, content_rect.height);
        }

        // Always sync viewport with cursor to ensure visibility after cursor movements
        // The sync_with_cursor method internally checks needs_sync and skip_resize_sync
        // so this is safe to call unconditionally. Previously needs_sync was set by
        // EditorState.apply() but now viewport is owned by SplitViewState.
        let primary = *cursors.primary();
        viewport.ensure_visible(buffer, &primary);
    }

    fn resolve_view_preferences(
        _state: &EditorState,
        split_view_states: Option<
            &HashMap<crate::model::event::SplitId, crate::view::split::SplitViewState>,
        >,
        split_id: crate::model::event::SplitId,
    ) -> ViewPreferences {
        if let Some(view_states) = split_view_states {
            if let Some(view_state) = view_states.get(&split_id) {
                return ViewPreferences {
                    view_mode: view_state.view_mode.clone(),
                    compose_width: view_state.compose_width,
                    compose_column_guides: view_state.compose_column_guides.clone(),
                    view_transform: view_state.view_transform.clone(),
                    rulers: view_state.rulers.clone(),
                    show_line_numbers: view_state.show_line_numbers,
                };
            }
        }

        // Fallback when no SplitViewState is available (shouldn't happen in practice)
        ViewPreferences {
            view_mode: ViewMode::Source,
            compose_width: None,
            compose_column_guides: None,
            view_transform: None,
            rulers: Vec::new(),
            show_line_numbers: None,
        }
    }

    fn scrollbar_line_counts(
        state: &EditorState,
        viewport: &crate::view::viewport::Viewport,
        large_file_threshold_bytes: u64,
        buffer_len: usize,
    ) -> (usize, usize) {
        if buffer_len > large_file_threshold_bytes as usize {
            return (0, 0);
        }

        // When line wrapping is enabled, count visual rows instead of logical lines
        if viewport.line_wrap_enabled {
            return Self::scrollbar_visual_row_counts(state, viewport, buffer_len);
        }

        let total_lines = if buffer_len > 0 {
            state.buffer.get_line_number(buffer_len.saturating_sub(1)) + 1
        } else {
            1
        };

        let top_line = if viewport.top_byte < buffer_len {
            state.buffer.get_line_number(viewport.top_byte)
        } else {
            0
        };

        (total_lines, top_line)
    }

    /// Calculate scrollbar position based on visual rows (for line-wrapped content)
    /// Returns (total_visual_rows, top_visual_row)
    fn scrollbar_visual_row_counts(
        state: &EditorState,
        viewport: &crate::view::viewport::Viewport,
        buffer_len: usize,
    ) -> (usize, usize) {
        use crate::primitives::line_wrapping::{wrap_line, WrapConfig};

        if buffer_len == 0 {
            return (1, 0);
        }

        let gutter_width = viewport.gutter_width(&state.buffer);
        let wrap_config = WrapConfig::new(viewport.width as usize, gutter_width, true);

        // Count total visual rows and find top visual row
        // Use get_line which doesn't require mutable buffer access
        let mut total_visual_rows = 0;
        let mut top_visual_row = 0;
        let mut found_top = false;

        // Get total line count (if available) or estimate
        let line_count = state.buffer.line_count().unwrap_or_else(|| {
            // Estimate based on buffer size
            (buffer_len / 80).max(1)
        });

        for line_idx in 0..line_count {
            // Get the line start offset to check if we've reached top_byte
            let line_start = state
                .buffer
                .line_start_offset(line_idx)
                .unwrap_or(buffer_len);

            // Check if this is the top line
            if !found_top && line_start >= viewport.top_byte {
                top_visual_row = total_visual_rows + viewport.top_view_line_offset;
                found_top = true;
            }

            // Get line content
            let line_content = if let Some(bytes) = state.buffer.get_line(line_idx) {
                String::from_utf8_lossy(&bytes)
                    .trim_end_matches('\n')
                    .trim_end_matches('\r')
                    .to_string()
            } else {
                break;
            };

            let segments = wrap_line(&line_content, &wrap_config);
            let visual_rows_in_line = segments.len().max(1);
            total_visual_rows += visual_rows_in_line;
        }

        // Handle case where top_byte is at/past end of buffer
        if !found_top {
            top_visual_row = total_visual_rows.saturating_sub(1);
        }

        // Ensure at least 1 total row
        total_visual_rows = total_visual_rows.max(1);

        (total_visual_rows, top_visual_row)
    }

    /// Render a scrollbar for a split
    /// Returns (thumb_start, thumb_end) positions for mouse hit testing
    #[allow(clippy::too_many_arguments)]
    fn render_scrollbar(
        frame: &mut Frame,
        state: &EditorState,
        viewport: &crate::view::viewport::Viewport,
        scrollbar_rect: Rect,
        is_active: bool,
        _theme: &crate::view::theme::Theme,
        large_file_threshold_bytes: u64,
        total_lines: usize,
        top_line: usize,
    ) -> (usize, usize) {
        let height = scrollbar_rect.height as usize;
        if height == 0 {
            return (0, 0);
        }

        let buffer_len = state.buffer.len();
        let viewport_top = viewport.top_byte;
        // Use the scrollbar height as the viewport height for line count calculations.
        // This represents the actual number of visible content rows, which matches
        // what the user sees. When line wrapping is enabled, total_lines represents
        // visual rows, so we need to compare against actual visible rows (height),
        // not the full terminal height (viewport.height includes menu, status bar, etc.)
        let viewport_height_lines = height;

        // Calculate scrollbar thumb position and size
        let (thumb_start, thumb_size) = if buffer_len > large_file_threshold_bytes as usize {
            // Large file: use constant 1-character thumb for performance
            let thumb_start = if buffer_len > 0 {
                ((viewport_top as f64 / buffer_len as f64) * height as f64) as usize
            } else {
                0
            };
            (thumb_start, 1)
        } else {
            // Small file: use actual line count for accurate scrollbar
            // total_lines and top_line are passed in (already calculated with mutable access)

            // Calculate thumb size based on viewport ratio to total document
            let thumb_size_raw = if total_lines > 0 {
                ((viewport_height_lines as f64 / total_lines as f64) * height as f64).ceil()
                    as usize
            } else {
                1
            };

            // Calculate the maximum scroll position first to determine if buffer fits in viewport
            // The maximum scroll position is when the last line of the file is at
            // the bottom of the viewport, i.e., max_scroll_line = total_lines - viewport_height
            let max_scroll_line = total_lines.saturating_sub(viewport_height_lines);

            // When buffer fits entirely in viewport (no scrolling possible),
            // fill the entire scrollbar to make it obvious to the user
            let thumb_size = if max_scroll_line == 0 {
                height
            } else {
                // Cap thumb size: minimum 1, maximum 80% of scrollbar height
                let max_thumb_size = (height as f64 * 0.8).floor() as usize;
                thumb_size_raw.max(1).min(max_thumb_size).min(height)
            };

            // Calculate thumb position using proper linear mapping:
            // - At line 0: thumb_start = 0
            // - At max scroll position: thumb_start = height - thumb_size
            let thumb_start = if max_scroll_line > 0 {
                // Linear interpolation from 0 to (height - thumb_size)
                let scroll_ratio = top_line.min(max_scroll_line) as f64 / max_scroll_line as f64;
                let max_thumb_start = height.saturating_sub(thumb_size);
                (scroll_ratio * max_thumb_start as f64) as usize
            } else {
                // File fits in viewport, thumb fills entire height starting at top
                0
            };

            (thumb_start, thumb_size)
        };

        let thumb_end = thumb_start + thumb_size;

        // Choose colors based on whether split is active
        let track_color = if is_active {
            Color::DarkGray
        } else {
            Color::Black
        };
        let thumb_color = if is_active {
            Color::Gray
        } else {
            Color::DarkGray
        };

        // Render as background fills to avoid glyph gaps in terminals like Apple Terminal.
        for row in 0..height {
            let cell_area = Rect::new(scrollbar_rect.x, scrollbar_rect.y + row as u16, 1, 1);

            let style = if row >= thumb_start && row < thumb_end {
                // Thumb
                Style::default().bg(thumb_color)
            } else {
                // Track
                Style::default().bg(track_color)
            };

            let paragraph = Paragraph::new(" ").style(style);
            frame.render_widget(paragraph, cell_area);
        }

        // Return thumb position for mouse hit testing
        (thumb_start, thumb_end)
    }

    /// Compute the maximum line length encountered so far (in display columns).
    /// Only scans the currently visible lines (plus a small margin) and updates
    /// the running maximum stored in the viewport. This avoids scanning the
    /// entire file, which would break large file support.
    fn compute_max_line_length(
        state: &mut EditorState,
        viewport: &mut crate::view::viewport::Viewport,
    ) -> usize {
        let buffer_len = state.buffer.len();
        let visible_width = viewport.width as usize;

        if buffer_len == 0 {
            return viewport.max_line_length_seen.max(visible_width);
        }

        // Scan only the visible lines (with a small margin) to update the running maximum
        let visible_lines = viewport.height as usize + 5; // small margin beyond visible area
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

        // Return at least visible_width (not left_column + visible_width) to avoid
        // a feedback loop where scrolling right increases the limit further
        viewport.max_line_length_seen.max(visible_width)
    }

    /// Render a horizontal scrollbar for a split.
    /// `max_content_width` should be the actual max line length (from compute_max_line_length).
    /// Returns (thumb_start_col, thumb_end_col) for mouse hit testing.
    fn render_horizontal_scrollbar(
        frame: &mut Frame,
        viewport: &crate::view::viewport::Viewport,
        hscrollbar_rect: Rect,
        is_active: bool,
        max_content_width: usize,
    ) -> (usize, usize) {
        let width = hscrollbar_rect.width as usize;
        if width == 0 || hscrollbar_rect.height == 0 {
            return (0, 0);
        }

        let track_color = if is_active {
            Color::DarkGray
        } else {
            Color::Black
        };

        // When line wrapping is enabled, render empty track
        if viewport.line_wrap_enabled {
            for col in 0..width {
                let cell_area = Rect::new(hscrollbar_rect.x + col as u16, hscrollbar_rect.y, 1, 1);
                let paragraph = Paragraph::new(" ").style(Style::default().bg(track_color));
                frame.render_widget(paragraph, cell_area);
            }
            return (0, width);
        }

        let visible_width = viewport.width as usize;
        let left_column = viewport.left_column;

        // If content fits entirely in viewport, fill the entire scrollbar with thumb
        let max_scroll = max_content_width.saturating_sub(visible_width);

        let (thumb_start, thumb_size) = if max_scroll == 0 {
            (0, width)
        } else {
            // Calculate thumb size proportional to visible/total ratio
            let thumb_size_raw =
                ((visible_width as f64 / max_content_width as f64) * width as f64).ceil() as usize;
            let thumb_size = thumb_size_raw.max(2).min(width); // min 2 cols for visibility

            // Calculate thumb position
            let scroll_ratio = left_column.min(max_scroll) as f64 / max_scroll as f64;
            let max_thumb_start = width.saturating_sub(thumb_size);
            let thumb_start = (scroll_ratio * max_thumb_start as f64).round() as usize;

            (thumb_start, thumb_size)
        };

        let thumb_end = thumb_start + thumb_size;

        let thumb_color = if is_active {
            Color::Gray
        } else {
            Color::DarkGray
        };

        // Render as background fills (horizontal)
        for col in 0..width {
            let cell_area = Rect::new(hscrollbar_rect.x + col as u16, hscrollbar_rect.y, 1, 1);

            let style = if col >= thumb_start && col < thumb_end {
                Style::default().bg(thumb_color)
            } else {
                Style::default().bg(track_color)
            };

            let paragraph = Paragraph::new(" ").style(style);
            frame.render_widget(paragraph, cell_area);
        }

        (thumb_start, thumb_end)
    }

    #[allow(clippy::too_many_arguments)]
    fn build_view_data(
        state: &mut EditorState,
        viewport: &crate::view::viewport::Viewport,
        view_transform: Option<ViewTransformPayload>,
        estimated_line_length: usize,
        visible_count: usize,
        line_wrap_enabled: bool,
        content_width: usize,
        gutter_width: usize,
        view_mode: &ViewMode,
    ) -> ViewData {
        // Check if buffer is binary before building tokens
        let is_binary = state.buffer.is_binary();
        let line_ending = state.buffer.line_ending();

        // Build base token stream from source
        let base_tokens = Self::build_base_tokens(
            &mut state.buffer,
            viewport.top_byte,
            estimated_line_length,
            visible_count,
            is_binary,
            line_ending,
        );

        // Use plugin transform if available, otherwise use base tokens
        let mut tokens = view_transform.map(|vt| vt.tokens).unwrap_or(base_tokens);

        // Apply soft breaks — marker-based line wrapping that survives edits without flicker.
        // Only apply in Compose mode; Source mode shows the raw unwrapped text.
        let is_compose = matches!(view_mode, ViewMode::Compose);
        if is_compose && !state.soft_breaks.is_empty() {
            let viewport_end = tokens
                .iter()
                .filter_map(|t| t.source_offset)
                .last()
                .unwrap_or(viewport.top_byte)
                + 1;
            let soft_breaks = state.soft_breaks.query_viewport(
                viewport.top_byte,
                viewport_end,
                &state.marker_list,
            );
            if !soft_breaks.is_empty() {
                tokens = Self::apply_soft_breaks(tokens, &soft_breaks);
            }
        }

        // Apply conceal ranges - filter/replace tokens that fall within concealed byte ranges.
        // Only apply in Compose mode; Source mode shows the raw markdown syntax.
        if is_compose && !state.conceals.is_empty() {
            let viewport_end = tokens
                .iter()
                .filter_map(|t| t.source_offset)
                .last()
                .unwrap_or(viewport.top_byte)
                + 1;
            let conceal_ranges =
                state
                    .conceals
                    .query_viewport(viewport.top_byte, viewport_end, &state.marker_list);
            if !conceal_ranges.is_empty() {
                tokens = Self::apply_conceal_ranges(tokens, &conceal_ranges);
            }
        }

        // Apply wrapping transform - always enabled for safety, but with different thresholds.
        // When line_wrap is on: wrap at viewport width for normal text flow.
        // When line_wrap is off: wrap at MAX_SAFE_LINE_WIDTH to prevent memory exhaustion
        // from extremely long lines (e.g., 10MB single-line JSON files).
        let effective_width = if line_wrap_enabled {
            content_width
        } else {
            MAX_SAFE_LINE_WIDTH
        };
        tokens = Self::apply_wrapping_transform(tokens, effective_width, gutter_width);

        // Convert tokens to display lines using the view pipeline
        // Each ViewLine preserves LineStart info for correct line number rendering
        // Use binary mode if the buffer contains binary content
        // Enable ANSI awareness for non-binary content to handle escape sequences correctly
        let is_binary = state.buffer.is_binary();
        let ansi_aware = !is_binary; // ANSI parsing for normal text files
        let source_lines: Vec<ViewLine> = ViewLineIterator::new(
            &tokens,
            is_binary,
            ansi_aware,
            state.buffer_settings.tab_size,
        )
        .collect();

        // Inject virtual lines (LineAbove/LineBelow) from VirtualTextManager
        let lines = Self::inject_virtual_lines(source_lines, state);

        ViewData { lines }
    }

    /// Create a ViewLine from virtual text content (for LineAbove/LineBelow)
    fn create_virtual_line(text: &str, style: ratatui::style::Style) -> ViewLine {
        use fresh_core::api::ViewTokenStyle;

        let text = text.to_string();
        let len = text.chars().count();

        // Convert ratatui Style to ViewTokenStyle
        let token_style = ViewTokenStyle {
            fg: style.fg.and_then(|c| match c {
                ratatui::style::Color::Rgb(r, g, b) => Some((r, g, b)),
                _ => None,
            }),
            bg: style.bg.and_then(|c| match c {
                ratatui::style::Color::Rgb(r, g, b) => Some((r, g, b)),
                _ => None,
            }),
            bold: style.add_modifier.contains(ratatui::style::Modifier::BOLD),
            italic: style
                .add_modifier
                .contains(ratatui::style::Modifier::ITALIC),
        };

        ViewLine {
            text,
            // Per-character data: all None - no source mapping (this is injected content)
            char_source_bytes: vec![None; len],
            // All have the virtual text's style
            char_styles: vec![Some(token_style); len],
            // Visual column positions for each character (0, 1, 2, ...)
            char_visual_cols: (0..len).collect(),
            // Per-visual-column: each column maps to its corresponding character
            visual_to_char: (0..len).collect(),
            tab_starts: HashSet::new(),
            // AfterInjectedNewline means no line number will be shown
            line_start: LineStart::AfterInjectedNewline,
            ends_with_newline: true,
        }
    }

    /// Inject virtual lines (LineAbove/LineBelow) into the ViewLine stream
    fn inject_virtual_lines(source_lines: Vec<ViewLine>, state: &EditorState) -> Vec<ViewLine> {
        use crate::view::virtual_text::VirtualTextPosition;

        // Get viewport byte range from source lines
        let viewport_start = source_lines
            .first()
            .and_then(|l| l.char_source_bytes.iter().find_map(|m| *m))
            .unwrap_or(0);
        let viewport_end = source_lines
            .last()
            .and_then(|l| l.char_source_bytes.iter().rev().find_map(|m| *m))
            .map(|b| b + 1)
            .unwrap_or(viewport_start);

        // Query virtual lines in viewport range
        let virtual_lines = state.virtual_texts.query_lines_in_range(
            &state.marker_list,
            viewport_start,
            viewport_end,
        );

        // If no virtual lines, return source lines unchanged
        if virtual_lines.is_empty() {
            return source_lines;
        }

        // Build result with virtual lines injected
        let mut result = Vec::with_capacity(source_lines.len() + virtual_lines.len());

        for source_line in source_lines {
            // Get this line's byte range
            let line_start_byte = source_line.char_source_bytes.iter().find_map(|m| *m);
            let line_end_byte = source_line
                .char_source_bytes
                .iter()
                .rev()
                .find_map(|m| *m)
                .map(|b| b + 1);

            // Find LineAbove virtual texts anchored to this line
            if let (Some(start), Some(end)) = (line_start_byte, line_end_byte) {
                for (anchor_pos, vtext) in &virtual_lines {
                    if *anchor_pos >= start
                        && *anchor_pos < end
                        && vtext.position == VirtualTextPosition::LineAbove
                    {
                        result.push(Self::create_virtual_line(&vtext.text, vtext.style));
                    }
                }
            }

            // Add the source line
            result.push(source_line.clone());

            // Find LineBelow virtual texts anchored to this line
            if let (Some(start), Some(end)) = (line_start_byte, line_end_byte) {
                for (anchor_pos, vtext) in &virtual_lines {
                    if *anchor_pos >= start
                        && *anchor_pos < end
                        && vtext.position == VirtualTextPosition::LineBelow
                    {
                        result.push(Self::create_virtual_line(&vtext.text, vtext.style));
                    }
                }
            }
        }

        result
    }

    /// Apply soft breaks to a token stream.
    ///
    /// Walks tokens with a sorted break list `[(position, indent)]`.
    /// When a token's `source_offset` matches a break position:
    /// - For Space tokens: replace with Newline + indent Spaces
    /// - For other tokens: insert Newline + indent Spaces before the token
    /// Tokens without source_offset (injected/virtual) pass through unchanged.
    fn apply_soft_breaks(
        tokens: Vec<fresh_core::api::ViewTokenWire>,
        soft_breaks: &[(usize, u16)],
    ) -> Vec<fresh_core::api::ViewTokenWire> {
        use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};

        if soft_breaks.is_empty() {
            return tokens;
        }

        let mut output = Vec::with_capacity(tokens.len() + soft_breaks.len() * 2);
        let mut break_idx = 0;

        for token in tokens {
            let offset = match token.source_offset {
                Some(o) => o,
                None => {
                    // Injected tokens pass through
                    output.push(token);
                    continue;
                }
            };

            // Check if any break points match this token's position
            // Advance past any break positions that are before this token
            while break_idx < soft_breaks.len() && soft_breaks[break_idx].0 < offset {
                break_idx += 1;
            }

            if break_idx < soft_breaks.len() && soft_breaks[break_idx].0 == offset {
                let indent = soft_breaks[break_idx].1;
                break_idx += 1;

                match &token.kind {
                    ViewTokenWireKind::Space => {
                        // Replace the Space with Newline + indent Spaces
                        output.push(ViewTokenWire {
                            source_offset: None,
                            kind: ViewTokenWireKind::Newline,
                            style: None,
                        });
                        for _ in 0..indent {
                            output.push(ViewTokenWire {
                                source_offset: None,
                                kind: ViewTokenWireKind::Space,
                                style: None,
                            });
                        }
                    }
                    _ => {
                        // Insert Newline + indent Spaces before the token
                        output.push(ViewTokenWire {
                            source_offset: None,
                            kind: ViewTokenWireKind::Newline,
                            style: None,
                        });
                        for _ in 0..indent {
                            output.push(ViewTokenWire {
                                source_offset: None,
                                kind: ViewTokenWireKind::Space,
                                style: None,
                            });
                        }
                        output.push(token);
                    }
                }
            } else {
                output.push(token);
            }
        }

        output
    }

    /// Apply conceal ranges to a token stream.
    ///
    /// Handles partial token overlap: if a Text token spans bytes that are
    /// partially concealed, the token is split at conceal boundaries.
    /// Non-text tokens (Space, Newline) are treated as single-byte.
    ///
    /// Tokens without source_offset (injected/virtual) always pass through.
    fn apply_conceal_ranges(
        tokens: Vec<fresh_core::api::ViewTokenWire>,
        conceal_ranges: &[(std::ops::Range<usize>, Option<&str>)],
    ) -> Vec<fresh_core::api::ViewTokenWire> {
        use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};
        use std::collections::HashSet;

        if conceal_ranges.is_empty() {
            return tokens;
        }

        let mut output = Vec::with_capacity(tokens.len());
        let mut emitted_replacements: HashSet<usize> = HashSet::new();

        // Helper: check if a byte offset is concealed, returns (is_concealed, conceal_index)
        let is_concealed = |byte_offset: usize| -> Option<usize> {
            for (idx, (range, _)) in conceal_ranges.iter().enumerate() {
                if byte_offset >= range.start && byte_offset < range.end {
                    return Some(idx);
                }
            }
            None
        };

        for token in tokens {
            let offset = match token.source_offset {
                Some(o) => o,
                None => {
                    // Injected tokens always pass through
                    output.push(token);
                    continue;
                }
            };

            match &token.kind {
                ViewTokenWireKind::Text(text) => {
                    // Text tokens may span multiple bytes. We need to check
                    // each character's byte offset against conceal ranges and
                    // split the token at conceal boundaries.
                    let mut current_byte = offset;
                    let mut visible_start: Option<usize> = None; // byte offset of visible run start
                    let mut visible_chars = String::new();

                    for ch in text.chars() {
                        let ch_len = ch.len_utf8();

                        if let Some(cidx) = is_concealed(current_byte) {
                            // This char is concealed - flush any visible run first
                            if !visible_chars.is_empty() {
                                output.push(ViewTokenWire {
                                    source_offset: visible_start,
                                    kind: ViewTokenWireKind::Text(std::mem::take(
                                        &mut visible_chars,
                                    )),
                                    style: token.style.clone(),
                                });
                                visible_start = None;
                            }

                            // Emit replacement text once per conceal range.
                            // Split into first-char (with source_offset for cursor/click
                            // positioning) and remaining chars (with None source_offset).
                            // Without this split, the view pipeline's byte-advancing logic
                            // (`source + byte_idx`) causes multi-character replacements to
                            // "claim" buffer byte positions beyond the concealed range,
                            // leading to ghost cursors and mispositioned hardware cursors.
                            if let Some(repl) = conceal_ranges[cidx].1 {
                                if !emitted_replacements.contains(&cidx) {
                                    emitted_replacements.insert(cidx);
                                    if !repl.is_empty() {
                                        let mut chars = repl.chars();
                                        if let Some(first_ch) = chars.next() {
                                            // First character maps to the concealed range start
                                            output.push(ViewTokenWire {
                                                source_offset: Some(conceal_ranges[cidx].0.start),
                                                kind: ViewTokenWireKind::Text(first_ch.to_string()),
                                                style: None,
                                            });
                                            let rest: String = chars.collect();
                                            if !rest.is_empty() {
                                                // Remaining characters are synthetic — no source mapping
                                                output.push(ViewTokenWire {
                                                    source_offset: None,
                                                    kind: ViewTokenWireKind::Text(rest),
                                                    style: None,
                                                });
                                            }
                                        }
                                    }
                                }
                            }
                        } else {
                            // Visible char - accumulate
                            if visible_start.is_none() {
                                visible_start = Some(current_byte);
                            }
                            visible_chars.push(ch);
                        }

                        current_byte += ch_len;
                    }

                    // Flush remaining visible chars
                    if !visible_chars.is_empty() {
                        output.push(ViewTokenWire {
                            source_offset: visible_start,
                            kind: ViewTokenWireKind::Text(visible_chars),
                            style: token.style.clone(),
                        });
                    }
                }
                ViewTokenWireKind::Space
                | ViewTokenWireKind::Newline
                | ViewTokenWireKind::Break => {
                    // Single-byte tokens: conceal or pass through
                    if is_concealed(offset).is_some() {
                        // Skip concealed space/newline
                    } else {
                        output.push(token);
                    }
                }
                ViewTokenWireKind::BinaryByte(_) => {
                    if is_concealed(offset).is_some() {
                        // Skip concealed binary byte
                    } else {
                        output.push(token);
                    }
                }
            }
        }

        output
    }

    fn build_base_tokens(
        buffer: &mut Buffer,
        top_byte: usize,
        estimated_line_length: usize,
        visible_count: usize,
        is_binary: bool,
        line_ending: crate::model::buffer::LineEnding,
    ) -> Vec<fresh_core::api::ViewTokenWire> {
        use crate::model::buffer::LineEnding;
        use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};

        let mut tokens = Vec::new();

        // For binary files, read raw bytes directly to preserve byte values
        // (LineIterator uses String::from_utf8_lossy which loses high bytes)
        if is_binary {
            return Self::build_base_tokens_binary(
                buffer,
                top_byte,
                estimated_line_length,
                visible_count,
            );
        }

        let mut iter = buffer.line_iterator(top_byte, estimated_line_length);
        let mut lines_seen = 0usize;
        let max_lines = visible_count.saturating_add(4);

        while lines_seen < max_lines {
            if let Some((line_start, line_content)) = iter.next_line() {
                let mut byte_offset = 0usize;
                let content_bytes = line_content.as_bytes();
                let mut skip_next_lf = false; // Track if we should skip \n after \r in CRLF
                let mut chars_this_line = 0usize; // Track chars to enforce MAX_SAFE_LINE_WIDTH
                for ch in line_content.chars() {
                    // Limit characters per line to prevent memory exhaustion from huge lines.
                    // Insert a Break token to force wrapping at safe intervals.
                    if chars_this_line >= MAX_SAFE_LINE_WIDTH {
                        tokens.push(ViewTokenWire {
                            source_offset: None,
                            kind: ViewTokenWireKind::Break,
                            style: None,
                        });
                        chars_this_line = 0;
                        // Count this as a new visual line for the max_lines limit
                        lines_seen += 1;
                        if lines_seen >= max_lines {
                            break;
                        }
                    }
                    chars_this_line += 1;

                    let ch_len = ch.len_utf8();
                    let source_offset = Some(line_start + byte_offset);

                    match ch {
                        '\r' => {
                            // In CRLF mode with \r\n: emit Newline at \r position, skip the \n
                            // This allows cursor at \r (end of line) to be visible
                            // In LF/Unix files, ANY \r is unusual and should be shown as <0D>
                            let is_crlf_file = line_ending == LineEnding::CRLF;
                            let next_byte = content_bytes.get(byte_offset + 1);
                            if is_crlf_file && next_byte == Some(&b'\n') {
                                // CRLF: emit Newline token at \r position for cursor visibility
                                tokens.push(ViewTokenWire {
                                    source_offset,
                                    kind: ViewTokenWireKind::Newline,
                                    style: None,
                                });
                                // Mark to skip the following \n in the char iterator
                                skip_next_lf = true;
                                byte_offset += ch_len;
                                continue;
                            }
                            // LF file or standalone \r - show as control character
                            tokens.push(ViewTokenWire {
                                source_offset,
                                kind: ViewTokenWireKind::BinaryByte(ch as u8),
                                style: None,
                            });
                        }
                        '\n' if skip_next_lf => {
                            // Skip \n that follows \r in CRLF mode (already emitted Newline at \r)
                            skip_next_lf = false;
                            byte_offset += ch_len;
                            continue;
                        }
                        '\n' => {
                            tokens.push(ViewTokenWire {
                                source_offset,
                                kind: ViewTokenWireKind::Newline,
                                style: None,
                            });
                        }
                        ' ' => {
                            tokens.push(ViewTokenWire {
                                source_offset,
                                kind: ViewTokenWireKind::Space,
                                style: None,
                            });
                        }
                        '\t' => {
                            // Tab is safe, emit as Text
                            tokens.push(ViewTokenWire {
                                source_offset,
                                kind: ViewTokenWireKind::Text(ch.to_string()),
                                style: None,
                            });
                        }
                        _ if Self::is_control_char(ch) => {
                            // Control character - emit as BinaryByte to render as <XX>
                            tokens.push(ViewTokenWire {
                                source_offset,
                                kind: ViewTokenWireKind::BinaryByte(ch as u8),
                                style: None,
                            });
                        }
                        _ => {
                            // Accumulate consecutive non-space/non-newline chars into Text tokens
                            if let Some(last) = tokens.last_mut() {
                                if let ViewTokenWireKind::Text(ref mut s) = last.kind {
                                    // Extend existing Text token if contiguous
                                    let expected_offset = last.source_offset.map(|o| o + s.len());
                                    if expected_offset == Some(line_start + byte_offset) {
                                        s.push(ch);
                                        byte_offset += ch_len;
                                        continue;
                                    }
                                }
                            }
                            tokens.push(ViewTokenWire {
                                source_offset,
                                kind: ViewTokenWireKind::Text(ch.to_string()),
                                style: None,
                            });
                        }
                    }
                    byte_offset += ch_len;
                }
                lines_seen += 1;
            } else {
                break;
            }
        }

        // Handle empty buffer
        if tokens.is_empty() {
            tokens.push(ViewTokenWire {
                source_offset: Some(top_byte),
                kind: ViewTokenWireKind::Text(String::new()),
                style: None,
            });
        }

        tokens
    }

    /// Build tokens for binary files by reading raw bytes directly
    /// This preserves byte values >= 0x80 that would be lost by String::from_utf8_lossy
    fn build_base_tokens_binary(
        buffer: &mut Buffer,
        top_byte: usize,
        estimated_line_length: usize,
        visible_count: usize,
    ) -> Vec<fresh_core::api::ViewTokenWire> {
        use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};

        let mut tokens = Vec::new();
        let max_lines = visible_count.saturating_add(4);
        let buffer_len = buffer.len();

        if top_byte >= buffer_len {
            tokens.push(ViewTokenWire {
                source_offset: Some(top_byte),
                kind: ViewTokenWireKind::Text(String::new()),
                style: None,
            });
            return tokens;
        }

        // Estimate how many bytes we need to read
        let estimated_bytes = estimated_line_length * max_lines * 2;
        let bytes_to_read = estimated_bytes.min(buffer_len - top_byte);

        // Read raw bytes directly from buffer
        let raw_bytes = buffer.slice_bytes(top_byte..top_byte + bytes_to_read);

        let mut byte_offset = 0usize;
        let mut lines_seen = 0usize;
        let mut current_text = String::new();
        let mut current_text_start: Option<usize> = None;

        // Helper to flush accumulated text to tokens
        let flush_text =
            |tokens: &mut Vec<ViewTokenWire>, text: &mut String, start: &mut Option<usize>| {
                if !text.is_empty() {
                    tokens.push(ViewTokenWire {
                        source_offset: *start,
                        kind: ViewTokenWireKind::Text(std::mem::take(text)),
                        style: None,
                    });
                    *start = None;
                }
            };

        while byte_offset < raw_bytes.len() && lines_seen < max_lines {
            let b = raw_bytes[byte_offset];
            let source_offset = top_byte + byte_offset;

            match b {
                b'\n' => {
                    flush_text(&mut tokens, &mut current_text, &mut current_text_start);
                    tokens.push(ViewTokenWire {
                        source_offset: Some(source_offset),
                        kind: ViewTokenWireKind::Newline,
                        style: None,
                    });
                    lines_seen += 1;
                }
                b' ' => {
                    flush_text(&mut tokens, &mut current_text, &mut current_text_start);
                    tokens.push(ViewTokenWire {
                        source_offset: Some(source_offset),
                        kind: ViewTokenWireKind::Space,
                        style: None,
                    });
                }
                _ => {
                    // For binary files, emit unprintable bytes as BinaryByte tokens
                    // This ensures view_pipeline.rs can map all 4 chars of <XX> to the same source byte
                    if Self::is_binary_unprintable(b) {
                        // Flush any accumulated printable text first
                        flush_text(&mut tokens, &mut current_text, &mut current_text_start);
                        // Emit as BinaryByte so cursor positioning works correctly
                        tokens.push(ViewTokenWire {
                            source_offset: Some(source_offset),
                            kind: ViewTokenWireKind::BinaryByte(b),
                            style: None,
                        });
                    } else {
                        // Printable ASCII - accumulate into text token
                        // Each printable char is 1 byte so accumulation works correctly
                        if current_text_start.is_none() {
                            current_text_start = Some(source_offset);
                        }
                        current_text.push(b as char);
                    }
                }
            }
            byte_offset += 1;
        }

        // Flush any remaining text
        flush_text(&mut tokens, &mut current_text, &mut current_text_start);

        // Handle empty buffer
        if tokens.is_empty() {
            tokens.push(ViewTokenWire {
                source_offset: Some(top_byte),
                kind: ViewTokenWireKind::Text(String::new()),
                style: None,
            });
        }

        tokens
    }

    /// Check if a byte should be displayed as <XX> in binary mode
    /// Returns true for:
    /// - Control characters (0x00-0x1F) except tab and newline
    /// - DEL (0x7F)
    /// - High bytes (0x80-0xFF) which are not valid single-byte UTF-8
    ///
    /// Note: In binary mode, we must be very strict about what characters we allow through,
    /// because control characters can move the terminal cursor and corrupt the display:
    /// - CR (0x0D) moves cursor to column 0, overwriting the gutter
    /// - VT (0x0B) and FF (0x0C) move cursor vertically
    /// - ESC (0x1B) starts ANSI escape sequences
    fn is_binary_unprintable(b: u8) -> bool {
        // Only allow: tab (0x09) and newline (0x0A)
        // These are the only safe whitespace characters in binary mode
        // All other control characters can corrupt terminal output
        if b == 0x09 || b == 0x0A {
            return false;
        }
        // All other control characters (0x00-0x1F) are unprintable in binary mode
        // This includes CR, VT, FF, ESC which can move the cursor
        if b < 0x20 {
            return true;
        }
        // DEL character (0x7F) is unprintable
        if b == 0x7F {
            return true;
        }
        // High bytes (0x80-0xFF) are unprintable in binary mode
        // (they're not valid single-byte UTF-8 and would be converted to replacement char)
        if b >= 0x80 {
            return true;
        }
        false
    }

    /// Check if a character is a control character that should be rendered as <XX>
    /// This applies to ALL files (binary and non-binary) to prevent terminal corruption
    fn is_control_char(ch: char) -> bool {
        let code = ch as u32;
        // Only check ASCII range
        if code >= 128 {
            return false;
        }
        let b = code as u8;
        // Allow: tab (0x09), newline (0x0A), ESC (0x1B - for ANSI sequences)
        if b == 0x09 || b == 0x0A || b == 0x1B {
            return false;
        }
        // Other control characters (0x00-0x1F) and DEL (0x7F) are dangerous
        // This includes CR (0x0D), VT (0x0B), FF (0x0C) which move the cursor
        b < 0x20 || b == 0x7F
    }

    /// Public wrapper for building base tokens - used by render.rs for the view_transform_request hook
    pub fn build_base_tokens_for_hook(
        buffer: &mut Buffer,
        top_byte: usize,
        estimated_line_length: usize,
        visible_count: usize,
        is_binary: bool,
        line_ending: crate::model::buffer::LineEnding,
    ) -> Vec<fresh_core::api::ViewTokenWire> {
        Self::build_base_tokens(
            buffer,
            top_byte,
            estimated_line_length,
            visible_count,
            is_binary,
            line_ending,
        )
    }

    fn apply_wrapping_transform(
        tokens: Vec<fresh_core::api::ViewTokenWire>,
        content_width: usize,
        gutter_width: usize,
    ) -> Vec<fresh_core::api::ViewTokenWire> {
        use crate::primitives::visual_layout::visual_width;
        use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};

        let mut wrapped = Vec::new();
        let mut current_line_width = 0;

        // Calculate available width (accounting for gutter on first line only)
        let available_width = content_width.saturating_sub(gutter_width);

        for token in tokens {
            match &token.kind {
                ViewTokenWireKind::Newline => {
                    // Real newlines always break the line
                    wrapped.push(token);
                    current_line_width = 0;
                }
                ViewTokenWireKind::Text(text) => {
                    // Use visual_width which properly handles tabs and ANSI codes
                    let text_visual_width = visual_width(text, current_line_width);

                    // If this token would exceed line width, insert Break before it
                    if current_line_width > 0
                        && current_line_width + text_visual_width > available_width
                    {
                        wrapped.push(ViewTokenWire {
                            source_offset: None,
                            kind: ViewTokenWireKind::Break,
                            style: None,
                        });
                        current_line_width = 0;
                    }

                    // Recalculate visual width after potential line break (tabs depend on column)
                    let text_visual_width = visual_width(text, current_line_width);

                    // If visible text is longer than line width, we need to split
                    // However, we don't split tokens containing ANSI codes to avoid
                    // breaking escape sequences. ANSI-heavy content may exceed line width.
                    if text_visual_width > available_width
                        && !crate::primitives::ansi::contains_ansi_codes(text)
                    {
                        use unicode_segmentation::UnicodeSegmentation;

                        // Collect graphemes with their byte offsets for proper Unicode handling
                        let graphemes: Vec<(usize, &str)> = text.grapheme_indices(true).collect();
                        let mut grapheme_idx = 0;
                        let source_base = token.source_offset;

                        while grapheme_idx < graphemes.len() {
                            // Calculate how many graphemes fit in remaining space
                            // by summing visual widths until we exceed available width
                            let remaining_width =
                                available_width.saturating_sub(current_line_width);
                            if remaining_width == 0 {
                                // Need to break to next line
                                wrapped.push(ViewTokenWire {
                                    source_offset: None,
                                    kind: ViewTokenWireKind::Break,
                                    style: None,
                                });
                                current_line_width = 0;
                                continue;
                            }

                            let mut chunk_visual_width = 0;
                            let mut chunk_grapheme_count = 0;
                            let mut col = current_line_width;

                            for &(_byte_offset, grapheme) in &graphemes[grapheme_idx..] {
                                let g_width = if grapheme == "\t" {
                                    crate::primitives::visual_layout::tab_expansion_width(col)
                                } else {
                                    crate::primitives::display_width::str_width(grapheme)
                                };

                                if chunk_visual_width + g_width > remaining_width
                                    && chunk_grapheme_count > 0
                                {
                                    break;
                                }

                                chunk_visual_width += g_width;
                                chunk_grapheme_count += 1;
                                col += g_width;
                            }

                            if chunk_grapheme_count == 0 {
                                // Single grapheme is wider than available width, force it
                                chunk_grapheme_count = 1;
                                let grapheme = graphemes[grapheme_idx].1;
                                chunk_visual_width = if grapheme == "\t" {
                                    crate::primitives::visual_layout::tab_expansion_width(
                                        current_line_width,
                                    )
                                } else {
                                    crate::primitives::display_width::str_width(grapheme)
                                };
                            }

                            // Build chunk from graphemes and calculate source offset
                            let chunk_start_byte = graphemes[grapheme_idx].0;
                            let chunk_end_byte =
                                if grapheme_idx + chunk_grapheme_count < graphemes.len() {
                                    graphemes[grapheme_idx + chunk_grapheme_count].0
                                } else {
                                    text.len()
                                };
                            let chunk = text[chunk_start_byte..chunk_end_byte].to_string();
                            let chunk_source = source_base.map(|b| b + chunk_start_byte);

                            wrapped.push(ViewTokenWire {
                                source_offset: chunk_source,
                                kind: ViewTokenWireKind::Text(chunk),
                                style: token.style.clone(),
                            });

                            current_line_width += chunk_visual_width;
                            grapheme_idx += chunk_grapheme_count;

                            // If we filled the line, break
                            if current_line_width >= available_width {
                                wrapped.push(ViewTokenWire {
                                    source_offset: None,
                                    kind: ViewTokenWireKind::Break,
                                    style: None,
                                });
                                current_line_width = 0;
                            }
                        }
                    } else {
                        wrapped.push(token);
                        current_line_width += text_visual_width;
                    }
                }
                ViewTokenWireKind::Space => {
                    // Spaces count toward line width
                    if current_line_width + 1 > available_width {
                        wrapped.push(ViewTokenWire {
                            source_offset: None,
                            kind: ViewTokenWireKind::Break,
                            style: None,
                        });
                        current_line_width = 0;
                    }
                    wrapped.push(token);
                    current_line_width += 1;
                }
                ViewTokenWireKind::Break => {
                    // Pass through existing breaks
                    wrapped.push(token);
                    current_line_width = 0;
                }
                ViewTokenWireKind::BinaryByte(_) => {
                    // Binary bytes render as <XX> which is 4 characters
                    let byte_display_width = 4;
                    if current_line_width + byte_display_width > available_width {
                        wrapped.push(ViewTokenWire {
                            source_offset: None,
                            kind: ViewTokenWireKind::Break,
                            style: None,
                        });
                        current_line_width = 0;
                    }
                    wrapped.push(token);
                    current_line_width += byte_display_width;
                }
            }
        }

        wrapped
    }

    fn calculate_view_anchor(view_lines: &[ViewLine], top_byte: usize) -> ViewAnchor {
        // Find the first line that contains source content at or after top_byte
        // Walk backwards to include any injected content (headers) that precede it
        for (idx, line) in view_lines.iter().enumerate() {
            // Check if this line has source content at or after top_byte
            if let Some(first_source) = line.char_source_bytes.iter().find_map(|m| *m) {
                if first_source >= top_byte {
                    // Found a line with source >= top_byte
                    // But we may need to include previous lines if they're injected headers
                    let mut start_idx = idx;
                    while start_idx > 0 {
                        let prev_line = &view_lines[start_idx - 1];
                        // If previous line is all injected (no source mappings), include it
                        let prev_has_source =
                            prev_line.char_source_bytes.iter().any(|m| m.is_some());
                        if !prev_has_source {
                            start_idx -= 1;
                        } else {
                            break;
                        }
                    }
                    return ViewAnchor {
                        start_line_idx: start_idx,
                        start_line_skip: 0,
                    };
                }
            }
        }

        // No matching source found, start from beginning
        ViewAnchor {
            start_line_idx: 0,
            start_line_skip: 0,
        }
    }

    fn calculate_compose_layout(
        area: Rect,
        view_mode: &ViewMode,
        compose_width: Option<u16>,
    ) -> ComposeLayout {
        // Enable centering/margins if:
        // 1. View mode is explicitly Compose, OR
        // 2. compose_width is set (plugin-driven compose mode)
        let should_compose = view_mode == &ViewMode::Compose || compose_width.is_some();

        if !should_compose {
            return ComposeLayout {
                render_area: area,
                left_pad: 0,
                right_pad: 0,
            };
        }

        let target_width = compose_width.unwrap_or(area.width);
        let clamped_width = target_width.min(area.width).max(1);
        if clamped_width >= area.width {
            return ComposeLayout {
                render_area: area,
                left_pad: 0,
                right_pad: 0,
            };
        }

        let pad_total = area.width - clamped_width;
        let left_pad = pad_total / 2;
        let right_pad = pad_total - left_pad;

        ComposeLayout {
            render_area: Rect::new(area.x + left_pad, area.y, clamped_width, area.height),
            left_pad,
            right_pad,
        }
    }

    fn render_compose_margins(
        frame: &mut Frame,
        area: Rect,
        layout: &ComposeLayout,
        _view_mode: &ViewMode,
        theme: &crate::view::theme::Theme,
        effective_editor_bg: ratatui::style::Color,
    ) {
        // Render margins if there are any pads (indicates compose layout is active)
        if layout.left_pad == 0 && layout.right_pad == 0 {
            return;
        }

        // Paper-on-desk effect: outer "desk" margin with inner "paper edge"
        // Layout: [desk][paper edge][content][paper edge][desk]
        const PAPER_EDGE_WIDTH: u16 = 1;

        let desk_style = Style::default().bg(theme.compose_margin_bg);
        let paper_style = Style::default().bg(effective_editor_bg);

        if layout.left_pad > 0 {
            let paper_edge = PAPER_EDGE_WIDTH.min(layout.left_pad);
            let desk_width = layout.left_pad.saturating_sub(paper_edge);

            // Desk area (outer)
            if desk_width > 0 {
                let desk_rect = Rect::new(area.x, area.y, desk_width, area.height);
                frame.render_widget(Block::default().style(desk_style), desk_rect);
            }

            // Paper edge (inner, adjacent to content)
            if paper_edge > 0 {
                let paper_rect = Rect::new(area.x + desk_width, area.y, paper_edge, area.height);
                frame.render_widget(Block::default().style(paper_style), paper_rect);
            }
        }

        if layout.right_pad > 0 {
            let paper_edge = PAPER_EDGE_WIDTH.min(layout.right_pad);
            let desk_width = layout.right_pad.saturating_sub(paper_edge);
            let right_start = area.x + layout.left_pad + layout.render_area.width;

            // Paper edge (inner, adjacent to content)
            if paper_edge > 0 {
                let paper_rect = Rect::new(right_start, area.y, paper_edge, area.height);
                frame.render_widget(Block::default().style(paper_style), paper_rect);
            }

            // Desk area (outer)
            if desk_width > 0 {
                let desk_rect =
                    Rect::new(right_start + paper_edge, area.y, desk_width, area.height);
                frame.render_widget(Block::default().style(desk_style), desk_rect);
            }
        }
    }

    fn selection_context(
        state: &EditorState,
        cursors: &crate::model::cursor::Cursors,
    ) -> SelectionContext {
        let ranges: Vec<Range<usize>> = cursors
            .iter()
            .filter_map(|(_, cursor)| {
                // Don't include normal selection for cursors in block selection mode
                // Block selections are rendered separately via block_rects
                if cursor.selection_mode == SelectionMode::Block {
                    None
                } else {
                    cursor.selection_range()
                }
            })
            .collect();

        let block_rects: Vec<(usize, usize, usize, usize)> = cursors
            .iter()
            .filter_map(|(_, cursor)| {
                if cursor.selection_mode == SelectionMode::Block {
                    if let Some(anchor) = cursor.block_anchor {
                        // Convert cursor position to 2D coords
                        let cur_line = state.buffer.get_line_number(cursor.position);
                        let cur_line_start = state.buffer.line_start_offset(cur_line).unwrap_or(0);
                        let cur_col = cursor.position.saturating_sub(cur_line_start);

                        // Return normalized rectangle (min values first)
                        Some((
                            anchor.line.min(cur_line),
                            anchor.column.min(cur_col),
                            anchor.line.max(cur_line),
                            anchor.column.max(cur_col),
                        ))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();

        let cursor_positions: Vec<usize> = if state.show_cursors {
            cursors.iter().map(|(_, cursor)| cursor.position).collect()
        } else {
            Vec::new()
        };

        SelectionContext {
            ranges,
            block_rects,
            cursor_positions,
            primary_cursor_position: cursors.primary().position,
        }
    }

    fn decoration_context(
        state: &mut EditorState,
        viewport_start: usize,
        viewport_end: usize,
        primary_cursor_position: usize,
        theme: &crate::view::theme::Theme,
        highlight_context_bytes: usize,
        view_mode: &ViewMode,
    ) -> DecorationContext {
        // Extend highlighting range by ~1 viewport size before/after for better context.
        // This helps tree-sitter parse multi-line constructs that span viewport boundaries.
        let viewport_size = viewport_end.saturating_sub(viewport_start);
        let highlight_start = viewport_start.saturating_sub(viewport_size);
        let highlight_end = viewport_end
            .saturating_add(viewport_size)
            .min(state.buffer.len());

        let highlight_spans = state.highlighter.highlight_viewport(
            &state.buffer,
            highlight_start,
            highlight_end,
            theme,
            highlight_context_bytes,
        );

        // Update reference highlight overlays (debounced, creates overlays that auto-adjust)
        state.reference_highlight_overlay.update(
            &state.buffer,
            &mut state.overlays,
            &mut state.marker_list,
            &mut state.reference_highlighter,
            primary_cursor_position,
            viewport_start,
            viewport_end,
            highlight_context_bytes,
            theme.semantic_highlight_bg,
        );

        // Update bracket highlight overlays
        state.bracket_highlight_overlay.update(
            &state.buffer,
            &mut state.overlays,
            &mut state.marker_list,
            primary_cursor_position,
        );

        // Semantic tokens are stored as overlays so their ranges track edits.
        // Convert them into highlight spans for the render pipeline.
        let is_compose = matches!(view_mode, ViewMode::Compose);
        let md_emphasis_ns =
            fresh_core::overlay::OverlayNamespace::from_string("md-emphasis".to_string());
        let mut semantic_token_spans = Vec::new();
        let mut viewport_overlays = Vec::new();
        for (overlay, range) in
            state
                .overlays
                .query_viewport(viewport_start, viewport_end, &state.marker_list)
        {
            if crate::services::lsp::semantic_tokens::is_semantic_token_overlay(overlay) {
                if let crate::view::overlay::OverlayFace::Foreground { color } = &overlay.face {
                    semantic_token_spans.push(crate::primitives::highlighter::HighlightSpan {
                        range,
                        color: *color,
                    });
                }
                continue;
            }

            // Skip markdown compose overlays in Source mode — they should only
            // render in the Compose-mode split.
            if !is_compose {
                if overlay.namespace.as_ref() == Some(&md_emphasis_ns) {
                    continue;
                }
            }

            viewport_overlays.push((overlay.clone(), range));
        }

        // Use the lsp-diagnostic namespace to identify diagnostic overlays
        let diagnostic_ns = crate::services::lsp::diagnostics::lsp_diagnostic_namespace();
        let diagnostic_lines: HashSet<usize> = viewport_overlays
            .iter()
            .filter_map(|(overlay, range)| {
                if overlay.namespace.as_ref() == Some(&diagnostic_ns) {
                    return Some(state.buffer.get_line_number(range.start));
                }
                None
            })
            .collect();

        let virtual_text_lookup: HashMap<usize, Vec<crate::view::virtual_text::VirtualText>> =
            state
                .virtual_texts
                .build_lookup(&state.marker_list, viewport_start, viewport_end)
                .into_iter()
                .map(|(position, texts)| (position, texts.into_iter().cloned().collect()))
                .collect();

        // Pre-compute line indicators for the viewport (only query markers in visible range)
        let line_indicators = state.margins.get_indicators_for_viewport(
            viewport_start,
            viewport_end,
            |byte_offset| state.buffer.get_line_number(byte_offset),
        );

        DecorationContext {
            highlight_spans,
            semantic_token_spans,
            viewport_overlays,
            virtual_text_lookup,
            diagnostic_lines,
            line_indicators,
        }
    }

    // semantic token colors are mapped when overlays are created

    fn calculate_viewport_end(
        state: &mut EditorState,
        viewport_start: usize,
        estimated_line_length: usize,
        visible_count: usize,
    ) -> usize {
        let mut iter_temp = state
            .buffer
            .line_iterator(viewport_start, estimated_line_length);
        let mut viewport_end = viewport_start;
        for _ in 0..visible_count {
            if let Some((line_start, line_content)) = iter_temp.next_line() {
                viewport_end = line_start + line_content.len();
            } else {
                break;
            }
        }
        viewport_end
    }

    fn render_view_lines(input: LineRenderInput<'_>) -> LineRenderOutput {
        let LineRenderInput {
            state,
            theme,
            view_lines,
            view_anchor,
            render_area,
            gutter_width,
            selection,
            decorations,
            starting_line_num,
            visible_line_count,
            lsp_waiting,
            is_active,
            line_wrap,
            estimated_lines,
            left_column,
            relative_line_numbers,
            session_mode,
        } = input;

        let selection_ranges = &selection.ranges;
        let block_selections = &selection.block_rects;
        let cursor_positions = &selection.cursor_positions;
        let primary_cursor_position = selection.primary_cursor_position;

        // Compute cursor line number for relative line numbers display
        let cursor_line = state.buffer.get_line_number(primary_cursor_position);

        let highlight_spans = &decorations.highlight_spans;
        let semantic_token_spans = &decorations.semantic_token_spans;
        let viewport_overlays = &decorations.viewport_overlays;
        let virtual_text_lookup = &decorations.virtual_text_lookup;
        let diagnostic_lines = &decorations.diagnostic_lines;
        let line_indicators = &decorations.line_indicators;

        let mut lines = Vec::new();
        let mut view_line_mappings = Vec::new();
        let mut lines_rendered = 0usize;
        let mut view_iter_idx = view_anchor.start_line_idx;
        let mut cursor_screen_x = 0u16;
        let mut cursor_screen_y = 0u16;
        let mut have_cursor = false;
        let mut last_line_end: Option<LastLineEnd> = None;

        let is_empty_buffer = state.buffer.is_empty();

        // Track cursor position during rendering (eliminates duplicate line iteration)
        let mut last_visible_x: u16 = 0;
        let _view_start_line_skip = view_anchor.start_line_skip; // Currently unused

        // Track the current source line number separately from display lines
        let mut current_source_line_num = starting_line_num;
        // Track whether the previous line was a source line (showed a line number)
        // Used to determine when to increment the line counter
        let mut prev_was_source_line = false;

        loop {
            // Get the current ViewLine from the pipeline
            let current_view_line = if let Some(vl) = view_lines.get(view_iter_idx) {
                vl
            } else if is_empty_buffer && lines_rendered == 0 {
                // Handle empty buffer case - create a minimal line
                static EMPTY_LINE: std::sync::OnceLock<ViewLine> = std::sync::OnceLock::new();
                EMPTY_LINE.get_or_init(|| ViewLine {
                    text: String::new(),
                    char_source_bytes: Vec::new(),
                    char_styles: Vec::new(),
                    char_visual_cols: Vec::new(),
                    visual_to_char: Vec::new(),
                    tab_starts: HashSet::new(),
                    line_start: LineStart::Beginning,
                    ends_with_newline: false,
                })
            } else {
                break;
            };

            // Extract line data
            let line_content = current_view_line.text.clone();
            let line_has_newline = current_view_line.ends_with_newline;
            let line_char_source_bytes = &current_view_line.char_source_bytes;
            let line_char_styles = &current_view_line.char_styles;
            let line_visual_to_char = &current_view_line.visual_to_char;
            let line_tab_starts = &current_view_line.tab_starts;
            let _line_start_type = current_view_line.line_start; // Available for future use

            // Helper to get source byte at a visual column using the new O(1) lookup
            let _source_byte_at_col = |vis_col: usize| -> Option<usize> {
                let char_idx = line_visual_to_char.get(vis_col).copied()?;
                line_char_source_bytes.get(char_idx).copied().flatten()
            };

            view_iter_idx += 1;

            if lines_rendered >= visible_line_count {
                break;
            }

            // Use the elegant pipeline's should_show_line_number function
            // This correctly handles: injected content, wrapped continuations, and source lines
            let show_line_number = should_show_line_number(current_view_line);

            // Only increment source line number when BOTH:
            // 1. We've already rendered at least one source line (prev_was_source_line)
            // 2. The CURRENT line is also a source line
            // This ensures virtual/injected lines don't cause line numbers to skip
            if show_line_number && prev_was_source_line {
                current_source_line_num += 1;
            }
            // Only update the flag when we see a source line - virtual lines
            // between source lines shouldn't reset the tracking
            if show_line_number {
                prev_was_source_line = true;
            }

            // is_continuation means "don't show line number" for rendering purposes
            let is_continuation = !show_line_number;

            lines_rendered += 1;

            // Apply horizontal scrolling - skip characters before left_column
            let left_col = left_column;

            // Build line with selection highlighting
            let mut line_spans = Vec::new();
            let mut line_view_map: Vec<Option<usize>> = Vec::new();
            let mut last_seg_y: Option<u16> = None;
            let mut _last_seg_width: usize = 0;

            // Accumulator for merging consecutive characters with the same style
            // This is critical for proper rendering of combining characters (Thai, etc.)
            let mut span_acc = SpanAccumulator::new();

            // Render left margin (indicators + line numbers + separator)
            render_left_margin(
                &LeftMarginContext {
                    state,
                    theme,
                    is_continuation,
                    current_source_line_num,
                    estimated_lines,
                    diagnostic_lines,
                    line_indicators,
                    cursor_line,
                    relative_line_numbers,
                },
                &mut line_spans,
                &mut line_view_map,
            );

            // Check if this line has any selected text
            let mut byte_index = 0; // Byte offset in line_content string
            let mut display_char_idx = 0usize; // Character index in text (for char_source_bytes)
            let mut col_offset = 0usize; // Visual column position

            // Performance optimization: For very long lines, only process visible characters
            // Calculate the maximum characters we might need to render based on screen width
            // For wrapped lines, we need enough characters to fill the visible viewport
            // For non-wrapped lines, we only need one screen width worth
            let visible_lines_remaining = visible_line_count.saturating_sub(lines_rendered);
            let max_visible_chars = if line_wrap {
                // With wrapping: might need chars for multiple wrapped lines
                // Be generous to avoid cutting off wrapped content
                (render_area.width as usize)
                    .saturating_mul(visible_lines_remaining.max(1))
                    .saturating_add(200)
            } else {
                // Without wrapping: only need one line worth of characters
                (render_area.width as usize).saturating_add(100)
            };
            let max_chars_to_process = left_col.saturating_add(max_visible_chars);

            // ANSI parser for this line to handle escape sequences
            // Optimization: only create parser if line contains ESC byte
            let line_has_ansi = line_content.contains('\x1b');
            let mut ansi_parser = if line_has_ansi {
                Some(AnsiParser::new())
            } else {
                None
            };
            // Track visible characters separately from byte position for ANSI handling
            let mut visible_char_count = 0usize;

            // Debug mode: track active highlight/overlay spans for WordPerfect-style reveal codes
            let mut debug_tracker = if state.debug_highlight_mode {
                Some(DebugSpanTracker::default())
            } else {
                None
            };

            // Track byte positions for extend_to_line_end feature
            let mut first_line_byte_pos: Option<usize> = None;
            let mut last_line_byte_pos: Option<usize> = None;

            let chars_iterator = line_content.chars().peekable();
            for ch in chars_iterator {
                // Get source byte for this character using character index
                // (char_source_bytes is indexed by character position, not visual column)
                let byte_pos = line_char_source_bytes
                    .get(display_char_idx)
                    .copied()
                    .flatten();

                // Track byte positions for extend_to_line_end
                if let Some(bp) = byte_pos {
                    if first_line_byte_pos.is_none() {
                        first_line_byte_pos = Some(bp);
                    }
                    last_line_byte_pos = Some(bp);
                }

                // Process character through ANSI parser first (if line has ANSI)
                // If parser returns None, the character is part of an escape sequence and should be skipped
                let ansi_style = if let Some(ref mut parser) = ansi_parser {
                    match parser.parse_char(ch) {
                        Some(style) => style,
                        None => {
                            // This character is part of an ANSI escape sequence, skip it
                            // ANSI escape chars have zero visual width, so don't increment col_offset
                            // IMPORTANT: If the cursor is on this ANSI byte, track it
                            if let Some(bp) = byte_pos {
                                if bp == primary_cursor_position && !have_cursor {
                                    // Account for horizontal scrolling by using col_offset - left_col
                                    cursor_screen_x = gutter_width as u16
                                        + col_offset.saturating_sub(left_col) as u16;
                                    cursor_screen_y = lines_rendered.saturating_sub(1) as u16;
                                    have_cursor = true;
                                }
                            }
                            byte_index += ch.len_utf8();
                            display_char_idx += 1;
                            // Note: col_offset not incremented - ANSI chars have 0 visual width
                            continue;
                        }
                    }
                } else {
                    // No ANSI in this line - use default style (fast path)
                    Style::default()
                };

                // Performance: skip expensive style calculations for characters beyond visible range
                // Use visible_char_count (not byte_index) since ANSI codes don't take up visible space
                if visible_char_count > max_chars_to_process {
                    // Fast path: skip remaining characters without processing
                    // This is critical for performance with very long lines (e.g., 100KB single line)
                    break;
                }

                // Skip characters before left_column
                if col_offset >= left_col {
                    // Check if this view position is the START of a tab expansion
                    let is_tab_start = line_tab_starts.contains(&col_offset);

                    // Check if this character is at a cursor position
                    // For tab expansions: only show cursor on the FIRST space (the tab_start position)
                    // This prevents cursor from appearing on all 8 expanded spaces
                    let is_cursor = byte_pos
                        .map(|bp| {
                            if !cursor_positions.contains(&bp) || bp >= state.buffer.len() {
                                return false;
                            }
                            // If this byte maps to a tab character, only show cursor at tab_start
                            // Check if this is part of a tab expansion by looking at previous char
                            let prev_char_idx = display_char_idx.saturating_sub(1);
                            let prev_byte_pos =
                                line_char_source_bytes.get(prev_char_idx).copied().flatten();
                            // Show cursor if: this is start of line, OR previous char had different byte pos
                            display_char_idx == 0 || prev_byte_pos != Some(bp)
                        })
                        .unwrap_or(false);

                    // Check if this character is in any selection range (but not at cursor position)
                    // Also check for block/rectangular selections
                    let is_in_block_selection = block_selections.iter().any(
                        |(start_line, start_col, end_line, end_col)| {
                            current_source_line_num >= *start_line
                                && current_source_line_num <= *end_line
                                && byte_index >= *start_col
                                && byte_index <= *end_col
                        },
                    );

                    // For primary cursor in active split, terminal hardware cursor provides
                    // visual indication, so we can still show selection background.
                    // Only exclude secondary cursors from selection (they use REVERSED styling).
                    // Bug #614: Previously excluded all cursor positions, causing first char
                    // of selection to display with wrong background for bar/underline cursors.
                    let is_primary_cursor = is_cursor && byte_pos == Some(primary_cursor_position);
                    let exclude_from_selection = is_cursor && !(is_active && is_primary_cursor);

                    let is_selected = !exclude_from_selection
                        && (byte_pos.is_some_and(|bp| {
                            selection_ranges.iter().any(|range| range.contains(&bp))
                        }) || is_in_block_selection);

                    // Compute character style using helper function
                    // char_styles is indexed by character position, not visual column
                    let token_style = line_char_styles
                        .get(display_char_idx)
                        .and_then(|s| s.as_ref());
                    let CharStyleOutput {
                        style,
                        is_secondary_cursor,
                    } = compute_char_style(&CharStyleContext {
                        byte_pos,
                        token_style,
                        ansi_style,
                        is_cursor,
                        is_selected,
                        theme,
                        highlight_spans,
                        semantic_token_spans,
                        viewport_overlays,
                        primary_cursor_position,
                        is_active,
                        session_mode,
                    });

                    // Determine display character (tabs already expanded in ViewLineIterator)
                    // Show tab indicator (→) at the start of tab expansions (if enabled for this language)
                    let tab_indicator: String;
                    let display_char: &str = if is_cursor && lsp_waiting && is_active {
                        "⋯"
                    } else if debug_tracker.is_some() && ch == '\r' {
                        // Debug mode: show CR explicitly
                        "\\r"
                    } else if debug_tracker.is_some() && ch == '\n' {
                        // Debug mode: show LF explicitly
                        "\\n"
                    } else if ch == '\n' {
                        ""
                    } else if is_tab_start && state.buffer_settings.show_whitespace_tabs {
                        // Visual indicator for tab: show → at the first position
                        tab_indicator = "→".to_string();
                        &tab_indicator
                    } else {
                        tab_indicator = ch.to_string();
                        &tab_indicator
                    };

                    if let Some(bp) = byte_pos {
                        if let Some(vtexts) = virtual_text_lookup.get(&bp) {
                            for vtext in vtexts
                                .iter()
                                .filter(|v| v.position == VirtualTextPosition::BeforeChar)
                            {
                                // Flush accumulated text before inserting virtual text
                                span_acc.flush(&mut line_spans, &mut line_view_map);
                                // Add extra space if at end of line (before newline)
                                let extra_space = if ch == '\n' { " " } else { "" };
                                let text_with_space = format!("{}{} ", extra_space, vtext.text);
                                push_span_with_map(
                                    &mut line_spans,
                                    &mut line_view_map,
                                    text_with_space,
                                    vtext.style,
                                    None,
                                );
                            }
                        }
                    }

                    if !display_char.is_empty() {
                        // Debug mode: insert opening tags for spans starting at this position
                        if let Some(ref mut tracker) = debug_tracker {
                            // Flush before debug tags
                            span_acc.flush(&mut line_spans, &mut line_view_map);
                            let opening_tags = tracker.get_opening_tags(
                                byte_pos,
                                highlight_spans,
                                viewport_overlays,
                            );
                            for tag in opening_tags {
                                push_debug_tag(&mut line_spans, &mut line_view_map, tag);
                            }
                        }

                        // Debug mode: show byte position before each character
                        if debug_tracker.is_some() {
                            if let Some(bp) = byte_pos {
                                push_debug_tag(
                                    &mut line_spans,
                                    &mut line_view_map,
                                    format!("[{}]", bp),
                                );
                            }
                        }

                        // Use accumulator to merge consecutive chars with same style
                        // This is critical for combining characters (Thai diacritics, etc.)
                        for c in display_char.chars() {
                            span_acc.push(c, style, byte_pos, &mut line_spans, &mut line_view_map);
                        }

                        // Debug mode: insert closing tags for spans ending at this position
                        // Check using the NEXT byte position to see if we're leaving a span
                        if let Some(ref mut tracker) = debug_tracker {
                            // Flush before debug tags
                            span_acc.flush(&mut line_spans, &mut line_view_map);
                            // Look ahead to next byte position to determine closing tags
                            let next_byte_pos = byte_pos.map(|bp| bp + ch.len_utf8());
                            let closing_tags = tracker.get_closing_tags(next_byte_pos);
                            for tag in closing_tags {
                                push_debug_tag(&mut line_spans, &mut line_view_map, tag);
                            }
                        }
                    }

                    // Track cursor position for zero-width characters
                    // Zero-width chars don't get map entries, so we need to explicitly record cursor pos
                    if !have_cursor {
                        if let Some(bp) = byte_pos {
                            if bp == primary_cursor_position && char_width(ch) == 0 {
                                // Account for horizontal scrolling by subtracting left_col
                                cursor_screen_x = gutter_width as u16
                                    + col_offset.saturating_sub(left_col) as u16;
                                cursor_screen_y = lines.len() as u16;
                                have_cursor = true;
                            }
                        }
                    }

                    if let Some(bp) = byte_pos {
                        if let Some(vtexts) = virtual_text_lookup.get(&bp) {
                            for vtext in vtexts
                                .iter()
                                .filter(|v| v.position == VirtualTextPosition::AfterChar)
                            {
                                let text_with_space = format!(" {}", vtext.text);
                                push_span_with_map(
                                    &mut line_spans,
                                    &mut line_view_map,
                                    text_with_space,
                                    vtext.style,
                                    None,
                                );
                            }
                        }
                    }

                    if is_cursor && ch == '\n' {
                        let should_add_indicator =
                            if is_active { is_secondary_cursor } else { true };
                        if should_add_indicator {
                            // Flush accumulated text before adding cursor indicator
                            // so the indicator appears after the line content, not before
                            span_acc.flush(&mut line_spans, &mut line_view_map);
                            let cursor_style = if is_active {
                                Style::default()
                                    .fg(theme.editor_fg)
                                    .bg(theme.editor_bg)
                                    .add_modifier(Modifier::REVERSED)
                            } else {
                                Style::default()
                                    .fg(theme.editor_fg)
                                    .bg(theme.inactive_cursor)
                            };
                            push_span_with_map(
                                &mut line_spans,
                                &mut line_view_map,
                                " ".to_string(),
                                cursor_style,
                                byte_pos,
                            );
                        }
                    }
                }

                byte_index += ch.len_utf8();
                display_char_idx += 1; // Increment character index for next lookup
                                       // col_offset tracks visual column position (for indexing into visual_to_char)
                                       // visual_to_char has one entry per visual column, not per character
                let ch_width = char_width(ch);
                col_offset += ch_width;
                visible_char_count += ch_width;
            }

            // Flush any remaining accumulated text at end of line
            span_acc.flush(&mut line_spans, &mut line_view_map);

            // Set last_seg_y early so cursor detection works for both empty and non-empty lines
            // For lines without wrapping, this will be the final y position
            // Also set for empty content lines (regardless of line_wrap) so cursor at EOF can be positioned
            let content_is_empty = line_content.is_empty();
            if line_spans.is_empty() || !line_wrap || content_is_empty {
                last_seg_y = Some(lines.len() as u16);
            }

            if !line_has_newline {
                let line_len_chars = line_content.chars().count();

                // Map view positions to buffer positions using per-line char_source_bytes
                let last_char_idx = line_len_chars.saturating_sub(1);
                let after_last_char_idx = line_len_chars;

                let last_char_buf_pos =
                    line_char_source_bytes.get(last_char_idx).copied().flatten();
                let after_last_char_buf_pos = line_char_source_bytes
                    .get(after_last_char_idx)
                    .copied()
                    .flatten();

                let cursor_at_end = cursor_positions.iter().any(|&pos| {
                    // Cursor is "at end" only if it's AFTER the last character, not ON it.
                    // A cursor ON the last character should render on that character (handled in main loop).
                    let matches_after = after_last_char_buf_pos.is_some_and(|bp| pos == bp);
                    // Fallback: when there's no mapping after last char (EOF), check if cursor is after last char
                    // The fallback should match the position that would be "after" if there was a mapping
                    let expected_after_pos = last_char_buf_pos.map(|p| p + 1).unwrap_or(0);
                    let matches_fallback =
                        after_last_char_buf_pos.is_none() && pos == expected_after_pos;

                    matches_after || matches_fallback
                });

                if cursor_at_end {
                    // Primary cursor is at end only if AFTER the last char, not ON it
                    let is_primary_at_end = after_last_char_buf_pos
                        .is_some_and(|bp| bp == primary_cursor_position)
                        || (after_last_char_buf_pos.is_none()
                            && primary_cursor_position >= state.buffer.len());

                    // Track cursor position for primary cursor
                    if let Some(seg_y) = last_seg_y {
                        if is_primary_at_end {
                            // Cursor position now includes gutter width (consistent with main cursor tracking)
                            // For empty lines, cursor is at gutter width (right after gutter)
                            // For non-empty lines without newline, cursor is after the last visible character
                            // Account for horizontal scrolling by using col_offset - left_col
                            cursor_screen_x = if line_len_chars == 0 {
                                gutter_width as u16
                            } else {
                                // col_offset is the visual column after the last character
                                // Subtract left_col to get the screen position after horizontal scroll
                                gutter_width as u16 + col_offset.saturating_sub(left_col) as u16
                            };
                            cursor_screen_y = seg_y;
                            have_cursor = true;
                        }
                    }

                    let should_add_indicator = if is_active { !is_primary_at_end } else { true };
                    if should_add_indicator {
                        let cursor_style = if is_active {
                            Style::default()
                                .fg(theme.editor_fg)
                                .bg(theme.editor_bg)
                                .add_modifier(Modifier::REVERSED)
                        } else {
                            Style::default()
                                .fg(theme.editor_fg)
                                .bg(theme.inactive_cursor)
                        };
                        push_span_with_map(
                            &mut line_spans,
                            &mut line_view_map,
                            " ".to_string(),
                            cursor_style,
                            None,
                        );
                    }
                }
            }

            // ViewLines are already wrapped (Break tokens became newlines in ViewLineIterator)
            // so each line is one visual line - no need to wrap again
            let current_y = lines.len() as u16;
            last_seg_y = Some(current_y);

            if !line_spans.is_empty() {
                // Find cursor position and track last visible x by iterating through line_view_map
                // Note: line_view_map includes both gutter and content character mappings
                //
                // When the cursor byte falls inside a concealed range (e.g. syntax markers
                // hidden by compose-mode plugins), no view_map entry will exactly match
                // primary_cursor_position.  In that case we fall back to the nearest
                // visible byte that is >= the cursor byte on the same line — this keeps
                // the cursor visible for the one frame between cursor movement and the
                // plugin's conceal-refresh response.
                let mut nearest_fallback: Option<(u16, usize)> = None; // (screen_x, byte_distance)
                for (screen_x, source_offset) in line_view_map.iter().enumerate() {
                    if let Some(src) = source_offset {
                        // Exact match: cursor byte is visible
                        if *src == primary_cursor_position && !have_cursor {
                            cursor_screen_x = screen_x as u16;
                            cursor_screen_y = current_y;
                            have_cursor = true;
                        }
                        // Track nearest visible byte >= cursor position for fallback
                        if !have_cursor && *src >= primary_cursor_position {
                            let dist = *src - primary_cursor_position;
                            if nearest_fallback.is_none() || dist < nearest_fallback.unwrap().1 {
                                nearest_fallback = Some((screen_x as u16, dist));
                            }
                        }
                        last_visible_x = screen_x as u16;
                    }
                }
                // Fallback: cursor byte was concealed — snap to nearest visible byte
                if !have_cursor {
                    if let Some((fallback_x, _)) = nearest_fallback {
                        cursor_screen_x = fallback_x;
                        cursor_screen_y = current_y;
                        have_cursor = true;
                    }
                }
            }

            // Fill remaining width for overlays with extend_to_line_end
            // Only when line wrapping is disabled (side-by-side diff typically disables wrapping)
            if !line_wrap {
                // Calculate the content area width (total width minus gutter)
                let content_width = render_area.width.saturating_sub(gutter_width as u16) as usize;
                let remaining_cols = content_width.saturating_sub(visible_char_count);

                if remaining_cols > 0 {
                    // Find the highest priority background color from overlays with extend_to_line_end
                    // that overlap with this line's byte range
                    let fill_style: Option<Style> = if let (Some(start), Some(end)) =
                        (first_line_byte_pos, last_line_byte_pos)
                    {
                        viewport_overlays
                            .iter()
                            .filter(|(overlay, range)| {
                                overlay.extend_to_line_end
                                    && range.start <= end
                                    && range.end >= start
                            })
                            .max_by_key(|(o, _)| o.priority)
                            .and_then(|(overlay, _)| {
                                match &overlay.face {
                                    crate::view::overlay::OverlayFace::Background { color } => {
                                        // Set both fg and bg to ensure ANSI codes are output
                                        Some(Style::default().fg(*color).bg(*color))
                                    }
                                    crate::view::overlay::OverlayFace::Style { style } => {
                                        // Extract background from style if present
                                        // Set fg to same as bg for invisible text
                                        style.bg.map(|bg| Style::default().fg(bg).bg(bg))
                                    }
                                    crate::view::overlay::OverlayFace::ThemedStyle {
                                        fallback_style,
                                        bg_theme,
                                        ..
                                    } => {
                                        // Try theme key first, fall back to style's bg
                                        let bg = bg_theme
                                            .as_ref()
                                            .and_then(|key| theme.resolve_theme_key(key))
                                            .or(fallback_style.bg);
                                        bg.map(|bg| Style::default().fg(bg).bg(bg))
                                    }
                                    _ => None,
                                }
                            })
                    } else {
                        None
                    };

                    if let Some(fill_bg) = fill_style {
                        let fill_text = " ".repeat(remaining_cols);
                        push_span_with_map(
                            &mut line_spans,
                            &mut line_view_map,
                            fill_text,
                            fill_bg,
                            None,
                        );
                    }
                }
            }

            // For virtual rows (no source bytes), inherit from previous row
            let prev_line_end_byte = view_line_mappings
                .last()
                .map(|prev: &ViewLineMapping| prev.line_end_byte)
                .unwrap_or(0);

            // Calculate line_end_byte for this line
            let line_end_byte = if current_view_line.ends_with_newline {
                // Position ON the newline - find the last source byte (the newline's position)
                current_view_line
                    .char_source_bytes
                    .iter()
                    .rev()
                    .find_map(|m| *m)
                    .unwrap_or(prev_line_end_byte)
            } else {
                // Position AFTER the last character - find last source byte and add char length
                if let Some((char_idx, &Some(last_byte_start))) = current_view_line
                    .char_source_bytes
                    .iter()
                    .enumerate()
                    .rev()
                    .find(|(_, m)| m.is_some())
                {
                    // Get the character at this index to find its UTF-8 byte length
                    if let Some(last_char) = current_view_line.text.chars().nth(char_idx) {
                        last_byte_start + last_char.len_utf8()
                    } else {
                        last_byte_start
                    }
                } else {
                    // Virtual row with no source bytes (e.g. table border from conceals).
                    // Inherit line_end_byte from the previous row so cursor movement
                    // through virtual rows lands at a valid source position.
                    prev_line_end_byte
                }
            };

            // Capture accurate view line mapping for mouse clicks
            // Content mapping starts after the gutter
            let content_map = if line_view_map.len() >= gutter_width {
                line_view_map[gutter_width..].to_vec()
            } else {
                Vec::new()
            };
            view_line_mappings.push(ViewLineMapping {
                char_source_bytes: content_map.clone(),
                visual_to_char: (0..content_map.len()).collect(),
                line_end_byte,
            });

            // Track if line was empty before moving line_spans
            let line_was_empty = line_spans.is_empty();
            lines.push(Line::from(line_spans));

            // Update last_line_end and check for cursor on newline BEFORE the break check
            // This ensures the last visible line's metadata is captured
            if let Some(y) = last_seg_y {
                // end_x is the cursor position after the last visible character.
                // For empty lines, last_visible_x stays at 0, so we need to ensure end_x is
                // at least gutter_width to place the cursor after the gutter, not in it.
                let end_x = if line_was_empty {
                    gutter_width as u16
                } else {
                    last_visible_x.saturating_add(1)
                };
                let line_len_chars = line_content.chars().count();

                last_line_end = Some(LastLineEnd {
                    pos: (end_x, y),
                    terminated_with_newline: line_has_newline,
                });

                if line_has_newline && line_len_chars > 0 {
                    let newline_idx = line_len_chars.saturating_sub(1);
                    if let Some(Some(src_newline)) = line_char_source_bytes.get(newline_idx) {
                        if *src_newline == primary_cursor_position {
                            // Cursor position now includes gutter width (consistent with main cursor tracking)
                            // For empty lines (just newline), cursor should be at gutter width (after gutter)
                            // For lines with content, cursor on newline should be after the content
                            if line_len_chars == 1 {
                                // Empty line - just the newline character
                                cursor_screen_x = gutter_width as u16;
                                cursor_screen_y = y;
                            } else {
                                // Line has content before the newline - cursor after last char
                                // end_x already includes gutter (from last_visible_x)
                                cursor_screen_x = end_x;
                                cursor_screen_y = y;
                            }
                            have_cursor = true;
                        }
                    }
                }
            }

            if lines_rendered >= visible_line_count {
                break;
            }
        }

        // If the last line ended with a newline, render an implicit empty line after it.
        // This shows the line number for the cursor position after the final newline.
        if let Some(ref end) = last_line_end {
            if end.terminated_with_newline && lines_rendered < visible_line_count {
                // Render the implicit line after the newline
                let mut implicit_line_spans = Vec::new();
                let implicit_line_num = current_source_line_num + 1;

                if state.margins.left_config.enabled {
                    // Indicator column: check for diagnostic markers on this implicit line
                    if decorations.diagnostic_lines.contains(&implicit_line_num) {
                        implicit_line_spans.push(Span::styled(
                            "●",
                            Style::default().fg(ratatui::style::Color::Red),
                        ));
                    } else {
                        implicit_line_spans.push(Span::styled(" ", Style::default()));
                    }

                    // Line number
                    let estimated_lines = (state.buffer.len() / 80).max(1);
                    let margin_content = state.margins.render_line(
                        implicit_line_num,
                        crate::view::margin::MarginPosition::Left,
                        estimated_lines,
                    );
                    let (rendered_text, style_opt) =
                        margin_content.render(state.margins.left_config.width);
                    let margin_style =
                        style_opt.unwrap_or_else(|| Style::default().fg(theme.line_number_fg));
                    implicit_line_spans.push(Span::styled(rendered_text, margin_style));

                    // Separator
                    if state.margins.left_config.show_separator {
                        implicit_line_spans.push(Span::styled(
                            state.margins.left_config.separator.to_string(),
                            Style::default().fg(theme.line_number_fg),
                        ));
                    }
                }

                let implicit_y = lines.len() as u16;
                lines.push(Line::from(implicit_line_spans));
                lines_rendered += 1;

                // Add mapping for implicit line
                // It has no content, so map is empty (gutter is handled by offset in screen_to_buffer_position)
                let buffer_len = state.buffer.len();

                view_line_mappings.push(ViewLineMapping {
                    char_source_bytes: Vec::new(),
                    visual_to_char: Vec::new(),
                    line_end_byte: buffer_len,
                });

                // NOTE: We intentionally do NOT update last_line_end here.
                // The implicit empty line is a visual display aid, not an actual content line.
                // last_line_end should track the last actual content line for cursor placement logic.

                // If primary cursor is at EOF (after the newline), set cursor on this line
                if primary_cursor_position == state.buffer.len() && !have_cursor {
                    cursor_screen_x = gutter_width as u16;
                    cursor_screen_y = implicit_y;
                    have_cursor = true;
                }
            }
        }

        // Fill remaining rows with tilde characters to indicate EOF (like vim/neovim).
        // This also ensures proper clearing in differential rendering because tildes
        // are guaranteed to differ from previous content, forcing ratatui to update.
        // See: https://github.com/ratatui/ratatui/issues/1606
        //
        // NOTE: We use a computed darker color instead of Modifier::DIM because the DIM
        // modifier can bleed through to overlays (like menus) rendered on top of these
        // lines due to how terminal escape sequences are output.
        // See: https://github.com/sinelaw/fresh/issues/458
        let eof_fg = dim_color_for_tilde(theme.line_number_fg);
        let eof_style = Style::default().fg(eof_fg);
        while lines.len() < render_area.height as usize {
            // Show tilde with dim styling, padded with spaces to fill the line
            let tilde_line = format!(
                "~{}",
                " ".repeat(render_area.width.saturating_sub(1) as usize)
            );
            lines.push(Line::styled(tilde_line, eof_style));
        }

        LineRenderOutput {
            lines,
            cursor: have_cursor.then_some((cursor_screen_x, cursor_screen_y)),
            last_line_end,
            content_lines_rendered: lines_rendered,
            view_line_mappings,
        }
    }

    fn resolve_cursor_fallback(
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
                // Cursor should appear on the implicit empty line after the newline
                // Include gutter width in x coordinate
                return Some((gutter_width as u16, end.pos.1.saturating_add(1)));
            }
            return Some((gutter_width as u16, lines_rendered as u16));
        }

        last_line_end.map(|end| end.pos)
    }

    /// Render a single buffer in a split pane
    /// Returns the view line mappings for mouse click handling
    #[allow(clippy::too_many_arguments)]
    fn render_buffer_in_split(
        frame: &mut Frame,
        state: &mut EditorState,
        cursors: &crate::model::cursor::Cursors,
        viewport: &mut crate::view::viewport::Viewport,
        event_log: Option<&mut EventLog>,
        area: Rect,
        is_active: bool,
        theme: &crate::view::theme::Theme,
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
        rulers: &[usize],
        show_line_numbers_override: Option<bool>,
    ) -> Vec<ViewLineMapping> {
        let _span = tracing::trace_span!("render_buffer_in_split").entered();

        // Apply per-split line number setting before rendering.
        // Because margins live on shared EditorState, we must always set
        // the value here so that a previous split's render (e.g. compose mode
        // hiding line numbers) doesn't leak into this split's render.
        // When no per-split override exists, default to showing line numbers.
        let show_line_numbers = show_line_numbers_override.unwrap_or(true);
        state.margins.set_line_numbers(show_line_numbers);

        // Compute effective editor background: terminal default or theme-defined
        let effective_editor_bg = if use_terminal_bg {
            ratatui::style::Color::Reset
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
        let estimated_lines = (buffer_len / 80).max(1);
        state.margins.update_width_for_buffer(estimated_lines);
        let gutter_width = state.margins.left_total_width();

        let compose_layout = Self::calculate_compose_layout(area, &view_mode, compose_width);
        let render_area = compose_layout.render_area;

        // Clone view_transform so we can reuse it if scrolling triggers a rebuild
        let view_transform_for_rebuild = view_transform.clone();

        let view_data = Self::build_view_data(
            state,
            viewport,
            view_transform,
            estimated_line_length,
            visible_count,
            line_wrap,
            render_area.width as usize,
            gutter_width,
            &view_mode,
        );

        // Ensure cursor is visible using Layout-aware check (handles virtual lines)
        // This detects when cursor is beyond the rendered view_lines and scrolls
        let primary = *cursors.primary();
        let scrolled = viewport.ensure_visible_in_layout(&view_data.lines, &primary, gutter_width);

        // If we scrolled, rebuild view_data from the new top_byte and then re-run the
        // layout-aware check so that top_view_line_offset is correct for the rebuilt data.
        // Without this, top_view_line_offset would be stale (relative to the old view_data),
        // causing gutter line numbers to desync from content.
        let view_data = if scrolled {
            // Reset offset before rebuild — it will be set correctly by the second
            // ensure_visible_in_layout call below.
            viewport.top_view_line_offset = 0;
            let rebuilt = Self::build_view_data(
                state,
                viewport,
                view_transform_for_rebuild,
                estimated_line_length,
                visible_count,
                line_wrap,
                render_area.width as usize,
                gutter_width,
                &view_mode,
            );
            // Re-run layout-aware cursor check on the rebuilt data so top_view_line_offset
            // correctly indexes the new view_lines (handles both source lines and virtual lines).
            viewport.ensure_visible_in_layout(&rebuilt.lines, &primary, gutter_width);
            rebuilt
        } else {
            view_data
        };

        let view_anchor = Self::calculate_view_anchor(&view_data.lines, viewport.top_byte);
        Self::render_compose_margins(
            frame,
            area,
            &compose_layout,
            &view_mode,
            theme,
            effective_editor_bg,
        );

        let selection = Self::selection_context(state, cursors);

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

        let starting_line_num = state
            .buffer
            .populate_line_cache(viewport.top_byte, visible_count);

        let viewport_start = viewport.top_byte;
        let viewport_end = Self::calculate_viewport_end(
            state,
            viewport_start,
            estimated_line_length,
            visible_count,
        );

        let decorations = Self::decoration_context(
            state,
            viewport_start,
            viewport_end,
            selection.primary_cursor_position,
            theme,
            highlight_context_bytes,
            &view_mode,
        );

        // Use top_view_line_offset to handle scrolling through virtual lines.
        // The viewport code (ensure_visible_in_layout) updates this when scrolling
        // to keep the cursor visible, including special handling for virtual lines.
        //
        // We recalculate starting_line_num below to ensure line numbers stay in sync
        // even if view_data was rebuilt from a different starting position.
        let calculated_offset = viewport.top_view_line_offset;

        tracing::trace!(
            top_byte = viewport.top_byte,
            top_view_line_offset = viewport.top_view_line_offset,
            calculated_offset,
            view_data_lines = view_data.lines.len(),
            "view line offset calculation"
        );
        let (view_lines_to_render, adjusted_starting_line_num, adjusted_view_anchor) =
            if calculated_offset > 0 && calculated_offset < view_data.lines.len() {
                let sliced = &view_data.lines[calculated_offset..];

                // Count how many source lines were in the skipped portion
                // A view line is a "source line" if it shows a line number (not a continuation)
                let skipped_lines = &view_data.lines[..calculated_offset];
                let skipped_source_lines = skipped_lines
                    .iter()
                    .filter(|vl| should_show_line_number(vl))
                    .count();

                let adjusted_line_num = starting_line_num + skipped_source_lines;

                // Recalculate view_anchor on the sliced array
                let adjusted_anchor = Self::calculate_view_anchor(sliced, viewport.top_byte);

                (sliced, adjusted_line_num, adjusted_anchor)
            } else {
                (&view_data.lines[..], starting_line_num, view_anchor)
            };

        let render_output = Self::render_view_lines(LineRenderInput {
            state,
            theme,
            view_lines: view_lines_to_render,
            view_anchor: adjusted_view_anchor,
            render_area,
            gutter_width,
            selection: &selection,
            decorations: &decorations,
            starting_line_num: adjusted_starting_line_num,
            visible_line_count: visible_count,
            lsp_waiting,
            is_active,
            line_wrap,
            estimated_lines,
            left_column: viewport.left_column,
            relative_line_numbers,
            session_mode,
        });

        let mut lines = render_output.lines;
        let background_x_offset = viewport.left_column;

        if let Some(bg) = ansi_background {
            Self::apply_background_to_lines(
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

        // Resolve cursor screen position early so we can avoid clobbering it
        // with OSC 8 2-char chunks.
        let buffer_ends_with_newline = if !state.buffer.is_empty() {
            let last_char = state.get_text_range(state.buffer.len() - 1, state.buffer.len());
            last_char == "\n"
        } else {
            false
        };

        let cursor = Self::resolve_cursor_fallback(
            render_output.cursor,
            selection.primary_cursor_position,
            state.buffer.len(),
            buffer_ends_with_newline,
            render_output.last_line_end,
            render_output.content_lines_rendered,
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

        // TODO: Re-enable OSC 8 hyperlink overlays once we fix the ratatui
        // Buffer::diff interaction. The 2-char chunking writes OSC 8 escape
        // sequences into cell symbols, but ratatui's diff uses
        // `symbol().width()` (unicode_width) to compute how many subsequent
        // cells to skip. Because the URL inside the OSC 8 string contains
        // printable characters, the width is ~30 instead of 2, causing the
        // diff to skip legitimate cell updates. This leads to stale cells in
        // the backend buffer when the overlay column range shifts (e.g.
        // concealed → unconcealed transitions). The fix is to apply OSC 8
        // after the ratatui diff, directly to the backend buffer.
        //
        // Self::apply_hyperlink_overlays(
        //     frame,
        //     &decorations.viewport_overlays,
        //     &render_output.view_line_mappings,
        //     render_area,
        //     gutter_width,
        //     cursor_screen_pos,
        // );

        // Render config-based vertical rulers (as background color tint)
        if !rulers.is_empty() {
            let ruler_cols: Vec<u16> = rulers.iter().map(|&r| r as u16).collect();
            Self::render_ruler_bg(
                frame,
                &ruler_cols,
                theme.ruler_bg,
                render_area,
                gutter_width,
                render_output.content_lines_rendered,
                viewport.left_column,
            );
        }

        // Render compose column guides (for tables, etc.)
        if let Some(guides) = compose_column_guides {
            let guide_style = Style::default()
                .fg(theme.line_number_fg)
                .add_modifier(Modifier::DIM);
            Self::render_column_guides(
                frame,
                &guides,
                guide_style,
                render_area,
                gutter_width,
                render_output.content_lines_rendered,
                0, // compose guides are already relative to content
            );
        }

        if let Some((screen_x, screen_y)) = cursor_screen_pos {
            frame.set_cursor_position((screen_x, screen_y));

            if let Some(event_log) = event_log {
                let cursor_pos = cursors.primary().position;
                let buffer_len = state.buffer.len();
                event_log.log_render_state(cursor_pos, screen_x, screen_y, buffer_len);
            }
        }

        // Extract view line mappings for mouse click handling
        // This maps screen coordinates to buffer byte positions
        render_output.view_line_mappings
    }

    /// Render vertical column guide lines in the editor content area.
    /// Used for both config-based vertical rulers and compose-mode column guides.
    fn render_column_guides(
        frame: &mut Frame,
        columns: &[u16],
        style: Style,
        render_area: Rect,
        gutter_width: usize,
        content_height: usize,
        left_column: usize,
    ) {
        let guide_height = content_height.min(render_area.height as usize);
        for &col in columns {
            // Account for horizontal scroll
            let Some(scrolled_col) = (col as usize).checked_sub(left_column) else {
                continue;
            };
            let guide_x = render_area.x + gutter_width as u16 + scrolled_col as u16;
            if guide_x < render_area.x + render_area.width {
                for row in 0..guide_height {
                    let cell = &mut frame.buffer_mut()[(guide_x, render_area.y + row as u16)];
                    cell.set_symbol("│");
                    if let Some(fg) = style.fg {
                        cell.set_fg(fg);
                    }
                    if !style.add_modifier.is_empty() {
                        cell.set_style(Style::default().add_modifier(style.add_modifier));
                    }
                }
            }
        }
    }

    /// Render vertical rulers as a subtle background color tint.
    /// Unlike `render_column_guides` which draws │ characters (for compose guides),
    /// this preserves the existing text content and only adjusts the background color.
    fn render_ruler_bg(
        frame: &mut Frame,
        columns: &[u16],
        color: Color,
        render_area: Rect,
        gutter_width: usize,
        content_height: usize,
        left_column: usize,
    ) {
        let guide_height = content_height.min(render_area.height as usize);
        for &col in columns {
            let Some(scrolled_col) = (col as usize).checked_sub(left_column) else {
                continue;
            };
            let guide_x = render_area.x + gutter_width as u16 + scrolled_col as u16;
            if guide_x < render_area.x + render_area.width {
                for row in 0..guide_height {
                    let cell = &mut frame.buffer_mut()[(guide_x, render_area.y + row as u16)];
                    cell.set_bg(color);
                }
            }
        }
    }

    /// Post-process the rendered frame to apply OSC 8 hyperlink escape sequences
    /// for any overlays that have a URL set.
    ///
    /// Uses view_line_mappings to translate overlay byte ranges into screen
    /// positions, then wraps the corresponding cells with OSC 8 sequences so
    /// they become clickable in terminals that support the protocol.
    fn apply_hyperlink_overlays(
        frame: &mut Frame,
        viewport_overlays: &[(crate::view::overlay::Overlay, Range<usize>)],
        view_line_mappings: &[ViewLineMapping],
        render_area: Rect,
        gutter_width: usize,
        cursor_screen_pos: Option<(u16, u16)>,
    ) {
        let hyperlink_overlays: Vec<_> = viewport_overlays
            .iter()
            .filter(|(overlay, _)| overlay.url.is_some())
            .collect();

        if hyperlink_overlays.is_empty() {
            return;
        }

        let buf = frame.buffer_mut();
        for (screen_row, mapping) in view_line_mappings.iter().enumerate() {
            let y = render_area.y + screen_row as u16;
            if y >= render_area.y + render_area.height {
                break;
            }
            for (overlay, range) in &hyperlink_overlays {
                let url = overlay.url.as_ref().unwrap();
                // Find screen columns in this row whose source byte falls in range
                let mut run_start: Option<u16> = None;
                let content_x_offset = render_area.x + gutter_width as u16;
                for (char_idx, maybe_byte) in mapping.char_source_bytes.iter().enumerate() {
                    let in_range = maybe_byte
                        .map(|b| b >= range.start && b < range.end)
                        .unwrap_or(false);
                    let screen_x = content_x_offset + char_idx as u16;
                    if in_range && screen_x < render_area.x + render_area.width {
                        if run_start.is_none() {
                            run_start = Some(screen_x);
                        }
                    } else if let Some(start_x) = run_start.take() {
                        Self::apply_osc8_to_cells(
                            buf,
                            start_x,
                            screen_x,
                            y,
                            url,
                            cursor_screen_pos,
                        );
                    }
                }
                // Flush trailing run
                if let Some(start_x) = run_start {
                    let end_x = content_x_offset + mapping.char_source_bytes.len() as u16;
                    let end_x = end_x.min(render_area.x + render_area.width);
                    Self::apply_osc8_to_cells(buf, start_x, end_x, y, url, cursor_screen_pos);
                }
            }
        }
    }

    /// Apply OSC 8 hyperlink escape sequences to a run of buffer cells.
    ///
    /// Uses 2-character chunking to work around Crossterm width accounting
    /// issues with OSC sequences (same approach as popup hyperlinks).
    /// When the cursor falls on the second character of a 2-char chunk, the
    /// chunk is split into two 1-char chunks so the terminal cursor remains
    /// visible on the correct cell.
    fn apply_osc8_to_cells(
        buf: &mut ratatui::buffer::Buffer,
        start_x: u16,
        end_x: u16,
        y: u16,
        url: &str,
        cursor_pos: Option<(u16, u16)>,
    ) {
        let area = *buf.area();
        if y < area.y || y >= area.y + area.height {
            return;
        }
        let max_x = area.x + area.width;
        // If the cursor is on this row, note its x so we can avoid
        // swallowing it into the second half of a 2-char chunk.
        let cursor_x = cursor_pos.and_then(|(cx, cy)| if cy == y { Some(cx) } else { None });
        let mut x = start_x;
        while x < end_x {
            if x >= max_x {
                break;
            }
            // Determine chunk size: normally 2, but use 1 if the cursor
            // sits on the second cell of the pair so it stays addressable.
            let chunk_size = if cursor_x == Some(x + 1) { 1 } else { 2 };

            let mut chunk = String::new();
            let chunk_start = x;
            for _ in 0..chunk_size {
                if x >= end_x || x >= max_x {
                    break;
                }
                let sym = buf[(x, y)].symbol().to_string();
                chunk.push_str(&sym);
                x += 1;
            }
            if !chunk.is_empty() {
                let actual_chunk_len = x - chunk_start;
                let hyperlink = format!("\x1B]8;;{}\x07{}\x1B]8;;\x07", url, chunk);
                buf[(chunk_start, y)].set_symbol(&hyperlink);
                // Clear trailing cells that were folded into this chunk so
                // ratatui's diff picks up the change when chunking shifts.
                for cx in (chunk_start + 1)..chunk_start + actual_chunk_len {
                    buf[(cx, y)].set_symbol("");
                }
            }
        }
    }

    /// Apply styles from original line_spans to a wrapped segment
    ///
    /// Maps each character in the segment text back to its original span to preserve
    /// syntax highlighting, selections, and other styling across wrapped lines.
    ///
    /// # Arguments
    /// * `segment_text` - The text content of this wrapped segment
    /// * `line_spans` - The original styled spans for the entire line
    /// * `segment_start_offset` - Character offset where this segment starts in the original line
    /// * `scroll_offset` - Additional offset for horizontal scrolling (non-wrap mode)
    #[allow(clippy::too_many_arguments)]
    fn apply_background_to_lines(
        lines: &mut Vec<Line<'static>>,
        area_width: u16,
        background: &AnsiBackground,
        theme_bg: Color,
        default_fg: Color,
        fade: f32,
        x_offset: usize,
        y_offset: usize,
    ) {
        if area_width == 0 {
            return;
        }

        let width = area_width as usize;

        for (y, line) in lines.iter_mut().enumerate() {
            // Flatten existing spans into per-character styles
            let mut existing: Vec<(char, Style)> = Vec::new();
            let spans = std::mem::take(&mut line.spans);
            for span in spans {
                let style = span.style;
                for ch in span.content.chars() {
                    existing.push((ch, style));
                }
            }

            let mut chars_with_style = Vec::with_capacity(width);
            for x in 0..width {
                let sample_x = x_offset + x;
                let sample_y = y_offset + y;

                let (ch, mut style) = if x < existing.len() {
                    existing[x]
                } else {
                    (' ', Style::default().fg(default_fg))
                };

                if let Some(bg_color) = background.faded_color(sample_x, sample_y, theme_bg, fade) {
                    if style.bg.is_none() || matches!(style.bg, Some(Color::Reset)) {
                        style = style.bg(bg_color);
                    }
                }

                chars_with_style.push((ch, style));
            }

            line.spans = Self::compress_chars(chars_with_style);
        }
    }

    fn compress_chars(chars: Vec<(char, Style)>) -> Vec<Span<'static>> {
        if chars.is_empty() {
            return vec![];
        }

        let mut spans = Vec::new();
        let mut current_style = chars[0].1;
        let mut current_text = String::new();
        current_text.push(chars[0].0);

        for (ch, style) in chars.into_iter().skip(1) {
            if style == current_style {
                current_text.push(ch);
            } else {
                spans.push(Span::styled(current_text.clone(), current_style));
                current_text.clear();
                current_text.push(ch);
                current_style = style;
            }
        }

        spans.push(Span::styled(current_text, current_style));
        spans
    }
}

#[cfg(test)]
mod tests {
    use crate::model::filesystem::StdFileSystem;
    use std::sync::Arc;

    fn test_fs() -> Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> {
        Arc::new(StdFileSystem)
    }
    use super::*;
    use crate::model::buffer::Buffer;
    use crate::primitives::display_width::str_width;
    use crate::view::theme;
    use crate::view::theme::Theme;
    use crate::view::viewport::Viewport;

    fn render_output_for(
        content: &str,
        cursor_pos: usize,
    ) -> (LineRenderOutput, usize, bool, usize) {
        render_output_for_with_gutters(content, cursor_pos, false)
    }

    fn render_output_for_with_gutters(
        content: &str,
        cursor_pos: usize,
        gutters_enabled: bool,
    ) -> (LineRenderOutput, usize, bool, usize) {
        let mut state = EditorState::new(20, 6, 1024, test_fs());
        state.buffer = Buffer::from_str(content, 1024, test_fs());
        let mut cursors = crate::model::cursor::Cursors::new();
        cursors.primary_mut().position = cursor_pos.min(state.buffer.len());
        // Create a standalone viewport (no longer part of EditorState)
        let viewport = Viewport::new(20, 4);
        // Enable/disable line numbers/gutters based on parameter
        state.margins.left_config.enabled = gutters_enabled;

        let render_area = Rect::new(0, 0, 20, 4);
        let visible_count = viewport.visible_line_count();
        let gutter_width = state.margins.left_total_width();

        let view_data = SplitRenderer::build_view_data(
            &mut state,
            &viewport,
            None,
            content.len().max(1),
            visible_count,
            false, // line wrap disabled for tests
            render_area.width as usize,
            gutter_width,
            &ViewMode::Source, // Tests use source mode
        );
        let view_anchor = SplitRenderer::calculate_view_anchor(&view_data.lines, 0);

        let estimated_lines = (state.buffer.len() / 80).max(1);
        state.margins.update_width_for_buffer(estimated_lines);
        let gutter_width = state.margins.left_total_width();

        let selection = SplitRenderer::selection_context(&state, &cursors);
        let starting_line_num = state
            .buffer
            .populate_line_cache(viewport.top_byte, visible_count);
        let viewport_start = viewport.top_byte;
        let viewport_end = SplitRenderer::calculate_viewport_end(
            &mut state,
            viewport_start,
            content.len().max(1),
            visible_count,
        );
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let decorations = SplitRenderer::decoration_context(
            &mut state,
            viewport_start,
            viewport_end,
            selection.primary_cursor_position,
            &theme,
            100_000,           // default highlight context bytes
            &ViewMode::Source, // Tests use source mode
        );

        let output = SplitRenderer::render_view_lines(LineRenderInput {
            state: &state,
            theme: &theme,
            view_lines: &view_data.lines,
            view_anchor,
            render_area,
            gutter_width,
            selection: &selection,
            decorations: &decorations,
            starting_line_num,
            visible_line_count: visible_count,
            lsp_waiting: false,
            is_active: true,
            line_wrap: viewport.line_wrap_enabled,
            estimated_lines,
            left_column: viewport.left_column,
            relative_line_numbers: false,
            session_mode: false,
        });

        (
            output,
            state.buffer.len(),
            content.ends_with('\n'),
            selection.primary_cursor_position,
        )
    }

    #[test]
    fn last_line_end_tracks_trailing_newline() {
        let output = render_output_for("abc\n", 4);
        assert_eq!(
            output.0.last_line_end,
            Some(LastLineEnd {
                pos: (3, 0),
                terminated_with_newline: true
            })
        );
    }

    #[test]
    fn last_line_end_tracks_no_trailing_newline() {
        let output = render_output_for("abc", 3);
        assert_eq!(
            output.0.last_line_end,
            Some(LastLineEnd {
                pos: (3, 0),
                terminated_with_newline: false
            })
        );
    }

    #[test]
    fn cursor_after_newline_places_on_next_line() {
        let (output, buffer_len, buffer_newline, cursor_pos) = render_output_for("abc\n", 4);
        let cursor = SplitRenderer::resolve_cursor_fallback(
            output.cursor,
            cursor_pos,
            buffer_len,
            buffer_newline,
            output.last_line_end,
            output.content_lines_rendered,
            0, // gutter_width (gutters disabled in tests)
        );
        assert_eq!(cursor, Some((0, 1)));
    }

    #[test]
    fn cursor_at_end_without_newline_stays_on_line() {
        let (output, buffer_len, buffer_newline, cursor_pos) = render_output_for("abc", 3);
        let cursor = SplitRenderer::resolve_cursor_fallback(
            output.cursor,
            cursor_pos,
            buffer_len,
            buffer_newline,
            output.last_line_end,
            output.content_lines_rendered,
            0, // gutter_width (gutters disabled in tests)
        );
        assert_eq!(cursor, Some((3, 0)));
    }

    // Helper to count all cursor positions in rendered output
    // Cursors can appear as:
    // 1. Primary cursor in output.cursor (hardware cursor position)
    // 2. Visual spans with REVERSED modifier (secondary cursors, or primary cursor with contrast fix)
    // 3. Visual spans with special background color (inactive cursors)
    fn count_all_cursors(output: &LineRenderOutput) -> Vec<(u16, u16)> {
        let mut cursor_positions = Vec::new();

        // Check for primary cursor in output.cursor field
        let primary_cursor = output.cursor;
        if let Some(cursor_pos) = primary_cursor {
            cursor_positions.push(cursor_pos);
        }

        // Check for visual cursor indicators in rendered spans (secondary/inactive cursors)
        for (line_idx, line) in output.lines.iter().enumerate() {
            let mut col = 0u16;
            for span in line.spans.iter() {
                // Check if this span has the REVERSED modifier (secondary cursor)
                if span
                    .style
                    .add_modifier
                    .contains(ratatui::style::Modifier::REVERSED)
                {
                    let pos = (col, line_idx as u16);
                    // Only add if this is not the primary cursor position
                    // (primary cursor may also have REVERSED for contrast)
                    if primary_cursor != Some(pos) {
                        cursor_positions.push(pos);
                    }
                }
                // Count the visual width of this span's content
                col += str_width(&span.content) as u16;
            }
        }

        cursor_positions
    }

    // Helper to dump rendered output for debugging
    fn dump_render_output(content: &str, cursor_pos: usize, output: &LineRenderOutput) {
        eprintln!("\n=== RENDER DEBUG ===");
        eprintln!("Content: {:?}", content);
        eprintln!("Cursor position: {}", cursor_pos);
        eprintln!("Hardware cursor (output.cursor): {:?}", output.cursor);
        eprintln!("Last line end: {:?}", output.last_line_end);
        eprintln!("Content lines rendered: {}", output.content_lines_rendered);
        eprintln!("\nRendered lines:");
        for (line_idx, line) in output.lines.iter().enumerate() {
            eprintln!("  Line {}: {} spans", line_idx, line.spans.len());
            for (span_idx, span) in line.spans.iter().enumerate() {
                let has_reversed = span
                    .style
                    .add_modifier
                    .contains(ratatui::style::Modifier::REVERSED);
                let bg_color = format!("{:?}", span.style.bg);
                eprintln!(
                    "    Span {}: {:?} (REVERSED: {}, BG: {})",
                    span_idx, span.content, has_reversed, bg_color
                );
            }
        }
        eprintln!("===================\n");
    }

    // Helper to get final cursor position after fallback resolution
    // Also validates that exactly one cursor is present
    fn get_final_cursor(content: &str, cursor_pos: usize) -> Option<(u16, u16)> {
        let (output, buffer_len, buffer_newline, cursor_pos) =
            render_output_for(content, cursor_pos);

        // Count all cursors (hardware + visual) in the rendered output
        let all_cursors = count_all_cursors(&output);

        // Validate that at most one cursor is present in rendered output
        // (Some cursors are added by fallback logic, not during rendering)
        assert!(
            all_cursors.len() <= 1,
            "Expected at most 1 cursor in rendered output, found {} at positions: {:?}",
            all_cursors.len(),
            all_cursors
        );

        let final_cursor = SplitRenderer::resolve_cursor_fallback(
            output.cursor,
            cursor_pos,
            buffer_len,
            buffer_newline,
            output.last_line_end,
            output.content_lines_rendered,
            0, // gutter_width (gutters disabled in tests)
        );

        // Debug dump if we find unexpected results
        if all_cursors.len() > 1 || (all_cursors.len() == 1 && Some(all_cursors[0]) != final_cursor)
        {
            dump_render_output(content, cursor_pos, &output);
        }

        // If a cursor was rendered, it should match the final cursor position
        if let Some(rendered_cursor) = all_cursors.first() {
            assert_eq!(
                Some(*rendered_cursor),
                final_cursor,
                "Rendered cursor at {:?} doesn't match final cursor {:?}",
                rendered_cursor,
                final_cursor
            );
        }

        // Validate that we have a final cursor position (either rendered or from fallback)
        assert!(
            final_cursor.is_some(),
            "Expected a final cursor position, but got None. Rendered cursors: {:?}",
            all_cursors
        );

        final_cursor
    }

    // Helper to simulate typing a character and check if it appears at cursor position
    fn check_typing_at_cursor(
        content: &str,
        cursor_pos: usize,
        char_to_type: char,
    ) -> (Option<(u16, u16)>, String) {
        // Get cursor position before typing
        let cursor_before = get_final_cursor(content, cursor_pos);

        // Simulate inserting the character at cursor position
        let mut new_content = content.to_string();
        if cursor_pos <= content.len() {
            new_content.insert(cursor_pos, char_to_type);
        }

        (cursor_before, new_content)
    }

    #[test]
    fn e2e_cursor_at_start_of_nonempty_line() {
        // "abc" with cursor at position 0 (before 'a')
        let cursor = get_final_cursor("abc", 0);
        assert_eq!(cursor, Some((0, 0)), "Cursor should be at column 0, line 0");

        let (cursor_pos, new_content) = check_typing_at_cursor("abc", 0, 'X');
        assert_eq!(
            new_content, "Xabc",
            "Typing should insert at cursor position"
        );
        assert_eq!(cursor_pos, Some((0, 0)));
    }

    #[test]
    fn e2e_cursor_in_middle_of_line() {
        // "abc" with cursor at position 1 (on 'b')
        let cursor = get_final_cursor("abc", 1);
        assert_eq!(cursor, Some((1, 0)), "Cursor should be at column 1, line 0");

        let (cursor_pos, new_content) = check_typing_at_cursor("abc", 1, 'X');
        assert_eq!(
            new_content, "aXbc",
            "Typing should insert at cursor position"
        );
        assert_eq!(cursor_pos, Some((1, 0)));
    }

    #[test]
    fn e2e_cursor_at_end_of_line_no_newline() {
        // "abc" with cursor at position 3 (after 'c', at EOF)
        let cursor = get_final_cursor("abc", 3);
        assert_eq!(
            cursor,
            Some((3, 0)),
            "Cursor should be at column 3, line 0 (after last char)"
        );

        let (cursor_pos, new_content) = check_typing_at_cursor("abc", 3, 'X');
        assert_eq!(new_content, "abcX", "Typing should append at end");
        assert_eq!(cursor_pos, Some((3, 0)));
    }

    #[test]
    fn e2e_cursor_at_empty_line() {
        // "\n" with cursor at position 0 (on the newline itself)
        let cursor = get_final_cursor("\n", 0);
        assert_eq!(
            cursor,
            Some((0, 0)),
            "Cursor on empty line should be at column 0"
        );

        let (cursor_pos, new_content) = check_typing_at_cursor("\n", 0, 'X');
        assert_eq!(new_content, "X\n", "Typing should insert before newline");
        assert_eq!(cursor_pos, Some((0, 0)));
    }

    #[test]
    fn e2e_cursor_after_newline_at_eof() {
        // "abc\n" with cursor at position 4 (after newline, at EOF)
        let cursor = get_final_cursor("abc\n", 4);
        assert_eq!(
            cursor,
            Some((0, 1)),
            "Cursor after newline at EOF should be on next line"
        );

        let (cursor_pos, new_content) = check_typing_at_cursor("abc\n", 4, 'X');
        assert_eq!(new_content, "abc\nX", "Typing should insert on new line");
        assert_eq!(cursor_pos, Some((0, 1)));
    }

    #[test]
    fn e2e_cursor_on_newline_with_content() {
        // "abc\n" with cursor at position 3 (on the newline character)
        let cursor = get_final_cursor("abc\n", 3);
        assert_eq!(
            cursor,
            Some((3, 0)),
            "Cursor on newline after content should be after last char"
        );

        let (cursor_pos, new_content) = check_typing_at_cursor("abc\n", 3, 'X');
        assert_eq!(new_content, "abcX\n", "Typing should insert before newline");
        assert_eq!(cursor_pos, Some((3, 0)));
    }

    #[test]
    fn e2e_cursor_multiline_start_of_second_line() {
        // "abc\ndef" with cursor at position 4 (start of second line, on 'd')
        let cursor = get_final_cursor("abc\ndef", 4);
        assert_eq!(
            cursor,
            Some((0, 1)),
            "Cursor at start of second line should be at column 0, line 1"
        );

        let (cursor_pos, new_content) = check_typing_at_cursor("abc\ndef", 4, 'X');
        assert_eq!(
            new_content, "abc\nXdef",
            "Typing should insert at start of second line"
        );
        assert_eq!(cursor_pos, Some((0, 1)));
    }

    #[test]
    fn e2e_cursor_multiline_end_of_first_line() {
        // "abc\ndef" with cursor at position 3 (on newline of first line)
        let cursor = get_final_cursor("abc\ndef", 3);
        assert_eq!(
            cursor,
            Some((3, 0)),
            "Cursor on newline of first line should be after content"
        );

        let (cursor_pos, new_content) = check_typing_at_cursor("abc\ndef", 3, 'X');
        assert_eq!(
            new_content, "abcX\ndef",
            "Typing should insert before newline"
        );
        assert_eq!(cursor_pos, Some((3, 0)));
    }

    #[test]
    fn e2e_cursor_empty_buffer() {
        // Empty buffer with cursor at position 0
        let cursor = get_final_cursor("", 0);
        assert_eq!(
            cursor,
            Some((0, 0)),
            "Cursor in empty buffer should be at origin"
        );

        let (cursor_pos, new_content) = check_typing_at_cursor("", 0, 'X');
        assert_eq!(
            new_content, "X",
            "Typing in empty buffer should insert character"
        );
        assert_eq!(cursor_pos, Some((0, 0)));
    }

    #[test]
    fn e2e_cursor_empty_buffer_with_gutters() {
        // Empty buffer with cursor at position 0, with gutters enabled
        // The cursor should be positioned at the gutter width (right after the gutter),
        // NOT at column 0 (which would be in the gutter area)
        let (output, buffer_len, buffer_newline, cursor_pos) =
            render_output_for_with_gutters("", 0, true);

        // With gutters enabled, the gutter width should be > 0
        // Default gutter includes: 1 char indicator + line number width + separator
        // For a 1-line buffer, line number width is typically 1 digit + padding
        let gutter_width = {
            let mut state = EditorState::new(20, 6, 1024, test_fs());
            state.margins.left_config.enabled = true;
            state.margins.update_width_for_buffer(1);
            state.margins.left_total_width()
        };
        assert!(gutter_width > 0, "Gutter width should be > 0 when enabled");

        // CRITICAL: Check the RENDERED cursor position directly from output.cursor
        // This is what the terminal will actually use for cursor positioning
        // The cursor should be rendered at gutter_width, not at 0
        assert_eq!(
            output.cursor,
            Some((gutter_width as u16, 0)),
            "RENDERED cursor in empty buffer should be at gutter_width ({}), got {:?}",
            gutter_width,
            output.cursor
        );

        let final_cursor = SplitRenderer::resolve_cursor_fallback(
            output.cursor,
            cursor_pos,
            buffer_len,
            buffer_newline,
            output.last_line_end,
            output.content_lines_rendered,
            gutter_width,
        );

        // Cursor should be at (gutter_width, 0) - right after the gutter on line 0
        assert_eq!(
            final_cursor,
            Some((gutter_width as u16, 0)),
            "Cursor in empty buffer with gutters should be at gutter_width, not column 0"
        );
    }

    #[test]
    fn e2e_cursor_between_empty_lines() {
        // "\n\n" with cursor at position 1 (on second newline)
        let cursor = get_final_cursor("\n\n", 1);
        assert_eq!(cursor, Some((0, 1)), "Cursor on second empty line");

        let (cursor_pos, new_content) = check_typing_at_cursor("\n\n", 1, 'X');
        assert_eq!(new_content, "\nX\n", "Typing should insert on second line");
        assert_eq!(cursor_pos, Some((0, 1)));
    }

    #[test]
    fn e2e_cursor_at_eof_after_multiple_lines() {
        // "abc\ndef\nghi" with cursor at position 11 (at EOF, no trailing newline)
        let cursor = get_final_cursor("abc\ndef\nghi", 11);
        assert_eq!(
            cursor,
            Some((3, 2)),
            "Cursor at EOF after 'i' should be at column 3, line 2"
        );

        let (cursor_pos, new_content) = check_typing_at_cursor("abc\ndef\nghi", 11, 'X');
        assert_eq!(new_content, "abc\ndef\nghiX", "Typing should append at end");
        assert_eq!(cursor_pos, Some((3, 2)));
    }

    #[test]
    fn e2e_cursor_at_eof_with_trailing_newline() {
        // "abc\ndef\nghi\n" with cursor at position 12 (after trailing newline)
        let cursor = get_final_cursor("abc\ndef\nghi\n", 12);
        assert_eq!(
            cursor,
            Some((0, 3)),
            "Cursor after trailing newline should be on line 3"
        );

        let (cursor_pos, new_content) = check_typing_at_cursor("abc\ndef\nghi\n", 12, 'X');
        assert_eq!(
            new_content, "abc\ndef\nghi\nX",
            "Typing should insert on new line"
        );
        assert_eq!(cursor_pos, Some((0, 3)));
    }

    #[test]
    fn e2e_jump_to_end_of_buffer_no_trailing_newline() {
        // Simulate Ctrl+End: jump from start to end of buffer without trailing newline
        let content = "abc\ndef\nghi";

        // Start at position 0
        let cursor_at_start = get_final_cursor(content, 0);
        assert_eq!(cursor_at_start, Some((0, 0)), "Cursor starts at beginning");

        // Jump to EOF (position 11, after 'i')
        let cursor_at_eof = get_final_cursor(content, 11);
        assert_eq!(
            cursor_at_eof,
            Some((3, 2)),
            "After Ctrl+End, cursor at column 3, line 2"
        );

        // Type a character at EOF
        let (cursor_before_typing, new_content) = check_typing_at_cursor(content, 11, 'X');
        assert_eq!(cursor_before_typing, Some((3, 2)));
        assert_eq!(new_content, "abc\ndef\nghiX", "Character appended at end");

        // Verify cursor position in the new content
        let cursor_after_typing = get_final_cursor(&new_content, 12);
        assert_eq!(
            cursor_after_typing,
            Some((4, 2)),
            "After typing, cursor moved to column 4"
        );

        // Move cursor to start of buffer - verify cursor is no longer at end
        let cursor_moved_away = get_final_cursor(&new_content, 0);
        assert_eq!(cursor_moved_away, Some((0, 0)), "Cursor moved to start");
        // The cursor should NOT be at the end anymore - verify by rendering without cursor at end
        // This implicitly tests that only one cursor is rendered
    }

    #[test]
    fn e2e_jump_to_end_of_buffer_with_trailing_newline() {
        // Simulate Ctrl+End: jump from start to end of buffer WITH trailing newline
        let content = "abc\ndef\nghi\n";

        // Start at position 0
        let cursor_at_start = get_final_cursor(content, 0);
        assert_eq!(cursor_at_start, Some((0, 0)), "Cursor starts at beginning");

        // Jump to EOF (position 12, after trailing newline)
        let cursor_at_eof = get_final_cursor(content, 12);
        assert_eq!(
            cursor_at_eof,
            Some((0, 3)),
            "After Ctrl+End, cursor at column 0, line 3 (new line)"
        );

        // Type a character at EOF
        let (cursor_before_typing, new_content) = check_typing_at_cursor(content, 12, 'X');
        assert_eq!(cursor_before_typing, Some((0, 3)));
        assert_eq!(
            new_content, "abc\ndef\nghi\nX",
            "Character inserted on new line"
        );

        // After typing, the cursor should move forward
        let cursor_after_typing = get_final_cursor(&new_content, 13);
        assert_eq!(
            cursor_after_typing,
            Some((1, 3)),
            "After typing, cursor should be at column 1, line 3"
        );

        // Move cursor to middle of buffer - verify cursor is no longer at end
        let cursor_moved_away = get_final_cursor(&new_content, 4);
        assert_eq!(
            cursor_moved_away,
            Some((0, 1)),
            "Cursor moved to start of line 1 (position 4 = start of 'def')"
        );
    }

    #[test]
    fn e2e_jump_to_end_of_empty_buffer() {
        // Edge case: Ctrl+End in empty buffer should stay at (0,0)
        let content = "";

        let cursor_at_eof = get_final_cursor(content, 0);
        assert_eq!(
            cursor_at_eof,
            Some((0, 0)),
            "Empty buffer: cursor at origin"
        );

        // Type a character
        let (cursor_before_typing, new_content) = check_typing_at_cursor(content, 0, 'X');
        assert_eq!(cursor_before_typing, Some((0, 0)));
        assert_eq!(new_content, "X", "Character inserted");

        // Verify cursor after typing
        let cursor_after_typing = get_final_cursor(&new_content, 1);
        assert_eq!(
            cursor_after_typing,
            Some((1, 0)),
            "After typing, cursor at column 1"
        );

        // Move cursor back to start - verify cursor is no longer at end
        let cursor_moved_away = get_final_cursor(&new_content, 0);
        assert_eq!(
            cursor_moved_away,
            Some((0, 0)),
            "Cursor moved back to start"
        );
    }

    #[test]
    fn e2e_jump_to_end_of_single_empty_line() {
        // Edge case: buffer with just a newline
        let content = "\n";

        // Position 0 is ON the newline
        let cursor_on_newline = get_final_cursor(content, 0);
        assert_eq!(
            cursor_on_newline,
            Some((0, 0)),
            "Cursor on the newline character"
        );

        // Position 1 is AFTER the newline (EOF)
        let cursor_at_eof = get_final_cursor(content, 1);
        assert_eq!(
            cursor_at_eof,
            Some((0, 1)),
            "After Ctrl+End, cursor on line 1"
        );

        // Type at EOF
        let (cursor_before_typing, new_content) = check_typing_at_cursor(content, 1, 'X');
        assert_eq!(cursor_before_typing, Some((0, 1)));
        assert_eq!(new_content, "\nX", "Character on second line");

        let cursor_after_typing = get_final_cursor(&new_content, 2);
        assert_eq!(
            cursor_after_typing,
            Some((1, 1)),
            "After typing, cursor at column 1, line 1"
        );

        // Move cursor to the newline - verify cursor is no longer at end
        let cursor_moved_away = get_final_cursor(&new_content, 0);
        assert_eq!(
            cursor_moved_away,
            Some((0, 0)),
            "Cursor moved to the newline on line 0"
        );
    }
    // NOTE: Tests for view transform header handling have been moved to src/ui/view_pipeline.rs
    // where the elegant token-based pipeline properly handles these cases.
    // The view_pipeline tests cover:
    // - test_simple_source_lines
    // - test_wrapped_continuation
    // - test_injected_header_then_source
    // - test_mixed_scenario

    // ==================== CRLF Tokenization Tests ====================

    use crate::model::buffer::LineEnding;
    use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};

    /// Helper to extract source_offset from tokens for easier assertion
    fn extract_token_offsets(tokens: &[ViewTokenWire]) -> Vec<(String, Option<usize>)> {
        tokens
            .iter()
            .map(|t| {
                let kind_str = match &t.kind {
                    ViewTokenWireKind::Text(s) => format!("Text({})", s),
                    ViewTokenWireKind::Newline => "Newline".to_string(),
                    ViewTokenWireKind::Space => "Space".to_string(),
                    ViewTokenWireKind::Break => "Break".to_string(),
                    ViewTokenWireKind::BinaryByte(b) => format!("Byte(0x{:02x})", b),
                };
                (kind_str, t.source_offset)
            })
            .collect()
    }

    /// Test tokenization of CRLF content with a single line.
    /// Verifies that Newline token is at \r position and \n is skipped.
    #[test]
    fn test_build_base_tokens_crlf_single_line() {
        // Content: "abc\r\n" (5 bytes: a=0, b=1, c=2, \r=3, \n=4)
        let content = b"abc\r\n";
        let mut buffer = Buffer::from_bytes(content.to_vec(), test_fs());
        buffer.set_line_ending(LineEnding::CRLF);

        let tokens = SplitRenderer::build_base_tokens_for_hook(
            &mut buffer,
            0,     // top_byte
            80,    // estimated_line_length
            10,    // visible_count
            false, // is_binary
            LineEnding::CRLF,
        );

        let offsets = extract_token_offsets(&tokens);

        // Should have: Text("abc") at 0, Newline at 3
        // The \n at byte 4 should be skipped
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Text(abc)" && *off == Some(0)),
            "Expected Text(abc) at offset 0, got: {:?}",
            offsets
        );
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Newline" && *off == Some(3)),
            "Expected Newline at offset 3 (\\r position), got: {:?}",
            offsets
        );

        // Verify there's only one Newline token
        let newline_count = offsets.iter().filter(|(k, _)| k == "Newline").count();
        assert_eq!(
            newline_count, 1,
            "Should have exactly 1 Newline token for CRLF, got {}: {:?}",
            newline_count, offsets
        );
    }

    /// Test tokenization of CRLF content with multiple lines.
    /// This verifies that source_offset correctly accumulates across lines.
    #[test]
    fn test_build_base_tokens_crlf_multiple_lines() {
        // Content: "abc\r\ndef\r\nghi\r\n" (15 bytes)
        // Line 1: a=0, b=1, c=2, \r=3, \n=4
        // Line 2: d=5, e=6, f=7, \r=8, \n=9
        // Line 3: g=10, h=11, i=12, \r=13, \n=14
        let content = b"abc\r\ndef\r\nghi\r\n";
        let mut buffer = Buffer::from_bytes(content.to_vec(), test_fs());
        buffer.set_line_ending(LineEnding::CRLF);

        let tokens = SplitRenderer::build_base_tokens_for_hook(
            &mut buffer,
            0,
            80,
            10,
            false,
            LineEnding::CRLF,
        );

        let offsets = extract_token_offsets(&tokens);

        // Expected tokens:
        // Text("abc") at 0, Newline at 3
        // Text("def") at 5, Newline at 8
        // Text("ghi") at 10, Newline at 13

        // Verify line 1 tokens
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Text(abc)" && *off == Some(0)),
            "Line 1: Expected Text(abc) at 0, got: {:?}",
            offsets
        );
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Newline" && *off == Some(3)),
            "Line 1: Expected Newline at 3, got: {:?}",
            offsets
        );

        // Verify line 2 tokens - THIS IS WHERE OFFSET DRIFT WOULD APPEAR
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Text(def)" && *off == Some(5)),
            "Line 2: Expected Text(def) at 5, got: {:?}",
            offsets
        );
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Newline" && *off == Some(8)),
            "Line 2: Expected Newline at 8, got: {:?}",
            offsets
        );

        // Verify line 3 tokens - DRIFT ACCUMULATES HERE
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Text(ghi)" && *off == Some(10)),
            "Line 3: Expected Text(ghi) at 10, got: {:?}",
            offsets
        );
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Newline" && *off == Some(13)),
            "Line 3: Expected Newline at 13, got: {:?}",
            offsets
        );

        // Verify exactly 3 Newline tokens
        let newline_count = offsets.iter().filter(|(k, _)| k == "Newline").count();
        assert_eq!(newline_count, 3, "Should have 3 Newline tokens");
    }

    /// Test tokenization of LF content to compare with CRLF.
    /// LF mode should NOT skip anything - each character gets its own offset.
    #[test]
    fn test_build_base_tokens_lf_mode_for_comparison() {
        // Content: "abc\ndef\n" (8 bytes)
        // Line 1: a=0, b=1, c=2, \n=3
        // Line 2: d=4, e=5, f=6, \n=7
        let content = b"abc\ndef\n";
        let mut buffer = Buffer::from_bytes(content.to_vec(), test_fs());
        buffer.set_line_ending(LineEnding::LF);

        let tokens = SplitRenderer::build_base_tokens_for_hook(
            &mut buffer,
            0,
            80,
            10,
            false,
            LineEnding::LF,
        );

        let offsets = extract_token_offsets(&tokens);

        // Verify LF offsets
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Text(abc)" && *off == Some(0)),
            "LF Line 1: Expected Text(abc) at 0"
        );
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Newline" && *off == Some(3)),
            "LF Line 1: Expected Newline at 3"
        );
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Text(def)" && *off == Some(4)),
            "LF Line 2: Expected Text(def) at 4"
        );
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Newline" && *off == Some(7)),
            "LF Line 2: Expected Newline at 7"
        );
    }

    /// Test that CRLF in LF-mode file shows \r as control character.
    /// This verifies that \r is rendered as <0D> in LF files.
    #[test]
    fn test_build_base_tokens_crlf_in_lf_mode_shows_control_char() {
        // Content: "abc\r\n" but buffer is in LF mode
        let content = b"abc\r\n";
        let mut buffer = Buffer::from_bytes(content.to_vec(), test_fs());
        buffer.set_line_ending(LineEnding::LF); // Force LF mode

        let tokens = SplitRenderer::build_base_tokens_for_hook(
            &mut buffer,
            0,
            80,
            10,
            false,
            LineEnding::LF,
        );

        let offsets = extract_token_offsets(&tokens);

        // In LF mode, \r should be rendered as BinaryByte(0x0d)
        assert!(
            offsets.iter().any(|(kind, _)| kind == "Byte(0x0d)"),
            "LF mode should render \\r as control char <0D>, got: {:?}",
            offsets
        );
    }

    /// Test tokenization starting from middle of file (top_byte != 0).
    /// Verifies that source_offset is correct even when not starting from byte 0.
    #[test]
    fn test_build_base_tokens_crlf_from_middle() {
        // Content: "abc\r\ndef\r\nghi\r\n" (15 bytes)
        // Start from byte 5 (beginning of "def")
        let content = b"abc\r\ndef\r\nghi\r\n";
        let mut buffer = Buffer::from_bytes(content.to_vec(), test_fs());
        buffer.set_line_ending(LineEnding::CRLF);

        let tokens = SplitRenderer::build_base_tokens_for_hook(
            &mut buffer,
            5, // Start from line 2
            80,
            10,
            false,
            LineEnding::CRLF,
        );

        let offsets = extract_token_offsets(&tokens);

        // Should have:
        // Text("def") at 5, Newline at 8
        // Text("ghi") at 10, Newline at 13
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Text(def)" && *off == Some(5)),
            "Starting from byte 5: Expected Text(def) at 5, got: {:?}",
            offsets
        );
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Text(ghi)" && *off == Some(10)),
            "Starting from byte 5: Expected Text(ghi) at 10, got: {:?}",
            offsets
        );
    }

    /// End-to-end test: verify full pipeline from CRLF buffer to ViewLine to highlighting lookup
    /// This test simulates the complete flow that would trigger the offset drift bug.
    #[test]
    fn test_crlf_highlight_span_lookup() {
        use crate::view::ui::view_pipeline::ViewLineIterator;

        // Simulate Java-like CRLF content:
        // "int x;\r\nint y;\r\n"
        // Bytes: i=0, n=1, t=2, ' '=3, x=4, ;=5, \r=6, \n=7,
        //        i=8, n=9, t=10, ' '=11, y=12, ;=13, \r=14, \n=15
        let content = b"int x;\r\nint y;\r\n";
        let mut buffer = Buffer::from_bytes(content.to_vec(), test_fs());
        buffer.set_line_ending(LineEnding::CRLF);

        // Step 1: Generate tokens
        let tokens = SplitRenderer::build_base_tokens_for_hook(
            &mut buffer,
            0,
            80,
            10,
            false,
            LineEnding::CRLF,
        );

        // Verify tokens have correct offsets
        let offsets = extract_token_offsets(&tokens);
        eprintln!("Tokens: {:?}", offsets);

        // Step 2: Convert tokens to ViewLines
        let view_lines: Vec<_> = ViewLineIterator::new(&tokens, false, false, 4).collect();
        assert_eq!(view_lines.len(), 2, "Should have 2 view lines");

        // Step 3: Verify char_source_bytes mapping for each line
        // Line 1: "int x;\n" displayed, maps to bytes 0-6
        eprintln!(
            "Line 1 char_source_bytes: {:?}",
            view_lines[0].char_source_bytes
        );
        assert_eq!(
            view_lines[0].char_source_bytes.len(),
            7,
            "Line 1 should have 7 chars: 'i','n','t',' ','x',';','\\n'"
        );
        // Check specific mappings
        assert_eq!(
            view_lines[0].char_source_bytes[0],
            Some(0),
            "Line 1 'i' -> byte 0"
        );
        assert_eq!(
            view_lines[0].char_source_bytes[4],
            Some(4),
            "Line 1 'x' -> byte 4"
        );
        assert_eq!(
            view_lines[0].char_source_bytes[5],
            Some(5),
            "Line 1 ';' -> byte 5"
        );
        assert_eq!(
            view_lines[0].char_source_bytes[6],
            Some(6),
            "Line 1 newline -> byte 6 (\\r pos)"
        );

        // Line 2: "int y;\n" displayed, maps to bytes 8-14
        eprintln!(
            "Line 2 char_source_bytes: {:?}",
            view_lines[1].char_source_bytes
        );
        assert_eq!(
            view_lines[1].char_source_bytes.len(),
            7,
            "Line 2 should have 7 chars: 'i','n','t',' ','y',';','\\n'"
        );
        // Check specific mappings - THIS IS WHERE DRIFT WOULD SHOW
        assert_eq!(
            view_lines[1].char_source_bytes[0],
            Some(8),
            "Line 2 'i' -> byte 8"
        );
        assert_eq!(
            view_lines[1].char_source_bytes[4],
            Some(12),
            "Line 2 'y' -> byte 12"
        );
        assert_eq!(
            view_lines[1].char_source_bytes[5],
            Some(13),
            "Line 2 ';' -> byte 13"
        );
        assert_eq!(
            view_lines[1].char_source_bytes[6],
            Some(14),
            "Line 2 newline -> byte 14 (\\r pos)"
        );

        // Step 4: Simulate highlight span lookup
        // If TreeSitter highlights "int" as keyword (bytes 0-3 for line 1, bytes 8-11 for line 2),
        // the lookup should find these correctly.
        let simulated_highlight_spans = [
            // "int" on line 1: bytes 0-3
            (0usize..3usize, "keyword"),
            // "int" on line 2: bytes 8-11
            (8usize..11usize, "keyword"),
        ];

        // Verify that looking up byte positions from char_source_bytes finds the right spans
        for (line_idx, view_line) in view_lines.iter().enumerate() {
            for (char_idx, byte_pos) in view_line.char_source_bytes.iter().enumerate() {
                if let Some(bp) = byte_pos {
                    let in_span = simulated_highlight_spans
                        .iter()
                        .find(|(range, _)| range.contains(bp))
                        .map(|(_, name)| *name);

                    // First 3 chars of each line should be in keyword span
                    let expected_in_keyword = char_idx < 3;
                    let actually_in_keyword = in_span == Some("keyword");

                    if expected_in_keyword != actually_in_keyword {
                        panic!(
                            "CRLF offset drift detected! Line {} char {} (byte {}): expected keyword={}, got keyword={}",
                            line_idx + 1, char_idx, bp, expected_in_keyword, actually_in_keyword
                        );
                    }
                }
            }
        }
    }

    /// Test that apply_wrapping_transform correctly breaks long lines.
    /// This prevents memory exhaustion from extremely long single-line files (issue #481).
    #[test]
    fn test_apply_wrapping_transform_breaks_long_lines() {
        use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};

        // Create a token with 25,000 characters (longer than MAX_SAFE_LINE_WIDTH of 10,000)
        let long_text = "x".repeat(25_000);
        let tokens = vec![
            ViewTokenWire {
                kind: ViewTokenWireKind::Text(long_text),
                source_offset: Some(0),
                style: None,
            },
            ViewTokenWire {
                kind: ViewTokenWireKind::Newline,
                source_offset: Some(25_000),
                style: None,
            },
        ];

        // Apply wrapping with MAX_SAFE_LINE_WIDTH (simulating line_wrap disabled)
        let wrapped = SplitRenderer::apply_wrapping_transform(tokens, MAX_SAFE_LINE_WIDTH, 0);

        // Count Break tokens - should have at least 2 breaks for 25K chars at 10K width
        let break_count = wrapped
            .iter()
            .filter(|t| matches!(t.kind, ViewTokenWireKind::Break))
            .count();

        assert!(
            break_count >= 2,
            "25K char line should have at least 2 breaks at 10K width, got {}",
            break_count
        );

        // Verify total content is preserved (excluding Break tokens)
        let total_chars: usize = wrapped
            .iter()
            .filter_map(|t| match &t.kind {
                ViewTokenWireKind::Text(s) => Some(s.len()),
                _ => None,
            })
            .sum();

        assert_eq!(
            total_chars, 25_000,
            "Total character count should be preserved after wrapping"
        );
    }

    /// Test that normal-length lines are not affected by safety wrapping.
    #[test]
    fn test_apply_wrapping_transform_preserves_short_lines() {
        use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};

        // Create a token with 100 characters (much shorter than MAX_SAFE_LINE_WIDTH)
        let short_text = "x".repeat(100);
        let tokens = vec![
            ViewTokenWire {
                kind: ViewTokenWireKind::Text(short_text.clone()),
                source_offset: Some(0),
                style: None,
            },
            ViewTokenWire {
                kind: ViewTokenWireKind::Newline,
                source_offset: Some(100),
                style: None,
            },
        ];

        // Apply wrapping with MAX_SAFE_LINE_WIDTH (simulating line_wrap disabled)
        let wrapped = SplitRenderer::apply_wrapping_transform(tokens, MAX_SAFE_LINE_WIDTH, 0);

        // Should have no Break tokens for short lines
        let break_count = wrapped
            .iter()
            .filter(|t| matches!(t.kind, ViewTokenWireKind::Break))
            .count();

        assert_eq!(
            break_count, 0,
            "Short lines should not have any breaks, got {}",
            break_count
        );

        // Original text should be preserved exactly
        let text_tokens: Vec<_> = wrapped
            .iter()
            .filter_map(|t| match &t.kind {
                ViewTokenWireKind::Text(s) => Some(s.clone()),
                _ => None,
            })
            .collect();

        assert_eq!(text_tokens.len(), 1, "Should have exactly one Text token");
        assert_eq!(
            text_tokens[0], short_text,
            "Text content should be unchanged"
        );
    }

    /// End-to-end test: verify large single-line content with sequential markers
    /// is correctly chunked, wrapped, and all data is preserved through the pipeline.
    #[test]
    fn test_large_single_line_sequential_data_preserved() {
        use crate::view::ui::view_pipeline::ViewLineIterator;
        use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};

        // Create content with sequential markers that span multiple chunks
        // Format: "[00001][00002]..." - each marker is 7 chars
        let num_markers = 5_000; // ~35KB, enough to test chunking at 10K char intervals
        let content: String = (1..=num_markers).map(|i| format!("[{:05}]", i)).collect();

        // Create tokens simulating what build_base_tokens would produce
        let tokens = vec![
            ViewTokenWire {
                kind: ViewTokenWireKind::Text(content.clone()),
                source_offset: Some(0),
                style: None,
            },
            ViewTokenWire {
                kind: ViewTokenWireKind::Newline,
                source_offset: Some(content.len()),
                style: None,
            },
        ];

        // Apply safety wrapping (simulating line_wrap=false with MAX_SAFE_LINE_WIDTH)
        let wrapped = SplitRenderer::apply_wrapping_transform(tokens, MAX_SAFE_LINE_WIDTH, 0);

        // Convert to ViewLines
        let view_lines: Vec<_> = ViewLineIterator::new(&wrapped, false, false, 4).collect();

        // Reconstruct content from ViewLines
        let mut reconstructed = String::new();
        for line in &view_lines {
            // Skip the trailing newline character in each line's text
            let text = line.text.trim_end_matches('\n');
            reconstructed.push_str(text);
        }

        // Verify all content is preserved
        assert_eq!(
            reconstructed.len(),
            content.len(),
            "Reconstructed content length should match original"
        );

        // Verify sequential markers are all present
        for i in 1..=num_markers {
            let marker = format!("[{:05}]", i);
            assert!(
                reconstructed.contains(&marker),
                "Missing marker {} after pipeline",
                marker
            );
        }

        // Verify order is preserved by checking sample positions
        let pos_100 = reconstructed.find("[00100]").expect("Should find [00100]");
        let pos_1000 = reconstructed.find("[01000]").expect("Should find [01000]");
        let pos_3000 = reconstructed.find("[03000]").expect("Should find [03000]");
        assert!(
            pos_100 < pos_1000 && pos_1000 < pos_3000,
            "Markers should be in sequential order: {} < {} < {}",
            pos_100,
            pos_1000,
            pos_3000
        );

        // Verify we got multiple visual lines (content was wrapped)
        assert!(
            view_lines.len() >= 3,
            "35KB content should produce multiple visual lines at 10K width, got {}",
            view_lines.len()
        );

        // Verify each ViewLine is bounded in size (memory safety check)
        for (i, line) in view_lines.iter().enumerate() {
            assert!(
                line.text.len() <= MAX_SAFE_LINE_WIDTH + 10, // +10 for newline and rounding
                "ViewLine {} exceeds safe width: {} chars",
                i,
                line.text.len()
            );
        }
    }

    /// Helper: strip OSC 8 escape sequences from a string, returning plain text.
    fn strip_osc8(s: &str) -> String {
        let mut result = String::with_capacity(s.len());
        let bytes = s.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            if i + 3 < bytes.len()
                && bytes[i] == 0x1b
                && bytes[i + 1] == b']'
                && bytes[i + 2] == b'8'
                && bytes[i + 3] == b';'
            {
                i += 4;
                while i < bytes.len() && bytes[i] != 0x07 {
                    i += 1;
                }
                if i < bytes.len() {
                    i += 1;
                }
            } else {
                result.push(bytes[i] as char);
                i += 1;
            }
        }
        result
    }

    /// Read a row from a ratatui buffer, skipping the second cell of 2-char
    /// OSC 8 chunks so we get clean text.
    fn read_row(buf: &ratatui::buffer::Buffer, y: u16) -> String {
        let width = buf.area().width;
        let mut s = String::new();
        let mut col = 0u16;
        while col < width {
            let cell = &buf[(col, y)];
            let stripped = strip_osc8(cell.symbol());
            let chars = stripped.chars().count();
            if chars > 1 {
                s.push_str(&stripped);
                col += chars as u16;
            } else {
                s.push_str(&stripped);
                col += 1;
            }
        }
        s.trim_end().to_string()
    }

    #[test]
    fn test_apply_osc8_to_cells_preserves_adjacent_cells() {
        use ratatui::buffer::Buffer;
        use ratatui::layout::Rect;

        // Simulate: "[Quick Install](#installation)" in a 40-wide buffer row 0
        let text = "[Quick Install](#installation)";
        let area = Rect::new(0, 0, 40, 1);
        let mut buf = Buffer::empty(area);
        for (i, ch) in text.chars().enumerate() {
            if (i as u16) < 40 {
                buf[(i as u16, 0)].set_symbol(&ch.to_string());
            }
        }

        // Overlay covers "Quick Install" = cols 1..14 (bytes 9..22 mapped to screen)
        let url = "https://example.com";

        // Apply with cursor at col 0 (not inside the overlay range)
        SplitRenderer::apply_osc8_to_cells(&mut buf, 1, 14, 0, url, Some((0, 0)));

        let row = read_row(&buf, 0);
        assert_eq!(
            row, text,
            "After OSC 8 application, reading the row should reproduce the original text"
        );

        // Cell 14 = ']' must not be touched
        let cell14 = strip_osc8(buf[(14, 0)].symbol());
        assert_eq!(cell14, "]", "Cell 14 (']') must not be modified by OSC 8");

        // Cell 0 = '[' must not be touched
        let cell0 = strip_osc8(buf[(0, 0)].symbol());
        assert_eq!(cell0, "[", "Cell 0 ('[') must not be modified by OSC 8");
    }

    #[test]
    fn test_apply_osc8_stable_across_reapply() {
        use ratatui::buffer::Buffer;
        use ratatui::layout::Rect;

        let text = "[Quick Install](#installation)";
        let area = Rect::new(0, 0, 40, 1);

        // First render: apply OSC 8 with cursor at col 0
        let mut buf1 = Buffer::empty(area);
        for (i, ch) in text.chars().enumerate() {
            if (i as u16) < 40 {
                buf1[(i as u16, 0)].set_symbol(&ch.to_string());
            }
        }
        SplitRenderer::apply_osc8_to_cells(
            &mut buf1,
            1,
            14,
            0,
            "https://example.com",
            Some((0, 0)),
        );
        let row1 = read_row(&buf1, 0);

        // Second render: fresh buffer, same text, apply OSC 8 with cursor at col 5
        let mut buf2 = Buffer::empty(area);
        for (i, ch) in text.chars().enumerate() {
            if (i as u16) < 40 {
                buf2[(i as u16, 0)].set_symbol(&ch.to_string());
            }
        }
        SplitRenderer::apply_osc8_to_cells(
            &mut buf2,
            1,
            14,
            0,
            "https://example.com",
            Some((5, 0)),
        );
        let row2 = read_row(&buf2, 0);

        assert_eq!(row1, text);
        assert_eq!(row2, text);
    }

    #[test]
    #[ignore = "OSC 8 hyperlinks disabled pending ratatui diff fix"]
    fn test_apply_osc8_diff_between_renders() {
        use ratatui::buffer::Buffer;
        use ratatui::layout::Rect;

        // Simulate ratatui's diff-based update: a "concealed" render followed
        // by an "unconcealed" render. The backend buffer accumulates diffs.
        let area = Rect::new(0, 0, 40, 1);

        // --- Render 1: concealed text "Quick Install" at cols 0..12, rest is space ---
        let concealed = "Quick Install";
        let mut frame1 = Buffer::empty(area);
        for (i, ch) in concealed.chars().enumerate() {
            frame1[(i as u16, 0)].set_symbol(&ch.to_string());
        }
        // OSC 8 covers cols 0..13 (concealed mapping)
        SplitRenderer::apply_osc8_to_cells(
            &mut frame1,
            0,
            13,
            0,
            "https://example.com",
            Some((0, 5)),
        );

        // Simulate backend: starts empty, apply diff from frame1
        let mut prev = Buffer::empty(area);
        let mut backend = Buffer::empty(area);
        let diff1 = prev.diff(&frame1);
        for (x, y, cell) in &diff1 {
            backend[(*x, *y)] = (*cell).clone();
        }

        // --- Render 2: unconcealed "[Quick Install](#installation)" ---
        let full = "[Quick Install](#installation)";
        let mut frame2 = Buffer::empty(area);
        for (i, ch) in full.chars().enumerate() {
            if (i as u16) < 40 {
                frame2[(i as u16, 0)].set_symbol(&ch.to_string());
            }
        }
        // OSC 8 covers cols 1..14 (unconcealed mapping)
        SplitRenderer::apply_osc8_to_cells(
            &mut frame2,
            1,
            14,
            0,
            "https://example.com",
            Some((0, 0)),
        );

        // Apply diff from frame1→frame2 to backend
        let diff2 = frame1.diff(&frame2);
        for (x, y, cell) in &diff2 {
            backend[(*x, *y)] = (*cell).clone();
        }

        // Backend should now show the full text when read
        let row = read_row(&backend, 0);
        assert_eq!(
            row, full,
            "After diff-based update from concealed to unconcealed, \
             backend should show full text"
        );

        // Specifically, cell 14 must be ']'
        let cell14 = strip_osc8(backend[(14, 0)].symbol());
        assert_eq!(cell14, "]", "Cell 14 must be ']' after unconcealed render");
    }
}
