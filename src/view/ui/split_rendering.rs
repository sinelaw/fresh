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
use crate::services::plugins::api::ViewTransformPayload;
use crate::state::{EditorState, ViewMode};
use crate::view::split::SplitManager;
use crate::view::ui::tabs::TabsRenderer;
use crate::view::ui::view_pipeline::{
    should_show_line_number, LineStart, ViewLine, ViewLineIterator,
};
use crate::view::virtual_text::VirtualTextPosition;
use ratatui::layout::Rect;
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Paragraph};
use ratatui::Frame;
use std::collections::{HashMap, HashSet};
use std::ops::Range;

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
    semantic_spans: Vec<crate::primitives::highlighter::HighlightSpan>,
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
}

struct ViewPreferences {
    view_mode: ViewMode,
    compose_width: Option<u16>,
    compose_column_guides: Option<Vec<u16>>,
    view_transform: Option<ViewTransformPayload>,
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
}

/// Context for computing the style of a single character
struct CharStyleContext<'a> {
    byte_pos: Option<usize>,
    token_style: Option<&'a crate::services::plugins::api::ViewTokenStyle>,
    ansi_style: Style,
    is_cursor: bool,
    is_selected: bool,
    theme: &'a crate::view::theme::Theme,
    highlight_spans: &'a [crate::primitives::highlighter::HighlightSpan],
    semantic_spans: &'a [crate::primitives::highlighter::HighlightSpan],
    viewport_overlays: &'a [(crate::view::overlay::Overlay, Range<usize>)],
    primary_cursor_position: usize,
    is_active: bool,
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
    if highlight_color.is_some()
        && ctx.ansi_style.fg.is_none()
        && (ctx.ansi_style.bg.is_some() || !ctx.ansi_style.add_modifier.is_empty())
    {
        style = style.fg(highlight_color.unwrap());
    }

    // Apply semantic highlighting
    if let Some(bp) = ctx.byte_pos {
        if let Some(semantic_span) = ctx
            .semantic_spans
            .iter()
            .find(|span| span.range.contains(&bp))
        {
            style = style.bg(semantic_span.color);
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
        }
    }

    // Apply selection highlighting
    if ctx.is_selected {
        style = Style::default()
            .fg(ctx.theme.editor_fg)
            .bg(ctx.theme.selection_bg);
    }

    // Apply cursor styling - make secondary cursors visible with reversed colors
    // Don't apply REVERSED to primary cursor to preserve terminal cursor visibility
    // For inactive splits, ALL cursors use a less pronounced color (no hardware cursor)
    let is_secondary_cursor = ctx.is_cursor && ctx.byte_pos != Some(ctx.primary_cursor_position);
    if ctx.is_active {
        if is_secondary_cursor {
            style = style.add_modifier(Modifier::REVERSED);
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
    pub fn render_content(
        frame: &mut Frame,
        area: Rect,
        split_manager: &SplitManager,
        buffers: &mut HashMap<BufferId, EditorState>,
        buffer_metadata: &HashMap<BufferId, BufferMetadata>,
        event_logs: &mut HashMap<BufferId, EventLog>,
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
    ) -> (
        Vec<(
            crate::model::event::SplitId,
            BufferId,
            Rect,
            Rect,
            usize,
            usize,
        )>,
        Vec<(crate::model::event::SplitId, BufferId, u16, u16, u16, u16)>,
        Vec<(crate::model::event::SplitId, u16, u16, u16)>, // close split button areas
        Vec<(crate::model::event::SplitId, u16, u16, u16)>, // maximize split button areas
        HashMap<crate::model::event::SplitId, Vec<ViewLineMapping>>, // view line mappings for mouse clicks
    ) {
        let _span = tracing::trace_span!("render_content").entered();

        // Get all visible splits with their areas
        let visible_buffers = split_manager.get_visible_buffers(area);
        let active_split_id = split_manager.active_split();
        let has_multiple_splits = visible_buffers.len() > 1;

        // Collect areas for mouse handling
        let mut split_areas = Vec::new();
        let mut all_tab_areas = Vec::new();
        let mut close_split_areas = Vec::new();
        let mut maximize_split_areas = Vec::new();
        let mut view_line_mappings: HashMap<crate::model::event::SplitId, Vec<ViewLineMapping>> =
            HashMap::new();

        // Render each split
        for (split_id, buffer_id, split_area) in visible_buffers {
            let is_active = split_id == active_split_id;

            let layout = Self::split_layout(split_area);
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

            // Render tabs for this split and collect hit areas
            let tab_hit_areas = TabsRenderer::render_for_split(
                frame,
                layout.tabs_rect,
                &split_buffers,
                buffers,
                buffer_metadata,
                buffer_id, // The currently displayed buffer in this split
                theme,
                is_active,
                tab_scroll_offset,
                tab_hover_for_split,
            );

            // Add tab row to hit areas (all tabs share the same row)
            let tab_row = layout.tabs_rect.y;
            for (buf_id, start_col, end_col, close_start) in tab_hit_areas {
                all_tab_areas.push((split_id, buf_id, tab_row, start_col, end_col, close_start));
            }

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

            // Get references separately to avoid double borrow
            let state_opt = buffers.get_mut(&buffer_id);
            let event_log_opt = event_logs.get_mut(&buffer_id);

            if let Some(state) = state_opt {
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

                let saved_cursors = Self::temporary_split_state(
                    state,
                    split_view_states.as_deref(),
                    split_id,
                    is_active,
                );
                Self::sync_viewport_to_content(
                    &mut viewport,
                    &mut state.buffer,
                    &state.cursors,
                    layout.content_rect,
                );
                let view_prefs =
                    Self::resolve_view_preferences(state, split_view_states.as_deref(), split_id);

                let split_view_mappings = Self::render_buffer_in_split(
                    frame,
                    state,
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

                // Render scrollbar for this split and get thumb position
                let (thumb_start, thumb_end) = Self::render_scrollbar(
                    frame,
                    state,
                    &viewport,
                    layout.scrollbar_rect,
                    is_active,
                    theme,
                    large_file_threshold_bytes,
                    total_lines,
                    top_line,
                );

                // Restore the original cursors after rendering content and scrollbar
                Self::restore_split_state(state, saved_cursors);

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
            }
        }

        // Render split separators
        let separators = split_manager.get_separators(area);
        for (direction, x, y, length) in separators {
            Self::render_separator(frame, direction, x, y, length, theme);
        }

        (
            split_areas,
            all_tab_areas,
            close_split_areas,
            maximize_split_areas,
            view_line_mappings,
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

    fn split_layout(split_area: Rect) -> SplitLayout {
        let tabs_height = 1u16;
        let scrollbar_width = 1u16;

        let tabs_rect = Rect::new(split_area.x, split_area.y, split_area.width, tabs_height);
        let content_rect = Rect::new(
            split_area.x,
            split_area.y + tabs_height,
            split_area.width.saturating_sub(scrollbar_width),
            split_area.height.saturating_sub(tabs_height),
        );
        let scrollbar_rect = Rect::new(
            split_area.x + split_area.width.saturating_sub(scrollbar_width),
            split_area.y + tabs_height,
            scrollbar_width,
            split_area.height.saturating_sub(tabs_height),
        );

        SplitLayout {
            tabs_rect,
            content_rect,
            scrollbar_rect,
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

    fn temporary_split_state(
        state: &mut EditorState,
        split_view_states: Option<
            &HashMap<crate::model::event::SplitId, crate::view::split::SplitViewState>,
        >,
        split_id: crate::model::event::SplitId,
        is_active: bool,
    ) -> Option<crate::model::cursor::Cursors> {
        if is_active {
            return None;
        }

        if let Some(view_states) = split_view_states {
            if let Some(view_state) = view_states.get(&split_id) {
                // Only save/restore cursors - viewport is now owned by SplitViewState
                let saved_cursors = Some(std::mem::replace(
                    &mut state.cursors,
                    view_state.cursors.clone(),
                ));
                return saved_cursors;
            }
        }

        None
    }

    fn restore_split_state(
        state: &mut EditorState,
        saved_cursors: Option<crate::model::cursor::Cursors>,
    ) {
        if let Some(cursors) = saved_cursors {
            state.cursors = cursors;
        }
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
        state: &EditorState,
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
                };
            }
        }

        ViewPreferences {
            view_mode: state.view_mode.clone(),
            compose_width: state.compose_width,
            compose_column_guides: state.compose_column_guides.clone(),
            view_transform: state.view_transform.clone(),
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

    /// Render a scrollbar for a split
    /// Returns (thumb_start, thumb_end) positions for mouse hit testing
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
        // Use the constant viewport height (allocated terminal rows), not visible_line_count()
        // which varies based on content. The scrollbar should represent the ratio of the
        // viewport AREA to total document size, remaining constant throughout scrolling.
        let viewport_height_lines = viewport.height as usize;

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

        // Render scrollbar track and thumb
        for row in 0..height {
            let cell_area = Rect::new(scrollbar_rect.x, scrollbar_rect.y + row as u16, 1, 1);

            let (char, color) = if row >= thumb_start && row < thumb_end {
                // Thumb
                ("█", thumb_color)
            } else {
                // Track
                ("│", track_color)
            };

            let paragraph = Paragraph::new(char).style(Style::default().fg(color));
            frame.render_widget(paragraph, cell_area);
        }

        // Return thumb position for mouse hit testing
        (thumb_start, thumb_end)
    }

    fn build_view_data(
        state: &mut EditorState,
        viewport: &crate::view::viewport::Viewport,
        view_transform: Option<ViewTransformPayload>,
        estimated_line_length: usize,
        visible_count: usize,
        line_wrap_enabled: bool,
        content_width: usize,
        gutter_width: usize,
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

        // Apply wrapping transform if enabled
        if line_wrap_enabled {
            tokens = Self::apply_wrapping_transform(tokens, content_width, gutter_width);
        }

        // Convert tokens to display lines using the view pipeline
        // Each ViewLine preserves LineStart info for correct line number rendering
        // Use binary mode if the buffer contains binary content
        // Enable ANSI awareness for non-binary content to handle escape sequences correctly
        let is_binary = state.buffer.is_binary();
        let ansi_aware = !is_binary; // ANSI parsing for normal text files
        let source_lines: Vec<ViewLine> =
            ViewLineIterator::new(&tokens, is_binary, ansi_aware, state.tab_size).collect();

        // Inject virtual lines (LineAbove/LineBelow) from VirtualTextManager
        let lines = Self::inject_virtual_lines(source_lines, state);

        ViewData { lines }
    }

    /// Create a ViewLine from virtual text content (for LineAbove/LineBelow)
    fn create_virtual_line(text: &str, style: ratatui::style::Style) -> ViewLine {
        use crate::services::plugins::api::ViewTokenStyle;

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
                    if *anchor_pos >= start && *anchor_pos < end {
                        if vtext.position == VirtualTextPosition::LineAbove {
                            result.push(Self::create_virtual_line(&vtext.text, vtext.style));
                        }
                    }
                }
            }

            // Add the source line
            result.push(source_line.clone());

            // Find LineBelow virtual texts anchored to this line
            if let (Some(start), Some(end)) = (line_start_byte, line_end_byte) {
                for (anchor_pos, vtext) in &virtual_lines {
                    if *anchor_pos >= start && *anchor_pos < end {
                        if vtext.position == VirtualTextPosition::LineBelow {
                            result.push(Self::create_virtual_line(&vtext.text, vtext.style));
                        }
                    }
                }
            }
        }

        result
    }

    fn build_base_tokens(
        buffer: &mut Buffer,
        top_byte: usize,
        estimated_line_length: usize,
        visible_count: usize,
        is_binary: bool,
        line_ending: crate::model::buffer::LineEnding,
    ) -> Vec<crate::services::plugins::api::ViewTokenWire> {
        use crate::model::buffer::LineEnding;
        use crate::services::plugins::api::{ViewTokenWire, ViewTokenWireKind};

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
            if let Some((line_start, line_content)) = iter.next() {
                let mut byte_offset = 0usize;
                let content_bytes = line_content.as_bytes();
                let mut skip_next_lf = false; // Track if we should skip \n after \r in CRLF
                for ch in line_content.chars() {
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
    ) -> Vec<crate::services::plugins::api::ViewTokenWire> {
        use crate::services::plugins::api::{ViewTokenWire, ViewTokenWireKind};

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
    ) -> Vec<crate::services::plugins::api::ViewTokenWire> {
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
        tokens: Vec<crate::services::plugins::api::ViewTokenWire>,
        content_width: usize,
        gutter_width: usize,
    ) -> Vec<crate::services::plugins::api::ViewTokenWire> {
        use crate::primitives::ansi::visible_char_count;
        use crate::services::plugins::api::{ViewTokenWire, ViewTokenWireKind};

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
                    // Use visible character count (excludes ANSI escape sequences)
                    // so line width calculation is based on actual visual width
                    let text_len = visible_char_count(text);

                    // If this token would exceed line width, insert Break before it
                    if current_line_width > 0 && current_line_width + text_len > available_width {
                        wrapped.push(ViewTokenWire {
                            source_offset: None,
                            kind: ViewTokenWireKind::Break,
                            style: None,
                        });
                        current_line_width = 0;
                    }

                    // If visible text is longer than line width, we need to split
                    // However, we don't split tokens containing ANSI codes to avoid
                    // breaking escape sequences. ANSI-heavy content may exceed line width.
                    if text_len > available_width
                        && !crate::primitives::ansi::contains_ansi_codes(text)
                    {
                        let chars: Vec<char> = text.chars().collect();
                        let mut char_idx = 0;
                        let source_base = token.source_offset;

                        while char_idx < chars.len() {
                            let remaining = chars.len() - char_idx;
                            let chunk_size = remaining.min(available_width - current_line_width);

                            if chunk_size == 0 {
                                // Need to break to next line
                                wrapped.push(ViewTokenWire {
                                    source_offset: None,
                                    kind: ViewTokenWireKind::Break,
                                    style: None,
                                });
                                current_line_width = 0;
                                continue;
                            }

                            let chunk: String =
                                chars[char_idx..char_idx + chunk_size].iter().collect();
                            let chunk_source = source_base.map(|b| b + char_idx);

                            wrapped.push(ViewTokenWire {
                                source_offset: chunk_source,
                                kind: ViewTokenWireKind::Text(chunk),
                                style: token.style.clone(),
                            });

                            current_line_width += chunk_size;
                            char_idx += chunk_size;

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
                        current_line_width += text_len;
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

        let target_width = compose_width.map(|w| w as u16).unwrap_or(area.width);
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
    ) {
        // Render margins if there are any pads (indicates compose layout is active)
        if layout.left_pad == 0 && layout.right_pad == 0 {
            return;
        }

        // Paper-on-desk effect: outer "desk" margin with inner "paper edge"
        // Layout: [desk][paper edge][content][paper edge][desk]
        const PAPER_EDGE_WIDTH: u16 = 1;

        let desk_style = Style::default().bg(theme.compose_margin_bg);
        let paper_style = Style::default().bg(theme.editor_bg);

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

    fn selection_context(state: &EditorState) -> SelectionContext {
        let ranges: Vec<Range<usize>> = state
            .cursors
            .iter()
            .filter_map(|(_, cursor)| cursor.selection_range())
            .collect();

        let block_rects: Vec<(usize, usize, usize, usize)> = state
            .cursors
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
            state
                .cursors
                .iter()
                .map(|(_, cursor)| cursor.position)
                .collect()
        } else {
            Vec::new()
        };

        SelectionContext {
            ranges,
            block_rects,
            cursor_positions,
            primary_cursor_position: state.cursors.primary().position,
        }
    }

    fn decoration_context(
        state: &mut EditorState,
        viewport_start: usize,
        viewport_end: usize,
        primary_cursor_position: usize,
        theme: &crate::view::theme::Theme,
        highlight_context_bytes: usize,
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

        // Update semantic highlighter color from theme
        state.semantic_highlighter.highlight_color = theme.semantic_highlight_bg;

        let semantic_spans = state.semantic_highlighter.highlight_occurrences(
            &state.buffer,
            primary_cursor_position,
            viewport_start,
            viewport_end,
            highlight_context_bytes,
        );

        let viewport_overlays = state
            .overlays
            .query_viewport(viewport_start, viewport_end, &state.marker_list)
            .into_iter()
            .map(|(overlay, range)| (overlay.clone(), range))
            .collect::<Vec<_>>();

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
            semantic_spans,
            viewport_overlays,
            virtual_text_lookup,
            diagnostic_lines,
            line_indicators,
        }
    }

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
            if let Some((line_start, line_content)) = iter_temp.next() {
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
        } = input;

        let selection_ranges = &selection.ranges;
        let block_selections = &selection.block_rects;
        let cursor_positions = &selection.cursor_positions;
        let primary_cursor_position = selection.primary_cursor_position;

        let highlight_spans = &decorations.highlight_spans;
        let semantic_spans = &decorations.semantic_spans;
        let viewport_overlays = &decorations.viewport_overlays;
        let virtual_text_lookup = &decorations.virtual_text_lookup;
        let diagnostic_lines = &decorations.diagnostic_lines;
        let line_indicators = &decorations.line_indicators;

        let mut lines = Vec::new();
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

            let mut chars_iterator = line_content.chars().peekable();
            while let Some(ch) = chars_iterator.next() {
                // Get source byte for this character using character index
                // (char_source_bytes is indexed by character position, not visual column)
                let byte_pos = line_char_source_bytes
                    .get(display_char_idx)
                    .copied()
                    .flatten();

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
                                    cursor_screen_x =
                                        gutter_width as u16 + visible_char_count as u16;
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
                if col_offset >= left_col as usize {
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

                    let is_selected = !is_cursor
                        && byte_pos.map_or(false, |bp| {
                            selection_ranges.iter().any(|range| range.contains(&bp))
                        })
                        || (!is_cursor && is_in_block_selection);

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
                        semantic_spans,
                        viewport_overlays,
                        primary_cursor_position,
                        is_active,
                    });

                    // Determine display character (tabs already expanded in ViewLineIterator)
                    // Show tab indicator (→) at the start of tab expansions (if enabled for this language)
                    let tab_indicator: String;
                    let display_char: &str = if is_cursor && lsp_waiting && is_active {
                        "⋯"
                    } else if is_cursor && is_active && ch == '\n' {
                        ""
                    } else if ch == '\n' {
                        ""
                    } else if is_tab_start && state.show_whitespace_tabs {
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
                                let text_with_space = format!("{} ", vtext.text);
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
                        push_span_with_map(
                            &mut line_spans,
                            &mut line_view_map,
                            display_char.to_string(),
                            style,
                            byte_pos,
                        );
                    }

                    // Track cursor position for zero-width characters
                    // Zero-width chars don't get map entries, so we need to explicitly record cursor pos
                    if !have_cursor {
                        if let Some(bp) = byte_pos {
                            if bp == primary_cursor_position && char_width(ch) == 0 {
                                cursor_screen_x = gutter_width as u16 + visible_char_count as u16;
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
                    let matches_after = after_last_char_buf_pos.map_or(false, |bp| pos == bp);
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
                        .map_or(false, |bp| bp == primary_cursor_position)
                        || (after_last_char_buf_pos.is_none()
                            && primary_cursor_position >= state.buffer.len());

                    // Track cursor position for primary cursor
                    if is_primary_at_end && last_seg_y.is_some() {
                        // Cursor position now includes gutter width (consistent with main cursor tracking)
                        // For empty lines, cursor is at gutter width (right after gutter)
                        // For non-empty lines without newline, cursor is after the last character
                        cursor_screen_x = if line_len_chars == 0 {
                            gutter_width as u16
                        } else {
                            gutter_width as u16 + line_len_chars as u16
                        };
                        cursor_screen_y = last_seg_y.unwrap();
                        have_cursor = true;
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
                for (screen_x, source_offset) in line_view_map.iter().enumerate() {
                    if let Some(src) = source_offset {
                        // Check if this is the primary cursor position
                        // Only set cursor on the FIRST screen position that maps to cursor byte
                        // (important for tabs where multiple spaces map to same byte)
                        if *src == primary_cursor_position && !have_cursor {
                            cursor_screen_x = screen_x as u16;
                            cursor_screen_y = current_y;
                            have_cursor = true;
                        }
                        // Track the last visible position (rightmost character with a source mapping)
                        // This is used for EOF cursor placement
                        last_visible_x = screen_x as u16;
                    }
                }
            }

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
                    // Indicator column (space)
                    implicit_line_spans.push(Span::styled(" ", Style::default()));

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
        let eof_style = Style::default()
            .fg(theme.line_number_fg)
            .add_modifier(ratatui::style::Modifier::DIM);
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
    fn render_buffer_in_split(
        frame: &mut Frame,
        state: &mut EditorState,
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
    ) -> Vec<ViewLineMapping> {
        let _span = tracing::trace_span!("render_buffer_in_split").entered();

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
        );

        // Ensure cursor is visible using Layout-aware check (handles virtual lines)
        // This detects when cursor is beyond the rendered view_lines and scrolls
        let primary = *state.cursors.primary();
        let scrolled = viewport.ensure_visible_in_layout(&view_data.lines, &primary, gutter_width);

        // If we scrolled, rebuild view_data from new position WITH the view_transform
        // This ensures virtual lines are included in the rebuilt view
        let view_data = if scrolled {
            Self::build_view_data(
                state,
                viewport,
                view_transform_for_rebuild,
                estimated_line_length,
                visible_count,
                line_wrap,
                render_area.width as usize,
                gutter_width,
            )
        } else {
            view_data
        };

        let view_anchor = Self::calculate_view_anchor(&view_data.lines, viewport.top_byte);
        Self::render_compose_margins(frame, area, &compose_layout, &view_mode, theme);

        let selection = Self::selection_context(state);

        tracing::trace!(
            "Rendering buffer with {} cursors at positions: {:?}, primary at {}, is_active: {}, buffer_len: {}",
            selection.cursor_positions.len(),
            selection.cursor_positions,
            selection.primary_cursor_position,
            is_active,
            state.buffer.len()
        );

        if !selection
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
        );

        // Apply top_view_line_offset to skip virtual lines when scrolling through them
        let view_line_offset = viewport.top_view_line_offset;
        let view_lines_to_render =
            if view_line_offset > 0 && view_line_offset < view_data.lines.len() {
                &view_data.lines[view_line_offset..]
            } else {
                &view_data.lines
            };

        let render_output = Self::render_view_lines(LineRenderInput {
            state,
            theme,
            view_lines: view_lines_to_render,
            view_anchor,
            render_area,
            gutter_width,
            selection: &selection,
            decorations: &decorations,
            starting_line_num,
            visible_line_count: visible_count,
            lsp_waiting,
            is_active,
            line_wrap,
            estimated_lines,
            left_column: viewport.left_column,
        });

        let mut lines = render_output.lines;
        let background_x_offset = viewport.left_column as usize;

        if let Some(bg) = ansi_background {
            Self::apply_background_to_lines(
                &mut lines,
                render_area.width,
                bg,
                theme.editor_bg,
                theme.editor_fg,
                background_fade,
                background_x_offset,
                starting_line_num,
            );
        }

        frame.render_widget(Clear, render_area);
        let editor_block = Block::default()
            .borders(Borders::NONE)
            .style(Style::default().bg(theme.editor_bg));
        frame.render_widget(Paragraph::new(lines).block(editor_block), render_area);

        // Render column guides if present (for tables, etc.)
        if let Some(guides) = compose_column_guides {
            let guide_style = Style::default()
                .fg(theme.line_number_fg)
                .add_modifier(Modifier::DIM);
            let guide_height = render_output
                .content_lines_rendered
                .min(render_area.height as usize);

            for col in guides {
                // Column guides are relative to content area (after gutter)
                let guide_x = render_area.x + gutter_width as u16 + col;

                // Only draw if the guide is within the visible area
                if guide_x >= render_area.x && guide_x < render_area.x + render_area.width {
                    for row in 0..guide_height {
                        let cell_area = Rect::new(guide_x, render_area.y + row as u16, 1, 1);
                        let guide_char = Paragraph::new("│").style(guide_style);
                        frame.render_widget(guide_char, cell_area);
                    }
                }
            }
        }

        let buffer_ends_with_newline = if state.buffer.len() > 0 {
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

        if is_active && state.show_cursors && !hide_cursor {
            if let Some((cursor_screen_x, cursor_screen_y)) = cursor {
                // cursor_screen_x already includes gutter width from line_view_map
                let screen_x = render_area.x.saturating_add(cursor_screen_x);
                let screen_y = render_area.y.saturating_add(cursor_screen_y);

                frame.set_cursor_position((screen_x, screen_y));

                if let Some(event_log) = event_log {
                    let cursor_pos = state.cursors.primary().position;
                    let buffer_len = state.buffer.len();
                    event_log.log_render_state(cursor_pos, screen_x, screen_y, buffer_len);
                }
            }
        }

        // Extract view line mappings for mouse click handling
        // This maps screen coordinates to buffer byte positions
        Self::extract_view_line_mappings(view_lines_to_render)
    }

    /// Extract ViewLineMapping from rendered view lines
    /// This captures the char_source_bytes and visual_to_char from each ViewLine
    /// for accurate mouse click positioning with O(1) lookup
    fn extract_view_line_mappings(view_lines: &[ViewLine]) -> Vec<ViewLineMapping> {
        tracing::debug!(
            "extract_view_line_mappings: {} view_lines",
            view_lines.len()
        );
        for (i, vl) in view_lines.iter().enumerate().take(5) {
            let first_bytes: Vec<_> = vl.char_source_bytes.iter().take(5).collect();
            tracing::debug!(
                "  ViewLine {}: text={:?} (len={}), first_source_bytes={:?}",
                i,
                vl.text.chars().take(20).collect::<String>(),
                vl.text.len(),
                first_bytes
            );
        }
        view_lines
            .iter()
            .map(|vl| {
                // line_end_byte should be the position AFTER the last character
                // char_source_bytes stores START positions of characters, so we need to
                // add the byte length of the last character
                let line_end_byte = if let Some(&Some(last_byte_start)) =
                    vl.char_source_bytes.iter().rev().find(|m| m.is_some())
                {
                    // Get the last char from text to find its byte length
                    if let Some(last_char) = vl.text.chars().last() {
                        last_byte_start + last_char.len_utf8()
                    } else {
                        last_byte_start
                    }
                } else {
                    0
                };

                ViewLineMapping {
                    char_source_bytes: vl.char_source_bytes.clone(),
                    visual_to_char: vl.visual_to_char.clone(),
                    line_end_byte,
                }
            })
            .collect()
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
    use super::*;
    use crate::model::buffer::Buffer;
    use crate::primitives::display_width::str_width;
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
        let mut state = EditorState::new(20, 6, 1024);
        state.buffer = Buffer::from_str(content, 1024);
        state.cursors.primary_mut().position = cursor_pos.min(state.buffer.len());
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
        );
        let view_anchor = SplitRenderer::calculate_view_anchor(&view_data.lines, 0);

        let estimated_lines = (state.buffer.len() / 80).max(1);
        state.margins.update_width_for_buffer(estimated_lines);
        let gutter_width = state.margins.left_total_width();

        let selection = SplitRenderer::selection_context(&state);
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
        let theme = Theme::default();
        let decorations = SplitRenderer::decoration_context(
            &mut state,
            viewport_start,
            viewport_end,
            selection.primary_cursor_position,
            &theme,
            100_000, // default highlight context bytes
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
    // 2. Visual spans with REVERSED modifier (secondary cursors)
    // 3. Visual spans with special background color (inactive cursors)
    fn count_all_cursors(output: &LineRenderOutput) -> Vec<(u16, u16)> {
        let mut cursor_positions = Vec::new();

        // Check for primary cursor in output.cursor field
        if let Some(cursor_pos) = output.cursor {
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
                    // Found a visual cursor - record its position
                    cursor_positions.push((col, line_idx as u16));
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
            let mut state = EditorState::new(20, 6, 1024);
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
    use crate::services::plugins::api::{ViewTokenWire, ViewTokenWireKind};

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
        let mut buffer = Buffer::from_bytes(content.to_vec());
        buffer.set_line_ending(LineEnding::CRLF);

        let tokens = SplitRenderer::build_base_tokens_for_hook(
            &mut buffer,
            0,   // top_byte
            80,  // estimated_line_length
            10,  // visible_count
            false, // is_binary
            LineEnding::CRLF,
        );

        let offsets = extract_token_offsets(&tokens);

        // Should have: Text("abc") at 0, Newline at 3
        // The \n at byte 4 should be skipped
        assert!(
            offsets.iter().any(|(kind, off)| kind == "Text(abc)" && *off == Some(0)),
            "Expected Text(abc) at offset 0, got: {:?}",
            offsets
        );
        assert!(
            offsets.iter().any(|(kind, off)| kind == "Newline" && *off == Some(3)),
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
        let mut buffer = Buffer::from_bytes(content.to_vec());
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
            offsets.iter().any(|(kind, off)| kind == "Text(abc)" && *off == Some(0)),
            "Line 1: Expected Text(abc) at 0, got: {:?}",
            offsets
        );
        assert!(
            offsets.iter().any(|(kind, off)| kind == "Newline" && *off == Some(3)),
            "Line 1: Expected Newline at 3, got: {:?}",
            offsets
        );

        // Verify line 2 tokens - THIS IS WHERE OFFSET DRIFT WOULD APPEAR
        assert!(
            offsets.iter().any(|(kind, off)| kind == "Text(def)" && *off == Some(5)),
            "Line 2: Expected Text(def) at 5, got: {:?}",
            offsets
        );
        assert!(
            offsets.iter().any(|(kind, off)| kind == "Newline" && *off == Some(8)),
            "Line 2: Expected Newline at 8, got: {:?}",
            offsets
        );

        // Verify line 3 tokens - DRIFT ACCUMULATES HERE
        assert!(
            offsets.iter().any(|(kind, off)| kind == "Text(ghi)" && *off == Some(10)),
            "Line 3: Expected Text(ghi) at 10, got: {:?}",
            offsets
        );
        assert!(
            offsets.iter().any(|(kind, off)| kind == "Newline" && *off == Some(13)),
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
        let mut buffer = Buffer::from_bytes(content.to_vec());
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
            offsets.iter().any(|(kind, off)| kind == "Text(abc)" && *off == Some(0)),
            "LF Line 1: Expected Text(abc) at 0"
        );
        assert!(
            offsets.iter().any(|(kind, off)| kind == "Newline" && *off == Some(3)),
            "LF Line 1: Expected Newline at 3"
        );
        assert!(
            offsets.iter().any(|(kind, off)| kind == "Text(def)" && *off == Some(4)),
            "LF Line 2: Expected Text(def) at 4"
        );
        assert!(
            offsets.iter().any(|(kind, off)| kind == "Newline" && *off == Some(7)),
            "LF Line 2: Expected Newline at 7"
        );
    }

    /// Test that CRLF in LF-mode file shows \r as control character.
    /// This verifies that \r is rendered as <0D> in LF files.
    #[test]
    fn test_build_base_tokens_crlf_in_lf_mode_shows_control_char() {
        // Content: "abc\r\n" but buffer is in LF mode
        let content = b"abc\r\n";
        let mut buffer = Buffer::from_bytes(content.to_vec());
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
        let mut buffer = Buffer::from_bytes(content.to_vec());
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
            offsets.iter().any(|(kind, off)| kind == "Text(def)" && *off == Some(5)),
            "Starting from byte 5: Expected Text(def) at 5, got: {:?}",
            offsets
        );
        assert!(
            offsets.iter().any(|(kind, off)| kind == "Text(ghi)" && *off == Some(10)),
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
        let mut buffer = Buffer::from_bytes(content.to_vec());
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
        eprintln!("Line 1 char_source_bytes: {:?}", view_lines[0].char_source_bytes);
        assert_eq!(
            view_lines[0].char_source_bytes.len(),
            7,
            "Line 1 should have 7 chars: 'i','n','t',' ','x',';','\\n'"
        );
        // Check specific mappings
        assert_eq!(view_lines[0].char_source_bytes[0], Some(0), "Line 1 'i' -> byte 0");
        assert_eq!(view_lines[0].char_source_bytes[4], Some(4), "Line 1 'x' -> byte 4");
        assert_eq!(view_lines[0].char_source_bytes[5], Some(5), "Line 1 ';' -> byte 5");
        assert_eq!(view_lines[0].char_source_bytes[6], Some(6), "Line 1 newline -> byte 6 (\\r pos)");

        // Line 2: "int y;\n" displayed, maps to bytes 8-14
        eprintln!("Line 2 char_source_bytes: {:?}", view_lines[1].char_source_bytes);
        assert_eq!(
            view_lines[1].char_source_bytes.len(),
            7,
            "Line 2 should have 7 chars: 'i','n','t',' ','y',';','\\n'"
        );
        // Check specific mappings - THIS IS WHERE DRIFT WOULD SHOW
        assert_eq!(view_lines[1].char_source_bytes[0], Some(8), "Line 2 'i' -> byte 8");
        assert_eq!(view_lines[1].char_source_bytes[4], Some(12), "Line 2 'y' -> byte 12");
        assert_eq!(view_lines[1].char_source_bytes[5], Some(13), "Line 2 ';' -> byte 13");
        assert_eq!(view_lines[1].char_source_bytes[6], Some(14), "Line 2 newline -> byte 14 (\\r pos)");

        // Step 4: Simulate highlight span lookup
        // If TreeSitter highlights "int" as keyword (bytes 0-3 for line 1, bytes 8-11 for line 2),
        // the lookup should find these correctly.
        let simulated_highlight_spans = vec![
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
}
