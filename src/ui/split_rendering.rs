//! Split pane layout and buffer rendering

use crate::ansi::AnsiParser;
use crate::ansi_background::AnsiBackground;
use crate::cursor::SelectionMode;
use crate::editor::BufferMetadata;
use crate::event::{BufferId, EventLog, SplitDirection};
use crate::plugin_api::ViewTransformPayload;
use crate::split::SplitManager;
use crate::state::{EditorState, ViewMode};
use crate::text_buffer::Buffer;
use crate::ui::tabs::TabsRenderer;
use crate::virtual_text::VirtualTextPosition;
use crate::view::flatten_tokens;
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
    for _ in text.chars() {
        map.push(source);
    }
    spans.push(Span::styled(text, style));
}

struct ViewLine {
    offset: usize,
    text: String,
    ends_with_newline: bool,
}

struct ViewData {
    mapping: Vec<Option<usize>>,
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
    highlight_spans: Vec<crate::highlighter::HighlightSpan>,
    semantic_spans: Vec<crate::highlighter::HighlightSpan>,
    viewport_overlays: Vec<(crate::overlay::Overlay, Range<usize>)>,
    virtual_text_lookup: HashMap<usize, Vec<crate::virtual_text::VirtualText>>,
    diagnostic_lines: HashSet<usize>,
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
    theme: &'a crate::theme::Theme,
    view_lines: &'a [ViewLine],
    view_mapping: &'a [Option<usize>],
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
        theme: &crate::theme::Theme,
        ansi_background: Option<&AnsiBackground>,
        background_fade: f32,
        lsp_waiting: bool,
        large_file_threshold_bytes: u64,
        _line_wrap: bool,
        estimated_line_length: usize,
        split_view_states: Option<&HashMap<crate::event::SplitId, crate::split::SplitViewState>>,
        hide_cursor: bool,
    ) -> Vec<(crate::event::SplitId, BufferId, Rect, Rect, usize, usize)> {
        let _span = tracing::trace_span!("render_content").entered();

        // Get all visible splits with their areas
        let visible_buffers = split_manager.get_visible_buffers(area);
        let active_split_id = split_manager.active_split();

        // Collect areas for mouse handling
        let mut split_areas = Vec::new();

        // Render each split
        for (split_id, buffer_id, split_area) in visible_buffers {
            let is_active = split_id == active_split_id;

            let layout = Self::split_layout(split_area);
            let (split_buffers, tab_scroll_offset) =
                Self::split_buffers_for_tabs(split_view_states, split_id, buffer_id);

            // Render tabs for this split
            TabsRenderer::render_for_split(
                frame,
                layout.tabs_rect,
                &split_buffers,
                buffers,
                buffer_metadata,
                buffer_id, // The currently displayed buffer in this split
                theme,
                is_active,
                tab_scroll_offset,
            );

            // Get references separately to avoid double borrow
            let state_opt = buffers.get_mut(&buffer_id);
            let event_log_opt = event_logs.get_mut(&buffer_id);

            if let Some(state) = state_opt {
                let saved_state =
                    Self::temporary_split_state(state, split_view_states, split_id, is_active);
                Self::sync_viewport_to_content(state, layout.content_rect);
                let view_prefs =
                    Self::resolve_view_preferences(state, split_view_states, split_id);

                Self::render_buffer_in_split(
                    frame,
                    state,
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
                    buffer_id,
                    hide_cursor,
                );

                // For small files, count actual lines for accurate scrollbar
                // For large files, we'll use a constant thumb size
                // NOTE: Calculate scrollbar BEFORE restoring state to use this split's viewport
                let buffer_len = state.buffer.len();
                let (total_lines, top_line) =
                    Self::scrollbar_line_counts(state, large_file_threshold_bytes, buffer_len);

                // Render scrollbar for this split and get thumb position
                // NOTE: Render scrollbar BEFORE restoring state to use this split's viewport
                let (thumb_start, thumb_end) = Self::render_scrollbar(
                    frame,
                    state,
                    layout.scrollbar_rect,
                    is_active,
                    theme,
                    large_file_threshold_bytes,
                    total_lines,
                    top_line,
                );

                // Restore the original cursors and viewport after rendering content and scrollbar
                Self::restore_split_state(state, saved_state);

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

        split_areas
    }

    /// Render a split separator line
    fn render_separator(
        frame: &mut Frame,
        direction: SplitDirection,
        x: u16,
        y: u16,
        length: u16,
        theme: &crate::theme::Theme,
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
        split_view_states: Option<&HashMap<crate::event::SplitId, crate::split::SplitViewState>>,
        split_id: crate::event::SplitId,
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
        split_view_states: Option<&HashMap<crate::event::SplitId, crate::split::SplitViewState>>,
        split_id: crate::event::SplitId,
        is_active: bool,
    ) -> (Option<crate::cursor::Cursors>, Option<crate::viewport::Viewport>) {
        if is_active {
            return (None, None);
        }

        if let Some(view_states) = split_view_states {
            if let Some(view_state) = view_states.get(&split_id) {
                let saved_cursors =
                    Some(std::mem::replace(&mut state.cursors, view_state.cursors.clone()));
                let saved_viewport =
                    Some(std::mem::replace(&mut state.viewport, view_state.viewport.clone()));
                return (saved_cursors, saved_viewport);
            }
        }

        (None, None)
    }

    fn restore_split_state(
        state: &mut EditorState,
        saved_state: (
            Option<crate::cursor::Cursors>,
            Option<crate::viewport::Viewport>,
        ),
    ) {
        let (saved_cursors, saved_viewport) = saved_state;
        if let Some(cursors) = saved_cursors {
            state.cursors = cursors;
        }
        if let Some(viewport) = saved_viewport {
            state.viewport = viewport;
        }
    }

    fn sync_viewport_to_content(state: &mut EditorState, content_rect: Rect) {
        if state.viewport.width != content_rect.width || state.viewport.height != content_rect.height
        {
            state
                .viewport
                .resize(content_rect.width, content_rect.height);
            let primary = *state.cursors.primary();
            state.viewport.ensure_visible(&mut state.buffer, &primary);
        }
    }

    fn resolve_view_preferences(
        state: &EditorState,
        split_view_states: Option<&HashMap<crate::event::SplitId, crate::split::SplitViewState>>,
        split_id: crate::event::SplitId,
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

        let top_line = if state.viewport.top_byte < buffer_len {
            state.buffer.get_line_number(state.viewport.top_byte)
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
        scrollbar_rect: Rect,
        is_active: bool,
        _theme: &crate::theme::Theme,
        large_file_threshold_bytes: u64,
        total_lines: usize,
        top_line: usize,
    ) -> (usize, usize) {
        let height = scrollbar_rect.height as usize;
        if height == 0 {
            return (0, 0);
        }

        let buffer_len = state.buffer.len();
        let viewport_top = state.viewport.top_byte;
        // Use the constant viewport height (allocated terminal rows), not visible_line_count()
        // which varies based on content. The scrollbar should represent the ratio of the
        // viewport AREA to total document size, remaining constant throughout scrolling.
        let viewport_height_lines = state.viewport.height as usize;

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
        view_transform: Option<ViewTransformPayload>,
        estimated_line_length: usize,
        visible_count: usize,
        line_wrap_enabled: bool,
        content_width: usize,
        gutter_width: usize,
    ) -> ViewData {
        // Build base token stream from source
        let base_tokens = Self::build_base_tokens(
            &mut state.buffer,
            state.viewport.top_byte,
            estimated_line_length,
            visible_count,
        );

        // Use plugin transform if available, otherwise use base tokens
        let mut tokens = view_transform
            .map(|vt| vt.tokens)
            .unwrap_or(base_tokens);

        // Apply wrapping transform if enabled
        if line_wrap_enabled {
            tokens = Self::apply_wrapping_transform(tokens, content_width, gutter_width);
        }

        // Convert tokens to text + mapping
        let (text, mapping) = flatten_tokens(&tokens);

        ViewData {
            lines: Self::build_view_lines(&text),
            mapping,
        }
    }

    fn build_base_tokens(
        buffer: &mut Buffer,
        top_byte: usize,
        estimated_line_length: usize,
        visible_count: usize,
    ) -> Vec<crate::plugin_api::ViewTokenWire> {
        use crate::plugin_api::{ViewTokenWire, ViewTokenWireKind};

        let mut tokens = Vec::new();
        let mut iter = buffer.line_iterator(top_byte, estimated_line_length);
        let mut lines_seen = 0usize;
        let max_lines = visible_count.saturating_add(4);

        while lines_seen < max_lines {
            if let Some((line_start, line_content)) = iter.next() {
                let mut byte_offset = 0usize;
                for ch in line_content.chars() {
                    let ch_len = ch.len_utf8();
                    let source_offset = Some(line_start + byte_offset);

                    match ch {
                        '\n' => {
                            tokens.push(ViewTokenWire {
                                source_offset,
                                kind: ViewTokenWireKind::Newline,
                            });
                        }
                        ' ' => {
                            tokens.push(ViewTokenWire {
                                source_offset,
                                kind: ViewTokenWireKind::Space,
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
            });
        }

        tokens
    }

    /// Public wrapper for building base tokens - used by render.rs for the view_transform_request hook
    pub fn build_base_tokens_for_hook(
        buffer: &mut Buffer,
        top_byte: usize,
        estimated_line_length: usize,
        visible_count: usize,
    ) -> Vec<crate::plugin_api::ViewTokenWire> {
        Self::build_base_tokens(buffer, top_byte, estimated_line_length, visible_count)
    }

    fn apply_wrapping_transform(
        tokens: Vec<crate::plugin_api::ViewTokenWire>,
        content_width: usize,
        gutter_width: usize,
    ) -> Vec<crate::plugin_api::ViewTokenWire> {
        use crate::plugin_api::{ViewTokenWire, ViewTokenWireKind};

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
                    let text_len = text.chars().count();

                    // If this token would exceed line width, insert Break before it
                    if current_line_width > 0 && current_line_width + text_len > available_width {
                        wrapped.push(ViewTokenWire {
                            source_offset: None,
                            kind: ViewTokenWireKind::Break,
                        });
                        current_line_width = 0;
                    }

                    // If token itself is longer than line width, split it
                    if text_len > available_width {
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
                                });
                                current_line_width = 0;
                                continue;
                            }

                            let chunk: String = chars[char_idx..char_idx + chunk_size].iter().collect();
                            let chunk_source = source_base.map(|b| b + char_idx);

                            wrapped.push(ViewTokenWire {
                                source_offset: chunk_source,
                                kind: ViewTokenWireKind::Text(chunk),
                            });

                            current_line_width += chunk_size;
                            char_idx += chunk_size;

                            // If we filled the line, break
                            if current_line_width >= available_width {
                                wrapped.push(ViewTokenWire {
                                    source_offset: None,
                                    kind: ViewTokenWireKind::Break,
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
            }
        }

        wrapped
    }

    fn build_view_lines(view_text: &str) -> Vec<ViewLine> {
        let mut view_lines: Vec<ViewLine> = Vec::new();
        let mut offset = 0usize;
        for segment in view_text.split_inclusive('\n') {
            let text = segment.to_string(); // keep newline so indices stay aligned
            let ends_with_newline = text.ends_with('\n');
            view_lines.push(ViewLine {
                offset,
                text,
                ends_with_newline,
            });
            offset += segment.chars().count();
        }
        if view_text.is_empty() {
            view_lines.push(ViewLine {
                offset: 0,
                text: String::new(),
                ends_with_newline: false,
            });
        }
        view_lines
    }

    fn calculate_view_anchor(
        view_lines: &[ViewLine],
        view_mapping: &[Option<usize>],
        top_byte: usize,
    ) -> ViewAnchor {
        let view_top = view_mapping
            .iter()
            .position(|m| m.map_or(false, |s| s >= top_byte))
            .unwrap_or(0);
        let mut view_start_line_idx = 0usize;
        let mut view_start_line_skip = 0usize;
        for (idx, line) in view_lines.iter().enumerate() {
            let len = line.text.chars().count();
            if view_top >= line.offset && view_top <= line.offset + len {
                view_start_line_idx = idx;
                view_start_line_skip = view_top.saturating_sub(line.offset);
                break;
            }
        }
        ViewAnchor {
            start_line_idx: view_start_line_idx,
            start_line_skip: view_start_line_skip,
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
        view_mode: &ViewMode,
        theme: &crate::theme::Theme,
    ) {
        // Render margins if there are any pads (indicates compose layout is active)
        if layout.left_pad == 0 && layout.right_pad == 0 {
            return;
        }

        let margin_style = Style::default().bg(theme.line_number_bg);
        if layout.left_pad > 0 {
            let left_rect = Rect::new(area.x, area.y, layout.left_pad, area.height);
            frame.render_widget(Block::default().style(margin_style), left_rect);
        }
        if layout.right_pad > 0 {
            let right_rect = Rect::new(
                area.x + layout.left_pad + layout.render_area.width,
                area.y,
                layout.right_pad,
                area.height,
            );
            frame.render_widget(Block::default().style(margin_style), right_rect);
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
    ) -> DecorationContext {
        let highlight_spans = if let Some(highlighter) = &mut state.highlighter {
            highlighter.highlight_viewport(&state.buffer, viewport_start, viewport_end)
        } else {
            Vec::new()
        };

        let semantic_spans = state.semantic_highlighter.highlight_occurrences(
            &state.buffer,
            primary_cursor_position,
            viewport_start,
            viewport_end,
        );

        let viewport_overlays = state
            .overlays
            .query_viewport(viewport_start, viewport_end, &state.marker_list)
            .into_iter()
            .map(|(overlay, range)| (overlay.clone(), range))
            .collect::<Vec<_>>();

        let diagnostic_lines: HashSet<usize> = viewport_overlays
            .iter()
            .filter_map(|(overlay, range)| {
                if let Some(id) = &overlay.id {
                    if id.starts_with("lsp-diagnostic-") {
                        return Some(state.buffer.get_line_number(range.start));
                    }
                }
                None
            })
            .collect();

        let virtual_text_lookup: HashMap<usize, Vec<crate::virtual_text::VirtualText>> = state
            .virtual_texts
            .build_lookup(&state.marker_list, viewport_start, viewport_end)
            .into_iter()
            .map(|(position, texts)| (position, texts.into_iter().cloned().collect()))
            .collect();

        DecorationContext {
            highlight_spans,
            semantic_spans,
            viewport_overlays,
            virtual_text_lookup,
            diagnostic_lines,
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
            view_mapping,
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
        let mut view_start_line_skip = view_anchor.start_line_skip;

        // Track the current source line number separately from display lines
        let mut current_source_line_num = starting_line_num;

        loop {
            let (line_view_offset, line_content, line_has_newline) =
                if let Some(ViewLine {
                    offset,
                    text,
                    ends_with_newline,
                }) = view_lines.get(view_iter_idx)
                {
                    let mut content = text.clone();
                    let mut base = *offset;
                    if view_iter_idx == view_anchor.start_line_idx && view_start_line_skip > 0 {
                        let skip = view_start_line_skip;
                        content = text.chars().skip(skip).collect();
                        base += skip;
                        view_start_line_skip = 0;
                    }
                    view_iter_idx += 1;
                    (base, content, *ends_with_newline)
                } else if is_empty_buffer && lines_rendered == 0 {
                    (0, String::new(), false)
                } else {
                    break;
                };

            if lines_rendered >= visible_line_count {
                break;
            }

            // Check if this is a wrapped continuation line
            // A continuation line comes AFTER a Break token's newline (which has None mapping)
            // So check if the previous character (the newline) was a Break
            let is_continuation = if line_view_offset > 0 {
                view_mapping.get(line_view_offset - 1) == Some(&None)
            } else {
                false // First line is never a continuation
            };

            // Only increment source line number if this is NOT a continuation
            if !is_continuation && lines_rendered > 0 {
                current_source_line_num += 1;
            }

            lines_rendered += 1;

            // Apply horizontal scrolling - skip characters before left_column
            let left_col = state.viewport.left_column;

            // Build line with selection highlighting
            let mut line_spans = Vec::new();
            let mut line_view_map: Vec<Option<usize>> = Vec::new();
            let mut last_seg_y: Option<u16> = None;
            let mut _last_seg_width: usize = 0;

            // Render left margin (indicators + line numbers + separator)
            if state.margins.left_config.enabled {
                // For continuation lines, don't show diagnostic indicators
                if !is_continuation && diagnostic_lines.contains(&current_source_line_num) {
                    // Show diagnostic indicator
                    push_span_with_map(
                        &mut line_spans,
                        &mut line_view_map,
                        "●".to_string(),
                        Style::default().fg(ratatui::style::Color::Red),
                        None,
                    );
                } else {
                    // Show space (reserved for future indicators like breakpoints)
                    push_span_with_map(
                        &mut line_spans,
                        &mut line_view_map,
                        " ".to_string(),
                        Style::default(),
                        None,
                    );
                }

                // Next N columns: render line number (right-aligned) or blank for continuations
                if is_continuation {
                    // For wrapped continuation lines, render blank space
                    let blank = " ".repeat(state.margins.left_config.width);
                    push_span_with_map(
                        &mut line_spans,
                        &mut line_view_map,
                        blank,
                        Style::default().fg(theme.line_number_fg),
                        None,
                    );
                } else {
                    let margin_content = state.margins.render_line(
                        current_source_line_num,
                        crate::margin::MarginPosition::Left,
                        estimated_lines,
                    );
                    let (rendered_text, style_opt) =
                        margin_content.render(state.margins.left_config.width);

                    // Use custom style if provided, otherwise use default theme color
                    let margin_style =
                        style_opt.unwrap_or_else(|| Style::default().fg(theme.line_number_fg));

                    push_span_with_map(
                        &mut line_spans,
                        &mut line_view_map,
                        rendered_text,
                        margin_style,
                        None,
                    );
                }

                // Render separator
                if state.margins.left_config.show_separator {
                    let separator_style = Style::default().fg(theme.line_number_fg);
                    push_span_with_map(
                        &mut line_spans,
                        &mut line_view_map,
                        state.margins.left_config.separator.clone(),
                        separator_style,
                        None,
                    );
                }
            }

            // Check if this line has any selected text
            let mut char_index = 0;
            let mut col_offset = 0usize;

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
            let mut ansi_parser = AnsiParser::new();
            // Track visible characters separately from byte position for ANSI handling
            let mut visible_char_count = 0usize;

            let mut chars_iterator = line_content.chars().peekable();
            while let Some(ch) = chars_iterator.next() {
                let view_idx = line_view_offset + col_offset;
                let byte_pos = view_mapping.get(view_idx).copied().flatten();

                // Process character through ANSI parser first
                // If it returns None, the character is part of an escape sequence and should be skipped
                let ansi_style = match ansi_parser.parse_char(ch) {
                    Some(style) => style,
                    None => {
                        // This character is part of an ANSI escape sequence, skip it
                        char_index += ch.len_utf8();
                        continue;
                    }
                };

                // Performance: skip expensive style calculations for characters beyond visible range
                // Use visible_char_count (not char_index) since ANSI codes don't take up visible space
                if visible_char_count > max_chars_to_process {
                    // Fast path: just count remaining characters without processing
                    // This is critical for performance with very long lines (e.g., 100KB single line)
                    char_index += ch.len_utf8();
                    for remaining_ch in chars_iterator.by_ref() {
                        char_index += remaining_ch.len_utf8();
                    }
                    break;
                }

                // Skip characters before left_column
                if col_offset >= left_col as usize {
                    // Check if this character is at a cursor position
                    let is_cursor = byte_pos
                        .map(|bp| bp < state.buffer.len() && cursor_positions.contains(&bp))
                        .unwrap_or(false);

                    // Check if this character is in any selection range (but not at cursor position)
                    // Also check for block/rectangular selections
                    let is_in_block_selection = block_selections.iter().any(
                        |(start_line, start_col, end_line, end_col)| {
                            current_source_line_num >= *start_line
                                && current_source_line_num <= *end_line
                                && char_index >= *start_col
                                && char_index <= *end_col
                        },
                    );

                    let is_selected = !is_cursor
                        && byte_pos.map_or(false, |bp| {
                            selection_ranges
                                .iter()
                                .any(|range| range.contains(&bp))
                        })
                        || (!is_cursor && is_in_block_selection);

                    let highlight_color = byte_pos.and_then(|bp| {
                        highlight_spans
                            .iter()
                            .find(|span| span.range.contains(&bp))
                            .map(|span| span.color)
                    });

                    let overlays: Vec<&crate::overlay::Overlay> = if let Some(bp) = byte_pos {
                        viewport_overlays
                            .iter()
                            .filter(|(_, range)| range.contains(&bp))
                            .map(|(overlay, _)| overlay)
                            .collect()
                    } else {
                        Vec::new()
                    };

                    // Build style by layering: base -> ansi -> syntax -> semantic -> overlays -> selection
                    // Start with ANSI style as base (if present), otherwise use theme default
                    let mut style = if ansi_style.fg.is_some()
                        || ansi_style.bg.is_some()
                        || !ansi_style.add_modifier.is_empty()
                    {
                        // Apply ANSI styling from escape codes
                        let mut s = Style::default();
                        if let Some(fg) = ansi_style.fg {
                            s = s.fg(fg);
                        } else {
                            s = s.fg(theme.editor_fg);
                        }
                        if let Some(bg) = ansi_style.bg {
                            s = s.bg(bg);
                        }
                        s = s.add_modifier(ansi_style.add_modifier);
                        s
                    } else if let Some(color) = highlight_color {
                        // Apply syntax highlighting
                        Style::default().fg(color)
                    } else {
                        // Default color from theme
                        Style::default().fg(theme.editor_fg)
                    };

                    // If we have ANSI style but also syntax highlighting, syntax takes precedence for color
                    // (unless ANSI has explicit color which we already applied above)
                    if highlight_color.is_some()
                        && ansi_style.fg.is_none()
                        && (ansi_style.bg.is_some() || !ansi_style.add_modifier.is_empty())
                    {
                        // ANSI had bg or modifiers but not fg, so apply syntax fg
                        style = style.fg(highlight_color.unwrap());
                    }

                    if let Some(bp) = byte_pos {
                        if let Some(semantic_span) =
                            semantic_spans.iter().find(|span| span.range.contains(&bp))
                        {
                            style = style.bg(semantic_span.color);
                        }
                    }

                    use crate::overlay::OverlayFace;
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

                    if is_selected {
                        style = Style::default().fg(theme.editor_fg).bg(theme.selection_bg);
                    }

                    // Cursor styling - make secondary cursors visible with reversed colors
                    // Don't apply REVERSED to primary cursor to preserve terminal cursor visibility
                    // For inactive splits, ALL cursors use a less pronounced color (no hardware cursor)
                    let is_secondary_cursor =
                        is_cursor && byte_pos != Some(primary_cursor_position);
                    if is_active {
                        if is_secondary_cursor {
                            style = style.add_modifier(Modifier::REVERSED);
                        }
                    } else if is_cursor {
                        style = style.fg(theme.editor_fg).bg(theme.inactive_cursor);
                    }

                    let display_char = if is_cursor && lsp_waiting && is_active {
                        "⋯"
                    } else if is_cursor && is_active && ch == '\n' {
                        ""
                    } else if ch == '\n' {
                        ""
                    } else {
                        &ch.to_string()
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
                        let should_add_indicator = if is_active {
                            is_secondary_cursor
                        } else {
                            true
                        };
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

                char_index += ch.len_utf8();
                col_offset += 1;
                visible_char_count += 1;
            }

            // Set last_seg_y early so cursor detection works for both empty and non-empty lines
            // For lines without wrapping, this will be the final y position
            if line_spans.is_empty() || !line_wrap {
                last_seg_y = Some(lines.len() as u16);
            }

            if !line_has_newline {
                let line_len_chars = line_content.chars().count();

                // Map view positions to buffer positions using view_mapping
                let last_char_view_idx = line_view_offset + line_len_chars.saturating_sub(1);
                let after_last_char_view_idx = line_view_offset + line_len_chars;

                let last_char_buf_pos = view_mapping.get(last_char_view_idx).copied().flatten();
                let after_last_char_buf_pos = view_mapping.get(after_last_char_view_idx).copied().flatten();

                let cursor_at_end = cursor_positions.iter().any(|&pos| {
                    let matches_last = last_char_buf_pos.map_or(false, |bp| pos == bp);
                    let matches_after = after_last_char_buf_pos.map_or(false, |bp| pos == bp);
                    // Fallback: when there's no mapping after last char (EOF), check if cursor is at/near end
                    // The fallback should match the position that would be "after" if there was a mapping
                    let expected_after_pos = last_char_buf_pos.map(|p| p + 1).unwrap_or(0);
                    let matches_fallback = after_last_char_buf_pos.is_none() && pos == expected_after_pos;

                    matches_last || matches_after || matches_fallback
                });

                if cursor_at_end {
                    let is_primary_at_end = last_char_buf_pos.map_or(false, |bp| bp == primary_cursor_position)
                        || after_last_char_buf_pos.map_or(false, |bp| bp == primary_cursor_position)
                        || (after_last_char_buf_pos.is_none() && primary_cursor_position >= state.buffer.len());

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

                    let should_add_indicator = if is_active {
                        !is_primary_at_end
                    } else {
                        true
                    };
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

            // ViewLines are already wrapped (Break tokens became newlines in flatten_tokens)
            // so each line is one visual line - no need to wrap again
            let current_y = lines.len() as u16;
            last_seg_y = Some(current_y);

            if !line_spans.is_empty() {
                // Find cursor position and track last visible x by iterating through line_view_map
                // Note: line_view_map includes both gutter and content character mappings
                for (screen_x, source_offset) in line_view_map.iter().enumerate() {
                    if let Some(src) = source_offset {
                        // Check if this is the primary cursor position
                        if *src == primary_cursor_position {
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

            lines.push(Line::from(line_spans));

            // Update last_line_end and check for cursor on newline BEFORE the break check
            // This ensures the last visible line's metadata is captured
            if let Some(y) = last_seg_y {
                let end_x = last_visible_x.saturating_add(1);
                let view_end_idx = line_view_offset + line_content.chars().count();

                last_line_end = Some(LastLineEnd {
                    pos: (end_x, y),
                    terminated_with_newline: line_has_newline,
                });

                if line_has_newline && line_content.chars().count() > 0 {
                    let newline_idx = view_end_idx.saturating_sub(1);
                    if let Some(Some(src_newline)) = view_mapping.get(newline_idx) {
                        if *src_newline == primary_cursor_position {
                            // Cursor position now includes gutter width (consistent with main cursor tracking)
                            // For empty lines (just newline), cursor should be at gutter width (after gutter)
                            // For lines with content, cursor on newline should be after the content
                            if line_content.chars().count() == 1 {
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

        while lines.len() < render_area.height as usize {
            lines.push(Line::raw(""));
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
    fn render_buffer_in_split(
        frame: &mut Frame,
        state: &mut EditorState,
        event_log: Option<&mut EventLog>,
        area: Rect,
        is_active: bool,
        theme: &crate::theme::Theme,
        ansi_background: Option<&AnsiBackground>,
        background_fade: f32,
        lsp_waiting: bool,
        view_mode: ViewMode,
        compose_width: Option<u16>,
        compose_column_guides: Option<Vec<u16>>,
        view_transform: Option<ViewTransformPayload>,
        estimated_line_length: usize,
        _buffer_id: BufferId,
        hide_cursor: bool,
    ) {
        let _span = tracing::trace_span!("render_buffer_in_split").entered();

        let line_wrap = state.viewport.line_wrap_enabled;

        let overlay_count = state.overlays.all().len();
        if overlay_count > 0 {
            tracing::trace!("render_content: {} overlays present", overlay_count);
        }

        let visible_count = state.viewport.visible_line_count();

        let buffer_len = state.buffer.len();
        let estimated_lines = (buffer_len / 80).max(1);
        state.margins.update_width_for_buffer(estimated_lines);
        let gutter_width = state.margins.left_total_width();

        let compose_layout = Self::calculate_compose_layout(area, &view_mode, compose_width);
        let render_area = compose_layout.render_area;

        let view_data = Self::build_view_data(
            state,
            view_transform,
            estimated_line_length,
            visible_count,
            line_wrap,
            render_area.width as usize,
            gutter_width,
        );
        let view_anchor =
            Self::calculate_view_anchor(&view_data.lines, &view_data.mapping, state.viewport.top_byte);
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

        let starting_line_num =
            state
                .buffer
                .populate_line_cache(state.viewport.top_byte, visible_count);

        let viewport_start = state.viewport.top_byte;
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
        );

        let render_output = Self::render_view_lines(LineRenderInput {
            state,
            theme,
            view_lines: &view_data.lines,
            view_mapping: &view_data.mapping,
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
        });

        let mut lines = render_output.lines;
        let background_x_offset = state.viewport.left_column as usize;

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
        frame.render_widget(
            Paragraph::new(lines).block(Block::default().borders(Borders::NONE)),
            render_area,
        );

        // Render column guides if present (for tables, etc.)
        if let Some(guides) = compose_column_guides {
            let guide_style = Style::default()
                .fg(theme.line_number_fg)
                .add_modifier(Modifier::DIM);
            let guide_height = render_output.content_lines_rendered.min(render_area.height as usize);

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
    use crate::text_buffer::Buffer;
    use crate::theme::Theme;

    fn render_output_for(
        content: &str,
        cursor_pos: usize,
    ) -> (LineRenderOutput, usize, bool, usize) {
        let mut state = EditorState::new(20, 6, 1024);
        state.buffer = Buffer::from_str(content, 1024);
        state.cursors.primary_mut().position = cursor_pos.min(state.buffer.len());
        state.viewport.resize(20, 4);
        // Disable line numbers/gutters for simpler cursor position testing
        state.margins.left_config.enabled = false;

        let render_area = Rect::new(0, 0, 20, 4);
        let visible_count = state.viewport.visible_line_count();
        let gutter_width = state.margins.left_total_width();

        let view_data = SplitRenderer::build_view_data(
            &mut state,
            None,
            content.len().max(1),
            visible_count,
            false, // line wrap disabled for tests
            render_area.width as usize,
            gutter_width,
        );
        let view_anchor =
            SplitRenderer::calculate_view_anchor(&view_data.lines, &view_data.mapping, 0);

        let estimated_lines = (state.buffer.len() / 80).max(1);
        state.margins.update_width_for_buffer(estimated_lines);
        let gutter_width = state.margins.left_total_width();

        let selection = SplitRenderer::selection_context(&state);
        let starting_line_num =
            state
                .buffer
                .populate_line_cache(state.viewport.top_byte, visible_count);
        let viewport_start = state.viewport.top_byte;
        let viewport_end = SplitRenderer::calculate_viewport_end(
            &mut state,
            viewport_start,
            content.len().max(1),
            visible_count,
        );
        let decorations = SplitRenderer::decoration_context(
            &mut state,
            viewport_start,
            viewport_end,
            selection.primary_cursor_position,
        );

        let output = SplitRenderer::render_view_lines(LineRenderInput {
            state: &state,
            theme: &Theme::default(),
            view_lines: &view_data.lines,
            view_mapping: &view_data.mapping,
            view_anchor,
            render_area,
            gutter_width,
            selection: &selection,
            decorations: &decorations,
            starting_line_num,
            visible_line_count: visible_count,
            lsp_waiting: false,
            is_active: true,
            line_wrap: state.viewport.line_wrap_enabled,
            estimated_lines,
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
                if span.style.add_modifier.contains(ratatui::style::Modifier::REVERSED) {
                    // Found a visual cursor - record its position
                    cursor_positions.push((col, line_idx as u16));
                }
                // Count the width of this span's content
                col += span.content.chars().count() as u16;
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
                let has_reversed = span.style.add_modifier.contains(ratatui::style::Modifier::REVERSED);
                let bg_color = format!("{:?}", span.style.bg);
                eprintln!("    Span {}: {:?} (REVERSED: {}, BG: {})",
                    span_idx, span.content, has_reversed, bg_color);
            }
        }
        eprintln!("===================\n");
    }

    // Helper to get final cursor position after fallback resolution
    // Also validates that exactly one cursor is present
    fn get_final_cursor(content: &str, cursor_pos: usize) -> Option<(u16, u16)> {
        let (output, buffer_len, buffer_newline, cursor_pos) = render_output_for(content, cursor_pos);

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
        if all_cursors.len() > 1 || (all_cursors.len() == 1 && Some(all_cursors[0]) != final_cursor) {
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
    fn check_typing_at_cursor(content: &str, cursor_pos: usize, char_to_type: char) -> (Option<(u16, u16)>, String) {
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
        assert_eq!(new_content, "Xabc", "Typing should insert at cursor position");
        assert_eq!(cursor_pos, Some((0, 0)));
    }

    #[test]
    fn e2e_cursor_in_middle_of_line() {
        // "abc" with cursor at position 1 (on 'b')
        let cursor = get_final_cursor("abc", 1);
        assert_eq!(cursor, Some((1, 0)), "Cursor should be at column 1, line 0");

        let (cursor_pos, new_content) = check_typing_at_cursor("abc", 1, 'X');
        assert_eq!(new_content, "aXbc", "Typing should insert at cursor position");
        assert_eq!(cursor_pos, Some((1, 0)));
    }

    #[test]
    fn e2e_cursor_at_end_of_line_no_newline() {
        // "abc" with cursor at position 3 (after 'c', at EOF)
        let cursor = get_final_cursor("abc", 3);
        assert_eq!(cursor, Some((3, 0)), "Cursor should be at column 3, line 0 (after last char)");

        let (cursor_pos, new_content) = check_typing_at_cursor("abc", 3, 'X');
        assert_eq!(new_content, "abcX", "Typing should append at end");
        assert_eq!(cursor_pos, Some((3, 0)));
    }

    #[test]
    fn e2e_cursor_at_empty_line() {
        // "\n" with cursor at position 0 (on the newline itself)
        let cursor = get_final_cursor("\n", 0);
        assert_eq!(cursor, Some((0, 0)), "Cursor on empty line should be at column 0");

        let (cursor_pos, new_content) = check_typing_at_cursor("\n", 0, 'X');
        assert_eq!(new_content, "X\n", "Typing should insert before newline");
        assert_eq!(cursor_pos, Some((0, 0)));
    }

    #[test]
    fn e2e_cursor_after_newline_at_eof() {
        // "abc\n" with cursor at position 4 (after newline, at EOF)
        let cursor = get_final_cursor("abc\n", 4);
        assert_eq!(cursor, Some((0, 1)), "Cursor after newline at EOF should be on next line");

        let (cursor_pos, new_content) = check_typing_at_cursor("abc\n", 4, 'X');
        assert_eq!(new_content, "abc\nX", "Typing should insert on new line");
        assert_eq!(cursor_pos, Some((0, 1)));
    }

    #[test]
    fn e2e_cursor_on_newline_with_content() {
        // "abc\n" with cursor at position 3 (on the newline character)
        let cursor = get_final_cursor("abc\n", 3);
        assert_eq!(cursor, Some((3, 0)), "Cursor on newline after content should be after last char");

        let (cursor_pos, new_content) = check_typing_at_cursor("abc\n", 3, 'X');
        assert_eq!(new_content, "abcX\n", "Typing should insert before newline");
        assert_eq!(cursor_pos, Some((3, 0)));
    }

    #[test]
    fn e2e_cursor_multiline_start_of_second_line() {
        // "abc\ndef" with cursor at position 4 (start of second line, on 'd')
        let cursor = get_final_cursor("abc\ndef", 4);
        assert_eq!(cursor, Some((0, 1)), "Cursor at start of second line should be at column 0, line 1");

        let (cursor_pos, new_content) = check_typing_at_cursor("abc\ndef", 4, 'X');
        assert_eq!(new_content, "abc\nXdef", "Typing should insert at start of second line");
        assert_eq!(cursor_pos, Some((0, 1)));
    }

    #[test]
    fn e2e_cursor_multiline_end_of_first_line() {
        // "abc\ndef" with cursor at position 3 (on newline of first line)
        let cursor = get_final_cursor("abc\ndef", 3);
        assert_eq!(cursor, Some((3, 0)), "Cursor on newline of first line should be after content");

        let (cursor_pos, new_content) = check_typing_at_cursor("abc\ndef", 3, 'X');
        assert_eq!(new_content, "abcX\ndef", "Typing should insert before newline");
        assert_eq!(cursor_pos, Some((3, 0)));
    }

    #[test]
    fn e2e_cursor_empty_buffer() {
        // Empty buffer with cursor at position 0
        let cursor = get_final_cursor("", 0);
        assert_eq!(cursor, Some((0, 0)), "Cursor in empty buffer should be at origin");

        let (cursor_pos, new_content) = check_typing_at_cursor("", 0, 'X');
        assert_eq!(new_content, "X", "Typing in empty buffer should insert character");
        assert_eq!(cursor_pos, Some((0, 0)));
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
        assert_eq!(cursor, Some((3, 2)), "Cursor at EOF after 'i' should be at column 3, line 2");

        let (cursor_pos, new_content) = check_typing_at_cursor("abc\ndef\nghi", 11, 'X');
        assert_eq!(new_content, "abc\ndef\nghiX", "Typing should append at end");
        assert_eq!(cursor_pos, Some((3, 2)));
    }

    #[test]
    fn e2e_cursor_at_eof_with_trailing_newline() {
        // "abc\ndef\nghi\n" with cursor at position 12 (after trailing newline)
        let cursor = get_final_cursor("abc\ndef\nghi\n", 12);
        assert_eq!(cursor, Some((0, 3)), "Cursor after trailing newline should be on line 3");

        let (cursor_pos, new_content) = check_typing_at_cursor("abc\ndef\nghi\n", 12, 'X');
        assert_eq!(new_content, "abc\ndef\nghi\nX", "Typing should insert on new line");
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
        assert_eq!(cursor_at_eof, Some((3, 2)), "After Ctrl+End, cursor at column 3, line 2");

        // Type a character at EOF
        let (cursor_before_typing, new_content) = check_typing_at_cursor(content, 11, 'X');
        assert_eq!(cursor_before_typing, Some((3, 2)));
        assert_eq!(new_content, "abc\ndef\nghiX", "Character appended at end");

        // Verify cursor position in the new content
        let cursor_after_typing = get_final_cursor(&new_content, 12);
        assert_eq!(cursor_after_typing, Some((4, 2)), "After typing, cursor moved to column 4");

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
        assert_eq!(cursor_at_eof, Some((0, 3)), "After Ctrl+End, cursor at column 0, line 3 (new line)");

        // Type a character at EOF
        let (cursor_before_typing, new_content) = check_typing_at_cursor(content, 12, 'X');
        assert_eq!(cursor_before_typing, Some((0, 3)));
        assert_eq!(new_content, "abc\ndef\nghi\nX", "Character inserted on new line");

        // After typing, the cursor should move forward
        let cursor_after_typing = get_final_cursor(&new_content, 13);
        assert_eq!(cursor_after_typing, Some((1, 3)), "After typing, cursor should be at column 1, line 3");

        // Move cursor to middle of buffer - verify cursor is no longer at end
        let cursor_moved_away = get_final_cursor(&new_content, 4);
        assert_eq!(cursor_moved_away, Some((0, 1)), "Cursor moved to start of line 1 (position 4 = start of 'def')");
    }

    #[test]
    fn e2e_jump_to_end_of_empty_buffer() {
        // Edge case: Ctrl+End in empty buffer should stay at (0,0)
        let content = "";

        let cursor_at_eof = get_final_cursor(content, 0);
        assert_eq!(cursor_at_eof, Some((0, 0)), "Empty buffer: cursor at origin");

        // Type a character
        let (cursor_before_typing, new_content) = check_typing_at_cursor(content, 0, 'X');
        assert_eq!(cursor_before_typing, Some((0, 0)));
        assert_eq!(new_content, "X", "Character inserted");

        // Verify cursor after typing
        let cursor_after_typing = get_final_cursor(&new_content, 1);
        assert_eq!(cursor_after_typing, Some((1, 0)), "After typing, cursor at column 1");

        // Move cursor back to start - verify cursor is no longer at end
        let cursor_moved_away = get_final_cursor(&new_content, 0);
        assert_eq!(cursor_moved_away, Some((0, 0)), "Cursor moved back to start");
    }

    #[test]
    fn e2e_jump_to_end_of_single_empty_line() {
        // Edge case: buffer with just a newline
        let content = "\n";

        // Position 0 is ON the newline
        let cursor_on_newline = get_final_cursor(content, 0);
        assert_eq!(cursor_on_newline, Some((0, 0)), "Cursor on the newline character");

        // Position 1 is AFTER the newline (EOF)
        let cursor_at_eof = get_final_cursor(content, 1);
        assert_eq!(cursor_at_eof, Some((0, 1)), "After Ctrl+End, cursor on line 1");

        // Type at EOF
        let (cursor_before_typing, new_content) = check_typing_at_cursor(content, 1, 'X');
        assert_eq!(cursor_before_typing, Some((0, 1)));
        assert_eq!(new_content, "\nX", "Character on second line");

        let cursor_after_typing = get_final_cursor(&new_content, 2);
        assert_eq!(cursor_after_typing, Some((1, 1)), "After typing, cursor at column 1, line 1");

        // Move cursor to the newline - verify cursor is no longer at end
        let cursor_moved_away = get_final_cursor(&new_content, 0);
        assert_eq!(cursor_moved_away, Some((0, 0)), "Cursor moved to the newline on line 0");
    }
}
