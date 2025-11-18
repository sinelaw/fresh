//! Split pane layout and buffer rendering

use crate::cursor::SelectionMode;
use crate::editor::BufferMetadata;
use crate::event::{BufferId, EventLog, SplitDirection};
use crate::line_wrapping::{char_position_to_segment, wrap_line, WrapConfig};
use crate::split::SplitManager;
use crate::state::EditorState;
use crate::ui::tabs::TabsRenderer;
use crate::virtual_text::VirtualTextPosition;
use ratatui::layout::Rect;
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Paragraph};
use ratatui::Frame;
use std::collections::HashMap;

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
        lsp_waiting: bool,
        large_file_threshold_bytes: u64,
        line_wrap: bool,
        estimated_line_length: usize,
        split_view_states: Option<&HashMap<crate::event::SplitId, crate::split::SplitViewState>>,
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

            // Reserve 1 line at top for tabs, 1 column on right for scrollbar
            let tabs_height = 1u16;
            let scrollbar_width = 1u16;

            // Tabs area at top of split
            let tabs_rect = Rect::new(
                split_area.x,
                split_area.y,
                split_area.width,
                tabs_height,
            );

            // Content area below tabs
            let content_rect = Rect::new(
                split_area.x,
                split_area.y + tabs_height,
                split_area.width.saturating_sub(scrollbar_width),
                split_area.height.saturating_sub(tabs_height),
            );

            // Scrollbar on the right side of content (not tabs)
            let scrollbar_rect = Rect::new(
                split_area.x + split_area.width.saturating_sub(scrollbar_width),
                split_area.y + tabs_height,
                scrollbar_width,
                split_area.height.saturating_sub(tabs_height),
            );

            // Get the open buffers for this split from split_view_states
            let split_buffers: Vec<BufferId> = if let Some(view_states) = split_view_states {
                if let Some(view_state) = view_states.get(&split_id) {
                    // Use the split's open_buffers list
                    view_state.open_buffers.clone()
                } else {
                    // No view state for this split - just show the current buffer
                    vec![buffer_id]
                }
            } else {
                // No view states at all - just show the current buffer
                vec![buffer_id]
            };

            // Render tabs for this split
            TabsRenderer::render_for_split(
                frame,
                tabs_rect,
                &split_buffers,
                buffers,
                buffer_metadata,
                buffer_id, // The currently displayed buffer in this split
                theme,
                is_active,
            );

            // Get references separately to avoid double borrow
            let state_opt = buffers.get_mut(&buffer_id);
            let event_log_opt = event_logs.get_mut(&buffer_id);

            if let Some(state) = state_opt {
                // For inactive splits, temporarily swap in the split's view state (cursors + viewport)
                // This allows each split to show its own cursor position
                let saved_cursors;
                let saved_viewport;
                if !is_active {
                    if let Some(view_states) = split_view_states {
                        if let Some(view_state) = view_states.get(&split_id) {
                            saved_cursors = Some(std::mem::replace(&mut state.cursors, view_state.cursors.clone()));
                            saved_viewport = Some(std::mem::replace(&mut state.viewport, view_state.viewport.clone()));
                        } else {
                            saved_cursors = None;
                            saved_viewport = None;
                        }
                    } else {
                        saved_cursors = None;
                        saved_viewport = None;
                    }
                } else {
                    saved_cursors = None;
                    saved_viewport = None;
                }

                // Sync viewport size with actual render area
                // This ensures the viewport matches the real available space,
                // automatically accounting for menu bar, tabs, status bar, etc.
                if state.viewport.width != content_rect.width
                    || state.viewport.height != content_rect.height
                {
                    state
                        .viewport
                        .resize(content_rect.width, content_rect.height);
                    // Re-ensure cursor is visible after resize
                    let primary = *state.cursors.primary();
                    state.viewport.ensure_visible(&mut state.buffer, &primary);
                }

                Self::render_buffer_in_split(
                    frame,
                    state,
                    event_log_opt,
                    content_rect,
                    is_active,
                    theme,
                    lsp_waiting,
                    line_wrap,
                    estimated_line_length,
                    buffer_id,
                );

                // For small files, count actual lines for accurate scrollbar
                // For large files, we'll use a constant thumb size
                // NOTE: Calculate scrollbar BEFORE restoring state to use this split's viewport
                let buffer_len = state.buffer.len();
                let (total_lines, top_line) = if buffer_len <= large_file_threshold_bytes as usize {
                    // Small file: count actual lines
                    let total_lines = if buffer_len > 0 {
                        // Get the line number of the last byte (which gives us total lines)
                        state.buffer.get_line_number(buffer_len.saturating_sub(1)) + 1
                    } else {
                        1
                    };

                    // Get the line number at the top of the viewport
                    let top_line = if state.viewport.top_byte < buffer_len {
                        state.buffer.get_line_number(state.viewport.top_byte)
                    } else {
                        0
                    };

                    (total_lines, top_line)
                } else {
                    // Large file: we'll use constant thumb size, so line count doesn't matter
                    (0, 0)
                };

                // Render scrollbar for this split and get thumb position
                // NOTE: Render scrollbar BEFORE restoring state to use this split's viewport
                let (thumb_start, thumb_end) = Self::render_scrollbar(
                    frame,
                    state,
                    scrollbar_rect,
                    is_active,
                    theme,
                    large_file_threshold_bytes,
                    total_lines,
                    top_line,
                );

                // Restore the original cursors and viewport after rendering content and scrollbar
                if let Some(cursors) = saved_cursors {
                    state.cursors = cursors;
                }
                if let Some(viewport) = saved_viewport {
                    state.viewport = viewport;
                }

                // Store the areas for mouse handling
                split_areas.push((
                    split_id,
                    buffer_id,
                    content_rect,
                    scrollbar_rect,
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

    /// Render a single buffer in a split pane
    fn render_buffer_in_split(
        frame: &mut Frame,
        state: &mut EditorState,
        event_log: Option<&mut EventLog>,
        area: Rect,
        is_active: bool,
        theme: &crate::theme::Theme,
        lsp_waiting: bool,
        line_wrap: bool,
        estimated_line_length: usize,
        _buffer_id: BufferId,
    ) {
        let _span = tracing::trace_span!("render_buffer_in_split").entered();

        // Debug: Log overlay count for diagnostics
        let overlay_count = state.overlays.all().len();
        if overlay_count > 0 {
            tracing::trace!("render_content: {} overlays present", overlay_count);
        }

        // Update margin width based on buffer size
        // Estimate total lines from buffer length (same as viewport.gutter_width)
        let buffer_len = state.buffer.len();
        let estimated_lines = (buffer_len / 80).max(1);
        state.margins.update_width_for_buffer(estimated_lines);

        // Calculate gutter width from margin manager
        let gutter_width = state.margins.left_total_width();

        let mut lines = Vec::new();

        // Collect all selection ranges from all cursors
        let selection_ranges: Vec<std::ops::Range<usize>> = state
            .cursors
            .iter()
            .filter_map(|(_, cursor)| cursor.selection_range())
            .collect();

        // Collect block selections as 2D rectangles (start_line, start_col, end_line, end_col)
        let block_selections: Vec<(usize, usize, usize, usize)> = state
            .cursors
            .iter()
            .filter_map(|(_, cursor)| {
                if cursor.selection_mode == SelectionMode::Block {
                    if let Some(anchor) = cursor.block_anchor {
                        // Convert cursor position to 2D coords
                        let cur_line = state.buffer.get_line_number(cursor.position);
                        let cur_line_start =
                            state.buffer.line_start_offset(cur_line).unwrap_or(0);
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

        // Collect all cursor positions (to avoid highlighting the cursor itself)
        // If show_cursors is false (e.g., for virtual buffers), use an empty list
        let cursor_positions: Vec<usize> = if state.show_cursors {
            state
                .cursors
                .iter()
                .map(|(_, cursor)| cursor.position)
                .collect()
        } else {
            Vec::new()
        };

        // Get primary cursor position - we won't apply REVERSED to it to preserve terminal cursor visibility
        // Even if show_cursors is false, we need to know where the primary cursor would be for viewport positioning
        let primary_cursor_position = state.cursors.primary().position;

        tracing::trace!(
            "Rendering buffer with {} cursors at positions: {:?}, primary at {}, is_active: {}, buffer_len: {}",
            cursor_positions.len(),
            cursor_positions,
            primary_cursor_position,
            is_active,
            state.buffer.len()
        );

        // Verify primary is in the list
        if !cursor_positions.contains(&primary_cursor_position) {
            tracing::warn!(
                "Primary cursor position {} not found in cursor_positions list: {:?}",
                primary_cursor_position,
                cursor_positions
            );
        }

        // Use line iterator starting from top_byte to render visible lines
        let visible_count = state.viewport.visible_line_count();

        // Pre-populate the line cache for the visible area
        let starting_line_num = state
            .buffer
            .populate_line_cache(state.viewport.top_byte, visible_count);

        // Compute syntax highlighting for the visible viewport (if highlighter exists)
        let viewport_start = state.viewport.top_byte;
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

        let highlight_spans = if let Some(highlighter) = &mut state.highlighter {
            highlighter.highlight_viewport(&state.buffer, viewport_start, viewport_end)
        } else {
            Vec::new()
        };

        // Compute semantic highlights for word occurrences under cursor
        let semantic_spans = state.semantic_highlighter.highlight_occurrences(
            &state.buffer,
            primary_cursor_position,
            viewport_start,
            viewport_end,
        );

        // Query overlays once for the entire viewport using interval tree
        // This is O(log N + k) instead of O(N * M) for per-character queries
        let viewport_overlays =
            state
                .overlays
                .query_viewport(viewport_start, viewport_end, &state.marker_list);

        // Build set of lines with diagnostic overlays for margin indicators
        // We use the resolved byte positions from viewport_overlays and convert to line numbers
        let diagnostic_lines: std::collections::HashSet<usize> = viewport_overlays
            .iter()
            .filter_map(|(overlay, range)| {
                // Check if this is a diagnostic overlay by ID prefix
                if let Some(id) = &overlay.id {
                    if id.starts_with("lsp-diagnostic-") {
                        // Convert byte position to line number
                        return Some(state.buffer.get_line_number(range.start));
                    }
                }
                None
            })
            .collect();

        // Query virtual texts for the viewport and build a lookup by position
        let virtual_text_lookup = state
            .virtual_texts
            .build_lookup(&state.marker_list, viewport_start, viewport_end);

        // Check if buffer is empty before creating iterator (to avoid borrow conflict)
        let is_empty_buffer = state.buffer.is_empty();

        let mut iter = state
            .buffer
            .line_iterator(state.viewport.top_byte, estimated_line_length);
        let mut lines_rendered = 0;

        // Track cursor position during rendering (eliminates duplicate line iteration)
        let mut cursor_screen_x = 0u16;
        let mut cursor_screen_y = 0u16;
        let mut cursor_found = false;

        loop {
            let (line_start, line_content) = if let Some(line_data) = iter.next() {
                line_data
            } else if is_empty_buffer && lines_rendered == 0 {
                // Special case: empty buffer should show line 1 with margin
                (0, String::new())
            } else {
                break;
            };

            if lines_rendered >= visible_count {
                break;
            }

            let current_line_num = starting_line_num + lines_rendered;
            lines_rendered += 1;

            // Apply horizontal scrolling - skip characters before left_column
            let left_col = state.viewport.left_column;

            // Build line with selection highlighting
            let mut line_spans = Vec::new();

            // Render left margin (indicators + line numbers + separator)
            if state.margins.left_config.enabled {
                // First column: render indicator or space
                // Check for diagnostic indicator on this line (computed dynamically from overlays)
                if diagnostic_lines.contains(&current_line_num) {
                    // Show diagnostic indicator
                    line_spans.push(Span::styled(
                        "●".to_string(),
                        Style::default().fg(ratatui::style::Color::Red),
                    ));
                } else {
                    // Show space (reserved for future indicators like breakpoints)
                    line_spans.push(Span::raw(" "));
                }

                // Next N columns: render line number (right-aligned)
                let margin_content = state.margins.render_line(
                    current_line_num,
                    crate::margin::MarginPosition::Left,
                    estimated_lines,
                );
                let (rendered_text, style_opt) =
                    margin_content.render(state.margins.left_config.width);

                // Use custom style if provided, otherwise use default theme color
                let margin_style =
                    style_opt.unwrap_or_else(|| Style::default().fg(theme.line_number_fg));

                line_spans.push(Span::styled(rendered_text, margin_style));

                // Render separator
                if state.margins.left_config.show_separator {
                    let separator_style = Style::default().fg(theme.line_number_fg);
                    line_spans.push(Span::styled(
                        state.margins.left_config.separator.clone(),
                        separator_style,
                    ));
                }
            }

            // Check if this line has any selected text
            let mut char_index = 0;

            // Debug: Log first line rendering with cursor info
            if lines_rendered == 0 && !cursor_positions.is_empty() {
                tracing::debug!(
                    "Rendering first line: line_start={}, line_len={}, left_col={}, cursor_positions={:?}",
                    line_start,
                    line_content.len(),
                    left_col,
                    cursor_positions
                );
            }

            // Performance optimization: For very long lines, only process visible characters
            // Calculate the maximum characters we might need to render based on screen width
            // For wrapped lines, we need enough characters to fill the visible viewport
            // For non-wrapped lines, we only need one screen width worth
            let visible_lines_remaining = visible_count.saturating_sub(lines_rendered);
            let max_visible_chars = if line_wrap {
                // With wrapping: might need chars for multiple wrapped lines
                // Be generous to avoid cutting off wrapped content
                (area.width as usize)
                    .saturating_mul(visible_lines_remaining.max(1))
                    .saturating_add(200)
            } else {
                // Without wrapping: only need one line worth of characters
                (area.width as usize).saturating_add(100)
            };
            let max_chars_to_process = left_col.saturating_add(max_visible_chars);

            let mut chars_iterator = line_content.chars().peekable();
            while let Some(ch) = chars_iterator.next() {
                let byte_pos = line_start + char_index;

                // Performance: skip expensive style calculations for characters beyond visible range
                if char_index > max_chars_to_process {
                    // Fast path: just count remaining characters without processing
                    // This is critical for performance with very long lines (e.g., 100KB single line)
                    char_index += ch.len_utf8();
                    for remaining_ch in chars_iterator.by_ref() {
                        char_index += remaining_ch.len_utf8();
                    }
                    break;
                }

                // Skip characters before left_column
                if char_index >= left_col {
                    // Check if this character is at a cursor position
                    let is_cursor = cursor_positions.contains(&byte_pos);

                    // Debug: Log when we find a cursor position
                    if is_cursor && is_active {
                        tracing::trace!(
                            "Found cursor at byte_pos={}, char_index={}, ch={:?}, is_active={}",
                            byte_pos,
                            char_index,
                            ch,
                            is_active
                        );
                    }

                    // Check if this character is in any selection range (but not at cursor position)
                    // Also check for block/rectangular selections
                    let is_in_block_selection = block_selections.iter().any(
                        |(start_line, start_col, end_line, end_col)| {
                            current_line_num >= *start_line
                                && current_line_num <= *end_line
                                && char_index >= *start_col
                                && char_index <= *end_col
                        },
                    );

                    let is_selected = !is_cursor
                        && (selection_ranges
                            .iter()
                            .any(|range| range.contains(&byte_pos))
                            || is_in_block_selection);

                    // Find syntax highlight color for this position
                    let highlight_color = highlight_spans
                        .iter()
                        .find(|span| span.range.contains(&byte_pos))
                        .map(|span| span.color);

                    // Find overlays at this position from the pre-queried viewport overlays
                    // This avoids expensive marker tree lookups for every character
                    let overlays: Vec<_> = viewport_overlays
                        .iter()
                        .filter(|(_, range)| range.contains(&byte_pos))
                        .map(|(overlay, _)| *overlay)
                        .collect();

                    // Build style by layering: base -> syntax -> semantic -> overlays -> selection
                    let mut style = if let Some(color) = highlight_color {
                        // Apply syntax highlighting
                        Style::default().fg(color)
                    } else {
                        // Default color from theme
                        Style::default().fg(theme.editor_fg)
                    };

                    // Apply semantic highlighting (word occurrences under cursor)
                    // This gives a subtle background to all instances of the word
                    if let Some(semantic_span) = semantic_spans
                        .iter()
                        .find(|span| span.range.contains(&byte_pos))
                    {
                        // Use the color from semantic highlight as background
                        style = style.bg(semantic_span.color);
                    }

                    // Apply overlay styles (in priority order, so higher priority overlays override)
                    use crate::overlay::OverlayFace;
                    for overlay in &overlays {
                        match &overlay.face {
                            OverlayFace::Underline {
                                color,
                                style: _underline_style,
                            } => {
                                // For now, we'll use color modifiers since ratatui doesn't have
                                // native wavy underlines. We'll add a colored underline modifier.
                                // TODO: Render actual wavy/dotted underlines in a second pass
                                tracing::trace!(
                                    "Applying underline overlay {:?} at byte {}: color={:?}",
                                    overlay.id,
                                    byte_pos,
                                    color
                                );
                                style = style.add_modifier(Modifier::UNDERLINED).fg(*color);
                            }
                            OverlayFace::Background { color } => {
                                tracing::trace!(
                                    "Applying background overlay {:?} at byte {}: color={:?}",
                                    overlay.id,
                                    byte_pos,
                                    color
                                );
                                style = style.bg(*color);
                            }
                            OverlayFace::Foreground { color } => {
                                tracing::trace!(
                                    "Applying foreground overlay {:?} at byte {}: color={:?}",
                                    overlay.id,
                                    byte_pos,
                                    color
                                );
                                style = style.fg(*color);
                            }
                            OverlayFace::Style {
                                style: overlay_style,
                            } => {
                                tracing::trace!(
                                    "Applying style overlay {:?} at byte {}",
                                    overlay.id,
                                    byte_pos
                                );
                                // Merge the overlay style
                                style = style.patch(*overlay_style);
                            }
                        }
                    }

                    // Selection overrides everything (use theme colors)
                    if is_selected {
                        style = Style::default().fg(theme.editor_fg).bg(theme.selection_bg);
                    }

                    // Cursor styling - make secondary cursors visible with reversed colors
                    // Don't apply REVERSED to primary cursor to preserve terminal cursor visibility
                    // For inactive splits, ALL cursors use a less pronounced color (no hardware cursor)
                    let is_secondary_cursor = is_cursor && byte_pos != primary_cursor_position;
                    if is_active {
                        // In active split: only reverse secondary cursors (primary uses hardware cursor)
                        if is_secondary_cursor {
                            tracing::trace!(
                                "Applying REVERSED modifier to secondary cursor at byte_pos={}, char={:?}",
                                byte_pos,
                                ch
                            );
                            style = style.add_modifier(Modifier::REVERSED);
                        }
                    } else if is_cursor {
                        // In inactive split: use less pronounced color for all cursors
                        tracing::trace!(
                            "Applying inactive cursor color at byte_pos={}, char={:?}",
                            byte_pos,
                            ch
                        );
                        style = style.fg(theme.editor_fg).bg(theme.inactive_cursor);
                    }

                    // Determine what character to display
                    let display_char = if is_cursor && lsp_waiting && is_active {
                        // Show LSP waiting indicator
                        "⋯"
                    } else if is_cursor && is_active && ch == '\n' {
                        // Show cursor on newline as a visible space (don't actually render \n which would break the line)
                        // We'll skip adding this to line_spans and handle it after the loop
                        ""
                    } else if ch == '\n' {
                        // Don't render the newline character itself - it's a line terminator
                        ""
                    } else {
                        &ch.to_string()
                    };

                    // Check for BeforeChar virtual texts at this position
                    if let Some(vtexts) = virtual_text_lookup.get(&byte_pos) {
                        for vtext in vtexts.iter().filter(|v| v.position == VirtualTextPosition::BeforeChar) {
                            // Add spacing: "hint_text " before the character
                            let text_with_space = format!("{} ", vtext.text);
                            line_spans.push(Span::styled(text_with_space, vtext.style));
                        }
                    }

                    // Only add non-empty spans
                    if !display_char.is_empty() {
                        if is_cursor && is_active {
                            tracing::trace!(
                                "Adding span with REVERSED cursor: display_char={:?}, has_reversed={}",
                                display_char,
                                style.add_modifier.contains(Modifier::REVERSED)
                            );
                        }
                        line_spans.push(Span::styled(display_char.to_string(), style));
                    }

                    // Check for AfterChar virtual texts at this position
                    if let Some(vtexts) = virtual_text_lookup.get(&byte_pos) {
                        for vtext in vtexts.iter().filter(|v| v.position == VirtualTextPosition::AfterChar) {
                            // Add spacing: " hint_text" after the character
                            let text_with_space = format!(" {}", vtext.text);
                            line_spans.push(Span::styled(text_with_space, vtext.style));
                        }
                    }

                    // If this is a cursor on a newline, we'll handle it after the char loop
                    // Only apply REVERSED for secondary cursors to preserve primary cursor visibility
                    // For inactive splits, use less pronounced color
                    if is_cursor && ch == '\n' {
                        let should_add_indicator = if is_active {
                            is_secondary_cursor // Only secondary cursors in active split
                        } else {
                            true // All cursors in inactive splits
                        };
                        if should_add_indicator {
                            // Add a visible cursor indicator (space with appropriate style)
                            let cursor_style = if is_active {
                                // Active split: use REVERSED for secondary cursors
                                Style::default()
                                    .fg(theme.editor_fg)
                                    .bg(theme.editor_bg)
                                    .add_modifier(Modifier::REVERSED)
                            } else {
                                // Inactive split: use less pronounced color
                                Style::default()
                                    .fg(theme.editor_fg)
                                    .bg(theme.inactive_cursor)
                            };
                            line_spans.push(Span::styled(" ", cursor_style));
                        }
                        // Primary cursor on newline will be shown by terminal hardware cursor (active split only)
                    }
                }

                char_index += ch.len_utf8();
            }

            // Note: We already handle cursors on newlines in the loop above.
            // For lines without newlines (last line or empty lines), check if cursor is at end
            let has_newline = line_content.ends_with('\n');
            if !has_newline {
                let line_end_pos = line_start + char_index;
                let cursor_at_end = cursor_positions.iter().any(|&pos| pos == line_end_pos);

                tracing::trace!(
                    "End-of-line check: line_start={}, char_index={}, line_end_pos={}, cursor_at_end={}, is_active={}",
                    line_start,
                    char_index,
                    line_end_pos,
                    cursor_at_end,
                    is_active
                );

                if cursor_at_end {
                    // Only add indicator for secondary cursors to preserve primary cursor visibility
                    // For inactive splits, use less pronounced color
                    let is_primary_at_end = line_end_pos == primary_cursor_position;
                    let should_add_indicator = if is_active {
                        !is_primary_at_end // Only secondary cursors in active split
                    } else {
                        true // All cursors in inactive splits
                    };
                    if should_add_indicator {
                        // Add a space character with appropriate style to show cursor at end of line
                        tracing::debug!(
                            "Adding cursor indicator at end of line, is_active={}, is_primary={}",
                            is_active,
                            is_primary_at_end
                        );
                        let cursor_style = if is_active {
                            // Active split: use REVERSED for secondary cursors
                            Style::default()
                                .fg(theme.editor_fg)
                                .bg(theme.editor_bg)
                                .add_modifier(Modifier::REVERSED)
                        } else {
                            // Inactive split: use less pronounced color
                            Style::default()
                                .fg(theme.editor_fg)
                                .bg(theme.inactive_cursor)
                        };
                        line_spans.push(Span::styled(" ", cursor_style));
                    }
                    // Primary cursor at end of line will be shown by terminal hardware cursor (active split only)
                }
            }

            // Always use wrap_line() - unifies wrapping and no-wrapping code paths
            // For no-wrap mode, we use infinite width so everything stays in one segment
            if !line_spans.is_empty() {
                let config = if line_wrap {
                    WrapConfig::new(area.width as usize, gutter_width, true)
                } else {
                    WrapConfig::no_wrap(gutter_width)
                };

                // Separate gutter spans from content spans
                // Count characters in gutter to find where content starts
                let mut gutter_char_count = 0;
                let mut gutter_span_count = 0;
                for span in &line_spans {
                    let span_len = span.content.chars().count();
                    if gutter_char_count + span_len <= gutter_width {
                        gutter_char_count += span_len;
                        gutter_span_count += 1;
                    } else {
                        break;
                    }
                }

                // Extract only the content spans (skip gutter spans)
                let content_spans = &line_spans[gutter_span_count..];

                // Extract text from content spans only (not gutter) for wrapping
                let line_text: String = content_spans.iter().map(|s| s.content.as_ref()).collect();

                // Wrap the line using the clean transformation
                let segments = wrap_line(&line_text, &config);

                // Check if primary cursor is on this line and calculate its position
                // Use line_content.len() (original line length) not line_text.len() (scrolled length)
                // to ensure we capture cursor even when it's past the horizontal scroll offset.
                //
                // For the upper bound check:
                // - If line ends with newline, cursor AT the newline belongs to next line (use <)
                // - If line has no newline, cursor can be at end of line (use <=)
                let line_has_newline = line_content.ends_with('\n');
                let line_end_exclusive = if line_has_newline {
                    line_start + line_content.len() - 1 // Exclude the newline
                } else {
                    line_start + line_content.len() // Include position at end
                };

                if !cursor_found
                    && primary_cursor_position >= line_start
                    && primary_cursor_position <= line_end_exclusive
                {
                    let column = primary_cursor_position.saturating_sub(line_start);

                    // For no-wrap mode with horizontal scrolling:
                    // segments were created from line_text (already scrolled), so we need to
                    // adjust column to be relative to the scrolled portion before calling
                    // char_position_to_segment, and NOT subtract left_col again afterward
                    let (segment_idx, col_in_segment) = if !line_wrap {
                        // Adjust column to be relative to scrolled text
                        let scrolled_column = column.saturating_sub(left_col);
                        char_position_to_segment(scrolled_column, &segments)
                    } else {
                        // For wrapped mode, column is already correct
                        char_position_to_segment(column, &segments)
                    };

                    // Calculate virtual text width before cursor position on this line
                    // This accounts for inlay hints that shift the visual cursor position
                    // Note: add 1 for the padding space we add during rendering
                    let mut virtual_text_offset: usize = 0;
                    for byte_pos in line_start..primary_cursor_position {
                        if let Some(vtexts) = virtual_text_lookup.get(&byte_pos) {
                            for vtext in vtexts {
                                // +1 for the padding space added during rendering
                                virtual_text_offset += vtext.text.chars().count() + 1;
                            }
                        }
                    }

                    // Cursor screen position relative to this line's rendered segments
                    // Note: cursor_screen_x is the column in the text content, NOT including
                    // the line number gutter (which gets added later at hardware cursor setting)
                    cursor_screen_x = (col_in_segment + virtual_text_offset) as u16;

                    // lines_rendered is 1-indexed (incremented before processing), but cursor position needs to be 0-indexed
                    cursor_screen_y = (lines_rendered - 1 + segment_idx) as u16;
                    cursor_found = true;
                }

                // Render each wrapped segment
                for (seg_idx, segment) in segments.iter().enumerate() {
                    let mut segment_spans = vec![];

                    // Add gutter for each segment
                    if seg_idx == 0 {
                        // First segment gets the actual gutter (line numbers, etc.)
                        segment_spans.extend_from_slice(&line_spans[..gutter_span_count]);
                    } else {
                        // Continuation lines get spaces in the gutter area
                        segment_spans.push(Span::raw(" ".repeat(gutter_width)));
                    }

                    // Note: horizontal scrolling is already applied when building line_spans
                    // (see the loop at line ~462 that skips chars before left_col)
                    // So we don't need to skip again here - just use the segment text as-is
                    let segment_text = segment.text.clone();

                    // Apply styles to segment (preserving syntax highlighting, selection, overlays, etc.)
                    let styled_spans = Self::apply_styles_to_segment(
                        &segment_text,
                        content_spans,
                        segment.start_char_offset,
                        if !line_wrap { left_col } else { 0 },
                    );
                    segment_spans.extend(styled_spans);

                    lines.push(Line::from(segment_spans));
                    lines_rendered += 1;

                    // Check if we've filled the viewport
                    if lines_rendered >= visible_count {
                        break;
                    }
                }

                // Adjust lines_rendered since we already incremented it in the outer loop
                lines_rendered = lines_rendered.saturating_sub(1);
            } else {
                // Empty line - just add the gutter
                lines.push(Line::from(line_spans));
            }

            // Break early if we've filled the viewport during wrapping
            if lines_rendered >= visible_count {
                break;
            }
        }

        // Handle cursor positioned after the last line (e.g., after pressing Enter at end of file)
        // The loop above only iterates over existing lines, but if cursor is at the very end
        // of the buffer after a newline, it represents a new empty line that needs to be rendered
        // with its margin/gutter
        if !cursor_found && primary_cursor_position == state.buffer.len() {
            // Check if buffer ends with newline (creating an implicit empty last line)
            let buffer_ends_with_newline = if state.buffer.len() > 0 {
                let last_char = state.get_text_range(state.buffer.len() - 1, state.buffer.len());
                last_char == "\n"
            } else {
                false
            };

            // If buffer ends with newline and we haven't filled viewport, render the empty last line
            if buffer_ends_with_newline && lines_rendered < visible_count {
                let current_line_num = starting_line_num + lines_rendered;

                let mut line_spans = Vec::new();

                // Render left margin for the empty last line
                if state.margins.left_config.enabled {
                    // First column: render indicator or space
                    // Check for diagnostic indicator on this line (computed dynamically from overlays)
                    if diagnostic_lines.contains(&current_line_num) {
                        line_spans.push(Span::styled(
                            "●".to_string(),
                            Style::default().fg(ratatui::style::Color::Red),
                        ));
                    } else {
                        line_spans.push(Span::raw(" "));
                    }

                    // Render line number
                    let margin_content = state.margins.render_line(
                        current_line_num,
                        crate::margin::MarginPosition::Left,
                        estimated_lines,
                    );
                    let (rendered_text, style_opt) =
                        margin_content.render(state.margins.left_config.width);

                    let margin_style =
                        style_opt.unwrap_or_else(|| Style::default().fg(theme.line_number_fg));

                    line_spans.push(Span::styled(rendered_text, margin_style));

                    // Render separator
                    if state.margins.left_config.show_separator {
                        let separator_style = Style::default().fg(theme.line_number_fg);
                        line_spans.push(Span::styled(
                            state.margins.left_config.separator.clone(),
                            separator_style,
                        ));
                    }
                }

                // Add the empty line to the paragraph
                lines.push(Line::from(line_spans));
                lines_rendered += 1;
            }

            // Cursor is at the end of the buffer - place it on the current line
            // cursor_screen_x is the column in the text content (NOT including gutter)
            // The gutter offset is added later when setting the hardware cursor
            cursor_screen_x = 0;
            cursor_screen_y = if lines_rendered > 0 {
                (lines_rendered - 1) as u16
            } else {
                0
            };
        }

        // Clear the area first to prevent rendering artifacts when switching buffers
        frame.render_widget(Clear, area);

        let paragraph = Paragraph::new(lines).block(Block::default().borders(Borders::NONE));

        frame.render_widget(paragraph, area);

        // Render cursor and log state (only for active split)
        // Only show hardware cursor if show_cursors is true for this buffer
        if is_active && state.show_cursors {
            // Use cursor position calculated during rendering (no need to call cursor_screen_position)
            let (x, y) = (cursor_screen_x, cursor_screen_y);

            tracing::trace!(
                "Setting hardware cursor to PRIMARY cursor position: ({}, {})",
                x,
                y
            );

            // Adjust for line numbers (gutter width is dynamic based on max line number)
            // and adjust Y for the content area offset (area.y accounts for tab bar)
            // NOTE: cursor_screen_x is already the column within the CONTENT (after gutter),
            // so we need to add gutter_width to account for the gutter that's rendered in the line
            let screen_x = area.x.saturating_add(x).saturating_add(gutter_width as u16);
            let screen_y = area.y.saturating_add(y);
            tracing::trace!(
                "Hardware cursor: area.x={}, area.y={}, gutter_width={}, cursor(x={},y={}) => screen({},{})",
                area.x,
                area.y,
                gutter_width,
                x,
                y,
                screen_x,
                screen_y
            );
            frame.set_cursor_position((screen_x, screen_y));

            // Log rendering state for debugging
            if let Some(event_log) = event_log {
                let cursor_pos = state.cursors.primary().position;
                let buffer_len = state.buffer.len();
                event_log.log_render_state(cursor_pos, screen_x, screen_y, buffer_len);
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
    fn apply_styles_to_segment(
        segment_text: &str,
        line_spans: &[Span<'static>],
        segment_start_offset: usize,
        _scroll_offset: usize,
    ) -> Vec<Span<'static>> {
        if line_spans.is_empty() {
            return vec![Span::raw(segment_text.to_string())];
        }

        let mut result_spans = Vec::new();
        let segment_chars: Vec<char> = segment_text.chars().collect();

        if segment_chars.is_empty() {
            return vec![Span::raw(String::new())];
        }

        // Build a map of character position -> style
        let mut char_styles: Vec<(char, Style)> = Vec::new();

        for span in line_spans {
            let span_text = span.content.as_ref();
            let style = span.style;

            for ch in span_text.chars() {
                char_styles.push((ch, style));
            }
        }

        // Extract the styles for this segment
        let mut current_text = String::new();
        let mut current_style = None;

        for (i, &ch) in segment_chars.iter().enumerate() {
            // segment_start_offset is relative to the line_text (which already accounts for scrolling),
            // so don't add scroll_offset again - it would double-count the horizontal scrolling
            let original_pos = segment_start_offset + i;

            let style_for_char = if original_pos < char_styles.len() {
                char_styles[original_pos].1
            } else {
                Style::default()
            };

            // If style changed, flush current span and start new one
            if let Some(prev_style) = current_style {
                if prev_style != style_for_char {
                    result_spans.push(Span::styled(current_text.clone(), prev_style));
                    current_text.clear();
                    current_style = Some(style_for_char);
                }
            } else {
                current_style = Some(style_for_char);
            }

            current_text.push(ch);
        }

        // Flush remaining text
        if !current_text.is_empty() {
            if let Some(style) = current_style {
                result_spans.push(Span::styled(current_text, style));
            }
        }

        if result_spans.is_empty() {
            vec![Span::raw(String::new())]
        } else {
            result_spans
        }
    }
}
