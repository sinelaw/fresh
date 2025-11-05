//! Split pane layout and buffer rendering

use crate::event::{BufferId, EventLog, SplitDirection};
use crate::split::SplitManager;
use crate::state::EditorState;
use ratatui::layout::Rect;
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Paragraph};
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
    /// * `event_logs` - Event logs for each buffer
    /// * `theme` - The active theme for colors
    pub fn render_content(
        frame: &mut Frame,
        area: Rect,
        split_manager: &SplitManager,
        buffers: &mut HashMap<BufferId, EditorState>,
        event_logs: &mut HashMap<BufferId, EventLog>,
        theme: &crate::theme::Theme,
        lsp_waiting: bool,
    ) {
        let _span = tracing::trace_span!("render_content").entered();

        // Get all visible splits with their areas
        let visible_buffers = split_manager.get_visible_buffers(area);
        let active_split_id = split_manager.active_split();

        // Render each split
        for (split_id, buffer_id, split_area) in visible_buffers {
            let is_active = split_id == active_split_id;

            // Get references separately to avoid double borrow
            let state_opt = buffers.get_mut(&buffer_id);
            let event_log_opt = event_logs.get_mut(&buffer_id);

            if let Some(state) = state_opt {
                Self::render_buffer_in_split(frame, state, event_log_opt, split_area, is_active, theme, lsp_waiting);
            }
        }

        // Render split separators
        let separators = split_manager.get_separators(area);
        for (direction, x, y, length) in separators {
            Self::render_separator(frame, direction, x, y, length, theme);
        }
    }

    /// Render a split separator line
    fn render_separator(frame: &mut Frame, direction: SplitDirection, x: u16, y: u16, length: u16, theme: &crate::theme::Theme) {
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

    /// Render a single buffer in a split pane
    fn render_buffer_in_split(
        frame: &mut Frame,
        state: &mut EditorState,
        event_log: Option<&mut EventLog>,
        area: Rect,
        is_active: bool,
        theme: &crate::theme::Theme,
        lsp_waiting: bool,
    ) {
        let _span = tracing::trace_span!("render_buffer_in_split").entered();

        // Debug: Log overlay count for diagnostics
        let overlay_count = state.overlays.all().len();
        if overlay_count > 0 {
            tracing::debug!("render_content: {} overlays present", overlay_count);
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

        // Collect all cursor positions (to avoid highlighting the cursor itself)
        let cursor_positions: Vec<usize> = state
            .cursors
            .iter()
            .map(|(_, cursor)| cursor.position)
            .collect();

        // Use line iterator starting from top_byte to render visible lines
        let visible_count = state.viewport.visible_line_count();

        // Pre-populate the line cache for the visible area
        let starting_line_num = state
            .buffer
            .populate_line_cache(state.viewport.top_byte, visible_count);

        // Compute syntax highlighting for the visible viewport (if highlighter exists)
        let viewport_start = state.viewport.top_byte;
        let mut iter_temp = state.buffer.line_iterator(viewport_start);
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

        let mut iter = state.buffer.line_iterator(state.viewport.top_byte);
        let mut lines_rendered = 0;

        // For empty buffers, render at least one line with the margin
        let is_empty_buffer = state.buffer.is_empty();

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

            // Render left margin (line numbers + annotations)
            if state.margins.left_config.enabled {
                let margin_content = state.margins.render_line(
                    current_line_num,
                    crate::margin::MarginPosition::Left,
                    estimated_lines,
                );
                let (rendered_text, style_opt) = margin_content.render(state.margins.left_config.width);

                // Combine margin text with separator
                let margin_text = if state.margins.left_config.show_separator {
                    format!("{}{}", rendered_text, state.margins.left_config.separator)
                } else {
                    rendered_text
                };

                // Use custom style if provided, otherwise use default theme color
                let margin_style = style_opt.unwrap_or_else(|| {
                    Style::default().fg(theme.line_number_fg)
                });

                line_spans.push(Span::styled(margin_text, margin_style));
            }

            // Check if this line has any selected text
            let mut char_index = 0;
            for ch in line_content.chars() {
                let byte_pos = line_start + char_index;

                // Skip characters before left_column
                if char_index >= left_col {
                    // Check if this character is at a cursor position
                    let is_cursor = cursor_positions.contains(&byte_pos);

                    // Check if this character is in any selection range (but not at cursor position)
                    let is_selected = !is_cursor
                        && selection_ranges
                            .iter()
                            .any(|range| range.contains(&byte_pos));

                    // Find syntax highlight color for this position
                    let highlight_color = highlight_spans
                        .iter()
                        .find(|span| span.range.contains(&byte_pos))
                        .map(|span| span.color);

                    // Find overlays at this position (sorted by priority, low to high)
                    let overlays = state.overlays.at_position(byte_pos);

                    // Build style by layering: base -> syntax -> overlays -> selection
                    let mut style = if let Some(color) = highlight_color {
                        // Apply syntax highlighting
                        Style::default().fg(color)
                    } else {
                        // Default color from theme
                        Style::default().fg(theme.editor_fg)
                    };

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
                                    "Applying underline overlay at byte {}: color={:?}",
                                    byte_pos,
                                    color
                                );
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
                                // Merge the overlay style
                                style = style.patch(*overlay_style);
                            }
                        }
                    }

                    // Selection overrides everything (use theme colors)
                    if is_selected {
                        style = Style::default().fg(theme.editor_fg).bg(theme.selection_bg);
                    }

                    // Cursor styling - make cursors visible with reversed colors
                    if is_cursor && is_active {
                        style = style.add_modifier(Modifier::REVERSED);
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

                    // Only add non-empty spans
                    if !display_char.is_empty() {
                        line_spans.push(Span::styled(display_char.to_string(), style));
                    }

                    // If this is a cursor on a newline, we'll handle it after the char loop
                    if is_cursor && is_active && ch == '\n' {
                        // Add a visible cursor indicator (space with REVERSED style)
                        let cursor_style = Style::default()
                            .fg(theme.editor_fg)
                            .bg(theme.editor_bg)
                            .add_modifier(Modifier::REVERSED);
                        line_spans.push(Span::styled(" ", cursor_style));
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

                if cursor_at_end && is_active {
                    // Add a space character with REVERSED style to show cursor at end of line
                    let cursor_style = Style::default()
                        .fg(theme.editor_fg)
                        .bg(theme.editor_bg)
                        .add_modifier(Modifier::REVERSED);
                    line_spans.push(Span::styled(" ", cursor_style));
                }
            }

            lines.push(Line::from(line_spans));
        }

        let paragraph = Paragraph::new(lines).block(Block::default().borders(Borders::NONE));

        frame.render_widget(paragraph, area);

        // Render cursor and log state (only for active split)
        if is_active {
            let cursor_positions = state.cursor_positions();
            if let Some(&(x, y)) = cursor_positions.first() {
                // Adjust for line numbers (gutter width is dynamic based on max line number)
                // and adjust Y for the content area offset (area.y accounts for tab bar)
                let screen_x = area.x.saturating_add(x).saturating_add(gutter_width as u16);
                let screen_y = area.y.saturating_add(y);
                frame.set_cursor_position((screen_x, screen_y));

                // Log rendering state for debugging
                if let Some(event_log) = event_log {
                    let cursor_pos = state.cursors.primary().position;
                    let buffer_len = state.buffer.len();
                    event_log.log_render_state(cursor_pos, screen_x, screen_y, buffer_len);
                }
            }
        }
    }
}
