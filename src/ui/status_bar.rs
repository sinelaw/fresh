//! Status bar and prompt/minibuffer rendering

use crate::keybindings::format_keybinding;
use crate::prompt::Prompt;
use crate::state::EditorState;
use crossterm::event::{KeyCode, KeyModifiers};
use ratatui::layout::Rect;
use ratatui::style::Style;
use ratatui::text::{Line, Span};
use ratatui::widgets::Paragraph;
use ratatui::Frame;

/// Renders the status bar and prompt/minibuffer
pub struct StatusBarRenderer;

impl StatusBarRenderer {
    /// Render the status bar or prompt/minibuffer
    ///
    /// If a prompt is active, renders the prompt instead of the status bar.
    ///
    /// # Arguments
    /// * `frame` - The ratatui frame to render to
    /// * `area` - The rectangular area to render in
    /// * `state` - The active buffer's editor state
    /// * `status_message` - Optional status message to display
    /// * `prompt` - Optional active prompt
    /// * `lsp_status` - LSP status indicator
    /// * `theme` - The active theme for colors
    /// * `display_name` - The display name for the file (project-relative path)
    pub fn render(
        frame: &mut Frame,
        area: Rect,
        state: &EditorState,
        status_message: &Option<String>,
        prompt: &Option<Prompt>,
        lsp_status: &str,
        theme: &crate::theme::Theme,
        display_name: &str,
    ) {
        // If we're in prompt mode, render the prompt instead of the status bar
        if let Some(prompt) = prompt {
            Self::render_prompt(frame, area, prompt, theme);
            return;
        }

        // Normal status bar rendering
        Self::render_status(
            frame,
            area,
            state,
            status_message,
            lsp_status,
            theme,
            display_name,
        );
    }

    /// Render the prompt/minibuffer
    fn render_prompt(frame: &mut Frame, area: Rect, prompt: &Prompt, theme: &crate::theme::Theme) {
        let base_style = Style::default().fg(theme.prompt_fg).bg(theme.prompt_bg);

        // Create spans for the prompt
        let mut spans = vec![Span::styled(prompt.message.clone(), base_style)];

        // If there's a selection, split the input into parts
        if let Some((sel_start, sel_end)) = prompt.selection_range() {
            let input = &prompt.input;

            // Text before selection
            if sel_start > 0 {
                spans.push(Span::styled(input[..sel_start].to_string(), base_style));
            }

            // Selected text (blue background for visibility, cursor remains visible)
            if sel_start < sel_end {
                // Use theme colors for selection to ensure consistency across themes
                let selection_style = Style::default()
                    .fg(theme.prompt_selection_fg)
                    .bg(theme.prompt_selection_bg);
                spans.push(Span::styled(
                    input[sel_start..sel_end].to_string(),
                    selection_style,
                ));
            }

            // Text after selection
            if sel_end < input.len() {
                spans.push(Span::styled(input[sel_end..].to_string(), base_style));
            }
        } else {
            // No selection, render entire input normally
            spans.push(Span::styled(prompt.input.clone(), base_style));
        }

        let line = Line::from(spans);
        let prompt_line = Paragraph::new(line).style(base_style);

        frame.render_widget(prompt_line, area);

        // Set cursor position in the prompt
        // Cursor should be at: message.len() + cursor_pos
        let cursor_x = (prompt.message.len() + prompt.cursor_pos) as u16;
        if cursor_x < area.width {
            frame.set_cursor_position((area.x + cursor_x, area.y));
        }
    }

    /// Render the normal status bar
    fn render_status(
        frame: &mut Frame,
        area: Rect,
        state: &EditorState,
        status_message: &Option<String>,
        lsp_status: &str,
        theme: &crate::theme::Theme,
        display_name: &str,
    ) {
        // Use the pre-computed display name from buffer metadata
        let filename = display_name;

        let modified = if state.buffer.is_modified() {
            " [+]"
        } else {
            ""
        };

        let cursor = *state.primary_cursor();

        // Get line number and column efficiently using cached values
        let (line, col) = {
            // Find the start of the line containing the cursor
            let cursor_iter = state.buffer.line_iterator(cursor.position);
            let line_start = cursor_iter.current_position();
            let col = cursor.position - line_start;

            // Use cached line number from state
            let line_num = state.primary_cursor_line_number.value();
            (line_num, col)
        };

        // Count diagnostics by severity
        let diagnostics = state.overlays.all();
        let mut error_count = 0;
        let mut warning_count = 0;
        let mut info_count = 0;

        for overlay in diagnostics {
            if let Some(id) = &overlay.id {
                if id.starts_with("lsp-diagnostic-") {
                    // Check priority to determine severity
                    // Based on lsp_diagnostics.rs: Error=100, Warning=50, Info=30, Hint=10
                    match overlay.priority {
                        100 => error_count += 1,
                        50 => warning_count += 1,
                        _ => info_count += 1,
                    }
                }
            }
        }

        // Build diagnostics summary if there are any
        let diagnostics_summary = if error_count + warning_count + info_count > 0 {
            let mut parts = Vec::new();
            if error_count > 0 {
                parts.push(format!("E:{}", error_count));
            }
            if warning_count > 0 {
                parts.push(format!("W:{}", warning_count));
            }
            if info_count > 0 {
                parts.push(format!("I:{}", info_count));
            }
            format!(" | {}", parts.join(" "))
        } else {
            String::new()
        };

        // Build cursor count indicator (only show if multiple cursors)
        let cursor_count_indicator = if state.cursors.count() > 1 {
            format!(" | {} cursors", state.cursors.count())
        } else {
            String::new()
        };

        // Build the status string with optional LSP status and status message
        let lsp_indicator = if !lsp_status.is_empty() {
            format!(" | {}", lsp_status)
        } else {
            String::new()
        };

        let left_status = if let Some(msg) = status_message {
            format!("{filename}{modified} | Ln {line}, Col {col}{diagnostics_summary}{cursor_count_indicator}{lsp_indicator} | {msg}")
        } else {
            format!("{filename}{modified} | Ln {line}, Col {col}{diagnostics_summary}{cursor_count_indicator}{lsp_indicator}")
        };

        // Build help indicator for right side
        // Use Ctrl+/ or ⌘+/ depending on platform
        // Always show help indicator on the right side
        let help_shortcut = format_keybinding(&KeyCode::Char('/'), &KeyModifiers::CONTROL);
        let help_indicator = format!("Help: {}", help_shortcut);
        let padded_help = format!(" {} ", help_indicator);

        // Calculate available width - always reserve space for help indicator
        let available_width = area.width as usize;
        let help_width = padded_help.len();

        // Only show help indicator if there's enough space (at least 15 chars for minimal display)
        let spans = if available_width >= 15 {
            // Reserve space for help indicator
            let left_max_width = if available_width > help_width + 1 {
                available_width - help_width - 1 // -1 for at least one space separator
            } else {
                1 // Minimal space
            };

            let mut spans = vec![];

            // Truncate left status if it's too long
            let displayed_left = if left_status.len() > left_max_width {
                let truncate_at = left_max_width.saturating_sub(3); // -3 for "..."
                if truncate_at > 0 {
                    format!("{}...", &left_status[..truncate_at])
                } else {
                    String::from("...")
                }
            } else {
                left_status.clone()
            };

            spans.push(Span::styled(
                displayed_left.clone(),
                Style::default()
                    .fg(theme.status_bar_fg)
                    .bg(theme.status_bar_bg),
            ));

            let displayed_left_len = displayed_left.len();

            // Add spacing to push help indicator to the right
            if displayed_left_len + help_width < available_width {
                let padding_len = available_width - displayed_left_len - help_width;
                spans.push(Span::styled(
                    " ".repeat(padding_len),
                    Style::default()
                        .fg(theme.status_bar_fg)
                        .bg(theme.status_bar_bg),
                ));
            } else if displayed_left_len < available_width {
                // Add minimal space
                spans.push(Span::styled(
                    " ",
                    Style::default()
                        .fg(theme.status_bar_fg)
                        .bg(theme.status_bar_bg),
                ));
            }

            // Add help indicator with distinct styling and padding
            spans.push(Span::styled(
                padded_help.clone(),
                Style::default()
                    .fg(theme.help_indicator_fg)
                    .bg(theme.help_indicator_bg),
            ));

            // Calculate total width covered by spans
            let total_width = displayed_left_len
                + if displayed_left_len + help_width < available_width {
                    available_width - displayed_left_len - help_width
                } else if displayed_left_len < available_width {
                    1
                } else {
                    0
                }
                + help_width;

            // Add final padding to fill exactly to area width if needed
            if total_width < available_width {
                spans.push(Span::styled(
                    " ".repeat(available_width - total_width),
                    Style::default()
                        .fg(theme.status_bar_fg)
                        .bg(theme.status_bar_bg),
                ));
            }

            spans
        } else {
            // Terminal too narrow or no help indicator - fill entire width with left status
            let mut spans = vec![];
            let displayed_left = if left_status.len() > available_width {
                let truncate_at = available_width.saturating_sub(3);
                if truncate_at > 0 {
                    format!("{}...", &left_status[..truncate_at])
                } else {
                    left_status.chars().take(available_width).collect()
                }
            } else {
                left_status.clone()
            };

            spans.push(Span::styled(
                displayed_left.clone(),
                Style::default()
                    .fg(theme.status_bar_fg)
                    .bg(theme.status_bar_bg),
            ));

            // Fill remaining width
            if displayed_left.len() < available_width {
                spans.push(Span::styled(
                    " ".repeat(available_width - displayed_left.len()),
                    Style::default()
                        .fg(theme.status_bar_fg)
                        .bg(theme.status_bar_bg),
                ));
            }

            spans
        };

        let status_line = Paragraph::new(Line::from(spans));

        frame.render_widget(status_line, area);
    }
}
