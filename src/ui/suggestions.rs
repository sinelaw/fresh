//! Autocomplete suggestions and command palette UI rendering

use crate::prompt::Prompt;
use ratatui::layout::Rect;
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Paragraph};
use ratatui::Frame;

/// Renders the autocomplete suggestions popup
pub struct SuggestionsRenderer;

impl SuggestionsRenderer {
    /// Render the suggestions popup (autocomplete/command palette)
    ///
    /// Displays a list of suggestions with the selected one highlighted.
    ///
    /// # Arguments
    /// * `frame` - The ratatui frame to render to
    /// * `area` - The rectangular area to render in
    /// * `prompt` - The active prompt containing suggestions
    /// * `theme` - The active theme for colors
    pub fn render(frame: &mut Frame, area: Rect, prompt: &Prompt, theme: &crate::theme::Theme) {
        if prompt.suggestions.is_empty() {
            return;
        }

        // Create a block with a border and background
        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(theme.popup_border_fg))
            .style(Style::default().bg(theme.suggestion_bg));

        let inner_area = block.inner(area);

        let mut lines = Vec::new();
        let visible_count = inner_area.height as usize;

        // Calculate scroll position to keep selected item visible
        let start_idx = if let Some(selected) = prompt.selected_suggestion {
            // Try to center the selected item, or at least keep it visible
            if selected < visible_count / 2 {
                // Near the top, start from beginning
                0
            } else if selected >= prompt.suggestions.len() - visible_count / 2 {
                // Near the bottom, show last page
                prompt.suggestions.len().saturating_sub(visible_count)
            } else {
                // In the middle, center the selected item
                selected.saturating_sub(visible_count / 2)
            }
        } else {
            0
        };

        let end_idx = (start_idx + visible_count).min(prompt.suggestions.len());

        for (idx, suggestion) in prompt.suggestions[start_idx..end_idx].iter().enumerate() {
            let actual_idx = start_idx + idx;
            let is_selected = prompt.selected_suggestion == Some(actual_idx);

            let base_style = if suggestion.disabled {
                // Greyed out disabled commands
                if is_selected {
                    Style::default()
                        .fg(Color::DarkGray)
                        .bg(theme.suggestion_selected_bg)
                        .add_modifier(Modifier::DIM)
                } else {
                    Style::default()
                        .fg(Color::DarkGray)
                        .bg(theme.suggestion_bg)
                        .add_modifier(Modifier::DIM)
                }
            } else if is_selected {
                // Highlight selected suggestion with theme colors
                Style::default()
                    .fg(theme.popup_text_fg)
                    .bg(theme.suggestion_selected_bg)
            } else {
                // Normal suggestion with theme colors
                Style::default()
                    .fg(theme.popup_text_fg)
                    .bg(theme.suggestion_bg)
            };

            // Build the line with keybinding aligned to the right
            let mut spans = Vec::new();

            // Format: "  Command Name  -  description"
            let main_text = if let Some(desc) = &suggestion.description {
                format!("  {}  -  {}", suggestion.text, desc)
            } else {
                format!("  {}", suggestion.text)
            };

            // Calculate padding to right-align keybinding
            let available_width = inner_area.width as usize;
            let keybinding_display = suggestion.keybinding.as_deref().unwrap_or("");
            let keybinding_len = keybinding_display.len();

            // Calculate space for padding (main_text + padding + keybinding + right_margin)
            let right_margin = 2;
            let text_and_keybinding_len = main_text.len() + keybinding_len + right_margin;

            if keybinding_len > 0 && text_and_keybinding_len < available_width {
                // Add main text
                spans.push(Span::styled(main_text.clone(), base_style));

                // Add padding to align keybinding to the right
                let padding_len = available_width.saturating_sub(text_and_keybinding_len);
                if padding_len > 0 {
                    spans.push(Span::styled(" ".repeat(padding_len), base_style));
                }

                // Add keybinding with slightly dimmed style
                let keybinding_style = if suggestion.disabled {
                    base_style
                } else if is_selected {
                    Style::default()
                        .fg(Color::Cyan)
                        .bg(theme.suggestion_selected_bg)
                } else {
                    Style::default()
                        .fg(Color::DarkGray)
                        .bg(theme.suggestion_bg)
                };
                spans.push(Span::styled(keybinding_display, keybinding_style));

                // Add right margin
                spans.push(Span::styled(" ".repeat(right_margin), base_style));
            } else {
                // No keybinding or not enough space, just show main text
                spans.push(Span::styled(main_text, base_style));
            }

            lines.push(Line::from(spans));
        }

        // Fill remaining lines with background color
        while lines.len() < visible_count {
            lines.push(Line::from(Span::styled(
                " ".repeat(inner_area.width as usize),
                Style::default().bg(theme.suggestion_bg),
            )));
        }

        let paragraph = Paragraph::new(lines).block(block);
        frame.render_widget(paragraph, area);
    }
}
