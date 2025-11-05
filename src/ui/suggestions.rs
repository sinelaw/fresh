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
        let start_idx = 0;
        let end_idx = visible_count.min(prompt.suggestions.len());

        for (idx, suggestion) in prompt.suggestions[start_idx..end_idx].iter().enumerate() {
            let actual_idx = start_idx + idx;
            let is_selected = prompt.selected_suggestion == Some(actual_idx);

            // Format: "Command Name - description"
            let text = if let Some(desc) = &suggestion.description {
                format!("  {}  -  {}", suggestion.text, desc)
            } else {
                format!("  {}", suggestion.text)
            };

            let style = if suggestion.disabled {
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

            lines.push(Line::from(Span::styled(text, style)));
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
