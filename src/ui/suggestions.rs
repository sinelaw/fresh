//! Autocomplete suggestions and command palette UI rendering

use crate::prompt::Prompt;
use ratatui::layout::Rect;
use ratatui::style::{Color, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::Paragraph;
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
    pub fn render(frame: &mut Frame, area: Rect, prompt: &Prompt) {
        if prompt.suggestions.is_empty() {
            return;
        }

        let mut lines = Vec::new();
        let visible_count = area.height as usize;
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

            let style = if is_selected {
                // Highlight selected suggestion with cyan background
                Style::default().fg(Color::Black).bg(Color::Cyan)
            } else {
                // Normal suggestion with dark gray background
                Style::default().fg(Color::White).bg(Color::DarkGray)
            };

            lines.push(Line::from(Span::styled(text, style)));
        }

        let paragraph = Paragraph::new(lines);
        frame.render_widget(paragraph, area);
    }
}
