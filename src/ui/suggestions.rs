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

        // Calculate column widths for visible suggestions
        let visible_suggestions = &prompt.suggestions[start_idx..end_idx];

        let max_name_width = visible_suggestions
            .iter()
            .map(|s| s.text.len())
            .max()
            .unwrap_or(0);

        let max_keybinding_width = visible_suggestions
            .iter()
            .filter_map(|s| s.keybinding.as_ref().map(|k| k.len()))
            .max()
            .unwrap_or(0);

        // Column layout: "  Name  |  Keybinding  |  Description"
        let left_margin = 2;
        let column_spacing = 2;
        let available_width = inner_area.width as usize;

        for (idx, suggestion) in visible_suggestions.iter().enumerate() {
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

            // Build the line with three columns
            let mut spans = Vec::new();

            // Left margin
            spans.push(Span::styled(" ".repeat(left_margin), base_style));

            // Column 1: Command name (padded to max_name_width)
            let name = &suggestion.text;
            spans.push(Span::styled(name.clone(), base_style));
            let name_padding = max_name_width.saturating_sub(name.len());
            if name_padding > 0 {
                spans.push(Span::styled(" ".repeat(name_padding), base_style));
            }

            // Spacing before keybinding column
            spans.push(Span::styled(" ".repeat(column_spacing), base_style));

            // Column 2: Keyboard shortcut (padded to max_keybinding_width)
            if max_keybinding_width > 0 {
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

                if let Some(keybinding) = &suggestion.keybinding {
                    spans.push(Span::styled(keybinding.clone(), keybinding_style));
                    let keybinding_padding = max_keybinding_width.saturating_sub(keybinding.len());
                    if keybinding_padding > 0 {
                        spans.push(Span::styled(" ".repeat(keybinding_padding), base_style));
                    }
                } else {
                    // No keybinding for this command, pad the column
                    spans.push(Span::styled(" ".repeat(max_keybinding_width), base_style));
                }

                // Spacing before description column
                spans.push(Span::styled(" ".repeat(column_spacing), base_style));
            }

            // Column 3: Description (takes remaining space)
            if let Some(desc) = &suggestion.description {
                // Calculate how much space we've used so far
                let used_width = left_margin + max_name_width + column_spacing
                    + max_keybinding_width + column_spacing;

                // Only show description if we have enough space
                if used_width < available_width {
                    let remaining_width = available_width.saturating_sub(used_width);
                    let desc_text = if desc.len() > remaining_width {
                        // Truncate description if it's too long
                        format!("{}...", &desc[..remaining_width.saturating_sub(3)])
                    } else {
                        desc.clone()
                    };
                    spans.push(Span::styled(desc_text, base_style));
                }
            }

            // Fill remaining space with background
            let current_width: usize = spans.iter().map(|s| s.content.len()).sum();
            if current_width < available_width {
                spans.push(Span::styled(
                    " ".repeat(available_width.saturating_sub(current_width)),
                    base_style,
                ));
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
