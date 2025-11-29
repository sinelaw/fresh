//! Autocomplete suggestions and command palette UI rendering

use crate::view::prompt::Prompt;
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
    ///
    /// # Returns
    /// * Optional tuple of (inner_rect, scroll_start_idx, visible_count, total_count) for mouse hit testing
    pub fn render(
        frame: &mut Frame,
        area: Rect,
        prompt: &Prompt,
        theme: &crate::view::theme::Theme,
    ) -> Option<(Rect, usize, usize, usize)> {
        Self::render_with_hover(frame, area, prompt, theme, None)
    }

    /// Render the suggestions popup with hover highlighting
    ///
    /// # Returns
    /// * Optional tuple of (inner_rect, scroll_start_idx, visible_count, total_count) for mouse hit testing
    pub fn render_with_hover(
        frame: &mut Frame,
        area: Rect,
        prompt: &Prompt,
        theme: &crate::view::theme::Theme,
        hover_target: Option<&crate::app::HoverTarget>,
    ) -> Option<(Rect, usize, usize, usize)> {
        if prompt.suggestions.is_empty() {
            return None;
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

        let visible_suggestions = &prompt.suggestions[start_idx..end_idx];

        // Fixed column layout: "  Name  |  Keybinding  |  Description"
        let left_margin = 2;
        let column_spacing = 2;
        let available_width = inner_area.width as usize;

        // Fixed column widths for consistent layout
        let name_column_width = 30; // Fixed width for command names
        let keybinding_column_width = 12; // Fixed width for keybindings (e.g., "Ctrl+Shift+P")

        for (idx, suggestion) in visible_suggestions.iter().enumerate() {
            let actual_idx = start_idx + idx;
            let is_selected = prompt.selected_suggestion == Some(actual_idx);
            let is_hovered = matches!(
                hover_target,
                Some(crate::app::HoverTarget::SuggestionItem(hovered_idx)) if *hovered_idx == actual_idx
            );

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
            } else if is_hovered {
                // Hover highlight
                Style::default()
                    .fg(theme.menu_hover_fg)
                    .bg(theme.menu_hover_bg)
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

            // Column 1: Command name (fixed width, truncate if too long)
            let name = &suggestion.text;
            let name_char_count = name.chars().count();
            let name_text = if name_char_count > name_column_width {
                // Truncate name if too long
                let truncated: String = name.chars().take(name_column_width - 1).collect();
                format!("{}…", truncated)
            } else {
                name.clone()
            };
            spans.push(Span::styled(name_text.clone(), base_style));
            let name_display_width = name_text.chars().count();
            let name_padding = name_column_width.saturating_sub(name_display_width);
            if name_padding > 0 {
                spans.push(Span::styled(" ".repeat(name_padding), base_style));
            }

            // Spacing before keybinding column
            spans.push(Span::styled(" ".repeat(column_spacing), base_style));

            // Column 2: Keyboard shortcut (fixed width)
            let keybinding_style = if suggestion.disabled {
                base_style
            } else if is_selected {
                Style::default()
                    .fg(theme.help_key_fg)
                    .bg(theme.suggestion_selected_bg)
            } else if is_hovered {
                Style::default()
                    .fg(theme.help_key_fg)
                    .bg(theme.menu_hover_bg)
            } else {
                Style::default()
                    .fg(theme.line_number_fg)
                    .bg(theme.suggestion_bg)
            };

            if let Some(keybinding) = &suggestion.keybinding {
                let kb_char_count = keybinding.chars().count();
                let kb_text = if kb_char_count > keybinding_column_width {
                    // Truncate keybinding if too long (unlikely but safe)
                    keybinding.chars().take(keybinding_column_width).collect()
                } else {
                    keybinding.clone()
                };
                spans.push(Span::styled(kb_text.clone(), keybinding_style));
                let kb_display_width = kb_text.chars().count();
                let kb_padding = keybinding_column_width.saturating_sub(kb_display_width);
                if kb_padding > 0 {
                    spans.push(Span::styled(" ".repeat(kb_padding), base_style));
                }
            } else {
                // No keybinding for this command, pad the column
                spans.push(Span::styled(" ".repeat(keybinding_column_width), base_style));
            }

            // Spacing before description column
            spans.push(Span::styled(" ".repeat(column_spacing), base_style));

            // Column 3: Description (takes remaining space)
            if let Some(desc) = &suggestion.description {
                // Calculate how much space we've used so far
                let used_width = left_margin
                    + name_column_width
                    + column_spacing
                    + keybinding_column_width
                    + column_spacing;

                // Only show description if we have enough space
                if used_width < available_width {
                    let remaining_width = available_width.saturating_sub(used_width);
                    // Use char count for truncation to avoid slicing in the middle of
                    // multi-byte UTF-8 characters (e.g., fancy quotes like ")
                    let desc_text = if desc.chars().count() > remaining_width {
                        // Truncate description if it's too long
                        let truncated: String = desc
                            .chars()
                            .take(remaining_width.saturating_sub(3))
                            .collect();
                        format!("{}...", truncated)
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

        // Return area info for mouse hit testing
        Some((
            inner_area,
            start_idx,
            visible_count,
            prompt.suggestions.len(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::input::commands::Suggestion;
    use crate::view::prompt::Prompt;
    use crate::view::theme::Theme;
    use ratatui::backend::TestBackend;
    use ratatui::Terminal;

    /// Regression test for UTF-8 truncation bug.
    ///
    /// The bug occurred when truncating a description containing multi-byte
    /// UTF-8 characters (like fancy quotes). The code used byte-based
    /// slicing which could cut in the middle of a multi-byte character,
    /// causing a panic.
    ///
    /// This test reliably reproduces the issue by:
    /// 1. Using a description with a fancy quote at a known position
    /// 2. Setting terminal width to force truncation at exactly that position
    #[test]
    fn test_suggestion_description_truncation_with_multibyte_utf8() {
        // The fancy quote \u{201C} is 3 bytes in UTF-8
        // Create a description where the quote appears at a position that will be truncated
        // 60 A's, then a fancy quote, then more text
        let fancy_quote = "\u{201C}"; // Left double quotation mark "
        let description = format!("{}{}test content after quote", "A".repeat(60), fancy_quote);

        // Verify the fancy quote is multi-byte
        assert_eq!(fancy_quote.len(), 3, "Fancy quote should be 3 bytes");
        assert_eq!(
            fancy_quote.chars().count(),
            1,
            "Fancy quote should be 1 char"
        );

        // Create a suggestion with this description
        let mut suggestion = Suggestion::new("Test Command".to_string());
        suggestion.description = Some(description.clone());

        // Create a prompt with this suggestion
        let mut prompt = Prompt::new(
            "Test: ".to_string(),
            crate::view::prompt::PromptType::Command,
        );
        prompt.suggestions = vec![suggestion];

        // Set up terminal with width that forces truncation at the multi-byte char
        // Column layout: "  Name  |  Keybinding  |  Description"
        // left_margin=2, name="Test Command"(12), column_spacing=2, no keybinding
        // used_width = 2 + 12 + 2 + 0 + 0 = 16
        // To truncate at position 63 (middle of the 3-byte quote at positions 60-62):
        // remaining_width = 63 + 3 = 66 (we subtract 3 for "...")
        // available_width = used_width + remaining_width = 16 + 66 = 82
        // Inner area width = 82, so total area with borders = 84
        let backend = TestBackend::new(84, 10);
        let mut terminal = Terminal::new(backend).unwrap();

        let theme = Theme::default();

        // This should NOT panic with the fix in place
        // Before the fix, this would panic with:
        // "byte index 63 is not a char boundary; it is inside '"' (bytes 60..63)"
        terminal
            .draw(|frame| {
                let area = Rect::new(0, 0, 84, 10);
                SuggestionsRenderer::render(frame, area, &prompt, &theme);
            })
            .unwrap();
    }

    /// Test that truncation produces valid UTF-8 output
    #[test]
    fn test_truncation_preserves_valid_utf8() {
        // Test with various multi-byte characters at different positions
        let test_cases = vec![
            // Fancy quotes (3 bytes each)
            "Create a \u{201C}virtual buffer\u{201D} for testing",
            // Emojis (4 bytes each)
            "Add emoji support \u{1F389} for better UX",
            // Japanese characters
            "\u{65E5}\u{672C}\u{8A9E} test with English",
            // Accented characters (2 bytes each)
            "Caf\u{00E9} r\u{00E9}sum\u{00E9} na\u{00EF}ve",
        ];

        for description in test_cases {
            let mut suggestion = Suggestion::new("Cmd".to_string());
            suggestion.description = Some(description.to_string());

            let mut prompt = Prompt::new(
                "Test: ".to_string(),
                crate::view::prompt::PromptType::Command,
            );
            prompt.suggestions = vec![suggestion];

            // Try various widths to catch any boundary issues
            for width in 20..100 {
                let backend = TestBackend::new(width, 5);
                let mut terminal = Terminal::new(backend).unwrap();
                let theme = Theme::default();

                // Should never panic regardless of width
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    terminal
                        .draw(|frame| {
                            let area = Rect::new(0, 0, width, 5);
                            SuggestionsRenderer::render(frame, area, &prompt, &theme);
                        })
                        .unwrap();
                }));

                assert!(
                    result.is_ok(),
                    "Panic at width {} with description: {}",
                    width,
                    description
                );
            }
        }
    }
}
