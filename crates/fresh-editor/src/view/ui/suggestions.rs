//! Autocomplete suggestions and command palette UI rendering

use crate::input::commands::CommandSource;
use crate::primitives::display_width::{char_width, str_width};
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

        // Fixed column layout: "  Name  |  Keybinding  |  Description  |  Source"
        let left_margin = 2;
        let column_spacing = 2;
        let available_width = inner_area.width as usize;

        // Check if any visible suggestions have keybinding or source
        // If not, we can use more space for the name column
        let has_keybinding = visible_suggestions.iter().any(|s| s.keybinding.is_some());
        let has_source = visible_suggestions.iter().any(|s| s.source.is_some());

        // Fixed column widths for consistent layout
        let keybinding_column_width = if has_keybinding { 12 } else { 0 };
        let source_column_width = if has_source { 15 } else { 0 };

        // Calculate name column width dynamically based on available space
        // Reserve space for: left_margin + name + spacing + keybinding + spacing + desc + spacing + source
        let reserved_for_other_columns = left_margin
            + column_spacing // after name
            + keybinding_column_width
            + (if has_keybinding { column_spacing } else { 0 }) // after keybinding
            + column_spacing // after desc
            + source_column_width;

        // Give name column a reasonable portion of remaining space
        // Minimum 30, but can expand if there's room and no keybinding/source
        let base_name_width = 30;
        let name_column_width = if !has_keybinding && !has_source {
            // For file finders etc., use up to 60% of available width for name
            let max_name_width = (available_width * 60 / 100).max(base_name_width);
            max_name_width.min(available_width.saturating_sub(reserved_for_other_columns))
        } else {
            base_name_width
        };

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
            let name_visual_width = str_width(name);
            let name_text = if name_visual_width > name_column_width {
                // Truncate name by visual width
                let truncate_at = name_column_width.saturating_sub(1); // -1 for "…"

                // For file paths (containing '/'), truncate from the beginning
                // to preserve the filename which is usually at the end
                if name.contains('/') || name.contains('\\') {
                    // Calculate how many chars to skip from the beginning
                    let mut total_width = 0;
                    let char_widths: Vec<(char, usize)> =
                        name.chars().map(|ch| (ch, char_width(ch))).collect();

                    // Find where to start to fit within truncate_at
                    let mut start_idx = 0;
                    for (i, &(_, w)) in char_widths.iter().enumerate().rev() {
                        if total_width + w <= truncate_at {
                            total_width += w;
                            start_idx = i;
                        } else {
                            break;
                        }
                    }

                    let truncated: String =
                        char_widths[start_idx..].iter().map(|(ch, _)| *ch).collect();
                    format!("…{}", truncated)
                } else {
                    // For non-paths, truncate from the end as before
                    let mut width = 0;
                    let truncated: String = name
                        .chars()
                        .take_while(|ch| {
                            let w = char_width(*ch);
                            if width + w <= truncate_at {
                                width += w;
                                true
                            } else {
                                false
                            }
                        })
                        .collect();
                    format!("{}…", truncated)
                }
            } else {
                name.clone()
            };
            spans.push(Span::styled(name_text.clone(), base_style));
            let name_display_width = str_width(&name_text);
            let name_padding = name_column_width.saturating_sub(name_display_width);
            if name_padding > 0 {
                spans.push(Span::styled(" ".repeat(name_padding), base_style));
            }

            // Column 2: Keyboard shortcut (only if any suggestions have keybindings)
            if has_keybinding {
                // Spacing before keybinding column
                spans.push(Span::styled(" ".repeat(column_spacing), base_style));

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
                    let kb_visual_width = str_width(keybinding);
                    let kb_text = if kb_visual_width > keybinding_column_width {
                        // Truncate keybinding by visual width
                        let mut width = 0;
                        keybinding
                            .chars()
                            .take_while(|ch| {
                                let w = char_width(*ch);
                                if width + w <= keybinding_column_width {
                                    width += w;
                                    true
                                } else {
                                    false
                                }
                            })
                            .collect()
                    } else {
                        keybinding.clone()
                    };
                    spans.push(Span::styled(kb_text.clone(), keybinding_style));
                    let kb_display_width = str_width(&kb_text);
                    let kb_padding = keybinding_column_width.saturating_sub(kb_display_width);
                    if kb_padding > 0 {
                        spans.push(Span::styled(" ".repeat(kb_padding), base_style));
                    }
                } else {
                    // No keybinding for this command, pad the column
                    spans.push(Span::styled(
                        " ".repeat(keybinding_column_width),
                        base_style,
                    ));
                }
            }

            // Spacing before description column
            spans.push(Span::styled(" ".repeat(column_spacing), base_style));

            // Calculate space used by fixed columns
            let fixed_columns_width = left_margin
                + name_column_width
                + column_spacing
                + (if has_keybinding {
                    keybinding_column_width + column_spacing
                } else {
                    0
                });

            // Reserve space for source column at the end (only if showing sources)
            let source_reserved = if has_source {
                column_spacing + source_column_width
            } else {
                0
            };

            // Column 3: Description (flexible width, leaves room for source)
            if let Some(desc) = &suggestion.description {
                // Only show description if we have enough space
                if fixed_columns_width + source_reserved < available_width {
                    let desc_width = available_width
                        .saturating_sub(fixed_columns_width)
                        .saturating_sub(source_reserved);
                    // Use visual width for truncation to handle double-width characters
                    let desc_visual_width = str_width(desc);
                    let desc_text = if desc_visual_width > desc_width {
                        // Truncate description by visual width
                        let truncate_at = desc_width.saturating_sub(3); // -3 for "..."
                        let mut width = 0;
                        let truncated: String = desc
                            .chars()
                            .take_while(|ch| {
                                let w = char_width(*ch);
                                if width + w <= truncate_at {
                                    width += w;
                                    true
                                } else {
                                    false
                                }
                            })
                            .collect();
                        format!("{}...", truncated)
                    } else {
                        desc.clone()
                    };
                    let desc_display_width = str_width(&desc_text);
                    spans.push(Span::styled(desc_text, base_style));
                    // Pad description to fill its allocated space
                    let desc_padding = desc_width.saturating_sub(desc_display_width);
                    if desc_padding > 0 {
                        spans.push(Span::styled(" ".repeat(desc_padding), base_style));
                    }
                }
            } else {
                // No description, but still need to pad to align source column
                let desc_width = available_width
                    .saturating_sub(fixed_columns_width)
                    .saturating_sub(source_reserved);
                if desc_width > 0 {
                    spans.push(Span::styled(" ".repeat(desc_width), base_style));
                }
            }

            // Column 4: Source (only if any suggestions have source info)
            if has_source {
                // Spacing before source column
                spans.push(Span::styled(" ".repeat(column_spacing), base_style));

                let source_style = if suggestion.disabled {
                    base_style
                } else if is_selected {
                    Style::default()
                        .fg(theme.line_number_fg)
                        .bg(theme.suggestion_selected_bg)
                        .add_modifier(Modifier::DIM)
                } else if is_hovered {
                    Style::default()
                        .fg(theme.line_number_fg)
                        .bg(theme.menu_hover_bg)
                        .add_modifier(Modifier::DIM)
                } else {
                    Style::default()
                        .fg(theme.line_number_fg)
                        .bg(theme.suggestion_bg)
                        .add_modifier(Modifier::DIM)
                };

                if let Some(source) = &suggestion.source {
                    let source_text = match source {
                        CommandSource::Builtin => "builtin".to_string(),
                        CommandSource::Plugin(name) => name.clone(),
                    };
                    let source_visual_width = str_width(&source_text);
                    let source_display = if source_visual_width > source_column_width {
                        // Truncate source by visual width
                        let truncate_at = source_column_width.saturating_sub(1); // -1 for "…"
                        let mut width = 0;
                        let truncated: String = source_text
                            .chars()
                            .take_while(|ch| {
                                let w = char_width(*ch);
                                if width + w <= truncate_at {
                                    width += w;
                                    true
                                } else {
                                    false
                                }
                            })
                            .collect();
                        format!("{}…", truncated)
                    } else {
                        source_text
                    };
                    let source_display_width = str_width(&source_display);
                    // Right-align the source text within its column
                    let source_padding = source_column_width.saturating_sub(source_display_width);
                    if source_padding > 0 {
                        spans.push(Span::styled(" ".repeat(source_padding), base_style));
                    }
                    spans.push(Span::styled(source_display, source_style));
                } else {
                    // No source info, just pad
                    spans.push(Span::styled(" ".repeat(source_column_width), base_style));
                }
            }

            // Fill any remaining space with background (shouldn't be needed but safe)
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
    use crate::view::theme;
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

        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();

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
                let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();

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
