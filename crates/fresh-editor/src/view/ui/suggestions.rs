//! Autocomplete suggestions and command palette UI rendering

use crate::input::commands::{CommandSource, Suggestion};
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
        Self::render_with_hover(frame, area, prompt, theme, None, true, true)
    }

    /// Render the suggestions popup with hover highlighting.
    /// `with_border` controls whether the popup draws its own
    /// frame; the floating-overlay prompt passes `false` because
    /// the overlay's outer frame already provides one and a
    /// nested border would create a visible double frame.
    ///
    /// # Returns
    /// * Optional tuple of (inner_rect, scroll_start_idx, visible_count, total_count) for mouse hit testing
    pub fn render_with_hover(
        frame: &mut Frame,
        area: Rect,
        prompt: &Prompt,
        theme: &crate::view::theme::Theme,
        hover_target: Option<&crate::app::HoverTarget>,
        with_border: bool,
        // When false, compute + return layout but skip emitting cells (the host
        // renders the palette from the semantic model). See UNIFIED_SCENE_DESIGN.md.
        draw: bool,
    ) -> Option<(Rect, usize, usize, usize)> {
        if prompt.suggestions.is_empty() {
            return None;
        }

        let block = Block::default()
            .borders(if with_border {
                Borders::ALL
            } else {
                Borders::NONE
            })
            .border_style(Style::default().fg(theme.popup_border_fg))
            .style(Style::default().bg(theme.suggestion_bg));

        let inner_area = block.inner(area);
        let visible_count = inner_area.height as usize;
        let available_width = inner_area.width as usize;

        // The scroll position is owned by the Prompt itself and only adjusted
        // when the selection moves out of the viewport (see
        // `Prompt::ensure_selected_visible`, called once before render). This
        // keeps a stable list under the cursor so a click near the bottom
        // doesn't trigger a recenter that shifts items mid-double-click.
        let (start_idx, end_idx) = visible_range(prompt, visible_count);
        let visible_suggestions = &prompt.suggestions[start_idx..end_idx];
        let layout = ColumnLayout::compute(visible_suggestions, available_width);

        let mut lines = Vec::with_capacity(visible_count);
        for (idx, suggestion) in visible_suggestions.iter().enumerate() {
            let actual_idx = start_idx + idx;
            let is_selected = prompt.selected_suggestion == Some(actual_idx);
            let is_hovered = matches!(
                hover_target,
                Some(crate::app::HoverTarget::SuggestionItem(hovered_idx)) if *hovered_idx == actual_idx
            );
            lines.push(render_row(
                suggestion,
                is_selected,
                is_hovered,
                &layout,
                available_width,
                theme,
            ));
        }

        // Fill remaining lines with background color
        while lines.len() < visible_count {
            lines.push(Line::from(Span::styled(
                " ".repeat(inner_area.width as usize),
                Style::default().bg(theme.suggestion_bg),
            )));
        }

        if draw {
            let paragraph = Paragraph::new(lines).block(block);
            frame.render_widget(paragraph, area);
        }

        // Return area info for mouse hit testing
        Some((
            inner_area,
            start_idx,
            visible_count,
            prompt.suggestions.len(),
        ))
    }
}

/// Compute the `[start, end)` slice of `prompt.suggestions` that fits in a
/// viewport of `visible_count` rows, honoring the prompt's owned scroll offset.
fn visible_range(prompt: &Prompt, visible_count: usize) -> (usize, usize) {
    let max_offset = prompt.suggestions.len().saturating_sub(visible_count);
    let start_idx = prompt.scroll_offset.min(max_offset);
    let end_idx = (start_idx + visible_count).min(prompt.suggestions.len());
    (start_idx, end_idx)
}

/// Fixed column layout for a suggestions popup row:
/// `"  Name  |  Keybinding  |  Description  |  Source"`.
///
/// Keybinding and source columns only appear when at least one visible
/// suggestion supplies them, freeing their width for the name otherwise.
struct ColumnLayout {
    left_margin: usize,
    column_spacing: usize,
    name_column_width: usize,
    keybinding_column_width: usize,
    source_column_width: usize,
    has_keybinding: bool,
    has_source: bool,
}

impl ColumnLayout {
    fn compute(visible_suggestions: &[Suggestion], available_width: usize) -> Self {
        let left_margin = 2;
        let column_spacing = 2;

        // Keybinding / source columns only show if some visible row has them.
        let has_keybinding = visible_suggestions.iter().any(|s| s.keybinding.is_some());
        let has_source = visible_suggestions.iter().any(|s| s.source.is_some());

        let keybinding_column_width = if has_keybinding { 12 } else { 0 };
        let source_column_width = if has_source { 15 } else { 0 };

        // Reserve space for: left_margin + name + spacing + keybinding + spacing + desc + spacing + source
        let reserved_for_other_columns = left_margin
            + column_spacing // after name
            + keybinding_column_width
            + (if has_keybinding { column_spacing } else { 0 }) // after keybinding
            + column_spacing // after desc
            + source_column_width;

        // Give the name column a portion of the remaining space, scaled with
        // terminal width so wide screens aren't wasted on descriptions.
        let base_name_width = 30;
        let actual_max_name_width = visible_suggestions
            .iter()
            .map(|s| str_width(&s.text))
            .max()
            .unwrap_or(0);
        let name_column_width = if !has_keybinding && !has_source {
            // For file finders etc., use up to 60% of available width for name,
            // but also cap to actual content width so descriptions get more room.
            let max_name_width = (available_width * 60 / 100).max(base_name_width);
            let content_based = actual_max_name_width.max(base_name_width);
            max_name_width
                .min(content_based)
                .min(available_width.saturating_sub(reserved_for_other_columns))
        } else {
            // Use ~30% of available width for the name, minimum 30.
            let dynamic_width = available_width * 30 / 100;
            dynamic_width.max(base_name_width)
        };

        Self {
            left_margin,
            column_spacing,
            name_column_width,
            keybinding_column_width,
            source_column_width,
            has_keybinding,
            has_source,
        }
    }

    /// Width consumed by the margin, name, and keybinding columns (everything
    /// left of the description).
    fn fixed_columns_width(&self) -> usize {
        self.left_margin
            + self.name_column_width
            + self.column_spacing
            + if self.has_keybinding {
                self.keybinding_column_width + self.column_spacing
            } else {
                0
            }
    }

    /// Width the source column reserves at the right edge of the row.
    fn source_reserved(&self) -> usize {
        if self.has_source {
            self.column_spacing + self.source_column_width
        } else {
            0
        }
    }
}

/// Render a single suggestion row into a styled [`Line`].
fn render_row(
    suggestion: &Suggestion,
    is_selected: bool,
    is_hovered: bool,
    layout: &ColumnLayout,
    available_width: usize,
    theme: &crate::view::theme::Theme,
) -> Line<'static> {
    let base_style = row_base_style(theme, suggestion.disabled, is_selected, is_hovered);
    let mut spans: Vec<Span<'static>> = Vec::new();

    // Left margin.
    spans.push(Span::styled(" ".repeat(layout.left_margin), base_style));

    push_name_column(&mut spans, &suggestion.text, layout, base_style);

    if layout.has_keybinding {
        push_keybinding_column(
            &mut spans,
            suggestion,
            layout,
            base_style,
            theme,
            is_selected,
            is_hovered,
        );
    }

    // Spacing before description column.
    spans.push(Span::styled(" ".repeat(layout.column_spacing), base_style));

    push_description_column(
        &mut spans,
        suggestion,
        layout,
        available_width,
        base_style,
        theme,
    );

    if layout.has_source {
        push_source_column(
            &mut spans,
            suggestion,
            layout,
            base_style,
            theme,
            is_selected,
            is_hovered,
        );
    }

    // Fill any remaining space with background (shouldn't be needed but safe).
    let current_width: usize = spans.iter().map(|s| s.content.len()).sum();
    if current_width < available_width {
        spans.push(Span::styled(
            " ".repeat(available_width.saturating_sub(current_width)),
            base_style,
        ));
    }

    Line::from(spans)
}

/// Column 1: the command name, truncated to the name column and left-padded
/// to fill it. Paths keep their trailing filename; other names keep their head.
fn push_name_column(
    spans: &mut Vec<Span<'static>>,
    name: &str,
    layout: &ColumnLayout,
    base_style: Style,
) {
    let width = layout.name_column_width;
    let name_text = if str_width(name) > width {
        // For file paths, keep the trailing filename; otherwise keep the head.
        if name.contains('/') || name.contains('\\') {
            truncate_head_ellipsis(name, width, "…")
        } else {
            truncate_tail_ellipsis(name, width, "…")
        }
    } else {
        name.to_string()
    };
    let display_width = str_width(&name_text);
    spans.push(Span::styled(name_text, base_style));
    let padding = width.saturating_sub(display_width);
    if padding > 0 {
        spans.push(Span::styled(" ".repeat(padding), base_style));
    }
}

/// Column 2: the keyboard shortcut (only emitted when [`ColumnLayout::has_keybinding`]).
fn push_keybinding_column(
    spans: &mut Vec<Span<'static>>,
    suggestion: &Suggestion,
    layout: &ColumnLayout,
    base_style: Style,
    theme: &crate::view::theme::Theme,
    is_selected: bool,
    is_hovered: bool,
) {
    // Spacing before keybinding column.
    spans.push(Span::styled(" ".repeat(layout.column_spacing), base_style));

    let style = keybinding_style(
        theme,
        base_style,
        suggestion.disabled,
        is_selected,
        is_hovered,
    );
    let width = layout.keybinding_column_width;
    if let Some(keybinding) = &suggestion.keybinding {
        // `truncate_end_to_width` returns the string unchanged when it fits.
        let kb_text = truncate_end_to_width(keybinding, width);
        let kb_display_width = str_width(&kb_text);
        spans.push(Span::styled(kb_text, style));
        let padding = width.saturating_sub(kb_display_width);
        if padding > 0 {
            spans.push(Span::styled(" ".repeat(padding), base_style));
        }
    } else {
        // No keybinding for this command, pad the column.
        spans.push(Span::styled(" ".repeat(width), base_style));
    }
}

/// Column 3: the description. A suggestion may carry styled `description_spans`
/// (rendered verbatim, each span with its own styling) or a plain `description`
/// string. Styled spans take precedence and let a row highlight part of itself
/// (e.g. the symbol word inside a code-line snippet).
fn push_description_column(
    spans: &mut Vec<Span<'static>>,
    suggestion: &Suggestion,
    layout: &ColumnLayout,
    available_width: usize,
    base_style: Style,
    theme: &crate::view::theme::Theme,
) {
    let fixed = layout.fixed_columns_width();
    let source_reserved = layout.source_reserved();
    let desc_width = available_width
        .saturating_sub(fixed)
        .saturating_sub(source_reserved);

    if let Some(segments) = &suggestion.description_spans {
        // Only render when there is room left of the source column.
        if fixed + source_reserved >= available_width {
            return;
        }
        let mut used = 0usize;
        for seg in segments {
            if used >= desc_width {
                break;
            }
            let seg_text = truncate_end_to_width(&seg.text, desc_width - used);
            if seg_text.is_empty() {
                continue;
            }
            used += str_width(&seg_text);
            let seg_style = styled_span_style(base_style, seg.style.as_ref(), theme);
            spans.push(Span::styled(seg_text, seg_style));
        }
        // Pad to fill the allocated space and align the source column.
        let padding = desc_width.saturating_sub(used);
        if padding > 0 {
            spans.push(Span::styled(" ".repeat(padding), base_style));
        }
    } else if let Some(desc) = &suggestion.description {
        // Only show description if we have enough space.
        if fixed + source_reserved >= available_width {
            return;
        }
        let desc_text = truncate_tail_ellipsis(desc, desc_width, "...");
        let display_width = str_width(&desc_text);
        spans.push(Span::styled(desc_text, base_style));
        let padding = desc_width.saturating_sub(display_width);
        if padding > 0 {
            spans.push(Span::styled(" ".repeat(padding), base_style));
        }
    } else if desc_width > 0 {
        // No description, but still need to pad to align the source column.
        spans.push(Span::styled(" ".repeat(desc_width), base_style));
    }
}

/// Column 4: the source label (builtin / plugin), right-aligned within its
/// column. Only emitted when [`ColumnLayout::has_source`].
fn push_source_column(
    spans: &mut Vec<Span<'static>>,
    suggestion: &Suggestion,
    layout: &ColumnLayout,
    base_style: Style,
    theme: &crate::view::theme::Theme,
    is_selected: bool,
    is_hovered: bool,
) {
    // Spacing before source column.
    spans.push(Span::styled(" ".repeat(layout.column_spacing), base_style));

    let style = source_style(
        theme,
        base_style,
        suggestion.disabled,
        is_selected,
        is_hovered,
    );
    let width = layout.source_column_width;
    if let Some(source) = &suggestion.source {
        let source_text = match source {
            CommandSource::Builtin => "builtin".to_string(),
            CommandSource::Plugin(name) => name.clone(),
        };
        let source_display = truncate_tail_ellipsis(&source_text, width, "…");
        let display_width = str_width(&source_display);
        // Right-align the source text within its column.
        let padding = width.saturating_sub(display_width);
        if padding > 0 {
            spans.push(Span::styled(" ".repeat(padding), base_style));
        }
        spans.push(Span::styled(source_display, style));
    } else {
        // No source info, just pad.
        spans.push(Span::styled(" ".repeat(width), base_style));
    }
}

/// Base style for a row, driving every column's default fg/bg.
fn row_base_style(
    theme: &crate::view::theme::Theme,
    disabled: bool,
    is_selected: bool,
    is_hovered: bool,
) -> Style {
    if disabled {
        // Greyed out disabled commands.
        let bg = if is_selected {
            theme.suggestion_selected_bg
        } else {
            theme.suggestion_bg
        };
        Style::default()
            .fg(Color::DarkGray)
            .bg(bg)
            .add_modifier(Modifier::DIM)
    } else if is_selected {
        Style::default()
            .fg(theme.popup_selection_fg)
            .bg(theme.suggestion_selected_bg)
    } else if is_hovered {
        Style::default()
            .fg(theme.menu_hover_fg)
            .bg(theme.menu_hover_bg)
    } else {
        Style::default()
            .fg(theme.popup_text_fg)
            .bg(theme.suggestion_bg)
    }
}

/// Style for the keybinding column text.
fn keybinding_style(
    theme: &crate::view::theme::Theme,
    base_style: Style,
    disabled: bool,
    is_selected: bool,
    is_hovered: bool,
) -> Style {
    if disabled {
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
    }
}

/// Style for the source column text (always dimmed).
fn source_style(
    theme: &crate::view::theme::Theme,
    base_style: Style,
    disabled: bool,
    is_selected: bool,
    is_hovered: bool,
) -> Style {
    if disabled {
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
    }
}

/// Truncate `text` so its visual width does not exceed `max_width`, keeping the
/// leading characters. Characters are kept whole, so multi-byte/wide characters
/// never split — this is what guards the UTF-8 boundary panic the tests cover.
/// Returns `text` unchanged when it already fits.
fn truncate_end_to_width(text: &str, max_width: usize) -> String {
    let mut width = 0;
    text.chars()
        .take_while(|ch| {
            let w = char_width(*ch);
            if width + w <= max_width {
                width += w;
                true
            } else {
                false
            }
        })
        .collect()
}

/// If `text` fits within `max_width`, return it unchanged; otherwise truncate
/// the tail and append `ellipsis`, keeping the whole result within `max_width`.
fn truncate_tail_ellipsis(text: &str, max_width: usize, ellipsis: &str) -> String {
    if str_width(text) <= max_width {
        return text.to_string();
    }
    let body = truncate_end_to_width(text, max_width.saturating_sub(str_width(ellipsis)));
    format!("{}{}", body, ellipsis)
}

/// Truncate `text` keeping the *trailing* characters (used for paths so the
/// filename at the end stays visible), prepending `ellipsis`. Intended for
/// callers that have already checked `text` exceeds `max_width`.
fn truncate_head_ellipsis(text: &str, max_width: usize, ellipsis: &str) -> String {
    let budget = max_width.saturating_sub(str_width(ellipsis));
    let char_widths: Vec<(char, usize)> = text.chars().map(|ch| (ch, char_width(ch))).collect();

    // Walk from the end, keeping whole characters until we exhaust the budget.
    let mut total_width = 0;
    let mut start_idx = 0;
    for (i, &(_, w)) in char_widths.iter().enumerate().rev() {
        if total_width + w <= budget {
            total_width += w;
            start_idx = i;
        } else {
            break;
        }
    }
    let tail: String = char_widths[start_idx..].iter().map(|(ch, _)| *ch).collect();
    format!("{}{}", ellipsis, tail)
}

/// Resolve an overlay color spec (RGB array or `"section.field"` theme key)
/// to a concrete [`Color`], using the active theme for key lookups.
fn overlay_spec_color(
    spec: &fresh_core::api::OverlayColorSpec,
    theme: &crate::view::theme::Theme,
) -> Option<Color> {
    if let Some((r, g, b)) = spec.as_rgb() {
        Some(Color::Rgb(r, g, b))
    } else if let Some(key) = spec.as_theme_key() {
        theme.resolve_theme_key(key)
    } else {
        None
    }
}

/// Build the style for a single description span: start from the row's
/// `base` style (so unset colors inherit the row/selection background) and
/// layer the span's own `fg`/`bg`/modifiers on top.
fn styled_span_style(
    base: Style,
    style: Option<&fresh_core::api::OverlayOptions>,
    theme: &crate::view::theme::Theme,
) -> Style {
    let mut s = base;
    let Some(opts) = style else {
        return s;
    };
    if let Some(fg) = &opts.fg {
        if let Some(c) = overlay_spec_color(fg, theme) {
            s = s.fg(c);
        }
    }
    if let Some(bg) = &opts.bg {
        if let Some(c) = overlay_spec_color(bg, theme) {
            s = s.bg(c);
        }
    }
    let mut modifiers = Modifier::empty();
    if opts.bold {
        modifiers |= Modifier::BOLD;
    }
    if opts.italic {
        modifiers |= Modifier::ITALIC;
    }
    if opts.underline {
        modifiers |= Modifier::UNDERLINED;
    }
    if opts.strikethrough {
        modifiers |= Modifier::CROSSED_OUT;
    }
    if !modifiers.is_empty() {
        s = s.add_modifier(modifiers);
    }
    s
}

#[cfg(test)]
mod tests {
    use super::*;
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
            crate::view::prompt::PromptType::QuickOpen,
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
                crate::view::prompt::PromptType::QuickOpen,
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
