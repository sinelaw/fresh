//! Text input rendering functions

use crate::primitives::display_width::{char_width, str_width};
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::Paragraph;
use ratatui::Frame;

use super::{FocusState, TextInputColors, TextInputLayout, TextInputState};

/// Render a text input control
///
/// # Arguments
/// * `frame` - The ratatui frame to render to
/// * `area` - Rectangle where the control should be rendered
/// * `state` - The text input state
/// * `colors` - Colors for rendering
/// * `field_width` - Width of the input field (not including label)
///
/// # Returns
/// Layout information for hit testing
pub fn render_text_input(
    frame: &mut Frame,
    area: Rect,
    state: &TextInputState,
    colors: &TextInputColors,
    field_width: u16,
) -> TextInputLayout {
    render_text_input_aligned(frame, area, state, colors, field_width, None)
}

/// Render a text input control with optional label width alignment
///
/// # Arguments
/// * `frame` - The ratatui frame to render to
/// * `area` - Rectangle where the control should be rendered
/// * `state` - The text input state
/// * `colors` - Colors for rendering
/// * `field_width` - Width of the input field (not including label)
/// * `label_width` - Optional minimum label width for alignment
///
/// # Returns
/// Layout information for hit testing
pub fn render_text_input_aligned(
    frame: &mut Frame,
    area: Rect,
    state: &TextInputState,
    colors: &TextInputColors,
    field_width: u16,
    label_width: Option<u16>,
) -> TextInputLayout {
    if area.height == 0 || area.width < 5 {
        return TextInputLayout::default();
    }

    let (label_color, text_color, border_color, placeholder_color) = match state.focus {
        FocusState::Normal => (colors.label, colors.text, colors.border, colors.placeholder),
        FocusState::Focused => (
            colors.focused,
            colors.text,
            colors.focused,
            colors.placeholder,
        ),
        FocusState::Hovered => (
            colors.focused,
            colors.text,
            colors.focused,
            colors.placeholder,
        ),
        FocusState::Disabled => (
            colors.disabled,
            colors.disabled,
            colors.disabled,
            colors.disabled,
        ),
    };

    let actual_label_width = label_width.unwrap_or(state.label.len() as u16);
    let final_label_width = actual_label_width + 2;
    let actual_field_width = field_width.min(area.width.saturating_sub(final_label_width + 2));

    let (display_text, is_placeholder) = if state.value.is_empty() && !state.placeholder.is_empty()
    {
        (&state.placeholder, true)
    } else {
        (&state.value, false)
    };

    let inner_width = actual_field_width.saturating_sub(2) as usize;

    // Calculate visual width of text before cursor for proper scrolling
    // state.cursor is a byte offset, we need the visual width
    let text_before_cursor = &state.value[..state.cursor.min(state.value.len())];
    let cursor_visual_pos = str_width(text_before_cursor);

    // Calculate scroll offset based on visual width
    let scroll_visual_offset = cursor_visual_pos.saturating_sub(inner_width);

    // Build visible text by iterating chars and tracking visual width
    let mut visible_text = String::new();
    let mut current_visual_pos = 0;
    for ch in display_text.chars() {
        let ch_width = char_width(ch);
        // Skip characters before scroll offset
        if current_visual_pos + ch_width <= scroll_visual_offset {
            current_visual_pos += ch_width;
            continue;
        }
        // Stop if we've filled the visible area
        if current_visual_pos - scroll_visual_offset >= inner_width {
            break;
        }
        visible_text.push(ch);
        current_visual_pos += ch_width;
    }

    // Pad to fill the field width
    let visible_width = str_width(&visible_text);
    let padding = " ".repeat(inner_width.saturating_sub(visible_width));
    let padded = format!("{}{}", visible_text, padding);

    let text_style = if is_placeholder {
        Style::default().fg(placeholder_color)
    } else {
        Style::default().fg(text_color)
    };

    let padded_label = format!(
        "{:width$}",
        state.label,
        width = actual_label_width as usize
    );

    let line = Line::from(vec![
        Span::styled(padded_label, Style::default().fg(label_color)),
        Span::styled(": ", Style::default().fg(label_color)),
        Span::styled("[", Style::default().fg(border_color)),
        Span::styled(padded, text_style),
        Span::styled("]", Style::default().fg(border_color)),
    ]);

    let paragraph = Paragraph::new(line);
    frame.render_widget(paragraph, area);

    let input_start = area.x + final_label_width;
    let input_area = Rect::new(input_start, area.y, actual_field_width + 2, 1);

    let cursor_pos = if state.focus == FocusState::Focused && !is_placeholder {
        // Calculate cursor visual position within the visible area
        let cursor_visual_in_field = cursor_visual_pos.saturating_sub(scroll_visual_offset);
        let cursor_x = input_start + 1 + cursor_visual_in_field as u16;
        if cursor_x < input_start + actual_field_width + 1 {
            let cursor_area = Rect::new(cursor_x, area.y, 1, 1);
            // Get the grapheme at cursor position for the highlight
            let cursor_char = if state.cursor < state.value.len() {
                crate::primitives::grapheme::grapheme_at(&state.value, state.cursor)
                    .map(|(g, _, _)| g.chars().next().unwrap_or(' '))
                    .unwrap_or(' ')
            } else {
                ' '
            };
            let cursor_span = Span::styled(
                cursor_char.to_string(),
                Style::default()
                    .fg(colors.cursor)
                    .add_modifier(Modifier::REVERSED),
            );
            frame.render_widget(Paragraph::new(Line::from(vec![cursor_span])), cursor_area);
            Some((cursor_x, area.y))
        } else {
            None
        }
    } else {
        None
    };

    TextInputLayout {
        input_area,
        full_area: Rect::new(
            area.x,
            area.y,
            input_start - area.x + actual_field_width + 2,
            1,
        ),
        cursor_pos,
    }
}
