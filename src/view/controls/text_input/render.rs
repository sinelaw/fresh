//! Text input rendering functions

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
    let scroll_offset = if state.cursor > inner_width {
        state.cursor - inner_width
    } else {
        0
    };

    let visible_text: String = display_text
        .chars()
        .skip(scroll_offset)
        .take(inner_width)
        .collect();

    let padded = format!("{:width$}", visible_text, width = inner_width);

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
        let cursor_x = input_start + 1 + (state.cursor - scroll_offset) as u16;
        if cursor_x < input_start + actual_field_width + 1 {
            let cursor_area = Rect::new(cursor_x, area.y, 1, 1);
            let cursor_char = state.value.chars().nth(state.cursor).unwrap_or(' ');
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
