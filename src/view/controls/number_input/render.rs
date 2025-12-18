//! Number input rendering functions

use ratatui::layout::Rect;
use ratatui::style::Style;
use ratatui::text::{Line, Span};
use ratatui::widgets::Paragraph;
use ratatui::Frame;

use super::{FocusState, NumberInputColors, NumberInputLayout, NumberInputState};

/// Render a number input control
///
/// # Arguments
/// * `frame` - The ratatui frame to render to
/// * `area` - Rectangle where the control should be rendered
/// * `state` - The number input state
/// * `colors` - Colors for rendering
///
/// # Returns
/// Layout information for hit testing
pub fn render_number_input(
    frame: &mut Frame,
    area: Rect,
    state: &NumberInputState,
    colors: &NumberInputColors,
) -> NumberInputLayout {
    render_number_input_aligned(frame, area, state, colors, None)
}

/// Render a number input control with optional label width alignment
///
/// # Arguments
/// * `frame` - The ratatui frame to render to
/// * `area` - Rectangle where the control should be rendered
/// * `state` - The number input state
/// * `colors` - Colors for rendering
/// * `label_width` - Optional minimum label width for alignment
///
/// # Returns
/// Layout information for hit testing
pub fn render_number_input_aligned(
    frame: &mut Frame,
    area: Rect,
    state: &NumberInputState,
    colors: &NumberInputColors,
    label_width: Option<u16>,
) -> NumberInputLayout {
    if area.height == 0 || area.width < 10 {
        return NumberInputLayout::default();
    }

    let (label_color, value_color, border_color, button_color) = match state.focus {
        FocusState::Normal => (colors.label, colors.value, colors.border, colors.button),
        FocusState::Focused => (colors.focused, colors.value, colors.focused, colors.focused),
        FocusState::Hovered => (colors.focused, colors.value, colors.focused, colors.focused),
        FocusState::Disabled => (
            colors.disabled,
            colors.disabled,
            colors.disabled,
            colors.disabled,
        ),
    };

    // Format: "Label: [ value ] [-] [+]"
    let value_str = state.display_text();
    let value_padded = if state.editing {
        format!("{}_", value_str)
    } else {
        format!("{:^5}", value_str)
    };

    let actual_label_width = label_width.unwrap_or(state.label.len() as u16);
    let padded_label = format!(
        "{:width$}",
        state.label,
        width = actual_label_width as usize
    );

    let line = Line::from(vec![
        Span::styled(padded_label, Style::default().fg(label_color)),
        Span::styled(": ", Style::default().fg(label_color)),
        Span::styled("[", Style::default().fg(border_color)),
        Span::styled(value_padded, Style::default().fg(value_color)),
        Span::styled("]", Style::default().fg(border_color)),
        Span::raw(" "),
        Span::styled("[-]", Style::default().fg(button_color)),
        Span::raw(" "),
        Span::styled("[+]", Style::default().fg(button_color)),
    ]);

    let paragraph = Paragraph::new(line);
    frame.render_widget(paragraph, area);

    let final_label_width = actual_label_width + 2;
    let value_start = area.x + final_label_width;
    let value_width = 7;

    let dec_start = value_start + value_width + 1;
    let dec_width = 3;

    let inc_start = dec_start + dec_width + 1;
    let inc_width = 3;

    NumberInputLayout {
        value_area: Rect::new(value_start, area.y, value_width, 1),
        decrement_area: Rect::new(dec_start, area.y, dec_width, 1),
        increment_area: Rect::new(inc_start, area.y, inc_width, 1),
        full_area: Rect::new(area.x, area.y, inc_start - area.x + inc_width, 1),
    }
}
