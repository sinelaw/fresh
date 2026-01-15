//! Button rendering functions

use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::Paragraph;
use ratatui::Frame;

use super::{ButtonColors, ButtonLayout, ButtonState, FocusState};

/// Render a button control
///
/// # Arguments
/// * `frame` - The ratatui frame to render to
/// * `area` - Rectangle where the button should be rendered
/// * `state` - The button state
/// * `colors` - Colors for rendering
///
/// # Returns
/// Layout information for hit testing
pub fn render_button(
    frame: &mut Frame,
    area: Rect,
    state: &ButtonState,
    colors: &ButtonColors,
) -> ButtonLayout {
    if area.height == 0 || area.width < 4 {
        return ButtonLayout::default();
    }

    let (text_color, border_color, bg_color) = match state.focus {
        FocusState::Normal => {
            if state.pressed {
                (colors.text, colors.border, Some(colors.pressed_bg))
            } else {
                (colors.text, colors.border, None)
            }
        }
        FocusState::Focused => {
            if state.pressed {
                (colors.text, colors.focused, Some(colors.pressed_bg))
            } else {
                (colors.focused, colors.focused, None)
            }
        }
        FocusState::Hovered => {
            // Hover uses dedicated hover color from theme
            (colors.hovered, colors.hovered, None)
        }
        FocusState::Disabled => (colors.disabled, colors.disabled, None),
    };

    // Calculate button width: "[ " + label + " ]"
    let button_width = (state.label.len() + 4) as u16;
    let actual_width = button_width.min(area.width);

    // Truncate label if needed
    let max_label_len = actual_width.saturating_sub(4) as usize;
    let display_label: String = state.label.chars().take(max_label_len).collect();

    let mut style = Style::default().fg(text_color);
    if let Some(bg) = bg_color {
        style = style.bg(bg);
    }
    if state.focus == FocusState::Focused {
        style = style.add_modifier(Modifier::BOLD);
    }

    let line = Line::from(vec![
        Span::styled("[", Style::default().fg(border_color)),
        Span::raw(" "),
        Span::styled(&display_label, style),
        Span::raw(" "),
        Span::styled("]", Style::default().fg(border_color)),
    ]);

    let button_area = Rect::new(area.x, area.y, actual_width, 1);
    let paragraph = Paragraph::new(line);
    frame.render_widget(paragraph, button_area);

    ButtonLayout { button_area }
}

/// Render a row of buttons with equal spacing
///
/// # Arguments
/// * `frame` - The ratatui frame to render to
/// * `area` - Rectangle where the buttons should be rendered
/// * `buttons` - Slice of (state, colors) tuples for each button
/// * `gap` - Space between buttons
///
/// # Returns
/// Layout information for each button
pub fn render_button_row(
    frame: &mut Frame,
    area: Rect,
    buttons: &[(&ButtonState, &ButtonColors)],
    gap: u16,
) -> Vec<ButtonLayout> {
    if buttons.is_empty() || area.height == 0 {
        return Vec::new();
    }

    let mut layouts = Vec::with_capacity(buttons.len());
    let mut x = area.x;

    for (state, colors) in buttons {
        let button_width = (state.label.len() + 4) as u16;
        if x + button_width > area.x + area.width {
            break;
        }

        let button_area = Rect::new(x, area.y, button_width, 1);
        let layout = render_button(frame, button_area, state, colors);
        layouts.push(layout);

        x += button_width + gap;
    }

    layouts
}
