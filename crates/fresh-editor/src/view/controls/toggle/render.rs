//! Toggle rendering functions

use ratatui::layout::Rect;
use ratatui::style::Style;
use ratatui::text::{Line, Span};
use ratatui::widgets::Paragraph;
use ratatui::Frame;

use super::{FocusState, ToggleColors, ToggleLayout, ToggleState};

/// Render a toggle control
///
/// # Arguments
/// * `frame` - The ratatui frame to render to
/// * `area` - Rectangle where the toggle should be rendered
/// * `state` - The toggle state
/// * `colors` - Colors for rendering
///
/// # Returns
/// Layout information for hit testing
pub fn render_toggle(
    frame: &mut Frame,
    area: Rect,
    state: &ToggleState,
    colors: &ToggleColors,
) -> ToggleLayout {
    render_toggle_aligned(frame, area, state, colors, None)
}

/// Render a toggle control with optional label width alignment
///
/// # Arguments
/// * `frame` - The ratatui frame to render to
/// * `area` - Rectangle where the toggle should be rendered
/// * `state` - The toggle state
/// * `colors` - Colors for rendering
/// * `label_width` - Optional minimum label width for alignment
///
/// # Returns
/// Layout information for hit testing
pub fn render_toggle_aligned(
    frame: &mut Frame,
    area: Rect,
    state: &ToggleState,
    colors: &ToggleColors,
    label_width: Option<u16>,
) -> ToggleLayout {
    if area.height == 0 || area.width < 4 {
        return ToggleLayout {
            checkbox_area: Rect::default(),
            full_area: area,
        };
    }

    let (bracket_color, _check_color, label_color) = match state.focus {
        FocusState::Normal => (colors.bracket, colors.checkmark, colors.label),
        FocusState::Focused => (colors.focused, colors.checkmark, colors.focused),
        FocusState::Hovered => (colors.focused, colors.checkmark, colors.focused),
        FocusState::Disabled => (colors.disabled, colors.disabled, colors.disabled),
    };

    let checkbox = if state.checked { "[x]" } else { "[ ]" };

    // Format: "Label: [x]" with optional padding
    let actual_label_width = label_width.unwrap_or(state.label.len() as u16);
    let padded_label = format!(
        "{:width$}",
        state.label,
        width = actual_label_width as usize
    );

    let line = Line::from(vec![
        Span::styled(padded_label, Style::default().fg(label_color)),
        Span::styled(": ", Style::default().fg(label_color)),
        Span::styled(checkbox, Style::default().fg(bracket_color)),
    ]);

    let paragraph = Paragraph::new(line);
    frame.render_widget(paragraph, area);

    // Checkbox position after label
    let checkbox_start = area.x + actual_label_width + 2; // label + ": "
    let checkbox_area = Rect::new(checkbox_start, area.y, 3.min(area.width), 1);

    // Full area is label + ": " + checkbox
    let full_width = (actual_label_width + 2 + 3).min(area.width);
    let full_area = Rect::new(area.x, area.y, full_width, 1);

    ToggleLayout {
        checkbox_area,
        full_area,
    }
}
