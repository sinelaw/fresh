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

    // When focused/hovered the chip sits on top of the row's highlight bg
    // (settings_selected_bg / menu_hover_bg). Use `focused_fg` for the
    // checkmark too — themes guarantee `focused_fg` contrasts with
    // `focused` (their bg), whereas `checkmark` is green-ish in most
    // themes and collides with green-tinted highlights (e.g. Nostalgia).
    let (bracket_color, check_color, label_color) = match state.focus {
        FocusState::Normal => (colors.bracket, colors.checkmark, colors.label),
        FocusState::Focused => (colors.focused_fg, colors.focused_fg, colors.focused_fg),
        FocusState::Hovered => (colors.focused_fg, colors.focused_fg, colors.focused_fg),
        FocusState::Disabled => (colors.disabled, colors.disabled, colors.disabled),
    };

    // Format: "Label: [v]" / "Label: [ ]" with optional padding.
    let actual_label_width = label_width.unwrap_or(state.label.len() as u16);
    let padded_label = format!(
        "{:width$}",
        state.label,
        width = actual_label_width as usize
    );

    // Compact checkbox glyph — matches the widget framework's
    // `[v]` / `[ ]` convention so an empty checkbox is not visually
    // confusable with an empty text input.
    //   checked:   [v]
    //   unchecked: [ ]
    const CHIP_WIDTH: u16 = 3;

    let line = if state.checked {
        Line::from(vec![
            Span::styled(padded_label, Style::default().fg(label_color)),
            Span::styled(": ", Style::default().fg(label_color)),
            Span::styled("[", Style::default().fg(bracket_color)),
            Span::styled(
                "v",
                Style::default()
                    .fg(check_color)
                    .add_modifier(ratatui::style::Modifier::BOLD),
            ),
            Span::styled("]", Style::default().fg(bracket_color)),
        ])
    } else {
        Line::from(vec![
            Span::styled(padded_label, Style::default().fg(label_color)),
            Span::styled(": ", Style::default().fg(label_color)),
            Span::styled("[", Style::default().fg(bracket_color)),
            Span::styled(" ", Style::default().fg(bracket_color)),
            Span::styled("]", Style::default().fg(bracket_color)),
        ])
    };

    let paragraph = Paragraph::new(line);
    frame.render_widget(paragraph, area);

    // Chip position after label (label + ": ").
    let label_overhead = actual_label_width.saturating_add(2);
    let checkbox_start = area.x.saturating_add(label_overhead);
    let chip_avail = area.width.saturating_sub(label_overhead.min(area.width));
    let checkbox_area = Rect::new(checkbox_start, area.y, CHIP_WIDTH.min(chip_avail), 1);

    // Full area is label + ": " + chip
    let full_width = (actual_label_width + 2 + CHIP_WIDTH).min(area.width);
    let full_area = Rect::new(area.x, area.y, full_width, 1);

    ToggleLayout {
        checkbox_area,
        full_area,
    }
}
