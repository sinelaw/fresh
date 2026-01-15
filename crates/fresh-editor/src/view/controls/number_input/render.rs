//! Number input rendering functions

use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
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

    let actual_label_width = label_width.unwrap_or(state.label.len() as u16);
    let padded_label = format!(
        "{:width$}",
        state.label,
        width = actual_label_width as usize
    );

    // Build value spans with cursor and selection support
    let value_spans = if state.editing() {
        build_editing_spans(&value_str, state, value_color, colors)
    } else {
        vec![Span::styled(
            format!("{:^5}", value_str),
            Style::default().fg(value_color),
        )]
    };

    let mut spans = vec![
        Span::styled(padded_label, Style::default().fg(label_color)),
        Span::styled(": ", Style::default().fg(label_color)),
        Span::styled("[", Style::default().fg(border_color)),
    ];
    spans.extend(value_spans);
    spans.extend(vec![
        Span::styled("]", Style::default().fg(border_color)),
        Span::raw(" "),
        Span::styled("[-]", Style::default().fg(button_color)),
        Span::raw(" "),
        Span::styled("[+]", Style::default().fg(button_color)),
    ]);

    let line = Line::from(spans);

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

/// Build spans for the editing value with cursor and selection highlighting
fn build_editing_spans(
    value: &str,
    state: &NumberInputState,
    value_color: ratatui::style::Color,
    colors: &NumberInputColors,
) -> Vec<Span<'static>> {
    let cursor_pos = state.cursor_col();
    let selection_range = state.selection_range();

    let normal_style = Style::default().fg(value_color);
    let cursor_style = Style::default()
        .fg(value_color)
        .add_modifier(Modifier::REVERSED);
    let selection_style = Style::default().fg(colors.value).bg(colors.focused);

    let chars: Vec<char> = value.chars().collect();
    let mut spans = Vec::new();

    if let Some((sel_start, sel_end)) = selection_range {
        // Render with selection highlighting
        // Text before selection
        if sel_start > 0 {
            let before: String = chars[..sel_start.min(chars.len())].iter().collect();
            spans.push(Span::styled(before, normal_style));
        }

        // Selected text
        let sel_end_clamped = sel_end.min(chars.len());
        if sel_start < sel_end_clamped {
            let selected: String = chars[sel_start..sel_end_clamped].iter().collect();
            spans.push(Span::styled(selected, selection_style));
        }

        // Text after selection
        if sel_end_clamped < chars.len() {
            let after: String = chars[sel_end_clamped..].iter().collect();
            spans.push(Span::styled(after, normal_style));
        }
    } else {
        // Render with cursor (no selection)
        // Text before cursor
        if cursor_pos > 0 && cursor_pos <= chars.len() {
            let before: String = chars[..cursor_pos].iter().collect();
            spans.push(Span::styled(before, normal_style));
        } else if cursor_pos == 0 {
            // Cursor at start, no text before
        } else {
            // Cursor beyond text - show all text
            spans.push(Span::styled(value.to_string(), normal_style));
        }

        // Cursor character (shown as reversed)
        if cursor_pos < chars.len() {
            let cursor_char = chars[cursor_pos].to_string();
            spans.push(Span::styled(cursor_char, cursor_style));

            // Text after cursor
            if cursor_pos + 1 < chars.len() {
                let after: String = chars[cursor_pos + 1..].iter().collect();
                spans.push(Span::styled(after, normal_style));
            }
        } else {
            // Cursor at end - show block cursor
            spans.push(Span::styled(" ", cursor_style));
        }
    }

    spans
}
