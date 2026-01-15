//! Text list rendering functions

use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::Paragraph;
use ratatui::Frame;

use super::{FocusState, TextListColors, TextListLayout, TextListRowLayout, TextListState};

/// Render a text list control
///
/// # Arguments
/// * `frame` - The ratatui frame to render to
/// * `area` - Rectangle where the control should be rendered
/// * `state` - The text list state
/// * `colors` - Colors for rendering
/// * `field_width` - Width of each text field
///
/// # Returns
/// Layout information for hit testing
pub fn render_text_list(
    frame: &mut Frame,
    area: Rect,
    state: &TextListState,
    colors: &TextListColors,
    field_width: u16,
) -> TextListLayout {
    if area.height == 0 || area.width < 10 {
        return TextListLayout::default();
    }

    let label_color = match state.focus {
        FocusState::Focused => colors.focused,
        FocusState::Hovered => colors.focused,
        FocusState::Disabled => colors.disabled,
        FocusState::Normal => colors.label,
    };

    let label_line = Line::from(vec![
        Span::styled(&state.label, Style::default().fg(label_color)),
        Span::raw(":"),
    ]);
    frame.render_widget(
        Paragraph::new(label_line),
        Rect::new(area.x, area.y, area.width, 1),
    );

    let mut rows = Vec::new();
    let mut y = area.y + 1;
    let indent = 2u16;
    let actual_field_width = field_width.min(area.width.saturating_sub(indent + 5));

    for (idx, item) in state.items.iter().enumerate() {
        if y >= area.y + area.height {
            break;
        }

        let is_focused = state.focused_item == Some(idx) && state.focus == FocusState::Focused;
        let (border_color, text_color) = if is_focused {
            (colors.focused, colors.text)
        } else if state.focus == FocusState::Disabled {
            (colors.disabled, colors.disabled)
        } else {
            (colors.border, colors.text)
        };

        let inner_width = actual_field_width.saturating_sub(2) as usize;
        let visible: String = item.chars().take(inner_width).collect();
        let padded = format!("{:width$}", visible, width = inner_width);

        let line = Line::from(vec![
            Span::raw(" ".repeat(indent as usize)),
            Span::styled("[", Style::default().fg(border_color)),
            Span::styled(padded, Style::default().fg(text_color)),
            Span::styled("]", Style::default().fg(border_color)),
            Span::raw(" "),
            Span::styled("[x]", Style::default().fg(colors.remove_button)),
        ]);

        let row_area = Rect::new(area.x, y, area.width, 1);
        frame.render_widget(Paragraph::new(line), row_area);

        if is_focused && state.cursor <= inner_width {
            let cursor_x = area.x + indent + 1 + state.cursor as u16;
            let cursor_char = item.chars().nth(state.cursor).unwrap_or(' ');
            let cursor_area = Rect::new(cursor_x, y, 1, 1);
            let cursor_span = Span::styled(
                cursor_char.to_string(),
                Style::default()
                    .fg(colors.cursor)
                    .add_modifier(Modifier::REVERSED),
            );
            frame.render_widget(Paragraph::new(Line::from(vec![cursor_span])), cursor_area);
        }

        rows.push(TextListRowLayout {
            text_area: Rect::new(area.x + indent, y, actual_field_width + 2, 1),
            button_area: Rect::new(area.x + indent + actual_field_width + 3, y, 3, 1),
            index: Some(idx),
        });

        y += 1;
    }

    if y < area.y + area.height {
        let is_focused = state.focused_item.is_none() && state.focus == FocusState::Focused;
        let (border_color, text_color) = if is_focused {
            (colors.focused, colors.text)
        } else if state.focus == FocusState::Disabled {
            (colors.disabled, colors.disabled)
        } else {
            (colors.border, colors.text)
        };

        let inner_width = actual_field_width.saturating_sub(2) as usize;
        let visible: String = state.new_item_text.chars().take(inner_width).collect();
        let padded = format!("{:width$}", visible, width = inner_width);

        let line = Line::from(vec![
            Span::raw(" ".repeat(indent as usize)),
            Span::styled("[", Style::default().fg(border_color)),
            Span::styled(padded, Style::default().fg(text_color)),
            Span::styled("]", Style::default().fg(border_color)),
            Span::raw(" "),
            Span::styled("[+]", Style::default().fg(colors.add_button)),
        ]);

        let row_area = Rect::new(area.x, y, area.width, 1);
        frame.render_widget(Paragraph::new(line), row_area);

        if is_focused && state.cursor <= inner_width {
            let cursor_x = area.x + indent + 1 + state.cursor as u16;
            let cursor_char = state.new_item_text.chars().nth(state.cursor).unwrap_or(' ');
            let cursor_area = Rect::new(cursor_x, y, 1, 1);
            let cursor_span = Span::styled(
                cursor_char.to_string(),
                Style::default()
                    .fg(colors.cursor)
                    .add_modifier(Modifier::REVERSED),
            );
            frame.render_widget(Paragraph::new(Line::from(vec![cursor_span])), cursor_area);
        }

        rows.push(TextListRowLayout {
            text_area: Rect::new(area.x + indent, y, actual_field_width + 2, 1),
            button_area: Rect::new(area.x + indent + actual_field_width + 3, y, 3, 1),
            index: None,
        });
    }

    TextListLayout {
        rows,
        full_area: area,
    }
}
