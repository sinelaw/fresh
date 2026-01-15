//! Map control rendering functions

use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::Paragraph;
use ratatui::Frame;

use super::{FocusState, MapColors, MapEntryLayout, MapLayout, MapState};

/// Render a map control
pub fn render_map(
    frame: &mut Frame,
    area: Rect,
    state: &MapState,
    colors: &MapColors,
    key_width: u16,
) -> MapLayout {
    let empty_layout = MapLayout {
        full_area: area,
        entry_areas: Vec::new(),
        add_row_area: None,
    };

    if area.height == 0 || area.width < 15 {
        return empty_layout;
    }

    let label_color = match state.focus {
        FocusState::Focused => colors.focused,
        FocusState::Hovered => colors.focused,
        FocusState::Disabled => colors.disabled,
        FocusState::Normal => colors.label,
    };

    // Render label
    let label_line = Line::from(vec![
        Span::styled(&state.label, Style::default().fg(label_color)),
        Span::raw(":"),
    ]);
    frame.render_widget(
        Paragraph::new(label_line),
        Rect::new(area.x, area.y, area.width, 1),
    );

    let mut entry_areas = Vec::new();
    let mut y = area.y + 1;
    let indent = 2u16;
    let actual_key_width = key_width.min(area.width.saturating_sub(indent + 8));

    // Render entries
    for (idx, (key, value)) in state.entries.iter().enumerate() {
        if y >= area.y + area.height {
            break;
        }

        let is_focused = state.focused_entry == Some(idx) && state.focus == FocusState::Focused;
        let is_expanded = state.is_expanded(idx);

        let arrow = if is_expanded { "▼" } else { ">" };

        // Value preview using display_field if available
        let value_preview = state.get_display_value(value);
        // Truncate if too long
        let max_preview_len = 30;
        let value_preview = if value_preview.len() > max_preview_len {
            format!("{}...", &value_preview[..max_preview_len - 3])
        } else {
            value_preview
        };

        let row_area = Rect::new(area.x, y, area.width, 1);

        // Full row background highlight for focused entry
        if is_focused {
            let highlight_style = Style::default().bg(colors.focused);
            let bg_line = Line::from(Span::styled(
                " ".repeat(area.width as usize),
                highlight_style,
            ));
            frame.render_widget(Paragraph::new(bg_line), row_area);
        }

        // Row content with appropriate colors
        let (arrow_color, key_color, value_color) = if is_focused {
            (colors.label, colors.label, colors.value_preview)
        } else {
            (colors.expand_arrow, colors.key, colors.value_preview)
        };

        let base_style = if is_focused {
            Style::default().bg(colors.focused)
        } else {
            Style::default()
        };

        let mut spans = vec![
            Span::styled(" ".repeat(indent as usize), base_style),
            Span::styled(arrow, base_style.fg(arrow_color)),
            Span::raw(" "),
            Span::styled(
                format!("{:width$}", key, width = actual_key_width as usize),
                base_style.fg(key_color),
            ),
            Span::raw(" "),
            Span::styled(value_preview, base_style.fg(value_color)),
        ];

        // Add [Edit] hint for focused entry
        if is_focused {
            spans.push(Span::styled(
                "  [Enter to edit]",
                base_style
                    .fg(colors.value_preview)
                    .add_modifier(Modifier::DIM),
            ));
        }

        frame.render_widget(Paragraph::new(Line::from(spans)), row_area);

        entry_areas.push(MapEntryLayout {
            index: idx,
            row_area,
            expand_area: Rect::new(area.x + indent, y, 1, 1),
            key_area: Rect::new(area.x + indent + 2, y, actual_key_width, 1),
            remove_area: Rect::new(area.x + indent + 2 + actual_key_width + 22, y, 3, 1),
        });

        y += 1;

        // If expanded, show nested values (simplified view)
        if is_expanded && y < area.y + area.height {
            if let Some(obj) = value.as_object() {
                for (nested_key, nested_value) in obj.iter().take(5) {
                    if y >= area.y + area.height {
                        break;
                    }
                    let nested_preview = format_value_preview(nested_value, 15);
                    let nested_line = Line::from(vec![
                        Span::raw(" ".repeat((indent + 4) as usize)),
                        Span::styled(
                            format!("{}: ", nested_key),
                            Style::default().fg(colors.label),
                        ),
                        Span::styled(nested_preview, Style::default().fg(colors.value_preview)),
                    ]);
                    frame.render_widget(
                        Paragraph::new(nested_line),
                        Rect::new(area.x, y, area.width, 1),
                    );
                    y += 1;
                }
                if obj.len() > 5 && y < area.y + area.height {
                    let more_line = Line::from(Span::styled(
                        format!(
                            "{}... and {} more",
                            " ".repeat((indent + 4) as usize),
                            obj.len() - 5
                        ),
                        Style::default()
                            .fg(colors.value_preview)
                            .add_modifier(Modifier::ITALIC),
                    ));
                    frame.render_widget(
                        Paragraph::new(more_line),
                        Rect::new(area.x, y, area.width, 1),
                    );
                    y += 1;
                }
            }
        }
    }

    // Render "add new" row
    let add_row_area = if y < area.y + area.height {
        let is_focused = state.focused_entry.is_none() && state.focus == FocusState::Focused;
        let (border_color, text_color) = if is_focused {
            (colors.focused, colors.label)
        } else if state.focus == FocusState::Disabled {
            (colors.disabled, colors.disabled)
        } else {
            (colors.border, colors.label)
        };

        let inner_width = actual_key_width.saturating_sub(2) as usize;
        let visible: String = state.new_key_text.chars().take(inner_width).collect();
        let padded = format!("{:width$}", visible, width = inner_width);

        let line = Line::from(vec![
            Span::raw(" ".repeat(indent as usize)),
            Span::styled("[", Style::default().fg(border_color)),
            Span::styled(padded, Style::default().fg(text_color)),
            Span::styled("]", Style::default().fg(border_color)),
            Span::raw(" "),
            Span::styled("[+]", Style::default().fg(colors.add_button)),
            Span::raw(" Add entry..."),
        ]);

        let row_area = Rect::new(area.x, y, area.width, 1);
        frame.render_widget(Paragraph::new(line), row_area);

        // Render cursor if focused
        if is_focused && state.cursor <= inner_width {
            let cursor_x = area.x + indent + 1 + state.cursor as u16;
            let cursor_char = state.new_key_text.chars().nth(state.cursor).unwrap_or(' ');
            let cursor_area = Rect::new(cursor_x, y, 1, 1);
            let cursor_span = Span::styled(
                cursor_char.to_string(),
                Style::default()
                    .fg(colors.cursor)
                    .add_modifier(Modifier::REVERSED),
            );
            frame.render_widget(Paragraph::new(Line::from(vec![cursor_span])), cursor_area);
        }

        Some(row_area)
    } else {
        None
    };

    MapLayout {
        full_area: area,
        entry_areas,
        add_row_area,
    }
}

/// Format a JSON value as a short preview string
pub(super) fn format_value_preview(value: &serde_json::Value, max_len: usize) -> String {
    let s = match value {
        serde_json::Value::Null => "null".to_string(),
        serde_json::Value::Bool(b) => b.to_string(),
        serde_json::Value::Number(n) => n.to_string(),
        serde_json::Value::String(s) => format!("\"{}\"", s),
        serde_json::Value::Array(arr) => format!("[{} items]", arr.len()),
        serde_json::Value::Object(obj) => format!("{{{} fields}}", obj.len()),
    };
    if s.len() > max_len {
        format!("{}...", &s[..max_len - 3])
    } else {
        s
    }
}
