//! Rendering for the dual-list control

use super::{DualListColors, DualListColumn, DualListLayout, DualListRowArea, DualListState};
use crate::view::controls::FocusState;
use ratatui::layout::Rect;
use ratatui::style::Style;
use ratatui::text::{Line, Span};
use ratatui::widgets::Paragraph;
use ratatui::Frame;

/// Render a dual-list control with skip_rows support for settings scroll
pub fn render_dual_list_partial(
    frame: &mut Frame,
    area: Rect,
    state: &DualListState,
    colors: &DualListColors,
    skip_rows: u16,
) -> DualListLayout {
    let empty_layout = DualListLayout {
        full_area: area,
        ..Default::default()
    };

    if area.height == 0 || area.width < 20 {
        return empty_layout;
    }

    let label_color = match state.focus {
        FocusState::Focused => colors.focused_fg,
        FocusState::Hovered => colors.focused_fg,
        FocusState::Disabled => colors.disabled,
        FocusState::Normal => colors.label,
    };

    let indent = 2u16;
    // Layout: indent | col1 | gap | buttons | gap | col2
    let btn_width = 3u16;
    let gap = 2u16;
    let usable = area.width.saturating_sub(indent + btn_width + gap * 2);
    // Cap column width so the two lists stay compact and visually balanced
    let col_width = (usable / 2).min(28);

    let available_items = state.available_items();
    let included_items = state.included_items();

    let mut layout = DualListLayout {
        full_area: area,
        ..Default::default()
    };

    let is_focused = state.focus == FocusState::Focused;
    let mut y = area.y;
    let mut content_row = 0u16;

    // Row layout: 0=label, 1=column headers, 2..=body rows (paired Available/Included cells)
    if skip_rows == 0 && y < area.y + area.height {
        let mut label_spans = vec![
            Span::styled(&state.label, Style::default().fg(label_color)),
            Span::raw(":"),
        ];
        if is_focused && !state.editing {
            label_spans.push(Span::styled(
                format!("  [{}]", rust_i18n::t!("settings.dual_list_enter_hint")),
                Style::default().fg(colors.disabled),
            ));
        } else if state.editing {
            label_spans.push(Span::styled(
                format!("  {}", rust_i18n::t!("settings.dual_list_shift_hint")),
                Style::default().fg(colors.disabled),
            ));
        }
        let label_line = Line::from(label_spans);
        frame.render_widget(
            Paragraph::new(label_line),
            Rect::new(area.x, y, area.width, 1),
        );
        y += 1;
    }
    content_row += 1;

    if content_row >= skip_rows && y < area.y + area.height {
        let header_style = Style::default().fg(colors.header);
        // Underline the active column header only when in editing mode
        let avail_style = if state.editing && state.active_column == DualListColumn::Available {
            Style::default()
                .fg(colors.focused_fg)
                .add_modifier(ratatui::style::Modifier::UNDERLINED)
        } else {
            header_style
        };
        let incl_style = if state.editing && state.active_column == DualListColumn::Included {
            Style::default()
                .fg(colors.focused_fg)
                .add_modifier(ratatui::style::Modifier::UNDERLINED)
        } else {
            header_style
        };
        let avail_header = format!("{:width$}", "Available", width = col_width as usize);
        let incl_header = format!("{:width$}", "Included", width = col_width as usize);

        let line = Line::from(vec![
            Span::raw(" ".repeat(indent as usize)),
            Span::styled(avail_header, avail_style),
            Span::raw(" ".repeat((gap + btn_width + gap) as usize)),
            Span::styled(incl_header, incl_style),
        ]);
        frame.render_widget(Paragraph::new(line), Rect::new(area.x, y, area.width, 1));
        y += 1;
    }
    content_row += 1;

    let body_rows = state.body_rows();

    for row_idx in 0..body_rows {
        if y >= area.y + area.height {
            break;
        }
        if content_row < skip_rows {
            content_row += 1;
            continue;
        }

        let col1_x = area.x + indent;
        let btn_x = col1_x + col_width + gap;
        let col2_x = btn_x + btn_width + gap;

        if row_idx < available_items.len() {
            let (_, name) = available_items[row_idx];
            let is_active = state.editing
                && state.active_column == DualListColumn::Available
                && state.available_cursor == row_idx;

            let display: String = name.chars().take(col_width as usize).collect();
            let padded = format!("{:width$}", display, width = col_width as usize);

            let style = if is_active {
                Style::default().fg(colors.focused_fg).bg(colors.focused_bg)
            } else {
                Style::default().fg(colors.text)
            };

            let cell_area = Rect::new(col1_x, y, col_width, 1);
            frame.render_widget(Paragraph::new(Span::styled(padded, style)), cell_area);

            layout.available_rows.push(DualListRowArea {
                area: cell_area,
                index: row_idx,
            });
        }

        // Action buttons between columns: add/remove transfer items, up/down reorder
        let btn_style = Style::default().fg(colors.button);
        let dim_style = Style::default().fg(colors.disabled);
        match row_idx {
            0 => {
                let btn_area = Rect::new(btn_x, y, btn_width, 1);
                frame.render_widget(Paragraph::new(Span::styled(" → ", btn_style)), btn_area);
                layout.add_button = Some(btn_area);
            }
            1 => {
                let btn_area = Rect::new(btn_x, y, btn_width, 1);
                frame.render_widget(Paragraph::new(Span::styled(" ← ", btn_style)), btn_area);
                layout.remove_button = Some(btn_area);
            }
            // Separator row between transfer and reorder buttons
            2 => {
                let btn_area = Rect::new(btn_x, y, btn_width, 1);
                frame.render_widget(Paragraph::new(Span::styled("───", dim_style)), btn_area);
            }
            3 => {
                let btn_area = Rect::new(btn_x, y, btn_width, 1);
                frame.render_widget(Paragraph::new(Span::styled(" ↑ ", btn_style)), btn_area);
                layout.move_up_button = Some(btn_area);
            }
            4 => {
                let btn_area = Rect::new(btn_x, y, btn_width, 1);
                frame.render_widget(Paragraph::new(Span::styled(" ↓ ", btn_style)), btn_area);
                layout.move_down_button = Some(btn_area);
            }
            _ => {}
        }

        if row_idx < included_items.len() {
            let (_, name) = included_items[row_idx];
            let is_active = state.editing
                && state.active_column == DualListColumn::Included
                && state.included_cursor == row_idx;

            let display: String = name.chars().take(col_width as usize).collect();
            let padded = format!("{:width$}", display, width = col_width as usize);

            let style = if is_active {
                Style::default().fg(colors.focused_fg).bg(colors.focused_bg)
            } else {
                Style::default().fg(colors.text)
            };

            let cell_area = Rect::new(col2_x, y, col_width, 1);
            frame.render_widget(Paragraph::new(Span::styled(padded, style)), cell_area);

            layout.included_rows.push(DualListRowArea {
                area: cell_area,
                index: row_idx,
            });
        }

        y += 1;
        content_row += 1;
    }

    layout
}
