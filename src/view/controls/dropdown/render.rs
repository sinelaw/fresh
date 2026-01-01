//! Dropdown rendering functions

use ratatui::layout::Rect;
use ratatui::style::{Color, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::Paragraph;
use ratatui::Frame;

use super::{DropdownColors, DropdownLayout, DropdownState, FocusState};

/// Render a dropdown control (closed state)
///
/// # Arguments
/// * `frame` - The ratatui frame to render to
/// * `area` - Rectangle where the control should be rendered
/// * `state` - The dropdown state
/// * `colors` - Colors for rendering
///
/// # Returns
/// Layout information for hit testing
pub fn render_dropdown(
    frame: &mut Frame,
    area: Rect,
    state: &DropdownState,
    colors: &DropdownColors,
) -> DropdownLayout {
    render_dropdown_aligned(frame, area, state, colors, None)
}

/// Render a dropdown control with optional label width alignment
///
/// # Arguments
/// * `frame` - The ratatui frame to render to
/// * `area` - Rectangle where the control should be rendered
/// * `state` - The dropdown state
/// * `colors` - Colors for rendering
/// * `label_width` - Optional minimum label width for alignment
///
/// # Returns
/// Layout information for hit testing
pub fn render_dropdown_aligned(
    frame: &mut Frame,
    area: Rect,
    state: &DropdownState,
    colors: &DropdownColors,
    label_width: Option<u16>,
) -> DropdownLayout {
    if area.height == 0 || area.width < 10 {
        return DropdownLayout::default();
    }

    let (label_color, selected_color, border_color, arrow_color) = match state.focus {
        FocusState::Normal => (colors.label, colors.selected, colors.border, colors.arrow),
        FocusState::Focused => (
            colors.focused,
            colors.selected,
            colors.focused,
            colors.focused,
        ),
        FocusState::Hovered => (
            colors.focused,
            colors.selected,
            colors.focused,
            colors.focused,
        ),
        FocusState::Disabled => (
            colors.disabled,
            colors.disabled,
            colors.disabled,
            colors.disabled,
        ),
    };

    let selected_text = state.selected_option().unwrap_or("");
    let max_option_len = state.options.iter().map(|s| s.len()).max().unwrap_or(10);
    let display_width = max_option_len.max(selected_text.len()).min(20);
    let padded = format!("{:width$}", selected_text, width = display_width);

    let arrow = if state.open { "▲" } else { "▼" };

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
        Span::styled(padded, Style::default().fg(selected_color)),
        Span::styled(" ", Style::default()),
        Span::styled(arrow, Style::default().fg(arrow_color)),
        Span::styled("]", Style::default().fg(border_color)),
    ]);

    let paragraph = Paragraph::new(line);
    frame.render_widget(paragraph, area);

    let final_label_width = actual_label_width + 2;
    let button_start = area.x + final_label_width;
    let button_width = display_width as u16 + 4;

    let mut option_areas = Vec::new();

    if state.open && area.height > 1 {
        let menu_y = area.y + 1;
        let available_height = area.height.saturating_sub(1) as usize;
        let options_to_show = state.options.len().min(available_height);
        let needs_scrollbar = state.options.len() > available_height;
        let scrollbar_width: u16 = if needs_scrollbar { 1 } else { 0 };
        let option_width = button_width.saturating_sub(scrollbar_width);

        // Use scroll offset from state
        let scroll_offset = state
            .scroll_offset
            .min(state.options.len().saturating_sub(options_to_show));

        for (i, option) in state
            .options
            .iter()
            .skip(scroll_offset)
            .take(options_to_show)
            .enumerate()
        {
            let actual_index = scroll_offset + i;
            let option_area = Rect::new(button_start, menu_y + i as u16, option_width, 1);
            option_areas.push(option_area);

            let is_selected = actual_index == state.selected;
            let (bg, fg) = if is_selected {
                (colors.highlight_bg, colors.selected)
            } else {
                (Color::Reset, colors.option)
            };

            let padded_option = format!(
                " {:width$} ",
                option,
                width = display_width.saturating_sub(1)
            );
            let option_line = Line::from(vec![Span::styled(
                padded_option,
                Style::default().fg(fg).bg(bg),
            )]);

            let option_para = Paragraph::new(option_line);
            frame.render_widget(option_para, option_area);
        }

        // Render scrollbar if needed
        if needs_scrollbar && available_height > 0 {
            let scrollbar_x = button_start + option_width;
            let total_options = state.options.len();
            let max_offset = total_options.saturating_sub(options_to_show);

            // Calculate thumb position and size
            let thumb_size = ((options_to_show as f32 / total_options as f32)
                * available_height as f32)
                .max(1.0) as usize;
            let thumb_pos = if max_offset > 0 {
                ((scroll_offset as f32 / max_offset as f32)
                    * (available_height - thumb_size) as f32) as usize
            } else {
                0
            };

            for i in 0..available_height {
                let scrollbar_char = if i >= thumb_pos && i < thumb_pos + thumb_size {
                    "█"
                } else {
                    "░"
                };
                let scrollbar_area = Rect::new(scrollbar_x, menu_y + i as u16, 1, 1);
                let scrollbar_span =
                    Span::styled(scrollbar_char, Style::default().fg(colors.border));
                frame.render_widget(Paragraph::new(Line::from(scrollbar_span)), scrollbar_area);
            }
        }
    }

    // Get scroll_offset for the layout (need to recalculate since we used it above)
    let layout_scroll_offset = if state.open && area.height > 1 {
        let available_height = area.height.saturating_sub(1) as usize;
        let options_to_show = state.options.len().min(available_height);
        state
            .scroll_offset
            .min(state.options.len().saturating_sub(options_to_show))
    } else {
        0
    };

    DropdownLayout {
        button_area: Rect::new(button_start, area.y, button_width, 1),
        option_areas,
        full_area: Rect::new(area.x, area.y, button_start - area.x + button_width, 1),
        scroll_offset: layout_scroll_offset,
    }
}
