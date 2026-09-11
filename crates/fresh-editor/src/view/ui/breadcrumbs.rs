//! Rendering for the per-buffer symbol breadcrumb row.

use crate::primitives::display_width::str_width;
use crate::view::theme::Theme;
use fresh_core::api::BreadcrumbItem;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Paragraph, Widget};

#[derive(Debug, Clone, PartialEq, Eq)]
struct LaidOutBreadcrumb {
    label: String,
    pub position: usize,
    pub area: Rect,
    is_current: bool,
}

fn truncate_label(label: &str, max_width: usize) -> String {
    let clean = label.replace(['\n', '\r'], " ");
    if str_width(&clean) <= max_width {
        return clean;
    }
    if max_width == 0 {
        return String::new();
    }
    if max_width == 1 {
        return "…".to_string();
    }
    let mut out = String::new();
    for ch in clean.chars() {
        let next = str_width(&out) + crate::primitives::display_width::char_width(ch);
        if next + 1 > max_width {
            break;
        }
        out.push(ch);
    }
    out.push('…');
    out
}

fn layout_breadcrumbs(area: Rect, items: &[BreadcrumbItem]) -> (bool, Vec<LaidOutBreadcrumb>) {
    if area.width == 0 || area.height == 0 || items.is_empty() {
        return (false, Vec::new());
    }

    let available = area.width as usize;
    let separator_width = str_width(" > ");
    let mut first = 0usize;
    let full_width = |start: usize| {
        1 + items[start..]
            .iter()
            .map(|item| str_width(&item.label))
            .sum::<usize>()
            + separator_width * items.len().saturating_sub(start + 1)
            + if start > 0 { str_width("… > ") } else { 0 }
    };
    while first + 1 < items.len() && full_width(first) > available {
        first += 1;
    }

    let mut x = area.x.saturating_add(1);
    let mut remaining = available.saturating_sub(1);
    let hidden_prefix = first > 0 && remaining >= str_width("… > ");
    if hidden_prefix {
        let width = str_width("… > ");
        x = x.saturating_add(width as u16);
        remaining = remaining.saturating_sub(width);
    }

    let mut laid_out = Vec::new();
    for (visible_index, item) in items[first..].iter().enumerate() {
        if visible_index > 0 {
            if remaining < separator_width {
                break;
            }
            x = x.saturating_add(separator_width as u16);
            remaining -= separator_width;
        }
        let later_separators = items.len().saturating_sub(first + visible_index + 1);
        let reserve = later_separators * separator_width;
        let label = truncate_label(&item.label, remaining.saturating_sub(reserve));
        let width = str_width(&label);
        if width == 0 {
            break;
        }
        laid_out.push(LaidOutBreadcrumb {
            label,
            position: item.position as usize,
            area: Rect::new(x, area.y, width as u16, 1),
            is_current: first + visible_index + 1 == items.len(),
        });
        x = x.saturating_add(width as u16);
        remaining = remaining.saturating_sub(width);
    }
    (hidden_prefix, laid_out)
}

/// Resolve a press using the same width-aware layout as the renderer.
pub fn breadcrumb_position_at(
    area: Rect,
    items: &[BreadcrumbItem],
    col: u16,
    row: u16,
) -> Option<usize> {
    layout_breadcrumbs(area, items)
        .1
        .into_iter()
        .find(|item| {
            col >= item.area.x
                && col < item.area.x.saturating_add(item.area.width)
                && row >= item.area.y
                && row < item.area.y.saturating_add(item.area.height)
        })
        .map(|item| item.position)
}

/// Paint a breadcrumb trail, preserving the innermost symbols when horizontal
/// space is tight.
pub fn render_breadcrumbs(
    buf: &mut ratatui::buffer::Buffer,
    area: Rect,
    items: &[BreadcrumbItem],
    theme: &Theme,
) {
    if area.width == 0 || area.height == 0 || items.is_empty() {
        return;
    }

    let background = Style::default()
        .fg(theme.line_number_fg)
        .bg(theme.tab_separator_bg);
    Paragraph::new(" ".repeat(area.width as usize))
        .style(background)
        .render(area, buf);

    let separator = " > ";
    let mut spans = Vec::new();
    spans.push(Span::raw(" "));
    let (hidden_prefix, laid_out) = layout_breadcrumbs(area, items);
    if hidden_prefix {
        spans.push(Span::styled("… > ", background));
    }
    for (visible_index, item) in laid_out.into_iter().enumerate() {
        if visible_index > 0 {
            spans.push(Span::styled(separator, background));
        }
        let style = if item.is_current {
            background
                .fg(theme.tab_inactive_fg)
                .add_modifier(Modifier::BOLD)
        } else {
            background
        };
        spans.push(Span::styled(item.label, style));
    }

    Paragraph::new(Line::from(spans)).render(area, buf);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn truncates_unicode_labels_to_display_width() {
        assert_eq!(truncate_label("abcdef", 4), "abc…");
        assert_eq!(str_width(&truncate_label("日本語", 4)), 3);
        assert_eq!(truncate_label("anything", 1), "…");
    }

    #[test]
    fn hit_testing_uses_the_rendered_breadcrumb_widths() {
        let items = vec![
            BreadcrumbItem {
                label: "Outer".into(),
                position: 3,
            },
            BreadcrumbItem {
                label: "inner".into(),
                position: 19,
            },
        ];
        let area = Rect::new(10, 4, 20, 1);
        assert_eq!(breadcrumb_position_at(area, &items, 11, 4), Some(3));
        assert_eq!(breadcrumb_position_at(area, &items, 19, 4), Some(19));
        assert_eq!(breadcrumb_position_at(area, &items, 10, 4), None);
        assert_eq!(breadcrumb_position_at(area, &items, 19, 5), None);
    }
}
