//! Rendering for the per-buffer symbol breadcrumb row.

use crate::model::event::{BufferId, LeafId};
use crate::primitives::display_width::str_width;
use crate::view::theme::Theme;
use fresh_core::api::BreadcrumbItem;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Paragraph, Widget};

/// Click target for one visible breadcrumb item.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BreadcrumbHit {
    pub split_id: LeafId,
    pub buffer_id: BufferId,
    pub position: usize,
    pub area: Rect,
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

/// Paint a breadcrumb trail, preserving the innermost symbols when horizontal
/// space is tight. Returns hit rectangles for the items that remain visible.
pub fn render_breadcrumbs(
    buf: &mut ratatui::buffer::Buffer,
    area: Rect,
    split_id: LeafId,
    buffer_id: BufferId,
    items: &[BreadcrumbItem],
    theme: &Theme,
) -> Vec<BreadcrumbHit> {
    if area.width == 0 || area.height == 0 || items.is_empty() {
        return Vec::new();
    }

    let background = Style::default()
        .fg(theme.line_number_fg)
        .bg(theme.tab_separator_bg);
    Paragraph::new(" ".repeat(area.width as usize))
        .style(background)
        .render(area, buf);

    let available = area.width as usize;
    let separator = " > ";
    let separator_width = str_width(separator);
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

    let mut spans = Vec::new();
    let mut hits = Vec::new();
    let mut x = area.x.saturating_add(1);
    let mut remaining = available.saturating_sub(1);
    spans.push(Span::raw(" "));
    if first > 0 && remaining >= str_width("… > ") {
        spans.push(Span::styled("… > ", background));
        let width = str_width("… > ");
        x = x.saturating_add(width as u16);
        remaining = remaining.saturating_sub(width);
    }

    for (visible_index, item) in items[first..].iter().enumerate() {
        if visible_index > 0 {
            if remaining < separator_width {
                break;
            }
            spans.push(Span::styled(separator, background));
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
        let is_current = first + visible_index + 1 == items.len();
        let style = if is_current {
            background
                .fg(theme.tab_inactive_fg)
                .add_modifier(Modifier::BOLD)
        } else {
            background
        };
        spans.push(Span::styled(label, style));
        hits.push(BreadcrumbHit {
            split_id,
            buffer_id,
            position: item.position as usize,
            area: Rect::new(x, area.y, width as u16, 1),
        });
        x = x.saturating_add(width as u16);
        remaining = remaining.saturating_sub(width);
    }

    Paragraph::new(Line::from(spans)).render(area, buf);
    hits
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
}
