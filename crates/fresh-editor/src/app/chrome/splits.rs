//! The split grid: widget-panel content surfaces, separators,
//! close/maximize buttons, tab bars, v/h scrollbars, and the editor
//! content rects.

use crate::model::event::SplitDirection;
use crate::widgets::LayoutBox;

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct Splits;

impl ChromeComponent for Splits {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        for (_, buffer_id, content_rect, ..) in &ed.active_layout().split_areas {
            if !ed.widget_registry.panels_for_buffer(*buffer_id).is_empty() {
                t.rect("chrome:split_widget_panel", 12, *content_rect);
            }
        }
        for (_, direction, sep_x, sep_y, sep_len) in &ed.active_layout().separator_areas {
            let (w, h) = match direction {
                SplitDirection::Horizontal => (*sep_len as u32, 1),
                SplitDirection::Vertical => (1, *sep_len as u32),
            };
            let mut b = LayoutBox::plain(
                "chrome:split_separators",
                *sep_y as u32,
                *sep_x as u32,
                w,
                h,
            );
            b.z = 8;
            t.boxes.push(b);
        }
        for (_, btn_row, start, end) in &ed.active_layout().close_split_areas {
            let mut b = LayoutBox::plain(
                "chrome:split_buttons",
                *btn_row as u32,
                *start as u32,
                end.saturating_sub(*start) as u32,
                1,
            );
            b.z = 7;
            t.boxes.push(b);
        }
        for (_, btn_row, start, end) in &ed.active_layout().maximize_split_areas {
            let mut b = LayoutBox::plain(
                "chrome:split_buttons",
                *btn_row as u32,
                *start as u32,
                end.saturating_sub(*start) as u32,
                1,
            );
            b.z = 7;
            t.boxes.push(b);
        }
        for (_, tl) in &ed.active_layout().tab_layouts {
            t.rect("chrome:tabs", 6, tl.bar_area);
        }
        for (_, _, _, scrollbar_rect, _, _) in &ed.active_layout().split_areas {
            t.rect("chrome:scrollbars", 5, *scrollbar_rect);
        }
        for (_, _, r, _, _, _) in &ed.active_layout().horizontal_scrollbar_areas {
            t.rect("chrome:h_scrollbar", 5, *r);
        }
        for (_, _, content_rect, ..) in &ed.active_layout().split_areas {
            t.rect("chrome:editor", 1, *content_rect);
        }
    }
}
