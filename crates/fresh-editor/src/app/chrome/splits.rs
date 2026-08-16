//! The split grid: widget-panel content surfaces, separators,
//! close/maximize buttons, tab bars, v/h scrollbars, and the editor
//! content rects.

use crate::app::types::HoverTarget;
use crate::model::event::SplitDirection;
use crate::view::ui::tabs::TabHit;
use crate::widgets::LayoutBox;

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

fn in_rect(col: u16, row: u16, rect: ratatui::layout::Rect) -> bool {
    col >= rect.x && col < rect.x + rect.width && row >= rect.y && row < rect.y + rect.height
}

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
            t.push(b);
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
            t.push(b);
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
            t.push(b);
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

    fn hover(&self, ed: &Editor, bx: &LayoutBox, col: u16, row: u16) -> Option<HoverTarget> {
        match bx.kind {
            "chrome:split_separators" => {
                for (split_id, direction, sep_x, sep_y, sep_length) in
                    &ed.active_layout().separator_areas
                {
                    let is_on_separator = match direction {
                        SplitDirection::Horizontal => {
                            row == *sep_y && col >= *sep_x && col < sep_x + sep_length
                        }
                        SplitDirection::Vertical => {
                            col == *sep_x && row >= *sep_y && row < sep_y + sep_length
                        }
                    };
                    if is_on_separator {
                        return Some(HoverTarget::SplitSeparator(*split_id, *direction));
                    }
                }
                None
            }
            "chrome:split_buttons" => {
                // Split control buttons sit on top of the tab row.
                for (split_id, btn_row, start_col, end_col) in &ed.active_layout().close_split_areas
                {
                    if row == *btn_row && col >= *start_col && col < *end_col {
                        return Some(HoverTarget::CloseSplitButton(*split_id));
                    }
                }
                for (split_id, btn_row, start_col, end_col) in
                    &ed.active_layout().maximize_split_areas
                {
                    if row == *btn_row && col >= *start_col && col < *end_col {
                        return Some(HoverTarget::MaximizeSplitButton(*split_id));
                    }
                }
                None
            }
            "chrome:tabs" => {
                for (split_id, tab_layout) in &ed.active_layout().tab_layouts {
                    match tab_layout.hit_test(col, row) {
                        Some(TabHit::CloseButton(target)) => {
                            return Some(HoverTarget::TabCloseButton(target, *split_id));
                        }
                        Some(TabHit::TabName(target)) => {
                            return Some(HoverTarget::TabName(target, *split_id));
                        }
                        Some(TabHit::ScrollLeft)
                        | Some(TabHit::ScrollRight)
                        | Some(TabHit::BarBackground)
                        | Some(TabHit::NewTabButton)
                        | None => {}
                    }
                }
                None
            }
            "chrome:scrollbars" => {
                for (split_id, _buffer_id, _content_rect, scrollbar_rect, thumb_start, thumb_end) in
                    &ed.active_layout().split_areas
                {
                    if in_rect(col, row, *scrollbar_rect) {
                        let relative_row = row.saturating_sub(scrollbar_rect.y) as usize;
                        let is_on_thumb = relative_row >= *thumb_start && relative_row < *thumb_end;
                        if is_on_thumb {
                            return Some(HoverTarget::ScrollbarThumb(*split_id));
                        } else {
                            return Some(HoverTarget::ScrollbarTrack(
                                *split_id,
                                relative_row as u16,
                            ));
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    fn on_pointer(
        &self,
        ed: &mut Editor,
        bx: &LayoutBox,
        ev: &super::ChromePointer,
    ) -> anyhow::Result<super::Disposition> {
        use super::{Disposition, PointerPress};
        if ev.press != PointerPress::Left {
            return Ok(Disposition::Pass);
        }
        let consumed = match bx.kind {
            "chrome:scrollbars" => ed.handle_click_scrollbar(ev.col, ev.row),
            "chrome:h_scrollbar" => ed.handle_click_horizontal_scrollbar(ev.col, ev.row),
            "chrome:split_separators" => ed.handle_click_split_separator(ev.col, ev.row),
            "chrome:split_buttons" => ed.handle_click_split_controls(ev.col, ev.row),
            "chrome:tabs" => ed.handle_click_tab_bar(ev.col, ev.row),
            "chrome:editor" => {
                let areas: Vec<_> = ed
                    .active_layout()
                    .split_areas
                    .iter()
                    .map(|(split_id, buffer_id, content_rect, _, _, _)| {
                        (*split_id, *buffer_id, *content_rect)
                    })
                    .collect();
                for (split_id, buffer_id, content_rect) in areas {
                    if in_rect(ev.col, ev.row, content_rect) {
                        ed.handle_editor_click(
                            ev.col,
                            ev.row,
                            split_id,
                            buffer_id,
                            content_rect,
                            ev.modifiers,
                        )?;
                        return Ok(Disposition::Consumed);
                    }
                }
                None
            }
            _ => None,
        };
        if let Some(r) = consumed {
            r?;
            return Ok(Disposition::Consumed);
        }
        Ok(Disposition::Pass)
    }

    fn on_wheel(
        &self,
        ed: &mut Editor,
        bx: &LayoutBox,
        col: u16,
        row: u16,
        delta: i32,
    ) -> anyhow::Result<super::Disposition> {
        use super::Disposition;
        if bx.kind != "chrome:split_widget_panel" {
            return Ok(Disposition::Pass);
        }
        if ed.handle_split_widget_panel_wheel(col, row, delta) {
            Ok(Disposition::Consumed)
        } else {
            Ok(Disposition::Pass)
        }
    }
}
