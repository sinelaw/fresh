//! The status bar row.

use crate::app::types::HoverTarget;
use crate::widgets::LayoutBox;

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct StatusBar;

impl ChromeComponent for StatusBar {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        if let Some((status_row, status_x, status_width)) = ed.active_chrome().status_bar.area {
            let mut b = LayoutBox::plain(
                "chrome:status_bar",
                status_row as u32,
                status_x as u32,
                status_width as u32,
                1,
            );
            b.z = 40;
            t.push(b);
        }
    }

    fn hover(&self, ed: &Editor, _bx: &LayoutBox, col: u16, row: u16) -> Option<HoverTarget> {
        // One generic hit-test over every clickable segment recorded
        // last frame (encoding, LSP, remote, ...).
        if let Some((status_row, _status_x, _status_width)) = ed.active_chrome().status_bar.area {
            if row == status_row {
                for (id, indicator_row, start, end) in &ed.active_chrome().status_bar.clickable {
                    if row == *indicator_row && col >= *start && col < *end {
                        return Some(HoverTarget::StatusBarClickable(*id));
                    }
                }
            }
        }
        None
    }

    fn on_pointer(
        &self,
        ed: &mut Editor,
        _bx: &LayoutBox,
        ev: &super::ChromePointer,
    ) -> anyhow::Result<super::Disposition> {
        use super::{Disposition, PointerPress};
        if ev.press != PointerPress::Left {
            return Ok(Disposition::Pass);
        }
        if let Some(r) = ed.handle_click_status_bar(ev.col, ev.row) {
            r?;
            return Ok(Disposition::Consumed);
        }
        Ok(Disposition::Pass)
    }
}
