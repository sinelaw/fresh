//! The status bar row.

use crate::app::types::HoverTarget;
use crate::widgets::LayoutBox;

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct StatusBar;

impl ChromeComponent for StatusBar {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        if let Some(area) = ed.status_bar_area_now() {
            let mut b = LayoutBox::plain(
                "chrome:status_bar",
                area.y as u32,
                area.x as u32,
                area.width as u32,
                area.height as u32,
            );
            b.z = 40;
            t.push(b);
        }
    }

    fn hover(&self, ed: &mut Editor, _bx: &LayoutBox, col: u16, row: u16) -> Option<HoverTarget> {
        // One generic hit-test over every clickable segment, on geometry
        // derived from live state (encoding, LSP, remote, ...).
        let (area, layout) = ed.status_bar_layout_now()?;
        if row != area.y {
            return None;
        }
        for (id, indicator_row, start, end) in &layout.clickable {
            if row == *indicator_row && col >= *start && col < *end {
                return Some(HoverTarget::StatusBarClickable(*id));
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
