//! Info/message popups: transient-dismiss guard, per-popup rects and
//! scrollbar tracks, and the absorb/dismiss guards.

use crate::app::types::HoverTarget;
use crate::widgets::LayoutBox;
use anyhow::Result as AnyhowResult;

use super::{ChromeComponent, ChromePointer, ChromeTreeBuilder, Disposition, Editor, PointerPress};

fn in_rect(col: u16, row: u16, rect: ratatui::layout::Rect) -> bool {
    col >= rect.x && col < rect.x + rect.width && row >= rect.y && row < rect.y + rect.height
}

pub(crate) struct Popups;

impl ChromeComponent for Popups {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        t.full("chrome:transient_guard", 17);
        // Each popup's scrollbar track at its painted rect.
        for area in &ed.active_chrome().popup_areas {
            if let Some(r) = area.5 {
                t.rect("chrome:popup_scrollbar", 17, r);
            }
        }
        // Popups are rect-bounded (a wheel or click outside every popup
        // rect falls through); the absorb/dismiss guards below stay
        // full-frame.
        for (_, popup_rect, ..) in &ed.active_chrome().global_popup_areas {
            t.rect("chrome:popups", 15, *popup_rect);
        }
        for area in &ed.active_chrome().popup_areas {
            t.rect("chrome:popups", 15, area.1);
        }
        t.full("chrome:popup_absorb", 14);
        // Block-or-dismiss guard for transient popups (double-click).
        t.full("chrome:popup_guard", 14);
    }

    fn hover(&self, ed: &Editor, bx: &LayoutBox, col: u16, row: u16) -> Option<HoverTarget> {
        if bx.kind != "chrome:popups" {
            return None;
        }
        // Check popups from top to bottom (reverse order since the
        // last popup is on top).
        for (popup_idx, _popup_rect, inner_rect, scroll_offset, num_items, _, _) in
            ed.active_chrome().popup_areas.iter().rev()
        {
            if in_rect(col, row, *inner_rect) && *num_items > 0 {
                let relative_row = (row - inner_rect.y) as usize;
                let item_idx = scroll_offset + relative_row;
                if item_idx < *num_items {
                    return Some(HoverTarget::PopupListItem(*popup_idx, item_idx));
                }
            }
        }
        None
    }

    fn on_pointer(
        &self,
        ed: &mut Editor,
        bx: &LayoutBox,
        ev: &ChromePointer,
    ) -> AnyhowResult<Disposition> {
        if ev.press != PointerPress::Double || bx.kind != "chrome:popup_guard" {
            return Ok(Disposition::Pass);
        }
        // Inside a popup: block. Outside: dismiss transients and keep
        // routing (act-then-continue guard).
        if ed.is_mouse_over_any_popup(ev.col, ev.row) {
            return Ok(Disposition::Consumed);
        }
        ed.dismiss_transient_popups();
        Ok(Disposition::PassAfter)
    }
}
