//! The menu bar, open dropdown/submenu boxes, and the full-frame
//! close guard while a menu is open.

use crate::app::types::HoverTarget;
use crate::widgets::LayoutBox;

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct Menu;

impl ChromeComponent for Menu {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        if ed.active_window().menu_bar_visible {
            if let Some(ml) = &ed.active_chrome().menu_layout {
                t.rect("chrome:menu_bar", 12, ml.bar_area);
            }
        }
        if ed.menu_state.active_menu.is_some() {
            if let Some(ml) = &ed.active_chrome().menu_layout {
                if let Some(r) = ml.dropdown_box {
                    t.rect("chrome:menu_dropdown", 12, r);
                }
                for (_, r) in &ml.submenu_boxes {
                    t.rect("chrome:menu_dropdown", 12, *r);
                }
            }
            // TRUE full-frame semantics: any click outside the open
            // menu closes it and is consumed.
            t.full("chrome:menu_close_guard", 11);
        }
    }

    fn hover(&self, ed: &Editor, bx: &LayoutBox, col: u16, row: u16) -> Option<HoverTarget> {
        match bx.kind {
            "chrome:menu_bar" => {
                let menu_layout = ed.active_chrome().menu_layout.as_ref()?;
                menu_layout.menu_at(col, row).map(HoverTarget::MenuBarItem)
            }
            "chrome:menu_dropdown" => {
                let active_idx = ed.menu_state.active_menu?;
                ed.compute_menu_dropdown_hover(col, row, active_idx)
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
        match bx.kind {
            "chrome:menu_bar" => {
                if let Some(r) = ed.handle_click_menu_bar(ev.col, ev.row) {
                    r?;
                    return Ok(Disposition::Consumed);
                }
                Ok(Disposition::Pass)
            }
            "chrome:menu_dropdown" => {
                if let Some(r) = ed.handle_click_menu_dropdown_surface(ev.col, ev.row) {
                    r?;
                    return Ok(Disposition::Consumed);
                }
                Ok(Disposition::Pass)
            }
            // Any click outside the open menu's boxes closes it and is
            // consumed (the rect surfaces above claimed inside clicks
            // first).
            "chrome:menu_close_guard" => {
                if ed.menu_state.active_menu.is_some() {
                    ed.close_menu_with_auto_hide();
                    return Ok(Disposition::Consumed);
                }
                Ok(Disposition::Pass)
            }
            _ => Ok(Disposition::Pass),
        }
    }
}
