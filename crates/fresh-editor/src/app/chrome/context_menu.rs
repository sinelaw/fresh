//! Context menu (right-click menus): the menu box and its full-frame
//! close guard.

use crate::app::types::HoverTarget;
use crate::widgets::LayoutBox;
use anyhow::Result as AnyhowResult;

use super::{ChromeComponent, ChromePointer, ChromeTreeBuilder, Disposition, Editor, PointerPress};

pub(crate) struct ContextMenu;

impl ChromeComponent for ContextMenu {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        if let Some(core) = ed.active_window().context_menu_core() {
            let frame = ed.active_chrome().last_frame;
            let r = core.rect(frame.width, frame.height);
            t.rect("chrome:context_menu", 180, r);
            // TRUE full-frame semantics: a click outside the menu box
            // dismisses it and is consumed.
            t.full("chrome:context_menu_close_guard", 180);
        }
    }

    fn hover(&self, ed: &Editor, bx: &LayoutBox, col: u16, row: u16) -> Option<HoverTarget> {
        if bx.kind != "chrome:context_menu" {
            return None;
        }
        // The native context menus (tab / "+" new-tab / file-explorer)
        // share one geometry core, so a single hit-test over the open
        // menu covers all three. An interior (item) row yields a hover
        // target; border rows and outside positions fall through.
        let core = ed.active_window().context_menu_core()?;
        if let crate::app::types::ContextMenuHit::Item(item_idx) = core.hit(
            col,
            row,
            ed.active_chrome().last_frame.width,
            ed.active_chrome().last_frame.height,
        ) {
            return Some(HoverTarget::ContextMenuItem(item_idx));
        }
        None
    }

    fn on_pointer(
        &self,
        ed: &mut Editor,
        bx: &LayoutBox,
        ev: &ChromePointer,
    ) -> AnyhowResult<Disposition> {
        match (ev.press, bx.kind) {
            (PointerPress::Left, "chrome:context_menu") => {
                if let Some(r) = ed.handle_click_context_menus(ev.col, ev.row) {
                    r?;
                    return Ok(Disposition::Consumed);
                }
                return Ok(Disposition::Pass);
            }
            (PointerPress::Left, "chrome:context_menu_close_guard") => {
                // Outside the menu's rect (which claimed inside clicks
                // above): dismiss and consume.
                if ed.active_window().open_context_menu().is_some() {
                    ed.active_window_mut().close_context_menus();
                    return Ok(Disposition::Consumed);
                }
                return Ok(Disposition::Pass);
            }
            (PointerPress::Right, "chrome:context_menu") => {}
            _ => return Ok(Disposition::Pass),
        }
        // A right-click inside an already-open native context menu
        // (file-explorer or tab) is swallowed so the menu stays put
        // rather than being re-opened / re-targeted.
        let frame_w = ed.active_chrome().last_frame.width;
        let frame_h = ed.active_chrome().last_frame.height;
        if let Some(core) = ed.active_window().context_menu_core() {
            if !matches!(
                core.hit(ev.col, ev.row, frame_w, frame_h),
                crate::app::types::ContextMenuHit::Outside
            ) {
                return Ok(Disposition::Consumed);
            }
        }
        Ok(Disposition::Pass)
    }
}
