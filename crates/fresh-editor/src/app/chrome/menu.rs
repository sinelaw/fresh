//! The menu bar, open dropdown/submenu boxes, and the full-frame
//! close guard while a menu is open.

use crate::app::types::HoverTarget;
use crate::widgets::LayoutBox;

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct Menu;

impl ChromeComponent for Menu {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        // The bar box needs only the row rect (cheap); the full
        // label/dropdown layout walk (`menu_layout_now`) runs only
        // while a menu is OPEN — collect fires on every pointer
        // event, and recomputing dropdown geometry on every mouse
        // move with no menu open would be pure waste.
        if let Some(area) = ed.menu_bar_area_now() {
            t.rect("chrome:menu_bar", 120, area);
        }
        if ed.menu_state.active_menu.is_some() {
            if let Some(ml) = ed.menu_layout_now() {
                if let Some(r) = ml.dropdown_box {
                    t.rect("chrome:menu_dropdown", 120, r);
                }
                for (_, r) in &ml.submenu_boxes {
                    t.rect("chrome:menu_dropdown", 120, *r);
                }
            }
            // TRUE full-frame semantics: any click outside the open
            // menu closes it and is consumed.
            t.full("chrome:menu_close_guard", 110);
        }
    }

    fn on_hover_change(
        &self,
        ed: &mut Editor,
        _old: Option<&HoverTarget>,
        new: Option<&HoverTarget>,
        _col: u16,
        _row: u16,
    ) -> bool {
        // An open menu follows the pointer (bar auto-switch, submenu
        // open/close, highlight). The machine lives with the menu's
        // other behavior in `menu_actions.rs`.
        ed.menu_hover_reaction(new)
    }

    fn hover(&self, ed: &mut Editor, bx: &LayoutBox, col: u16, row: u16) -> Option<HoverTarget> {
        match bx.kind {
            "chrome:menu_bar" => {
                let menu_layout = ed.menu_layout_now()?;
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

    fn layers(&self, ed: &Editor, out: &mut Vec<(u16, crate::app::overlay::Layer)>) {
        use crate::app::overlay::{Layer, LayerKind};
        if ed.menu_state.active_menu.is_some() {
            out.push((
                super::layer_rank::MENU,
                Layer {
                    kind: LayerKind::Menu,
                    owns_keyboard: true,
                    key_context: Some(crate::input::keybindings::KeyContext::Menu),
                    blocks_terminal_input: true,
                },
            ));
        }
    }
}
