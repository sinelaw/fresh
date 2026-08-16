//! The base surface: splits / tab strip fallback (right-click's tab
//! context menu, double-click's word select, the wheel's
//! `wheel_surface_at` resolution).

use crate::app::types::TabContextMenu;
use crate::view::ui::tabs::TabHit;
use crate::widgets::LayoutBox;
use anyhow::Result as AnyhowResult;

use super::{ChromeComponent, ChromePointer, ChromeTreeBuilder, Disposition, Editor, PointerPress};

pub(crate) struct Base;

impl ChromeComponent for Base {
    fn collect(&self, _ed: &Editor, t: &mut ChromeTreeBuilder) {
        t.full("chrome:base", 0);
    }

    fn on_pointer(
        &self,
        ed: &mut Editor,
        _bx: &LayoutBox,
        ev: &ChromePointer,
    ) -> AnyhowResult<Disposition> {
        if ev.press != PointerPress::Right {
            return Ok(Disposition::Pass);
        }
        // Right-click on a tab raises its context menu; anywhere else
        // on the base surface clears it. Context menus only make sense
        // for buffer tabs; groups are plugin-managed.
        let tab_hit =
            ed.active_layout().tab_layouts.iter().find_map(
                |(split_id, tab_layout)| match tab_layout.hit_test(ev.col, ev.row) {
                    Some(TabHit::TabName(target) | TabHit::CloseButton(target)) => {
                        target.as_buffer().map(|bid| (*split_id, bid))
                    }
                    _ => None,
                },
            );
        if let Some((split_id, buffer_id)) = tab_hit {
            ed.active_window_mut().tab_context_menu =
                Some(TabContextMenu::new(buffer_id, split_id, ev.col, ev.row + 1));
        } else {
            ed.active_window_mut().tab_context_menu = None;
        }
        Ok(Disposition::Consumed)
    }
}
