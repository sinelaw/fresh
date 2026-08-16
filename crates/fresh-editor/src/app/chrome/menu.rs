//! The menu bar, open dropdown/submenu boxes, and the full-frame
//! close guard while a menu is open.

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
}
