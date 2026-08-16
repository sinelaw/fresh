//! Context menu (right-click menus): the menu box and its full-frame
//! close guard.

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct ContextMenu;

impl ChromeComponent for ContextMenu {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        if let Some(core) = ed.active_window().context_menu_core() {
            let frame = ed.active_chrome().last_frame;
            let r = core.rect(frame.width, frame.height);
            t.rect("chrome:context_menu", 18, r);
            // TRUE full-frame semantics: a click outside the menu box
            // dismisses it and is consumed.
            t.full("chrome:context_menu_close_guard", 18);
        }
    }
}
