//! The file explorer sidebar and its off-panel menu-clear guard.

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct FileExplorer;

impl ChromeComponent for FileExplorer {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        if let Some(r) = ed.active_layout().file_explorer_area {
            t.rect("chrome:file_explorer", 10, r);
        }
        // Off-explorer right-click clears its menu (declining guard).
        t.full("chrome:clear_explorer_menu", 9);
    }
}
