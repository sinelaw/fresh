//! The file-open browser dialog.

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct FileBrowser;

impl ChromeComponent for FileBrowser {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        if ed.is_file_open_active() {
            if let Some(layout) = &ed.active_window().file_browser_layout {
                t.rect("chrome:file_browser", 13, layout.popup_area);
            } else {
                // No layout recorded yet: the dialog is modal, so the
                // full frame absorbs strays until the first paint.
                t.full("chrome:file_browser", 13);
            }
        }
    }
}
