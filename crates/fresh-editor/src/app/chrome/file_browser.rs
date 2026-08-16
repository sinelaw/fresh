//! The file-open browser dialog.

use crate::app::types::HoverTarget;
use crate::widgets::LayoutBox;
use anyhow::Result as AnyhowResult;

use super::{ChromeComponent, ChromePointer, ChromeTreeBuilder, Disposition, Editor, PointerPress};

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

    fn hover(&self, ed: &Editor, _bx: &LayoutBox, col: u16, row: u16) -> Option<HoverTarget> {
        ed.compute_file_browser_hover(col, row)
    }

    fn on_pointer(
        &self,
        ed: &mut Editor,
        _bx: &LayoutBox,
        ev: &ChromePointer,
    ) -> AnyhowResult<Disposition> {
        if ev.press != PointerPress::Double {
            return Ok(Disposition::Pass);
        }
        if ed.handle_file_open_double_click(ev.col, ev.row) {
            return Ok(Disposition::Consumed);
        }
        Ok(Disposition::Pass)
    }
}
