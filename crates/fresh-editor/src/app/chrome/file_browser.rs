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
        match ev.press {
            PointerPress::Left => {
                if ed.is_file_open_active() && ed.handle_file_open_click(ev.col, ev.row) {
                    return Ok(Disposition::Consumed);
                }
                Ok(Disposition::Pass)
            }
            PointerPress::Double => {
                if ed.handle_file_open_double_click(ev.col, ev.row) {
                    return Ok(Disposition::Consumed);
                }
                Ok(Disposition::Pass)
            }
            PointerPress::Right => Ok(Disposition::Pass),
        }
    }

    fn on_wheel(
        &self,
        ed: &mut Editor,
        _bx: &LayoutBox,
        col: u16,
        row: u16,
        delta: i32,
    ) -> AnyhowResult<Disposition> {
        // Mirrors the popups arm (the two surfaces historically shared
        // one): browser list first, then the popup stack.
        if ed.is_file_open_active()
            && ed.is_mouse_over_file_browser(col, row)
            && ed.handle_file_open_scroll(delta)
        {
            return Ok(Disposition::Consumed);
        }
        if !ed.is_mouse_over_any_popup(col, row) {
            return Ok(Disposition::Pass);
        }
        ed.scroll_popup(delta);
        Ok(Disposition::Consumed)
    }
}
