//! The file explorer sidebar and its off-panel menu-clear guard.

use crate::app::types::HoverTarget;
use crate::widgets::LayoutBox;
use anyhow::Result as AnyhowResult;

use super::{ChromeComponent, ChromePointer, ChromeTreeBuilder, Disposition, Editor, PointerPress};

pub(crate) struct FileExplorer;

impl ChromeComponent for FileExplorer {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        if let Some(r) = ed.active_layout().file_explorer_area {
            t.rect("chrome:file_explorer", 100, r);
        }
        // Off-explorer right-click clears its menu (declining guard).
        t.full("chrome:clear_explorer_menu", 90);
    }

    fn hover(&self, ed: &mut Editor, bx: &LayoutBox, col: u16, row: u16) -> Option<HoverTarget> {
        if bx.kind != "chrome:file_explorer" {
            return None;
        }
        ed.hover_target_in_file_explorer(col, row)
    }

    fn on_pointer(
        &self,
        ed: &mut Editor,
        bx: &LayoutBox,
        ev: &ChromePointer,
    ) -> AnyhowResult<Disposition> {
        match (ev.press, bx.kind) {
            (PointerPress::Left, "chrome:file_explorer") => {
                if let Some(r) = ed.handle_click_file_explorer_area(ev.col, ev.row) {
                    r?;
                    return Ok(Disposition::Consumed);
                }
                Ok(Disposition::Pass)
            }
            (PointerPress::Right, "chrome:file_explorer") => {
                let Some(explorer_area) = ed.active_layout().file_explorer_area else {
                    return Ok(Disposition::Pass);
                };
                // The union box spans the whole explorer; the title row
                // is not a right-click target.
                if ev.row <= explorer_area.y {
                    return Ok(Disposition::Pass);
                }
                let relative_row = ev.row.saturating_sub(explorer_area.y + 1);
                let (is_multi, is_root_selected) =
                    if let Some(explorer) = ed.file_explorer_mut().as_mut() {
                        let mut clicked_is_root = false;
                        if let Some((node_id, _)) =
                            explorer.get_display_node_at_viewport_row(relative_row as usize)
                        {
                            explorer.set_selected(Some(node_id));
                            clicked_is_root = node_id == explorer.tree().root_id();
                        }
                        (explorer.has_multi_selection(), clicked_is_root)
                    } else {
                        (false, false)
                    };
                ed.active_window_mut().key_context =
                    crate::input::keybindings::KeyContext::FileExplorer;
                ed.active_window_mut().tab_context_menu = None;
                ed.active_window_mut().file_explorer_context_menu =
                    Some(crate::app::types::FileExplorerContextMenu::new(
                        ev.col,
                        ev.row + 1,
                        is_multi,
                        is_root_selected,
                    ));
                Ok(Disposition::Consumed)
            }
            (PointerPress::Right, "chrome:clear_explorer_menu") => {
                // Off-explorer right-click dismisses its menu, then
                // routing continues (act-then-continue guard).
                ed.active_window_mut().file_explorer_context_menu = None;
                Ok(Disposition::PassAfter)
            }
            (PointerPress::Double, "chrome:file_explorer") => {
                // Title row is not a double-click target (the union box
                // spans the whole explorer).
                if let Some(r) = ed.active_layout().file_explorer_area {
                    if ev.row <= r.y {
                        return Ok(Disposition::Pass);
                    }
                }
                // Open file AND focus editor.
                ed.file_explorer_open_file()?;
                Ok(Disposition::Consumed)
            }
            _ => Ok(Disposition::Pass),
        }
    }
}
