//! Context menu (right-click menus): the menu box and its full-frame
//! close guard.

use anyhow::Result as AnyhowResult;

use super::Editor;

/// Behavior owned by this component (moved from mouse_input.rs —
/// the handlers its arms dispatch to).
impl Editor {
    /// Activate the highlighted item of the open context menu: resolve the
    /// item + its payload from the concrete menu, dismiss the menu, then run
    /// the matching `execute_*` action. Shared by both the keyboard (Enter)
    /// and mouse (click) paths so activation lives in exactly one place — the
    /// pointer path now reaches it from the shell (`apply_ui_fact`), the
    /// keyboard path still from this component.
    pub(crate) fn activate_highlighted_context_menu(
        &mut self,
        kind: crate::app::types::ContextMenuKind,
    ) -> AnyhowResult<()> {
        use crate::app::types::ContextMenuKind;
        match kind {
            ContextMenuKind::Tab => {
                let selected = self
                    .active_window()
                    .tab_context_menu
                    .as_ref()
                    .map(|m| (m.highlighted_item(), m.buffer_id, m.split_id));
                self.active_window_mut().close_context_menus();
                if let Some((item, buffer_id, split_id)) = selected {
                    return self.execute_tab_context_menu_action(item, buffer_id, split_id);
                }
            }
            ContextMenuKind::NewTab => {
                let selected = self
                    .active_window()
                    .new_tab_menu
                    .as_ref()
                    .map(|m| (m.highlighted_item(), m.split_id));
                self.active_window_mut().close_context_menus();
                if let Some((item, split_id)) = selected {
                    return self.execute_new_tab_menu_action(item, split_id);
                }
            }
            ContextMenuKind::FileExplorer => {
                let selected = self
                    .active_window()
                    .file_explorer_context_menu
                    .as_ref()
                    .map(|m| m.highlighted_item());
                self.active_window_mut().close_context_menus();
                if let Some(item) = selected {
                    self.execute_file_explorer_context_menu_action(item);
                }
            }
            ContextMenuKind::CloseSplit => {
                let selected = self
                    .active_window()
                    .close_split_menu
                    .as_ref()
                    .map(|m| (m.highlighted_item(), m.split_id));
                self.active_window_mut().close_context_menus();
                if let Some((item, split_id)) = selected {
                    self.execute_close_split_menu_action(item, split_id);
                }
            }
        }
        Ok(())
    }
}
