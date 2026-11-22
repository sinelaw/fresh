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
        use crate::app::types::{ContextMenuKind, FileExplorerMenuChoice, TabMenuChoice};
        match kind {
            ContextMenuKind::Tab => {
                let selected = self
                    .active_window()
                    .tab_context_menu
                    .as_ref()
                    .and_then(|m| Some((m.highlighted_item()?, m.buffer_id, m.split_id)));
                self.active_window_mut().close_context_menus();
                match selected {
                    Some((TabMenuChoice::Builtin(item), buffer_id, split_id)) => {
                        return self.execute_tab_context_menu_action(item, buffer_id, split_id);
                    }
                    Some((TabMenuChoice::Plugin { handler }, buffer_id, _)) => {
                        let window = self.active_window_id();
                        let path = self
                            .tab_file(buffer_id)
                            .map(|p| fresh_core::api::WindowPath {
                                window,
                                value: p.to_string_lossy().into_owned(),
                            });
                        self.run_plugin_menu_handler(
                            &handler,
                            &fresh_core::api::TabMenuTarget {
                                window_id: window,
                                buffer_id,
                                path,
                            },
                        );
                    }
                    None => {}
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
                    .and_then(|m| m.highlighted_item());
                self.active_window_mut().close_context_menus();
                match selected {
                    Some(FileExplorerMenuChoice::Builtin(item)) => {
                        self.execute_file_explorer_context_menu_action(item);
                    }
                    Some(FileExplorerMenuChoice::Plugin { handler, target }) => {
                        self.run_plugin_menu_handler(&handler, &target);
                    }
                    None => {}
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

    /// The file backing a tab of the active window, as plugins see it in
    /// `BufferInfo.path`; `None` for a tab no file backs (a terminal, a
    /// plugin's panel).
    pub(crate) fn tab_file(
        &self,
        buffer_id: crate::model::event::BufferId,
    ) -> Option<std::path::PathBuf> {
        self.active_window()
            .tab_file_buffers()
            .find(|(id, _)| *id == buffer_id)
            .map(|(_, path)| path.to_path_buf())
    }

    /// Plugins' right-click menu items whose placement `offered` accepts,
    /// in the order the plugins added them.
    pub(crate) fn plugin_menu_entries(
        &self,
        offered: impl Fn(&fresh_core::api::PluginContextMenuPlacement) -> bool,
    ) -> Vec<crate::app::types::PluginMenuEntry> {
        self.plugin_context_menu_items
            .iter()
            .filter(|item| offered(&item.placement))
            .map(|item| crate::app::types::PluginMenuEntry {
                label: item.label.clone(),
                handler: item.handler.clone(),
            })
            .collect()
    }

    /// Call the plugin handler of a right-click menu item with what the menu
    /// was opened on. Runs like a palette command: the plugin sees state as
    /// of now, and a handler that throws is logged.
    fn run_plugin_menu_handler<T: serde::Serialize>(&mut self, handler: &str, target: &T) {
        #[cfg(feature = "plugins")]
        {
            let args = match serde_json::to_string(target) {
                Ok(args) => args,
                Err(e) => {
                    tracing::error!("menu item {handler}: {e}");
                    return;
                }
            };
            self.refresh_plugin_state_snapshot();
            let result =
                self.plugin_manager
                    .read()
                    .unwrap()
                    .execute_action_async(handler, Some(args), None);
            match result {
                Some(Ok(receiver)) => self
                    .pending_plugin_actions
                    .push((handler.to_string(), receiver)),
                Some(Err(e)) => {
                    self.set_status_message(
                        fresh_i18n::t!("view.plugin_error", error = e.to_string()).to_string(),
                    );
                    tracing::error!("menu item {handler}: {e}");
                }
                None => {}
            }
        }
        #[cfg(not(feature = "plugins"))]
        let _ = (handler, target);
    }
}
