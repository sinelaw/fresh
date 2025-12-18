//! Menu-related action handlers.
//!
//! This module contains handlers for menu navigation and execution.

use super::Editor;
use crate::config::Menu;
use crate::input::keybindings::Action;

impl Editor {
    /// Get all menus (config menus + plugin menus).
    fn all_menus(&self) -> Vec<Menu> {
        self.config
            .menu
            .menus
            .iter()
            .chain(self.menu_state.plugin_menus.iter())
            .cloned()
            .collect()
    }

    /// Handle MenuActivate action - opens the first menu.
    pub fn handle_menu_activate(&mut self) {
        self.on_editor_focus_lost();
        self.menu_state.open_menu(0);
    }

    /// Handle MenuClose action - closes the active menu.
    pub fn handle_menu_close(&mut self) {
        self.menu_state.close_menu();
    }

    /// Handle MenuLeft action - close submenu or go to previous menu.
    pub fn handle_menu_left(&mut self) {
        if !self.menu_state.close_submenu() {
            let total_menus = self.config.menu.menus.len() + self.menu_state.plugin_menus.len();
            self.menu_state.prev_menu(total_menus);
        }
    }

    /// Handle MenuRight action - open submenu or go to next menu.
    pub fn handle_menu_right(&mut self) {
        let all_menus = self.all_menus();
        if !self.menu_state.open_submenu(&all_menus) {
            let total_menus = self.config.menu.menus.len() + self.menu_state.plugin_menus.len();
            self.menu_state.next_menu(total_menus);
        }
    }

    /// Handle MenuUp action - select previous item in menu.
    pub fn handle_menu_up(&mut self) {
        if let Some(active_idx) = self.menu_state.active_menu {
            let all_menus = self.all_menus();
            if let Some(menu) = all_menus.get(active_idx) {
                self.menu_state.prev_item(menu);
            }
        }
    }

    /// Handle MenuDown action - select next item in menu.
    pub fn handle_menu_down(&mut self) {
        if let Some(active_idx) = self.menu_state.active_menu {
            let all_menus = self.all_menus();
            if let Some(menu) = all_menus.get(active_idx) {
                self.menu_state.next_item(menu);
            }
        }
    }

    /// Handle MenuExecute action - execute highlighted item or open submenu.
    ///
    /// Returns `Some(action)` if an action should be executed after this call.
    pub fn handle_menu_execute(&mut self) -> Option<Action> {
        let all_menus = self.all_menus();

        // Check if highlighted item is a submenu - if so, open it
        if self.menu_state.is_highlighted_submenu(&all_menus) {
            self.menu_state.open_submenu(&all_menus);
            return None;
        }

        // Update context before checking if action is enabled
        use crate::view::ui::context_keys;
        self.menu_state
            .context
            .set(context_keys::HAS_SELECTION, self.has_active_selection())
            .set(
                context_keys::FILE_EXPLORER_FOCUSED,
                self.key_context == crate::input::keybindings::KeyContext::FileExplorer,
            );

        if let Some((action_name, args)) = self.menu_state.get_highlighted_action(&all_menus) {
            // Close the menu
            self.menu_state.close_menu();

            // Parse and return the action
            if let Some(action) = Action::from_str(&action_name, &args) {
                Some(action)
            } else {
                // Treat as a plugin action (global Lua function)
                Some(Action::PluginAction(action_name))
            }
        } else {
            None
        }
    }

    /// Handle MenuOpen action - open a specific menu by name.
    pub fn handle_menu_open(&mut self, menu_name: &str) {
        self.on_editor_focus_lost();

        let all_menus = self.all_menus();
        for (idx, menu) in all_menus.iter().enumerate() {
            if menu.label.eq_ignore_ascii_case(menu_name) {
                self.menu_state.open_menu(idx);
                break;
            }
        }
    }
}
