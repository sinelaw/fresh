//! Menu-related action handlers.
//!
//! This module contains handlers for menu navigation, execution, and mouse interaction.

use super::Editor;
use crate::app::types::HoverTarget;
use crate::config::{generate_dynamic_items, Menu, MenuExt, MenuItem};
use crate::input::keybindings::Action;
use anyhow::Result as AnyhowResult;

impl Editor {
    /// Get all menus (built-in menus + plugin menus) with DynamicSubmenus expanded.
    fn all_menus(&self) -> Vec<Menu> {
        self.menus
            .menus
            .iter()
            .chain(self.menu_state.plugin_menus.iter())
            .cloned()
            .map(|mut menu| {
                menu.expand_dynamic_items(&self.menu_state.themes_dir);
                menu
            })
            .collect()
    }

    /// Handle MenuActivate action - opens the first menu.
    /// If the menu bar is hidden, it will be temporarily shown.
    pub fn handle_menu_activate(&mut self) {
        // Auto-show menu bar if hidden
        if !self.menu_bar_visible {
            self.menu_bar_visible = true;
            self.menu_bar_auto_shown = true;
        }
        self.on_editor_focus_lost();
        self.menu_state.open_menu(0);
    }

    /// Close the menu and auto-hide the menu bar if it was temporarily shown.
    /// Use this method instead of `menu_state.close_menu()` to ensure auto-hide works.
    pub fn close_menu_with_auto_hide(&mut self) {
        self.menu_state.close_menu();
        if self.menu_bar_auto_shown {
            self.menu_bar_visible = false;
            self.menu_bar_auto_shown = false;
        }
    }

    /// Handle MenuClose action - closes the active menu.
    /// If the menu bar was auto-shown, it will be hidden again.
    pub fn handle_menu_close(&mut self) {
        self.close_menu_with_auto_hide();
    }

    /// Handle MenuLeft action - close submenu or go to previous menu.
    pub fn handle_menu_left(&mut self) {
        if !self.menu_state.close_submenu() {
            let all_menus = self.all_menus();
            self.menu_state.prev_menu(&all_menus);
        }
    }

    /// Handle MenuRight action - open submenu or go to next menu.
    pub fn handle_menu_right(&mut self) {
        let all_menus = self.all_menus();
        if !self.menu_state.open_submenu(&all_menus) {
            self.menu_state.next_menu(&all_menus);
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
            // Close the menu with auto-hide support
            self.close_menu_with_auto_hide();

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
    /// If the menu bar is hidden, it will be temporarily shown.
    pub fn handle_menu_open(&mut self, menu_name: &str) {
        // Auto-show menu bar if hidden
        if !self.menu_bar_visible {
            self.menu_bar_visible = true;
            self.menu_bar_auto_shown = true;
        }
        self.on_editor_focus_lost();

        let all_menus = self.all_menus();
        for (idx, menu) in all_menus.iter().enumerate() {
            // Match by id (locale-independent) rather than label (translated)
            if menu.match_id().eq_ignore_ascii_case(menu_name) {
                self.menu_state.open_menu(idx);
                break;
            }
        }
    }

    /// Compute hover target for menu dropdown chain (main dropdown and submenus).
    /// Uses the cached menu layout from the previous render frame.
    pub(crate) fn compute_menu_dropdown_hover(
        &self,
        col: u16,
        row: u16,
        menu_index: usize,
    ) -> Option<HoverTarget> {
        let menu_layout = self.cached_layout.menu_layout.as_ref()?;

        // Check submenu items first (they're rendered on top)
        if let Some((depth, item_idx)) = menu_layout.submenu_item_at(col, row) {
            return Some(HoverTarget::SubmenuItem(depth, item_idx));
        }

        // Check main dropdown items
        if let Some(item_idx) = menu_layout.item_at(col, row) {
            return Some(HoverTarget::MenuDropdownItem(menu_index, item_idx));
        }

        None
    }

    /// Handle click on menu dropdown chain (main dropdown and any open submenus).
    /// Returns Some(Ok(())) if click was handled, None if click was outside all dropdowns.
    /// Uses the cached menu layout from the previous render frame for hit testing.
    pub(crate) fn handle_menu_dropdown_click(
        &mut self,
        col: u16,
        row: u16,
        menu: &Menu,
    ) -> AnyhowResult<Option<AnyhowResult<()>>> {
        use crate::view::ui::menu::MenuHit;

        let menu_layout = match &self.cached_layout.menu_layout {
            Some(layout) => layout.clone(),
            None => return Ok(None),
        };

        // Use the layout to determine what was clicked
        let hit = match menu_layout.hit_test(col, row) {
            Some(MenuHit::DropdownItem(item_idx)) => (0, item_idx),
            Some(MenuHit::SubmenuItem { depth, index }) => (depth, index),
            _ => return Ok(None), // Click outside dropdown areas
        };

        let (depth, item_idx) = hit;

        // Navigate to the clicked item in the menu structure
        let items = if depth == 0 {
            // Main dropdown items
            menu.items.clone()
        } else {
            // Navigate through submenu path to find items at this depth
            let mut current_items = menu.items.clone();
            for d in 0..depth {
                if d < self.menu_state.submenu_path.len() {
                    let submenu_idx = self.menu_state.submenu_path[d];
                    match current_items.get(submenu_idx) {
                        Some(MenuItem::Submenu { items, .. }) => {
                            current_items = items.clone();
                        }
                        Some(MenuItem::DynamicSubmenu { source, .. }) => {
                            current_items =
                                generate_dynamic_items(source, &self.menu_state.themes_dir);
                        }
                        _ => return Ok(Some(Ok(()))),
                    }
                } else {
                    return Ok(Some(Ok(())));
                }
            }
            current_items
        };

        let Some(item) = items.get(item_idx) else {
            return Ok(Some(Ok(())));
        };

        // Handle the clicked item
        match item {
            MenuItem::Separator { .. } | MenuItem::Label { .. } => {
                // Clicked on separator or label - do nothing but consume the click
                Ok(Some(Ok(())))
            }
            MenuItem::Submenu {
                items: submenu_items,
                ..
            } => {
                // Clicked on submenu - open it
                self.menu_state.submenu_path.truncate(depth);
                if !submenu_items.is_empty() {
                    self.menu_state.submenu_path.push(item_idx);
                    self.menu_state.highlighted_item = Some(0);
                }
                Ok(Some(Ok(())))
            }
            MenuItem::DynamicSubmenu { source, .. } => {
                // Clicked on dynamic submenu - open it
                self.menu_state.submenu_path.truncate(depth);
                let generated = generate_dynamic_items(source, &self.menu_state.themes_dir);
                if !generated.is_empty() {
                    self.menu_state.submenu_path.push(item_idx);
                    self.menu_state.highlighted_item = Some(0);
                }
                Ok(Some(Ok(())))
            }
            MenuItem::Action { action, args, .. } => {
                // Clicked on action - execute it
                let action_name = action.clone();
                let action_args = args.clone();

                self.close_menu_with_auto_hide();

                if let Some(action) = Action::from_str(&action_name, &action_args) {
                    return Ok(Some(self.handle_action(action)));
                }
                Ok(Some(Ok(())))
            }
        }
    }
}
