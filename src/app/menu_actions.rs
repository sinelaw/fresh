//! Menu-related action handlers.
//!
//! This module contains handlers for menu navigation, execution, and mouse interaction.

use super::Editor;
use crate::app::types::HoverTarget;
use crate::config::{Menu, MenuItem};
use crate::input::keybindings::Action;
use crate::primitives::display_width::str_width;
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
                menu.expand_dynamic_items();
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
            let total_menus = self.menus.menus.len() + self.menu_state.plugin_menus.len();
            self.menu_state.prev_menu(total_menus);
        }
    }

    /// Handle MenuRight action - open submenu or go to next menu.
    pub fn handle_menu_right(&mut self) {
        let all_menus = self.all_menus();
        if !self.menu_state.open_submenu(&all_menus) {
            let total_menus = self.menus.menus.len() + self.menu_state.plugin_menus.len();
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
    pub(crate) fn compute_menu_dropdown_hover(
        &self,
        col: u16,
        row: u16,
        menu: &Menu,
        menu_index: usize,
        all_menus: &[Menu],
    ) -> Option<HoverTarget> {
        // Calculate dropdown positions for the entire chain
        let mut x_offset = 0usize;
        for (idx, m) in all_menus.iter().enumerate() {
            if idx == menu_index {
                break;
            }
            x_offset += str_width(&m.label) + 3;
        }

        let mut current_items: &[MenuItem] = &menu.items;
        let mut current_x = x_offset as u16;
        let mut current_y = 1u16;

        let mut dropdown_rects = Vec::new();

        for depth in 0..=self.menu_state.submenu_path.len() {
            let max_width = current_items
                .iter()
                .map(|item| match item {
                    MenuItem::Action { label, .. } => str_width(label) + 20,
                    MenuItem::Submenu { label, .. } => str_width(label) + 20,
                    MenuItem::DynamicSubmenu { label, .. } => str_width(label) + 20,
                    MenuItem::Separator { .. } => 20,
                    MenuItem::Label { info } => str_width(info) + 4,
                })
                .max()
                .unwrap_or(20)
                .min(40) as u16;

            let dropdown_height = current_items.len() as u16 + 2;

            dropdown_rects.push((
                current_x,
                current_y,
                max_width,
                dropdown_height,
                depth,
                current_items.len(),
            ));

            if depth < self.menu_state.submenu_path.len() {
                let submenu_idx = self.menu_state.submenu_path[depth];
                if let Some(MenuItem::Submenu { items, .. }) = current_items.get(submenu_idx) {
                    let next_x = current_x + max_width - 1;
                    let next_y = current_y + submenu_idx as u16 + 1;
                    current_items = items;
                    current_x = next_x;
                    current_y = next_y;
                } else {
                    break;
                }
            }
        }

        // Check from deepest submenu to main dropdown
        for (dx, dy, width, height, depth, item_count) in dropdown_rects.iter().rev() {
            if col >= *dx && col < dx + width && row >= *dy && row < dy + height {
                let item_row = row.saturating_sub(*dy + 1);
                let item_idx = item_row as usize;

                if item_idx < *item_count {
                    if *depth == 0 {
                        return Some(HoverTarget::MenuDropdownItem(menu_index, item_idx));
                    } else {
                        return Some(HoverTarget::SubmenuItem(*depth, item_idx));
                    }
                }
            }
        }

        None
    }

    /// Handle click on menu dropdown chain (main dropdown and any open submenus).
    /// Returns Some(Ok(())) if click was handled, None if click was outside all dropdowns.
    pub(crate) fn handle_menu_dropdown_click(
        &mut self,
        col: u16,
        row: u16,
        menu: &Menu,
        menu_index: usize,
        all_menus: &[Menu],
    ) -> AnyhowResult<Option<AnyhowResult<()>>> {
        // Calculate dropdown positions for the entire chain
        // Similar to render_dropdown_chain but for hit testing

        // Calculate the x position of the top-level dropdown
        let mut x_offset = 0usize;
        for (idx, m) in all_menus.iter().enumerate() {
            if idx == menu_index {
                break;
            }
            x_offset += str_width(&m.label) + 3;
        }

        let mut current_items: &[MenuItem] = &menu.items;
        let mut current_x = x_offset as u16;
        let mut current_y = 1u16; // Below menu bar

        // Check each dropdown level from deepest to shallowest
        // This ensures clicks on nested submenus take priority
        let mut dropdown_rects = Vec::new();

        for depth in 0..=self.menu_state.submenu_path.len() {
            let max_width = current_items
                .iter()
                .map(|item| match item {
                    MenuItem::Action { label, .. } => str_width(label) + 20,
                    MenuItem::Submenu { label, .. } => str_width(label) + 20,
                    MenuItem::DynamicSubmenu { label, .. } => str_width(label) + 20,
                    MenuItem::Separator { .. } => 20,
                    MenuItem::Label { info } => str_width(info) + 4,
                })
                .max()
                .unwrap_or(20)
                .min(40) as u16;

            let dropdown_height = current_items.len() as u16 + 2;

            dropdown_rects.push((
                current_x,
                current_y,
                max_width,
                dropdown_height,
                depth,
                current_items.to_vec(),
            ));

            // Navigate to next level if there is one
            if depth < self.menu_state.submenu_path.len() {
                let submenu_idx = self.menu_state.submenu_path[depth];
                if let Some(MenuItem::Submenu { items, .. }) = current_items.get(submenu_idx) {
                    let next_x = current_x + max_width - 1;
                    let next_y = current_y + submenu_idx as u16 + 1;
                    current_items = items;
                    current_x = next_x;
                    current_y = next_y;
                } else {
                    break;
                }
            }
        }

        // Check clicks from deepest submenu to main dropdown
        for (dx, dy, width, height, depth, items) in dropdown_rects.iter().rev() {
            if col >= *dx && col < dx + width && row >= *dy && row < dy + height {
                // Click is inside this dropdown
                let item_row = row.saturating_sub(*dy + 1); // -1 for border
                let item_idx = item_row as usize;

                if item_idx < items.len() {
                    // Check what kind of item was clicked
                    match &items[item_idx] {
                        MenuItem::Separator { .. } | MenuItem::Label { .. } => {
                            // Clicked on separator or label - do nothing but consume the click
                            return Ok(Some(Ok(())));
                        }
                        MenuItem::Submenu {
                            items: submenu_items,
                            ..
                        } => {
                            // Clicked on submenu - open it
                            // First, truncate submenu_path to this depth
                            self.menu_state.submenu_path.truncate(*depth);
                            // Then add this submenu
                            if !submenu_items.is_empty() {
                                self.menu_state.submenu_path.push(item_idx);
                                self.menu_state.highlighted_item = Some(0);
                            }
                            return Ok(Some(Ok(())));
                        }
                        MenuItem::DynamicSubmenu { source, .. } => {
                            // Clicked on dynamic submenu - open it
                            self.menu_state.submenu_path.truncate(*depth);
                            let generated = MenuItem::generate_dynamic_items(source);
                            if !generated.is_empty() {
                                self.menu_state.submenu_path.push(item_idx);
                                self.menu_state.highlighted_item = Some(0);
                            }
                            return Ok(Some(Ok(())));
                        }
                        MenuItem::Action { action, args, .. } => {
                            // Clicked on action - execute it
                            let action_name = action.clone();
                            let action_args = args.clone();

                            self.close_menu_with_auto_hide();

                            if let Some(action) = Action::from_str(&action_name, &action_args) {
                                return Ok(Some(self.handle_action(action)));
                            }
                            return Ok(Some(Ok(())));
                        }
                    }
                }
                return Ok(Some(Ok(())));
            }
        }

        // Click was outside all dropdowns
        Ok(None)
    }
}
