//! Menu-related action handlers.
//!
//! This module contains handlers for menu navigation, execution, and mouse interaction.

use super::Editor;
use crate::app::types::HoverTarget;
use crate::config::{generate_dynamic_items, Menu, MenuExt, MenuItem};
use crate::input::keybindings::Action;
use anyhow::Result as AnyhowResult;

impl Editor {
    /// Replace `self.menus` and invalidate the expanded-menu cache. Use this
    /// in place of a direct `self.menus = …` assignment so the cached
    /// `DynamicSubmenu` expansion can never go stale relative to the
    /// underlying menu config.
    pub fn set_menus(&mut self, menus: crate::config::MenuConfig) {
        self.menus = menus;
        self.expanded_menus_cache.invalidate();
    }

    /// Find a built-in or plugin menu by its stable `id` ("View", "File",
    /// …) or, failing that, its display `label`, mutate it via `f`, and
    /// invalidate the expanded-menu cache. Returns `None` if no matching
    /// menu was found (in which case the cache is left alone).
    ///
    /// The id is tried first because labels are translated: a plugin
    /// contributing a row to the View menu (`add_menu_item`) would
    /// otherwise silently miss on every non-English locale.
    pub fn with_menu_by_label<F, R>(&mut self, label: &str, f: F) -> Option<R>
    where
        F: FnOnce(&mut Menu) -> R,
    {
        let matches = |m: &Menu| m.id.as_deref() == Some(label) || m.label == label;
        if let Some(idx) = self.menus.menus.iter().position(matches) {
            let r = f(&mut self.menus.menus[idx]);
            self.expanded_menus_cache.invalidate();
            return Some(r);
        }
        if let Some(idx) = self.menu_state.plugin_menus.iter().position(matches) {
            let r = f(&mut self.menu_state.plugin_menus[idx]);
            self.expanded_menus_cache.invalidate();
            return Some(r);
        }
        None
    }

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
        if !self.active_window_mut().menu_bar_visible {
            self.active_window_mut().menu_bar_visible = true;
            self.active_window_mut().menu_bar_auto_shown = true;
        }
        self.active_window_mut().on_editor_focus_lost();
        self.menu_state.open_menu(0);
    }

    /// Close the menu and auto-hide the menu bar if it was temporarily shown.
    /// Use this method instead of `menu_state.close_menu()` to ensure auto-hide works.
    pub fn close_menu_with_auto_hide(&mut self) {
        self.menu_state.close_menu();
        if self.active_window_mut().menu_bar_auto_shown {
            self.active_window_mut().menu_bar_visible = false;
            self.active_window_mut().menu_bar_auto_shown = false;
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
        let has_sel = self.has_active_selection();
        let fe_focused =
            self.active_window().key_context == crate::input::keybindings::KeyContext::FileExplorer;
        self.menu_state
            .context
            .set(context_keys::HAS_SELECTION, has_sel)
            .set(context_keys::FILE_EXPLORER_FOCUSED, fe_focused);

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
        if !self.active_window_mut().menu_bar_visible {
            self.active_window_mut().menu_bar_visible = true;
            self.active_window_mut().menu_bar_auto_shown = true;
        }
        self.active_window_mut().on_editor_focus_lost();

        let all_menus = self.all_menus();
        for (idx, menu) in all_menus.iter().enumerate() {
            // Match by id (locale-independent) rather than label (translated)
            if menu.match_id().eq_ignore_ascii_case(menu_name) {
                self.menu_state.open_menu(idx);
                break;
            }
        }
    }

    /// The menu's hover REACTION: an open menu follows the pointer —
    /// bar hover switches the open menu, dropdown hover opens/closes
    /// submenus and moves the highlight. Moved verbatim from the
    /// central `update_hover_target` ladder; called from
    /// `chrome::Menu::on_hover_change`. Returns true when menu state
    /// changed (needs a re-render beyond the target diff).
    pub(crate) fn menu_hover_reaction(&mut self, new_target: Option<&HoverTarget>) -> bool {
        let Some(active_menu_idx) = self.menu_state.active_menu else {
            return false;
        };
        let all_menus: Vec<crate::config::Menu> = self
            .menus
            .menus
            .iter()
            .chain(self.menu_state.plugin_menus.iter())
            .cloned()
            .collect();
        if let Some(HoverTarget::MenuBarItem(hovered_menu_idx)) = new_target {
            if *hovered_menu_idx != active_menu_idx {
                self.menu_state.open_menu(*hovered_menu_idx);
                return true; // Force re-render since menu changed
            }
        }

        // If hovering over a menu dropdown item, check if it's a submenu and open it
        if let Some(HoverTarget::MenuDropdownItem(_, item_idx)) = new_target {
            let item_idx = *item_idx;
            // If this item is the parent of the currently open submenu, keep it open.
            // This prevents blinking when hovering over the parent item of an open submenu.
            if self.menu_state.submenu_path.first() == Some(&item_idx) {
                tracing::trace!(
                    "menu hover: staying on submenu parent item_idx={}, submenu_path={:?}",
                    item_idx,
                    self.menu_state.submenu_path
                );
                return false;
            }

            // Clear any open submenus since we're at a different item in the main dropdown
            if !self.menu_state.submenu_path.is_empty() {
                tracing::trace!(
                    "menu hover: clearing submenu_path={:?} for different item_idx={}",
                    self.menu_state.submenu_path,
                    item_idx
                );
                self.menu_state.submenu_path.clear();
                self.menu_state.highlighted_item = Some(item_idx);
                return true;
            }

            // Check if the hovered item is a submenu
            if let Some(menu) = all_menus.get(active_menu_idx) {
                if let Some(crate::config::MenuItem::Submenu { items, .. }) =
                    menu.items.get(item_idx)
                {
                    if !items.is_empty() {
                        tracing::trace!("menu hover: opening submenu at item_idx={}", item_idx);
                        self.menu_state.submenu_path.push(item_idx);
                        self.menu_state.highlighted_item = Some(0);
                        return true;
                    }
                }
            }
            // Update highlighted item for non-submenu items too
            if self.menu_state.highlighted_item != Some(item_idx) {
                self.menu_state.highlighted_item = Some(item_idx);
                return true;
            }
        }

        // If hovering over a submenu item, handle submenu navigation
        if let Some(HoverTarget::SubmenuItem(depth, item_idx)) = new_target {
            let (depth, item_idx) = (*depth, *item_idx);
            // If this item is the parent of a currently open nested submenu, keep it open.
            // This prevents blinking when hovering over the parent item of an open nested submenu.
            // submenu_path[depth] stores the index of the nested submenu opened from this level.
            if self.menu_state.submenu_path.len() > depth
                && self.menu_state.submenu_path.get(depth) == Some(&item_idx)
            {
                tracing::trace!(
                    "menu hover: staying on nested submenu parent depth={}, item_idx={}, submenu_path={:?}",
                    depth,
                    item_idx,
                    self.menu_state.submenu_path
                );
                return false;
            }

            // Truncate submenu path to this depth (close any deeper submenus)
            if self.menu_state.submenu_path.len() > depth {
                tracing::trace!(
                    "menu hover: truncating submenu_path={:?} to depth={} for item_idx={}",
                    self.menu_state.submenu_path,
                    depth,
                    item_idx
                );
                self.menu_state.submenu_path.truncate(depth);
            }

            // Get the items at this depth
            if let Some(items) = self
                .menu_state
                .get_current_items(&all_menus, active_menu_idx)
            {
                // Check if hovered item is a submenu - if so, open it
                if let Some(crate::config::MenuItem::Submenu {
                    items: sub_items, ..
                }) = items.get(item_idx)
                {
                    if !sub_items.is_empty() && !self.menu_state.submenu_path.contains(&item_idx) {
                        tracing::trace!(
                            "menu hover: opening nested submenu at depth={}, item_idx={}",
                            depth,
                            item_idx
                        );
                        self.menu_state.submenu_path.push(item_idx);
                        self.menu_state.highlighted_item = Some(0);
                        return true;
                    }
                }
                // Update highlighted item
                if self.menu_state.highlighted_item != Some(item_idx) {
                    self.menu_state.highlighted_item = Some(item_idx);
                    return true;
                }
            }
        }
        false
    }

    /// Compute hover target for menu dropdown chain (main dropdown and submenus).
    /// Geometry derived from live state (`menu_layout_now`).
    pub(crate) fn compute_menu_dropdown_hover(
        &self,
        col: u16,
        row: u16,
        menu_index: usize,
    ) -> Option<HoverTarget> {
        let menu_layout = self.menu_layout_now()?;

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
    /// Hit-tests geometry derived from live state (`menu_layout_now`).
    pub(crate) fn handle_menu_dropdown_click(
        &mut self,
        col: u16,
        row: u16,
        menu: &Menu,
    ) -> AnyhowResult<Option<AnyhowResult<()>>> {
        use crate::view::ui::menu::MenuHit;

        let menu_layout = match self.menu_layout_now() {
            Some(layout) => layout,
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
