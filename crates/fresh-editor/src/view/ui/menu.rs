//! The menu bar's state, styles and item rules. Its description is
//! `view::shell::menu`; the walk that laid it out by hand is gone (design §3.4).

use crate::config::{generate_dynamic_items, Menu, MenuItem, MenuItemExt};

// Re-export context_keys from the shared types module
pub use crate::types::context_keys;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BarLabelStyle {
    Normal,
    /// The menu whose dropdown is open. Bold, as the painter had it.
    Active,
    Hovered,
}

impl BarLabelStyle {
    pub(crate) fn of(is_active: bool, is_hovered: bool) -> BarLabelStyle {
        if is_active {
            BarLabelStyle::Active
        } else if is_hovered {
            BarLabelStyle::Hovered
        } else {
            BarLabelStyle::Normal
        }
    }

    /// The `(fg, bg)` theme keys this label reads.
    ///
    /// One pair, for every consumer: the inspector's provenance, the ratatui
    /// painter's colours, and the shell's `ThemeKey`. Hover used to report the
    /// *resting* keys here while `style()` returned the hover colours — the two
    /// disagreed, and nothing noticed because only the inspector read this one.
    pub(crate) fn theme_keys(self) -> (&'static str, &'static str) {
        match self {
            BarLabelStyle::Normal => ("ui.menu_fg", "ui.menu_bg"),
            BarLabelStyle::Active => ("ui.menu_active_fg", "ui.menu_active_bg"),
            BarLabelStyle::Hovered => ("ui.menu_hover_fg", "ui.menu_hover_bg"),
        }
    }

    /// The description's name for this label, and for its mnemonic character —
    /// which differs only by an underline, and an underline is part of how a
    /// run looks, so it is part of the name.
    pub(crate) fn shell_theme(self, mnemonic: bool) -> String {
        use crate::app::shell_host::shell_theme;
        let (fg, bg) = self.theme_keys();
        // Structural attributes, not themed ones: the active label is bold
        // because it is active, the mnemonic underlined because it is a
        // mnemonic. They compose, so `active + mnemonic` needs no sixth name.
        let mut attrs: Vec<&str> = Vec::new();
        if self == BarLabelStyle::Active {
            attrs.push("bold");
        }
        if mnemonic {
            attrs.push("underline");
        }
        shell_theme::attrs(fg, bg, &attrs)
    }
}

/// How one dropdown row is coloured.
///
/// The single style decision for a row: paint reads it for a `Style`, the
/// theme inspector reads it for provenance keys, and the shell's description
/// reads it for a `ThemeKey`. Three consumers, one ladder — they used to be
/// two ladders that disagreed about hover.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MenuRowStyle {
    Normal,
    Highlighted,
    Hovered,
    Disabled,
    /// A `Label` item: disabled ink on the ordinary dropdown ground.
    Info,
    Separator,
}

impl MenuRowStyle {
    /// The ladder itself. `enabled` is only meaningful for the kinds that can
    /// be disabled; the caller passes what the item resolved to.
    pub(crate) fn of(
        item: &MenuItem,
        enabled: bool,
        is_highlighted: bool,
        is_hovered: bool,
        has_open_submenu: bool,
    ) -> MenuRowStyle {
        match item {
            MenuItem::Separator { .. } => MenuRowStyle::Separator,
            MenuItem::Label { .. } => MenuRowStyle::Info,
            MenuItem::Action { .. } if !enabled => MenuRowStyle::Disabled,
            _ if is_highlighted || has_open_submenu => MenuRowStyle::Highlighted,
            _ if is_hovered => MenuRowStyle::Hovered,
            _ => MenuRowStyle::Normal,
        }
    }

    /// The `(fg, bg)` provenance keys the theme inspector reports.
    pub(crate) fn theme_keys(self) -> (&'static str, &'static str) {
        match self {
            MenuRowStyle::Normal => ("ui.menu_dropdown_fg", "ui.menu_dropdown_bg"),
            MenuRowStyle::Highlighted => ("ui.menu_highlight_fg", "ui.menu_highlight_bg"),
            MenuRowStyle::Hovered => ("ui.menu_hover_fg", "ui.menu_hover_bg"),
            MenuRowStyle::Disabled => ("ui.menu_disabled_fg", "ui.menu_disabled_bg"),
            MenuRowStyle::Info => ("ui.menu_disabled_fg", "ui.menu_dropdown_bg"),
            MenuRowStyle::Separator => ("ui.menu_separator_fg", "ui.menu_dropdown_bg"),
        }
    }

    /// The name the shell's description carries, resolved by the fold's
    /// palette. One name per style, so the two backends cannot drift.
    pub(crate) fn shell_theme(self) -> String {
        let (fg, bg) = self.theme_keys();
        crate::app::shell_host::shell_theme::pair(fg, bg)
    }
}

// Re-export MenuContext from fresh-core so existing editor code keeps compiling.
pub use fresh_core::menu::MenuContext;

/// Whether a menu item is enabled given the current menu context. Shared by the
/// TUI renderer and the web `menu_view` projection so both frontends agree on
/// item state from one definition (see view/scene.rs).
pub(crate) fn is_menu_item_enabled(item: &MenuItem, context: &MenuContext) -> bool {
    match item {
        MenuItem::Action { when, .. } => {
            match when.as_deref() {
                Some(condition) => context.get(condition),
                None => true, // No condition means always enabled
            }
        }
        _ => true,
    }
}

/// Whether a checkbox menu item is checked given the current context. Shared by
/// the TUI renderer and the web `menu_view` projection.
pub(crate) fn is_checkbox_checked(checkbox: &Option<String>, context: &MenuContext) -> bool {
    match checkbox.as_deref() {
        Some(name) => context.get(name),
        None => false,
    }
}

/// Whether a top-level menu is visible given its `when` condition. Shared by the
/// shell's description and the web `menu_view` projection so menu visibility is
/// computed in one place rather than independently per frontend.
pub(crate) fn is_menu_visible(menu: &Menu, context: &MenuContext) -> bool {
    match &menu.when {
        Some(condition) => context.get(condition),
        None => true, // No condition = always visible
    }
}

/// Menu bar state (tracks which menu is open and which item is highlighted)
///
/// TODO: The menu system design could be improved to handle dynamic items better.
/// Currently, `themes_dir` is stored here to support `DynamicSubmenu` expansion.
/// A cleaner approach might be:
/// 1. Accept a pure data value representing the entire expanded menu system
/// 2. Have the "dynamic" item expansion done externally by the caller
/// 3. Allow updating the menu data by re-setting with a new expanded value
///
/// This would decouple menu rendering/navigation from theme loading concerns.
#[derive(Debug, Clone)]
pub struct MenuState {
    /// Index of the currently open menu (None if menu bar is closed)
    pub active_menu: Option<usize>,
    /// Index of the highlighted item within the active menu or current submenu
    pub highlighted_item: Option<usize>,
    /// Path of indices into nested submenus (empty = at top level menu)
    /// Each element is the index of the submenu item that was opened
    pub submenu_path: Vec<usize>,
    /// Runtime menu additions from plugins
    pub plugin_menus: Vec<Menu>,
    /// Context containing named boolean states for conditions and checkboxes
    pub context: MenuContext,
    /// Path to the themes directory for expanding DynamicSubmenu items.
    /// See TODO above for potential design improvement.
    pub themes_dir: std::path::PathBuf,
}

impl MenuState {
    pub fn new(themes_dir: std::path::PathBuf) -> Self {
        Self {
            active_menu: None,
            highlighted_item: None,
            submenu_path: Vec::new(),
            plugin_menus: Vec::new(),
            context: MenuContext::default(),
            themes_dir,
        }
    }

    /// Create a MenuState for testing with an empty themes directory.
    #[cfg(test)]
    pub fn for_testing() -> Self {
        Self::new(std::path::PathBuf::new())
    }

    /// Open a menu by index
    pub fn open_menu(&mut self, index: usize) {
        self.active_menu = Some(index);
        self.highlighted_item = Some(0);
        self.submenu_path.clear();
    }

    /// Close the currently open menu (and all submenus)
    pub fn close_menu(&mut self) {
        self.active_menu = None;
        self.highlighted_item = None;
        self.submenu_path.clear();
    }

    /// Navigate to the next menu (right) - only at top level
    /// Skips menus that are hidden (where `when` condition evaluates to false)
    pub fn next_menu(&mut self, menus: &[Menu]) {
        let Some(active) = self.active_menu else {
            return;
        };
        let total = menus.len();
        if total == 0 {
            return;
        }

        // Find the next visible menu, wrapping around
        for i in 1..=total {
            let next_idx = (active + i) % total;
            if self.is_menu_visible(&menus[next_idx]) {
                self.active_menu = Some(next_idx);
                self.highlighted_item = Some(0);
                self.submenu_path.clear();
                return;
            }
        }
        // No visible menu found, stay on current
    }

    /// Navigate to the previous menu (left) - only at top level
    /// Skips menus that are hidden (where `when` condition evaluates to false)
    pub fn prev_menu(&mut self, menus: &[Menu]) {
        let Some(active) = self.active_menu else {
            return;
        };
        let total = menus.len();
        if total == 0 {
            return;
        }

        // Find the previous visible menu, wrapping around
        for i in 1..=total {
            let prev_idx = (active + total - i) % total;
            if self.is_menu_visible(&menus[prev_idx]) {
                self.active_menu = Some(prev_idx);
                self.highlighted_item = Some(0);
                self.submenu_path.clear();
                return;
            }
        }
        // No visible menu found, stay on current
    }

    /// Check if a menu is visible based on its `when` condition. Delegates to
    /// the shared `is_menu_visible` so the TUI and the web projection agree.
    fn is_menu_visible(&self, menu: &Menu) -> bool {
        is_menu_visible(menu, &self.context)
    }

    /// Check if we're currently in a submenu
    pub fn in_submenu(&self) -> bool {
        !self.submenu_path.is_empty()
    }

    /// Get the current submenu depth (0 = top level menu)
    pub fn submenu_depth(&self) -> usize {
        self.submenu_path.len()
    }

    /// Open a submenu at the current highlighted item
    /// Returns true if a submenu was opened, false if the item wasn't a submenu
    pub fn open_submenu(&mut self, menus: &[Menu]) -> bool {
        let Some(active_idx) = self.active_menu else {
            return false;
        };
        let Some(highlighted) = self.highlighted_item else {
            return false;
        };

        // Get the current menu items
        let Some(menu) = menus.get(active_idx) else {
            return false;
        };
        let Some(items) = self.get_current_items_cloned(menu) else {
            return false;
        };

        // Check if highlighted item is a submenu (including DynamicSubmenu which was expanded)
        if let Some(item) = items.get(highlighted) {
            match item {
                MenuItem::Submenu {
                    items: submenu_items,
                    ..
                } if !submenu_items.is_empty() => {
                    self.submenu_path.push(highlighted);
                    self.highlighted_item = Some(0);
                    return true;
                }
                MenuItem::DynamicSubmenu { source, .. } => {
                    // Generate items to check if non-empty
                    let generated = generate_dynamic_items(source, &self.themes_dir);
                    if !generated.is_empty() {
                        self.submenu_path.push(highlighted);
                        self.highlighted_item = Some(0);
                        return true;
                    }
                }
                _ => {}
            }
        }
        false
    }

    /// Close the current submenu and go back to parent
    /// Returns true if a submenu was closed, false if already at top level
    pub fn close_submenu(&mut self) -> bool {
        if let Some(parent_idx) = self.submenu_path.pop() {
            self.highlighted_item = Some(parent_idx);
            true
        } else {
            false
        }
    }

    /// Get the menu items at the current submenu level
    pub fn get_current_items<'a>(
        &self,
        menus: &'a [Menu],
        active_idx: usize,
    ) -> Option<&'a [MenuItem]> {
        let menu = menus.get(active_idx)?;
        let mut items: &[MenuItem] = &menu.items;

        for &idx in &self.submenu_path {
            match items.get(idx)? {
                MenuItem::Submenu {
                    items: submenu_items,
                    ..
                } => {
                    items = submenu_items;
                }
                _ => return None,
            }
        }

        Some(items)
    }

    /// Get owned vec of current items (for use when Menu is cloned)
    /// DynamicSubmenus are expanded to regular Submenus
    pub fn get_current_items_cloned(&self, menu: &Menu) -> Option<Vec<MenuItem>> {
        // Expand all items (handles DynamicSubmenu -> Submenu)
        let mut items: Vec<MenuItem> = menu
            .items
            .iter()
            .map(|i| i.expand_dynamic(&self.themes_dir))
            .collect();

        for &idx in &self.submenu_path {
            match items.get(idx)?.expand_dynamic(&self.themes_dir) {
                MenuItem::Submenu {
                    items: submenu_items,
                    ..
                } => {
                    items = submenu_items;
                }
                _ => return None,
            }
        }

        Some(items)
    }

    /// Navigate to the next item in the current menu/submenu (down)
    pub fn next_item(&mut self, menu: &Menu) {
        let Some(idx) = self.highlighted_item else {
            return;
        };

        // Get current items (may be in a submenu)
        let Some(items) = self.get_current_items_cloned(menu) else {
            return;
        };

        if items.is_empty() {
            return;
        }

        // Skip separators and disabled items
        let mut next = (idx + 1) % items.len();
        while next != idx && self.should_skip_item(&items[next]) {
            next = (next + 1) % items.len();
        }
        self.highlighted_item = Some(next);
    }

    /// Navigate to the previous item in the current menu/submenu (up)
    pub fn prev_item(&mut self, menu: &Menu) {
        let Some(idx) = self.highlighted_item else {
            return;
        };

        // Get current items (may be in a submenu)
        let Some(items) = self.get_current_items_cloned(menu) else {
            return;
        };

        if items.is_empty() {
            return;
        }

        // Skip separators and disabled items
        let total = items.len();
        let mut prev = (idx + total - 1) % total;
        while prev != idx && self.should_skip_item(&items[prev]) {
            prev = (prev + total - 1) % total;
        }
        self.highlighted_item = Some(prev);
    }

    /// Check if a menu item should be skipped during navigation
    fn should_skip_item(&self, item: &MenuItem) -> bool {
        match item {
            MenuItem::Separator { .. } => true,
            MenuItem::Action { when, .. } => {
                // Skip disabled items (when condition evaluates to false)
                match when.as_deref() {
                    Some(condition) => !self.context.get(condition),
                    None => false, // No condition means enabled, don't skip
                }
            }
            _ => false,
        }
    }

    /// Get the currently highlighted action (if any)
    /// This navigates through the submenu path to find the currently highlighted item
    pub fn get_highlighted_action(
        &self,
        menus: &[Menu],
    ) -> Option<(String, std::collections::HashMap<String, serde_json::Value>)> {
        let active_menu = self.active_menu?;
        let highlighted_item = self.highlighted_item?;

        // Get the items at the current submenu level, handling DynamicSubmenu
        let menu = menus.get(active_menu)?;
        let items = self.get_current_items_cloned(menu)?;
        let item = items.get(highlighted_item)?;

        match item {
            MenuItem::Action { action, args, .. } => {
                if is_menu_item_enabled(item, &self.context) {
                    Some((action.clone(), args.clone()))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Check if the currently highlighted item is a submenu
    pub fn is_highlighted_submenu(&self, menus: &[Menu]) -> bool {
        let Some(active_menu) = self.active_menu else {
            return false;
        };
        let Some(highlighted_item) = self.highlighted_item else {
            return false;
        };

        // Use get_current_items_cloned to handle DynamicSubmenu
        let Some(menu) = menus.get(active_menu) else {
            return false;
        };
        let Some(items) = self.get_current_items_cloned(menu) else {
            return false;
        };

        matches!(
            items.get(highlighted_item),
            Some(MenuItem::Submenu { .. } | MenuItem::DynamicSubmenu { .. })
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::MenuConfig;
    use std::collections::HashMap;

    fn create_test_menus() -> Vec<Menu> {
        vec![
            Menu {
                id: None,
                label: "File".to_string(),
                items: vec![
                    MenuItem::Action {
                        label: "New".to_string(),
                        action: "new_file".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Save".to_string(),
                        action: "save".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Quit".to_string(),
                        action: "quit".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                ],
                when: None,
            },
            Menu {
                id: None,
                label: "Edit".to_string(),
                items: vec![
                    MenuItem::Action {
                        label: "Undo".to_string(),
                        action: "undo".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Redo".to_string(),
                        action: "redo".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                ],
                when: None,
            },
            Menu {
                id: None,
                label: "View".to_string(),
                items: vec![MenuItem::Action {
                    label: "Toggle Explorer".to_string(),
                    action: "toggle_file_explorer".to_string(),
                    args: HashMap::new(),
                    when: None,
                    checkbox: None,
                }],
                when: None,
            },
        ]
    }

    #[test]
    fn test_menu_state_default() {
        let state = MenuState::for_testing();
        assert_eq!(state.active_menu, None);
        assert_eq!(state.highlighted_item, None);
        assert!(state.plugin_menus.is_empty());
    }

    #[test]
    fn test_menu_state_open_menu() {
        let mut state = MenuState::for_testing();
        state.open_menu(2);
        assert_eq!(state.active_menu, Some(2));
        assert_eq!(state.highlighted_item, Some(0));
    }

    #[test]
    fn test_menu_state_close_menu() {
        let mut state = MenuState::for_testing();
        state.open_menu(1);
        state.close_menu();
        assert_eq!(state.active_menu, None);
        assert_eq!(state.highlighted_item, None);
    }

    #[test]
    fn test_menu_state_next_menu() {
        let mut state = MenuState::for_testing();
        let menus = create_test_menus();
        state.open_menu(0);

        state.next_menu(&menus);
        assert_eq!(state.active_menu, Some(1));

        state.next_menu(&menus);
        assert_eq!(state.active_menu, Some(2));

        // Wrap around
        state.next_menu(&menus);
        assert_eq!(state.active_menu, Some(0));
    }

    #[test]
    fn test_menu_state_prev_menu() {
        let mut state = MenuState::for_testing();
        let menus = create_test_menus();
        state.open_menu(0);

        // Wrap around backwards
        state.prev_menu(&menus);
        assert_eq!(state.active_menu, Some(2));

        state.prev_menu(&menus);
        assert_eq!(state.active_menu, Some(1));

        state.prev_menu(&menus);
        assert_eq!(state.active_menu, Some(0));
    }

    #[test]
    fn test_menu_state_next_item_skips_separator() {
        let mut state = MenuState::for_testing();
        let menus = create_test_menus();
        state.open_menu(0);

        // highlighted_item starts at 0 (New)
        assert_eq!(state.highlighted_item, Some(0));

        // Next should skip separator and go to Save (index 2)
        state.next_item(&menus[0]);
        assert_eq!(state.highlighted_item, Some(2));

        // Next goes to Quit (index 3)
        state.next_item(&menus[0]);
        assert_eq!(state.highlighted_item, Some(3));

        // Wrap around to New (index 0)
        state.next_item(&menus[0]);
        assert_eq!(state.highlighted_item, Some(0));
    }

    #[test]
    fn test_menu_state_prev_item_skips_separator() {
        let mut state = MenuState::for_testing();
        let menus = create_test_menus();
        state.open_menu(0);
        state.highlighted_item = Some(2); // Start at Save

        // Prev should skip separator and go to New (index 0)
        state.prev_item(&menus[0]);
        assert_eq!(state.highlighted_item, Some(0));

        // Wrap around backwards to Quit (index 3)
        state.prev_item(&menus[0]);
        assert_eq!(state.highlighted_item, Some(3));
    }

    #[test]
    fn test_get_highlighted_action() {
        let mut state = MenuState::for_testing();
        let menus = create_test_menus();
        state.open_menu(0);
        state.highlighted_item = Some(2); // Save action

        let action = state.get_highlighted_action(&menus);
        assert!(action.is_some());
        let (action_name, _args) = action.unwrap();
        assert_eq!(action_name, "save");
    }

    #[test]
    fn test_menu_item_when_requires_selection() {
        let mut state = MenuState::for_testing();
        let select_menu = Menu {
            id: None,
            label: "Edit".to_string(),
            items: vec![MenuItem::Action {
                label: "Find in Selection".to_string(),
                action: "find_in_selection".to_string(),
                args: HashMap::new(),
                when: Some(context_keys::HAS_SELECTION.to_string()),
                checkbox: None,
            }],
            when: None,
        };
        state.open_menu(0);
        state.highlighted_item = Some(0);

        // Without has_selection set, action should be disabled
        assert!(state
            .get_highlighted_action(std::slice::from_ref(&select_menu))
            .is_none());

        // With has_selection set to true, action should be enabled
        state.context.set(context_keys::HAS_SELECTION, true);
        assert!(state.get_highlighted_action(&[select_menu]).is_some());
    }

    #[test]
    fn test_get_highlighted_action_none_when_closed() {
        let state = MenuState::for_testing();
        let menus = create_test_menus();
        assert!(state.get_highlighted_action(&menus).is_none());
    }

    #[test]
    fn test_get_highlighted_action_none_for_separator() {
        let mut state = MenuState::for_testing();
        let menus = create_test_menus();
        state.open_menu(0);
        state.highlighted_item = Some(1); // Separator

        assert!(state.get_highlighted_action(&menus).is_none());
    }

    #[test]
    fn test_menu_config_json_parsing() {
        let json = r#"{
            "menus": [
                {
                    "label": "File",
                    "items": [
                        { "label": "New", "action": "new_file" },
                        { "separator": true },
                        { "label": "Save", "action": "save" }
                    ]
                }
            ]
        }"#;

        let config: MenuConfig = serde_json::from_str(json).unwrap();
        assert_eq!(config.menus.len(), 1);
        assert_eq!(config.menus[0].label, "File");
        assert_eq!(config.menus[0].items.len(), 3);

        match &config.menus[0].items[0] {
            MenuItem::Action { label, action, .. } => {
                assert_eq!(label, "New");
                assert_eq!(action, "new_file");
            }
            _ => panic!("Expected Action"),
        }

        assert!(matches!(
            config.menus[0].items[1],
            MenuItem::Separator { .. }
        ));

        match &config.menus[0].items[2] {
            MenuItem::Action { label, action, .. } => {
                assert_eq!(label, "Save");
                assert_eq!(action, "save");
            }
            _ => panic!("Expected Action"),
        }
    }

    #[test]
    fn test_menu_item_with_args() {
        let json = r#"{
            "label": "Go to Line",
            "action": "goto_line",
            "args": { "line": 42 }
        }"#;

        let item: MenuItem = serde_json::from_str(json).unwrap();
        match item {
            MenuItem::Action {
                label,
                action,
                args,
                ..
            } => {
                assert_eq!(label, "Go to Line");
                assert_eq!(action, "goto_line");
                assert_eq!(args.get("line").unwrap().as_i64(), Some(42));
            }
            _ => panic!("Expected Action with args"),
        }
    }

    #[test]
    fn test_empty_menu_config() {
        let json = r#"{ "menus": [] }"#;
        let config: MenuConfig = serde_json::from_str(json).unwrap();
        assert!(config.menus.is_empty());
    }

    #[test]
    fn test_menu_mnemonic_lookup() {
        use crate::config::Config;
        use crate::input::keybindings::KeybindingResolver;

        let config = Config::default();
        let resolver = KeybindingResolver::new(&config);

        // Check that default Alt+letter bindings are configured
        assert_eq!(resolver.find_menu_mnemonic("File"), Some('f'));
        assert_eq!(resolver.find_menu_mnemonic("Edit"), Some('e'));
        assert_eq!(resolver.find_menu_mnemonic("View"), Some('v'));
        assert_eq!(resolver.find_menu_mnemonic("Selection"), Some('s'));
        assert_eq!(resolver.find_menu_mnemonic("Go"), Some('g'));
        assert_eq!(resolver.find_menu_mnemonic("Help"), Some('h'));

        // Case-insensitive matching
        assert_eq!(resolver.find_menu_mnemonic("file"), Some('f'));
        assert_eq!(resolver.find_menu_mnemonic("FILE"), Some('f'));

        // Non-existent menu should return None
        assert_eq!(resolver.find_menu_mnemonic("NonExistent"), None);
    }

    fn create_menu_with_submenus() -> Vec<Menu> {
        vec![Menu {
            id: None,
            label: "View".to_string(),
            items: vec![
                MenuItem::Action {
                    label: "Toggle Explorer".to_string(),
                    action: "toggle_file_explorer".to_string(),
                    args: HashMap::new(),
                    when: None,
                    checkbox: None,
                },
                MenuItem::Submenu {
                    label: "Terminal".to_string(),
                    items: vec![
                        MenuItem::Action {
                            label: "Open Terminal".to_string(),
                            action: "open_terminal".to_string(),
                            args: HashMap::new(),
                            when: None,
                            checkbox: None,
                        },
                        MenuItem::Action {
                            label: "Close Terminal".to_string(),
                            action: "close_terminal".to_string(),
                            args: HashMap::new(),
                            when: None,
                            checkbox: None,
                        },
                        MenuItem::Submenu {
                            label: "Terminal Settings".to_string(),
                            items: vec![MenuItem::Action {
                                label: "Font Size".to_string(),
                                action: "terminal_font_size".to_string(),
                                args: HashMap::new(),
                                when: None,
                                checkbox: None,
                            }],
                        },
                    ],
                },
                MenuItem::Separator { separator: true },
                MenuItem::Action {
                    label: "Zoom In".to_string(),
                    action: "zoom_in".to_string(),
                    args: HashMap::new(),
                    when: None,
                    checkbox: None,
                },
            ],
            when: None,
        }]
    }

    #[test]
    fn test_submenu_open_and_close() {
        let mut state = MenuState::for_testing();
        let menus = create_menu_with_submenus();

        state.open_menu(0);
        assert!(state.submenu_path.is_empty());
        assert!(!state.in_submenu());

        // Move to Terminal submenu item (index 1)
        state.highlighted_item = Some(1);

        // Open the submenu
        assert!(state.open_submenu(&menus));
        assert_eq!(state.submenu_path, vec![1]);
        assert!(state.in_submenu());
        assert_eq!(state.submenu_depth(), 1);
        assert_eq!(state.highlighted_item, Some(0)); // Reset to first item

        // Close the submenu
        assert!(state.close_submenu());
        assert!(state.submenu_path.is_empty());
        assert!(!state.in_submenu());
        assert_eq!(state.highlighted_item, Some(1)); // Restored to parent item
    }

    #[test]
    fn test_nested_submenu() {
        let mut state = MenuState::for_testing();
        let menus = create_menu_with_submenus();

        state.open_menu(0);
        state.highlighted_item = Some(1); // Terminal submenu

        // Open first level submenu
        assert!(state.open_submenu(&menus));
        assert_eq!(state.submenu_depth(), 1);

        // Move to Terminal Settings (nested submenu at index 2)
        state.highlighted_item = Some(2);

        // Open second level submenu
        assert!(state.open_submenu(&menus));
        assert_eq!(state.submenu_path, vec![1, 2]);
        assert_eq!(state.submenu_depth(), 2);
        assert_eq!(state.highlighted_item, Some(0));

        // Close back to first level
        assert!(state.close_submenu());
        assert_eq!(state.submenu_path, vec![1]);
        assert_eq!(state.highlighted_item, Some(2));

        // Close back to main menu
        assert!(state.close_submenu());
        assert!(state.submenu_path.is_empty());
        assert_eq!(state.highlighted_item, Some(1));

        // Can't close further
        assert!(!state.close_submenu());
    }

    #[test]
    fn test_get_highlighted_action_in_submenu() {
        let mut state = MenuState::for_testing();
        let menus = create_menu_with_submenus();

        state.open_menu(0);
        state.highlighted_item = Some(1); // Terminal submenu

        // On a submenu item, get_highlighted_action should return None
        assert!(state.get_highlighted_action(&menus).is_none());

        // Open the submenu
        state.open_submenu(&menus);
        // Now highlighted_item is 0 which is "Open Terminal"
        let action = state.get_highlighted_action(&menus);
        assert!(action.is_some());
        let (action_name, _) = action.unwrap();
        assert_eq!(action_name, "open_terminal");

        // Navigate to second item
        state.highlighted_item = Some(1);
        let action = state.get_highlighted_action(&menus);
        assert!(action.is_some());
        let (action_name, _) = action.unwrap();
        assert_eq!(action_name, "close_terminal");
    }

    #[test]
    fn test_get_current_items_at_different_depths() {
        let mut state = MenuState::for_testing();
        let menus = create_menu_with_submenus();

        state.open_menu(0);

        // At top level, should get main menu items
        let items = state.get_current_items(&menus, 0).unwrap();
        assert_eq!(items.len(), 4); // Action, Submenu, Separator, Action

        // Open Terminal submenu
        state.highlighted_item = Some(1);
        state.open_submenu(&menus);

        // Now should get Terminal submenu items
        let items = state.get_current_items(&menus, 0).unwrap();
        assert_eq!(items.len(), 3); // Open, Close, Settings submenu

        // Open nested Terminal Settings submenu
        state.highlighted_item = Some(2);
        state.open_submenu(&menus);

        // Now should get Terminal Settings submenu items
        let items = state.get_current_items(&menus, 0).unwrap();
        assert_eq!(items.len(), 1); // Font Size
    }

    #[test]
    fn test_is_highlighted_submenu() {
        let mut state = MenuState::for_testing();
        let menus = create_menu_with_submenus();

        state.open_menu(0);
        state.highlighted_item = Some(0); // Toggle Explorer (action)
        assert!(!state.is_highlighted_submenu(&menus));

        state.highlighted_item = Some(1); // Terminal (submenu)
        assert!(state.is_highlighted_submenu(&menus));

        state.highlighted_item = Some(2); // Separator
        assert!(!state.is_highlighted_submenu(&menus));

        state.highlighted_item = Some(3); // Zoom In (action)
        assert!(!state.is_highlighted_submenu(&menus));
    }

    #[test]
    fn test_open_menu_clears_submenu_path() {
        let mut state = MenuState::for_testing();
        let menus = create_menu_with_submenus();

        state.open_menu(0);
        state.highlighted_item = Some(1);
        state.open_submenu(&menus);
        assert!(!state.submenu_path.is_empty());

        // Opening a new menu should clear the submenu path
        state.open_menu(0);
        assert!(state.submenu_path.is_empty());
    }

    #[test]
    fn test_next_prev_menu_clears_submenu_path() {
        let mut state = MenuState::for_testing();
        let menus = create_menu_with_submenus();

        state.open_menu(0);
        state.highlighted_item = Some(1);
        state.open_submenu(&menus);
        assert!(!state.submenu_path.is_empty());

        // next_menu should clear submenu path
        state.next_menu(&menus);
        assert!(state.submenu_path.is_empty());

        // Re-open submenu
        state.open_menu(0);
        state.highlighted_item = Some(1);
        state.open_submenu(&menus);

        // prev_menu should clear submenu path
        state.prev_menu(&menus);
        assert!(state.submenu_path.is_empty());
    }

    #[test]
    fn test_navigation_in_submenu() {
        let mut state = MenuState::for_testing();
        let menus = create_menu_with_submenus();

        state.open_menu(0);
        state.highlighted_item = Some(1);
        state.open_submenu(&menus);

        // In Terminal submenu, start at index 0
        assert_eq!(state.highlighted_item, Some(0));

        // Navigate down
        state.next_item(&menus[0]);
        assert_eq!(state.highlighted_item, Some(1));

        // Navigate down again
        state.next_item(&menus[0]);
        assert_eq!(state.highlighted_item, Some(2));

        // Navigate down wraps to start
        state.next_item(&menus[0]);
        assert_eq!(state.highlighted_item, Some(0));

        // Navigate up wraps to end
        state.prev_item(&menus[0]);
        assert_eq!(state.highlighted_item, Some(2));
    }
}
