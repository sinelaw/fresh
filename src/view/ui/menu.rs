//! Menu bar rendering

use crate::config::{Menu, MenuConfig, MenuItem};
use crate::view::theme::Theme;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Paragraph};
use ratatui::Frame;

/// Constants for menu context state keys
/// These are used both in menu item `when` conditions and `checkbox` states
pub mod context_keys {
    pub const LINE_NUMBERS: &str = "line_numbers";
    pub const LINE_WRAP: &str = "line_wrap";
    pub const COMPOSE_MODE: &str = "compose_mode";
    pub const FILE_EXPLORER: &str = "file_explorer";
    pub const FILE_EXPLORER_FOCUSED: &str = "file_explorer_focused";
    pub const MOUSE_CAPTURE: &str = "mouse_capture";
    pub const FILE_EXPLORER_SHOW_HIDDEN: &str = "file_explorer_show_hidden";
    pub const FILE_EXPLORER_SHOW_GITIGNORED: &str = "file_explorer_show_gitignored";
    pub const HAS_SELECTION: &str = "has_selection";
}

/// Menu state context - provides named boolean states for menu item conditions
/// Both `when` conditions and `checkbox` states look up values here
#[derive(Debug, Clone, Default)]
pub struct MenuContext {
    states: std::collections::HashMap<String, bool>,
}

impl MenuContext {
    pub fn new() -> Self {
        Self {
            states: std::collections::HashMap::new(),
        }
    }

    /// Set a named boolean state
    pub fn set(&mut self, name: impl Into<String>, value: bool) -> &mut Self {
        self.states.insert(name.into(), value);
        self
    }

    /// Get a named boolean state (defaults to false if not set)
    pub fn get(&self, name: &str) -> bool {
        self.states.get(name).copied().unwrap_or(false)
    }

    /// Builder-style setter
    pub fn with(mut self, name: impl Into<String>, value: bool) -> Self {
        self.set(name, value);
        self
    }
}

fn is_menu_item_enabled(item: &MenuItem, context: &MenuContext) -> bool {
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

fn is_checkbox_checked(checkbox: &Option<String>, context: &MenuContext) -> bool {
    match checkbox.as_deref() {
        Some(name) => context.get(name),
        None => false,
    }
}

/// Menu bar state (tracks which menu is open and which item is highlighted)
#[derive(Debug, Clone, Default)]
pub struct MenuState {
    /// Index of the currently open menu (None if menu bar is closed)
    pub active_menu: Option<usize>,
    /// Index of the highlighted item within the active menu
    pub highlighted_item: Option<usize>,
    /// Runtime menu additions from plugins
    pub plugin_menus: Vec<Menu>,
    /// Context containing named boolean states for conditions and checkboxes
    pub context: MenuContext,
}

impl MenuState {
    pub fn new() -> Self {
        Self::default()
    }

    /// Open a menu by index
    pub fn open_menu(&mut self, index: usize) {
        self.active_menu = Some(index);
        self.highlighted_item = Some(0);
    }

    /// Close the currently open menu
    pub fn close_menu(&mut self) {
        self.active_menu = None;
        self.highlighted_item = None;
    }

    /// Navigate to the next menu (right)
    pub fn next_menu(&mut self, total_menus: usize) {
        if let Some(active) = self.active_menu {
            self.active_menu = Some((active + 1) % total_menus);
            self.highlighted_item = Some(0);
        }
    }

    /// Navigate to the previous menu (left)
    pub fn prev_menu(&mut self, total_menus: usize) {
        if let Some(active) = self.active_menu {
            self.active_menu = Some((active + total_menus - 1) % total_menus);
            self.highlighted_item = Some(0);
        }
    }

    /// Navigate to the next item in the current menu (down)
    pub fn next_item(&mut self, menu: &Menu) {
        if let Some(idx) = self.highlighted_item {
            // Skip separators
            let mut next = (idx + 1) % menu.items.len();
            while matches!(menu.items[next], MenuItem::Separator { .. }) && next != idx {
                next = (next + 1) % menu.items.len();
            }
            self.highlighted_item = Some(next);
        }
    }

    /// Navigate to the previous item in the current menu (up)
    pub fn prev_item(&mut self, menu: &Menu) {
        if let Some(idx) = self.highlighted_item {
            // Skip separators
            let total = menu.items.len();
            let mut prev = (idx + total - 1) % total;
            while matches!(menu.items[prev], MenuItem::Separator { .. }) && prev != idx {
                prev = (prev + total - 1) % total;
            }
            self.highlighted_item = Some(prev);
        }
    }

    /// Get the currently highlighted action (if any)
    pub fn get_highlighted_action(
        &self,
        menus: &[Menu],
    ) -> Option<(String, std::collections::HashMap<String, serde_json::Value>)> {
        let active_menu = self.active_menu?;
        let highlighted_item = self.highlighted_item?;

        let menu = menus.get(active_menu)?;
        let item = menu.items.get(highlighted_item)?;

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

    /// Get the menu index at a given x position in the menu bar
    /// Returns the menu index if the click is on a menu label
    pub fn get_menu_at_position(&self, menus: &[Menu], x: u16) -> Option<usize> {
        let mut current_x = 0u16;

        for (idx, menu) in menus.iter().enumerate() {
            let label_width = menu.label.len() as u16 + 2; // " Label "
            let total_width = label_width + 1; // Plus trailing space

            if x >= current_x && x < current_x + label_width {
                return Some(idx);
            }

            current_x += total_width;
        }

        None
    }

    /// Get the item index at a given y position in the dropdown
    /// y is relative to the menu bar (so y=1 is the first item in dropdown)
    pub fn get_item_at_position(&self, menu: &Menu, y: u16) -> Option<usize> {
        // y=0 is menu bar, y=1 is top border, y=2 is first item
        if y < 2 {
            return None;
        }

        let item_index = (y - 2) as usize;
        if item_index < menu.items.len() {
            // Don't return separator indices
            if matches!(menu.items[item_index], MenuItem::Separator { .. }) {
                None
            } else {
                Some(item_index)
            }
        } else {
            None
        }
    }
}

/// Renders the menu bar
pub struct MenuRenderer;

impl MenuRenderer {
    /// Render the menu bar at the top of the screen
    ///
    /// # Arguments
    /// * `frame` - The ratatui frame to render to
    /// * `area` - The rectangular area to render the menu bar in
    /// * `menu_config` - The menu configuration
    /// * `menu_state` - Current menu state (which menu/item is active, and context)
    /// * `keybindings` - Keybinding resolver for displaying shortcuts
    /// * `theme` - The active theme for colors
    /// * `hover_target` - The currently hovered UI element (if any)
    pub fn render(
        frame: &mut Frame,
        area: Rect,
        menu_config: &MenuConfig,
        menu_state: &MenuState,
        keybindings: &crate::input::keybindings::KeybindingResolver,
        theme: &Theme,
        hover_target: Option<&crate::app::HoverTarget>,
    ) {
        // Combine config menus with plugin menus
        let all_menus: Vec<&Menu> = menu_config
            .menus
            .iter()
            .chain(menu_state.plugin_menus.iter())
            .collect();

        // Build spans for each menu label
        let mut spans = Vec::new();

        for (idx, menu) in all_menus.iter().enumerate() {
            let is_active = menu_state.active_menu == Some(idx);
            let is_hovered =
                matches!(hover_target, Some(crate::app::HoverTarget::MenuBarItem(i)) if *i == idx);

            let base_style = if is_active {
                Style::default()
                    .fg(theme.menu_active_fg)
                    .bg(theme.menu_active_bg)
                    .add_modifier(Modifier::BOLD)
            } else if is_hovered {
                Style::default()
                    .fg(theme.menu_hover_fg)
                    .bg(theme.menu_hover_bg)
            } else {
                Style::default().fg(theme.menu_fg).bg(theme.menu_bg)
            };

            // Check for mnemonic character (Alt+letter keybinding)
            let mnemonic = keybindings.find_menu_mnemonic(&menu.label);

            // Build the label with underlined mnemonic
            spans.push(Span::styled(" ", base_style));

            if let Some(mnemonic_char) = mnemonic {
                // Find the first occurrence of the mnemonic character in the label
                let mut found = false;
                for c in menu.label.chars() {
                    if !found && c.to_ascii_lowercase() == mnemonic_char {
                        // Underline this character
                        spans.push(Span::styled(
                            c.to_string(),
                            base_style.add_modifier(Modifier::UNDERLINED),
                        ));
                        found = true;
                    } else {
                        spans.push(Span::styled(c.to_string(), base_style));
                    }
                }
            } else {
                // No mnemonic, just render the label normally
                spans.push(Span::styled(menu.label.clone(), base_style));
            }

            spans.push(Span::styled(" ", base_style));
            spans.push(Span::raw(" "));
        }

        let line = Line::from(spans);
        let paragraph = Paragraph::new(line).style(Style::default().bg(theme.menu_bg));
        frame.render_widget(paragraph, area);

        // Render dropdown if a menu is active
        if let Some(active_idx) = menu_state.active_menu {
            if let Some(menu) = all_menus.get(active_idx) {
                Self::render_dropdown(
                    frame,
                    area,
                    menu,
                    menu_state.highlighted_item,
                    active_idx,
                    &all_menus,
                    keybindings,
                    theme,
                    hover_target,
                    &menu_state.context,
                );
            }
        }
    }

    /// Render a dropdown menu below the active menu label
    fn render_dropdown(
        frame: &mut Frame,
        menu_bar_area: Rect,
        menu: &Menu,
        highlighted_item: Option<usize>,
        menu_index: usize,
        all_menus: &[&Menu],
        keybindings: &crate::input::keybindings::KeybindingResolver,
        theme: &Theme,
        hover_target: Option<&crate::app::HoverTarget>,
        context: &MenuContext,
    ) {
        // Calculate the x position of the dropdown based on menu index
        let mut x_offset = 0;
        for (idx, m) in all_menus.iter().enumerate() {
            if idx == menu_index {
                break;
            }
            x_offset += m.label.len() + 3; // label + spaces
        }

        // Calculate dropdown width (longest item + padding)
        let max_width = menu
            .items
            .iter()
            .filter_map(|item| match item {
                MenuItem::Action { label, .. } => Some(label.len() + 20), // Extra space for keybindings
                MenuItem::Submenu { label, .. } => Some(label.len() + 20),
                MenuItem::Separator { .. } => Some(20),
            })
            .max()
            .unwrap_or(20)
            .min(40); // Cap at 40 chars

        let dropdown_height = menu.items.len() + 2; // +2 for borders

        // Calculate the desired position
        let desired_x = menu_bar_area.x.saturating_add(x_offset as u16);
        let desired_y = menu_bar_area.y.saturating_add(1);
        let desired_width = max_width as u16;
        let desired_height = dropdown_height as u16;

        // Get the terminal size from the frame area (assuming frame covers full terminal)
        // We need to ensure dropdown fits within the visible area
        let terminal_width = frame.area().width;
        let terminal_height = frame.area().height;

        // Bounds check: ensure dropdown doesn't overflow terminal
        // If dropdown would go off the right edge, move it left
        let x = if desired_x.saturating_add(desired_width) > terminal_width {
            terminal_width.saturating_sub(desired_width)
        } else {
            desired_x
        };

        // If dropdown would go off the bottom, cap the height
        let available_height = terminal_height.saturating_sub(desired_y);
        let height = desired_height.min(available_height);

        // Cap width to available space
        let available_width = terminal_width.saturating_sub(x);
        let width = desired_width.min(available_width);

        // Only render if we have at least minimal space
        if width < 10 || height < 3 {
            // Terminal is too small to render dropdown meaningfully
            return;
        }

        // Position dropdown below the menu bar
        let dropdown_area = Rect {
            x,
            y: desired_y,
            width,
            height,
        };

        // Build dropdown content
        let mut lines = Vec::new();

        // Calculate how many items we can show (accounting for borders)
        let max_items = (height.saturating_sub(2)) as usize;
        let items_to_show = menu.items.len().min(max_items);

        // Use the actual width for formatting (accounting for borders)
        let content_width = (width as usize).saturating_sub(2);

        for (idx, item) in menu.items.iter().enumerate().take(items_to_show) {
            let is_highlighted = highlighted_item == Some(idx);
            let is_hovered = matches!(
                hover_target,
                Some(crate::app::HoverTarget::MenuDropdownItem(mi, ii)) if *mi == menu_index && *ii == idx
            );
            let enabled = is_menu_item_enabled(item, context);

            let line = match item {
                MenuItem::Action {
                    label,
                    action,
                    checkbox,
                    ..
                } => {
                    let style = if !enabled {
                        // Disabled items use subdued theme colors
                        Style::default()
                            .fg(theme.menu_disabled_fg)
                            .bg(theme.menu_disabled_bg)
                    } else if is_highlighted {
                        Style::default()
                            .fg(theme.menu_highlight_fg)
                            .bg(theme.menu_highlight_bg)
                    } else if is_hovered {
                        Style::default()
                            .fg(theme.menu_hover_fg)
                            .bg(theme.menu_hover_bg)
                    } else {
                        Style::default()
                            .fg(theme.menu_dropdown_fg)
                            .bg(theme.menu_dropdown_bg)
                    };

                    // Find keybinding for this action
                    let keybinding = keybindings
                        .find_keybinding_for_action(
                            action,
                            crate::input::keybindings::KeyContext::Normal,
                        )
                        .unwrap_or_default();

                    // Determine checkbox icon if checkbox is present
                    let checkbox_icon = if checkbox.is_some() {
                        if is_checkbox_checked(checkbox, context) {
                            "☑ "
                        } else {
                            "☐ "
                        }
                    } else {
                        ""
                    };

                    // Calculate spacing for alignment using actual content width
                    let checkbox_width = if checkbox.is_some() { 2 } else { 0 };
                    let label_width =
                        content_width.saturating_sub(keybinding.len() + checkbox_width + 2);
                    let text = if keybinding.is_empty() {
                        let padding = content_width.saturating_sub(checkbox_width);
                        format!(" {}{:<width$}", checkbox_icon, label, width = padding)
                    } else {
                        format!(" {}{:<label_width$} {}", checkbox_icon, label, keybinding)
                    };

                    Line::from(vec![Span::styled(text, style)])
                }
                MenuItem::Separator { .. } => {
                    let separator = "─".repeat(content_width);
                    Line::from(vec![Span::styled(
                        format!(" {separator}"),
                        Style::default()
                            .fg(theme.menu_separator_fg)
                            .bg(theme.menu_dropdown_bg),
                    )])
                }
                MenuItem::Submenu { label, .. } => {
                    let style = if is_highlighted {
                        Style::default()
                            .fg(theme.menu_highlight_fg)
                            .bg(theme.menu_highlight_bg)
                    } else if is_hovered {
                        Style::default()
                            .fg(theme.menu_hover_fg)
                            .bg(theme.menu_hover_bg)
                    } else {
                        Style::default()
                            .fg(theme.menu_dropdown_fg)
                            .bg(theme.menu_dropdown_bg)
                    };

                    let arrow_padding = content_width.saturating_sub(2);
                    Line::from(vec![Span::styled(
                        format!(" {:<width$} ▶", label, width = arrow_padding),
                        style,
                    )])
                }
            };

            lines.push(line);
        }

        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(theme.menu_border_fg))
            .style(Style::default().bg(theme.menu_dropdown_bg));

        let paragraph = Paragraph::new(lines).block(block);
        frame.render_widget(paragraph, dropdown_area);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    fn create_test_menus() -> Vec<Menu> {
        vec![
            Menu {
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
            },
            Menu {
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
            },
            Menu {
                label: "View".to_string(),
                items: vec![MenuItem::Action {
                    label: "Toggle Explorer".to_string(),
                    action: "toggle_file_explorer".to_string(),
                    args: HashMap::new(),
                    when: None,
                    checkbox: None,
                }],
            },
        ]
    }

    #[test]
    fn test_menu_state_default() {
        let state = MenuState::new();
        assert_eq!(state.active_menu, None);
        assert_eq!(state.highlighted_item, None);
        assert!(state.plugin_menus.is_empty());
    }

    #[test]
    fn test_menu_state_open_menu() {
        let mut state = MenuState::new();
        state.open_menu(2);
        assert_eq!(state.active_menu, Some(2));
        assert_eq!(state.highlighted_item, Some(0));
    }

    #[test]
    fn test_menu_state_close_menu() {
        let mut state = MenuState::new();
        state.open_menu(1);
        state.close_menu();
        assert_eq!(state.active_menu, None);
        assert_eq!(state.highlighted_item, None);
    }

    #[test]
    fn test_menu_state_next_menu() {
        let mut state = MenuState::new();
        state.open_menu(0);

        state.next_menu(3);
        assert_eq!(state.active_menu, Some(1));

        state.next_menu(3);
        assert_eq!(state.active_menu, Some(2));

        // Wrap around
        state.next_menu(3);
        assert_eq!(state.active_menu, Some(0));
    }

    #[test]
    fn test_menu_state_prev_menu() {
        let mut state = MenuState::new();
        state.open_menu(0);

        // Wrap around backwards
        state.prev_menu(3);
        assert_eq!(state.active_menu, Some(2));

        state.prev_menu(3);
        assert_eq!(state.active_menu, Some(1));

        state.prev_menu(3);
        assert_eq!(state.active_menu, Some(0));
    }

    #[test]
    fn test_menu_state_next_item_skips_separator() {
        let mut state = MenuState::new();
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
        let mut state = MenuState::new();
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
        let mut state = MenuState::new();
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
        let mut state = MenuState::new();
        let select_menu = Menu {
            label: "Edit".to_string(),
            items: vec![MenuItem::Action {
                label: "Find in Selection".to_string(),
                action: "find_in_selection".to_string(),
                args: HashMap::new(),
                when: Some(context_keys::HAS_SELECTION.to_string()),
                checkbox: None,
            }],
        };
        state.open_menu(0);
        state.highlighted_item = Some(0);

        // Without has_selection set, action should be disabled
        assert!(state
            .get_highlighted_action(&[select_menu.clone()])
            .is_none());

        // With has_selection set to true, action should be enabled
        state.context.set(context_keys::HAS_SELECTION, true);
        assert!(state.get_highlighted_action(&[select_menu]).is_some());
    }

    #[test]
    fn test_get_highlighted_action_none_when_closed() {
        let state = MenuState::new();
        let menus = create_test_menus();
        assert!(state.get_highlighted_action(&menus).is_none());
    }

    #[test]
    fn test_get_highlighted_action_none_for_separator() {
        let mut state = MenuState::new();
        let menus = create_test_menus();
        state.open_menu(0);
        state.highlighted_item = Some(1); // Separator

        assert!(state.get_highlighted_action(&menus).is_none());
    }

    #[test]
    fn test_get_menu_at_position() {
        let state = MenuState::new();
        let menus = create_test_menus();

        // Menu positions: " File " (6 chars) + " " = 7, " Edit " (6 chars) + " " = 7, " View " (6 chars)
        // File: x=0-5
        assert_eq!(state.get_menu_at_position(&menus, 0), Some(0));
        assert_eq!(state.get_menu_at_position(&menus, 3), Some(0));
        assert_eq!(state.get_menu_at_position(&menus, 5), Some(0));

        // Space between: x=6
        assert_eq!(state.get_menu_at_position(&menus, 6), None);

        // Edit: x=7-12
        assert_eq!(state.get_menu_at_position(&menus, 7), Some(1));
        assert_eq!(state.get_menu_at_position(&menus, 10), Some(1));
        assert_eq!(state.get_menu_at_position(&menus, 12), Some(1));

        // Space between: x=13
        assert_eq!(state.get_menu_at_position(&menus, 13), None);

        // View: x=14-19
        assert_eq!(state.get_menu_at_position(&menus, 14), Some(2));
        assert_eq!(state.get_menu_at_position(&menus, 17), Some(2));
        assert_eq!(state.get_menu_at_position(&menus, 19), Some(2));

        // After View
        assert_eq!(state.get_menu_at_position(&menus, 20), None);
        assert_eq!(state.get_menu_at_position(&menus, 100), None);
    }

    #[test]
    fn test_get_item_at_position() {
        let state = MenuState::new();
        let menus = create_test_menus();

        // File menu has: New (0), Separator (1), Save (2), Quit (3)
        // y=0: menu bar
        // y=1: top border
        // y=2: first item (New)
        // y=3: second item (Separator)
        // y=4: third item (Save)
        // y=5: fourth item (Quit)
        // y=6: bottom border

        // y < 2 returns None
        assert_eq!(state.get_item_at_position(&menus[0], 0), None);
        assert_eq!(state.get_item_at_position(&menus[0], 1), None);

        // y=2: New (index 0)
        assert_eq!(state.get_item_at_position(&menus[0], 2), Some(0));

        // y=3: Separator returns None
        assert_eq!(state.get_item_at_position(&menus[0], 3), None);

        // y=4: Save (index 2)
        assert_eq!(state.get_item_at_position(&menus[0], 4), Some(2));

        // y=5: Quit (index 3)
        assert_eq!(state.get_item_at_position(&menus[0], 5), Some(3));

        // Beyond items
        assert_eq!(state.get_item_at_position(&menus[0], 6), None);
        assert_eq!(state.get_item_at_position(&menus[0], 100), None);
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
}
