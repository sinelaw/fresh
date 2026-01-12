//! Menu bar rendering

use crate::config::{Menu, MenuConfig, MenuItem};
use crate::primitives::display_width::str_width;
use crate::view::theme::Theme;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Paragraph};
use ratatui::Frame;

// Re-export context_keys from the shared types module
pub use crate::types::context_keys;

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
    /// Index of the highlighted item within the active menu or current submenu
    pub highlighted_item: Option<usize>,
    /// Path of indices into nested submenus (empty = at top level menu)
    /// Each element is the index of the submenu item that was opened
    pub submenu_path: Vec<usize>,
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
        self.submenu_path.clear();
    }

    /// Close the currently open menu (and all submenus)
    pub fn close_menu(&mut self) {
        self.active_menu = None;
        self.highlighted_item = None;
        self.submenu_path.clear();
    }

    /// Navigate to the next menu (right) - only at top level
    pub fn next_menu(&mut self, total_menus: usize) {
        if let Some(active) = self.active_menu {
            self.active_menu = Some((active + 1) % total_menus);
            self.highlighted_item = Some(0);
            self.submenu_path.clear();
        }
    }

    /// Navigate to the previous menu (left) - only at top level
    pub fn prev_menu(&mut self, total_menus: usize) {
        if let Some(active) = self.active_menu {
            self.active_menu = Some((active + total_menus - 1) % total_menus);
            self.highlighted_item = Some(0);
            self.submenu_path.clear();
        }
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
                    let generated = MenuItem::generate_dynamic_items(source);
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
        let mut items: Vec<MenuItem> = menu.items.iter().map(|i| i.expand_dynamic()).collect();

        for &idx in &self.submenu_path {
            match items.get(idx)?.expand_dynamic() {
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

    /// Get the menu index at a given x position in the menu bar
    /// Returns the menu index if the click is on a menu label
    pub fn get_menu_at_position(&self, menus: &[Menu], x: u16) -> Option<usize> {
        let mut current_x = 0u16;

        for (idx, menu) in menus.iter().enumerate() {
            let label_width = str_width(&menu.label) as u16 + 2; // " Label "
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
        // Combine config menus with plugin menus, expanding any DynamicSubmenus
        let all_menus: Vec<Menu> = menu_config
            .menus
            .iter()
            .chain(menu_state.plugin_menus.iter())
            .cloned()
            .map(|mut menu| {
                menu.expand_dynamic_items();
                menu
            })
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
                Self::render_dropdown_chain(
                    frame,
                    area,
                    menu,
                    menu_state,
                    active_idx,
                    &all_menus,
                    keybindings,
                    theme,
                    hover_target,
                );
            }
        }
    }

    /// Render a dropdown menu and all its open submenus
    #[allow(clippy::too_many_arguments)]
    fn render_dropdown_chain(
        frame: &mut Frame,
        menu_bar_area: Rect,
        menu: &Menu,
        menu_state: &MenuState,
        menu_index: usize,
        all_menus: &[Menu],
        keybindings: &crate::input::keybindings::KeybindingResolver,
        theme: &Theme,
        hover_target: Option<&crate::app::HoverTarget>,
    ) {
        // Calculate the x position of the top-level dropdown based on menu index
        let mut x_offset = 0usize;
        for (idx, m) in all_menus.iter().enumerate() {
            if idx == menu_index {
                break;
            }
            x_offset += str_width(&m.label) + 3; // label + spaces
        }

        let terminal_width = frame.area().width;
        let terminal_height = frame.area().height;

        // Track dropdown positions for rendering submenus
        let mut current_items: &[MenuItem] = &menu.items;
        let mut current_x = menu_bar_area.x.saturating_add(x_offset as u16);
        let mut current_y = menu_bar_area.y.saturating_add(1);

        // Render the main dropdown and collect submenu rendering info
        // We'll render depth 0, then 1, etc.
        for depth in 0..=menu_state.submenu_path.len() {
            let is_deepest = depth == menu_state.submenu_path.len();
            let highlighted_item = if is_deepest {
                menu_state.highlighted_item
            } else {
                Some(menu_state.submenu_path[depth])
            };

            // Render this dropdown level
            let dropdown_rect = Self::render_dropdown_level(
                frame,
                current_items,
                highlighted_item,
                current_x,
                current_y,
                terminal_width,
                terminal_height,
                depth,
                &menu_state.submenu_path,
                menu_index,
                keybindings,
                theme,
                hover_target,
                &menu_state.context,
            );

            // If not at the deepest level, navigate into the submenu for next iteration
            if !is_deepest {
                let submenu_idx = menu_state.submenu_path[depth];
                // Handle both Submenu and DynamicSubmenu
                let submenu_items = match current_items.get(submenu_idx) {
                    Some(MenuItem::Submenu { items, .. }) => Some(items.as_slice()),
                    Some(MenuItem::DynamicSubmenu { .. }) => {
                        // DynamicSubmenu items will be generated and stored temporarily
                        // This case shouldn't happen in normal flow since we expand before entering
                        None
                    }
                    _ => None,
                };
                if let Some(items) = submenu_items {
                    current_items = items;
                    // Position submenu to the right of parent, aligned with the highlighted item
                    current_x = dropdown_rect
                        .x
                        .saturating_add(dropdown_rect.width.saturating_sub(1));
                    current_y = dropdown_rect.y.saturating_add(submenu_idx as u16 + 1); // +1 for border

                    // Adjust if submenu would go off screen to the right - flip to left side
                    let next_width = Self::calculate_dropdown_width(items);
                    if current_x.saturating_add(next_width as u16) > terminal_width {
                        current_x = dropdown_rect
                            .x
                            .saturating_sub(next_width as u16)
                            .saturating_add(1);
                    }
                } else {
                    break;
                }
            }
        }
    }

    /// Calculate the width needed for a dropdown containing the given items
    fn calculate_dropdown_width(items: &[MenuItem]) -> usize {
        items
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
            .min(40)
    }

    /// Render a single dropdown level and return its bounding Rect
    #[allow(clippy::too_many_arguments)]
    fn render_dropdown_level(
        frame: &mut Frame,
        items: &[MenuItem],
        highlighted_item: Option<usize>,
        x: u16,
        y: u16,
        terminal_width: u16,
        terminal_height: u16,
        depth: usize,
        submenu_path: &[usize],
        menu_index: usize,
        keybindings: &crate::input::keybindings::KeybindingResolver,
        theme: &Theme,
        hover_target: Option<&crate::app::HoverTarget>,
        context: &MenuContext,
    ) -> Rect {
        let max_width = Self::calculate_dropdown_width(items);
        let dropdown_height = items.len() + 2; // +2 for borders

        let desired_width = max_width as u16;
        let desired_height = dropdown_height as u16;

        // Bounds check: ensure dropdown fits within the visible area
        let adjusted_x = if x.saturating_add(desired_width) > terminal_width {
            terminal_width.saturating_sub(desired_width)
        } else {
            x
        };

        let available_height = terminal_height.saturating_sub(y);
        let height = desired_height.min(available_height);

        let available_width = terminal_width.saturating_sub(adjusted_x);
        let width = desired_width.min(available_width);

        // Only render if we have at least minimal space
        if width < 10 || height < 3 {
            return Rect {
                x: adjusted_x,
                y,
                width,
                height,
            };
        }

        let dropdown_area = Rect {
            x: adjusted_x,
            y,
            width,
            height,
        };

        // Build dropdown content
        let mut lines = Vec::new();
        let max_items = (height.saturating_sub(2)) as usize;
        let items_to_show = items.len().min(max_items);
        let content_width = (width as usize).saturating_sub(2);

        for (idx, item) in items.iter().enumerate().take(items_to_show) {
            let is_highlighted = highlighted_item == Some(idx);
            // Check if this item is in the submenu path (has an open child submenu)
            let has_open_submenu = depth < submenu_path.len() && submenu_path[depth] == idx;

            // For hover target matching at submenu levels
            let is_hovered = if depth == 0 {
                matches!(
                    hover_target,
                    Some(crate::app::HoverTarget::MenuDropdownItem(mi, ii)) if *mi == menu_index && *ii == idx
                )
            } else {
                matches!(
                    hover_target,
                    Some(crate::app::HoverTarget::SubmenuItem(d, ii)) if *d == depth && *ii == idx
                )
            };
            let enabled = is_menu_item_enabled(item, context);

            let line = match item {
                MenuItem::Action {
                    label,
                    action,
                    checkbox,
                    ..
                } => {
                    let style = if !enabled {
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

                    let keybinding = keybindings
                        .find_keybinding_for_action(
                            action,
                            crate::input::keybindings::KeyContext::Normal,
                        )
                        .unwrap_or_default();

                    let checkbox_icon = if checkbox.is_some() {
                        if is_checkbox_checked(checkbox, context) {
                            "☑ "
                        } else {
                            "☐ "
                        }
                    } else {
                        ""
                    };

                    let checkbox_width = if checkbox.is_some() { 2 } else { 0 };
                    let label_display_width = str_width(label);
                    let keybinding_display_width = str_width(&keybinding);

                    let text = if keybinding.is_empty() {
                        let padding_needed =
                            content_width.saturating_sub(checkbox_width + label_display_width + 1);
                        format!(" {}{}{}", checkbox_icon, label, " ".repeat(padding_needed))
                    } else {
                        let padding_needed = content_width.saturating_sub(
                            checkbox_width + label_display_width + keybinding_display_width + 2,
                        );
                        format!(
                            " {}{}{} {}",
                            checkbox_icon,
                            label,
                            " ".repeat(padding_needed),
                            keybinding
                        )
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
                MenuItem::Submenu { label, .. } | MenuItem::DynamicSubmenu { label, .. } => {
                    // Highlight submenu items that have an open child
                    let style = if is_highlighted || has_open_submenu {
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

                    // Format: " Label        > " - label left-aligned, arrow near the end with padding
                    // content_width minus: leading space (1) + space before arrow (1) + arrow (1) + trailing space (2)
                    let label_display_width = str_width(label);
                    let padding_needed = content_width.saturating_sub(label_display_width + 5);
                    Line::from(vec![Span::styled(
                        format!(" {}{} >  ", label, " ".repeat(padding_needed)),
                        style,
                    )])
                }
                MenuItem::Label { info } => {
                    // Disabled info label - always shown in disabled style
                    let style = Style::default()
                        .fg(theme.menu_disabled_fg)
                        .bg(theme.menu_dropdown_bg);
                    let info_display_width = str_width(info);
                    let padding_needed = content_width.saturating_sub(info_display_width);
                    Line::from(vec![Span::styled(
                        format!(" {}{}", info, " ".repeat(padding_needed)),
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

        dropdown_area
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
            id: None,
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
        }]
    }

    #[test]
    fn test_submenu_open_and_close() {
        let mut state = MenuState::new();
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
        let mut state = MenuState::new();
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
        let mut state = MenuState::new();
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
        let mut state = MenuState::new();
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
        let mut state = MenuState::new();
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
        let mut state = MenuState::new();
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
        let mut state = MenuState::new();
        let menus = create_menu_with_submenus();

        state.open_menu(0);
        state.highlighted_item = Some(1);
        state.open_submenu(&menus);
        assert!(!state.submenu_path.is_empty());

        // next_menu should clear submenu path
        state.next_menu(1);
        assert!(state.submenu_path.is_empty());

        // Re-open submenu
        state.open_menu(0);
        state.highlighted_item = Some(1);
        state.open_submenu(&menus);

        // prev_menu should clear submenu path
        state.prev_menu(1);
        assert!(state.submenu_path.is_empty());
    }

    #[test]
    fn test_navigation_in_submenu() {
        let mut state = MenuState::new();
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
