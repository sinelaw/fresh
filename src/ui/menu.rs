//! Menu bar rendering

use crate::config::{Menu, MenuItem, MenuConfig};
use crate::theme::Theme;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Paragraph};
use ratatui::Frame;

/// Menu bar state (tracks which menu is open and which item is highlighted)
#[derive(Debug, Clone, Default)]
pub struct MenuState {
    /// Index of the currently open menu (None if menu bar is closed)
    pub active_menu: Option<usize>,
    /// Index of the highlighted item within the active menu
    pub highlighted_item: Option<usize>,
    /// Runtime menu additions from plugins
    pub plugin_menus: Vec<Menu>,
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
    pub fn get_highlighted_action(&self, menus: &[Menu]) -> Option<(String, std::collections::HashMap<String, serde_json::Value>)> {
        let active_menu = self.active_menu?;
        let highlighted_item = self.highlighted_item?;

        let menu = menus.get(active_menu)?;
        let item = menu.items.get(highlighted_item)?;

        match item {
            MenuItem::Action { action, args, .. } => Some((action.clone(), args.clone())),
            _ => None,
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
    /// * `menu_state` - Current menu state (which menu/item is active)
    /// * `theme` - The active theme for colors
    pub fn render(
        frame: &mut Frame,
        area: Rect,
        menu_config: &MenuConfig,
        menu_state: &MenuState,
        theme: &Theme,
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

            let style = if is_active {
                Style::default()
                    .fg(theme.menu_active_fg)
                    .bg(theme.menu_active_bg)
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default()
                    .fg(theme.menu_fg)
                    .bg(theme.menu_bg)
            };

            spans.push(Span::styled(format!(" {} ", menu.label), style));
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
                    theme,
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
        theme: &Theme,
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

        // Position dropdown below the menu bar
        let dropdown_area = Rect {
            x: menu_bar_area.x + x_offset as u16,
            y: menu_bar_area.y + 1,
            width: max_width as u16,
            height: dropdown_height as u16,
        };

        // Build dropdown content
        let mut lines = Vec::new();
        for (idx, item) in menu.items.iter().enumerate() {
            let is_highlighted = highlighted_item == Some(idx);

            let line = match item {
                MenuItem::Action { label, .. } => {
                    let style = if is_highlighted {
                        Style::default()
                            .fg(theme.menu_highlight_fg)
                            .bg(theme.menu_highlight_bg)
                    } else {
                        Style::default()
                            .fg(theme.menu_dropdown_fg)
                            .bg(theme.menu_dropdown_bg)
                    };

                    // TODO: Add keybinding display here (Phase 3)
                    Line::from(vec![Span::styled(
                        format!(" {:<width$}", label, width = max_width - 2),
                        style,
                    )])
                }
                MenuItem::Separator { .. } => {
                    let separator = "─".repeat(max_width - 2);
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
                    } else {
                        Style::default()
                            .fg(theme.menu_dropdown_fg)
                            .bg(theme.menu_dropdown_bg)
                    };

                    Line::from(vec![Span::styled(
                        format!(" {:<width$} ▶", label, width = max_width - 4),
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
