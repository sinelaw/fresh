//! Settings UI renderer
//!
//! Renders the settings modal with category navigation and setting controls.

use super::items::SettingControl;
use super::layout::{SettingsHit, SettingsLayout};
use super::search::SearchResult;
use super::state::SettingsState;
use crate::view::controls::{
    render_dropdown_aligned, render_number_input_aligned, render_text_input_aligned,
    render_toggle_aligned, DropdownColors, MapColors, NumberInputColors, TextInputColors,
    TextListColors, ToggleColors,
};
use crate::view::theme::Theme;
use ratatui::layout::{Constraint, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Paragraph};
use ratatui::Frame;

/// Render the settings modal
pub fn render_settings(
    frame: &mut Frame,
    area: Rect,
    state: &mut SettingsState,
    theme: &Theme,
) -> SettingsLayout {
    // Calculate modal size (80% of screen, max 100 wide, 40 tall)
    let modal_width = (area.width * 80 / 100).min(100);
    let modal_height = (area.height * 80 / 100).min(40);
    let modal_x = (area.width.saturating_sub(modal_width)) / 2;
    let modal_y = (area.height.saturating_sub(modal_height)) / 2;

    let modal_area = Rect::new(modal_x, modal_y, modal_width, modal_height);

    // Clear the modal area and draw border
    frame.render_widget(Clear, modal_area);

    let title = if state.has_changes() {
        " Settings • (modified) "
    } else {
        " Settings "
    };

    let block = Block::default()
        .title(title)
        .borders(Borders::ALL)
        .border_style(Style::default().fg(theme.popup_border_fg))
        .style(Style::default().bg(theme.popup_bg));
    frame.render_widget(block, modal_area);

    // Inner area after border
    let inner_area = Rect::new(
        modal_area.x + 1,
        modal_area.y + 1,
        modal_area.width.saturating_sub(2),
        modal_area.height.saturating_sub(2),
    );

    // Render search header if search is active
    let (search_header_height, content_area) = if state.search_active {
        let search_area = Rect::new(inner_area.x, inner_area.y, inner_area.width, 2);
        render_search_header(frame, search_area, state, theme);
        (
            2,
            Rect::new(
                inner_area.x,
                inner_area.y + 2,
                inner_area.width,
                inner_area.height.saturating_sub(2),
            ),
        )
    } else {
        (0, inner_area)
    };
    let _ = search_header_height; // suppress unused warning

    // Layout: [left panel (categories)] | [right panel (settings)]
    let chunks =
        Layout::horizontal([Constraint::Length(25), Constraint::Min(40)]).split(content_area);

    let categories_area = chunks[0];
    let settings_area = chunks[1];

    // Create layout tracker
    let mut layout = SettingsLayout::new(modal_area);

    // Render category list (left panel)
    render_categories(frame, categories_area, state, theme, &mut layout);

    // Render separator
    let separator_area = Rect::new(
        categories_area.x + categories_area.width,
        categories_area.y,
        1,
        categories_area.height,
    );
    render_separator(frame, separator_area, theme);

    // Render settings (right panel) or search results
    // Add horizontal padding from separator
    let horizontal_padding = 2;
    let settings_inner = Rect::new(
        settings_area.x + horizontal_padding,
        settings_area.y,
        settings_area.width.saturating_sub(horizontal_padding),
        settings_area.height,
    );

    if state.search_active && !state.search_results.is_empty() {
        render_search_results(frame, settings_inner, state, theme, &mut layout);
    } else {
        render_settings_panel(frame, settings_inner, state, theme, &mut layout);
    }

    // Render footer with buttons
    render_footer(frame, modal_area, state, theme, &mut layout);

    // Determine the topmost dialog layer and apply dimming to layers below
    let has_confirm = state.showing_confirm_dialog;
    let has_entry = state.showing_entry_dialog();
    let has_help = state.showing_help;

    // Render confirmation dialog if showing
    if has_confirm {
        // Dim the main settings modal if confirm is showing
        // (but only if confirm is the topmost, otherwise entry/help dialog will dim it)
        if !has_entry && !has_help {
            crate::view::dimming::apply_dimming(frame, modal_area);
        }
        render_confirm_dialog(frame, modal_area, state, theme);
    }

    // Render entry detail dialog if showing
    if has_entry {
        // Dim everything below (including confirm dialog if visible)
        if !has_help {
            crate::view::dimming::apply_dimming(frame, modal_area);
        }
        render_entry_dialog(frame, modal_area, state, theme);
    }

    // Render help overlay if showing
    if has_help {
        // Help is topmost, dim everything below
        crate::view::dimming::apply_dimming(frame, modal_area);
        render_help_overlay(frame, modal_area, theme);
    }

    layout
}

/// Render the category list
fn render_categories(
    frame: &mut Frame,
    area: Rect,
    state: &SettingsState,
    theme: &Theme,
    layout: &mut SettingsLayout,
) {
    use super::layout::SettingsHit;
    use super::state::FocusPanel;

    for (idx, page) in state.pages.iter().enumerate() {
        if idx as u16 >= area.height {
            break;
        }

        let is_selected = idx == state.selected_category;
        let is_hovered = matches!(state.hover_hit, Some(SettingsHit::Category(i)) if i == idx);
        let row_area = Rect::new(area.x, area.y + idx as u16, area.width, 1);

        layout.add_category(idx, row_area);

        let style = if is_selected {
            if state.focus_panel == FocusPanel::Categories {
                Style::default()
                    .fg(theme.menu_highlight_fg)
                    .bg(theme.menu_highlight_bg)
            } else {
                Style::default().fg(theme.menu_fg).bg(theme.selection_bg)
            }
        } else if is_hovered {
            // Hover highlight using menu hover colors
            Style::default()
                .fg(theme.menu_hover_fg)
                .bg(theme.menu_hover_bg)
        } else {
            Style::default().fg(theme.popup_text_fg)
        };

        // Indicator for categories with modified settings
        let has_changes = page.items.iter().any(|i| i.modified);
        let prefix = if has_changes { "● " } else { "  " };

        let text = format!("{}{}", prefix, page.name);
        let line = Line::from(Span::styled(text, style));
        frame.render_widget(Paragraph::new(line), row_area);
    }
}

/// Render vertical separator
fn render_separator(frame: &mut Frame, area: Rect, theme: &Theme) {
    for y in 0..area.height {
        let cell = Rect::new(area.x, area.y + y, 1, 1);
        let sep = Paragraph::new("│").style(Style::default().fg(theme.split_separator_fg));
        frame.render_widget(sep, cell);
    }
}

/// Context for rendering a setting item (extracted to avoid borrow issues)
struct RenderContext {
    selected_item: usize,
    settings_focused: bool,
    hover_hit: Option<SettingsHit>,
    editing_text: bool,
}

/// Render the settings panel for the current category
fn render_settings_panel(
    frame: &mut Frame,
    area: Rect,
    state: &mut SettingsState,
    theme: &Theme,
    layout: &mut SettingsLayout,
) {
    let page = match state.current_page() {
        Some(p) => p,
        None => return,
    };

    // Render page title and description
    let mut y = area.y;
    let header_start_y = y;

    // Page title
    let title_style = Style::default()
        .fg(theme.menu_active_fg)
        .add_modifier(Modifier::BOLD);
    let title = Line::from(Span::styled(&page.name, title_style));
    frame.render_widget(Paragraph::new(title), Rect::new(area.x, y, area.width, 1));
    y += 1;

    // Page description
    if let Some(ref desc) = page.description {
        let desc_style = Style::default().fg(theme.line_number_fg);
        let desc_line = Line::from(Span::styled(desc, desc_style));
        frame.render_widget(
            Paragraph::new(desc_line),
            Rect::new(area.x, y, area.width, 1),
        );
        y += 1;
    }

    y += 1; // Blank line

    let header_height = (y - header_start_y) as usize;
    let items_start_y = y;

    // Calculate available height for items
    let available_height = area.height.saturating_sub(header_height as u16 + 1);

    // Update scroll panel with current viewport and content
    let page = state.pages.get(state.selected_category).unwrap();
    state.scroll_panel.set_viewport(available_height);
    state.scroll_panel.update_content_height(&page.items);

    // Extract state needed for rendering (to avoid borrow issues with scroll_panel)
    use super::state::FocusPanel;
    let render_ctx = RenderContext {
        selected_item: state.selected_item,
        settings_focused: state.focus_panel == FocusPanel::Settings,
        hover_hit: state.hover_hit.clone(),
        editing_text: state.editing_text,
    };

    // Area for items (below header)
    let items_area = Rect::new(area.x, items_start_y, area.width, available_height.max(1));

    // Get items reference for rendering
    let page = state.pages.get(state.selected_category).unwrap();

    // Calculate max label width for column alignment (only for single-row controls)
    let max_label_width = page
        .items
        .iter()
        .filter_map(|item| {
            // Only consider single-row controls for alignment
            match &item.control {
                SettingControl::Toggle(s) => Some(s.label.len() as u16),
                SettingControl::Number(s) => Some(s.label.len() as u16),
                SettingControl::Dropdown(s) => Some(s.label.len() as u16),
                SettingControl::Text(s) => Some(s.label.len() as u16),
                // Multi-row controls have their labels on separate lines
                _ => None,
            }
        })
        .max();

    // Use ScrollablePanel to render items with automatic scroll handling
    let panel_layout = state.scroll_panel.render(
        frame,
        items_area,
        &page.items,
        |frame, info, item| {
            render_setting_item_pure(
                frame,
                info.area,
                item,
                info.index,
                info.skip_top,
                &render_ctx,
                theme,
                max_label_width,
            )
        },
        theme,
    );

    // Transfer item layouts to SettingsLayout
    let page = state.pages.get(state.selected_category).unwrap();
    for item_info in panel_layout.item_layouts {
        layout.add_item(
            item_info.index,
            page.items[item_info.index].path.clone(),
            item_info.area,
            item_info.layout,
        );
    }

    // Track the settings panel area for scroll hit testing
    layout.settings_panel_area = Some(panel_layout.content_area);

    // Track scrollbar area for drag detection
    if let Some(sb_area) = panel_layout.scrollbar_area {
        layout.scrollbar_area = Some(sb_area);
    }
}

/// Wrap text to fit within a given width
fn wrap_text(text: &str, width: usize) -> Vec<String> {
    if width == 0 || text.is_empty() {
        return vec![text.to_string()];
    }

    let mut lines = Vec::new();
    let mut current_line = String::new();
    let mut current_len = 0;

    for word in text.split_whitespace() {
        let word_len = word.chars().count();

        if current_len == 0 {
            // First word on line
            current_line = word.to_string();
            current_len = word_len;
        } else if current_len + 1 + word_len <= width {
            // Word fits on current line
            current_line.push(' ');
            current_line.push_str(word);
            current_len += 1 + word_len;
        } else {
            // Start new line
            lines.push(current_line);
            current_line = word.to_string();
            current_len = word_len;
        }
    }

    if !current_line.is_empty() {
        lines.push(current_line);
    }

    if lines.is_empty() {
        lines.push(String::new());
    }

    lines
}

/// Pure render function for a setting item (returns layout, doesn't modify external state)
///
/// # Arguments
/// * `skip_top` - Number of rows to skip at top of item (for partial visibility when scrolling)
/// * `label_width` - Optional label width for column alignment
fn render_setting_item_pure(
    frame: &mut Frame,
    area: Rect,
    item: &super::items::SettingItem,
    idx: usize,
    skip_top: u16,
    ctx: &RenderContext,
    theme: &Theme,
    label_width: Option<u16>,
) -> ControlLayoutInfo {
    let is_selected = ctx.settings_focused && idx == ctx.selected_item;

    // Check if this item or any of its controls is being hovered
    let is_item_hovered = match ctx.hover_hit {
        Some(SettingsHit::Item(i)) => i == idx,
        Some(SettingsHit::ControlToggle(i)) => i == idx,
        Some(SettingsHit::ControlDecrement(i)) => i == idx,
        Some(SettingsHit::ControlIncrement(i)) => i == idx,
        Some(SettingsHit::ControlDropdown(i)) => i == idx,
        Some(SettingsHit::ControlText(i)) => i == idx,
        Some(SettingsHit::ControlTextListRow(i, _)) => i == idx,
        Some(SettingsHit::ControlMapRow(i, _)) => i == idx,
        _ => false,
    };

    let is_focused_or_hovered = is_selected || is_item_hovered;

    // Calculate content height - expanded when focused/hovered
    let content_height = if is_focused_or_hovered {
        item.content_height_expanded(area.width)
    } else {
        item.content_height()
    };
    // Adjust for skipped rows
    let visible_content_height = content_height.saturating_sub(skip_top);

    // Draw selection or hover highlight background (only for content rows, not spacing)
    if is_focused_or_hovered {
        let bg_style = if is_selected {
            Style::default().bg(theme.current_line_bg)
        } else {
            Style::default().bg(theme.menu_hover_bg)
        };
        for row in 0..visible_content_height.min(area.height) {
            let row_area = Rect::new(area.x, area.y + row, area.width, 1);
            frame.render_widget(Paragraph::new("").style(bg_style), row_area);
        }
    }

    // Calculate control height and area
    let control_height = item.control.control_height();
    let visible_control_height = control_height.saturating_sub(skip_top);
    let control_area = Rect::new(
        area.x,
        area.y,
        area.width,
        visible_control_height.min(area.height),
    );

    // Render the control
    let layout = render_control(
        frame,
        control_area,
        &item.control,
        &item.name,
        item.modified,
        skip_top,
        theme,
        label_width,
        ctx.editing_text,
    );

    // Render description below the control (if visible and exists)
    if let Some(ref description) = item.description {
        // Description starts after the control
        let desc_start_row = control_height.saturating_sub(skip_top);
        if desc_start_row < area.height {
            let desc_y = area.y + desc_start_row;
            let desc_style = Style::default().fg(theme.line_number_fg);
            let max_width = area.width.saturating_sub(2) as usize;

            if is_focused_or_hovered && description.len() > max_width {
                // Wrap description to multiple lines when focused/hovered
                let wrapped_lines = wrap_text(description, max_width);
                let available_rows = area.height.saturating_sub(desc_start_row) as usize;

                for (i, line) in wrapped_lines.iter().take(available_rows).enumerate() {
                    frame.render_widget(
                        Paragraph::new(line.as_str()).style(desc_style),
                        Rect::new(area.x, desc_y + i as u16, area.width, 1),
                    );
                }
            } else {
                // Single line - truncate if too long
                let display_desc = if description.len() > max_width {
                    format!("{}...", &description[..max_width.saturating_sub(3)])
                } else {
                    description.clone()
                };
                frame.render_widget(
                    Paragraph::new(display_desc).style(desc_style),
                    Rect::new(area.x, desc_y, area.width, 1),
                );
            }
        }
    }

    layout
}

/// Render the appropriate control for a setting
///
/// # Arguments
/// * `name` - Setting name (for controls that render their own label)
/// * `modified` - Whether the setting has been modified from default
/// * `skip_rows` - Number of rows to skip at top of control (for partial visibility)
/// * `label_width` - Optional label width for column alignment
/// * `editing_text` - Whether text editing mode is active
fn render_control(
    frame: &mut Frame,
    area: Rect,
    control: &SettingControl,
    name: &str,
    modified: bool,
    skip_rows: u16,
    theme: &Theme,
    label_width: Option<u16>,
    editing_text: bool,
) -> ControlLayoutInfo {
    match control {
        // Single-row controls: only render if not skipped
        SettingControl::Toggle(state) => {
            if skip_rows > 0 {
                return ControlLayoutInfo::Toggle(Rect::default());
            }
            let colors = ToggleColors::from_theme(theme);
            let toggle_layout = render_toggle_aligned(frame, area, state, &colors, label_width);
            ControlLayoutInfo::Toggle(toggle_layout.full_area)
        }

        SettingControl::Number(state) => {
            if skip_rows > 0 {
                return ControlLayoutInfo::Number {
                    decrement: Rect::default(),
                    increment: Rect::default(),
                    value: Rect::default(),
                };
            }
            let colors = NumberInputColors::from_theme(theme);
            let num_layout = render_number_input_aligned(frame, area, state, &colors, label_width);
            ControlLayoutInfo::Number {
                decrement: num_layout.decrement_area,
                increment: num_layout.increment_area,
                value: num_layout.value_area,
            }
        }

        SettingControl::Dropdown(state) => {
            if skip_rows > 0 {
                return ControlLayoutInfo::Dropdown(Rect::default());
            }
            let colors = DropdownColors::from_theme(theme);
            let drop_layout = render_dropdown_aligned(frame, area, state, &colors, label_width);
            ControlLayoutInfo::Dropdown(drop_layout.button_area)
        }

        SettingControl::Text(state) => {
            if skip_rows > 0 {
                return ControlLayoutInfo::Text(Rect::default());
            }
            let colors = TextInputColors::from_theme(theme);
            let text_layout =
                render_text_input_aligned(frame, area, state, &colors, 30, label_width);
            ControlLayoutInfo::Text(text_layout.input_area)
        }

        // Multi-row controls: pass skip_rows to render partial view
        SettingControl::TextList(state) => {
            let colors = TextListColors::from_theme(theme);
            let list_layout = render_text_list_partial(frame, area, state, &colors, 30, skip_rows);
            ControlLayoutInfo::TextList {
                rows: list_layout.rows.iter().map(|r| r.text_area).collect(),
            }
        }

        SettingControl::Map(state) => {
            let colors = MapColors::from_theme(theme);
            let map_layout =
                render_map_partial(frame, area, state, &colors, 20, skip_rows, editing_text);
            ControlLayoutInfo::Map {
                entry_rows: map_layout.entry_areas.iter().map(|e| e.row_area).collect(),
            }
        }

        SettingControl::KeybindingList(state) => {
            let colors = crate::view::controls::KeybindingListColors {
                label_fg: theme.editor_fg,
                key_fg: theme.help_key_fg,
                action_fg: theme.syntax_function,
                focused_bg: theme.selection_bg,
                delete_fg: theme.diagnostic_error_fg,
                add_fg: theme.syntax_string,
            };
            let kb_layout =
                render_keybinding_list_partial(frame, area, state, &colors, skip_rows, modified);
            ControlLayoutInfo::KeybindingList {
                entry_rows: kb_layout.entry_rects,
            }
        }

        SettingControl::Complex { type_name } => {
            if skip_rows > 0 {
                return ControlLayoutInfo::Complex;
            }
            // Render label with modified indicator
            let label_style = Style::default().fg(theme.editor_fg);
            let value_style = Style::default().fg(theme.line_number_fg);
            let modified_indicator = if modified { "• " } else { "" };

            let label = Span::styled(format!("{}{}: ", modified_indicator, name), label_style);
            let value = Span::styled(
                format!("<{} - edit in config.toml>", type_name),
                value_style,
            );

            frame.render_widget(Paragraph::new(Line::from(vec![label, value])), area);
            ControlLayoutInfo::Complex
        }
    }
}

/// Render TextList with partial visibility (skipping top rows)
fn render_text_list_partial(
    frame: &mut Frame,
    area: Rect,
    state: &crate::view::controls::TextListState,
    colors: &TextListColors,
    field_width: u16,
    skip_rows: u16,
) -> crate::view::controls::TextListLayout {
    use crate::view::controls::text_list::{TextListLayout, TextListRowLayout};
    use crate::view::controls::FocusState;

    let empty_layout = TextListLayout {
        rows: Vec::new(),
        full_area: area,
    };

    if area.height == 0 || area.width < 10 {
        return empty_layout;
    }

    let label_color = match state.focus {
        FocusState::Focused => colors.focused,
        FocusState::Hovered => colors.focused,
        FocusState::Disabled => colors.disabled,
        FocusState::Normal => colors.label,
    };

    let mut rows = Vec::new();
    let mut y = area.y;
    let mut content_row = 0u16; // Which row of content we're at

    // Row 0 is label
    if skip_rows == 0 {
        let label_line = Line::from(vec![
            Span::styled(&state.label, Style::default().fg(label_color)),
            Span::raw(":"),
        ]);
        frame.render_widget(
            Paragraph::new(label_line),
            Rect::new(area.x, y, area.width, 1),
        );
        y += 1;
    }
    content_row += 1;

    let indent = 2u16;
    let actual_field_width = field_width.min(area.width.saturating_sub(indent + 5));

    // Render existing items (rows 1 to N)
    for (idx, item) in state.items.iter().enumerate() {
        if y >= area.y + area.height {
            break;
        }

        // Skip rows before skip_rows
        if content_row < skip_rows {
            content_row += 1;
            continue;
        }

        let is_focused = state.focused_item == Some(idx) && state.focus == FocusState::Focused;
        let (border_color, text_color) = if is_focused {
            (colors.focused, colors.text)
        } else if state.focus == FocusState::Disabled {
            (colors.disabled, colors.disabled)
        } else {
            (colors.border, colors.text)
        };

        let inner_width = actual_field_width.saturating_sub(2) as usize;
        let visible: String = item.chars().take(inner_width).collect();
        let padded = format!("{:width$}", visible, width = inner_width);

        let line = Line::from(vec![
            Span::raw(" ".repeat(indent as usize)),
            Span::styled("[", Style::default().fg(border_color)),
            Span::styled(padded, Style::default().fg(text_color)),
            Span::styled("]", Style::default().fg(border_color)),
            Span::raw(" "),
            Span::styled("[x]", Style::default().fg(colors.remove_button)),
        ]);

        let row_area = Rect::new(area.x, y, area.width, 1);
        frame.render_widget(Paragraph::new(line), row_area);

        let text_area = Rect::new(area.x + indent, y, actual_field_width, 1);
        let button_area = Rect::new(area.x + indent + actual_field_width + 1, y, 3, 1);
        rows.push(TextListRowLayout {
            text_area,
            button_area,
            index: Some(idx),
        });

        y += 1;
        content_row += 1;
    }

    // Add-new row
    if y < area.y + area.height && content_row >= skip_rows {
        let add_line = Line::from(vec![
            Span::raw(" ".repeat(indent as usize)),
            Span::styled("[+] Add new", Style::default().fg(colors.add_button)),
        ]);
        let row_area = Rect::new(area.x, y, area.width, 1);
        frame.render_widget(Paragraph::new(add_line), row_area);

        rows.push(TextListRowLayout {
            text_area: Rect::new(area.x + indent, y, 11, 1), // "[+] Add new"
            button_area: Rect::new(area.x + indent, y, 11, 1),
            index: None,
        });
    }

    TextListLayout {
        rows,
        full_area: area,
    }
}

/// Render Map with partial visibility (skipping top rows)
fn render_map_partial(
    frame: &mut Frame,
    area: Rect,
    state: &crate::view::controls::MapState,
    colors: &MapColors,
    key_width: u16,
    skip_rows: u16,
    editing_text: bool,
) -> crate::view::controls::MapLayout {
    use crate::view::controls::map_input::{MapEntryLayout, MapLayout};
    use crate::view::controls::FocusState;

    let empty_layout = MapLayout {
        entry_areas: Vec::new(),
        add_row_area: None,
        full_area: area,
    };

    if area.height == 0 || area.width < 15 {
        return empty_layout;
    }

    let label_color = match state.focus {
        FocusState::Focused => colors.focused,
        FocusState::Hovered => colors.focused,
        FocusState::Disabled => colors.disabled,
        FocusState::Normal => colors.label,
    };

    let mut entry_areas = Vec::new();
    let mut y = area.y;
    let mut content_row = 0u16;

    // Row 0 is label
    if skip_rows == 0 {
        let label_line = Line::from(vec![
            Span::styled(&state.label, Style::default().fg(label_color)),
            Span::raw(":"),
        ]);
        frame.render_widget(
            Paragraph::new(label_line),
            Rect::new(area.x, y, area.width, 1),
        );
        y += 1;
    }
    content_row += 1;

    let indent = 2u16;

    // Render entries
    for (idx, (key, _value)) in state.entries.iter().enumerate() {
        if y >= area.y + area.height {
            break;
        }

        if content_row < skip_rows {
            content_row += 1;
            continue;
        }

        let is_focused = state.focused_entry == Some(idx) && state.focus == FocusState::Focused;
        let key_color = if is_focused {
            colors.focused
        } else if state.focus == FocusState::Disabled {
            colors.disabled
        } else {
            colors.key
        };

        let display_key: String = key.chars().take(key_width as usize).collect();
        let line = Line::from(vec![
            Span::raw(" ".repeat(indent as usize)),
            Span::styled(
                format!("{:width$}", display_key, width = key_width as usize),
                Style::default().fg(key_color),
            ),
            Span::raw(" "),
            Span::styled("[x]", Style::default().fg(colors.remove_button)),
        ]);

        let row_area = Rect::new(area.x, y, area.width, 1);
        frame.render_widget(Paragraph::new(line), row_area);

        entry_areas.push(MapEntryLayout {
            index: idx,
            row_area,
            expand_area: Rect::default(), // Not rendering expand button in partial view
            key_area: Rect::new(area.x + indent, y, key_width, 1),
            remove_area: Rect::new(area.x + indent + key_width + 1, y, 3, 1),
        });

        y += 1;
        content_row += 1;
    }

    // Add-new row
    let add_row_area = if y < area.y + area.height && content_row >= skip_rows {
        let row_area = Rect::new(area.x, y, area.width, 1);

        // Show text input when editing, otherwise show "[+] Add new" button
        if editing_text && state.focused_entry.is_none() {
            // Render text input field for new entry key
            let field_width = key_width.min(area.width.saturating_sub(indent + 4));
            let text = &state.new_key_text;
            let cursor_pos = state.cursor;

            // Pad or truncate text to field width
            let display_text = if text.len() >= field_width as usize {
                &text[..field_width as usize]
            } else {
                text.as_str()
            };
            let padding = field_width as usize - display_text.len();
            let padded = format!("{}{}", display_text, " ".repeat(padding));

            let add_line = Line::from(vec![
                Span::raw(" ".repeat(indent as usize)),
                Span::styled("[", Style::default().fg(colors.border)),
                Span::styled(
                    padded,
                    Style::default()
                        .fg(colors.key)
                        .add_modifier(ratatui::style::Modifier::UNDERLINED),
                ),
                Span::styled("]", Style::default().fg(colors.border)),
            ]);
            frame.render_widget(Paragraph::new(add_line), row_area);

            // Render cursor
            let cursor_x = area.x + indent + 1 + cursor_pos as u16;
            if cursor_x < area.x + area.width {
                frame.set_cursor_position((cursor_x, y));
            }
        } else {
            let add_line = Line::from(vec![
                Span::raw(" ".repeat(indent as usize)),
                Span::styled("[+] Add new", Style::default().fg(colors.add_button)),
            ]);
            frame.render_widget(Paragraph::new(add_line), row_area);
        }
        Some(row_area)
    } else {
        None
    };

    MapLayout {
        entry_areas,
        add_row_area,
        full_area: area,
    }
}

/// Render KeybindingList with partial visibility
fn render_keybinding_list_partial(
    frame: &mut Frame,
    area: Rect,
    state: &crate::view::controls::KeybindingListState,
    colors: &crate::view::controls::KeybindingListColors,
    skip_rows: u16,
    modified: bool,
) -> crate::view::controls::KeybindingListLayout {
    use crate::view::controls::keybinding_list::format_key_combo;
    use crate::view::controls::FocusState;
    use ratatui::text::{Line, Span};
    use ratatui::widgets::Paragraph;

    let empty_layout = crate::view::controls::KeybindingListLayout {
        entry_rects: Vec::new(),
        delete_rects: Vec::new(),
        add_rect: None,
    };

    if area.height == 0 {
        return empty_layout;
    }

    let indent = 2u16;
    let is_focused = state.focus == FocusState::Focused;
    let mut entry_rects = Vec::new();
    let mut delete_rects = Vec::new();
    let mut content_row = 0u16;
    let mut y = area.y;

    // Render label (row 0)
    if content_row >= skip_rows {
        let modified_indicator = if modified { "• " } else { "" };
        let label_line = Line::from(vec![Span::styled(
            format!("{}{}:", modified_indicator, state.label),
            Style::default().fg(colors.label_fg),
        )]);
        frame.render_widget(
            Paragraph::new(label_line),
            Rect::new(area.x, y, area.width, 1),
        );
        y += 1;
    }
    content_row += 1;

    // Render each keybinding entry
    for (idx, binding) in state.bindings.iter().enumerate() {
        if y >= area.y + area.height {
            break;
        }

        if content_row >= skip_rows {
            let entry_area = Rect::new(area.x + indent, y, area.width.saturating_sub(indent), 1);
            entry_rects.push(entry_area);

            let is_entry_focused = is_focused && state.focused_index == Some(idx);
            let bg = if is_entry_focused {
                colors.focused_bg
            } else {
                Color::Reset
            };

            let key_combo = format_key_combo(binding);
            let action = binding
                .get("action")
                .and_then(|a| a.as_str())
                .unwrap_or("(no action)");

            let indicator = if is_entry_focused { "> " } else { "  " };
            let line = Line::from(vec![
                Span::styled(indicator, Style::default().fg(colors.label_fg).bg(bg)),
                Span::styled(
                    format!("{:<20}", key_combo),
                    Style::default().fg(colors.key_fg).bg(bg),
                ),
                Span::styled(" → ", Style::default().fg(colors.label_fg).bg(bg)),
                Span::styled(action, Style::default().fg(colors.action_fg).bg(bg)),
                Span::styled(" [x]", Style::default().fg(colors.delete_fg).bg(bg)),
            ]);
            frame.render_widget(Paragraph::new(line), entry_area);

            // Track delete button area
            let delete_x = entry_area.x + entry_area.width.saturating_sub(4);
            delete_rects.push(Rect::new(delete_x, y, 3, 1));

            y += 1;
        }
        content_row += 1;
    }

    // Render add-new row
    let add_rect = if y < area.y + area.height && content_row >= skip_rows {
        let is_add_focused = is_focused && state.focused_index.is_none();
        let bg = if is_add_focused {
            colors.focused_bg
        } else {
            Color::Reset
        };

        let indicator = if is_add_focused { "> " } else { "  " };
        let line = Line::from(vec![
            Span::styled(indicator, Style::default().fg(colors.label_fg).bg(bg)),
            Span::styled("[+] Add new", Style::default().fg(colors.add_fg).bg(bg)),
        ]);
        let add_area = Rect::new(area.x + indent, y, area.width.saturating_sub(indent), 1);
        frame.render_widget(Paragraph::new(line), add_area);
        Some(add_area)
    } else {
        None
    };

    crate::view::controls::KeybindingListLayout {
        entry_rects,
        delete_rects,
        add_rect,
    }
}

/// Layout info for a control (for hit testing)
#[derive(Debug, Clone)]
pub enum ControlLayoutInfo {
    Toggle(Rect),
    Number {
        decrement: Rect,
        increment: Rect,
        value: Rect,
    },
    Dropdown(Rect),
    Text(Rect),
    TextList {
        rows: Vec<Rect>,
    },
    Map {
        entry_rows: Vec<Rect>,
    },
    KeybindingList {
        entry_rows: Vec<Rect>,
    },
    Complex,
}

/// Render footer with action buttons
fn render_footer(
    frame: &mut Frame,
    modal_area: Rect,
    state: &SettingsState,
    theme: &Theme,
    layout: &mut SettingsLayout,
) {
    use super::layout::SettingsHit;
    use super::state::FocusPanel;

    let footer_y = modal_area.y + modal_area.height - 2;
    let footer_area = Rect::new(
        modal_area.x + 1,
        footer_y,
        modal_area.width.saturating_sub(2),
        1,
    );

    // Draw separator line
    let sep_area = Rect::new(
        modal_area.x + 1,
        footer_y - 1,
        modal_area.width.saturating_sub(2),
        1,
    );
    let sep_line: String = "─".repeat(sep_area.width as usize);
    frame.render_widget(
        Paragraph::new(sep_line).style(Style::default().fg(theme.split_separator_fg)),
        sep_area,
    );

    // Check if footer has keyboard focus
    let footer_focused = state.focus_panel == FocusPanel::Footer;

    // Determine hover and keyboard focus states for buttons
    // Button indices: 0=Reset, 1=Save, 2=Cancel
    let reset_hovered = matches!(state.hover_hit, Some(SettingsHit::ResetButton));
    let save_hovered = matches!(state.hover_hit, Some(SettingsHit::SaveButton));
    let cancel_hovered = matches!(state.hover_hit, Some(SettingsHit::CancelButton));

    let reset_focused = footer_focused && state.footer_button_index == 0;
    let save_focused = footer_focused && state.footer_button_index == 1;
    let cancel_focused = footer_focused && state.footer_button_index == 2;

    // Calculate button positions from right
    // When focused, buttons get a "▶" prefix adding 1 char
    let cancel_width = if cancel_focused { 11 } else { 10 }; // "▶[ Cancel ]" or "[ Cancel ]"
    let save_width = if save_focused { 9 } else { 8 }; // "▶[ Save ]" or "[ Save ]"
    let reset_width = if reset_focused { 10 } else { 9 }; // "▶[ Reset ]" or "[ Reset ]"
    let gap = 2;

    let cancel_x = footer_area.x + footer_area.width - cancel_width;
    let save_x = cancel_x - save_width - gap;
    let reset_x = save_x - reset_width - gap;

    // Render buttons with focus indicators
    // Reset button
    let reset_area = Rect::new(reset_x, footer_y, reset_width, 1);
    if reset_focused {
        let style = Style::default()
            .fg(theme.menu_highlight_fg)
            .bg(theme.menu_highlight_bg)
            .add_modifier(Modifier::BOLD);
        frame.render_widget(Paragraph::new("▶[ Reset ]").style(style), reset_area);
    } else if reset_hovered {
        let style = Style::default()
            .fg(theme.menu_hover_fg)
            .bg(theme.menu_hover_bg);
        frame.render_widget(Paragraph::new("[ Reset ]").style(style), reset_area);
    } else {
        frame.render_widget(
            Paragraph::new("[ Reset ]").style(Style::default().fg(theme.popup_text_fg)),
            reset_area,
        );
    }
    layout.reset_button = Some(reset_area);

    // Save button
    let save_area = Rect::new(save_x, footer_y, save_width, 1);
    if save_focused {
        let style = Style::default()
            .fg(theme.menu_highlight_fg)
            .bg(theme.menu_highlight_bg)
            .add_modifier(Modifier::BOLD);
        frame.render_widget(Paragraph::new("▶[ Save ]").style(style), save_area);
    } else if save_hovered {
        let style = Style::default()
            .fg(theme.menu_hover_fg)
            .bg(theme.menu_hover_bg);
        frame.render_widget(Paragraph::new("[ Save ]").style(style), save_area);
    } else {
        frame.render_widget(
            Paragraph::new("[ Save ]").style(Style::default().fg(theme.popup_text_fg)),
            save_area,
        );
    }
    layout.save_button = Some(save_area);

    // Cancel button
    let cancel_area = Rect::new(cancel_x, footer_y, cancel_width, 1);
    if cancel_focused {
        let style = Style::default()
            .fg(theme.menu_highlight_fg)
            .bg(theme.menu_highlight_bg)
            .add_modifier(Modifier::BOLD);
        frame.render_widget(Paragraph::new("▶[ Cancel ]").style(style), cancel_area);
    } else if cancel_hovered {
        let style = Style::default()
            .fg(theme.menu_hover_fg)
            .bg(theme.menu_hover_bg);
        frame.render_widget(Paragraph::new("[ Cancel ]").style(style), cancel_area);
    } else {
        frame.render_widget(
            Paragraph::new("[ Cancel ]").style(Style::default().fg(theme.popup_text_fg)),
            cancel_area,
        );
    }
    layout.cancel_button = Some(cancel_area);

    // Help text on the left
    let help = if state.search_active {
        "Type to search, ↑↓:Navigate  Enter:Jump  Esc:Cancel"
    } else if footer_focused {
        "←/→:Select button  Enter:Activate  Tab:Switch panel  Esc:Close"
    } else {
        "↑↓:Navigate  Tab:Switch panel  Enter:Edit  /:Search  Esc:Close"
    };
    let help_style = Style::default().fg(theme.line_number_fg);
    frame.render_widget(
        Paragraph::new(help).style(help_style),
        Rect::new(footer_area.x, footer_y, reset_x - footer_area.x - 1, 1),
    );
}

/// Render the search header with query input
fn render_search_header(frame: &mut Frame, area: Rect, state: &SettingsState, theme: &Theme) {
    // First line: Search input
    let search_style = Style::default().fg(theme.popup_text_fg);
    let cursor_style = Style::default()
        .fg(theme.menu_highlight_fg)
        .add_modifier(Modifier::UNDERLINED);

    let spans = vec![
        Span::styled("🔍 ", search_style),
        Span::styled(&state.search_query, search_style),
        Span::styled("█", cursor_style), // Cursor
    ];
    let line = Line::from(spans);
    frame.render_widget(
        Paragraph::new(line),
        Rect::new(area.x, area.y, area.width, 1),
    );

    // Second line: Result count
    let result_count = state.search_results.len();
    let count_text = if result_count == 0 {
        if state.search_query.is_empty() {
            String::new()
        } else {
            "No results found".to_string()
        }
    } else if result_count == 1 {
        "1 result".to_string()
    } else {
        format!("{} results", result_count)
    };

    let count_style = Style::default().fg(theme.line_number_fg);
    frame.render_widget(
        Paragraph::new(count_text).style(count_style),
        Rect::new(area.x, area.y + 1, area.width, 1),
    );
}

/// Render search results with breadcrumbs
fn render_search_results(
    frame: &mut Frame,
    area: Rect,
    state: &SettingsState,
    theme: &Theme,
    layout: &mut SettingsLayout,
) {
    let mut y = area.y;

    for (idx, result) in state.search_results.iter().enumerate() {
        if y >= area.y + area.height.saturating_sub(3) {
            break;
        }

        let is_selected = idx == state.selected_search_result;
        let item_area = Rect::new(area.x, y, area.width, 3);

        render_search_result_item(frame, item_area, result, is_selected, theme, layout);
        y += 3;
    }
}

/// Render a single search result with breadcrumb
fn render_search_result_item(
    frame: &mut Frame,
    area: Rect,
    result: &SearchResult,
    is_selected: bool,
    theme: &Theme,
    layout: &mut SettingsLayout,
) {
    // Draw selection highlight background
    if is_selected {
        let bg_style = Style::default().bg(theme.current_line_bg);
        for row in 0..area.height.min(3) {
            let row_area = Rect::new(area.x, area.y + row, area.width, 1);
            frame.render_widget(Paragraph::new("").style(bg_style), row_area);
        }
    }

    // First line: Setting name with highlighting
    let name_style = if is_selected {
        Style::default().fg(theme.menu_highlight_fg)
    } else {
        Style::default().fg(theme.popup_text_fg)
    };

    // Build name with match highlighting
    let name_line = build_highlighted_text(
        &result.item.name,
        &result.name_matches,
        name_style,
        Style::default()
            .fg(theme.diagnostic_warning_fg)
            .add_modifier(Modifier::BOLD),
    );
    frame.render_widget(
        Paragraph::new(name_line),
        Rect::new(area.x, area.y, area.width, 1),
    );

    // Second line: Breadcrumb
    let breadcrumb_style = Style::default()
        .fg(theme.line_number_fg)
        .add_modifier(Modifier::ITALIC);
    let breadcrumb = format!("  {} > {}", result.breadcrumb, result.item.path);
    let breadcrumb_line = Line::from(Span::styled(breadcrumb, breadcrumb_style));
    frame.render_widget(
        Paragraph::new(breadcrumb_line),
        Rect::new(area.x, area.y + 1, area.width, 1),
    );

    // Third line: Description (if any)
    if let Some(ref desc) = result.item.description {
        let desc_style = Style::default().fg(theme.line_number_fg);
        let truncated_desc = if desc.len() > area.width as usize - 2 {
            format!("  {}...", &desc[..area.width as usize - 5])
        } else {
            format!("  {}", desc)
        };
        frame.render_widget(
            Paragraph::new(truncated_desc).style(desc_style),
            Rect::new(area.x, area.y + 2, area.width, 1),
        );
    }

    // Track this item in layout
    layout.add_search_result(result.page_index, result.item_index, area);
}

/// Build a line with highlighted match positions
fn build_highlighted_text(
    text: &str,
    matches: &[usize],
    normal_style: Style,
    highlight_style: Style,
) -> Line<'static> {
    if matches.is_empty() {
        return Line::from(Span::styled(text.to_string(), normal_style));
    }

    let chars: Vec<char> = text.chars().collect();
    let mut spans = Vec::new();
    let mut current = String::new();
    let mut in_highlight = false;

    for (idx, ch) in chars.iter().enumerate() {
        let should_highlight = matches.contains(&idx);

        if should_highlight != in_highlight {
            if !current.is_empty() {
                let style = if in_highlight {
                    highlight_style
                } else {
                    normal_style
                };
                spans.push(Span::styled(current, style));
                current = String::new();
            }
            in_highlight = should_highlight;
        }

        current.push(*ch);
    }

    // Push remaining
    if !current.is_empty() {
        let style = if in_highlight {
            highlight_style
        } else {
            normal_style
        };
        spans.push(Span::styled(current, style));
    }

    Line::from(spans)
}

/// Render the unsaved changes confirmation dialog
fn render_confirm_dialog(
    frame: &mut Frame,
    parent_area: Rect,
    state: &SettingsState,
    theme: &Theme,
) {
    // Calculate dialog size
    let changes = state.get_change_descriptions();
    let dialog_width = 50.min(parent_area.width.saturating_sub(4));
    // Base height: 2 borders + 2 prompt lines + 1 separator + 1 buttons + 1 help = 7
    // Plus one line per change
    let dialog_height = (7 + changes.len() as u16)
        .min(20)
        .min(parent_area.height.saturating_sub(4));

    // Center the dialog
    let dialog_x = parent_area.x + (parent_area.width.saturating_sub(dialog_width)) / 2;
    let dialog_y = parent_area.y + (parent_area.height.saturating_sub(dialog_height)) / 2;
    let dialog_area = Rect::new(dialog_x, dialog_y, dialog_width, dialog_height);

    // Clear and draw border
    frame.render_widget(Clear, dialog_area);

    let block = Block::default()
        .title(" Unsaved Changes ")
        .borders(Borders::ALL)
        .border_style(Style::default().fg(theme.diagnostic_warning_fg))
        .style(Style::default().bg(theme.popup_bg));
    frame.render_widget(block, dialog_area);

    // Inner area
    let inner = Rect::new(
        dialog_area.x + 2,
        dialog_area.y + 1,
        dialog_area.width.saturating_sub(4),
        dialog_area.height.saturating_sub(2),
    );

    let mut y = inner.y;

    // Prompt text
    let prompt = "You have unsaved changes:";
    let prompt_style = Style::default().fg(theme.popup_text_fg);
    frame.render_widget(
        Paragraph::new(prompt).style(prompt_style),
        Rect::new(inner.x, y, inner.width, 1),
    );
    y += 2;

    // List changes
    let change_style = Style::default().fg(theme.popup_text_fg);
    for change in changes
        .iter()
        .take((dialog_height as usize).saturating_sub(7))
    {
        let truncated = if change.len() > inner.width as usize - 2 {
            format!("• {}...", &change[..inner.width as usize - 5])
        } else {
            format!("• {}", change)
        };
        frame.render_widget(
            Paragraph::new(truncated).style(change_style),
            Rect::new(inner.x, y, inner.width, 1),
        );
        y += 1;
    }

    // Skip to button row
    let button_y = dialog_area.y + dialog_area.height - 3;

    // Draw separator
    let sep_line: String = "─".repeat(inner.width as usize);
    frame.render_widget(
        Paragraph::new(sep_line).style(Style::default().fg(theme.split_separator_fg)),
        Rect::new(inner.x, button_y - 1, inner.width, 1),
    );

    // Render the three options
    let options = ["Save and Exit", "Discard", "Cancel"];
    let total_width: u16 = options.iter().map(|o| o.len() as u16 + 4).sum::<u16>() + 4; // +4 for gaps
    let mut x = inner.x + (inner.width.saturating_sub(total_width)) / 2;

    for (idx, label) in options.iter().enumerate() {
        let is_selected = idx == state.confirm_dialog_selection;
        let button_width = label.len() as u16 + 4;

        let style = if is_selected {
            Style::default()
                .fg(theme.menu_highlight_fg)
                .bg(theme.menu_highlight_bg)
                .add_modifier(ratatui::style::Modifier::BOLD)
        } else {
            Style::default().fg(theme.popup_text_fg)
        };

        let text = if is_selected {
            format!("▶[ {} ]", label)
        } else {
            format!(" [ {} ]", label)
        };
        frame.render_widget(
            Paragraph::new(text).style(style),
            Rect::new(x, button_y, button_width + 1, 1),
        );

        x += button_width + 3;
    }

    // Help text
    let help = "←/→: Select   Enter: Confirm   Esc: Cancel";
    let help_style = Style::default().fg(theme.line_number_fg);
    frame.render_widget(
        Paragraph::new(help).style(help_style),
        Rect::new(inner.x, button_y + 1, inner.width, 1),
    );
}

/// Render the entry detail dialog for editing Language/LSP/Keybinding entries
fn render_entry_dialog(frame: &mut Frame, parent_area: Rect, state: &SettingsState, theme: &Theme) {
    use super::entry_dialog::FieldValue;

    let Some(dialog) = &state.entry_dialog else {
        return;
    };

    // Calculate dialog size - slightly smaller than parent
    let dialog_width = (parent_area.width * 75 / 100).min(80).max(50);
    let dialog_height = (parent_area.height * 80 / 100).min(30).max(15);
    let dialog_x = parent_area.x + (parent_area.width.saturating_sub(dialog_width)) / 2;
    let dialog_y = parent_area.y + (parent_area.height.saturating_sub(dialog_height)) / 2;

    let dialog_area = Rect::new(dialog_x, dialog_y, dialog_width, dialog_height);

    // Clear and draw border
    frame.render_widget(Clear, dialog_area);

    let title = format!(" {} ", dialog.title);

    let block = Block::default()
        .title(title)
        .borders(Borders::ALL)
        .border_style(Style::default().fg(theme.popup_border_fg))
        .style(Style::default().bg(theme.popup_bg));
    frame.render_widget(block, dialog_area);

    // Inner area
    let inner = Rect::new(
        dialog_area.x + 2,
        dialog_area.y + 1,
        dialog_area.width.saturating_sub(4),
        dialog_area.height.saturating_sub(3),
    );

    // Calculate optimal label column width based on actual labels
    // Cap at half of available width to ensure space for values
    let max_label_width = (inner.width / 2).max(20);
    let label_col_width = dialog
        .fields
        .iter()
        .map(|f| f.label.len() as u16 + 2) // +2 for ": "
        .filter(|&w| w <= max_label_width) // Only consider labels that fit
        .max()
        .unwrap_or(20)
        .min(max_label_width);

    let mut y = inner.y;

    for (idx, field) in dialog.fields.iter().enumerate() {
        if y >= inner.y + inner.height.saturating_sub(2) {
            break;
        }

        let is_focused = !dialog.focus_on_buttons && dialog.focused_field == idx;
        let label_style = if is_focused {
            Style::default().fg(theme.menu_highlight_fg)
        } else {
            Style::default().fg(theme.editor_fg)
        };

        let label = format!("{}: ", field.label);
        let label_len = label.len() as u16;

        // Determine if label is too long and value should wrap to next line
        let wrap_value = label_len > max_label_width;

        // Render label (full width if wrapping, or column width if inline)
        let label_area_width = if wrap_value {
            inner.width
        } else {
            label_col_width
        };
        let display_label = if wrap_value {
            label.clone()
        } else {
            format!("{:width$}", label, width = label_col_width as usize)
        };

        frame.render_widget(
            Paragraph::new(display_label).style(label_style),
            Rect::new(inner.x, y, label_area_width, 1),
        );

        // Calculate control position
        let (control_x, control_width, control_y) = if wrap_value {
            // Value on next line, indented
            y += 1;
            if y >= inner.y + inner.height.saturating_sub(2) {
                break;
            }
            (inner.x + 2, inner.width.saturating_sub(2), y)
        } else {
            // Value inline after label column
            (
                inner.x + label_col_width,
                inner.width.saturating_sub(label_col_width),
                y,
            )
        };
        let control_area = Rect::new(control_x, control_y, control_width, 1);

        match &field.value {
            FieldValue::Bool(checked) => {
                let colors = ToggleColors::from_theme(theme);
                let toggle_text = if *checked { "[x]" } else { "[ ]" };
                let toggle_style = if is_focused {
                    Style::default()
                        .fg(colors.focused)
                        .add_modifier(Modifier::BOLD)
                } else if *checked {
                    Style::default().fg(colors.checkmark)
                } else {
                    Style::default().fg(colors.bracket)
                };
                frame.render_widget(
                    Paragraph::new(toggle_text).style(toggle_style),
                    control_area,
                );
            }

            FieldValue::Text {
                value,
                cursor,
                editing,
            } => {
                render_dialog_text_field(
                    frame,
                    control_area,
                    value,
                    *cursor,
                    *editing,
                    is_focused,
                    theme,
                );
            }

            FieldValue::OptionalText {
                value,
                cursor,
                editing,
            } => {
                let display = value.as_deref().unwrap_or("(none)");
                render_dialog_text_field(
                    frame,
                    control_area,
                    display,
                    *cursor,
                    *editing,
                    is_focused,
                    theme,
                );
            }

            FieldValue::StringList {
                items,
                focused_index,
                new_text,
                cursor: _,
                editing,
            } => {
                // Render as compact inline list: [item1, item2, ...]  [+ Add]
                let items_str = if items.is_empty() {
                    "(empty)".to_string()
                } else {
                    items.join(", ")
                };

                let style = if is_focused {
                    Style::default().fg(theme.menu_highlight_fg)
                } else {
                    Style::default().fg(theme.editor_fg)
                };

                // Truncate if too long
                let max_len = control_width.saturating_sub(10) as usize;
                let display = if items_str.len() > max_len {
                    format!("{}...", &items_str[..max_len.saturating_sub(3)])
                } else {
                    items_str
                };

                frame.render_widget(Paragraph::new(display).style(style), control_area);

                // Show list items on next lines if focused
                if is_focused && !items.is_empty() {
                    y += 1;
                    for (item_idx, item) in items.iter().enumerate() {
                        if y >= inner.y + inner.height.saturating_sub(2) {
                            break;
                        }
                        let item_focused = *focused_index == Some(item_idx);
                        let item_style = if item_focused {
                            Style::default()
                                .fg(theme.menu_highlight_fg)
                                .add_modifier(Modifier::BOLD)
                        } else {
                            Style::default().fg(theme.editor_fg)
                        };
                        let prefix = if item_focused { "  > " } else { "    " };
                        frame.render_widget(
                            Paragraph::new(format!("{}{} [x]", prefix, item)).style(item_style),
                            Rect::new(control_x, y, control_width, 1),
                        );
                        y += 1;
                    }
                    // Add input row
                    if y < inner.y + inner.height.saturating_sub(2) {
                        let input_focused = focused_index.is_none();
                        let input_style = if input_focused && *editing {
                            Style::default().fg(theme.cursor)
                        } else if input_focused {
                            Style::default().fg(theme.menu_highlight_fg)
                        } else {
                            Style::default().fg(theme.line_number_fg)
                        };
                        frame.render_widget(
                            Paragraph::new(format!("    [{}] [+]", new_text)).style(input_style),
                            Rect::new(control_x, y, control_width, 1),
                        );
                    }
                }
            }

            FieldValue::Integer {
                value,
                editing,
                text,
                ..
            } => {
                let display = if *editing {
                    text.clone()
                } else {
                    value.to_string()
                };
                render_dialog_text_field(
                    frame,
                    control_area,
                    &display,
                    display.len(),
                    *editing,
                    is_focused,
                    theme,
                );
            }

            FieldValue::Dropdown {
                options,
                selected,
                open,
            } => {
                let current = options.get(*selected).map(|s| s.as_str()).unwrap_or("");
                let style = if is_focused {
                    Style::default().fg(theme.menu_highlight_fg)
                } else {
                    Style::default().fg(theme.editor_fg)
                };
                frame.render_widget(
                    Paragraph::new(format!("[{} ▼]", current)).style(style),
                    control_area,
                );

                // Show dropdown options if open
                if *open {
                    for (opt_idx, opt) in options.iter().enumerate() {
                        y += 1;
                        if y >= inner.y + inner.height.saturating_sub(2) {
                            break;
                        }
                        let opt_style = if opt_idx == *selected {
                            Style::default()
                                .fg(theme.menu_highlight_fg)
                                .add_modifier(Modifier::BOLD)
                        } else {
                            Style::default().fg(theme.editor_fg)
                        };
                        let prefix = if opt_idx == *selected { "  > " } else { "    " };
                        frame.render_widget(
                            Paragraph::new(format!("{}{}", prefix, opt)).style(opt_style),
                            Rect::new(control_x, y, control_width, 1),
                        );
                    }
                }
            }

            FieldValue::Object { json, .. } => {
                let preview = match json {
                    serde_json::Value::Object(obj) => format!("{{...}} ({} fields)", obj.len()),
                    _ => "{}".to_string(),
                };
                let style = Style::default().fg(theme.line_number_fg);
                frame.render_widget(Paragraph::new(preview).style(style), control_area);
            }
        }

        y += 1;
    }

    // Render buttons at bottom
    let button_y = dialog_area.y + dialog_area.height - 2;
    let buttons: Vec<&str> = if dialog.is_new {
        vec!["[ Save ]", "[ Cancel ]"]
    } else {
        vec!["[ Save ]", "[ Delete ]", "[ Cancel ]"]
    };
    let button_width: u16 = buttons.iter().map(|b| b.len() as u16 + 2).sum();
    let button_x = dialog_area.x + (dialog_area.width.saturating_sub(button_width)) / 2;

    let mut x = button_x;
    for (idx, label) in buttons.iter().enumerate() {
        let is_selected = dialog.focus_on_buttons && dialog.focused_button == idx;
        let is_delete = !dialog.is_new && idx == 1;
        let style = if is_selected {
            Style::default()
                .fg(theme.menu_highlight_fg)
                .add_modifier(Modifier::BOLD | Modifier::REVERSED)
        } else if is_delete {
            Style::default().fg(theme.diagnostic_error_fg)
        } else {
            Style::default().fg(theme.editor_fg)
        };
        frame.render_widget(
            Paragraph::new(*label).style(style),
            Rect::new(x, button_y, label.len() as u16, 1),
        );
        x += label.len() as u16 + 2;
    }

    // Render help text
    let help = "↑↓:Navigate  Tab:Fields/Buttons  Enter:Edit/Confirm  Esc:Cancel";
    let help_style = Style::default().fg(theme.line_number_fg);
    frame.render_widget(
        Paragraph::new(help).style(help_style),
        Rect::new(
            dialog_area.x + 2,
            button_y + 1,
            dialog_area.width.saturating_sub(4),
            1,
        ),
    );
}

/// Helper to render a text field in the entry dialog
fn render_dialog_text_field(
    frame: &mut Frame,
    area: Rect,
    value: &str,
    cursor: usize,
    editing: bool,
    focused: bool,
    theme: &Theme,
) {
    let style = if editing {
        Style::default().fg(theme.cursor)
    } else if focused {
        Style::default().fg(theme.menu_highlight_fg)
    } else {
        Style::default().fg(theme.editor_fg)
    };

    // Truncate if needed
    let max_len = area.width.saturating_sub(2) as usize;
    let display = if value.len() > max_len {
        format!("{}...", &value[..max_len.saturating_sub(3)])
    } else {
        value.to_string()
    };

    frame.render_widget(Paragraph::new(format!("[{}]", display)).style(style), area);

    // Show cursor if editing
    if editing && cursor <= display.len() && cursor < area.width.saturating_sub(1) as usize {
        let cursor_x = area.x + 1 + cursor as u16;
        let cursor_char = display.chars().nth(cursor).unwrap_or(' ');
        let cursor_span = Span::styled(
            cursor_char.to_string(),
            Style::default()
                .fg(theme.cursor)
                .add_modifier(Modifier::REVERSED),
        );
        frame.render_widget(
            Paragraph::new(Line::from(cursor_span)),
            Rect::new(cursor_x, area.y, 1, 1),
        );
    }
}

/// Render the help overlay showing keyboard shortcuts
fn render_help_overlay(frame: &mut Frame, parent_area: Rect, theme: &Theme) {
    // Define the help content
    let help_items = [
        (
            "Navigation",
            vec![
                ("↑ / ↓", "Move up/down"),
                ("Tab", "Switch between categories and settings"),
                ("Enter", "Activate/toggle setting"),
            ],
        ),
        (
            "Search",
            vec![
                ("/", "Start search"),
                ("Esc", "Cancel search"),
                ("↑ / ↓", "Navigate results"),
                ("Enter", "Jump to result"),
            ],
        ),
        (
            "Actions",
            vec![
                ("Ctrl+S", "Save settings"),
                ("Ctrl+R", "Reset to default"),
                ("Esc", "Close settings"),
                ("?", "Toggle this help"),
            ],
        ),
    ];

    // Calculate dialog size
    let dialog_width = 50.min(parent_area.width.saturating_sub(4));
    let dialog_height = 20.min(parent_area.height.saturating_sub(4));

    // Center the dialog
    let dialog_x = parent_area.x + (parent_area.width.saturating_sub(dialog_width)) / 2;
    let dialog_y = parent_area.y + (parent_area.height.saturating_sub(dialog_height)) / 2;
    let dialog_area = Rect::new(dialog_x, dialog_y, dialog_width, dialog_height);

    // Clear and draw border
    frame.render_widget(Clear, dialog_area);

    let block = Block::default()
        .title(" Keyboard Shortcuts ")
        .borders(Borders::ALL)
        .border_style(Style::default().fg(theme.menu_highlight_fg))
        .style(Style::default().bg(theme.popup_bg));
    frame.render_widget(block, dialog_area);

    // Inner area
    let inner = Rect::new(
        dialog_area.x + 2,
        dialog_area.y + 1,
        dialog_area.width.saturating_sub(4),
        dialog_area.height.saturating_sub(2),
    );

    let mut y = inner.y;

    for (section_name, bindings) in &help_items {
        if y >= inner.y + inner.height.saturating_sub(1) {
            break;
        }

        // Section header
        let header_style = Style::default()
            .fg(theme.menu_active_fg)
            .add_modifier(Modifier::BOLD);
        frame.render_widget(
            Paragraph::new(*section_name).style(header_style),
            Rect::new(inner.x, y, inner.width, 1),
        );
        y += 1;

        for (key, description) in bindings {
            if y >= inner.y + inner.height.saturating_sub(1) {
                break;
            }

            let key_style = Style::default()
                .fg(theme.diagnostic_info_fg)
                .add_modifier(Modifier::BOLD);
            let desc_style = Style::default().fg(theme.popup_text_fg);

            let line = Line::from(vec![
                Span::styled(format!("  {:12}", key), key_style),
                Span::styled(*description, desc_style),
            ]);
            frame.render_widget(Paragraph::new(line), Rect::new(inner.x, y, inner.width, 1));
            y += 1;
        }

        y += 1; // Blank line between sections
    }

    // Footer hint
    let footer_y = dialog_area.y + dialog_area.height - 2;
    let footer = "Press ? or Esc or Enter to close";
    let footer_style = Style::default().fg(theme.line_number_fg);
    let centered_x = inner.x + (inner.width.saturating_sub(footer.len() as u16)) / 2;
    frame.render_widget(
        Paragraph::new(footer).style(footer_style),
        Rect::new(centered_x, footer_y, footer.len() as u16, 1),
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    // Basic compile test - actual rendering tests would need a test backend
    #[test]
    fn test_control_layout_info() {
        let toggle = ControlLayoutInfo::Toggle(Rect::new(0, 0, 10, 1));
        assert!(matches!(toggle, ControlLayoutInfo::Toggle(_)));

        let number = ControlLayoutInfo::Number {
            decrement: Rect::new(0, 0, 3, 1),
            increment: Rect::new(4, 0, 3, 1),
            value: Rect::new(8, 0, 5, 1),
        };
        assert!(matches!(number, ControlLayoutInfo::Number { .. }));
    }
}
