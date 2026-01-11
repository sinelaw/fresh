//! Settings UI renderer
//!
//! Renders the settings modal with category navigation and setting controls.

use rust_i18n::t;

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

/// Build spans for a text line with selection highlighting
///
/// Returns a vector of spans where selected portions are highlighted.
fn build_selection_spans(
    display_text: &str,
    display_len: usize,
    line_idx: usize,
    start_row: usize,
    start_col: usize,
    end_row: usize,
    end_col: usize,
    text_color: Color,
    selection_bg: Color,
) -> Vec<Span<'static>> {
    let chars: Vec<char> = display_text.chars().collect();
    let char_count = chars.len();

    // Determine selection range for this line
    let (sel_start, sel_end) = if line_idx < start_row || line_idx > end_row {
        // Line not in selection
        (char_count, char_count)
    } else if line_idx == start_row && line_idx == end_row {
        // Selection within single line
        let start = byte_to_char_idx(display_text, start_col).min(char_count);
        let end = byte_to_char_idx(display_text, end_col).min(char_count);
        (start, end)
    } else if line_idx == start_row {
        // Selection starts on this line
        let start = byte_to_char_idx(display_text, start_col).min(char_count);
        (start, char_count)
    } else if line_idx == end_row {
        // Selection ends on this line
        let end = byte_to_char_idx(display_text, end_col).min(char_count);
        (0, end)
    } else {
        // Entire line is selected
        (0, char_count)
    };

    let mut spans = Vec::new();
    let normal_style = Style::default().fg(text_color);
    let selected_style = Style::default().fg(text_color).bg(selection_bg);

    if sel_start >= sel_end || sel_start >= char_count {
        // No selection on this line
        let padded = format!("{:width$}", display_text, width = display_len);
        spans.push(Span::styled(padded, normal_style));
    } else {
        // Before selection
        if sel_start > 0 {
            let before: String = chars[..sel_start].iter().collect();
            spans.push(Span::styled(before, normal_style));
        }

        // Selection
        let selected: String = chars[sel_start..sel_end].iter().collect();
        spans.push(Span::styled(selected, selected_style));

        // After selection
        if sel_end < char_count {
            let after: String = chars[sel_end..].iter().collect();
            spans.push(Span::styled(after, normal_style));
        }

        // Pad to display_len
        let current_len = char_count;
        if current_len < display_len {
            let padding = " ".repeat(display_len - current_len);
            spans.push(Span::styled(padding, normal_style));
        }
    }

    spans
}

/// Convert byte offset to char index in a string
fn byte_to_char_idx(s: &str, byte_offset: usize) -> usize {
    s.char_indices()
        .take_while(|(i, _)| *i < byte_offset)
        .count()
}

/// Render the settings modal
pub fn render_settings(
    frame: &mut Frame,
    area: Rect,
    state: &mut SettingsState,
    theme: &Theme,
) -> SettingsLayout {
    // Calculate modal size (80% of screen width, 90% height to fill most of available space)
    let modal_width = (area.width * 80 / 100).min(100);
    let modal_height = area.height * 90 / 100;
    let modal_x = (area.width.saturating_sub(modal_width)) / 2;
    let modal_y = (area.height.saturating_sub(modal_height)) / 2;

    let modal_area = Rect::new(modal_x, modal_y, modal_width, modal_height);

    // Clear the modal area and draw border
    frame.render_widget(Clear, modal_area);

    let title = if state.has_changes() {
        format!(" Settings [{}] • (modified) ", state.target_layer_name())
    } else {
        format!(" Settings [{}] ", state.target_layer_name())
    };

    let block = Block::default()
        .title(title.as_str())
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
        let modified_indicator = if has_changes { "●" } else { " " };

        // Show ">" when selected and focused for clearer selection indicator
        let selection_indicator = if is_selected && state.focus_panel == FocusPanel::Categories {
            ">"
        } else {
            " "
        };

        let text = format!(
            "{}{} {}",
            selection_indicator, modified_indicator, page.name
        );
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
struct RenderContext<'a> {
    selected_item: usize,
    settings_focused: bool,
    hover_hit: Option<SettingsHit>,
    layer_sources: &'a std::collections::HashMap<String, crate::config_io::ConfigLayer>,
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
        layer_sources: &state.layer_sources,
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
    ctx: &RenderContext<'_>,
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

    // Focus indicator takes 2 chars ("> ")
    let focus_indicator_width: u16 = 2;

    // Calculate content height - expanded when focused/hovered
    let content_height = if is_focused_or_hovered {
        item.content_height_expanded(area.width.saturating_sub(focus_indicator_width))
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

    // Render focus indicator ">" for selected items (when settings panel is focused)
    if is_selected && skip_top == 0 {
        let indicator_style = Style::default()
            .fg(theme.menu_highlight_fg)
            .add_modifier(Modifier::BOLD);
        frame.render_widget(
            Paragraph::new(">").style(indicator_style),
            Rect::new(area.x, area.y, 1, 1),
        );
    }

    // Calculate control height and area (offset by focus indicator)
    let control_height = item.control.control_height();
    let visible_control_height = control_height.saturating_sub(skip_top);
    let control_area = Rect::new(
        area.x + focus_indicator_width,
        area.y,
        area.width.saturating_sub(focus_indicator_width),
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
        label_width.map(|w| w.saturating_sub(focus_indicator_width)),
        item.read_only,
    );

    // Render description below the control (if visible and exists)
    // Description is also offset by focus_indicator_width to align with control
    let desc_start_row = control_height.saturating_sub(skip_top);

    // Get layer source for this item (only show if not default)
    let layer_source = ctx
        .layer_sources
        .get(&item.path)
        .copied()
        .unwrap_or(crate::config_io::ConfigLayer::System);
    let layer_label = match layer_source {
        crate::config_io::ConfigLayer::System => None, // Don't show for defaults
        crate::config_io::ConfigLayer::User => Some("user"),
        crate::config_io::ConfigLayer::Project => Some("project"),
        crate::config_io::ConfigLayer::Session => Some("session"),
    };

    if let Some(ref description) = item.description {
        if desc_start_row < area.height {
            let desc_x = area.x + focus_indicator_width;
            let desc_y = area.y + desc_start_row;
            let desc_width = area.width.saturating_sub(focus_indicator_width);
            let desc_style = Style::default().fg(theme.line_number_fg);
            let max_width = desc_width.saturating_sub(2) as usize;

            if is_focused_or_hovered && description.len() > max_width {
                // Wrap description to multiple lines when focused/hovered
                let wrapped_lines = wrap_text(description, max_width);
                let available_rows = area.height.saturating_sub(desc_start_row) as usize;

                for (i, line) in wrapped_lines.iter().take(available_rows).enumerate() {
                    frame.render_widget(
                        Paragraph::new(line.as_str()).style(desc_style),
                        Rect::new(desc_x, desc_y + i as u16, desc_width, 1),
                    );
                }
            } else {
                // Single line with optional layer indicator
                let mut display_desc = if description.len() > max_width.saturating_sub(12) {
                    format!(
                        "{}...",
                        &description[..max_width.saturating_sub(15).max(10)]
                    )
                } else {
                    description.clone()
                };
                // Add layer indicator if not default
                if let Some(layer) = layer_label {
                    display_desc.push_str(&format!(" ({})", layer));
                }
                frame.render_widget(
                    Paragraph::new(display_desc).style(desc_style),
                    Rect::new(desc_x, desc_y, desc_width, 1),
                );
            }
        }
    } else if let Some(layer) = layer_label {
        // No description, but show layer indicator for non-default values
        if desc_start_row < area.height && is_focused_or_hovered {
            let desc_x = area.x + focus_indicator_width;
            let desc_y = area.y + desc_start_row;
            let desc_width = area.width.saturating_sub(focus_indicator_width);
            let layer_style = Style::default().fg(theme.line_number_fg);
            frame.render_widget(
                Paragraph::new(format!("({})", layer)).style(layer_style),
                Rect::new(desc_x, desc_y, desc_width, 1),
            );
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
/// * `read_only` - Whether this field is read-only (displays as plain text instead of input)
fn render_control(
    frame: &mut Frame,
    area: Rect,
    control: &SettingControl,
    name: &str,
    modified: bool,
    skip_rows: u16,
    theme: &Theme,
    label_width: Option<u16>,
    read_only: bool,
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
            if read_only {
                // Render read-only text as plain text (not editable)
                let label_w = label_width.unwrap_or(20);
                let label_style = Style::default().fg(theme.editor_fg);
                let value_style = Style::default().fg(theme.line_number_fg);
                let label = format!("{}: ", state.label);
                let value = &state.value;

                let label_area = Rect::new(area.x, area.y, label_w, 1);
                let value_area = Rect::new(
                    area.x + label_w,
                    area.y,
                    area.width.saturating_sub(label_w),
                    1,
                );

                frame.render_widget(Paragraph::new(label.clone()).style(label_style), label_area);
                frame.render_widget(
                    Paragraph::new(value.as_str()).style(value_style),
                    value_area,
                );
                ControlLayoutInfo::Text(Rect::default()) // No clickable area for read-only
            } else {
                let colors = TextInputColors::from_theme(theme);
                let text_layout =
                    render_text_input_aligned(frame, area, state, &colors, 30, label_width);
                ControlLayoutInfo::Text(text_layout.input_area)
            }
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
            let map_layout = render_map_partial(frame, area, state, &colors, 20, skip_rows);
            ControlLayoutInfo::Map {
                entry_rows: map_layout.entry_areas.iter().map(|e| e.row_area).collect(),
            }
        }

        SettingControl::ObjectArray(state) => {
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
            ControlLayoutInfo::ObjectArray {
                entry_rows: kb_layout.entry_rects,
            }
        }

        SettingControl::Json(state) => {
            render_json_control(frame, area, state, name, modified, skip_rows, theme)
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

/// Render a multiline JSON editor control
fn render_json_control(
    frame: &mut Frame,
    area: Rect,
    state: &super::items::JsonEditState,
    name: &str,
    modified: bool,
    skip_rows: u16,
    theme: &Theme,
) -> ControlLayoutInfo {
    use crate::view::controls::FocusState;

    let empty_layout = ControlLayoutInfo::Json {
        edit_area: Rect::default(),
    };

    if area.height == 0 || area.width < 10 {
        return empty_layout;
    }

    let is_focused = state.focus == FocusState::Focused;
    let is_valid = state.is_valid();

    let label_color = if is_focused {
        theme.menu_highlight_fg
    } else {
        theme.editor_fg
    };

    let text_color = theme.editor_fg;
    let border_color = if !is_valid {
        theme.diagnostic_error_fg
    } else if is_focused {
        theme.menu_highlight_fg
    } else {
        theme.split_separator_fg
    };

    let mut y = area.y;
    let mut content_row = 0u16;

    // Row 0: label
    if content_row >= skip_rows {
        let modified_indicator = if modified { "• " } else { "" };
        let label_line = Line::from(vec![Span::styled(
            format!("{}{}:", modified_indicator, name),
            Style::default().fg(label_color),
        )]);
        frame.render_widget(
            Paragraph::new(label_line),
            Rect::new(area.x, y, area.width, 1),
        );
        y += 1;
    }
    content_row += 1;

    let indent = 2u16;
    let edit_width = area.width.saturating_sub(indent + 1);
    let edit_x = area.x + indent;
    let edit_start_y = y;

    // Render all lines (scrolling handled by entry dialog/scroll panel)
    let lines = state.lines();
    let total_lines = lines.len();
    for line_idx in 0..total_lines {
        let actual_line_idx = line_idx;

        if content_row < skip_rows {
            content_row += 1;
            continue;
        }

        if y >= area.y + area.height {
            break;
        }

        let line_content = lines.get(actual_line_idx).map(|s| s.as_str()).unwrap_or("");

        // Truncate line if too long
        let display_len = edit_width.saturating_sub(2) as usize;
        let display_text: String = line_content.chars().take(display_len).collect();

        // Get selection range and cursor position
        let selection = state.selection_range();
        let (cursor_row, cursor_col) = state.cursor_pos();

        // Build content spans with selection highlighting
        let content_spans = if is_focused {
            if let Some(((start_row, start_col), (end_row, end_col))) = selection {
                build_selection_spans(
                    &display_text,
                    display_len,
                    actual_line_idx,
                    start_row,
                    start_col,
                    end_row,
                    end_col,
                    text_color,
                    theme.selection_bg,
                )
            } else {
                vec![Span::styled(
                    format!("{:width$}", display_text, width = display_len),
                    Style::default().fg(text_color),
                )]
            }
        } else {
            vec![Span::styled(
                format!("{:width$}", display_text, width = display_len),
                Style::default().fg(text_color),
            )]
        };

        // Build line with border
        let mut spans = vec![
            Span::raw(" ".repeat(indent as usize)),
            Span::styled("│", Style::default().fg(border_color)),
        ];
        spans.extend(content_spans);
        spans.push(Span::styled("│", Style::default().fg(border_color)));
        let line = Line::from(spans);

        frame.render_widget(Paragraph::new(line), Rect::new(area.x, y, area.width, 1));

        // Draw cursor if focused and on this line (overlays selection)
        if is_focused && actual_line_idx == cursor_row {
            let cursor_x = edit_x + 1 + cursor_col.min(display_len) as u16;
            if cursor_x < area.x + area.width - 1 {
                let cursor_char = line_content.chars().nth(cursor_col).unwrap_or(' ');
                let cursor_span = Span::styled(
                    cursor_char.to_string(),
                    Style::default()
                        .fg(theme.cursor)
                        .add_modifier(Modifier::REVERSED),
                );
                frame.render_widget(
                    Paragraph::new(Line::from(vec![cursor_span])),
                    Rect::new(cursor_x, y, 1, 1),
                );
            }
        }

        y += 1;
        content_row += 1;
    }

    // Show invalid JSON indicator
    if !is_valid && y < area.y + area.height {
        let warning = Span::styled(
            "  ⚠ Invalid JSON",
            Style::default().fg(theme.diagnostic_warning_fg),
        );
        frame.render_widget(
            Paragraph::new(Line::from(vec![warning])),
            Rect::new(area.x, y, area.width, 1),
        );
    }

    let edit_height = y.saturating_sub(edit_start_y);
    ControlLayoutInfo::Json {
        edit_area: Rect::new(edit_x, edit_start_y, edit_width, edit_height),
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
        // Check if we're focused on the add-new input (focused_item is None and focused)
        let is_add_focused = state.focused_item.is_none() && state.focus == FocusState::Focused;

        if is_add_focused {
            // Show input field with new_item_text
            let inner_width = actual_field_width.saturating_sub(2) as usize;
            let visible: String = state.new_item_text.chars().take(inner_width).collect();
            let padded = format!("{:width$}", visible, width = inner_width);

            let line = Line::from(vec![
                Span::raw(" ".repeat(indent as usize)),
                Span::styled("[", Style::default().fg(colors.focused)),
                Span::styled(padded, Style::default().fg(colors.text)),
                Span::styled("]", Style::default().fg(colors.focused)),
                Span::raw(" "),
                Span::styled("[+]", Style::default().fg(colors.add_button)),
            ]);
            let row_area = Rect::new(area.x, y, area.width, 1);
            frame.render_widget(Paragraph::new(line), row_area);

            // Render cursor
            if state.cursor <= inner_width {
                let cursor_x = area.x + indent + 1 + state.cursor as u16;
                let cursor_char = state.new_item_text.chars().nth(state.cursor).unwrap_or(' ');
                let cursor_area = Rect::new(cursor_x, y, 1, 1);
                let cursor_span = Span::styled(
                    cursor_char.to_string(),
                    Style::default()
                        .fg(colors.focused)
                        .add_modifier(ratatui::style::Modifier::REVERSED),
                );
                frame.render_widget(Paragraph::new(Line::from(vec![cursor_span])), cursor_area);
            }

            rows.push(TextListRowLayout {
                text_area: Rect::new(area.x + indent, y, actual_field_width, 1),
                button_area: Rect::new(area.x + indent + actual_field_width + 1, y, 3, 1),
                index: None,
            });
        } else {
            // Show static "[+] Add new" label
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
    for (idx, (key, value)) in state.entries.iter().enumerate() {
        if y >= area.y + area.height {
            break;
        }

        if content_row < skip_rows {
            content_row += 1;
            continue;
        }

        let is_focused = state.focused_entry == Some(idx) && state.focus == FocusState::Focused;

        let row_area = Rect::new(area.x, y, area.width, 1);

        // Full row background highlight for focused entry
        if is_focused {
            let highlight_style = Style::default().bg(colors.focused);
            let bg_line = Line::from(Span::styled(
                " ".repeat(area.width as usize),
                highlight_style,
            ));
            frame.render_widget(Paragraph::new(bg_line), row_area);
        }

        let (key_color, value_color) = if is_focused {
            (colors.label, colors.value_preview)
        } else if state.focus == FocusState::Disabled {
            (colors.disabled, colors.disabled)
        } else {
            (colors.key, colors.value_preview)
        };

        let base_style = if is_focused {
            Style::default().bg(colors.focused)
        } else {
            Style::default()
        };

        // Get display value
        let value_preview = state.get_display_value(value);
        let max_preview_len = 20;
        let value_preview = if value_preview.len() > max_preview_len {
            format!("{}...", &value_preview[..max_preview_len - 3])
        } else {
            value_preview
        };

        let display_key: String = key.chars().take(key_width as usize).collect();
        let mut spans = vec![
            Span::styled(" ".repeat(indent as usize), base_style),
            Span::styled(
                format!("{:width$}", display_key, width = key_width as usize),
                base_style.fg(key_color),
            ),
            Span::raw(" "),
            Span::styled(value_preview, base_style.fg(value_color)),
        ];

        // Add [Edit] hint for focused entry
        if is_focused {
            spans.push(Span::styled(
                "  [Enter to edit]",
                base_style
                    .fg(colors.value_preview)
                    .add_modifier(Modifier::DIM),
            ));
        }

        frame.render_widget(Paragraph::new(Line::from(spans)), row_area);

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

    // Add-new row (only show if adding is allowed)
    let add_row_area = if !state.no_add && y < area.y + area.height && content_row >= skip_rows {
        let row_area = Rect::new(area.x, y, area.width, 1);
        let is_focused = state.focused_entry.is_none() && state.focus == FocusState::Focused;

        // Highlight row when focused
        if is_focused {
            let highlight_style = Style::default().bg(colors.focused);
            let bg_line = Line::from(Span::styled(
                " ".repeat(area.width as usize),
                highlight_style,
            ));
            frame.render_widget(Paragraph::new(bg_line), row_area);
        }

        let base_style = if is_focused {
            Style::default().bg(colors.focused)
        } else {
            Style::default()
        };

        let mut spans = vec![
            Span::styled(" ".repeat(indent as usize), base_style),
            Span::styled("[+] Add new", base_style.fg(colors.add_button)),
        ];

        if is_focused {
            spans.push(Span::styled(
                "  [Enter to add]",
                base_style
                    .fg(colors.value_preview)
                    .add_modifier(Modifier::DIM),
            ));
        }

        frame.render_widget(Paragraph::new(Line::from(spans)), row_area);
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
            // Use display_field from state if available, otherwise default to "action"
            let field_name = state
                .display_field
                .as_ref()
                .and_then(|p| p.strip_prefix('/'))
                .unwrap_or("action");
            let action = binding
                .get(field_name)
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
    ObjectArray {
        entry_rows: Vec<Rect>,
    },
    Json {
        edit_area: Rect,
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
    // Button indices: 0=Layer, 1=Reset, 2=Save, 3=Cancel, 4=Edit (on left, for advanced users)
    let layer_hovered = matches!(state.hover_hit, Some(SettingsHit::LayerButton));
    let reset_hovered = matches!(state.hover_hit, Some(SettingsHit::ResetButton));
    let save_hovered = matches!(state.hover_hit, Some(SettingsHit::SaveButton));
    let cancel_hovered = matches!(state.hover_hit, Some(SettingsHit::CancelButton));
    let edit_hovered = matches!(state.hover_hit, Some(SettingsHit::EditButton));

    let layer_focused = footer_focused && state.footer_button_index == 0;
    let reset_focused = footer_focused && state.footer_button_index == 1;
    let save_focused = footer_focused && state.footer_button_index == 2;
    let cancel_focused = footer_focused && state.footer_button_index == 3;
    let edit_focused = footer_focused && state.footer_button_index == 4;

    // Build layer button text dynamically
    let layer_text = format!("[ {} ]", state.target_layer_name());
    let layer_text_focused = format!(">[ {} ]", state.target_layer_name());

    // Calculate button positions from right (main buttons)
    // When focused, buttons get a ">" prefix adding 1 char
    let cancel_width = if cancel_focused { 11 } else { 10 }; // ">[ Cancel ]" or "[ Cancel ]"
    let save_width = if save_focused { 9 } else { 8 }; // ">[ Save ]" or "[ Save ]"
    let reset_width = if reset_focused { 10 } else { 9 }; // ">[ Reset ]" or "[ Reset ]"
    let layer_width = if layer_focused {
        layer_text_focused.len() as u16
    } else {
        layer_text.len() as u16
    };
    let gap = 2;

    let cancel_x = footer_area.x + footer_area.width - cancel_width;
    let save_x = cancel_x - save_width - gap;
    let reset_x = save_x - reset_width - gap;
    let layer_x = reset_x - layer_width - gap;

    // Edit button on left (separated for advanced users)
    let edit_width = if edit_focused { 9 } else { 8 }; // ">[ Edit ]" or "[ Edit ]"
    let edit_x = footer_area.x; // Left-aligned

    // Render buttons with focus indicators
    // Layer button
    let layer_area = Rect::new(layer_x, footer_y, layer_width, 1);
    if layer_focused {
        let style = Style::default()
            .fg(theme.menu_highlight_fg)
            .bg(theme.menu_highlight_bg)
            .add_modifier(Modifier::BOLD);
        frame.render_widget(
            Paragraph::new(layer_text_focused.as_str()).style(style),
            layer_area,
        );
    } else if layer_hovered {
        let style = Style::default()
            .fg(theme.menu_hover_fg)
            .bg(theme.menu_hover_bg);
        frame.render_widget(Paragraph::new(layer_text.as_str()).style(style), layer_area);
    } else {
        frame.render_widget(
            Paragraph::new(layer_text.as_str()).style(Style::default().fg(theme.popup_text_fg)),
            layer_area,
        );
    }
    layout.layer_button = Some(layer_area);

    // Reset button
    let reset_area = Rect::new(reset_x, footer_y, reset_width, 1);
    if reset_focused {
        let style = Style::default()
            .fg(theme.menu_highlight_fg)
            .bg(theme.menu_highlight_bg)
            .add_modifier(Modifier::BOLD);
        frame.render_widget(Paragraph::new(">[ Reset ]").style(style), reset_area);
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
        frame.render_widget(Paragraph::new(">[ Save ]").style(style), save_area);
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
        frame.render_widget(Paragraph::new(">[ Cancel ]").style(style), cancel_area);
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

    // Edit button (on left, for advanced users)
    let edit_area = Rect::new(edit_x, footer_y, edit_width, 1);
    if edit_focused {
        let style = Style::default()
            .fg(theme.menu_highlight_fg)
            .bg(theme.menu_highlight_bg)
            .add_modifier(Modifier::BOLD);
        frame.render_widget(Paragraph::new(">[ Edit ]").style(style), edit_area);
    } else if edit_hovered {
        let style = Style::default()
            .fg(theme.menu_hover_fg)
            .bg(theme.menu_hover_bg);
        frame.render_widget(Paragraph::new("[ Edit ]").style(style), edit_area);
    } else {
        // Dim style for advanced option
        frame.render_widget(
            Paragraph::new("[ Edit ]").style(Style::default().fg(theme.line_number_fg)),
            edit_area,
        );
    }
    layout.edit_button = Some(edit_area);

    // Help text (between Edit button and main buttons)
    let help_x = edit_x + edit_width + 2;
    let help_width = layer_x.saturating_sub(help_x + 1);
    let help = if state.search_active {
        "Type to search, ↑↓:Navigate  Enter:Jump  Esc:Cancel"
    } else if footer_focused {
        "Tab:Next button  Enter:Activate  Esc:Close"
    } else {
        "↑↓:Navigate  Tab:Next  Enter:Edit  /:Search  Esc:Close"
    };
    let help_style = Style::default().fg(theme.line_number_fg);
    frame.render_widget(
        Paragraph::new(help).style(help_style),
        Rect::new(help_x, footer_y, help_width, 1),
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

    let title = format!(" {} ", t!("confirm.unsaved_changes_title"));
    let block = Block::default()
        .title(title)
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
    let prompt = t!("confirm.unsaved_changes_prompt").to_string();
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
    let options = [
        t!("confirm.save_and_exit").to_string(),
        t!("confirm.discard").to_string(),
        t!("confirm.cancel").to_string(),
    ];
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
            format!(">[ {} ]", label)
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
///
/// Now uses the same SettingItem/SettingControl infrastructure as the main settings UI,
/// eliminating duplication and ensuring consistent rendering.
fn render_entry_dialog(
    frame: &mut Frame,
    parent_area: Rect,
    state: &mut SettingsState,
    theme: &Theme,
) {
    let Some(dialog) = state.entry_dialog_mut() else {
        return;
    };

    // Calculate dialog size - use most of available space for editing
    let dialog_width = (parent_area.width * 85 / 100).min(90).max(50);
    let dialog_height = (parent_area.height * 90 / 100).max(15);
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

    // Inner area (reserve 2 lines for buttons and help at bottom)
    let inner = Rect::new(
        dialog_area.x + 2,
        dialog_area.y + 1,
        dialog_area.width.saturating_sub(4),
        dialog_area.height.saturating_sub(5), // 1 border + 2 button/help rows + 2 padding
    );

    // Calculate optimal label column width based on actual item names
    let max_label_width = (inner.width / 2).max(20);
    let label_col_width = dialog
        .items
        .iter()
        .map(|item| item.name.len() as u16 + 2) // +2 for ": "
        .filter(|&w| w <= max_label_width)
        .max()
        .unwrap_or(20)
        .min(max_label_width);

    // Calculate total content height and viewport
    let total_content_height = dialog.total_content_height();
    let viewport_height = inner.height as usize;

    // Store viewport height for use in focus navigation
    dialog.viewport_height = viewport_height;

    let scroll_offset = dialog.scroll_offset;
    let needs_scroll = total_content_height > viewport_height;

    // Track current position in content (for scrolling)
    let mut content_y: usize = 0;
    let mut screen_y = inner.y;

    for (idx, item) in dialog.items.iter().enumerate() {
        let control_height = item.control.control_height() as usize;

        // Check if this item is visible in the viewport
        let item_start = content_y;
        let item_end = content_y + control_height;

        // Skip items completely above the viewport
        if item_end <= scroll_offset {
            content_y = item_end;
            continue;
        }

        // Stop if we're past the viewport
        if screen_y >= inner.y + inner.height {
            break;
        }

        // Calculate how many rows to skip at top of this item
        let skip_rows = if item_start < scroll_offset {
            (scroll_offset - item_start) as u16
        } else {
            0
        };

        // Calculate visible height for this item
        let visible_height = control_height.saturating_sub(skip_rows as usize);
        let available_height = (inner.y + inner.height).saturating_sub(screen_y) as usize;
        let render_height = visible_height.min(available_height);

        if render_height == 0 {
            content_y = item_end;
            continue;
        }

        let is_focused = !dialog.focus_on_buttons && dialog.selected_item == idx;
        let is_hovered = dialog.hover_item == Some(idx);

        // Draw selection or hover highlight background
        if is_focused || is_hovered {
            let bg_style = if is_focused {
                Style::default().bg(theme.current_line_bg)
            } else {
                Style::default().bg(theme.menu_hover_bg)
            };
            for row in 0..render_height as u16 {
                let row_area = Rect::new(inner.x, screen_y + row, inner.width, 1);
                frame.render_widget(Paragraph::new("").style(bg_style), row_area);
            }
        }

        // Render focus indicator ">" for the focused item
        let focus_indicator_width: u16 = 2; // "> "
        if is_focused && skip_rows == 0 {
            let indicator_style = Style::default()
                .fg(theme.menu_highlight_fg)
                .add_modifier(Modifier::BOLD);
            frame.render_widget(
                Paragraph::new(">").style(indicator_style),
                Rect::new(inner.x, screen_y, 1, 1),
            );
        }

        // Calculate control area (offset by focus indicator width)
        let control_area = Rect::new(
            inner.x + focus_indicator_width,
            screen_y,
            inner.width.saturating_sub(focus_indicator_width),
            render_height as u16,
        );

        // Render using the same render_control function as main settings
        let _layout = render_control(
            frame,
            control_area,
            &item.control,
            &item.name,
            item.modified,
            skip_rows,
            theme,
            Some(label_col_width.saturating_sub(focus_indicator_width)),
            item.read_only,
        );

        screen_y += render_height as u16;
        content_y = item_end;
    }

    // Render scrollbar if needed
    if needs_scroll {
        use crate::view::ui::scrollbar::{render_scrollbar, ScrollbarColors, ScrollbarState};

        let scrollbar_x = dialog_area.x + dialog_area.width - 3;
        let scrollbar_area = Rect::new(scrollbar_x, inner.y, 1, inner.height);
        let scrollbar_state =
            ScrollbarState::new(total_content_height, viewport_height, scroll_offset);
        let scrollbar_colors = ScrollbarColors::from_theme(theme);
        render_scrollbar(frame, scrollbar_area, &scrollbar_state, &scrollbar_colors);
    }

    // Render buttons at bottom
    let button_y = dialog_area.y + dialog_area.height - 2;
    let buttons: Vec<&str> = if dialog.is_new {
        vec!["[ Save ]", "[ Cancel ]"]
    } else {
        vec!["[ Save ]", "[ Delete ]", "[ Cancel ]"]
    };
    let button_width: u16 = buttons.iter().map(|b: &&str| b.len() as u16 + 2).sum();
    let button_x = dialog_area.x + (dialog_area.width.saturating_sub(button_width)) / 2;

    let mut x = button_x;
    for (idx, label) in buttons.iter().enumerate() {
        let is_selected = dialog.focus_on_buttons && dialog.focused_button == idx;
        let is_hovered = dialog.hover_button == Some(idx);
        let is_delete = !dialog.is_new && idx == 1;
        let style = if is_selected {
            Style::default()
                .fg(theme.menu_highlight_fg)
                .add_modifier(Modifier::BOLD | Modifier::REVERSED)
        } else if is_hovered {
            Style::default()
                .fg(theme.menu_hover_fg)
                .bg(theme.menu_hover_bg)
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

    // Check if current item has invalid JSON (for Text controls with validation)
    // and if we're actively editing a JSON control
    let is_editing_json = dialog.editing_text && dialog.is_editing_json();
    let (has_invalid_json, is_json_control) = dialog
        .current_item()
        .map(|item| match &item.control {
            SettingControl::Text(state) => (!state.is_valid(), false),
            SettingControl::Json(state) => (!state.is_valid(), is_editing_json),
            _ => (false, false),
        })
        .unwrap_or((false, false));

    // Render help text or warning
    let help_area = Rect::new(
        dialog_area.x + 2,
        button_y + 1,
        dialog_area.width.saturating_sub(4),
        1,
    );

    if has_invalid_json && !is_json_control {
        // Text control with JSON validation - must fix before leaving
        let warning = "⚠ Invalid JSON - fix before leaving field";
        let warning_style = Style::default().fg(theme.diagnostic_warning_fg);
        frame.render_widget(Paragraph::new(warning).style(warning_style), help_area);
    } else if has_invalid_json && is_json_control {
        // JSON control with invalid JSON
        let warning = "⚠ Invalid JSON";
        let warning_style = Style::default().fg(theme.diagnostic_warning_fg);
        frame.render_widget(Paragraph::new(warning).style(warning_style), help_area);
    } else if is_json_control {
        // Editing JSON control
        let help = "↑↓←→:Move  Enter:Newline  Tab/Esc:Exit";
        let help_style = Style::default().fg(theme.line_number_fg);
        frame.render_widget(Paragraph::new(help).style(help_style), help_area);
    } else {
        let help = "↑↓:Navigate  Tab:Fields/Buttons  Enter:Edit/Confirm  Esc:Cancel";
        let help_style = Style::default().fg(theme.line_number_fg);
        frame.render_widget(Paragraph::new(help).style(help_style), help_area);
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
