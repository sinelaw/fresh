//! Settings UI renderer
//!
//! Renders the settings modal with category navigation and setting controls.

use rust_i18n::t;

use crate::primitives::display_width::str_width;

use super::items::SettingControl;
use super::layout::{SettingsHit, SettingsLayout};
use super::search::{DeepMatch, SearchResult};
use super::state::SettingsState;
use crate::view::controls::{
    render_dropdown_aligned, render_dual_list_partial, render_number_input_aligned,
    render_text_input_aligned, render_toggle_aligned, DropdownColors, DualListColors, MapColors,
    NumberInputColors, TextInputColors, TextListColors, ToggleColors,
};
use crate::view::theme::Theme;
use crate::view::ui::scrollbar::{render_scrollbar, ScrollbarColors, ScrollbarState};
use ratatui::layout::{Constraint, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, BorderType, Borders, Clear, Paragraph};
use ratatui::Frame;

/// Build spans for a text line with selection highlighting
///
/// Returns a vector of spans where selected portions are highlighted.
#[allow(clippy::too_many_arguments)]
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

/// Truncate `s` to at most `max_chars` characters, appending `"..."` if it
/// was actually shortened. Counts characters (not bytes) so non-ASCII
/// inputs (CJK descriptions, emoji, etc.) don't byte-slice through a
/// multi-byte UTF-8 sequence and panic — same class as #1718.
fn truncate_chars_with_ellipsis(s: &str, max_chars: usize) -> String {
    if s.chars().count() <= max_chars {
        s.to_string()
    } else {
        let kept: String = s.chars().take(max_chars.saturating_sub(3)).collect();
        format!("{}...", kept)
    }
}

/// Render the settings modal
pub fn render_settings(
    frame: &mut Frame,
    area: Rect,
    state: &mut SettingsState,
    theme: &Theme,
) -> SettingsLayout {
    // Minimum size guard — prevent panics from zero-sized layout arithmetic
    if area.width < 40 || area.height < 10 {
        let msg = "[Terminal too small for settings]";
        let x = area.x + area.width.saturating_sub(msg.len() as u16) / 2;
        let y = area.y + area.height / 2;
        if area.width > 0 && area.height > 0 {
            frame.render_widget(
                Paragraph::new(msg).style(Style::default().fg(theme.diagnostic_warning_fg)),
                Rect::new(x, y, msg.len() as u16, 1),
            );
        }
        return SettingsLayout::new(Rect::ZERO);
    }

    // Calculate modal size (90% of screen width, 90% height to fill most of available space)
    let modal_width = (area.width * 90 / 100).min(160);
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
        .border_type(BorderType::Rounded)
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

    // Determine layout mode: vertical (narrow) vs horizontal (wide)
    // Narrow mode when inner width < 60 columns
    let narrow_mode = inner_area.width < 60;

    // Always render search bar at the top (1 line height to avoid layout
    // jump), with a 1-row blank gap below it so the bar reads as a header
    // rather than running into the panels.
    let search_area = Rect::new(inner_area.x, inner_area.y, inner_area.width, 1);
    let search_header_height = 1u16;
    let search_gap = 1u16;
    if state.search_active {
        render_search_header(frame, search_area, state, theme);
    } else {
        render_search_hint(frame, search_area, theme);
    }

    // Footer height: 2 lines for horizontal (separator + buttons), 7 for vertical
    let footer_height = if narrow_mode { 7 } else { 2 };
    let chrome_height = search_header_height + search_gap + footer_height;
    let content_area = Rect::new(
        inner_area.x,
        inner_area.y + search_header_height + search_gap,
        inner_area.width,
        inner_area.height.saturating_sub(chrome_height),
    );

    // Create layout tracker
    let mut layout = SettingsLayout::new(modal_area);

    if narrow_mode {
        // Vertical layout: categories on top, items below
        render_vertical_layout(frame, content_area, modal_area, state, theme, &mut layout);
    } else {
        // Horizontal layout: categories left, items right
        render_horizontal_layout(frame, content_area, modal_area, state, theme, &mut layout);
    }

    // Determine the topmost dialog layer and apply dimming to layers below
    let has_confirm = state.showing_confirm_dialog;
    let has_reset = state.showing_reset_dialog;
    let has_entry = state.showing_entry_dialog();
    let has_help = state.showing_help;

    // Render confirmation dialog if showing
    if has_confirm {
        if !has_entry && !has_help {
            crate::view::dimming::apply_dimming(frame, modal_area);
        }
        render_confirm_dialog(frame, modal_area, state, theme);
    }

    // Render reset confirmation dialog if showing
    if has_reset {
        if !has_confirm && !has_entry && !has_help {
            crate::view::dimming::apply_dimming(frame, modal_area);
        }
        render_reset_dialog(frame, modal_area, state, theme);
    }

    // Render entry dialog stack — dim between each level
    if has_entry {
        let stack_depth = state.entry_dialog_stack.len();
        for dialog_idx in 0..stack_depth {
            if !has_help || dialog_idx < stack_depth - 1 {
                crate::view::dimming::apply_dimming(frame, modal_area);
            }
            render_entry_dialog_at(frame, modal_area, state, theme, dialog_idx);
        }
    }

    // Render entry-dialog discard-confirm prompt if showing, on top of
    // the entry dialog stack. Dim first so the user can see the prompt
    // is asking about the dialog underneath.
    if state.showing_entry_discard_confirm {
        crate::view::dimming::apply_dimming(frame, modal_area);
        render_entry_discard_confirm(frame, modal_area, state, theme);
    }

    // Render help overlay if showing
    if has_help {
        crate::view::dimming::apply_dimming(frame, modal_area);
        render_help_overlay(frame, modal_area, theme);
    }

    layout
}

/// Render horizontal layout (wide mode): categories left, items right
fn render_horizontal_layout(
    frame: &mut Frame,
    content_area: Rect,
    modal_area: Rect,
    state: &mut SettingsState,
    theme: &Theme,
    layout: &mut SettingsLayout,
) {
    // Layout: [left panel (categories)] | [right panel (settings)]
    // 24 cols for categories, 1 col for the divider, the rest for settings.
    let chunks = Layout::horizontal([
        Constraint::Length(24),
        Constraint::Length(1),
        Constraint::Min(40),
    ])
    .split(content_area);

    let categories_area = chunks[0];
    let divider_area = chunks[1];
    let settings_area = chunks[2];

    // Render category list (left panel)
    render_categories(frame, categories_area, state, theme, layout);

    // Single straight vertical line dividing categories from settings.
    let divider_style = Style::default().fg(theme.split_separator_fg);
    for y in 0..divider_area.height {
        frame.render_widget(
            Paragraph::new("│").style(divider_style),
            Rect::new(divider_area.x, divider_area.y + y, 1, 1),
        );
    }

    // 1-col gutter on each side of the settings panel for breathing room.
    let horizontal_padding = 1u16;
    let settings_inner = Rect::new(
        settings_area.x + horizontal_padding,
        settings_area.y,
        settings_area.width.saturating_sub(horizontal_padding * 2),
        settings_area.height,
    );

    if state.search_active && !state.search_results.is_empty() {
        render_search_results(frame, settings_inner, state, theme, layout);
    } else {
        render_settings_panel(frame, settings_inner, state, theme, layout);
    }

    // Render footer with buttons (horizontal layout)
    render_footer(frame, modal_area, state, theme, layout, false);
}

/// Render vertical layout (narrow mode): categories on top, items below
fn render_vertical_layout(
    frame: &mut Frame,
    content_area: Rect,
    modal_area: Rect,
    state: &mut SettingsState,
    theme: &Theme,
    layout: &mut SettingsLayout,
) {
    // Calculate footer height for vertical buttons (5 buttons + separators)
    let footer_height = 7;

    // Layout: [categories (3 lines)] / [separator] / [settings] / [footer]
    let main_height = content_area.height.saturating_sub(footer_height);
    let category_height = 3u16.min(main_height);
    let settings_height = main_height.saturating_sub(category_height + 1); // +1 for separator

    // Categories area (horizontal strip at top)
    let categories_area = Rect::new(
        content_area.x,
        content_area.y,
        content_area.width,
        category_height,
    );

    // Separator line
    let sep_y = content_area.y + category_height;

    // Settings area
    let settings_area = Rect::new(
        content_area.x,
        sep_y + 1,
        content_area.width,
        settings_height,
    );

    // Render horizontal category strip
    render_categories_horizontal(frame, categories_area, state, theme, layout);

    // Render horizontal separator
    if sep_y < content_area.y + content_area.height {
        let sep_line: String = "─".repeat(content_area.width as usize);
        frame.render_widget(
            Paragraph::new(sep_line).style(Style::default().fg(theme.split_separator_fg)),
            Rect::new(content_area.x, sep_y, content_area.width, 1),
        );
    }

    // Render settings panel
    if state.search_active && !state.search_results.is_empty() {
        render_search_results(frame, settings_area, state, theme, layout);
    } else {
        render_settings_panel(frame, settings_area, state, theme, layout);
    }

    // Render footer with buttons (vertical layout)
    render_footer(frame, modal_area, state, theme, layout, true);
}

/// Render categories as a horizontal strip (for narrow mode)
fn render_categories_horizontal(
    frame: &mut Frame,
    area: Rect,
    state: &SettingsState,
    theme: &Theme,
    layout: &mut SettingsLayout,
) {
    use super::state::FocusPanel;

    if area.height == 0 || area.width == 0 {
        return;
    }

    let is_focused = state.focus_panel() == FocusPanel::Categories;

    // Build category labels with indicators
    let mut spans = Vec::new();
    let mut total_width = 0u16;

    for (i, page) in state.pages.iter().enumerate() {
        let is_selected = i == state.selected_category;
        let has_modified = page.items.iter().any(|item| item.modified);

        let indicator = if has_modified { "● " } else { "  " };
        let name = &page.name;

        let style = if is_selected && is_focused {
            Style::default()
                .fg(theme.menu_highlight_fg)
                .bg(theme.menu_highlight_bg)
                .add_modifier(Modifier::BOLD)
        } else if is_selected {
            Style::default()
                .fg(theme.menu_highlight_fg)
                .add_modifier(Modifier::BOLD)
        } else {
            Style::default().fg(theme.popup_text_fg)
        };

        let indicator_style = if has_modified {
            Style::default().fg(theme.menu_highlight_fg)
        } else {
            style
        };

        // Add separator between categories
        if i > 0 {
            spans.push(Span::styled(
                " │ ",
                Style::default().fg(theme.split_separator_fg),
            ));
            total_width += 3;
        }

        spans.push(Span::styled(indicator, indicator_style));
        spans.push(Span::styled(name.as_str(), style));
        total_width += (indicator.len() + name.len()) as u16;

        // Track category rect for click handling (approximate)
        let cat_x = area.x + total_width.saturating_sub((indicator.len() + name.len()) as u16);
        let cat_width = (indicator.len() + name.len()) as u16;
        layout
            .categories
            .push((i, Rect::new(cat_x, area.y, cat_width, 1)));
    }

    // Render the category line
    let line = Line::from(spans);
    frame.render_widget(Paragraph::new(line), area);

    // Show navigation hint on line 2 if space
    if area.height >= 2 {
        let hint = "←→: Switch category";
        let hint_style = Style::default().fg(theme.line_number_fg);
        frame.render_widget(
            Paragraph::new(hint).style(hint_style),
            Rect::new(area.x, area.y + 1, area.width, 1),
        );
    }
}

/// Get an icon for a settings category name (Nerd Font icons)
fn category_icon(name: &str) -> &'static str {
    match name.to_lowercase().as_str() {
        "general" => "\u{f013} ",       //
        "editor" => "\u{f044} ",        //
        "clipboard" => "\u{f328} ",     //
        "file browser" => "\u{f07b} ",  //
        "file explorer" => "\u{f07c} ", //
        "packages" => "\u{f487} ",      //
        "plugins" => "\u{f1e6} ",       //
        "terminal" => "\u{f120} ",      //
        "warnings" => "\u{f071} ",      //
        "keybindings" => "\u{f11c} ",   //
        _ => "\u{f111} ",               //  (dot circle as fallback)
    }
}

/// Render the category tree (categories + expanded sections) in the left panel.
///
/// Rows are flattened by [`SettingsState::visible_tree`] and rendered through
/// [`ScrollablePanel`], which handles partial-row clipping and the scrollbar.
/// Per-row Rects are recorded on `layout` for hit-testing.
fn render_categories(
    frame: &mut Frame,
    area: Rect,
    state: &mut SettingsState,
    theme: &Theme,
    layout: &mut SettingsLayout,
) {
    use super::state::{FocusPanel, TreeRow};

    layout.categories_panel_area = Some(area);

    let rows = state.visible_tree();
    state.categories_scroll.set_viewport(area.height);
    state
        .categories_scroll
        .update_content_height(&rows, area.width);

    let focus_panel = state.focus_panel();
    let selected_category = state.selected_category;
    // Where the keyboard cursor lives in the tree. `None` = on the
    // category row; `Some(s_idx)` = on the s-th section row inside the
    // currently-selected category. This is the single source of truth
    // for the `>` indicator and the row-bg highlight.
    let tree_cursor = state.tree_cursor_section;

    // Snapshot the data each row needs so we don't hold a borrow on `state`
    // through the render callback.
    struct RowData {
        chevron: &'static str,
        is_expandable: bool,
        is_selected: bool,
        has_changes: bool,
        indent_cols: u16,
        is_category: bool,
        cat_idx: Option<usize>,
        section_idx: Option<usize>,
        label: String,
        icon: Option<&'static str>,
    }
    let row_data: Vec<RowData> = rows
        .iter()
        .map(|row| match *row {
            TreeRow::Category {
                idx,
                expandable,
                expanded,
            } => {
                let page = &state.pages[idx];
                RowData {
                    chevron: if expandable {
                        if expanded {
                            "▼"
                        } else {
                            "▶"
                        }
                    } else {
                        " "
                    },
                    is_expandable: expandable,
                    // Category row is "selected" iff the keyboard cursor
                    // is sitting on it (no section is the cursor target).
                    is_selected: idx == selected_category && tree_cursor.is_none(),
                    has_changes: page.items.iter().any(|i| i.modified),
                    indent_cols: 0,
                    is_category: true,
                    cat_idx: Some(idx),
                    section_idx: None,
                    label: page.name.clone(),
                    icon: Some(category_icon(&page.name)),
                }
            }
            TreeRow::Section {
                cat_idx,
                section_idx,
            } => {
                let section = &state.pages[cat_idx].sections[section_idx];
                // Section row is "selected" iff the explicit tree cursor
                // points at it. The cursor follows the user's keyboard
                // navigation AND syncs to body scroll (handled by the
                // sync-on-scroll path), so this single check covers
                // both keyboard and wheel-driven highlight updates.
                let is_current = cat_idx == selected_category && tree_cursor == Some(section_idx);
                RowData {
                    chevron: " ",
                    is_expandable: false,
                    is_selected: is_current,
                    has_changes: false,
                    indent_cols: 4,
                    is_category: false,
                    cat_idx: Some(cat_idx),
                    section_idx: Some(section_idx),
                    label: section.name.clone(),
                    icon: None,
                }
            }
        })
        .collect();

    // Render through ScrollablePanel so we get scrollbar + clipping.
    let panel_layout = state.categories_scroll.render(
        frame,
        area,
        &rows,
        |frame, info, row| {
            // Find this row's snapshot. `rows` and `row_data` are 1:1 by index.
            let idx = info.index;
            let data = &row_data[idx];
            let row_area = info.area;

            // Only the cursor row paints a bg — no separate hover-bg
            // path. Hover bg in addition to the cursor bg produced two
            // visually-highlighted rows simultaneously (with two
            // *different* colors, since hover and selection use
            // different theme keys), which violates the single-cursor
            // invariant. The OS mouse cursor itself is the user's
            // "where am I" indicator; we don't need an in-app one.
            let row_bg = if data.is_selected {
                if focus_panel == FocusPanel::Categories {
                    Some(theme.menu_highlight_bg)
                } else {
                    Some(theme.selection_bg)
                }
            } else {
                None
            };
            if let Some(bg) = row_bg {
                frame.render_widget(
                    Paragraph::new(" ".repeat(row_area.width as usize))
                        .style(Style::default().bg(bg)),
                    row_area,
                );
            }

            let fg = if data.is_selected {
                if focus_panel == FocusPanel::Categories {
                    theme.menu_highlight_fg
                } else {
                    theme.menu_fg
                }
            } else {
                theme.popup_text_fg
            };
            let bg = row_bg.unwrap_or(theme.popup_bg);
            let style = Style::default().fg(fg).bg(bg);

            let mut spans: Vec<Span> = Vec::with_capacity(8);
            // Selection indicator (">" when this row is the focused one in
            // the categories panel) lives in col 0 before any indentation.
            // The category-selection-indicator-visible test asserts on this.
            let selected_marker = if data.is_selected && focus_panel == FocusPanel::Categories {
                ">"
            } else {
                " "
            };
            spans.push(Span::styled(selected_marker.to_string(), style));
            if data.indent_cols > 0 {
                spans.push(Span::styled(" ".repeat(data.indent_cols as usize), style));
            }
            // Chevron occupies one column; followed by a space for breathing room.
            spans.push(Span::styled(format!("{} ", data.chevron), style));
            if data.has_changes {
                spans.push(Span::styled(
                    "● ",
                    Style::default().fg(theme.menu_highlight_fg).bg(bg),
                ));
            } else {
                spans.push(Span::styled("  ", style));
            }
            if let Some(icon) = data.icon {
                spans.push(Span::styled(
                    icon.to_string(),
                    Style::default().fg(theme.popup_border_fg).bg(bg),
                ));
            } else {
                spans.push(Span::styled(" ", style));
            }
            spans.push(Span::styled(data.label.clone(), style));

            frame.render_widget(Paragraph::new(Line::from(spans)), row_area);

            // Hand back the row identity so we can register hit-test areas
            // after rendering.
            (
                row_area,
                data.is_category,
                data.is_expandable,
                data.cat_idx,
                data.section_idx,
                data.indent_cols,
                *row,
            )
        },
        theme,
    );

    // Translate per-row Rects into hit-test entries.
    for layout_info in panel_layout.item_layouts.iter() {
        let (row_area, is_category, is_expandable, cat_idx, section_idx, indent_cols, _row) =
            layout_info.layout;
        if is_category {
            if let Some(idx) = cat_idx {
                layout.add_category(idx, row_area);
                if is_expandable {
                    // Chevron sits one column after the selection-indicator
                    // marker plus any indent for nested rows.
                    let chevron_x = row_area.x.saturating_add(1 + indent_cols);
                    let chevron_area = Rect::new(chevron_x, row_area.y, 1, 1);
                    layout.add_category_disclosure(idx, chevron_area);
                }
            }
        } else if let (Some(c), Some(s)) = (cat_idx, section_idx) {
            layout.add_section(c, s, row_area);
        }
    }
    if let Some(scrollbar) = panel_layout.scrollbar_area {
        layout.categories_scrollbar_area = Some(scrollbar);
    }
}

/// Context for rendering a setting item (extracted to avoid borrow issues)
struct RenderContext {
    selected_item: usize,
    settings_focused: bool,
    hover_hit: Option<SettingsHit>,
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

    // Page description suppressed: it duplicated the category name visible
    // in the sidebar and pushed the actual settings down without adding
    // information. The category names + section headers carry enough
    // context.
    let mut y = area.y;
    let header_start_y = y;

    // "Clear" button for nullable categories (e.g., Option<LanguageConfig>)
    if page.nullable && state.current_category_has_values() {
        let btn_text = format!("[{}]", t!("settings.btn_clear_category"));
        let btn_len = btn_text.len() as u16;
        let is_hovered = matches!(state.hover_hit, Some(SettingsHit::ClearCategoryButton));
        let btn_style = if is_hovered {
            Style::default()
                .fg(theme.menu_hover_fg)
                .bg(theme.menu_hover_bg)
        } else {
            Style::default().fg(theme.line_number_fg)
        };
        let btn_area = Rect::new(area.x, y, btn_len, 1);
        frame.render_widget(Paragraph::new(btn_text).style(btn_style), btn_area);
        layout.clear_category_button = Some(btn_area);
        y += 1;
    } else {
        layout.clear_category_button = None;
    }

    y += 1; // Blank line

    let header_height = (y - header_start_y) as usize;
    let items_start_y = y;

    // Calculate available height for items
    let available_height = area.height.saturating_sub(header_height as u16);

    // The body panel width is the full width of the area allocated to items.
    // Items size themselves against this width directly via the ScrollItem
    // trait — there's no longer a cached per-item layout_width to keep in
    // sync.
    state.layout_width = area.width;

    // Update scroll panel with current viewport and content
    let page = state.pages.get(state.selected_category).unwrap();
    state.scroll_panel.set_viewport(available_height);
    state
        .scroll_panel
        .update_content_height(&page.items, area.width);

    // Extract state needed for rendering (to avoid borrow issues with scroll_panel)
    use super::state::FocusPanel;
    let render_ctx = RenderContext {
        selected_item: state.selected_item,
        settings_focused: state.focus_panel() == FocusPanel::Settings,
        hover_hit: state.hover_hit,
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
            item_info.layout.control,
            item_info.layout.inherit_button,
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
/// Driven by `item.layout_box(area.width, &item.style)` — every y-offset comes
/// from the resulting `ItemBox`, so adjusting card chrome (border, padding,
/// section header height) happens by changing `ItemBoxStyle`, not by editing
/// renderer arithmetic.
///
/// # Arguments
/// * `skip_top` - Number of rows to skip at top of item (for partial visibility when scrolling)
/// * `label_width` - Optional label width for column alignment
#[allow(clippy::too_many_arguments)]
fn render_setting_item_pure(
    frame: &mut Frame,
    area: Rect,
    item: &super::items::SettingItem,
    idx: usize,
    skip_top: u16,
    ctx: &RenderContext,
    theme: &Theme,
    label_width: Option<u16>,
) -> SettingItemLayoutInfo {
    let plan = item.layout_box(area.width, &item.style);
    let style = item.style;
    let viewport_end_logical = skip_top.saturating_add(area.height); // exclusive

    // Translate a logical band [logical_y, logical_y + rows) to a physical
    // sub-rectangle of `area`, accounting for `skip_top` clipping. Returns
    // None when the band is entirely outside the visible viewport.
    let band_rect = |logical_y: u16, rows: u16| -> Option<Rect> {
        if rows == 0 {
            return None;
        }
        let band_end = logical_y.saturating_add(rows);
        if band_end <= skip_top || logical_y >= viewport_end_logical {
            return None;
        }
        let visible_top_logical = logical_y.max(skip_top);
        let visible_bottom_logical = band_end.min(viewport_end_logical);
        let physical_y = area.y + (visible_top_logical - skip_top);
        let visible_h = visible_bottom_logical - visible_top_logical;
        Some(Rect::new(area.x, physical_y, area.width, visible_h))
    };

    // ── Section header band ────────────────────────────────────────────────
    // Layout: blank gap on the leading rows, title on the last row of the
    // band. This puts the breathing room above the heading and butts the
    // title against the card it labels, which reads as "title belongs to
    // what's below" rather than "title belongs to what's above".
    if let (Some(section_name), Some(_header_rect)) = (
        item.section.as_deref().filter(|_| item.is_section_start),
        band_rect(0, plan.section_header_rows),
    ) {
        let title_logical_y = plan.section_header_rows.saturating_sub(1);
        if let Some(title_rect) = band_rect(title_logical_y, 1) {
            let header_style = Style::default()
                .fg(theme.editor_fg)
                .add_modifier(Modifier::BOLD);
            frame.render_widget(
                Paragraph::new(section_name).style(header_style),
                Rect::new(title_rect.x, title_rect.y, title_rect.width, 1),
            );
        }
    }

    // ── Card box ───────────────────────────────────────────────────────────
    // The card spans logical rows [card_top_y, total_rows). Render it with a
    // single Block, choosing which edges to draw based on which logical rows
    // are inside the visible viewport.
    let card_logical_top = plan.card_top_y();
    let card_logical_bottom = plan.total_rows();
    if let Some(card_rect) = band_rect(
        card_logical_top,
        card_logical_bottom.saturating_sub(card_logical_top),
    ) {
        let mut borders = Borders::NONE;
        if style.card_border_cols > 0 {
            borders |= Borders::LEFT | Borders::RIGHT;
        }
        if style.card_border_rows > 0 {
            // TOP edge is only visible when its logical row sits inside [skip_top, viewport_end).
            if card_logical_top >= skip_top {
                borders |= Borders::TOP;
            }
            // BOTTOM edge is the last logical row of the card.
            let bottom_logical = card_logical_bottom.saturating_sub(1);
            if bottom_logical >= skip_top && bottom_logical < viewport_end_logical {
                borders |= Borders::BOTTOM;
            }
        }
        if !borders.is_empty() {
            // Subdued color for the card chrome — distinct from the
            // panel/popup border around the modal so the cards read as
            // secondary structure, not nested popups.
            let block = Block::default()
                .borders(borders)
                .border_type(BorderType::Rounded)
                .border_style(Style::default().fg(theme.split_separator_fg));
            frame.render_widget(block, card_rect);
        }
    }

    // ── Content area (control + description) ───────────────────────────────
    let is_selected = ctx.settings_focused && idx == ctx.selected_item;
    let is_item_hovered = matches!(
        ctx.hover_hit,
        Some(SettingsHit::Item(i))
            | Some(SettingsHit::ControlToggle(i))
            | Some(SettingsHit::ControlDecrement(i))
            | Some(SettingsHit::ControlIncrement(i))
            | Some(SettingsHit::ControlDropdown(i))
            | Some(SettingsHit::ControlText(i))
            | Some(SettingsHit::ControlTextListRow(i, _))
            | Some(SettingsHit::ControlMapRow(i, _))
            | Some(SettingsHit::ControlInherit(i))
        if i == idx
    );
    let is_focused_or_hovered = is_selected || is_item_hovered;

    // Inner area is the card minus the side borders. Y-axis is the union of
    // the control + description bands.
    let content_logical_top = plan.control_y();
    let content_logical_bottom = plan.bottom_border_y();
    let mut control_layout = ControlLayoutInfo::default();
    let mut inherit_button_area: Option<Rect> = None;
    if let Some(content_rect) = band_rect(
        content_logical_top,
        content_logical_bottom.saturating_sub(content_logical_top),
    ) {
        // Trim left/right by the card side borders.
        let inner_x = content_rect.x.saturating_add(style.card_border_cols);
        let inner_width = content_rect
            .width
            .saturating_sub(2 * style.card_border_cols);
        let inner_area = Rect::new(inner_x, content_rect.y, inner_width, content_rect.height);

        // Highlight background for focused/hovered items. Limited to the
        // label row so chip / description text below stays on popup_bg
        // and remains legible regardless of how saturated the theme's
        // highlight bg is. The colors come from the theme's
        // `settings_selected_bg` (selected) and `menu_hover_bg` (hovered)
        // — each theme is responsible for picking values that contrast
        // with its own popup_bg AND don't collide with chip text colors.
        let label_visible = skip_top <= content_logical_top;
        if is_focused_or_hovered && inner_width > 0 && label_visible {
            let bg_style = if is_selected {
                Style::default().bg(theme.settings_selected_bg)
            } else {
                Style::default().bg(theme.menu_hover_bg)
            };
            let row_area = Rect::new(inner_area.x, inner_area.y, inner_area.width, 1);
            frame.render_widget(Paragraph::new("").style(bg_style), row_area);
        }

        // skip_top relative to the start of the control band — used by
        // multi-row controls and by the description renderer to know how
        // many leading rows are off-screen.
        let content_skip_top = skip_top.saturating_sub(content_logical_top);

        // Focus indicator (`>`) at column 0 of inner area, modified marker
        // (`●`) at column 1. Only paint them when the control's first row is
        // visible (i.e. nothing has been clipped off the top of the content).
        let label_row_visible = content_skip_top == 0 && inner_area.height > 0;
        if is_selected && label_row_visible {
            frame.render_widget(
                Paragraph::new(">").style(
                    Style::default()
                        .fg(theme.settings_selected_fg)
                        .add_modifier(Modifier::BOLD),
                ),
                Rect::new(inner_area.x, inner_area.y, 1, 1),
            );
        }
        if item.modified && label_row_visible && inner_area.width >= 2 {
            frame.render_widget(
                Paragraph::new("●").style(Style::default().fg(theme.settings_selected_fg)),
                Rect::new(inner_area.x + 1, inner_area.y, 1, 1),
            );
        }

        // Control occupies its own band at the top of the content rect.
        let control_logical_rows = plan.control_rows;
        if let Some(control_rect) = band_rect(content_logical_top, control_logical_rows).map(|r| {
            let x =
                r.x.saturating_add(style.card_border_cols + style.focus_indicator_cols);
            let w = r
                .width
                .saturating_sub(2 * style.card_border_cols + style.focus_indicator_cols);
            Rect::new(x, r.y, w, r.height)
        }) {
            control_layout = render_control(
                frame,
                control_rect,
                &item.control,
                &item.name,
                content_skip_top,
                theme,
                label_width
                    .map(|w| w.saturating_sub(style.card_border_cols + style.focus_indicator_cols)),
                item.read_only,
                item.is_null,
            );

            // (Inherited) badge / [Inherit] button: rendered on the same row
            // as the control's first line, at its right edge.
            if item.nullable && content_skip_top == 0 && control_rect.width > 0 {
                if item.is_null {
                    let badge_text = t!("settings.inherited_badge").to_string();
                    let badge_len = badge_text.len() as u16 + 1;
                    let badge_x = control_rect
                        .x
                        .saturating_add(control_rect.width)
                        .saturating_sub(badge_len);
                    if badge_x > control_rect.x {
                        frame.render_widget(
                            Paragraph::new(badge_text).style(
                                Style::default()
                                    .fg(theme.line_number_fg)
                                    .add_modifier(Modifier::ITALIC),
                            ),
                            Rect::new(badge_x, control_rect.y, badge_len, 1),
                        );
                    }
                } else {
                    let btn_text = format!("[{}]", t!("settings.btn_inherit"));
                    let btn_len = btn_text.len() as u16 + 1;
                    let btn_x = control_rect
                        .x
                        .saturating_add(control_rect.width)
                        .saturating_sub(btn_len);
                    if btn_x > control_rect.x {
                        let btn_area = Rect::new(btn_x, control_rect.y, btn_len, 1);
                        let is_hovered = matches!(
                            ctx.hover_hit,
                            Some(SettingsHit::ControlInherit(i)) if i == idx
                        );
                        let btn_style = if is_hovered {
                            Style::default()
                                .fg(theme.menu_hover_fg)
                                .bg(theme.menu_hover_bg)
                        } else {
                            Style::default().fg(theme.line_number_fg)
                        };
                        frame.render_widget(Paragraph::new(btn_text).style(btn_style), btn_area);
                        inherit_button_area = Some(btn_area);
                    }
                }
            }
        }

        // Description band: below the control. Wraps to the inner text width
        // computed by the style, falling back to a layer label when there's
        // no description but we still need to show the source layer.
        let desc_logical_rows = plan.description_rows;
        let layer_label = match item.layer_source {
            crate::config_io::ConfigLayer::System => None,
            crate::config_io::ConfigLayer::User => Some("user"),
            crate::config_io::ConfigLayer::Project => Some("project"),
            crate::config_io::ConfigLayer::Session => Some("session"),
        };

        if desc_logical_rows > 0 {
            if let Some(desc_rect) = band_rect(plan.description_y(), desc_logical_rows).map(|r| {
                let x =
                    r.x.saturating_add(style.card_border_cols + style.focus_indicator_cols);
                let w = r
                    .width
                    .saturating_sub(2 * style.card_border_cols + style.focus_indicator_cols);
                Rect::new(x, r.y, w, r.height)
            }) {
                let desc_skip = skip_top.saturating_sub(plan.description_y());
                let max_text_width = desc_rect
                    .width
                    .saturating_sub(style.description_right_padding_cols)
                    as usize;
                let mut lines = match item.description.as_deref() {
                    Some(d) if !d.is_empty() => wrap_text(d, max_text_width),
                    _ => Vec::new(),
                };
                if let Some(layer) = layer_label {
                    if let Some(last) = lines.last_mut() {
                        last.push_str(&format!(" ({})", layer));
                    } else {
                        lines.push(format!("({})", layer));
                    }
                }
                let desc_style = Style::default().fg(theme.line_number_fg);
                let take = desc_rect.height as usize;
                for (i, line) in lines.iter().skip(desc_skip as usize).take(take).enumerate() {
                    frame.render_widget(
                        Paragraph::new(line.as_str()).style(desc_style),
                        Rect::new(desc_rect.x, desc_rect.y + i as u16, desc_rect.width, 1),
                    );
                }
            }
        } else if let Some(layer) = layer_label {
            // No description, just a layer label on the row immediately
            // below the control.
            if let Some(layer_rect) = band_rect(plan.description_y(), 1).map(|r| {
                let x =
                    r.x.saturating_add(style.card_border_cols + style.focus_indicator_cols);
                let w = r
                    .width
                    .saturating_sub(2 * style.card_border_cols + style.focus_indicator_cols);
                Rect::new(x, r.y, w, r.height)
            }) {
                frame.render_widget(
                    Paragraph::new(format!("({})", layer))
                        .style(Style::default().fg(theme.line_number_fg)),
                    layer_rect,
                );
            }
        }
    }

    SettingItemLayoutInfo {
        control: control_layout,
        inherit_button: inherit_button_area,
    }
}

/// Render the appropriate control for a setting
///
/// # Arguments
/// * `name` - Setting name (for controls that render their own label)
/// * `skip_rows` - Number of rows to skip at top of control (for partial visibility)
/// * `label_width` - Optional label width for column alignment
/// * `read_only` - Whether this field is read-only (displays as plain text instead of input)
#[allow(clippy::too_many_arguments)]
fn render_control(
    frame: &mut Frame,
    area: Rect,
    control: &SettingControl,
    name: &str,
    skip_rows: u16,
    theme: &Theme,
    label_width: Option<u16>,
    read_only: bool,
    is_null: bool,
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
                return ControlLayoutInfo::Dropdown {
                    button_area: Rect::default(),
                    option_areas: Vec::new(),
                    scroll_offset: 0,
                };
            }
            let colors = DropdownColors::from_theme(theme);
            let drop_layout = render_dropdown_aligned(frame, area, state, &colors, label_width);
            ControlLayoutInfo::Dropdown {
                button_area: drop_layout.button_area,
                option_areas: drop_layout.option_areas,
                scroll_offset: drop_layout.scroll_offset,
            }
        }

        SettingControl::Text(state) => {
            if skip_rows > 0 {
                return ControlLayoutInfo::Text(Rect::default());
            }
            if read_only {
                // Truly read-only fields (e.g., Key: in entry dialogs) render as plain text
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
                ControlLayoutInfo::Text(Rect::default())
            } else if is_null {
                // Nullable-null fields render with dimmed brackets to indicate input presence
                let colors = TextInputColors::from_theme_disabled(theme);
                let text_layout =
                    render_text_input_aligned(frame, area, state, &colors, 30, label_width);
                ControlLayoutInfo::Text(text_layout.input_area)
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
                rows: list_layout
                    .rows
                    .iter()
                    .map(|r| (r.index, r.text_area))
                    .collect(),
            }
        }

        SettingControl::DualList(state) => {
            let colors = DualListColors::from_theme(theme);
            let dual_layout = render_dual_list_partial(frame, area, state, &colors, skip_rows);
            ControlLayoutInfo::DualList(dual_layout)
        }

        SettingControl::Map(state) => {
            let colors = MapColors::from_theme(theme);
            let map_layout = render_map_partial(frame, area, state, &colors, 20, skip_rows);
            ControlLayoutInfo::Map {
                entry_rows: map_layout
                    .entry_areas
                    .iter()
                    .map(|e| (e.index, e.row_area))
                    .collect(),
                add_row_area: map_layout.add_row_area,
            }
        }

        SettingControl::ObjectArray(state) => {
            let colors = crate::view::controls::KeybindingListColors {
                label_fg: theme.editor_fg,
                key_fg: theme.help_key_fg,
                action_fg: theme.syntax_function,
                // Use settings colors for focused items in settings UI
                focused_bg: theme.settings_selected_bg,
                focused_fg: theme.settings_selected_fg,
                delete_fg: theme.diagnostic_error_fg,
                add_fg: theme.syntax_string,
            };
            let kb_layout = render_keybinding_list_partial(frame, area, state, &colors, skip_rows);
            ControlLayoutInfo::ObjectArray {
                entry_rows: kb_layout
                    .entry_rects
                    .iter()
                    .map(|&(idx, rect)| (idx, rect))
                    .collect(),
            }
        }

        SettingControl::Json(state) => {
            render_json_control(frame, area, state, name, skip_rows, theme)
        }

        SettingControl::Complex { type_name } => {
            if skip_rows > 0 {
                return ControlLayoutInfo::Complex;
            }
            // Render label (modified indicator is shown in the row indicator column)
            let label_style = Style::default().fg(theme.editor_fg);
            let value_style = Style::default().fg(theme.line_number_fg);

            let label = Span::styled(format!("{}: ", name), label_style);
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

    // Row 0: label (modified indicator is shown in the row indicator column)
    if content_row >= skip_rows {
        let label_line = Line::from(vec![Span::styled(
            format!("{}:", name),
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

    // Use focused_fg for label when focused (not focused, which is the bg color)
    let label_color = match state.focus {
        FocusState::Focused => colors.focused_fg,
        FocusState::Hovered => colors.focused_fg,
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

    // Use focused_fg for label when focused (not focused, which is the bg color)
    let label_color = match state.focus {
        FocusState::Focused => colors.focused_fg,
        FocusState::Hovered => colors.focused_fg,
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

    // Row 1 is column headers (if display_field is set)
    if state.display_field.is_some() && y < area.y + area.height {
        if content_row >= skip_rows {
            // Derive header name from display_field (e.g., "/enabled" -> "Enabled")
            let value_header = state
                .display_field
                .as_ref()
                .map(|f| {
                    let name = f.trim_start_matches('/');
                    // Capitalize first letter
                    let mut chars = name.chars();
                    match chars.next() {
                        None => String::new(),
                        Some(c) => c.to_uppercase().collect::<String>() + chars.as_str(),
                    }
                })
                .unwrap_or_else(|| "Value".to_string());

            let header_style = Style::default()
                .fg(colors.label)
                .add_modifier(Modifier::DIM);
            let header_line = Line::from(vec![
                Span::styled(" ".repeat(indent as usize), header_style),
                Span::styled(
                    format!("{:width$}", "Name", width = key_width as usize),
                    header_style,
                ),
                Span::raw(" "),
                Span::styled(value_header, header_style),
            ]);
            frame.render_widget(
                Paragraph::new(header_line),
                Rect::new(area.x, y, area.width, 1),
            );
            y += 1;
        }
        content_row += 1;
    }

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
            // Use focused_fg for text on the focused background
            (colors.focused_fg, colors.focused_fg)
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

        // Get display value. `truncate_chars_with_ellipsis` counts
        // characters (not bytes) so a localized / CJK preview value
        // doesn't panic on truncation (same class as #1718).
        let value_preview = state.get_display_value(value);
        let value_preview = truncate_chars_with_ellipsis(&value_preview, 20);

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
                base_style.fg(colors.focused_fg).add_modifier(Modifier::DIM),
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
                base_style.fg(colors.focused_fg).add_modifier(Modifier::DIM),
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

    // Render label (row 0) - modified indicator is shown in the row indicator column
    if content_row >= skip_rows {
        let label_line = Line::from(vec![Span::styled(
            format!("{}:", state.label),
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
            entry_rects.push((idx, entry_area));

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
            // Use focused_fg for all text when entry is focused for good contrast
            let (indicator_fg, key_fg, arrow_fg, action_fg, delete_fg) = if is_entry_focused {
                (
                    colors.focused_fg,
                    colors.focused_fg,
                    colors.focused_fg,
                    colors.focused_fg,
                    colors.focused_fg,
                )
            } else {
                (
                    colors.label_fg,
                    colors.key_fg,
                    colors.label_fg,
                    colors.action_fg,
                    colors.delete_fg,
                )
            };
            let line = Line::from(vec![
                Span::styled(indicator, Style::default().fg(indicator_fg).bg(bg)),
                Span::styled(
                    format!("{:<20}", key_combo),
                    Style::default().fg(key_fg).bg(bg),
                ),
                Span::styled(" → ", Style::default().fg(arrow_fg).bg(bg)),
                Span::styled(action, Style::default().fg(action_fg).bg(bg)),
                Span::styled(" [x]", Style::default().fg(delete_fg).bg(bg)),
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
        // Use focused_fg for text when add row is focused
        let (indicator_fg, add_fg) = if is_add_focused {
            (colors.focused_fg, colors.focused_fg)
        } else {
            (colors.label_fg, colors.add_fg)
        };
        let line = Line::from(vec![
            Span::styled(indicator, Style::default().fg(indicator_fg).bg(bg)),
            Span::styled("[+] Add new", Style::default().fg(add_fg).bg(bg)),
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

/// Combined layout info for a setting item (control + inherit button)
#[derive(Debug, Clone, Default)]
pub struct SettingItemLayoutInfo {
    pub control: ControlLayoutInfo,
    pub inherit_button: Option<Rect>,
}

/// Layout info for a control (for hit testing)
#[derive(Debug, Clone, Default)]
pub enum ControlLayoutInfo {
    Toggle(Rect),
    Number {
        decrement: Rect,
        increment: Rect,
        value: Rect,
    },
    Dropdown {
        button_area: Rect,
        option_areas: Vec<Rect>,
        scroll_offset: usize,
    },
    Text(Rect),
    TextList {
        /// (data_index, screen_area) - None index means "add new" row
        rows: Vec<(Option<usize>, Rect)>,
    },
    DualList(crate::view::controls::DualListLayout),
    Map {
        /// (data_index, screen_area)
        entry_rows: Vec<(usize, Rect)>,
        add_row_area: Option<Rect>,
    },
    ObjectArray {
        /// (data_index, screen_area)
        entry_rows: Vec<(usize, Rect)>,
    },
    Json {
        edit_area: Rect,
    },
    #[default]
    Complex,
}

/// Render a single button with focus/hover states
#[allow(clippy::too_many_arguments)]
fn render_button(
    frame: &mut Frame,
    area: Rect,
    text: &str,
    focused_text: &str,
    is_focused: bool,
    is_hovered: bool,
    theme: &Theme,
    dimmed: bool,
) {
    if is_focused {
        let style = Style::default()
            .fg(theme.menu_highlight_fg)
            .bg(theme.menu_highlight_bg)
            .add_modifier(Modifier::BOLD);
        frame.render_widget(Paragraph::new(focused_text).style(style), area);
    } else if is_hovered {
        let style = Style::default()
            .fg(theme.menu_hover_fg)
            .bg(theme.menu_hover_bg);
        frame.render_widget(Paragraph::new(text).style(style), area);
    } else {
        let fg = if dimmed {
            theme.line_number_fg
        } else {
            theme.popup_text_fg
        };
        frame.render_widget(Paragraph::new(text).style(Style::default().fg(fg)), area);
    }
}

/// Render footer with action buttons
/// When `vertical` is true, buttons are stacked vertically (for narrow mode)
fn render_footer(
    frame: &mut Frame,
    modal_area: Rect,
    state: &SettingsState,
    theme: &Theme,
    layout: &mut SettingsLayout,
    vertical: bool,
) {
    use super::layout::SettingsHit;
    use super::state::FocusPanel;

    // Guard against too-small modal
    if modal_area.height < 4 || modal_area.width < 10 {
        return;
    }

    if vertical {
        render_footer_vertical(frame, modal_area, state, theme, layout);
        return;
    }

    let footer_y = modal_area.y + modal_area.height.saturating_sub(2);
    let footer_width = modal_area.width.saturating_sub(2);
    let footer_area = Rect::new(modal_area.x + 1, footer_y, footer_width, 1);

    // Draw separator line (only if we have room above footer)
    if footer_y > modal_area.y {
        let sep_y = footer_y.saturating_sub(1);
        let sep_area = Rect::new(modal_area.x + 1, sep_y, footer_width, 1);
        let sep_line: String = "─".repeat(sep_area.width as usize);
        frame.render_widget(
            Paragraph::new(sep_line).style(Style::default().fg(theme.split_separator_fg)),
            sep_area,
        );
    }

    // Check if footer has keyboard focus
    let footer_focused = state.focus_panel() == FocusPanel::Footer;

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

    // Get translated button labels
    // Use "Inherit" label instead of "Reset" when current item is nullable and explicitly set
    let current_is_nullable_set = state
        .current_item()
        .map(|item| item.nullable && !item.is_null)
        .unwrap_or(false);
    let save_label = t!("settings.btn_save").to_string();
    let cancel_label = t!("settings.btn_cancel").to_string();
    let reset_label = if current_is_nullable_set {
        t!("settings.btn_inherit").to_string()
    } else {
        t!("settings.btn_reset").to_string()
    };
    let edit_label = t!("settings.btn_edit").to_string();

    // Build button text with brackets (layer button uses layer name)
    let layer_text = format!("[ {} ]", state.target_layer_name());
    let layer_text_focused = format!(">[ {} ]", state.target_layer_name());
    let save_text = format!("[ {} ]", save_label);
    let save_text_focused = format!(">[ {} ]", save_label);
    let cancel_text = format!("[ {} ]", cancel_label);
    let cancel_text_focused = format!(">[ {} ]", cancel_label);
    let reset_text = format!("[ {} ]", reset_label);
    let reset_text_focused = format!(">[ {} ]", reset_label);
    let edit_text = format!("[ {} ]", edit_label);
    let edit_text_focused = format!(">[ {} ]", edit_label);

    // Calculate button widths using display width (handles unicode)
    let cancel_width = str_width(if cancel_focused {
        &cancel_text_focused
    } else {
        &cancel_text
    }) as u16;
    let save_width = str_width(if save_focused {
        &save_text_focused
    } else {
        &save_text
    }) as u16;
    let reset_width = str_width(if reset_focused {
        &reset_text_focused
    } else {
        &reset_text
    }) as u16;
    let layer_width = str_width(if layer_focused {
        &layer_text_focused
    } else {
        &layer_text
    }) as u16;
    let edit_width = str_width(if edit_focused {
        &edit_text_focused
    } else {
        &edit_text
    }) as u16;
    let gap: u16 = 2;

    // Calculate total width needed for all buttons
    // Minimum needed: Save + Cancel
    let min_buttons_width = save_width + gap + cancel_width;
    // Full buttons: Edit + Layer + Reset + Save + Cancel with gaps
    let all_buttons_width =
        edit_width + gap + layer_width + gap + reset_width + gap + save_width + gap + cancel_width;

    // Determine which buttons to show based on available width
    let available = footer_area.width;
    let show_edit = available >= all_buttons_width;
    let show_layer = available >= (layer_width + gap + reset_width + gap + min_buttons_width);
    let show_reset = available >= (reset_width + gap + min_buttons_width);

    // Calculate X positions using saturating_sub to prevent overflow
    let cancel_x = footer_area
        .x
        .saturating_add(footer_area.width.saturating_sub(cancel_width));
    let save_x = cancel_x.saturating_sub(save_width + gap);
    let reset_x = if show_reset {
        save_x.saturating_sub(reset_width + gap)
    } else {
        0
    };
    let layer_x = if show_layer {
        reset_x.saturating_sub(layer_width + gap)
    } else {
        0
    };
    let edit_x = footer_area.x; // Left-aligned

    // Render buttons using helper function
    // Layer button (conditionally shown)
    if show_layer {
        let layer_area = Rect::new(layer_x, footer_y, layer_width, 1);
        render_button(
            frame,
            layer_area,
            &layer_text,
            &layer_text_focused,
            layer_focused,
            layer_hovered,
            theme,
            false,
        );
        layout.layer_button = Some(layer_area);
    }

    // Reset button (conditionally shown)
    if show_reset {
        let reset_area = Rect::new(reset_x, footer_y, reset_width, 1);
        render_button(
            frame,
            reset_area,
            &reset_text,
            &reset_text_focused,
            reset_focused,
            reset_hovered,
            theme,
            false,
        );
        layout.reset_button = Some(reset_area);
    }

    // Save button (always shown)
    let save_area = Rect::new(save_x, footer_y, save_width, 1);
    render_button(
        frame,
        save_area,
        &save_text,
        &save_text_focused,
        save_focused,
        save_hovered,
        theme,
        false,
    );
    layout.save_button = Some(save_area);

    // Cancel button (always shown)
    let cancel_area = Rect::new(cancel_x, footer_y, cancel_width, 1);
    render_button(
        frame,
        cancel_area,
        &cancel_text,
        &cancel_text_focused,
        cancel_focused,
        cancel_hovered,
        theme,
        false,
    );
    layout.cancel_button = Some(cancel_area);

    // Edit button (on left, for advanced users, conditionally shown)
    if show_edit {
        let edit_area = Rect::new(edit_x, footer_y, edit_width, 1);
        render_button(
            frame,
            edit_area,
            &edit_text,
            &edit_text_focused,
            edit_focused,
            edit_hovered,
            theme,
            true, // dimmed for advanced option
        );
        layout.edit_button = Some(edit_area);
    }

    // Help text (between Edit button and main buttons)
    // Calculate position based on which buttons are visible
    let help_start_x = if show_edit {
        edit_x + edit_width + 2
    } else {
        footer_area.x
    };
    let help_end_x = if show_layer {
        layer_x
    } else if show_reset {
        reset_x
    } else {
        save_x
    };
    let help_width = help_end_x.saturating_sub(help_start_x + 1);

    // Get translated help text
    let help = if state.search_active {
        t!("settings.help_search").to_string()
    } else if footer_focused {
        t!("settings.help_footer").to_string()
    } else {
        t!("settings.help_default").to_string()
    };
    // Render help text with reverse-video styling for key hints
    // Parse "Key:Action  Key:Action" format
    let help_line = build_keyhint_line(&help, theme);
    frame.render_widget(
        Paragraph::new(help_line),
        Rect::new(help_start_x, footer_y, help_width, 1),
    );
}

/// Build a Line with reverse-video styled key hints from "Key:Action  Key:Action" format
fn build_keyhint_line<'a>(text: &str, theme: &Theme) -> Line<'a> {
    let key_style = Style::default()
        .fg(theme.popup_text_fg)
        .bg(theme.split_separator_fg);
    let desc_style = Style::default().fg(theme.line_number_fg);
    let sep_style = Style::default().fg(theme.line_number_fg);

    let mut spans: Vec<Span<'a>> = Vec::new();

    // Split by double-space to get individual key hints
    for (i, segment) in text.split("  ").enumerate() {
        let segment = segment.trim();
        if segment.is_empty() {
            continue;
        }
        if i > 0 {
            spans.push(Span::styled(" ", sep_style));
        }
        // Split by first ":" to separate key from description
        if let Some(colon_pos) = segment.find(':') {
            let key = &segment[..colon_pos];
            let action = &segment[colon_pos + 1..];
            spans.push(Span::styled(format!(" {} ", key), key_style));
            spans.push(Span::styled(action.to_string(), desc_style));
        } else {
            // No colon - just render as text
            spans.push(Span::styled(segment.to_string(), desc_style));
        }
    }

    Line::from(spans)
}

/// Render footer with buttons stacked vertically (for narrow mode)
fn render_footer_vertical(
    frame: &mut Frame,
    modal_area: Rect,
    state: &SettingsState,
    theme: &Theme,
    layout: &mut SettingsLayout,
) {
    use super::layout::SettingsHit;
    use super::state::FocusPanel;

    // Footer takes bottom 7 lines: separator + 5 buttons + help
    let footer_height = 7u16;
    let footer_y = modal_area
        .y
        .saturating_add(modal_area.height.saturating_sub(footer_height));
    let footer_width = modal_area.width.saturating_sub(2);

    // Draw top separator
    let sep_y = footer_y;
    if sep_y > modal_area.y {
        let sep_line: String = "─".repeat(footer_width as usize);
        frame.render_widget(
            Paragraph::new(sep_line).style(Style::default().fg(theme.split_separator_fg)),
            Rect::new(modal_area.x + 1, sep_y, footer_width, 1),
        );
    }

    // Check if footer has keyboard focus
    let footer_focused = state.focus_panel() == FocusPanel::Footer;

    // Determine hover and keyboard focus states for buttons
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

    // Get translated button labels
    // Use "Inherit" label instead of "Reset" when current item is nullable and explicitly set
    let current_is_nullable_set = state
        .current_item()
        .map(|item| item.nullable && !item.is_null)
        .unwrap_or(false);
    let save_label = t!("settings.btn_save").to_string();
    let cancel_label = t!("settings.btn_cancel").to_string();
    let reset_label = if current_is_nullable_set {
        t!("settings.btn_inherit").to_string()
    } else {
        t!("settings.btn_reset").to_string()
    };
    let edit_label = t!("settings.btn_edit").to_string();

    // Build button text
    let layer_text = format!("[ {} ]", state.target_layer_name());
    let layer_text_focused = format!(">[ {} ]", state.target_layer_name());
    let save_text = format!("[ {} ]", save_label);
    let save_text_focused = format!(">[ {} ]", save_label);
    let cancel_text = format!("[ {} ]", cancel_label);
    let cancel_text_focused = format!(">[ {} ]", cancel_label);
    let reset_text = format!("[ {} ]", reset_label);
    let reset_text_focused = format!(">[ {} ]", reset_label);
    let edit_text = format!("[ {} ]", edit_label);
    let edit_text_focused = format!(">[ {} ]", edit_label);

    // Render buttons vertically, centered
    let button_x = modal_area.x + 2;
    let mut y = sep_y + 1;

    // Layer button
    let layer_width = str_width(if layer_focused {
        &layer_text_focused
    } else {
        &layer_text
    }) as u16;
    let layer_area = Rect::new(button_x, y, layer_width.min(footer_width), 1);
    render_button(
        frame,
        layer_area,
        &layer_text,
        &layer_text_focused,
        layer_focused,
        layer_hovered,
        theme,
        false,
    );
    layout.layer_button = Some(layer_area);
    y += 1;

    // Save button
    let save_width = str_width(if save_focused {
        &save_text_focused
    } else {
        &save_text
    }) as u16;
    let save_area = Rect::new(button_x, y, save_width.min(footer_width), 1);
    render_button(
        frame,
        save_area,
        &save_text,
        &save_text_focused,
        save_focused,
        save_hovered,
        theme,
        false,
    );
    layout.save_button = Some(save_area);
    y += 1;

    // Reset button
    let reset_width = str_width(if reset_focused {
        &reset_text_focused
    } else {
        &reset_text
    }) as u16;
    let reset_area = Rect::new(button_x, y, reset_width.min(footer_width), 1);
    render_button(
        frame,
        reset_area,
        &reset_text,
        &reset_text_focused,
        reset_focused,
        reset_hovered,
        theme,
        false,
    );
    layout.reset_button = Some(reset_area);
    y += 1;

    // Cancel button
    let cancel_width = str_width(if cancel_focused {
        &cancel_text_focused
    } else {
        &cancel_text
    }) as u16;
    let cancel_area = Rect::new(button_x, y, cancel_width.min(footer_width), 1);
    render_button(
        frame,
        cancel_area,
        &cancel_text,
        &cancel_text_focused,
        cancel_focused,
        cancel_hovered,
        theme,
        false,
    );
    layout.cancel_button = Some(cancel_area);
    y += 1;

    // Edit button
    let edit_width = str_width(if edit_focused {
        &edit_text_focused
    } else {
        &edit_text
    }) as u16;
    let edit_area = Rect::new(button_x, y, edit_width.min(footer_width), 1);
    render_button(
        frame,
        edit_area,
        &edit_text,
        &edit_text_focused,
        edit_focused,
        edit_hovered,
        theme,
        true, // dimmed
    );
    layout.edit_button = Some(edit_area);
}

/// Render the search header with query input
fn render_search_header(frame: &mut Frame, area: Rect, state: &SettingsState, theme: &Theme) {
    let search_style = Style::default().fg(theme.settings_selected_fg);
    let cursor_style = Style::default()
        .fg(theme.settings_selected_fg)
        .add_modifier(Modifier::REVERSED);

    // Show result count and scroll position inline after cursor
    let result_count = state.search_results.len();
    let count_text = if state.search_query.is_empty() {
        String::new()
    } else if result_count == 0 {
        " (no results)".to_string()
    } else if result_count == 1 {
        " (1 result)".to_string()
    } else if state.search_max_visible >= result_count {
        // All results visible, no need to show range
        format!(" ({} results)", result_count)
    } else {
        // Show current position in results
        let first = state.search_scroll_offset + 1;
        let last = (state.search_scroll_offset + state.search_max_visible).min(result_count);
        format!(" ({}-{} of {})", first, last, result_count)
    };

    // Add scroll indicators
    let has_more_above = state.search_scroll_offset > 0;
    let has_more_below = state.search_scroll_offset + state.search_max_visible < result_count;
    let scroll_indicator = match (has_more_above, has_more_below) {
        (true, true) => " ↑↓",
        (true, false) => " ↑",
        (false, true) => " ↓",
        (false, false) => "",
    };

    let count_style = Style::default().fg(theme.line_number_fg);
    let indicator_style = Style::default()
        .fg(theme.menu_active_fg)
        .add_modifier(Modifier::BOLD);

    let spans = vec![
        Span::styled("> ", search_style),
        Span::styled(&state.search_query, search_style),
        Span::styled(" ", cursor_style), // Cursor
        Span::styled(count_text, count_style),
        Span::styled(scroll_indicator, indicator_style),
    ];
    let line = Line::from(spans);
    frame.render_widget(Paragraph::new(line), area);
}

/// Render search hint when search is not active
fn render_search_hint(frame: &mut Frame, area: Rect, theme: &Theme) {
    let hint_style = Style::default().fg(theme.line_number_fg);
    let key_style = Style::default()
        .fg(theme.popup_text_fg)
        .bg(theme.split_separator_fg);

    let spans = vec![
        Span::styled("Press ", hint_style),
        Span::styled(" / ", key_style),
        Span::styled(" to search settings...", hint_style),
    ];
    let line = Line::from(spans);
    frame.render_widget(Paragraph::new(line), area);
}

/// Render search results with breadcrumbs
fn render_search_results(
    frame: &mut Frame,
    area: Rect,
    state: &mut SettingsState,
    theme: &Theme,
    layout: &mut SettingsLayout,
) {
    // Calculate max visible results (each result is 3 rows tall)
    let max_visible = (area.height.saturating_sub(3) / 3) as usize;
    state.search_max_visible = max_visible.max(1);

    // Ensure scroll offset is valid
    if state.search_scroll_offset >= state.search_results.len() {
        state.search_scroll_offset = state.search_results.len().saturating_sub(1);
    }

    // Determine if we need a scrollbar
    let needs_scrollbar = state.search_results.len() > state.search_max_visible;
    let scrollbar_width = if needs_scrollbar { 1 } else { 0 };

    // Reserve space for scrollbar on the right
    let content_area = Rect::new(
        area.x,
        area.y,
        area.width.saturating_sub(scrollbar_width),
        area.height,
    );

    let mut y = content_area.y;

    for (idx, result) in state
        .search_results
        .iter()
        .enumerate()
        .skip(state.search_scroll_offset)
    {
        if y >= content_area.y + content_area.height.saturating_sub(3) {
            break;
        }

        let is_selected = idx == state.selected_search_result;
        let is_hovered = matches!(state.hover_hit, Some(SettingsHit::SearchResult(i)) if i == idx);
        let item_area = Rect::new(content_area.x, y, content_area.width, 3);

        render_search_result_item(
            frame,
            item_area,
            result,
            is_selected,
            is_hovered,
            theme,
            layout,
        );
        y += 3;
    }

    // Track search results area in layout for mouse wheel support
    layout.search_results_area = Some(content_area);

    // Render scrollbar if needed
    if needs_scrollbar {
        let scrollbar_area = Rect::new(
            area.x + area.width - 1,
            area.y,
            1,
            area.height.saturating_sub(3), // Leave space at bottom
        );

        let scrollbar_state = ScrollbarState::new(
            state.search_results.len(),
            state.search_max_visible,
            state.search_scroll_offset,
        );

        let colors = ScrollbarColors::from_theme(theme);
        render_scrollbar(frame, scrollbar_area, &scrollbar_state, &colors);

        // Track scrollbar area in layout for click/drag support
        layout.search_scrollbar_area = Some(scrollbar_area);
    } else {
        layout.search_scrollbar_area = None;
    }
}

/// Render a single search result with breadcrumb
fn render_search_result_item(
    frame: &mut Frame,
    area: Rect,
    result: &SearchResult,
    is_selected: bool,
    is_hovered: bool,
    theme: &Theme,
    layout: &mut SettingsLayout,
) {
    // Draw selection or hover highlight background
    if is_selected {
        // Use dedicated settings colors for selected items
        let bg_style = Style::default().bg(theme.settings_selected_bg);
        for row in 0..area.height.min(3) {
            let row_area = Rect::new(area.x, area.y + row, area.width, 1);
            frame.render_widget(Paragraph::new("").style(bg_style), row_area);
        }
    } else if is_hovered {
        // Subtle hover highlight using menu hover colors
        let bg_style = Style::default().bg(theme.menu_hover_bg);
        for row in 0..area.height.min(3) {
            let row_area = Rect::new(area.x, area.y + row, area.width, 1);
            frame.render_widget(Paragraph::new("").style(bg_style), row_area);
        }
    }

    // Determine display name and description based on deep match
    let (display_name, display_desc) = match &result.deep_match {
        Some(DeepMatch::MapKey { key, .. }) => (key.clone(), Some(result.item.name.clone())),
        Some(DeepMatch::MapValue {
            matched_text, key, ..
        }) => (
            matched_text.clone(),
            Some(format!("{} > {}", result.item.name, key)),
        ),
        Some(DeepMatch::TextListItem { text, .. }) => {
            (text.clone(), Some(result.item.name.clone()))
        }
        None => (result.item.name.clone(), result.item.description.clone()),
    };

    // First line: Setting name with highlighting
    let name_style = if is_selected {
        Style::default().fg(theme.settings_selected_fg)
    } else if is_hovered {
        Style::default().fg(theme.menu_hover_fg)
    } else {
        Style::default().fg(theme.popup_text_fg)
    };

    // Build name with match highlighting, prefixed with selection indicator
    let indicator = if is_selected { "▸ " } else { "  " };
    let indicator_style = if is_selected {
        Style::default()
            .fg(theme.settings_selected_fg)
            .add_modifier(Modifier::BOLD)
    } else {
        name_style
    };
    let mut name_line = build_highlighted_text(
        &display_name,
        &result.name_matches,
        name_style,
        Style::default()
            .fg(theme.diagnostic_warning_fg)
            .add_modifier(Modifier::BOLD),
    );
    name_line
        .spans
        .insert(0, Span::styled(indicator, indicator_style));
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

    // Third line: Description (if any). Counts characters (not bytes)
    // when checking and truncating: descriptions can be localized (e.g.
    // CJK translations) and a byte-based slice could land inside a
    // multi-byte UTF-8 sequence and panic — same class as #1718.
    if let Some(ref desc) = display_desc {
        let desc_style = Style::default().fg(theme.line_number_fg);
        let max_chars = (area.width as usize).saturating_sub(2);
        let truncated_desc = format!("  {}", truncate_chars_with_ellipsis(desc, max_chars));
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
        .border_type(BorderType::Rounded)
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

    // List changes. Character-based truncation here (rather than byte
    // truncation) keeps CJK / emoji change descriptions from byte-slicing
    // through a multi-byte UTF-8 sequence and panicking — same class as
    // #1718.
    let change_style = Style::default().fg(theme.popup_text_fg);
    for change in changes
        .iter()
        .take((dialog_height as usize).saturating_sub(7))
    {
        let max_chars = (inner.width as usize).saturating_sub(2);
        let truncated = format!("• {}", truncate_chars_with_ellipsis(change, max_chars));
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
        let is_hovered = state.confirm_dialog_hover == Some(idx);
        let button_width = label.len() as u16 + 4;

        let style = if is_selected {
            Style::default()
                .fg(theme.menu_highlight_fg)
                .bg(theme.menu_highlight_bg)
                .add_modifier(ratatui::style::Modifier::BOLD)
        } else if is_hovered {
            Style::default()
                .fg(theme.menu_hover_fg)
                .bg(theme.menu_hover_bg)
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
    let help = "←/→/Tab: Select   Enter: Confirm   Esc: Cancel";
    let help_style = Style::default().fg(theme.line_number_fg);
    frame.render_widget(
        Paragraph::new(help).style(help_style),
        Rect::new(inner.x, button_y + 1, inner.width, 1),
    );
}

/// Render the reset confirmation dialog
fn render_reset_dialog(frame: &mut Frame, parent_area: Rect, state: &SettingsState, theme: &Theme) {
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
        .title(" Reset All Changes ")
        .borders(Borders::ALL)
        .border_type(BorderType::Rounded)
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
    let prompt_style = Style::default().fg(theme.popup_text_fg);
    frame.render_widget(
        Paragraph::new("Discard all pending changes?").style(prompt_style),
        Rect::new(inner.x, y, inner.width, 1),
    );
    y += 2;

    // List changes. Character-based truncation here (rather than byte
    // truncation) keeps CJK / emoji change descriptions from byte-slicing
    // through a multi-byte UTF-8 sequence and panicking — same class as
    // #1718.
    let change_style = Style::default().fg(theme.popup_text_fg);
    for change in changes
        .iter()
        .take((dialog_height as usize).saturating_sub(7))
    {
        let max_chars = (inner.width as usize).saturating_sub(2);
        let truncated = format!("• {}", truncate_chars_with_ellipsis(change, max_chars));
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

    // Render the two options: Reset, Cancel
    let options = ["Reset", "Cancel"];
    let total_width: u16 = options.iter().map(|o| o.len() as u16 + 4).sum::<u16>() + 4;
    let mut x = inner.x + (inner.width.saturating_sub(total_width)) / 2;

    for (idx, label) in options.iter().enumerate() {
        let is_selected = idx == state.reset_dialog_selection;
        let is_hovered = state.reset_dialog_hover == Some(idx);
        let button_width = label.len() as u16 + 4;

        let style = if is_selected {
            Style::default()
                .fg(theme.menu_highlight_fg)
                .bg(theme.menu_highlight_bg)
                .add_modifier(ratatui::style::Modifier::BOLD)
        } else if is_hovered {
            Style::default()
                .fg(theme.menu_hover_fg)
                .bg(theme.menu_hover_bg)
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
    let help = "←/→/Tab: Select   Enter: Confirm   Esc: Cancel";
    let help_style = Style::default().fg(theme.line_number_fg);
    frame.render_widget(
        Paragraph::new(help).style(help_style),
        Rect::new(inner.x, button_y + 1, inner.width, 1),
    );
}

/// Render the "Discard changes?" prompt that appears when the user
/// presses Esc on a dirty entry dialog.
fn render_entry_discard_confirm(
    frame: &mut Frame,
    parent_area: Rect,
    state: &SettingsState,
    theme: &Theme,
) {
    let dialog_width = 50.min(parent_area.width.saturating_sub(4));
    let dialog_height = 7u16.min(parent_area.height.saturating_sub(4));
    let dialog_x = parent_area.x + (parent_area.width.saturating_sub(dialog_width)) / 2;
    let dialog_y = parent_area.y + (parent_area.height.saturating_sub(dialog_height)) / 2;
    let dialog_area = Rect::new(dialog_x, dialog_y, dialog_width, dialog_height);

    frame.render_widget(Clear, dialog_area);

    let block = Block::default()
        .title(" Discard changes? ")
        .borders(Borders::ALL)
        .border_type(BorderType::Rounded)
        .border_style(Style::default().fg(theme.diagnostic_warning_fg))
        .style(Style::default().bg(theme.popup_bg));
    frame.render_widget(block, dialog_area);

    let inner = Rect::new(
        dialog_area.x + 2,
        dialog_area.y + 1,
        dialog_area.width.saturating_sub(4),
        dialog_area.height.saturating_sub(2),
    );

    let prompt_style = Style::default().fg(theme.popup_text_fg);
    frame.render_widget(
        Paragraph::new("You have uncommitted edits in this dialog.").style(prompt_style),
        Rect::new(inner.x, inner.y, inner.width, 1),
    );

    // Buttons. 0 = Keep editing (default), 1 = Discard. Discard styled
    // in the danger fg to make the destructive choice unmistakable.
    let button_y = dialog_area.y + dialog_area.height - 3;
    let options = ["Keep editing", "Discard"];
    let total_width: u16 = options.iter().map(|o| o.len() as u16 + 4).sum::<u16>() + 4;
    let mut x = inner.x + (inner.width.saturating_sub(total_width)) / 2;

    for (idx, label) in options.iter().enumerate() {
        let is_selected = idx == state.entry_discard_confirm_selection;
        let is_discard = idx == 1;
        let style = if is_selected && is_discard {
            Style::default()
                .fg(theme.diagnostic_error_fg)
                .bg(theme.popup_selection_bg)
                .add_modifier(Modifier::BOLD)
        } else if is_selected {
            Style::default()
                .fg(theme.popup_selection_fg)
                .bg(theme.popup_selection_bg)
                .add_modifier(Modifier::BOLD)
        } else if is_discard {
            Style::default()
                .fg(theme.diagnostic_error_fg)
                .add_modifier(Modifier::BOLD)
        } else {
            Style::default().fg(theme.popup_text_fg)
        };
        let text = if is_selected {
            format!(">[ {} ]", label)
        } else {
            format!(" [ {} ]", label)
        };
        let w = label.len() as u16 + 5;
        frame.render_widget(
            Paragraph::new(text).style(style),
            Rect::new(x, button_y, w, 1),
        );
        x += w + 2;
    }

    let help = "Tab/←→: Select   Enter: Confirm   Esc: Keep editing";
    let help_style = Style::default().fg(theme.line_number_fg);
    frame.render_widget(
        Paragraph::new(help).style(help_style),
        Rect::new(inner.x, button_y + 1, inner.width, 1),
    );
}

/// Render a specific entry dialog from the stack by index.
fn render_entry_dialog_at(
    frame: &mut Frame,
    parent_area: Rect,
    state: &mut SettingsState,
    theme: &Theme,
    dialog_idx: usize,
) {
    let Some(dialog) = state.entry_dialog_stack.get_mut(dialog_idx) else {
        return;
    };
    render_entry_dialog_inner(frame, parent_area, dialog, theme);
}

/// Render the entry detail dialog for editing Language/LSP/Keybinding entries
///
/// Now uses the same SettingItem/SettingControl infrastructure as the main settings UI,
/// eliminating duplication and ensuring consistent rendering.
fn render_entry_dialog_inner(
    frame: &mut Frame,
    parent_area: Rect,
    dialog: &mut super::entry_dialog::EntryDialogState,
    theme: &Theme,
) {
    // Calculate dialog size - use most of available space for editing
    let dialog_width = (parent_area.width * 85 / 100).clamp(50, 90);
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
        .border_type(BorderType::Rounded)
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

    // Track if we need to render a separator (between read-only and editable items)
    let first_editable = dialog.first_editable_index;
    let has_readonly_items = first_editable > 0;
    let has_editable_items = first_editable < dialog.items.len();
    let needs_separator = has_readonly_items && has_editable_items;

    for (idx, item) in dialog.items.iter().enumerate() {
        // Render separator before first editable item
        if needs_separator && idx == first_editable {
            // Add separator row to content height calculation
            let separator_start = content_y;
            let separator_end = content_y + 1;

            if separator_end > scroll_offset && screen_y < inner.y + inner.height {
                // Separator is visible
                let skip_sep = if separator_start < scroll_offset {
                    1
                } else {
                    0
                };
                if skip_sep == 0 {
                    let sep_style = Style::default().fg(theme.line_number_fg);
                    let separator_line = "─".repeat(inner.width.saturating_sub(2) as usize);
                    frame.render_widget(
                        Paragraph::new(separator_line).style(sep_style),
                        Rect::new(inner.x + 1, screen_y, inner.width.saturating_sub(2), 1),
                    );
                    screen_y += 1;
                }
            }
            content_y = separator_end;
        }

        // Render section header if this is the first item in a section
        if item.is_section_start {
            if let Some(ref section_name) = item.section {
                let header_start = content_y;
                let header_end = content_y + 2; // 2 lines: label + separator

                if header_end > scroll_offset && screen_y < inner.y + inner.height {
                    let skip_h = if header_start < scroll_offset {
                        (scroll_offset - header_start) as u16
                    } else {
                        0
                    };
                    if skip_h == 0 {
                        // Section label
                        let section_style = Style::default()
                            .fg(theme.line_number_fg)
                            .add_modifier(Modifier::BOLD);
                        frame.render_widget(
                            Paragraph::new(format!("── {} ──", section_name)).style(section_style),
                            Rect::new(inner.x + 1, screen_y, inner.width.saturating_sub(2), 1),
                        );
                        screen_y += 1;
                    }
                    if skip_h <= 1 && screen_y < inner.y + inner.height {
                        // Blank line after section header
                        screen_y += 1;
                    }
                }
                content_y = header_end;
            }
        }

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

        // Read-only items are not focusable - no focus/hover highlighting
        let is_readonly = item.read_only;
        let is_focused = !is_readonly && !dialog.focus_on_buttons && dialog.selected_item == idx;
        let is_hovered = !is_readonly && dialog.hover_item == Some(idx);

        // Draw selection or hover highlight background (only for editable items)
        if is_focused || is_hovered {
            let bg_style = if is_focused {
                Style::default().bg(theme.settings_selected_bg)
            } else {
                Style::default().bg(theme.menu_hover_bg)
            };

            if item.control.is_composite() {
                // For composite controls, only highlight the focused sub-row
                let sub_row = item.control.focused_sub_row();
                if sub_row >= skip_rows && (sub_row - skip_rows) < render_height as u16 {
                    let highlight_y = screen_y + sub_row - skip_rows;
                    let row_area = Rect::new(inner.x, highlight_y, inner.width, 1);
                    frame.render_widget(Paragraph::new("").style(bg_style), row_area);
                }
            } else {
                // For simple controls, highlight the entire area
                for row in 0..render_height as u16 {
                    let row_area = Rect::new(inner.x, screen_y + row, inner.width, 1);
                    frame.render_widget(Paragraph::new("").style(bg_style), row_area);
                }
            }
        }

        // Indicator area takes 3 chars: [>][●][ ] -> focus, modified, separator
        // Examples: ">● ", ">  ", " ● ", "   "
        let focus_indicator_width: u16 = 3;

        // Render focus indicator ">" — on sub-row for composites, first row for simple controls
        if is_focused && skip_rows == 0 {
            let indicator_style = Style::default()
                .fg(theme.settings_selected_fg)
                .add_modifier(Modifier::BOLD);

            let indicator_y = if item.control.is_composite() {
                let sub_row = item.control.focused_sub_row();
                if sub_row < render_height as u16 {
                    screen_y + sub_row
                } else {
                    screen_y
                }
            } else {
                screen_y
            };

            frame.render_widget(
                Paragraph::new(">").style(indicator_style),
                Rect::new(inner.x, indicator_y, 1, 1),
            );
        } else if is_focused && skip_rows > 0 {
            // If the item is partially scrolled, check if the focused sub-row is visible
            if item.control.is_composite() {
                let sub_row = item.control.focused_sub_row();
                if sub_row >= skip_rows && (sub_row - skip_rows) < render_height as u16 {
                    let indicator_style = Style::default()
                        .fg(theme.settings_selected_fg)
                        .add_modifier(Modifier::BOLD);
                    let indicator_y = screen_y + sub_row - skip_rows;
                    frame.render_widget(
                        Paragraph::new(">").style(indicator_style),
                        Rect::new(inner.x, indicator_y, 1, 1),
                    );
                }
            }
        }

        // Render modified indicator "●" at position 1 for modified items
        if item.modified && skip_rows == 0 {
            let modified_style = Style::default().fg(theme.settings_selected_fg);
            frame.render_widget(
                Paragraph::new("●").style(modified_style),
                Rect::new(inner.x + 1, screen_y, 1, 1),
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
            skip_rows,
            theme,
            Some(label_col_width.saturating_sub(focus_indicator_width)),
            item.read_only,
            item.is_null,
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

    // Render buttons at bottom.
    //
    // Order is [Save, Cancel, Delete] — Save and Cancel are the
    // non-destructive pair the user reaches first via Tab, with
    // Delete pushed to the far right (with extra spacing) so the
    // destructive action is impossible to hit by accidentally
    // tabbing one too many times. Delete keeps its red Danger
    // styling whether focused or not.
    let button_y = dialog_area.y + dialog_area.height - 2;
    let has_delete = !dialog.is_new && !dialog.no_delete;
    let buttons: Vec<&str> = if has_delete {
        vec!["[ Save ]", "[ Cancel ]", "[ Delete ]"]
    } else {
        vec!["[ Save ]", "[ Cancel ]"]
    };
    // Index of Delete (last entry, when present). Used for the
    // visual gap between safe buttons and Delete.
    let delete_idx = if has_delete {
        Some(buttons.len() - 1)
    } else {
        None
    };
    const BUTTON_GAP: u16 = 2;
    const DELETE_GAP: u16 = 6;
    let button_width: u16 = buttons
        .iter()
        .enumerate()
        .map(|(i, b)| {
            let gap = if Some(i) == delete_idx {
                DELETE_GAP
            } else if i == 0 {
                0
            } else {
                BUTTON_GAP
            };
            b.len() as u16 + gap
        })
        .sum();
    let button_x = dialog_area.x + (dialog_area.width.saturating_sub(button_width)) / 2;

    let mut x = button_x;
    for (idx, label) in buttons.iter().enumerate() {
        let is_selected = dialog.focus_on_buttons && dialog.focused_button == idx;
        let is_hovered = dialog.hover_button == Some(idx);
        let is_delete = Some(idx) == delete_idx;
        // Apply the inter-button gap before this button (except the
        // first one). Delete gets a wider gap so it stands apart.
        if idx > 0 {
            let gap = if is_delete { DELETE_GAP } else { BUTTON_GAP };
            x += gap;
        }
        // Render ">" focus indicator before selected button
        if is_selected {
            let indicator_style = Style::default()
                .fg(theme.settings_selected_fg)
                .add_modifier(Modifier::BOLD);
            frame.render_widget(
                Paragraph::new(">").style(indicator_style),
                Rect::new(x.saturating_sub(2), button_y, 1, 1),
            );
        }
        let style = if is_selected && is_delete {
            // Selected Delete: keep the red fg as a "this is still a
            // destructive action" cue, but add a focused bg so the
            // user can still see Tab landed here.
            Style::default()
                .fg(theme.diagnostic_error_fg)
                .bg(theme.popup_selection_bg)
                .add_modifier(Modifier::BOLD)
        } else if is_selected {
            Style::default()
                .fg(theme.popup_selection_fg)
                .bg(theme.popup_selection_bg)
                .add_modifier(Modifier::BOLD)
        } else if is_hovered && is_delete {
            Style::default()
                .fg(theme.diagnostic_error_fg)
                .bg(theme.menu_hover_bg)
                .add_modifier(Modifier::BOLD)
        } else if is_hovered {
            Style::default()
                .fg(theme.menu_hover_fg)
                .bg(theme.menu_hover_bg)
        } else if is_delete {
            Style::default()
                .fg(theme.diagnostic_error_fg)
                .add_modifier(Modifier::BOLD)
        } else {
            Style::default().fg(theme.editor_fg)
        };
        frame.render_widget(
            Paragraph::new(*label).style(style),
            Rect::new(x, button_y, label.len() as u16, 1),
        );
        x += label.len() as u16;
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
        let help = "↑↓:Navigate  Tab:Fields/Buttons  Enter:Edit  Ctrl+S:Save  Esc:Cancel";
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
        .border_type(BorderType::Rounded)
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
                .fg(theme.popup_text_fg)
                .bg(theme.split_separator_fg);
            let desc_style = Style::default().fg(theme.popup_text_fg);

            let line = Line::from(vec![
                Span::styled("  ", Style::default()),
                Span::styled(format!(" {} ", key), key_style),
                Span::styled(format!("  {}", description), desc_style),
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

    #[test]
    fn truncate_chars_with_ellipsis_ascii_fits() {
        assert_eq!(truncate_chars_with_ellipsis("hi", 10), "hi");
    }

    #[test]
    fn truncate_chars_with_ellipsis_ascii_truncates() {
        assert_eq!(truncate_chars_with_ellipsis("hello world!", 8), "hello...");
    }

    #[test]
    fn truncate_chars_with_ellipsis_multibyte_does_not_panic() {
        // Regression: byte-slicing this string at `max - 3` would land
        // inside the 3-byte UTF-8 sequence for `こ` and panic — same class
        // as #1718.
        let out = truncate_chars_with_ellipsis("こんにちは世界からのテスト", 8);
        assert!(out.ends_with("..."));
        // 5 kept chars + 3 ellipsis chars = 8 total chars.
        assert_eq!(out.chars().count(), 8);
    }

    #[test]
    fn truncate_chars_with_ellipsis_emoji_does_not_panic() {
        let out = truncate_chars_with_ellipsis("📦📦📦📦📦📦📦📦", 5);
        assert!(out.ends_with("..."));
        assert_eq!(out.chars().count(), 5);
    }

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
