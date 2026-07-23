//! Settings UI renderer
//!
//! Renders the settings modal with category navigation and setting controls.

use rust_i18n::t;

use crate::primitives::display_width::{char_width, str_width};

use super::entry_dialog::EntryDialogState;
use super::items::{ItemBox, ItemBoxStyle, SettingControl, SettingItem};
use super::layout::{SettingsHit, SettingsLayout};
use super::search::{DeepMatch, SearchResult};
use super::state::SettingsState;
use crate::view::theme::Theme;
use crate::view::ui::scrollbar::{render_scrollbar, ScrollbarColors, ScrollbarState};
use ratatui::layout::{Constraint, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, BorderType, Borders, Clear, Paragraph};
use ratatui::Frame;

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

/// Truncate `s` to at most `max_width` terminal columns, appending `"..."`.
///
/// Unlike [`truncate_chars_with_ellipsis`], this is display-width aware so a
/// CJK or emoji label cannot overflow a fixed-width TUI cell after truncation.
fn truncate_display_width_with_ellipsis(s: &str, max_width: usize) -> String {
    if str_width(s) <= max_width {
        return s.to_string();
    }
    if max_width == 0 {
        return String::new();
    }

    let ellipsis = "...";
    let ellipsis_width = str_width(ellipsis);
    if max_width <= ellipsis_width {
        return ".".repeat(max_width);
    }

    let target_width = max_width - ellipsis_width;
    let mut kept = String::new();
    let mut used = 0usize;
    for ch in s.chars() {
        let width = char_width(ch);
        if used + width > target_width {
            break;
        }
        kept.push(ch);
        used += width;
    }
    kept.push_str(ellipsis);
    kept
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
    // Offsets must be ABSOLUTE — `area.x` / `area.y` are nonzero when
    // `area` is the chrome region right of the dock (or a bottom-anchored
    // split). Centring with bare `area.width / 2` placed the modal at the
    // FRAME origin, where the dock then over-drew its left edge — hiding
    // the title bar and clipping the rounded top-left corner.
    let modal_x = area.x + (area.width.saturating_sub(modal_width)) / 2;
    let modal_y = area.y + (area.height.saturating_sub(modal_height)) / 2;

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

    // Render entry-dialog delete-confirm prompt on top of everything
    // else. Same chrome as the discard prompt but worded for
    // destructive action.
    if state.showing_entry_delete_confirm {
        crate::view::dimming::apply_dimming(frame, modal_area);
        render_entry_delete_confirm(frame, modal_area, state, theme);
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
        let has_modified = state.page_has_pending_changes(i);

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

/// Get an icon for a settings category name.
///
/// Two sets are available. The Nerd Font set uses private-use-area
/// codepoints that require a patched "Nerd Font" in the terminal — PUA
/// glyphs have no system-font fallback, so on any other font they
/// render as `?` or empty boxes (issue #2032). The default set uses
/// standard BMP codepoints (default text presentation, width 1) from
/// the same compatibility class as the `▶`/`✓`/`●` glyphs the UI
/// already relies on, so terminal font fallback can always supply
/// them. The Nerd Font set is used only when `editor.nerd_font_icons`
/// is enabled.
fn category_icon(name: &str, nerd_fonts: bool) -> &'static str {
    let name = name.to_lowercase();
    if nerd_fonts {
        return match name.as_str() {
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
        };
    }
    if name.starts_with("plugin: ") {
        return "\u{271a} "; // ✚ heavy plus (add-on)
    }
    match name.as_str() {
        "general" => "\u{2699} ",       // ⚙ gear
        "editor" => "\u{270e} ",        // ✎ pencil
        "clipboard" => "\u{2702} ",     // ✂ scissors (cut/copy)
        "file browser" => "\u{25a4} ",  // ▤ square with lines (document)
        "file explorer" => "\u{25a6} ", // ▦ square with grid (tree)
        "packages" => "\u{25c6} ",      // ◆ diamond
        "plugins" => "\u{271a} ",       // ✚ heavy plus (add-on)
        "terminal" => "\u{00bb} ",      // » prompt chevron
        "warnings" => "\u{26a0} ",      // ⚠ warning sign
        "keybindings" => "\u{2328} ",   // ⌨ keyboard
        _ => "\u{2022} ",               // • bullet as fallback
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
        is_plugin_category: bool,
        cat_idx: Option<usize>,
        section_idx: Option<usize>,
        label: String,
        icon: Option<&'static str>,
    }
    let nerd_fonts = state.nerd_font_icons_enabled();
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
                    has_changes: state.page_has_pending_changes(idx),
                    indent_cols: 0,
                    is_category: true,
                    is_plugin_category: page.name.starts_with("Plugin: "),
                    cat_idx: Some(idx),
                    section_idx: None,
                    label: page.name.clone(),
                    icon: Some(category_icon(&page.name, nerd_fonts)),
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
                    is_plugin_category: false,
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
            let label = if data.is_plugin_category {
                let prefix_width: usize = spans
                    .iter()
                    .map(|span| str_width(span.content.as_ref()))
                    .sum();
                let label_width = row_area.width as usize;
                let label_width = label_width.saturating_sub(prefix_width);
                truncate_display_width_with_ellipsis(&data.label, label_width)
            } else {
                data.label.clone()
            };
            spans.push(Span::styled(label, style));

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
struct RenderContext<'a> {
    selected_item: usize,
    settings_focused: bool,
    hover_hit: Option<SettingsHit>,
    /// Persistent widget instance-state store (see
    /// [`SettingsState::widget_states`]). Passed to the widget-rendered
    /// controls as their `prev` instance-state so runtime-owned cursor /
    /// selection / edit affordances persist across frames. A disjoint
    /// borrow from `state.scroll_panel`, which `render` holds mutably.
    widget_states: &'a std::collections::HashMap<String, crate::widgets::WidgetInstanceState>,
}

/// Render the settings panel for the current category
fn render_settings_panel(
    frame: &mut Frame,
    area: Rect,
    state: &mut SettingsState,
    theme: &Theme,
    layout: &mut SettingsLayout,
) {
    let (page_title, page_nullable) = match state.current_page() {
        Some(p) => (p.name.clone(), p.nullable),
        None => return,
    };

    let mut y = area.y;
    let header_start_y = y;

    // Right-panel page title is the full context fallback for sidebar labels,
    // which are width-clamped because plugin names are external input.
    if area.height > 0 && area.width > 0 {
        let title = truncate_display_width_with_ellipsis(&page_title, area.width as usize);
        let title_style = Style::default()
            .fg(theme.editor_fg)
            .add_modifier(Modifier::BOLD);
        frame.render_widget(
            Paragraph::new(title).style(title_style),
            Rect::new(area.x, y, area.width, 1),
        );
        y += 1;
    }

    // "Clear" button for nullable categories (e.g., Option<LanguageConfig>)
    if page_nullable && state.current_category_has_values() {
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
        widget_states: &state.widget_states,
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
                SettingControl::Toggle(s) => Some(str_width(&s.label) as u16),
                SettingControl::Number(s) => Some(str_width(&s.label) as u16),
                SettingControl::Dropdown(s) => Some(str_width(&s.label) as u16),
                SettingControl::Text(s) => Some(str_width(&s.label) as u16),
                // Multi-row controls have their labels on separate lines
                _ => None,
            }
        })
        .max();
    let pending_dirty_by_item: Vec<bool> = page
        .items
        .iter()
        .map(|item| state.path_has_pending_change(&item.path))
        .collect();

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
                pending_dirty_by_item
                    .get(info.index)
                    .copied()
                    .unwrap_or(false),
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

/// The vertical clip window for one item's bands: the physical `area` it
/// occupies, the count of logical rows scrolled off the top (`skip_top`), and
/// the exclusive logical row where the viewport ends. Bundling these three
/// keeps the band helpers from threading the same trio of arguments around.
#[derive(Clone, Copy)]
struct BandViewport {
    area: Rect,
    skip_top: u16,
    viewport_end_logical: u16,
}

impl BandViewport {
    fn new(area: Rect, skip_top: u16) -> Self {
        Self {
            area,
            skip_top,
            viewport_end_logical: skip_top.saturating_add(area.height), // exclusive
        }
    }

    /// Translate a logical band `[logical_y, logical_y + rows)` to a physical
    /// sub-rectangle of `area`, accounting for `skip_top` clipping. Returns
    /// `None` when the band is entirely outside the visible viewport.
    fn band_rect(&self, logical_y: u16, rows: u16) -> Option<Rect> {
        if rows == 0 {
            return None;
        }
        let band_end = logical_y.saturating_add(rows);
        if band_end <= self.skip_top || logical_y >= self.viewport_end_logical {
            return None;
        }
        let visible_top_logical = logical_y.max(self.skip_top);
        let visible_bottom_logical = band_end.min(self.viewport_end_logical);
        let physical_y = self.area.y + (visible_top_logical - self.skip_top);
        let visible_h = visible_bottom_logical - visible_top_logical;
        Some(Rect::new(
            self.area.x,
            physical_y,
            self.area.width,
            visible_h,
        ))
    }
}

/// Shrink `r` horizontally, trimming `left` columns from the start and
/// `right` columns from the end. The y/height are left untouched.
fn inset_horizontal(r: Rect, left: u16, right: u16) -> Rect {
    Rect::new(
        r.x.saturating_add(left),
        r.y,
        r.width.saturating_sub(left.saturating_add(right)),
        r.height,
    )
}

/// Inset a band by the card's left chrome (border + focus indicator gutter)
/// and right chrome (border), matching the alignment used by the control and
/// description text.
fn inset_by_chrome(r: Rect, style: &ItemBoxStyle) -> Rect {
    inset_horizontal(
        r,
        style.card_border_cols + style.focus_indicator_cols,
        style.card_border_cols,
    )
}

/// Decide which edges of the card box are visible given the viewport clip.
/// LEFT/RIGHT follow the side-border setting; TOP/BOTTOM are only drawn when
/// their logical row actually sits inside `[skip_top, viewport_end_logical)`.
fn card_borders(
    style: &ItemBoxStyle,
    card_logical_top: u16,
    card_logical_bottom: u16,
    vp: BandViewport,
) -> Borders {
    let mut borders = Borders::NONE;
    if style.card_border_cols > 0 {
        borders |= Borders::LEFT | Borders::RIGHT;
    }
    if style.card_border_rows > 0 {
        if card_logical_top >= vp.skip_top {
            borders |= Borders::TOP;
        }
        let bottom_logical = card_logical_bottom.saturating_sub(1);
        if bottom_logical >= vp.skip_top && bottom_logical < vp.viewport_end_logical {
            borders |= Borders::BOTTOM;
        }
    }
    borders
}

/// Paint the section heading band: a blank gap on the leading rows, with the
/// title butted against the top of the card it labels. This puts the
/// breathing room above the heading so the title reads as "belongs to what's
/// below" rather than "belongs to what's above".
fn render_section_header(
    frame: &mut Frame,
    vp: BandViewport,
    plan: &ItemBox,
    item: &SettingItem,
    theme: &Theme,
) {
    let Some(section_name) = item.section.as_deref().filter(|_| item.is_section_start) else {
        return;
    };
    if vp.band_rect(0, plan.section_header_rows).is_none() {
        return;
    }
    let title_logical_y = plan.section_header_rows.saturating_sub(1);
    let Some(title_rect) = vp.band_rect(title_logical_y, 1) else {
        return;
    };
    let header_style = Style::default()
        .fg(theme.editor_fg)
        .add_modifier(Modifier::BOLD);
    frame.render_widget(
        Paragraph::new(section_name).style(header_style),
        Rect::new(title_rect.x, title_rect.y, title_rect.width, 1),
    );
}

/// Render the trailing "(Inherited)" badge or "[Inherit]" button on the
/// control's first row. Returns the button's hit-test rect when a clickable
/// button was drawn (the badge is decorative and returns `None`).
fn render_inherit_affordance(
    frame: &mut Frame,
    control_rect: Rect,
    item: &SettingItem,
    idx: usize,
    hover_hit: Option<SettingsHit>,
    theme: &Theme,
) -> Option<Rect> {
    if !item.nullable || control_rect.width == 0 {
        return None;
    }
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
        None
    } else {
        let btn_text = format!("[{}]", t!("settings.btn_inherit"));
        let btn_len = btn_text.len() as u16 + 1;
        let btn_x = control_rect
            .x
            .saturating_add(control_rect.width)
            .saturating_sub(btn_len);
        if btn_x <= control_rect.x {
            return None;
        }
        let btn_area = Rect::new(btn_x, control_rect.y, btn_len, 1);
        let is_hovered = matches!(hover_hit, Some(SettingsHit::ControlInherit(i)) if i == idx);
        let btn_style = if is_hovered {
            Style::default()
                .fg(theme.menu_hover_fg)
                .bg(theme.menu_hover_bg)
        } else {
            Style::default().fg(theme.line_number_fg)
        };
        frame.render_widget(Paragraph::new(btn_text).style(btn_style), btn_area);
        Some(btn_area)
    }
}

/// Render the wrapped description text below the control, falling back to just
/// the config-layer label when there's no description but the source layer
/// still needs to be shown.
fn render_description_band(
    frame: &mut Frame,
    vp: BandViewport,
    plan: &ItemBox,
    style: &ItemBoxStyle,
    item: &SettingItem,
    theme: &Theme,
) {
    let layer_label = match item.layer_source {
        crate::config_io::ConfigLayer::System => None,
        crate::config_io::ConfigLayer::User => Some("user"),
        crate::config_io::ConfigLayer::Project => Some("project"),
        crate::config_io::ConfigLayer::Session => Some("session"),
    };

    if plan.description_rows > 0 {
        let Some(desc_rect) = vp
            .band_rect(plan.description_y(), plan.description_rows)
            .map(|r| inset_by_chrome(r, style))
        else {
            return;
        };
        let desc_skip = vp.skip_top.saturating_sub(plan.description_y());
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
    } else if let Some(layer) = layer_label {
        // No description, just a layer label on the row immediately below the control.
        let Some(layer_rect) = vp
            .band_rect(plan.description_y(), 1)
            .map(|r| inset_by_chrome(r, style))
        else {
            return;
        };
        frame.render_widget(
            Paragraph::new(format!("({})", layer)).style(Style::default().fg(theme.line_number_fg)),
            layer_rect,
        );
    }
}

/// Pure render function for a setting item (returns layout, doesn't modify external state)
///
/// Driven by `item.layout_box(area.width, &item.style)` — every y-offset comes
/// from the resulting `ItemBox`, so adjusting card chrome (border, padding,
/// section header height) happens by changing `ItemBoxStyle`, not by editing
/// renderer arithmetic. Each visual band (section header, card box, control,
/// description) is painted by a dedicated helper; this function only computes
/// geometry and wires them together.
///
/// # Arguments
/// * `skip_top` - Number of rows to skip at top of item (for partial visibility when scrolling)
/// * `label_width` - Optional label width for column alignment
#[allow(clippy::too_many_arguments)]
fn render_setting_item_pure(
    frame: &mut Frame,
    area: Rect,
    item: &SettingItem,
    idx: usize,
    skip_top: u16,
    ctx: &RenderContext<'_>,
    theme: &Theme,
    label_width: Option<u16>,
    pending_dirty: bool,
) -> SettingItemLayoutInfo {
    let plan = item.layout_box(area.width, &item.style);
    let style = item.style;
    let vp = BandViewport::new(area, skip_top);

    // ── Section header band ────────────────────────────────────────────────
    render_section_header(frame, vp, &plan, item, theme);

    // ── Card box ───────────────────────────────────────────────────────────
    // The card spans logical rows [card_top_y, total_rows). Render it with a
    // single Block, choosing which edges to draw based on the viewport clip.
    let card_logical_top = plan.card_top_y();
    let card_logical_bottom = plan.total_rows();
    if let Some(card_rect) = vp.band_rect(
        card_logical_top,
        card_logical_bottom.saturating_sub(card_logical_top),
    ) {
        let borders = card_borders(&style, card_logical_top, card_logical_bottom, vp);
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
    if let Some(content_rect) = vp.band_rect(
        content_logical_top,
        content_logical_bottom.saturating_sub(content_logical_top),
    ) {
        let inner_area =
            inset_horizontal(content_rect, style.card_border_cols, style.card_border_cols);

        // Highlight background for focused/hovered items. Limited to the
        // label row so chip / description text below stays on popup_bg
        // and remains legible regardless of how saturated the theme's
        // highlight bg is. The colors come from the theme's
        // `settings_selected_bg` (selected) and `menu_hover_bg` (hovered)
        // — each theme is responsible for picking values that contrast
        // with its own popup_bg AND don't collide with chip text colors.
        let label_visible = vp.skip_top <= content_logical_top;
        if is_focused_or_hovered && inner_area.width > 0 && label_visible {
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
        let content_skip_top = vp.skip_top.saturating_sub(content_logical_top);

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
        if pending_dirty && label_row_visible && inner_area.width >= 2 {
            frame.render_widget(
                Paragraph::new("●").style(Style::default().fg(theme.settings_selected_fg)),
                Rect::new(inner_area.x + 1, inner_area.y, 1, 1),
            );
        }

        // Control occupies its own band at the top of the content rect.
        if let Some(control_rect) = vp
            .band_rect(content_logical_top, plan.control_rows)
            .map(|r| inset_by_chrome(r, &style))
        {
            control_layout = render_control(
                frame,
                control_rect,
                &item.control,
                &item.name,
                content_skip_top,
                theme,
                label_width,
                item.read_only,
                item.is_null,
                ctx.widget_states,
            );

            // (Inherited) badge / [Inherit] button, on the control's first row.
            if content_skip_top == 0 {
                inherit_button_area =
                    render_inherit_affordance(frame, control_rect, item, idx, ctx.hover_hit, theme);
            }
        }

        // Description band: below the control.
        render_description_band(frame, vp, &plan, &style, item, theme);
    }

    SettingItemLayoutInfo {
        control: control_layout,
        inherit_button: inherit_button_area,
    }
}

/// Render a scalar setting control (Toggle/Number/Dropdown) through the
/// plugin widget framework: map it to a `WidgetSpec`, render with
/// `render_spec`, and paint the resulting entries into `area` via the
/// shared `paint_text_property_entry`. Returns the reconciler output so
/// the caller can derive click geometry from the real hit areas.
fn render_scalar_via_widget(
    frame: &mut Frame,
    area: Rect,
    control: &SettingControl,
    name: &str,
    theme: &Theme,
    label_width: Option<u16>,
    prev: &std::collections::HashMap<String, crate::widgets::WidgetInstanceState>,
) -> crate::widgets::RenderOutput {
    render_control_via_widget(frame, area, control, name, theme, 0, "", label_width, prev)
}

/// Like [`render_scalar_via_widget`] but for multi-row controls: paints
/// entries starting at `skip_rows` (the settings viewport clips tall
/// controls at the top when scrolled) into `area`.
///
/// `focus_key` (usually the control's `name`) marks the widget focused so
/// the renderer paints the focus highlight and the block caret — pass it
/// when the control is actively editing (Text/JSON); pass `""` otherwise
/// (the settings chrome shows selection for the rest).
#[allow(clippy::too_many_arguments)]
fn render_control_via_widget(
    frame: &mut Frame,
    area: Rect,
    control: &SettingControl,
    name: &str,
    theme: &Theme,
    skip_rows: u16,
    focus_key: &str,
    label_width: Option<u16>,
    prev: &std::collections::HashMap<String, crate::widgets::WidgetInstanceState>,
) -> crate::widgets::RenderOutput {
    let spec = crate::view::settings::widget_map::setting_control_to_widget_aligned(
        name,
        control,
        label_width,
    );
    let out =
        crate::widgets::render_spec_no_autofocus(&spec, prev, focus_key, area.width.max(1) as u32);
    for (i, entry) in out.entries.iter().enumerate() {
        let row = i as u16;
        if row < skip_rows {
            continue;
        }
        let dst = row - skip_rows;
        if dst < area.height {
            crate::app::render::paint_text_property_entry(
                frame,
                entry,
                area.x,
                area.y + dst,
                area.width,
                theme,
                None,
            );
        }
    }
    out
}

/// Screen rect of the first hit with the given kind + event type,
/// derived from the *real* rendered geometry: the hit's byte range is
/// converted to display columns against its row's entry text. Returns
/// `Rect::default()` (empty, unhittable) when absent or scrolled off.
fn hit_rect(
    out: &crate::widgets::RenderOutput,
    widget_kind: &str,
    event_type: &str,
    area: Rect,
    skip_rows: u16,
) -> Rect {
    use crate::primitives::display_width::str_width;
    for h in &out.hits {
        if h.widget_kind != widget_kind || h.event_type != event_type {
            continue;
        }
        let Some(dst) = (h.buffer_row as u16).checked_sub(skip_rows) else {
            continue;
        };
        if dst >= area.height {
            continue;
        }
        let Some(entry) = out.entries.get(h.buffer_row as usize) else {
            continue;
        };
        let text = entry.text.trim_end_matches('\n');
        let s = h.byte_start.min(text.len());
        let e = h.byte_end.min(text.len());
        let x = str_width(&text[..s]) as u16;
        let w = str_width(&text[s..e]).max(1) as u16;
        if x >= area.width {
            continue;
        }
        return Rect::new(area.x + x, area.y + dst, w.min(area.width - x), 1);
    }
    Rect::default()
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
    // Nullable-null fields previously rendered with dimmed brackets;
    // the widget-rendered path shows them like any other value for now.
    _is_null: bool,
    // Persistent widget instance-state store used as the `prev` for the
    // widget-rendered controls (runtime-owned cursor / selection / edit
    // affordances). Empty until a control's input is routed through the
    // runtime, in which case rendering matches the old per-control State.
    prev: &std::collections::HashMap<String, crate::widgets::WidgetInstanceState>,
) -> ControlLayoutInfo {
    match control {
        // Single-row controls: only render if not skipped.
        //
        // Every control renders through the plugin widget framework
        // (Settings↔widget unification): the control maps to a
        // `WidgetSpec` (with the page's label column width for
        // alignment) and paints via `render_spec` +
        // `paint_text_property_entry`, the same path plugin panels
        // use. Click geometry comes from the reconciler's real hit
        // areas (`hit_rect`), not approximations. The control's State
        // stays the model (input.rs still drives it); the widget rows
        // faithfully project that state each frame.
        SettingControl::Toggle(_) => {
            if skip_rows > 0 {
                return ControlLayoutInfo::Toggle(Rect::default());
            }
            let out =
                render_scalar_via_widget(frame, area, control, name, theme, label_width, prev);
            ControlLayoutInfo::Toggle(hit_rect(&out, "toggle", "toggle", area, 0))
        }

        SettingControl::Number(_) => {
            if skip_rows > 0 {
                return ControlLayoutInfo::Number {
                    decrement: Rect::default(),
                    increment: Rect::default(),
                    value: Rect::default(),
                };
            }
            let out =
                render_scalar_via_widget(frame, area, control, name, theme, label_width, prev);
            ControlLayoutInfo::Number {
                decrement: Rect::default(),
                increment: Rect::default(),
                value: hit_rect(&out, "number", "number_value", area, 0),
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
            let out =
                render_scalar_via_widget(frame, area, control, name, theme, label_width, prev);
            let button_area = hit_rect(&out, "dropdown", "dropdown_toggle", area, 0);
            // When open, paint the option list inline beneath the button.
            //
            // The shared widget framework (`collect_dropdown`) surfaces an
            // open dropdown's options as a *floating* screen-level pop-over
            // (`RenderOutput::dropdown_popups`) for plugin panels, and
            // discards `render_dropdown`'s inline `option_rows`. The Settings
            // modal does not draw those floating pop-overs — it reserves
            // inline rows for the open list via `SettingControl::height`. So
            // relying on `render_scalar_via_widget` alone leaves the reserved
            // rows blank: the dropdown opens to an empty box (theme and every
            // other settings dropdown showed no options — #2765).
            //
            // Render the option rows directly and paint them under the button
            // (row 0), exactly where the reserved height expects them. Use the
            // same label/label-width/selected/scroll the button render used so
            // the option column aligns under the value cell, and build one hit
            // rect per visible row in screen order (`layout.rs` pairs them with
            // `scroll_offset` to recover absolute indices).
            let mut option_areas = Vec::new();
            let mut scroll_offset = 0;
            if state.open {
                let rendered = crate::widgets::render_dropdown(
                    &state.options,
                    state.selected as i32,
                    &state.label,
                    false,
                    label_width.unwrap_or(0) as u32,
                    true,
                    state.scroll_offset as u32,
                );
                scroll_offset = rendered.scroll_offset;
                for (row_i, (_idx, entry)) in rendered.option_rows.iter().enumerate() {
                    // Row 0 is the button that `render_scalar_via_widget`
                    // already painted; options start at row 1.
                    let dst = 1 + row_i as u16;
                    if dst >= area.height {
                        break;
                    }
                    crate::app::render::paint_text_property_entry(
                        frame,
                        entry,
                        area.x,
                        area.y + dst,
                        area.width,
                        theme,
                        None,
                    );
                    option_areas.push(Rect::new(area.x, area.y + dst, area.width, 1));
                }
            }
            ControlLayoutInfo::Dropdown {
                button_area,
                option_areas,
                scroll_offset,
            }
        }

        SettingControl::Text(state) => {
            if skip_rows > 0 {
                return ControlLayoutInfo::Text {
                    area: Rect::default(),
                    geometry: None,
                };
            }
            if read_only {
                // Truly read-only fields (e.g., Key: in entry dialogs) render as plain text
                let label_w = label_width.unwrap_or(20);
                let label_style = Style::default().fg(theme.editor_fg);
                let value_style = Style::default().fg(theme.line_number_fg);
                let label = format!("{}: ", state.label);
                let value = state.value();

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
                ControlLayoutInfo::Text {
                    area: Rect::default(),
                    geometry: None,
                }
            } else {
                // Editable text (and nullable-null) render through the
                // widget framework. While editing, focus the widget (by
                // its name key) so it paints the focus highlight and
                // the block caret the mapping carries.
                let focus_key = if state.editing { name } else { "" };
                let out = render_control_via_widget(
                    frame,
                    area,
                    control,
                    name,
                    theme,
                    0,
                    focus_key,
                    label_width,
                    prev,
                );
                let rect = hit_rect(&out, "text", "focus", area, 0);
                // Stamp the field's click geometry from the render we just
                // did, so a later click maps to a caret position without
                // reverse-engineering the layout. The row is painted at
                // `area.x` (its byte 0).
                let geometry =
                    crate::widgets::WidgetTextClickGeometry::from_render_output(&out, area.x);
                ControlLayoutInfo::Text {
                    area: if rect.width > 0 {
                        rect
                    } else {
                        Rect::new(area.x, area.y, area.width, 1)
                    },
                    geometry,
                }
            }
        }

        // Multi-row controls: pass skip_rows to render partial view
        SettingControl::TextList(state) => {
            // Bracketed item cells + [x] buttons + the add row, all
            // projected from the control state by the widget mapping.
            // Per-row hit rects are derived from the on-screen row
            // positions so clicks still target the right item; the
            // trailing entry (index None) is the add row.
            render_control_via_widget(
                frame,
                area,
                control,
                name,
                theme,
                skip_rows,
                "",
                label_width,
                prev,
            );
            let mut rows = Vec::new();
            // Row 0 is the label header; items start at row 1, the add
            // row follows them. The item cell spans `[...]` starting at
            // the 2-column indent (matching the rendered row).
            let cell_w = 30u16.min(area.width.saturating_sub(2));
            for (i, _) in state.items.iter().enumerate() {
                let logical = 1 + i as u16;
                if logical >= skip_rows {
                    let dst = logical - skip_rows;
                    if dst < area.height {
                        rows.push((Some(i), Rect::new(area.x + 2, area.y + dst, cell_w, 1)));
                    }
                }
            }
            let add_logical = 1 + state.items.len() as u16;
            if add_logical >= skip_rows {
                let dst = add_logical - skip_rows;
                if dst < area.height {
                    rows.push((None, Rect::new(area.x + 2, area.y + dst, cell_w, 1)));
                }
            }
            ControlLayoutInfo::TextList { rows }
        }

        SettingControl::DualList(_) => {
            // View migrated to the widget `DualList` kind (two-column
            // Available/Included picker); editing still runs through the
            // settings input path. Mouse hit geometry is approximate for
            // now (keyboard nav is the primary path).
            render_control_via_widget(
                frame,
                area,
                control,
                name,
                theme,
                skip_rows,
                "",
                label_width,
                prev,
            );
            ControlLayoutInfo::DualList(Default::default())
        }

        SettingControl::Map(state) => {
            // Label + optional column header + List-rendered entries +
            // add row, all projected from the control state.
            render_control_via_widget(
                frame,
                area,
                control,
                name,
                theme,
                skip_rows,
                "",
                label_width,
                prev,
            );
            let row_rect = |logical: u16| -> Option<Rect> {
                logical.checked_sub(skip_rows).and_then(|dst| {
                    (dst < area.height).then(|| Rect::new(area.x, area.y + dst, area.width, 1))
                })
            };
            // Row 0 is the `label:` header. When the control has a
            // `display_field`, the mapping inserts a `Name  <Title>`
            // column header at row 1, so entries start at row 2; without
            // it they start at row 1. The hit geometry must track that
            // offset or clicks land one row above every entry and the
            // "add new" row.
            let first_entry_row = if state.display_field.is_some() { 2 } else { 1 };
            let entry_rows = state
                .entries
                .iter()
                .enumerate()
                .filter_map(|(i, _)| row_rect(first_entry_row + i as u16).map(|r| (i, r)))
                .collect();
            let add_row_area = if state.no_add {
                None
            } else {
                row_rect(first_entry_row + state.entries.len() as u16)
            };
            ControlLayoutInfo::Map {
                entry_rows,
                add_row_area,
            }
        }

        SettingControl::ObjectArray(state) => {
            render_control_via_widget(
                frame,
                area,
                control,
                name,
                theme,
                skip_rows,
                "",
                label_width,
                prev,
            );
            let entry_rows = state
                .bindings
                .iter()
                .enumerate()
                .filter_map(|(i, _)| {
                    (1 + i as u16).checked_sub(skip_rows).and_then(|dst| {
                        (dst < area.height)
                            .then(|| (i, Rect::new(area.x, area.y + dst, area.width, 1)))
                    })
                })
                .collect();
            ControlLayoutInfo::ObjectArray { entry_rows }
        }

        // The multiline JSON editor renders through the widget
        // framework too: label + bordered line box + block caret +
        // selection + invalid-JSON warning, projected from the
        // control's `TextEdit` by the mapping. Editing (and JSON
        // validation) still runs through the settings input path.
        SettingControl::Json(state) => {
            let focus_key = if matches!(state.focus, crate::view::controls::FocusState::Focused) {
                name
            } else {
                ""
            };
            render_control_via_widget(
                frame,
                area,
                control,
                name,
                theme,
                skip_rows,
                focus_key,
                label_width,
                prev,
            );
            ControlLayoutInfo::Text {
                area: Rect::new(area.x, area.y, area.width, 1),
                geometry: None,
            }
        }

        SettingControl::Complex { .. } => {
            if skip_rows > 0 {
                return ControlLayoutInfo::Complex;
            }
            // Uneditable placeholder, rendered through the widget
            // framework like the other controls.
            render_control_via_widget(
                frame,
                area,
                control,
                name,
                theme,
                skip_rows,
                "",
                label_width,
                prev,
            );
            ControlLayoutInfo::Complex
        }
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
    Text {
        area: Rect,
        /// Stamped at render time for single-line editable text fields so a
        /// click can be mapped to a caret position without reconstructing
        /// the field's label/bracket/truncation layout (#2573). `None` for
        /// read-only or multi-line (JSON) text. When Settings controls are
        /// mounted as real widget panels this becomes redundant with the
        /// registry hit path (see `widgets::WidgetTextClickGeometry`).
        geometry: Option<crate::widgets::WidgetTextClickGeometry>,
    },
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
    // Show result count and scroll position inline after cursor
    let result_count = state.search_results.len();
    let count_text = if state.search_query().is_empty() {
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

    // The editable query renders through the plugin widget framework —
    // the same `WidgetSpec::Text` + `render_spec` path every settings
    // field now uses — instead of hand-rolled cursor spans. The widget
    // owns the caret (a REVERSED block cell via `block_caret`) and the
    // selection highlight, driven statelessly from the search input's
    // `TextEdit` (value + byte cursor + live selection). The result
    // count / scroll indicators are search chrome, painted after the
    // field at its rendered width.
    let query = state.search_query();
    let cursor = state.search_cursor().min(query.len()) as i32;
    let (sel_start, sel_end) = state
        .search_input
        .editor
        .selection_flat_range()
        .map(|(a, b)| (a as i32, b as i32))
        .unwrap_or((-1, -1));
    let field_spec = fresh_core::api::WidgetSpec::Text {
        value: query.to_string(),
        cursor_byte: cursor,
        focused: true,
        label: String::new(),
        placeholder: None,
        rows: 1,
        field_width: 0,
        max_visible_chars: 0,
        full_width: false,
        completions: Vec::new(),
        completions_visible_rows: 0,
        block_caret: true,
        sel_start,
        sel_end,
        label_width: 0,
        key: None,
    };
    let out = crate::widgets::render_spec_no_autofocus(
        &field_spec,
        &std::collections::HashMap::new(),
        "",
        area.width.max(1) as u32,
    );
    let field_width = out
        .entries
        .first()
        .map(|e| crate::primitives::display_width::str_width(e.text.trim_end_matches('\n')))
        .unwrap_or(0) as u16;
    if let Some(entry) = out.entries.first() {
        crate::app::render::paint_text_property_entry(
            frame, entry, area.x, area.y, area.width, theme, None,
        );
    }

    // Result-count + scroll-indicator suffix, right after the field.
    let suffix_x = area.x.saturating_add(field_width);
    if suffix_x < area.x + area.width && !(count_text.is_empty() && scroll_indicator.is_empty()) {
        let suffix = Line::from(vec![
            Span::styled(count_text, count_style),
            Span::styled(scroll_indicator, indicator_style),
        ]);
        let suffix_w = (area.x + area.width).saturating_sub(suffix_x);
        frame.render_widget(
            Paragraph::new(suffix),
            Rect::new(suffix_x, area.y, suffix_w, 1),
        );
    }
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
/// Draw a centered modal dialog: clear the region, paint a rounded border in
/// `border_fg`, and return `(dialog_area, inner)` where `inner` is the
/// 2-column / 1-row padded content rect. Shared by every settings confirm
/// dialog so the centering, border, and inset math live in one place.
fn centered_dialog_frame(
    frame: &mut Frame,
    parent_area: Rect,
    width: u16,
    height: u16,
    title: String,
    border_fg: Color,
    theme: &Theme,
) -> (Rect, Rect) {
    let dialog_x = parent_area.x + (parent_area.width.saturating_sub(width)) / 2;
    let dialog_y = parent_area.y + (parent_area.height.saturating_sub(height)) / 2;
    let dialog_area = Rect::new(dialog_x, dialog_y, width, height);

    frame.render_widget(Clear, dialog_area);

    let block = Block::default()
        .title(title)
        .borders(Borders::ALL)
        .border_type(BorderType::Rounded)
        .border_style(Style::default().fg(border_fg))
        .style(Style::default().bg(theme.popup_bg));
    frame.render_widget(block, dialog_area);

    let inner = Rect::new(
        dialog_area.x + 2,
        dialog_area.y + 1,
        dialog_area.width.saturating_sub(4),
        dialog_area.height.saturating_sub(2),
    );
    (dialog_area, inner)
}

/// Render the standard one-line key-hint footer just below the button row.
fn render_dialog_help(frame: &mut Frame, inner: Rect, button_y: u16, help: &str, theme: &Theme) {
    frame.render_widget(
        Paragraph::new(help.to_string()).style(Style::default().fg(theme.line_number_fg)),
        Rect::new(inner.x, button_y + 1, inner.width, 1),
    );
}

/// List the pending-change descriptions as bulleted, width-truncated lines
/// starting at `start_y`. Character-based truncation (rather than byte
/// truncation) keeps CJK / emoji descriptions from slicing through a
/// multi-byte UTF-8 sequence and panicking — same class as #1718.
fn render_change_list(
    frame: &mut Frame,
    inner: Rect,
    start_y: u16,
    changes: &[String],
    dialog_height: u16,
    theme: &Theme,
) {
    let change_style = Style::default().fg(theme.popup_text_fg);
    for (i, change) in changes
        .iter()
        .take((dialog_height as usize).saturating_sub(7))
        .enumerate()
    {
        let max_chars = (inner.width as usize).saturating_sub(2);
        let truncated = format!("• {}", truncate_chars_with_ellipsis(change, max_chars));
        frame.render_widget(
            Paragraph::new(truncated).style(change_style),
            Rect::new(inner.x, start_y + i as u16, inner.width, 1),
        );
    }
}

/// Render a centered row of `[ label ]` choice buttons using the menu
/// highlight/hover palette. The selected button is prefixed with `>` and bold;
/// a hovered (but unselected) button uses the hover palette. Shared by the
/// unsaved-changes and reset confirm dialogs.
fn render_choice_buttons(
    frame: &mut Frame,
    inner: Rect,
    button_y: u16,
    options: &[String],
    selected: usize,
    hover: Option<usize>,
    theme: &Theme,
) {
    let total_width: u16 = options.iter().map(|o| o.len() as u16 + 4).sum::<u16>() + 4; // +4 for gaps
    let mut x = inner.x + (inner.width.saturating_sub(total_width)) / 2;

    for (idx, label) in options.iter().enumerate() {
        let is_selected = idx == selected;
        let is_hovered = hover == Some(idx);
        let button_width = label.len() as u16 + 4;

        let style = if is_selected {
            Style::default()
                .fg(theme.menu_highlight_fg)
                .bg(theme.menu_highlight_bg)
                .add_modifier(Modifier::BOLD)
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
}

/// Render a centered row of `[ label ]` buttons for a destructive-action
/// confirm dialog: the button at `destructive_idx` is tinted with the danger
/// foreground, and the selected button gets the popup-selection background.
/// Shared by the entry discard / delete confirm dialogs.
fn render_destructive_buttons(
    frame: &mut Frame,
    inner: Rect,
    button_y: u16,
    options: &[&str],
    selected: usize,
    destructive_idx: usize,
    theme: &Theme,
) {
    let total_width: u16 =
        options.iter().map(|o| o.len() as u16 + 5).sum::<u16>() + 2 * (options.len() as u16 - 1);
    let mut x = inner.x + (inner.width.saturating_sub(total_width)) / 2;

    for (idx, label) in options.iter().enumerate() {
        let is_selected = idx == selected;
        let is_destructive = idx == destructive_idx;
        let style = if is_selected && is_destructive {
            Style::default()
                .fg(theme.diagnostic_error_fg)
                .bg(theme.popup_selection_bg)
                .add_modifier(Modifier::BOLD)
        } else if is_selected {
            Style::default()
                .fg(theme.popup_selection_fg)
                .bg(theme.popup_selection_bg)
                .add_modifier(Modifier::BOLD)
        } else if is_destructive {
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
}

fn render_confirm_dialog(
    frame: &mut Frame,
    parent_area: Rect,
    state: &SettingsState,
    theme: &Theme,
) {
    let changes = state.get_change_descriptions();
    let dialog_width = 50.min(parent_area.width.saturating_sub(4));
    // Base height: 2 borders + 2 prompt lines + 1 separator + 1 buttons + 1 help = 7
    // Plus one line per change
    let dialog_height = (7 + changes.len() as u16)
        .min(20)
        .min(parent_area.height.saturating_sub(4));

    let title = format!(" {} ", t!("confirm.unsaved_changes_title"));
    let (dialog_area, inner) = centered_dialog_frame(
        frame,
        parent_area,
        dialog_width,
        dialog_height,
        title,
        theme.diagnostic_warning_fg,
        theme,
    );

    // Prompt text
    let prompt = t!("confirm.unsaved_changes_prompt").to_string();
    frame.render_widget(
        Paragraph::new(prompt).style(Style::default().fg(theme.popup_text_fg)),
        Rect::new(inner.x, inner.y, inner.width, 1),
    );
    render_change_list(frame, inner, inner.y + 2, &changes, dialog_height, theme);

    let button_y = dialog_area.y + dialog_area.height - 3;

    // Draw separator
    let sep_line: String = "─".repeat(inner.width as usize);
    frame.render_widget(
        Paragraph::new(sep_line).style(Style::default().fg(theme.split_separator_fg)),
        Rect::new(inner.x, button_y - 1, inner.width, 1),
    );

    let options = [
        t!("confirm.save_and_exit").to_string(),
        t!("confirm.discard").to_string(),
        t!("confirm.cancel").to_string(),
    ];
    render_choice_buttons(
        frame,
        inner,
        button_y,
        &options,
        state.confirm_dialog_selection,
        state.confirm_dialog_hover,
        theme,
    );
    render_dialog_help(
        frame,
        inner,
        button_y,
        "←/→/Tab: Select   Enter: Confirm   Esc: Cancel",
        theme,
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

    let (dialog_area, inner) = centered_dialog_frame(
        frame,
        parent_area,
        dialog_width,
        dialog_height,
        " Reset All Changes ".to_string(),
        theme.diagnostic_warning_fg,
        theme,
    );

    // Prompt text
    frame.render_widget(
        Paragraph::new("Discard all pending changes?")
            .style(Style::default().fg(theme.popup_text_fg)),
        Rect::new(inner.x, inner.y, inner.width, 1),
    );
    render_change_list(frame, inner, inner.y + 2, &changes, dialog_height, theme);

    let button_y = dialog_area.y + dialog_area.height - 3;

    // Draw separator
    let sep_line: String = "─".repeat(inner.width as usize);
    frame.render_widget(
        Paragraph::new(sep_line).style(Style::default().fg(theme.split_separator_fg)),
        Rect::new(inner.x, button_y - 1, inner.width, 1),
    );

    let options = ["Reset".to_string(), "Cancel".to_string()];
    render_choice_buttons(
        frame,
        inner,
        button_y,
        &options,
        state.reset_dialog_selection,
        state.reset_dialog_hover,
        theme,
    );
    render_dialog_help(
        frame,
        inner,
        button_y,
        "←/→/Tab: Select   Enter: Confirm   Esc: Cancel",
        theme,
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
    let (dialog_area, inner) = centered_dialog_frame(
        frame,
        parent_area,
        dialog_width,
        dialog_height,
        " Discard changes? ".to_string(),
        theme.diagnostic_warning_fg,
        theme,
    );

    frame.render_widget(
        Paragraph::new("You have uncommitted edits in this dialog.")
            .style(Style::default().fg(theme.popup_text_fg)),
        Rect::new(inner.x, inner.y, inner.width, 1),
    );

    // Buttons. 0 = Keep editing (default), 1 = Discard. Discard styled
    // in the danger fg to make the destructive choice unmistakable.
    let button_y = dialog_area.y + dialog_area.height - 3;
    render_destructive_buttons(
        frame,
        inner,
        button_y,
        &["Keep editing", "Discard"],
        state.entry_discard_confirm_selection,
        1,
        theme,
    );
    render_dialog_help(
        frame,
        inner,
        button_y,
        "Tab/←→: Select   Enter: Confirm   Esc: Keep editing",
        theme,
    );
}

/// Compute the footer Delete-button label for an entry dialog.
///
/// Schema-driven: shows the map key for map entries (e.g.
/// `[ Delete "rust" ]`), a generic "item" for array items (the
/// numeric index isn't meaningful to the user), or a bare fallback
/// when neither is available. The key is truncated so a very long
/// identifier can't blow out the dialog footer.
fn entry_delete_button_label(dialog: &EntryDialogState) -> String {
    const MAX_KEY_IN_LABEL: usize = 24;
    if dialog.is_array_item {
        "[ Delete item ]".to_string()
    } else if dialog.entry_key.is_empty() {
        "[ Delete entry ]".to_string()
    } else {
        let key = if dialog.entry_key.chars().count() > MAX_KEY_IN_LABEL {
            let truncated: String = dialog
                .entry_key
                .chars()
                .take(MAX_KEY_IN_LABEL - 1)
                .collect();
            format!("{}…", truncated)
        } else {
            dialog.entry_key.clone()
        };
        format!("[ Delete \"{}\" ]", key)
    }
}

/// Render the "Delete <name>?" prompt that appears when the user
/// activates the Delete button on an entry dialog.
fn render_entry_delete_confirm(
    frame: &mut Frame,
    parent_area: Rect,
    state: &SettingsState,
    theme: &Theme,
) {
    let dialog_width = 60.min(parent_area.width.saturating_sub(4));
    let dialog_height = 7u16.min(parent_area.height.saturating_sub(4));

    let title = if !state.entry_delete_target_name.is_empty() {
        format!(" Delete \"{}\"? ", state.entry_delete_target_name)
    } else if state.entry_delete_target_is_array_item {
        " Delete item? ".to_string()
    } else {
        " Delete entry? ".to_string()
    };

    let (dialog_area, inner) = centered_dialog_frame(
        frame,
        parent_area,
        dialog_width,
        dialog_height,
        title,
        theme.diagnostic_error_fg,
        theme,
    );

    let body = if !state.entry_delete_target_name.is_empty() {
        format!(
            "This will permanently remove \"{}\".",
            state.entry_delete_target_name
        )
    } else if state.entry_delete_target_is_array_item {
        "This will permanently remove this item.".to_string()
    } else {
        "This will permanently remove the entry.".to_string()
    };
    frame.render_widget(
        Paragraph::new(body).style(Style::default().fg(theme.popup_text_fg)),
        Rect::new(inner.x, inner.y, inner.width, 1),
    );

    let button_y = dialog_area.y + dialog_area.height - 3;
    render_destructive_buttons(
        frame,
        inner,
        button_y,
        &["Cancel", "Delete"],
        state.entry_delete_confirm_selection,
        1,
        theme,
    );
    render_dialog_help(
        frame,
        inner,
        button_y,
        "Tab/←→: Select   Enter: Confirm   Esc: Cancel",
        theme,
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

/// Render the scrolled list of items and (when needed) the scrollbar.
#[allow(clippy::too_many_arguments)]
fn render_entry_items(
    frame: &mut Frame,
    dialog_area: Rect,
    inner: Rect,
    dialog: &super::entry_dialog::EntryDialogState,
    theme: &Theme,
    label_col_width: u16,
    scroll_offset: usize,
    total_content_height: usize,
    viewport_height: usize,
) {
    let needs_scroll = total_content_height > viewport_height;
    let mut content_y: usize = 0;
    let mut screen_y = inner.y;

    let first_editable = dialog.first_editable_index;
    let needs_separator = first_editable > 0 && first_editable < dialog.items.len();

    for (idx, item) in dialog.items.iter().enumerate() {
        // Separator between read-only and editable sections
        if needs_separator && idx == first_editable {
            let separator_end = content_y + 1;
            if separator_end > scroll_offset
                && screen_y < inner.y + inner.height
                && content_y >= scroll_offset
            {
                let sep_style = Style::default().fg(theme.line_number_fg);
                let separator_line = "─".repeat(inner.width.saturating_sub(2) as usize);
                frame.render_widget(
                    Paragraph::new(separator_line).style(sep_style),
                    Rect::new(inner.x + 1, screen_y, inner.width.saturating_sub(2), 1),
                );
                screen_y += 1;
            }
            content_y = separator_end;
        }

        // Section header (2 logical rows: title + blank spacer below)
        if item.is_section_start {
            if let Some(ref section_name) = item.section {
                let header_start = content_y;
                let header_end = content_y + 2;
                if header_end > scroll_offset && screen_y < inner.y + inner.height {
                    let skip_h = header_start.saturating_sub(scroll_offset) as u16;
                    if skip_h == 0 {
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
                        screen_y += 1; // blank line after header
                    }
                }
                content_y = header_end;
            }
        }

        let control_height = item.control.control_height() as usize;
        let item_start = content_y;
        let item_end = content_y + control_height;

        if item_end <= scroll_offset {
            content_y = item_end;
            continue;
        }
        if screen_y >= inner.y + inner.height {
            break;
        }

        let skip_rows = if item_start < scroll_offset {
            (scroll_offset - item_start) as u16
        } else {
            0
        };
        let visible_height = control_height.saturating_sub(skip_rows as usize);
        let available_height = (inner.y + inner.height).saturating_sub(screen_y) as usize;
        let render_height = visible_height.min(available_height);

        if render_height == 0 {
            content_y = item_end;
            continue;
        }

        let is_readonly = item.read_only;
        let is_focused = !is_readonly && !dialog.focus_on_buttons && dialog.selected_item == idx;
        let is_hovered = !is_readonly && dialog.hover_item == Some(idx);

        if is_focused || is_hovered {
            let bg_style = if is_focused {
                Style::default().bg(theme.settings_selected_bg)
            } else {
                Style::default().bg(theme.menu_hover_bg)
            };
            if item.control.is_composite() {
                let sub_row = item.control.focused_sub_row();
                if sub_row >= skip_rows && (sub_row - skip_rows) < render_height as u16 {
                    let highlight_y = screen_y + sub_row - skip_rows;
                    frame.render_widget(
                        Paragraph::new("").style(bg_style),
                        Rect::new(inner.x, highlight_y, inner.width, 1),
                    );
                }
            } else {
                for row in 0..render_height as u16 {
                    frame.render_widget(
                        Paragraph::new("").style(bg_style),
                        Rect::new(inner.x, screen_y + row, inner.width, 1),
                    );
                }
            }
        }

        // Indicator column: [>] focus  [●] modified  [ ] spacer
        let focus_indicator_width: u16 = 3;
        if is_focused {
            let indicator_y = if item.control.is_composite() {
                let sub_row = item.control.focused_sub_row();
                let visible_sub = sub_row.saturating_sub(skip_rows);
                if visible_sub < render_height as u16 {
                    screen_y + visible_sub
                } else {
                    screen_y
                }
            } else {
                screen_y
            };
            if indicator_y >= screen_y && indicator_y < screen_y + render_height as u16 {
                let indicator_style = Style::default()
                    .fg(theme.settings_selected_fg)
                    .add_modifier(Modifier::BOLD);
                frame.render_widget(
                    Paragraph::new(">").style(indicator_style),
                    Rect::new(inner.x, indicator_y, 1, 1),
                );
            }
        }
        if item.modified && skip_rows == 0 {
            let modified_style = Style::default().fg(theme.settings_selected_fg);
            frame.render_widget(
                Paragraph::new("●").style(modified_style),
                Rect::new(inner.x + 1, screen_y, 1, 1),
            );
        }

        let control_area = Rect::new(
            inner.x + focus_indicator_width,
            screen_y,
            inner.width.saturating_sub(focus_indicator_width),
            render_height as u16,
        );
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
            // Entry-dialog controls will carry their own runtime store once
            // that path is mounted; until then render statelessly (empty prev).
            &std::collections::HashMap::new(),
        );

        // Per-field affordances on the control's first row at the right edge:
        // a dim `(Inherited)` badge when the value is inherited, otherwise the
        // applicable action buttons (`[Reset]` to the built-in default and/or
        // `[Inherit]` to the global/parent value). A field only offers the
        // action(s) that lead to a different result (issue #2345). Hit-testing
        // mirrors this geometry in `handle_entry_dialog_item_click`.
        if !item.read_only && skip_rows == 0 && control_area.width > 0 {
            let right_edge = control_area.x.saturating_add(control_area.width);
            let inherits = dialog
                .inheritable_fields
                .contains(item.path.trim_start_matches('/'));
            if item.nullable && item.is_null {
                // Only show the "(Inherited)" badge when the unset value really
                // does inherit a parent value; a clear-only field (e.g. a
                // formatter) just reads as empty/not-set.
                if inherits {
                    let badge = t!("settings.inherited_badge").to_string();
                    let w = badge.chars().count() as u16 + 1;
                    let x = right_edge.saturating_sub(w);
                    if x > control_area.x {
                        frame.render_widget(
                            Paragraph::new(badge).style(
                                Style::default()
                                    .fg(theme.line_number_fg)
                                    .add_modifier(Modifier::ITALIC),
                            ),
                            Rect::new(x, screen_y, w, 1),
                        );
                    }
                }
            } else {
                let buttons = dialog.field_action_buttons(idx);
                let positions =
                    super::entry_dialog::layout_field_action_buttons(&buttons, right_edge);
                let focused = if dialog.selected_item == idx {
                    dialog.field_button_focus
                } else {
                    None
                };
                for (bi, ((_, label), (_, x, w))) in
                    buttons.iter().zip(positions.iter()).enumerate()
                {
                    if *x <= control_area.x {
                        continue;
                    }
                    let style = if Some(bi) == focused {
                        Style::default()
                            .fg(theme.menu_hover_fg)
                            .bg(theme.menu_hover_bg)
                            .add_modifier(Modifier::BOLD)
                    } else {
                        Style::default().fg(theme.line_number_fg)
                    };
                    frame.render_widget(
                        Paragraph::new(label.clone()).style(style),
                        Rect::new(*x, screen_y, *w, 1),
                    );
                }
            }
        }

        screen_y += render_height as u16;
        content_y = item_end;
    }

    if needs_scroll {
        let scrollbar_x = dialog_area.x + dialog_area.width - 3;
        let scrollbar_area = Rect::new(scrollbar_x, inner.y, 1, inner.height);
        let scrollbar_state =
            ScrollbarState::new(total_content_height, viewport_height, scroll_offset);
        let scrollbar_colors = ScrollbarColors::from_theme(theme);
        render_scrollbar(frame, scrollbar_area, &scrollbar_state, &scrollbar_colors);
    }
}

/// Render the Save / Cancel / Delete button row.
///
/// Order: [Save] [Cancel]  [Delete …] — Delete is separated by a wider gap so
/// the destructive action cannot be reached by accidentally pressing Tab one
/// extra time.  Delete uses a per-entry label (map key or generic "item") so
/// the user knows what will be removed before committing.
fn render_entry_buttons(
    frame: &mut Frame,
    dialog_area: Rect,
    dialog: &super::entry_dialog::EntryDialogState,
    theme: &Theme,
) {
    let button_y = dialog_area.y + dialog_area.height - 2;
    let has_delete = !dialog.is_new && !dialog.no_delete;
    let delete_label = entry_delete_button_label(dialog);
    let buttons: Vec<String> = if has_delete {
        vec![
            "[ Save ]".to_string(),
            "[ Cancel ]".to_string(),
            delete_label,
        ]
    } else {
        vec!["[ Save ]".to_string(), "[ Cancel ]".to_string()]
    };
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

        if idx > 0 {
            x += if is_delete { DELETE_GAP } else { BUTTON_GAP };
        }
        if is_selected {
            let indicator_style = Style::default()
                .fg(theme.settings_selected_fg)
                .add_modifier(Modifier::BOLD);
            frame.render_widget(
                Paragraph::new(">").style(indicator_style),
                Rect::new(x.saturating_sub(2), button_y, 1, 1),
            );
        }

        // Selected Delete keeps red fg as a "still destructive" cue while
        // REVERSED signals keyboard focus — consistent with other selected items.
        let style = if is_selected && is_delete {
            Style::default()
                .fg(theme.diagnostic_error_fg)
                .bg(theme.popup_selection_bg)
                .add_modifier(Modifier::BOLD | Modifier::REVERSED)
        } else if is_selected {
            Style::default()
                .fg(theme.popup_selection_fg)
                .bg(theme.popup_selection_bg)
                .add_modifier(Modifier::BOLD | Modifier::REVERSED)
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
            Paragraph::new(label.as_str()).style(style),
            Rect::new(x, button_y, label.len() as u16, 1),
        );
        x += label.len() as u16;
    }
}

/// Render the field-description hint (row above buttons) and the keybinding
/// legend (row below buttons) at the bottom of the entry dialog.
fn render_entry_footer(
    frame: &mut Frame,
    dialog_area: Rect,
    inner: Rect,
    dialog: &super::entry_dialog::EntryDialogState,
    theme: &Theme,
) {
    let button_y = dialog_area.y + dialog_area.height - 2;
    let helper_y = button_y.saturating_sub(1);

    // One line of contextual help immediately above the buttons.
    if !dialog.focus_on_buttons && helper_y > inner.y {
        // When the cursor is on a TextList's "[+] Add new" row the focused
        // item slot is None; surface a caption that names what Enter/Esc do
        // rather than silently absorbing keystrokes.
        let pending_list_caption = dialog.current_item().and_then(|it| {
            if let SettingControl::TextList(state) = &it.control {
                if state.focused_item.is_none() {
                    return Some(if !state.pending_active && state.new_item_text.is_empty() {
                        "Press Enter (or type) to add a new item; ↓/Tab to leave"
                    } else if state.new_item_text.is_empty() {
                        "Type the new item — Enter to add, Esc to cancel"
                    } else {
                        "Editing new item — Enter to add, Esc to cancel"
                    });
                }
            }
            None
        });

        let text: Option<String> = pending_list_caption.map(String::from).or_else(|| {
            dialog
                .current_item()
                .and_then(|it| it.description.as_deref())
                .filter(|d| !d.is_empty())
                .map(String::from)
        });

        if let Some(text) = text {
            let max_width = dialog_area.width.saturating_sub(4) as usize;
            let truncated: String = text.chars().take(max_width).collect();
            let helper_style = Style::default()
                .fg(theme.line_number_fg)
                .add_modifier(Modifier::ITALIC);
            frame.render_widget(
                Paragraph::new(truncated).style(helper_style),
                Rect::new(
                    dialog_area.x + 2,
                    helper_y,
                    dialog_area.width.saturating_sub(4),
                    1,
                ),
            );
        }
    }

    // Keybinding legend / validation warning on the row below the buttons.
    let is_editing_json = dialog.editing_text && dialog.is_editing_json();
    let (has_invalid_json, is_json_control) = dialog
        .current_item()
        .map(|item| match &item.control {
            SettingControl::Text(state) => (!state.is_valid(), false),
            SettingControl::Json(state) => (!state.is_valid(), is_editing_json),
            _ => (false, false),
        })
        .unwrap_or((false, false));

    let help_area = Rect::new(
        dialog_area.x + 2,
        button_y + 1,
        dialog_area.width.saturating_sub(4),
        1,
    );

    let (text, style) = if has_invalid_json && !is_json_control {
        (
            "⚠ Invalid JSON - fix before leaving field",
            Style::default().fg(theme.diagnostic_warning_fg),
        )
    } else if has_invalid_json {
        (
            "⚠ Invalid JSON",
            Style::default().fg(theme.diagnostic_warning_fg),
        )
    } else if is_json_control {
        (
            "↑↓←→:Move  Enter:Newline  Tab/Esc:Exit",
            Style::default().fg(theme.line_number_fg),
        )
    } else if dialog.editing_text {
        (
            "Enter/Tab:Commit field  Esc:Cancel",
            Style::default().fg(theme.line_number_fg),
        )
    } else {
        // The `●:modified` legend is the only place that explains the row-indicator.
        (
            "↑↓:Navigate  Tab:Fields/Buttons  Enter:Edit/Apply  Ctrl+S:Save  Esc:Cancel  ●:modified",
            Style::default().fg(theme.line_number_fg),
        )
    };
    frame.render_widget(Paragraph::new(text).style(style), help_area);
}

/// Draw the entry-edit dialog into `parent_area`.
fn render_entry_dialog_inner(
    frame: &mut Frame,
    parent_area: Rect,
    dialog: &mut super::entry_dialog::EntryDialogState,
    theme: &Theme,
) {
    let dialog_width = (parent_area.width * 85 / 100).clamp(50, 90);
    let dialog_height = (parent_area.height * 90 / 100).max(15);
    let dialog_x = parent_area.x + (parent_area.width.saturating_sub(dialog_width)) / 2;
    let dialog_y = parent_area.y + (parent_area.height.saturating_sub(dialog_height)) / 2;
    let dialog_area = Rect::new(dialog_x, dialog_y, dialog_width, dialog_height);

    frame.render_widget(Clear, dialog_area);

    // Title shows "• modified" when the form has uncommitted edits.
    let title = if dialog.is_dirty() {
        format!(" {} • modified ", dialog.title)
    } else {
        format!(" {} ", dialog.title)
    };
    let border_color = if dialog.is_dirty() {
        theme.diagnostic_warning_fg
    } else {
        theme.popup_border_fg
    };
    let block = Block::default()
        .title(title)
        .borders(Borders::ALL)
        .border_type(BorderType::Rounded)
        .border_style(Style::default().fg(border_color))
        .style(Style::default().bg(theme.popup_bg));
    frame.render_widget(block, dialog_area);

    // Reserve 2 lines at the bottom for the button row + keybinding hint.
    let inner = Rect::new(
        dialog_area.x + 2,
        dialog_area.y + 1,
        dialog_area.width.saturating_sub(4),
        dialog_area.height.saturating_sub(5),
    );

    let max_label_width = (inner.width / 2).max(20);
    let label_col_width = dialog
        .items
        .iter()
        .map(|item| item.name.len() as u16 + 2)
        .filter(|&w| w <= max_label_width)
        .max()
        .unwrap_or(20)
        .min(max_label_width);

    let total_content_height = dialog.total_content_height();
    let viewport_height = inner.height as usize;
    dialog.viewport_height = viewport_height;
    let scroll_offset = dialog.scroll_offset;

    render_entry_items(
        frame,
        dialog_area,
        inner,
        dialog,
        theme,
        label_col_width,
        scroll_offset,
        total_content_height,
        viewport_height,
    );
    render_entry_buttons(frame, dialog_area, dialog, theme);
    render_entry_footer(frame, dialog_area, inner, dialog, theme);
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

    #[test]
    fn truncate_display_width_with_ellipsis_ascii_truncates_to_width() {
        let out = truncate_display_width_with_ellipsis("Plugin: very-long-plugin-name", 18);
        assert_eq!(out, "Plugin: very-lo...");
        assert!(str_width(&out) <= 18);
    }

    #[test]
    fn truncate_display_width_with_ellipsis_handles_tiny_widths() {
        assert_eq!(truncate_display_width_with_ellipsis("abcdef", 0), "");
        assert_eq!(truncate_display_width_with_ellipsis("abcdef", 1), ".");
        assert_eq!(truncate_display_width_with_ellipsis("abcdef", 2), "..");
        assert_eq!(truncate_display_width_with_ellipsis("abcdef", 3), "...");
    }

    #[test]
    fn truncate_display_width_with_ellipsis_multicolumn_does_not_overflow() {
        let out = truncate_display_width_with_ellipsis("Plugin: 你好世界📦📦", 14);
        assert!(out.ends_with("..."));
        assert!(str_width(&out) <= 14, "{out:?} was too wide");
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

    /// Regression for #2765: an *open* settings dropdown must actually paint
    /// its option rows into the frame.
    ///
    /// The shared widget framework (`collect_dropdown`) turns an open
    /// dropdown's option list into a floating screen-level pop-over and
    /// discards the inline `option_rows`. The Settings modal does not draw
    /// those floating pop-overs — it reserves inline rows for the open list —
    /// so rendering through `render_scalar_via_widget` alone left the reserved
    /// rows blank and the dropdown opened to an empty box (the Theme and every
    /// other dynamic dropdown showed no options at runtime).
    ///
    /// This drives the real `render_control` paint path (not a hand-built
    /// widget spec) and asserts the option names land in the painted buffer
    /// and that per-option hit rects are produced.
    #[test]
    fn open_dropdown_paints_option_rows() {
        use crate::view::controls::DropdownState;
        use crate::view::theme::{self, Theme};
        use ratatui::backend::TestBackend;
        use ratatui::Terminal;

        // An open dropdown with distinctive display names (mirrors the Theme
        // dropdown: display != stored value, e.g. a user theme).
        let mut dd = DropdownState::with_values(
            vec![
                "dark".to_string(),
                "light".to_string(),
                "my-cool-theme".to_string(),
            ],
            vec![
                "builtin://dark".to_string(),
                "builtin://light".to_string(),
                "my-cool-theme.json".to_string(),
            ],
            "Theme",
        )
        .with_selected(0);
        dd.open = true;
        let control = SettingControl::Dropdown(dd);

        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let prev = std::collections::HashMap::new();

        let width = 60u16;
        let height = 6u16;
        let backend = TestBackend::new(width, height);
        let mut terminal = Terminal::new(backend).unwrap();

        let mut layout: Option<ControlLayoutInfo> = None;
        terminal
            .draw(|frame| {
                let area = Rect::new(0, 0, width, height);
                layout = Some(render_control(
                    frame,
                    area,
                    &control,
                    "/theme",
                    0,
                    &theme,
                    Some(10),
                    false,
                    false,
                    &prev,
                ));
            })
            .unwrap();

        // The open list must yield one hit rect per option row.
        match layout {
            Some(ControlLayoutInfo::Dropdown { option_areas, .. }) => {
                assert_eq!(
                    option_areas.len(),
                    3,
                    "open dropdown should expose one hit rect per option row"
                );
            }
            other => panic!("expected Dropdown layout, got {other:?}"),
        }

        // And every option's display name must appear somewhere in the
        // painted buffer — the pre-fix code painted only the button row, so
        // the option names were absent.
        let buffer = terminal.backend().buffer().clone();
        let screen: String = (0..height)
            .map(|y| {
                (0..width)
                    .map(|x| buffer[(x, y)].symbol().to_string())
                    .collect::<String>()
            })
            .collect::<Vec<_>>()
            .join("\n");
        for name in ["dark", "light", "my-cool-theme"] {
            assert!(
                screen.contains(name),
                "option {name:?} not painted in open dropdown; screen was:\n{screen}"
            );
        }
    }
}
