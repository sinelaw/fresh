//! File browser popup renderer for the Open File dialog
//!
//! Renders a structured popup above the prompt with:
//! - Navigation shortcuts (parent, root, home)
//! - Sortable column headers (name, size, modified)
//! - File list with metadata
//! - Scrollbar for long lists

use super::scrollbar::{render_scrollbar, ScrollbarColors, ScrollbarState};
use super::status_bar::truncate_path;
use crate::app::file_open::{
    format_modified, format_size, FileOpenSection, FileOpenState, SortMode,
};
use crate::primitives::display_width::str_width;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Paragraph};
use ratatui::Frame;
use rust_i18n::t;

/// Renderer for the file browser popup
pub struct FileBrowserRenderer;

impl FileBrowserRenderer {
    /// Render the file browser popup
    ///
    /// # Arguments
    /// * `frame` - The ratatui frame to render to
    /// * `area` - The rectangular area for the popup (above the prompt)
    /// * `state` - The file open dialog state
    /// * `theme` - The active theme for colors
    /// * `hover_target` - Current mouse hover target (for highlighting)
    /// * `keybindings` - Optional keybinding resolver for displaying shortcuts
    ///
    /// # Arguments (cont.)
    /// * `draw` - Paint cells, or only compute the layout. A frontend that
    ///   renders this popup itself (the web renders it natively from
    ///   `Editor::file_browser_view`) passes `false`: every rect, span and
    ///   viewport below is still computed — the projection and the mouse
    ///   hit-tests read them — but nothing is painted, so no TUI cells bleed
    ///   behind the native card. The TUI always passes `true`.
    ///
    /// # Returns
    /// Information for mouse hit testing (scrollbar area, thumb positions, etc.)
    pub fn render(
        frame: &mut Frame,
        area: Rect,
        state: &mut FileOpenState,
        theme: &crate::view::theme::Theme,
        hover_target: &Option<crate::app::HoverTarget>,
        keybindings: Option<&crate::input::keybindings::KeybindingResolver>,
        draw: bool,
    ) -> Option<FileBrowserLayout> {
        if area.height < 5 || area.width < 20 {
            return None;
        }

        // Clear the area behind the popup
        if draw {
            frame.render_widget(Clear, area);
        }

        // Truncate path for title if needed (leave space for borders and padding)
        let max_title_len = (area.width as usize).saturating_sub(4); // 2 for borders, 2 for padding
        let truncated_path = truncate_path(&state.current_dir, max_title_len);
        let title = format!(" {} ", truncated_path.to_string_plain());

        // Create styled title with highlighted [...] if truncated
        let title_line = if truncated_path.truncated {
            Line::from(vec![
                Span::raw(" "),
                Span::styled(
                    truncated_path.prefix.clone(),
                    Style::default().fg(theme.popup_border_fg),
                ),
                Span::styled(
                    format!("{}[...]", truncated_path.sep),
                    Style::default().fg(theme.menu_highlight_fg),
                ),
                Span::styled(
                    truncated_path.suffix.clone(),
                    Style::default().fg(theme.popup_border_fg),
                ),
                Span::raw(" "),
            ])
        } else {
            Line::from(title)
        };

        // Create the popup block with border
        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(theme.popup_border_fg))
            .style(Style::default().bg(theme.popup_bg))
            .title(title_line);

        let inner_area = block.inner(area);
        if draw {
            frame.render_widget(block, area);
        }

        if inner_area.height < 3 || inner_area.width < 10 {
            return None;
        }

        // Layout: Navigation (2-3 rows) | Header (1 row) | File list (remaining) | Scrollbar (1 col)
        let nav_height = 2u16; // Navigation shortcuts section
        let header_height = 1u16;
        let scrollbar_width = 1u16;

        let content_width = inner_area.width.saturating_sub(scrollbar_width);
        let list_height = inner_area.height.saturating_sub(nav_height + header_height);

        // Navigation area
        let nav_area = Rect::new(inner_area.x, inner_area.y, content_width, nav_height);

        // Header area
        let header_area = Rect::new(
            inner_area.x,
            inner_area.y + nav_height,
            content_width,
            header_height,
        );

        // File list area
        let list_area = Rect::new(
            inner_area.x,
            inner_area.y + nav_height + header_height,
            content_width,
            list_height,
        );

        // Scrollbar area
        let scrollbar_area = Rect::new(
            inner_area.x + content_width,
            inner_area.y + nav_height + header_height,
            scrollbar_width,
            list_height,
        );

        // Render each section with hover state. Each one reports back the cell
        // spans it laid its interactive elements out at, so the hit-tests (and
        // the web projection) read the real positions instead of re-deriving
        // them from hardcoded label widths — those were wrong the moment a
        // label was localized or a keybinding string changed length.
        let (toggle_spans, shortcut_spans) = Self::render_navigation(
            frame,
            nav_area,
            state,
            theme,
            hover_target,
            keybindings,
            draw,
        );
        let column_spans =
            Self::render_header(frame, header_area, state, theme, hover_target, draw);
        let visible_rows =
            Self::render_file_list(frame, list_area, state, theme, hover_target, draw);

        // Render scrollbar with theme colors (hover-aware)
        let scrollbar_state =
            ScrollbarState::new(state.entries.len(), visible_rows, state.scroll_offset);
        let is_scrollbar_hovered = matches!(
            hover_target,
            Some(crate::app::HoverTarget::FileBrowserScrollbar)
        );
        let colors = if is_scrollbar_hovered {
            ScrollbarColors::from_theme_hover(theme)
        } else {
            ScrollbarColors::from_theme(theme)
        };
        let (thumb_start, thumb_end) = if draw {
            render_scrollbar(frame, scrollbar_area, &scrollbar_state, &colors)
        } else {
            // Same geometry the painted path would produce — the native
            // frontend draws its own thumb from these numbers.
            let (start, size) = scrollbar_state.thumb_geometry(scrollbar_area.height as usize);
            (start, start + size)
        };

        Some(FileBrowserLayout {
            popup_area: area,
            nav_area,
            header_area,
            list_area,
            scrollbar_area,
            thumb_start,
            thumb_end,
            visible_rows,
            content_width,
            toggle_spans,
            shortcut_spans,
            column_spans,
        })
    }

    /// Render navigation shortcuts section with checkboxes on first row.
    ///
    /// Returns the cell spans it laid out: `(toggle spans, shortcut spans)`,
    /// each `(id, x, width)`. Toggles sit on `area.y`, shortcuts on
    /// `area.y + 1`.
    #[allow(clippy::type_complexity)]
    fn render_navigation(
        frame: &mut Frame,
        area: Rect,
        state: &FileOpenState,
        theme: &crate::view::theme::Theme,
        hover_target: &Option<crate::app::HoverTarget>,
        keybindings: Option<&crate::input::keybindings::KeybindingResolver>,
        draw: bool,
    ) -> (Vec<FileBrowserToggleSpan>, Vec<(usize, u16, u16)>) {
        use crate::app::HoverTarget;

        // Look up keybindings for toggle actions
        let hidden_shortcut = keybindings
            .and_then(|kb| {
                kb.get_keybinding_for_action(
                    &crate::input::keybindings::Action::FileBrowserToggleHidden,
                    crate::input::keybindings::KeyContext::Prompt,
                )
            })
            .unwrap_or_default();

        let encoding_shortcut = keybindings
            .and_then(|kb| {
                kb.get_keybinding_for_action(
                    &crate::input::keybindings::Action::FileBrowserToggleDetectEncoding,
                    crate::input::keybindings::KeyContext::Prompt,
                )
            })
            .unwrap_or_default();

        // First line: "Show Hidden" and "Detect Encoding" checkboxes
        let mut checkbox_spans = Vec::new();

        // Show Hidden checkbox
        let hidden_icon = if state.show_hidden { "☑" } else { "☐" };
        let hidden_label = format!("{} {}", hidden_icon, t!("file_browser.show_hidden"));
        let hidden_shortcut_text = if hidden_shortcut.is_empty() {
            String::new()
        } else {
            format!(" ({})", hidden_shortcut)
        };

        let is_hidden_hovered = matches!(
            hover_target,
            Some(HoverTarget::FileBrowserShowHiddenCheckbox)
        );
        let hidden_style = if is_hidden_hovered {
            Style::default()
                .fg(theme.menu_hover_fg)
                .bg(theme.menu_hover_bg)
        } else if state.show_hidden {
            Style::default()
                .fg(theme.menu_highlight_fg)
                .bg(theme.popup_bg)
        } else {
            Style::default().fg(theme.help_key_fg).bg(theme.popup_bg)
        };
        let hidden_shortcut_style = if is_hidden_hovered {
            Style::default()
                .fg(theme.menu_hover_fg)
                .bg(theme.menu_hover_bg)
        } else {
            Style::default()
                .fg(theme.help_separator_fg)
                .bg(theme.popup_bg)
        };

        // Span bookkeeping: each interactive element records the range of
        // `checkbox_spans` it occupies; the ranges become cell spans below.
        let hidden_from = checkbox_spans.len();
        checkbox_spans.push(Span::styled(format!(" {}", hidden_label), hidden_style));
        if !hidden_shortcut_text.is_empty() {
            checkbox_spans.push(Span::styled(hidden_shortcut_text, hidden_shortcut_style));
        }
        let hidden_to = checkbox_spans.len();

        // Separator between checkboxes
        checkbox_spans.push(Span::styled(
            " │ ",
            Style::default()
                .fg(theme.help_separator_fg)
                .bg(theme.popup_bg),
        ));

        // Detect Encoding checkbox with underlined E
        let encoding_icon = if state.detect_encoding { "☑" } else { "☐" };
        let is_encoding_hovered = matches!(
            hover_target,
            Some(HoverTarget::FileBrowserDetectEncodingCheckbox)
        );
        let encoding_style = if is_encoding_hovered {
            Style::default()
                .fg(theme.menu_hover_fg)
                .bg(theme.menu_hover_bg)
        } else if state.detect_encoding {
            Style::default()
                .fg(theme.menu_highlight_fg)
                .bg(theme.popup_bg)
        } else {
            Style::default().fg(theme.help_key_fg).bg(theme.popup_bg)
        };
        let encoding_underline_style = if is_encoding_hovered {
            Style::default()
                .fg(theme.menu_hover_fg)
                .bg(theme.menu_hover_bg)
                .add_modifier(Modifier::UNDERLINED)
        } else if state.detect_encoding {
            Style::default()
                .fg(theme.menu_highlight_fg)
                .bg(theme.popup_bg)
                .add_modifier(Modifier::UNDERLINED)
        } else {
            Style::default()
                .fg(theme.help_key_fg)
                .bg(theme.popup_bg)
                .add_modifier(Modifier::UNDERLINED)
        };
        let encoding_shortcut_style = if is_encoding_hovered {
            Style::default()
                .fg(theme.menu_hover_fg)
                .bg(theme.menu_hover_bg)
        } else {
            Style::default()
                .fg(theme.help_separator_fg)
                .bg(theme.popup_bg)
        };

        // "☐ Detect " + "E" (underlined) + "ncoding"
        let encoding_from = checkbox_spans.len();
        checkbox_spans.push(Span::styled(
            format!("{} Detect ", encoding_icon),
            encoding_style,
        ));
        checkbox_spans.push(Span::styled("E", encoding_underline_style));
        checkbox_spans.push(Span::styled("ncoding", encoding_style));

        if !encoding_shortcut.is_empty() {
            checkbox_spans.push(Span::styled(
                format!(" ({})", encoding_shortcut),
                encoding_shortcut_style,
            ));
        }
        let encoding_to = checkbox_spans.len();

        // Turn the recorded index ranges into (x, width) cell spans on the
        // checkbox row.
        let span_cells = |spans: &[Span], from: usize, to: usize| -> (u16, u16) {
            let before: usize = spans[..from].iter().map(|s| str_width(&s.content)).sum();
            let width: usize = spans[from..to].iter().map(|s| str_width(&s.content)).sum();
            (area.x + before as u16, width as u16)
        };
        let (hidden_x, hidden_w) = span_cells(&checkbox_spans, hidden_from, hidden_to);
        let (encoding_x, encoding_w) = span_cells(&checkbox_spans, encoding_from, encoding_to);
        let toggle_spans = vec![
            FileBrowserToggleSpan {
                id: FileBrowserToggle::ShowHidden,
                label: t!("file_browser.show_hidden").to_string(),
                shortcut: (!hidden_shortcut.is_empty()).then(|| hidden_shortcut.clone()),
                active: state.show_hidden,
                x: hidden_x,
                w: hidden_w,
            },
            FileBrowserToggleSpan {
                id: FileBrowserToggle::DetectEncoding,
                label: "Detect Encoding".to_string(),
                shortcut: (!encoding_shortcut.is_empty()).then(|| encoding_shortcut.clone()),
                active: state.detect_encoding,
                x: encoding_x,
                w: encoding_w,
            },
        ];

        // Fill rest of row with background
        let checkbox_line_width: usize = checkbox_spans.iter().map(|s| str_width(&s.content)).sum();
        let remaining = (area.width as usize).saturating_sub(checkbox_line_width);
        if remaining > 0 {
            checkbox_spans.push(Span::styled(
                " ".repeat(remaining),
                Style::default().bg(theme.popup_bg),
            ));
        }
        let checkbox_line = Line::from(checkbox_spans);

        // Second line: Navigation shortcuts
        let is_nav_active = state.active_section == FileOpenSection::Navigation;

        let mut nav_spans = Vec::new();
        nav_spans.push(Span::styled(
            format!(" {}", t!("file_browser.navigation")),
            Style::default()
                .fg(theme.help_separator_fg)
                .bg(theme.popup_bg),
        ));

        let mut shortcut_spans: Vec<(usize, u16, u16)> = Vec::new();
        // Running cell offset within the row, seeded past the "Navigation:"
        // prefix that was just pushed.
        let mut cx = area.x + str_width(&nav_spans[0].content) as u16;
        for (idx, shortcut) in state.shortcuts.iter().enumerate() {
            let is_selected = is_nav_active && idx == state.selected_shortcut;
            let is_hovered =
                matches!(hover_target, Some(HoverTarget::FileBrowserNavShortcut(i)) if *i == idx);

            let style = if is_selected {
                Style::default()
                    .fg(theme.popup_text_fg)
                    .bg(theme.suggestion_selected_bg)
                    .add_modifier(Modifier::BOLD)
            } else if is_hovered {
                Style::default()
                    .fg(theme.menu_hover_fg)
                    .bg(theme.menu_hover_bg)
            } else {
                Style::default().fg(theme.help_key_fg).bg(theme.popup_bg)
            };

            let label = format!(" {} ", shortcut.label);
            let label_w = str_width(&label) as u16;
            shortcut_spans.push((idx, cx, label_w));
            cx += label_w;
            nav_spans.push(Span::styled(label, style));

            if idx < state.shortcuts.len() - 1 {
                let sep = " │ ";
                nav_spans.push(Span::styled(
                    sep,
                    Style::default()
                        .fg(theme.help_separator_fg)
                        .bg(theme.popup_bg),
                ));
                cx += str_width(sep) as u16;
            }
        }

        // Fill rest of navigation row with background
        let nav_line_width: usize = nav_spans.iter().map(|s| str_width(&s.content)).sum();
        let nav_remaining = (area.width as usize).saturating_sub(nav_line_width);
        if nav_remaining > 0 {
            nav_spans.push(Span::styled(
                " ".repeat(nav_remaining),
                Style::default().bg(theme.popup_bg),
            ));
        }
        let nav_line = Line::from(nav_spans);

        if draw {
            let paragraph = Paragraph::new(vec![checkbox_line, nav_line]);
            frame.render_widget(paragraph, area);
        }

        (toggle_spans, shortcut_spans)
    }

    /// Render sortable column headers.
    ///
    /// Returns the cell span `(mode, x, width)` of each sortable column on
    /// `area.y`.
    fn render_header(
        frame: &mut Frame,
        area: Rect,
        state: &FileOpenState,
        theme: &crate::view::theme::Theme,
        hover_target: &Option<crate::app::HoverTarget>,
        draw: bool,
    ) -> Vec<(SortMode, u16, u16)> {
        use crate::app::HoverTarget;

        let width = area.width as usize;

        // Column widths
        let size_col_width = 10;
        let date_col_width = 14;
        let name_col_width = width.saturating_sub(size_col_width + date_col_width + 4);

        let header_style = Style::default()
            .fg(theme.help_key_fg)
            .bg(theme.menu_dropdown_bg)
            .add_modifier(Modifier::BOLD);

        let active_header_style = Style::default()
            .fg(theme.menu_highlight_fg)
            .bg(theme.menu_dropdown_bg)
            .add_modifier(Modifier::BOLD);

        let hover_header_style = Style::default()
            .fg(theme.menu_hover_fg)
            .bg(theme.menu_hover_bg)
            .add_modifier(Modifier::BOLD);

        // Sort indicator
        let sort_arrow = if state.sort_ascending { "▲" } else { "▼" };

        let mut spans = Vec::new();

        // Name column
        let name_header = format!(
            " {}{}",
            t!("file_browser.name"),
            if state.sort_mode == SortMode::Name {
                sort_arrow
            } else {
                " "
            }
        );
        let is_name_hovered = matches!(
            hover_target,
            Some(HoverTarget::FileBrowserHeader(SortMode::Name))
        );
        let name_style = if state.sort_mode == SortMode::Name {
            active_header_style
        } else if is_name_hovered {
            hover_header_style
        } else {
            header_style
        };
        let name_display = fit_header_to_col_width(&name_header, name_col_width);
        let name_w = str_width(&name_display) as u16;
        spans.push(Span::styled(name_display, name_style));

        // Size column
        let size_header = format!(
            "{:>width$}",
            format!(
                "{}{}",
                t!("file_browser.size"),
                if state.sort_mode == SortMode::Size {
                    sort_arrow
                } else {
                    " "
                }
            ),
            width = size_col_width
        );
        let is_size_hovered = matches!(
            hover_target,
            Some(HoverTarget::FileBrowserHeader(SortMode::Size))
        );
        let size_style = if state.sort_mode == SortMode::Size {
            active_header_style
        } else if is_size_hovered {
            hover_header_style
        } else {
            header_style
        };
        let size_w = str_width(&size_header) as u16;
        spans.push(Span::styled(size_header, size_style));

        // Separator
        spans.push(Span::styled("  ", header_style));

        // Modified column
        let modified_header = format!(
            "{:>width$}",
            format!(
                "{}{}",
                t!("file_browser.modified"),
                if state.sort_mode == SortMode::Modified {
                    sort_arrow
                } else {
                    " "
                }
            ),
            width = date_col_width
        );
        let is_modified_hovered = matches!(
            hover_target,
            Some(HoverTarget::FileBrowserHeader(SortMode::Modified))
        );
        let modified_style = if state.sort_mode == SortMode::Modified {
            active_header_style
        } else if is_modified_hovered {
            hover_header_style
        } else {
            header_style
        };
        spans.push(Span::styled(modified_header, modified_style));

        if draw {
            let line = Line::from(spans);
            let paragraph = Paragraph::new(vec![line]);
            frame.render_widget(paragraph, area);
        }

        // Column spans, in laid-out order. The Modified column absorbs the
        // two-space separator before it and the rest of the row, so every
        // click on the header row lands on some column exactly as the old
        // width-arithmetic hit-test did.
        let size_x = area.x + name_w;
        let modified_x = size_x + size_w;
        vec![
            (SortMode::Name, area.x, name_w),
            (SortMode::Size, size_x, size_w),
            (
                SortMode::Modified,
                modified_x,
                (area.x + area.width).saturating_sub(modified_x),
            ),
        ]
    }

    /// Render the file list with metadata columns
    ///
    /// Returns the number of visible rows
    fn render_file_list(
        frame: &mut Frame,
        area: Rect,
        state: &mut FileOpenState,
        theme: &crate::view::theme::Theme,
        hover_target: &Option<crate::app::HoverTarget>,
        draw: bool,
    ) -> usize {
        use crate::app::HoverTarget;

        let visible_rows = area.height as usize;
        // Sync scroll/selection with the actual viewport before drawing —
        // input handlers had to guess the height; only the renderer knows it.
        state.update_scroll_for_visible_rows(visible_rows);
        let width = area.width as usize;

        // Column widths (matching header)
        let size_col_width = 10;
        let date_col_width = 14;
        let name_col_width = width.saturating_sub(size_col_width + date_col_width + 4);

        let is_files_active = state.active_section == FileOpenSection::Files;

        // Loading state
        if state.loading {
            let loading_line = Line::from(Span::styled(
                t!("file_browser.loading").to_string(),
                Style::default()
                    .fg(theme.help_separator_fg)
                    .bg(theme.popup_bg),
            ));
            if draw {
                let paragraph = Paragraph::new(vec![loading_line]);
                frame.render_widget(paragraph, area);
            }
            return visible_rows;
        }

        // Error state
        if let Some(error) = &state.error {
            let error_line = Line::from(Span::styled(
                t!("file_browser.error", error = error).to_string(),
                Style::default()
                    .fg(theme.diagnostic_error_fg)
                    .bg(theme.popup_bg),
            ));
            if draw {
                let paragraph = Paragraph::new(vec![error_line]);
                frame.render_widget(paragraph, area);
            }
            return visible_rows;
        }

        // Empty state
        if state.entries.is_empty() {
            let empty_line = Line::from(Span::styled(
                format!(" {}", t!("file_browser.empty")),
                Style::default()
                    .fg(theme.help_separator_fg)
                    .bg(theme.popup_bg),
            ));
            if draw {
                let paragraph = Paragraph::new(vec![empty_line]);
                frame.render_widget(paragraph, area);
            }
            return visible_rows;
        }

        let mut lines = Vec::new();
        let visible_entries = state.visible_entries(visible_rows);

        for (view_idx, entry) in visible_entries.iter().enumerate() {
            let actual_idx = state.scroll_offset + view_idx;
            let is_selected = is_files_active && state.selected_index == Some(actual_idx);
            let is_hovered =
                matches!(hover_target, Some(HoverTarget::FileBrowserEntry(i)) if *i == actual_idx);

            // Base style based on selection, hover, and filter match
            let base_style = if is_selected {
                Style::default()
                    .fg(theme.popup_text_fg)
                    .bg(theme.suggestion_selected_bg)
            } else if is_hovered && entry.matches_filter {
                Style::default()
                    .fg(theme.menu_hover_fg)
                    .bg(theme.menu_hover_bg)
            } else if !entry.matches_filter {
                // Non-matching items are dimmed using the separator color
                Style::default()
                    .fg(theme.help_separator_fg)
                    .bg(theme.popup_bg)
                    .add_modifier(Modifier::DIM)
            } else {
                Style::default().fg(theme.popup_text_fg).bg(theme.popup_bg)
            };

            let mut spans = Vec::new();

            // Name column with trailing type indicator (dirs get /, symlinks get @)
            let name_with_indicator = if entry.fs_entry.is_dir() {
                format!("{}/", entry.fs_entry.name)
            } else if entry.fs_entry.is_symlink() {
                format!("{}@", entry.fs_entry.name)
            } else {
                entry.fs_entry.name.clone()
            };
            let name_display = if name_with_indicator.len() < name_col_width {
                format!("{:<width$}", name_with_indicator, width = name_col_width)
            } else {
                // Truncate with ellipsis
                let truncated: String = name_with_indicator
                    .chars()
                    .take(name_col_width - 3)
                    .collect();
                format!("{}...", truncated)
            };

            // Color directories differently
            let name_style = if entry.fs_entry.is_dir() && !is_selected {
                base_style.fg(theme.help_key_fg)
            } else {
                base_style
            };
            spans.push(Span::styled(name_display, name_style));

            // Size column
            let size_display = if entry.fs_entry.is_dir() {
                format!("{:>width$}", "--", width = size_col_width)
            } else {
                let size = entry
                    .fs_entry
                    .metadata
                    .as_ref()
                    .map(|m| format_size(m.size))
                    .unwrap_or_else(|| "--".to_string());
                format!("{:>width$}", size, width = size_col_width)
            };
            spans.push(Span::styled(size_display, base_style));

            // Separator
            spans.push(Span::styled("  ", base_style));

            // Modified column
            let modified_display = entry
                .fs_entry
                .metadata
                .as_ref()
                .and_then(|m| m.modified)
                .map(format_modified)
                .unwrap_or_else(|| "--".to_string());
            let modified_formatted =
                format!("{:>width$}", modified_display, width = date_col_width);
            spans.push(Span::styled(modified_formatted, base_style));

            lines.push(Line::from(spans));
        }

        // Fill remaining rows with empty lines
        while lines.len() < visible_rows {
            lines.push(Line::from(Span::styled(
                " ".repeat(width),
                Style::default().bg(theme.popup_bg),
            )));
        }

        if draw {
            let paragraph = Paragraph::new(lines);
            frame.render_widget(paragraph, area);
        }

        visible_rows
    }
}

/// Pad or truncate a header string so it occupies exactly `col_width`
/// character positions. Counts characters (not bytes) so headers
/// containing the sort arrow `▲`/`▼` (3 UTF-8 bytes each) or localized
/// labels from `t!()` don't byte-slice through a multi-byte sequence and
/// panic — same class as #1718.
fn fit_header_to_col_width(header: &str, col_width: usize) -> String {
    let chars = header.chars().count();
    if chars < col_width {
        format!("{:<width$}", header, width = col_width)
    } else {
        header.chars().take(col_width).collect()
    }
}

/// One of the file browser's two checkbox toggles, identifying a recorded
/// cell span on the navigation row.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileBrowserToggle {
    ShowHidden,
    DetectEncoding,
}

impl FileBrowserToggle {
    /// Stable name for the scene projection / frontend.
    pub fn name(self) -> &'static str {
        match self {
            FileBrowserToggle::ShowHidden => "showHidden",
            FileBrowserToggle::DetectEncoding => "detectEncoding",
        }
    }
}

/// A checkbox toggle as the renderer laid it out: its state, its resolved
/// label and keybinding hint, and the cells it occupies. The renderer is the
/// only place that knows the localized label and the live shortcut string, so
/// it records them here for the hit-test AND for the web projection instead of
/// either one re-deriving them.
#[derive(Debug, Clone)]
pub struct FileBrowserToggleSpan {
    pub id: FileBrowserToggle,
    pub label: String,
    pub shortcut: Option<String>,
    pub active: bool,
    pub x: u16,
    pub w: u16,
}

/// Layout information for mouse hit testing
#[derive(Debug, Clone)]
pub struct FileBrowserLayout {
    /// The overall popup area (including borders)
    pub popup_area: Rect,
    /// Navigation shortcuts area
    pub nav_area: Rect,
    /// Column headers area
    pub header_area: Rect,
    /// File list area
    pub list_area: Rect,
    /// Scrollbar area
    pub scrollbar_area: Rect,
    /// Scrollbar thumb start position
    pub thumb_start: usize,
    /// Scrollbar thumb end position
    pub thumb_end: usize,
    /// Number of visible rows in the file list
    pub visible_rows: usize,
    /// Width of the content area (for checkbox position calculation)
    pub content_width: u16,
    /// Cell span `(toggle, x, width)` of each checkbox, as the renderer laid
    /// it out on `nav_area.y`. Recorded rather than recomputed: the labels are
    /// localized and carry live keybinding strings, so their widths are not
    /// knowable ahead of the render.
    pub toggle_spans: Vec<FileBrowserToggleSpan>,
    /// Cell span `(index, x, width)` of each navigation shortcut, on
    /// `nav_area.y + 1`.
    pub shortcut_spans: Vec<(usize, u16, u16)>,
    /// Cell span `(sort mode, x, width)` of each sortable column header, on
    /// `header_area.y`.
    pub column_spans: Vec<(SortMode, u16, u16)>,
}

impl FileBrowserLayout {
    /// Check if a position is within the overall popup area (including borders)
    pub fn contains(&self, x: u16, y: u16) -> bool {
        x >= self.popup_area.x
            && x < self.popup_area.x + self.popup_area.width
            && y >= self.popup_area.y
            && y < self.popup_area.y + self.popup_area.height
    }

    /// Check if a position is within the file list area
    pub fn is_in_list(&self, x: u16, y: u16) -> bool {
        x >= self.list_area.x
            && x < self.list_area.x + self.list_area.width
            && y >= self.list_area.y
            && y < self.list_area.y + self.list_area.height
    }

    /// Convert a click in the list area to an entry index
    pub fn click_to_index(&self, y: u16, scroll_offset: usize) -> Option<usize> {
        if y < self.list_area.y || y >= self.list_area.y + self.list_area.height {
            return None;
        }
        let row = (y - self.list_area.y) as usize;
        Some(scroll_offset + row)
    }

    /// Check if a position is in the navigation area
    pub fn is_in_nav(&self, x: u16, y: u16) -> bool {
        x >= self.nav_area.x
            && x < self.nav_area.x + self.nav_area.width
            && y >= self.nav_area.y
            && y < self.nav_area.y + self.nav_area.height
    }

    /// Determine which navigation shortcut is at `x`, from the spans the
    /// renderer recorded. Shortcuts sit on the second row of the nav area
    /// (`nav_area.y + 1`); the "Navigation:" prefix is not a shortcut and has
    /// no span, so clicks on it fall through to `None`.
    pub fn nav_shortcut_at(&self, x: u16, y: u16) -> Option<usize> {
        if y != self.nav_area.y + 1 {
            return None;
        }
        self.shortcut_spans
            .iter()
            .find(|(_, sx, w)| x >= *sx && x < sx.saturating_add(*w))
            .map(|(idx, _, _)| *idx)
    }

    /// Check if a position is in the header area (for sorting)
    pub fn is_in_header(&self, x: u16, y: u16) -> bool {
        x >= self.header_area.x
            && x < self.header_area.x + self.header_area.width
            && y >= self.header_area.y
            && y < self.header_area.y + self.header_area.height
    }

    /// Determine which column header is at `x`, from the spans the renderer
    /// recorded. The Modified column's span runs to the end of the header row,
    /// so any click on the row resolves to a column.
    pub fn header_column_at(&self, x: u16) -> Option<SortMode> {
        self.column_spans
            .iter()
            .find(|(_, sx, w)| x >= *sx && x < sx.saturating_add(*w))
            .map(|(mode, _, _)| *mode)
    }

    /// Check if a position is in the scrollbar area
    pub fn is_in_scrollbar(&self, x: u16, y: u16) -> bool {
        x >= self.scrollbar_area.x
            && x < self.scrollbar_area.x + self.scrollbar_area.width
            && y >= self.scrollbar_area.y
            && y < self.scrollbar_area.y + self.scrollbar_area.height
    }

    /// Check if a position is in the scrollbar thumb
    pub fn is_in_thumb(&self, y: u16) -> bool {
        let rel_y = y.saturating_sub(self.scrollbar_area.y) as usize;
        rel_y >= self.thumb_start && rel_y < self.thumb_end
    }

    /// Which checkbox toggle is at `(x, y)`, from the spans the renderer
    /// recorded on the checkbox row.
    pub fn toggle_at(&self, x: u16, y: u16) -> Option<FileBrowserToggle> {
        if y != self.nav_area.y {
            return None;
        }
        self.toggle_spans
            .iter()
            .find(|t| x >= t.x && x < t.x.saturating_add(t.w))
            .map(|t| t.id)
    }

    /// Check if a position is on the "Show Hidden" checkbox
    pub fn is_on_show_hidden_checkbox(&self, x: u16, y: u16) -> bool {
        self.toggle_at(x, y) == Some(FileBrowserToggle::ShowHidden)
    }

    /// Check if a position is on the "Detect Encoding" checkbox
    pub fn is_on_detect_encoding_checkbox(&self, x: u16, y: u16) -> bool {
        self.toggle_at(x, y) == Some(FileBrowserToggle::DetectEncoding)
    }
}

#[cfg(test)]
mod tests {
    use super::fit_header_to_col_width;
    use super::*;

    /// A layout with hand-written spans standing in for a render, so the
    /// hit-tests can be exercised without a frame.
    fn layout(
        toggles: Vec<FileBrowserToggleSpan>,
        shortcuts: Vec<(usize, u16, u16)>,
    ) -> FileBrowserLayout {
        FileBrowserLayout {
            popup_area: Rect::new(0, 10, 80, 20),
            nav_area: Rect::new(1, 11, 78, 2),
            header_area: Rect::new(1, 13, 78, 1),
            list_area: Rect::new(1, 14, 78, 15),
            scrollbar_area: Rect::new(79, 14, 1, 15),
            thumb_start: 0,
            thumb_end: 3,
            visible_rows: 15,
            content_width: 78,
            toggle_spans: toggles,
            shortcut_spans: shortcuts,
            column_spans: vec![
                (SortMode::Name, 1, 50),
                (SortMode::Size, 51, 10),
                (SortMode::Modified, 61, 18),
            ],
        }
    }

    fn toggle(id: FileBrowserToggle, x: u16, w: u16) -> FileBrowserToggleSpan {
        FileBrowserToggleSpan {
            id,
            label: String::new(),
            shortcut: None,
            active: false,
            x,
            w,
        }
    }

    /// The toggles are hit at the cells the renderer recorded — not at the
    /// fixed 24/27/28-character offsets the old hit-test assumed, which broke
    /// as soon as a label was localized or a keybinding string changed length.
    #[test]
    fn toggles_hit_test_at_their_recorded_spans() {
        let l = layout(
            vec![
                toggle(FileBrowserToggle::ShowHidden, 1, 30),
                toggle(FileBrowserToggle::DetectEncoding, 34, 40),
            ],
            vec![],
        );
        assert!(l.is_on_show_hidden_checkbox(1, 11));
        assert!(l.is_on_show_hidden_checkbox(30, 11));
        // 31..34 is the separator between them: neither toggle.
        assert!(!l.is_on_show_hidden_checkbox(31, 11));
        assert!(!l.is_on_detect_encoding_checkbox(31, 11));
        // A wide (e.g. localized) label is still hit across its full width —
        // the old 24-char assumption stopped at x=25.
        assert!(l.is_on_detect_encoding_checkbox(34, 11));
        assert!(l.is_on_detect_encoding_checkbox(70, 11));
        assert!(!l.is_on_detect_encoding_checkbox(74, 11));
        // Wrong row: the shortcut row, not the checkbox row.
        assert!(!l.is_on_show_hidden_checkbox(1, 12));
    }

    #[test]
    fn nav_shortcuts_hit_test_at_their_recorded_spans() {
        let l = layout(vec![], vec![(0, 14, 4), (1, 21, 3), (2, 27, 3)]);
        assert_eq!(l.nav_shortcut_at(14, 12), Some(0));
        assert_eq!(l.nav_shortcut_at(17, 12), Some(0));
        assert_eq!(l.nav_shortcut_at(21, 12), Some(1));
        assert_eq!(l.nav_shortcut_at(28, 12), Some(2));
        // Gaps (separators) and the "Navigation:" prefix hit nothing.
        assert_eq!(l.nav_shortcut_at(19, 12), None);
        assert_eq!(l.nav_shortcut_at(2, 12), None);
        // Shortcuts live on the second nav row only.
        assert_eq!(l.nav_shortcut_at(14, 11), None);
    }

    #[test]
    fn header_columns_hit_test_at_their_recorded_spans() {
        let l = layout(vec![], vec![]);
        assert_eq!(l.header_column_at(1), Some(SortMode::Name));
        assert_eq!(l.header_column_at(50), Some(SortMode::Name));
        assert_eq!(l.header_column_at(51), Some(SortMode::Size));
        assert_eq!(l.header_column_at(60), Some(SortMode::Size));
        assert_eq!(l.header_column_at(61), Some(SortMode::Modified));
        // The Modified span runs to the end of the row, so every click on the
        // header lands on a column.
        assert_eq!(l.header_column_at(78), Some(SortMode::Modified));
    }

    #[test]
    fn fit_header_pads_when_short() {
        assert_eq!(fit_header_to_col_width("Name", 8), "Name    ");
    }

    #[test]
    fn fit_header_truncates_ascii() {
        assert_eq!(fit_header_to_col_width("Filename▲", 4), "File");
    }

    #[test]
    fn fit_header_truncates_with_sort_arrow_does_not_panic() {
        // Regression: header ` Name ▲` is 9 bytes (`▲` = 3 bytes) but
        // 7 characters. Under the old byte-based code, col_width=7 would
        // byte-slice at index 7 — inside the 3-byte UTF-8 sequence for
        // `▲` — and panic the editor (same class as #1718). Now the
        // header is truncated by character count.
        let out = fit_header_to_col_width(" Name ▲", 7);
        assert_eq!(out, " Name ▲");
        assert_eq!(out.chars().count(), 7);
    }

    #[test]
    fn fit_header_truncates_localized_does_not_panic() {
        // Localized header (e.g. Japanese) where every label char is
        // 3 UTF-8 bytes. Old byte-based truncation at col_width=4 would
        // panic mid-character; character-based truncation keeps 4 chars.
        let out = fit_header_to_col_width(" 名前 ▲", 4);
        assert!(out.is_char_boundary(out.len()));
        assert_eq!(out.chars().count(), 4);
    }
}
