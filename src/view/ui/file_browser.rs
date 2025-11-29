//! File browser popup renderer for the Open File dialog
//!
//! Renders a structured popup above the prompt with:
//! - Navigation shortcuts (parent, root, home)
//! - Sortable column headers (name, size, modified)
//! - File list with metadata
//! - Scrollbar for long lists

use super::scrollbar::{render_scrollbar, ScrollbarColors, ScrollbarState};
use crate::app::file_open::{
    format_modified, format_size, FileOpenSection, FileOpenState, SortMode,
};
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Paragraph};
use ratatui::Frame;

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
    ///
    /// # Returns
    /// Information for mouse hit testing (scrollbar area, thumb positions, etc.)
    pub fn render(
        frame: &mut Frame,
        area: Rect,
        state: &FileOpenState,
        theme: &crate::view::theme::Theme,
        hover_target: &Option<crate::app::HoverTarget>,
    ) -> Option<FileBrowserLayout> {
        if area.height < 5 || area.width < 20 {
            return None;
        }

        // Clear the area behind the popup
        frame.render_widget(Clear, area);

        // Create the popup block with border
        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(theme.popup_border_fg))
            .style(Style::default().bg(theme.popup_bg))
            .title(format!(" {} ", state.current_dir.display()));

        let inner_area = block.inner(area);
        frame.render_widget(block, area);

        if inner_area.height < 3 || inner_area.width < 10 {
            return None;
        }

        // Layout: Navigation (2-3 rows) | Header (1 row) | File list (remaining) | Scrollbar (1 col)
        let nav_height = 2u16; // Navigation shortcuts section
        let header_height = 1u16;
        let scrollbar_width = 1u16;

        let content_width = inner_area.width.saturating_sub(scrollbar_width);
        let list_height = inner_area
            .height
            .saturating_sub(nav_height + header_height);

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

        // Render each section with hover state
        Self::render_navigation(frame, nav_area, state, theme, hover_target);
        Self::render_header(frame, header_area, state, theme, hover_target);
        let visible_rows = Self::render_file_list(frame, list_area, state, theme, hover_target);

        // Render scrollbar with theme colors (hover-aware)
        let scrollbar_state = ScrollbarState::new(
            state.entries.len(),
            visible_rows,
            state.scroll_offset,
        );
        let is_scrollbar_hovered = matches!(
            hover_target,
            Some(crate::app::HoverTarget::FileBrowserScrollbar)
        );
        let colors = if is_scrollbar_hovered {
            ScrollbarColors::from_theme_hover(theme)
        } else {
            ScrollbarColors::from_theme(theme)
        };
        let (thumb_start, thumb_end) = render_scrollbar(frame, scrollbar_area, &scrollbar_state, &colors);

        Some(FileBrowserLayout {
            nav_area,
            header_area,
            list_area,
            scrollbar_area,
            thumb_start,
            thumb_end,
            visible_rows,
        })
    }

    /// Render navigation shortcuts section
    fn render_navigation(
        frame: &mut Frame,
        area: Rect,
        state: &FileOpenState,
        theme: &crate::view::theme::Theme,
        hover_target: &Option<crate::app::HoverTarget>,
    ) {
        use crate::app::HoverTarget;

        let is_nav_active = state.active_section == FileOpenSection::Navigation;

        let mut spans = Vec::new();
        spans.push(Span::styled(
            " Navigation: ",
            Style::default()
                .fg(theme.help_separator_fg)
                .bg(theme.popup_bg),
        ));

        for (idx, shortcut) in state.shortcuts.iter().enumerate() {
            let is_selected = is_nav_active && idx == state.selected_shortcut;
            let is_hovered = matches!(hover_target, Some(HoverTarget::FileBrowserNavShortcut(i)) if *i == idx);

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
                Style::default()
                    .fg(theme.help_key_fg)
                    .bg(theme.popup_bg)
            };

            spans.push(Span::styled(format!(" {} ", shortcut.label), style));

            if idx < state.shortcuts.len() - 1 {
                spans.push(Span::styled(
                    " │ ",
                    Style::default()
                        .fg(theme.help_separator_fg)
                        .bg(theme.popup_bg),
                ));
            }
        }

        // Fill remaining width
        let current_width: usize = spans.iter().map(|s| s.content.len()).sum();
        if current_width < area.width as usize {
            spans.push(Span::styled(
                " ".repeat(area.width as usize - current_width),
                Style::default().bg(theme.popup_bg),
            ));
        }

        let line = Line::from(spans);
        let paragraph = Paragraph::new(vec![line]);
        frame.render_widget(paragraph, area);
    }

    /// Render sortable column headers
    fn render_header(
        frame: &mut Frame,
        area: Rect,
        state: &FileOpenState,
        theme: &crate::view::theme::Theme,
        hover_target: &Option<crate::app::HoverTarget>,
    ) {
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
        let name_header = format!(" Name{}", if state.sort_mode == SortMode::Name { sort_arrow } else { " " });
        let is_name_hovered = matches!(hover_target, Some(HoverTarget::FileBrowserHeader(SortMode::Name)));
        let name_style = if state.sort_mode == SortMode::Name {
            active_header_style
        } else if is_name_hovered {
            hover_header_style
        } else {
            header_style
        };
        let name_display = if name_header.len() < name_col_width {
            format!("{:<width$}", name_header, width = name_col_width)
        } else {
            name_header[..name_col_width].to_string()
        };
        spans.push(Span::styled(name_display, name_style));

        // Size column
        let size_header = format!(
            "{:>width$}",
            format!("Size{}", if state.sort_mode == SortMode::Size { sort_arrow } else { " " }),
            width = size_col_width
        );
        let is_size_hovered = matches!(hover_target, Some(HoverTarget::FileBrowserHeader(SortMode::Size)));
        let size_style = if state.sort_mode == SortMode::Size {
            active_header_style
        } else if is_size_hovered {
            hover_header_style
        } else {
            header_style
        };
        spans.push(Span::styled(size_header, size_style));

        // Separator
        spans.push(Span::styled("  ", header_style));

        // Modified column
        let modified_header = format!(
            "{:>width$}",
            format!("Modified{}", if state.sort_mode == SortMode::Modified { sort_arrow } else { " " }),
            width = date_col_width
        );
        let is_modified_hovered = matches!(hover_target, Some(HoverTarget::FileBrowserHeader(SortMode::Modified)));
        let modified_style = if state.sort_mode == SortMode::Modified {
            active_header_style
        } else if is_modified_hovered {
            hover_header_style
        } else {
            header_style
        };
        spans.push(Span::styled(modified_header, modified_style));

        let line = Line::from(spans);
        let paragraph = Paragraph::new(vec![line]);
        frame.render_widget(paragraph, area);
    }

    /// Render the file list with metadata columns
    ///
    /// Returns the number of visible rows
    fn render_file_list(
        frame: &mut Frame,
        area: Rect,
        state: &FileOpenState,
        theme: &crate::view::theme::Theme,
        hover_target: &Option<crate::app::HoverTarget>,
    ) -> usize {
        use crate::app::HoverTarget;

        let visible_rows = area.height as usize;
        let width = area.width as usize;

        // Column widths (matching header)
        let size_col_width = 10;
        let date_col_width = 14;
        let name_col_width = width.saturating_sub(size_col_width + date_col_width + 4);

        let is_files_active = state.active_section == FileOpenSection::Files;

        // Loading state
        if state.loading {
            let loading_line = Line::from(Span::styled(
                " Loading...",
                Style::default()
                    .fg(theme.help_separator_fg)
                    .bg(theme.popup_bg),
            ));
            let paragraph = Paragraph::new(vec![loading_line]);
            frame.render_widget(paragraph, area);
            return visible_rows;
        }

        // Error state
        if let Some(error) = &state.error {
            let error_line = Line::from(Span::styled(
                format!(" Error: {}", error),
                Style::default()
                    .fg(theme.diagnostic_error_fg)
                    .bg(theme.popup_bg),
            ));
            let paragraph = Paragraph::new(vec![error_line]);
            frame.render_widget(paragraph, area);
            return visible_rows;
        }

        // Empty state
        if state.entries.is_empty() {
            let empty_line = Line::from(Span::styled(
                " (empty directory)",
                Style::default()
                    .fg(theme.help_separator_fg)
                    .bg(theme.popup_bg),
            ));
            let paragraph = Paragraph::new(vec![empty_line]);
            frame.render_widget(paragraph, area);
            return visible_rows;
        }

        let mut lines = Vec::new();
        let visible_entries = state.visible_entries(visible_rows);

        for (view_idx, entry) in visible_entries.iter().enumerate() {
            let actual_idx = state.scroll_offset + view_idx;
            let is_selected = is_files_active && state.selected_index == Some(actual_idx);
            let is_hovered = matches!(hover_target, Some(HoverTarget::FileBrowserEntry(i)) if *i == actual_idx);

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
                Style::default()
                    .fg(theme.popup_text_fg)
                    .bg(theme.popup_bg)
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
                let truncated: String = name_with_indicator.chars().take(name_col_width - 3).collect();
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
                    .and_then(|m| m.size)
                    .map(format_size)
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
            let modified_formatted = format!("{:>width$}", modified_display, width = date_col_width);
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

        let paragraph = Paragraph::new(lines);
        frame.render_widget(paragraph, area);

        visible_rows
    }
}

/// Layout information for mouse hit testing
#[derive(Debug, Clone)]
pub struct FileBrowserLayout {
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
}

impl FileBrowserLayout {
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

    /// Determine which navigation shortcut was clicked based on x position
    /// The layout is: " Navigation: " (13 chars) then for each shortcut: " {label} " + " │ " separator
    pub fn nav_shortcut_at(&self, x: u16, shortcut_labels: &[&str]) -> Option<usize> {
        let rel_x = x.saturating_sub(self.nav_area.x) as usize;

        // Skip " Navigation: " prefix
        let prefix_len = 13;
        if rel_x < prefix_len {
            return None;
        }

        let mut current_x = prefix_len;
        for (idx, label) in shortcut_labels.iter().enumerate() {
            // Each shortcut: " {label} " = label.len() + 2
            let shortcut_width = label.chars().count() + 2;

            if rel_x >= current_x && rel_x < current_x + shortcut_width {
                return Some(idx);
            }
            current_x += shortcut_width;

            // Separator: " │ " = 3 chars
            if idx < shortcut_labels.len() - 1 {
                current_x += 3;
            }
        }

        None
    }

    /// Check if a position is in the header area (for sorting)
    pub fn is_in_header(&self, x: u16, y: u16) -> bool {
        x >= self.header_area.x
            && x < self.header_area.x + self.header_area.width
            && y >= self.header_area.y
            && y < self.header_area.y + self.header_area.height
    }

    /// Determine which column header was clicked
    pub fn header_column_at(&self, x: u16) -> Option<SortMode> {
        let rel_x = x.saturating_sub(self.header_area.x) as usize;
        let width = self.header_area.width as usize;

        let size_col_width = 10;
        let date_col_width = 14;
        let name_col_width = width.saturating_sub(size_col_width + date_col_width + 4);

        if rel_x < name_col_width {
            Some(SortMode::Name)
        } else if rel_x < name_col_width + size_col_width {
            Some(SortMode::Size)
        } else {
            Some(SortMode::Modified)
        }
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
}
