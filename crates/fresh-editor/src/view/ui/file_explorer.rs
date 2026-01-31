use crate::input::fuzzy::FuzzyMatch;
use crate::primitives::display_width::str_width;
use crate::view::file_tree::{FileExplorerDecorationCache, FileTreeView, NodeId};
use crate::view::theme::Theme;
use ratatui::{
    layout::Rect,
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, List, ListItem, ListState},
    Frame,
};

use std::collections::HashSet;
use std::path::PathBuf;

pub struct FileExplorerRenderer;

impl FileExplorerRenderer {
    /// Check if a directory contains any modified files
    fn folder_has_modified_files(
        folder_path: &PathBuf,
        files_with_unsaved_changes: &HashSet<PathBuf>,
    ) -> bool {
        for modified_file in files_with_unsaved_changes {
            if modified_file.starts_with(folder_path) {
                return true;
            }
        }
        false
    }

    /// Render the file explorer in the given frame area
    #[allow(clippy::too_many_arguments)]
    pub fn render(
        view: &mut FileTreeView,
        frame: &mut Frame,
        area: Rect,
        is_focused: bool,
        files_with_unsaved_changes: &HashSet<PathBuf>,
        decorations: &FileExplorerDecorationCache,
        keybinding_resolver: &crate::input::keybindings::KeybindingResolver,
        current_context: crate::input::keybindings::KeyContext,
        theme: &Theme,
        close_button_hovered: bool,
        remote_connection: Option<&str>,
    ) {
        let search_active = view.is_search_active();

        // Update viewport height for scrolling calculations
        // Account for borders (top + bottom = 2)
        let viewport_height = area.height.saturating_sub(2) as usize;
        view.set_viewport_height(viewport_height);

        let display_nodes = view.get_display_nodes();
        let scroll_offset = view.get_scroll_offset();
        let selected_index = view.get_selected_index();

        // Clamp scroll_offset to valid range to prevent panic after tree mutations
        // (e.g., when deleting a folder with many children while scrolled down)
        // Issue #562: scroll_offset can become larger than display_nodes.len()
        let scroll_offset = scroll_offset.min(display_nodes.len());

        // Only render the visible subset of items (for manual scroll control)
        // This prevents ratatui's List widget from auto-scrolling
        let visible_end = (scroll_offset + viewport_height).min(display_nodes.len());
        let visible_items = &display_nodes[scroll_offset..visible_end];

        // Available width for content (subtract borders and cursor indicator)
        let content_width = area.width.saturating_sub(3) as usize;

        // Create list items for visible nodes only
        let items: Vec<ListItem> = visible_items
            .iter()
            .enumerate()
            .map(|(viewport_idx, &(node_id, indent))| {
                // The actual index in the full list
                let actual_idx = scroll_offset + viewport_idx;
                let is_selected = selected_index == Some(actual_idx);
                // Get match positions for highlighting
                let fuzzy_match = if search_active {
                    view.get_match_for_node(node_id)
                } else {
                    None
                };
                Self::render_node(
                    view,
                    node_id,
                    indent,
                    is_selected,
                    is_focused,
                    files_with_unsaved_changes,
                    decorations,
                    theme,
                    content_width,
                    fuzzy_match.as_ref(),
                )
            })
            .collect();

        // Build the title with keybinding and optional remote host
        let keybinding_suffix = keybinding_resolver
            .get_keybinding_for_action(
                &crate::input::keybindings::Action::FocusFileExplorer,
                current_context,
            )
            .map(|kb| format!(" ({})", kb))
            .unwrap_or_default();

        // Show search query in title when search is active
        let title = if search_active {
            format!(" /{} ", view.search_query())
        } else if let Some(host) = remote_connection {
            // Extract just the hostname from "user@host" or "user@host:port"
            let hostname = host
                .split('@')
                .last()
                .unwrap_or(host)
                .split(':')
                .next()
                .unwrap_or(host);
            format!(" [{}]{} ", hostname, keybinding_suffix)
        } else {
            format!(" File Explorer{} ", keybinding_suffix)
        };

        // Title style: inverted colors (dark on light) when focused using theme colors
        let (title_style, border_style) = if is_focused {
            (
                Style::default()
                    .fg(theme.editor_bg)
                    .bg(theme.editor_fg)
                    .add_modifier(Modifier::BOLD),
                Style::default().fg(theme.cursor),
            )
        } else {
            (
                Style::default().fg(theme.line_number_fg),
                Style::default().fg(theme.split_separator_fg),
            )
        };

        // Create the list widget
        let list = List::new(items)
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .title(title)
                    .title_style(title_style)
                    .border_style(border_style)
                    .style(Style::default().bg(theme.editor_bg)),
            )
            .highlight_style(if is_focused {
                Style::default().bg(theme.selection_bg).fg(theme.editor_fg)
            } else {
                Style::default().bg(theme.current_line_bg)
            });

        // Create list state for scrolling
        // Since we're only passing visible items, the selection is relative to viewport
        let mut list_state = ListState::default();
        if let Some(selected) = selected_index {
            if selected >= scroll_offset && selected < scroll_offset + viewport_height {
                // Selected item is in the visible range
                list_state.select(Some(selected - scroll_offset));
            }
        }

        frame.render_stateful_widget(list, area, &mut list_state);

        // Render close button "×" at the right side of the title bar
        let close_button_x = area.x + area.width.saturating_sub(3);
        let close_fg = if close_button_hovered {
            theme.tab_close_hover_fg
        } else {
            theme.line_number_fg
        };
        let close_button =
            ratatui::widgets::Paragraph::new("×").style(Style::default().fg(close_fg));
        let close_area = Rect::new(close_button_x, area.y, 1, 1);
        frame.render_widget(close_button, close_area);

        // When focused, show a blinking cursor indicator at the selected row
        // We render a cursor indicator character and position the hardware cursor there
        // The hardware cursor provides efficient terminal-native blinking
        if is_focused {
            if let Some(selected) = selected_index {
                if selected >= scroll_offset && selected < scroll_offset + viewport_height {
                    // Position at the left edge of the selected row (after border)
                    let cursor_x = area.x + 1;
                    let cursor_y = area.y + 1 + (selected - scroll_offset) as u16;

                    // Render a cursor indicator character that the hardware cursor will blink over
                    let cursor_indicator = ratatui::widgets::Paragraph::new("▌")
                        .style(Style::default().fg(theme.cursor));
                    let cursor_area = ratatui::layout::Rect::new(cursor_x, cursor_y, 1, 1);
                    frame.render_widget(cursor_indicator, cursor_area);

                    // Position hardware cursor here for blinking effect
                    frame.set_cursor_position((cursor_x, cursor_y));
                }
            }
        }
    }

    /// Render a single tree node as a ListItem
    #[allow(clippy::too_many_arguments)]
    fn render_node(
        view: &FileTreeView,
        node_id: NodeId,
        indent: usize,
        is_selected: bool,
        is_focused: bool,
        files_with_unsaved_changes: &HashSet<PathBuf>,
        decorations: &FileExplorerDecorationCache,
        theme: &Theme,
        content_width: usize,
        fuzzy_match: Option<&FuzzyMatch>,
    ) -> ListItem<'static> {
        let node = view.tree().get_node(node_id).expect("Node should exist");

        // Build the line with indentation and tree structure
        let mut spans = Vec::new();

        // Calculate the left side width for padding calculation
        let indent_width = indent * 2;
        let indicator_width = if node.is_dir() { 2 } else { 2 }; // "▼ " or "  "
        let name_width = str_width(&node.entry.name);
        let left_side_width = indent_width + indicator_width + name_width;

        // Indentation
        if indent > 0 {
            spans.push(Span::raw("  ".repeat(indent)));
        }

        // Tree expansion indicator (only for directories)
        if node.is_dir() {
            let indicator = if node.is_expanded() {
                "▼ "
            } else if node.is_collapsed() {
                "> "
            } else if node.is_loading() {
                "⟳ "
            } else {
                "! "
            };
            spans.push(Span::styled(
                indicator,
                Style::default().fg(theme.diagnostic_warning_fg),
            ));
        } else {
            // For files, add spacing to align with directory names
            spans.push(Span::raw("  "));
        }

        // Name styling using theme colors
        let base_fg = if is_selected && is_focused {
            theme.editor_fg
        } else if node
            .entry
            .metadata
            .as_ref()
            .map(|m| m.is_hidden)
            .unwrap_or(false)
        {
            theme.line_number_fg
        } else if node.entry.is_symlink() {
            // Symlinks use a distinct color (type color, typically cyan)
            theme.syntax_type
        } else if node.is_dir() {
            theme.syntax_keyword
        } else {
            theme.editor_fg
        };

        // Render name with match highlighting
        if let Some(fm) = fuzzy_match {
            Self::render_name_with_highlights(
                &node.entry.name,
                &fm.match_positions,
                base_fg,
                theme,
                &mut spans,
            );
        } else {
            spans.push(Span::styled(
                node.entry.name.clone(),
                Style::default().fg(base_fg),
            ));
        }

        // Determine the right-side indicator (status symbol)
        // Priority: unsaved changes > direct decoration > bubbled decoration (for dirs)
        let has_unsaved = if node.is_dir() {
            Self::folder_has_modified_files(&node.entry.path, files_with_unsaved_changes)
        } else {
            files_with_unsaved_changes.contains(&node.entry.path)
        };

        let direct_decoration = decorations.direct_for_path(&node.entry.path);
        let bubbled_decoration = if node.is_dir() {
            decorations
                .bubbled_for_path(&node.entry.path)
                .filter(|_| direct_decoration.is_none())
        } else {
            None
        };

        let right_indicator: Option<(String, Color)> = if has_unsaved {
            Some(("●".to_string(), theme.diagnostic_warning_fg))
        } else if let Some(decoration) = direct_decoration {
            let symbol = Self::decoration_symbol(&decoration.symbol);
            Some((symbol, Self::decoration_color(decoration)))
        } else {
            bubbled_decoration
                .map(|decoration| ("●".to_string(), Self::decoration_color(decoration)))
        };

        // Calculate right-side content width
        let right_indicator_width = right_indicator
            .as_ref()
            .map(|(s, _)| str_width(s))
            .unwrap_or(0);

        // Error indicator
        let error_text = if node.is_error() { " [Error]" } else { "" };
        let error_width = str_width(error_text);

        let total_right_width = right_indicator_width + error_width;

        // Calculate padding for right-alignment
        let min_gap = 1;
        let padding = if left_side_width + min_gap + total_right_width < content_width {
            content_width - left_side_width - total_right_width
        } else {
            min_gap
        };

        spans.push(Span::raw(" ".repeat(padding)));

        // Add right-aligned status indicator
        if let Some((symbol, color)) = right_indicator {
            spans.push(Span::styled(symbol, Style::default().fg(color)));
        }

        // Error indicator
        if node.is_error() {
            spans.push(Span::styled(
                error_text,
                Style::default().fg(theme.diagnostic_error_fg),
            ));
        }

        ListItem::new(Line::from(spans)).style(Style::default().bg(theme.editor_bg))
    }

    fn decoration_symbol(symbol: &str) -> String {
        symbol
            .chars()
            .next()
            .map(|c| c.to_string())
            .unwrap_or_else(|| " ".to_string())
    }

    fn decoration_color(decoration: &crate::view::file_tree::FileExplorerDecoration) -> Color {
        let [r, g, b] = decoration.color;
        Color::Rgb(r, g, b)
    }

    /// Render a file/directory name with matched characters highlighted
    fn render_name_with_highlights(
        name: &str,
        match_positions: &[usize],
        base_fg: Color,
        theme: &Theme,
        spans: &mut Vec<Span<'static>>,
    ) {
        if match_positions.is_empty() {
            spans.push(Span::styled(name.to_string(), Style::default().fg(base_fg)));
            return;
        }

        let chars: Vec<char> = name.chars().collect();
        let match_set: std::collections::HashSet<usize> = match_positions.iter().copied().collect();

        let base_style = Style::default().fg(base_fg);
        let highlight_style = Style::default()
            .fg(theme.search_match_fg)
            .bg(theme.search_match_bg);

        let mut current_span = String::new();
        let mut current_is_match = false;

        for (i, &c) in chars.iter().enumerate() {
            let is_match = match_set.contains(&i);

            if i == 0 {
                current_is_match = is_match;
                current_span.push(c);
            } else if is_match == current_is_match {
                current_span.push(c);
            } else {
                // Style changed, push current span and start new one
                let style = if current_is_match {
                    highlight_style
                } else {
                    base_style
                };
                spans.push(Span::styled(current_span.clone(), style));
                current_span.clear();
                current_span.push(c);
                current_is_match = is_match;
            }
        }

        // Push final span
        if !current_span.is_empty() {
            let style = if current_is_match {
                highlight_style
            } else {
                base_style
            };
            spans.push(Span::styled(current_span, style));
        }
    }
}
