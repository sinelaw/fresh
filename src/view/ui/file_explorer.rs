use crate::view::file_tree::{FileTreeView, NodeId};
use crate::view::theme::Theme;
use ratatui::{
    layout::Rect,
    style::{Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, List, ListItem, ListState},
    Frame,
};

use std::collections::HashSet;
use std::path::PathBuf;

pub struct FileExplorerRenderer;

impl FileExplorerRenderer {
    /// Render the file explorer in the given frame area
    pub fn render(
        view: &mut FileTreeView,
        frame: &mut Frame,
        area: Rect,
        is_focused: bool,
        files_with_unsaved_changes: &HashSet<PathBuf>,
        keybinding_resolver: &crate::input::keybindings::KeybindingResolver,
        current_context: crate::input::keybindings::KeyContext,
        theme: &Theme,
    ) {
        // Update viewport height for scrolling calculations
        // Account for borders (top + bottom = 2)
        let viewport_height = area.height.saturating_sub(2) as usize;
        view.set_viewport_height(viewport_height);

        let display_nodes = view.get_display_nodes();
        let scroll_offset = view.get_scroll_offset();
        let selected_index = view.get_selected_index();

        // Only render the visible subset of items (for manual scroll control)
        // This prevents ratatui's List widget from auto-scrolling
        let visible_end = (scroll_offset + viewport_height).min(display_nodes.len());
        let visible_items = &display_nodes[scroll_offset..visible_end];

        // Create list items for visible nodes only
        let items: Vec<ListItem> = visible_items
            .iter()
            .enumerate()
            .map(|(viewport_idx, &(node_id, indent))| {
                // The actual index in the full list
                let actual_idx = scroll_offset + viewport_idx;
                let is_selected = selected_index == Some(actual_idx);
                Self::render_node(
                    view,
                    node_id,
                    indent,
                    is_selected,
                    is_focused,
                    files_with_unsaved_changes,
                    theme,
                )
            })
            .collect();

        // Build the title with keybinding
        let title = if let Some(keybinding) = keybinding_resolver.get_keybinding_for_action(
            &crate::input::keybindings::Action::ToggleFileExplorer,
            current_context,
        ) {
            format!(" File Explorer ({}) ", keybinding)
        } else {
            " File Explorer ".to_string()
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
                    .border_style(border_style),
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
    fn render_node(
        view: &FileTreeView,
        node_id: NodeId,
        indent: usize,
        is_selected: bool,
        is_focused: bool,
        files_with_unsaved_changes: &HashSet<PathBuf>,
        theme: &Theme,
    ) -> ListItem<'static> {
        let node = view.tree().get_node(node_id).expect("Node should exist");

        // Build the line with indentation and tree structure
        let mut spans = Vec::new();

        // Indentation
        if indent > 0 {
            spans.push(Span::raw("  ".repeat(indent)));
        }

        // Tree expansion indicator (only for directories)
        if node.is_dir() {
            let indicator = if node.is_expanded() {
                "▼ "
            } else if node.is_collapsed() {
                "▶ "
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
            // For files, show unsaved change indicator if applicable
            if files_with_unsaved_changes.contains(&node.entry.path) {
                spans.push(Span::styled(
                    "● ",
                    Style::default().fg(theme.diagnostic_warning_fg),
                ));
            } else {
                spans.push(Span::raw("  "));
            }
        }

        // Name styling using theme colors
        let name_style = if is_selected && is_focused {
            Style::default().fg(theme.editor_fg)
        } else if node
            .entry
            .metadata
            .as_ref()
            .map(|m| m.is_hidden)
            .unwrap_or(false)
        {
            Style::default().fg(theme.line_number_fg)
        } else if node.is_dir() {
            Style::default().fg(theme.syntax_keyword)
        } else {
            Style::default().fg(theme.editor_fg)
        };

        spans.push(Span::styled(node.entry.name.clone(), name_style));

        // Size info for files
        if node.is_file() {
            if let Some(metadata) = &node.entry.metadata {
                if let Some(size) = metadata.size {
                    let size_str = format!(" ({})", Self::format_size(size));
                    spans.push(Span::styled(
                        size_str,
                        Style::default().fg(theme.line_number_fg),
                    ));
                }
            }
        }

        // Error indicator
        if node.is_error() {
            spans.push(Span::styled(
                " [Error]",
                Style::default().fg(theme.diagnostic_error_fg),
            ));
        }

        ListItem::new(Line::from(spans))
    }

    /// Format file size for display
    fn format_size(size: u64) -> String {
        const KB: u64 = 1024;
        const MB: u64 = KB * 1024;
        const GB: u64 = MB * 1024;

        if size >= GB {
            format!("{:.2} GB", size as f64 / GB as f64)
        } else if size >= MB {
            format!("{:.2} MB", size as f64 / MB as f64)
        } else if size >= KB {
            format!("{:.2} KB", size as f64 / KB as f64)
        } else {
            format!("{} B", size)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_size() {
        assert_eq!(FileExplorerRenderer::format_size(500), "500 B");
        assert_eq!(FileExplorerRenderer::format_size(1024), "1.00 KB");
        assert_eq!(FileExplorerRenderer::format_size(1536), "1.50 KB");
        assert_eq!(FileExplorerRenderer::format_size(1024 * 1024), "1.00 MB");
        assert_eq!(
            FileExplorerRenderer::format_size(1024 * 1024 * 1024),
            "1.00 GB"
        );
    }
}
