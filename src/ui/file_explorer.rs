use crate::file_tree::{FileTreeView, NodeId};
use crate::fs::FsEntryType;
use ratatui::{
    layout::Rect,
    style::{Color, Style},
    text::{Line, Span},
    widgets::{Block, Borders, List, ListItem, ListState},
    Frame,
};

pub struct FileExplorerRenderer;

impl FileExplorerRenderer {
    /// Render the file explorer in the given frame area
    pub fn render(view: &FileTreeView, frame: &mut Frame, area: Rect, is_focused: bool) {
        let display_nodes = view.get_display_nodes();
        let scroll_offset = view.get_scroll_offset();
        let selected_index = view.get_selected_index();

        // Create list items for visible nodes
        let items: Vec<ListItem> = display_nodes
            .iter()
            .enumerate()
            .map(|(idx, &(node_id, indent))| {
                let is_selected = selected_index == Some(idx);
                Self::render_node(view, node_id, indent, is_selected, is_focused)
            })
            .collect();

        // Create the list widget
        let list = List::new(items)
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .title(" File Explorer ")
                    .border_style(if is_focused {
                        Style::default().fg(Color::Cyan)
                    } else {
                        Style::default()
                    }),
            )
            .highlight_style(if is_focused {
                Style::default().bg(Color::DarkGray).fg(Color::White)
            } else {
                Style::default().bg(Color::DarkGray)
            });

        // Create list state for scrolling
        let mut list_state = ListState::default();
        if let Some(selected) = selected_index {
            list_state.select(Some(selected.saturating_sub(scroll_offset)));
        }

        frame.render_stateful_widget(list, area, &mut list_state);
    }

    /// Render a single tree node as a ListItem
    fn render_node(
        view: &FileTreeView,
        node_id: NodeId,
        indent: usize,
        is_selected: bool,
        is_focused: bool,
    ) -> ListItem<'static> {
        let node = view.tree().get_node(node_id).expect("Node should exist");

        // Build the line with indentation and tree structure
        let mut spans = Vec::new();

        // Indentation
        if indent > 0 {
            spans.push(Span::raw("  ".repeat(indent)));
        }

        // Tree expansion indicator
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
            spans.push(Span::styled(indicator, Style::default().fg(Color::Yellow)));
        } else {
            spans.push(Span::raw("  "));
        }

        // Icon
        let icon = Self::get_icon(&node.entry.entry_type, &node.entry.name);
        spans.push(Span::styled(icon, Self::get_icon_color(&node.entry)));

        // Name
        let name_style = if is_selected && is_focused {
            Style::default().fg(Color::White)
        } else if node
            .entry
            .metadata
            .as_ref()
            .map(|m| m.is_hidden)
            .unwrap_or(false)
        {
            Style::default().fg(Color::DarkGray)
        } else if node.is_dir() {
            Style::default().fg(Color::Cyan)
        } else {
            Style::default().fg(Color::White)
        };

        spans.push(Span::styled(node.entry.name.clone(), name_style));

        // Size info for files
        if node.is_file() {
            if let Some(metadata) = &node.entry.metadata {
                if let Some(size) = metadata.size {
                    let size_str = format!(" ({})", Self::format_size(size));
                    spans.push(Span::styled(size_str, Style::default().fg(Color::DarkGray)));
                }
            }
        }

        // Error indicator
        if node.is_error() {
            spans.push(Span::styled(" [Error]", Style::default().fg(Color::Red)));
        }

        ListItem::new(Line::from(spans))
    }

    /// Get icon for file type
    fn get_icon(entry_type: &FsEntryType, name: &str) -> &'static str {
        match entry_type {
            FsEntryType::Directory => "[D] ",
            FsEntryType::Symlink => "[L] ",
            FsEntryType::File => {
                // Determine icon based on file extension
                if let Some(ext) = name.rsplit('.').next() {
                    match ext.to_lowercase().as_str() {
                        "rs" => "[R] ",
                        "py" => "[P] ",
                        "js" | "ts" | "jsx" | "tsx" => "[J] ",
                        "html" | "htm" => "[H] ",
                        "css" | "scss" | "sass" => "[C] ",
                        "json" | "yaml" | "yml" | "toml" => "[*] ",
                        "md" | "txt" => "[T] ",
                        "jpg" | "jpeg" | "png" | "gif" | "svg" => "[I] ",
                        "mp3" | "wav" | "ogg" => "[A] ",
                        "mp4" | "avi" | "mkv" => "[V] ",
                        "zip" | "tar" | "gz" | "7z" => "[Z] ",
                        "pdf" => "[F] ",
                        "sh" | "bash" | "zsh" => "[S] ",
                        _ => "[F] ",
                    }
                } else {
                    "[F] "
                }
            }
        }
    }

    /// Get color for icon
    fn get_icon_color(entry: &crate::fs::FsEntry) -> Style {
        match entry.entry_type {
            FsEntryType::Directory => Style::default().fg(Color::Blue),
            FsEntryType::Symlink => Style::default().fg(Color::Cyan),
            FsEntryType::File => Style::default().fg(Color::White),
        }
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

    #[test]
    fn test_get_icon() {
        assert_eq!(
            FileExplorerRenderer::get_icon(&FsEntryType::Directory, "test"),
            "[D] "
        );
        assert_eq!(
            FileExplorerRenderer::get_icon(&FsEntryType::Symlink, "test"),
            "[L] "
        );
        assert_eq!(
            FileExplorerRenderer::get_icon(&FsEntryType::File, "test.rs"),
            "[R] "
        );
        assert_eq!(
            FileExplorerRenderer::get_icon(&FsEntryType::File, "test.py"),
            "[P] "
        );
        assert_eq!(
            FileExplorerRenderer::get_icon(&FsEntryType::File, "test.txt"),
            "[T] "
        );
        assert_eq!(
            FileExplorerRenderer::get_icon(&FsEntryType::File, "unknown"),
            "[F] "
        );
    }
}
