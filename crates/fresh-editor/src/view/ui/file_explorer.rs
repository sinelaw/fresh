use crate::app::types::CellThemeRecorder;
use crate::input::fuzzy::FuzzyMatch;
use crate::primitives::display_width::str_width;
use crate::view::file_tree::{
    ExplorerSlotContext, ExplorerSlotResolution, ExplorerSlotResolver, FileExplorerDecorationCache,
    FileExplorerSlotOverrideCache, FileTreeView, NodeId,
};
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

/// The plugin-driven decoration inputs for the file explorer: the slot
/// resolver plus the decoration and slot-override caches. These three always
/// travel together through the render pipeline, so they're bundled rather
/// than threaded as three parallel parameters. `Copy` (the resolver is two
/// `&dyn` pointers, the caches are shared refs), so it passes by value into
/// the per-row closure without cloning.
#[derive(Clone, Copy)]
pub struct ExplorerDecorations<'a> {
    pub slot_resolver: ExplorerSlotResolver<'a>,
    pub decorations: &'a FileExplorerDecorationCache,
    pub slot_overrides: &'a FileExplorerSlotOverrideCache,
}

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
        deco: ExplorerDecorations<'_>,
        is_focused: bool,
        files_with_unsaved_changes: &HashSet<PathBuf>,
        keybinding_resolver: &crate::input::keybindings::KeybindingResolver,
        current_context: crate::input::keybindings::KeyContext,
        theme: &Theme,
        close_button_hovered: bool,
        remote_connection: Option<&str>,
        cut_paths: &[PathBuf],
        config: &crate::config::FileExplorerConfig,
        // The explorer is only ever painted by the TUI path, which always
        // records theme-key provenance — so this isn't `Option` like the other
        // chrome renderers (tabs/menu/status_bar), whose legacy/offscreen
        // callers pass `None`.
        rec: &mut CellThemeRecorder,
        // When false, compute layout (viewport height for scrolling) but draw no
        // cells — the frontend renders the sidebar natively from
        // `Editor::file_explorer_view`. The TUI always passes `true`.
        draw: bool,
    ) {
        // Viewport height drives scrolling math AND the web projection's visible
        // window, so it must be set on every render regardless of `draw`.
        let viewport_height_pre = area.height.saturating_sub(2) as usize;
        view.set_viewport_height(viewport_height_pre);
        if !draw {
            return;
        }
        let search_active = view.is_search_active();
        // The tree-indicator glyphs are the only config the inner renderers
        // need; pull them out here and forward as `&str` so the helpers don't
        // depend on the whole config struct.
        let tree_indicator_collapsed = config.tree_indicator_collapsed.as_str();
        let tree_indicator_expanded = config.tree_indicator_expanded.as_str();

        // Seed the whole explorer rect with its surface keys so border/content
        // rows resolve to the explorer; the selected row is refined below.
        for row in area.y..area.y + area.height {
            rec.run(
                area.x,
                row,
                area.width,
                Some("editor.fg"),
                Some("editor.bg"),
                "File Explorer",
            );
        }

        // Viewport height already applied above (before the `draw` early-out).
        let viewport_height = viewport_height_pre;

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

        let multi_selection = view.multi_selection();

        // Create list items for visible nodes only
        let items: Vec<ListItem> = visible_items
            .iter()
            .enumerate()
            .map(|(viewport_idx, &(node_id, indent))| {
                let actual_idx = scroll_offset + viewport_idx;
                let is_selected = selected_index == Some(actual_idx);
                let is_multi_selected = multi_selection.contains(&node_id);
                let fuzzy_match = if search_active {
                    view.get_match_for_node(node_id)
                } else {
                    None
                };
                Self::render_node(
                    view,
                    deco,
                    node_id,
                    indent,
                    is_selected,
                    is_multi_selected,
                    is_focused,
                    files_with_unsaved_changes,
                    theme,
                    content_width,
                    fuzzy_match.as_ref(),
                    cut_paths,
                    tree_indicator_collapsed,
                    tree_indicator_expanded,
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
                .next_back()
                .unwrap_or(host)
                .split(':')
                .next()
                .unwrap_or(host);
            format!(" [{}]{} ", hostname, keybinding_suffix)
        } else {
            format!(" File Explorer{} ", keybinding_suffix)
        };

        // Title style: use warning colors when remote is disconnected,
        // otherwise inverted colors (dark on light) when focused.
        let remote_disconnected = remote_connection
            .map(|c| c.contains("(Disconnected)"))
            .unwrap_or(false);
        let (title_style, border_style) = if remote_disconnected {
            (
                Style::default()
                    .fg(theme.status_error_indicator_fg)
                    .bg(theme.status_error_indicator_bg)
                    .add_modifier(Modifier::BOLD),
                Style::default().fg(theme.status_error_indicator_bg),
            )
        } else if is_focused {
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

        // Refine the selected row with its highlight keys (focused → selection
        // background, blurred → current-line background).
        if let Some(selected) = selected_index {
            if selected >= scroll_offset && selected < scroll_offset + viewport_height {
                let row = area.y + 1 + (selected - scroll_offset) as u16;
                let inner_x = area.x + 1;
                let inner_w = area.width.saturating_sub(2);
                let bg_key = if is_focused {
                    "editor.selection_bg"
                } else {
                    "editor.current_line_bg"
                };
                rec.run(
                    inner_x,
                    row,
                    inner_w,
                    Some("editor.fg"),
                    Some(bg_key),
                    "File Explorer",
                );
            }
        }

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
        deco: ExplorerDecorations<'_>,
        node_id: NodeId,
        indent: usize,
        is_selected: bool,
        is_multi_selected: bool,
        is_focused: bool,
        files_with_unsaved_changes: &HashSet<PathBuf>,
        theme: &Theme,
        content_width: usize,
        fuzzy_match: Option<&FuzzyMatch>,
        cut_paths: &[PathBuf],
        tree_indicator_collapsed: &str,
        tree_indicator_expanded: &str,
    ) -> ListItem<'static> {
        let line = Self::build_node_line(
            view,
            deco,
            node_id,
            indent,
            is_selected,
            is_multi_selected,
            is_focused,
            files_with_unsaved_changes,
            theme,
            content_width,
            fuzzy_match,
            cut_paths,
            tree_indicator_collapsed,
            tree_indicator_expanded,
        );
        let row_bg = if (is_selected || is_multi_selected) && is_focused {
            theme.selection_bg
        } else {
            theme.editor_bg
        };
        ListItem::new(line).style(Style::default().bg(row_bg))
    }

    #[allow(clippy::too_many_arguments)]
    fn build_node_line(
        view: &FileTreeView,
        deco: ExplorerDecorations<'_>,
        node_id: NodeId,
        indent: usize,
        is_selected: bool,
        is_multi_selected: bool,
        is_focused: bool,
        files_with_unsaved_changes: &HashSet<PathBuf>,
        theme: &Theme,
        content_width: usize,
        fuzzy_match: Option<&FuzzyMatch>,
        cut_paths: &[PathBuf],
        tree_indicator_collapsed: &str,
        tree_indicator_expanded: &str,
    ) -> Line<'static> {
        let node = view.tree().get_node(node_id).expect("Node should exist");

        let mut spans = Vec::new();
        // Names of any ancestor directories that compact-mode folded into
        // this row. Outermost-first; each gets prefixed before the anchor
        // name and joined by `/`.
        let chain_prefix_names: Vec<String> = view
            .compact_chain_for_anchor(node_id)
            .into_iter()
            .filter_map(|id| view.tree().get_node(id).map(|n| n.entry.name.clone()))
            .collect();

        // Width reserved for the tree-indicator column. We size it from the
        // configured collapsed/expanded glyphs (plus a trailing space) so file
        // and directory names stay aligned even when the user picks wider
        // custom indicators.
        let collapsed_w = str_width(tree_indicator_collapsed);
        let expanded_w = str_width(tree_indicator_expanded);
        let indicator_width = collapsed_w.max(expanded_w).max(1) + 1;

        let has_unsaved = if node.is_dir() {
            Self::folder_has_modified_files(&node.entry.path, files_with_unsaved_changes)
        } else {
            files_with_unsaved_changes.contains(&node.entry.path)
        };

        let is_pending_cut = cut_paths.iter().any(|cp| cp == &node.entry.path);
        let neutral_fg = if node
            .entry
            .metadata
            .as_ref()
            .map(|m| m.is_hidden)
            .unwrap_or(false)
        {
            theme.line_number_fg
        } else if node.entry.is_symlink() {
            theme.syntax_type
        } else if node.is_dir() {
            theme.syntax_keyword
        } else {
            theme.editor_fg
        };
        let slot_context = ExplorerSlotContext {
            path: &node.entry.path,
            is_dir: node.is_dir(),
            has_unsaved,
            is_symlink: node.entry.is_symlink(),
            is_hidden: node
                .entry
                .metadata
                .as_ref()
                .map(|m| m.is_hidden)
                .unwrap_or(false),
            decorations: deco.decorations,
            slot_overrides: deco.slot_overrides,
            theme,
            neutral_fg,
        };
        let slot_resolution = deco.slot_resolver.resolve(&slot_context);
        let leading_slot_width = slot_resolution
            .leading
            .as_ref()
            .map(|slot| slot.width() + 1)
            .unwrap_or(0);

        let base_fg = if is_pending_cut {
            theme.line_number_fg
        } else if let Some(name_color_hint) = slot_resolution.name_color_hint {
            name_color_hint
        } else if (is_selected || is_multi_selected) && is_focused {
            theme.editor_fg
        } else {
            neutral_fg
        };

        let chain_prefix_width: usize = chain_prefix_names.iter().map(|s| str_width(s) + 1).sum();
        let name_width = str_width(&node.entry.name);

        let indent_width = indent * 2;
        let left_side_width =
            indent_width + indicator_width + leading_slot_width + chain_prefix_width + name_width;
        let trailing_slot_width = slot_resolution
            .trailing
            .as_ref()
            .map(|slot| slot.width())
            .unwrap_or(0);
        let error_text = if node.is_error() { " [Error]" } else { "" };
        let error_width = str_width(error_text);
        let total_right_width = trailing_slot_width + error_width;

        if indent > 0 {
            spans.push(Span::raw("  ".repeat(indent)));
        }

        if node.is_dir() {
            let (indicator, glyph_width) = if node.is_expanded() {
                (format!("{} ", tree_indicator_expanded), expanded_w + 1)
            } else if node.is_collapsed() {
                (format!("{} ", tree_indicator_collapsed), collapsed_w + 1)
            } else if node.is_loading() {
                ("⟳ ".to_string(), 2)
            } else {
                ("! ".to_string(), 2)
            };
            spans.push(Span::styled(
                indicator,
                Style::default().fg(theme.diagnostic_warning_fg),
            ));
            let pad = indicator_width.saturating_sub(glyph_width);
            if pad > 0 {
                spans.push(Span::raw(" ".repeat(pad)));
            }
        } else {
            spans.push(Span::raw(" ".repeat(indicator_width)));
        }

        if let Some(slot) = slot_resolution.leading {
            let slot_width = slot.width();
            let slot_text_width = str_width(&slot.text);
            spans.push(Span::styled(slot.text, Style::default().fg(slot.fg)));
            let slot_padding = slot_width.saturating_sub(slot_text_width) + 1;
            spans.push(Span::raw(" ".repeat(slot_padding)));
        }

        let chain_segment_style = Style::default().fg(theme.syntax_keyword);
        let chain_separator_style = Style::default().fg(theme.line_number_fg);
        for name in &chain_prefix_names {
            spans.push(Span::styled(name.clone(), chain_segment_style));
            spans.push(Span::styled("/", chain_separator_style));
        }

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

        let min_gap = 1;
        let padding = if left_side_width + min_gap + total_right_width < content_width {
            content_width - left_side_width - total_right_width
        } else {
            min_gap
        };
        spans.push(Span::raw(" ".repeat(padding)));

        if let Some(slot) = slot_resolution.trailing {
            spans.push(Span::styled(slot.text, Style::default().fg(slot.fg)));
        }

        if node.is_error() {
            spans.push(Span::styled(
                error_text,
                Style::default().fg(theme.diagnostic_error_fg),
            ));
        }

        Line::from(spans)
    }

    pub(crate) fn trailing_slot_screen_bounds(
        view: &FileTreeView,
        node_id: NodeId,
        indent: usize,
        content_width: usize,
        slot_resolution: &ExplorerSlotResolution,
        tree_indicator_collapsed: &str,
        tree_indicator_expanded: &str,
        explorer_area: Rect,
    ) -> Option<(u16, u16)> {
        let trailing_slot = slot_resolution.trailing.as_ref()?;
        let node = view.tree().get_node(node_id).expect("Node should exist");

        let chain_prefix_names: Vec<String> = view
            .compact_chain_for_anchor(node_id)
            .into_iter()
            .filter_map(|id| view.tree().get_node(id).map(|n| n.entry.name.clone()))
            .collect();
        let collapsed_w = str_width(tree_indicator_collapsed);
        let expanded_w = str_width(tree_indicator_expanded);
        let indicator_width = collapsed_w.max(expanded_w).max(1) + 1;
        let leading_slot_width = slot_resolution
            .leading
            .as_ref()
            .map(|slot| slot.width() + 1)
            .unwrap_or(0);
        let chain_prefix_width: usize = chain_prefix_names.iter().map(|s| str_width(s) + 1).sum();
        let name_width = str_width(&node.entry.name);
        let left_side_width =
            indent * 2 + indicator_width + leading_slot_width + chain_prefix_width + name_width;
        let trailing_slot_width = trailing_slot.width();
        let error_width = if node.is_error() {
            str_width(" [Error]")
        } else {
            0
        };
        let total_right_width = trailing_slot_width + error_width;
        let min_gap = 1;
        let padding = if left_side_width + min_gap + total_right_width < content_width {
            content_width - left_side_width - total_right_width
        } else {
            min_gap
        };
        let content_start_x = explorer_area.x + 2;
        let slot_start = content_start_x + (left_side_width + padding) as u16;
        let slot_end = slot_start + trailing_slot_width as u16;
        Some((slot_start, slot_end))
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::filesystem::StdFileSystem;
    use crate::services::fs::FsManager;
    use std::collections::{HashMap, HashSet};
    use std::fs as std_fs;
    use std::sync::Arc;
    use tempfile::TempDir;

    async fn create_renderer_view() -> (TempDir, FileTreeView) {
        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path();

        std_fs::create_dir(root.join("src")).unwrap();
        std_fs::write(root.join("README.md"), "hello").unwrap();
        std_fs::write(root.join("src/schema.ts"), "export const value = 1;\n").unwrap();

        let manager = Arc::new(FsManager::new(Arc::new(StdFileSystem)));
        let mut tree = crate::view::file_tree::FileTree::new(root.to_path_buf(), manager)
            .await
            .unwrap();
        let root_id = tree.root_id();
        tree.expand_node(root_id).await.unwrap();
        let src_id = tree
            .get_node(root_id)
            .unwrap()
            .children
            .iter()
            .copied()
            .find(|id| tree.get_node(*id).unwrap().entry.name == "src")
            .unwrap();
        tree.expand_node(src_id).await.unwrap();

        (temp_dir, FileTreeView::new(tree))
    }

    fn build_line(
        view: &FileTreeView,
        node_id: NodeId,
        indent: usize,
        decorations: &FileExplorerDecorationCache,
        slot_overrides: &FileExplorerSlotOverrideCache,
        theme: &Theme,
    ) -> Line<'static> {
        let deco = ExplorerDecorations {
            slot_resolver: crate::view::file_tree::default_slot_providers().resolver(),
            decorations,
            slot_overrides,
        };
        FileExplorerRenderer::build_node_line(
            view,
            deco,
            node_id,
            indent,
            false,
            false,
            false,
            &HashSet::new(),
            theme,
            80,
            None,
            &[],
            ">",
            "▼",
        )
    }

    #[tokio::test]
    async fn renderer_line_shows_plugin_decoration_badge() {
        let (_temp_dir, view) = create_renderer_view().await;
        let theme = Theme::load_builtin("dark").unwrap();
        let schema_path = view.tree().root_path().join("src/schema.ts");
        let schema_id = view.tree().get_node_by_path(&schema_path).unwrap().id;
        let decorations = FileExplorerDecorationCache::rebuild(
            vec![crate::view::file_tree::FileExplorerDecoration {
                path: schema_path,
                symbol: "M".to_string(),
                color: fresh_core::api::OverlayColorSpec::ThemeKey(
                    "ui.file_status_modified_fg".into(),
                ),
                priority: 50,
            }],
            view.tree().root_path(),
            &HashMap::new(),
        );

        let line = build_line(
            &view,
            schema_id,
            2,
            &decorations,
            &FileExplorerSlotOverrideCache::default(),
            &theme,
        );

        assert!(line.spans.iter().any(|span| {
            span.content.as_ref() == "M" && span.style.fg == Some(theme.file_status_modified_fg)
        }));
    }

    #[tokio::test]
    async fn directories_render_bubbled_plugin_status() {
        let (_temp_dir, view) = create_renderer_view().await;
        let theme = Theme::load_builtin("dark").unwrap();
        let src_path = view.tree().root_path().join("src");
        let schema_path = src_path.join("schema.ts");
        let src_id = view.tree().get_node_by_path(&src_path).unwrap().id;
        let decorations = FileExplorerDecorationCache::rebuild(
            vec![crate::view::file_tree::FileExplorerDecoration {
                path: schema_path,
                symbol: "R".to_string(),
                color: fresh_core::api::OverlayColorSpec::ThemeKey(
                    "ui.file_status_renamed_fg".into(),
                ),
                priority: 40,
            }],
            view.tree().root_path(),
            &HashMap::new(),
        );

        let line = build_line(
            &view,
            src_id,
            1,
            &decorations,
            &FileExplorerSlotOverrideCache::default(),
            &theme,
        );

        assert!(line.spans.iter().any(|span| {
            span.content.as_ref() == "●" && span.style.fg == Some(theme.file_status_renamed_fg)
        }));
    }

    #[tokio::test]
    async fn default_slot_providers_allow_explicit_slot_and_name_color_overrides() {
        let (_temp_dir, view) = create_renderer_view().await;
        let theme = Theme::load_builtin("dark").unwrap();
        let schema_path = view.tree().root_path().join("src/schema.ts");
        let schema_id = view.tree().get_node_by_path(&schema_path).unwrap().id;
        let slot_overrides = FileExplorerSlotOverrideCache::rebuild(
            vec![fresh_core::file_explorer::FileExplorerSlotEntry {
                path: schema_path.clone(),
                leading: Some(fresh_core::file_explorer::FileExplorerLeadingSlot {
                    text: "PL".to_string(),
                    color: fresh_core::api::OverlayColorSpec::ThemeKey("syntax.string".into()),
                    min_width: 2,
                }),
                trailing: Some(fresh_core::file_explorer::FileExplorerTrailingSlot {
                    text: "X".to_string(),
                    color: fresh_core::api::OverlayColorSpec::ThemeKey("syntax.type".into()),
                    tooltip: Some(fresh_core::file_explorer::FileExplorerTooltip {
                        title: "Plugin".to_string(),
                        lines: vec!["Overridden".to_string()],
                    }),
                }),
                name_color: Some(fresh_core::api::OverlayColorSpec::ThemeKey(
                    "ui.file_status_added_fg".into(),
                )),
                priority: 50,
                suppress_leading: false,
                suppress_trailing: false,
                suppress_name_color: false,
            }],
            view.tree().root_path(),
            &HashMap::new(),
        );

        let line = build_line(
            &view,
            schema_id,
            2,
            &FileExplorerDecorationCache::default(),
            &slot_overrides,
            &theme,
        );

        assert!(line.spans.iter().any(|span| span.content.as_ref() == "PL"));
        assert!(line.spans.iter().any(|span| span.content.as_ref() == "X"));
        assert!(line.spans.iter().any(|span| {
            span.content.as_ref() == "schema.ts"
                && span.style.fg == Some(theme.file_status_added_fg)
        }));
    }

    #[tokio::test]
    async fn default_slot_providers_fall_back_when_only_name_color_is_overridden() {
        let (_temp_dir, view) = create_renderer_view().await;
        let theme = Theme::load_builtin("dark").unwrap();
        let schema_path = view.tree().root_path().join("src/schema.ts");
        let schema_id = view.tree().get_node_by_path(&schema_path).unwrap().id;
        let decorations = FileExplorerDecorationCache::rebuild(
            vec![crate::view::file_tree::FileExplorerDecoration {
                path: schema_path.clone(),
                symbol: "M".to_string(),
                color: fresh_core::api::OverlayColorSpec::ThemeKey(
                    "ui.file_status_modified_fg".into(),
                ),
                priority: 50,
            }],
            view.tree().root_path(),
            &HashMap::new(),
        );
        let slot_overrides = FileExplorerSlotOverrideCache::rebuild(
            vec![fresh_core::file_explorer::FileExplorerSlotEntry {
                path: schema_path,
                leading: None,
                trailing: None,
                name_color: Some(fresh_core::api::OverlayColorSpec::ThemeKey(
                    "syntax.string".into(),
                )),
                priority: 50,
                suppress_leading: false,
                suppress_trailing: false,
                suppress_name_color: false,
            }],
            view.tree().root_path(),
            &HashMap::new(),
        );

        let line = build_line(&view, schema_id, 2, &decorations, &slot_overrides, &theme);

        assert!(line.spans.iter().any(|span| {
            span.content.as_ref() == "schema.ts" && span.style.fg == Some(theme.syntax_string)
        }));
        assert!(line.spans.iter().any(|span| {
            span.content.as_ref() == "M" && span.style.fg == Some(theme.file_status_modified_fg)
        }));
    }

    #[tokio::test]
    async fn trailing_slot_bounds_track_rendered_right_edge_geometry() {
        let (_temp_dir, view) = create_renderer_view().await;
        let theme = Theme::load_builtin("dark").unwrap();
        let schema_path = view.tree().root_path().join("src/schema.ts");
        let schema_id = view.tree().get_node_by_path(&schema_path).unwrap().id;
        let decorations = FileExplorerDecorationCache::rebuild(
            vec![crate::view::file_tree::FileExplorerDecoration {
                path: schema_path.clone(),
                symbol: "M".to_string(),
                color: fresh_core::api::OverlayColorSpec::ThemeKey(
                    "ui.file_status_modified_fg".into(),
                ),
                priority: 50,
            }],
            view.tree().root_path(),
            &HashMap::new(),
        );
        let slot_context = ExplorerSlotContext {
            path: &schema_path,
            is_dir: false,
            has_unsaved: false,
            is_symlink: false,
            is_hidden: false,
            decorations: &decorations,
            slot_overrides: &FileExplorerSlotOverrideCache::default(),
            theme: &theme,
            neutral_fg: theme.editor_fg,
        };
        let slot_resolution = crate::view::file_tree::default_slot_providers()
            .resolver()
            .resolve(&slot_context);
        let area = Rect::new(0, 0, 40, 10);
        let content_width = area.width.saturating_sub(3) as usize;

        let bounds = FileExplorerRenderer::trailing_slot_screen_bounds(
            &view,
            schema_id,
            2,
            content_width,
            &slot_resolution,
            ">",
            "▼",
            area,
        )
        .expect("modified file should render a trailing slot");

        assert_eq!(bounds.1, area.x + area.width.saturating_sub(1));
        assert_eq!(bounds.1 - bounds.0, 1);
    }
}
