//! The file explorer sidebar and its off-panel menu-clear guard.

use crate::app::types::HoverTarget;
use crate::widgets::LayoutBox;
use anyhow::Result as AnyhowResult;

use super::{
    in_rect, ChromeComponent, ChromePointer, ChromeTreeBuilder, Disposition, Editor, PointerPress,
};

pub(crate) struct FileExplorer;

impl ChromeComponent for FileExplorer {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        if let Some(r) = ed.active_layout().file_explorer_area {
            t.rect("chrome:file_explorer", 100, r);
        }
        // Off-explorer right-click clears its menu (declining guard).
        t.full("chrome:clear_explorer_menu", 90);
    }

    fn hover(&self, ed: &mut Editor, bx: &LayoutBox, col: u16, row: u16) -> Option<HoverTarget> {
        if bx.kind != "chrome:file_explorer" {
            return None;
        }
        ed.hover_target_in_file_explorer(col, row)
    }

    fn on_hover_change(
        &self,
        ed: &mut Editor,
        old: Option<&HoverTarget>,
        new: Option<&HoverTarget>,
        col: u16,
        row: u16,
    ) -> bool {
        if old == new {
            return false;
        }
        // Leaving a status indicator dismisses its tooltip; entering
        // one shows it. Independent of any other surface's reaction —
        // the old central ladder could skip the dismiss when a menu
        // reaction returned early.
        if matches!(old, Some(HoverTarget::FileExplorerStatusIndicator(_))) {
            ed.dismiss_file_explorer_status_tooltip();
        }
        if let Some(HoverTarget::FileExplorerStatusIndicator(path)) = new {
            ed.show_file_explorer_status_tooltip(path.clone(), col, row);
            return true;
        }
        false
    }

    fn on_wheel(
        &self,
        ed: &mut Editor,
        bx: &LayoutBox,
        col: u16,
        row: u16,
        delta: i32,
    ) -> AnyhowResult<Disposition> {
        if bx.kind != "chrome:file_explorer" {
            return Ok(Disposition::Pass);
        }
        // The explorer scrolls its own viewport (moved from the old
        // central `wheel_surface_at` fork — the surface's wheel lives
        // with the surface).
        ed.dismiss_transient_popups();
        ed.active_window().wheel_plugin_hook(col, row, delta);
        ed.active_window_mut().scroll_file_explorer_view(delta);
        Ok(Disposition::Consumed)
    }

    fn on_pointer(
        &self,
        ed: &mut Editor,
        bx: &LayoutBox,
        ev: &ChromePointer,
    ) -> AnyhowResult<Disposition> {
        match (ev.press, bx.kind) {
            (PointerPress::Left, "chrome:file_explorer") => {
                if let Some(r) = ed.handle_click_file_explorer_area(ev.col, ev.row) {
                    r?;
                    return Ok(Disposition::Consumed);
                }
                Ok(Disposition::Pass)
            }
            (PointerPress::Right, "chrome:file_explorer") => {
                let Some(explorer_area) = ed.active_layout().file_explorer_area else {
                    return Ok(Disposition::Pass);
                };
                // The union box spans the whole explorer; the title row
                // is not a right-click target.
                if ev.row <= explorer_area.y {
                    return Ok(Disposition::Pass);
                }
                let relative_row = ev.row.saturating_sub(explorer_area.y + 1);
                let (is_multi, is_root_selected) =
                    if let Some(explorer) = ed.file_explorer_mut().as_mut() {
                        let mut clicked_is_root = false;
                        if let Some((node_id, _)) =
                            explorer.get_display_node_at_viewport_row(relative_row as usize)
                        {
                            explorer.set_selected(Some(node_id));
                            clicked_is_root = node_id == explorer.tree().root_id();
                        }
                        (explorer.has_multi_selection(), clicked_is_root)
                    } else {
                        (false, false)
                    };
                ed.active_window_mut().key_context =
                    crate::input::keybindings::KeyContext::FileExplorer;
                ed.active_window_mut().tab_context_menu = None;
                ed.active_window_mut().file_explorer_context_menu =
                    Some(crate::app::types::FileExplorerContextMenu::new(
                        ev.col,
                        ev.row + 1,
                        is_multi,
                        is_root_selected,
                    ));
                Ok(Disposition::Consumed)
            }
            (PointerPress::Right, "chrome:clear_explorer_menu") => {
                // Off-explorer right-click dismisses its menu, then
                // routing continues (act-then-continue guard).
                ed.active_window_mut().file_explorer_context_menu = None;
                Ok(Disposition::PassAfter)
            }
            (PointerPress::Double, "chrome:file_explorer") => {
                // Title row is not a double-click target (the union box
                // spans the whole explorer).
                if let Some(r) = ed.active_layout().file_explorer_area {
                    if ev.row <= r.y {
                        return Ok(Disposition::Pass);
                    }
                }
                // Open file AND focus editor.
                ed.file_explorer_open_file()?;
                Ok(Disposition::Consumed)
            }
            _ => Ok(Disposition::Pass),
        }
    }
}

/// Behavior owned by this component (moved from mouse_input.rs —
/// the handlers its arms dispatch to).
impl Editor {
    /// The `hover:file_explorer` box: the close button on the title
    /// row, per-item trailing status indicators, and the resize border
    /// on the rightmost column.
    pub(super) fn hover_target_in_file_explorer(&self, col: u16, row: u16) -> Option<HoverTarget> {
        // Check file explorer close button and border (for resize)
        if let Some(explorer_area) = self.active_layout().file_explorer_area {
            // Close button is at position: explorer_area.x + explorer_area.width - 3 to -1
            let close_button_x = explorer_area.x + explorer_area.width.saturating_sub(3);
            if row == explorer_area.y
                && col >= close_button_x
                && col < explorer_area.x + explorer_area.width
            {
                return Some(HoverTarget::FileExplorerCloseButton);
            }

            // Check if hovering over a status indicator in the file explorer content area
            let content_start_y = explorer_area.y + 1; // +1 for title bar
            let content_end_y = explorer_area.y + explorer_area.height.saturating_sub(1); // -1 for bottom border
            let content_width = explorer_area.width.saturating_sub(3) as usize;

            if row >= content_start_y && row < content_end_y {
                // Determine which item is at this row
                if let Some(explorer) = self.file_explorer().as_ref() {
                    let relative_row = row.saturating_sub(content_start_y) as usize;
                    if let Some((node_id, indent)) =
                        explorer.get_display_node_at_viewport_row(relative_row)
                    {
                        if let Some(node) = explorer.tree().get_node(node_id) {
                            let theme = self.theme.read().unwrap();
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
                            let slot_resolver = self.file_explorer_slot_resolver();
                            let slot_context = crate::view::file_tree::ExplorerSlotContext {
                                path: &node.entry.path,
                                is_dir: node.is_dir(),
                                has_unsaved: self.file_explorer_node_has_unsaved_changes(
                                    &node.entry.path,
                                    node.is_dir(),
                                ),
                                is_symlink: node.entry.is_symlink(),
                                is_hidden: node
                                    .entry
                                    .metadata
                                    .as_ref()
                                    .map(|m| m.is_hidden)
                                    .unwrap_or(false),
                                decorations: &self.active_window().file_explorer_decoration_cache,
                                slot_overrides: &self
                                    .active_window()
                                    .file_explorer_slot_override_cache,
                                theme: &theme,
                                neutral_fg,
                            };
                            let slot_resolution = slot_resolver.resolve(&slot_context);
                            if let Some((slot_start, slot_end)) = crate::view::ui::file_explorer::FileExplorerRenderer::trailing_slot_screen_bounds(
                                crate::view::ui::file_explorer::TrailingSlotBoundsCtx {
                                    view: explorer,
                                    node_id,
                                    indent,
                                    content_width,
                                    slot_resolution: &slot_resolution,
                                    tree_indicator_collapsed: &self.config.file_explorer.tree_indicator_collapsed,
                                    tree_indicator_expanded: &self.config.file_explorer.tree_indicator_expanded,
                                    explorer_area,
                                },
                            ) {
                                if col >= slot_start && col < slot_end {
                                    return Some(HoverTarget::FileExplorerStatusIndicator(
                                        node.entry.path.clone(),
                                    ));
                                }
                            }
                        }
                    }
                }
            }

            // The border is at the rightmost column of the file explorer area
            // (the drawn border character), not one past it.
            let border_x = explorer_area.x + explorer_area.width.saturating_sub(1);
            if col == border_x
                && row >= explorer_area.y
                && row < explorer_area.y + explorer_area.height
            {
                return Some(HoverTarget::FileExplorerBorder);
            }
        }

        None
    }

    pub(super) fn handle_click_file_explorer_area(
        &mut self,
        col: u16,
        row: u16,
    ) -> Option<AnyhowResult<()>> {
        let explorer_area = self.active_layout().file_explorer_area?;
        let border_x = explorer_area.x + explorer_area.width.saturating_sub(1);
        if col == border_x && row >= explorer_area.y && row < explorer_area.y + explorer_area.height
        {
            self.active_window_mut().mouse_state.dragging_file_explorer = true;
            self.active_window_mut().mouse_state.drag_start_position = Some((col, row));
            self.active_window_mut()
                .mouse_state
                .drag_start_explorer_width = Some(self.active_window().file_explorer_width);
            return Some(Ok(()));
        }
        if in_rect(col, row, explorer_area) {
            return Some(self.handle_file_explorer_click(col, row, explorer_area));
        }
        None
    }

    /// Show a tooltip for a file explorer status indicator
    pub(super) fn show_file_explorer_status_tooltip(
        &mut self,
        path: std::path::PathBuf,
        col: u16,
        row: u16,
    ) {
        use crate::view::popup::{Popup, PopupPosition};
        use ratatui::style::Style;

        let is_directory = path.is_dir();
        let has_unsaved_changes = self.file_explorer_node_has_unsaved_changes(&path, is_directory);

        let node_metadata = self
            .file_explorer()
            .and_then(|explorer| explorer.tree().get_node_by_path(&path))
            .and_then(|node| node.entry.metadata.as_ref());
        let is_hidden = node_metadata.map(|m| m.is_hidden).unwrap_or(false);
        let is_symlink = path.is_symlink();
        let theme = self.theme.read().unwrap();
        let neutral_fg = if is_hidden {
            theme.line_number_fg
        } else if is_symlink {
            theme.syntax_type
        } else if is_directory {
            theme.syntax_keyword
        } else {
            theme.editor_fg
        };
        let slot_resolver = self.file_explorer_slot_resolver();
        let slot_context = crate::view::file_tree::ExplorerSlotContext {
            path: &path,
            is_dir: is_directory,
            has_unsaved: has_unsaved_changes,
            is_symlink,
            is_hidden,
            decorations: &self.active_window().file_explorer_decoration_cache,
            slot_overrides: &self.active_window().file_explorer_slot_override_cache,
            theme: &theme,
            neutral_fg,
        };
        let slot_resolution = slot_resolver.resolve(&slot_context);

        // Build tooltip content
        let Some(summary) = slot_resolution.trailing.and_then(|slot| slot.tooltip) else {
            return; // No status to show
        };
        let mut lines = summary.lines;
        let has_custom_trailing_override = self
            .active_window()
            .file_explorer_slot_override_cache
            .has_trailing_override_for_path(&path);

        if !has_custom_trailing_override {
            // Compatibility tooltips enrich native git/status content with
            // directory child summaries and file diff stats. Explicit slot
            // overrides own their hover content end-to-end.
            if is_directory {
                if let Some(modified_files) = self.get_modified_files_in_directory(&path) {
                    lines.push(String::new()); // Empty line separator
                    lines.push("Modified files:".to_string());
                    const MAX_FILES: usize = 8;
                    for (i, file) in modified_files.iter().take(MAX_FILES).enumerate() {
                        // Show relative path from the directory
                        let display_name = file
                            .strip_prefix(&path)
                            .unwrap_or(file)
                            .to_string_lossy()
                            .to_string();
                        lines.push(format!("  {}", display_name));
                        if i == MAX_FILES - 1 && modified_files.len() > MAX_FILES {
                            lines.push(format!(
                                "  ... and {} more",
                                modified_files.len() - MAX_FILES
                            ));
                            break;
                        }
                    }
                }
            } else if let Some(stats) = self.get_git_diff_stats(&path) {
                // For files, try to get git diff stats
                lines.push(String::new()); // Empty line separator
                lines.push(stats);
            }
        }

        if lines.is_empty() {
            return;
        }

        // Create popup
        let mut popup = Popup::text(lines, &self.theme.read().unwrap());
        popup.title = Some(summary.title);
        popup.transient = true;
        popup.position = PopupPosition::Fixed { x: col, y: row + 1 };
        popup.width = 50;
        popup.max_height = 15;
        popup.border_style = Style::default().fg(self.theme.read().unwrap().popup_border_fg);
        popup.background_style = Style::default().bg(self.theme.read().unwrap().popup_bg);

        // Show the popup
        let __buffer_id = self.active_buffer();
        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&__buffer_id)
        {
            state.popups.show(popup);
        }
    }

    fn file_explorer_node_has_unsaved_changes(
        &self,
        path: &std::path::Path,
        is_directory: bool,
    ) -> bool {
        if is_directory {
            self.windows
                .get(&self.active_window)
                .map(|w| &w.buffers)
                .expect("active window present")
                .iter()
                .any(|(buffer_id, state)| {
                    if state.buffer.is_modified() {
                        if let Some(metadata) = self.active_window().buffer_metadata.get(buffer_id)
                        {
                            if let Some(file_path) = metadata.file_path() {
                                return file_path.starts_with(path);
                            }
                        }
                    }
                    false
                })
        } else {
            self.windows
                .get(&self.active_window)
                .map(|w| &w.buffers)
                .expect("active window present")
                .iter()
                .any(|(buffer_id, state)| {
                    if state.buffer.is_modified() {
                        if let Some(metadata) = self.active_window().buffer_metadata.get(buffer_id)
                        {
                            return metadata.file_path().map(|p| p.as_path()) == Some(path);
                        }
                    }
                    false
                })
        }
    }

    /// Dismiss the file explorer status tooltip
    pub(super) fn dismiss_file_explorer_status_tooltip(&mut self) {
        // Dismiss any transient popups
        let __buffer_id = self.active_buffer();
        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&__buffer_id)
        {
            state.popups.dismiss_transient();
        }
    }

    /// Get git diff stats for a file (insertions/deletions)
    fn get_git_diff_stats(&self, path: &std::path::Path) -> Option<String> {
        use crate::services::process_hidden::HideWindow;
        use std::process::Command;

        // Run git diff --numstat for the file
        let output = Command::new("git")
            .args(["diff", "--numstat", "--"])
            .arg(path)
            .current_dir(self.working_dir())
            .hide_window()
            .output()
            .ok()?;

        if !output.status.success() {
            return None;
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        let line = stdout.lines().next()?;
        let parts: Vec<&str> = line.split('\t').collect();

        if parts.len() >= 2 {
            let insertions = parts[0];
            let deletions = parts[1];

            // Handle binary files (shows as -)
            if insertions == "-" && deletions == "-" {
                return Some("Binary file changed".to_string());
            }

            let ins: i32 = insertions.parse().unwrap_or(0);
            let del: i32 = deletions.parse().unwrap_or(0);

            if ins > 0 || del > 0 {
                return Some(format!("+{} -{} lines", ins, del));
            }
        }

        // Also check staged changes
        let staged_output = Command::new("git")
            .args(["diff", "--numstat", "--cached", "--"])
            .arg(path)
            .current_dir(self.working_dir())
            .hide_window()
            .output()
            .ok()?;

        if staged_output.status.success() {
            let staged_stdout = String::from_utf8_lossy(&staged_output.stdout);
            if let Some(line) = staged_stdout.lines().next() {
                let parts: Vec<&str> = line.split('\t').collect();
                if parts.len() >= 2 {
                    let insertions = parts[0];
                    let deletions = parts[1];

                    if insertions == "-" && deletions == "-" {
                        return Some("Binary file staged".to_string());
                    }

                    let ins: i32 = insertions.parse().unwrap_or(0);
                    let del: i32 = deletions.parse().unwrap_or(0);

                    if ins > 0 || del > 0 {
                        return Some(format!("+{} -{} lines (staged)", ins, del));
                    }
                }
            }
        }

        None
    }

    /// Get list of modified files in a directory
    fn get_modified_files_in_directory(
        &self,
        dir_path: &std::path::Path,
    ) -> Option<Vec<std::path::PathBuf>> {
        let modified_files = self
            .active_window()
            .file_explorer_decoration_cache
            .direct_paths_under(dir_path);

        (!modified_files.is_empty()).then_some(modified_files)
    }
    pub(super) fn execute_file_explorer_context_menu_action(
        &mut self,
        item: crate::app::types::FileExplorerContextMenuItem,
    ) {
        use crate::app::types::FileExplorerContextMenuItem;
        match item {
            FileExplorerContextMenuItem::NewFile => self.file_explorer_new_file(),
            FileExplorerContextMenuItem::NewDirectory => self.file_explorer_new_directory(),
            FileExplorerContextMenuItem::Rename => self.file_explorer_rename(),
            FileExplorerContextMenuItem::Cut => self.active_window_mut().file_explorer_cut(),
            FileExplorerContextMenuItem::Copy => self.active_window_mut().file_explorer_copy(),
            FileExplorerContextMenuItem::Paste => self.file_explorer_paste(),
            FileExplorerContextMenuItem::Duplicate => self.file_explorer_duplicate(),
            FileExplorerContextMenuItem::Delete => self.file_explorer_delete(),
            FileExplorerContextMenuItem::CopyFullPath => self.file_explorer_copy_path(false),
            FileExplorerContextMenuItem::CopyRelativePath => self.file_explorer_copy_path(true),
        }
    }

    /// Handle click in file explorer
    pub(super) fn handle_file_explorer_click(
        &mut self,
        col: u16,
        row: u16,
        explorer_area: ratatui::layout::Rect,
    ) -> AnyhowResult<()> {
        // Check if click is on the title bar (first row)
        if row == explorer_area.y {
            // Check if click is on close button (× at right side of title bar)
            // Close button is at position: explorer_area.x + explorer_area.width - 3 to -1
            let close_button_x = explorer_area.x + explorer_area.width.saturating_sub(3);
            if col >= close_button_x && col < explorer_area.x + explorer_area.width {
                self.toggle_file_explorer();
                return Ok(());
            }
        }

        // Focus file explorer. `open_file_preview` below routes through
        // `set_active_buffer`, which detects "leaving a terminal buffer
        // while terminal_mode is on" and resets `key_context = Normal`
        // (active_focus.rs:103-107) — clobbering our FileExplorer write
        // and stealing focus to the previewed editor buffer (issue
        // #2029, sub-issue 1b). Use `take_focus_for_file_explorer` so
        // terminal_mode is cleared *before* the preview opens; then
        // re-assert `key_context = FileExplorer` after the preview in
        // case `set_active_buffer` reset it via one of its other
        // branches (e.g. switching to a regular file buffer).
        self.take_focus_for_file_explorer();

        // Calculate which item was clicked (accounting for border and title)
        // The file explorer has a 1-line border at top and bottom
        let relative_row = row.saturating_sub(explorer_area.y + 1); // +1 for top border

        if let Some(explorer) = self.file_explorer_mut().as_mut() {
            if let Some((node_id, _indent)) =
                explorer.get_display_node_at_viewport_row(relative_row as usize)
            {
                // Select this node
                explorer.set_selected(Some(node_id));

                // Check if it's a file or directory
                let node = explorer.tree().get_node(node_id);
                if let Some(node) = node {
                    if node.is_dir() {
                        // Toggle expand/collapse using the existing method
                        self.file_explorer_toggle_expand();
                    } else if node.is_file() {
                        // Open the file but keep focus on file explorer (single click).
                        // Double-click or Enter will focus the editor and promote to
                        // a permanent tab. Single-click opens in "preview" mode so a
                        // string of exploratory clicks doesn't accumulate tabs.
                        let path = node.entry.path.clone();
                        let name = node.entry.name.clone();
                        match self.open_file_preview(&path) {
                            Ok(_) => {
                                self.set_status_message(
                                    rust_i18n::t!("explorer.opened_file", name = &name).to_string(),
                                );
                            }
                            Err(e) => {
                                // Check if this is a large file encoding confirmation error
                                if let Some(confirmation) = e.downcast_ref::<
                                    crate::model::buffer::LargeFileEncodingConfirmation,
                                >() {
                                    self.start_large_file_encoding_confirmation(confirmation);
                                } else {
                                    self.set_status_message(
                                        rust_i18n::t!("file.error_opening", error = e.to_string())
                                            .to_string(),
                                    );
                                }
                            }
                        }
                        // `set_active_buffer` may have flipped key_context
                        // back to Normal during the preview open; restore it.
                        self.active_window_mut().key_context =
                            crate::input::keybindings::KeyContext::FileExplorer;
                    }
                }
            }
        }

        Ok(())
    }

    /// Handle file explorer border drag for resizing
    pub(crate) fn handle_file_explorer_border_drag(&mut self, col: u16) -> AnyhowResult<()> {
        let Some((start_col, _start_row)) =
            self.active_window_mut().mouse_state.drag_start_position
        else {
            return Ok(());
        };
        let Some(start_width) = self
            .active_window_mut()
            .mouse_state
            .drag_start_explorer_width
        else {
            return Ok(());
        };

        let delta = col as i32 - start_col as i32;
        let total_width = self.terminal_width as i32;

        // Drag preserves the variant the user chose. A user editing
        // columns doesn't want their mode silently flipped to percent
        // just because they grabbed the divider.
        if total_width > 0 {
            use crate::config::ExplorerWidth;
            self.active_window_mut().file_explorer_width = match start_width {
                ExplorerWidth::Percent(start_pct) => {
                    let percent_delta = (delta * 100) / total_width;
                    let new_pct = (start_pct as i32 + percent_delta).clamp(0, 100) as u8;
                    ExplorerWidth::Percent(new_pct)
                }
                ExplorerWidth::Columns(start_cols) => {
                    let new_cols = (start_cols as i32 + delta).clamp(0, total_width) as u16;
                    ExplorerWidth::Columns(new_cols)
                }
            };
            // The sidebar width changed: reflow terminals/viewports/panels
            // through the single layout funnel.
            self.relayout();
        }

        Ok(())
    }
}
