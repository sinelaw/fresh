//! The file explorer sidebar: its hover reactions, and the handlers the
//! tree's messages land in.

use crate::app::types::HoverTarget;
use anyhow::Result as AnyhowResult;

use super::{ChromeComponent, Editor};

pub(crate) struct FileExplorer;

impl ChromeComponent for FileExplorer {
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
}

/// Behavior owned by this component (moved from mouse_input.rs —
/// the handlers its arms dispatch to).
impl Editor {
    /// A left press on a tree row, by viewport index.
    ///
    /// This is `handle_file_explorer_click` minus its geometry: the row is
    /// named rather than derived from `row - (area.y + 1)`, and the title-bar
    /// and close-button branches are gone because the title line is its own
    /// node in the tree.
    ///
    /// It also absorbs the old `Double` arm. `clicks` is which press of a run
    /// this is, carried on the event from the editor's own multi-click
    /// detector — so one fact covers both routes, and they cannot disagree
    /// about which row they mean.
    pub(crate) fn explorer_row_pressed(&mut self, index: usize, clicks: u8) {
        // Focus first. `open_file_preview` below routes through
        // `set_active_buffer`, which detects "leaving a terminal buffer while
        // terminal_mode is on" and resets `key_context = Normal`
        // (active_focus.rs) — clobbering our FileExplorer write and stealing
        // focus to the previewed buffer (issue #2029). Taking focus here
        // clears terminal_mode *before* the preview opens; the write is
        // re-asserted afterwards in case one of `set_active_buffer`'s other
        // branches reset it.
        self.take_focus_for_file_explorer();
        let double = clicks >= 2;
        // Everything the branches below need, read out under one borrow of
        // the tree so the editor is free again by the time a file is opened.
        let picked = self.file_explorer_mut().and_then(|explorer| {
            let (node_id, _indent) = explorer.get_display_node_at_viewport_row(index)?;
            explorer.set_selected(Some(node_id));
            let node = explorer.tree().get_node(node_id)?;
            Some((
                node.is_dir(),
                node.is_file(),
                node.entry.path.clone(),
                node.entry.name.clone(),
            ))
        });
        let Some((is_dir, is_file, path, name)) = picked else {
            return;
        };
        if double {
            // Open AND focus the editor — the old double-click arm.
            if let Err(e) = self.file_explorer_open_file() {
                tracing::warn!("file explorer open failed: {e}");
            }
            return;
        }
        if is_dir {
            self.file_explorer_toggle_expand();
        } else if is_file {
            // Single click opens in *preview* mode and keeps focus on the
            // panel, so a string of exploratory clicks doesn't accumulate
            // tabs; the double above promotes it to a permanent one.
            match self.open_file_preview(&path) {
                Ok(_) => {
                    self.set_status_message(
                        fresh_i18n::t!("explorer.opened_file", name = &name).to_string(),
                    );
                }
                Err(e) => {
                    if let Some(confirmation) =
                        e.downcast_ref::<crate::model::buffer::LargeFileEncodingConfirmation>()
                    {
                        self.start_large_file_encoding_confirmation(confirmation);
                    } else {
                        self.set_status_message(
                            fresh_i18n::t!("file.error_opening", error = e.to_string()).to_string(),
                        );
                    }
                }
            }
            self.active_window_mut().key_context =
                crate::input::keybindings::KeyContext::FileExplorer;
        }
    }

    /// A right press on a tree row: select it, then open its context menu just
    /// below the pointer.
    pub(crate) fn explorer_row_context(&mut self, index: usize, x: u16, y: u16) {
        let (is_multi, is_root_selected) = if let Some(explorer) = self.file_explorer_mut().as_mut()
        {
            let mut clicked_is_root = false;
            if let Some((node_id, _)) = explorer.get_display_node_at_viewport_row(index) {
                explorer.set_selected(Some(node_id));
                clicked_is_root = node_id == explorer.tree().root_id();
            }
            (explorer.has_multi_selection(), clicked_is_root)
        } else {
            (false, false)
        };
        self.active_window_mut().key_context = crate::input::keybindings::KeyContext::FileExplorer;
        self.active_window_mut().tab_context_menu = None;
        self.active_window_mut().file_explorer_context_menu = Some(
            crate::app::types::FileExplorerContextMenu::new(x, y + 1, is_multi, is_root_selected),
        );
    }

    /// A right-press on the panel that no row claimed.
    ///
    /// Resolves the viewport row from the panel's own rectangle and hands off
    /// to [`Self::explorer_row_context`], which already tolerates an index
    /// past the last entry — `get_display_node_at_viewport_row` returns
    /// `None`, no selection moves, and the menu opens in its root form. That
    /// is the component's behaviour: `relative_row = ev.row - (area.y + 1)`,
    /// with the title row declining rather than opening anything.
    pub(crate) fn explorer_body_context(&mut self, x: u16, y: u16) {
        let area = self.shell_region_now(crate::view::shell::frame::HostRegion::Explorer);
        // The title row is not a right-click target.
        if area.height == 0 || y <= area.y {
            return;
        }
        let index = y.saturating_sub(area.y + 1) as usize;
        self.explorer_row_context(index, x, y);
    }

    fn explorer_scrollbar_geometry(
        &self,
    ) -> Option<(
        ratatui::layout::Rect,
        crate::view::ui::scrollbar::ScrollbarState,
    )> {
        let track = self.panel_rect(&crate::view::shell::file_explorer::scrollbar_key())?;
        let viewport = track.height as usize;
        let view = self.file_explorer()?;
        let max_scroll = view.max_scroll_offset();
        if max_scroll == 0 || viewport < 2 {
            return None;
        }
        let state = crate::view::ui::scrollbar::ScrollbarState::new(
            max_scroll.saturating_add(viewport),
            viewport,
            view.get_scroll_offset().min(max_scroll),
        );
        Some((track, state))
    }

    pub(crate) fn explorer_scrollbar_pressed(&mut self, row: u16) {
        let Some((track, state)) = self.explorer_scrollbar_geometry() else {
            return;
        };
        let offset = self
            .active_window_mut()
            .mouse_state
            .file_explorer_scrollbar_mouse
            .press(state, track, track.x, row);
        if let (Some(offset), Some(view)) = (offset, self.file_explorer_mut()) {
            view.set_scroll_offset(offset);
        }
    }

    pub(crate) fn explorer_scrollbar_dragged(&mut self, row: u16) {
        let Some((track, state)) = self.explorer_scrollbar_geometry() else {
            return;
        };
        let offset = self
            .active_window_mut()
            .mouse_state
            .file_explorer_scrollbar_mouse
            .drag(state, track, row);
        if let (Some(offset), Some(view)) = (offset, self.file_explorer_mut()) {
            view.set_scroll_offset(offset);
        }
    }

    pub(crate) fn explorer_scrollbar_released(&mut self) {
        self.active_window_mut()
            .mouse_state
            .file_explorer_scrollbar_mouse
            .release();
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
            let new_width = match start_width {
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
            // **Only a width that actually moved reflows**, the way the dock's
            // grip has always guarded its own drag. A grip's `Move` fires for
            // every motion report the pointer produces while it holds the
            // capture — including the ones that only travel *along* the
            // divider, and the ones a percent width rounds back to the column
            // it was already at. `relayout` is a full geometry pass (every
            // window's panes placed, every visible PTY resized) and it was
            // being paid for each of those, for no visible change.
            if self.active_window().file_explorer_width == new_width {
                return Ok(());
            }
            self.active_window_mut().file_explorer_width = new_width;
            // The sidebar width changed: reflow terminals/viewports/panels
            // through the single layout funnel.
            self.relayout();
        }

        Ok(())
    }
}
