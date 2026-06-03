use anyhow::Result as AnyhowResult;
use rust_i18n::t;

use super::*;
use crate::view::file_tree::TreeNode;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct FileExplorerClipboard {
    pub paths: Vec<PathBuf>,
    pub is_cut: bool,
}

/// Config-derived defaults handed to `Window::install_initialized_file_explorer`
/// so the apply logic lives on `Window` without it borrowing the editor's
/// `Config` (which would clash with the `&mut Window` borrow).
#[derive(Debug, Clone, Copy)]
pub(crate) struct FileExplorerViewDefaults {
    pub show_hidden: bool,
    pub show_gitignored: bool,
    pub compact_directories: bool,
}

/// Outcome of a single filesystem-level paste op (`paste_one_fs_op`).
/// The `SourceRemovalFailed` variant is a partial success: the destination
/// exists but the original source could not be removed, so the file is
/// effectively at both locations. Callers must surface this to the user —
/// returning just an `Err` would hide the fact that the copy landed.
#[derive(Debug)]
enum PasteOpOutcome {
    /// Move / copy completed end-to-end.
    Ok,
    /// Cross-filesystem cut: copy succeeded, but removing the source failed.
    /// The file now exists at both `dst` and the original location.
    SourceRemovalFailed { dst: PathBuf, err: std::io::Error },
    /// Any other failure. Destination (if partially created) has already
    /// been cleaned up by `paste_one_fs_op`.
    Failed(std::io::Error),
}

/// Get the parent directory path from a file tree node.
/// If the node is a directory, returns its path. If it's a file, returns the parent directory.
fn get_parent_dir_path(node: &TreeNode) -> PathBuf {
    if node.is_dir() {
        node.entry.path.clone()
    } else {
        node.entry
            .path
            .parent()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| node.entry.path.clone())
    }
}

/// Generate a timestamp suffix for naming new files/directories.
fn timestamp_suffix() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs()
}

/// Get the parent node ID for refreshing after file operations.
/// If the node is a directory, the node itself is the parent. Otherwise, look up the actual parent.
fn get_parent_node_id(
    tree: &crate::view::file_tree::FileTree,
    selected_id: crate::view::file_tree::NodeId,
    node_is_dir: bool,
) -> crate::view::file_tree::NodeId {
    if node_is_dir {
        selected_id
    } else {
        tree.get_node(selected_id)
            .and_then(|n| n.parent)
            .unwrap_or(selected_id)
    }
}

impl Editor {
    pub fn file_explorer_visible(&self) -> bool {
        self.active_window().file_explorer_visible
    }

    /// Transfer keyboard focus from whatever owns it (most importantly:
    /// a live terminal) to the file explorer.
    ///
    /// `dispatch_terminal_input` routes keys to the PTY whenever
    /// `terminal_mode` is set, regardless of `key_context`. So just
    /// writing `key_context = FileExplorer` is not enough — if the user
    /// was in a terminal, every keystroke would still be swallowed by
    /// the PTY and the explorer would only *look* focused (issue #2029).
    /// Clear `terminal_mode` and remember the terminal buffer in
    /// `terminal_mode_resume` so re-focusing the terminal later restores
    /// live mode, mirroring the buffer-switch path in
    /// `set_active_buffer`.
    pub(super) fn take_focus_for_file_explorer(&mut self) {
        let win = self.active_window_mut();
        if win.terminal_mode {
            let active = win.active_buffer();
            if win.is_terminal_buffer(active) {
                win.terminal_mode_resume.insert(active);
            }
            win.terminal_mode = false;
        }
        win.key_context = KeyContext::FileExplorer;
    }

    pub fn toggle_file_explorer(&mut self) {
        let new_visible = !self.active_window().file_explorer_visible;
        self.active_window_mut().file_explorer_visible = new_visible;

        if new_visible {
            if self.file_explorer().is_none() {
                self.init_file_explorer();
            }
            self.take_focus_for_file_explorer();
            self.set_status_message(t!("explorer.opened").to_string());
            self.active_window_mut().sync_file_explorer_to_active_file();
        } else {
            self.active_window_mut().key_context = KeyContext::Normal;
            self.set_status_message(t!("explorer.closed").to_string());
        }

        // Showing/hiding the sidebar changes the editor's available width.
        // Route through the single layout funnel so split viewports and
        // terminal PTYs reflow and plugins get the (deduped) resize hook.
        self.relayout();
    }

    pub fn show_file_explorer(&mut self) {
        if !self.file_explorer_visible() {
            self.toggle_file_explorer();
        }
    }

    pub fn focus_file_explorer(&mut self) {
        if self.file_explorer_visible() {
            // Dismiss transient popups and clear hover state when focusing file explorer
            self.active_window_mut().on_editor_focus_lost();

            // Cancel search/replace prompts when switching focus away from editor
            self.active_window_mut().cancel_search_prompt_if_active();

            self.take_focus_for_file_explorer();
            self.set_status_message(t!("explorer.focused").to_string());
            self.active_window_mut().sync_file_explorer_to_active_file();
        } else {
            self.toggle_file_explorer();
        }
    }

    // `focus_editor` lives on `impl Window` — call it via
    // `self.active_window_mut().focus_editor()`.

    /// Thin delegator: building the file explorer is a per-window
    /// operation that lives on `Window` and roots at `self.root`. The
    /// editor just forwards to the active window (issue #2056 defect #3).
    pub(crate) fn init_file_explorer(&mut self) {
        self.active_window_mut().init_file_explorer();
    }

    pub fn file_explorer_navigate_up(&mut self) {
        if let Some(explorer) = self.file_explorer_mut() {
            explorer.select_prev_match();
            explorer.update_scroll_for_selection();
        }
        self.file_explorer_preview_selected();
    }

    pub fn file_explorer_navigate_down(&mut self) {
        if let Some(explorer) = self.file_explorer_mut() {
            explorer.select_next_match();
            explorer.update_scroll_for_selection();
        }
        self.file_explorer_preview_selected();
    }

    pub fn file_explorer_page_up(&mut self) {
        if let Some(explorer) = self.file_explorer_mut() {
            explorer.select_page_up();
            explorer.update_scroll_for_selection();
        }
        self.file_explorer_preview_selected();
    }

    pub fn file_explorer_page_down(&mut self) {
        if let Some(explorer) = self.file_explorer_mut() {
            explorer.select_page_down();
            explorer.update_scroll_for_selection();
        }
        self.file_explorer_preview_selected();
    }

    /// Open the currently selected file in preview mode, mirroring the
    /// single-click flow in `handle_file_explorer_click`. No-op if the
    /// selection is a directory, preview-tabs are disabled, or the open
    /// would surface an interactive prompt (e.g. large-file encoding
    /// confirmation) — the user can still commit with Enter to get the
    /// full error flow. Keeps focus on the file explorer so further
    /// keyboard navigation continues to update the preview.
    fn file_explorer_preview_selected(&mut self) {
        // Avoid turning every arrow press into a permanent tab when the
        // user has opted out of preview tabs.
        if !self.config.file_explorer.preview_tabs {
            return;
        }

        let path = match self
            .file_explorer()
            .as_ref()
            .and_then(|explorer| explorer.get_selected_entry())
        {
            Some(entry) if !entry.is_dir() => entry.path.clone(),
            _ => return,
        };

        if let Err(e) = self.open_file_preview(&path) {
            tracing::debug!(
                "file_explorer_preview_selected: skipping preview for {:?}: {}",
                path,
                e
            );
        }
    }

    /// Collapse behavior for left arrow:
    /// - If on expanded directory: collapse it
    /// - If on file or collapsed directory: select parent directory
    pub fn file_explorer_collapse(&mut self) {
        let Some(explorer) = self.file_explorer() else {
            return;
        };

        let Some(selected_id) = explorer.get_selected() else {
            return;
        };

        let Some(node) = explorer.tree().get_node(selected_id) else {
            return;
        };

        // If expanded directory, collapse it
        if node.is_dir() && node.is_expanded() {
            self.file_explorer_toggle_expand();
            return;
        }

        // Otherwise, select parent
        if let Some(explorer) = self.file_explorer_mut() {
            explorer.select_parent();
            explorer.update_scroll_for_selection();
        }
    }

    pub fn file_explorer_toggle_expand(&mut self) {
        let selected_id = if let Some(explorer) = self.file_explorer() {
            explorer.get_selected()
        } else {
            return;
        };

        let Some(selected_id) = selected_id else {
            return;
        };

        let (is_dir, is_expanded, name) = if let Some(explorer) = self.file_explorer() {
            let node = explorer.tree().get_node(selected_id);
            if let Some(node) = node {
                (node.is_dir(), node.is_expanded(), node.entry.name.clone())
            } else {
                return;
            }
        } else {
            return;
        };

        if !is_dir {
            return;
        }

        let status_msg = if is_expanded {
            t!("explorer.collapsing").to_string()
        } else {
            t!("explorer.loading_dir", name = &name).to_string()
        };
        self.set_status_message(status_msg);

        let active_id = self.active_window;
        // Disjoint borrow: `self.windows.get_mut(...)` keeps the
        // mutable explorer scoped to `self.windows`; the body still
        // reads `self.tokio_runtime`, `self.authority.filesystem`,
        // etc. on different fields.
        if let (Some(runtime), Some(explorer)) = (
            self.tokio_runtime.as_ref(),
            self.windows
                .get_mut(&active_id)
                .and_then(|w| w.file_explorer.as_mut()),
        ) {
            let result = runtime.block_on(explorer.toggle_with_chain(selected_id));

            let final_name = explorer
                .tree()
                .get_node(selected_id)
                .map(|n| n.entry.name.clone());
            let final_expanded = explorer
                .tree()
                .get_node(selected_id)
                .map(|n| n.is_expanded())
                .unwrap_or(false);

            // Track if we need to rebuild decoration cache (for symlink directories)
            let mut needs_decoration_rebuild = false;

            match result {
                Ok(()) => {
                    if final_expanded {
                        let node_info = explorer
                            .tree()
                            .get_node(selected_id)
                            .map(|n| (n.entry.path.clone(), n.entry.is_symlink()));

                        if let Some((dir_path, is_symlink)) = node_info {
                            crate::app::file_operations::load_gitignore_via_fs(
                                self.authority.filesystem.as_ref(),
                                explorer,
                                &dir_path,
                            );

                            // If a symlink directory was just expanded, we need to rebuild
                            // the decoration cache so decorations under the canonical target
                            // also appear under the symlink path
                            if is_symlink {
                                tracing::debug!(
                                    "Symlink directory expanded, will rebuild decoration cache: {:?}",
                                    dir_path
                                );
                                needs_decoration_rebuild = true;
                            }
                        }
                    }

                    if let Some(name) = final_name {
                        let msg = if final_expanded {
                            t!("explorer.expanded", name = &name).to_string()
                        } else {
                            t!("explorer.collapsed", name = &name).to_string()
                        };
                        self.set_status_message(msg);
                    }
                }
                Err(e) => {
                    self.set_status_message(
                        t!("explorer.error", error = e.to_string()).to_string(),
                    );
                }
            }

            // Rebuild decoration cache outside the explorer borrow
            if needs_decoration_rebuild {
                self.active_window_mut()
                    .rebuild_file_explorer_decoration_cache();
            }
        }
    }

    pub fn file_explorer_open_file(&mut self) -> AnyhowResult<()> {
        let entry_type = self
            .file_explorer()
            .as_ref()
            .and_then(|explorer| explorer.get_selected_entry())
            .map(|entry| (entry.is_dir(), entry.path.clone(), entry.name.clone()));

        if let Some((is_dir, path, name)) = entry_type {
            if is_dir {
                self.file_explorer_toggle_expand();
            } else {
                tracing::info!("[SYNTAX DEBUG] file_explorer opening file: {:?}", path);
                match self.open_file(&path) {
                    Ok(id) => {
                        // Double-click / Enter is the "I mean it" gesture — always
                        // promote the tab out of preview mode so subsequent clicks
                        // on *other* files don't replace this one.
                        self.active_window_mut().promote_buffer_from_preview(id);
                        self.set_status_message(
                            t!("explorer.opened_file", name = &name).to_string(),
                        );
                        self.active_window_mut().focus_editor();
                    }
                    Err(e) => {
                        // Check if this is a large file encoding confirmation error
                        // These should be shown as prompts in the UI, not as fatal errors
                        if let Some(confirmation) =
                            e.downcast_ref::<crate::model::buffer::LargeFileEncodingConfirmation>()
                        {
                            self.start_large_file_encoding_confirmation(confirmation);
                        } else {
                            self.set_status_message(
                                t!("file.error_opening", error = e.to_string()).to_string(),
                            );
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub fn file_explorer_refresh(&mut self) {
        let (selected_id, node_name) = if let Some(explorer) = self.file_explorer() {
            if let Some(selected_id) = explorer.get_selected() {
                let node_name = explorer
                    .tree()
                    .get_node(selected_id)
                    .map(|n| n.entry.name.clone());
                (Some(selected_id), node_name)
            } else {
                (None, None)
            }
        } else {
            return;
        };

        let Some(selected_id) = selected_id else {
            return;
        };

        if let Some(name) = &node_name {
            self.set_status_message(t!("explorer.refreshing", name = name).to_string());
        }

        let active_id = self.active_window;
        if let (Some(runtime), Some(explorer)) = (
            self.tokio_runtime.as_ref(),
            self.windows
                .get_mut(&active_id)
                .and_then(|w| w.file_explorer.as_mut()),
        ) {
            let tree = explorer.tree_mut();
            let result = runtime.block_on(tree.refresh_node(selected_id));
            match result {
                Ok(()) => {
                    if let Some(name) = node_name {
                        self.set_status_message(t!("explorer.refreshed", name = &name).to_string());
                    } else {
                        self.set_status_message(t!("explorer.refreshed_default").to_string());
                    }
                }
                Err(e) => {
                    self.set_status_message(
                        t!("explorer.error_refreshing", error = e.to_string()).to_string(),
                    );
                }
            }
        }
    }

    pub fn file_explorer_new_file(&mut self) {
        let active_id = self.active_window;
        if let Some(explorer) = self
            .windows
            .get_mut(&active_id)
            .and_then(|w| w.file_explorer.as_mut())
        {
            if let Some(selected_id) = explorer.get_selected() {
                let node = explorer.tree().get_node(selected_id);
                if let Some(node) = node {
                    let parent_path = get_parent_dir_path(node);
                    let filename = format!("untitled_{}.txt", timestamp_suffix());
                    let file_path = parent_path.join(&filename);

                    if let Some(runtime) = &self.tokio_runtime {
                        let path_clone = file_path.clone();
                        let result = self
                            .authority
                            .filesystem
                            .create_file(&path_clone)
                            .map(|_| ());

                        match result {
                            Ok(_) => {
                                let parent_id =
                                    get_parent_node_id(explorer.tree(), selected_id, node.is_dir());
                                let tree = explorer.tree_mut();
                                if let Err(e) =
                                    runtime.block_on(tree.reload_expanded_node(parent_id))
                                {
                                    tracing::warn!("Failed to refresh file tree: {}", e);
                                }
                                if let Some(explorer) = self.file_explorer_mut().as_mut() {
                                    explorer.navigate_to_path(&path_clone);
                                }
                                self.set_status_message(
                                    t!("explorer.created_file", name = &filename).to_string(),
                                );
                                self.notify_file_explorer_change(&path_clone);

                                // Open the file in the buffer
                                if let Err(e) = self.open_file(&path_clone) {
                                    tracing::warn!("Failed to open new file: {}", e);
                                }

                                let prompt = crate::view::prompt::Prompt::new(
                                    t!("explorer.new_file_prompt").to_string(),
                                    crate::view::prompt::PromptType::FileExplorerRename {
                                        original_path: path_clone,
                                        original_name: filename.clone(),
                                        is_new_file: true,
                                    },
                                );
                                self.active_window_mut().prompt = Some(prompt);
                            }
                            Err(e) => {
                                self.set_status_message(
                                    t!("explorer.error_creating_file", error = e.to_string())
                                        .to_string(),
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn file_explorer_new_directory(&mut self) {
        let active_id = self.active_window;
        if let Some(explorer) = self
            .windows
            .get_mut(&active_id)
            .and_then(|w| w.file_explorer.as_mut())
        {
            if let Some(selected_id) = explorer.get_selected() {
                let node = explorer.tree().get_node(selected_id);
                if let Some(node) = node {
                    let parent_path = get_parent_dir_path(node);
                    let dirname = format!("New Folder {}", timestamp_suffix());
                    let dir_path = parent_path.join(&dirname);

                    if let Some(runtime) = &self.tokio_runtime {
                        let path_clone = dir_path.clone();
                        let dirname_clone = dirname.clone();
                        let result = self.authority.filesystem.create_dir(&path_clone);

                        match result {
                            Ok(_) => {
                                let parent_id =
                                    get_parent_node_id(explorer.tree(), selected_id, node.is_dir());
                                let tree = explorer.tree_mut();
                                if let Err(e) =
                                    runtime.block_on(tree.reload_expanded_node(parent_id))
                                {
                                    tracing::warn!("Failed to refresh file tree: {}", e);
                                }
                                if let Some(explorer) = self.file_explorer_mut().as_mut() {
                                    explorer.navigate_to_path(&path_clone);
                                }
                                self.set_status_message(
                                    t!("explorer.created_dir", name = &dirname_clone).to_string(),
                                );
                                self.notify_file_explorer_change(&path_clone);

                                let prompt = crate::view::prompt::Prompt::with_initial_text(
                                    t!("explorer.new_directory_prompt").to_string(),
                                    crate::view::prompt::PromptType::FileExplorerRename {
                                        original_path: path_clone,
                                        original_name: dirname_clone,
                                        is_new_file: true,
                                    },
                                    dirname,
                                );
                                self.active_window_mut().prompt = Some(prompt);
                            }
                            Err(e) => {
                                self.set_status_message(
                                    t!("explorer.error_creating_dir", error = e.to_string())
                                        .to_string(),
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn file_explorer_delete(&mut self) {
        let Some(explorer) = self.file_explorer() else {
            return;
        };
        let root_id = explorer.tree().root_id();
        let selected_ids = explorer.effective_selection();

        let paths: Vec<(PathBuf, bool)> = selected_ids
            .iter()
            .filter(|&&id| id != root_id)
            .filter_map(|&id| {
                explorer
                    .tree()
                    .get_node(id)
                    .map(|n| (n.entry.path.clone(), n.is_dir()))
            })
            .collect();

        if paths.is_empty() {
            self.set_status_message(t!("explorer.cannot_delete_root").to_string());
            return;
        }

        if paths.len() == 1 {
            let (path, is_dir) = paths.into_iter().next().unwrap();
            let name = path
                .file_name()
                .unwrap_or_default()
                .to_string_lossy()
                .to_string();
            let type_str = if is_dir { "directory" } else { "file" };
            self.start_prompt(
                t!("explorer.delete_confirm", "type" = type_str, name = &name).to_string(),
                PromptType::ConfirmDeleteFile { path, is_dir },
            );
        } else {
            let count = paths.len();
            let all_paths: Vec<PathBuf> = paths.into_iter().map(|(p, _)| p).collect();
            // Preview the first few names so the user can eyeball what's
            // about to be deleted. Include '…' when there are more than
            // fit in the minibuffer budget.
            let names = format_path_preview_for_prompt(&all_paths, 3);
            self.start_prompt(
                t!(
                    "explorer.delete_multi_confirm",
                    count = count,
                    names = &names
                )
                .to_string(),
                PromptType::ConfirmMultiDelete { paths: all_paths },
            );
        }
    }

    /// Perform the actual file explorer delete operation (called after prompt confirmation)
    /// For local files: moves to system trash/recycle bin
    /// For remote files: moves to ~/.local/share/fresh/trash/ on remote
    pub fn perform_file_explorer_delete(&mut self, path: std::path::PathBuf, _is_dir: bool) {
        let name = path
            .file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_default();

        // For remote files, move to remote trash directory
        // For local files, use system trash
        let delete_result = if self.authority.filesystem.remote_connection_info().is_some() {
            self.move_to_remote_trash(&path)
        } else {
            trash::delete(&path).map_err(std::io::Error::other)
        };

        match delete_result {
            Ok(_) => {
                // Close any open buffers backed by the deleted path (or
                // any file that lived under it, for a directory delete).
                // Without this, the tab keeps rendering with stale
                // content and `Ctrl+S` would write the buffer right back
                // to the trashed path, silently resurrecting the file
                // the user just deleted. The user confirmed the trash
                // action, which implies discarding unsaved edits to the
                // doomed file too — `force_close_buffer` skips the
                // modified-check so the buffer really goes away.
                let to_close = self.buffer_ids_under_path(&path);
                for id in to_close {
                    if let Err(e) = self.force_close_buffer(id) {
                        tracing::warn!(
                            "Failed to close buffer {:?} after delete of {:?}: {}",
                            id,
                            path,
                            e
                        );
                    }
                }

                // Refresh the parent directory in the file explorer
                let active_id = self.active_window;
                if let Some(explorer) = self
                    .windows
                    .get_mut(&active_id)
                    .and_then(|w| w.file_explorer.as_mut())
                {
                    if let Some(runtime) = &self.tokio_runtime {
                        // Find the node for the deleted path and get its parent
                        if let Some(node) = explorer.tree().get_node_by_path(&path) {
                            let node_id = node.id;
                            let parent_id = get_parent_node_id(explorer.tree(), node_id, false);

                            // Remember the index of the deleted node in the visible list
                            let deleted_index = explorer.get_selected_index();

                            if let Err(e) = runtime
                                .block_on(explorer.tree_mut().reload_expanded_node(parent_id))
                            {
                                tracing::warn!("Failed to refresh file tree after delete: {}", e);
                            }

                            // The deleted node's NodeId (and any siblings
                            // that went away with the parent refresh) can
                            // still be in multi_selection. Drop the stale
                            // entries so the next op targets the fresh cursor.
                            explorer.clear_multi_selection();

                            // After refresh, select the next best node:
                            // Try to stay at the same index, or select the last visible item
                            let count = explorer.visible_count();
                            if count > 0 {
                                let new_index = if let Some(idx) = deleted_index {
                                    idx.min(count.saturating_sub(1))
                                } else {
                                    0
                                };
                                if let Some(node_id) = explorer.get_node_at_index(new_index) {
                                    explorer.set_selected(Some(node_id));
                                }
                            } else {
                                // No visible nodes, select parent
                                explorer.set_selected(Some(parent_id));
                            }
                        }
                    }
                }
                self.set_status_message(t!("explorer.moved_to_trash", name = &name).to_string());
                self.notify_file_explorer_change(&path);

                // Ensure focus remains on file explorer
                self.active_window_mut().key_context = KeyContext::FileExplorer;
            }
            Err(e) => {
                self.set_status_message(
                    t!("explorer.error_trash", error = e.to_string()).to_string(),
                );
            }
        }
    }

    /// Move a file/directory to the remote trash directory (~/.local/share/fresh/trash/)
    fn move_to_remote_trash(&self, path: &std::path::Path) -> std::io::Result<()> {
        // Get remote home directory
        let home = self.authority.filesystem.home_dir()?;
        let trash_dir = home.join(".local/share/fresh/trash");

        // Create trash directory if it doesn't exist
        if !self.authority.filesystem.exists(&trash_dir) {
            self.authority.filesystem.create_dir_all(&trash_dir)?;
        }

        // Generate unique name with timestamp to avoid collisions
        let file_name = path
            .file_name()
            .unwrap_or_else(|| std::ffi::OsStr::new("unnamed"));
        let timestamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);
        let trash_name = format!("{}.{}", file_name.to_string_lossy(), timestamp);
        let trash_path = trash_dir.join(trash_name);

        // Move to trash
        self.authority.filesystem.rename(path, &trash_path)
    }

    pub fn file_explorer_rename(&mut self) {
        if let Some(explorer) = self.file_explorer() {
            if let Some(selected_id) = explorer.get_selected() {
                // Don't allow renaming the root directory
                if selected_id == explorer.tree().root_id() {
                    self.set_status_message(t!("explorer.cannot_rename_root").to_string());
                    return;
                }

                let node = explorer.tree().get_node(selected_id);
                if let Some(node) = node {
                    let old_path = node.entry.path.clone();
                    let old_name = node.entry.name.clone();

                    // Create a prompt for the new name, pre-filled with the
                    // old name and cursor at the end — the user typically
                    // edits a suffix or extension rather than replacing the
                    // whole name, so keep the prefill and let them type.
                    let prompt = crate::view::prompt::Prompt::with_initial_text_for_edit(
                        t!("explorer.rename_prompt").to_string(),
                        crate::view::prompt::PromptType::FileExplorerRename {
                            original_path: old_path,
                            original_name: old_name.clone(),
                            is_new_file: false,
                        },
                        old_name,
                    );
                    self.active_window_mut().prompt = Some(prompt);
                }
            }
        }
    }

    /// Perform the actual file explorer rename operation (called after prompt confirmation)
    pub fn perform_file_explorer_rename(
        &mut self,
        original_path: std::path::PathBuf,
        original_name: String,
        new_name: String,
        is_new_file: bool,
    ) {
        if new_name.is_empty() || new_name == original_name {
            self.set_status_message(t!("explorer.rename_cancelled").to_string());
            return;
        }

        // Reject any platform path separator — `/` on all OSes plus `\` on
        // Windows. `is_separator` is const-folded per platform so this keeps
        // the same behavior on Linux (reject `/`) while also rejecting `\`
        // when running on Windows.
        if new_name.chars().any(std::path::is_separator) {
            self.set_status_message(t!("explorer.rename_invalid_separator").to_string());
            return;
        }
        if new_name == "." || new_name == ".." {
            self.set_status_message(t!("explorer.rename_invalid_dot").to_string());
            return;
        }

        let new_path = original_path
            .parent()
            .map(|p| p.join(&new_name))
            .unwrap_or_else(|| original_path.clone());

        if self.tokio_runtime.is_some() {
            let result = self.authority.filesystem.rename(&original_path, &new_path);

            match result {
                Ok(_) => {
                    // Refresh the parent directory and select the renamed item.
                    // Direct `self.windows.get_mut(...)` keeps the explorer
                    // borrow disjoint from `self.tokio_runtime`.
                    let active_id = self.active_window;
                    if let (Some(runtime), Some(explorer)) = (
                        self.tokio_runtime.as_ref(),
                        self.windows
                            .get_mut(&active_id)
                            .and_then(|w| w.file_explorer.as_mut()),
                    ) {
                        if let Some(selected_id) = explorer.get_selected() {
                            let parent_id = get_parent_node_id(explorer.tree(), selected_id, false);
                            let tree = explorer.tree_mut();
                            if let Err(e) = runtime.block_on(tree.reload_expanded_node(parent_id)) {
                                tracing::warn!("Failed to refresh file tree after rename: {}", e);
                            }
                        }
                        // The renamed node has a new NodeId under the parent;
                        // drop stale selections before navigating to the new
                        // path so subsequent ops target the renamed item.
                        explorer.clear_multi_selection();
                        // Navigate to the renamed file to restore selection
                        explorer.navigate_to_path(&new_path);
                    }

                    // Update every buffer whose path lives at or under the
                    // renamed root — for a plain file this is the buffer for
                    // that file itself; for a directory rename it's every
                    // buffer backed by a file inside the renamed directory.
                    // Without this, saving such a buffer would recreate the
                    // old-name path, leaving behind a ghost alongside the
                    // renamed file.
                    let relocated = self.relocate_buffers_for_rename(&original_path, &new_path);

                    // Only switch focus to the buffer if this is a new file
                    // being created. For renames from the explorer, keep
                    // focus in the explorer.
                    if is_new_file && !relocated.is_empty() {
                        self.active_window_mut().key_context = KeyContext::Normal;
                    }

                    self.set_status_message(
                        t!("explorer.renamed", old = &original_name, new = &new_name).to_string(),
                    );
                    self.notify_file_explorer_change(&new_path);
                }
                Err(e) => {
                    self.set_status_message(
                        t!("explorer.error_renaming", error = e.to_string()).to_string(),
                    );
                }
            }
        }
    }

    pub fn file_explorer_toggle_hidden(&mut self) {
        let show_hidden = if let Some(explorer) = self.file_explorer_mut() {
            explorer.toggle_show_hidden();
            explorer.ignore_patterns().show_hidden()
        } else {
            return;
        };

        let msg = if show_hidden {
            t!("explorer.showing_hidden")
        } else {
            t!("explorer.hiding_hidden")
        };
        self.set_status_message(msg.to_string());

        // Persist to config so the setting survives across sessions
        self.config_mut().file_explorer.show_hidden = show_hidden;
        self.persist_config_change(
            "/file_explorer/show_hidden",
            serde_json::Value::Bool(show_hidden),
        );
    }

    pub fn file_explorer_toggle_gitignored(&mut self) {
        let show_gitignored = if let Some(explorer) = self.file_explorer_mut() {
            explorer.toggle_show_gitignored();
            explorer.ignore_patterns().show_gitignored()
        } else {
            return;
        };

        let msg = if show_gitignored {
            t!("explorer.showing_gitignored")
        } else {
            t!("explorer.hiding_gitignored")
        };
        self.set_status_message(msg.to_string());

        // Persist to config so the setting survives across sessions
        self.config_mut().file_explorer.show_gitignored = show_gitignored;
        self.persist_config_change(
            "/file_explorer/show_gitignored",
            serde_json::Value::Bool(show_gitignored),
        );
    }

    /// Clear the file explorer search (or multi-selection, pending cut, or transfer focus)
    // `file_explorer_search_clear` lives on `impl Window` — call it via
    // `self.active_window_mut().file_explorer_search_clear()`.

    // `file_explorer_extend_selection_up/down`,
    // `file_explorer_toggle_select`, `file_explorer_select_all`,
    // `file_explorer_search_push_char`, `file_explorer_search_pop_char`
    // moved to `impl Window`. Editor callers reach them via
    // `self.active_window_mut().file_explorer_X(...)`.

    // `handle_set_file_explorer_decorations`,
    // `handle_clear_file_explorer_decorations`, and
    // `rebuild_file_explorer_decoration_cache` live on `impl Window` —
    // call them via `self.active_window_mut()`.

    // `file_explorer_clipboard`, `file_explorer_copy`, `file_explorer_cut`
    // and the shared `set_explorer_clipboard` helper live on `impl Window`
    // — call them via `self.active_window()` / `self.active_window_mut()`.

    pub fn file_explorer_paste(&mut self) {
        let clipboard = match self.active_window().file_explorer_clipboard.clone() {
            Some(c) => c,
            None => {
                self.set_status_message(t!("explorer.paste_no_source").to_string());
                return;
            }
        };

        let dst_dir = if let Some(explorer) = self.file_explorer() {
            if let Some(selected_id) = explorer.get_selected() {
                if let Some(node) = explorer.tree().get_node(selected_id) {
                    get_parent_dir_path(node)
                } else {
                    return;
                }
            } else {
                return;
            }
        } else {
            return;
        };

        let is_cut = clipboard.is_cut;

        if clipboard.paths.len() == 1 {
            let src = clipboard.paths[0].clone();
            let file_name = match src.file_name() {
                Some(n) => n.to_os_string(),
                None => return,
            };
            let dst_path = dst_dir.join(&file_name);

            if src.parent().map(|p| p == dst_dir).unwrap_or(false) {
                if is_cut {
                    // Same-dir paste of a cut is effectively "changed my
                    // mind": treat it as a cancel rather than surfacing a
                    // scary error. Must clear the clipboard, otherwise a
                    // later paste elsewhere would silently move the file.
                    self.active_window_mut().file_explorer_clipboard = None;
                    self.set_status_message(t!("explorer.cut_cancelled").to_string());
                    return;
                } else {
                    let unique = unique_paste_name(
                        &*self.authority.filesystem,
                        &dst_dir,
                        &file_name.to_string_lossy(),
                    );
                    self.perform_file_explorer_paste(src, unique, false);
                    return;
                }
            }

            if self.authority.filesystem.exists(&dst_path) {
                let name = truncate_name_for_prompt(&file_name.to_string_lossy(), 40);
                self.start_prompt(
                    t!("explorer.paste_conflict", name = &name).to_string(),
                    crate::view::prompt::PromptType::ConfirmPasteConflict {
                        src,
                        dst: dst_path,
                        is_cut,
                    },
                );
            } else {
                self.perform_file_explorer_paste(src, dst_path, is_cut);
            }
        } else {
            // Multi-path: categorize into safe and conflicting destinations
            let mut safe: Vec<(PathBuf, PathBuf)> = Vec::new();
            let mut conflicts: Vec<(PathBuf, PathBuf)> = Vec::new();

            for src in &clipboard.paths {
                let file_name = match src.file_name() {
                    Some(n) => n.to_os_string(),
                    None => continue,
                };
                let dst_path = dst_dir.join(&file_name);
                let is_same_location = src.parent().map(|p| p == dst_dir).unwrap_or(false);

                if is_same_location {
                    if !is_cut {
                        // Copy to same dir: auto-rename so it lands in safe
                        let unique = unique_paste_name(
                            &*self.authority.filesystem,
                            &dst_dir,
                            &file_name.to_string_lossy(),
                        );
                        safe.push((src.clone(), unique));
                    }
                    // Cut to same dir: skip — nothing to do
                } else if self.authority.filesystem.exists(&dst_path) {
                    conflicts.push((src.clone(), dst_path));
                } else {
                    safe.push((src.clone(), dst_path));
                }
            }

            if safe.is_empty() && conflicts.is_empty() {
                // For cut, an all-same-dir paste is a cancel (see the
                // single-path branch above). Clear the clipboard so a
                // later paste can't silently move the files after all.
                if is_cut {
                    self.active_window_mut().file_explorer_clipboard = None;
                    self.set_status_message(t!("explorer.cut_cancelled").to_string());
                } else {
                    self.set_status_message(t!("explorer.paste_same_location").to_string());
                }
                return;
            }

            if conflicts.is_empty() {
                self.execute_resolved_multi_paste(safe, vec![], is_cut);
            } else {
                let name = truncate_name_for_prompt(
                    &conflicts[0]
                        .1
                        .file_name()
                        .unwrap_or_default()
                        .to_string_lossy(),
                    40,
                );
                self.start_prompt(
                    t!("explorer.paste_conflict_multi", name = &name).to_string(),
                    crate::view::prompt::PromptType::ConfirmMultiPasteConflict {
                        safe,
                        confirmed: Vec::new(),
                        pending: conflicts,
                        is_cut,
                    },
                );
            }
        }
    }

    /// Paste all resolved items (safe + confirmed-overwrite) from a multi-conflict flow.
    ///
    /// Runs every filesystem op first, then does a single tree refresh and
    /// a single navigate to the first successfully pasted item. Each paste
    /// inside `perform_file_explorer_paste` would otherwise re-reload the
    /// same parent directories N times and flash N different status
    /// messages, with only the last one ever being visible.
    pub(super) fn execute_resolved_multi_paste(
        &mut self,
        safe: Vec<(PathBuf, PathBuf)>,
        to_overwrite: Vec<(PathBuf, PathBuf)>,
        is_cut: bool,
    ) {
        let total = safe.len() + to_overwrite.len();
        if total == 0 {
            return;
        }

        let mut succeeded: Vec<(PathBuf, PathBuf)> = Vec::with_capacity(total);
        // Clean moves are those that actually relocated the file off of
        // `src`. Partial moves (copy landed, source delete failed)
        // appear in `succeeded` so the tree refresh picks up the new
        // dst, but are intentionally NOT in `clean_moves`: their
        // sources still exist, so open buffers for them should keep
        // pointing at `src`, not follow the copy.
        let mut clean_moves: Vec<(PathBuf, PathBuf)> = Vec::with_capacity(total);
        let mut first_error: Option<std::io::Error> = None;
        let mut partial_moves: Vec<(PathBuf, std::io::Error)> = Vec::new();
        for (src, dst) in safe.into_iter().chain(to_overwrite) {
            match self.paste_one_fs_op(&src, &dst, is_cut) {
                PasteOpOutcome::Ok => {
                    clean_moves.push((src.clone(), dst.clone()));
                    succeeded.push((src, dst));
                }
                PasteOpOutcome::SourceRemovalFailed {
                    dst: landed_dst,
                    err,
                } => {
                    // Copy landed; count the dst as visible in the tree
                    // (so the refresh below picks it up), but track the
                    // partial state so the status message calls it out.
                    succeeded.push((src, landed_dst.clone()));
                    partial_moves.push((landed_dst, err));
                }
                PasteOpOutcome::Failed(e) => {
                    if first_error.is_none() {
                        first_error = Some(e);
                    }
                }
            }
        }

        // For cut (move), re-point any open buffer whose file was
        // among the clean moves to its new on-disk home. Without this,
        // saving such a buffer would recreate the file at its old
        // source path. Copies don't need this — they create a new
        // file at dst without disturbing the source buffer.
        if is_cut {
            for (src, dst) in &clean_moves {
                self.relocate_buffers_for_rename(src, dst);
            }
        }

        if !succeeded.is_empty() {
            let first_dst = succeeded[0].1.clone();
            let any_src = succeeded[0].0.clone();
            self.refresh_tree_after_paste(&any_src, &first_dst, is_cut);
        }

        if !partial_moves.is_empty() {
            // Partial-move always wins the status line: the user needs to
            // know some sources are still present.
            let (first_dst, first_err) = &partial_moves[0];
            let name = first_dst
                .file_name()
                .map(|n| n.to_string_lossy().to_string())
                .unwrap_or_default();
            let msg = if partial_moves.len() == 1 {
                t!(
                    "explorer.move_source_removal_failed",
                    name = &name,
                    error = first_err.to_string()
                )
                .to_string()
            } else {
                t!(
                    "explorer.move_source_removal_failed_n",
                    count = partial_moves.len()
                )
                .to_string()
            };
            self.set_status_message(msg);
        } else if let Some(e) = &first_error {
            let msg = if is_cut {
                t!("explorer.error_moving", error = e.to_string()).to_string()
            } else {
                t!("explorer.error_copying", error = e.to_string()).to_string()
            };
            self.set_status_message(msg);
        } else if total > 1 {
            let msg = if is_cut {
                t!("explorer.pasted_moved_n", count = total).to_string()
            } else {
                t!("explorer.pasted_n", count = total).to_string()
            };
            self.set_status_message(msg);
        } else if let Some((_, dst)) = succeeded.first() {
            let name = dst
                .file_name()
                .map(|n| n.to_string_lossy().to_string())
                .unwrap_or_default();
            let msg = if is_cut {
                t!("explorer.pasted_moved", name = &name).to_string()
            } else {
                t!("explorer.pasted", name = &name).to_string()
            };
            self.set_status_message(msg);
        }

        // Clear the clipboard only when the move was fully clean — if a
        // source is still sitting at its original location the user may
        // want to retry, and the clipboard still contains the right path.
        if is_cut && first_error.is_none() && partial_moves.is_empty() {
            self.active_window_mut().file_explorer_clipboard = None;
        }
        self.active_window_mut().key_context = KeyContext::FileExplorer;
    }

    /// Move or copy a single item at the filesystem level. No tree or UI
    /// state is touched — callers are responsible for refreshing the
    /// explorer afterwards.
    fn paste_one_fs_op(&self, src: &Path, dst: &Path, is_cut: bool) -> PasteOpOutcome {
        let src_is_dir = self.authority.filesystem.is_dir(src).unwrap_or(false);

        // Guard against pasting a directory into itself or into one of its
        // own descendants. Without this, `copy_dir_all(/d, /d/d)` would
        // create `/d/d`, then iterate `/d` — which now contains the
        // just-created `/d/d` — and recurse forever until stack overflow
        // or disk-full. The check applies only when the source is a
        // directory; file-into-itself is already handled by the
        // same-location check in `file_explorer_paste`.
        if src_is_dir && dst.starts_with(src) {
            return PasteOpOutcome::Failed(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "Cannot paste a directory into itself",
            ));
        }

        if is_cut {
            // Try rename first (works if same filesystem). Only fall back to
            // copy+delete for cross-device errors — any other rename failure
            // (permission denied, etc.) must surface as-is so we don't
            // silently succeed via a different codepath.
            match self.authority.filesystem.rename(src, dst) {
                Ok(()) => PasteOpOutcome::Ok,
                Err(e) if e.kind() == std::io::ErrorKind::CrossesDevices => {
                    let copy_result = if src_is_dir {
                        self.authority.filesystem.copy_dir_all(src, dst)
                    } else {
                        self.authority.filesystem.copy(src, dst).map(|_| ())
                    };
                    match copy_result {
                        Ok(()) => {
                            // Copy landed. Now remove the source to complete
                            // the move. If that fails, surface it as a
                            // distinct outcome — the user needs to know the
                            // copy is at `dst` AND the original is still at
                            // `src`, so they can decide what to do.
                            let remove_result = if src_is_dir {
                                self.authority.filesystem.remove_dir_all(src)
                            } else {
                                self.authority.filesystem.remove_file(src)
                            };
                            match remove_result {
                                Ok(()) => PasteOpOutcome::Ok,
                                Err(remove_err) => PasteOpOutcome::SourceRemovalFailed {
                                    dst: dst.to_path_buf(),
                                    err: remove_err,
                                },
                            }
                        }
                        Err(copy_err) => {
                            // Roll back the half-written destination so the
                            // user isn't left with a partial copy alongside
                            // the intact source. Cleanup errors are
                            // swallowed — the copy error is the interesting
                            // one to surface — but logged.
                            let cleanup = if src_is_dir {
                                self.authority.filesystem.remove_dir_all(dst)
                            } else {
                                self.authority.filesystem.remove_file(dst)
                            };
                            if let Err(cleanup_err) = cleanup {
                                tracing::warn!(
                                    "Failed to roll back partial destination {:?} after copy \
                                     fallback failed: {}",
                                    dst,
                                    cleanup_err
                                );
                            }
                            PasteOpOutcome::Failed(copy_err)
                        }
                    }
                }
                Err(e) => PasteOpOutcome::Failed(e),
            }
        } else if src_is_dir {
            match self.authority.filesystem.copy_dir_all(src, dst) {
                Ok(()) => PasteOpOutcome::Ok,
                Err(e) => PasteOpOutcome::Failed(e),
            }
        } else {
            match self.authority.filesystem.copy(src, dst) {
                Ok(_) => PasteOpOutcome::Ok,
                Err(e) => PasteOpOutcome::Failed(e),
            }
        }
    }

    /// Refresh the destination (and source parent, if this was a cut) in
    /// the explorer tree after paste operations land on disk, then navigate
    /// the cursor to `dst`. Factored out so multi-paste can invoke it
    /// exactly once for a whole batch rather than N times.
    fn refresh_tree_after_paste(&mut self, src: &Path, dst: &Path, is_cut: bool) {
        let active_id = self.active_window;
        // Disjoint borrow on `self.windows` so the body can also read
        // `self.tokio_runtime`.
        let Some(explorer) = self
            .windows
            .get_mut(&active_id)
            .and_then(|w| w.file_explorer.as_mut())
        else {
            return;
        };
        if let Some(runtime) = &self.tokio_runtime {
            // Refresh destination parent in-place to avoid collapsing it
            if let Some(dst_parent) = dst.parent() {
                if let Some(dst_parent_node) = explorer.tree().get_node_by_path(dst_parent) {
                    let pid = dst_parent_node.id;
                    if let Err(e) = runtime.block_on(explorer.tree_mut().reload_expanded_node(pid))
                    {
                        tracing::warn!("Failed to reload destination directory after paste: {}", e);
                    }
                }
            }
            // Refresh source parent too (if cut). Using `reload_expanded_node`
            // here rather than `refresh_node` is important: refresh_node
            // collapses and re-expands the source parent, which wipes out
            // every descendant NodeId — including the destination directory
            // that was just expanded above. That in turn invalidates the
            // cursor (`selected_node`) and any NodeIds held elsewhere
            // (e.g. hover, decorations). The in-place reload keeps
            // unchanged siblings intact and only drops the nodes that
            // really went away.
            if is_cut {
                if let Some(src_parent) = src.parent() {
                    if let Some(src_parent_node) = explorer.tree().get_node_by_path(src_parent) {
                        let pid = src_parent_node.id;
                        if let Err(e) =
                            runtime.block_on(explorer.tree_mut().reload_expanded_node(pid))
                        {
                            tracing::warn!("Failed to refresh source directory after move: {}", e);
                        }
                    }
                }
            }
        }
        // Any source NodeIds that were in the multi-selection are now stale
        // (the tree was reloaded / source parent refreshed). Drop the
        // selection so subsequent actions act on the fresh cursor, not
        // ghost IDs.
        explorer.clear_multi_selection();
        explorer.navigate_to_path(dst);

        self.notify_file_explorer_change(dst);
    }

    /// Fire the `after_file_explorer_change` plugin hook for an
    /// explorer-driven on-disk mutation (create / rename / delete /
    /// paste / duplicate / ...). Plugins that surface filesystem-derived
    /// state — git status badges, etc. — subscribe to this in addition
    /// to `after_file_save`, since explorer-driven changes never fire
    /// the buffer-save hooks.
    ///
    /// `path` is one of the affected paths (destination for move/copy,
    /// the deleted path for delete, the new path for create/rename).
    /// Multi-target operations call this once per refresh, not once per
    /// file.
    pub(super) fn notify_file_explorer_change(&self, path: &Path) {
        self.plugin_manager.read().unwrap().run_hook(
            "after_file_explorer_change",
            crate::services::plugins::hooks::HookArgs::AfterFileExplorerChange {
                path: path.to_path_buf(),
            },
        );
    }

    pub fn perform_file_explorer_paste(&mut self, src: PathBuf, dst: PathBuf, is_cut: bool) {
        let name = dst
            .file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_default();

        match self.paste_one_fs_op(&src, &dst, is_cut) {
            PasteOpOutcome::Ok => {
                // For cut (move), re-point any open buffer at src to
                // its new home at dst — before the tree refresh, since
                // the refresh re-resolves the cursor by path and we
                // want the buffer state consistent with the tree at
                // all observation points. A pure copy doesn't disturb
                // source buffers.
                if is_cut {
                    self.relocate_buffers_for_rename(&src, &dst);
                }
                self.refresh_tree_after_paste(&src, &dst, is_cut);
                if is_cut {
                    self.active_window_mut().file_explorer_clipboard = None;
                    self.set_status_message(t!("explorer.pasted_moved", name = &name).to_string());
                } else {
                    self.set_status_message(t!("explorer.pasted", name = &name).to_string());
                }
                self.active_window_mut().key_context = KeyContext::FileExplorer;
            }
            PasteOpOutcome::SourceRemovalFailed {
                dst: landed_dst,
                err,
            } => {
                // The copy is at landed_dst; the source is still at src.
                // Refresh the tree so both are visible, keep the clipboard
                // populated so the user can retry, and spell out both
                // sides of the partial state in the status line.
                self.refresh_tree_after_paste(&src, &landed_dst, is_cut);
                self.set_status_message(
                    t!(
                        "explorer.move_source_removal_failed",
                        name = &name,
                        error = err.to_string()
                    )
                    .to_string(),
                );
                // NB: don't clear the clipboard — source is still at its
                // original location and the user may want to retry.
                self.active_window_mut().key_context = KeyContext::FileExplorer;
            }
            PasteOpOutcome::Failed(e) => {
                let msg = if is_cut {
                    t!("explorer.error_moving", error = e.to_string()).to_string()
                } else {
                    t!("explorer.error_copying", error = e.to_string()).to_string()
                };
                self.set_status_message(msg);
            }
        }
    }

    /// Duplicate the selected file/directory in-place, naming the new copy
    /// using the same `name copy[.ext]` convention as Paste's auto-rename.
    ///
    /// Multi-selection duplicates each item independently; the project
    /// root is skipped (you can't duplicate the project root itself).
    pub fn file_explorer_duplicate(&mut self) {
        let Some(explorer) = self.file_explorer() else {
            return;
        };
        let root_id = explorer.tree().root_id();
        let selected_ids = explorer.effective_selection();
        let sources: Vec<PathBuf> = selected_ids
            .iter()
            .filter(|&&id| id != root_id)
            .filter_map(|&id| explorer.tree().get_node(id).map(|n| n.entry.path.clone()))
            .collect();

        if sources.is_empty() {
            self.set_status_message(t!("explorer.cannot_duplicate_root").to_string());
            return;
        }

        // Resolve destination paths up front so we don't observe an
        // intermediate filesystem state for siblings duplicated in the
        // same parent directory.
        let mut ops: Vec<(PathBuf, PathBuf)> = Vec::with_capacity(sources.len());
        for src in &sources {
            let Some(parent) = src.parent() else {
                continue;
            };
            let Some(file_name) = src.file_name() else {
                continue;
            };
            let dst = unique_paste_name(
                &*self.authority.filesystem,
                parent,
                &file_name.to_string_lossy(),
            );
            ops.push((src.clone(), dst));
        }

        if ops.is_empty() {
            return;
        }

        let mut succeeded: Vec<(PathBuf, PathBuf)> = Vec::with_capacity(ops.len());
        let mut first_error: Option<std::io::Error> = None;
        for (src, dst) in ops {
            match self.paste_one_fs_op(&src, &dst, false) {
                PasteOpOutcome::Ok => succeeded.push((src, dst)),
                PasteOpOutcome::SourceRemovalFailed { .. } => {
                    // is_cut=false above; this variant is unreachable for copies.
                    unreachable!("paste_one_fs_op returned SourceRemovalFailed for a non-cut op");
                }
                PasteOpOutcome::Failed(e) => {
                    if first_error.is_none() {
                        first_error = Some(e);
                    }
                }
            }
        }

        if !succeeded.is_empty() {
            let (first_src, first_dst) = succeeded[0].clone();
            self.refresh_tree_after_paste(&first_src, &first_dst, false);
        }

        let msg = if let Some(e) = &first_error {
            t!("explorer.error_copying", error = e.to_string()).to_string()
        } else if succeeded.len() == 1 {
            let name = succeeded[0]
                .1
                .file_name()
                .map(|n| n.to_string_lossy().to_string())
                .unwrap_or_default();
            t!("explorer.duplicated", name = &name).to_string()
        } else {
            t!("explorer.duplicated_n", count = succeeded.len()).to_string()
        };
        self.set_status_message(msg);
        self.active_window_mut().key_context = KeyContext::FileExplorer;
    }

    /// Copy the selected node's path(s) to the clipboard.
    ///
    /// `relative=true` strips `working_dir` from each path when it is a
    /// prefix; otherwise the absolute path is used. Multiple selections
    /// are joined by newlines, in the same visible order shown by the
    /// tree, so the result is friendly for pasting into a shell or list.
    pub fn file_explorer_copy_path(&mut self, relative: bool) {
        let Some(explorer) = self.file_explorer() else {
            return;
        };
        let selected_ids = explorer.effective_selection();
        let paths: Vec<PathBuf> = selected_ids
            .iter()
            .filter_map(|&id| explorer.tree().get_node(id).map(|n| n.entry.path.clone()))
            .collect();

        if paths.is_empty() {
            self.set_status_message(t!("clipboard.no_file_path").to_string());
            return;
        }

        let working_dir = self.working_dir().to_path_buf();
        let rendered: Vec<String> = paths
            .iter()
            .map(|p| {
                if relative {
                    p.strip_prefix(&working_dir)
                        .unwrap_or(p)
                        .to_string_lossy()
                        .into_owned()
                } else {
                    p.to_string_lossy().into_owned()
                }
            })
            .collect();

        let joined = rendered.join("\n");
        self.clipboard.copy(joined.clone());

        let msg = if rendered.len() == 1 {
            t!("clipboard.copied_path", path = &rendered[0]).to_string()
        } else {
            t!("clipboard.copied_paths_n", count = rendered.len()).to_string()
        };
        self.set_status_message(msg);
    }
}

impl crate::app::window::Window {
    /// Build this window's file explorer, rooted at the window's own
    /// `root`. A `Window` has no access to any other project's path, so
    /// the explorer is correct-by-construction (issue #2056 defect #3):
    /// it spawns the tree build on the window's own runtime/bridge using
    /// `self.resources`. For remote mode, fall back to the remote home
    /// dir only when `root` doesn't exist on the remote filesystem.
    pub(crate) fn init_file_explorer(&mut self) {
        let is_remote = self
            .resources
            .authority
            .filesystem
            .remote_connection_info()
            .is_some();
        let root_exists = self
            .resources
            .authority
            .filesystem
            .is_dir(&self.root)
            .unwrap_or(false);
        let root_path = if is_remote && !root_exists {
            match self.resources.authority.filesystem.home_dir() {
                Ok(home) => home,
                Err(e) => {
                    tracing::error!("Failed to get remote home directory: {}", e);
                    self.set_status_message(format!("Failed to get remote home: {}", e));
                    return;
                }
            }
        } else {
            self.root.clone()
        };

        let Some(runtime) = self.resources.tokio_runtime.clone() else {
            return;
        };
        let fs_manager = Arc::clone(&self.resources.fs_manager);
        let sender = self.bridge.sender();
        // Tag the result with *this* window so it lands here even if another
        // window is active by the time the async build finishes.
        let window_id = self.id;
        runtime.spawn(async move {
            match FileTree::new(root_path, fs_manager).await {
                Ok(mut tree) => {
                    let root_id = tree.root_id();
                    if let Err(e) = tree.expand_node(root_id).await {
                        tracing::warn!("Failed to expand root directory: {}", e);
                    }
                    let view = FileTreeView::new(tree);
                    // Receiver may have been dropped during shutdown.
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = sender.send(AsyncMessage::FileExplorerInitialized {
                        window: window_id,
                        view,
                    });
                }
                Err(e) => {
                    tracing::error!("Failed to initialize file explorer: {}", e);
                }
            }
        });
        self.set_status_message(t!("explorer.initializing").to_string());
    }

    /// Install a freshly-built file tree (from `init_file_explorer`) onto
    /// *this* window. The editor's async dispatch routes the result back to the
    /// requesting window and calls this — so a tree built for a backgrounded
    /// (e.g. previewed) window can never land on whatever window happens to be
    /// active, which previously clobbered the active window's explorer.
    pub(crate) fn install_initialized_file_explorer(
        &mut self,
        mut view: FileTreeView,
        defaults: FileExplorerViewDefaults,
    ) {
        let root_id = view.tree().root_id();
        if let Some(root_path) = view.tree().get_node(root_id).map(|n| n.entry.path.clone()) {
            crate::app::file_operations::load_gitignore_via_fs(
                self.resources.authority.filesystem.as_ref(),
                &mut view,
                &root_path,
            );
        }
        // Pending session-restore values win; otherwise fall back to config so
        // the setting survives across sessions (fixes #569).
        let show_hidden = self
            .pending_file_explorer_show_hidden
            .take()
            .unwrap_or(defaults.show_hidden);
        view.ignore_patterns_mut().set_show_hidden(show_hidden);
        let show_gitignored = self
            .pending_file_explorer_show_gitignored
            .take()
            .unwrap_or(defaults.show_gitignored);
        view.ignore_patterns_mut()
            .set_show_gitignored(show_gitignored);
        view.set_compact_directories(defaults.compact_directories);
        self.file_explorer = Some(view);
        // Auto-expand to reveal the active file on first open (issue #1569),
        // but only when this window is actually showing the explorer.
        if self.file_explorer_visible {
            self.sync_file_explorer_to_active_file();
        }
    }

    /// Install an async expand-to-path result onto *this* window (routed
    /// per-window for the same reason as `install_initialized_file_explorer`).
    pub(crate) fn install_expanded_file_explorer(&mut self, mut view: FileTreeView) {
        view.update_scroll_for_selection();
        self.file_explorer = Some(view);
        self.file_explorer_sync_in_progress = false;
    }

    /// Shift focus back to the editor pane (away from the file explorer)
    /// and post a per-window "Editor focused" status message.
    pub fn focus_editor(&mut self) {
        self.key_context = KeyContext::Normal;
        self.set_status_message(t!("editor.focused").to_string());
    }

    /// Clear file-explorer state in priority order:
    ///   1. If a pending cut sits in the clipboard, just cancel it (so a
    ///      forgotten cut can't silently move a file on the next paste).
    ///   2. If the explorer has a multi-selection, clear it.
    ///   3. If the explorer's search input is active, clear the query.
    ///   4. Otherwise, transfer focus back to the editor.
    pub fn file_explorer_search_clear(&mut self) {
        if matches!(
            self.file_explorer_clipboard,
            Some(FileExplorerClipboard { is_cut: true, .. })
        ) {
            self.file_explorer_clipboard = None;
            self.set_status_message(t!("explorer.cut_cancelled").to_string());
            return;
        }
        let action = self.file_explorer.as_mut().map(|explorer| {
            if explorer.has_multi_selection() {
                explorer.clear_multi_selection();
                None
            } else if explorer.is_search_active() {
                explorer.search_clear();
                None
            } else {
                Some(())
            }
        });
        if let Some(Some(())) = action {
            self.focus_editor();
        }
    }

    /// Install (or replace) a namespace of plugin-supplied file-explorer
    /// decorations for this window. Paths outside the window root are
    /// dropped silently. Triggers a rebuild of the per-path decoration
    /// cache the renderer reads.
    pub fn handle_set_file_explorer_decorations(
        &mut self,
        namespace: String,
        decorations: Vec<crate::view::file_tree::FileExplorerDecoration>,
    ) {
        let root = self.root.clone();
        let normalized: Vec<crate::view::file_tree::FileExplorerDecoration> = decorations
            .into_iter()
            .filter_map(|mut decoration| {
                let path = if decoration.path.is_absolute() {
                    decoration.path
                } else {
                    root.join(&decoration.path)
                };
                let path = crate::app::normalize_path(&path);
                if path.starts_with(&root) {
                    decoration.path = path;
                    Some(decoration)
                } else {
                    None
                }
            })
            .collect();

        self.file_explorer_decorations.insert(namespace, normalized);
        self.rebuild_file_explorer_decoration_cache();
    }

    /// Drop a namespace of plugin-supplied decorations and rebuild the
    /// per-path cache without it.
    pub fn handle_clear_file_explorer_decorations(&mut self, namespace: &str) {
        self.file_explorer_decorations.remove(namespace);
        self.rebuild_file_explorer_decoration_cache();
    }

    /// Recompute the `file_explorer_decoration_cache` from the current
    /// per-namespace decoration entries + the explorer's symlink
    /// mappings. Called after any decoration-mutating operation.
    pub fn rebuild_file_explorer_decoration_cache(&mut self) {
        let decorations: Vec<_> = self
            .file_explorer_decorations
            .values()
            .flat_map(|entries| entries.iter().cloned())
            .collect();

        let symlink_mappings = self
            .file_explorer
            .as_ref()
            .map(|fe| fe.collect_symlink_mappings())
            .unwrap_or_default();

        self.file_explorer_decoration_cache =
            crate::view::file_tree::FileExplorerDecorationCache::rebuild(
                decorations.into_iter(),
                &self.root,
                &symlink_mappings,
            );
    }

    /// Read-only access to this window's file-explorer cut/copy clipboard.
    pub fn file_explorer_clipboard(&self) -> Option<&FileExplorerClipboard> {
        self.file_explorer_clipboard.as_ref()
    }

    /// Copy the file-explorer selection to this window's clipboard.
    pub fn file_explorer_copy(&mut self) {
        self.set_explorer_clipboard(false);
    }

    /// Cut the file-explorer selection to this window's clipboard.
    pub fn file_explorer_cut(&mut self) {
        self.set_explorer_clipboard(true);
    }

    /// Shared body of `file_explorer_copy` and `file_explorer_cut`: read
    /// the explorer's selection, derive the path list, and prime the
    /// window's clipboard slot with a `FileExplorerClipboard`. Posts a
    /// status message summarising what got stashed.
    fn set_explorer_clipboard(&mut self, is_cut: bool) {
        let Some(explorer) = self.file_explorer.as_ref() else {
            return;
        };
        let root_id = explorer.tree().root_id();
        let selected_ids = explorer.effective_selection();
        let paths: Vec<PathBuf> = selected_ids
            .iter()
            .filter(|&&id| id != root_id)
            .filter_map(|&id| explorer.tree().get_node(id).map(|n| n.entry.path.clone()))
            .collect();
        if paths.is_empty() {
            let msg = if is_cut {
                t!("explorer.cannot_cut_root").to_string()
            } else {
                t!("explorer.cannot_copy_root").to_string()
            };
            self.set_status_message(msg);
            return;
        }
        let msg = if paths.len() == 1 {
            let name = paths[0]
                .file_name()
                .unwrap_or_default()
                .to_string_lossy()
                .to_string();
            if is_cut {
                t!("explorer.cut", name = &name).to_string()
            } else {
                t!("explorer.copied", name = &name).to_string()
            }
        } else {
            let count = paths.len();
            if is_cut {
                t!("explorer.cut_n", count = count).to_string()
            } else {
                t!("explorer.copied_n", count = count).to_string()
            }
        };
        self.file_explorer_clipboard = Some(FileExplorerClipboard { paths, is_cut });
        self.set_status_message(msg);
    }

    /// Spawn an async expand-to-path of this window's file-explorer tree,
    /// targeting the active buffer's file. No-op when the explorer isn't
    /// visible, a sync is already running, or the target path is outside
    /// the window's root.
    pub fn sync_file_explorer_to_active_file(&mut self) {
        if !self.file_explorer_visible {
            return;
        }

        // Don't start a new sync if one is already in progress
        if self.file_explorer_sync_in_progress {
            return;
        }

        let active_buf = self.active_buffer();
        let Some(metadata) = self.buffer_metadata.get(&active_buf) else {
            return;
        };
        let Some(file_path) = metadata.file_path() else {
            return;
        };
        let target_path = file_path.clone();

        if !target_path.starts_with(&self.root) {
            return;
        }

        let Some(mut view) = self.file_explorer.take() else {
            return;
        };
        tracing::trace!(
            "sync_file_explorer_to_active_file: taking file_explorer for async expand to {:?}",
            target_path
        );
        let runtime_handle = self
            .resources
            .tokio_runtime
            .as_ref()
            .map(|r| r.handle().clone());
        let sender = self.resources.async_bridge.as_ref().map(|b| b.sender());
        let window_id = self.id;
        if let (Some(runtime), Some(sender)) = (runtime_handle, sender) {
            // Mark sync as in progress so render knows to keep the layout
            self.file_explorer_sync_in_progress = true;

            runtime.spawn(async move {
                let _success = view.expand_and_select_file(&target_path).await;
                // Receiver may have been dropped during shutdown.
                #[allow(clippy::let_underscore_must_use)]
                let _ = sender.send(
                    crate::services::async_bridge::AsyncMessage::FileExplorerExpandedToPath {
                        window: window_id,
                        view,
                    },
                );
            });
        } else {
            self.file_explorer = Some(view);
        }
    }
}

/// Generate a unique non-conflicting paste name in dst_dir for a file/dir named `name`.
/// Returns `dst_dir/name copy.ext`, `dst_dir/name copy 2.ext`, etc.
fn unique_paste_name(
    fs: &dyn crate::model::filesystem::FileSystem,
    dst_dir: &Path,
    name: &str,
) -> PathBuf {
    let (stem, ext) = split_stem_ext(name);
    let mut n = 1u32;
    loop {
        let candidate = if n == 1 {
            if ext.is_empty() {
                format!("{} copy", stem)
            } else {
                format!("{} copy.{}", stem, ext)
            }
        } else if ext.is_empty() {
            format!("{} copy {}", stem, n)
        } else {
            format!("{} copy {}.{}", stem, n, ext)
        };
        let path = dst_dir.join(&candidate);
        if !fs.exists(&path) {
            return path;
        }
        n += 1;
        if n > 1000 {
            // Fallback: use a timestamp-based name to avoid an infinite loop
            return dst_dir.join(format!("{} copy {}", stem, timestamp_suffix()));
        }
    }
}

/// Truncate a filename to at most `max` Unicode chars for display in a minibuffer prompt.
pub(super) fn truncate_name_for_prompt(name: &str, max: usize) -> String {
    if name.chars().count() <= max {
        name.to_string()
    } else {
        let truncated: String = name.chars().take(max.saturating_sub(1)).collect();
        format!("{}\u{2026}", truncated)
    }
}

/// Build a short, comma-separated preview of file names for a bulk-operation
/// prompt — e.g. `'foo.rs', 'bar.rs', 'baz.rs'` or `'a.rs', 'b.rs', … (5 more)`.
/// Each individual name is truncated at 24 unicode chars to keep the
/// preview on one minibuffer row.
pub(super) fn format_path_preview_for_prompt(paths: &[PathBuf], max_shown: usize) -> String {
    let names: Vec<String> = paths
        .iter()
        .map(|p| {
            let raw = p
                .file_name()
                .map(|n| n.to_string_lossy().to_string())
                .unwrap_or_default();
            format!("'{}'", truncate_name_for_prompt(&raw, 24))
        })
        .collect();
    if names.len() <= max_shown {
        names.join(", ")
    } else {
        let shown = names[..max_shown].join(", ");
        let more = names.len() - max_shown;
        format!("{}, \u{2026} ({} more)", shown, more)
    }
}

fn split_stem_ext(name: &str) -> (&str, &str) {
    // Hidden files like ".gitignore" have no extension; treat the whole name as stem
    if let Some(dot_pos) = name.rfind('.') {
        if dot_pos > 0 {
            return (&name[..dot_pos], &name[dot_pos + 1..]);
        }
    }
    (name, "")
}
