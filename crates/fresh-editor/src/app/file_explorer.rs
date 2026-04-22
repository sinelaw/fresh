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
        self.file_explorer_visible
    }

    pub fn file_explorer(&self) -> Option<&FileTreeView> {
        self.file_explorer.as_ref()
    }

    pub fn toggle_file_explorer(&mut self) {
        self.file_explorer_visible = !self.file_explorer_visible;

        if self.file_explorer_visible {
            if self.file_explorer.is_none() {
                self.init_file_explorer();
            }
            self.key_context = KeyContext::FileExplorer;
            self.set_status_message(t!("explorer.opened").to_string());
            self.sync_file_explorer_to_active_file();
        } else {
            self.key_context = KeyContext::Normal;
            self.set_status_message(t!("explorer.closed").to_string());
        }

        // Notify plugins that the viewport dimensions changed (sidebar affects available width)
        self.plugin_manager.run_hook(
            "resize",
            fresh_core::hooks::HookArgs::Resize {
                width: self.terminal_width,
                height: self.terminal_height,
            },
        );
    }

    pub fn show_file_explorer(&mut self) {
        if !self.file_explorer_visible {
            self.toggle_file_explorer();
        }
    }

    pub fn sync_file_explorer_to_active_file(&mut self) {
        if !self.file_explorer_visible {
            return;
        }

        // Don't start a new sync if one is already in progress
        if self.file_explorer_sync_in_progress {
            return;
        }

        if let Some(metadata) = self.buffer_metadata.get(&self.active_buffer()) {
            if let Some(file_path) = metadata.file_path() {
                let target_path = file_path.clone();
                let working_dir = self.working_dir.clone();

                if target_path.starts_with(&working_dir) {
                    if let Some(mut view) = self.file_explorer.take() {
                        tracing::trace!(
                            "sync_file_explorer_to_active_file: taking file_explorer for async expand to {:?}",
                            target_path
                        );
                        if let (Some(runtime), Some(bridge)) =
                            (&self.tokio_runtime, &self.async_bridge)
                        {
                            let sender = bridge.sender();
                            // Mark sync as in progress so render knows to keep the layout
                            self.file_explorer_sync_in_progress = true;

                            runtime.spawn(async move {
                                let _success = view.expand_and_select_file(&target_path).await;
                                // Receiver may have been dropped during shutdown.
                                #[allow(clippy::let_underscore_must_use)]
                                let _ = sender.send(AsyncMessage::FileExplorerExpandedToPath(view));
                            });
                        } else {
                            self.file_explorer = Some(view);
                        }
                    }
                }
            }
        }
    }

    pub fn focus_file_explorer(&mut self) {
        if self.file_explorer_visible {
            // Dismiss transient popups and clear hover state when focusing file explorer
            self.on_editor_focus_lost();

            // Cancel search/replace prompts when switching focus away from editor
            self.cancel_search_prompt_if_active();

            self.key_context = KeyContext::FileExplorer;
            self.set_status_message(t!("explorer.focused").to_string());
            self.sync_file_explorer_to_active_file();
        } else {
            self.toggle_file_explorer();
        }
    }

    pub fn focus_editor(&mut self) {
        self.key_context = KeyContext::Normal;
        self.set_status_message(t!("editor.focused").to_string());
    }

    pub(crate) fn init_file_explorer(&mut self) {
        // Use working directory as root. For remote mode, fall back to the remote
        // home directory only when working_dir doesn't exist on the remote
        // filesystem (e.g. when no path was provided and working_dir defaulted
        // to the local current directory).
        let root_path = if self.authority.filesystem.remote_connection_info().is_some()
            && !self
                .authority
                .filesystem
                .is_dir(&self.working_dir)
                .unwrap_or(false)
        {
            match self.authority.filesystem.home_dir() {
                Ok(home) => home,
                Err(e) => {
                    tracing::error!("Failed to get remote home directory: {}", e);
                    self.set_status_message(format!("Failed to get remote home: {}", e));
                    return;
                }
            }
        } else {
            self.working_dir.clone()
        };

        if let (Some(runtime), Some(bridge)) = (&self.tokio_runtime, &self.async_bridge) {
            let fs_manager = Arc::clone(&self.fs_manager);
            let sender = bridge.sender();

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
                        let _ = sender.send(AsyncMessage::FileExplorerInitialized(view));
                    }
                    Err(e) => {
                        tracing::error!("Failed to initialize file explorer: {}", e);
                    }
                }
            });

            self.set_status_message(t!("explorer.initializing").to_string());
        }
    }

    pub fn file_explorer_navigate_up(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.select_prev_match();
            explorer.update_scroll_for_selection();
        }
    }

    pub fn file_explorer_navigate_down(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.select_next_match();
            explorer.update_scroll_for_selection();
        }
    }

    pub fn file_explorer_page_up(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.select_page_up();
            explorer.update_scroll_for_selection();
        }
    }

    pub fn file_explorer_page_down(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.select_page_down();
            explorer.update_scroll_for_selection();
        }
    }

    /// Collapse behavior for left arrow:
    /// - If on expanded directory: collapse it
    /// - If on file or collapsed directory: select parent directory
    pub fn file_explorer_collapse(&mut self) {
        let Some(explorer) = &self.file_explorer else {
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
        if let Some(explorer) = &mut self.file_explorer {
            explorer.select_parent();
            explorer.update_scroll_for_selection();
        }
    }

    pub fn file_explorer_toggle_expand(&mut self) {
        let selected_id = if let Some(explorer) = &self.file_explorer {
            explorer.get_selected()
        } else {
            return;
        };

        let Some(selected_id) = selected_id else {
            return;
        };

        let (is_dir, is_expanded, name) = if let Some(explorer) = &self.file_explorer {
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

        if let (Some(runtime), Some(explorer)) = (&self.tokio_runtime, &mut self.file_explorer) {
            let tree = explorer.tree_mut();
            let result = runtime.block_on(tree.toggle_node(selected_id));

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
                self.rebuild_file_explorer_decoration_cache();
            }
        }
    }

    pub fn file_explorer_open_file(&mut self) -> AnyhowResult<()> {
        let entry_type = self
            .file_explorer
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
                        self.promote_buffer_from_preview(id);
                        self.set_status_message(
                            t!("explorer.opened_file", name = &name).to_string(),
                        );
                        self.focus_editor();
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
        let (selected_id, node_name) = if let Some(explorer) = &self.file_explorer {
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

        if let (Some(runtime), Some(explorer)) = (&self.tokio_runtime, &mut self.file_explorer) {
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
        if let Some(explorer) = &mut self.file_explorer {
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
                                if let Err(e) = runtime.block_on(tree.refresh_node(parent_id)) {
                                    tracing::warn!("Failed to refresh file tree: {}", e);
                                }
                                self.set_status_message(
                                    t!("explorer.created_file", name = &filename).to_string(),
                                );

                                // Open the file in the buffer
                                if let Err(e) = self.open_file(&path_clone) {
                                    tracing::warn!("Failed to open new file: {}", e);
                                }

                                // Enter rename mode for the new file with empty prompt
                                // so user can type the desired filename from scratch
                                let prompt = crate::view::prompt::Prompt::new(
                                    t!("explorer.rename_prompt").to_string(),
                                    crate::view::prompt::PromptType::FileExplorerRename {
                                        original_path: path_clone,
                                        original_name: filename.clone(),
                                        is_new_file: true,
                                    },
                                );
                                self.prompt = Some(prompt);
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
        if let Some(explorer) = &mut self.file_explorer {
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
                                if let Err(e) = runtime.block_on(tree.refresh_node(parent_id)) {
                                    tracing::warn!("Failed to refresh file tree: {}", e);
                                }
                                self.set_status_message(
                                    t!("explorer.created_dir", name = &dirname_clone).to_string(),
                                );

                                // Enter rename mode for the new folder
                                let prompt = crate::view::prompt::Prompt::with_initial_text(
                                    t!("explorer.rename_prompt").to_string(),
                                    crate::view::prompt::PromptType::FileExplorerRename {
                                        original_path: path_clone,
                                        original_name: dirname_clone,
                                        is_new_file: true,
                                    },
                                    dirname,
                                );
                                self.prompt = Some(prompt);
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
        let Some(explorer) = &self.file_explorer else {
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
            self.start_prompt(
                t!("explorer.delete_multi_confirm", count = count).to_string(),
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
                // Refresh the parent directory in the file explorer
                if let Some(explorer) = &mut self.file_explorer {
                    if let Some(runtime) = &self.tokio_runtime {
                        // Find the node for the deleted path and get its parent
                        if let Some(node) = explorer.tree().get_node_by_path(&path) {
                            let node_id = node.id;
                            let parent_id = get_parent_node_id(explorer.tree(), node_id, false);

                            // Remember the index of the deleted node in the visible list
                            let deleted_index = explorer.get_selected_index();

                            if let Err(e) =
                                runtime.block_on(explorer.tree_mut().refresh_node(parent_id))
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

                // Ensure focus remains on file explorer
                self.key_context = KeyContext::FileExplorer;
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
        if let Some(explorer) = &self.file_explorer {
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

                    // Create a prompt for the new name, pre-filled with the old name
                    let prompt = crate::view::prompt::Prompt::with_initial_text(
                        t!("explorer.rename_prompt").to_string(),
                        crate::view::prompt::PromptType::FileExplorerRename {
                            original_path: old_path,
                            original_name: old_name.clone(),
                            is_new_file: false,
                        },
                        old_name,
                    );
                    self.prompt = Some(prompt);
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

        if new_name.contains('/') {
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

        if let Some(runtime) = &self.tokio_runtime {
            let result = self.authority.filesystem.rename(&original_path, &new_path);

            match result {
                Ok(_) => {
                    // Refresh the parent directory and select the renamed item
                    if let Some(explorer) = &mut self.file_explorer {
                        if let Some(selected_id) = explorer.get_selected() {
                            let parent_id = get_parent_node_id(explorer.tree(), selected_id, false);
                            let tree = explorer.tree_mut();
                            if let Err(e) = runtime.block_on(tree.refresh_node(parent_id)) {
                                tracing::warn!("Failed to refresh file tree after rename: {}", e);
                            }
                        }
                        // Navigate to the renamed file to restore selection
                        explorer.navigate_to_path(&new_path);
                    }

                    // Update buffer metadata if this file is open in a buffer
                    let buffer_to_update = self
                        .buffers
                        .iter()
                        .find(|(_, state)| state.buffer.file_path() == Some(&original_path))
                        .map(|(id, _)| *id);

                    if let Some(buffer_id) = buffer_to_update {
                        // Update the buffer's file path after rename
                        if let Some(state) = self.buffers.get_mut(&buffer_id) {
                            state.buffer.rename_file_path(new_path.clone());
                        }

                        // Update the buffer metadata
                        if let Some(metadata) = self.buffer_metadata.get_mut(&buffer_id) {
                            // Compute new URI
                            let file_uri = super::types::file_path_to_lsp_uri(&new_path);

                            // Update kind with new path and URI
                            metadata.kind = super::BufferKind::File {
                                path: new_path.clone(),
                                uri: file_uri,
                            };

                            // Update display name
                            metadata.display_name = super::BufferMetadata::display_name_for_path(
                                &new_path,
                                &self.working_dir,
                            );
                        }

                        // Only switch focus to the buffer if this is a new file being created
                        // For renaming existing files from the explorer, keep focus in explorer.
                        if is_new_file {
                            self.key_context = KeyContext::Normal;
                        }
                    }

                    self.set_status_message(
                        t!("explorer.renamed", old = &original_name, new = &new_name).to_string(),
                    );
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
        let show_hidden = if let Some(explorer) = &mut self.file_explorer {
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
        let show_gitignored = if let Some(explorer) = &mut self.file_explorer {
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

    /// Clear the file explorer search (or multi-selection, or transfer focus)
    pub fn file_explorer_search_clear(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            if explorer.has_multi_selection() {
                explorer.clear_multi_selection();
            } else if explorer.is_search_active() {
                explorer.search_clear();
            } else {
                self.focus_editor();
            }
        }
    }

    pub fn file_explorer_extend_selection_up(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.extend_selection_up();
        }
    }

    pub fn file_explorer_extend_selection_down(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.extend_selection_down();
        }
    }

    pub fn file_explorer_toggle_select(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.toggle_select();
        }
    }

    pub fn file_explorer_select_all(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.select_all();
        }
    }

    /// Add a character to the file explorer search
    pub fn file_explorer_search_push_char(&mut self, c: char) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.search_push_char(c);
            explorer.update_scroll_for_selection();
        }
    }

    /// Remove a character from the file explorer search (backspace)
    pub fn file_explorer_search_pop_char(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.search_pop_char();
            explorer.update_scroll_for_selection();
        }
    }

    pub fn handle_set_file_explorer_decorations(
        &mut self,
        namespace: String,
        decorations: Vec<crate::view::file_tree::FileExplorerDecoration>,
    ) {
        let normalized: Vec<crate::view::file_tree::FileExplorerDecoration> = decorations
            .into_iter()
            .filter_map(|mut decoration| {
                let path = if decoration.path.is_absolute() {
                    decoration.path
                } else {
                    self.working_dir.join(&decoration.path)
                };
                let path = normalize_path(&path);
                if path.starts_with(&self.working_dir) {
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

    pub fn handle_clear_file_explorer_decorations(&mut self, namespace: &str) {
        self.file_explorer_decorations.remove(namespace);
        self.rebuild_file_explorer_decoration_cache();
    }

    pub(super) fn rebuild_file_explorer_decoration_cache(&mut self) {
        let decorations = self
            .file_explorer_decorations
            .values()
            .flat_map(|entries| entries.iter().cloned());

        // Collect symlink mappings from the file explorer
        let symlink_mappings = self
            .file_explorer
            .as_ref()
            .map(|fe| fe.collect_symlink_mappings())
            .unwrap_or_default();

        self.file_explorer_decoration_cache =
            crate::view::file_tree::FileExplorerDecorationCache::rebuild(
                decorations,
                &self.working_dir,
                &symlink_mappings,
            );
    }

    pub fn file_explorer_clipboard(&self) -> Option<&FileExplorerClipboard> {
        self.file_explorer_clipboard.as_ref()
    }

    pub fn file_explorer_copy(&mut self) {
        self.set_explorer_clipboard(false);
    }

    pub fn file_explorer_cut(&mut self) {
        self.set_explorer_clipboard(true);
    }

    fn set_explorer_clipboard(&mut self, is_cut: bool) {
        let Some(explorer) = &self.file_explorer else {
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

    pub fn file_explorer_paste(&mut self) {
        let clipboard = match self.file_explorer_clipboard.clone() {
            Some(c) => c,
            None => {
                self.set_status_message(t!("explorer.paste_no_source").to_string());
                return;
            }
        };

        let dst_dir = if let Some(explorer) = &self.file_explorer {
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

            if dst_path == src || src.parent().map(|p| p == dst_dir).unwrap_or(false) {
                if is_cut {
                    self.set_status_message(t!("explorer.paste_same_location").to_string());
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
                let is_same_location =
                    dst_path == *src || src.parent().map(|p| p == dst_dir).unwrap_or(false);

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
                self.set_status_message(t!("explorer.paste_same_location").to_string());
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
    pub(super) fn execute_resolved_multi_paste(
        &mut self,
        safe: Vec<(PathBuf, PathBuf)>,
        to_overwrite: Vec<(PathBuf, PathBuf)>,
        is_cut: bool,
    ) {
        let total = safe.len() + to_overwrite.len();
        for (src, dst) in safe.into_iter().chain(to_overwrite) {
            self.perform_file_explorer_paste(src, dst, is_cut);
        }
        if total > 1 {
            let msg = if is_cut {
                t!("explorer.pasted_moved_n", count = total).to_string()
            } else {
                t!("explorer.pasted_n", count = total).to_string()
            };
            self.set_status_message(msg);
        }
    }

    pub fn perform_file_explorer_paste(&mut self, src: PathBuf, dst: PathBuf, is_cut: bool) {
        let name = dst
            .file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_default();

        let src_is_dir = self.authority.filesystem.is_dir(&src).unwrap_or(false);

        let result = if is_cut {
            // Try rename first (works if same filesystem). Only fall back to
            // copy+delete for cross-device errors — any other rename failure
            // (permission denied, etc.) must surface as-is so we don't
            // silently succeed via a different codepath.
            match self.authority.filesystem.rename(&src, &dst) {
                Ok(()) => Ok(()),
                Err(e) if e.kind() == std::io::ErrorKind::CrossesDevices => {
                    let copy_result = if src_is_dir {
                        self.authority.filesystem.copy_dir_all(&src, &dst)
                    } else {
                        self.authority.filesystem.copy(&src, &dst).map(|_| ())
                    };
                    match copy_result {
                        Ok(()) => {
                            if src_is_dir {
                                self.authority.filesystem.remove_dir_all(&src)
                            } else {
                                self.authority.filesystem.remove_file(&src)
                            }
                        }
                        Err(copy_err) => {
                            // Roll back the half-written destination so the
                            // user isn't left with a partial copy alongside
                            // the intact source. Cleanup errors are
                            // swallowed — the copy error is the interesting
                            // one to surface — but logged.
                            let cleanup = if src_is_dir {
                                self.authority.filesystem.remove_dir_all(&dst)
                            } else {
                                self.authority.filesystem.remove_file(&dst)
                            };
                            if let Err(cleanup_err) = cleanup {
                                tracing::warn!(
                                    "Failed to roll back partial destination {:?} after copy \
                                     fallback failed: {}",
                                    dst,
                                    cleanup_err
                                );
                            }
                            Err(copy_err)
                        }
                    }
                }
                Err(e) => Err(e),
            }
        } else if src_is_dir {
            self.authority.filesystem.copy_dir_all(&src, &dst)
        } else {
            self.authority.filesystem.copy(&src, &dst).map(|_| ())
        };

        match result {
            Ok(()) => {
                if let Some(explorer) = &mut self.file_explorer {
                    if let Some(runtime) = &self.tokio_runtime {
                        // Refresh destination parent in-place to avoid collapsing it
                        if let Some(dst_parent) = dst.parent() {
                            if let Some(dst_parent_node) =
                                explorer.tree().get_node_by_path(dst_parent)
                            {
                                let pid = dst_parent_node.id;
                                if let Err(e) =
                                    runtime.block_on(explorer.tree_mut().reload_expanded_node(pid))
                                {
                                    tracing::warn!(
                                        "Failed to reload destination directory after paste: {}",
                                        e
                                    );
                                }
                            }
                        }
                        // Refresh source parent too (if cut). Using
                        // `reload_expanded_node` here rather than
                        // `refresh_node` is important: refresh_node collapses
                        // and re-expands the source parent, which wipes out
                        // every descendant NodeId — including the
                        // destination directory that was just expanded
                        // above. That in turn invalidates the cursor
                        // (`selected_node`) and any NodeIds held elsewhere
                        // (e.g. hover, decorations). The in-place reload
                        // keeps unchanged siblings intact and only drops the
                        // nodes that really went away.
                        if is_cut {
                            if let Some(src_parent) = src.parent() {
                                if let Some(src_parent_node) =
                                    explorer.tree().get_node_by_path(src_parent)
                                {
                                    let pid = src_parent_node.id;
                                    if let Err(e) = runtime
                                        .block_on(explorer.tree_mut().reload_expanded_node(pid))
                                    {
                                        tracing::warn!(
                                            "Failed to refresh source directory after move: {}",
                                            e
                                        );
                                    }
                                }
                            }
                        }
                    }
                    // Any source NodeIds that were in the multi-selection are
                    // now stale (the tree was reloaded / source parent
                    // refreshed). Drop the selection so subsequent actions
                    // act on the fresh cursor, not ghost IDs.
                    explorer.clear_multi_selection();
                    // Navigate to the pasted item
                    explorer.navigate_to_path(&dst);
                }

                if is_cut {
                    self.file_explorer_clipboard = None;
                    self.set_status_message(t!("explorer.pasted_moved", name = &name).to_string());
                } else {
                    self.set_status_message(t!("explorer.pasted", name = &name).to_string());
                }

                self.key_context = KeyContext::FileExplorer;
            }
            Err(e) => {
                let msg = if is_cut {
                    t!("explorer.error_moving", error = e.to_string()).to_string()
                } else {
                    t!("explorer.error_copying", error = e.to_string()).to_string()
                };
                self.set_status_message(msg);
            }
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
        } else {
            if ext.is_empty() {
                format!("{} copy {}", stem, n)
            } else {
                format!("{} copy {}.{}", stem, n, ext)
            }
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

fn split_stem_ext(name: &str) -> (&str, &str) {
    // Hidden files like ".gitignore" have no extension; treat the whole name as stem
    if let Some(dot_pos) = name.rfind('.') {
        if dot_pos > 0 {
            return (&name[..dot_pos], &name[dot_pos + 1..]);
        }
    }
    (name, "")
}
