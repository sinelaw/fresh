use rust_i18n::t;

use super::*;
use crate::view::file_tree::TreeNode;
use std::path::PathBuf;

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
        let root_path = self.working_dir.clone();

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
            explorer.select_prev();
            explorer.update_scroll_for_selection();
        }
    }

    pub fn file_explorer_navigate_down(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.select_next();
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

            match result {
                Ok(()) => {
                    if final_expanded {
                        let dir_path = explorer
                            .tree()
                            .get_node(selected_id)
                            .map(|n| n.entry.path.clone());

                        if let Some(dir_path) = dir_path {
                            if let Err(e) = explorer.load_gitignore_for_dir(&dir_path) {
                                tracing::warn!(
                                    "Failed to load .gitignore from {:?}: {}",
                                    dir_path,
                                    e
                                );
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
                    self.set_status_message(t!("explorer.error", error = e.to_string()).to_string());
                }
            }
        }
    }

    pub fn file_explorer_open_file(&mut self) -> io::Result<()> {
        let entry_type = self
            .file_explorer
            .as_ref()
            .and_then(|explorer| explorer.get_selected_entry())
            .map(|entry| (entry.is_dir(), entry.path.clone(), entry.name.clone()));

        if let Some((is_dir, path, name)) = entry_type {
            if is_dir {
                self.file_explorer_toggle_expand();
            } else {
                self.open_file(&path)?;
                self.set_status_message(t!("explorer.opened_file", name = &name).to_string());
                self.focus_editor();
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
                    self.set_status_message(t!("explorer.error_refreshing", error = e.to_string()).to_string());
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
                        let selected_id = selected_id;
                        let result =
                            runtime.block_on(async { tokio::fs::File::create(&path_clone).await });

                        match result {
                            Ok(_) => {
                                let parent_id =
                                    get_parent_node_id(explorer.tree(), selected_id, node.is_dir());
                                let tree = explorer.tree_mut();
                                let _ = runtime.block_on(tree.refresh_node(parent_id));
                                self.set_status_message(t!("explorer.created_file", name = &filename).to_string());
                            }
                            Err(e) => {
                                self.set_status_message(t!("explorer.error_creating_file", error = e.to_string()).to_string());
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
                        let selected_id = selected_id;
                        let result =
                            runtime.block_on(async { tokio::fs::create_dir(&path_clone).await });

                        match result {
                            Ok(_) => {
                                let parent_id =
                                    get_parent_node_id(explorer.tree(), selected_id, node.is_dir());
                                let tree = explorer.tree_mut();
                                let _ = runtime.block_on(tree.refresh_node(parent_id));
                                self.set_status_message(t!("explorer.created_dir", name = &dirname_clone).to_string());

                                // Enter rename mode for the new folder
                                let prompt = crate::view::prompt::Prompt::with_initial_text(
                                    t!("explorer.rename_prompt").to_string(),
                                    crate::view::prompt::PromptType::FileExplorerRename {
                                        original_path: path_clone,
                                        original_name: dirname_clone,
                                    },
                                    dirname,
                                );
                                self.prompt = Some(prompt);
                            }
                            Err(e) => {
                                self.set_status_message(t!("explorer.error_creating_dir", error = e.to_string()).to_string());
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn file_explorer_delete(&mut self) {
        if let Some(explorer) = &self.file_explorer {
            if let Some(selected_id) = explorer.get_selected() {
                // Don't allow deleting the root directory
                if selected_id == explorer.tree().root_id() {
                    self.set_status_message(t!("explorer.cannot_delete_root").to_string());
                    return;
                }

                let node = explorer.tree().get_node(selected_id);
                if let Some(node) = node {
                    let path = node.entry.path.clone();
                    let name = node.entry.name.clone();
                    let is_dir = node.is_dir();

                    let type_str = if is_dir { "directory" } else { "file" };
                    self.start_prompt(
                        t!("explorer.delete_confirm", "type" = type_str, name = &name).to_string(),
                        PromptType::ConfirmDeleteFile { path, is_dir },
                    );
                }
            }
        }
    }

    /// Perform the actual file explorer delete operation (called after prompt confirmation)
    /// Moves the file/directory to the system trash/recycle bin
    pub fn perform_file_explorer_delete(&mut self, path: std::path::PathBuf, _is_dir: bool) {
        let name = path
            .file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_default();

        // Move to trash instead of permanent deletion
        match trash::delete(&path) {
            Ok(_) => {
                // Refresh the parent directory in the file explorer
                if let Some(explorer) = &mut self.file_explorer {
                    if let Some(runtime) = &self.tokio_runtime {
                        // Find the node for the deleted path and get its parent
                        if let Some(node) = explorer.tree().get_node_by_path(&path) {
                            let node_id = node.id;
                            let parent_id = get_parent_node_id(explorer.tree(), node_id, false);
                            let _ = runtime.block_on(explorer.tree_mut().refresh_node(parent_id));
                        }
                    }
                }
                self.set_status_message(t!("explorer.moved_to_trash", name = &name).to_string());
            }
            Err(e) => {
                self.set_status_message(t!("explorer.error_trash", error = e.to_string()).to_string());
            }
        }
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
    ) {
        if new_name.is_empty() || new_name == original_name {
            self.set_status_message(t!("explorer.rename_cancelled").to_string());
            return;
        }

        let new_path = original_path
            .parent()
            .map(|p| p.join(&new_name))
            .unwrap_or_else(|| original_path.clone());

        if let Some(runtime) = &self.tokio_runtime {
            let result =
                runtime.block_on(async { tokio::fs::rename(&original_path, &new_path).await });

            match result {
                Ok(_) => {
                    // Refresh the parent directory and select the renamed item
                    if let Some(explorer) = &mut self.file_explorer {
                        if let Some(selected_id) = explorer.get_selected() {
                            let parent_id = get_parent_node_id(explorer.tree(), selected_id, false);
                            let tree = explorer.tree_mut();
                            let _ = runtime.block_on(tree.refresh_node(parent_id));
                        }
                        // Navigate to the renamed file to restore selection
                        explorer.navigate_to_path(&new_path);
                    }
                    self.set_status_message(t!("explorer.renamed", old = &original_name, new = &new_name).to_string());
                }
                Err(e) => {
                    self.set_status_message(t!("explorer.error_renaming", error = e.to_string()).to_string());
                }
            }
        }
    }

    pub fn file_explorer_toggle_hidden(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.toggle_show_hidden();
            let msg = if explorer.ignore_patterns().show_hidden() {
                t!("explorer.showing_hidden")
            } else {
                t!("explorer.hiding_hidden")
            };
            self.set_status_message(msg.to_string());
        }
    }

    pub fn file_explorer_toggle_gitignored(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.toggle_show_gitignored();
            let show = explorer.ignore_patterns().show_gitignored();
            let msg = if show {
                t!("explorer.showing_gitignored")
            } else {
                t!("explorer.hiding_gitignored")
            };
            self.set_status_message(msg.to_string());
        }
    }
}
