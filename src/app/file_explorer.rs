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
            self.set_status_message("File explorer opened".to_string());
            self.sync_file_explorer_to_active_file();
        } else {
            self.key_context = KeyContext::Normal;
            self.set_status_message("File explorer closed".to_string());
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

        if let Some(metadata) = self.buffer_metadata.get(&self.active_buffer()) {
            if let Some(file_path) = metadata.file_path() {
                let target_path = file_path.clone();
                let working_dir = self.working_dir.clone();

                if target_path.starts_with(&working_dir) {
                    if let Some(mut view) = self.file_explorer.take() {
                        if let (Some(runtime), Some(bridge)) =
                            (&self.tokio_runtime, &self.async_bridge)
                        {
                            let sender = bridge.sender();

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
            // Cancel search/replace prompts when switching focus away from editor
            self.cancel_search_prompt_if_active();

            self.key_context = KeyContext::FileExplorer;
            self.set_status_message("File explorer focused".to_string());
            self.sync_file_explorer_to_active_file();
        } else {
            self.toggle_file_explorer();
        }
    }

    pub fn focus_editor(&mut self) {
        self.key_context = KeyContext::Normal;
        self.set_status_message("Editor focused".to_string());
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

            self.set_status_message("Initializing file explorer...".to_string());
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
            "Collapsing...".to_string()
        } else {
            format!("Loading {}...", name)
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
                            format!("Expanded: {}", name)
                        } else {
                            format!("Collapsed: {}", name)
                        };
                        self.set_status_message(msg);
                    }
                }
                Err(e) => {
                    self.set_status_message(format!("Error: {}", e));
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
                self.set_status_message(format!("Opened: {}", name));
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
            self.set_status_message(format!("Refreshing {}...", name));
        }

        if let (Some(runtime), Some(explorer)) = (&self.tokio_runtime, &mut self.file_explorer) {
            let tree = explorer.tree_mut();
            let result = runtime.block_on(tree.refresh_node(selected_id));
            match result {
                Ok(()) => {
                    if let Some(name) = node_name {
                        self.set_status_message(format!("Refreshed: {}", name));
                    } else {
                        self.set_status_message("Refreshed".to_string());
                    }
                }
                Err(e) => {
                    self.set_status_message(format!("Error refreshing: {}", e));
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
                                self.set_status_message(format!("Created {}", filename));
                            }
                            Err(e) => {
                                self.set_status_message(format!("Error creating file: {}", e));
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
                                self.set_status_message(format!("Created {}", dirname_clone));

                                // Enter rename mode for the new folder
                                let prompt = crate::view::prompt::Prompt::with_initial_text(
                                    "Rename to: ".to_string(),
                                    crate::view::prompt::PromptType::FileExplorerRename {
                                        original_path: path_clone,
                                        original_name: dirname_clone,
                                    },
                                    dirname,
                                );
                                self.prompt = Some(prompt);
                            }
                            Err(e) => {
                                self.set_status_message(format!("Error creating directory: {}", e));
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn file_explorer_delete(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            if let Some(selected_id) = explorer.get_selected() {
                // Don't allow deleting the root directory
                if selected_id == explorer.tree().root_id() {
                    self.set_status_message("Cannot delete project root".to_string());
                    return;
                }

                let node = explorer.tree().get_node(selected_id);
                if let Some(node) = node {
                    let path = node.entry.path.clone();
                    let name = node.entry.name.clone();

                    if let Some(runtime) = &self.tokio_runtime {
                        let result = if node.is_dir() {
                            runtime.block_on(async { tokio::fs::remove_dir_all(&path).await })
                        } else {
                            runtime.block_on(async { tokio::fs::remove_file(&path).await })
                        };

                        match result {
                            Ok(_) => {
                                // For delete, always get the parent (the deleted item can't be refreshed)
                                let parent_id =
                                    get_parent_node_id(explorer.tree(), selected_id, false);
                                let tree = explorer.tree_mut();
                                let _ = runtime.block_on(tree.refresh_node(parent_id));
                                self.set_status_message(format!("Deleted {}", name));
                            }
                            Err(e) => {
                                self.set_status_message(format!("Error deleting: {}", e));
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn file_explorer_rename(&mut self) {
        if let Some(explorer) = &self.file_explorer {
            if let Some(selected_id) = explorer.get_selected() {
                // Don't allow renaming the root directory
                if selected_id == explorer.tree().root_id() {
                    self.set_status_message("Cannot rename project root".to_string());
                    return;
                }

                let node = explorer.tree().get_node(selected_id);
                if let Some(node) = node {
                    let old_path = node.entry.path.clone();
                    let old_name = node.entry.name.clone();

                    // Create a prompt for the new name, pre-filled with the old name
                    let prompt = crate::view::prompt::Prompt::with_initial_text(
                        "Rename to: ".to_string(),
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
            self.set_status_message("Rename cancelled".to_string());
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
                    self.set_status_message(format!("Renamed {} to {}", original_name, new_name));
                }
                Err(e) => {
                    self.set_status_message(format!("Error renaming: {}", e));
                }
            }
        }
    }

    pub fn file_explorer_toggle_hidden(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.toggle_show_hidden();
            let msg = if explorer.ignore_patterns().show_hidden() {
                "Showing hidden files"
            } else {
                "Hiding hidden files"
            };
            self.set_status_message(msg.to_string());
        }
    }

    pub fn file_explorer_toggle_gitignored(&mut self) {
        if let Some(explorer) = &mut self.file_explorer {
            explorer.toggle_show_gitignored();
            let show = explorer.ignore_patterns().show_gitignored();
            let msg = if show {
                "Showing gitignored files"
            } else {
                "Hiding gitignored files"
            };
            self.set_status_message(msg.to_string());
        }
    }
}
