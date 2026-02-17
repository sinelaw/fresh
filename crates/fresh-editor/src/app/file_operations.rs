//! File operations for the Editor.
//!
//! This module contains file I/O and watching operations:
//! - Saving buffers
//! - Reverting to saved version
//! - Auto-revert and file change polling
//! - LSP file notifications (open, change)
//! - File modification time tracking
//! - Save conflict detection

use crate::model::buffer::SudoSaveRequired;
use crate::view::prompt::PromptType;
use std::path::{Path, PathBuf};

use lsp_types::TextDocumentContentChangeEvent;
use rust_i18n::t;

use crate::model::event::{BufferId, EventLog};
use crate::services::lsp::manager::LspSpawnResult;
use crate::state::EditorState;

use super::{BufferMetadata, Editor};

impl Editor {
    /// Save the active buffer
    pub fn save(&mut self) -> anyhow::Result<()> {
        let path = self
            .active_state()
            .buffer
            .file_path()
            .map(|p| p.to_path_buf());

        match self.active_state_mut().buffer.save() {
            Ok(()) => self.finalize_save(path),
            Err(e) => {
                if let Some(sudo_info) = e.downcast_ref::<SudoSaveRequired>() {
                    let info = sudo_info.clone();
                    self.start_prompt(
                        t!("prompt.sudo_save_confirm").to_string(),
                        PromptType::ConfirmSudoSave { info },
                    );
                    Ok(())
                } else {
                    Err(e)
                }
            }
        }
    }

    /// Internal helper to finalize save state (mark as saved, notify LSP, etc.)
    pub(crate) fn finalize_save(&mut self, path: Option<PathBuf>) -> anyhow::Result<()> {
        // Auto-detect language if it's currently "text" and we have a path
        if let Some(ref p) = path {
            let buffer_id = self.active_buffer();
            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                if state.language == "text" {
                    // Use path directly for syntax detection
                    // Don't use set_language_from_name as it's designed for virtual buffers
                    // and strips colons (breaking Windows paths like C:\...)
                    state.highlighter =
                        crate::primitives::highlight_engine::HighlightEngine::for_file(
                            p,
                            &self.grammar_registry,
                        );

                    if let Some(language) = crate::primitives::highlighter::Language::from_path(p) {
                        state.reference_highlighter.set_language(&language);
                        state.language = language.to_string();
                    } else {
                        state.language = "text".to_string();
                    }
                }
            }
        }

        self.status_message = Some(t!("status.file_saved").to_string());

        // Mark the event log position as saved (for undo modified tracking)
        self.active_event_log_mut().mark_saved();

        // Update file modification time after save
        if let Some(ref p) = path {
            if let Ok(metadata) = self.filesystem.metadata(p) {
                if let Some(mtime) = metadata.modified {
                    self.file_mod_times.insert(p.clone(), mtime);
                }
            }
        }

        // Notify LSP of save
        self.notify_lsp_save();

        // Delete recovery file (buffer is now saved)
        let _ = self.delete_buffer_recovery(self.active_buffer());

        // Emit control event
        if let Some(ref p) = path {
            self.emit_event(
                crate::model::control_event::events::FILE_SAVED.name,
                serde_json::json!({
                    "path": p.display().to_string()
                }),
            );
        }

        // Fire AfterFileSave hook for plugins
        if let Some(ref p) = path {
            let buffer_id = self.active_buffer();
            self.plugin_manager.run_hook(
                "after_file_save",
                crate::services::plugins::hooks::HookArgs::AfterFileSave {
                    buffer_id,
                    path: p.clone(),
                },
            );
        }

        // Run on-save actions (formatters, linters, etc.)
        match self.run_on_save_actions() {
            Ok(true) => {
                // Actions ran successfully - if status_message was set by run_on_save_actions
                // (e.g., for missing optional formatters), keep it. Otherwise update status.
                if self.status_message.as_deref() == Some(&t!("status.file_saved")) {
                    self.status_message = Some(t!("status.file_saved_with_actions").to_string());
                }
                // else: keep the message set by run_on_save_actions (e.g., missing formatter)
            }
            Ok(false) => {
                // No actions configured, keep original status
            }
            Err(e) => {
                // Action failed, show error but don't fail the save
                self.status_message = Some(e);
            }
        }

        Ok(())
    }

    /// Revert the active buffer to the last saved version on disk
    /// Returns Ok(true) if reverted, Ok(false) if no file path, Err on failure
    pub fn revert_file(&mut self) -> anyhow::Result<bool> {
        let path = match self.active_state().buffer.file_path() {
            Some(p) => p.to_path_buf(),
            None => {
                self.status_message = Some(t!("status.no_file_to_revert").to_string());
                return Ok(false);
            }
        };

        if !path.exists() {
            self.status_message =
                Some(t!("status.file_not_exists", path = path.display().to_string()).to_string());
            return Ok(false);
        }

        // Save scroll position (from SplitViewState) and cursor positions before reloading
        let active_split = self.split_manager.active_split();
        let (old_top_byte, old_left_column) = self
            .split_view_states
            .get(&active_split)
            .map(|vs| (vs.viewport.top_byte, vs.viewport.left_column))
            .unwrap_or((0, 0));
        let old_cursors = self.active_cursors().clone();

        // Preserve user settings before reloading
        let old_buffer_settings = self.active_state().buffer_settings.clone();

        // Load the file content fresh from disk
        let mut new_state = EditorState::from_file_with_languages(
            &path,
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
            &self.grammar_registry,
            &self.config.languages,
            std::sync::Arc::clone(&self.filesystem),
        )?;

        // Restore cursor positions (clamped to valid range for new file size)
        let new_file_size = new_state.buffer.len();
        let mut restored_cursors = old_cursors;
        restored_cursors.map(|cursor| {
            cursor.position = cursor.position.min(new_file_size);
            // Clear selection since the content may have changed
            cursor.clear_selection();
        });
        // Restore user settings (tab size, indentation, etc.)
        new_state.buffer_settings = old_buffer_settings;
        // Line number visibility is in per-split BufferViewState (survives buffer replacement)

        // Replace the current buffer with the new state
        let buffer_id = self.active_buffer();
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            *state = new_state;
            // Note: line_wrap_enabled is now in SplitViewState.viewport
        }

        // Restore cursor positions in SplitViewState (clamped to valid range for new file size)
        let active_split = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.cursors = restored_cursors;
        }

        // Restore scroll position in SplitViewState (clamped to valid range for new file size)
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.viewport.top_byte = old_top_byte.min(new_file_size);
            view_state.viewport.left_column = old_left_column;
        }

        // Clear the undo/redo history for this buffer
        if let Some(event_log) = self.event_logs.get_mut(&buffer_id) {
            *event_log = EventLog::new();
        }

        // Clear seen_byte_ranges so plugins get notified of all visible lines
        self.seen_byte_ranges.remove(&buffer_id);

        // Update the file modification time
        if let Ok(metadata) = self.filesystem.metadata(&path) {
            if let Some(mtime) = metadata.modified {
                self.file_mod_times.insert(path.clone(), mtime);
            }
        }

        // Notify LSP that the file was changed
        self.notify_lsp_file_changed(&path);

        self.status_message = Some(t!("status.reverted").to_string());
        Ok(true)
    }

    /// Toggle auto-revert mode
    pub fn toggle_auto_revert(&mut self) {
        self.auto_revert_enabled = !self.auto_revert_enabled;

        if self.auto_revert_enabled {
            self.status_message = Some(t!("status.auto_revert_enabled").to_string());
        } else {
            self.status_message = Some(t!("status.auto_revert_disabled").to_string());
        }
    }

    /// Poll for file changes (called from main loop)
    ///
    /// Checks modification times of open files to detect external changes.
    /// Returns true if any file was changed (requires re-render).
    pub fn poll_file_changes(&mut self) -> bool {
        // Skip if auto-revert is disabled
        if !self.auto_revert_enabled {
            return false;
        }

        // Check poll interval
        let poll_interval =
            std::time::Duration::from_millis(self.config.editor.auto_revert_poll_interval_ms);
        let elapsed = self.time_source.elapsed_since(self.last_auto_revert_poll);
        tracing::trace!(
            "poll_file_changes: elapsed={:?}, poll_interval={:?}",
            elapsed,
            poll_interval
        );
        if elapsed < poll_interval {
            return false;
        }
        self.last_auto_revert_poll = self.time_source.now();

        // Collect paths of open files that need checking
        let files_to_check: Vec<PathBuf> = self
            .buffers
            .values()
            .filter_map(|state| state.buffer.file_path().map(PathBuf::from))
            .collect();

        let mut any_changed = false;

        for path in files_to_check {
            // Get current mtime
            let current_mtime = match self.filesystem.metadata(&path) {
                Ok(meta) => match meta.modified {
                    Some(mtime) => mtime,
                    None => continue,
                },
                Err(_) => continue, // File might have been deleted
            };

            // Check if mtime has changed
            if let Some(&stored_mtime) = self.file_mod_times.get(&path) {
                if current_mtime != stored_mtime {
                    // Handle the file change (this includes debouncing)
                    // Note: file_mod_times is updated by handle_file_changed after successful revert,
                    // not here, to avoid the race where the revert check sees the already-updated mtime
                    let path_str = path.display().to_string();
                    if self.handle_async_file_changed(path_str) {
                        any_changed = true;
                    }
                }
            } else {
                // First time seeing this file, record its mtime
                self.file_mod_times.insert(path, current_mtime);
            }
        }

        any_changed
    }

    /// Poll for file tree changes (called from main loop)
    ///
    /// Checks modification times of expanded directories to detect new/deleted files.
    /// Returns true if any directory was refreshed (requires re-render).
    pub fn poll_file_tree_changes(&mut self) -> bool {
        // Check poll interval
        let poll_interval =
            std::time::Duration::from_millis(self.config.editor.file_tree_poll_interval_ms);
        if self.time_source.elapsed_since(self.last_file_tree_poll) < poll_interval {
            return false;
        }
        self.last_file_tree_poll = self.time_source.now();

        // Get file explorer reference
        let Some(explorer) = &self.file_explorer else {
            return false;
        };

        // Collect expanded directories (node_id, path)
        use crate::view::file_tree::NodeId;
        let expanded_dirs: Vec<(NodeId, PathBuf)> = explorer
            .tree()
            .all_nodes()
            .filter(|node| node.is_dir() && node.is_expanded())
            .map(|node| (node.id, node.entry.path.clone()))
            .collect();

        // Check mtimes and collect directories that need refresh
        let mut dirs_to_refresh: Vec<NodeId> = Vec::new();

        for (node_id, path) in expanded_dirs {
            // Get current mtime
            let current_mtime = match self.filesystem.metadata(&path) {
                Ok(meta) => match meta.modified {
                    Some(mtime) => mtime,
                    None => continue,
                },
                Err(_) => continue, // Directory might have been deleted
            };

            // Check if mtime has changed
            if let Some(&stored_mtime) = self.dir_mod_times.get(&path) {
                if current_mtime != stored_mtime {
                    // Update stored mtime
                    self.dir_mod_times.insert(path.clone(), current_mtime);
                    dirs_to_refresh.push(node_id);
                    tracing::debug!("Directory changed: {:?}", path);
                }
            } else {
                // First time seeing this directory, record its mtime
                self.dir_mod_times.insert(path, current_mtime);
            }
        }

        // Refresh changed directories
        if dirs_to_refresh.is_empty() {
            return false;
        }

        // Refresh each changed directory
        if let (Some(runtime), Some(explorer)) = (&self.tokio_runtime, &mut self.file_explorer) {
            for node_id in dirs_to_refresh {
                let tree = explorer.tree_mut();
                if let Err(e) = runtime.block_on(tree.refresh_node(node_id)) {
                    tracing::warn!("Failed to refresh directory: {}", e);
                }
            }
        }

        true
    }

    /// Notify LSP server about a newly opened file
    /// Handles language detection, spawning LSP clients, and sending didOpen notifications
    pub(crate) fn notify_lsp_file_opened(
        &mut self,
        path: &Path,
        buffer_id: BufferId,
        metadata: &mut BufferMetadata,
    ) {
        // Get language from buffer state
        let Some(language) = self.buffers.get(&buffer_id).map(|s| s.language.clone()) else {
            tracing::debug!("No buffer state for file: {}", path.display());
            return;
        };

        let Some(uri) = metadata.file_uri().cloned() else {
            tracing::warn!(
                "No URI in metadata for file: {} (failed to compute absolute path)",
                path.display()
            );
            return;
        };

        // Check file size
        let file_size = self
            .filesystem
            .metadata(path)
            .ok()
            .map(|m| m.size)
            .unwrap_or(0);
        if file_size > self.config.editor.large_file_threshold_bytes {
            let reason = format!("File too large ({} bytes)", file_size);
            tracing::warn!(
                "Skipping LSP for large file: {} ({})",
                path.display(),
                reason
            );
            metadata.disable_lsp(reason);
            return;
        }

        // Get text before borrowing lsp
        let text = match self
            .buffers
            .get(&buffer_id)
            .and_then(|state| state.buffer.to_string())
        {
            Some(t) => t,
            None => {
                tracing::debug!("Buffer not fully loaded for LSP notification");
                return;
            }
        };

        let enable_inlay_hints = self.config.editor.enable_inlay_hints;
        let previous_result_id = self.diagnostic_result_ids.get(uri.as_str()).cloned();

        // Get buffer line count for inlay hints
        let (last_line, last_char) = self
            .buffers
            .get(&buffer_id)
            .map(|state| {
                let line_count = state.buffer.line_count().unwrap_or(1000);
                (line_count.saturating_sub(1) as u32, 10000u32)
            })
            .unwrap_or((999, 10000));

        // Now borrow lsp and do all LSP operations
        let Some(lsp) = &mut self.lsp else {
            tracing::debug!("No LSP manager available");
            return;
        };

        tracing::debug!("LSP manager available for file: {}", path.display());
        tracing::debug!(
            "Detected language: {} for file: {}",
            language,
            path.display()
        );
        tracing::debug!("Using URI from metadata: {}", uri.as_str());
        tracing::debug!("Attempting to spawn LSP client for language: {}", language);

        match lsp.try_spawn(&language) {
            LspSpawnResult::Spawned => {
                if let Some(client) = lsp.get_handle_mut(&language) {
                    // Send didOpen
                    tracing::info!("Sending didOpen to LSP for: {}", uri.as_str());
                    if let Err(e) = client.did_open(uri.clone(), text, language.clone()) {
                        tracing::warn!("Failed to send didOpen to LSP: {}", e);
                        return;
                    }
                    tracing::info!("Successfully sent didOpen to LSP");

                    // Mark this buffer as opened with this server instance
                    metadata.lsp_opened_with.insert(client.id());

                    // Request pull diagnostics
                    let request_id = self.next_lsp_request_id;
                    self.next_lsp_request_id += 1;
                    if let Err(e) =
                        client.document_diagnostic(request_id, uri.clone(), previous_result_id)
                    {
                        tracing::debug!(
                            "Failed to request pull diagnostics (server may not support): {}",
                            e
                        );
                    } else {
                        tracing::info!(
                            "Requested pull diagnostics for {} (request_id={})",
                            uri.as_str(),
                            request_id
                        );
                    }

                    // Request inlay hints
                    if enable_inlay_hints {
                        let request_id = self.next_lsp_request_id;
                        self.next_lsp_request_id += 1;
                        self.pending_inlay_hints_request = Some(request_id);

                        if let Err(e) =
                            client.inlay_hints(request_id, uri.clone(), 0, 0, last_line, last_char)
                        {
                            tracing::debug!(
                                "Failed to request inlay hints (server may not support): {}",
                                e
                            );
                            self.pending_inlay_hints_request = None;
                        } else {
                            tracing::info!(
                                "Requested inlay hints for {} (request_id={})",
                                uri.as_str(),
                                request_id
                            );
                        }
                    }
                }
            }
            LspSpawnResult::NotAutoStart => {
                tracing::debug!(
                    "LSP for {} not auto-starting (auto_start=false). Use command palette to start manually.",
                    language
                );
            }
            LspSpawnResult::Failed => {
                tracing::warn!("Failed to spawn LSP client for language: {}", language);
            }
        }
    }

    /// Record a file's modification time (called when opening files)
    /// This is used by the polling-based auto-revert to detect external changes
    pub(crate) fn watch_file(&mut self, path: &Path) {
        // Record current modification time for polling
        if let Ok(metadata) = self.filesystem.metadata(path) {
            if let Some(mtime) = metadata.modified {
                self.file_mod_times.insert(path.to_path_buf(), mtime);
            }
        }
    }

    /// Notify LSP that a file's contents changed (e.g., after revert)
    pub(crate) fn notify_lsp_file_changed(&mut self, path: &Path) {
        use crate::services::lsp::manager::LspSpawnResult;

        let Ok(uri) = url::Url::from_file_path(path) else {
            return;
        };
        let Ok(lsp_uri) = uri.as_str().parse::<lsp_types::Uri>() else {
            return;
        };

        // Find the buffer ID, content, and language for this path
        let Some((buffer_id, content, language)) = self
            .buffers
            .iter()
            .find(|(_, s)| s.buffer.file_path() == Some(path))
            .and_then(|(id, state)| {
                state
                    .buffer
                    .to_string()
                    .map(|t| (*id, t, state.language.clone()))
            })
        else {
            return;
        };

        // Check if we can spawn LSP (respects auto_start setting)
        let spawn_result = {
            let Some(lsp) = self.lsp.as_mut() else {
                return;
            };
            lsp.try_spawn(&language)
        };

        // Only proceed if spawned successfully (or already running)
        if spawn_result != LspSpawnResult::Spawned {
            return;
        }

        // Get handle ID (handle should exist now since try_spawn succeeded)
        let handle_id = {
            let Some(lsp) = self.lsp.as_mut() else {
                return;
            };
            let Some(handle) = lsp.get_handle_mut(&language) else {
                return;
            };
            handle.id()
        };

        // Check if didOpen needs to be sent first
        let needs_open = {
            let Some(metadata) = self.buffer_metadata.get(&buffer_id) else {
                return;
            };
            !metadata.lsp_opened_with.contains(&handle_id)
        };

        if needs_open {
            // Send didOpen first
            if let Some(lsp) = self.lsp.as_mut() {
                if let Some(handle) = lsp.get_handle_mut(&language) {
                    if let Err(e) =
                        handle.did_open(lsp_uri.clone(), content.clone(), language.clone())
                    {
                        tracing::warn!("Failed to send didOpen before didChange: {}", e);
                        return;
                    }
                    tracing::debug!(
                        "Sent didOpen for {} to LSP handle {} before file change notification",
                        lsp_uri.as_str(),
                        handle_id
                    );
                }
            }

            // Mark as opened
            if let Some(metadata) = self.buffer_metadata.get_mut(&buffer_id) {
                metadata.lsp_opened_with.insert(handle_id);
            }
        }

        // Use full document sync - send the entire new content
        if let Some(lsp) = &mut self.lsp {
            if let Some(client) = lsp.get_handle_mut(&language) {
                let content_change = TextDocumentContentChangeEvent {
                    range: None, // None means full document replacement
                    range_length: None,
                    text: content,
                };
                if let Err(e) = client.did_change(lsp_uri, vec![content_change]) {
                    tracing::warn!("Failed to notify LSP of file change: {}", e);
                }
            }
        }
    }

    /// Revert a specific buffer by ID without affecting the active viewport.
    ///
    /// This is used for auto-reverting background buffers that aren't currently
    /// visible in the active split. It reloads the buffer content and updates
    /// cursors (clamped to valid positions), but does NOT touch any viewport state.
    pub(crate) fn revert_buffer_by_id(
        &mut self,
        buffer_id: BufferId,
        path: &Path,
    ) -> anyhow::Result<()> {
        // Preserve user settings before reloading
        // TODO: Consider moving line numbers to SplitViewState (per-view setting)
        // Get cursors from split view states for this buffer (find any split showing it)
        let old_cursors = self
            .split_view_states
            .values()
            .find_map(|vs| {
                if vs.keyed_states.contains_key(&buffer_id) {
                    vs.keyed_states.get(&buffer_id).map(|bs| bs.cursors.clone())
                } else {
                    None
                }
            })
            .unwrap_or_default();
        let old_buffer_settings = self
            .buffers
            .get(&buffer_id)
            .map(|s| s.buffer_settings.clone())
            .unwrap_or_default();

        // Load the file content fresh from disk
        let mut new_state = EditorState::from_file_with_languages(
            path,
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
            &self.grammar_registry,
            &self.config.languages,
            std::sync::Arc::clone(&self.filesystem),
        )?;

        // Get the new file size for clamping
        let new_file_size = new_state.buffer.len();

        // Restore cursor positions (clamped to valid range for new file size)
        let mut restored_cursors = old_cursors;
        restored_cursors.map(|cursor| {
            cursor.position = cursor.position.min(new_file_size);
            cursor.clear_selection();
        });
        // Restore user settings (tab size, indentation, etc.)
        new_state.buffer_settings = old_buffer_settings;
        // Line number visibility is in per-split BufferViewState (survives buffer replacement)

        // Replace the buffer content
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            *state = new_state;
        }

        // Restore cursors in any split view states that have this buffer
        for vs in self.split_view_states.values_mut() {
            if let Some(buf_state) = vs.keyed_states.get_mut(&buffer_id) {
                buf_state.cursors = restored_cursors.clone();
            }
        }

        // Clear the undo/redo history for this buffer
        if let Some(event_log) = self.event_logs.get_mut(&buffer_id) {
            *event_log = EventLog::new();
        }

        // Clear seen_byte_ranges so plugins get notified of all visible lines
        self.seen_byte_ranges.remove(&buffer_id);

        // Update the file modification time
        if let Ok(metadata) = self.filesystem.metadata(path) {
            if let Some(mtime) = metadata.modified {
                self.file_mod_times.insert(path.to_path_buf(), mtime);
            }
        }

        // Notify LSP that the file was changed
        self.notify_lsp_file_changed(path);

        Ok(())
    }

    /// Handle a file change notification (from file watcher)
    pub fn handle_file_changed(&mut self, changed_path: &str) {
        let path = PathBuf::from(changed_path);

        // Find buffers that have this file open
        let buffer_ids: Vec<BufferId> = self
            .buffers
            .iter()
            .filter(|(_, state)| state.buffer.file_path() == Some(&path))
            .map(|(id, _)| *id)
            .collect();

        if buffer_ids.is_empty() {
            return;
        }

        for buffer_id in buffer_ids {
            // Skip terminal buffers - they manage their own content via PTY streaming
            // and should not be auto-reverted (which would reset editing_disabled and line_numbers)
            if self.terminal_buffers.contains_key(&buffer_id) {
                continue;
            }

            let state = match self.buffers.get(&buffer_id) {
                Some(s) => s,
                None => continue,
            };

            // Check if the file actually changed (compare mod times)
            // We use optimistic concurrency: check mtime, and if we decide to revert,
            // re-check to handle the race where a save completed between our checks.
            let current_mtime = match self
                .filesystem
                .metadata(&path)
                .ok()
                .and_then(|m| m.modified)
            {
                Some(mtime) => mtime,
                None => continue, // Can't read file, skip
            };

            let dominated_by_stored = self
                .file_mod_times
                .get(&path)
                .map(|stored| current_mtime <= *stored)
                .unwrap_or(false);

            if dominated_by_stored {
                continue;
            }

            // If buffer has local modifications, show a warning (don't auto-revert)
            if state.buffer.is_modified() {
                self.status_message = Some(format!(
                    "File {} changed on disk (buffer has unsaved changes)",
                    path.display()
                ));
                continue;
            }

            // Auto-revert if enabled and buffer is not modified
            if self.auto_revert_enabled {
                // Optimistic concurrency: re-check mtime before reverting.
                // A save may have completed between our first check and now,
                // updating file_mod_times. If so, skip the revert.
                let still_needs_revert = self
                    .file_mod_times
                    .get(&path)
                    .map(|stored| current_mtime > *stored)
                    .unwrap_or(true);

                if !still_needs_revert {
                    continue;
                }

                // Check if this buffer is currently displayed in the active split
                let is_active_buffer = buffer_id == self.active_buffer();

                if is_active_buffer {
                    // Use revert_file() which preserves viewport for active buffer
                    if let Err(e) = self.revert_file() {
                        tracing::error!("Failed to auto-revert file {:?}: {}", path, e);
                    } else {
                        tracing::info!("Auto-reverted file: {:?}", path);
                    }
                } else {
                    // Use revert_buffer_by_id() which doesn't touch any viewport
                    // This prevents corrupting the active split's viewport state
                    if let Err(e) = self.revert_buffer_by_id(buffer_id, &path) {
                        tracing::error!("Failed to auto-revert background file {:?}: {}", path, e);
                    } else {
                        tracing::info!("Auto-reverted file: {:?}", path);
                    }
                }

                // Update the modification time tracking for this file
                self.watch_file(&path);
            }
        }
    }

    /// Check if saving would overwrite changes made by another process
    /// Returns Some(current_mtime) if there's a conflict, None otherwise
    pub fn check_save_conflict(&self) -> Option<std::time::SystemTime> {
        let path = self.active_state().buffer.file_path()?;

        // Get current file modification time
        let current_mtime = match self.filesystem.metadata(path).ok().and_then(|m| m.modified) {
            Some(mtime) => mtime,
            None => return None, // File doesn't exist or can't read metadata
        };

        // Compare with our recorded modification time
        match self.file_mod_times.get(path) {
            Some(recorded_mtime) if current_mtime > *recorded_mtime => {
                // File was modified externally since we last loaded/saved it
                Some(current_mtime)
            }
            _ => None,
        }
    }
}
