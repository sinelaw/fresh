//! Async message handlers for the Editor
//!
//! This module contains handlers for AsyncMessage variants, grouped by domain:
//! - LSP diagnostics (push and pull models)
//! - LSP feature responses (inlay hints, progress, status)
//! - File system events
//! - File explorer events
//! - Plugin events

use crate::model::event::BufferId;
use crate::services::async_bridge::{LspMessageType, LspProgressValue, LspServerStatus};
use crate::view::file_tree::{FileTreeView, NodeId};
use lsp_types::{Diagnostic, InlayHint};
use serde_json::Value;
use std::path::PathBuf;
use std::time::{Duration, Instant};

use super::types::{LspMessageEntry, LspProgressInfo};
use super::Editor;

// =============================================================================
// Shared Helpers
// =============================================================================

impl Editor {
    /// Find a buffer by its LSP URI
    ///
    /// This is a common pattern used by diagnostics, inlay hints, and other LSP handlers
    pub(super) fn find_buffer_by_uri(&self, uri: &str) -> Option<BufferId> {
        let parsed_uri = uri.parse::<lsp_types::Uri>().ok()?;
        self.buffer_metadata
            .iter()
            .find(|(_, m)| m.file_uri() == Some(&parsed_uri))
            .map(|(buffer_id, _)| *buffer_id)
    }

    /// Apply diagnostics to a buffer identified by URI.
    /// Returns the buffer_id if diagnostics were applied, None if buffer not found.
    fn apply_diagnostics_to_buffer(
        &mut self,
        uri: &str,
        diagnostics: &[Diagnostic],
    ) -> Option<BufferId> {
        let buffer_id = self.find_buffer_by_uri(uri)?;
        let state = self.buffers.get_mut(&buffer_id)?;
        crate::services::lsp::diagnostics::apply_diagnostics_to_state_cached(
            state,
            diagnostics,
            &self.theme,
        );
        Some(buffer_id)
    }
}

// =============================================================================
// LSP Diagnostics Handlers
// =============================================================================

impl Editor {
    /// Handle LSP diagnostics (push model)
    pub(super) fn handle_lsp_diagnostics(&mut self, uri: String, diagnostics: Vec<Diagnostic>) {
        tracing::debug!(
            "Processing {} LSP diagnostics for {}",
            diagnostics.len(),
            uri
        );

        if let Some(buffer_id) = self.apply_diagnostics_to_buffer(&uri, &diagnostics) {
            tracing::info!(
                "Applied {} diagnostics to buffer {:?}",
                diagnostics.len(),
                buffer_id
            );
        } else {
            tracing::debug!("No buffer found for diagnostic URI: {}", uri);
        }
    }

    /// Handle LSP pulled diagnostics (pull model - LSP 3.17+)
    pub(super) fn handle_lsp_pulled_diagnostics(
        &mut self,
        uri: String,
        result_id: Option<String>,
        diagnostics: Vec<Diagnostic>,
        unchanged: bool,
    ) {
        if unchanged {
            tracing::debug!(
                "Diagnostics unchanged for {} (result_id: {:?})",
                uri,
                result_id
            );
            return;
        }

        tracing::debug!(
            "Processing {} pulled diagnostics for {} (result_id: {:?})",
            diagnostics.len(),
            uri,
            result_id
        );

        if let Some(buffer_id) = self.apply_diagnostics_to_buffer(&uri, &diagnostics) {
            tracing::info!(
                "Applied {} pulled diagnostics to buffer {:?}",
                diagnostics.len(),
                buffer_id
            );
        } else {
            tracing::debug!("No buffer found for pulled diagnostic URI: {}", uri);
        }

        // Store result_id for incremental updates
        if let Some(result_id) = result_id {
            self.diagnostic_result_ids.insert(uri, result_id);
        }
    }
}

// =============================================================================
// LSP Feature Handlers
// =============================================================================

impl Editor {
    /// Handle LSP inlay hints response
    pub(super) fn handle_lsp_inlay_hints(
        &mut self,
        request_id: u64,
        uri: String,
        hints: Vec<InlayHint>,
    ) {
        if self.pending_inlay_hints_request != Some(request_id) {
            tracing::debug!(
                "Ignoring stale inlay hints response (request_id={})",
                request_id
            );
            return;
        }

        self.pending_inlay_hints_request = None;

        tracing::info!(
            "Received {} inlay hints for {} (request_id={})",
            hints.len(),
            uri,
            request_id
        );

        if let Some(buffer_id) = self.find_buffer_by_uri(&uri) {
            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                Self::apply_inlay_hints_to_state(state, &hints);
                tracing::info!(
                    "Applied {} inlay hints as virtual text to buffer {:?}",
                    hints.len(),
                    buffer_id
                );
            }
        } else {
            tracing::warn!("No buffer found for inlay hints URI: {}", uri);
        }
    }

    /// Handle LSP server quiescent notification (rust-analyzer project fully loaded)
    pub(super) fn handle_lsp_server_quiescent(&mut self, language: String) {
        tracing::info!(
            "LSP ({}) project fully loaded, re-requesting inlay hints",
            language
        );

        // Skip if inlay hints are disabled
        if !self.config.editor.enable_inlay_hints {
            return;
        }

        let Some(lsp) = self.lsp.as_mut() else {
            return;
        };

        let Some(client) = lsp.get_or_spawn(&language) else {
            return;
        };

        // Collect buffer info first to avoid borrow issues
        let buffer_infos: Vec<_> = self
            .buffer_metadata
            .iter()
            .filter_map(|(buffer_id, metadata)| {
                metadata.file_uri().map(|uri| {
                    let line_count = self
                        .buffers
                        .get(buffer_id)
                        .and_then(|s| s.buffer.line_count())
                        .unwrap_or(1000);
                    (uri.clone(), line_count)
                })
            })
            .collect();

        // Request inlay hints for each buffer
        for (uri, line_count) in buffer_infos {
            let request_id = self.next_lsp_request_id;
            self.next_lsp_request_id += 1;
            self.pending_inlay_hints_request = Some(request_id);

            let last_line = line_count.saturating_sub(1) as u32;
            if let Err(e) = client.inlay_hints(request_id, uri.clone(), 0, 0, last_line, 10000) {
                tracing::debug!(
                    "Failed to re-request inlay hints for {}: {}",
                    uri.as_str(),
                    e
                );
            } else {
                tracing::info!(
                    "Re-requested inlay hints for {} (request_id={})",
                    uri.as_str(),
                    request_id
                );
            }
        }
    }

    /// Handle LSP progress notification ($/progress)
    pub(super) fn handle_lsp_progress(
        &mut self,
        language: String,
        token: String,
        value: LspProgressValue,
    ) {
        match value {
            LspProgressValue::Begin {
                title,
                message,
                percentage,
            } => {
                self.lsp_progress.insert(
                    token.clone(),
                    LspProgressInfo {
                        language,
                        title,
                        message,
                        percentage,
                    },
                );
                self.update_lsp_status_from_progress();
            }
            LspProgressValue::Report {
                message,
                percentage,
            } => {
                if let Some(info) = self.lsp_progress.get_mut(&token) {
                    info.message = message;
                    info.percentage = percentage;
                    self.update_lsp_status_from_progress();
                }
            }
            LspProgressValue::End { .. } => {
                self.lsp_progress.remove(&token);
                self.update_lsp_status_from_progress();
            }
        }
    }

    /// Handle LSP window message (window/showMessage)
    pub(super) fn handle_lsp_window_message(
        &mut self,
        language: String,
        message_type: LspMessageType,
        message: String,
    ) {
        // Add to window messages list
        self.lsp_window_messages.push(LspMessageEntry {
            language: language.clone(),
            message_type,
            message: message.clone(),
            timestamp: Instant::now(),
        });

        // Keep only last 100 messages
        if self.lsp_window_messages.len() > 100 {
            self.lsp_window_messages.remove(0);
        }

        // Show important messages in status bar
        match message_type {
            LspMessageType::Error | LspMessageType::Warning => {
                self.status_message = Some(format!("LSP ({}): {}", language, message));
            }
            _ => {
                // Info and Log messages are not shown in status bar
            }
        }
    }

    /// Handle LSP log message (window/logMessage)
    pub(super) fn handle_lsp_log_message(
        &mut self,
        language: String,
        message_type: LspMessageType,
        message: String,
    ) {
        self.lsp_log_messages.push(LspMessageEntry {
            language,
            message_type,
            message,
            timestamp: Instant::now(),
        });

        // Keep only last 500 log messages
        if self.lsp_log_messages.len() > 500 {
            self.lsp_log_messages.remove(0);
        }
    }

    /// Handle LSP server status update
    pub(super) fn handle_lsp_status_update(&mut self, language: String, status: LspServerStatus) {
        use crate::services::async_bridge::LspServerStatus;

        // Get old status for event
        let old_status = self.lsp_server_statuses.get(&language).cloned();

        // Update server status
        self.lsp_server_statuses
            .insert(language.clone(), status.clone());
        self.update_lsp_status_from_server_statuses();

        // Handle server crash - trigger auto-restart
        if status == LspServerStatus::Error {
            let was_running = old_status
                .as_ref()
                .map(|s| matches!(s, LspServerStatus::Running | LspServerStatus::Initializing))
                .unwrap_or(false);

            if was_running {
                if let Some(lsp) = self.lsp.as_mut() {
                    let message = lsp.handle_server_crash(&language);
                    self.status_message = Some(message);
                }
            }
        }

        // Emit control event
        let status_str = match status {
            LspServerStatus::Starting => "starting",
            LspServerStatus::Initializing => "initializing",
            LspServerStatus::Running => "running",
            LspServerStatus::Error => "error",
            LspServerStatus::Shutdown => "shutdown",
        };
        let old_status_str = old_status
            .map(|s| match s {
                LspServerStatus::Starting => "starting",
                LspServerStatus::Initializing => "initializing",
                LspServerStatus::Running => "running",
                LspServerStatus::Error => "error",
                LspServerStatus::Shutdown => "shutdown",
            })
            .unwrap_or("none");

        self.emit_event(
            crate::model::control_event::events::LSP_STATUS_CHANGED.name,
            serde_json::json!({
                "language": language,
                "old_status": old_status_str,
                "status": status_str
            }),
        );
    }

    /// Handle custom LSP notification
    pub(super) fn handle_custom_notification(
        &mut self,
        language: String,
        method: String,
        params: Option<Value>,
    ) {
        tracing::debug!("Custom LSP notification {} from {}", method, language);
        let payload = serde_json::json!({
            "language": language,
            "method": method,
            "params": params,
        });
        self.emit_event("lsp/custom_notification", payload);
    }

    /// Handle plugin LSP response
    pub(super) fn handle_plugin_lsp_response(
        &mut self,
        request_id: u64,
        result: Result<Value, String>,
    ) {
        tracing::debug!("Received plugin LSP response (request_id={})", request_id);
        self.send_plugin_response(crate::services::plugins::api::PluginResponse::LspRequest {
            request_id,
            result,
        });
    }
}

// =============================================================================
// File System Event Handlers
// =============================================================================

impl Editor {
    /// Handle file changed externally notification (from AsyncMessage)
    ///
    /// Includes debounce logic to prevent rapid auto-reverts from overwhelming the editor.
    /// This is different from `handle_file_changed` which actually reloads the file.
    pub(super) fn handle_async_file_changed(&mut self, path: String) -> bool {
        const DEBOUNCE_WINDOW: Duration = Duration::from_secs(10);
        const RAPID_REVERT_THRESHOLD: u32 = 10; // Require 10 reverts in 10 seconds to disable

        // Skip if auto-revert is disabled
        if !self.auto_revert_enabled {
            return false;
        }

        let path_buf = PathBuf::from(&path);

        // Only track events for files that are actually open in the editor
        let is_file_open = self
            .buffers
            .iter()
            .any(|(_, state)| state.buffer.file_path() == Some(&path_buf));

        if !is_file_open {
            tracing::trace!("Ignoring file change event for non-open file: {}", path);
            return false;
        }

        // Track rapid file change events - only disable after many reverts in short window
        if let Some((window_start, count)) = self.file_rapid_change_counts.get_mut(&path_buf) {
            if window_start.elapsed() < DEBOUNCE_WINDOW {
                *count += 1;

                if *count >= RAPID_REVERT_THRESHOLD {
                    // Disable auto-revert and stop the file watcher
                    self.auto_revert_enabled = false;
                    self.file_watcher = None;
                    self.watched_dirs.clear();
                    self.status_message = Some(format!(
                        "Auto-revert disabled: {} is updating too frequently (use Ctrl+Shift+R to re-enable)",
                        path_buf.file_name().unwrap_or_default().to_string_lossy()
                    ));
                    tracing::info!(
                        "Auto-revert disabled for {:?} ({} reverts in {:?})",
                        path_buf,
                        count,
                        DEBOUNCE_WINDOW
                    );
                    return false;
                }
            } else {
                // Reset counter - start a new window
                *count = 1;
                *window_start = Instant::now();
            }
        } else {
            // First event for this file
            self.file_rapid_change_counts
                .insert(path_buf.clone(), (Instant::now(), 1));
        }

        tracing::info!("File changed externally: {}", path);
        self.handle_file_changed(&path);
        true
    }
}

// =============================================================================
// File Explorer Handlers
// =============================================================================

impl Editor {
    /// Handle file explorer initialized
    pub(super) fn handle_file_explorer_initialized(&mut self, mut view: FileTreeView) {
        tracing::info!("File explorer initialized");

        // Load root .gitignore
        let root_id = view.tree().root_id();
        let root_path = view.tree().get_node(root_id).map(|n| n.entry.path.clone());

        if let Some(root_path) = root_path {
            if let Err(e) = view.load_gitignore_for_dir(&root_path) {
                tracing::warn!("Failed to load root .gitignore from {:?}: {}", root_path, e);
            } else {
                tracing::debug!("Loaded root .gitignore from {:?}", root_path);
            }
        }

        self.file_explorer = Some(view);
        self.set_status_message("File explorer ready".to_string());
    }

    /// Handle file explorer node toggle completed
    pub(super) fn handle_file_explorer_toggle_node(&mut self, node_id: NodeId) {
        tracing::debug!("File explorer toggle completed for node {:?}", node_id);
    }

    /// Handle file explorer node refresh completed
    pub(super) fn handle_file_explorer_refresh_node(&mut self, node_id: NodeId) {
        tracing::debug!("File explorer refresh completed for node {:?}", node_id);
        self.set_status_message("Refreshed".to_string());
    }

    /// Handle file explorer expanded to path
    pub(super) fn handle_file_explorer_expanded_to_path(&mut self, mut view: FileTreeView) {
        tracing::debug!("File explorer expanded to active file path");
        view.update_scroll_for_selection();
        self.file_explorer = Some(view);
    }
}

// =============================================================================
// Plugin Handlers
// =============================================================================

impl Editor {
    /// Handle plugin process output completion
    pub(super) fn handle_plugin_process_output(
        &mut self,
        process_id: u64,
        stdout: String,
        stderr: String,
        exit_code: i32,
    ) {
        tracing::debug!(
            "Process {} completed: exit_code={}, stdout_len={}, stderr_len={}",
            process_id,
            exit_code,
            stdout.len(),
            stderr.len()
        );
    }

    /// Process TypeScript plugin commands
    ///
    /// Returns true if any commands were processed
    pub(super) fn process_plugin_commands(&mut self) -> bool {
        let Some(ref mut manager) = self.ts_plugin_manager else {
            tracing::trace!("process_async_messages: no plugin manager");
            return false;
        };

        let commands = manager.process_commands();
        if commands.is_empty() {
            return false;
        }

        tracing::trace!(
            "process_plugin_commands: processing {} commands",
            commands.len()
        );

        for command in commands {
            tracing::trace!(
                "process_plugin_commands: handling command {:?}",
                std::mem::discriminant(&command)
            );
            if let Err(e) = self.handle_plugin_command(command) {
                tracing::error!("Error handling TypeScript plugin command: {}", e);
            }
        }

        true
    }

    /// Process pending plugin action completions
    pub(super) fn process_pending_plugin_actions(&mut self) {
        self.pending_plugin_actions
            .retain(|(action_name, receiver)| {
                match receiver.try_recv() {
                    Ok(result) => {
                        match result {
                            Ok(()) => {
                                tracing::info!(
                                    "Plugin action '{}' executed successfully",
                                    action_name
                                );
                            }
                            Err(e) => {
                                tracing::error!("Plugin action '{}' error: {}", action_name, e);
                            }
                        }
                        false // Remove completed action
                    }
                    Err(std::sync::mpsc::TryRecvError::Empty) => {
                        true // Keep pending action
                    }
                    Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                        tracing::error!(
                            "Plugin thread disconnected during action '{}'",
                            action_name
                        );
                        false // Remove disconnected action
                    }
                }
            });
    }

    /// Process pending LSP server restarts (with exponential backoff)
    pub(super) fn process_pending_lsp_restarts(&mut self) {
        let Some(lsp) = self.lsp.as_mut() else {
            return;
        };

        let restart_results = lsp.process_pending_restarts();

        for (language, success, message) in restart_results {
            self.status_message = Some(message.clone());

            if success {
                self.resend_did_open_for_language(&language);
            }
        }
    }

    /// Re-send didOpen notifications for all buffers of a given language
    fn resend_did_open_for_language(&mut self, language: &str) {
        // Find all open buffers for this language
        let buffers_for_language: Vec<_> = self
            .buffer_metadata
            .iter()
            .filter_map(|(buf_id, meta)| {
                meta.file_path().and_then(|path| {
                    if crate::services::lsp::manager::detect_language(path)
                        == Some(language.to_string())
                    {
                        Some((*buf_id, path.clone()))
                    } else {
                        None
                    }
                })
            })
            .collect();

        // Re-send didOpen for each buffer
        for (buffer_id, path) in buffers_for_language {
            if let Some(state) = self.buffers.get(&buffer_id) {
                let content = match state.buffer.to_string() {
                    Some(c) => c,
                    None => continue, // Skip buffers that aren't fully loaded
                };
                let uri: Option<lsp_types::Uri> = url::Url::from_file_path(&path)
                    .ok()
                    .and_then(|u| u.as_str().parse::<lsp_types::Uri>().ok());

                if let Some(uri) = uri {
                    if let Some(lang_id) = crate::services::lsp::manager::detect_language(&path) {
                        if let Some(lsp) = self.lsp.as_mut() {
                            if let Some(handle) = lsp.get_or_spawn(&lang_id) {
                                let _ = handle.did_open(uri, content, lang_id);
                            }
                        }
                    }
                }
            }
        }
    }
}
