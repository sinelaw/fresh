//! Async message handlers for the Editor
//!
//! This module contains handlers for AsyncMessage variants, grouped by domain:
//! - LSP diagnostics (push and pull models)
//! - LSP feature responses (inlay hints, progress, status)
//! - File system events
//! - File explorer events
//! - Plugin events

use crate::model::buffer::Buffer;
use crate::model::event::BufferId;
use crate::services::async_bridge::{
    LspMessageType, LspProgressValue, LspSemanticTokensResponse, LspServerStatus,
};
use crate::state::{SemanticTokenSpan, SemanticTokenStore};
use crate::view::file_tree::{FileTreeView, NodeId};
use lsp_types::{
    Diagnostic, FoldingRange, InlayHint, SemanticToken, SemanticTokensEdit,
    SemanticTokensFullDeltaResult, SemanticTokensLegend, SemanticTokensRangeResult,
    SemanticTokensResult,
};
use rust_i18n::t;
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
    /// Store and apply diagnostics, emit hook for plugins
    fn store_and_apply_diagnostics(&mut self, uri: String, diagnostics: Vec<Diagnostic>) {
        // Store diagnostics for later retrieval by plugins
        if diagnostics.is_empty() {
            self.stored_diagnostics.remove(&uri);
        } else {
            self.stored_diagnostics
                .insert(uri.clone(), diagnostics.clone());
        }

        if let Some(buffer_id) = self.apply_diagnostics_to_buffer(&uri, &diagnostics) {
            tracing::info!(
                "Applied {} diagnostics to buffer {:?}",
                diagnostics.len(),
                buffer_id
            );
        } else {
            tracing::debug!("No buffer found for diagnostic URI: {}", uri);
        }

        // Emit diagnostics_updated hook for plugins
        self.plugin_manager.run_hook(
            "diagnostics_updated",
            crate::services::plugins::hooks::HookArgs::DiagnosticsUpdated {
                uri,
                count: diagnostics.len(),
            },
        );
    }

    /// Handle LSP diagnostics (push model)
    pub(super) fn handle_lsp_diagnostics(&mut self, uri: String, diagnostics: Vec<Diagnostic>) {
        tracing::debug!(
            "Processing {} LSP diagnostics for {}",
            diagnostics.len(),
            uri
        );
        self.store_and_apply_diagnostics(uri, diagnostics);
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

        // Store result_id for incremental updates
        if let Some(result_id) = result_id {
            self.diagnostic_result_ids.insert(uri.clone(), result_id);
        }

        self.store_and_apply_diagnostics(uri, diagnostics);
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

    /// Handle LSP folding ranges response
    pub(super) fn handle_lsp_folding_ranges(
        &mut self,
        request_id: u64,
        uri: String,
        ranges: Vec<FoldingRange>,
    ) {
        let Some(request) = self.pending_folding_range_requests.remove(&request_id) else {
            tracing::debug!(
                "Ignoring folding ranges response without pending request (request_id={})",
                request_id
            );
            return;
        };

        self.folding_ranges_in_flight.remove(&request.buffer_id);

        // Ignore stale responses (buffer changed since request)
        if let Some(state) = self.buffers.get(&request.buffer_id) {
            if state.buffer.version() != request.version {
                tracing::debug!(
                    "Ignoring stale folding ranges for {} (request_id={}, version={}, current={})",
                    uri,
                    request_id,
                    request.version,
                    state.buffer.version()
                );
                self.schedule_folding_ranges_refresh(request.buffer_id);
                return;
            }
        } else {
            return;
        }

        if ranges.is_empty() {
            self.stored_folding_ranges.remove(&uri);
        } else {
            self.stored_folding_ranges.insert(uri.clone(), ranges);
        }

        if let Some(state) = self.buffers.get_mut(&request.buffer_id) {
            state.folding_ranges = self
                .stored_folding_ranges
                .get(&uri)
                .cloned()
                .unwrap_or_default();
        }
    }

    /// Handle LSP semantic tokens response
    pub(super) fn handle_lsp_semantic_tokens(
        &mut self,
        request_id: u64,
        uri: String,
        response: LspSemanticTokensResponse,
    ) {
        let (
            buffer_id,
            target_version,
            full_request_kind,
            requested_range,
            requested_start_line,
            requested_end_line,
        ) = if let Some(range_request) = self.take_pending_semantic_token_range_request(request_id)
        {
            (
                range_request.buffer_id,
                range_request.version,
                None,
                Some(range_request.range),
                Some(range_request.start_line),
                Some(range_request.end_line),
            )
        } else if let Some(full_request) = self.take_pending_semantic_token_request(request_id) {
            (
                full_request.buffer_id,
                full_request.version,
                Some(full_request.kind),
                None,
                None,
                None,
            )
        } else {
            tracing::debug!(
                "Semantic tokens response {} for {} without pending entry",
                request_id,
                uri
            );
            return;
        };

        // Get language from buffer's stored state
        let Some(language) = self.buffers.get(&buffer_id).map(|s| s.language.clone()) else {
            return;
        };

        let legend = match self
            .lsp
            .as_ref()
            .and_then(|manager| manager.semantic_tokens_legend(&language).cloned())
        {
            Some(legend) => legend,
            None => {
                tracing::debug!("Semantic tokens legend missing for language {}", language);
                return;
            }
        };

        let Some(state) = self.buffers.get_mut(&buffer_id) else {
            return;
        };

        let current_version = state.buffer.version();
        if current_version != target_version {
            // Stale response - ignore; next render will request fresh tokens.
            return;
        }

        match (requested_range, full_request_kind) {
            (Some(range), None) => {
                let result = match response {
                    LspSemanticTokensResponse::Range(result) => result,
                    _ => {
                        tracing::warn!(
                            "Semantic tokens range response {} for {} had mismatched type",
                            request_id,
                            uri
                        );
                        return;
                    }
                };

                match result {
                    Err(e) => {
                        tracing::warn!(
                            "Semantic tokens range request {} for {} failed: {}",
                            request_id,
                            uri,
                            e
                        );
                    }
                    Ok(tokens_opt) => {
                        let spans = match tokens_opt {
                            Some(SemanticTokensRangeResult::Tokens(tokens)) => {
                                // LSP semantic tokens are always delta-encoded from document
                                // position (0,0), even for range requests. The range only
                                // filters which tokens are returned, not the encoding origin.
                                let decoded = decode_semantic_token_data(
                                    &state.buffer,
                                    &legend,
                                    &tokens.data,
                                    0,
                                );
                                decoded.spans
                            }
                            Some(SemanticTokensRangeResult::Partial(partial)) => {
                                let decoded = decode_semantic_token_data(
                                    &state.buffer,
                                    &legend,
                                    &partial.data,
                                    0,
                                );
                                decoded.spans
                            }
                            None => Vec::new(),
                        };

                        crate::services::lsp::semantic_tokens::apply_semantic_tokens_range_to_state(
                            state,
                            range.clone(),
                            &spans,
                            &self.theme,
                        );
                        self.semantic_tokens_range_applied.insert(
                            buffer_id,
                            (
                                requested_start_line.unwrap_or(0),
                                requested_end_line.unwrap_or(0),
                                current_version,
                            ),
                        );
                    }
                }
            }
            (None, Some(super::SemanticTokensFullRequestKind::Full)) => {
                let result = match response {
                    LspSemanticTokensResponse::Full(result) => result,
                    _ => {
                        tracing::warn!(
                            "Semantic tokens response {} for {} had mismatched type",
                            request_id,
                            uri
                        );
                        return;
                    }
                };

                match result {
                    Err(e) => {
                        tracing::warn!(
                            "Semantic tokens request {} for {} failed: {}",
                            request_id,
                            uri,
                            e
                        );
                    }
                    Ok(tokens_opt) => {
                        let decoded = match tokens_opt {
                            Some(SemanticTokensResult::Tokens(tokens)) => {
                                let decoded = decode_semantic_token_data(
                                    &state.buffer,
                                    &legend,
                                    &tokens.data,
                                    0,
                                );
                                SemanticTokensFullDecode {
                                    result_id: tokens.result_id.clone(),
                                    raw_data: decoded.raw,
                                    spans: decoded.spans,
                                }
                            }
                            Some(SemanticTokensResult::Partial(partial)) => {
                                let decoded = decode_semantic_token_data(
                                    &state.buffer,
                                    &legend,
                                    &partial.data,
                                    0,
                                );
                                SemanticTokensFullDecode {
                                    result_id: None,
                                    raw_data: decoded.raw,
                                    spans: decoded.spans,
                                }
                            }
                            None => SemanticTokensFullDecode {
                                result_id: None,
                                raw_data: Vec::new(),
                                spans: Vec::new(),
                            },
                        };

                        crate::services::lsp::semantic_tokens::apply_semantic_tokens_to_state(
                            state,
                            &decoded.spans,
                            &self.theme,
                        );

                        state.set_semantic_tokens(SemanticTokenStore {
                            version: current_version,
                            result_id: decoded.result_id,
                            data: decoded.raw_data,
                            tokens: decoded.spans,
                        });
                    }
                }
            }
            (None, Some(super::SemanticTokensFullRequestKind::FullDelta)) => {
                let result = match response {
                    LspSemanticTokensResponse::FullDelta(result) => result,
                    _ => {
                        tracing::warn!(
                            "Semantic tokens delta response {} for {} had mismatched type",
                            request_id,
                            uri
                        );
                        return;
                    }
                };

                match result {
                    Err(e) => {
                        tracing::warn!(
                            "Semantic tokens delta request {} for {} failed: {}",
                            request_id,
                            uri,
                            e
                        );
                    }
                    Ok(tokens_opt) => {
                        let existing_store = state.semantic_tokens.as_ref();
                        let existing_result_id =
                            existing_store.and_then(|store| store.result_id.clone());
                        let existing_data = existing_store.map(|store| store.data.clone());

                        let decoded = match tokens_opt {
                            Some(SemanticTokensFullDeltaResult::Tokens(tokens)) => {
                                SemanticTokensDeltaDecode {
                                    result_id: tokens.result_id.clone(),
                                    raw_data: semantic_tokens_to_raw(&tokens.data),
                                }
                            }
                            Some(SemanticTokensFullDeltaResult::TokensDelta(delta)) => {
                                let Some(existing) = existing_data else {
                                    tracing::warn!(
                                        "Semantic tokens delta response {} for {} missing baseline",
                                        request_id,
                                        uri
                                    );
                                    return;
                                };
                                let updated = match apply_semantic_token_edits(
                                    existing,
                                    &delta.edits,
                                ) {
                                    Some(data) => data,
                                    None => {
                                        tracing::warn!(
                                            "Semantic tokens delta response {} for {} had invalid edits",
                                            request_id,
                                            uri
                                        );
                                        return;
                                    }
                                };
                                SemanticTokensDeltaDecode {
                                    result_id: delta.result_id.clone().or(existing_result_id),
                                    raw_data: updated,
                                }
                            }
                            Some(SemanticTokensFullDeltaResult::PartialTokensDelta { edits }) => {
                                let Some(existing) = existing_data else {
                                    tracing::warn!(
                                        "Semantic tokens delta response {} for {} missing baseline",
                                        request_id,
                                        uri
                                    );
                                    return;
                                };
                                let updated = match apply_semantic_token_edits(existing, &edits) {
                                    Some(data) => data,
                                    None => {
                                        tracing::warn!(
                                            "Semantic tokens delta response {} for {} had invalid edits",
                                            request_id,
                                            uri
                                        );
                                        return;
                                    }
                                };
                                SemanticTokensDeltaDecode {
                                    result_id: existing_result_id,
                                    raw_data: updated,
                                }
                            }
                            None => SemanticTokensDeltaDecode {
                                result_id: None,
                                raw_data: Vec::new(),
                            },
                        };

                        let spans = decode_semantic_token_raw_data(
                            &state.buffer,
                            &legend,
                            &decoded.raw_data,
                            0,
                        );

                        crate::services::lsp::semantic_tokens::apply_semantic_tokens_to_state(
                            state,
                            &spans,
                            &self.theme,
                        );

                        state.set_semantic_tokens(SemanticTokenStore {
                            version: current_version,
                            result_id: decoded.result_id,
                            data: decoded.raw_data,
                            tokens: spans,
                        });
                    }
                }
            }
            _ => {
                tracing::warn!(
                    "Semantic tokens response {} for {} had mismatched pending state",
                    request_id,
                    uri
                );
            }
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

        // LSP should already be running since we got a quiescent notification
        let Some(client) = lsp.get_handle_mut(&language) else {
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

        // Folding ranges may improve after project is fully loaded
        self.request_folding_ranges_for_language(&language);
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
        self.lsp_server_statuses.insert(language.clone(), status);
        self.update_lsp_status_from_server_statuses();

        // Update warning domain for LSP status indicator
        self.update_lsp_warning_domain();

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
    #[allow(dead_code)] // Prepared for future use when AsyncMessage::LspCustomNotification is added
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

    /// Handle LSP server request (server -> client)
    /// These are requests from the LSP server that require handling, typically
    /// custom/extension methods specific to certain language servers.
    pub(super) fn handle_lsp_server_request(
        &mut self,
        language: String,
        server_command: String,
        method: String,
        params: Option<Value>,
    ) {
        tracing::debug!(
            "LSP server request {} from {} ({})",
            method,
            language,
            server_command
        );

        // Convert params to JSON string for the hook
        let params_str = params.map(|p| p.to_string());

        // Run the lsp_server_request hook for plugins
        self.plugin_manager.run_hook(
            "lsp_server_request",
            crate::services::plugins::hooks::HookArgs::LspServerRequest {
                language,
                method,
                server_command,
                params: params_str,
            },
        );
    }

    /// Handle plugin LSP response
    pub(super) fn handle_plugin_lsp_response(
        &mut self,
        request_id: u64,
        result: Result<Value, String>,
    ) {
        use fresh_core::api::JsCallbackId;
        tracing::debug!("Received plugin LSP response (request_id={})", request_id);
        let callback_id = JsCallbackId::from(request_id);
        match result {
            Ok(value) => {
                self.plugin_manager
                    .resolve_callback(callback_id, value.to_string());
            }
            Err(err) => {
                self.plugin_manager.reject_callback(callback_id, err);
            }
        }
    }

    /// Handle generic plugin response (e.g., GetBufferText result)
    pub(super) fn handle_plugin_response(&mut self, response: fresh_core::api::PluginResponse) {
        tracing::debug!("Received plugin response: {:?}", response);
        self.send_plugin_response(response);
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
            if self.time_source.elapsed_since(*window_start) < DEBOUNCE_WINDOW {
                *count += 1;

                if *count >= RAPID_REVERT_THRESHOLD {
                    // Disable auto-revert
                    self.auto_revert_enabled = false;
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
                *window_start = self.time_source.now();
            }
        } else {
            // First event for this file
            self.file_rapid_change_counts
                .insert(path_buf.clone(), (self.time_source.now(), 1));
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

        // Apply show_hidden / show_gitignored settings.
        // Use pending session-restore values if present, otherwise fall back
        // to the persisted config so the setting survives across sessions.
        let show_hidden = self
            .pending_file_explorer_show_hidden
            .take()
            .unwrap_or(self.config.file_explorer.show_hidden);
        view.ignore_patterns_mut().set_show_hidden(show_hidden);
        tracing::debug!("Applied show_hidden={} on init", show_hidden);

        let show_gitignored = self
            .pending_file_explorer_show_gitignored
            .take()
            .unwrap_or(self.config.file_explorer.show_gitignored);
        view.ignore_patterns_mut()
            .set_show_gitignored(show_gitignored);
        tracing::debug!("Applied show_gitignored={} on init", show_gitignored);

        self.file_explorer = Some(view);
        self.set_status_message(t!("status.file_explorer_ready").to_string());
    }

    /// Handle file explorer node toggle completed
    pub(super) fn handle_file_explorer_toggle_node(&mut self, node_id: NodeId) {
        tracing::debug!("File explorer toggle completed for node {:?}", node_id);
    }

    /// Handle file explorer node refresh completed
    pub(super) fn handle_file_explorer_refresh_node(&mut self, node_id: NodeId) {
        tracing::debug!("File explorer refresh completed for node {:?}", node_id);
        self.set_status_message(t!("explorer.refreshed_default").to_string());
    }

    /// Handle file explorer expanded to path
    pub(super) fn handle_file_explorer_expanded_to_path(&mut self, mut view: FileTreeView) {
        tracing::trace!(
            "handle_file_explorer_expanded_to_path: restoring file_explorer after async expand"
        );
        view.update_scroll_for_selection();
        self.file_explorer = Some(view);
        self.file_explorer_sync_in_progress = false;
    }
}

// =============================================================================
// Plugin Handlers
// =============================================================================

impl Editor {
    /// Handle plugin process output completion
    pub(super) fn handle_plugin_process_output(
        &mut self,
        callback_id: fresh_core::api::JsCallbackId,
        stdout: String,
        stderr: String,
        exit_code: i32,
    ) {
        tracing::debug!(
            "Process {} completed: exit_code={}, stdout_len={}, stderr_len={}",
            callback_id,
            exit_code,
            stdout.len(),
            stderr.len()
        );
        // Resolve the plugin callback with the process output
        // Using SpawnResult struct ensures field names match TypeScript types
        let result = fresh_core::api::SpawnResult {
            stdout,
            stderr,
            exit_code,
        };
        self.plugin_manager
            .resolve_callback(callback_id, serde_json::to_string(&result).unwrap());
    }

    /// Process TypeScript plugin commands
    ///
    /// Returns true if any visual commands were processed (i.e. a re-render is needed).
    /// No-op sentinels like `HookCompleted` do not count.
    pub(super) fn process_plugin_commands(&mut self) -> bool {
        let commands = self.plugin_manager.process_commands();
        if commands.is_empty() {
            return false;
        }

        let has_visual_commands = commands
            .iter()
            .any(|c| !matches!(c, fresh_core::api::PluginCommand::HookCompleted { .. }));

        let cmd_names: Vec<String> = commands.iter().map(|c| c.debug_variant_name()).collect();
        tracing::info!(
            count = commands.len(),
            cmds = ?cmd_names,
            "process_plugin_commands"
        );

        for command in &commands {
            match command {
                fresh_core::api::PluginCommand::RegisterGrammar {
                    language,
                    grammar_path,
                    extensions,
                } => {
                    tracing::info!(
                        "[SYNTAX DEBUG] processing RegisterGrammar: lang='{}', path='{}', ext={:?}",
                        language,
                        grammar_path,
                        extensions
                    );
                }
                fresh_core::api::PluginCommand::ReloadGrammars => {
                    tracing::info!("[SYNTAX DEBUG] processing ReloadGrammars command");
                }
                _ => {}
            }
        }

        for command in commands {
            tracing::trace!(
                "process_plugin_commands: handling command {:?}",
                std::mem::discriminant(&command)
            );
            if let Err(e) = self.handle_plugin_command(command) {
                tracing::error!("Error handling TypeScript plugin command: {}", e);
            }
        }

        has_visual_commands
    }

    /// Process pending plugin action completions
    #[cfg(feature = "plugins")]
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
    pub(super) fn resend_did_open_for_language(&mut self, language: &str) {
        // Find all open buffers for this language using stored buffer language
        let buffers_for_language: Vec<_> = self
            .buffers
            .iter()
            .filter_map(|(buf_id, state)| {
                if state.language == language {
                    self.buffer_metadata
                        .get(buf_id)
                        .and_then(|meta| meta.file_path().map(|p| (*buf_id, p.clone())))
                } else {
                    None
                }
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
                    let lang_id = state.language.clone();
                    if let Some(lsp) = self.lsp.as_mut() {
                        // LSP should already be running since we just restarted it
                        if let Some(handle) = lsp.get_handle_mut(&lang_id) {
                            let handle_id = handle.id();
                            if let Err(e) = handle.did_open(uri, content, lang_id) {
                                tracing::warn!("LSP did_open failed after restart: {}", e);
                            } else {
                                // Mark buffer as opened with this handle so that
                                // send_lsp_changes_for_buffer doesn't re-send didOpen
                                if let Some(metadata) = self.buffer_metadata.get_mut(&buffer_id) {
                                    metadata.lsp_opened_with.insert(handle_id);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /// Request semantic tokens for all open buffers matching a language.
    pub(super) fn request_semantic_tokens_for_language(&mut self, language: &str) {
        // Use stored buffer language instead of detecting from path
        let buffer_ids: Vec<_> = self
            .buffers
            .iter()
            .filter_map(|(buffer_id, state)| {
                if state.language == language {
                    Some(*buffer_id)
                } else {
                    None
                }
            })
            .collect();

        for buffer_id in buffer_ids {
            self.schedule_semantic_tokens_full_refresh(buffer_id);
        }
    }

    /// Request folding ranges for all open buffers matching a language.
    pub(super) fn request_folding_ranges_for_language(&mut self, language: &str) {
        let buffer_ids: Vec<_> = self
            .buffers
            .iter()
            .filter_map(|(buffer_id, state)| {
                if state.language == language {
                    Some(*buffer_id)
                } else {
                    None
                }
            })
            .collect();

        for buffer_id in buffer_ids {
            self.schedule_folding_ranges_refresh(buffer_id);
        }
    }
}

fn semantic_tokens_to_raw(tokens: &[SemanticToken]) -> Vec<u32> {
    let mut raw = Vec::with_capacity(tokens.len().saturating_mul(5));
    for token in tokens {
        raw.push(token.delta_line);
        raw.push(token.delta_start);
        raw.push(token.length);
        raw.push(token.token_type);
        raw.push(token.token_modifiers_bitset);
    }
    raw
}

fn decode_semantic_token_raw_data(
    buffer: &Buffer,
    legend: &SemanticTokensLegend,
    data: &[u32],
    base_line: usize,
) -> Vec<SemanticTokenSpan> {
    if !data.len().is_multiple_of(5) {
        tracing::warn!(
            "Semantic token data length {} is not divisible by 5",
            data.len()
        );
        return Vec::new();
    }

    let mut result = Vec::with_capacity(data.len() / 5);
    let mut current_line = base_line as u32;
    let mut current_start = 0u32;

    for chunk in data.chunks_exact(5) {
        let delta_line = chunk[0];
        let delta_start = chunk[1];
        let length = chunk[2];
        let token_type = chunk[3];
        let token_modifiers_bitset = chunk[4];

        current_line += delta_line;
        if delta_line == 0 {
            current_start += delta_start;
        } else {
            current_start = delta_start;
        }

        let start_utf16 = current_start as usize;
        let end_utf16 = start_utf16 + length as usize;
        let start_byte = buffer.lsp_position_to_byte(current_line as usize, start_utf16);
        let end_byte = buffer.lsp_position_to_byte(current_line as usize, end_utf16);

        let token_type_name = legend
            .token_types
            .get(token_type as usize)
            .map(|ty| ty.as_str().to_string())
            .unwrap_or_else(|| "unknown".to_string());

        let mut modifiers = Vec::new();
        for (idx, modifier) in legend.token_modifiers.iter().enumerate() {
            if (token_modifiers_bitset >> idx) & 1 == 1 {
                modifiers.push(modifier.as_str().to_string());
            }
        }

        result.push(SemanticTokenSpan {
            range: start_byte..end_byte,
            token_type: token_type_name,
            modifiers,
        });
    }

    result
}

struct SemanticTokenDecode {
    raw: Vec<u32>,
    spans: Vec<SemanticTokenSpan>,
}

struct SemanticTokensFullDecode {
    result_id: Option<String>,
    raw_data: Vec<u32>,
    spans: Vec<SemanticTokenSpan>,
}

struct SemanticTokensDeltaDecode {
    result_id: Option<String>,
    raw_data: Vec<u32>,
}

fn decode_semantic_token_data(
    buffer: &Buffer,
    legend: &SemanticTokensLegend,
    data: &[SemanticToken],
    base_line: usize,
) -> SemanticTokenDecode {
    let raw = semantic_tokens_to_raw(data);
    let spans = decode_semantic_token_raw_data(buffer, legend, &raw, base_line);
    SemanticTokenDecode { raw, spans }
}

fn apply_semantic_token_edits(
    mut data: Vec<u32>,
    edits: &[SemanticTokensEdit],
) -> Option<Vec<u32>> {
    if edits.is_empty() {
        return Some(data);
    }

    for edit in edits.iter().rev() {
        let start = edit.start as usize;
        let delete_count = edit.delete_count as usize;
        if start > data.len() || start.saturating_add(delete_count) > data.len() {
            return None;
        }

        let insert = edit
            .data
            .as_ref()
            .map(|tokens| semantic_tokens_to_raw(tokens))
            .unwrap_or_default();

        data.splice(start..start + delete_count, insert);
    }

    Some(data)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn semantic_token_delta_edits_apply() {
        let base = vec![0, 0, 2, 0, 0, 0, 3, 4, 1, 0];
        let edit = SemanticTokensEdit {
            start: 5,
            delete_count: 5,
            data: Some(vec![SemanticToken {
                delta_line: 0,
                delta_start: 5,
                length: 1,
                token_type: 2,
                token_modifiers_bitset: 0,
            }]),
        };

        let updated = apply_semantic_token_edits(base, &[edit]).expect("edit should apply");
        assert_eq!(updated.len(), 10);
        assert_eq!(&updated[5..10], &[0, 5, 1, 2, 0]);
    }
}
