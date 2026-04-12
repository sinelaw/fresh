//! LSP-related action handlers.
//!
//! This module contains handlers for LSP actions that require complex logic,
//! such as restarting LSP servers and managing server lifecycle.

use super::Editor;
use crate::input::commands::Suggestion;
use crate::model::event::BufferId;
use crate::view::prompt::{Prompt, PromptType};
use rust_i18n::t;

impl Editor {
    /// Handle the LspRestart action.
    ///
    /// For a single-server config, restarts immediately (no prompt).
    /// For multiple servers, shows a prompt to select which server(s) to restart.
    pub fn handle_lsp_restart(&mut self) {
        // Get the language and file path from the active buffer
        let buffer_id = self.active_buffer();
        let Some(state) = self.buffers.get(&buffer_id) else {
            return;
        };
        let language = state.language.clone();
        let file_path = self
            .buffer_metadata
            .get(&buffer_id)
            .and_then(|meta| meta.file_path().cloned());

        // Get configured servers for this language
        let configs: Vec<_> = self
            .lsp
            .as_ref()
            .and_then(|lsp| lsp.get_configs(&language))
            .map(|c| c.to_vec())
            .unwrap_or_default();

        if configs.is_empty() {
            self.set_status_message(t!("lsp.no_server_configured").to_string());
            return;
        }

        // Single server: restart immediately without a prompt (backward compat)
        if configs.len() == 1 {
            let Some(lsp) = self.lsp.as_mut() else {
                self.set_status_message(t!("lsp.no_manager").to_string());
                return;
            };

            let (success, message) = lsp.manual_restart(&language, file_path.as_deref());
            self.status_message = Some(message);

            if success {
                self.reopen_buffers_for_language(&language);
            }
            return;
        }

        // Multiple servers: show a prompt
        let mut suggestions: Vec<Suggestion> = Vec::new();

        // Default option: restart all enabled servers
        let enabled_names: Vec<_> = configs
            .iter()
            .filter(|c| c.enabled && !c.command.is_empty())
            .map(|c| c.display_name())
            .collect();
        let all_description = if enabled_names.is_empty() {
            Some("No enabled servers".to_string())
        } else {
            Some(enabled_names.join(", "))
        };
        suggestions.push(Suggestion {
            text: format!("{} (all enabled)", language),
            description: all_description,
            value: Some(language.clone()),
            disabled: enabled_names.is_empty(),
            keybinding: None,
            source: None,
        });

        // Individual server options
        for config in &configs {
            if config.command.is_empty() {
                continue;
            }
            let name = config.display_name();
            let status = if config.enabled { "" } else { " [disabled]" };
            suggestions.push(Suggestion {
                text: format!("{}/{}{}", language, name, status),
                description: Some(format!("Command: {}", config.command)),
                value: Some(format!("{}/{}", language, name)),
                disabled: false,
                keybinding: None,
                source: None,
            });
        }

        // Start prompt with suggestions
        self.prompt = Some(Prompt::with_suggestions(
            "Restart LSP server: ".to_string(),
            PromptType::RestartLspServer,
            suggestions.clone(),
        ));

        // Configure initial selection
        if let Some(prompt) = self.prompt.as_mut() {
            prompt.selected_suggestion = Some(0);
        }
    }

    /// Send didOpen notifications for all buffers of a given language to any
    /// server handles that haven't received them yet.
    ///
    /// Called after an LSP server starts or restarts so it immediately knows
    /// about every open file (rather than waiting for the next user edit).
    pub(crate) fn reopen_buffers_for_language(&mut self, language: &str) {
        // Collect buffer info first to avoid borrow conflicts
        // Use buffer's stored language rather than detecting from path
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

        for (buffer_id, buf_path) in buffers_for_language {
            let Some(state) = self.buffers.get(&buffer_id) else {
                continue;
            };

            let Some(content) = state.buffer.to_string() else {
                continue; // Skip buffers that aren't fully loaded
            };

            let Some(uri) = super::types::file_path_to_lsp_uri(&buf_path) else {
                continue;
            };

            let lang_id = state.language.clone();

            if let Some(lsp) = self.lsp.as_mut() {
                // Respect auto_start setting for this user action
                use crate::services::lsp::manager::LspSpawnResult;
                if lsp.try_spawn(&lang_id, Some(&buf_path)) != LspSpawnResult::Spawned {
                    continue;
                }

                // Collect handles that need didOpen (not yet tracked in
                // lsp_opened_with for this buffer).
                let opened_with = self
                    .buffer_metadata
                    .get(&buffer_id)
                    .map(|m| m.lsp_opened_with.clone())
                    .unwrap_or_default();

                let handles_needing_open: Vec<(String, u64)> = lsp
                    .get_handles(&lang_id)
                    .into_iter()
                    .filter(|sh| !opened_with.contains(&sh.handle.id()))
                    .map(|sh| (sh.name.clone(), sh.handle.id()))
                    .collect();

                // Send didOpen to each handle that hasn't seen this buffer yet
                for (name, handle_id) in handles_needing_open {
                    let sh = lsp
                        .get_handles_mut(&lang_id)
                        .into_iter()
                        .find(|s| s.handle.id() == handle_id);

                    if let Some(sh) = sh {
                        if let Err(e) =
                            sh.handle
                                .did_open(uri.clone(), content.clone(), lang_id.clone())
                        {
                            tracing::warn!("LSP did_open to '{}' failed: {}", name, e);
                        } else if let Some(metadata) = self.buffer_metadata.get_mut(&buffer_id) {
                            metadata.lsp_opened_with.insert(handle_id);
                        }
                    }
                }
            }
        }
    }

    /// Handle the LspStop action.
    ///
    /// Shows a prompt to select which LSP server to stop, with suggestions
    /// for all currently running servers.
    pub fn handle_lsp_stop(&mut self) {
        let running_languages: Vec<String> = self
            .lsp
            .as_ref()
            .map(|lsp| lsp.running_servers())
            .unwrap_or_default();

        if running_languages.is_empty() {
            self.set_status_message(t!("lsp.no_servers_running").to_string());
            return;
        }

        // Build suggestions showing server names when multiple servers per language
        let mut suggestions: Vec<Suggestion> = Vec::new();
        for lang in &running_languages {
            let server_names: Vec<String> = self
                .lsp
                .as_ref()
                .map(|lsp| lsp.server_names_for_language(lang))
                .unwrap_or_default();

            if server_names.len() > 1 {
                // Multiple servers: show each individually
                for name in &server_names {
                    let description = Some(format!("Server: {}", name));
                    suggestions.push(Suggestion {
                        text: format!("{}/{}", lang, name),
                        description,
                        // Value carries "language/server_name" so the handler
                        // knows exactly which server to stop.
                        value: Some(format!("{}/{}", lang, name)),
                        disabled: false,
                        keybinding: None,
                        source: None,
                    });
                }
            } else {
                // Single server: show language only (value = just language)
                let description = self
                    .lsp
                    .as_ref()
                    .and_then(|lsp| lsp.get_config(lang))
                    .filter(|c| !c.command.is_empty())
                    .map(|c| format!("Command: {}", c.command));

                suggestions.push(Suggestion {
                    text: lang.clone(),
                    description,
                    value: Some(lang.clone()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                });
            }
        }

        // Start prompt with suggestions
        self.prompt = Some(Prompt::with_suggestions(
            "Stop LSP server: ".to_string(),
            PromptType::StopLspServer,
            suggestions.clone(),
        ));

        // Configure initial selection
        if let Some(prompt) = self.prompt.as_mut() {
            if suggestions.len() == 1 {
                // If only one entry, pre-fill the input with it
                prompt.input = suggestions[0].text.clone();
                prompt.cursor_pos = prompt.input.len();
                prompt.selected_suggestion = Some(0);
            } else if !prompt.suggestions.is_empty() {
                // Auto-select first suggestion
                prompt.selected_suggestion = Some(0);
            }
        }
    }

    /// Handle the LspToggleForBuffer action.
    ///
    /// Toggles LSP on/off for the current buffer only.
    /// Requires an LSP server to be configured for the current buffer's language.
    pub fn handle_lsp_toggle_for_buffer(&mut self) {
        let buffer_id = self.active_buffer();

        // Get the buffer's language to check if LSP is configured
        let language = {
            let Some(state) = self.buffers.get(&buffer_id) else {
                return;
            };
            state.language.clone()
        };

        // Check if LSP is configured for this language
        let lsp_configured = self
            .lsp
            .as_ref()
            .and_then(|lsp| lsp.get_config(&language))
            .is_some();

        if !lsp_configured {
            self.set_status_message(t!("lsp.no_server_configured").to_string());
            return;
        }

        // Check current LSP state
        let (was_enabled, file_path) = {
            let Some(metadata) = self.buffer_metadata.get(&buffer_id) else {
                return;
            };
            (metadata.lsp_enabled, metadata.file_path().cloned())
        };

        if was_enabled {
            self.disable_lsp_for_buffer(buffer_id);
        } else {
            self.enable_lsp_for_buffer(buffer_id, &language, file_path);
        }
    }

    /// Handle an action from the LSP status details popup.
    ///
    /// Action keys have the format:
    /// - `restart:<language>/<server_name>` — restart a specific server
    /// - `start:<language>` — start LSP server(s) for a language
    /// - `stop:<language>/<server_name>` — stop a specific server
    /// - `log:<language>` — open the LSP log file for the language
    pub fn handle_lsp_status_action(&mut self, action_key: &str) {
        if let Some(language) = action_key.strip_prefix("start:") {
            // Start/restart LSP for this language (same as the "Start/Restart LSP" command)
            let file_path = self
                .buffer_metadata
                .get(&self.active_buffer())
                .and_then(|meta| meta.file_path().cloned());

            if let Some(lsp) = self.lsp.as_mut() {
                let (_, message) = lsp.manual_restart(language, file_path.as_deref());
                self.status_message = Some(message);
            } else {
                self.status_message = Some("No LSP manager available".to_string());
            }
            self.reopen_buffers_for_language(language);
        } else if let Some(target) = action_key.strip_prefix("restart:") {
            // Parse language/server_name
            if let Some((language, server_name)) = target.split_once('/') {
                let file_path = self
                    .buffer_metadata
                    .get(&self.active_buffer())
                    .and_then(|meta| meta.file_path().cloned());

                if let Some(lsp) = self.lsp.as_mut() {
                    // Shutdown the specific server first, then re-spawn
                    lsp.shutdown_server_by_name(language, server_name);
                }
                // Remove the status entry so it gets re-created on spawn
                self.lsp_server_statuses
                    .remove(&(language.to_string(), server_name.to_string()));
                if let Some(lsp) = self.lsp.as_mut() {
                    let _ = lsp.manual_restart(language, file_path.as_deref());
                }
                self.reopen_buffers_for_language(language);
                self.status_message = Some(format!(
                    "Restarting LSP server: {}/{}",
                    language, server_name
                ));
            }
        } else if let Some(target) = action_key.strip_prefix("stop:") {
            if let Some((language, server_name)) = target.split_once('/') {
                let stopped = if let Some(lsp) = self.lsp.as_mut() {
                    lsp.shutdown_server_by_name(language, server_name)
                } else {
                    false
                };
                if stopped {
                    self.lsp_server_statuses
                        .remove(&(language.to_string(), server_name.to_string()));
                    self.status_message =
                        Some(format!("Stopped LSP server: {}/{}", language, server_name));
                } else {
                    self.status_message = Some(format!(
                        "LSP server not running: {}/{}",
                        language, server_name
                    ));
                }
            }
        } else if let Some(language) = action_key.strip_prefix("log:") {
            let log_path = crate::services::log_dirs::lsp_log_path(language);
            if log_path.exists() {
                match self.open_local_file(&log_path) {
                    Ok(buffer_id) => {
                        self.mark_buffer_read_only(buffer_id, true);
                    }
                    Err(e) => {
                        self.status_message = Some(format!("Failed to open LSP log: {}", e));
                    }
                }
            } else {
                self.status_message = Some(format!("No log file found for {}", language));
            }
        }
    }

    /// Toggle folding at the current cursor position.
    pub fn toggle_fold_at_cursor(&mut self) {
        let buffer_id = self.active_buffer();
        let pos = self.active_cursors().primary().position;
        self.toggle_fold_at_byte(buffer_id, pos);
    }

    /// Toggle folding for the given line in the specified buffer.
    ///
    /// Kept for callers that only have a line number (e.g. gutter clicks
    /// that already resolved the line).  Converts to a byte position and
    /// delegates to [`Self::toggle_fold_at_byte`].
    pub fn toggle_fold_at_line(&mut self, buffer_id: BufferId, line: usize) {
        let byte_pos = {
            let Some(state) = self.buffers.get(&buffer_id) else {
                return;
            };
            state.buffer.line_start_offset(line).unwrap_or_else(|| {
                use crate::view::folding::indent_folding;
                let approx = line * state.buffer.estimated_line_length();
                indent_folding::find_line_start_byte(&state.buffer, approx)
            })
        };
        self.toggle_fold_at_byte(buffer_id, byte_pos);
    }

    /// Toggle folding at the given byte position in the specified buffer.
    pub fn toggle_fold_at_byte(&mut self, buffer_id: BufferId, byte_pos: usize) {
        let split_id = self.split_manager.active_split();
        let (buffers, split_view_states) = (&mut self.buffers, &mut self.split_view_states);

        let Some(state) = buffers.get_mut(&buffer_id) else {
            return;
        };

        let Some(view_state) = split_view_states.get_mut(&split_id) else {
            return;
        };
        let buf_state = view_state.ensure_buffer_state(buffer_id);

        // Try to unfold first — check if this byte's line is a fold header.
        let header_byte = {
            use crate::view::folding::indent_folding;
            indent_folding::find_line_start_byte(&state.buffer, byte_pos)
        };
        if buf_state
            .folds
            .remove_by_header_byte(&state.buffer, &mut state.marker_list, header_byte)
        {
            return;
        }

        // Also unfold if the byte position is inside an existing fold.
        if buf_state
            .folds
            .remove_if_contains_byte(&mut state.marker_list, byte_pos)
        {
            return;
        }

        // Determine the fold byte range: prefer LSP ranges, fall back to indent-based.
        if !state.folding_ranges.is_empty() {
            // --- LSP-provided ranges (line-based) ---
            // LSP ranges use line numbers, so we need get_line_number here.
            let line = state.buffer.get_line_number(byte_pos);
            let mut exact_range: Option<&lsp_types::FoldingRange> = None;
            let mut exact_span = usize::MAX;
            let mut containing_range: Option<&lsp_types::FoldingRange> = None;
            let mut containing_span = usize::MAX;

            for range in &state.folding_ranges {
                let start_line = range.start_line as usize;
                let range_end = range.end_line as usize;
                if range_end <= start_line {
                    continue;
                }
                let span = range_end.saturating_sub(start_line);

                if start_line == line && span < exact_span {
                    exact_span = span;
                    exact_range = Some(range);
                }
                if start_line <= line && line <= range_end && span < containing_span {
                    containing_span = span;
                    containing_range = Some(range);
                }
            }

            let chosen = exact_range.or(containing_range);
            let Some(range) = chosen else {
                return;
            };
            let placeholder = range
                .collapsed_text
                .as_ref()
                .filter(|text| !text.trim().is_empty())
                .cloned();
            let header_line = range.start_line as usize;
            let end_line = range.end_line as usize;
            let first_hidden = header_line.saturating_add(1);
            if first_hidden > end_line {
                return;
            }
            let Some(sb) = state.buffer.line_start_offset(first_hidden) else {
                return;
            };
            let eb = state
                .buffer
                .line_start_offset(end_line.saturating_add(1))
                .unwrap_or_else(|| state.buffer.len());
            let hb = state.buffer.line_start_offset(header_line).unwrap_or(0);
            Self::create_fold(state, buf_state, sb, eb, hb, placeholder);
        } else {
            // --- Indent-based folding on bytes ---
            use crate::view::folding::indent_folding;
            let tab_size = state.buffer_settings.tab_size;
            let max_upward = crate::config::INDENT_FOLD_MAX_UPWARD_SCAN;
            let est_ll = state.buffer.estimated_line_length();
            let max_scan_bytes = crate::config::INDENT_FOLD_MAX_SCAN_LINES * est_ll;

            // Ensure the region around the cursor is loaded from disk so the
            // immutable slice_bytes in find_fold_range_at_byte can read it.
            let upward_bytes = max_upward * est_ll;
            let load_start = byte_pos.saturating_sub(upward_bytes);
            let load_end = byte_pos
                .saturating_add(max_scan_bytes)
                .min(state.buffer.len());
            // Load chunks from disk so immutable slice_bytes in
            // find_fold_range_at_byte can read the region.
            drop(
                state
                    .buffer
                    .get_text_range_mut(load_start, load_end - load_start),
            );

            if let Some((hb, sb, eb)) = indent_folding::find_fold_range_at_byte(
                &state.buffer,
                byte_pos,
                tab_size,
                max_scan_bytes,
                max_upward,
            ) {
                Self::create_fold(state, buf_state, sb, eb, hb, None);
            }
        }
    }

    fn create_fold(
        state: &mut crate::state::EditorState,
        buf_state: &mut crate::view::split::BufferViewState,
        start_byte: usize,
        end_byte: usize,
        header_byte: usize,
        placeholder: Option<String>,
    ) {
        if end_byte <= start_byte {
            return;
        }

        // Move any cursors inside the soon-to-be-hidden range to the header line.
        buf_state.cursors.map(|cursor| {
            let in_hidden_range = cursor.position >= start_byte && cursor.position < end_byte;
            let anchor_in_hidden = cursor
                .anchor
                .is_some_and(|anchor| anchor >= start_byte && anchor < end_byte);
            if in_hidden_range || anchor_in_hidden {
                cursor.position = header_byte;
                cursor.anchor = None;
                cursor.sticky_column = 0;
                cursor.selection_mode = crate::model::cursor::SelectionMode::Normal;
                cursor.block_anchor = None;
                cursor.deselect_on_move = true;
            }
        });

        buf_state
            .folds
            .add(&mut state.marker_list, start_byte, end_byte, placeholder);

        // If the viewport top is now inside the folded range, move it to the header.
        if buf_state.viewport.top_byte >= start_byte && buf_state.viewport.top_byte < end_byte {
            buf_state.viewport.top_byte = header_byte;
            buf_state.viewport.top_view_line_offset = 0;
        }
    }

    /// Send didClose to a specific named server for all buffers of a language.
    ///
    /// Used when stopping a single server out of multiple for the same language,
    /// where we don't want to fully disable LSP for the buffers.
    pub(crate) fn send_did_close_to_server(&mut self, language: &str, server_name: &str) {
        let uris: Vec<_> = self
            .buffers
            .iter()
            .filter(|(_, s)| s.language == language)
            .filter_map(|(id, _)| {
                self.buffer_metadata
                    .get(id)
                    .and_then(|m| m.file_uri())
                    .cloned()
            })
            .collect();

        if let Some(lsp) = self.lsp.as_mut() {
            for sh in lsp.get_handles_mut(language) {
                if sh.name == server_name {
                    for uri in &uris {
                        tracing::info!(
                            "Sending didClose for {} to '{}' (language: {})",
                            uri.as_str(),
                            sh.name,
                            language
                        );
                        if let Err(e) = sh.handle.did_close(uri.clone()) {
                            tracing::warn!("Failed to send didClose to '{}': {}", sh.name, e);
                        }
                    }
                    break;
                }
            }
        }
    }

    /// Disable LSP for a specific buffer and clear all LSP-related data
    pub(crate) fn disable_lsp_for_buffer(&mut self, buffer_id: crate::model::event::BufferId) {
        // Send didClose to the LSP server so it removes the document from its
        // tracking. This is critical: without didClose, the async handler's
        // document_versions still has the path, and should_skip_did_open will
        // block the didOpen when LSP is re-enabled — causing a desync where
        // the server has stale content. (GitHub issue #952)
        if let Some(uri) = self
            .buffer_metadata
            .get(&buffer_id)
            .and_then(|m| m.file_uri())
            .cloned()
        {
            let language = self
                .buffers
                .get(&buffer_id)
                .map(|s| s.language.clone())
                .unwrap_or_default();
            if let Some(lsp) = self.lsp.as_mut() {
                // Broadcast didClose to all handles for this language
                if !lsp.has_handles(&language) {
                    tracing::warn!(
                        "disable_lsp_for_buffer: no handle for language '{}'",
                        language
                    );
                } else {
                    for sh in lsp.get_handles_mut(&language) {
                        tracing::info!(
                            "Sending didClose for {} to '{}' (language: {})",
                            uri.as_str(),
                            sh.name,
                            language
                        );
                        if let Err(e) = sh.handle.did_close(uri.clone()) {
                            tracing::warn!("Failed to send didClose to '{}': {}", sh.name, e);
                        }
                    }
                }
            } else {
                tracing::warn!("disable_lsp_for_buffer: no LSP manager");
            }
        } else {
            tracing::warn!("disable_lsp_for_buffer: no URI for buffer");
        }

        // Disable LSP in metadata
        if let Some(metadata) = self.buffer_metadata.get_mut(&buffer_id) {
            metadata.disable_lsp(t!("lsp.disabled.user").to_string());
            // Clear LSP opened tracking so it will be sent again if re-enabled
            metadata.lsp_opened_with.clear();
        }
        self.set_status_message(t!("lsp.disabled_for_buffer").to_string());

        // Clear diagnostics for this buffer
        let uri = self
            .buffer_metadata
            .get(&buffer_id)
            .and_then(|m| m.file_uri())
            .map(|u| u.as_str().to_string());

        if let Some(uri_str) = uri {
            self.stored_diagnostics.remove(&uri_str);
            self.stored_push_diagnostics.remove(&uri_str);
            self.stored_pull_diagnostics.remove(&uri_str);
            self.diagnostic_result_ids.remove(&uri_str);
            self.stored_folding_ranges.remove(&uri_str);
        }

        // Cancel scheduled diagnostic pull if it targets this buffer
        if let Some((scheduled_buf, _)) = &self.scheduled_diagnostic_pull {
            if *scheduled_buf == buffer_id {
                self.scheduled_diagnostic_pull = None;
            }
        }

        self.folding_ranges_in_flight.remove(&buffer_id);
        self.folding_ranges_debounce.remove(&buffer_id);
        self.pending_folding_range_requests
            .retain(|_, req| req.buffer_id != buffer_id);

        // Clear all LSP-related overlays for this buffer (diagnostics + inlay hints)
        let diagnostic_ns = crate::services::lsp::diagnostics::lsp_diagnostic_namespace();
        let (buffers, split_view_states) = (&mut self.buffers, &mut self.split_view_states);
        if let Some(state) = buffers.get_mut(&buffer_id) {
            state
                .overlays
                .clear_namespace(&diagnostic_ns, &mut state.marker_list);
            state.virtual_texts.clear(&mut state.marker_list);
            state.folding_ranges.clear();
            for view_state in split_view_states.values_mut() {
                if let Some(buf_state) = view_state.keyed_states.get_mut(&buffer_id) {
                    buf_state.folds.clear(&mut state.marker_list);
                }
            }
        }
    }

    /// Enable LSP for a specific buffer and send didOpen notification
    fn enable_lsp_for_buffer(
        &mut self,
        buffer_id: crate::model::event::BufferId,
        language: &str,
        file_path: Option<std::path::PathBuf>,
    ) {
        // Re-enable LSP in metadata
        if let Some(metadata) = self.buffer_metadata.get_mut(&buffer_id) {
            metadata.lsp_enabled = true;
            metadata.lsp_disabled_reason = None;
        }
        self.set_status_message(t!("lsp.enabled_for_buffer").to_string());

        // Send didOpen if we have a file path
        if let Some(_path) = file_path {
            self.send_lsp_did_open_for_buffer(buffer_id, language);
        }
    }

    /// Send LSP didOpen notification for a buffer
    fn send_lsp_did_open_for_buffer(
        &mut self,
        buffer_id: crate::model::event::BufferId,
        language: &str,
    ) {
        // Get the URI and buffer text
        let (uri, text) = {
            let metadata = self.buffer_metadata.get(&buffer_id);
            let uri = metadata.and_then(|m| m.file_uri()).cloned();
            let text = self
                .buffers
                .get(&buffer_id)
                .and_then(|state| state.buffer.to_string());
            (uri, text)
        };

        let Some(uri) = uri else { return };
        let Some(text) = text else { return };

        // Try to spawn and send didOpen
        use crate::services::lsp::manager::LspSpawnResult;
        let file_path = self
            .buffer_metadata
            .get(&buffer_id)
            .and_then(|m| m.file_path())
            .cloned();
        let Some(lsp) = self.lsp.as_mut() else {
            return;
        };

        if lsp.try_spawn(language, file_path.as_deref()) != LspSpawnResult::Spawned {
            return;
        }

        let Some(handle) = lsp.get_handle_mut(language) else {
            return;
        };

        let handle_id = handle.id();
        if let Err(e) = handle.did_open(uri.clone(), text, language.to_string()) {
            tracing::warn!("Failed to send didOpen to LSP: {}", e);
            return;
        }

        // Mark buffer as opened with this server
        if let Some(metadata) = self.buffer_metadata.get_mut(&buffer_id) {
            metadata.lsp_opened_with.insert(handle_id);
        }

        // Request diagnostics
        let request_id = self.next_lsp_request_id;
        self.next_lsp_request_id += 1;
        let previous_result_id = self.diagnostic_result_ids.get(uri.as_str()).cloned();
        if let Err(e) = handle.document_diagnostic(request_id, uri.clone(), previous_result_id) {
            tracing::warn!("LSP document_diagnostic request failed: {}", e);
        }

        // Request inlay hints if enabled
        if self.config.editor.enable_inlay_hints {
            let (last_line, last_char) = self
                .buffers
                .get(&buffer_id)
                .map(|state| {
                    let line_count = state.buffer.line_count().unwrap_or(1000);
                    (line_count.saturating_sub(1) as u32, 10000u32)
                })
                .unwrap_or((999, 10000));

            let request_id = self.next_lsp_request_id;
            self.next_lsp_request_id += 1;
            if let Err(e) = handle.inlay_hints(request_id, uri, 0, 0, last_line, last_char) {
                tracing::warn!("LSP inlay_hints request failed: {}", e);
            }
        }

        // Schedule folding range refresh
        self.schedule_folding_ranges_refresh(buffer_id);
    }

    /// Set up a plugin development workspace for LSP support on a buffer.
    ///
    /// Creates a temp directory with `fresh.d.ts` + `tsconfig.json` so that
    /// `typescript-language-server` can provide autocomplete and type checking
    /// for plugin buffers (including unsaved/unnamed ones).
    pub(crate) fn setup_plugin_dev_lsp(&mut self, buffer_id: BufferId, content: &str) {
        use crate::services::plugins::plugin_dev_workspace::PluginDevWorkspace;

        // Use the exact cached extraction location for fresh.d.ts
        #[cfg(feature = "embed-plugins")]
        let fresh_dts_path = {
            let Some(embedded_dir) = crate::services::plugins::embedded::get_embedded_plugins_dir()
            else {
                tracing::warn!(
                    "Cannot set up plugin dev LSP: embedded plugins directory not available"
                );
                return;
            };
            let path = embedded_dir.join("lib").join("fresh.d.ts");
            if !path.exists() {
                tracing::warn!(
                    "Cannot set up plugin dev LSP: fresh.d.ts not found at {:?}",
                    path
                );
                return;
            }
            path
        };

        #[cfg(not(feature = "embed-plugins"))]
        let fresh_dts_path = {
            // In non-embedded builds (development), use the source tree path
            let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("plugins")
                .join("lib")
                .join("fresh.d.ts");
            if !path.exists() {
                tracing::warn!(
                    "Cannot set up plugin dev LSP: fresh.d.ts not found at {:?}",
                    path
                );
                return;
            }
            path
        };

        // Create the workspace
        let buffer_id_num: usize = buffer_id.0;
        match PluginDevWorkspace::create(buffer_id_num, content, &fresh_dts_path) {
            Ok(workspace) => {
                let plugin_file = workspace.plugin_file.clone();

                // Update buffer metadata to point at the temp file, enabling LSP
                if let Some(metadata) = self.buffer_metadata.get_mut(&buffer_id) {
                    if let Some(uri) = super::types::file_path_to_lsp_uri(&plugin_file) {
                        metadata.kind = super::types::BufferKind::File {
                            path: plugin_file.clone(),
                            uri: Some(uri),
                        };
                        metadata.lsp_enabled = true;
                        metadata.lsp_disabled_reason = None;
                        // Clear any previous LSP opened state so didOpen is sent fresh
                        metadata.lsp_opened_with.clear();

                        tracing::info!(
                            "Plugin dev LSP enabled for buffer {} via {:?}",
                            buffer_id_num,
                            plugin_file
                        );
                    }
                }

                // Set buffer language to TypeScript so LSP requests use the right handle
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    let detected =
                        crate::primitives::detected_language::DetectedLanguage::from_path(
                            &plugin_file,
                            &self.grammar_registry,
                            &self.config.languages,
                        );
                    state.apply_language(detected);
                }

                // Allow TypeScript language so LSP auto-spawns
                if let Some(lsp) = &mut self.lsp {
                    lsp.allow_language("typescript");
                }

                // Store workspace for cleanup
                let workspace_dir = workspace.dir().to_path_buf();
                self.plugin_dev_workspaces.insert(buffer_id, workspace);

                // Actually spawn the LSP server and send didOpen for this buffer
                self.send_lsp_did_open_for_buffer(buffer_id, "typescript");

                // Add the plugin workspace folder so tsserver discovers tsconfig.json + fresh.d.ts
                if let Some(lsp) = &self.lsp {
                    if let Some(handle) = lsp.get_handle("typescript") {
                        if let Some(uri) = super::types::file_path_to_lsp_uri(&workspace_dir) {
                            let name = workspace_dir
                                .file_name()
                                .unwrap_or_default()
                                .to_string_lossy()
                                .into_owned();
                            if let Err(e) = handle.add_workspace_folder(uri, name) {
                                tracing::warn!("Failed to add plugin workspace folder: {}", e);
                            } else {
                                tracing::info!(
                                    "Added plugin workspace folder: {:?}",
                                    workspace_dir
                                );
                            }
                        }
                    }
                }
            }
            Err(e) => {
                tracing::warn!("Failed to create plugin dev workspace: {}", e);
            }
        }
    }
}
