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
    /// Restarts the LSP server for the current buffer's language and re-sends
    /// didOpen notifications for all buffers of that language.
    pub fn handle_lsp_restart(&mut self) {
        // Get the language from the buffer's stored state
        let buffer_id = self.active_buffer();
        let Some(state) = self.buffers.get(&buffer_id) else {
            return;
        };
        let language = state.language.clone();

        // Attempt restart
        let Some(lsp) = self.lsp.as_mut() else {
            self.set_status_message(t!("lsp.no_manager").to_string());
            return;
        };

        let (success, message) = lsp.manual_restart(&language);
        self.status_message = Some(message);

        if !success {
            return;
        }

        // Re-send didOpen for all buffers of this language
        self.reopen_buffers_for_language(&language);
    }

    /// Re-send didOpen notifications for all buffers of a given language.
    ///
    /// Called after LSP server restart to re-register open files.
    fn reopen_buffers_for_language(&mut self, language: &str) {
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

            let Some(uri) = url::Url::from_file_path(&buf_path)
                .ok()
                .and_then(|u| u.as_str().parse::<lsp_types::Uri>().ok())
            else {
                continue;
            };

            let lang_id = state.language.clone();

            if let Some(lsp) = self.lsp.as_mut() {
                // Respect auto_start setting for this user action
                use crate::services::lsp::manager::LspSpawnResult;
                if lsp.try_spawn(&lang_id) == LspSpawnResult::Spawned {
                    if let Some(handle) = lsp.get_handle_mut(&lang_id) {
                        let handle_id = handle.id();
                        if let Err(e) = handle.did_open(uri, content, lang_id) {
                            tracing::warn!("LSP did_open failed: {}", e);
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

    /// Handle the LspStop action.
    ///
    /// Shows a prompt to select which LSP server to stop, with suggestions
    /// for all currently running servers.
    pub fn handle_lsp_stop(&mut self) {
        let running_servers: Vec<String> = self
            .lsp
            .as_ref()
            .map(|lsp| lsp.running_servers())
            .unwrap_or_default();

        if running_servers.is_empty() {
            self.set_status_message(t!("lsp.no_servers_running").to_string());
            return;
        }

        // Create suggestions from running servers
        let suggestions: Vec<Suggestion> = running_servers
            .iter()
            .map(|lang| {
                let description = self
                    .lsp
                    .as_ref()
                    .and_then(|lsp| lsp.get_config(lang))
                    .filter(|c| !c.command.is_empty())
                    .map(|c| format!("Command: {}", c.command));

                Suggestion {
                    text: lang.clone(),
                    description,
                    value: Some(lang.clone()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }
            })
            .collect();

        // Start prompt with suggestions
        self.prompt = Some(Prompt::with_suggestions(
            "Stop LSP server: ".to_string(),
            PromptType::StopLspServer,
            suggestions,
        ));

        // Configure initial selection
        if let Some(prompt) = self.prompt.as_mut() {
            if running_servers.len() == 1 {
                // If only one server, pre-fill the input with it
                prompt.input = running_servers[0].clone();
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

    /// Disable LSP for a specific buffer and clear all LSP-related data
    fn disable_lsp_for_buffer(&mut self, buffer_id: crate::model::event::BufferId) {
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
                if let Some(handle) = lsp.get_handle_mut(&language) {
                    tracing::info!(
                        "Sending didClose for {} (language: {})",
                        uri.as_str(),
                        language
                    );
                    if let Err(e) = handle.did_close(uri) {
                        tracing::warn!("Failed to send didClose to LSP: {}", e);
                    }
                } else {
                    tracing::warn!(
                        "disable_lsp_for_buffer: no handle for language '{}'",
                        language
                    );
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
            self.diagnostic_result_ids.remove(&uri_str);
            self.stored_folding_ranges.remove(&uri_str);
        }

        self.folding_ranges_in_flight.remove(&buffer_id);
        self.folding_ranges_debounce.remove(&buffer_id);
        self.pending_folding_range_requests
            .retain(|_, req| req.buffer_id != buffer_id);

        // Clear LSP-related overlays (inlay hints) for this buffer
        let (buffers, split_view_states) = (&mut self.buffers, &mut self.split_view_states);
        if let Some(state) = buffers.get_mut(&buffer_id) {
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
        let Some(lsp) = self.lsp.as_mut() else {
            return;
        };

        if lsp.try_spawn(language) != LspSpawnResult::Spawned {
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
}
