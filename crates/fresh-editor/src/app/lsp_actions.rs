//! LSP-related action handlers.
//!
//! This module contains handlers for LSP actions that require complex logic,
//! such as restarting LSP servers and managing server lifecycle.

use super::Editor;
use crate::input::commands::Suggestion;
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
                        let _ = handle.did_open(uri, content, lang_id);

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

    /// Disable LSP for a specific buffer and clear all LSP-related data
    fn disable_lsp_for_buffer(&mut self, buffer_id: crate::model::event::BufferId) {
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
        }

        // Clear LSP-related overlays (inlay hints) for this buffer
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.virtual_texts.clear(&mut state.marker_list);
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
        let _ = handle.document_diagnostic(request_id, uri.clone(), previous_result_id);

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
            let _ = handle.inlay_hints(request_id, uri, 0, 0, last_line, last_char);
        }
    }
}
