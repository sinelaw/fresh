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
                        let _ = handle.did_open(uri, content, lang_id);
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
}
