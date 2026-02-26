//! Prompt confirmation action handlers.
//!
//! This module contains handlers for different prompt types when the user confirms input.

use rust_i18n::t;

use super::normalize_path;
use super::BufferId;
use super::BufferMetadata;
use super::Editor;
use crate::config_io::{ConfigLayer, ConfigResolver};
use crate::input::keybindings::Action;
use crate::primitives::path_utils::expand_tilde;
use crate::services::plugins::hooks::HookArgs;
use crate::view::prompt::PromptType;

/// Result of handling a prompt confirmation.
pub enum PromptResult {
    /// Prompt handled, continue normally
    Done,
    /// Prompt handled, should execute this action next
    ExecuteAction(Action),
    /// Prompt handled, should return early from handle_action
    EarlyReturn,
}

impl Editor {
    /// Handle prompt confirmation based on the prompt type.
    ///
    /// Returns a `PromptResult` indicating what the caller should do next.
    pub fn handle_prompt_confirm_input(
        &mut self,
        input: String,
        prompt_type: PromptType,
        selected_index: Option<usize>,
    ) -> PromptResult {
        match prompt_type {
            PromptType::OpenFile => {
                // Expand tilde to home directory first
                let expanded_path = expand_tilde(&input);
                let resolved_path = if expanded_path.is_absolute() {
                    normalize_path(&expanded_path)
                } else {
                    normalize_path(&self.working_dir.join(&expanded_path))
                };

                if let Err(e) = self.open_file(&resolved_path) {
                    self.set_status_message(
                        t!("file.error_opening", error = e.to_string()).to_string(),
                    );
                } else {
                    self.set_status_message(
                        t!("buffer.opened", name = resolved_path.display().to_string()).to_string(),
                    );
                }
            }
            PromptType::OpenFileWithEncoding { path } => {
                self.handle_open_file_with_encoding(&path, &input);
            }
            PromptType::ReloadWithEncoding => {
                self.handle_reload_with_encoding(&input);
            }
            PromptType::SwitchProject => {
                // Expand tilde to home directory first
                let expanded_path = expand_tilde(&input);
                let resolved_path = if expanded_path.is_absolute() {
                    normalize_path(&expanded_path)
                } else {
                    normalize_path(&self.working_dir.join(&expanded_path))
                };

                if resolved_path.is_dir() {
                    self.change_working_dir(resolved_path);
                } else {
                    self.set_status_message(
                        t!(
                            "file.not_directory",
                            path = resolved_path.display().to_string()
                        )
                        .to_string(),
                    );
                }
            }
            PromptType::SaveFileAs => {
                self.handle_save_file_as(&input);
            }
            PromptType::Search => {
                self.perform_search(&input);
            }
            PromptType::ReplaceSearch => {
                self.perform_search(&input);
                self.start_prompt(
                    t!("replace.prompt", search = &input).to_string(),
                    PromptType::Replace {
                        search: input.clone(),
                    },
                );
            }
            PromptType::Replace { search } => {
                if self.search_confirm_each {
                    self.start_interactive_replace(&search, &input);
                } else {
                    self.perform_replace(&search, &input);
                }
            }
            PromptType::QueryReplaceSearch => {
                self.perform_search(&input);
                self.start_prompt(
                    t!("replace.query_prompt", search = &input).to_string(),
                    PromptType::QueryReplace {
                        search: input.clone(),
                    },
                );
            }
            PromptType::QueryReplace { search } => {
                if self.search_confirm_each {
                    self.start_interactive_replace(&search, &input);
                } else {
                    self.perform_replace(&search, &input);
                }
            }
            PromptType::Command => {
                let commands = self.command_registry.read().unwrap().get_all();
                if let Some(cmd) = commands.iter().find(|c| c.get_localized_name() == input) {
                    let action = cmd.action.clone();
                    let cmd_name = cmd.get_localized_name();
                    self.command_registry
                        .write()
                        .unwrap()
                        .record_usage(&cmd_name);
                    return PromptResult::ExecuteAction(action);
                } else {
                    self.set_status_message(
                        t!("error.unknown_command", input = &input).to_string(),
                    );
                }
            }
            PromptType::GotoLine => match input.trim().parse::<usize>() {
                Ok(line_num) if line_num > 0 => {
                    self.goto_line_col(line_num, None);
                    self.set_status_message(t!("goto.jumped", line = line_num).to_string());
                }
                Ok(_) => {
                    self.set_status_message(t!("goto.line_must_be_positive").to_string());
                }
                Err(_) => {
                    self.set_status_message(t!("error.invalid_line", input = &input).to_string());
                }
            },
            PromptType::GotoByteOffset => {
                // Parse byte offset — strip optional trailing 'B' or 'b' suffix
                let trimmed = input.trim();
                let num_str = trimmed
                    .strip_suffix('B')
                    .or_else(|| trimmed.strip_suffix('b'))
                    .unwrap_or(trimmed);
                match num_str.parse::<usize>() {
                    Ok(offset) => {
                        self.goto_byte_offset(offset);
                        self.set_status_message(
                            t!("goto.jumped_byte", offset = offset).to_string(),
                        );
                    }
                    Err(_) => {
                        self.set_status_message(
                            t!("goto.invalid_byte_offset", input = &input).to_string(),
                        );
                    }
                }
            }
            PromptType::GotoLineScanConfirm => {
                let answer = input.trim().to_lowercase();
                if answer == "y" || answer == "yes" {
                    // Start incremental scan (non-blocking, updates progress in status bar)
                    self.start_incremental_line_scan(true);
                    // The GotoLine prompt will be opened when the scan completes
                    // (in process_line_scan)
                } else {
                    // No scan — open byte offset prompt (exact byte navigation)
                    self.start_prompt(
                        t!("goto.byte_offset_prompt").to_string(),
                        PromptType::GotoByteOffset,
                    );
                }
            }
            PromptType::QuickOpen => {
                // Handle Quick Open confirmation based on prefix
                return self.handle_quick_open_confirm(&input, selected_index);
            }
            PromptType::SetBackgroundFile => {
                if let Err(e) = self.load_ansi_background(&input) {
                    self.set_status_message(
                        t!("error.background_load_failed", error = e.to_string()).to_string(),
                    );
                }
            }
            PromptType::SetBackgroundBlend => match input.trim().parse::<f32>() {
                Ok(val) => {
                    let clamped = val.clamp(0.0, 1.0);
                    self.background_fade = clamped;
                    self.set_status_message(
                        t!(
                            "error.background_blend_set",
                            value = format!("{:.2}", clamped)
                        )
                        .to_string(),
                    );
                }
                Err(_) => {
                    self.set_status_message(t!("error.invalid_blend", input = &input).to_string());
                }
            },
            PromptType::SetComposeWidth => {
                self.handle_set_compose_width(&input);
            }
            PromptType::RecordMacro => {
                self.handle_register_input(
                    &input,
                    |editor, c| editor.toggle_macro_recording(c),
                    "Macro",
                );
            }
            PromptType::PlayMacro => {
                self.handle_register_input(&input, |editor, c| editor.play_macro(c), "Macro");
            }
            PromptType::SetBookmark => {
                self.handle_register_input(&input, |editor, c| editor.set_bookmark(c), "Bookmark");
            }
            PromptType::JumpToBookmark => {
                self.handle_register_input(
                    &input,
                    |editor, c| editor.jump_to_bookmark(c),
                    "Bookmark",
                );
            }
            PromptType::Plugin { custom_type } => {
                tracing::info!(
                    "prompt_confirmed: dispatching hook for prompt_type='{}', input='{}', selected_index={:?}",
                    custom_type, input, selected_index
                );
                self.plugin_manager.run_hook(
                    "prompt_confirmed",
                    HookArgs::PromptConfirmed {
                        prompt_type: custom_type.clone(),
                        input,
                        selected_index,
                    },
                );
                tracing::info!(
                    "prompt_confirmed: hook dispatched for prompt_type='{}'",
                    custom_type
                );
            }
            PromptType::ConfirmRevert => {
                let input_lower = input.trim().to_lowercase();
                let revert_key = t!("prompt.key.revert").to_string().to_lowercase();
                if input_lower == revert_key || input_lower == "revert" {
                    if let Err(e) = self.revert_file() {
                        self.set_status_message(
                            t!("file.revert_failed", error = e.to_string()).to_string(),
                        );
                    }
                } else {
                    self.set_status_message(t!("buffer.revert_cancelled").to_string());
                }
            }
            PromptType::ConfirmSaveConflict => {
                let input_lower = input.trim().to_lowercase();
                if input_lower == "o" || input_lower == "overwrite" {
                    if let Err(e) = self.save() {
                        self.set_status_message(
                            t!("file.save_failed", error = e.to_string()).to_string(),
                        );
                    }
                } else {
                    self.set_status_message(t!("buffer.save_cancelled").to_string());
                }
            }
            PromptType::ConfirmSudoSave { info } => {
                let input_lower = input.trim().to_lowercase();
                if input_lower == "y" || input_lower == "yes" {
                    // Hide prompt before starting blocking command to clear the line
                    self.cancel_prompt();

                    // Read temp file and write via sudo (works for both local and remote)
                    let result = (|| -> anyhow::Result<()> {
                        let data = self.filesystem.read_file(&info.temp_path)?;
                        self.filesystem.sudo_write(
                            &info.dest_path,
                            &data,
                            info.mode,
                            info.uid,
                            info.gid,
                        )?;
                        // Best-effort cleanup of temp file.
                        #[allow(clippy::let_underscore_must_use)]
                        let _ = self.filesystem.remove_file(&info.temp_path);
                        Ok(())
                    })();

                    match result {
                        Ok(_) => {
                            if let Err(e) = self
                                .active_state_mut()
                                .buffer
                                .finalize_external_save(info.dest_path.clone())
                            {
                                tracing::warn!("Failed to finalize sudo save: {}", e);
                                self.set_status_message(
                                    t!("prompt.sudo_save_failed", error = e.to_string())
                                        .to_string(),
                                );
                            } else if let Err(e) = self.finalize_save(Some(info.dest_path)) {
                                tracing::warn!("Failed to finalize save after sudo: {}", e);
                                self.set_status_message(
                                    t!("prompt.sudo_save_failed", error = e.to_string())
                                        .to_string(),
                                );
                            }
                        }
                        Err(e) => {
                            tracing::warn!("Sudo save failed: {}", e);
                            self.set_status_message(
                                t!("prompt.sudo_save_failed", error = e.to_string()).to_string(),
                            );
                            // Best-effort cleanup of temp file.
                            #[allow(clippy::let_underscore_must_use)]
                            let _ = self.filesystem.remove_file(&info.temp_path);
                        }
                    }
                } else {
                    self.set_status_message(t!("buffer.save_cancelled").to_string());
                    // Best-effort cleanup of temp file.
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = self.filesystem.remove_file(&info.temp_path);
                }
            }
            PromptType::ConfirmOverwriteFile { path } => {
                let input_lower = input.trim().to_lowercase();
                if input_lower == "o" || input_lower == "overwrite" {
                    self.perform_save_file_as(path);
                } else {
                    self.set_status_message(t!("buffer.save_cancelled").to_string());
                }
            }
            PromptType::ConfirmCloseBuffer { buffer_id } => {
                if self.handle_confirm_close_buffer(&input, buffer_id) {
                    return PromptResult::EarlyReturn;
                }
            }
            PromptType::ConfirmQuitWithModified => {
                let input_lower = input.trim().to_lowercase();
                let discard_key = t!("prompt.key.discard").to_string().to_lowercase();
                if input_lower == discard_key || input_lower == "discard" {
                    self.should_quit = true;
                } else {
                    self.set_status_message(t!("buffer.close_cancelled").to_string());
                }
            }
            PromptType::LspRename {
                original_text,
                start_pos,
                end_pos: _,
                overlay_handle,
            } => {
                self.perform_lsp_rename(input, original_text, start_pos, overlay_handle);
            }
            PromptType::FileExplorerRename {
                original_path,
                original_name,
                is_new_file,
            } => {
                self.perform_file_explorer_rename(original_path, original_name, input, is_new_file);
            }
            PromptType::ConfirmDeleteFile { path, is_dir } => {
                let input_lower = input.trim().to_lowercase();
                if input_lower == "y" || input_lower == "yes" {
                    self.perform_file_explorer_delete(path, is_dir);
                } else {
                    self.set_status_message(t!("explorer.delete_cancelled").to_string());
                }
            }
            PromptType::ConfirmLargeFileEncoding { path } => {
                let input_lower = input.trim().to_lowercase();
                let load_key = t!("file.large_encoding.key.load")
                    .to_string()
                    .to_lowercase();
                let encoding_key = t!("file.large_encoding.key.encoding")
                    .to_string()
                    .to_lowercase();
                let cancel_key = t!("file.large_encoding.key.cancel")
                    .to_string()
                    .to_lowercase();
                // Default (empty input or load key) loads the file
                if input_lower.is_empty() || input_lower == load_key {
                    if let Err(e) = self.open_file_large_encoding_confirmed(&path) {
                        self.set_status_message(
                            t!("file.error_opening", error = e.to_string()).to_string(),
                        );
                    }
                } else if input_lower == encoding_key {
                    // Let user pick a different encoding
                    self.start_open_file_with_encoding_prompt(path);
                } else if input_lower == cancel_key {
                    self.set_status_message(t!("file.open_cancelled").to_string());
                } else {
                    // Unknown input - default to load
                    if let Err(e) = self.open_file_large_encoding_confirmed(&path) {
                        self.set_status_message(
                            t!("file.error_opening", error = e.to_string()).to_string(),
                        );
                    }
                }
            }
            PromptType::StopLspServer => {
                self.handle_stop_lsp_server(&input);
            }
            PromptType::SelectTheme { .. } => {
                self.apply_theme(input.trim());
            }
            PromptType::SelectKeybindingMap => {
                self.apply_keybinding_map(input.trim());
            }
            PromptType::SelectCursorStyle => {
                self.apply_cursor_style(input.trim());
            }
            PromptType::SelectLocale => {
                self.apply_locale(input.trim());
            }
            PromptType::CopyWithFormattingTheme => {
                self.copy_selection_with_theme(input.trim());
            }
            PromptType::SwitchToTab => {
                if let Ok(id) = input.trim().parse::<usize>() {
                    self.switch_to_tab(BufferId(id));
                }
            }
            PromptType::QueryReplaceConfirm => {
                // This is handled by InsertChar, not PromptConfirm
                // But if somehow Enter is pressed, treat it as skip (n)
                if let Some(c) = input.chars().next() {
                    if let Err(e) = self.handle_interactive_replace_key(c) {
                        tracing::warn!("Interactive replace failed: {}", e);
                    }
                }
            }
            PromptType::AddRuler => {
                self.handle_add_ruler(&input);
            }
            PromptType::RemoveRuler => {
                self.handle_remove_ruler(&input);
            }
            PromptType::SetTabSize => {
                self.handle_set_tab_size(&input);
            }
            PromptType::SetLineEnding => {
                self.handle_set_line_ending(&input);
            }
            PromptType::SetEncoding => {
                self.handle_set_encoding(&input);
            }
            PromptType::SetLanguage => {
                self.handle_set_language(&input);
            }
            PromptType::ShellCommand { replace } => {
                self.handle_shell_command(&input, replace);
            }
            PromptType::AsyncPrompt => {
                // Resolve the pending async prompt callback with the input text
                if let Some(callback_id) = self.pending_async_prompt_callback.take() {
                    // Serialize the input as a JSON string
                    let json = serde_json::to_string(&input).unwrap_or_else(|_| "null".to_string());
                    self.plugin_manager.resolve_callback(callback_id, json);
                }
            }
        }
        PromptResult::Done
    }

    /// Handle SaveFileAs prompt confirmation.
    fn handle_save_file_as(&mut self, input: &str) {
        // Expand tilde to home directory first
        let expanded_path = expand_tilde(input);
        let full_path = if expanded_path.is_absolute() {
            normalize_path(&expanded_path)
        } else {
            normalize_path(&self.working_dir.join(&expanded_path))
        };

        // Check if we're saving to a different file that already exists
        let current_file_path = self
            .active_state()
            .buffer
            .file_path()
            .map(|p| p.to_path_buf());
        let is_different_file = current_file_path.as_ref() != Some(&full_path);

        if is_different_file && full_path.is_file() {
            // File exists and is different from current - ask for confirmation
            let filename = full_path
                .file_name()
                .map(|n| n.to_string_lossy().to_string())
                .unwrap_or_else(|| full_path.display().to_string());
            self.start_prompt(
                t!("buffer.overwrite_confirm", name = &filename).to_string(),
                PromptType::ConfirmOverwriteFile { path: full_path },
            );
            return;
        }

        // Proceed with save
        self.perform_save_file_as(full_path);
    }

    /// Perform the actual SaveFileAs operation (called after confirmation if needed).
    pub(crate) fn perform_save_file_as(&mut self, full_path: std::path::PathBuf) {
        let before_idx = self.active_event_log().current_index();
        let before_len = self.active_event_log().len();
        tracing::debug!(
            "SaveFileAs BEFORE: event_log index={}, len={}",
            before_idx,
            before_len
        );

        match self.active_state_mut().buffer.save_to_file(&full_path) {
            Ok(()) => {
                let after_save_idx = self.active_event_log().current_index();
                let after_save_len = self.active_event_log().len();
                tracing::debug!(
                    "SaveFileAs AFTER buffer.save_to_file: event_log index={}, len={}",
                    after_save_idx,
                    after_save_len
                );

                let metadata = BufferMetadata::with_file(full_path.clone(), &self.working_dir);
                self.buffer_metadata.insert(self.active_buffer(), metadata);

                // Auto-detect language if it's currently "text"
                // This ensures syntax highlighting works immediately after "Save As"
                let mut language_changed = false;
                let mut new_language = String::new();
                if let Some(state) = self.buffers.get_mut(&self.active_buffer()) {
                    if state.language == "text" {
                        let detected =
                            crate::primitives::detected_language::DetectedLanguage::from_path(
                                &full_path,
                                &self.grammar_registry,
                                &self.config.languages,
                            );
                        new_language = detected.name.clone();
                        state.apply_language(detected);
                        language_changed = new_language != "text";
                    }
                }
                if language_changed {
                    #[cfg(feature = "plugins")]
                    self.update_plugin_state_snapshot();
                    self.plugin_manager.run_hook(
                        "language_changed",
                        crate::services::plugins::hooks::HookArgs::LanguageChanged {
                            buffer_id: self.active_buffer(),
                            language: new_language,
                        },
                    );
                }

                self.active_event_log_mut().mark_saved();
                tracing::debug!(
                    "SaveFileAs AFTER mark_saved: event_log index={}, len={}",
                    self.active_event_log().current_index(),
                    self.active_event_log().len()
                );

                if let Ok(metadata) = self.filesystem.metadata(&full_path) {
                    if let Some(mtime) = metadata.modified {
                        self.file_mod_times.insert(full_path.clone(), mtime);
                    }
                }

                self.notify_lsp_save();

                self.emit_event(
                    crate::model::control_event::events::FILE_SAVED.name,
                    serde_json::json!({"path": full_path.display().to_string()}),
                );

                self.plugin_manager.run_hook(
                    "after_file_save",
                    crate::services::plugins::hooks::HookArgs::AfterFileSave {
                        buffer_id: self.active_buffer(),
                        path: full_path.clone(),
                    },
                );

                if let Some(buffer_to_close) = self.pending_close_buffer.take() {
                    if let Err(e) = self.force_close_buffer(buffer_to_close) {
                        self.set_status_message(
                            t!("file.saved_cannot_close", error = e.to_string()).to_string(),
                        );
                    } else {
                        self.set_status_message(t!("buffer.saved_and_closed").to_string());
                    }
                } else {
                    self.set_status_message(
                        t!("file.saved_as", path = full_path.display().to_string()).to_string(),
                    );
                }
            }
            Err(e) => {
                self.pending_close_buffer = None;
                self.set_status_message(t!("file.error_saving", error = e.to_string()).to_string());
            }
        }
    }

    /// Handle SetComposeWidth prompt confirmation.
    fn handle_set_compose_width(&mut self, input: &str) {
        let active_split = self.split_manager.active_split();
        let trimmed = input.trim();

        if trimmed.is_empty() {
            if let Some(vs) = self.split_view_states.get_mut(&active_split) {
                vs.compose_width = None;
            }
            self.set_status_message(t!("settings.compose_width_cleared").to_string());
        } else {
            match trimmed.parse::<u16>() {
                Ok(val) if val > 0 => {
                    if let Some(vs) = self.split_view_states.get_mut(&active_split) {
                        vs.compose_width = Some(val);
                    }
                    self.set_status_message(
                        t!("settings.compose_width_set", value = val).to_string(),
                    );
                }
                _ => {
                    self.set_status_message(
                        t!("error.invalid_compose_width", input = input).to_string(),
                    );
                }
            }
        }
    }

    /// Handle AddRuler prompt confirmation.
    fn handle_add_ruler(&mut self, input: &str) {
        let trimmed = input.trim();
        match trimmed.parse::<usize>() {
            Ok(col) if col > 0 => {
                let active_split = self.split_manager.active_split();
                if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
                    if !view_state.rulers.contains(&col) {
                        view_state.rulers.push(col);
                        view_state.rulers.sort();
                    }
                }
                // Persist to user config
                self.config.editor.rulers = self
                    .split_view_states
                    .get(&active_split)
                    .map(|vs| vs.rulers.clone())
                    .unwrap_or_default();
                self.save_rulers_to_config();
                self.set_status_message(t!("rulers.added", column = col).to_string());
            }
            Ok(_) => {
                self.set_status_message(t!("rulers.must_be_positive").to_string());
            }
            Err(_) => {
                self.set_status_message(t!("rulers.invalid_column", input = input).to_string());
            }
        }
    }

    /// Handle RemoveRuler prompt confirmation.
    fn handle_remove_ruler(&mut self, input: &str) {
        let trimmed = input.trim();
        if let Ok(col) = trimmed.parse::<usize>() {
            let active_split = self.split_manager.active_split();
            if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
                view_state.rulers.retain(|&r| r != col);
            }
            // Persist to user config
            self.config.editor.rulers = self
                .split_view_states
                .get(&active_split)
                .map(|vs| vs.rulers.clone())
                .unwrap_or_default();
            self.save_rulers_to_config();
            self.set_status_message(t!("rulers.removed", column = col).to_string());
        }
    }

    /// Save the current rulers setting to the user's config file
    fn save_rulers_to_config(&mut self) {
        if let Err(e) = self.filesystem.create_dir_all(&self.dir_context.config_dir) {
            tracing::warn!("Failed to create config directory: {}", e);
            return;
        }
        let resolver = ConfigResolver::new(self.dir_context.clone(), self.working_dir.clone());
        if let Err(e) = resolver.save_to_layer(&self.config, ConfigLayer::User) {
            tracing::warn!("Failed to save rulers to config: {}", e);
        }
    }

    /// Handle SetTabSize prompt confirmation.
    fn handle_set_tab_size(&mut self, input: &str) {
        let buffer_id = self.active_buffer();
        let trimmed = input.trim();

        match trimmed.parse::<usize>() {
            Ok(val) if val > 0 => {
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    state.buffer_settings.tab_size = val;
                }
                self.set_status_message(t!("settings.tab_size_set", value = val).to_string());
            }
            Ok(_) => {
                self.set_status_message(t!("settings.tab_size_positive").to_string());
            }
            Err(_) => {
                self.set_status_message(t!("error.invalid_tab_size", input = input).to_string());
            }
        }
    }

    /// Handle SetLineEnding prompt confirmation.
    fn handle_set_line_ending(&mut self, input: &str) {
        use crate::model::buffer::LineEnding;

        // Extract the line ending code from the input (e.g., "LF" from "LF (Unix/Linux/Mac)")
        let trimmed = input.trim();
        let code = trimmed.split_whitespace().next().unwrap_or(trimmed);

        let line_ending = match code.to_uppercase().as_str() {
            "LF" => Some(LineEnding::LF),
            "CRLF" => Some(LineEnding::CRLF),
            "CR" => Some(LineEnding::CR),
            _ => None,
        };

        match line_ending {
            Some(le) => {
                self.active_state_mut().buffer.set_line_ending(le);
                self.set_status_message(
                    t!("settings.line_ending_set", value = le.display_name()).to_string(),
                );
            }
            None => {
                self.set_status_message(t!("error.unknown_line_ending", input = input).to_string());
            }
        }
    }

    /// Handle SetEncoding prompt confirmation.
    fn handle_set_encoding(&mut self, input: &str) {
        use crate::model::buffer::Encoding;

        let trimmed = input.trim();

        // First try to match the full input against encoding display names
        // This handles multi-word names like "UTF-16 LE" and "UTF-8 BOM"
        let encoding = Encoding::all()
            .iter()
            .find(|enc| enc.display_name().eq_ignore_ascii_case(trimmed))
            .copied()
            .or_else(|| {
                // If no match, try extracting before the parenthesis (e.g., "UTF-8" from "UTF-8 (Unicode)")
                let before_paren = trimmed.split('(').next().unwrap_or(trimmed).trim();
                Encoding::all()
                    .iter()
                    .find(|enc| enc.display_name().eq_ignore_ascii_case(before_paren))
                    .copied()
            });

        match encoding {
            Some(enc) => {
                self.active_state_mut().buffer.set_encoding(enc);
                self.set_status_message(format!("Encoding set to {}", enc.display_name()));
            }
            None => {
                self.set_status_message(format!("Unknown encoding: {}", input));
            }
        }
    }

    /// Handle OpenFileWithEncoding prompt confirmation.
    /// Opens a file with a specific encoding (no auto-detection).
    ///
    /// For large files with non-resynchronizable encodings, shows a confirmation prompt
    /// before loading the entire file into memory.
    fn handle_open_file_with_encoding(&mut self, path: &std::path::Path, input: &str) {
        use crate::model::buffer::Encoding;
        use crate::view::prompt::PromptType;

        let trimmed = input.trim();

        // Parse the encoding from input
        let encoding = Encoding::all()
            .iter()
            .find(|enc| enc.display_name().eq_ignore_ascii_case(trimmed))
            .copied()
            .or_else(|| {
                let before_paren = trimmed.split('(').next().unwrap_or(trimmed).trim();
                Encoding::all()
                    .iter()
                    .find(|enc| enc.display_name().eq_ignore_ascii_case(before_paren))
                    .copied()
            });

        match encoding {
            Some(enc) => {
                // Check if this is a large file with non-resynchronizable encoding
                // If so, show confirmation prompt before loading
                let threshold = self.config.editor.large_file_threshold_bytes as usize;
                let file_size = self
                    .filesystem
                    .metadata(path)
                    .map(|m| m.size as usize)
                    .unwrap_or(0);

                if file_size >= threshold && enc.requires_full_file_load() {
                    // Show confirmation prompt for large file with non-resynchronizable encoding
                    let size_mb = file_size as f64 / (1024.0 * 1024.0);
                    let load_key = t!("file.large_encoding.key.load").to_string();
                    let encoding_key = t!("file.large_encoding.key.encoding").to_string();
                    let cancel_key = t!("file.large_encoding.key.cancel").to_string();
                    let prompt_msg = t!(
                        "file.large_encoding_prompt",
                        encoding = enc.display_name(),
                        size = format!("{:.0}", size_mb),
                        load_key = load_key,
                        encoding_key = encoding_key,
                        cancel_key = cancel_key
                    )
                    .to_string();
                    self.start_prompt(
                        prompt_msg,
                        PromptType::ConfirmLargeFileEncoding {
                            path: path.to_path_buf(),
                        },
                    );
                    return;
                }

                // Reset key context to Normal so editor gets focus
                self.key_context = crate::input::keybindings::KeyContext::Normal;

                // Open the file with the specified encoding
                if let Err(e) = self.open_file_with_encoding(path, enc) {
                    self.set_status_message(
                        t!("file.error_opening", error = e.to_string()).to_string(),
                    );
                } else {
                    self.set_status_message(format!(
                        "Opened {} with {} encoding",
                        path.display(),
                        enc.display_name()
                    ));
                }
            }
            None => {
                self.set_status_message(format!("Unknown encoding: {}", input));
            }
        }
    }

    /// Handle ReloadWithEncoding prompt confirmation.
    /// Reloads the current file with a specific encoding.
    fn handle_reload_with_encoding(&mut self, input: &str) {
        use crate::model::buffer::Encoding;

        let trimmed = input.trim();

        // Parse the encoding from input
        let encoding = Encoding::all()
            .iter()
            .find(|enc| enc.display_name().eq_ignore_ascii_case(trimmed))
            .copied()
            .or_else(|| {
                let before_paren = trimmed.split('(').next().unwrap_or(trimmed).trim();
                Encoding::all()
                    .iter()
                    .find(|enc| enc.display_name().eq_ignore_ascii_case(before_paren))
                    .copied()
            });

        match encoding {
            Some(enc) => {
                // Reload the file with the specified encoding
                if let Err(e) = self.reload_with_encoding(enc) {
                    self.set_status_message(format!("Failed to reload: {}", e));
                } else {
                    self.set_status_message(format!(
                        "Reloaded with {} encoding",
                        enc.display_name()
                    ));
                }
            }
            None => {
                self.set_status_message(format!("Unknown encoding: {}", input));
            }
        }
    }

    /// Resolve a syntect syntax name to the canonical config language ID.
    ///
    /// Resolve a syntect syntax display name to its canonical config language ID.
    /// Delegates to the free function in `detected_language`.
    pub(crate) fn resolve_language_id(&self, syntax_name: &str) -> Option<String> {
        crate::primitives::detected_language::resolve_language_id(
            syntax_name,
            &self.grammar_registry,
            &self.config.languages,
        )
    }

    /// Handle SetLanguage prompt confirmation.
    fn handle_set_language(&mut self, input: &str) {
        use crate::primitives::detected_language::DetectedLanguage;

        let trimmed = input.trim();

        // Check for "Plain Text" (no highlighting)
        if trimmed == "Plain Text" || trimmed.to_lowercase() == "text" {
            let buffer_id = self.active_buffer();
            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                state.apply_language(DetectedLanguage::plain_text());
                self.set_status_message("Language set to Plain Text".to_string());
            }
            #[cfg(feature = "plugins")]
            self.update_plugin_state_snapshot();
            self.plugin_manager.run_hook(
                "language_changed",
                crate::services::plugins::hooks::HookArgs::LanguageChanged {
                    buffer_id: self.active_buffer(),
                    language: "text".to_string(),
                },
            );
            return;
        }

        // Try to find the syntax by name and resolve canonical language ID from config
        if let Some(detected) = DetectedLanguage::from_syntax_name(
            trimmed,
            &self.grammar_registry,
            &self.config.languages,
        ) {
            let language = detected.name.clone();
            let buffer_id = self.active_buffer();
            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                state.apply_language(detected);
                self.set_status_message(format!("Language set to {}", trimmed));
            }
            #[cfg(feature = "plugins")]
            self.update_plugin_state_snapshot();
            self.plugin_manager.run_hook(
                "language_changed",
                crate::services::plugins::hooks::HookArgs::LanguageChanged {
                    buffer_id,
                    language,
                },
            );
        } else {
            self.set_status_message(format!("Unknown language: {}", input));
        }
    }

    /// Handle register-based input (macros, bookmarks).
    fn handle_register_input<F>(&mut self, input: &str, action: F, register_type: &str)
    where
        F: FnOnce(&mut Self, char),
    {
        if let Some(c) = input.trim().chars().next() {
            if c.is_ascii_digit() {
                action(self, c);
            } else {
                self.set_status_message(
                    t!("register.must_be_digit", "type" = register_type).to_string(),
                );
            }
        } else {
            self.set_status_message(t!("register.not_specified").to_string());
        }
    }

    /// Handle ConfirmCloseBuffer prompt. Returns true if early return is needed.
    fn handle_confirm_close_buffer(&mut self, input: &str, buffer_id: BufferId) -> bool {
        let input_lower = input.trim().to_lowercase();
        let save_key = t!("prompt.key.save").to_string().to_lowercase();
        let discard_key = t!("prompt.key.discard").to_string().to_lowercase();

        let first_char = input_lower.chars().next();
        let save_first = save_key.chars().next();
        let discard_first = discard_key.chars().next();

        if first_char == save_first {
            // Save and close
            let has_path = self
                .buffers
                .get(&buffer_id)
                .map(|s| s.buffer.file_path().is_some())
                .unwrap_or(false);

            if has_path {
                let old_active = self.active_buffer();
                self.set_active_buffer(buffer_id);
                if let Err(e) = self.save() {
                    self.set_status_message(
                        t!("file.save_failed", error = e.to_string()).to_string(),
                    );
                    self.set_active_buffer(old_active);
                    return true; // Early return
                }
                self.set_active_buffer(old_active);
                if let Err(e) = self.force_close_buffer(buffer_id) {
                    self.set_status_message(
                        t!("file.cannot_close", error = e.to_string()).to_string(),
                    );
                } else {
                    self.set_status_message(t!("buffer.saved_and_closed").to_string());
                }
            } else {
                self.pending_close_buffer = Some(buffer_id);
                self.start_prompt_with_initial_text(
                    t!("file.save_as_prompt").to_string(),
                    PromptType::SaveFileAs,
                    String::new(),
                );
            }
        } else if first_char == discard_first {
            // Discard and close
            if let Err(e) = self.force_close_buffer(buffer_id) {
                self.set_status_message(t!("file.cannot_close", error = e.to_string()).to_string());
            } else {
                self.set_status_message(t!("buffer.changes_discarded").to_string());
            }
        } else {
            self.set_status_message(t!("buffer.close_cancelled").to_string());
        }
        false
    }

    /// Handle StopLspServer prompt confirmation.
    fn handle_stop_lsp_server(&mut self, input: &str) {
        let language = input.trim();
        if language.is_empty() {
            return;
        }

        if let Some(lsp) = &mut self.lsp {
            if lsp.shutdown_server(language) {
                if let Some(lsp_config) = self.config.lsp.get_mut(language) {
                    lsp_config.auto_start = false;
                    if let Err(e) = self.save_config() {
                        tracing::warn!(
                            "Failed to save config after disabling LSP auto-start: {}",
                            e
                        );
                    } else {
                        let config_path = self.dir_context.config_path();
                        self.emit_event(
                            "config_changed",
                            serde_json::json!({
                                "path": config_path.to_string_lossy(),
                            }),
                        );
                    }
                }
                self.set_status_message(t!("lsp.server_stopped", language = language).to_string());
            } else {
                self.set_status_message(
                    t!("lsp.server_not_found", language = language).to_string(),
                );
            }
        }
    }

    /// Handle Quick Open prompt confirmation based on prefix routing
    fn handle_quick_open_confirm(
        &mut self,
        input: &str,
        selected_index: Option<usize>,
    ) -> PromptResult {
        // Determine the mode based on prefix
        if input.starts_with('>') {
            // Command mode - find and execute the selected command
            let query = &input[1..];
            return self.handle_quick_open_command(query, selected_index);
        }

        if input.starts_with('#') {
            // Buffer mode - switch to selected buffer
            let query = &input[1..];
            return self.handle_quick_open_buffer(query, selected_index);
        }

        if input.starts_with(':') {
            // Go to line mode
            let line_str = &input[1..];
            if let Ok(line_num) = line_str.parse::<usize>() {
                if line_num > 0 {
                    self.goto_line_col(line_num, None);
                    self.set_status_message(t!("goto.jumped", line = line_num).to_string());
                } else {
                    self.set_status_message(t!("goto.line_must_be_positive").to_string());
                }
            } else {
                self.set_status_message(t!("error.invalid_line", input = line_str).to_string());
            }
            return PromptResult::Done;
        }

        // Default: file mode - open the selected file
        self.handle_quick_open_file(input, selected_index)
    }

    /// Handle Quick Open command selection
    fn handle_quick_open_command(
        &mut self,
        query: &str,
        selected_index: Option<usize>,
    ) -> PromptResult {
        let suggestions = {
            let registry = self.command_registry.read().unwrap();
            let selection_active = self.has_active_selection();
            let active_buffer_mode = self
                .buffer_metadata
                .get(&self.active_buffer())
                .and_then(|m| m.virtual_mode());

            registry.filter(
                query,
                self.key_context,
                &self.keybindings,
                selection_active,
                &self.active_custom_contexts,
                active_buffer_mode,
            )
        };

        if let Some(idx) = selected_index {
            if let Some(suggestion) = suggestions.get(idx) {
                if suggestion.disabled {
                    self.set_status_message(t!("status.command_not_available").to_string());
                    return PromptResult::Done;
                }

                // Find and execute the command
                let commands = self.command_registry.read().unwrap().get_all();
                if let Some(cmd) = commands
                    .iter()
                    .find(|c| c.get_localized_name() == suggestion.text)
                {
                    let action = cmd.action.clone();
                    let cmd_name = cmd.get_localized_name();
                    self.command_registry
                        .write()
                        .unwrap()
                        .record_usage(&cmd_name);
                    return PromptResult::ExecuteAction(action);
                }
            }
        }

        self.set_status_message(t!("status.no_selection").to_string());
        PromptResult::Done
    }

    /// Handle Quick Open buffer selection
    fn handle_quick_open_buffer(
        &mut self,
        query: &str,
        selected_index: Option<usize>,
    ) -> PromptResult {
        // Regenerate buffer suggestions since prompt was already taken by confirm_prompt
        let suggestions = self.get_buffer_suggestions(query);

        if let Some(idx) = selected_index {
            if let Some(suggestion) = suggestions.get(idx) {
                if let Some(value) = &suggestion.value {
                    if let Ok(buffer_id) = value.parse::<usize>() {
                        let buffer_id = crate::model::event::BufferId(buffer_id);
                        if self.buffers.contains_key(&buffer_id) {
                            self.set_active_buffer(buffer_id);
                            if let Some(name) = self.active_state().buffer.file_path() {
                                self.set_status_message(
                                    t!("buffer.switched", name = name.display().to_string())
                                        .to_string(),
                                );
                            }
                            return PromptResult::Done;
                        }
                    }
                }
            }
        }

        self.set_status_message(t!("status.no_selection").to_string());
        PromptResult::Done
    }

    /// Handle Quick Open file selection
    fn handle_quick_open_file(
        &mut self,
        input: &str,
        selected_index: Option<usize>,
    ) -> PromptResult {
        // Regenerate file suggestions since prompt was already taken by confirm_prompt
        let suggestions = self.get_file_suggestions(input);

        if let Some(idx) = selected_index {
            if let Some(suggestion) = suggestions.get(idx) {
                if let Some(path_str) = &suggestion.value {
                    let path = std::path::PathBuf::from(path_str);
                    let full_path = if path.is_absolute() {
                        path
                    } else {
                        self.working_dir.join(&path)
                    };

                    // Record file access for frecency
                    self.file_provider.record_access(path_str);

                    match self.open_file(&full_path) {
                        Ok(_) => {
                            self.set_status_message(
                                t!("buffer.opened", name = full_path.display().to_string())
                                    .to_string(),
                            );
                        }
                        Err(e) => {
                            // Check if this is a large file encoding confirmation error
                            if let Some(confirmation) = e.downcast_ref::<
                                crate::model::buffer::LargeFileEncodingConfirmation,
                            >() {
                                self.start_large_file_encoding_confirmation(confirmation);
                            } else {
                                self.set_status_message(
                                    t!("file.error_opening", error = e.to_string()).to_string(),
                                );
                            }
                        }
                    }
                    return PromptResult::Done;
                }
            }
        }

        self.set_status_message(t!("status.no_selection").to_string());
        PromptResult::Done
    }
}
