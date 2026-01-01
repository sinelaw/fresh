//! Prompt confirmation action handlers.
//!
//! This module contains handlers for different prompt types when the user confirms input.

use rust_i18n::t;

use super::normalize_path;
use super::BufferId;
use super::BufferMetadata;
use super::Editor;
use crate::input::keybindings::Action;
use crate::services::plugins::hooks::HookArgs;
use crate::view::prompt::PromptType;
use std::path::Path;

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
                let input_path = Path::new(&input);
                let resolved_path = if input_path.is_absolute() {
                    normalize_path(input_path)
                } else {
                    normalize_path(&self.working_dir.join(input_path))
                };

                if let Err(e) = self.open_file(&resolved_path) {
                    self.set_status_message(t!("file.error_opening", error = e.to_string()).to_string());
                } else {
                    self.set_status_message(t!("buffer.opened", name = resolved_path.display().to_string()).to_string());
                }
            }
            PromptType::SwitchProject => {
                let input_path = Path::new(&input);
                let resolved_path = if input_path.is_absolute() {
                    normalize_path(input_path)
                } else {
                    normalize_path(&self.working_dir.join(input_path))
                };

                if resolved_path.is_dir() {
                    self.change_working_dir(resolved_path);
                } else {
                    self.set_status_message(t!("file.not_directory", path = resolved_path.display().to_string()).to_string());
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
                if let Some(cmd) = commands.iter().find(|c| c.name == input) {
                    let action = cmd.action.clone();
                    let cmd_name = cmd.name.clone();
                    self.set_status_message(t!("error.executing", cmd = &cmd_name).to_string());
                    self.command_registry
                        .write()
                        .unwrap()
                        .record_usage(&cmd_name);
                    return PromptResult::ExecuteAction(action);
                } else {
                    self.set_status_message(t!("error.unknown_command", input = &input).to_string());
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
            PromptType::SetBackgroundFile => {
                if let Err(e) = self.load_ansi_background(&input) {
                    self.set_status_message(t!("error.background_load_failed", error = e.to_string()).to_string());
                }
            }
            PromptType::SetBackgroundBlend => match input.trim().parse::<f32>() {
                Ok(val) => {
                    let clamped = val.clamp(0.0, 1.0);
                    self.background_fade = clamped;
                    self.set_status_message(t!("error.background_blend_set", value = format!("{:.2}", clamped)).to_string());
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
                self.plugin_manager.run_hook(
                    "prompt_confirmed",
                    HookArgs::PromptConfirmed {
                        prompt_type: custom_type,
                        input,
                        selected_index,
                    },
                );
            }
            PromptType::ConfirmRevert => {
                let input_lower = input.trim().to_lowercase();
                let revert_key = t!("prompt.key.revert").to_string().to_lowercase();
                if input_lower == revert_key || input_lower == "revert" {
                    if let Err(e) = self.revert_file() {
                        self.set_status_message(t!("file.revert_failed", error = e.to_string()).to_string());
                    }
                } else {
                    self.set_status_message(t!("buffer.revert_cancelled").to_string());
                }
            }
            PromptType::ConfirmSaveConflict => {
                let input_lower = input.trim().to_lowercase();
                if input_lower == "o" || input_lower == "overwrite" {
                    if let Err(e) = self.save() {
                        self.set_status_message(t!("file.save_failed", error = e.to_string()).to_string());
                    }
                } else {
                    self.set_status_message(t!("buffer.save_cancelled").to_string());
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
            } => {
                self.perform_file_explorer_rename(original_path, original_name, input);
            }
            PromptType::ConfirmDeleteFile { path, is_dir } => {
                let input_lower = input.trim().to_lowercase();
                if input_lower == "y" || input_lower == "yes" {
                    self.perform_file_explorer_delete(path, is_dir);
                } else {
                    self.set_status_message(t!("explorer.delete_cancelled").to_string());
                }
            }
            PromptType::StopLspServer => {
                self.handle_stop_lsp_server(&input);
            }
            PromptType::SelectTheme => {
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
                    let _ = self.handle_interactive_replace_key(c);
                }
            }
            PromptType::SetTabSize => {
                self.handle_set_tab_size(&input);
            }
            PromptType::SetLineEnding => {
                self.handle_set_line_ending(&input);
            }
            PromptType::ShellCommand { replace } => {
                self.handle_shell_command(&input, replace);
            }
        }
        PromptResult::Done
    }

    /// Handle SaveFileAs prompt confirmation.
    fn handle_save_file_as(&mut self, input: &str) {
        let input_path = Path::new(input);
        let full_path = if input_path.is_absolute() {
            normalize_path(input_path)
        } else {
            normalize_path(&self.working_dir.join(input_path))
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
    fn perform_save_file_as(&mut self, full_path: std::path::PathBuf) {
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

                self.active_event_log_mut().mark_saved();
                tracing::debug!(
                    "SaveFileAs AFTER mark_saved: event_log index={}, len={}",
                    self.active_event_log().current_index(),
                    self.active_event_log().len()
                );

                if let Ok(metadata) = std::fs::metadata(&full_path) {
                    if let Ok(mtime) = metadata.modified() {
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
                        self.set_status_message(t!("file.saved_cannot_close", error = e.to_string()).to_string());
                    } else {
                        self.set_status_message(t!("buffer.saved_and_closed").to_string());
                    }
                } else {
                    self.set_status_message(t!("file.saved_as", path = full_path.display().to_string()).to_string());
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
        let buffer_id = self.active_buffer();
        let active_split = self.split_manager.active_split();
        let trimmed = input.trim();

        if trimmed.is_empty() {
            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                state.compose_width = None;
            }
            if let Some(vs) = self.split_view_states.get_mut(&active_split) {
                vs.compose_width = None;
            }
            self.set_status_message(t!("settings.compose_width_cleared").to_string());
        } else {
            match trimmed.parse::<u16>() {
                Ok(val) if val > 0 => {
                    if let Some(state) = self.buffers.get_mut(&buffer_id) {
                        state.compose_width = Some(val);
                    }
                    if let Some(vs) = self.split_view_states.get_mut(&active_split) {
                        vs.compose_width = Some(val);
                    }
                    self.set_status_message(t!("settings.compose_width_set", value = val).to_string());
                }
                _ => {
                    self.set_status_message(t!("error.invalid_compose_width", input = input).to_string());
                }
            }
        }
    }

    /// Handle SetTabSize prompt confirmation.
    fn handle_set_tab_size(&mut self, input: &str) {
        let buffer_id = self.active_buffer();
        let trimmed = input.trim();

        match trimmed.parse::<usize>() {
            Ok(val) if val > 0 => {
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    state.tab_size = val;
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
                self.set_status_message(t!("settings.line_ending_set", value = le.display_name()).to_string());
            }
            None => {
                self.set_status_message(t!("error.unknown_line_ending", input = input).to_string());
            }
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
                self.set_status_message(t!("register.must_be_digit", "type" = register_type).to_string());
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
                    self.set_status_message(t!("file.save_failed", error = e.to_string()).to_string());
                    self.set_active_buffer(old_active);
                    return true; // Early return
                }
                self.set_active_buffer(old_active);
                if let Err(e) = self.force_close_buffer(buffer_id) {
                    self.set_status_message(t!("file.cannot_close", error = e.to_string()).to_string());
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
                self.set_status_message(t!("lsp.server_not_found", language = language).to_string());
            }
        }
    }
}
