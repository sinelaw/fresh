use super::*;
use crate::hooks::HookArgs;
impl Editor {
    /// Determine the current keybinding context based on UI state
    pub(super) fn get_key_context(&self) -> crate::keybindings::KeyContext {
        use crate::keybindings::KeyContext;

        // Priority order: Menu > Prompt > Popup > Rename > Current context (FileExplorer or Normal)
        if self.menu_state.active_menu.is_some() {
            KeyContext::Menu
        } else if self.is_prompting() {
            KeyContext::Prompt
        } else if self.active_state().popups.is_visible() {
            KeyContext::Popup
        } else {
            // Use the current context (can be FileExplorer or Normal)
            self.key_context
        }
    }

    /// Handle a key event and return whether it was handled
    /// This is the central key handling logic used by both main.rs and tests
    pub fn handle_key(
        &mut self,
        code: crossterm::event::KeyCode,
        modifiers: crossterm::event::KeyModifiers,
    ) -> std::io::Result<()> {
        use crate::keybindings::Action;

        let _t_total = std::time::Instant::now();

        tracing::debug!(
            "Editor.handle_key: code={:?}, modifiers={:?}",
            code,
            modifiers
        );

        // Determine the current context first
        let mut context = self.get_key_context();

        // Special case: Hover and Signature Help popups should be dismissed on any key press
        if matches!(context, crate::keybindings::KeyContext::Popup) {
            // Check if the current popup is a hover or signature help popup (identified by title)
            let is_dismissable_popup = self
                .active_state()
                .popups
                .top()
                .and_then(|p| p.title.as_ref())
                .is_some_and(|title| title == "Hover" || title == "Signature Help");

            if is_dismissable_popup {
                // Dismiss the popup on any key press
                self.hide_popup();
                tracing::debug!("Dismissed hover/signature help popup on key press");
                // Recalculate context now that popup is gone
                context = self.get_key_context();
            }
        }

        // Only check buffer mode keybindings if we're not in a higher-priority context
        // (Menu, Prompt, Popup should take precedence over mode bindings)
        let should_check_mode_bindings = matches!(
            context,
            crate::keybindings::KeyContext::Normal | crate::keybindings::KeyContext::FileExplorer
        );

        if should_check_mode_bindings {
            // Check buffer mode keybindings (for virtual buffers with custom modes)
            if let Some(command_name) = self.resolve_mode_keybinding(code, modifiers) {
                tracing::debug!("Mode keybinding resolved to command: {}", command_name);
                // Execute the command via the command registry
                let commands = self.command_registry.read().unwrap().get_all();
                if let Some(cmd) = commands.iter().find(|c| c.name == command_name) {
                    let action = cmd.action.clone();
                    drop(commands);
                    return self.handle_action(action);
                } else if command_name == "close-buffer" {
                    // Handle built-in mode commands
                    let buffer_id = self.active_buffer;
                    return self.close_buffer(buffer_id);
                } else if command_name == "revert-buffer" {
                    // Refresh the buffer (for virtual buffers, this would re-query data)
                    self.set_status_message("Refreshing buffer...".to_string());
                    return Ok(());
                } else {
                    // Try as a plugin action
                    let action = Action::PluginAction(command_name.clone());
                    drop(commands);
                    return self.handle_action(action);
                }
            }
        }

        // Check for chord sequence matches first
        let key_event = crossterm::event::KeyEvent::new(code, modifiers);
        let chord_result = self.keybindings.resolve_chord(&self.chord_state, &key_event, context);

        match chord_result {
            crate::keybindings::ChordResolution::Complete(action) => {
                // Complete chord match - execute action and clear chord state
                tracing::debug!("Complete chord match -> Action: {:?}", action);
                self.chord_state.clear();
                return self.handle_action(action);
            }
            crate::keybindings::ChordResolution::Partial => {
                // Partial match - add to chord state and wait for more keys
                tracing::debug!("Partial chord match - waiting for next key");
                self.chord_state.push((code, modifiers));
                return Ok(());
            }
            crate::keybindings::ChordResolution::NoMatch => {
                // No chord match - clear state and try regular resolution
                if !self.chord_state.is_empty() {
                    tracing::debug!("Chord sequence abandoned, clearing state");
                    self.chord_state.clear();
                }
            }
        }

        // Regular single-key resolution
        let action = self.keybindings.resolve(&key_event, context);

        tracing::debug!("Context: {:?} -> Action: {:?}", context, action);

        // Cancel pending LSP requests on user actions (except LSP actions themselves)
        // This ensures stale completions don't show up after the user has moved on
        match action {
            Action::LspCompletion
            | Action::LspGotoDefinition
            | Action::LspReferences
            | Action::LspHover
            | Action::None => {
                // Don't cancel for LSP actions or no-op
            }
            _ => {
                // Cancel any pending LSP requests
                self.cancel_pending_lsp_requests();
            }
        }

        // Handle the action
        match action {
            // Prompt mode actions - delegate to handle_action
            Action::PromptConfirm => {
                return self.handle_action(action);
            }
            Action::PromptCancel => {
                self.cancel_prompt();
            }
            Action::PromptBackspace => {
                if let Some(prompt) = self.prompt_mut() {
                    // If there's a selection, delete it; otherwise delete one character backward
                    if prompt.has_selection() {
                        prompt.delete_selection();
                    } else if prompt.cursor_pos > 0 {
                        let byte_pos = prompt.cursor_pos;
                        let mut char_start = byte_pos - 1;
                        while char_start > 0 && !prompt.input.is_char_boundary(char_start) {
                            char_start -= 1;
                        }
                        prompt.input.remove(char_start);
                        prompt.cursor_pos = char_start;
                    }
                }
                self.update_prompt_suggestions();
            }
            Action::PromptDelete => {
                if let Some(prompt) = self.prompt_mut() {
                    // If there's a selection, delete it; otherwise delete one character forward
                    if prompt.has_selection() {
                        prompt.delete_selection();
                    } else if prompt.cursor_pos < prompt.input.len() {
                        let mut char_end = prompt.cursor_pos + 1;
                        while char_end < prompt.input.len()
                            && !prompt.input.is_char_boundary(char_end)
                        {
                            char_end += 1;
                        }
                        prompt.input.drain(prompt.cursor_pos..char_end);
                    }
                }
                self.update_prompt_suggestions();
            }
            Action::PromptMoveLeft => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.clear_selection();
                    if prompt.cursor_pos > 0 {
                        let mut new_pos = prompt.cursor_pos - 1;
                        while new_pos > 0 && !prompt.input.is_char_boundary(new_pos) {
                            new_pos -= 1;
                        }
                        prompt.cursor_pos = new_pos;
                    }
                }
            }
            Action::PromptMoveRight => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.clear_selection();
                    if prompt.cursor_pos < prompt.input.len() {
                        let mut new_pos = prompt.cursor_pos + 1;
                        while new_pos < prompt.input.len()
                            && !prompt.input.is_char_boundary(new_pos)
                        {
                            new_pos += 1;
                        }
                        prompt.cursor_pos = new_pos;
                    }
                }
            }
            Action::PromptMoveStart => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.clear_selection();
                    prompt.cursor_pos = 0;
                }
            }
            Action::PromptMoveEnd => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.clear_selection();
                    prompt.cursor_pos = prompt.input.len();
                }
            }
            Action::PromptSelectPrev => {
                if let Some(prompt) = self.prompt_mut() {
                    if !prompt.suggestions.is_empty() {
                        // Suggestions exist: navigate suggestions
                        if let Some(selected) = prompt.selected_suggestion {
                            // Don't wrap around - stay at 0 if already at the beginning
                            prompt.selected_suggestion = if selected == 0 {
                                Some(0)
                            } else {
                                Some(selected - 1)
                            };
                        }
                    } else {
                        // No suggestions: navigate history (Up arrow)
                        let prompt_type = prompt.prompt_type.clone();
                        let current_input = prompt.input.clone();

                        // Get the appropriate history based on prompt type
                        let history_item = match prompt_type {
                            PromptType::Search
                            | PromptType::ReplaceSearch
                            | PromptType::QueryReplaceSearch => {
                                self.search_history.navigate_prev(&current_input)
                            }
                            PromptType::Replace { .. } | PromptType::QueryReplace { .. } => {
                                self.replace_history.navigate_prev(&current_input)
                            }
                            _ => None,
                        };

                        // Update prompt input if history item exists
                        if let Some(history_text) = history_item {
                            if let Some(prompt) = self.prompt_mut() {
                                prompt.set_input(history_text.clone());

                                // For search prompts, update highlights incrementally
                                if matches!(
                                    prompt_type,
                                    PromptType::Search
                                        | PromptType::ReplaceSearch
                                        | PromptType::QueryReplaceSearch
                                ) {
                                    self.update_search_highlights(&history_text);
                                }
                            }
                        }
                    }
                }
            }
            Action::PromptSelectNext => {
                if let Some(prompt) = self.prompt_mut() {
                    if !prompt.suggestions.is_empty() {
                        // Suggestions exist: navigate suggestions
                        if let Some(selected) = prompt.selected_suggestion {
                            // Don't wrap around - stay at the end if already at the last item
                            let new_pos = selected + 1;
                            prompt.selected_suggestion =
                                Some(new_pos.min(prompt.suggestions.len() - 1));
                        }
                    } else {
                        // No suggestions: navigate history (Down arrow)
                        let prompt_type = prompt.prompt_type.clone();

                        // Get the appropriate history based on prompt type
                        let history_item = match prompt_type {
                            PromptType::Search
                            | PromptType::ReplaceSearch
                            | PromptType::QueryReplaceSearch => self.search_history.navigate_next(),
                            PromptType::Replace { .. } | PromptType::QueryReplace { .. } => {
                                self.replace_history.navigate_next()
                            }
                            _ => None,
                        };

                        // Update prompt input if history item exists
                        if let Some(history_text) = history_item {
                            if let Some(prompt) = self.prompt_mut() {
                                prompt.set_input(history_text.clone());

                                // For search prompts, update highlights incrementally
                                if matches!(
                                    prompt_type,
                                    PromptType::Search
                                        | PromptType::ReplaceSearch
                                        | PromptType::QueryReplaceSearch
                                ) {
                                    self.update_search_highlights(&history_text);
                                }
                            }
                        }
                    }
                }
            }
            Action::PromptPageUp => {
                if let Some(prompt) = self.prompt_mut() {
                    if !prompt.suggestions.is_empty() {
                        if let Some(selected) = prompt.selected_suggestion {
                            // Move up by 10, but stop at 0 instead of wrapping
                            prompt.selected_suggestion = Some(selected.saturating_sub(10));
                        }
                    }
                }
            }
            Action::PromptPageDown => {
                if let Some(prompt) = self.prompt_mut() {
                    if !prompt.suggestions.is_empty() {
                        if let Some(selected) = prompt.selected_suggestion {
                            // Move down by 10, but stop at the end instead of wrapping
                            let len = prompt.suggestions.len();
                            let new_pos = selected + 10;
                            prompt.selected_suggestion = Some(new_pos.min(len - 1));
                        }
                    }
                }
            }
            Action::PromptAcceptSuggestion => {
                if let Some(prompt) = self.prompt_mut() {
                    if let Some(selected) = prompt.selected_suggestion {
                        if let Some(suggestion) = prompt.suggestions.get(selected) {
                            // Don't accept disabled suggestions (greyed out commands)
                            if !suggestion.disabled {
                                prompt.input = suggestion.get_value().to_string();
                                prompt.cursor_pos = prompt.input.len();
                                prompt.clear_selection();
                            }
                        }
                    }
                }
                // Refresh suggestions after accepting (important for path completion)
                self.update_prompt_suggestions();
            }
            Action::PromptMoveWordLeft => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.move_word_left();
                }
            }
            Action::PromptMoveWordRight => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.move_word_right();
                }
            }
            // Advanced prompt editing actions
            Action::PromptDeleteWordForward => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.delete_word_forward();
                }
                self.update_prompt_suggestions();
            }
            Action::PromptDeleteWordBackward => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.delete_word_backward();
                }
                self.update_prompt_suggestions();
            }
            Action::PromptCopy => {
                if let Some(prompt) = &self.prompt {
                    // If there's a selection, copy selected text; otherwise copy entire input
                    self.clipboard = if let Some(selected) = prompt.selected_text() {
                        selected
                    } else {
                        prompt.get_text()
                    };
                    self.set_status_message("Copied".to_string());
                }
            }
            Action::PromptCut => {
                // Get text first (selected or entire input)
                let text = if let Some(prompt) = &self.prompt {
                    if let Some(selected) = prompt.selected_text() {
                        selected
                    } else {
                        prompt.get_text()
                    }
                } else {
                    String::new()
                };
                // Update clipboard before taking mutable borrow
                self.clipboard = text;
                // Now cut the text (delete selection or clear entire input)
                if let Some(prompt) = self.prompt_mut() {
                    if prompt.has_selection() {
                        prompt.delete_selection();
                    } else {
                        prompt.clear();
                    }
                }
                self.set_status_message("Cut".to_string());
                self.update_prompt_suggestions();
            }
            Action::PromptPaste => {
                let text = self.clipboard.clone();
                if let Some(prompt) = self.prompt_mut() {
                    prompt.insert_str(&text);
                }
                self.update_prompt_suggestions();
            }
            // Prompt selection actions
            Action::PromptMoveLeftSelecting => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.move_left_selecting();
                }
            }
            Action::PromptMoveRightSelecting => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.move_right_selecting();
                }
            }
            Action::PromptMoveHomeSelecting => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.move_home_selecting();
                }
            }
            Action::PromptMoveEndSelecting => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.move_end_selecting();
                }
            }
            Action::PromptSelectWordLeft => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.move_word_left_selecting();
                }
            }
            Action::PromptSelectWordRight => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.move_word_right_selecting();
                }
            }
            Action::PromptSelectAll => {
                if let Some(prompt) = self.prompt_mut() {
                    prompt.selection_anchor = Some(0);
                    prompt.cursor_pos = prompt.input.len();
                }
            }

            // Popup mode actions
            Action::PopupSelectNext => {
                self.popup_select_next();
            }
            Action::PopupSelectPrev => {
                self.popup_select_prev();
            }
            Action::PopupPageUp => {
                self.popup_page_up();
            }
            Action::PopupPageDown => {
                self.popup_page_down();
            }
            Action::PopupConfirm => {
                return self.handle_action(action);
            }
            Action::PopupCancel => {
                return self.handle_action(action);
            }

            // Normal mode actions - delegate to handle_action
            _ => {
                return self.handle_action(action);
            }
        }

        Ok(())
    }

    fn dispatch_plugin_hook(&mut self, hook_name: &str, args: HookArgs, fallback: &str) {
        if let Some(ts_manager) = &self.ts_plugin_manager {
            if ts_manager.has_hook_handlers(hook_name) {
                ts_manager.run_hook(hook_name, args);
                return;
            }
        }
        self.set_status_message(fallback.to_string());
    }

    /// Handle an action (for normal mode and command execution)
    pub(super) fn handle_action(&mut self, action: Action) -> std::io::Result<()> {
        use crate::keybindings::Action;

        // Record action to macro if recording
        self.record_macro_action(&action);

        match action {
            Action::Quit => self.quit(),
            Action::Save => self.save()?,
            Action::SaveAs => {
                // Get current filename as default suggestion
                let current_path = self
                    .active_state()
                    .buffer
                    .file_path()
                    .map(|p| {
                        // Make path relative to working_dir if possible
                        p.strip_prefix(&self.working_dir)
                            .unwrap_or(p)
                            .to_string_lossy()
                            .to_string()
                    })
                    .unwrap_or_default();
                self.start_prompt_with_initial_text(
                    "Save as: ".to_string(),
                    PromptType::SaveFileAs,
                    current_path,
                );
            }
            Action::Open => {
                self.start_prompt("Open file: ".to_string(), PromptType::OpenFile);
                self.prefill_open_file_prompt();
            }
            Action::GotoLine => self.start_prompt("Go to line: ".to_string(), PromptType::GotoLine),
            Action::New => {
                self.new_buffer();
            }
            Action::Close => {
                let buffer_id = self.active_buffer;
                if let Err(e) = self.close_buffer(buffer_id) {
                    self.set_status_message(format!("Cannot close buffer: {}", e));
                } else {
                    self.set_status_message("Buffer closed".to_string());
                }
            }
            Action::Copy => self.copy_selection(),
            Action::Cut => {
                if self.is_editing_disabled() {
                    self.set_status_message("Editing disabled in this buffer".to_string());
                    return Ok(());
                }
                self.cut_selection()
            }
            Action::Paste => {
                if self.is_editing_disabled() {
                    self.set_status_message("Editing disabled in this buffer".to_string());
                    return Ok(());
                }
                self.paste()
            }
            Action::Undo => {
                if self.is_editing_disabled() {
                    self.set_status_message("Editing disabled in this buffer".to_string());
                    return Ok(());
                }
                let events = self.active_event_log_mut().undo();
                // Apply all inverse events collected during undo
                for event in events {
                    self.apply_event_to_active_buffer(&event);
                }
            }
            Action::Redo => {
                if self.is_editing_disabled() {
                    self.set_status_message("Editing disabled in this buffer".to_string());
                    return Ok(());
                }
                let events = self.active_event_log_mut().redo();
                // Apply all events collected during redo
                for event in events {
                    self.apply_event_to_active_buffer(&event);
                }
            }
            Action::ShowHelp => {
                self.dispatch_plugin_hook(
                    "manual_page",
                    HookArgs::ManualPage,
                    "Manual not available (plugins not loaded)",
                );
            }
            Action::ShowKeyboardShortcuts => {
                let bindings = self.keybindings.get_all_bindings();
                self.dispatch_plugin_hook(
                    "keyboard_shortcuts",
                    HookArgs::KeyboardShortcuts { bindings },
                    "Keyboard shortcuts not available (plugins not loaded)",
                );
            }
            Action::CommandPalette => {
                // Toggle command palette: close if already open, otherwise open it
                if let Some(prompt) = &self.prompt {
                    if prompt.prompt_type == PromptType::Command {
                        self.cancel_prompt();
                        return Ok(());
                    }
                }

                // Use the current context for filtering commands
                let suggestions = self.command_registry.read().unwrap().filter(
                    "",
                    self.key_context,
                    &self.keybindings,
                    self.has_active_selection(),
                );
                self.start_prompt_with_suggestions(
                    "Command: ".to_string(),
                    PromptType::Command,
                    suggestions,
                );
            }
            Action::ToggleLineWrap => {
                self.config.editor.line_wrap = !self.config.editor.line_wrap;

                // Update all viewports to reflect the new line wrap setting
                for state in self.buffers.values_mut() {
                    state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
                }

                let state = if self.config.editor.line_wrap {
                    "enabled"
                } else {
                    "disabled"
                };
                self.set_status_message(format!("Line wrap {}", state));
            }
            Action::ToggleComposeMode => {
                let default_wrap = self.config.editor.line_wrap;
                let default_line_numbers = self.config.editor.line_numbers;
                let active_split = self.split_manager.active_split();
                let mut view_mode = {
                    if let Some(vs) = self.split_view_states.get(&active_split) {
                        vs.view_mode.clone()
                    } else {
                        self.active_state().view_mode.clone()
                    }
                };

                view_mode = match view_mode {
                    crate::state::ViewMode::Compose => crate::state::ViewMode::Source,
                    _ => crate::state::ViewMode::Compose,
                };

                // Update split view state
                let current_line_numbers = self.active_state().margins.show_line_numbers;
                if let Some(vs) = self.split_view_states.get_mut(&active_split) {
                    vs.view_mode = view_mode.clone();
                    vs.viewport.line_wrap_enabled = matches!(view_mode, crate::state::ViewMode::Compose) || default_wrap;
                    match view_mode {
                        crate::state::ViewMode::Compose => {
                            vs.compose_prev_line_numbers = Some(current_line_numbers);
                            self.active_state_mut().margins.set_line_numbers(false);
                        }
                        crate::state::ViewMode::Source => {
                            let restore = vs
                                .compose_prev_line_numbers
                                .take()
                                .unwrap_or(default_line_numbers);
                            self.active_state_mut().margins.set_line_numbers(restore);
                        }
                    }
                }

                // Keep buffer-level view mode for status/use
                {
                    let state = self.active_state_mut();
                    state.view_mode = view_mode.clone();
                    state.viewport.line_wrap_enabled =
                        matches!(view_mode, crate::state::ViewMode::Compose) || default_wrap;
                }

                let mode_label = match view_mode {
                    crate::state::ViewMode::Compose => "Compose",
                    crate::state::ViewMode::Source => "Source",
                };
                self.set_status_message(format!("Mode: {}", mode_label));
            }
            Action::SetComposeWidth => {
                let active_split = self.split_manager.active_split();
                let current = self
                    .split_view_states
                    .get(&active_split)
                    .and_then(|v| v.compose_width.map(|w| w.to_string()))
                    .unwrap_or_default();
                self.start_prompt_with_initial_text(
                    "Compose width (empty = viewport): ".to_string(),
                    PromptType::SetComposeWidth,
                    current,
                );
            }
            Action::SetBackground => {
                let default_path = self
                    .ansi_background_path
                    .as_ref()
                    .and_then(|p| {
                        p.strip_prefix(&self.working_dir)
                            .ok()
                            .map(|rel| rel.to_string_lossy().to_string())
                    })
                    .unwrap_or_else(|| DEFAULT_BACKGROUND_FILE.to_string());

                self.start_prompt_with_initial_text(
                    "Background file: ".to_string(),
                    PromptType::SetBackgroundFile,
                    default_path,
                );
            }
            Action::SetBackgroundBlend => {
                let default_amount = format!("{:.2}", self.background_fade);
                self.start_prompt_with_initial_text(
                    "Background blend (0-1): ".to_string(),
                    PromptType::SetBackgroundBlend,
                    default_amount,
                );
            }
            Action::LspCompletion => {
                self.request_completion()?;
            }
            Action::LspGotoDefinition => {
                self.request_goto_definition()?;
            }
            Action::LspRename => {
                self.start_rename()?;
            }
            Action::LspHover => {
                self.request_hover()?;
            }
            Action::LspReferences => {
                self.request_references()?;
            }
            Action::LspSignatureHelp => {
                self.request_signature_help()?;
            }
            Action::LspCodeActions => {
                self.request_code_actions()?;
            }
            Action::LspRestart => {
                // Get the language for the current buffer
                if let Some(metadata) = self.buffer_metadata.get(&self.active_buffer) {
                    if let Some(path) = metadata.file_path() {
                        if let Some(language) = crate::lsp_manager::detect_language(path) {
                            let restart_result = if let Some(lsp) = self.lsp.as_mut() {
                                Some(lsp.manual_restart(&language))
                            } else {
                                None
                            };

                            if let Some((success, message)) = restart_result {
                                self.status_message = Some(message);
                                if success {
                                    // Re-send didOpen for all buffers of this language
                                    let buffers_for_language: Vec<_> = self
                                        .buffer_metadata
                                        .iter()
                                        .filter_map(|(buf_id, meta)| {
                                            if let Some(p) = meta.file_path() {
                                                if crate::lsp_manager::detect_language(p)
                                                    == Some(language.clone())
                                                {
                                                    Some((*buf_id, p.clone()))
                                                } else {
                                                    None
                                                }
                                            } else {
                                                None
                                            }
                                        })
                                        .collect();

                                    for (buffer_id, buf_path) in buffers_for_language {
                                        if let Some(state) = self.buffers.get(&buffer_id) {
                                            let content = state.buffer.to_string();
                                            let uri: Option<lsp_types::Uri> =
                                                url::Url::from_file_path(&buf_path).ok().and_then(
                                                    |u| u.as_str().parse::<lsp_types::Uri>().ok(),
                                                );
                                            if let Some(uri) = uri {
                                                if let Some(lang_id) =
                                                    crate::lsp_manager::detect_language(&buf_path)
                                                {
                                                    if let Some(lsp) = self.lsp.as_mut() {
                                                        if let Some(handle) =
                                                            lsp.get_or_spawn(&lang_id)
                                                        {
                                                            let _ = handle
                                                                .did_open(uri, content, lang_id);
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            } else {
                                self.status_message = Some("No LSP manager available".to_string());
                            }
                        } else {
                            self.status_message =
                                Some("No LSP server configured for this file type".to_string());
                        }
                    } else {
                        self.status_message =
                            Some("Current buffer has no associated file".to_string());
                    }
                }
            }
            Action::ToggleInlayHints => {
                self.toggle_inlay_hints();
            }
            Action::DumpConfig => {
                self.dump_config();
            }
            Action::Search => {
                // If already in a search-related prompt, Ctrl+F acts like Enter (confirm search)
                let is_search_prompt = self.prompt.as_ref().is_some_and(|p| {
                    matches!(
                        p.prompt_type,
                        PromptType::Search
                            | PromptType::ReplaceSearch
                            | PromptType::QueryReplaceSearch
                    )
                });

                if is_search_prompt {
                    self.confirm_prompt();
                } else {
                    self.start_search_prompt("Search: ".to_string(), PromptType::Search, false);
                }
            }
            Action::Replace => {
                self.start_search_prompt("Replace: ".to_string(), PromptType::ReplaceSearch, false);
            }
            Action::QueryReplace => {
                self.start_search_prompt(
                    "Query replace: ".to_string(),
                    PromptType::QueryReplaceSearch,
                    false,
                );
            }
            Action::FindInSelection => {
                self.start_search_prompt("Search: ".to_string(), PromptType::Search, true);
            }
            Action::FindNext => {
                self.find_next();
            }
            Action::FindPrevious => {
                self.find_previous();
            }
            Action::AddCursorNextMatch => self.add_cursor_at_next_match(),
            Action::AddCursorAbove => self.add_cursor_above(),
            Action::AddCursorBelow => self.add_cursor_below(),
            Action::NextBuffer => self.next_buffer(),
            Action::PrevBuffer => self.prev_buffer(),

            // Tab scrolling
            Action::ScrollTabsLeft => {
                let active_split_id = self.split_manager.active_split();
                if let Some(view_state) = self.split_view_states.get_mut(&active_split_id) {
                    view_state.tab_scroll_offset = view_state.tab_scroll_offset.saturating_sub(5);
                    // After manual scroll, re-evaluate to clamp and show indicators
                    self.ensure_active_tab_visible(
                        active_split_id,
                        self.active_buffer,
                        self.terminal_width,
                    );
                    self.set_status_message("Scrolled tabs left".to_string());
                }
            }
            Action::ScrollTabsRight => {
                let active_split_id = self.split_manager.active_split();
                if let Some(view_state) = self.split_view_states.get_mut(&active_split_id) {
                    view_state.tab_scroll_offset = view_state.tab_scroll_offset.saturating_add(5);
                    // After manual scroll, re-evaluate to clamp and show indicators
                    self.ensure_active_tab_visible(
                        active_split_id,
                        self.active_buffer,
                        self.terminal_width,
                    );
                    self.set_status_message("Scrolled tabs right".to_string());
                }
            }
            Action::NavigateBack => self.navigate_back(),
            Action::NavigateForward => self.navigate_forward(),
            Action::SplitHorizontal => self.split_pane_horizontal(),
            Action::SplitVertical => self.split_pane_vertical(),
            Action::CloseSplit => self.close_active_split(),
            Action::NextSplit => self.next_split(),
            Action::PrevSplit => self.prev_split(),
            Action::IncreaseSplitSize => self.adjust_split_size(0.05),
            Action::DecreaseSplitSize => self.adjust_split_size(-0.05),
            Action::ToggleFileExplorer => self.toggle_file_explorer(),
            Action::ToggleLineNumbers => self.toggle_line_numbers(),
            Action::FocusFileExplorer => self.focus_file_explorer(),
            Action::FocusEditor => self.focus_editor(),
            Action::FileExplorerUp => self.file_explorer_navigate_up(),
            Action::FileExplorerDown => self.file_explorer_navigate_down(),
            Action::FileExplorerPageUp => self.file_explorer_page_up(),
            Action::FileExplorerPageDown => self.file_explorer_page_down(),
            Action::FileExplorerExpand => self.file_explorer_toggle_expand(),
            Action::FileExplorerCollapse => self.file_explorer_toggle_expand(), // Same as expand
            Action::FileExplorerOpen => self.file_explorer_open_file()?,
            Action::FileExplorerRefresh => self.file_explorer_refresh(),
            Action::FileExplorerNewFile => self.file_explorer_new_file(),
            Action::FileExplorerNewDirectory => self.file_explorer_new_directory(),
            Action::FileExplorerDelete => self.file_explorer_delete(),
            Action::FileExplorerRename => self.file_explorer_rename(),
            Action::FileExplorerToggleHidden => self.file_explorer_toggle_hidden(),
            Action::FileExplorerToggleGitignored => self.file_explorer_toggle_gitignored(),
            Action::RemoveSecondaryCursors => {
                // Convert action to events and apply them
                if let Some(events) = self.action_to_events(Action::RemoveSecondaryCursors) {
                    // Wrap in batch for atomic undo
                    let batch = Event::Batch {
                        events: events.clone(),
                        description: "Remove secondary cursors".to_string(),
                    };
                    self.active_event_log_mut().append(batch.clone());
                    self.apply_event_to_active_buffer(&batch);

                    // Ensure the primary cursor is visible after removing secondary cursors
                    let state = self.active_state_mut();
                    let primary = *state.cursors.primary();
                    state.viewport.ensure_visible(&mut state.buffer, &primary);
                }
            }

            // Menu navigation actions
            Action::MenuActivate => {
                // Open the first menu
                self.menu_state.open_menu(0);
            }
            Action::MenuClose => {
                self.menu_state.close_menu();
            }
            Action::MenuLeft => {
                let total_menus = self.config.menu.menus.len() + self.menu_state.plugin_menus.len();
                self.menu_state.prev_menu(total_menus);
            }
            Action::MenuRight => {
                let total_menus = self.config.menu.menus.len() + self.menu_state.plugin_menus.len();
                self.menu_state.next_menu(total_menus);
            }
            Action::MenuUp => {
                if let Some(active_idx) = self.menu_state.active_menu {
                    let all_menus: Vec<crate::config::Menu> = self
                        .config
                        .menu
                        .menus
                        .iter()
                        .chain(self.menu_state.plugin_menus.iter())
                        .cloned()
                        .collect();
                    if let Some(menu) = all_menus.get(active_idx) {
                        self.menu_state.prev_item(menu);
                    }
                }
            }
            Action::MenuDown => {
                if let Some(active_idx) = self.menu_state.active_menu {
                    let all_menus: Vec<crate::config::Menu> = self
                        .config
                        .menu
                        .menus
                        .iter()
                        .chain(self.menu_state.plugin_menus.iter())
                        .cloned()
                        .collect();
                    if let Some(menu) = all_menus.get(active_idx) {
                        self.menu_state.next_item(menu);
                    }
                }
            }
            Action::MenuExecute => {
                // Execute the highlighted menu item's action
                let all_menus: Vec<crate::config::Menu> = self
                    .config
                    .menu
                    .menus
                    .iter()
                    .chain(self.menu_state.plugin_menus.iter())
                    .cloned()
                    .collect();

                if let Some((action_name, args)) = self
                    .menu_state
                    .get_highlighted_action(&all_menus, self.has_active_selection())
                {
                    // Close the menu
                    self.menu_state.close_menu();

                    // Parse and execute the action
                    // First try built-in actions, then fall back to plugin actions
                    if let Some(action) = Action::from_str(&action_name, &args) {
                        return self.handle_action(action);
                    } else {
                        // Treat as a plugin action (global Lua function)
                        return self.handle_action(Action::PluginAction(action_name));
                    }
                }
            }
            Action::MenuOpen(menu_name) => {
                // Find the menu by name and open it
                let all_menus: Vec<crate::config::Menu> = self
                    .config
                    .menu
                    .menus
                    .iter()
                    .chain(self.menu_state.plugin_menus.iter())
                    .cloned()
                    .collect();

                for (idx, menu) in all_menus.iter().enumerate() {
                    if menu.label.eq_ignore_ascii_case(&menu_name) {
                        self.menu_state.open_menu(idx);
                        break;
                    }
                }
            }

            Action::SwitchKeybindingMap(map_name) => {
                // Check if the map exists (either built-in or user-defined)
                let is_builtin = matches!(map_name.as_str(), "default" | "emacs" | "vscode");
                let is_user_defined = self.config.keybinding_maps.contains_key(&map_name);

                if is_builtin || is_user_defined {
                    // Update the active keybinding map in config
                    self.config.active_keybinding_map = map_name.clone();

                    // Reload the keybinding resolver with the new map
                    self.keybindings = crate::keybindings::KeybindingResolver::new(&self.config);

                    self.set_status_message(format!("Switched to '{}' keybindings", map_name));
                } else {
                    self.set_status_message(format!("Unknown keybinding map: '{}'", map_name));
                }
            }

            Action::SmartHome => {
                self.smart_home();
            }
            Action::IndentSelection => {
                self.indent_selection();
            }
            Action::DedentSelection => {
                self.dedent_selection();
            }
            Action::ToggleComment => {
                self.toggle_comment();
            }
            Action::GoToMatchingBracket => {
                self.goto_matching_bracket();
            }
            Action::JumpToNextError => {
                self.jump_to_next_error();
            }
            Action::JumpToPreviousError => {
                self.jump_to_previous_error();
            }
            Action::SetBookmark(key) => {
                self.set_bookmark(key);
            }
            Action::JumpToBookmark(key) => {
                self.jump_to_bookmark(key);
            }
            Action::ClearBookmark(key) => {
                self.clear_bookmark(key);
            }
            Action::ListBookmarks => {
                self.list_bookmarks();
            }
            Action::ToggleSearchCaseSensitive => {
                self.search_case_sensitive = !self.search_case_sensitive;
                let state = if self.search_case_sensitive {
                    "enabled"
                } else {
                    "disabled"
                };
                self.set_status_message(format!("Case-sensitive search {}", state));
                // Re-run search if active
                if let Some(search_state) = &self.search_state {
                    let query = search_state.query.clone();
                    self.perform_search(&query);
                }
            }
            Action::ToggleSearchWholeWord => {
                self.search_whole_word = !self.search_whole_word;
                let state = if self.search_whole_word {
                    "enabled"
                } else {
                    "disabled"
                };
                self.set_status_message(format!("Whole word search {}", state));
                // Re-run search if active
                if let Some(search_state) = &self.search_state {
                    let query = search_state.query.clone();
                    self.perform_search(&query);
                }
            }
            Action::StartMacroRecording => {
                // This is a no-op; use ToggleMacroRecording instead
                self.set_status_message(
                    "Use Ctrl+Shift+R to start recording (will prompt for register)".to_string(),
                );
            }
            Action::StopMacroRecording => {
                self.stop_macro_recording();
            }
            Action::PlayMacro(key) => {
                self.play_macro(key);
            }
            Action::ToggleMacroRecording(key) => {
                self.toggle_macro_recording(key);
            }
            Action::ShowMacro(key) => {
                self.show_macro_in_buffer(key);
            }
            Action::ListMacros => {
                self.list_macros_in_buffer();
            }
            Action::PromptRecordMacro => {
                self.start_prompt("Record macro (0-9): ".to_string(), PromptType::RecordMacro);
            }
            Action::PromptPlayMacro => {
                self.start_prompt("Play macro (0-9): ".to_string(), PromptType::PlayMacro);
            }
            Action::PlayLastMacro => {
                if let Some(key) = self.last_macro_register {
                    self.play_macro(key);
                } else {
                    self.set_status_message("No macro has been recorded yet".to_string());
                }
            }
            Action::PromptSetBookmark => {
                self.start_prompt("Set bookmark (0-9): ".to_string(), PromptType::SetBookmark);
            }
            Action::PromptJumpToBookmark => {
                self.start_prompt(
                    "Jump to bookmark (0-9): ".to_string(),
                    PromptType::JumpToBookmark,
                );
            }
            Action::None => {}
            Action::DeleteBackward => {
                if self.is_editing_disabled() {
                    self.set_status_message("Editing disabled in this buffer".to_string());
                    return Ok(());
                }
                // Normal backspace handling
                if let Some(events) = self.action_to_events(Action::DeleteBackward) {
                    if events.len() > 1 {
                        let batch = Event::Batch {
                            events: events.clone(),
                            description: "Delete backward".to_string(),
                        };
                        self.active_event_log_mut().append(batch.clone());
                        self.apply_event_to_active_buffer(&batch);
                        // Note: LSP notifications now handled automatically by apply_event_to_active_buffer
                    } else {
                        for event in events {
                            self.active_event_log_mut().append(event.clone());
                            self.apply_event_to_active_buffer(&event);
                            // Note: LSP notifications now handled automatically by apply_event_to_active_buffer
                        }
                    }
                }
            }
            Action::PluginAction(action_name) => {
                // Execute the plugin callback via TypeScript plugin thread
                // Use non-blocking version to avoid deadlock with async plugin ops
                if let Some(ref manager) = self.ts_plugin_manager {
                    match manager.execute_action_async(&action_name) {
                        Ok(receiver) => {
                            // Store pending action for processing in main loop
                            self.pending_plugin_actions
                                .push((action_name.clone(), receiver));
                        }
                        Err(e) => {
                            self.set_status_message(format!("Plugin error: {}", e));
                            tracing::error!("Plugin action error: {}", e);
                        }
                    }
                } else {
                    self.set_status_message("Plugin manager not available".to_string());
                }
            }
            Action::PromptConfirm => {
                // Handle prompt confirmation (same logic as in handle_key)
                if let Some((input, prompt_type, selected_index)) = self.confirm_prompt() {
                    use std::path::Path;
                    match prompt_type {
                        PromptType::OpenFile => {
                            let input_path = Path::new(&input);
                            let resolved_path = if input_path.is_absolute() {
                                input_path.to_path_buf()
                            } else {
                                self.working_dir.join(input_path)
                            };

                            if let Err(e) = self.open_file(&resolved_path) {
                                self.set_status_message(format!("Error opening file: {e}"));
                            } else {
                                self.set_status_message(format!("Opened {}", resolved_path.display()));
                            }
                        }
                        PromptType::SaveFileAs => {
                            // Resolve path: if relative, make it relative to working_dir
                            let input_path = Path::new(&input);
                            let full_path = if input_path.is_absolute() {
                                input_path.to_path_buf()
                            } else {
                                self.working_dir.join(input_path)
                            };

                            // Save the buffer to the new file
                            match self.active_state_mut().buffer.save_to_file(&full_path) {
                                Ok(()) => {
                                    // Update metadata with the new path
                                    let metadata = BufferMetadata::with_file(
                                        full_path.clone(),
                                        &self.working_dir,
                                    );
                                    self.buffer_metadata.insert(self.active_buffer, metadata);

                                    // Notify LSP of the new file if applicable
                                    self.notify_lsp_save();

                                    // Emit file saved event
                                    self.emit_event(
                                        crate::control_event::events::FILE_SAVED.name,
                                        serde_json::json!({"path": full_path.display().to_string()}),
                                    );

                                    self.set_status_message(format!(
                                        "Saved as: {}",
                                        full_path.display()
                                    ));
                                }
                                Err(e) => {
                                    self.set_status_message(format!("Error saving file: {}", e));
                                }
                            }
                        }
                        PromptType::Search => {
                            self.perform_search(&input);
                        }
                        PromptType::ReplaceSearch => {
                            self.perform_search(&input);
                            self.start_prompt(
                                format!("Replace '{}' with: ", input),
                                PromptType::Replace {
                                    search: input.clone(),
                                },
                            );
                        }
                        PromptType::Replace { search } => {
                            self.perform_replace(&search, &input);
                        }
                        PromptType::QueryReplaceSearch => {
                            self.perform_search(&input);
                            self.start_prompt(
                                format!("Query replace '{}' with: ", input),
                                PromptType::QueryReplace {
                                    search: input.clone(),
                                },
                            );
                        }
                        PromptType::QueryReplace { search } => {
                            self.start_interactive_replace(&search, &input);
                        }
                        PromptType::Command => {
                            let commands = self.command_registry.read().unwrap().get_all();
                            if let Some(cmd) = commands.iter().find(|c| c.name == input) {
                                let action = cmd.action.clone();
                                let cmd_name = cmd.name.clone();
                                self.set_status_message(format!("Executing: {}", cmd_name));
                                // Record command usage for history
                                self.command_registry
                                    .write()
                                    .unwrap()
                                    .record_usage(&cmd_name);
                                return self.handle_action(action);
                            } else {
                                self.set_status_message(format!("Unknown command: {input}"));
                            }
                        }
                        PromptType::GotoLine => {
                            match input.trim().parse::<usize>() {
                                Ok(line_num) if line_num > 0 => {
                                    let target_line = line_num.saturating_sub(1);
                                    let buffer_id = self.active_buffer;
                                    let estimated_line_length =
                                        self.config.editor.estimated_line_length;

                                    if let Some(state) = self.buffers.get(&buffer_id) {
                                        let cursor_id = state.cursors.primary_id();
                                        let old_position = state.cursors.primary().position;
                                        let old_anchor = state.cursors.primary().anchor;
                                        let old_sticky_column =
                                            state.cursors.primary().sticky_column;
                                        let is_large_file = state.buffer.line_count().is_none();
                                        let buffer_len = state.buffer.len();

                                        let (position, status_message) = if is_large_file {
                                            // Large file mode: estimate byte offset based on line number
                                            let estimated_offset =
                                                target_line * estimated_line_length;
                                            let clamped_offset = estimated_offset.min(buffer_len);

                                            // Use LineIterator to find the actual line start at the estimated position
                                            let position = if let Some(state) =
                                                self.buffers.get_mut(&buffer_id)
                                            {
                                                let iter = state.buffer.line_iterator(
                                                    clamped_offset,
                                                    estimated_line_length,
                                                );
                                                iter.current_position()
                                            } else {
                                                clamped_offset
                                            };

                                            let msg = format!(
                                                "Jumped to estimated line {} (large file mode)",
                                                line_num
                                            );
                                            (position, msg)
                                        } else {
                                            // Small file mode: use exact line position
                                            let max_line = state
                                                .buffer
                                                .line_count()
                                                .unwrap_or(1)
                                                .saturating_sub(1);
                                            let actual_line = target_line.min(max_line);
                                            let position =
                                                state.buffer.line_col_to_position(actual_line, 0);

                                            let msg = if target_line > max_line {
                                                format!(
                                                    "Line {} doesn't exist, jumped to line {}",
                                                    line_num,
                                                    actual_line + 1
                                                )
                                            } else {
                                                format!("Jumped to line {}", line_num)
                                            };
                                            (position, msg)
                                        };

                                        let event = crate::event::Event::MoveCursor {
                                            cursor_id,
                                            old_position,
                                            new_position: position,
                                            old_anchor,
                                            new_anchor: None,
                                            old_sticky_column,
                                            new_sticky_column: 0,
                                        };
                                        if let Some(state) = self.buffers.get_mut(&buffer_id) {
                                            state.apply(&event);
                                        }
                                        self.set_status_message(status_message);
                                    }
                                }
                                Ok(_) => {
                                    self.set_status_message(
                                        "Line number must be positive".to_string(),
                                    );
                                }
                                Err(_) => {
                                    self.set_status_message(format!(
                                        "Invalid line number: {}",
                                        input
                                    ));
                                }
                            }
                        }
                        PromptType::SetBackgroundFile => {
                            if let Err(e) = self.load_ansi_background(&input) {
                                self.set_status_message(format!(
                                    "Failed to load background: {}",
                                    e
                                ));
                            }
                        }
                        PromptType::SetBackgroundBlend => {
                            let parsed = input.trim().parse::<f32>();
                            match parsed {
                                Ok(val) => {
                                    let clamped = val.clamp(0.0, 1.0);
                                    self.background_fade = clamped;
                                    self.set_status_message(format!(
                                        "Background blend set to {:.2}",
                                        clamped
                                    ));
                                }
                                Err(_) => {
                                    self.set_status_message(format!(
                                        "Invalid blend value: {}",
                                        input
                                    ));
                                }
                            }
                        }
                        PromptType::SetComposeWidth => {
                            let buffer_id = self.active_buffer;
                            let active_split = self.split_manager.active_split();
                            let trimmed = input.trim();
                            if trimmed.is_empty() {
                                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                                    state.compose_width = None;
                                }
                                if let Some(vs) = self.split_view_states.get_mut(&active_split) {
                                    vs.compose_width = None;
                                }
                                self.set_status_message("Compose width cleared (viewport)".to_string());
                            } else {
                                match trimmed.parse::<u16>() {
                                    Ok(val) if val > 0 => {
                                        if let Some(state) = self.buffers.get_mut(&buffer_id) {
                                            state.compose_width = Some(val);
                                        }
                                        if let Some(vs) = self.split_view_states.get_mut(&active_split) {
                                            vs.compose_width = Some(val);
                                        }
                                        self.set_status_message(format!(
                                            "Compose width set to {}",
                                            val
                                        ));
                                    }
                                    _ => {
                                        self.set_status_message(format!(
                                            "Invalid compose width: {}",
                                            input
                                        ));
                                    }
                                }
                            }
                        }
                        PromptType::RecordMacro => {
                            if let Some(c) = input.trim().chars().next() {
                                if c.is_ascii_digit() {
                                    self.toggle_macro_recording(c);
                                } else {
                                    self.set_status_message(
                                        "Macro register must be 0-9".to_string(),
                                    );
                                }
                            } else {
                                self.set_status_message("No register specified".to_string());
                            }
                        }
                        PromptType::PlayMacro => {
                            if let Some(c) = input.trim().chars().next() {
                                if c.is_ascii_digit() {
                                    self.play_macro(c);
                                } else {
                                    self.set_status_message(
                                        "Macro register must be 0-9".to_string(),
                                    );
                                }
                            } else {
                                self.set_status_message("No register specified".to_string());
                            }
                        }
                        PromptType::SetBookmark => {
                            if let Some(c) = input.trim().chars().next() {
                                if c.is_ascii_digit() {
                                    self.set_bookmark(c);
                                } else {
                                    self.set_status_message(
                                        "Bookmark register must be 0-9".to_string(),
                                    );
                                }
                            } else {
                                self.set_status_message("No register specified".to_string());
                            }
                        }
                        PromptType::JumpToBookmark => {
                            if let Some(c) = input.trim().chars().next() {
                                if c.is_ascii_digit() {
                                    self.jump_to_bookmark(c);
                                } else {
                                    self.set_status_message(
                                        "Bookmark register must be 0-9".to_string(),
                                    );
                                }
                            } else {
                                self.set_status_message("No register specified".to_string());
                            }
                        }
                        PromptType::Plugin { custom_type } => {
                            let hook_args = HookArgs::PromptConfirmed {
                                prompt_type: custom_type,
                                input,
                                selected_index,
                            };

                            if let Some(ref ts_manager) = self.ts_plugin_manager {
                                ts_manager.run_hook("prompt_confirmed", hook_args);
                            }
                        }
                        PromptType::LspRename {
                            original_text,
                            start_pos,
                            end_pos: _,
                            overlay_id,
                        } => {
                            // Perform LSP rename with the new name from the prompt input
                            self.perform_lsp_rename(input, original_text, start_pos, overlay_id);
                        }
                    }
                }
            }
            Action::PopupConfirm => {
                // If it's a completion popup, insert the selected item
                let completion_text = if let Some(popup) = self.active_state().popups.top() {
                    if let Some(title) = &popup.title {
                        if title == "Completion" {
                            if let Some(item) = popup.selected_item() {
                                item.data.clone()
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                };

                // Now perform the completion if we have text
                if let Some(text) = completion_text {
                    use crate::word_navigation::find_completion_word_start;

                    let (cursor_id, cursor_pos, word_start) = {
                        let state = self.active_state();
                        let cursor_id = state.cursors.primary_id();
                        let cursor_pos = state.cursors.primary().position;
                        let word_start = find_completion_word_start(&state.buffer, cursor_pos);
                        (cursor_id, cursor_pos, word_start)
                    };

                    let deleted_text = if word_start < cursor_pos {
                        self.active_state_mut()
                            .get_text_range(word_start, cursor_pos)
                    } else {
                        String::new()
                    };

                    if word_start < cursor_pos {
                        let delete_event = crate::event::Event::Delete {
                            range: word_start..cursor_pos,
                            deleted_text,
                            cursor_id,
                        };

                        self.active_event_log_mut().append(delete_event.clone());
                        self.apply_event_to_active_buffer(&delete_event);

                        let buffer_len = self.active_state().buffer.len();
                        let insert_pos = word_start.min(buffer_len);

                        let insert_event = crate::event::Event::Insert {
                            position: insert_pos,
                            text,
                            cursor_id,
                        };

                        self.active_event_log_mut().append(insert_event.clone());
                        self.apply_event_to_active_buffer(&insert_event);
                    } else {
                        let insert_event = crate::event::Event::Insert {
                            position: cursor_pos,
                            text,
                            cursor_id,
                        };

                        self.active_event_log_mut().append(insert_event.clone());
                        self.apply_event_to_active_buffer(&insert_event);
                    }
                }

                self.hide_popup();
            }
            Action::PopupCancel => {
                self.hide_popup();
            }
            Action::InsertChar(c) => {
                // Handle character insertion in interactive replace mode
                if self.interactive_replace_state.is_some() {
                    return self.handle_interactive_replace_key(c);
                // Handle character insertion in prompt mode
                } else if self.is_prompting() {
                    // Reset history navigation when user starts typing
                    // This allows them to press Up to get back to history items
                    if let Some(ref prompt) = self.prompt {
                        match &prompt.prompt_type {
                            PromptType::Search
                            | PromptType::ReplaceSearch
                            | PromptType::QueryReplaceSearch => {
                                self.search_history.reset_navigation();
                            }
                            PromptType::Replace { .. } | PromptType::QueryReplace { .. } => {
                                self.replace_history.reset_navigation();
                            }
                            _ => {}
                        }
                    }

                    if let Some(prompt) = self.prompt_mut() {
                        // Use insert_str to properly handle selection deletion
                        let s = c.to_string();
                        prompt.insert_str(&s);
                    }
                    self.update_prompt_suggestions();
                } else {
                    // Check if editing is disabled (show_cursors = false)
                    if self.is_editing_disabled() {
                        self.set_status_message("Editing disabled in this buffer".to_string());
                        return Ok(());
                    }
                    // Normal mode character insertion
                    // Cancel any pending LSP requests since the text is changing
                    self.cancel_pending_lsp_requests();

                    if let Some(events) = self.action_to_events(Action::InsertChar(c)) {
                        // Wrap multiple events (multi-cursor) in a Batch for atomic undo
                        if events.len() > 1 {
                            let batch = Event::Batch {
                                events: events.clone(),
                                description: format!("Insert '{}'", c),
                            };
                            self.active_event_log_mut().append(batch.clone());
                            self.apply_event_to_active_buffer(&batch);
                            // Note: LSP notifications now handled automatically by apply_event_to_active_buffer
                        } else {
                            // Single cursor - no need for batch
                            for event in events {
                                self.active_event_log_mut().append(event.clone());
                                self.apply_event_to_active_buffer(&event);
                                // Note: LSP notifications now handled automatically by apply_event_to_active_buffer
                            }
                        }
                    }

                    // Auto-trigger signature help on '(' and ','
                    if c == '(' || c == ',' {
                        let _ = self.request_signature_help();
                    }
                }
            }
            _ => {
                // Convert action to events and apply them
                // Get description before moving action
                let action_description = format!("{:?}", action);

                // Check if this is an editing action and editing is disabled
                let is_editing_action = matches!(
                    action,
                    Action::InsertNewline
                        | Action::InsertTab
                        | Action::DeleteForward
                        | Action::DeleteWordBackward
                        | Action::DeleteWordForward
                        | Action::DeleteLine
                        | Action::IndentSelection
                        | Action::DedentSelection
                        | Action::ToggleComment
                );

                if is_editing_action && self.is_editing_disabled() {
                    self.set_status_message("Editing disabled in this buffer".to_string());
                    return Ok(());
                }

                if let Some(events) = self.action_to_events(action) {
                    // Wrap multiple events (multi-cursor) in a Batch for atomic undo
                    if events.len() > 1 {
                        let batch = Event::Batch {
                            events: events.clone(),
                            description: action_description,
                        };
                        self.active_event_log_mut().append(batch.clone());
                        self.apply_event_to_active_buffer(&batch);
                        // Note: LSP notifications now handled automatically by apply_event_to_active_buffer

                        // Track position history for all events in the batch
                        for event in &events {
                            // Track cursor movements in position history (but not during navigation)
                            if !self.in_navigation {
                                if let Event::MoveCursor {
                                    new_position,
                                    new_anchor,
                                    ..
                                } = event
                                {
                                    self.position_history.record_movement(
                                        self.active_buffer,
                                        *new_position,
                                        *new_anchor,
                                    );
                                }
                            }
                        }
                    } else {
                        // Single cursor - no need for batch
                        for event in events {
                            self.active_event_log_mut().append(event.clone());
                            self.apply_event_to_active_buffer(&event);
                            // Note: LSP notifications now handled automatically by apply_event_to_active_buffer

                            // Track cursor movements in position history (but not during navigation)
                            if !self.in_navigation {
                                if let Event::MoveCursor {
                                    new_position,
                                    new_anchor,
                                    ..
                                } = event
                                {
                                    self.position_history.record_movement(
                                        self.active_buffer,
                                        new_position,
                                        new_anchor,
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Handle a mouse event
    pub fn handle_mouse(
        &mut self,
        mouse_event: crossterm::event::MouseEvent,
    ) -> std::io::Result<()> {
        use crossterm::event::{MouseButton, MouseEventKind};

        // Cancel LSP rename prompt on any mouse interaction
        if let Some(ref prompt) = self.prompt {
            if matches!(prompt.prompt_type, PromptType::LspRename { .. }) {
                self.cancel_prompt();
            }
        }

        let col = mouse_event.column;
        let row = mouse_event.row;

        tracing::debug!(
            "handle_mouse: kind={:?}, col={}, row={}",
            mouse_event.kind,
            col,
            row
        );

        match mouse_event.kind {
            MouseEventKind::Down(MouseButton::Left) => {
                self.handle_mouse_click(col, row)?;
            }
            MouseEventKind::Drag(MouseButton::Left) => {
                self.handle_mouse_drag(col, row)?;
            }
            MouseEventKind::Up(MouseButton::Left) => {
                // Stop dragging and clear drag state
                self.mouse_state.dragging_scrollbar = None;
                self.mouse_state.drag_start_row = None;
                self.mouse_state.drag_start_top_byte = None;
                self.mouse_state.dragging_separator = None;
                self.mouse_state.drag_start_position = None;
                self.mouse_state.drag_start_ratio = None;
            }
            MouseEventKind::Moved => {
                self.update_hover_target(col, row);
            }
            MouseEventKind::ScrollUp => {
                // Dismiss hover/signature help popups on scroll
                self.dismiss_transient_popups();
                self.handle_mouse_scroll(col, row, -3)?;
            }
            MouseEventKind::ScrollDown => {
                // Dismiss hover/signature help popups on scroll
                self.dismiss_transient_popups();
                self.handle_mouse_scroll(col, row, 3)?;
            }
            _ => {
                // Ignore other mouse events for now
            }
        }

        self.mouse_state.last_position = Some((col, row));
        Ok(())
    }

    /// Update the current hover target based on mouse position
    pub(super) fn update_hover_target(&mut self, col: u16, row: u16) {
        // Check suggestions area first (command palette, autocomplete)
        if let Some((inner_rect, start_idx, _visible_count, total_count)) =
            &self.cached_layout.suggestions_area
        {
            if col >= inner_rect.x
                && col < inner_rect.x + inner_rect.width
                && row >= inner_rect.y
                && row < inner_rect.y + inner_rect.height
            {
                let relative_row = (row - inner_rect.y) as usize;
                let item_idx = start_idx + relative_row;

                if item_idx < *total_count {
                    self.mouse_state.hover_target = Some(HoverTarget::SuggestionItem(item_idx));
                    return;
                }
            }
        }

        // Check popups (they're rendered on top)
        // Check from top to bottom (reverse order since last popup is on top)
        for (popup_idx, _popup_rect, inner_rect, scroll_offset, num_items) in
            self.cached_layout.popup_areas.iter().rev()
        {
            if col >= inner_rect.x
                && col < inner_rect.x + inner_rect.width
                && row >= inner_rect.y
                && row < inner_rect.y + inner_rect.height
                && *num_items > 0
            {
                // Calculate which item is being hovered
                let relative_row = (row - inner_rect.y) as usize;
                let item_idx = scroll_offset + relative_row;

                if item_idx < *num_items {
                    self.mouse_state.hover_target =
                        Some(HoverTarget::PopupListItem(*popup_idx, item_idx));
                    return;
                }
            }
        }

        // Check menu bar (row 0)
        if row == 0 {
            let all_menus: Vec<crate::config::Menu> = self
                .config
                .menu
                .menus
                .iter()
                .chain(self.menu_state.plugin_menus.iter())
                .cloned()
                .collect();

            if let Some(menu_idx) = self.menu_state.get_menu_at_position(&all_menus, col) {
                self.mouse_state.hover_target = Some(HoverTarget::MenuBarItem(menu_idx));
                return;
            }
        }

        // Check menu dropdown items if a menu is open
        if let Some(active_idx) = self.menu_state.active_menu {
            let all_menus: Vec<crate::config::Menu> = self
                .config
                .menu
                .menus
                .iter()
                .chain(self.menu_state.plugin_menus.iter())
                .cloned()
                .collect();

            if let Some(menu) = all_menus.get(active_idx) {
                if let Some(item_idx) = self.menu_state.get_item_at_position(menu, row) {
                    self.mouse_state.hover_target =
                        Some(HoverTarget::MenuDropdownItem(active_idx, item_idx));
                    return;
                }
            }
        }

        // Check split separators
        for (split_id, direction, sep_x, sep_y, sep_length) in &self.cached_layout.separator_areas {
            let is_on_separator = match direction {
                SplitDirection::Horizontal => {
                    row == *sep_y && col >= *sep_x && col < sep_x + sep_length
                }
                SplitDirection::Vertical => {
                    col == *sep_x && row >= *sep_y && row < sep_y + sep_length
                }
            };

            if is_on_separator {
                self.mouse_state.hover_target =
                    Some(HoverTarget::SplitSeparator(*split_id, *direction));
                return;
            }
        }

        // Check scrollbars
        for (split_id, _buffer_id, _content_rect, scrollbar_rect, thumb_start, thumb_end) in
            &self.cached_layout.split_areas
        {
            if col >= scrollbar_rect.x
                && col < scrollbar_rect.x + scrollbar_rect.width
                && row >= scrollbar_rect.y
                && row < scrollbar_rect.y + scrollbar_rect.height
            {
                let relative_row = row.saturating_sub(scrollbar_rect.y) as usize;
                let is_on_thumb = relative_row >= *thumb_start && relative_row < *thumb_end;

                if is_on_thumb {
                    self.mouse_state.hover_target = Some(HoverTarget::ScrollbarThumb(*split_id));
                } else {
                    self.mouse_state.hover_target = Some(HoverTarget::ScrollbarTrack(*split_id));
                }
                return;
            }
        }

        // No hover target
        self.mouse_state.hover_target = None;
    }

    /// Handle mouse click (down event)
    pub(super) fn handle_mouse_click(&mut self, col: u16, row: u16) -> std::io::Result<()> {
        // Check if click is on suggestions (command palette, autocomplete)
        if let Some((inner_rect, start_idx, _visible_count, total_count)) =
            &self.cached_layout.suggestions_area.clone()
        {
            if col >= inner_rect.x
                && col < inner_rect.x + inner_rect.width
                && row >= inner_rect.y
                && row < inner_rect.y + inner_rect.height
            {
                let relative_row = (row - inner_rect.y) as usize;
                let item_idx = start_idx + relative_row;

                if item_idx < *total_count {
                    // Select and execute the clicked suggestion
                    if let Some(prompt) = &mut self.prompt {
                        prompt.selected_suggestion = Some(item_idx);
                    }
                    // Execute the suggestion (same as pressing Enter)
                    return self.handle_action(Action::PromptConfirm);
                }
            }
        }

        // Check if click is on a popup (they're rendered on top)
        for (_popup_idx, _popup_rect, inner_rect, scroll_offset, num_items) in
            self.cached_layout.popup_areas.iter().rev()
        {
            if col >= inner_rect.x
                && col < inner_rect.x + inner_rect.width
                && row >= inner_rect.y
                && row < inner_rect.y + inner_rect.height
                && *num_items > 0
            {
                // Calculate which item was clicked
                let relative_row = (row - inner_rect.y) as usize;
                let item_idx = scroll_offset + relative_row;

                if item_idx < *num_items {
                    // Select and execute the clicked item
                    let state = self.active_state_mut();
                    if let Some(popup) = state.popups.top_mut() {
                        if let crate::popup::PopupContent::List { items: _, selected } =
                            &mut popup.content
                        {
                            *selected = item_idx;
                        }
                    }
                    // Execute the popup selection (same as pressing Enter)
                    return self.handle_action(Action::PopupConfirm);
                }
            }
        }

        // Check if click is on menu bar (row 0)
        if row == 0 {
            let all_menus: Vec<crate::config::Menu> = self
                .config
                .menu
                .menus
                .iter()
                .chain(self.menu_state.plugin_menus.iter())
                .cloned()
                .collect();

            if let Some(menu_idx) = self.menu_state.get_menu_at_position(&all_menus, col) {
                // Toggle menu: if same menu is open, close it; otherwise open clicked menu
                if self.menu_state.active_menu == Some(menu_idx) {
                    self.menu_state.close_menu();
                } else {
                    self.menu_state.open_menu(menu_idx);
                }
            } else {
                // Clicked on menu bar but not on a menu label - close any open menu
                self.menu_state.close_menu();
            }
            return Ok(());
        }

        // Check if click is on an open menu dropdown
        if let Some(active_idx) = self.menu_state.active_menu {
            let all_menus: Vec<crate::config::Menu> = self
                .config
                .menu
                .menus
                .iter()
                .chain(self.menu_state.plugin_menus.iter())
                .cloned()
                .collect();

            if let Some(menu) = all_menus.get(active_idx) {
                // Calculate menu dropdown bounds
                // Menu position: sum of widths of all menus before this one
                let mut menu_x = 0u16;
                for m in all_menus.iter().take(active_idx) {
                    menu_x += m.label.len() as u16 + 3; // " Label " + trailing space
                }

                // Find the widest item to determine dropdown width
                let max_label_len = menu
                    .items
                    .iter()
                    .map(|item| match item {
                        crate::config::MenuItem::Action { label, .. } => label.len(),
                        crate::config::MenuItem::Separator { .. } => 0,
                        crate::config::MenuItem::Submenu { label, .. } => label.len(),
                    })
                    .max()
                    .unwrap_or(0);
                let dropdown_width = max_label_len + 30; // Label + padding + keybinding space

                // Dropdown starts at row 1 (below menu bar), with border at row 1
                // Items start at row 2, and there's a border at the bottom
                let dropdown_height = menu.items.len() as u16 + 2; // items + top/bottom border

                // Check if click is inside dropdown bounds
                if col >= menu_x
                    && col < menu_x + dropdown_width as u16
                    && row >= 1
                    && row < 1 + dropdown_height
                {
                    // Check if click is on an item (not border)
                    if let Some(item_idx) = self.menu_state.get_item_at_position(menu, row) {
                        // Execute the menu item action
                        if let Some(crate::config::MenuItem::Action { action, args, .. }) =
                            menu.items.get(item_idx)
                        {
                            let action_name = action.clone();
                            let action_args = args.clone();

                            // Close the menu first
                            self.menu_state.close_menu();

                            // Parse and execute the action
                            if let Some(action) = Action::from_str(&action_name, &action_args) {
                                return self.handle_action(action);
                            }
                        }
                    }
                    return Ok(());
                }
            }

            // Click outside the dropdown - close the menu
            self.menu_state.close_menu();
            return Ok(());
        }

        // Check if click is on file explorer
        if let Some(explorer_area) = self.cached_layout.file_explorer_area {
            if col >= explorer_area.x
                && col < explorer_area.x + explorer_area.width
                && row >= explorer_area.y
                && row < explorer_area.y + explorer_area.height
            {
                self.handle_file_explorer_click(col, row, explorer_area)?;
                return Ok(());
            }
        }

        // Check if click is on a scrollbar
        let scrollbar_hit = self.cached_layout.split_areas.iter().find_map(
            |(split_id, buffer_id, _content_rect, scrollbar_rect, thumb_start, thumb_end)| {
                if col >= scrollbar_rect.x
                    && col < scrollbar_rect.x + scrollbar_rect.width
                    && row >= scrollbar_rect.y
                    && row < scrollbar_rect.y + scrollbar_rect.height
                {
                    let relative_row = row.saturating_sub(scrollbar_rect.y) as usize;
                    let is_on_thumb = relative_row >= *thumb_start && relative_row < *thumb_end;
                    Some((*split_id, *buffer_id, *scrollbar_rect, is_on_thumb))
                } else {
                    None
                }
            },
        );

        if let Some((split_id, buffer_id, scrollbar_rect, is_on_thumb)) = scrollbar_hit {
            // Focus this split
            self.split_manager.set_active_split(split_id);
            if buffer_id != self.active_buffer {
                self.position_history.commit_pending_movement();
                self.set_active_buffer(buffer_id);
            }

            if is_on_thumb {
                // Click on thumb - start drag from current position (don't jump)
                self.mouse_state.dragging_scrollbar = Some(split_id);
                self.mouse_state.drag_start_row = Some(row);
                // Record the current viewport position
                if let Some(state) = self.buffers.get(&buffer_id) {
                    self.mouse_state.drag_start_top_byte = Some(state.viewport.top_byte);
                }
            } else {
                // Click on track - jump to position
                self.mouse_state.dragging_scrollbar = Some(split_id);
                self.handle_scrollbar_jump(col, row, buffer_id, scrollbar_rect)?;
            }
            return Ok(());
        }

        // Check if click is on a split separator (for drag resizing)
        for (split_id, direction, sep_x, sep_y, sep_length) in &self.cached_layout.separator_areas {
            let is_on_separator = match direction {
                SplitDirection::Horizontal => {
                    // Horizontal separator: spans full width at a specific y
                    row == *sep_y && col >= *sep_x && col < sep_x + sep_length
                }
                SplitDirection::Vertical => {
                    // Vertical separator: spans full height at a specific x
                    col == *sep_x && row >= *sep_y && row < sep_y + sep_length
                }
            };

            if is_on_separator {
                // Start separator drag
                self.mouse_state.dragging_separator = Some((*split_id, *direction));
                self.mouse_state.drag_start_position = Some((col, row));
                // Store the initial ratio
                if let Some(ratio) = self.split_manager.get_ratio(*split_id) {
                    self.mouse_state.drag_start_ratio = Some(ratio);
                }
                return Ok(());
            }
        }

        // Check if click is in tab bar area (1 row above content_rect)
        let tab_hit = self.cached_layout.split_areas.iter().find_map(
            |(split_id, buffer_id, content_rect, scrollbar_rect, _thumb_start, _thumb_end)| {
                // Tab bar is 1 row high, above content_rect, spanning content + scrollbar width
                let tabs_y = content_rect.y.saturating_sub(1);
                let tabs_width = content_rect.width + scrollbar_rect.width;
                if row == tabs_y && col >= content_rect.x && col < content_rect.x + tabs_width {
                    Some((*split_id, *buffer_id, content_rect.x))
                } else {
                    None
                }
            },
        );

        if let Some((split_id, _current_buffer_id, tabs_x)) = tab_hit {
            // Focus this split when clicking on its tab bar
            self.split_manager.set_active_split(split_id);

            // Determine which tab was clicked based on position
            // Get the open buffers for this split
            let clicked_buffer = if let Some(view_state) = self.split_view_states.get(&split_id) {
                let relative_x = col.saturating_sub(tabs_x) as usize;
                let mut current_x = 0usize;

                let mut found_buffer = None;
                for buffer_id in &view_state.open_buffers {
                    // Calculate tab width: " {name}{modified} " + separator " "
                    let tab_width = if let Some(state) = self.buffers.get(buffer_id) {
                        let name = if let Some(metadata) = self.buffer_metadata.get(buffer_id) {
                            metadata.display_name.as_str()
                        } else {
                            state
                                .buffer
                                .file_path()
                                .and_then(|p| p.file_name())
                                .and_then(|n| n.to_str())
                                .unwrap_or("[No Name]")
                        };
                        let modified = if state.buffer.is_modified() { 1 } else { 0 };
                        // " {name}{modified} " = 2 + name.len() + modified
                        2 + name.chars().count() + modified
                    } else {
                        continue;
                    };

                    if relative_x >= current_x && relative_x < current_x + tab_width {
                        found_buffer = Some(*buffer_id);
                        break;
                    }

                    // Move past this tab and separator (1 char)
                    current_x += tab_width + 1;
                }
                found_buffer
            } else {
                None
            };

            // Switch to the clicked buffer if found, otherwise just focus the split
            let target_buffer = clicked_buffer.unwrap_or(_current_buffer_id);
            if target_buffer != self.active_buffer {
                self.position_history.commit_pending_movement();
                self.set_active_buffer(target_buffer);
            }
            return Ok(());
        }

        // Check if click is in editor content area
        for (split_id, buffer_id, content_rect, _scrollbar_rect, _thumb_start, _thumb_end) in
            &self.cached_layout.split_areas
        {
            if col >= content_rect.x
                && col < content_rect.x + content_rect.width
                && row >= content_rect.y
                && row < content_rect.y + content_rect.height
            {
                // Click in editor - focus split and position cursor
                self.handle_editor_click(col, row, *split_id, *buffer_id, *content_rect)?;
                return Ok(());
            }
        }

        Ok(())
    }

    /// Handle mouse drag event
    pub(super) fn handle_mouse_drag(&mut self, col: u16, row: u16) -> std::io::Result<()> {
        // If dragging scrollbar, update scroll position
        if let Some(dragging_split_id) = self.mouse_state.dragging_scrollbar {
            // Find the buffer and scrollbar rect for this split
            for (split_id, buffer_id, _content_rect, scrollbar_rect, _thumb_start, _thumb_end) in
                &self.cached_layout.split_areas
            {
                if *split_id == dragging_split_id {
                    // Check if we started dragging from the thumb (have drag_start_row)
                    if self.mouse_state.drag_start_row.is_some() {
                        // Relative drag from thumb
                        self.handle_scrollbar_drag_relative(row, *buffer_id, *scrollbar_rect)?;
                    } else {
                        // Jump drag (started from track)
                        self.handle_scrollbar_jump(col, row, *buffer_id, *scrollbar_rect)?;
                    }
                    return Ok(());
                }
            }
        }

        // If dragging separator, update split ratio
        if let Some((split_id, direction)) = self.mouse_state.dragging_separator {
            self.handle_separator_drag(col, row, split_id, direction)?;
            return Ok(());
        }

        Ok(())
    }

    /// Handle separator drag for split resizing
    pub(super) fn handle_separator_drag(
        &mut self,
        col: u16,
        row: u16,
        split_id: SplitId,
        direction: SplitDirection,
    ) -> std::io::Result<()> {
        let Some((start_col, start_row)) = self.mouse_state.drag_start_position else {
            return Ok(());
        };
        let Some(start_ratio) = self.mouse_state.drag_start_ratio else {
            return Ok(());
        };
        let Some(editor_area) = self.cached_layout.editor_content_area else {
            return Ok(());
        };

        // Calculate the delta in screen space
        let (delta, total_size) = match direction {
            SplitDirection::Horizontal => {
                // For horizontal splits, we move the separator up/down (row changes)
                let delta = row as i32 - start_row as i32;
                let total = editor_area.height as i32;
                (delta, total)
            }
            SplitDirection::Vertical => {
                // For vertical splits, we move the separator left/right (col changes)
                let delta = col as i32 - start_col as i32;
                let total = editor_area.width as i32;
                (delta, total)
            }
        };

        // Convert screen delta to ratio delta
        // The ratio represents the fraction of space the first split gets
        if total_size > 0 {
            let ratio_delta = delta as f32 / total_size as f32;
            let new_ratio = (start_ratio + ratio_delta).clamp(0.1, 0.9);

            // Update the split ratio
            let _ = self.split_manager.set_ratio(split_id, new_ratio);
        }

        Ok(())
    }

    /// Handle mouse wheel scroll event
    pub(super) fn handle_mouse_scroll(
        &mut self,
        col: u16,
        row: u16,
        delta: i32,
    ) -> std::io::Result<()> {
        // Check if scroll is over the file explorer
        if let Some(explorer_area) = self.cached_layout.file_explorer_area {
            if col >= explorer_area.x
                && col < explorer_area.x + explorer_area.width
                && row >= explorer_area.y
                && row < explorer_area.y + explorer_area.height
            {
                // Scroll the file explorer
                if let Some(explorer) = &mut self.file_explorer {
                    let visible = explorer.tree().get_visible_nodes();
                    if visible.is_empty() {
                        return Ok(());
                    }

                    // Get current selected index
                    let current_index = explorer.get_selected_index().unwrap_or(0);

                    // Calculate new index based on scroll delta
                    let new_index = if delta < 0 {
                        // Scroll up (negative delta)
                        current_index.saturating_sub(delta.abs() as usize)
                    } else {
                        // Scroll down (positive delta)
                        (current_index + delta as usize).min(visible.len() - 1)
                    };

                    // Set the new selection
                    if let Some(node_id) = explorer.get_node_at_index(new_index) {
                        explorer.set_selected(Some(node_id));
                        explorer.update_scroll_for_selection();
                    }
                }
                return Ok(());
            }
        }

        // Otherwise, scroll the editor in the active split
        if let Some(state) = self.buffers.get_mut(&self.active_buffer) {
            // Scroll the viewport by the delta amount
            if delta < 0 {
                // Scroll up
                let lines_to_scroll = delta.abs() as usize;
                state.viewport.scroll_up(&mut state.buffer, lines_to_scroll);
            } else {
                // Scroll down
                let lines_to_scroll = delta as usize;
                state
                    .viewport
                    .scroll_down(&mut state.buffer, lines_to_scroll);
            }
        }

        Ok(())
    }

    /// Handle scrollbar drag with relative movement (when dragging from thumb)
    pub(super) fn handle_scrollbar_drag_relative(
        &mut self,
        row: u16,
        buffer_id: BufferId,
        scrollbar_rect: ratatui::layout::Rect,
    ) -> std::io::Result<()> {
        let drag_start_row = match self.mouse_state.drag_start_row {
            Some(r) => r,
            None => return Ok(()), // No drag start, shouldn't happen
        };

        let drag_start_top_byte = match self.mouse_state.drag_start_top_byte {
            Some(b) => b,
            None => return Ok(()), // No drag start, shouldn't happen
        };

        // Calculate the offset in rows
        let row_offset = (row as i32) - (drag_start_row as i32);

        // Get the buffer state
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let scrollbar_height = scrollbar_rect.height as usize;
            if scrollbar_height == 0 {
                return Ok(());
            }

            let buffer_len = state.buffer.len();
            let large_file_threshold = self.config.editor.large_file_threshold_bytes as usize;
            let viewport_height = state.viewport.height as usize;

            // For small files, use precise line-based calculations
            // For large files, fall back to byte-based estimation
            let new_top_byte = if buffer_len <= large_file_threshold {
                // Small file: use line-based calculation for precision
                // Count total lines
                let total_lines = if buffer_len > 0 {
                    state.buffer.get_line_number(buffer_len.saturating_sub(1)) + 1
                } else {
                    1
                };

                // Calculate max scroll line
                let max_scroll_line = total_lines.saturating_sub(viewport_height);

                if max_scroll_line == 0 {
                    // File fits in viewport, no scrolling
                    0
                } else {
                    // Calculate which line the mouse position corresponds to using linear interpolation
                    // Convert absolute mouse row to relative position within scrollbar
                    let relative_mouse_row = row.saturating_sub(scrollbar_rect.y) as usize;
                    // Divide by (height - 1) to map first row to 0.0 and last row to 1.0
                    let scroll_ratio = if scrollbar_height > 1 {
                        (relative_mouse_row as f64 / (scrollbar_height - 1) as f64).clamp(0.0, 1.0)
                    } else {
                        0.0
                    };

                    // Map scroll ratio to target line
                    let target_line = (scroll_ratio * max_scroll_line as f64).round() as usize;
                    let target_line = target_line.min(max_scroll_line);

                    // Find byte position of target line
                    // We need to iterate 'target_line' times to skip past lines 0..target_line-1,
                    // then one more time to get the position of line 'target_line'
                    let mut iter = state.buffer.line_iterator(0, 80);
                    let mut line_byte = 0;

                    for _ in 0..target_line {
                        if let Some((pos, _content)) = iter.next() {
                            line_byte = pos;
                        } else {
                            break;
                        }
                    }

                    // Get the position of the target line
                    if let Some((pos, _)) = iter.next() {
                        pos
                    } else {
                        line_byte // Reached end of buffer
                    }
                }
            } else {
                // Large file: use byte-based estimation (original logic)
                let bytes_per_pixel = buffer_len as f64 / scrollbar_height as f64;
                let byte_offset = (row_offset as f64 * bytes_per_pixel) as i64;

                let new_top_byte = if byte_offset >= 0 {
                    drag_start_top_byte.saturating_add(byte_offset as usize)
                } else {
                    drag_start_top_byte.saturating_sub((-byte_offset) as usize)
                };

                // Clamp to valid range using byte-based max (avoid iterating entire buffer)
                new_top_byte.min(buffer_len.saturating_sub(1))
            };

            // Find the line start for this byte position
            let iter = state.buffer.line_iterator(new_top_byte, 80);
            let line_start = iter.current_position();

            // Set viewport top to this position
            state.viewport.top_byte = line_start;
        }

        // Move cursor to be visible in the new viewport (after releasing the state borrow)
        self.move_cursor_to_visible_area(buffer_id);

        Ok(())
    }

    /// Handle scrollbar jump (clicking on track or absolute positioning)
    pub(super) fn handle_scrollbar_jump(
        &mut self,
        _col: u16,
        row: u16,
        buffer_id: BufferId,
        scrollbar_rect: ratatui::layout::Rect,
    ) -> std::io::Result<()> {
        // Calculate which line to scroll to based on mouse position
        let scrollbar_height = scrollbar_rect.height as usize;
        if scrollbar_height == 0 {
            return Ok(());
        }

        // Get relative position in scrollbar (0.0 to 1.0)
        // Divide by (height - 1) to map first row to 0.0 and last row to 1.0
        let relative_row = row.saturating_sub(scrollbar_rect.y);
        let ratio = if scrollbar_height > 1 {
            ((relative_row as f64) / ((scrollbar_height - 1) as f64)).clamp(0.0, 1.0)
        } else {
            0.0
        };

        // Get the buffer state
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let buffer_len = state.buffer.len();
            let large_file_threshold = self.config.editor.large_file_threshold_bytes as usize;
            let viewport_height = state.viewport.height as usize;

            // For small files, use precise line-based calculations
            // For large files, fall back to byte-based estimation
            let target_byte = if buffer_len <= large_file_threshold {
                // Small file: use line-based calculation for precision
                let total_lines = if buffer_len > 0 {
                    state.buffer.get_line_number(buffer_len.saturating_sub(1)) + 1
                } else {
                    1
                };

                let max_scroll_line = total_lines.saturating_sub(viewport_height);

                if max_scroll_line == 0 {
                    // File fits in viewport, no scrolling
                    0
                } else {
                    // Map ratio to target line
                    let target_line = (ratio * max_scroll_line as f64).round() as usize;
                    let target_line = target_line.min(max_scroll_line);

                    // Find byte position of target line
                    // We need to iterate 'target_line' times to skip past lines 0..target_line-1,
                    // then one more time to get the position of line 'target_line'
                    let mut iter = state.buffer.line_iterator(0, 80);
                    let mut line_byte = 0;

                    for _ in 0..target_line {
                        if let Some((pos, _content)) = iter.next() {
                            line_byte = pos;
                        } else {
                            break;
                        }
                    }

                    // Get the position of the target line
                    if let Some((pos, _)) = iter.next() {
                        pos
                    } else {
                        line_byte // Reached end of buffer
                    }
                }
            } else {
                // Large file: use byte-based estimation (original logic)
                let target_byte = (buffer_len as f64 * ratio) as usize;
                target_byte.min(buffer_len.saturating_sub(1))
            };

            // Find the line start for this byte position
            let iter = state.buffer.line_iterator(target_byte, 80);
            let line_start = iter.current_position();

            // Apply scroll limiting
            // Use viewport.height (constant allocated rows) not visible_line_count (varies with content)
            // For large files, use byte-based max to avoid iterating entire buffer
            let max_top_byte = if buffer_len <= large_file_threshold {
                Self::calculate_max_scroll_position(&mut state.buffer, viewport_height)
            } else {
                buffer_len.saturating_sub(1)
            };
            let limited_line_start = line_start.min(max_top_byte);

            // Set viewport top to this position
            state.viewport.top_byte = limited_line_start;
        }

        // Move cursor to be visible in the new viewport (after releasing the state borrow)
        self.move_cursor_to_visible_area(buffer_id);

        Ok(())
    }

    /// Move the cursor to a visible position within the current viewport
    /// This is called after scrollbar operations to ensure the cursor is in view
    pub(super) fn move_cursor_to_visible_area(&mut self, buffer_id: BufferId) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let top_byte = state.viewport.top_byte;
            let viewport_height = state.viewport.height as usize;
            let buffer_len = state.buffer.len();

            // Find the bottom byte of the viewport
            // We iterate through viewport_height lines starting from top_byte
            let mut iter = state.buffer.line_iterator(top_byte, 80);
            let mut bottom_byte = buffer_len;

            // Consume viewport_height lines to find where the visible area ends
            for _ in 0..viewport_height {
                if let Some((pos, line)) = iter.next() {
                    // The bottom of this line is at pos + line.len()
                    bottom_byte = pos + line.len();
                } else {
                    // Reached end of buffer
                    bottom_byte = buffer_len;
                    break;
                }
            }

            // Check if cursor is outside visible range and move it if needed
            let cursor_pos = state.cursors.primary().position;
            if cursor_pos < top_byte || cursor_pos > bottom_byte {
                // Move cursor to the top of the viewport
                let cursor = state.cursors.primary_mut();
                cursor.position = top_byte;
                // Keep the existing sticky_column value so vertical navigation preserves column
            }
        }
    }

    /// Calculate the maximum allowed scroll position
    /// Ensures the last line is always at the bottom unless the buffer is smaller than viewport
    pub(super) fn calculate_max_scroll_position(
        buffer: &mut crate::text_buffer::Buffer,
        viewport_height: usize,
    ) -> usize {
        if viewport_height == 0 {
            return 0;
        }

        let buffer_len = buffer.len();
        if buffer_len == 0 {
            return 0;
        }

        // Count total lines in buffer
        let mut line_count = 0;
        let mut iter = buffer.line_iterator(0, 80);
        while iter.next().is_some() {
            line_count += 1;
        }

        // If buffer has fewer lines than viewport, can't scroll at all
        if line_count <= viewport_height {
            return 0;
        }

        // Calculate how many lines from the start we can scroll
        // We want to be able to scroll so that the last line is at the bottom
        let scrollable_lines = line_count.saturating_sub(viewport_height);

        // Find the byte position of the line at scrollable_lines offset
        let mut iter = buffer.line_iterator(0, 80);
        let mut current_line = 0;
        let mut max_byte_pos = 0;

        while current_line < scrollable_lines {
            if let Some((pos, _content)) = iter.next() {
                max_byte_pos = pos;
                current_line += 1;
            } else {
                break;
            }
        }

        max_byte_pos
    }

    /// Handle click in editor content area
    pub(super) fn handle_editor_click(
        &mut self,
        col: u16,
        row: u16,
        split_id: crate::event::SplitId,
        buffer_id: BufferId,
        content_rect: ratatui::layout::Rect,
    ) -> std::io::Result<()> {
        use crate::event::Event;

        // Focus this split
        self.split_manager.set_active_split(split_id);
        if buffer_id != self.active_buffer {
            self.position_history.commit_pending_movement();
            self.set_active_buffer(buffer_id);
        }

        // Calculate clicked position in buffer
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            // Account for left margin (line numbers)
            let gutter_width = state.margins.left_total_width() as u16;

            // Calculate relative position in content area
            let content_col = col.saturating_sub(content_rect.x);
            let content_row = row.saturating_sub(content_rect.y);

            // Skip if click is in the gutter
            if content_col < gutter_width {
                return Ok(());
            }

            // Adjust for gutter
            let text_col = content_col.saturating_sub(gutter_width);

            // Account for horizontal scroll
            let actual_col = (text_col as usize) + state.viewport.left_column;

            // Find the byte position for this line and column
            let mut line_iter = state.buffer.line_iterator(state.viewport.top_byte, 80);

            // Navigate to the clicked line
            let mut line_start = state.viewport.top_byte;
            let target_position;
            for _ in 0..content_row {
                if let Some((pos, _content)) = line_iter.next() {
                    line_start = pos;
                } else {
                    break;
                }
            }

            // Get the content of the target line
            if let Some((pos, line_content)) = line_iter.next() {
                line_start = pos;
                // Calculate byte offset within the line by iterating through characters
                // to properly handle multi-byte UTF-8 characters
                let mut byte_offset = 0;
                let mut col_count = 0;
                for ch in line_content.chars() {
                    if col_count >= actual_col {
                        break;
                    }
                    byte_offset += ch.len_utf8();
                    col_count += 1;
                }
                target_position = line_start + byte_offset;
            } else {
                // If we're past the last line, use the line start
                target_position = line_start;
            }

            // Move the primary cursor to this position
            let primary_cursor_id = state.cursors.primary_id();
            let event = Event::MoveCursor {
                cursor_id: primary_cursor_id,
                old_position: 0, // TODO: Get actual old position
                new_position: target_position,
                old_anchor: None, // TODO: Get actual old anchor
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0, // Reset sticky column for goto line
            };

            // Apply the event
            if let Some(event_log) = self.event_logs.get_mut(&buffer_id) {
                event_log.append(event.clone());
            }
            state.apply(&event);

            // Track position history
            if !self.in_navigation {
                self.position_history
                    .record_movement(buffer_id, target_position, None);
            }
        }

        Ok(())
    }

    /// Handle click in file explorer
    pub(super) fn handle_file_explorer_click(
        &mut self,
        _col: u16,
        row: u16,
        explorer_area: ratatui::layout::Rect,
    ) -> std::io::Result<()> {
        // Focus file explorer
        self.key_context = crate::keybindings::KeyContext::FileExplorer;

        // Calculate which item was clicked (accounting for border and title)
        // The file explorer has a 1-line border at top and bottom
        let relative_row = row.saturating_sub(explorer_area.y + 1); // +1 for top border

        if let Some(ref mut explorer) = self.file_explorer {
            let display_nodes = explorer.get_display_nodes();
            let scroll_offset = explorer.get_scroll_offset();
            let clicked_index = (relative_row as usize) + scroll_offset;

            if clicked_index < display_nodes.len() {
                let (node_id, _indent) = display_nodes[clicked_index];

                // Select this node
                explorer.set_selected(Some(node_id));

                // Check if it's a file or directory
                let node = explorer.tree().get_node(node_id);
                if let Some(node) = node {
                    if node.is_dir() {
                        // Toggle expand/collapse using the existing method
                        self.file_explorer_toggle_expand();
                    } else if node.is_file() {
                        // Open the file using the existing method
                        self.file_explorer_open_file()?;
                        // Switch focus back to editor after opening file
                        self.key_context = crate::keybindings::KeyContext::Normal;
                    }
                }
            }
        }

        Ok(())
    }
}
