//! Input dispatch using the hierarchical InputHandler system.
//!
//! This module provides the bridge between Editor and the InputHandler trait,
//! dispatching input to modal components and processing deferred actions.

use super::terminal_input::{should_enter_terminal_mode, TerminalModeInputHandler};
use super::Editor;
use crate::input::handler::{DeferredAction, InputContext, InputHandler, InputResult};
use crate::input::keybindings::Action;
use crate::view::file_browser_input::FileBrowserInputHandler;
use crate::view::query_replace_input::QueryReplaceConfirmInputHandler;
use crate::view::ui::MenuInputHandler;
use anyhow::Result as AnyhowResult;
use crossterm::event::KeyEvent;
use rust_i18n::t;

impl Editor {
    /// Dispatch input when in terminal mode.
    ///
    /// Returns `Some(InputResult)` if terminal mode handled the input,
    /// `None` if not in terminal mode or if a modal is active.
    pub fn dispatch_terminal_input(&mut self, event: &KeyEvent) -> Option<InputResult> {
        // Skip if we're in a prompt/popup (those need to handle keys normally)
        let in_modal = self.is_prompting()
            || self.active_state().popups.is_visible()
            || self.menu_state.active_menu.is_some()
            || self.settings_state.as_ref().is_some_and(|s| s.visible)
            || self.calibration_wizard.is_some();

        if in_modal {
            return None;
        }

        // Handle terminal mode input
        if self.terminal_mode {
            let mut ctx = InputContext::new();
            let mut handler =
                TerminalModeInputHandler::new(self.keyboard_capture, &self.keybindings);
            let result = handler.dispatch_input(event, &mut ctx);
            self.process_deferred_actions(ctx);
            return Some(result);
        }

        // Check for keys that should re-enter terminal mode from read-only view
        if self.is_terminal_buffer(self.active_buffer()) && should_enter_terminal_mode(event) {
            self.enter_terminal_mode();
            return Some(InputResult::Consumed);
        }

        None
    }

    /// Dispatch input to the appropriate modal handler.
    ///
    /// Returns `Some(InputResult)` if a modal handled the input,
    /// `None` if no modal is active and input should be handled normally.
    pub fn dispatch_modal_input(&mut self, event: &KeyEvent) -> Option<InputResult> {
        let mut ctx = InputContext::new();

        // Settings has highest priority
        if let Some(ref mut settings) = self.settings_state {
            if settings.visible {
                let result = settings.dispatch_input(event, &mut ctx);
                self.process_deferred_actions(ctx);
                return Some(result);
            }
        }

        // Calibration wizard is next (modal, blocks all other input)
        if self.calibration_wizard.is_some() {
            let result = self.handle_calibration_input(event);
            return Some(result);
        }

        // Menu is next
        if self.menu_state.active_menu.is_some() {
            let all_menus: Vec<crate::config::Menu> = self
                .menus
                .menus
                .iter()
                .chain(self.menu_state.plugin_menus.iter())
                .cloned()
                .collect();

            let mut handler = MenuInputHandler::new(&mut self.menu_state, &all_menus);
            let result = handler.dispatch_input(event, &mut ctx);
            self.process_deferred_actions(ctx);
            return Some(result);
        }

        // Prompt is next
        if self.prompt.is_some() {
            // Check for Alt+key keybindings first (before prompt consumes them as modal)
            if event
                .modifiers
                .contains(crossterm::event::KeyModifiers::ALT)
            {
                if let crossterm::event::KeyCode::Char(_) = event.code {
                    let action = self
                        .keybindings
                        .resolve(event, crate::input::keybindings::KeyContext::Prompt);
                    if !matches!(action, Action::None) {
                        // Handle the action (ignore errors for modal context)
                        let _ = self.handle_action(action);
                        return Some(InputResult::Consumed);
                    }
                }
            }

            // File browser prompts use FileBrowserInputHandler
            if self.is_file_open_active() {
                if let (Some(ref mut file_state), Some(ref mut prompt)) =
                    (&mut self.file_open_state, &mut self.prompt)
                {
                    let mut handler = FileBrowserInputHandler::new(file_state, prompt);
                    let result = handler.dispatch_input(event, &mut ctx);
                    self.process_deferred_actions(ctx);
                    return Some(result);
                }
            }

            // QueryReplaceConfirm prompts use QueryReplaceConfirmInputHandler
            use crate::view::prompt::PromptType;
            let is_query_replace_confirm = self
                .prompt
                .as_ref()
                .is_some_and(|p| p.prompt_type == PromptType::QueryReplaceConfirm);
            if is_query_replace_confirm {
                let mut handler = QueryReplaceConfirmInputHandler::new();
                let result = handler.dispatch_input(event, &mut ctx);
                self.process_deferred_actions(ctx);
                return Some(result);
            }

            if let Some(ref mut prompt) = self.prompt {
                let result = prompt.dispatch_input(event, &mut ctx);
                self.process_deferred_actions(ctx);
                return Some(result);
            }
        }

        // Popup is next
        if self.active_state().popups.is_visible() {
            let result = self
                .active_state_mut()
                .popups
                .dispatch_input(event, &mut ctx);
            self.process_deferred_actions(ctx);
            return Some(result);
        }

        None
    }

    /// Process deferred actions collected during input handling.
    pub fn process_deferred_actions(&mut self, ctx: InputContext) {
        // Set status message if provided
        if let Some(msg) = ctx.status_message {
            self.set_status_message(msg);
        }

        // Process each deferred action
        for action in ctx.deferred_actions {
            if let Err(e) = self.execute_deferred_action(action) {
                self.set_status_message(
                    t!("error.deferred_action", error = e.to_string()).to_string(),
                );
            }
        }
    }

    /// Execute a single deferred action.
    fn execute_deferred_action(&mut self, action: DeferredAction) -> AnyhowResult<()> {
        match action {
            // Settings actions
            DeferredAction::CloseSettings { save } => {
                if save {
                    self.save_settings();
                }
                self.close_settings(false);
            }
            DeferredAction::PasteToSettings => {
                if let Some(text) = self.clipboard.paste() {
                    if !text.is_empty() {
                        if let Some(settings) = &mut self.settings_state {
                            if let Some(dialog) = settings.entry_dialog_mut() {
                                dialog.insert_str(&text);
                            }
                        }
                    }
                }
            }
            DeferredAction::OpenConfigFile { layer } => {
                self.open_config_file(layer)?;
            }

            // Menu actions
            DeferredAction::CloseMenu => {
                self.close_menu_with_auto_hide();
            }
            DeferredAction::ExecuteMenuAction { action, args } => {
                // Convert menu action to keybinding Action and execute
                if let Some(kb_action) = self.menu_action_to_action(&action, args) {
                    self.handle_action(kb_action)?;
                }
            }

            // Prompt actions
            DeferredAction::ClosePrompt => {
                self.cancel_prompt();
            }
            DeferredAction::ConfirmPrompt => {
                self.handle_action(Action::PromptConfirm)?;
            }
            DeferredAction::UpdatePromptSuggestions => {
                self.update_prompt_suggestions();
            }
            DeferredAction::PromptHistoryPrev => {
                self.prompt_history_prev();
            }
            DeferredAction::PromptHistoryNext => {
                self.prompt_history_next();
            }
            DeferredAction::PreviewThemeFromPrompt => {
                if let Some(prompt) = &self.prompt {
                    if matches!(
                        prompt.prompt_type,
                        crate::view::prompt::PromptType::SelectTheme { .. }
                    ) {
                        let theme_name = prompt.input.clone();
                        self.preview_theme(&theme_name);
                    }
                }
            }
            DeferredAction::PromptSelectionChanged { selected_index } => {
                // Fire hook for plugin prompts so they can update live preview
                if let Some(prompt) = &self.prompt {
                    if let crate::view::prompt::PromptType::Plugin { custom_type } =
                        &prompt.prompt_type
                    {
                        self.plugin_manager.run_hook(
                            "prompt_selection_changed",
                            crate::services::plugins::hooks::HookArgs::PromptSelectionChanged {
                                prompt_type: custom_type.clone(),
                                selected_index,
                            },
                        );
                    }
                }
            }

            // Popup actions
            DeferredAction::ClosePopup => {
                self.hide_popup();
            }
            DeferredAction::ConfirmPopup => {
                self.handle_action(Action::PopupConfirm)?;
            }
            DeferredAction::CompletionEnterKey => {
                use crate::config::AcceptSuggestionOnEnter;
                match self.config.editor.accept_suggestion_on_enter {
                    AcceptSuggestionOnEnter::On => {
                        // Enter always accepts
                        self.handle_action(Action::PopupConfirm)?;
                    }
                    AcceptSuggestionOnEnter::Off => {
                        // Enter inserts newline - close popup and insert newline
                        self.hide_popup();
                        self.handle_action(Action::InsertNewline)?;
                    }
                    AcceptSuggestionOnEnter::Smart => {
                        // Accept if completion differs from typed text
                        // For now, we check if there's a selected item with data
                        // that differs from what's in the buffer
                        let should_accept = self
                            .active_state()
                            .popups
                            .top()
                            .and_then(|p| p.selected_item())
                            .map(|item| {
                                // If there's selection data, accept the completion
                                item.data.is_some()
                            })
                            .unwrap_or(false);

                        if should_accept {
                            self.handle_action(Action::PopupConfirm)?;
                        } else {
                            self.hide_popup();
                            self.handle_action(Action::InsertNewline)?;
                        }
                    }
                }
            }
            DeferredAction::PopupTypeChar(c) => {
                self.handle_popup_type_char(c);
            }
            DeferredAction::PopupBackspace => {
                self.handle_popup_backspace();
            }
            DeferredAction::CopyToClipboard(text) => {
                self.clipboard.copy(text);
                self.set_status_message(t!("clipboard.copied").to_string());
            }

            // Generic action execution
            DeferredAction::ExecuteAction(kb_action) => {
                self.handle_action(kb_action)?;
            }

            // Character insertion with suggestion update
            DeferredAction::InsertCharAndUpdate(c) => {
                if let Some(ref mut prompt) = self.prompt {
                    prompt.insert_char(c);
                }
                self.update_prompt_suggestions();
            }

            // File browser actions
            DeferredAction::FileBrowserSelectPrev => {
                if let Some(state) = &mut self.file_open_state {
                    state.select_prev();
                }
            }
            DeferredAction::FileBrowserSelectNext => {
                if let Some(state) = &mut self.file_open_state {
                    state.select_next();
                }
            }
            DeferredAction::FileBrowserPageUp => {
                if let Some(state) = &mut self.file_open_state {
                    state.page_up(10);
                }
            }
            DeferredAction::FileBrowserPageDown => {
                if let Some(state) = &mut self.file_open_state {
                    state.page_down(10);
                }
            }
            DeferredAction::FileBrowserConfirm => {
                // Must call handle_file_open_action directly to get proper
                // file browser behavior (e.g., project switch triggering restart)
                self.handle_file_open_action(&Action::PromptConfirm);
            }
            DeferredAction::FileBrowserAcceptSuggestion => {
                self.handle_file_open_action(&Action::PromptAcceptSuggestion);
            }
            DeferredAction::FileBrowserGoParent => {
                // Navigate to parent directory
                let parent = self
                    .file_open_state
                    .as_ref()
                    .and_then(|s| s.current_dir.parent())
                    .map(|p| p.to_path_buf());
                if let Some(parent_path) = parent {
                    self.load_file_open_directory(parent_path);
                }
            }
            DeferredAction::FileBrowserUpdateFilter => {
                self.update_file_open_filter();
            }
            DeferredAction::FileBrowserToggleHidden => {
                self.file_open_toggle_hidden();
            }

            // Interactive replace actions
            DeferredAction::InteractiveReplaceKey(c) => {
                self.handle_interactive_replace_key(c)?;
            }
            DeferredAction::CancelInteractiveReplace => {
                self.cancel_prompt();
                self.interactive_replace_state = None;
            }

            // Terminal mode actions
            DeferredAction::ToggleKeyboardCapture => {
                self.keyboard_capture = !self.keyboard_capture;
                if self.keyboard_capture {
                    self.set_status_message(
                        "Keyboard capture ON - all keys go to terminal (F9 to toggle)".to_string(),
                    );
                } else {
                    self.set_status_message(
                        "Keyboard capture OFF - UI bindings active (F9 to toggle)".to_string(),
                    );
                }
            }
            DeferredAction::SendTerminalKey(code, modifiers) => {
                self.send_terminal_key(code, modifiers);
            }
            DeferredAction::ExitTerminalMode { explicit } => {
                self.terminal_mode = false;
                self.key_context = crate::input::keybindings::KeyContext::Normal;
                if explicit {
                    // User explicitly exited - don't auto-resume when switching back
                    self.terminal_mode_resume.remove(&self.active_buffer());
                    self.sync_terminal_to_buffer(self.active_buffer());
                    self.set_status_message(
                        "Terminal mode disabled - read only (Ctrl+Space to resume)".to_string(),
                    );
                }
            }
            DeferredAction::EnterScrollbackMode => {
                self.terminal_mode = false;
                self.key_context = crate::input::keybindings::KeyContext::Normal;
                self.sync_terminal_to_buffer(self.active_buffer());
                self.set_status_message(
                    "Scrollback mode - use PageUp/Down to scroll (Ctrl+Space to resume)"
                        .to_string(),
                );
                // Scroll up using normal buffer scrolling
                self.handle_action(Action::MovePageUp)?;
            }
            DeferredAction::EnterTerminalMode => {
                self.enter_terminal_mode();
            }
        }

        Ok(())
    }

    /// Convert a menu action string to a keybinding Action.
    fn menu_action_to_action(
        &self,
        action_name: &str,
        args: std::collections::HashMap<String, serde_json::Value>,
    ) -> Option<Action> {
        // Try to parse as a built-in action first
        if let Some(action) = Action::from_str(action_name, &args) {
            return Some(action);
        }

        // Otherwise treat as a plugin action
        Some(Action::PluginAction(action_name.to_string()))
    }

    /// Navigate to previous history entry in prompt.
    fn prompt_history_prev(&mut self) {
        // Get the prompt type and current input
        let prompt_info = self
            .prompt
            .as_ref()
            .map(|p| (p.prompt_type.clone(), p.input.clone()));

        if let Some((prompt_type, current_input)) = prompt_info {
            // Get the history key for this prompt type
            if let Some(key) = Self::prompt_type_to_history_key(&prompt_type) {
                if let Some(history) = self.prompt_histories.get_mut(&key) {
                    if let Some(entry) = history.navigate_prev(&current_input) {
                        if let Some(ref mut prompt) = self.prompt {
                            prompt.set_input(entry);
                        }
                    }
                }
            }
        }
    }

    /// Navigate to next history entry in prompt.
    fn prompt_history_next(&mut self) {
        let prompt_type = self.prompt.as_ref().map(|p| p.prompt_type.clone());

        if let Some(prompt_type) = prompt_type {
            // Get the history key for this prompt type
            if let Some(key) = Self::prompt_type_to_history_key(&prompt_type) {
                if let Some(history) = self.prompt_histories.get_mut(&key) {
                    if let Some(entry) = history.navigate_next() {
                        if let Some(ref mut prompt) = self.prompt {
                            prompt.set_input(entry);
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_deferred_action_close_menu() {
        // This is a basic structure test - full integration tests
        // would require a complete Editor setup
        let action = DeferredAction::CloseMenu;
        assert!(matches!(action, DeferredAction::CloseMenu));
    }

    #[test]
    fn test_deferred_action_execute_menu_action() {
        let action = DeferredAction::ExecuteMenuAction {
            action: "save".to_string(),
            args: std::collections::HashMap::new(),
        };
        if let DeferredAction::ExecuteMenuAction { action: name, .. } = action {
            assert_eq!(name, "save");
        } else {
            panic!("Expected ExecuteMenuAction");
        }
    }
}
