//! Input dispatch using the hierarchical InputHandler system.
//!
//! This module provides the bridge between Editor and the InputHandler trait,
//! dispatching input to modal components and processing deferred actions.

use super::Editor;
use crate::input::handler::{DeferredAction, InputContext, InputHandler, InputResult};
use crate::input::keybindings::Action;
use crate::view::ui::MenuInputHandler;
use crossterm::event::KeyEvent;

impl Editor {
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

        // Menu is next
        if self.menu_state.active_menu.is_some() {
            let all_menus: Vec<crate::config::Menu> = self
                .config
                .menu
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

            // For file browser prompts (OpenFile, SwitchProject), navigation keys need to
            // go through keybinding resolution to reach handle_file_open_action.
            // Only let the prompt InputHandler handle text editing keys.
            if self.is_file_open_active() {
                use crossterm::event::KeyCode;
                let input_empty = self.prompt.as_ref().map_or(true, |p| p.input.is_empty());
                match event.code {
                    // Navigation keys - let keybindings handle them for file browser
                    KeyCode::Enter
                    | KeyCode::Up
                    | KeyCode::Down
                    | KeyCode::Tab
                    | KeyCode::PageUp
                    | KeyCode::PageDown => {
                        // Don't dispatch to prompt - let keybinding resolution handle it
                        return None;
                    }
                    // Backspace when input is empty goes to parent directory
                    KeyCode::Backspace if input_empty => {
                        return None;
                    }
                    // All other keys (text input, cursor movement) handled by prompt
                    _ => {}
                }
            }

            // QueryReplaceConfirm prompts need special handling - character input goes
            // directly to handle_interactive_replace_key instead of being inserted
            use crate::view::prompt::PromptType;
            let is_query_replace_confirm = self
                .prompt
                .as_ref()
                .map_or(false, |p| p.prompt_type == PromptType::QueryReplaceConfirm);
            if is_query_replace_confirm {
                if let crossterm::event::KeyCode::Char(c) = event.code {
                    let _ = self.handle_interactive_replace_key(c);
                    return Some(InputResult::Consumed);
                }
                if event.code == crossterm::event::KeyCode::Esc {
                    self.cancel_prompt();
                    self.interactive_replace_state = None;
                    return Some(InputResult::Consumed);
                }
                // Consume other keys for modal behavior
                return Some(InputResult::Consumed);
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
                self.set_status_message(format!("Error: {}", e));
            }
        }
    }

    /// Execute a single deferred action.
    fn execute_deferred_action(&mut self, action: DeferredAction) -> std::io::Result<()> {
        match action {
            // Settings actions
            DeferredAction::CloseSettings { save } => {
                if save {
                    self.save_settings();
                }
                self.close_settings(false);
            }

            // Menu actions
            DeferredAction::CloseMenu => {
                self.menu_state.close_menu();
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

            // Popup actions
            DeferredAction::ClosePopup => {
                self.hide_popup();
            }
            DeferredAction::ConfirmPopup => {
                self.handle_action(Action::PopupConfirm)?;
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
        use crate::view::prompt::PromptType;

        // Get the prompt type and current input
        let prompt_info = self
            .prompt
            .as_ref()
            .map(|p| (p.prompt_type.clone(), p.input.clone()));

        if let Some((prompt_type, current_input)) = prompt_info {
            // Search prompts use search history
            if matches!(
                prompt_type,
                PromptType::Search | PromptType::ReplaceSearch | PromptType::QueryReplaceSearch
            ) {
                if let Some(entry) = self.search_history.navigate_prev(&current_input) {
                    if let Some(ref mut prompt) = self.prompt {
                        prompt.set_input(entry);
                    }
                }
            }
            // Replacement prompts use replace history
            else if matches!(
                prompt_type,
                PromptType::Replace { .. } | PromptType::QueryReplace { .. }
            ) {
                if let Some(entry) = self.replace_history.navigate_prev(&current_input) {
                    if let Some(ref mut prompt) = self.prompt {
                        prompt.set_input(entry);
                    }
                }
            }
        }
    }

    /// Navigate to next history entry in prompt.
    fn prompt_history_next(&mut self) {
        use crate::view::prompt::PromptType;

        let prompt_type = self.prompt.as_ref().map(|p| p.prompt_type.clone());

        if let Some(prompt_type) = prompt_type {
            // Search prompts use search history
            if matches!(
                prompt_type,
                PromptType::Search | PromptType::ReplaceSearch | PromptType::QueryReplaceSearch
            ) {
                if let Some(entry) = self.search_history.navigate_next() {
                    if let Some(ref mut prompt) = self.prompt {
                        prompt.set_input(entry);
                    }
                }
            }
            // Replacement prompts use replace history
            else if matches!(
                prompt_type,
                PromptType::Replace { .. } | PromptType::QueryReplace { .. }
            ) {
                if let Some(entry) = self.replace_history.navigate_next() {
                    if let Some(ref mut prompt) = self.prompt {
                        prompt.set_input(entry);
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
