//! Input handling for the Prompt (minibuffer).
//!
//! Implements the InputHandler trait for Prompt, handling text editing,
//! cursor movement, and suggestion navigation.

use super::prompt::Prompt;
use crate::input::handler::{DeferredAction, InputContext, InputHandler, InputResult};
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

impl InputHandler for Prompt {
    fn handle_key_event(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult {
        let ctrl = event.modifiers.contains(KeyModifiers::CONTROL);
        let alt = event.modifiers.contains(KeyModifiers::ALT);
        let shift = event.modifiers.contains(KeyModifiers::SHIFT);

        match event.code {
            // Confirmation and cancellation
            KeyCode::Enter => {
                ctx.defer(DeferredAction::ConfirmPrompt);
                InputResult::Consumed
            }
            KeyCode::Esc => {
                ctx.defer(DeferredAction::ClosePrompt);
                InputResult::Consumed
            }

            // Alt+key combinations should pass through to keybindings
            KeyCode::Char(_) if alt => InputResult::Ignored,

            // Character input (no modifiers or just shift)
            KeyCode::Char(c) if !ctrl => {
                // Delete any selection before inserting
                if self.has_selection() {
                    self.delete_selection();
                }
                if shift {
                    self.insert_char(c.to_ascii_uppercase());
                } else {
                    self.insert_char(c);
                }
                ctx.defer(DeferredAction::UpdatePromptSuggestions);
                InputResult::Consumed
            }
            KeyCode::Char(c) if ctrl => self.handle_ctrl_key(c, ctx),

            // Deletion
            KeyCode::Backspace if ctrl => {
                self.delete_word_backward();
                ctx.defer(DeferredAction::UpdatePromptSuggestions);
                InputResult::Consumed
            }
            KeyCode::Backspace => {
                if self.has_selection() {
                    self.delete_selection();
                } else {
                    self.backspace();
                }
                ctx.defer(DeferredAction::UpdatePromptSuggestions);
                InputResult::Consumed
            }
            KeyCode::Delete if ctrl => {
                self.delete_word_forward();
                ctx.defer(DeferredAction::UpdatePromptSuggestions);
                InputResult::Consumed
            }
            KeyCode::Delete => {
                if self.has_selection() {
                    self.delete_selection();
                } else {
                    self.delete();
                }
                ctx.defer(DeferredAction::UpdatePromptSuggestions);
                InputResult::Consumed
            }

            // Cursor movement
            KeyCode::Left if ctrl && shift => {
                self.move_word_left_selecting();
                InputResult::Consumed
            }
            KeyCode::Left if ctrl => {
                self.move_word_left();
                InputResult::Consumed
            }
            KeyCode::Left if shift => {
                self.move_left_selecting();
                InputResult::Consumed
            }
            KeyCode::Left => {
                self.clear_selection();
                self.cursor_left();
                InputResult::Consumed
            }
            KeyCode::Right if ctrl && shift => {
                self.move_word_right_selecting();
                InputResult::Consumed
            }
            KeyCode::Right if ctrl => {
                self.move_word_right();
                InputResult::Consumed
            }
            KeyCode::Right if shift => {
                self.move_right_selecting();
                InputResult::Consumed
            }
            KeyCode::Right => {
                self.clear_selection();
                self.cursor_right();
                InputResult::Consumed
            }
            KeyCode::Home if shift => {
                self.move_home_selecting();
                InputResult::Consumed
            }
            KeyCode::Home => {
                self.clear_selection();
                self.move_to_start();
                InputResult::Consumed
            }
            KeyCode::End if shift => {
                self.move_end_selecting();
                InputResult::Consumed
            }
            KeyCode::End => {
                self.clear_selection();
                self.move_to_end();
                InputResult::Consumed
            }

            // Suggestion navigation
            // TODO: Refactor to use callbacks - the prompt creator (e.g. SelectTheme, SelectLocale)
            // should be able to register a callback for selection changes instead of having
            // hardcoded prompt type checks here. This would make the suggestion UI more flexible
            // and allow custom handling for any prompt type without modifying this code.
            KeyCode::Up => {
                if !self.suggestions.is_empty() {
                    // Don't wrap around - stay at 0 if already at the beginning
                    if let Some(selected) = self.selected_suggestion {
                        let new_selected = if selected == 0 { 0 } else { selected - 1 };
                        self.selected_suggestion = Some(new_selected);
                        // For non-plugin prompts, update input to match selected suggestion
                        if !matches!(
                            self.prompt_type,
                            crate::view::prompt::PromptType::Plugin { .. }
                        ) {
                            if let Some(suggestion) = self.suggestions.get(new_selected) {
                                self.input = suggestion.get_value().to_string();
                                self.cursor_pos = self.input.len();
                            }
                        }
                        // For theme selection, trigger live preview
                        if matches!(
                            self.prompt_type,
                            crate::view::prompt::PromptType::SelectTheme { .. }
                        ) {
                            ctx.defer(DeferredAction::PreviewThemeFromPrompt);
                        }
                        // For plugin prompts, notify about selection change (for live preview)
                        if matches!(
                            self.prompt_type,
                            crate::view::prompt::PromptType::Plugin { .. }
                        ) {
                            ctx.defer(DeferredAction::PromptSelectionChanged {
                                selected_index: new_selected,
                            });
                        }
                    }
                } else {
                    // No suggestions - use history
                    ctx.defer(DeferredAction::PromptHistoryPrev);
                }
                InputResult::Consumed
            }
            KeyCode::Down => {
                if !self.suggestions.is_empty() {
                    // Don't wrap around - stay at end if already at the last item
                    if let Some(selected) = self.selected_suggestion {
                        let new_selected = (selected + 1).min(self.suggestions.len() - 1);
                        self.selected_suggestion = Some(new_selected);
                        // For non-plugin prompts, update input to match selected suggestion
                        if !matches!(
                            self.prompt_type,
                            crate::view::prompt::PromptType::Plugin { .. }
                        ) {
                            if let Some(suggestion) = self.suggestions.get(new_selected) {
                                self.input = suggestion.get_value().to_string();
                                self.cursor_pos = self.input.len();
                            }
                        }
                        // For theme selection, trigger live preview
                        if matches!(
                            self.prompt_type,
                            crate::view::prompt::PromptType::SelectTheme { .. }
                        ) {
                            ctx.defer(DeferredAction::PreviewThemeFromPrompt);
                        }
                        // For plugin prompts, notify about selection change (for live preview)
                        if matches!(
                            self.prompt_type,
                            crate::view::prompt::PromptType::Plugin { .. }
                        ) {
                            ctx.defer(DeferredAction::PromptSelectionChanged {
                                selected_index: new_selected,
                            });
                        }
                    }
                } else {
                    // No suggestions - use history
                    ctx.defer(DeferredAction::PromptHistoryNext);
                }
                InputResult::Consumed
            }
            KeyCode::PageUp => {
                if let Some(selected) = self.selected_suggestion {
                    self.selected_suggestion = Some(selected.saturating_sub(10));
                }
                InputResult::Consumed
            }
            KeyCode::PageDown => {
                if let Some(selected) = self.selected_suggestion {
                    let len = self.suggestions.len();
                    let new_pos = selected + 10;
                    self.selected_suggestion = Some(new_pos.min(len.saturating_sub(1)));
                }
                InputResult::Consumed
            }

            // Tab accepts suggestion
            KeyCode::Tab => {
                if let Some(selected) = self.selected_suggestion {
                    if let Some(suggestion) = self.suggestions.get(selected) {
                        if !suggestion.disabled {
                            self.input = suggestion.get_value().to_string();
                            self.cursor_pos = self.input.len();
                            self.clear_selection();
                        }
                    }
                }
                ctx.defer(DeferredAction::UpdatePromptSuggestions);
                InputResult::Consumed
            }

            _ => InputResult::Consumed, // Modal - consume all unhandled keys
        }
    }

    fn is_modal(&self) -> bool {
        true
    }
}

impl Prompt {
    fn handle_ctrl_key(&mut self, c: char, ctx: &mut InputContext) -> InputResult {
        match c {
            'a' => {
                // Select all
                self.selection_anchor = Some(0);
                self.cursor_pos = self.input.len();
                InputResult::Consumed
            }
            'c' => {
                // Copy - defer to Editor for clipboard access
                ctx.defer(DeferredAction::ExecuteAction(
                    crate::input::keybindings::Action::PromptCopy,
                ));
                InputResult::Consumed
            }
            'x' => {
                // Cut - defer to Editor for clipboard access
                ctx.defer(DeferredAction::ExecuteAction(
                    crate::input::keybindings::Action::PromptCut,
                ));
                InputResult::Consumed
            }
            'v' => {
                // Paste - defer to Editor for clipboard access
                ctx.defer(DeferredAction::ExecuteAction(
                    crate::input::keybindings::Action::PromptPaste,
                ));
                InputResult::Consumed
            }
            'k' => {
                // Delete to end of line
                self.delete_to_end();
                ctx.defer(DeferredAction::UpdatePromptSuggestions);
                InputResult::Consumed
            }
            _ => InputResult::Consumed,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::prompt::PromptType;

    fn key(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::NONE)
    }

    fn key_with_ctrl(c: char) -> KeyEvent {
        KeyEvent::new(KeyCode::Char(c), KeyModifiers::CONTROL)
    }

    fn key_with_shift(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::SHIFT)
    }

    #[test]
    fn test_prompt_character_input() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        let mut ctx = InputContext::new();

        prompt.handle_key_event(
            &KeyEvent::new(KeyCode::Char('h'), KeyModifiers::NONE),
            &mut ctx,
        );
        prompt.handle_key_event(
            &KeyEvent::new(KeyCode::Char('i'), KeyModifiers::NONE),
            &mut ctx,
        );

        assert_eq!(prompt.input, "hi");
        assert_eq!(prompt.cursor_pos, 2);
    }

    #[test]
    fn test_prompt_backspace() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "hello".to_string();
        prompt.cursor_pos = 5;
        let mut ctx = InputContext::new();

        prompt.handle_key_event(&key(KeyCode::Backspace), &mut ctx);
        assert_eq!(prompt.input, "hell");
        assert_eq!(prompt.cursor_pos, 4);
    }

    #[test]
    fn test_prompt_cursor_movement() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "hello".to_string();
        prompt.cursor_pos = 5;
        let mut ctx = InputContext::new();

        // Move to start
        prompt.handle_key_event(&key(KeyCode::Home), &mut ctx);
        assert_eq!(prompt.cursor_pos, 0);

        // Move to end
        prompt.handle_key_event(&key(KeyCode::End), &mut ctx);
        assert_eq!(prompt.cursor_pos, 5);

        // Move left
        prompt.handle_key_event(&key(KeyCode::Left), &mut ctx);
        assert_eq!(prompt.cursor_pos, 4);

        // Move right
        prompt.handle_key_event(&key(KeyCode::Right), &mut ctx);
        assert_eq!(prompt.cursor_pos, 5);
    }

    #[test]
    fn test_prompt_selection() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "hello world".to_string();
        prompt.cursor_pos = 0;
        let mut ctx = InputContext::new();

        // Select with Shift+Right
        prompt.handle_key_event(&key_with_shift(KeyCode::Right), &mut ctx);
        prompt.handle_key_event(&key_with_shift(KeyCode::Right), &mut ctx);
        assert!(prompt.has_selection());
        assert_eq!(prompt.selected_text(), Some("he".to_string()));

        // Select all with Ctrl+A
        prompt.handle_key_event(&key_with_ctrl('a'), &mut ctx);
        assert_eq!(prompt.selected_text(), Some("hello world".to_string()));
    }

    #[test]
    fn test_prompt_enter_confirms() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        let mut ctx = InputContext::new();

        prompt.handle_key_event(&key(KeyCode::Enter), &mut ctx);
        assert!(ctx
            .deferred_actions
            .iter()
            .any(|a| matches!(a, DeferredAction::ConfirmPrompt)));
    }

    #[test]
    fn test_prompt_escape_cancels() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        let mut ctx = InputContext::new();

        prompt.handle_key_event(&key(KeyCode::Esc), &mut ctx);
        assert!(ctx
            .deferred_actions
            .iter()
            .any(|a| matches!(a, DeferredAction::ClosePrompt)));
    }

    #[test]
    fn test_prompt_is_modal() {
        let prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        assert!(prompt.is_modal());
    }
}
