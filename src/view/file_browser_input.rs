//! Input handling for the File Browser (Open File / Switch Project).
//!
//! This handler wraps both the file browser state and the prompt,
//! handling navigation while delegating text input to the prompt.

use crate::app::file_open::FileOpenState;
use crate::input::handler::{DeferredAction, InputContext, InputHandler, InputResult};
use crate::view::prompt::Prompt;
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

/// Input handler for file browser prompts (OpenFile, SwitchProject).
///
/// Handles navigation keys (Up, Down, Enter, Tab, etc.) directly,
/// delegates text editing to the wrapped Prompt.
pub struct FileBrowserInputHandler<'a> {
    pub file_state: &'a mut FileOpenState,
    pub prompt: &'a mut Prompt,
}

impl<'a> FileBrowserInputHandler<'a> {
    pub fn new(file_state: &'a mut FileOpenState, prompt: &'a mut Prompt) -> Self {
        Self { file_state, prompt }
    }
}

impl<'a> InputHandler for FileBrowserInputHandler<'a> {
    fn handle_key_event(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult {
        let ctrl = event.modifiers.contains(KeyModifiers::CONTROL);
        let alt = event.modifiers.contains(KeyModifiers::ALT);

        // Alt+key combinations pass through to keybindings
        if alt {
            if let KeyCode::Char(_) = event.code {
                return InputResult::Ignored;
            }
        }

        match event.code {
            // Navigation in file list
            KeyCode::Up => {
                ctx.defer(DeferredAction::FileBrowserSelectPrev);
                InputResult::Consumed
            }
            KeyCode::Down => {
                ctx.defer(DeferredAction::FileBrowserSelectNext);
                InputResult::Consumed
            }
            KeyCode::PageUp => {
                ctx.defer(DeferredAction::FileBrowserPageUp);
                InputResult::Consumed
            }
            KeyCode::PageDown => {
                ctx.defer(DeferredAction::FileBrowserPageDown);
                InputResult::Consumed
            }

            // Confirmation
            KeyCode::Enter => {
                ctx.defer(DeferredAction::FileBrowserConfirm);
                InputResult::Consumed
            }

            // Tab accepts suggestion / navigates into directory
            KeyCode::Tab => {
                ctx.defer(DeferredAction::FileBrowserAcceptSuggestion);
                InputResult::Consumed
            }

            // Escape cancels
            KeyCode::Esc => {
                ctx.defer(DeferredAction::ClosePrompt);
                InputResult::Consumed
            }

            // Backspace: if input is empty, go to parent directory
            // Otherwise, delegate to prompt for character deletion
            KeyCode::Backspace if !ctrl => {
                if self.prompt.input.is_empty() {
                    ctx.defer(DeferredAction::FileBrowserGoParent);
                    InputResult::Consumed
                } else {
                    // Delegate to prompt for backspace
                    if self.prompt.has_selection() {
                        self.prompt.delete_selection();
                    } else {
                        self.prompt.backspace();
                    }
                    ctx.defer(DeferredAction::FileBrowserUpdateFilter);
                    InputResult::Consumed
                }
            }

            // Ctrl+Backspace: delete word backward
            KeyCode::Backspace if ctrl => {
                self.prompt.delete_word_backward();
                ctx.defer(DeferredAction::FileBrowserUpdateFilter);
                InputResult::Consumed
            }

            // Delete key
            KeyCode::Delete if ctrl => {
                self.prompt.delete_word_forward();
                ctx.defer(DeferredAction::FileBrowserUpdateFilter);
                InputResult::Consumed
            }
            KeyCode::Delete => {
                if self.prompt.has_selection() {
                    self.prompt.delete_selection();
                } else {
                    self.prompt.delete();
                }
                ctx.defer(DeferredAction::FileBrowserUpdateFilter);
                InputResult::Consumed
            }

            // Character input - insert into prompt and update filter
            KeyCode::Char(c) if !ctrl && !alt => {
                if self.prompt.has_selection() {
                    self.prompt.delete_selection();
                }
                self.prompt.insert_char(c);
                ctx.defer(DeferredAction::FileBrowserUpdateFilter);
                InputResult::Consumed
            }

            // Ctrl+key combinations
            KeyCode::Char(c) if ctrl => {
                match c {
                    'a' => {
                        // Select all
                        self.prompt.selection_anchor = Some(0);
                        self.prompt.cursor_pos = self.prompt.input.len();
                        InputResult::Consumed
                    }
                    'c' => {
                        // Copy - defer to Editor for clipboard
                        ctx.defer(DeferredAction::ExecuteAction(
                            crate::input::keybindings::Action::PromptCopy,
                        ));
                        InputResult::Consumed
                    }
                    'x' => {
                        // Cut
                        ctx.defer(DeferredAction::ExecuteAction(
                            crate::input::keybindings::Action::PromptCut,
                        ));
                        InputResult::Consumed
                    }
                    'v' => {
                        // Paste
                        ctx.defer(DeferredAction::ExecuteAction(
                            crate::input::keybindings::Action::PromptPaste,
                        ));
                        InputResult::Consumed
                    }
                    'k' => {
                        // Delete to end of line
                        self.prompt.delete_to_end();
                        ctx.defer(DeferredAction::FileBrowserUpdateFilter);
                        InputResult::Consumed
                    }
                    _ => InputResult::Consumed,
                }
            }

            // Cursor movement within prompt
            KeyCode::Left if ctrl => {
                self.prompt.move_word_left();
                InputResult::Consumed
            }
            KeyCode::Left => {
                self.prompt.clear_selection();
                self.prompt.cursor_left();
                InputResult::Consumed
            }
            KeyCode::Right if ctrl => {
                self.prompt.move_word_right();
                InputResult::Consumed
            }
            KeyCode::Right => {
                self.prompt.clear_selection();
                self.prompt.cursor_right();
                InputResult::Consumed
            }
            KeyCode::Home => {
                self.prompt.clear_selection();
                self.prompt.move_to_start();
                InputResult::Consumed
            }
            KeyCode::End => {
                self.prompt.clear_selection();
                self.prompt.move_to_end();
                InputResult::Consumed
            }

            // Consume all other keys (modal behavior)
            _ => InputResult::Consumed,
        }
    }

    fn is_modal(&self) -> bool {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::prompt::PromptType;
    use std::path::PathBuf;

    fn create_test_file_state() -> FileOpenState {
        FileOpenState::new(PathBuf::from("/tmp"))
    }

    fn create_test_prompt() -> Prompt {
        Prompt::new("Open: ".to_string(), PromptType::OpenFile)
    }

    fn key(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::NONE)
    }

    #[test]
    fn test_navigation_keys() {
        let mut file_state = create_test_file_state();
        let mut prompt = create_test_prompt();
        let mut handler = FileBrowserInputHandler::new(&mut file_state, &mut prompt);
        let mut ctx = InputContext::new();

        // Up should defer FileBrowserSelectPrev
        let result = handler.handle_key_event(&key(KeyCode::Up), &mut ctx);
        assert_eq!(result, InputResult::Consumed);
        assert!(ctx
            .deferred_actions
            .iter()
            .any(|a| matches!(a, DeferredAction::FileBrowserSelectPrev)));
    }

    #[test]
    fn test_character_input_updates_filter() {
        let mut file_state = create_test_file_state();
        let mut prompt = create_test_prompt();
        let mut handler = FileBrowserInputHandler::new(&mut file_state, &mut prompt);
        let mut ctx = InputContext::new();

        handler.handle_key_event(&key(KeyCode::Char('t')), &mut ctx);
        handler.handle_key_event(&key(KeyCode::Char('e')), &mut ctx);
        handler.handle_key_event(&key(KeyCode::Char('s')), &mut ctx);
        handler.handle_key_event(&key(KeyCode::Char('t')), &mut ctx);

        assert_eq!(prompt.input, "test");
        // Should have deferred filter updates
        assert!(ctx
            .deferred_actions
            .iter()
            .any(|a| matches!(a, DeferredAction::FileBrowserUpdateFilter)));
    }

    #[test]
    fn test_backspace_empty_goes_parent() {
        let mut file_state = create_test_file_state();
        let mut prompt = create_test_prompt();
        prompt.input = String::new();
        let mut handler = FileBrowserInputHandler::new(&mut file_state, &mut prompt);
        let mut ctx = InputContext::new();

        handler.handle_key_event(&key(KeyCode::Backspace), &mut ctx);

        assert!(ctx
            .deferred_actions
            .iter()
            .any(|a| matches!(a, DeferredAction::FileBrowserGoParent)));
    }

    #[test]
    fn test_backspace_with_text_deletes() {
        let mut file_state = create_test_file_state();
        let mut prompt = create_test_prompt();
        prompt.input = "test".to_string();
        prompt.cursor_pos = 4;
        let mut handler = FileBrowserInputHandler::new(&mut file_state, &mut prompt);
        let mut ctx = InputContext::new();

        handler.handle_key_event(&key(KeyCode::Backspace), &mut ctx);

        assert_eq!(prompt.input, "tes");
        assert!(ctx
            .deferred_actions
            .iter()
            .any(|a| matches!(a, DeferredAction::FileBrowserUpdateFilter)));
    }

    #[test]
    fn test_is_modal() {
        let mut file_state = create_test_file_state();
        let mut prompt = create_test_prompt();
        let handler = FileBrowserInputHandler::new(&mut file_state, &mut prompt);
        assert!(handler.is_modal());
    }

    #[test]
    fn test_enter_confirms() {
        let mut file_state = create_test_file_state();
        let mut prompt = create_test_prompt();
        let mut handler = FileBrowserInputHandler::new(&mut file_state, &mut prompt);
        let mut ctx = InputContext::new();

        handler.handle_key_event(&key(KeyCode::Enter), &mut ctx);

        assert!(ctx
            .deferred_actions
            .iter()
            .any(|a| matches!(a, DeferredAction::FileBrowserConfirm)));
    }

    #[test]
    fn test_escape_closes() {
        let mut file_state = create_test_file_state();
        let mut prompt = create_test_prompt();
        let mut handler = FileBrowserInputHandler::new(&mut file_state, &mut prompt);
        let mut ctx = InputContext::new();

        handler.handle_key_event(&key(KeyCode::Esc), &mut ctx);

        assert!(ctx
            .deferred_actions
            .iter()
            .any(|a| matches!(a, DeferredAction::ClosePrompt)));
    }
}
