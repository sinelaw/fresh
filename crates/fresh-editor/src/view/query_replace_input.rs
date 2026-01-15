//! Input handler for query-replace confirmation prompts.
//!
//! This handler routes character input to the interactive replace system
//! instead of inserting it into a prompt buffer.

use crate::input::handler::{DeferredAction, InputContext, InputHandler, InputResult};
use crossterm::event::{KeyCode, KeyEvent};

/// Input handler for QueryReplaceConfirm prompts.
///
/// This is a simple modal handler that:
/// - Routes character keys (y/n/a/q/!) to the interactive replace handler
/// - Handles Escape to cancel
/// - Consumes all other keys to maintain modal behavior
pub struct QueryReplaceConfirmInputHandler;

impl QueryReplaceConfirmInputHandler {
    pub fn new() -> Self {
        Self
    }
}

impl Default for QueryReplaceConfirmInputHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl InputHandler for QueryReplaceConfirmInputHandler {
    fn handle_key_event(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult {
        match event.code {
            // Character input goes to interactive replace handler
            KeyCode::Char(c) => {
                ctx.defer(DeferredAction::InteractiveReplaceKey(c));
                InputResult::Consumed
            }
            // Escape cancels the operation
            KeyCode::Esc => {
                ctx.defer(DeferredAction::CancelInteractiveReplace);
                InputResult::Consumed
            }
            // Consume all other keys for modal behavior
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
    use crossterm::event::KeyModifiers;

    fn key(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::NONE)
    }

    #[test]
    fn test_character_keys_defer_to_handler() {
        let mut handler = QueryReplaceConfirmInputHandler::new();
        let mut ctx = InputContext::new();

        let result = handler.handle_key_event(&key(KeyCode::Char('y')), &mut ctx);
        assert!(matches!(result, InputResult::Consumed));
        assert_eq!(ctx.deferred_actions.len(), 1);
        assert!(matches!(
            ctx.deferred_actions[0],
            DeferredAction::InteractiveReplaceKey('y')
        ));
    }

    #[test]
    fn test_escape_cancels() {
        let mut handler = QueryReplaceConfirmInputHandler::new();
        let mut ctx = InputContext::new();

        let result = handler.handle_key_event(&key(KeyCode::Esc), &mut ctx);
        assert!(matches!(result, InputResult::Consumed));
        assert_eq!(ctx.deferred_actions.len(), 1);
        assert!(matches!(
            ctx.deferred_actions[0],
            DeferredAction::CancelInteractiveReplace
        ));
    }

    #[test]
    fn test_other_keys_consumed() {
        let mut handler = QueryReplaceConfirmInputHandler::new();
        let mut ctx = InputContext::new();

        let result = handler.handle_key_event(&key(KeyCode::Enter), &mut ctx);
        assert!(matches!(result, InputResult::Consumed));
        assert!(ctx.deferred_actions.is_empty());

        let result = handler.handle_key_event(&key(KeyCode::Up), &mut ctx);
        assert!(matches!(result, InputResult::Consumed));
        assert!(ctx.deferred_actions.is_empty());
    }

    #[test]
    fn test_is_modal() {
        let handler = QueryReplaceConfirmInputHandler::new();
        assert!(handler.is_modal());
    }
}
