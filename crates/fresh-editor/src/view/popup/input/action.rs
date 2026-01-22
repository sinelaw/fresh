//! Input handling for action popups.
//!
//! Action popups show a list of actions and support:
//! - Escape: dismiss the popup
//! - Enter: execute the selected action
//! - Arrow keys: navigate the action list
//! - Tab/Shift+Tab: navigate the action list

use super::base::{try_handle_shared, SharedHandleResult};
use crate::input::handler::{DeferredAction, InputContext, InputResult};
use crate::view::popup::Popup;
use crossterm::event::{KeyCode, KeyEvent};

/// Handle input for action popups
pub fn handle_action_input(
    event: &KeyEvent,
    popup: &mut Popup,
    ctx: &mut InputContext,
) -> InputResult {
    // Try shared handling first (Esc, PageUp/Down, Ctrl+C)
    match try_handle_shared(event, Some(popup), ctx) {
        SharedHandleResult::Handled(result) => return result,
        SharedHandleResult::NotHandled => {}
    }

    match event.code {
        // Enter executes the selected action
        KeyCode::Enter => {
            ctx.defer(DeferredAction::ConfirmPopup);
            InputResult::Consumed
        }

        // Arrow navigation
        KeyCode::Up if event.modifiers.is_empty() => {
            popup.select_prev();
            InputResult::Consumed
        }
        KeyCode::Down if event.modifiers.is_empty() => {
            popup.select_next();
            InputResult::Consumed
        }

        // Tab navigates forward
        KeyCode::Tab if event.modifiers.is_empty() => {
            popup.select_next();
            InputResult::Consumed
        }
        // Shift+Tab navigates backward
        KeyCode::BackTab => {
            popup.select_prev();
            InputResult::Consumed
        }

        // Consume all other keys (modal behavior)
        _ => InputResult::Consumed,
    }
}
