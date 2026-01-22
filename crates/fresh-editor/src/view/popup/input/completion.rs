//! Input handling for completion popups.
//!
//! Completion popups support:
//! - Type-to-filter: typing characters filters the completion list
//! - Tab/Enter: accept the selected completion
//! - Backspace: remove last filter character
//! - Arrow keys: navigate the list

use super::base::{try_handle_shared, SharedHandleResult};
use crate::input::handler::{DeferredAction, InputContext, InputResult};
use crate::view::popup::Popup;
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

/// Handle input for completion popups
pub fn handle_completion_input(
    event: &KeyEvent,
    popup: Option<&mut Popup>,
    ctx: &mut InputContext,
) -> InputResult {
    // Try shared handling first (Esc, PageUp/Down, etc.)
    match try_handle_shared(event, popup, ctx) {
        SharedHandleResult::Handled(result) => return result,
        SharedHandleResult::NotHandled => {}
    }

    // Reborrow popup for completion-specific handling
    // (we need to re-get it since try_handle_shared consumed the borrow)

    match event.code {
        // Enter - behavior depends on accept_suggestion_on_enter config
        KeyCode::Enter => {
            ctx.defer(DeferredAction::CompletionEnterKey);
            InputResult::Consumed
        }

        // Tab always accepts the completion
        KeyCode::Tab if event.modifiers.is_empty() => {
            ctx.defer(DeferredAction::ConfirmPopup);
            InputResult::Consumed
        }

        // Arrow navigation
        KeyCode::Up | KeyCode::Down if event.modifiers.is_empty() => {
            // We can't use popup here since it was moved, but the caller will handle this
            InputResult::Consumed
        }

        // Type-to-filter: allow both lowercase and uppercase letters
        KeyCode::Char(c)
            if event.modifiers.is_empty() || event.modifiers == KeyModifiers::SHIFT =>
        {
            ctx.defer(DeferredAction::PopupTypeChar(c));
            InputResult::Consumed
        }

        // Backspace removes last filter character
        KeyCode::Backspace if event.modifiers.is_empty() => {
            ctx.defer(DeferredAction::PopupBackspace);
            InputResult::Consumed
        }

        // Consume all other keys (modal behavior)
        _ => InputResult::Consumed,
    }
}

/// Handle completion input with mutable popup access for navigation
pub fn handle_completion_input_with_popup(
    event: &KeyEvent,
    popup: &mut Popup,
    ctx: &mut InputContext,
) -> InputResult {
    // Try shared handling first
    match try_handle_shared(event, Some(popup), ctx) {
        SharedHandleResult::Handled(result) => return result,
        SharedHandleResult::NotHandled => {}
    }

    match event.code {
        // Enter - behavior depends on accept_suggestion_on_enter config
        KeyCode::Enter => {
            ctx.defer(DeferredAction::CompletionEnterKey);
            InputResult::Consumed
        }

        // Tab always accepts the completion
        KeyCode::Tab if event.modifiers.is_empty() => {
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

        // Type-to-filter: allow both lowercase and uppercase letters
        KeyCode::Char(c)
            if event.modifiers.is_empty() || event.modifiers == KeyModifiers::SHIFT =>
        {
            ctx.defer(DeferredAction::PopupTypeChar(c));
            InputResult::Consumed
        }

        // Backspace removes last filter character
        KeyCode::Backspace if event.modifiers.is_empty() => {
            ctx.defer(DeferredAction::PopupBackspace);
            InputResult::Consumed
        }

        // Consume all other keys (modal behavior)
        _ => InputResult::Consumed,
    }
}
