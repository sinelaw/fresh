//! Input handling for completion popups.
//!
//! Accept and dismiss are resolved upstream against `KeyContext::Completion`
//! (default: Tab → `completion_accept`, Esc → `completion_dismiss`). The keys
//! handled here cover the behaviours that do *not* go through the keybinding
//! system:
//! - Type-to-filter: typing characters filters the completion list
//! - Enter: dismiss the popup and insert newline (passthrough)
//! - Ctrl+Space: toggle (dismiss) the popup
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
    // Accept (default: Tab) and dismiss (default: Esc) are resolved upstream
    // against `KeyContext::Completion` before this handler runs, so we only
    // see keys that the resolver did not claim.

    // Try shared handling first (Esc fallback, PageUp/Down, etc.)
    match try_handle_shared(event, popup, ctx) {
        SharedHandleResult::Handled(result) => return result,
        SharedHandleResult::NotHandled => {}
    }

    // Reborrow popup for completion-specific handling
    // (we need to re-get it since try_handle_shared consumed the borrow)

    match event.code {
        // Ctrl+Space toggles the popup off (consumed so it won't re-open)
        KeyCode::Char(' ') if event.modifiers == KeyModifiers::CONTROL => {
            ctx.defer(DeferredAction::ClosePopup);
            InputResult::Consumed
        }

        // Enter dismisses popup and inserts newline (passthrough)
        KeyCode::Enter => {
            ctx.defer(DeferredAction::ClosePopup);
            InputResult::Ignored
        }

        // Arrow navigation (Up/Down navigate the list)
        KeyCode::Up | KeyCode::Down if event.modifiers.is_empty() => {
            // We can't use popup here since it was moved, but the caller will handle this
            InputResult::Consumed
        }

        // Type-to-filter: only word characters (letters, digits, underscore)
        KeyCode::Char(c)
            if (event.modifiers.is_empty() || event.modifiers == KeyModifiers::SHIFT)
                && (c.is_alphanumeric() || c == '_') =>
        {
            ctx.defer(DeferredAction::PopupTypeChar(c));
            InputResult::Consumed
        }

        // Backspace removes last filter character.
        // Shift+Backspace is treated the same as plain Backspace so an
        // accidentally-held Shift key doesn't dismiss the popup.
        KeyCode::Backspace
            if event.modifiers.is_empty() || event.modifiers == KeyModifiers::SHIFT =>
        {
            ctx.defer(DeferredAction::PopupBackspace);
            InputResult::Consumed
        }

        // All other keys (non-word chars, arrows, Ctrl+key, Delete, etc.)
        // close the popup and pass through to normal input handling
        _ => {
            ctx.defer(DeferredAction::ClosePopup);
            InputResult::Ignored
        }
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
        // Ctrl+Space toggles the popup off (consumed so it won't re-open)
        KeyCode::Char(' ') if event.modifiers == KeyModifiers::CONTROL => {
            ctx.defer(DeferredAction::ClosePopup);
            InputResult::Consumed
        }

        // Enter dismisses popup and inserts newline (passthrough)
        KeyCode::Enter => {
            ctx.defer(DeferredAction::ClosePopup);
            InputResult::Ignored
        }

        // Arrow navigation (Up/Down navigate the list)
        KeyCode::Up if event.modifiers.is_empty() => {
            popup.select_prev();
            InputResult::Consumed
        }
        KeyCode::Down if event.modifiers.is_empty() => {
            popup.select_next();
            InputResult::Consumed
        }

        // Type-to-filter: only word characters (letters, digits, underscore)
        KeyCode::Char(c)
            if (event.modifiers.is_empty() || event.modifiers == KeyModifiers::SHIFT)
                && (c.is_alphanumeric() || c == '_') =>
        {
            ctx.defer(DeferredAction::PopupTypeChar(c));
            InputResult::Consumed
        }

        // Backspace removes last filter character.
        // Shift+Backspace is treated the same as plain Backspace so an
        // accidentally-held Shift key doesn't dismiss the popup.
        KeyCode::Backspace
            if event.modifiers.is_empty() || event.modifiers == KeyModifiers::SHIFT =>
        {
            ctx.defer(DeferredAction::PopupBackspace);
            InputResult::Consumed
        }

        // All other keys (non-word chars, arrows, Ctrl+key, Delete, etc.)
        // close the popup and pass through to normal input handling
        _ => {
            ctx.defer(DeferredAction::ClosePopup);
            InputResult::Ignored
        }
    }
}
