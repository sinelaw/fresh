//! Input handling for hover/documentation popups.
//!
//! Hover popups are read-only and support:
//! - Escape: dismiss the popup
//! - Arrow keys: scroll content
//! - PageUp/PageDown: scroll by page
//! - Ctrl+C: copy selected text

use super::base::{try_handle_shared, SharedHandleResult};
use crate::input::handler::{DeferredAction, InputContext, InputResult};
use crate::view::popup::Popup;
use crossterm::event::{KeyCode, KeyEvent};

/// Handle input for hover/documentation popups
pub fn handle_hover_input(
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
        // Arrow keys scroll the content
        KeyCode::Up if event.modifiers.is_empty() => {
            popup.scroll_by(-1);
            InputResult::Consumed
        }
        KeyCode::Down if event.modifiers.is_empty() => {
            popup.scroll_by(1);
            InputResult::Consumed
        }

        // Enter dismisses hover popup
        KeyCode::Enter => {
            ctx.defer(DeferredAction::ClosePopup);
            InputResult::Consumed
        }

        // Any other key dismisses the popup (hover is transient)
        _ => {
            ctx.defer(DeferredAction::ClosePopup);
            InputResult::Consumed
        }
    }
}
