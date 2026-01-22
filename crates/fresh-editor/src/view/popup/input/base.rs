//! Shared input handling logic for popups.
//!
//! Contains common functionality used by multiple popup types:
//! - Escape to dismiss
//! - PageUp/PageDown for scrolling
//! - Home/End for jumping to start/end
//! - Ctrl+C for copying selected text

use crate::input::handler::{DeferredAction, InputContext, InputResult};
use crate::view::popup::Popup;
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

/// Result of attempting to handle a key event with shared logic
pub enum SharedHandleResult {
    /// The key was handled by shared logic
    Handled(InputResult),
    /// The key should be handled by popup-specific logic
    NotHandled,
}

/// Try to handle common popup keys (Esc, PageUp/Down, Home/End, Ctrl+C)
pub fn try_handle_shared(
    event: &KeyEvent,
    popup: Option<&mut Popup>,
    ctx: &mut InputContext,
) -> SharedHandleResult {
    match event.code {
        // Escape always dismisses
        KeyCode::Esc => {
            ctx.defer(DeferredAction::ClosePopup);
            SharedHandleResult::Handled(InputResult::Consumed)
        }

        // Page navigation
        KeyCode::PageUp => {
            if let Some(p) = popup {
                p.page_up();
            }
            SharedHandleResult::Handled(InputResult::Consumed)
        }
        KeyCode::PageDown => {
            if let Some(p) = popup {
                p.page_down();
            }
            SharedHandleResult::Handled(InputResult::Consumed)
        }
        KeyCode::Home => {
            if let Some(p) = popup {
                p.select_first();
            }
            SharedHandleResult::Handled(InputResult::Consumed)
        }
        KeyCode::End => {
            if let Some(p) = popup {
                p.select_last();
            }
            SharedHandleResult::Handled(InputResult::Consumed)
        }

        // Ctrl+C to copy selected text
        KeyCode::Char('c') if event.modifiers == KeyModifiers::CONTROL => {
            if let Some(p) = popup {
                if p.has_selection() {
                    if let Some(text) = p.get_selected_text() {
                        ctx.defer(DeferredAction::CopyToClipboard(text));
                    }
                }
            }
            SharedHandleResult::Handled(InputResult::Consumed)
        }

        _ => SharedHandleResult::NotHandled,
    }
}

/// Handle arrow key navigation for list-based popups
pub fn handle_list_navigation(event: &KeyEvent, popup: Option<&mut Popup>) -> Option<InputResult> {
    match event.code {
        KeyCode::Up if event.modifiers.is_empty() => {
            if let Some(p) = popup {
                p.select_prev();
            }
            Some(InputResult::Consumed)
        }
        KeyCode::Down if event.modifiers.is_empty() => {
            if let Some(p) = popup {
                p.select_next();
            }
            Some(InputResult::Consumed)
        }
        _ => None,
    }
}
