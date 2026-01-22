//! Popup input handling modules.
//!
//! This module contains specialized input handlers for different popup types:
//! - `completion`: LSP completion popups with type-to-filter
//! - `hover`: Read-only hover/documentation popups
//! - `action`: Action popups with selectable actions
//! - `base`: Shared input handling logic

pub mod action;
pub mod base;
pub mod completion;
pub mod hover;

pub use action::handle_action_input;
pub use base::{handle_list_navigation, try_handle_shared, SharedHandleResult};
pub use completion::{handle_completion_input, handle_completion_input_with_popup};
pub use hover::handle_hover_input;

use crate::input::handler::{InputContext, InputResult};
use crate::view::popup::{Popup, PopupKind};
use crossterm::event::KeyEvent;

/// Dispatch input handling to the appropriate handler based on popup kind.
///
/// This is the main entry point for popup-specific input handling.
pub fn handle_popup_input(
    event: &KeyEvent,
    popup: &mut Popup,
    ctx: &mut InputContext,
) -> InputResult {
    match popup.kind {
        PopupKind::Completion => handle_completion_input_with_popup(event, popup, ctx),
        PopupKind::Hover => handle_hover_input(event, popup, ctx),
        PopupKind::Action => handle_action_input(event, popup, ctx),
        PopupKind::List | PopupKind::Text => {
            // Generic list/text popups use the default action-like behavior
            handle_action_input(event, popup, ctx)
        }
    }
}
