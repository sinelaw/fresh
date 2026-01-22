//! Input handling for Popups.
//!
//! Implements the InputHandler trait for PopupManager.
//! Delegates to popup-specific handlers based on PopupKind.

use super::popup::input::handle_popup_input;
use super::popup::PopupManager;
use crate::input::handler::{InputContext, InputHandler, InputResult};
use crossterm::event::KeyEvent;

impl InputHandler for PopupManager {
    fn handle_key_event(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult {
        // Only handle if there are active popups
        if !self.is_visible() {
            return InputResult::Ignored;
        }

        // Get the topmost popup and delegate to the appropriate handler
        if let Some(popup) = self.top_mut() {
            handle_popup_input(event, popup, ctx)
        } else {
            InputResult::Ignored
        }
    }

    fn is_modal(&self) -> bool {
        self.is_visible()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::input::handler::DeferredAction;
    use crate::view::popup::{Popup, PopupKind, PopupListItem};
    use crate::view::theme;
    use crate::view::theme::Theme;
    use crossterm::event::{KeyCode, KeyModifiers};

    fn key(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::NONE)
    }

    fn create_popup_with_items(count: usize) -> PopupManager {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let items: Vec<PopupListItem> = (0..count)
            .map(|i| PopupListItem::new(format!("Item {}", i)))
            .collect();
        let popup = Popup::list(items, &theme).with_kind(PopupKind::Action);
        let mut manager = PopupManager::new();
        manager.show(popup);
        manager
    }

    #[test]
    fn test_popup_navigation() {
        let mut manager = create_popup_with_items(5);
        let mut ctx = InputContext::new();

        // Initially at item 0
        assert_eq!(
            manager.top().unwrap().selected_item().unwrap().text,
            "Item 0"
        );

        // Down arrow moves to next
        manager.handle_key_event(&key(KeyCode::Down), &mut ctx);
        assert_eq!(
            manager.top().unwrap().selected_item().unwrap().text,
            "Item 1"
        );

        // Up arrow moves back
        manager.handle_key_event(&key(KeyCode::Up), &mut ctx);
        assert_eq!(
            manager.top().unwrap().selected_item().unwrap().text,
            "Item 0"
        );
    }

    #[test]
    fn test_popup_enter_confirms() {
        let mut manager = create_popup_with_items(3);
        let mut ctx = InputContext::new();

        manager.handle_key_event(&key(KeyCode::Enter), &mut ctx);
        assert!(ctx
            .deferred_actions
            .iter()
            .any(|a| matches!(a, DeferredAction::ConfirmPopup)));
    }

    #[test]
    fn test_popup_escape_cancels() {
        let mut manager = create_popup_with_items(3);
        let mut ctx = InputContext::new();

        manager.handle_key_event(&key(KeyCode::Esc), &mut ctx);
        assert!(ctx
            .deferred_actions
            .iter()
            .any(|a| matches!(a, DeferredAction::ClosePopup)));
    }

    #[test]
    fn test_popup_is_modal_when_visible() {
        let mut manager = PopupManager::new();
        assert!(!manager.is_modal());

        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        manager.show(Popup::text(vec!["test".to_string()], &theme));
        assert!(manager.is_modal());
    }

    #[test]
    fn test_popup_ignored_when_empty() {
        let mut manager = PopupManager::new();
        let mut ctx = InputContext::new();

        let result = manager.handle_key_event(&key(KeyCode::Down), &mut ctx);
        assert_eq!(result, InputResult::Ignored);
    }
}
