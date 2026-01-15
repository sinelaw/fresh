//! Input handling for Popups.
//!
//! Implements the InputHandler trait for PopupManager, handling
//! selection navigation and confirmation/cancellation.

use super::popup::PopupManager;
use crate::input::handler::{DeferredAction, InputContext, InputHandler, InputResult};
use crossterm::event::{KeyCode, KeyEvent};

impl InputHandler for PopupManager {
    fn handle_key_event(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult {
        // Only handle if there are active popups
        if !self.is_visible() {
            return InputResult::Ignored;
        }

        match event.code {
            // Confirmation and cancellation
            KeyCode::Enter => {
                ctx.defer(DeferredAction::ConfirmPopup);
                InputResult::Consumed
            }
            KeyCode::Esc => {
                ctx.defer(DeferredAction::ClosePopup);
                InputResult::Consumed
            }

            // Selection navigation
            KeyCode::Up | KeyCode::Char('k') if event.modifiers.is_empty() => {
                if let Some(popup) = self.top_mut() {
                    popup.select_prev();
                }
                InputResult::Consumed
            }
            KeyCode::Down | KeyCode::Char('j') if event.modifiers.is_empty() => {
                if let Some(popup) = self.top_mut() {
                    popup.select_next();
                }
                InputResult::Consumed
            }
            KeyCode::PageUp => {
                if let Some(popup) = self.top_mut() {
                    popup.page_up();
                }
                InputResult::Consumed
            }
            KeyCode::PageDown => {
                if let Some(popup) = self.top_mut() {
                    popup.page_down();
                }
                InputResult::Consumed
            }
            KeyCode::Home => {
                if let Some(popup) = self.top_mut() {
                    popup.select_first();
                }
                InputResult::Consumed
            }
            KeyCode::End => {
                if let Some(popup) = self.top_mut() {
                    popup.select_last();
                }
                InputResult::Consumed
            }

            // Tab also navigates
            KeyCode::Tab if event.modifiers.is_empty() => {
                if let Some(popup) = self.top_mut() {
                    popup.select_next();
                }
                InputResult::Consumed
            }
            KeyCode::BackTab => {
                if let Some(popup) = self.top_mut() {
                    popup.select_prev();
                }
                InputResult::Consumed
            }

            // Type-to-filter for completion popups
            KeyCode::Char(c) if event.modifiers.is_empty() => {
                // Check if this is a completion popup that supports type-to-filter
                if self.is_completion_popup() {
                    ctx.defer(DeferredAction::PopupTypeChar(c));
                }
                InputResult::Consumed
            }

            // Backspace for type-to-filter in completion popups
            KeyCode::Backspace if event.modifiers.is_empty() => {
                if self.is_completion_popup() {
                    ctx.defer(DeferredAction::PopupBackspace);
                }
                InputResult::Consumed
            }

            // Consume all other keys (modal behavior)
            _ => InputResult::Consumed,
        }
    }

    fn is_modal(&self) -> bool {
        self.is_visible()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::popup::{Popup, PopupListItem};
    use crate::view::theme;
    use crate::view::theme::Theme;
    use crossterm::event::KeyModifiers;

    fn key(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::NONE)
    }

    fn create_popup_with_items(count: usize) -> PopupManager {
        let theme = Theme::from_name(theme::THEME_DARK).unwrap();
        let items: Vec<PopupListItem> = (0..count)
            .map(|i| PopupListItem::new(format!("Item {}", i)))
            .collect();
        let popup = Popup::list(items, &theme);
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

        // j/k also work
        manager.handle_key_event(
            &KeyEvent::new(KeyCode::Char('j'), KeyModifiers::NONE),
            &mut ctx,
        );
        assert_eq!(
            manager.top().unwrap().selected_item().unwrap().text,
            "Item 1"
        );

        manager.handle_key_event(
            &KeyEvent::new(KeyCode::Char('k'), KeyModifiers::NONE),
            &mut ctx,
        );
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

        let theme = Theme::from_name(theme::THEME_DARK).unwrap();
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
