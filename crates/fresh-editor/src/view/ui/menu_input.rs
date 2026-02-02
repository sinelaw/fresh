//! Input handling for the Menu system.
//!
//! Provides InputHandler implementation for menu navigation.
//! Uses a wrapper struct to bundle MenuState with menu configuration.

use super::menu::MenuState;
use crate::config::Menu;
use crate::input::handler::{DeferredAction, InputContext, InputHandler, InputResult};
use crossterm::event::{KeyCode, KeyEvent};

/// Wrapper that provides InputHandler for MenuState with menu configuration.
pub struct MenuInputHandler<'a> {
    pub state: &'a mut MenuState,
    pub menus: &'a [Menu],
}

impl<'a> MenuInputHandler<'a> {
    pub fn new(state: &'a mut MenuState, menus: &'a [Menu]) -> Self {
        Self { state, menus }
    }
}

impl InputHandler for MenuInputHandler<'_> {
    fn handle_key_event(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult {
        // Only handle if menu is active
        if self.state.active_menu.is_none() {
            return InputResult::Ignored;
        }

        match event.code {
            // Close menu
            KeyCode::Esc => {
                ctx.defer(DeferredAction::CloseMenu);
                InputResult::Consumed
            }

            // Execute/confirm
            KeyCode::Enter => {
                // Check if highlighted item is a submenu - if so, open it
                if self.state.is_highlighted_submenu(self.menus) {
                    self.state.open_submenu(self.menus);
                    return InputResult::Consumed;
                }

                // Get the action to execute
                if let Some((action, args)) = self.state.get_highlighted_action(self.menus) {
                    ctx.defer(DeferredAction::ExecuteMenuAction { action, args });
                    ctx.defer(DeferredAction::CloseMenu);
                }
                InputResult::Consumed
            }

            // Navigation
            KeyCode::Up | KeyCode::Char('k') if event.modifiers.is_empty() => {
                if let Some(active_idx) = self.state.active_menu {
                    if let Some(menu) = self.menus.get(active_idx) {
                        self.state.prev_item(menu);
                    }
                }
                InputResult::Consumed
            }
            KeyCode::Down | KeyCode::Char('j') if event.modifiers.is_empty() => {
                if let Some(active_idx) = self.state.active_menu {
                    if let Some(menu) = self.menus.get(active_idx) {
                        self.state.next_item(menu);
                    }
                }
                InputResult::Consumed
            }
            KeyCode::Left | KeyCode::Char('h') if event.modifiers.is_empty() => {
                // If in a submenu, close it and go back to parent
                // Otherwise, go to the previous menu
                if !self.state.close_submenu() {
                    self.state.prev_menu(self.menus);
                }
                InputResult::Consumed
            }
            KeyCode::Right | KeyCode::Char('l') if event.modifiers.is_empty() => {
                // If on a submenu item, open it
                // Otherwise, go to the next menu
                if !self.state.open_submenu(self.menus) {
                    self.state.next_menu(self.menus);
                }
                InputResult::Consumed
            }

            // Home/End for quick navigation
            KeyCode::Home => {
                self.state.highlighted_item = Some(0);
                InputResult::Consumed
            }
            KeyCode::End => {
                if let Some(active_idx) = self.state.active_menu {
                    if let Some(menu) = self.menus.get(active_idx) {
                        if let Some(items) = self.state.get_current_items_cloned(menu) {
                            if !items.is_empty() {
                                self.state.highlighted_item = Some(items.len() - 1);
                            }
                        }
                    }
                }
                InputResult::Consumed
            }

            // Consume all other keys (modal behavior)
            _ => InputResult::Consumed,
        }
    }

    fn is_modal(&self) -> bool {
        self.state.active_menu.is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::MenuItem;
    use crossterm::event::KeyModifiers;
    use std::collections::HashMap;

    fn key(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::NONE)
    }

    fn create_test_menus() -> Vec<Menu> {
        vec![
            Menu {
                id: None,
                label: "File".to_string(),
                items: vec![
                    MenuItem::Action {
                        label: "New".to_string(),
                        action: "new_file".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Separator { separator: true },
                    MenuItem::Action {
                        label: "Save".to_string(),
                        action: "save".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                ],
                when: None,
            },
            Menu {
                id: None,
                label: "Edit".to_string(),
                items: vec![
                    MenuItem::Action {
                        label: "Undo".to_string(),
                        action: "undo".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                    MenuItem::Action {
                        label: "Redo".to_string(),
                        action: "redo".to_string(),
                        args: HashMap::new(),
                        when: None,
                        checkbox: None,
                    },
                ],
                when: None,
            },
        ]
    }

    #[test]
    fn test_menu_navigation_down() {
        let menus = create_test_menus();
        let mut state = MenuState::for_testing();
        state.open_menu(0);

        let mut handler = MenuInputHandler::new(&mut state, &menus);
        let mut ctx = InputContext::new();

        // Initially at first item
        assert_eq!(handler.state.highlighted_item, Some(0));

        // Down arrow moves to next (skipping separator)
        handler.handle_key_event(&key(KeyCode::Down), &mut ctx);
        assert_eq!(handler.state.highlighted_item, Some(2)); // Skipped separator at 1
    }

    #[test]
    fn test_menu_navigation_between_menus() {
        let menus = create_test_menus();
        let mut state = MenuState::for_testing();
        state.open_menu(0);

        let mut handler = MenuInputHandler::new(&mut state, &menus);
        let mut ctx = InputContext::new();

        // Initially on File menu
        assert_eq!(handler.state.active_menu, Some(0));

        // Right arrow moves to next menu
        handler.handle_key_event(&key(KeyCode::Right), &mut ctx);
        assert_eq!(handler.state.active_menu, Some(1));

        // Left arrow moves back
        handler.handle_key_event(&key(KeyCode::Left), &mut ctx);
        assert_eq!(handler.state.active_menu, Some(0));
    }

    #[test]
    fn test_menu_escape_closes() {
        let menus = create_test_menus();
        let mut state = MenuState::for_testing();
        state.open_menu(0);

        let mut handler = MenuInputHandler::new(&mut state, &menus);
        let mut ctx = InputContext::new();

        handler.handle_key_event(&key(KeyCode::Esc), &mut ctx);
        assert!(ctx
            .deferred_actions
            .iter()
            .any(|a| matches!(a, DeferredAction::CloseMenu)));
    }

    #[test]
    fn test_menu_enter_executes() {
        let menus = create_test_menus();
        let mut state = MenuState::for_testing();
        state.open_menu(0);
        state.highlighted_item = Some(0); // "New" action

        let mut handler = MenuInputHandler::new(&mut state, &menus);
        let mut ctx = InputContext::new();

        handler.handle_key_event(&key(KeyCode::Enter), &mut ctx);
        assert!(ctx.deferred_actions.iter().any(|a| matches!(
            a,
            DeferredAction::ExecuteMenuAction { action, .. } if action == "new_file"
        )));
    }

    #[test]
    fn test_menu_is_modal_when_active() {
        let menus = create_test_menus();
        let mut state = MenuState::for_testing();

        let handler = MenuInputHandler::new(&mut state, &menus);
        assert!(!handler.is_modal());

        state.open_menu(0);
        let handler = MenuInputHandler::new(&mut state, &menus);
        assert!(handler.is_modal());
    }

    #[test]
    fn test_menu_ignored_when_inactive() {
        let menus = create_test_menus();
        let mut state = MenuState::for_testing();

        let mut handler = MenuInputHandler::new(&mut state, &menus);
        let mut ctx = InputContext::new();

        let result = handler.handle_key_event(&key(KeyCode::Down), &mut ctx);
        assert_eq!(result, InputResult::Ignored);
    }
}
