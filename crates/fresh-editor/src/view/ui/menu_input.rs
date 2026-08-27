//! What an open menu does with a key it does not act on: nothing, loudly.
//!
//! Menu *navigation* is no longer here. Every arm this file used to hold is an
//! `Intent` the open chain declares, bound to keys by the keymap and resolved
//! by the tree before the legacy walk runs — see `shell::menu::menu_intents`
//! and `Editor::menu_shortcuts`.
//!
//! The swallow is what remains, and it is load-bearing: an open menu is modal
//! to the keyboard, so a key it does not act on must not reach the buffer and
//! type into the document. `Modality` cannot express that today — it gates
//! pointer routing and focus traversal, and the dropdown chain is deliberately
//! `Modality::None` so the bar stays clickable for switching menus. This file
//! retires when keyboard-only modality does.

use super::menu::MenuState;
use crate::config::Menu;
use crate::input::handler::{InputContext, InputHandler, InputResult};
use crossterm::event::KeyEvent;

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
    fn handle_key_event(&mut self, event: &KeyEvent, _ctx: &mut InputContext) -> InputResult {
        // Only handle if menu is active
        if self.state.active_menu.is_none() {
            return InputResult::Ignored;
        }

        // **Navigation is the tree's.** Every arm that used to be here — Esc,
        // Enter, the arrows and `hjkl`, Home, End — is now an `Intent` the
        // open chain declares and the keymap binds keys to
        // (`Editor::menu_shortcuts`, `shell::menu::menu_intents`). The shell
        // is offered the key first, so those resolve before this handler runs
        // at all.
        //
        // What is left is the swallow, and it is the reason this handler still
        // exists: an open menu is modal to the keyboard, so a key it does not
        // act on must not reach the buffer underneath and type into the
        // document. `Modality` cannot say that yet — it gates pointer routing
        // and focus traversal, and the chain is deliberately `Modality::None`
        // so the bar stays clickable for switching menus. When keyboard-only
        // modality exists in the library, this file goes.
        let _ = event;
        InputResult::Consumed
    }

    fn is_modal(&self) -> bool {
        self.state.active_menu.is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::MenuItem;
    use crossterm::event::{KeyCode, KeyModifiers};
    use std::collections::HashMap;

    fn key(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::NONE)
    }

    fn menus() -> Vec<Menu> {
        vec![Menu {
            id: None,
            when: None,
            label: "File".to_string(),
            items: vec![MenuItem::Action {
                label: "New".to_string(),
                action: "new_file".to_string(),
                args: HashMap::new(),
                when: None,
                checkbox: None,
            }],
        }]
    }

    /// The navigation tests that used to live here have moved to
    /// `view::shell::menu::intent_tests`, because navigation has: keys resolve
    /// to intents on the open chain, and the keymap binds them. What is tested
    /// here is the one thing left — that an open menu is modal to the keyboard.
    #[test]
    fn an_open_menu_swallows_what_it_does_not_act_on() {
        let all = menus();
        let mut state = MenuState::for_testing();
        state.open_menu(0);
        let mut ctx = InputContext::new();
        let mut h = MenuInputHandler::new(&mut state, &all);
        // A printable key must not reach the buffer and type into it.
        assert_eq!(
            h.handle_key_event(&key(KeyCode::Char('x')), &mut ctx),
            InputResult::Consumed
        );
    }

    /// With no menu open it claims nothing, so the key takes its ordinary
    /// route.
    #[test]
    fn a_closed_menu_claims_nothing() {
        let all = menus();
        let mut state = MenuState::for_testing();
        let mut ctx = InputContext::new();
        let mut h = MenuInputHandler::new(&mut state, &all);
        assert_eq!(
            h.handle_key_event(&key(KeyCode::Char('x')), &mut ctx),
            InputResult::Ignored
        );
    }

    #[test]
    fn is_modal_follows_the_open_menu() {
        let all = menus();
        let mut state = MenuState::for_testing();
        assert!(!MenuInputHandler::new(&mut state, &all).is_modal());
        state.open_menu(0);
        assert!(MenuInputHandler::new(&mut state, &all).is_modal());
    }
}
