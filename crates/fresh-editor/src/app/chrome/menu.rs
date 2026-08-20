//! What is left of the menu bar's chrome component: its layer entry and its
//! keyboard grab.
//!
//! Paint and pointer input have both migrated. The bar row is a native region
//! in the shell's tree, the open dropdown chain is a stack of `Layer`s, and
//! the full-frame close guard is the outermost layer's `OUTSIDE_POINTER`
//! dismissal.

use anyhow::Result as AnyhowResult;

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct Menu;

impl ChromeComponent for Menu {
    fn collect(&self, _ed: &Editor, _t: &mut ChromeTreeBuilder) {
        // Nothing. The bar row is a native region in the shell's tree and the
        // open chain is a stack of `Layer`s; both answer the pointer
        // themselves. The three boxes that used to be pushed here —
        // `chrome:menu_bar`, `chrome:menu_dropdown` per level, and the
        // full-frame `chrome:menu_close_guard` — are gone with them: the guard
        // is `Dismiss::OUTSIDE_POINTER` on the outermost level, which is a
        // property of the layer rather than a rectangle someone has to push,
        // rank and keep in sync.
        //
        // The keyboard grab (`on_layer_key`) and the layer entry below have
        // not migrated yet.
    }

    fn layers(&self, ed: &Editor, out: &mut Vec<(u16, crate::app::overlay::Layer)>) {
        use crate::app::overlay::{Layer, LayerKind};
        if ed.menu_state.active_menu.is_some() {
            out.push((
                super::layer_rank::MENU,
                Layer {
                    kind: LayerKind::Menu,
                    owns_keyboard: true,
                    key_context: Some(crate::input::keybindings::KeyContext::Menu),
                    blocks_terminal_input: true,
                },
            ));
        }
    }

    fn on_layer_key(
        &self,
        ed: &mut Editor,
        _layer: &crate::app::overlay::Layer,
        event: &crossterm::event::KeyEvent,
    ) -> Option<AnyhowResult<crate::input::handler::InputResult>> {
        use crate::input::handler::{InputContext, InputHandler};
        // A `menu`-context binding is consulted first. `MenuInputHandler` is
        // capture-all — its final arm consumes every key it doesn't recognise —
        // so without this the `menu` section of every keymap is decorative:
        // nothing a user or a keymap binds there can ever fire (Emacs `C-n` /
        // `C-g` in an open menu did nothing at all).
        //
        // Narrowly scoped to the menu's own actions, so a binding that means
        // something else keeps whatever route it has today, and the handler
        // still owns navigation, mnemonics and dismissal for every key the
        // keymap leaves alone.
        if let Some(action) = self.menu_action_binding(ed, event) {
            return Some(
                ed.handle_action(action)
                    .map(|_| crate::input::handler::InputResult::Consumed),
            );
        }

        // An open menu is capture-all: navigation, mnemonics and
        // dismissal all belong to `MenuInputHandler` while its layer
        // is up.
        let mut ctx = InputContext::new();
        let all_menus: Vec<crate::config::Menu> = ed
            .menus
            .menus
            .iter()
            .chain(ed.menu_state.plugin_menus.iter())
            .cloned()
            .collect();
        let result = {
            let mut handler =
                crate::view::ui::MenuInputHandler::new(&mut ed.menu_state, &all_menus);
            handler.dispatch_input(event, &mut ctx)
        };
        ed.process_deferred_actions(ctx);
        Some(Ok(result))
    }
}

impl Menu {
    /// The menu action this key resolves to in the `menu` context, if any.
    ///
    /// Only the menu's own navigation and dismissal actions qualify, so this
    /// can change which menu operation a key performs and nothing else.
    /// `menu_open` is deliberately excluded: the mnemonics it powers are the
    /// handler's business, and re-opening a menu from inside one is not a
    /// menu-navigation step.
    fn menu_action_binding(
        &self,
        ed: &Editor,
        event: &crossterm::event::KeyEvent,
    ) -> Option<crate::input::keybindings::Action> {
        use crate::input::keybindings::{Action, KeyContext};
        if ed.get_key_context() != KeyContext::Menu {
            return None;
        }
        let action = ed.keybindings.read().ok()?.resolve(event, KeyContext::Menu);
        matches!(
            action,
            Action::MenuUp
                | Action::MenuDown
                | Action::MenuLeft
                | Action::MenuRight
                | Action::MenuExecute
                | Action::MenuClose
        )
        .then_some(action)
    }
}
