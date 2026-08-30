//! What is left of the menu bar's chrome component: its layer entry and the
//! reaction half of hover.
//!
//! Paint, pointer and keyboard have all migrated. The bar row is a native
//! region in the shell's tree, the open dropdown chain is a stack of `Layer`s,
//! the full-frame close guard is the outermost layer's `OUTSIDE_POINTER`
//! dismissal, and the keys are shortcuts and intents on the open chain with
//! `Modality::Keyboard` owning the ones it declines.

use super::{ChromeComponent, Editor};
use crate::app::types::HoverTarget;

pub(crate) struct Menu;

impl ChromeComponent for Menu {
    /// Opening a submenu is a *reaction to hover*, and it lives here.
    ///
    /// Paint and pointer input migrated, so this component pushes no boxes —
    /// which means the legacy hover walk can no longer reach it. The shell's
    /// `UiFact::Hover` fans out to every registered component precisely so a
    /// migrated surface keeps its reactions; the menu was registered but had
    /// no `on_hover_change`, so it took the trait's `false` and
    /// `menu_hover_reaction` was left with no callers at all. Hovering a
    /// submenu parent therefore opened nothing
    /// (`test_submenu_first_item_aligns_with_parent_item`).
    ///
    /// The fan-out was the right shape; this is the half that was missing
    /// from it.
    fn on_hover_change(
        &self,
        ed: &mut Editor,
        old: Option<&HoverTarget>,
        new: Option<&HoverTarget>,
        _col: u16,
        _row: u16,
    ) -> bool {
        if old == new {
            return false;
        }
        ed.menu_hover_reaction(new)
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

    // **No keyboard here at all any more.** What was left of this handler was
    // the swallow: an open menu is modal to the keyboard, so a key it does not
    // act on must not reach the buffer underneath and type into the document.
    // The library says that now — the open chain declares
    // `Modality::Keyboard`, which owns the keys its focus chain declines while
    // leaving the bar underneath clickable — so the walk never reaches this
    // component, `MenuInputHandler` is gone, and `view/ui/menu_input.rs` with
    // it. The navigation itself has been the tree's since the bindings became
    // shortcuts on the open chain (`Editor::menu_shortcuts`).
}

impl Editor {
    /// Carry out one menu-bar navigation step.
    ///
    /// The arms `MenuInputHandler` used to hold, minus the key matching: which
    /// key means which step is the keymap's answer, declared on the open chain
    /// as shortcuts and resolved by the tree. This only says what each step
    /// does to `MenuState`.
    pub(crate) fn menu_nav(&mut self, step: crate::view::shell::msg::MenuNav) {
        use crate::view::shell::msg::MenuNav;
        let all: Vec<crate::config::Menu> = self
            .menus
            .menus
            .iter()
            .chain(self.menu_state.plugin_menus.iter())
            .cloned()
            .collect();
        let active = self.menu_state.active_menu;
        match step {
            MenuNav::PrevItem => {
                if let Some(m) = active.and_then(|i| all.get(i)) {
                    self.menu_state.prev_item(m);
                }
            }
            MenuNav::NextItem => {
                if let Some(m) = active.and_then(|i| all.get(i)) {
                    self.menu_state.next_item(m);
                }
            }
            // One fact, two meanings, decided by where the chain is: inside a
            // submenu these close and open a level, at the top they step
            // between menus. That is what Left and Right have always meant on
            // a menu bar.
            MenuNav::Back => {
                if !self.menu_state.close_submenu() {
                    self.menu_state.prev_menu(&all);
                }
            }
            MenuNav::Forward => {
                if !self.menu_state.open_submenu(&all) {
                    self.menu_state.next_menu(&all);
                }
            }
            MenuNav::First => self.menu_state.highlighted_item = Some(0),
            MenuNav::Last => {
                if let Some(m) = active.and_then(|i| all.get(i)) {
                    if let Some(items) = self.menu_state.get_current_items_cloned(m) {
                        if !items.is_empty() {
                            self.menu_state.highlighted_item = Some(items.len() - 1);
                        }
                    }
                }
            }
            MenuNav::Activate => {
                if self.menu_state.is_highlighted_submenu(&all) {
                    self.menu_state.open_submenu(&all);
                    return;
                }
                if let Some((action, args)) = self.menu_state.get_highlighted_action(&all) {
                    // Close before running: the action may open a prompt or a
                    // modal, and a menu still showing over it is the stale
                    // overlay `dismiss_menu_popups_for_prompt` exists to
                    // prevent. The deferred path closed after for the same
                    // reason, in the other order — both close, and the effect
                    // is the same because `handle_action` cannot re-open this
                    // menu.
                    self.close_menu_with_auto_hide();
                    if let Some(a) = self.menu_action_to_action(&action, args) {
                        if let Err(e) = self.handle_action(a) {
                            tracing::warn!("menu action {action} failed: {e}");
                        }
                    }
                }
            }
        }
    }
}

impl Editor {
    /// The `menu` section of the keymap, as shortcuts for the open chain.
    ///
    /// **This is the direction that fixes the bug.** The keymap used to be
    /// consulted from inside a key handler that ran *after* the shell had
    /// already been offered the key — so a capture-all menu handler swallowed
    /// user bindings before anyone asked what they meant. Here the bindings
    /// flow down into the description as data, and the tree resolves
    /// key → intent → action with nothing in front of it.
    ///
    /// Only the menu's own navigation actions map. A key bound in the `menu`
    /// section to something else keeps whatever route it has today, which is
    /// the same restriction `menu_action_binding` applied — it can change
    /// which menu operation a key performs and nothing else.
    pub(crate) fn menu_shortcuts(&self) -> Vec<crate::view::shell::menu::MenuShortcut> {
        use crate::input::keybindings::{Action, KeyContext};
        use crate::view::shell::menu::MenuShortcut;
        use fresh_ui::Intent;

        if self.menu_state.active_menu.is_none() {
            return Vec::new();
        }
        let Ok(kb) = self.keybindings.read() else {
            return Vec::new();
        };
        let mut out = Vec::new();
        for ((code, mods), action) in kb.bindings_in_context(KeyContext::Menu) {
            let intent = match action {
                Action::MenuUp => Intent::Up,
                Action::MenuDown => Intent::Down,
                Action::MenuLeft => Intent::Left,
                Action::MenuRight => Intent::Right,
                Action::MenuExecute => Intent::Confirm,
                Action::MenuClose => Intent::Cancel,
                _ => continue,
            };
            if let Some(code) = crate::view::shell::input::key_code(code) {
                out.push(MenuShortcut {
                    key: fresh_ui::KeyPress {
                        code,
                        mods: crate::view::shell::input::mods(mods),
                    },
                    intent,
                });
            }
        }
        out
    }
}
