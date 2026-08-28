//! Context menu (right-click menus): the menu box and its full-frame
//! close guard.

use anyhow::Result as AnyhowResult;

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct ContextMenu;

impl ChromeComponent for ContextMenu {
    fn collect(&self, _ed: &Editor, _t: &mut ChromeTreeBuilder) {
        // Nothing. The menu is a `Layer` in the shell's tree, and its pointer
        // behaviour comes from properties rather than boxes: `Modality::Inert`
        // makes everything outside non-interactive — which is what the
        // full-frame close-guard box simulated — and `OUTSIDE_POINTER`
        // dismissal closes it. The shell is offered the pointer before this
        // walk runs, so neither box has anything left to do.
        //
        // The keyboard grab that sat beside them is gone too — the pre-band
        // stage it belonged to has no members left. The layer entry below is
        // what answers keys now.
    }

    /// The last thing this component does.
    ///
    /// Paint, pointer and keyboard have all moved to the `Layer` in the
    /// shell's tree, which declares `Modality::Exclusive` — the same two facts
    /// this entry states by hand: nothing outside is interactive, and the
    /// terminal takes no raw input beneath it.
    ///
    /// It stays because the PTY gate reads `blocks_terminal_input` off the
    /// overlay stack, while the library derives the same thing from
    /// `raw_input()` — which is only meaningful once host leaves *declare*
    /// that they take raw input. Today every region is a `PlainHost`, whose
    /// `takes_raw_input` is false, so deriving it now would report that the
    /// terminal is blocked on every frame. It retires with the terminal grid's
    /// own host leaf (S5), and `layer_rank::CONTEXT_MENU` goes with it.
    fn layers(&self, ed: &Editor, out: &mut Vec<(u16, crate::app::overlay::Layer)>) {
        use crate::app::overlay::{Layer, LayerKind};
        // While one is open it owns the keyboard — from the tree now, not
        // from a pre-band grab here — so no `KeyContext` is exposed. Like any covering
        // overlay it blocks PTY routing. Ranked below `Popup` so the
        // unfocused-popup `take_while` guard is unaffected. One entry
        // covers all four menus — they share the geometry core and are
        // mutually exclusive (opening one closes the others).
        if ed.active_window().open_context_menu().is_some() {
            out.push((
                super::layer_rank::CONTEXT_MENU,
                Layer {
                    kind: LayerKind::ContextMenu,
                    owns_keyboard: true,
                    key_context: None,
                    blocks_terminal_input: true,
                },
            ));
        }
    }
}

/// Behavior owned by this component (moved from mouse_input.rs —
/// the handlers its arms dispatch to).
impl Editor {
    /// Activate the highlighted item of the open context menu: resolve the
    /// item + its payload from the concrete menu, dismiss the menu, then run
    /// the matching `execute_*` action. Shared by both the keyboard (Enter)
    /// and mouse (click) paths so activation lives in exactly one place — the
    /// pointer path now reaches it from the shell (`apply_ui_fact`), the
    /// keyboard path still from this component.
    pub(crate) fn activate_highlighted_context_menu(
        &mut self,
        kind: crate::app::types::ContextMenuKind,
    ) -> AnyhowResult<()> {
        use crate::app::types::ContextMenuKind;
        match kind {
            ContextMenuKind::Tab => {
                let selected = self
                    .active_window()
                    .tab_context_menu
                    .as_ref()
                    .map(|m| (m.highlighted_item(), m.buffer_id, m.split_id));
                self.active_window_mut().close_context_menus();
                if let Some((item, buffer_id, split_id)) = selected {
                    return self.execute_tab_context_menu_action(item, buffer_id, split_id);
                }
            }
            ContextMenuKind::NewTab => {
                let selected = self
                    .active_window()
                    .new_tab_menu
                    .as_ref()
                    .map(|m| (m.highlighted_item(), m.split_id));
                self.active_window_mut().close_context_menus();
                if let Some((item, split_id)) = selected {
                    return self.execute_new_tab_menu_action(item, split_id);
                }
            }
            ContextMenuKind::FileExplorer => {
                let selected = self
                    .active_window()
                    .file_explorer_context_menu
                    .as_ref()
                    .map(|m| m.highlighted_item());
                self.active_window_mut().close_context_menus();
                if let Some(item) = selected {
                    self.execute_file_explorer_context_menu_action(item);
                }
            }
            ContextMenuKind::CloseSplit => {
                let selected = self
                    .active_window()
                    .close_split_menu
                    .as_ref()
                    .map(|m| (m.highlighted_item(), m.split_id));
                self.active_window_mut().close_context_menus();
                if let Some((item, split_id)) = selected {
                    self.execute_close_split_menu_action(item, split_id);
                }
            }
        }
        Ok(())
    }
}
