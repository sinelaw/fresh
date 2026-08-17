//! Context menu (right-click menus): the menu box and its full-frame
//! close guard.

use crate::app::types::HoverTarget;
use crate::widgets::LayoutBox;
use anyhow::Result as AnyhowResult;

use super::{ChromeComponent, ChromePointer, ChromeTreeBuilder, Disposition, Editor, PointerPress};

pub(crate) struct ContextMenu;

impl ChromeComponent for ContextMenu {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        if let Some(core) = ed.active_window().context_menu_core() {
            let frame = ed.active_chrome().last_frame;
            let r = core.rect(frame.width, frame.height);
            t.rect("chrome:context_menu", 180, r);
            // TRUE full-frame semantics: a click outside the menu box
            // dismisses it and is consumed.
            t.full("chrome:context_menu_close_guard", 180);
        }
    }

    fn on_hover_change(
        &self,
        ed: &mut Editor,
        _old: Option<&HoverTarget>,
        new: Option<&HoverTarget>,
        _col: u16,
        _row: u16,
    ) -> bool {
        // Hovering an item in whichever native context menu is open
        // moves its highlight. One handler covers all three menus via
        // the shared core.
        if let Some(HoverTarget::ContextMenuItem(item_idx)) = new {
            if let Some(core) = ed.active_window_mut().context_menu_core_mut() {
                if core.highlighted != *item_idx {
                    core.highlighted = *item_idx;
                    return true;
                }
            }
        }
        false
    }

    fn hover(&self, ed: &mut Editor, bx: &LayoutBox, col: u16, row: u16) -> Option<HoverTarget> {
        if bx.kind != "chrome:context_menu" {
            return None;
        }
        // The native context menus (tab / "+" new-tab / file-explorer)
        // share one geometry core, so a single hit-test over the open
        // menu covers all three. An interior (item) row yields a hover
        // target; border rows and outside positions fall through.
        let core = ed.active_window().context_menu_core()?;
        if let crate::app::types::ContextMenuHit::Item(item_idx) = core.hit(
            col,
            row,
            ed.active_chrome().last_frame.width,
            ed.active_chrome().last_frame.height,
        ) {
            return Some(HoverTarget::ContextMenuItem(item_idx));
        }
        None
    }

    fn on_pointer(
        &self,
        ed: &mut Editor,
        bx: &LayoutBox,
        ev: &ChromePointer,
    ) -> AnyhowResult<Disposition> {
        match (ev.press, bx.kind) {
            (PointerPress::Left, "chrome:context_menu") => {
                if let Some(r) = ed.handle_click_context_menus(ev.col, ev.row) {
                    r?;
                    return Ok(Disposition::Consumed);
                }
                return Ok(Disposition::Pass);
            }
            (PointerPress::Left, "chrome:context_menu_close_guard") => {
                // Outside the menu's rect (which claimed inside clicks
                // above): dismiss and consume.
                if ed.active_window().open_context_menu().is_some() {
                    ed.active_window_mut().close_context_menus();
                    return Ok(Disposition::Consumed);
                }
                return Ok(Disposition::Pass);
            }
            (PointerPress::Right, "chrome:context_menu") => {}
            _ => return Ok(Disposition::Pass),
        }
        // A right-click inside an already-open native context menu
        // (file-explorer or tab) is swallowed so the menu stays put
        // rather than being re-opened / re-targeted.
        let frame_w = ed.active_chrome().last_frame.width;
        let frame_h = ed.active_chrome().last_frame.height;
        if let Some(core) = ed.active_window().context_menu_core() {
            if !matches!(
                core.hit(ev.col, ev.row, frame_w, frame_h),
                crate::app::types::ContextMenuHit::Outside
            ) {
                return Ok(Disposition::Consumed);
            }
        }
        Ok(Disposition::Pass)
    }

    /// The open native context menu (tab / "+" new-tab /
    /// file-explorer / close-split) grabs the keyboard: navigation
    /// and activation on unmodified keys, everything else swallowed
    /// (#2587). One handler covers all of them via the shared
    /// geometry core.
    fn on_key(
        &self,
        ed: &mut Editor,
        code: crossterm::event::KeyCode,
        modifiers: crossterm::event::KeyModifiers,
    ) -> Option<AnyhowResult<()>> {
        ed.handle_context_menu_key(code, modifiers)
    }

    fn layers(&self, ed: &Editor, out: &mut Vec<(u16, crate::app::overlay::Layer)>) {
        use crate::app::overlay::{Layer, LayerKind};
        // The native context menus are modal chrome: while one is open
        // it owns the keyboard via the custom dispatcher (`on_key`
        // above), so no `KeyContext` is exposed. Like any covering
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
    /// Route a left-click to whichever native context menu is open (tab /
    /// "+" new-tab / file-explorer). Returns `None` when no menu is open so
    /// the caller continues the normal click pipeline.
    ///
    /// The shared geometry core does the hit-test; only the *activation* of a
    /// selected item differs per menu, so that is the one part that branches
    /// on [`ContextMenuKind`]. Click-outside dismisses; border rows are inert;
    /// an item click closes the menu and runs its `execute_*` action.
    pub(super) fn handle_click_context_menus(
        &mut self,
        col: u16,
        row: u16,
    ) -> Option<AnyhowResult<()>> {
        use crate::app::types::ContextMenuHit;

        let (kind, core) = self.active_window().open_context_menu()?;
        let hit = core.hit(
            col,
            row,
            self.active_chrome().last_frame.width,
            self.active_chrome().last_frame.height,
        );
        match hit {
            // Click outside the box dismisses the menu.
            ContextMenuHit::Outside => {
                self.active_window_mut().close_context_menus();
                Some(Ok(()))
            }
            // Border rows are inert — swallow without acting or closing.
            ContextMenuHit::Border => Some(Ok(())),
            // An item click moves the highlight to it and activates through
            // the same path as a keyboard Enter.
            ContextMenuHit::Item(idx) => {
                if let Some(core) = self.active_window_mut().context_menu_core_mut() {
                    core.highlighted = idx;
                }
                Some(self.activate_highlighted_context_menu(kind))
            }
        }
    }
    /// Handle a key event while a native context menu (tab / "+" new-tab /
    /// file-explorer) is open — the one keyboard handler for all three.
    ///
    /// The open menu **grabs the keyboard**: Up/Down move the highlight,
    /// Enter activates the highlighted item, Esc dismisses, and every other
    /// key — printable characters, Backspace, modified chords — is swallowed
    /// so it can't leak into the buffer or the explorer's type-ahead find
    /// underneath and silently retarget the selection the menu acts on
    /// (#2587). Navigation/activation act only on *unmodified* keys; a
    /// modified chord is swallowed like any other non-menu key.
    ///
    /// Returns `Some` whenever a menu is open (the key is always consumed),
    /// `None` when no menu is open so normal dispatch continues.
    pub(super) fn handle_context_menu_key(
        &mut self,
        code: crossterm::event::KeyCode,
        modifiers: crossterm::event::KeyModifiers,
    ) -> Option<AnyhowResult<()>> {
        use crossterm::event::{KeyCode, KeyModifiers};

        let kind = self.active_window().open_context_menu().map(|(k, _)| k)?;

        if modifiers == KeyModifiers::NONE {
            match code {
                KeyCode::Up => {
                    if let Some(core) = self.active_window_mut().context_menu_core_mut() {
                        core.prev_item();
                    }
                    return Some(Ok(()));
                }
                KeyCode::Down => {
                    if let Some(core) = self.active_window_mut().context_menu_core_mut() {
                        core.next_item();
                    }
                    return Some(Ok(()));
                }
                KeyCode::Enter => {
                    return Some(self.activate_highlighted_context_menu(kind));
                }
                KeyCode::Esc => {
                    self.active_window_mut().close_context_menus();
                    return Some(Ok(()));
                }
                _ => {}
            }
        }

        // Modal: swallow every other key while a menu is open.
        Some(Ok(()))
    }

    /// Activate the highlighted item of the open context menu: resolve the
    /// item + its payload from the concrete menu, dismiss the menu, then run
    /// the matching `execute_*` action. Shared by both the keyboard (Enter)
    /// and mouse (click) paths so activation lives in exactly one place.
    pub(super) fn activate_highlighted_context_menu(
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
