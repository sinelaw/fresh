//! The centered floating widget panel (modal dialogs).

use anyhow::Result as AnyhowResult;

use super::{ChromeComponent, Editor};

pub(crate) struct FloatingModal;

/// ONE activity predicate, consulted by both `layers()` here and
/// `Editor::modal_slot`, which decides whose layer claims the pointer — see
/// `modals.rs` for why the pairing must be a single fn.
fn panel_up(ed: &Editor) -> bool {
    ed.floating_widget_panel.is_some()
}

impl ChromeComponent for FloatingModal {
    // **No `on_layer_key`.** As the dock's: a focused centred panel's keys
    // arrive through `view::shell::panel::keys_layer`, a `Modality::Focus`
    // layer that confines the keyboard and hands back what
    // `dispatch_floating_widget_key` declines. Declared over the dock's and
    // under the popups', which is `POPUP > FLOATING_MODAL > DOCK` without the
    // integers.

    fn layers(&self, ed: &Editor, out: &mut Vec<(u16, crate::app::overlay::Layer)>) {
        use crate::app::overlay::{Layer, LayerKind};
        // Owns the keyboard when focused; resolves as `Normal`
        // regardless of the underlying buffer's (possibly stale)
        // context so mode-keybinding lookups still fire for the
        // panel's own chords. Blocks PTY routing whenever present —
        // it sits on top of (and obscures) the active terminal.
        if !panel_up(ed) {
            return;
        }
        if let Some(f) = ed.floating_widget_panel.as_ref() {
            out.push((
                super::layer_rank::FLOATING_MODAL,
                Layer {
                    kind: LayerKind::FloatingModal,
                    owns_keyboard: f.focused,
                    key_context: Some(crate::input::keybindings::KeyContext::Normal),
                    blocks_terminal_input: true,
                },
            ));
        }
    }
}

/// Behavior owned by this component (moved from mouse_input.rs —
/// the handlers its arms dispatch to).
impl Editor {
    /// Mouse handler for the centered widget modal (`floating_widget_panel`).
    ///
    /// The dialog is fully modal: every event that reaches here is swallowed,
    /// so nothing gets to the buffer, terminal, or dock beneath. Always
    /// returns `Ok(true)` (a render is cheap and the modal just consumed an
    /// event).
    ///
    /// **What reaches here is what the panel's own nodes declined.** Its
    /// widgets answer their own presses (`hit_node`) and its box claims and
    /// stops the rest (`view::shell::panel::frame_box`), so the presses left
    /// for this handler land *outside* the box — which is why the only one it
    /// acts on is an anchored popup's dismissal. The press hit-test, the
    /// scrollbar press and the scrollbar drag that used to be here were
    /// resolved against rectangles the interior painter recorded, and that
    /// painter is deleted; they could not answer on any path (S7). The wheel
    /// is not in that class: it still routes to the panel by
    /// `last_inner_rect`, which layout writes for a described panel.
    pub(crate) fn handle_floating_modal_mouse(
        &mut self,
        mouse_event: crossterm::event::MouseEvent,
    ) -> AnyhowResult<bool> {
        use crossterm::event::{MouseButton, MouseEventKind};
        let (col, row) = (mouse_event.column, mouse_event.row);
        match mouse_event.kind {
            MouseEventKind::Down(MouseButton::Left) => {
                // The `[×]` used to be checked here, against a rectangle the
                // painter had filed — before the general hit-test, so the
                // press could not also focus a widget underneath. It is a node
                // in the tree now (`view::shell::panel`), offered the pointer
                // before this arm is ever reached, and it stops the event.
                // An anchored popup (right-click context menu) dismisses when
                // the press lands outside its box — standard menu behaviour.
                // The centered modal instead swallows outside-clicks (it has
                // explicit Cancel / Esc).
                if self.floating_panel_is_anchored()
                    && !self.point_in_floating_panel(crate::app::PanelSlot::Floating, col, row)
                {
                    self.dismiss_floating_panel_with_cancel(crate::app::PanelSlot::Floating);
                    return Ok(true);
                }
            }
            // Swallowed rather than starting a buffer text-selection.
            MouseEventKind::Drag(MouseButton::Left) => {}
            MouseEventKind::Up(MouseButton::Left) => {}
            MouseEventKind::ScrollUp => {
                self.handle_floating_widget_panel_wheel(
                    crate::app::PanelSlot::Floating,
                    col,
                    row,
                    -3,
                );
            }
            MouseEventKind::ScrollDown => {
                self.handle_floating_widget_panel_wheel(
                    crate::app::PanelSlot::Floating,
                    col,
                    row,
                    3,
                );
            }
            // The modal owns the whole mouse channel, so it has to drive
            // hover for its own panel — the normal pipeline's tracker is
            // unreachable from here. Scoped to `Floating`: the dock may
            // still be mounted underneath, and a pointer over the modal is
            // not over the dock widget it happens to cover.
            MouseEventKind::Moved => {}
            // Right-click, horizontal scroll, other-button releases:
            // swallowed — the modal eats them all.
            _ => {}
        }
        Ok(true)
    }
}
