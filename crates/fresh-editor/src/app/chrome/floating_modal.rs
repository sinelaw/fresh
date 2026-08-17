//! The centered floating widget panel (modal dialogs).

use crate::widgets::LayoutBox;
use anyhow::Result as AnyhowResult;

use super::{in_rect, ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct FloatingModal;

impl ChromeComponent for FloatingModal {
    /// The centered widget modal captures the whole mouse channel
    /// while mounted (see `handle_floating_modal_mouse`: clicks
    /// hit-test the panel, wheel scrolls it, drags drive only its
    /// scrollbar, everything else — and every press outside the box —
    /// is swallowed so nothing reaches the buffer, terminal, or dock
    /// beneath).
    fn capture_mouse(
        &self,
        ed: &mut Editor,
        ev: crossterm::event::MouseEvent,
        _is_double_click: bool,
    ) -> Option<anyhow::Result<bool>> {
        if ed.floating_widget_panel.is_some() {
            return Some(ed.handle_floating_modal_mouse(ev));
        }
        None
    }

    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        if ed.floating_widget_panel.is_some() {
            // A centered modal consumes the wheel even on a miss.
            t.full("chrome:floating_panel", 130);
        }
    }

    fn on_wheel(
        &self,
        ed: &mut Editor,
        _bx: &LayoutBox,
        col: u16,
        row: u16,
        delta: i32,
    ) -> anyhow::Result<super::Disposition> {
        // A centered modal consumes the wheel even on a miss.
        ed.handle_floating_widget_panel_wheel(crate::app::PanelSlot::Floating, col, row, delta);
        Ok(super::Disposition::Consumed)
    }

    fn on_key(
        &self,
        ed: &mut Editor,
        code: crossterm::event::KeyCode,
        modifiers: crossterm::event::KeyModifiers,
    ) -> Option<anyhow::Result<()>> {
        // The focused floating panel claims all keys while visible.
        // Esc unmounts + fires a `widget_event` "cancel"; smart-key
        // names (Tab/Return/Backspace/…/Up/Down) route through the
        // widget command dispatcher; printable chars feed
        // `textInputChar` to the focused TextInput. Registered before
        // Dock, so a focused centered modal takes keyboard precedence
        // over the dock (the New-Session form opened on top of it) —
        // ONE precedence source: this registry order matches the
        // `layers()` ranks (FLOATING_MODAL 820 > DOCK 810).
        if ed.floating_widget_panel.as_ref().is_some_and(|f| f.focused)
            && ed.dispatch_floating_widget_key(crate::app::PanelSlot::Floating, code, modifiers)
        {
            return Some(Ok(()));
        }
        None
    }

    fn layers(&self, ed: &Editor, out: &mut Vec<(u16, crate::app::overlay::Layer)>) {
        use crate::app::overlay::{Layer, LayerKind};
        // Owns the keyboard when focused; resolves as `Normal`
        // regardless of the underlying buffer's (possibly stale)
        // context so mode-keybinding lookups still fire for the
        // panel's own chords. Blocks PTY routing whenever present —
        // it sits on top of (and obscures) the active terminal.
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
    /// The dialog is fully modal: presses hit-test the panel (focusing the
    /// clicked widget / placing the text cursor), wheel scrolls it, and a
    /// drag drives only its scrollbar. Every other event — and every press
    /// that lands outside the panel box — is swallowed, so nothing reaches
    /// the buffer, terminal, or dock beneath. Always returns
    /// `Ok(true)` (a render is cheap and the modal just consumed an event).
    pub(super) fn handle_floating_modal_mouse(
        &mut self,
        mouse_event: crossterm::event::MouseEvent,
    ) -> AnyhowResult<bool> {
        use crossterm::event::{MouseButton, MouseEventKind};
        let (col, row) = (mouse_event.column, mouse_event.row);
        match mouse_event.kind {
            MouseEventKind::Down(MouseButton::Left) => {
                // A press on the native modal-frame `[×]` close button
                // dismisses the panel exactly like Esc / Cancel (same
                // `dismiss_floating_panel_with_cancel` path that fires the
                // panel's `cancel` widget_event). Checked BEFORE the general
                // panel hit-test so the click never also focuses a widget in
                // the interior beneath the button.
                if let Some(cbr) = self
                    .panel(crate::app::PanelSlot::Floating)
                    .and_then(|f| f.close_button_rect)
                {
                    if in_rect(col, row, cbr) {
                        self.dismiss_floating_panel_with_cancel(crate::app::PanelSlot::Floating);
                        return Ok(true);
                    }
                }
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
                // Single / double / triple clicks all map to one panel
                // hit-test — never the buffer's word/line select beneath.
                self.handle_floating_widget_click(crate::app::PanelSlot::Floating, col, row);
            }
            MouseEventKind::Drag(MouseButton::Left) => {
                // Only a scrollbar drag is meaningful; other drags are
                // swallowed rather than starting a buffer text-selection.
                self.try_widget_scrollbar_drag(crate::app::PanelSlot::Floating, row);
            }
            MouseEventKind::Up(MouseButton::Left) => {
                self.release_widget_scrollbar();
            }
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
            MouseEventKind::Moved => {
                self.update_widget_hover(col, row, Some(crate::app::PanelSlot::Floating));
            }
            // Right-click, horizontal scroll, other-button releases:
            // swallowed — the modal eats them all.
            _ => {}
        }
        Ok(true)
    }
}
