//! The centered floating widget panel (modal dialogs).

use crate::widgets::LayoutBox;

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

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
