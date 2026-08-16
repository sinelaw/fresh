//! The left dock column (orchestrator sessions panel).

use crate::widgets::LayoutBox;

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct Dock;

impl ChromeComponent for Dock {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        if let Some(dock) = &ed.dock {
            if let Some(inner) = dock.last_inner_rect {
                t.rect("chrome:dock", 130, inner);
            }
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
        if ed.handle_floating_widget_panel_wheel(crate::app::PanelSlot::Dock, col, row, delta) {
            Ok(super::Disposition::Consumed)
        } else {
            Ok(super::Disposition::Pass)
        }
    }

    fn layers(&self, ed: &Editor, out: &mut Vec<(u16, crate::app::overlay::Layer)>) {
        use crate::app::overlay::{Layer, LayerKind};
        // Owns the keyboard only while focused; a blurred dock stays
        // visible but lets the buffer underneath keep the keyboard
        // AND receive PTY routing (the dock lives beside the chrome,
        // not over it).
        if let Some(d) = ed.dock.as_ref() {
            out.push((
                super::layer_rank::DOCK,
                Layer {
                    kind: LayerKind::Dock,
                    owns_keyboard: d.focused,
                    key_context: Some(crate::input::keybindings::KeyContext::Dock),
                    blocks_terminal_input: d.focused,
                },
            ));
        }
    }
}
