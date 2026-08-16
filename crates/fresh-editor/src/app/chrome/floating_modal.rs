//! The centered floating widget panel (modal dialogs).

use crate::widgets::LayoutBox;

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct FloatingModal;

impl ChromeComponent for FloatingModal {
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
}
