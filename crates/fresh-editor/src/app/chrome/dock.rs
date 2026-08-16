//! The left dock column (orchestrator sessions panel).

use crate::widgets::LayoutBox;

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct Dock;

impl ChromeComponent for Dock {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        if let Some(dock) = &ed.dock {
            if let Some(inner) = dock.last_inner_rect {
                t.rect("chrome:dock", 13, inner);
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
}
