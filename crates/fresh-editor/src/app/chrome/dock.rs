//! The left dock column (orchestrator sessions panel).

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
}
