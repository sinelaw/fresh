//! The centered floating widget panel (modal dialogs).

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct FloatingModal;

impl ChromeComponent for FloatingModal {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        if ed.floating_widget_panel.is_some() {
            // A centered modal consumes the wheel even on a miss.
            t.full("chrome:floating_panel", 13);
        }
    }
}
