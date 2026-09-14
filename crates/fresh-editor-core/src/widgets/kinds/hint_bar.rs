//! `HintBar` — the keyboard-hint footer row.

use fresh_core::api::WidgetSpec;

use super::WidgetImpl;

pub struct HintBar;

impl WidgetImpl for HintBar {
    fn box_meta(&self, _spec: &WidgetSpec) -> super::BoxMeta {
        super::BoxMeta::plain("hint_bar")
    }
}
