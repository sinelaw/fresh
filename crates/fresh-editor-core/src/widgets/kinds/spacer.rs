//! `Spacer` — fixed-width padding (flex sizing is a `Row` concern).

use fresh_core::api::WidgetSpec;

use super::WidgetImpl;

pub struct Spacer;

impl WidgetImpl for Spacer {
    fn box_meta(&self, _spec: &WidgetSpec) -> super::BoxMeta {
        super::BoxMeta::plain("spacer")
    }
}
