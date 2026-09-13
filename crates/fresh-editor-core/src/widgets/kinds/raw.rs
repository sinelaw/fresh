//! `Raw` — the pre-rendered text-property escape hatch.

use fresh_core::api::WidgetSpec;

use super::WidgetImpl;

pub struct Raw;

impl WidgetImpl for Raw {
    fn box_meta(&self, _spec: &WidgetSpec) -> super::BoxMeta {
        super::BoxMeta::plain("raw")
    }
}
