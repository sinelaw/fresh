//! `Divider` — a host-width horizontal rule.

use fresh_core::api::WidgetSpec;

use super::WidgetImpl;

pub struct Divider;

impl WidgetImpl for Divider {
    fn box_meta(&self, _spec: &WidgetSpec) -> super::BoxMeta {
        super::BoxMeta::plain("divider")
    }
}
