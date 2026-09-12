//! `WindowEmbed` — reserves a rectangle for a native window render.

use fresh_core::api::WidgetSpec;

use super::WidgetImpl;

pub struct WindowEmbed;

impl WidgetImpl for WindowEmbed {
    fn box_meta(&self, _spec: &WidgetSpec) -> super::BoxMeta {
        super::BoxMeta::plain("window_embed")
    }
}
