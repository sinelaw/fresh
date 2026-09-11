//! `Component` — a transparent focus/event scope around a subtree.
//!
//! Renders its child unchanged; `focus_trap` on its `box_meta` is what
//! scopes Tab cycling (the tree's ring, read by `Ui::next_in`, and the layout
//! box that carries the same flag) to the subtree. See
//! `docs/internal/retained-mode-ui.md` "Where each surface lives".

use fresh_core::api::WidgetSpec;

use super::WidgetImpl;

pub struct Component;

impl WidgetImpl for Component {
    fn box_meta(&self, spec: &WidgetSpec) -> super::BoxMeta {
        let mut m = super::BoxMeta::plain("component");
        if let WidgetSpec::Component { key: Some(k), .. } = spec {
            if !k.is_empty() {
                m.key = Some(k.clone());
            }
        }
        // The point of the kind: Tab cycles inside this subtree.
        m.focus_trap = true;
        m
    }
}
