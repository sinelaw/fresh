//! `Component` — a transparent focus/event scope around a subtree.
//!
//! Renders its child unchanged; `focus_trap` on its `box_meta` is what
//! scopes Tab cycling (`render::focus_ring_scoped_in_spec`, and the layout
//! box that carries the same flag) to the subtree. See
//! `docs/internal/retained-mode-ui.md` §3.5.

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;

use super::WidgetImpl;
use crate::widgets::registry::WidgetInstanceState;
use crate::widgets::render::{render_collected, CollectedOutput, RenderContext};

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
    fn collect(
        &self,
        spec: &WidgetSpec,
        prev: &HashMap<String, WidgetInstanceState>,
        next_state: &mut HashMap<String, WidgetInstanceState>,
        ctx: RenderContext<'_>,
        panel_width: u32,
    ) -> CollectedOutput {
        let WidgetSpec::Component { child, .. } = spec else {
            return CollectedOutput::default();
        };
        render_collected(child, prev, next_state, ctx, panel_width)
    }
}
