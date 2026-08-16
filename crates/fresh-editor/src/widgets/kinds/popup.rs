//! `Popup` — a popup layer as a first-class tree node.
//!
//! Slice (a) of replacing the `overlays`/`dropdown_popup` side
//! channels (widget-framework-v2-review.md phase 7): the variant
//! exists, renders through the same promoted-overlay path `Overlay`
//! uses, and its box is pointer-opaque. Later slices grow
//! screen-space anchoring (escaping panel clipping) and migrate the
//! internal producers (dropdown pop-over, completion popup) onto it,
//! which is what lets the side channels be deleted.

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;

use super::WidgetImpl;
use crate::widgets::registry::WidgetInstanceState;
use crate::widgets::render::{CollectedOutput, RenderContext};

pub(crate) struct Popup;

impl WidgetImpl for Popup {
    fn box_meta(&self, spec: &WidgetSpec) -> super::BoxMeta {
        let mut m = super::BoxMeta::plain("popup");
        // A popup is an opaque surface: a click inside it that nothing
        // consumes must not fall through to the rows beneath.
        m.pointer_opaque = true;
        if let WidgetSpec::Popup { key: Some(k), .. } = spec {
            if !k.is_empty() {
                m.key = Some(k.clone());
            }
        }
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
        let WidgetSpec::Popup { child, .. } = spec else {
            return CollectedOutput::default();
        };
        // Same promoted-overlay collection as `Overlay` for now; the
        // screen-space escape lands with the anchoring slice.
        super::containers::collect_overlay(child, prev, next_state, ctx, panel_width)
    }
}
