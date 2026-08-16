//! `Popup` — a popup layer as a first-class tree node.
//!
//! Replaces the `overlays`/`popup` side channels as the plugin-facing
//! vocabulary (widget-framework-v2-review.md phase 7). Two modes:
//! panel-clipped (`screen_space: false`) renders through the same
//! promoted-overlay path `Overlay` uses; `screen_space: true`
//! projects the child's fully-rendered rows through the generalized
//! [`PanelPopup`] channel — the same channel the Dropdown pop-over
//! rides — so the box escapes the panel/modal border and is painted
//! (bordered, anchored, flipped and clamped on screen) by the host,
//! which knows nothing about the content. The remaining internal
//! producers (dropdown pop-over, completion popup) migrate onto this
//! node in later slices, which is what lets the side channels close.

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;

use super::WidgetImpl;
use crate::widgets::registry::WidgetInstanceState;
use crate::widgets::render::{render_collected, CollectedOutput, PanelPopup, RenderContext};

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
        let WidgetSpec::Popup {
            child,
            key,
            anchor,
            screen_space,
        } = spec
        else {
            return CollectedOutput::default();
        };
        if *screen_space {
            // Render the child subtree exactly like any other node,
            // then project its rows through the pop-over channel
            // instead of the panel flow: the popup contributes no
            // inline rows, and its content escapes panel clipping.
            // `row_indices` stays empty — a generic popup's rows are
            // not option cells, so the host records no select hits
            // (clicks inside the box are absorbed by its rect).
            let child_out = render_collected(child, prev, next_state, ctx, panel_width);
            let mut out = CollectedOutput::default();
            let (anchor_row, anchor_col, anchor_absolute) = match anchor {
                // Explicit anchor: absolute panel-inner coordinates —
                // the container merges must not shift it with the flow.
                Some([row, col]) => (*row, *col, true),
                // No anchor: drop from the popup's own position in the
                // tree (parents shift anchor_row as they merge).
                None => (0, 0, false),
            };
            out.popups.push(PanelPopup {
                widget_key: key.clone().unwrap_or_default(),
                anchor_row,
                anchor_col,
                anchor_absolute,
                entries: child_out.entries,
                row_indices: Vec::new(),
            });
            // The pop-over as a box, mirroring the dropdown's: its
            // final rectangle is resolved at paint (screen-space), two
            // stacking levels up; panel-space hit-testing skips it.
            out.boxes.push({
                let mut b = crate::widgets::LayoutBox::plain("panel_popup", 0, 0, 0, 0);
                b.screen_space = true;
                b.z = 2;
                b
            });
            return out;
        }
        // Panel-clipped: same promoted-overlay collection as `Overlay`.
        super::containers::collect_overlay(child, prev, next_state, ctx, panel_width)
    }
}
