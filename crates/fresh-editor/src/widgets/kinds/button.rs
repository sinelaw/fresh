//! `Button` — framed or bare action button with intent styling.

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;
use serde_json::json;

use super::WidgetImpl;
use crate::widgets::registry::{HitArea, WidgetInstanceState};
use crate::widgets::render::{
    ensure_trailing_newline, fill_button_label, render_bare_button, render_button, CollectedOutput,
    RenderContext,
};

pub(crate) struct Button;

impl WidgetImpl for Button {
    fn box_meta(&self, spec: &WidgetSpec) -> super::BoxMeta {
        let mut m = super::BoxMeta::plain("button");
        if let WidgetSpec::Button {
            key: Some(k),
            disabled,
            focusable,
            ..
        } = spec
        {
            if !k.is_empty() {
                m.key = Some(k.clone());
                m.focusable = !*disabled && *focusable;
            }
        }
        m
    }
    fn collect(
        &self,
        spec: &WidgetSpec,
        _prev: &HashMap<String, WidgetInstanceState>,
        _next_state: &mut HashMap<String, WidgetInstanceState>,
        ctx: RenderContext<'_>,
        panel_width: u32,
    ) -> CollectedOutput {
        let WidgetSpec::Button {
            label,
            focused,
            intent,
            key,
            disabled,
            bare,
            full_width,
            hover_style,
            ..
        } = spec
        else {
            return CollectedOutput::default();
        };
        let key = key.as_deref();
        let mut out = CollectedOutput::default();
        let is_focused = !disabled
            && if key.is_some_and(|k| !k.is_empty()) {
                ctx.is_focused(key)
            } else {
                *focused
            };
        // A `hover_style` applies only while the pointer is actually on this
        // widget, and never to a disabled one — an inert control advertising
        // itself as live would lie.
        let hovered = !disabled && ctx.is_hovered(key);
        let hover = hover_style.as_ref().filter(|_| hovered);
        // A `full_width` button is stretched by padding its *label*, before
        // the chrome goes on, so the finished control (frame and all) is
        // exactly `panel_width` columns — the focus / hover band is painted
        // over the button's own cells, so filling the label is what makes
        // the band span the row rather than hugging the word.
        let filled =
            full_width.then(|| fill_button_label(label, *bare, ctx.marker_gutter, panel_width));
        let label = filled.as_deref().unwrap_or(label);
        let mut entry = if *bare {
            render_bare_button(label, is_focused, *intent, *disabled, hover, hovered)
        } else {
            render_button(
                label,
                is_focused,
                *intent,
                *disabled,
                ctx.marker_gutter,
                hover,
                hovered,
            )
        };
        // Disabled buttons skip the hit area entirely — clicks on
        // them are no-ops, matching the non-tabbable behavior in
        // `collect_tabbable`. Without this, a stray click would
        // still focus + activate a button whose handler is
        // already gated by the same disabled condition the
        // plugin computed.
        if !disabled {
            let byte_end = entry.text.len();
            out.hits.push(HitArea {
                overlay: false,
                widget_key: key.unwrap_or("").to_string(),
                widget_kind: "button",
                buffer_row: 0,
                byte_start: 0,
                byte_end,
                payload: json!({}),
                event_type: "activate",
            });
        }
        ensure_trailing_newline(&mut entry);
        out.entries.push(entry);
        out
    }
}
