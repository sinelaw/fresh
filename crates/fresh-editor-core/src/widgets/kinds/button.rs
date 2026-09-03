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

pub struct Button;

impl WidgetImpl for Button {
    fn on_key(
        &self,
        spec: &WidgetSpec,
        _widget_key: &str,
        _panel: &mut crate::widgets::WidgetPanelState,
        _viewport: super::Viewport,
        key: &str,
        fx: &mut super::KeyFx,
    ) -> super::KeyDisposition {
        if !matches!(key, "Enter" | "Space") {
            return super::KeyDisposition::Pass;
        }
        // Disabled buttons don't fire activate. The renderer already
        // excludes them from the tab cycle; a focus key still pointing
        // at one is a stale-focus race — drop the key.
        if let WidgetSpec::Button { disabled, .. } = spec {
            if !disabled {
                fx.events.push(("activate".into(), serde_json::json!({})));
            }
        }
        super::KeyDisposition::Consumed
    }

    /// `WidgetAction::Activate` (a plugin mode binding's Enter) on a
    /// focused Button fires `activate` — unless disabled: the
    /// renderer excludes disabled buttons from the tab cycle, so a
    /// focus key still pointing at one is a stale-focus race and the
    /// event is dropped.
    fn activate_event(&self, spec: &WidgetSpec) -> Option<(&'static str, serde_json::Value)> {
        match spec {
            WidgetSpec::Button { disabled: true, .. } => None,
            WidgetSpec::Button { .. } => Some(("activate", serde_json::json!({}))),
            _ => None,
        }
    }

    fn arrows_advance_focus(&self) -> bool {
        // No vertical axis of its own: panel arrows walk the controls
        // like Tab (button-only popups such as the dock context menu).
        true
    }

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
            focusable,
            intent,
            key,
            disabled,
            bare,
            full_width,
            hover_style,
            style,
            ..
        } = spec
        else {
            return CollectedOutput::default();
        };
        let key = key.as_deref();
        let mut out = CollectedOutput::default();
        // `focusable: false` drops the button from the Tab cycle, so it
        // can never be what focus is on — and must not render as though
        // it were. This matters when several buttons share one key to
        // act as a single control: the focus clamp lands on the one
        // tabbable member, and without this gate every other member
        // painted itself focused too, putting the focus band across the
        // whole group at rest.
        let is_focused = !disabled
            && *focusable
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
            render_bare_button(
                label,
                is_focused,
                *intent,
                *disabled,
                hover,
                hovered,
                style.as_ref(),
            )
        } else {
            render_button(
                label,
                is_focused,
                *intent,
                *disabled,
                ctx.marker_gutter,
                hover,
                hovered,
                style.as_ref(),
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
                buffer_row: 0,
                byte_start: 0,
                byte_end,
                event: crate::widgets::WidgetEvent {
                    row_target: false,
                    context_click: false,
                    widget_key: key.unwrap_or("").to_string(),
                    widget_kind: "button",
                    payload: json!({}),
                    event_type: "activate",
                    owner_key: None,
                },
            });
        }
        ensure_trailing_newline(&mut entry);
        out.entries.push(entry);
        out
    }
}
