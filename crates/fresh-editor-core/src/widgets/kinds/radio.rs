//! `Radio` — an inline single-select option group, rendered as
//! `label: (•) A   ( ) B   ( ) C`.
//!
//! The choice a `Dropdown` hides behind a pop-over, a `Radio` lays out
//! in the row: every option is visible and one press away, which is the
//! right shape for a short, fixed set (a backend, a mode, a scope) that
//! a form asks about first. It also settles what a *selected* option
//! looks like — the filled `(•)` — so a selected choice no longer has to
//! borrow a primary button's ink to be told apart from its siblings.
//!
//! Like `Dropdown`, the selected index is host-owned instance state
//! ([`WidgetInstanceState::Radio`]) after first render; the spec's
//! `selected_index` is a seed. Left/Right cycle the selection, Home/End
//! jump it, and a click on an option selects it — every move fires
//! `change { index, value }`. Up/Down are not this kind's: they walk the
//! form like Tab ([`WidgetImpl::arrows_advance_focus`]).

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;
use serde_json::json;

use super::WidgetImpl;
use crate::widgets::registry::{HitArea, WidgetInstanceState};
use crate::widgets::render::{
    apply_hover_band, ensure_trailing_newline, render_radio, CollectedOutput, RenderContext,
    RenderedRadio,
};

pub struct Radio;

impl WidgetImpl for Radio {
    fn on_key(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        _viewport: super::Viewport,
        key: &str,
        fx: &mut super::KeyFx,
    ) -> super::KeyDisposition {
        use super::KeyDisposition::{Consumed, Pass};
        match key {
            "Left" => {
                cycle_selection(spec, widget_key, panel, -1, fx);
                Consumed
            }
            "Right" => {
                cycle_selection(spec, widget_key, panel, 1, fx);
                Consumed
            }
            "Home" => {
                set_selection(spec, widget_key, panel, 0, fx);
                Consumed
            }
            "End" => {
                set_selection(spec, widget_key, panel, i32::MAX, fx);
                Consumed
            }
            _ => Pass,
        }
    }

    /// A click on an option row-piece selects that option. The host
    /// owns the index, so the hit is fully handled here — the plugin
    /// sees the `change` the move fires, never the raw click.
    fn on_pointer(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        event_type: &str,
        payload: &serde_json::Value,
        fx: &mut super::PointerFx,
    ) -> super::PointerDisposition {
        if event_type != "radio_select" {
            return super::PointerDisposition::Default;
        }
        if let Some(idx) = payload.get("index").and_then(|v| v.as_i64()) {
            set_selection(spec, widget_key, panel, idx as i32, &mut fx.key);
        }
        super::PointerDisposition::Consumed
    }

    fn arrows_advance_focus(&self) -> bool {
        // The options run left-to-right; the vertical axis belongs to
        // the form around the control.
        true
    }

    fn box_meta(&self, spec: &WidgetSpec) -> super::BoxMeta {
        let mut m = super::BoxMeta::plain("radio");
        if let WidgetSpec::Radio { key: Some(k), .. } = spec {
            if !k.is_empty() {
                m.key = Some(k.clone());
                m.focusable = true;
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
        _panel_width: u32,
    ) -> CollectedOutput {
        let WidgetSpec::Radio {
            options,
            selected_index,
            label,
            focused,
            label_width,
            key,
        } = spec
        else {
            return CollectedOutput::default();
        };
        let key = key.as_deref();
        let mut out = CollectedOutput::default();
        // A keyed widget takes focus from the host's resolved focus key; an
        // unkeyed one falls back to the spec's initial-only `focused` hint.
        let is_focused = if key.is_some_and(|k| !k.is_empty()) {
            ctx.is_focused(key)
        } else {
            *focused
        };
        let selected = resolve(options, *selected_index, key, prev);
        // The walk carries this widget's state; it does not decide it (see
        // `Dropdown`'s collector for why). An absent entry stays absent: the
        // spec is the seed until a handler makes a decision.
        if let Some(k) = key.filter(|k| !k.is_empty()) {
            if let Some(stored) = prev.get(k) {
                next_state.insert(k.to_string(), stored.clone());
            }
        }
        let RenderedRadio {
            mut entry,
            option_ranges,
        } = render_radio(
            options,
            selected,
            label,
            is_focused,
            *label_width,
            ctx.label_align,
            ctx.marker_gutter,
        );
        // Focus paints its own band, so hover only shows where focus is not.
        if ctx.is_hovered(key) && !is_focused {
            apply_hover_band(&mut entry);
        }
        let widget_key = key.unwrap_or("").to_string();
        for (index, (start, end)) in option_ranges.into_iter().enumerate() {
            out.hits.push(HitArea {
                overlay: false,
                buffer_row: 0,
                byte_start: start,
                byte_end: end,
                event: crate::widgets::WidgetEvent {
                    row_target: false,
                    context_click: false,
                    widget_key: widget_key.clone(),
                    widget_kind: "radio",
                    payload: json!({ "index": index }),
                    event_type: "radio_select",
                    owner_key: None,
                },
            });
        }
        ensure_trailing_newline(&mut entry);
        out.entries.push(entry);
        out
    }
}

/// **Where a `Radio`'s selection actually comes from.** Instance state
/// is authoritative after first render; the spec's `selected_index` is a
/// seed. Clamped into the option set either way. Pure, so the shell's
/// description and the collector get one answer.
pub fn resolve(
    options: &[String],
    spec_selected: i32,
    key: Option<&str>,
    prev: &HashMap<String, WidgetInstanceState>,
) -> i32 {
    let cur = match key {
        Some(k) if !k.is_empty() => match prev.get(k) {
            Some(WidgetInstanceState::Radio { selected_index }) => *selected_index,
            _ => spec_selected,
        },
        _ => spec_selected,
    };
    clamp(cur, options.len())
}

fn clamp(index: i32, len: usize) -> i32 {
    if len == 0 {
        0
    } else {
        index.clamp(0, len as i32 - 1)
    }
}

fn current(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &crate::widgets::WidgetPanelState,
) -> Option<(Vec<String>, i32)> {
    let WidgetSpec::Radio {
        options,
        selected_index,
        ..
    } = spec
    else {
        return None;
    };
    if options.is_empty() {
        return None;
    }
    let cur = resolve(
        options,
        *selected_index,
        Some(widget_key),
        &panel.instance_states,
    );
    Some((options.clone(), cur))
}

fn commit(
    options: &[String],
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    from: i32,
    to: i32,
    fx: &mut super::KeyFx,
) {
    panel.instance_states.insert(
        widget_key.to_string(),
        WidgetInstanceState::Radio { selected_index: to },
    );
    if to != from {
        let value = options.get(to as usize).cloned().unwrap_or_default();
        fx.events
            .push(("change".into(), json!({ "index": to, "value": value })));
    }
}

/// Step the selection by `delta` with wraparound; queues `change` when
/// it moved.
pub fn cycle_selection(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    delta: i32,
    fx: &mut super::KeyFx,
) {
    let Some((options, cur)) = current(spec, widget_key, panel) else {
        return;
    };
    let to = crate::widgets::wrap_index(cur, delta, options.len());
    commit(&options, widget_key, panel, cur, to, fx);
}

/// Set the selection to an absolute index (a click on an option),
/// clamped into the option set; queues `change` when it moved.
pub fn set_selection(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    index: i32,
    fx: &mut super::KeyFx,
) {
    let Some((options, cur)) = current(spec, widget_key, panel) else {
        return;
    };
    let to = clamp(index, options.len());
    commit(&options, widget_key, panel, cur, to, fx);
}

/// Kind policy for the plugin `SetRadio` mutation: clamp the wire index
/// into THIS spec's option set. The mutation arm in `plugin_dispatch` is
/// a pure delegation.
pub fn set_index_state(spec: &WidgetSpec, index: i32) -> WidgetInstanceState {
    let len = match spec {
        WidgetSpec::Radio { options, .. } => options.len(),
        _ => 0,
    };
    WidgetInstanceState::Radio {
        selected_index: clamp(index, len),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::widgets::render::render_spec;

    fn radio(selected: i32, key: Option<&str>) -> WidgetSpec {
        WidgetSpec::Radio {
            options: vec!["Local".into(), "SSH".into(), "Kubernetes".into()],
            selected_index: selected,
            label: "Run in".into(),
            focused: false,
            label_width: 0,
            key: key.map(|k| k.to_string()),
        }
    }

    fn panel_of(spec: &WidgetSpec) -> crate::widgets::WidgetPanelState {
        let out = render_spec(spec, &HashMap::new(), "", 60);
        crate::widgets::WidgetPanelState {
            buffer_id: Some(crate::model::event::BufferId(1)),
            spec: spec.clone(),
            instance_states: out.instance_states,
            focus_key: out.focus_key,
            painted: out.painted,
            boxes: out.boxes,
            auto_focus_first: true,
            page: false,
            focus_follows_cursor: false,
            hovered_widget_key: String::new(),
            hovered_item_key: String::new(),
            h_pan: Default::default(),
        }
    }

    #[test]
    fn renders_every_option_with_the_selected_one_filled() {
        let out = render_spec(&radio(1, Some("r")), &HashMap::new(), "", 60);
        let text = out.entries[0].text.trim_end();
        assert_eq!(text, "Run in: ( ) Local   (•) SSH   ( ) Kubernetes");
        // One hit per option, each over its own glyph + name.
        assert_eq!(out.hits.len(), 3);
        let piece = |i: usize| &text[out.hits[i].byte_start..out.hits[i].byte_end];
        assert_eq!(piece(0), "( ) Local");
        assert_eq!(piece(1), "(•) SSH");
        assert_eq!(piece(2), "( ) Kubernetes");
        assert_eq!(out.hits[2].event.event_type, "radio_select");
        assert_eq!(out.hits[2].event.payload["index"], 2);
    }

    #[test]
    fn left_right_cycle_and_fire_change_with_the_value() {
        let spec = radio(0, Some("r"));
        let mut panel = panel_of(&spec);
        let mut fx = super::super::KeyFx::default();
        let vp = super::super::Viewport::default();
        Radio.on_key(&spec, "r", &mut panel, vp, "Right", &mut fx);
        assert_eq!(fx.events.len(), 1);
        assert_eq!(fx.events[0].0, "change");
        assert_eq!(fx.events[0].1["index"], 1);
        assert_eq!(fx.events[0].1["value"], "SSH");
        // Wraps at the ends.
        Radio.on_key(&spec, "r", &mut panel, vp, "Left", &mut fx);
        Radio.on_key(&spec, "r", &mut panel, vp, "Left", &mut fx);
        assert_eq!(fx.events.last().unwrap().1["index"], 2);
        assert_eq!(
            resolve(
                &["a".into(), "b".into(), "c".into()],
                0,
                Some("r"),
                &panel.instance_states
            ),
            2
        );
    }

    #[test]
    fn a_click_selects_that_option_and_reselecting_is_silent() {
        let spec = radio(0, Some("r"));
        let mut panel = panel_of(&spec);
        let mut fx = super::super::PointerFx::default();
        Radio.on_pointer(
            &spec,
            "r",
            &mut panel,
            "radio_select",
            &json!({ "index": 2 }),
            &mut fx,
        );
        assert_eq!(fx.key.events.len(), 1);
        assert_eq!(fx.key.events[0].1["value"], "Kubernetes");
        Radio.on_pointer(
            &spec,
            "r",
            &mut panel,
            "radio_select",
            &json!({ "index": 2 }),
            &mut fx,
        );
        assert_eq!(
            fx.key.events.len(),
            1,
            "no change event for the same option"
        );
    }

    #[test]
    fn instance_state_outranks_the_spec_seed_after_first_render() {
        let spec = radio(0, Some("r"));
        let mut states = HashMap::new();
        states.insert("r".to_string(), set_index_state(&spec, 99));
        let out = render_spec(&spec, &states, "", 60);
        assert!(
            out.entries[0].text.contains("(•) Kubernetes"),
            "clamped to the last option"
        );
    }
}
