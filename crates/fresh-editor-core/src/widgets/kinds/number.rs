//! `Number` — form value cell with in-place edit mode.

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;
use serde_json::json;

use super::WidgetImpl;
use crate::widgets::registry::{HitArea, WidgetInstanceState};
use crate::widgets::render::{
    clamp_number, ensure_trailing_newline, render_number, CollectedOutput, NumberEdit,
    RenderContext, RenderedNumber,
};

pub struct Number;

impl WidgetImpl for Number {
    fn on_key(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        _viewport: super::Viewport,
        key: &str,
        fx: &mut super::KeyFx,
    ) -> super::KeyDisposition {
        // Up/Right increment, Down/Left decrement — matching the
        // ◂/▸ glyphs (the reverse of a list's Up = select-previous).
        let steps = match key {
            "Up" | "Right" => 1,
            "Down" | "Left" => -1,
            _ => return super::KeyDisposition::Pass,
        };
        adjust(spec, widget_key, panel, steps, fx);
        super::KeyDisposition::Consumed
    }

    /// Pointer model: a value-cell click is only a focus move (which
    /// already happened before this runs) — the value changes by
    /// typing or arrow keys on the focused widget — so the recorded
    /// `number_value` hit event is swallowed rather than surfaced to
    /// the plugin.
    fn on_pointer(
        &self,
        _spec: &WidgetSpec,
        _widget_key: &str,
        _panel: &mut crate::widgets::WidgetPanelState,
        event_type: &str,
        _payload: &serde_json::Value,
        _fx: &mut super::PointerFx,
    ) -> super::PointerDisposition {
        if event_type == "number_value" {
            super::PointerDisposition::Consumed
        } else {
            super::PointerDisposition::Default
        }
    }

    fn box_meta(&self, spec: &WidgetSpec) -> super::BoxMeta {
        let mut m = super::BoxMeta::plain("number");
        if let WidgetSpec::Number { key: Some(k), .. } = spec {
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
        let WidgetSpec::Number {
            value,
            min,
            max,
            integer,
            percent,
            label,
            focused,
            label_width,
            edit_text,
            edit_cursor,
            edit_sel_start,
            edit_sel_end,
            key,
            ..
        } = spec
        else {
            return CollectedOutput::default();
        };
        collect_number(
            *value,
            *min,
            *max,
            *integer,
            *percent,
            label,
            *focused,
            *label_width,
            edit_text.as_deref().map(|t| NumberEdit {
                text: t,
                cursor: *edit_cursor,
                sel_start: *edit_sel_start,
                sel_end: *edit_sel_end,
            }),
            key.as_deref(),
            prev,
            next_state,
            ctx,
        )
    }
}

#[allow(clippy::too_many_arguments)]
fn collect_number(
    spec_value: f64,
    min: Option<f64>,
    max: Option<f64>,
    integer: bool,
    percent: bool,
    label: &str,
    focused: bool,
    label_width: u32,
    edit: Option<NumberEdit<'_>>,
    key: Option<&str>,
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    ctx: RenderContext<'_>,
) -> CollectedOutput {
    let mut out = CollectedOutput::default();
    // A keyed widget takes focus from the host's resolved focus key; an
    // unkeyed one falls back to the spec's initial-only `focused` hint.
    let is_focused = if key.is_some_and(|k| !k.is_empty()) {
        ctx.is_focused(key)
    } else {
        focused
    };
    // Instance state is authoritative once a handler has written one; the
    // spec's `value` is the seed until then.
    let cur = match key {
        Some(k) if !k.is_empty() => match prev.get(k) {
            Some(WidgetInstanceState::Number { value }) => *value,
            _ => spec_value,
        },
        _ => spec_value,
    };
    let cur = clamp_number(cur, min, max);
    // **The walk carries this widget's state; it does not decide it.** The
    // clamp above is a derivation, applied on every read, so writing it back
    // stored nothing a reader could not work out — while making the render
    // walk a second writer of a field `on_key` and `on_pointer` also own. The
    // pass-through is what keeps the entry alive across the whole-map replace
    // in `update_side_effects`; an absent one stays absent, so a number nobody
    // has touched still reads its value from the spec. Same rule as
    // `kinds::dropdown`, and for the same reason.
    if let Some(k) = key.filter(|k| !k.is_empty()) {
        if let Some(stored) = prev.get(k) {
            next_state.insert(k.to_string(), stored.clone());
        }
    }

    let rendered = render_number(
        cur,
        integer,
        percent,
        label,
        is_focused,
        label_width,
        edit,
        ctx.marker_gutter,
    );
    let RenderedNumber {
        mut entry,
        value_range,
    } = rendered;
    // A click on the value cell begins in-place editing host-side
    // (see `deliver_widget_hit`'s `number_value` special case).
    out.hits.push(HitArea {
        overlay: false,
        buffer_row: 0,
        byte_start: value_range.0,
        byte_end: value_range.1,
        event: crate::widgets::WidgetEvent {
            row_target: false,
            context_click: false,
            widget_key: key.unwrap_or("").to_string(),
            widget_kind: "number",
            payload: json!({}),
            event_type: "number_value",
            owner_key: None,
        },
    });
    ensure_trailing_newline(&mut entry);
    out.entries.push(entry);
    out
}

/// Step the host-owned value by `steps * step`, clamped to
/// `[min, max]`; queues `change` when the value actually moved. Used
/// by `Number::on_key` and the click paths (`◂`/`▸` press) through
/// the Editor's shared mutation shell.
pub fn adjust(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    steps: i32,
    fx: &mut super::KeyFx,
) {
    let WidgetSpec::Number {
        value: spec_value,
        min,
        max,
        step,
        ..
    } = spec
    else {
        return;
    };
    let cur = match panel.instance_states.get(widget_key) {
        Some(WidgetInstanceState::Number { value }) => *value,
        _ => *spec_value,
    };
    let raw = cur + (steps as f64) * step;
    let clamped = crate::widgets::clamp_number(raw, *min, *max);
    let changed = clamped != cur;
    panel.instance_states.insert(
        widget_key.to_string(),
        WidgetInstanceState::Number { value: clamped },
    );
    if changed {
        fx.events
            .push(("change".into(), serde_json::json!({ "value": clamped })));
    }
}

/// Kind policy for the plugin `SetNumber` mutation: clamp the wire
/// value to THIS spec's bounds and produce the instance state. The
/// mutation arm in `plugin_dispatch` is a pure delegation — the
/// per-kind knowledge (where the bounds live) stays here.
pub fn set_value_state(spec: &WidgetSpec, value: f64) -> crate::widgets::WidgetInstanceState {
    let (min, max) = match spec {
        WidgetSpec::Number { min, max, .. } => (*min, *max),
        _ => (None, None),
    };
    crate::widgets::WidgetInstanceState::Number {
        value: crate::widgets::clamp_number(value, min, max),
    }
}
