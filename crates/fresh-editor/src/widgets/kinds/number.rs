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

pub(crate) struct Number;

impl WidgetImpl for Number {
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
    // Instance state is authoritative once the widget has rendered;
    // the spec's `value` is a seed only. Read prior value by key,
    // clamp, and persist for the next render.
    let cur = match key {
        Some(k) if !k.is_empty() => match prev.get(k) {
            Some(WidgetInstanceState::Number { value }) => *value,
            _ => spec_value,
        },
        _ => spec_value,
    };
    let cur = clamp_number(cur, min, max);
    if let Some(k) = key {
        if !k.is_empty() {
            next_state.insert(k.to_string(), WidgetInstanceState::Number { value: cur });
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
        widget_key: key.unwrap_or("").to_string(),
        widget_kind: "number",
        buffer_row: 0,
        byte_start: value_range.0,
        byte_end: value_range.1,
        payload: json!({}),
        event_type: "number_value",
    });
    ensure_trailing_newline(&mut entry);
    out.entries.push(entry);
    out
}
