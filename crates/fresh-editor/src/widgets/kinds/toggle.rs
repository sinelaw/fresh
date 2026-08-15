//! `Toggle` — checkbox chip, chip-first (`[v] label`) or form layout
//! (`label: [v]` via `label_first`).

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;
use serde_json::json;

use super::WidgetImpl;
use crate::widgets::registry::{HitArea, WidgetInstanceState};
use crate::widgets::render::{
    apply_hover_band, ensure_trailing_newline, render_toggle, render_toggle_form, CollectedOutput,
    RenderContext,
};

pub(crate) struct Toggle;

impl WidgetImpl for Toggle {
    fn collect(
        &self,
        spec: &WidgetSpec,
        _prev: &HashMap<String, WidgetInstanceState>,
        _next_state: &mut HashMap<String, WidgetInstanceState>,
        ctx: RenderContext<'_>,
        panel_width: u32,
    ) -> CollectedOutput {
        let WidgetSpec::Toggle {
            checked,
            label,
            focused,
            indeterminate,
            label_first,
            label_width,
            key,
        } = spec
        else {
            return CollectedOutput::default();
        };
        let key = key.as_deref();
        let mut out = CollectedOutput::default();
        // Host-managed focus overrides the spec's `focused`
        // when this widget has a key and is the panel's focused
        // widget. Plugin-passed `focused` is ignored when the
        // host owns focus (i.e. the panel has any tabbable
        // widgets); without it, the renderer falls back to the
        // spec value (legacy path).
        // A keyed widget takes focus from the host's resolved focus key; an
        // unkeyed one falls back to the spec's initial-only `focused` hint.
        let is_focused = if key.is_some_and(|k| !k.is_empty()) {
            ctx.is_focused(key)
        } else {
            *focused
        };
        // Form layout (`label: [v]`) restricts the hit to the chip so a
        // click on the label doesn't flip the value (the settings dialog's
        // long-standing contract); the default chip-first layout keeps the
        // whole row clickable, which is what plugin panels expect.
        let (mut entry, chip_range) = if *label_first {
            render_toggle_form(
                *checked,
                *indeterminate,
                label,
                is_focused,
                *label_width,
                panel_width,
                ctx.marker_gutter,
            )
        } else {
            let entry = render_toggle(*checked, label, is_focused, ctx.marker_gutter);
            let end = entry.text.len();
            (entry, (0, end))
        };
        // The pointer lights the whole chip+label the same way it lights a
        // button. Focus already paints its own band, so hover only shows on
        // the controls focus isn't on.
        if ctx.is_hovered(key) && !is_focused {
            apply_hover_band(&mut entry);
        }
        out.hits.push(HitArea {
            overlay: false,
            widget_key: key.unwrap_or("").to_string(),
            widget_kind: "toggle",
            buffer_row: 0,
            byte_start: chip_range.0,
            byte_end: chip_range.1,
            payload: json!({ "checked": !checked }),
            event_type: "toggle",
        });
        ensure_trailing_newline(&mut entry);
        out.entries.push(entry);
        out
    }
}
