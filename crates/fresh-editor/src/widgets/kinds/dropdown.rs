//! `Dropdown` — `label: [value ▼]` trigger with a floating option pop-over.

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;
use serde_json::json;

use super::WidgetImpl;
use crate::widgets::registry::{HitArea, WidgetInstanceState};
use crate::widgets::render::{
    ensure_trailing_newline, render_dropdown, CollectedOutput, DropdownPopup, RenderContext,
    RenderedDropdown,
};

pub(crate) struct Dropdown;

impl WidgetImpl for Dropdown {
    fn collect(
        &self,
        spec: &WidgetSpec,
        prev: &HashMap<String, WidgetInstanceState>,
        next_state: &mut HashMap<String, WidgetInstanceState>,
        ctx: RenderContext<'_>,
        _panel_width: u32,
    ) -> CollectedOutput {
        let WidgetSpec::Dropdown {
            options,
            selected_index,
            label,
            focused,
            label_width,
            open,
            scroll_offset,
            key,
        } = spec
        else {
            return CollectedOutput::default();
        };
        collect_dropdown(
            options,
            *selected_index,
            label,
            *focused,
            *label_width,
            *open,
            *scroll_offset,
            key.as_deref(),
            prev,
            next_state,
            ctx,
        )
    }
}

#[allow(clippy::too_many_arguments)]
fn collect_dropdown(
    options: &[String],
    spec_selected: i32,
    label: &str,
    focused: bool,
    label_width: u32,
    spec_open: bool,
    spec_scroll: u32,
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
    // Instance state is authoritative after first render; clamp the
    // selected index into the current option set and persist. A panel
    // that renders statelessly (no prior instance state — e.g. the
    // Settings dialog re-emitting its model each frame) falls back to
    // the spec's `open`/`scroll_offset`: the host model drives the
    // expansion directly, so the spec's `open` is honored as-is (no
    // focus gate — the surface's own focus model already decided).
    let (cur, state_open) = match key {
        Some(k) if !k.is_empty() => match prev.get(k) {
            Some(WidgetInstanceState::Dropdown {
                selected_index,
                open,
            }) => (*selected_index, Some(*open)),
            _ => (spec_selected, None),
        },
        _ => (spec_selected, None),
    };
    let cur = if options.is_empty() {
        0
    } else {
        cur.clamp(0, options.len() as i32 - 1)
    };
    // Instance-state open only persists while the widget is focused —
    // a blur (Tab away, click elsewhere) closes it.
    let open = match state_open {
        Some(o) => o && is_focused,
        None => spec_open,
    } && !options.is_empty();
    if let Some(k) = key {
        if !k.is_empty() {
            next_state.insert(
                k.to_string(),
                WidgetInstanceState::Dropdown {
                    selected_index: cur,
                    open,
                },
            );
        }
    }

    let RenderedDropdown {
        mut entry,
        button_range,
        option_rows,
        scroll_offset,
    } = render_dropdown(
        options,
        cur,
        label,
        is_focused,
        label_width,
        open,
        spec_scroll,
        ctx.marker_gutter,
    );
    // The open list now floats as a screen-level pop-over
    // (`out.dropdown_popups`) instead of growing inline, so the panel
    // keeps only the compact `[value ▲]` trigger row and never
    // grows/clips inside the frame. `render_dropdown`'s inline
    // `option_rows` are discarded here (the Settings dialog, which calls
    // `render_dropdown` directly, still uses them for its inline list).
    let _ = option_rows;
    let widget_key = key.unwrap_or("").to_string();
    // A click on the `[value ▼]` button toggles the option list open
    // (see `deliver_widget_hit`'s `dropdown_toggle` special case).
    out.hits.push(HitArea {
        overlay: false,
        widget_key: widget_key.clone(),
        widget_kind: "dropdown",
        buffer_row: 0,
        byte_start: button_range.0,
        byte_end: button_range.1,
        payload: json!({}),
        event_type: "dropdown_toggle",
    });
    // Open: surface the option list as a floating pop-over anchored to
    // the trigger's row (row 0 within this sub-render; Col/Row/Section
    // collapse shifts `anchor_row` up to the panel-inner row). The host
    // draws + hit-tests it at screen coordinates, so it extends past the
    // panel/modal border instead of reflowing the panel. Option hit
    // areas are registered by the host draw pass, not here (they live
    // outside the panel's buffer rows).
    if open {
        // Anchor column = the display width of the row text before the
        // button's `[`, so the pop-over drops directly under the value cell
        // rather than at the panel's left content edge. `button_range.0` is a
        // byte offset into `entry.text`; measure its display width (the focus
        // marker `▸ ` is 4 bytes but 2 columns, so byte length would misalign).
        use crate::primitives::display_width::str_width;
        let anchor_col = entry
            .text
            .get(..button_range.0)
            .map(|prefix| str_width(prefix) as u32)
            .unwrap_or(0);
        out.dropdown_popups.push(DropdownPopup {
            widget_key,
            anchor_row: 0,
            anchor_col,
            options: options.to_vec(),
            selected: cur as usize,
            scroll: scroll_offset,
        });
    }
    ensure_trailing_newline(&mut entry);
    out.entries.insert(0, entry);
    out
}
