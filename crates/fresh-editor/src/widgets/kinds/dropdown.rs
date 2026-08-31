//! `Dropdown` — `label: [value ▼]` trigger with a floating option pop-over.

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;
use serde_json::json;

use super::WidgetImpl;
use crate::widgets::registry::{HitArea, WidgetInstanceState};
use crate::widgets::render::{
    ensure_trailing_newline, render_dropdown, CollectedOutput, PanelPopup, RenderContext,
    RenderedDropdown,
};

pub(crate) struct Dropdown;

impl WidgetImpl for Dropdown {
    fn on_key(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        key: &str,
        fx: &mut super::KeyFx,
    ) -> super::KeyDisposition {
        use super::KeyDisposition::{Consumed, Pass};
        if !is_open(widget_key, panel) {
            // Closed: arrows cycle the value in place (matching the
            // ◂/▸ glyphs), Enter/Space open the option popup —
            // everything else bubbles to the panel dispatch.
            return match key {
                "Up" | "Left" => {
                    cycle_selection(spec, widget_key, panel, -1, fx);
                    Consumed
                }
                "Down" | "Right" => {
                    cycle_selection(spec, widget_key, panel, 1, fx);
                    Consumed
                }
                "Enter" | "Space" => {
                    set_open(spec, widget_key, panel, true, fx);
                    Consumed
                }
                _ => Pass,
            };
        }
        // Open: Up/Down move the (live) selection, Enter/Space
        // commit-and-close, Esc closes.
        if !matches!(key, "Up" | "Down" | "Enter" | "Space" | "Escape") {
            return Pass;
        }
        match key {
            "Up" => {
                cycle_selection(spec, widget_key, panel, -1, fx);
                Consumed
            }
            "Down" => {
                cycle_selection(spec, widget_key, panel, 1, fx);
                Consumed
            }
            "Enter" | "Space" | "Escape" => {
                // The selection is already live (Up/Down fired
                // `change`); closing just dismisses the list.
                set_open(spec, widget_key, panel, false, fx);
                Consumed
            }
            _ => Pass,
        }
    }

    /// Pointer model: clicking the `[value ▼]` trigger toggles the
    /// option popup open/closed; clicking an option row commits that
    /// index (fires `change` through the same kind-owned mutation
    /// keyboard nav uses) and closes the popup. The host owns both
    /// the open flag and the index, so both hits are fully handled
    /// here — the recorded events never reach the plugin raw.
    fn on_pointer(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        event_type: &str,
        payload: &serde_json::Value,
        fx: &mut super::PointerFx,
    ) -> super::PointerDisposition {
        match event_type {
            "dropdown_toggle" => {
                let now_open = !is_open(widget_key, panel);
                set_open(spec, widget_key, panel, now_open, &mut fx.key);
                super::PointerDisposition::Consumed
            }
            "dropdown_select" => {
                if let Some(idx) = payload.get("index").and_then(|v| v.as_i64()) {
                    set_selection(spec, widget_key, panel, idx as i32, &mut fx.key);
                }
                set_open(spec, widget_key, panel, false, &mut fx.key);
                super::PointerDisposition::Consumed
            }
            _ => super::PointerDisposition::Default,
        }
    }

    fn box_meta(&self, spec: &WidgetSpec) -> super::BoxMeta {
        let mut m = super::BoxMeta::plain("dropdown");
        if let WidgetSpec::Dropdown { key: Some(k), .. } = spec {
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
    let Resolved {
        selected: cur,
        open,
    } = resolve(options, spec_selected, spec_open, key, prev, is_focused);
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
    // (`out.popups`) instead of growing inline, so the panel
    // keeps only the compact `[value ▲]` trigger row and never
    // grows/clips inside the frame. `render_dropdown`'s inline
    // `option_rows` are discarded here (the Settings dialog, which calls
    // `render_dropdown` directly, still uses them for its inline list).
    let _ = option_rows;
    let widget_key = key.unwrap_or("").to_string();
    // A click on the `[value ▼]` button toggles the option list open
    // (see `deliver_widget_hit`'s `dropdown_toggle` special case).
    out.hits.push(HitArea {
        row_target: false,
        context_click: false,
        overlay: false,
        widget_key: widget_key.clone(),
        widget_kind: "dropdown",
        buffer_row: 0,
        byte_start: button_range.0,
        byte_end: button_range.1,
        payload: json!({}),
        event_type: "dropdown_toggle",
        owner_key: None,
    });
    // Open: surface the option list as a floating pop-over anchored to
    // the trigger's row (row 0 within this sub-render; Col/Row/Section
    // collapse shifts `anchor_row` up to the panel-inner row). The host
    // draws + hit-tests it at screen coordinates, so it extends past the
    // panel/modal border instead of reflowing the panel. Option hit
    // areas are registered by the host draw pass, not here (they live
    // outside the panel's buffer rows).
    if open {
        out.popups.push(popup_of(
            options,
            cur,
            scroll_offset as u32,
            ctx.hover_popup_row,
            &widget_key,
            anchor_col(&entry.text, button_range.0),
        ));
        // The pop-over as a box: screen-space (its final rectangle is
        // resolved at paint, flipping above the anchor near the frame
        // edge), two stacking levels up. Panel-space hit-testing skips
        // screen-space boxes — the click path checks the paint-recorded
        // rect first, same ordering as before.
        out.boxes.push({
            let mut b = crate::widgets::LayoutBox::plain("dropdown_popup", 0, 0, 0, 0);
            b.screen_space = true;
            b.z = 2;
            b
        });
    }
    ensure_trailing_newline(&mut entry);
    out.entries.insert(0, entry);
    out
}

/// A `Dropdown`'s two pieces of state, once the spec and the instance map
/// have been reconciled.
pub(crate) struct Resolved {
    /// The option index the trigger shows, clamped into the current set.
    pub selected: i32,
    /// Whether the option pop-over is up.
    pub open: bool,
}

/// **Where a `Dropdown`'s selection and open flag actually come from.**
///
/// Instance state is authoritative after first render; the spec's
/// `selected_index` is a seed. A panel that renders statelessly — no prior
/// instance state, e.g. the Settings dialog re-emitting its model each frame —
/// falls back to the spec's `open`: the host model drives the expansion
/// directly, so it is honored as-is with no focus gate, because that surface's
/// own focus model already decided.
///
/// Pulled out of the collector because the *description* needs the same
/// answer, and a second copy of these rules is a second place for them to
/// drift. This is a pure function of what it is handed — no `next_state`
/// write — which is what lets a description call it. See
/// `view::shell::widgets`'s `Dropdown` arm.
pub(crate) fn resolve(
    options: &[String],
    spec_selected: i32,
    spec_open: bool,
    key: Option<&str>,
    prev: &HashMap<String, WidgetInstanceState>,
    is_focused: bool,
) -> Resolved {
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
    let selected = if options.is_empty() {
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
    Resolved { selected, open }
}

/// The column the pop-over drops under: the display width of the trigger row's
/// text before the button's `[`.
///
/// Measured in *display* width, never bytes — the focus marker `▸ ` is 4 bytes
/// but 2 columns, so byte length pushed the pop-over two cells right of the
/// value it belongs under.
pub(crate) fn anchor_col(row_text: &str, button_start: usize) -> u32 {
    use crate::primitives::display_width::str_width;
    row_text
        .get(..button_start)
        .map(|prefix| str_width(prefix) as u32)
        .unwrap_or(0)
}

/// The open option list: windowing, padding and row styling.
///
/// Windowing lives here with the rest of the render — clamp the scroll, slice
/// the visible rows — and each row is handed over as display text plus its
/// absolute index, so the consumer knows nothing about options or selection.
///
/// **Every row is the width of the widest option.** They were each their own
/// text's length, so the selected row's highlight was the width of its *word* —
/// "default" lit while "macos-gui" beside it set the box's width, leaving the
/// band two columns short of the edge on one row and flush on another. A
/// pop-over is a column of equal cells; padding them here also fixes the box,
/// which takes its width from the longest row it is given.
pub(crate) fn popup_of(
    options: &[String],
    selected_index: i32,
    scroll_offset: u32,
    hover_popup_row: &str,
    widget_key: &str,
    anchor_col: u32,
) -> PanelPopup {
    use crate::widgets::render::{KEY_COMPLETION_FG, KEY_COMPLETION_SEL_BG, KEY_COMPLETION_SEL_FG};
    use fresh_core::api::{OverlayColorSpec, OverlayOptions};
    use fresh_core::text_property::{InlineOverlay, OffsetUnit, TextPropertyEntry};

    let visible = options.len().min(crate::widgets::DROPDOWN_VISIBLE_OPTIONS);
    let max_scroll = options.len().saturating_sub(visible);
    let scroll = (scroll_offset as usize).min(max_scroll);
    let cell_cols = options
        .iter()
        .map(|o| crate::primitives::display_width::str_width(o))
        .max()
        .unwrap_or(0);
    let mut entries = Vec::new();
    let mut row_indices = Vec::new();
    for (idx, opt) in options.iter().enumerate().skip(scroll).take(visible) {
        let text = format!(" {} ", crate::widgets::render::cell(opt, cell_cols));
        let mut e = TextPropertyEntry::text(&text);
        let selected = idx == selected_index as usize;
        // The row under the pointer, which the tree reports because the
        // runtime's own hover probe cannot see a pop-over's rows. Selected
        // wins: a hover band under the selection would only mute it.
        let hovered = !selected && hover_popup_row == idx.to_string();
        e.inline_overlays.push(InlineOverlay {
            start: 0,
            end: text.len(),
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(if selected {
                    KEY_COMPLETION_SEL_FG
                } else {
                    KEY_COMPLETION_FG
                })),
                bg: match (selected, hovered) {
                    (true, _) => Some(OverlayColorSpec::theme_key(KEY_COMPLETION_SEL_BG)),
                    (false, true) => Some(OverlayColorSpec::theme_key(
                        crate::widgets::render::KEY_HOVER_BG,
                    )),
                    (false, false) => None,
                },
                bold: selected,
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
        entries.push(e);
        row_indices.push(idx);
    }
    PanelPopup {
        widget_key: widget_key.to_string(),
        anchor_row: 0,
        anchor_col,
        anchor_absolute: false,
        entries,
        row_indices,
    }
}

/// Is this Dropdown's option popup open?
pub(crate) fn is_open(widget_key: &str, panel: &crate::widgets::WidgetPanelState) -> bool {
    matches!(
        panel.instance_states.get(widget_key),
        Some(WidgetInstanceState::Dropdown { open: true, .. })
    )
}

/// Step the selection by `delta` with wraparound, preserving the
/// popup's open state; queues `change` when the selection moved.
pub(crate) fn cycle_selection(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    delta: i32,
    fx: &mut super::KeyFx,
) {
    let WidgetSpec::Dropdown {
        options,
        selected_index: spec_sel,
        ..
    } = spec
    else {
        return;
    };
    if options.is_empty() {
        return;
    }
    let (cur, open) = match panel.instance_states.get(widget_key) {
        Some(WidgetInstanceState::Dropdown {
            selected_index,
            open,
        }) => (*selected_index, *open),
        _ => (*spec_sel, false),
    };
    let cur = cur.clamp(0, options.len() as i32 - 1);
    let new_sel = crate::widgets::wrap_index(cur, delta, options.len());
    panel.instance_states.insert(
        widget_key.to_string(),
        WidgetInstanceState::Dropdown {
            selected_index: new_sel,
            // Preserve the popup's open state across a cycle so
            // Up/Down inside the open list keeps it open.
            open,
        },
    );
    if new_sel != cur {
        let value = options.get(new_sel as usize).cloned().unwrap_or_default();
        fx.events.push((
            "change".into(),
            serde_json::json!({ "index": new_sel, "value": value }),
        ));
    }
}

/// Set the selection to an absolute index (a click on an option row
/// of the open list), clamped into the option set, preserving the
/// popup's open state; queues `change` when the selection actually
/// moved. The absolute-index sibling of [`cycle_selection`].
pub(crate) fn set_selection(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    index: i32,
    fx: &mut super::KeyFx,
) {
    let WidgetSpec::Dropdown {
        options,
        selected_index: spec_sel,
        ..
    } = spec
    else {
        return;
    };
    if options.is_empty() {
        return;
    }
    let (cur, open) = match panel.instance_states.get(widget_key) {
        Some(WidgetInstanceState::Dropdown {
            selected_index,
            open,
        }) => (*selected_index, *open),
        _ => (*spec_sel, false),
    };
    let new_sel = index.clamp(0, options.len() as i32 - 1);
    let changed = new_sel != cur.clamp(0, options.len() as i32 - 1);
    panel.instance_states.insert(
        widget_key.to_string(),
        WidgetInstanceState::Dropdown {
            selected_index: new_sel,
            open,
        },
    );
    if changed {
        let value = options.get(new_sel as usize).cloned().unwrap_or_default();
        fx.events.push((
            "change".into(),
            serde_json::json!({ "index": new_sel, "value": value }),
        ));
    }
}

/// Open or close the option popup, preserving the selected index;
/// queues `dropdown_open` when the state actually flipped (never
/// `change` — opening/closing is not a value edit; the plugin needs
/// the distinction so e.g. Escape can close the list vs cancel the
/// dialog).
pub(crate) fn set_open(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    open: bool,
    fx: &mut super::KeyFx,
) {
    let WidgetSpec::Dropdown {
        selected_index: spec_sel,
        ..
    } = spec
    else {
        return;
    };
    let (cur, prev_open) = match panel.instance_states.get(widget_key) {
        Some(WidgetInstanceState::Dropdown {
            selected_index,
            open,
        }) => (*selected_index, *open),
        _ => (*spec_sel, false),
    };
    panel.instance_states.insert(
        widget_key.to_string(),
        WidgetInstanceState::Dropdown {
            selected_index: cur,
            open,
        },
    );
    if open != prev_open {
        fx.events
            .push(("dropdown_open".into(), serde_json::json!({ "open": open })));
    }
}

/// Kind policy for the plugin `SetDropdown` mutation: clamp the wire
/// index into THIS spec's option set, preserving an open popup. The
/// mutation arm in `plugin_dispatch` is a pure delegation.
pub(crate) fn set_index_state(
    spec: &WidgetSpec,
    prev: Option<&crate::widgets::WidgetInstanceState>,
    index: i32,
) -> crate::widgets::WidgetInstanceState {
    let len = match spec {
        WidgetSpec::Dropdown { options, .. } => options.len(),
        _ => 0,
    };
    let clamped = if len == 0 {
        0
    } else {
        index.clamp(0, len as i32 - 1)
    };
    let open = matches!(
        prev,
        Some(crate::widgets::WidgetInstanceState::Dropdown { open: true, .. })
    );
    crate::widgets::WidgetInstanceState::Dropdown {
        selected_index: clamped,
        open,
    }
}
