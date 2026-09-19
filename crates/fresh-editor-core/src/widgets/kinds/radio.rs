//! `Radio` — an inline single-select group: `label: (•) A   ( ) B   ( ) C`.
//!
//! What a `Dropdown` hides behind a pop-over, this lays out in the row —
//! the shape for a short, fixed set a form asks about first. Like
//! `Dropdown`, the selected index is host-owned after first render and the
//! spec's `selected_index` is only a seed. Left/Right and Home/End move it;
//! Up/Down are left to walk the form, like Tab.

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;
use serde_json::json;

use super::WidgetImpl;
use crate::widgets::registry::WidgetInstanceState;

pub struct Radio;

impl WidgetImpl for Radio {
    fn on_key(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        _viewport: super::Viewport,
        key: &crate::keys::KeySeq,
        fx: &mut super::KeyFx,
    ) -> super::KeyDisposition {
        use super::KeyDisposition::{Consumed, Pass};
        use crossterm::event::KeyCode;
        let Some(key) = key.single().filter(|k| k.mods().is_empty()) else {
            return Pass;
        };
        match key.code() {
            KeyCode::Left => {
                cycle_selection(spec, widget_key, panel, -1, fx);
                Consumed
            }
            KeyCode::Right => {
                cycle_selection(spec, widget_key, panel, 1, fx);
                Consumed
            }
            KeyCode::Home => {
                set_selection(spec, widget_key, panel, 0, fx);
                Consumed
            }
            KeyCode::End => {
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
}

/// **Where a `Radio`'s selection actually comes from.** Instance state
/// is authoritative after first render; the spec's `selected_index` is a
/// seed. Clamped into the option set either way. Pure, so every caller
/// gets one answer.
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::widgets::render::{render_radio, resolve_panel, RenderedRadio};
    use fresh_core::api::LabelAlign;

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

    /// The row as a panel would draw it: the selection resolved the way the
    /// description resolves it, then the kind's own formatter.
    fn drawn(
        spec: &WidgetSpec,
        states: &HashMap<String, WidgetInstanceState>,
        width: u32,
    ) -> RenderedRadio {
        let (WidgetSpec::Radio {
            options,
            selected_index,
            label,
            label_width,
            key,
            ..
        }) = spec
        else {
            unreachable!("a radio")
        };
        render_radio(
            options,
            resolve(options, *selected_index, key.as_deref(), states),
            label,
            false,
            *label_width,
            LabelAlign::Left,
            false,
            width,
        )
    }

    fn panel_of(spec: &WidgetSpec) -> crate::widgets::WidgetPanelState {
        let out = resolve_panel(spec, &HashMap::new(), "", true, None);
        crate::widgets::WidgetPanelState {
            buffer_id: Some(crate::model::event::BufferId(1)),
            spec: spec.clone(),
            instance_states: out.instance_states,
            focus_key: out.focus_key,
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
        let out = drawn(&radio(1, Some("r")), &HashMap::new(), 60);
        let text = out.entry.text.trim_end();
        assert_eq!(text, "Run in: ( ) Local   (\u{2022}) SSH   ( ) Kubernetes");
        // One range per option, each over its own glyph + name — the edges
        // the description splits the row at.
        assert_eq!(out.option_ranges.len(), 3);
        let piece = |i: usize| &text[out.option_ranges[i].0..out.option_ranges[i].1];
        assert_eq!(piece(0), "( ) Local");
        assert_eq!(piece(1), "(\u{2022}) SSH");
        assert_eq!(piece(2), "( ) Kubernetes");
    }

    /// **Narrow gives up spacing, never an option.** Every option has to
    /// stay readable and clickable, so the row sheds the gaps and then the
    /// label's column rather than running past the panel.
    #[test]
    fn a_narrow_panel_shrinks_the_row_instead_of_overflowing() {
        let spec = WidgetSpec::Radio {
            options: vec!["Local".into(), "SSH".into(), "Kubernetes".into()],
            selected_index: 0,
            label: "Run in".into(),
            focused: false,
            label_width: 15,
            key: Some("r".into()),
        };
        let at = |w: u32| drawn(&spec, &HashMap::new(), w);
        assert_eq!(
            at(60).entry.text.trim_end(),
            "Run in         : (\u{2022}) Local   ( ) SSH   ( ) Kubernetes",
            "a panel with room keeps the label column and the gaps"
        );
        assert_eq!(
            at(50).entry.text.trim_end(),
            "Run in         : (\u{2022}) Local ( ) SSH ( ) Kubernetes",
            "50 columns: the gaps go first, the label column stays"
        );
        // Below that the label column goes too. Three options and their
        // glyphs are 40 columns on their own, which is the floor: narrower
        // than that the row is as small as it can be made.
        for width in [44u32, 36, 30] {
            let out = at(width);
            let text = out.entry.text.trim_end();
            assert_eq!(
                text, "Run in: (\u{2022}) Local ( ) SSH ( ) Kubernetes",
                "{width}: shrunk as far as it goes"
            );
            assert_eq!(
                out.option_ranges.len(),
                3,
                "{width}: every option stays a target"
            );
            let piece = |i: usize| &text[out.option_ranges[i].0..out.option_ranges[i].1];
            assert_eq!(piece(2), "( ) Kubernetes", "{width}: and stays whole");
        }
    }

    #[test]
    fn left_right_cycle_and_fire_change_with_the_value() {
        let spec = radio(0, Some("r"));
        let mut panel = panel_of(&spec);
        let mut fx = super::super::KeyFx::default();
        let vp = super::super::Viewport::default();
        let press = |k: &str| -> crate::keys::KeySeq { k.parse().expect("test key name parses") };
        Radio.on_key(&spec, "r", &mut panel, vp, &press("Right"), &mut fx);
        assert_eq!(fx.events.len(), 1);
        assert_eq!(fx.events[0].0, "change");
        assert_eq!(fx.events[0].1["index"], 1);
        assert_eq!(fx.events[0].1["value"], "SSH");
        // Wraps at the ends.
        Radio.on_key(&spec, "r", &mut panel, vp, &press("Left"), &mut fx);
        Radio.on_key(&spec, "r", &mut panel, vp, &press("Left"), &mut fx);
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
        states.insert(
            "r".to_string(),
            WidgetInstanceState::Radio { selected_index: 99 },
        );
        assert!(
            drawn(&spec, &states, 60)
                .entry
                .text
                .contains("(\u{2022}) Kubernetes"),
            "clamped to the last option"
        );
    }
}
