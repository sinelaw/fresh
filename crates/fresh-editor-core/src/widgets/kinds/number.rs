//! `Number` — form value cell with in-place edit mode.
//!
//! The cell has two states, both the kind's own. Displayed, it shows the
//! value and the arrows step it. Editing, it shows the draft the user is
//! typing — digits, a sign, a point — with a caret and a selection, in
//! the value's display units; Enter commits the draft (parsed, scaled
//! back, clamped) and Tab commits it before it advances, Escape and a
//! blur abandon it. Enter, a click on the cell, or simply typing a digit
//! begins one, with the whole value selected so the first digit replaces
//! it. The draft is instance state ([`WidgetInstanceState::Number`]) and
//! never crosses the plugin boundary: the plugin sees the `change` a
//! commit fires, and nothing before it.

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;
use serde_json::json;

use super::WidgetImpl;
use crate::primitives::text_edit::TextEdit;
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
        use super::KeyDisposition::{Consumed, Pass, PassAfter};
        if editing(widget_key, panel) {
            return match key {
                "Enter" => {
                    commit(spec, widget_key, panel, fx);
                    Consumed
                }
                "Escape" => {
                    cancel(widget_key, panel);
                    Consumed
                }
                // The commit is the field's; the advance is the surface's.
                "Tab" | "Shift+Tab" => {
                    commit(spec, widget_key, panel, fx);
                    PassAfter
                }
                "C-a" => {
                    with_draft(widget_key, panel, |e| e.select_all());
                    Consumed
                }
                // A draft has no vertical axis and no words to type; these
                // keys are swallowed rather than handed to the surface, so a
                // half-typed value is never left behind by a page move.
                "Space" | "Up" | "Down" | "PageUp" | "PageDown" => Consumed,
                _ => match super::text::key_name_to_event(key) {
                    Some(event) => {
                        with_draft(widget_key, panel, |e| {
                            crate::primitives::text_key::apply_text_key(
                                e,
                                &event,
                                crate::primitives::text_key::TextKeyContext::single_line(),
                            );
                        });
                        Consumed
                    }
                    None => Pass,
                },
            };
        }
        // Up/Right increment, Down/Left decrement — matching the
        // ◂/▸ glyphs (the reverse of a list's Up = select-previous).
        let steps = match key {
            "Up" | "Right" => 1,
            "Down" | "Left" => -1,
            "Enter" => {
                begin_edit(spec, widget_key, panel, None);
                return Consumed;
            }
            _ => return Pass,
        };
        adjust(spec, widget_key, panel, steps, fx);
        Consumed
    }

    /// Typing into the cell: a digit, a sign or a point begins the edit
    /// (the typed text replacing the value) or extends the draft; any
    /// other character is not a number's and passes.
    fn on_text(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        text: &str,
        _fx: &mut super::KeyFx,
    ) -> super::KeyDisposition {
        let typed: String = text.chars().filter(|c| is_number_char(*c)).collect();
        if editing(widget_key, panel) {
            if !typed.is_empty() {
                with_draft(widget_key, panel, |e| e.insert_str(&typed));
            }
            return super::KeyDisposition::Consumed;
        }
        if typed.is_empty() {
            return super::KeyDisposition::Pass;
        }
        begin_edit(spec, widget_key, panel, Some(&typed));
        super::KeyDisposition::Consumed
    }

    /// Pointer model: a click on the value cell begins the in-place edit
    /// (focus already moved here before this runs), with the value
    /// selected so the next digit replaces it. The recorded
    /// `number_value` hit is the kind's and never surfaces to the plugin.
    fn on_pointer(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        event_type: &str,
        _payload: &serde_json::Value,
        _fx: &mut super::PointerFx,
    ) -> super::PointerDisposition {
        if event_type == "number_value" {
            if !editing(widget_key, panel) {
                begin_edit(spec, widget_key, panel, None);
            }
            super::PointerDisposition::Consumed
        } else {
            super::PointerDisposition::Default
        }
    }

    /// Leaving the cell any way but Enter or Tab abandons the draft: the
    /// same rule a dropdown's blur applies to its list.
    fn on_focus_change(
        &self,
        panel: &mut crate::widgets::WidgetPanelState,
        key: &str,
        gained: bool,
    ) {
        if !gained {
            cancel(key, panel);
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
    let Resolved { value: cur, draft } = resolve(spec_value, min, max, key, prev);
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
        draft.as_ref().map(NumberEdit::from),
        ctx.marker_gutter,
    );
    let RenderedNumber {
        mut entry,
        value_range,
    } = rendered;
    // A click on the value cell begins in-place editing (`on_pointer`).
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

/// A `Number`'s two pieces of state, once the spec and the instance map
/// have been reconciled: the value the cell shows, clamped into the spec's
/// bounds, and the draft being typed into it, if one is open.
pub struct Resolved {
    pub value: f64,
    pub draft: Option<Draft>,
}

/// The open edit, as the cell paints it.
pub struct Draft {
    pub text: String,
    pub cursor: usize,
    pub selection: Option<(usize, usize)>,
}

impl<'a> From<&'a Draft> for NumberEdit<'a> {
    fn from(d: &'a Draft) -> Self {
        NumberEdit {
            text: &d.text,
            cursor: d.cursor as i32,
            sel_start: d.selection.map(|(a, _)| a as i32).unwrap_or(-1),
            sel_end: d.selection.map(|(_, b)| b as i32).unwrap_or(-1),
        }
    }
}

/// **Where a `Number`'s value and draft come from.** Instance state is
/// authoritative once a handler has written one; the spec's `value` is
/// the seed until then. Pure, so a description can call it — see
/// `view::shell::widgets`'s `Number` arm.
pub fn resolve(
    spec_value: f64,
    min: Option<f64>,
    max: Option<f64>,
    key: Option<&str>,
    prev: &HashMap<String, WidgetInstanceState>,
) -> Resolved {
    let (cur, draft) = match key.filter(|k| !k.is_empty()).and_then(|k| prev.get(k)) {
        Some(WidgetInstanceState::Number { value, edit }) => (
            *value,
            edit.as_ref().map(|e| Draft {
                text: e.value(),
                cursor: e.flat_cursor_byte(),
                selection: e.selection_flat_range(),
            }),
        ),
        _ => (spec_value, None),
    };
    Resolved {
        value: clamp_number(cur, min, max),
        draft,
    }
}

fn is_number_char(c: char) -> bool {
    c.is_ascii_digit() || c == '-' || c == '.'
}

/// Whether the keyed cell has a draft open.
pub fn editing(widget_key: &str, panel: &crate::widgets::WidgetPanelState) -> bool {
    matches!(
        panel.instance_states.get(widget_key),
        Some(WidgetInstanceState::Number { edit: Some(_), .. })
    )
}

/// The value the spec and the instance map agree on, unclamped.
fn current(spec: &WidgetSpec, widget_key: &str, panel: &crate::widgets::WidgetPanelState) -> f64 {
    let spec_value = match spec {
        WidgetSpec::Number { value, .. } => *value,
        _ => 0.0,
    };
    match panel.instance_states.get(widget_key) {
        Some(WidgetInstanceState::Number { value, .. }) => *value,
        _ => spec_value,
    }
}

/// The digits a value is edited as: its display units, without the
/// `%` a percent cell shows — `0.25` is edited as `25`.
fn draft_text(value: f64, integer: bool, percent: bool) -> String {
    if percent {
        format!("{}", (value * 100.0).round() as i64)
    } else {
        crate::widgets::render::format_number_value(value, integer, false)
    }
}

/// Open the draft on the current value, wholly selected so the first
/// keystroke replaces it — or, when `replace` carries what was typed to
/// begin it, on that text with the caret at its end.
pub fn begin_edit(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    replace: Option<&str>,
) {
    let WidgetSpec::Number {
        min,
        max,
        integer,
        percent,
        ..
    } = spec
    else {
        return;
    };
    let value = clamp_number(current(spec, widget_key, panel), *min, *max);
    let mut editor = match replace {
        Some(text) => TextEdit::single_line_with_text(text),
        None => TextEdit::single_line_with_text(&draft_text(value, *integer, *percent)),
    };
    editor.move_end();
    if replace.is_none() {
        editor.select_all();
    }
    panel.instance_states.insert(
        widget_key.to_string(),
        WidgetInstanceState::Number {
            value,
            edit: Some(editor),
        },
    );
}

fn with_draft(
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    op: impl FnOnce(&mut TextEdit),
) {
    if let Some(WidgetInstanceState::Number {
        edit: Some(editor), ..
    }) = panel.instance_states.get_mut(widget_key)
    {
        op(editor);
    }
}

/// Close the draft, keeping the value it opened on.
pub fn cancel(widget_key: &str, panel: &mut crate::widgets::WidgetPanelState) {
    if let Some(WidgetInstanceState::Number { edit, .. }) =
        panel.instance_states.get_mut(widget_key)
    {
        *edit = None;
    }
}

/// Close the draft, parsing it into the value: display units scaled
/// back (a percent's `25` is `0.25`), an integer rounded, the result
/// clamped into the spec's bounds. A draft that does not parse leaves
/// the value as it was. Queues `change` when the value moved.
pub fn commit(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    fx: &mut super::KeyFx,
) {
    let WidgetSpec::Number {
        min,
        max,
        integer,
        percent,
        ..
    } = spec
    else {
        return;
    };
    let Some(WidgetInstanceState::Number { value, edit }) =
        panel.instance_states.get_mut(widget_key)
    else {
        return;
    };
    let Some(editor) = edit.take() else {
        return;
    };
    let before = clamp_number(*value, *min, *max);
    let parsed = editor.value().trim().parse::<f64>().ok().map(|n| {
        let n = if *percent { n / 100.0 } else { n };
        if *integer {
            n.round()
        } else {
            n
        }
    });
    let after = clamp_number(parsed.unwrap_or(before), *min, *max);
    *value = after;
    if after != before {
        fx.events
            .push(("change".into(), serde_json::json!({ "value": after })));
    }
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
    let WidgetSpec::Number { min, max, step, .. } = spec else {
        return;
    };
    let cur = current(spec, widget_key, panel);
    let raw = cur + (steps as f64) * step;
    let clamped = crate::widgets::clamp_number(raw, *min, *max);
    let changed = clamped != cur;
    panel.instance_states.insert(
        widget_key.to_string(),
        WidgetInstanceState::Number {
            value: clamped,
            edit: None,
        },
    );
    if changed {
        fx.events
            .push(("change".into(), serde_json::json!({ "value": clamped })));
    }
}

/// Kind policy for the plugin `SetNumber` mutation: clamp the wire
/// value to THIS spec's bounds and produce the instance state. The
/// mutation arm in `plugin_dispatch` is a pure delegation — the
/// per-kind knowledge (where the bounds live) stays here. A plugin's
/// set closes any draft: the value it names is the value now.
pub fn set_value_state(spec: &WidgetSpec, value: f64) -> crate::widgets::WidgetInstanceState {
    let (min, max) = match spec {
        WidgetSpec::Number { min, max, .. } => (*min, *max),
        _ => (None, None),
    };
    crate::widgets::WidgetInstanceState::Number {
        value: crate::widgets::clamp_number(value, min, max),
        edit: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::widgets::kinds::{behavior, KeyDisposition, KeyFx};
    use crate::widgets::WidgetPanelState;

    fn number(value: f64, integer: bool, percent: bool) -> WidgetSpec {
        WidgetSpec::Number {
            value,
            min: Some(0.0),
            max: Some(100.0),
            step: 1.0,
            integer,
            percent,
            label: "n".into(),
            focused: false,
            label_width: 0,
            key: Some("n".into()),
        }
    }

    fn key(spec: &WidgetSpec, panel: &mut WidgetPanelState, k: &str) -> (KeyDisposition, KeyFx) {
        let mut fx = KeyFx::default();
        let d = behavior(spec).on_key(spec, "n", panel, Default::default(), k, &mut fx);
        (d, fx)
    }

    fn text(spec: &WidgetSpec, panel: &mut WidgetPanelState, t: &str) -> KeyDisposition {
        let mut fx = KeyFx::default();
        behavior(spec).on_text(spec, "n", panel, t, &mut fx)
    }

    fn value_of(panel: &WidgetPanelState) -> f64 {
        match panel.instance_states.get("n") {
            Some(WidgetInstanceState::Number { value, .. }) => *value,
            _ => panic!("no number state"),
        }
    }

    #[test]
    fn typing_a_digit_begins_the_edit_with_the_digit_replacing_the_value() {
        let spec = number(42.0, true, false);
        let mut panel = WidgetPanelState::surface(spec.clone());
        assert_eq!(text(&spec, &mut panel, "7"), KeyDisposition::Consumed);
        let draft = resolve(42.0, None, None, Some("n"), &panel.instance_states)
            .draft
            .expect("a draft is open");
        assert_eq!(draft.text, "7");
        let (d, fx) = key(&spec, &mut panel, "Enter");
        assert_eq!(d, KeyDisposition::Consumed);
        assert_eq!(value_of(&panel), 7.0);
        assert_eq!(fx.events[0].0, "change");
        assert!(!editing("n", &panel));
    }

    #[test]
    fn enter_opens_the_draft_selected_so_the_next_digits_replace_it() {
        let spec = number(500.0, true, false);
        let mut panel = WidgetPanelState::surface(spec.clone());
        // The spec bounds cap at 100; the draft still shows what it opened on.
        let (d, _) = key(&spec, &mut panel, "Enter");
        assert_eq!(d, KeyDisposition::Consumed);
        let draft = resolve(500.0, None, None, Some("n"), &panel.instance_states)
            .draft
            .unwrap();
        assert_eq!(draft.text, "100");
        assert_eq!(draft.selection, Some((0, 3)));
        text(&spec, &mut panel, "1");
        text(&spec, &mut panel, "0");
        let draft = resolve(500.0, None, None, Some("n"), &panel.instance_states)
            .draft
            .unwrap();
        assert_eq!(
            draft.text, "10",
            "the selection was replaced, not appended to"
        );
    }

    #[test]
    fn escape_abandons_the_draft_and_tab_commits_before_it_passes() {
        let spec = number(5.0, true, false);
        let mut panel = WidgetPanelState::surface(spec.clone());
        text(&spec, &mut panel, "9");
        let (d, fx) = key(&spec, &mut panel, "Escape");
        assert_eq!(d, KeyDisposition::Consumed);
        assert!(fx.events.is_empty());
        assert_eq!(value_of(&panel), 5.0);
        text(&spec, &mut panel, "9");
        let (d, fx) = key(&spec, &mut panel, "Tab");
        assert_eq!(d, KeyDisposition::PassAfter);
        assert_eq!(value_of(&panel), 9.0);
        assert_eq!(fx.events.len(), 1);
    }

    #[test]
    fn a_percent_is_edited_in_display_units() {
        let spec = WidgetSpec::Number {
            value: 0.25,
            min: Some(0.0),
            max: Some(1.0),
            step: 0.01,
            integer: false,
            percent: true,
            label: String::new(),
            focused: false,
            label_width: 0,
            key: Some("n".into()),
        };
        let mut panel = WidgetPanelState::surface(spec.clone());
        key(&spec, &mut panel, "Enter");
        let draft = resolve(0.25, None, None, Some("n"), &panel.instance_states)
            .draft
            .unwrap();
        assert_eq!(draft.text, "25");
        text(&spec, &mut panel, "40");
        key(&spec, &mut panel, "Enter");
        assert_eq!(value_of(&panel), 0.40);
    }

    #[test]
    fn caret_keys_edit_the_draft_and_letters_are_not_a_numbers() {
        let spec = number(5.0, true, false);
        let mut panel = WidgetPanelState::surface(spec.clone());
        text(&spec, &mut panel, "123");
        key(&spec, &mut panel, "Left");
        key(&spec, &mut panel, "Backspace");
        assert_eq!(text(&spec, &mut panel, "x"), KeyDisposition::Consumed);
        let draft = resolve(5.0, None, None, Some("n"), &panel.instance_states)
            .draft
            .unwrap();
        assert_eq!(draft.text, "13");
        assert_eq!(draft.cursor, 1);
        // Displayed, a letter is not the cell's key at all.
        key(&spec, &mut panel, "Escape");
        assert_eq!(text(&spec, &mut panel, "x"), KeyDisposition::Pass);
    }

    #[test]
    fn a_blur_abandons_the_draft() {
        let spec = number(5.0, true, false);
        let mut panel = WidgetPanelState::surface(spec.clone());
        text(&spec, &mut panel, "9");
        behavior(&spec).on_focus_change(&mut panel, "n", false);
        assert!(!editing("n", &panel));
        assert_eq!(value_of(&panel), 5.0);
    }
}
