//! A control edited by its widget kind, against a surface's store.
//!
//! The settings page and each entry dialog are forms of the same
//! `WidgetSpec`s a plugin panel is made of, and their controls — toggle,
//! number, dropdown, text, the dual list, the JSON editor — are edited by
//! the same kinds
//! (`fresh_editor_core::widgets::kinds`, design §3.5, §3.6). What a surface
//! keeps is the *model* (`SettingControl`) and a store the kinds read and
//! write ([`WidgetPanelState::surface`]); what it does is hand a key or a
//! press to the kind and apply the effects the kind reports — the same two
//! things `Editor::handle_widget_key` and `deliver_widget_hit` do for a
//! plugin's panel, with the plugin's `widget_event` replaced by a write to
//! the model.
//!
//! **Live.** A control is live while the store's `focus_key` names it — or
//! one of its rows: a text list's fields are keyed `{path}::row::{i}` and
//! `{path}::add`, and the live one is the field the keyboard is in. Its
//! keys are its own until it is left, so the surface's ring (Tab included)
//! does not take them. A surface decides when a control becomes live
//! (Enter on a card, a press on a value cell) and when it stops (the kind
//! closed its draft or its list; Enter, Tab or Escape on a text field or a
//! dual list, whose edit-mode is the surface's convention rather than the
//! kind's; Tab or Escape on a JSON editor, whose Enter is a newline). A
//! map's or an object array's `List` is live while its card is selected:
//! the arrows walk its rows, and at either end the surface moves on.

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use fresh_core::api::WidgetSpec;

use super::items::SettingControl;
use crate::widgets::kinds::{behavior, KeyDisposition, KeyFx, Viewport};
use crate::widgets::{WidgetInstanceState, WidgetPanelState};

/// What a key is to a widget kind: one of the named keys the kinds'
/// vocabulary distinguishes, or text to type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KeyName {
    Named(String),
    Text(String),
}

/// The name a widget kind knows a key by — the same spelling the panel
/// router hands `Editor::handle_widget_key` (`"S-Left"`, `"C-a"`,
/// `"Shift+Tab"`, `"Space"`), plus `"Escape"`, which the router resolves
/// before it names anything because a panel's Escape is its own.
pub fn key_name(ev: &KeyEvent) -> Option<KeyName> {
    let ctrl = ev.modifiers.contains(KeyModifiers::CONTROL);
    let shift = ev.modifiers.contains(KeyModifiers::SHIFT);
    let alt = ev.modifiers.contains(KeyModifiers::ALT);
    let named = |s: &str| Some(KeyName::Named(s.to_string()));
    match ev.code {
        KeyCode::Esc => named("Escape"),
        KeyCode::Tab if shift => named("Shift+Tab"),
        KeyCode::Tab => named("Tab"),
        KeyCode::BackTab => named("Shift+Tab"),
        KeyCode::Enter => named("Enter"),
        KeyCode::Backspace | KeyCode::Delete if ctrl => {
            let base = if ev.code == KeyCode::Backspace {
                "Backspace"
            } else {
                "Delete"
            };
            Some(KeyName::Named(format!("C-{base}")))
        }
        KeyCode::Backspace => named("Backspace"),
        KeyCode::Delete => named("Delete"),
        KeyCode::PageUp => named("PageUp"),
        KeyCode::PageDown => named("PageDown"),
        KeyCode::Home
        | KeyCode::End
        | KeyCode::Left
        | KeyCode::Right
        | KeyCode::Up
        | KeyCode::Down => {
            let base = match ev.code {
                KeyCode::Home => "Home",
                KeyCode::End => "End",
                KeyCode::Left => "Left",
                KeyCode::Right => "Right",
                KeyCode::Up => "Up",
                _ => "Down",
            };
            Some(KeyName::Named(format!(
                "{}{}{base}",
                if ctrl { "C-" } else { "" },
                if shift { "S-" } else { "" }
            )))
        }
        KeyCode::Char(c) if ctrl || alt => Some(KeyName::Named(format!(
            "{}{}{}",
            if ctrl { "C-" } else { "" },
            if alt { "A-" } else { "" },
            c.to_ascii_lowercase()
        ))),
        KeyCode::Char(' ') => named("Space"),
        KeyCode::Char(c) => {
            let ch = if shift {
                c.to_uppercase().next().unwrap_or(c)
            } else {
                c
            };
            Some(KeyName::Text(ch.to_string()))
        }
        _ => None,
    }
}

/// What the kind did with a key, and what it asks the surface to do.
pub struct Outcome {
    pub disposition: KeyDisposition,
    pub fx: KeyFx,
}

/// Hand a key to the control's kind. `spec` is the control as the surface
/// describes it (`widget_map`), `key` the control's widget key — its
/// setting path — in `store`.
pub fn key(store: &mut WidgetPanelState, spec: &WidgetSpec, key: &str, ev: &KeyEvent) -> Outcome {
    let mut fx = KeyFx::default();
    let disposition = match key_name(ev) {
        Some(KeyName::Named(name)) => {
            behavior(spec).on_key(spec, key, store, Viewport::from_spec(spec), &name, &mut fx)
        }
        Some(KeyName::Text(text)) => behavior(spec).on_text(spec, key, store, &text, &mut fx),
        None => KeyDisposition::Pass,
    };
    Outcome { disposition, fx }
}

/// Hand a named key to the control's kind — a surface's own decision
/// (`"Enter"` to activate, `"Escape"` to leave) rather than a keystroke.
pub fn named(store: &mut WidgetPanelState, spec: &WidgetSpec, key: &str, name: &str) -> Outcome {
    let mut fx = KeyFx::default();
    let disposition =
        behavior(spec).on_key(spec, key, store, Viewport::from_spec(spec), name, &mut fx);
    Outcome { disposition, fx }
}

/// Type text into the control's kind — a paste, or the character that
/// began an edit.
pub fn text(store: &mut WidgetPanelState, spec: &WidgetSpec, key: &str, text: &str) -> Outcome {
    let mut fx = KeyFx::default();
    let disposition = behavior(spec).on_text(spec, key, store, text, &mut fx);
    Outcome { disposition, fx }
}

/// Hand a pointer hit to the control's kind, focus already on it.
pub fn pointer(
    store: &mut WidgetPanelState,
    spec: &WidgetSpec,
    key: &str,
    event_type: &str,
    payload: &serde_json::Value,
) -> Outcome {
    let mut fx = crate::widgets::kinds::PointerFx::default();
    let disposition = behavior(spec).on_pointer(spec, key, store, event_type, payload, &mut fx);
    Outcome {
        disposition: match disposition {
            crate::widgets::kinds::PointerDisposition::Consumed => KeyDisposition::Consumed,
            crate::widgets::kinds::PointerDisposition::Default => KeyDisposition::Pass,
        },
        fx: fx.key,
    }
}

/// Apply the events a kind reported to the model. `widget_key` is the key
/// the kind answered for — the control's, or one of a text list's rows.
/// Returns whether the control's value changed — the surface's cue to
/// record the change.
///
/// A text field's and a JSON editor's `change` is applied as it is typed:
/// the value is the model's throughout (a dialog's title reads its key
/// field live; a JSON editor's validity is read off its text), and the
/// surface records or reverts it when the edit ends. A text list row's
/// `change` is applied the same way; its add row's draft is the field's
/// alone until the surface takes it (`text_list::take_draft`). A list's
/// `select` and `activate` change no value — they are the surface's to
/// act on.
pub fn apply(
    control: &mut SettingControl,
    widget_key: &str,
    events: &[(String, serde_json::Value)],
) -> bool {
    let mut changed = false;
    for (event, payload) in events {
        match (&mut *control, event.as_str()) {
            (
                SettingControl::Toggle {
                    checked, inherited, ..
                },
                "toggle",
            ) => {
                if let Some(now) = payload.get("checked").and_then(|v| v.as_bool()) {
                    changed |= *checked != now || *inherited;
                    *checked = now;
                    *inherited = false;
                }
            }
            (SettingControl::Number { value, .. }, "change") => {
                if let Some(now) = payload.get("value").and_then(|v| v.as_f64()) {
                    changed |= *value != now;
                    *value = now;
                }
            }
            (SettingControl::Dropdown { selected, .. }, "change") => {
                if let Some(now) = payload.get("index").and_then(|v| v.as_u64()) {
                    changed |= *selected != now as usize;
                    *selected = now as usize;
                }
            }
            (SettingControl::Text { value, .. }, "change") => {
                if let Some(now) = payload.get("value").and_then(|v| v.as_str()) {
                    changed |= value != now;
                    *value = now.to_string();
                }
            }
            (SettingControl::Json { text, .. }, "change") => {
                if let Some(now) = payload.get("value").and_then(|v| v.as_str()) {
                    changed |= text != now;
                    *text = now.to_string();
                }
            }
            (SettingControl::DualList { included, .. }, "change") => {
                if let Some(now) = payload.get("included").and_then(|v| v.as_array()) {
                    let now: Vec<String> = now
                        .iter()
                        .filter_map(|v| v.as_str().map(String::from))
                        .collect();
                    changed |= *included != now;
                    *included = now;
                }
            }
            (SettingControl::TextList { items, .. }, "change") => {
                if let (Some(Some(i)), Some(now)) = (
                    text_list::row_of(widget_key),
                    payload.get("value").and_then(|v| v.as_str()),
                ) {
                    if let Some(item) = items.get_mut(i) {
                        changed |= item != now;
                        *item = now.to_string();
                    }
                }
            }
            _ => {}
        }
    }
    changed
}

/// Whether `control` is one the kinds edit: every control but the opaque
/// `Complex`, which is edited in the config file.
pub fn kind_edited(control: &SettingControl) -> bool {
    !matches!(control, SettingControl::Complex { .. })
}

/// Whether the kind still holds the control after a key: a number with a
/// draft open, a dropdown with its list up. The others have no such state
/// of their own — their edit-mode is the surface's — so they answer `true`
/// and the surface decides; a toggle is never held.
pub fn kind_holds(control: &SettingControl, store: &WidgetPanelState, key: &str) -> bool {
    match control {
        SettingControl::Number { .. } => crate::widgets::kinds::number::editing(key, store),
        SettingControl::Dropdown { .. } => crate::widgets::kinds::dropdown::is_open(key, store),
        SettingControl::Toggle { .. } | SettingControl::Complex { .. } => false,
        _ => true,
    }
}

/// Put a composite's `List` cursor on `row`: the control takes the
/// keyboard, its rows walked by the kind from here.
pub fn seed_list(store: &mut WidgetPanelState, key: &str, row: usize) {
    store.instance_states.insert(
        key.to_string(),
        WidgetInstanceState::List {
            selected_index: row as i32,
            user_scrolled: false,
        },
    );
    store.focus_key = key.to_string();
}

/// The row a composite's `List` cursor is on, as the kind resolves it —
/// `spec` being the list's node — or `None` when it has none.
pub fn list_row(store: &WidgetPanelState, spec: &WidgetSpec, key: &str) -> Option<usize> {
    let sel = crate::widgets::kinds::list::resolve_in(spec, key, &store.instance_states).selected;
    (sel >= 0).then_some(sel as usize)
}

/// A text list's rows as fields: which one is live, opening one, and the
/// add row's draft.
pub mod text_list {
    use super::{drop_state, seed_text, text_editor, WidgetInstanceState, WidgetPanelState};
    use crate::view::settings::items::SettingControl;

    /// The row a widget key names — `Some(i)` an item's field, `None` the
    /// add row's — or nothing when the key is not a text list row's.
    pub fn row_of(key: &str) -> Option<Option<usize>> {
        let (head, last) = key.rsplit_once("::")?;
        if last == "add" {
            return Some(None);
        }
        match head.rsplit_once("::") {
            Some((_, "row")) => last.parse().ok().map(Some),
            _ => None,
        }
    }

    /// The row whose field is live, when one of this list's is.
    pub fn live_row(store: &WidgetPanelState, path: &str) -> Option<Option<usize>> {
        store
            .focus_key
            .strip_prefix(path)
            .filter(|rest| rest.starts_with("::"))
            .and_then(|_| row_of(&store.focus_key))
    }

    /// Open a row's field, the caret at the end: an item's with its value,
    /// the add row's empty. The field that was live is left.
    pub fn edit_row(
        store: &mut WidgetPanelState,
        path: &str,
        items: &[String],
        row: Option<usize>,
    ) {
        if let Some(live) = live_row(store, path) {
            if live != row {
                drop_state(store, &SettingControl::text_list_row_key(path, live));
            }
        }
        let key = SettingControl::text_list_row_key(path, row);
        let value = row.and_then(|i| items.get(i)).cloned().unwrap_or_default();
        seed_text(store, &key, &value, false, false);
        store.focus_key = key;
    }

    /// The add row's draft as typed, or nothing when the row is not open.
    pub fn draft(store: &WidgetPanelState, path: &str) -> Option<String> {
        match store
            .instance_states
            .get(&SettingControl::text_list_row_key(path, None))
        {
            Some(WidgetInstanceState::Text { editor, .. }) => Some(editor.value()),
            _ => None,
        }
    }

    /// Take the add row's draft to make a row of it: the text typed there,
    /// the field left empty for the next. `None` when there is nothing but
    /// whitespace.
    pub fn take_draft(store: &mut WidgetPanelState, path: &str) -> Option<String> {
        let editor = text_editor(store, &SettingControl::text_list_row_key(path, None))?;
        let text = editor.value();
        if text.trim().is_empty() {
            return None;
        }
        editor.set_value("");
        Some(text)
    }

    /// Leave the live row: its field's state is forgotten, the add row's
    /// draft with it.
    pub fn leave(store: &mut WidgetPanelState, path: &str) {
        if let Some(row) = live_row(store, path) {
            drop_state(store, &SettingControl::text_list_row_key(path, row));
        }
    }
}

/// Seed a text field's instance state from the model, the caret at the
/// end and — when `select_all` — the whole value selected so the first
/// keystroke replaces it. The kind seeds from the spec on first touch
/// otherwise; this is the surface saying where the edit starts. A JSON
/// editor's field is `multiline`; a text field's flattens newlines.
pub fn seed_text(
    store: &mut WidgetPanelState,
    key: &str,
    value: &str,
    select_all: bool,
    multiline: bool,
) {
    let mut editor = match multiline {
        true => crate::primitives::text_edit::TextEdit::with_text(value),
        false => crate::primitives::text_edit::TextEdit::single_line_with_text(value),
    };
    editor.set_cursor_from_flat(value.len());
    if select_all {
        editor.select_all();
    }
    store.instance_states.insert(
        key.to_string(),
        WidgetInstanceState::Text {
            editor,
            scroll: 0,
            completions: Vec::new(),
            completion_selected_index: 0,
            completion_scroll_offset: 0,
            completion_navigated: false,
            user_scrolled: false,
        },
    );
}

/// The text field's editor in the store, while it is live.
pub fn text_editor<'a>(
    store: &'a mut WidgetPanelState,
    key: &str,
) -> Option<&'a mut crate::primitives::text_edit::TextEdit> {
    match store.instance_states.get_mut(key) {
        Some(WidgetInstanceState::Text { editor, .. }) => Some(editor),
        _ => None,
    }
}

/// Forget a control's interaction state: the surface left it.
pub fn drop_state(store: &mut WidgetPanelState, key: &str) {
    store.instance_states.remove(key);
    if store.focus_key == key {
        store.focus_key.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ev(code: KeyCode, m: KeyModifiers) -> KeyEvent {
        KeyEvent::new(code, m)
    }

    #[test]
    fn keys_are_named_as_the_kinds_know_them() {
        let n = |c, m| key_name(&ev(c, m));
        assert_eq!(
            n(KeyCode::Esc, KeyModifiers::NONE),
            Some(KeyName::Named("Escape".into()))
        );
        assert_eq!(
            n(KeyCode::Left, KeyModifiers::CONTROL | KeyModifiers::SHIFT),
            Some(KeyName::Named("C-S-Left".into()))
        );
        assert_eq!(
            n(KeyCode::BackTab, KeyModifiers::SHIFT),
            Some(KeyName::Named("Shift+Tab".into()))
        );
        assert_eq!(
            n(KeyCode::Char('a'), KeyModifiers::CONTROL),
            Some(KeyName::Named("C-a".into()))
        );
        assert_eq!(
            n(KeyCode::Char(' '), KeyModifiers::NONE),
            Some(KeyName::Named("Space".into()))
        );
        assert_eq!(
            n(KeyCode::Char('x'), KeyModifiers::SHIFT),
            Some(KeyName::Text("X".into()))
        );
        assert_eq!(n(KeyCode::F(1), KeyModifiers::NONE), None);
    }

    #[test]
    fn a_toggle_event_lands_on_the_model_and_clears_inheritance() {
        let mut c = SettingControl::Toggle {
            label: "t".into(),
            checked: false,
            inherited: true,
        };
        let changed = apply(
            &mut c,
            "k",
            &[("toggle".into(), serde_json::json!({ "checked": true }))],
        );
        assert!(changed);
        assert!(matches!(
            c,
            SettingControl::Toggle {
                checked: true,
                inherited: false,
                ..
            }
        ));
    }

    #[test]
    fn a_change_that_changes_nothing_is_not_a_change() {
        let mut c = SettingControl::Number {
            label: "n".into(),
            value: 4.0,
            min: None,
            max: None,
            integer: true,
            percent: false,
        };
        assert!(!apply(
            &mut c,
            "k",
            &[("change".into(), serde_json::json!({ "value": 4.0 }))]
        ));
        assert!(apply(
            &mut c,
            "k",
            &[("change".into(), serde_json::json!({ "value": 5.0 }))]
        ));
    }
}
