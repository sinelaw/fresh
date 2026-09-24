//! `List` — virtual-scrolled select list (classic rows or card items).

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;
use serde_json::json;

use super::WidgetImpl;
use crate::widgets::registry::WidgetInstanceState;

pub struct List;

impl WidgetImpl for List {
    fn picker_nav(&self) -> super::PickerNav {
        // A peek keeps the filter input focused for typing while the
        // arrow moves the list selection.
        super::PickerNav::Peek
    }

    fn activates_on_picker_enter(&self) -> bool {
        true
    }

    fn picker_activate_event(
        &self,
        spec: &WidgetSpec,
        key: &str,
        panel: &crate::widgets::WidgetPanelState,
    ) -> Option<(String, serde_json::Value)> {
        activate_event(spec, key, panel)
    }

    fn box_meta(&self, spec: &WidgetSpec) -> super::BoxMeta {
        let mut m = super::BoxMeta::plain("list");
        if let WidgetSpec::List {
            key: Some(k),
            focusable,
            ..
        } = spec
        {
            if !k.is_empty() {
                m.key = Some(k.clone());
                m.focusable = *focusable;
                m.scrollable = true;
                m.picker_scroll_target = true;
            }
        }
        m
    }

    /// Keyboard model: arrows move the host-owned selection, Page
    /// keys jump by a viewport page (one row of overlap so the user
    /// keeps a visual anchor), Enter and Space activate the selected
    /// item. All self-contained state + events, so it lives with the
    /// kind; the panel-level picker forwarding (arrows on a sibling
    /// filter input) reuses [`select_move`] through the host shell.
    fn on_key(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        viewport: super::Viewport,
        key: &crate::keys::KeySeq,
        fx: &mut super::KeyFx,
    ) -> super::KeyDisposition {
        use crossterm::event::KeyCode;
        let Some(key) = key.single() else {
            return super::KeyDisposition::Pass;
        };
        // Every key a list answers is unmodified.
        if !key.mods().is_empty() {
            return super::KeyDisposition::Pass;
        }
        match key.code() {
            KeyCode::Up | KeyCode::Down => {
                let delta = if key.code() == KeyCode::Up { -1 } else { 1 };
                select_move(spec, widget_key, panel, delta, fx);
            }
            KeyCode::PageUp | KeyCode::PageDown => {
                // **A page is a window of items, and it arrives as one.**
                // `select_move`'s delta counts items — it adds it to the
                // selection and clamps against the item *count* — so a row
                // count handed over undivided pages `item_height` times too
                // far: a list of three-row cards in a twelve-row window
                // jumped eleven cards when four were on screen. The window
                // now reaches this seam already in items, so there is no
                // conversion here to get wrong (and none duplicated from
                // `on_wheel`, which had the only correct copy).
                //
                // One item of overlap so the user keeps a visual anchor.
                let page = viewport.items.saturating_sub(1).max(1) as i32;
                let delta = if key.code() == KeyCode::PageUp {
                    -page
                } else {
                    page
                };
                select_move(spec, widget_key, panel, delta, fx);
            }
            // Home / End: the first and the last item (the listbox pattern).
            KeyCode::Home | KeyCode::End => {
                let to = match key.code() {
                    KeyCode::Home => 0,
                    _ => total_items(spec) as i32 - 1,
                };
                select_to(spec, widget_key, panel, to, fx);
            }
            KeyCode::Enter | KeyCode::Char(' ') => {
                if let Some(ev) = activate_event(spec, widget_key, panel) {
                    fx.events.push(ev);
                }
            }
            _ => return super::KeyDisposition::Pass,
        }
        super::KeyDisposition::Consumed
    }

    /// **Type-ahead** (the listbox pattern): characters typed in quick
    /// succession build a prefix, and the selection moves to the next item
    /// whose text starts with it — case-insensitively, wrapping. The same
    /// character typed again cycles through the items it starts. A pause
    /// longer than [`TYPE_AHEAD_PAUSE`] starts a new prefix.
    fn on_text(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        text: &str,
        fx: &mut super::KeyFx,
    ) -> super::KeyDisposition {
        let typed: String = text.chars().filter(|c| !c.is_control()).collect();
        if typed.is_empty() {
            return super::KeyDisposition::Pass;
        }
        let labels = item_labels(spec);
        if labels.is_empty() {
            return super::KeyDisposition::Pass;
        }
        let prefix = type_ahead_prefix(widget_key, &typed);
        let cur = resolve_in(spec, widget_key, &panel.instance_states).selected;
        if let Some(to) = type_ahead_match(&labels, cur, &prefix) {
            select_to(spec, widget_key, panel, to as i32, fx);
        }
        super::KeyDisposition::Consumed
    }
    /// Pointer model: a row click syncs the host-owned selection to
    /// the clicked index, then lets the recorded `select`
    /// event fire against the List's own key, identical to keyboard
    /// nav plus the `via: "click"` marker. Right-click `context`
    /// hits pass through untouched.
    fn on_pointer(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        event_type: &str,
        payload: &serde_json::Value,
        _fx: &mut super::PointerFx,
    ) -> super::PointerDisposition {
        if event_type == "select" {
            if let Some(idx) = payload.get("index").and_then(|v| v.as_i64()) {
                // Through the same clamp every other reader uses: the
                // payload names a row this render drew, but the render
                // that drew it is not the one this click lands on, and
                // nothing sanitises a stored index any more.
                panel.instance_states.insert(
                    widget_key.to_string(),
                    WidgetInstanceState::List {
                        selected_index: clamp_selection(idx as i32, total_items(spec)),
                    },
                );
            }
        }
        super::PointerDisposition::Default
    }
}

/// A `List`'s state, once the spec and the instance map have been
/// reconciled. The window is deliberately absent: that is the viewport's,
/// and the tree's viewport element is where it lives.
pub struct Resolved {
    /// The selection clamped into the current dataset, or `-1` for none
    /// (an empty list, or a list nobody has selected in).
    pub selected: i32,
}

/// **Where a `List`'s selection actually comes from.**
///
/// Instance state is authoritative once a handler has decided one; the
/// spec's `selected_index` is a seed until then. The stored value is
/// clamped into the *current* dataset on every read, because a dataset can
/// shrink underneath a standing selection and nothing writes the clamp
/// down: `collect_list` used to, which made the render walk an authority on
/// state that `select_move` and the pointer path also write, and made every
/// other reader depend on a paint having happened first.
///
/// Pulled out of the collector because the collector is not the only caller
/// that needs the answer — the key path, the pointer path, the picker-Enter
/// path and the *description* all do, and a second copy of the clamp is a
/// second place for it to drift. Pure in what it is handed, which is what
/// lets a description call it (`view::shell::widgets`'s `List` arms). The
/// shape [`crate::widgets::kinds::dropdown::resolve`] already has.
pub fn resolve(
    total: u32,
    spec_selected: i32,
    key: Option<&str>,
    prev: &HashMap<String, WidgetInstanceState>,
) -> Resolved {
    let stored = match key.filter(|k| !k.is_empty()).and_then(|k| prev.get(k)) {
        Some(WidgetInstanceState::List { selected_index }) => *selected_index,
        _ => spec_selected,
    };
    Resolved {
        selected: clamp_selection(stored, total),
    }
}

/// [`resolve`] against a whole `List` spec — the form every handler wants,
/// since a handler holds the spec node and the panel rather than the
/// collector's unpacked fields.
pub fn resolve_in(
    spec: &WidgetSpec,
    widget_key: &str,
    prev: &HashMap<String, WidgetInstanceState>,
) -> Resolved {
    let spec_selected = match spec {
        WidgetSpec::List { selected_index, .. } => *selected_index,
        _ => -1,
    };
    resolve(total_items(spec), spec_selected, Some(widget_key), prev)
}

/// **The clamp itself, so that there is one of it.** A selection either
/// names an item that exists or it is `-1`; an empty list has no selection
/// at all. [`resolve`] applies it to a stored index and `List::on_pointer`
/// to a clicked one — the only two ways an index enters.
pub fn clamp_selection(sel: i32, total: u32) -> i32 {
    if sel < 0 || total == 0 {
        -1
    } else {
        sel.min(total as i32 - 1)
    }
}

/// How many items a `List` has. Cards override the plain `items` rows —
/// see `WidgetSpec::List::item_specs` — and every count in this module is
/// in items, never rows.
pub fn total_items(spec: &WidgetSpec) -> u32 {
    match spec {
        WidgetSpec::List {
            items, item_specs, ..
        } => {
            if item_specs.is_empty() {
                items.len() as u32
            } else {
                item_specs.len() as u32
            }
        }
        _ => 0,
    }
}

/// Move the host-owned selection by `delta` (clamped to the item
/// range) and queue `select` — but only when the index actually moved:
/// a clamped move at the list's top/bottom edge still repaints but must
/// not spam the plugin with same-index selections — each one re-runs the
/// plugin's preview / live-switch work.
pub fn select_move(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    delta: i32,
    fx: &mut super::KeyFx,
) {
    let WidgetSpec::List { item_keys, .. } = spec else {
        return;
    };
    let total = total_items(spec);
    if total == 0 {
        return;
    }
    let cur_sel = resolve_in(spec, widget_key, &panel.instance_states).selected;
    let raw = if cur_sel < 0 { 0 } else { cur_sel + delta };
    let new_sel = raw.clamp(0, total as i32 - 1);
    let new_key = item_keys.get(new_sel as usize).cloned().unwrap_or_default();
    panel.instance_states.insert(
        widget_key.to_string(),
        WidgetInstanceState::List {
            selected_index: new_sel,
        },
    );
    if new_sel != cur_sel {
        fx.events
            .push(("select".into(), json!({ "index": new_sel, "key": new_key })));
    }
}

/// Move the selection to `to` (clamped), firing `select` when it moved.
fn select_to(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    to: i32,
    fx: &mut super::KeyFx,
) {
    let cur = resolve_in(spec, widget_key, &panel.instance_states)
        .selected
        .max(0);
    select_move(spec, widget_key, panel, to - cur, fx);
}

/// How long a pause starts a new type-ahead prefix.
pub const TYPE_AHEAD_PAUSE: std::time::Duration = std::time::Duration::from_millis(1000);

thread_local! {
    /// Per list: the type-ahead prefix so far and when it was last extended.
    /// Transient input state, like a key chord in progress — not part of the
    /// list's model, so it stays out of the instance-state map.
    static TYPE_AHEAD: std::cell::RefCell<HashMap<String, (std::time::Instant, String)>> =
        std::cell::RefCell::new(HashMap::new());
}

/// Extend `widget_key`'s type-ahead prefix with `typed`, or start a new one
/// after a pause. Returns the prefix to search for.
fn type_ahead_prefix(widget_key: &str, typed: &str) -> String {
    let now = std::time::Instant::now();
    TYPE_AHEAD.with(|m| {
        let mut m = m.borrow_mut();
        let prefix = match m.get(widget_key) {
            Some((at, p)) if now.duration_since(*at) < TYPE_AHEAD_PAUSE => format!("{p}{typed}"),
            _ => typed.to_string(),
        };
        m.insert(widget_key.to_string(), (now, prefix.clone()));
        prefix
    })
}

/// The item a type-ahead `prefix` selects, from the selection `cur`: the
/// next item (wrapping) whose label starts with it. A prefix of one repeated
/// character (`"ii"`) cycles through the items that character starts; a
/// longer prefix stays on the current item while it still matches, so
/// typing a name out does not skip past it.
pub fn type_ahead_match(labels: &[String], cur: i32, prefix: &str) -> Option<usize> {
    let lower = prefix.to_lowercase();
    let mut chars = lower.chars();
    let first = chars.next()?;
    let repeated = chars.all(|c| c == first);
    let (needle, from_next) = match repeated {
        true => (first.to_string(), true),
        false => (lower, false),
    };
    let n = labels.len();
    let start = match (cur < 0, from_next) {
        (true, _) => 0,
        (false, true) => (cur as usize + 1) % n,
        (false, false) => cur as usize % n,
    };
    (0..n)
        .map(|i| (start + i) % n)
        .find(|&i| labels[i].trim_start().to_lowercase().starts_with(&needle))
}

/// Each item's text, for type-ahead: a classic row's text, or a card's
/// first text.
fn item_labels(spec: &WidgetSpec) -> Vec<String> {
    let WidgetSpec::List { items, .. } = spec else {
        return Vec::new();
    };
    items.iter().map(|e| e.text.clone()).collect()
}

/// The `activate` event for the currently-selected item, if any.
/// Shared by Enter/Space in [`List::on_key`] and the panel-level
/// picker forwarding (Enter on a sibling filter input activates the
/// list without moving focus).
pub fn activate_event(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &crate::widgets::WidgetPanelState,
) -> Option<(String, serde_json::Value)> {
    let WidgetSpec::List { item_keys, .. } = spec else {
        return None;
    };
    // **Clamped at the read, because nothing sanitises the write.**
    // `select_move` clamps its own result, so the stored index is in range
    // for as long as the selection moved it — but a dataset that *shrank*
    // underneath a standing selection was only brought back into range by
    // the collector's per-frame write-back, and that write-back is gone.
    // Reading it raw fires `activate` with an out-of-range `index` and an
    // empty `key`. [`resolve`] is where that clamp lives now, for every
    // reader alike.
    let sel = resolve_in(spec, widget_key, &panel.instance_states).selected;
    if sel < 0 {
        return None;
    }
    let item_key = item_keys.get(sel as usize).cloned().unwrap_or_default();
    Some(("activate".into(), json!({ "index": sel, "key": item_key, })))
}

#[cfg(test)]
mod tests {
    use super::type_ahead_match;

    fn labels(v: &[&str]) -> Vec<String> {
        v.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn a_prefix_finds_the_next_item_it_starts() {
        let l = labels(&["..", "work/", "config", "id_ed25519", "id_rsa"]);
        assert_eq!(type_ahead_match(&l, 0, "i"), Some(3));
        // Typing the name out stays on the item while it still matches…
        assert_eq!(type_ahead_match(&l, 3, "id"), Some(3));
        // …and moves on once only another item does.
        assert_eq!(type_ahead_match(&l, 3, "id_r"), Some(4));
        // Case-insensitive, and nothing matched leaves nothing to select.
        assert_eq!(type_ahead_match(&l, 0, "W"), Some(1));
        assert_eq!(type_ahead_match(&l, 0, "zz"), None);
    }

    #[test]
    fn a_repeated_character_cycles_the_items_it_starts() {
        let l = labels(&["..", "id_ed25519", "config", "id_rsa"]);
        assert_eq!(type_ahead_match(&l, 1, "ii"), Some(3));
        assert_eq!(type_ahead_match(&l, 3, "iii"), Some(1));
    }
}
