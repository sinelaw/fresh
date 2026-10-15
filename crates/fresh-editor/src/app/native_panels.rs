//! Events from a natively laid-out plugin panel.
//!
//! A panel the web lays out itself (`view::native`) has no cells to click: its
//! controls are the page's own elements, sized and placed by CSS. So a press
//! on one arrives as the widget's *identity* — its key, the event the kind
//! answers a press with, and the row it is on — and this module turns that
//! into the very `WidgetEvent` the terminal's tree builds for a press on the
//! same control (`view::shell::widgets`), then applies it as the same
//! `UiFact`. From there the delivery is the terminal's, focus handling and
//! the plugin's `widget_event` hook included.
//!
//! **Built from the panel's spec, never taken from the page.** The page names
//! which control and which row; what the press *means* — the toggle's new
//! value, whether a tree row expands or collapses, which item key a list row
//! carries, whether the button is enabled at all — is read off the mounted
//! spec and the host's instance state, exactly as the tree reads it. A name
//! that matches nothing in the spec is dropped. So the page can ask for
//! anything a terminal user could click, and nothing else.
//!
//! **Rows outside the terminal's window resolve too.** The tree builds a
//! list's or tree's rows only for its visible window; a native list renders
//! every row and scrolls on its own. Identity delivery does not care which
//! rows the terminal happens to have built.

use crate::app::Editor;
use crate::view::shell::msg::UiFact;
use crate::view::shell::widgets::Slot;
use crate::widgets::{WidgetEvent, WidgetInstanceState};
use fresh_core::api::WidgetSpec;
use serde_json::{json, Value};
use std::collections::HashMap;

/// A press on a native panel control, as the page reports it.
#[derive(Debug, Clone, Default)]
pub(crate) struct NativeWidgetPress {
    /// The control's spec key — for a list row, the row's item key (the
    /// list is named by `payload.list_key`, or found by the item).
    pub widget_key: String,
    /// The event the kind answers a press with (`activate`, `toggle`,
    /// `select`, `expand`, …), or `context` for a right press on a row.
    /// Empty with `text_cursor` set: a click inside a text field.
    pub event_type: String,
    /// The row (`index`, `key`) or option (`index`, `column`) pressed.
    pub payload: Value,
    /// Where the caret goes in a text field, in bytes of its value.
    pub text_cursor: Option<usize>,
    /// The browser's click count.
    pub clicks: u8,
    /// The cell under the pointer, for a context menu's anchor.
    pub at: (u16, u16),
}

fn event(kind: &'static str, key: &str, event_type: &'static str, payload: Value) -> WidgetEvent {
    WidgetEvent {
        widget_key: key.to_string(),
        widget_kind: kind,
        payload,
        event_type,
        owner_key: None,
        row_target: false,
        context_click: false,
        drag_source: false,
    }
}

/// The first widget in `spec` (card items and pop-overs included) that
/// `pred` accepts.
fn find<'a>(spec: &'a WidgetSpec, pred: &dyn Fn(&WidgetSpec) -> bool) -> Option<&'a WidgetSpec> {
    if pred(spec) {
        return Some(spec);
    }
    if let WidgetSpec::List { item_specs, .. } = spec {
        if let Some(f) = item_specs.iter().find_map(|c| find(c, pred)) {
            return Some(f);
        }
    }
    spec.children().find_map(|c| find(c, pred))
}

fn keyed<'a>(spec: &'a WidgetSpec, key: &str) -> Option<&'a WidgetSpec> {
    find(spec, &|s| s.key() == Some(key))
}

fn index_of(payload: &Value) -> Option<usize> {
    payload.get("index")?.as_u64().map(|i| i as usize)
}

/// The terminal's `WidgetEvent` for `press` on `spec`, or `None` when the
/// press names nothing a terminal user could click.
pub(crate) fn resolve(
    spec: &WidgetSpec,
    states: &HashMap<String, WidgetInstanceState>,
    press: &NativeWidgetPress,
) -> Option<WidgetEvent> {
    let wk = press.widget_key.as_str();
    let p = &press.payload;
    match press.event_type.as_str() {
        "activate" => match keyed(spec, wk)? {
            WidgetSpec::Button {
                disabled: false, ..
            } => Some(event("button", wk, "activate", json!({}))),
            _ => None,
        },
        "focus" | "" => match keyed(spec, wk)? {
            WidgetSpec::Text { .. } => Some(event("text", wk, "focus", json!({}))),
            _ => None,
        },
        "number_value" => match keyed(spec, wk)? {
            WidgetSpec::Number { .. } => Some(event("number", wk, "number_value", json!({}))),
            _ => None,
        },
        "radio_select" => match keyed(spec, wk)? {
            WidgetSpec::Radio { options, .. } if index_of(p)? < options.len() => Some(event(
                "radio",
                wk,
                "radio_select",
                json!({ "index": index_of(p)? }),
            )),
            _ => None,
        },
        "dropdown_toggle" => match keyed(spec, wk)? {
            WidgetSpec::Dropdown { .. } => {
                Some(event("dropdown", wk, "dropdown_toggle", json!({})))
            }
            _ => None,
        },
        "dropdown_select" => match keyed(spec, wk)? {
            WidgetSpec::Dropdown { options, .. } if index_of(p)? < options.len() => {
                let mut e = event(
                    "dropdown",
                    wk,
                    "dropdown_select",
                    json!({ "index": index_of(p)? }),
                );
                e.row_target = true;
                Some(e)
            }
            _ => None,
        },
        "dual_focus" => match keyed(spec, wk)? {
            WidgetSpec::DualList { .. } => {
                let column = p.get("column")?.as_str()?;
                if column != "available" && column != "included" {
                    return None;
                }
                Some(event(
                    "dual_list",
                    wk,
                    "dual_focus",
                    json!({ "column": column, "index": index_of(p)? }),
                ))
            }
            _ => None,
        },
        "completion_pick" => match (keyed(spec, wk)?, states.get(wk)) {
            (WidgetSpec::Text { .. }, Some(WidgetInstanceState::Text { completions, .. }))
                if index_of(p)? < completions.len() =>
            {
                let mut e = event(
                    "text",
                    wk,
                    "completion_pick",
                    json!({ "index": index_of(p)? }),
                );
                e.row_target = true;
                Some(e)
            }
            _ => None,
        },
        "toggle" => match keyed(spec, wk)? {
            WidgetSpec::Toggle { checked, .. } => Some(event(
                "toggle",
                wk,
                "toggle",
                json!({ "checked": !checked }),
            )),
            tree @ WidgetSpec::Tree { .. } => tree_event(tree, states, wk, "toggle", p),
            _ => None,
        },
        "select" | "context" => {
            // A tree is named by its own key; a list row by its item key,
            // with the list in `list_key` (or found by the row).
            if let Some(tree @ WidgetSpec::Tree { .. }) = keyed(spec, wk) {
                return tree_event(tree, states, wk, "select", p);
            }
            list_event(spec, wk, p)
        }
        "expand" | "action" => match keyed(spec, wk)? {
            tree @ WidgetSpec::Tree { .. } => {
                let kind = if press.event_type == "expand" {
                    "expand"
                } else {
                    "action"
                };
                tree_event(tree, states, wk, kind, p)
            }
            _ => None,
        },
        _ => None,
    }
}

/// A list row's `select`, as `widgets::list_node` builds it: the item key as
/// the widget, the list as the owner. A card list's payload names the list.
fn list_event(spec: &WidgetSpec, item_key: &str, p: &Value) -> Option<WidgetEvent> {
    let index = index_of(p)?;
    let named = p.get("list_key").and_then(|k| k.as_str());
    let list = find(spec, &|s| match s {
        WidgetSpec::List {
            key,
            item_keys,
            items,
            item_specs,
            ..
        } => {
            let len = if item_specs.is_empty() {
                items.len()
            } else {
                item_specs.len()
            };
            index < len
                && match named {
                    Some(n) => key.as_deref() == Some(n),
                    None => item_keys.get(index).map(String::as_str).unwrap_or("") == item_key,
                }
        }
        _ => false,
    })?;
    let WidgetSpec::List {
        key,
        item_keys,
        item_specs,
        ..
    } = list
    else {
        return None;
    };
    let list_key = key.clone().unwrap_or_default();
    let item = item_keys.get(index).cloned().unwrap_or_default();
    let payload = match item_specs.is_empty() {
        true => json!({ "index": index, "key": item }),
        false => json!({ "index": index, "key": item, "list_key": list_key }),
    };
    let mut e = event("list", &item, "select", payload);
    e.owner_key = Some(list_key);
    e.row_target = true;
    e.context_click = true;
    Some(e)
}

/// A tree row's event, as `widgets::tree_node` builds it.
fn tree_event(
    tree: &WidgetSpec,
    states: &HashMap<String, WidgetInstanceState>,
    tree_key: &str,
    kind: &'static str,
    p: &Value,
) -> Option<WidgetEvent> {
    let WidgetSpec::Tree {
        nodes,
        item_keys,
        expanded_keys,
        ..
    } = tree
    else {
        return None;
    };
    let index = index_of(p)?;
    let node = nodes.get(index)?;
    let item_key = item_keys.get(index).cloned().unwrap_or_default();
    // The row the page saw must still be the row at that index.
    if p.get("key")
        .and_then(|k| k.as_str())
        .is_some_and(|k| k != item_key)
    {
        return None;
    }
    let open = match states.get(tree_key) {
        Some(WidgetInstanceState::Tree { expanded_keys, .. }) => expanded_keys.contains(&item_key),
        _ => expanded_keys.contains(&item_key),
    };
    let payload = match kind {
        "expand" if node.has_children => {
            json!({ "index": index, "key": item_key, "expanded": !open })
        }
        "toggle" if node.checked.is_some() => json!({
            "index": index, "key": item_key, "checked": !node.checked.unwrap_or(false),
        }),
        "action" if node.action.is_some() => json!({ "index": index, "key": item_key }),
        "select" => json!({ "index": index, "key": item_key }),
        _ => return None,
    };
    let mut e = event("tree", tree_key, kind, payload);
    if kind == "select" {
        e.row_target = true;
        e.context_click = true;
        e.drag_source = node.draggable;
    }
    Some(e)
}

impl Editor {
    /// Apply a press on a natively laid-out panel control (see the module
    /// docs). Dropped when the panel is not mounted or the press names
    /// nothing in its spec.
    pub(crate) fn press_native_widget(&mut self, slot: Slot, press: NativeWidgetPress) {
        let panel_key = match slot {
            Slot::Dock => self.panel(crate::app::PanelSlot::Dock),
            Slot::Floating => self.panel(crate::app::PanelSlot::Floating),
            Slot::Sidebar(i) => self.panel(crate::app::PanelSlot::Sidebar(i)),
            Slot::Pane(pane) => {
                // A pane names its panel through the buffer it shows.
                let Some(key) = self.pane_panel_key(pane) else {
                    return;
                };
                return self.press_native_panel(slot, key, press);
            }
            _ => return,
        }
        .map(|p| p.panel_key.clone());
        let Some(panel_key) = panel_key else {
            return;
        };
        self.press_native_panel(slot, panel_key, press);
    }

    fn press_native_panel(
        &mut self,
        slot: Slot,
        panel_key: crate::widgets::PanelKey,
        press: NativeWidgetPress,
    ) {
        // A press outside an anchored pop-up, which the page's scrim caught:
        // spent on dismissing it, as the tree's layer spends it
        // (`shell::panel`, `Dismiss::OUTSIDE_POINTER`).
        if press.event_type == "dismiss" {
            let anchored = matches!(slot, Slot::Floating)
                && self.floating_widget_panel.as_ref().is_some_and(|f| {
                    matches!(f.placement, crate::app::PanelPlacement::Anchored { .. })
                });
            if anchored {
                self.apply_native_fact(UiFact::PanelClosed);
            }
            return;
        }
        let Some(spec) = self.widget_registry.get(&panel_key).map(|p| p.spec.clone()) else {
            return;
        };
        let states = self
            .widget_registry
            .instance_states(&panel_key)
            .cloned()
            .unwrap_or_default();
        let Some(event) = resolve(&spec, &states, &press) else {
            return;
        };
        let fact = if press.event_type == "context" {
            UiFact::WidgetContext {
                slot,
                event,
                x: press.at.0,
                y: press.at.1,
            }
        } else {
            UiFact::WidgetHit {
                slot,
                event,
                byte: press.text_cursor,
                clicks: press.clicks.max(1),
            }
        };
        self.apply_native_fact(fact);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn spec(v: Value) -> WidgetSpec {
        serde_json::from_value(v).expect("a valid widget spec")
    }

    fn press(key: &str, event_type: &str, payload: Value) -> NativeWidgetPress {
        NativeWidgetPress {
            widget_key: key.into(),
            event_type: event_type.into(),
            payload,
            ..Default::default()
        }
    }

    fn panel() -> WidgetSpec {
        spec(json!({
            "kind": "col",
            "children": [
                { "kind": "button", "key": "go", "label": "Go" },
                { "kind": "button", "key": "off", "label": "Off", "disabled": true },
                { "kind": "toggle", "key": "all", "label": "All", "checked": true },
                { "kind": "radio", "key": "mode", "options": ["a", "b"] },
                { "kind": "list", "key": "rows", "items": [{ "text": "one" }, { "text": "two" }],
                  "itemKeys": ["r1", "r2"] },
                { "kind": "tree", "key": "t", "itemKeys": ["f", "leaf"],
                  "expandedKeys": [],
                  "nodes": [
                      { "text": { "text": "folder" }, "depth": 0, "hasChildren": true },
                      { "text": { "text": "leaf" }, "depth": 1, "checked": false }
                  ] }
            ]
        }))
    }

    #[test]
    fn a_button_press_is_the_buttons_activate_and_a_disabled_one_is_nothing() {
        let states = HashMap::new();
        let e = resolve(&panel(), &states, &press("go", "activate", json!({}))).unwrap();
        assert_eq!((e.widget_kind, e.event_type), ("button", "activate"));
        assert!(resolve(&panel(), &states, &press("off", "activate", json!({}))).is_none());
        assert!(resolve(&panel(), &states, &press("nope", "activate", json!({}))).is_none());
        // A key that names a toggle is not a button.
        assert!(resolve(&panel(), &states, &press("all", "activate", json!({}))).is_none());
    }

    #[test]
    fn what_a_press_means_comes_from_the_spec_not_the_page() {
        let states = HashMap::new();
        // The page says nothing about the new value; the spec's `checked`
        // decides it, as the tree's toggle does.
        let e = resolve(
            &panel(),
            &states,
            &press("all", "toggle", json!({ "checked": true })),
        )
        .unwrap();
        assert_eq!(e.payload, json!({ "checked": false }));
        assert!(resolve(
            &panel(),
            &states,
            &press("mode", "radio_select", json!({ "index": 5 }))
        )
        .is_none());
    }

    #[test]
    fn a_list_row_is_its_item_key_owned_by_its_list() {
        let states = HashMap::new();
        let e = resolve(
            &panel(),
            &states,
            &press("r2", "select", json!({ "index": 1, "key": "r2" })),
        )
        .unwrap();
        assert_eq!(e.widget_key, "r2");
        assert_eq!(e.owner_key.as_deref(), Some("rows"));
        assert_eq!(e.payload, json!({ "index": 1, "key": "r2" }));
        assert!(e.row_target && e.context_click);
        // A row that is not there any more resolves to nothing.
        assert!(resolve(
            &panel(),
            &states,
            &press("r9", "select", json!({ "index": 7 }))
        )
        .is_none());
    }

    #[test]
    fn a_tree_row_expands_by_the_hosts_state_and_rejects_a_stale_row() {
        let mut states = HashMap::new();
        let e = resolve(
            &panel(),
            &states,
            &press("t", "expand", json!({ "index": 0, "key": "f" })),
        )
        .unwrap();
        assert_eq!(e.payload["expanded"], json!(true), "collapsed in the spec");
        states.insert(
            "t".to_string(),
            WidgetInstanceState::Tree {
                selected_index: 0,
                expanded_keys: ["f".to_string()].into_iter().collect(),
            },
        );
        let e = resolve(
            &panel(),
            &states,
            &press("t", "expand", json!({ "index": 0, "key": "f" })),
        )
        .unwrap();
        assert_eq!(
            e.payload["expanded"],
            json!(false),
            "open in the host's state"
        );
        // A leaf has nothing to expand; a row whose key moved is not pressed.
        assert!(resolve(
            &panel(),
            &states,
            &press("t", "expand", json!({ "index": 1 }))
        )
        .is_none());
        assert!(resolve(
            &panel(),
            &states,
            &press("t", "select", json!({ "index": 1, "key": "f" }))
        )
        .is_none());
        let e = resolve(
            &panel(),
            &states,
            &press("t", "toggle", json!({ "index": 1, "key": "leaf" })),
        )
        .unwrap();
        assert_eq!(e.payload["checked"], json!(true));
    }
}
