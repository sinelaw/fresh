//! `Tree` — disclosure tree with optional checkboxes and card density.

use std::collections::{HashMap, HashSet};

use fresh_core::api::WidgetSpec;
use serde_json::json;

use super::WidgetImpl;
use crate::widgets::registry::WidgetInstanceState;

pub struct Tree;

use crate::widgets::render::PAN_COLUMNS;

impl WidgetImpl for Tree {
    fn picker_nav(&self) -> super::PickerNav {
        // A Tree is a real (tabbable) focus target. Peek-forwarding
        // would move the tree's selection while the previously focused
        // button/field keeps its focus ring — two focused elements at
        // once, and Enter would still act on the button, not the
        // highlighted row. Focus moves INTO the tree instead.
        super::PickerNav::TakeFocus
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
        let mut m = super::BoxMeta::plain("tree");
        if let WidgetSpec::Tree { key: Some(k), .. } = spec {
            if !k.is_empty() {
                m.key = Some(k.clone());
                m.focusable = true;
                m.scrollable = true;
                m.picker_scroll_target = true;
            }
        }
        m
    }

    /// Keep the single-focus invariant when panel focus crosses a
    /// Tree boundary. A Tree renders a highlight on its selected row
    /// independent of panel focus — deliberate, so editor-driven
    /// match navigation can highlight a row while the panel is
    /// unfocused. The cost is that focus moving within the panel
    /// could leave a toolbar button's focus ring next to a
    /// highlighted tree row (two focused elements), or Tab onto the
    /// tree with no visible selection (invisible focus). So: clear a
    /// blurred tree's selection, and seed a newly focused tree's to
    /// its first visible row when it has none. Kind-owned (moved from
    /// the central focus-move path).
    fn on_focus_change(
        &self,
        panel: &mut crate::widgets::WidgetPanelState,
        key: &str,
        gained: bool,
    ) {
        if !gained {
            panel.set_selected_index(key, -1);
            return;
        }
        // Through the one resolver: an untouched tree records no state
        // at all now, and the spec's `selected_index` is its seed — so
        // reading the map raw would land focus on the first row of a
        // tree whose plugin had already said which row was selected.
        let cur_sel = match crate::widgets::find_widget_by_key(&panel.spec, key) {
            Some(spec) => resolve(spec, key, &panel.instance_states).selected,
            None => -1,
        };
        if cur_sel >= 0 {
            return;
        }
        // First visible (un-collapsed) node, honoring the host's
        // expansion set through the same resolver — which falls back to
        // the spec's `expanded_keys` seed, so this is one statement of
        // that rule rather than a second. Computed in a scope so the
        // spec borrow ends before the selection write.
        let first = {
            let Some(node_spec) = crate::widgets::find_widget_by_key(&panel.spec, key) else {
                return;
            };
            let WidgetSpec::Tree {
                nodes, item_keys, ..
            } = node_spec
            else {
                return;
            };
            let expanded = resolve(node_spec, key, &panel.instance_states).expanded;
            collect_visible_tree_indices(nodes, item_keys, &expanded)
                .first()
                .map(|&i| i as i32)
        };
        if let Some(first) = first {
            panel.set_selected_index(key, first);
        }
    }

    /// Keyboard model: arrows walk the visible-flat order (skipping
    /// descendants of collapsed nodes), Page keys jump by a viewport
    /// page in *nodes*, Right expands / Left collapses-or-parents,
    /// Enter activates, Space toggles a checkable row's checkbox
    /// (falling back to activate). Self-contained state + events, so
    /// it lives with the kind.
    fn on_key(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        viewport: super::Viewport,
        key: &str,
        fx: &mut super::KeyFx,
    ) -> super::KeyDisposition {
        match key {
            "Up" | "Down" => {
                let delta = if key == "Up" { -1 } else { 1 };
                select_move(spec, widget_key, panel, delta, fx);
            }
            "PageUp" | "PageDown" => {
                // A Tree paces in *nodes*, and the window arrives in
                // them: `viewport.items` is the row budget already
                // divided by the rows one node occupies (bordered cards
                // take two extra each, so that division is a
                // conservative page estimate and never overshoots). The
                // division is the resolver's — this seam only pages.
                // One node of overlap so the user keeps a visual anchor.
                let page = viewport.items.saturating_sub(1).max(1) as i32;
                let delta = if key == "PageUp" { -page } else { page };
                select_move(spec, widget_key, panel, delta, fx);
            }
            "Left" | "Right" => {
                lateral(spec, widget_key, panel, key == "Right", fx);
            }
            // Panning. `Left`/`Right` are collapse/expand — the tree meaning
            // every OS tree widget and the ARIA tree pattern give them — so
            // sideways takes the one arrow chord whose conventional meaning a
            // read-only, single-select tree does not have: extend selection.
            // (`Alt`+arrows are back/forward, `Ctrl`+arrows word-wise; both
            // are bound.) Issue #1580.
            "S-Left" | "S-Right" | "S-Home" | "S-End" => {
                let bounds = crate::widgets::render::pan_bounds(spec, viewport.cols, None);
                // `S-End` is "the end of the row I am on", not "the end of
                // the longest row": the pan is shared, rows of unequal length
                // cannot all sit at their tail at once, and answering with
                // the longest one leaves the selected row clamped with
                // keystrokes still to spend before it moves.
                let selected = resolve(spec, widget_key, &panel.instance_states).selected;
                let end = usize::try_from(selected)
                    .ok()
                    .map(|r| crate::widgets::render::pan_bounds(spec, viewport.cols, Some(r)).1)
                    // A row with nowhere to go — a file header among match
                    // rows — would make the key a no-op while the rows around
                    // it still had a tail to show. Fall back to the longest.
                    .filter(|&r| r > 0)
                    .unwrap_or(bounds.1);
                let delta = match key {
                    "S-Left" => Some(-PAN_COLUMNS),
                    "S-Right" => Some(PAN_COLUMNS),
                    // Home is where each row's content says it should rest —
                    // its own match — not the head of the line. The head is a
                    // few more `S-Left`s away, and a reader who wants the
                    // match back should not have to pan to find it.
                    "S-Home" => None,
                    // Far enough that the per-row clamp lands every row on
                    // its own tail — and no further, so `S-Left` walks back
                    // from the end of the longest row rather than from a
                    // number no content justifies.
                    _ => Some(end),
                };
                if !panel.pan_h(widget_key, delta, bounds) {
                    // Already home, or already at the value asked for: say so,
                    // so the key can mean something else further out rather
                    // than being swallowed by a tree that did nothing.
                    return super::KeyDisposition::Pass;
                }
            }
            "Enter" => {
                if let Some(ev) = activate_event(spec, widget_key, panel) {
                    fx.events.push(ev);
                }
            }
            "Space" => {
                // On a checkable Tree, Space is the conventional
                // checkbox key — toggle the focused row (matching what
                // a click on its `[v]`/`[ ]` glyph would do). Falls
                // back to `activate` for trees that aren't checkable,
                // or rows without a checkbox glyph (`checked: None`).
                if let Some(ev) = toggle_if_checkable_event(spec, widget_key, panel) {
                    fx.events.push(ev);
                } else if let Some(ev) = activate_event(spec, widget_key, panel) {
                    fx.events.push(ev);
                }
            }
            _ => return super::KeyDisposition::Pass,
        }
        super::KeyDisposition::Consumed
    }

    /// Pointer model: a disclosure-column click toggles the row's
    /// expansion (the host owns the expanded-keys set) and fires its
    /// own `expand` event with the post-toggle state — the recorded
    /// hit event is suppressed. A row-body click syncs the host-owned
    /// selection to the clicked index and then lets the recorded
    /// `select` fire, mirroring the List path — without the sync a
    /// click would leave the highlight where it was. Checkbox
    /// `toggle` and right-click `context` hits pass through.
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
            "expand" => {
                let Some(item_key) = payload.get("key").and_then(|v| v.as_str()) else {
                    // Keyless row: nothing to toggle, and the recorded
                    // event would name no row — swallow, as the old
                    // central handler did.
                    return super::PointerDisposition::Consumed;
                };
                let Resolved {
                    selected: cur_sel,
                    mut expanded,
                    user_scrolled: cur_user_scrolled,
                } = resolve(spec, widget_key, &panel.instance_states);
                let now_expanded = if expanded.contains(item_key) {
                    expanded.remove(item_key);
                    false
                } else {
                    expanded.insert(item_key.to_string());
                    true
                };
                panel.instance_states.insert(
                    widget_key.to_string(),
                    WidgetInstanceState::Tree {
                        selected_index: cur_sel,
                        expanded_keys: expanded,
                        // A disclosure click doesn't move the selection —
                        // keep the user's scroll suppression as-is.
                        user_scrolled: cur_user_scrolled,
                    },
                );
                fx.key.events.push((
                    "expand".to_string(),
                    serde_json::json!({ "key": item_key, "expanded": now_expanded }),
                ));
                super::PointerDisposition::Consumed
            }
            "select" => {
                if let Some(idx) = payload.get("index").and_then(|v| v.as_i64()) {
                    panel.set_selected_index(widget_key, idx as i32);
                }
                super::PointerDisposition::Default
            }
            _ => super::PointerDisposition::Default,
        }
    }
}

/// A `Tree`'s state, once the spec and the instance map have been
/// reconciled. As with [`crate::widgets::kinds::list::Resolved`], the
/// window is absent: that is the viewport's, and the tree's viewport
/// element holds it.
pub struct Resolved {
    /// The selected node's ABSOLUTE index into `nodes`, or `-1`.
    ///
    /// **Not clamped to what is visible** — that clamp needs the
    /// visible-flat walk and belongs with the one that does it
    /// (`render_widget_tree`'s `clamp_to_visible`, which then also has
    /// to find the selection's position for scroll math). Every handler
    /// read this value raw before, and still does; what changed is only
    /// where the seed comes from.
    pub selected: i32,
    /// The expanded-key set: the stored one once anything has expanded
    /// or collapsed a node, the spec's seed until then.
    pub expanded: HashSet<String>,
    /// Whether the user has taken the window off the selection by mouse.
    pub user_scrolled: bool,
}

/// **Where a `Tree`'s selection and expansion actually come from.**
///
/// Instance state is authoritative once a handler has decided; the spec's
/// `selected_index` / `expanded_keys` are seeds until then. That used to be
/// true only of the first frame, because the render walk wrote a resolved
/// entry back on every one — so every handler could read the map raw and
/// get a seeded answer. The walk no longer decides, so an untouched tree has
/// no entry at all and the seeding has to happen where every reader is: at
/// the read. Without this, a wheel notch or a focus arrival on a tree whose
/// plugin had already named a selected row would silently discard it.
pub fn resolve(
    spec: &WidgetSpec,
    widget_key: &str,
    prev: &HashMap<String, WidgetInstanceState>,
) -> Resolved {
    let (spec_selected, spec_expanded) = match spec {
        WidgetSpec::Tree {
            selected_index,
            expanded_keys,
            ..
        } => (*selected_index, expanded_keys.as_slice()),
        _ => (-1, &[] as &[String]),
    };
    resolve_seeded(spec_selected, spec_expanded, widget_key, prev)
}

/// [`resolve`] against the seeds directly, for the collector — which is
/// handed a `Tree`'s fields unpacked rather than the spec node itself.
pub fn resolve_seeded(
    spec_selected: i32,
    spec_expanded: &[String],
    widget_key: &str,
    prev: &HashMap<String, WidgetInstanceState>,
) -> Resolved {
    match prev.get(widget_key).filter(|_| !widget_key.is_empty()) {
        Some(WidgetInstanceState::Tree {
            selected_index,
            expanded_keys,
            user_scrolled,
        }) => Resolved {
            selected: *selected_index,
            expanded: expanded_keys.clone(),
            user_scrolled: *user_scrolled,
        },
        _ => Resolved {
            selected: spec_selected,
            expanded: spec_expanded.iter().cloned().collect(),
            user_scrolled: false,
        },
    }
}

/// Move the host-owned selection by `delta` along the visible-flat
/// order (descendants of collapsed nodes are skipped — selection is
/// the *absolute* `nodes` index, so we walk the visible order to
/// find the neighbour), re-arming scroll-follows-selection, and
/// queue `select`. Also requests the host's scrollbar flash so
/// keyboard nav in an overflowing dock list stays oriented.
pub fn select_move(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    delta: i32,
    fx: &mut super::KeyFx,
) {
    let WidgetSpec::Tree {
        nodes, item_keys, ..
    } = spec
    else {
        return;
    };
    if nodes.is_empty() {
        return;
    }
    let Resolved {
        selected: cur_sel,
        expanded,
        ..
    } = resolve(spec, widget_key, &panel.instance_states);
    let visible_indices = collect_visible_tree_indices(nodes, item_keys, &expanded);
    if visible_indices.is_empty() {
        return;
    }
    let cur_pos = if cur_sel < 0 {
        if delta > 0 {
            -1
        } else {
            visible_indices.len() as i32
        }
    } else {
        visible_indices
            .iter()
            .position(|&v| v as i32 == cur_sel)
            .map(|p| p as i32)
            .unwrap_or(-1)
    };
    let new_pos = (cur_pos + delta).clamp(0, (visible_indices.len() as i32) - 1);
    let new_abs = visible_indices[new_pos as usize];
    let new_key = item_keys.get(new_abs).cloned().unwrap_or_default();
    panel.instance_states.insert(
        widget_key.to_string(),
        WidgetInstanceState::Tree {
            selected_index: new_abs as i32,
            expanded_keys: expanded,
            // Keyboard nav is a deliberate selection move —
            // re-arm scroll-follows-selection.
            user_scrolled: false,
        },
    );
    fx.flash_scrollbar = true;
    fx.events.push((
        "select".into(),
        json!({ "index": new_abs as i64, "key": new_key }),
    ));
}

/// Right/Left arrow.
///
/// * Right: if the selected node has children and is collapsed,
///   expand it. Else no-op.
/// * Left: if the selected node has children and is expanded,
///   collapse it. Else move selection up to the parent.
///
/// Updates host instance state and (when a change happened) queues
/// `expand` or `select`.
pub fn lateral(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    is_right: bool,
    fx: &mut super::KeyFx,
) {
    let WidgetSpec::Tree {
        nodes, item_keys, ..
    } = spec
    else {
        return;
    };
    if nodes.is_empty() {
        return;
    }
    let Resolved {
        selected: cur_sel,
        mut expanded,
        user_scrolled: cur_user_scrolled,
    } = resolve(spec, widget_key, &panel.instance_states);
    if cur_sel < 0 {
        return;
    }
    let sel_idx = cur_sel as usize;
    let Some(node) = nodes.get(sel_idx) else {
        return;
    };
    let key = item_keys.get(sel_idx).cloned().unwrap_or_default();
    let was_expanded = !key.is_empty() && expanded.contains(&key);

    let mut new_sel = cur_sel;
    let mut expansion_changed: Option<bool> = None; // Some(new_state)
    if is_right {
        if node.has_children && !was_expanded && !key.is_empty() {
            expanded.insert(key.clone());
            expansion_changed = Some(true);
        }
    } else if node.has_children && was_expanded && !key.is_empty() {
        expanded.remove(&key);
        expansion_changed = Some(false);
    } else if let Some(parent_idx) = crate::widgets::tree_parent_index(nodes, sel_idx) {
        new_sel = parent_idx as i32;
    }
    // No change → bail (don't fire spurious select/expand).
    if expansion_changed.is_none() && new_sel == cur_sel {
        return;
    }
    let final_key = item_keys.get(new_sel as usize).cloned().unwrap_or_default();
    panel.instance_states.insert(
        widget_key.to_string(),
        WidgetInstanceState::Tree {
            selected_index: new_sel,
            expanded_keys: expanded,
            // Jumping to the parent is a deliberate selection
            // move (re-arm follow); a pure expansion flip keeps
            // the user's scroll intact.
            user_scrolled: cur_user_scrolled && new_sel == cur_sel,
        },
    );
    if let Some(now_expanded) = expansion_changed {
        fx.events.push((
            "expand".into(),
            json!({
                "index": cur_sel as i64,
                "key": key,
                "expanded": now_expanded,
            }),
        ));
    } else if new_sel != cur_sel {
        fx.events.push((
            "select".into(),
            json!({
                "index": new_sel as i64,
                "key": final_key,
            }),
        ));
    }
}

/// The `activate` event for the currently-selected node, if any.
/// Shared by Enter in [`Tree::on_key`] and the panel-level picker
/// forwarding — the plugin's handler decides what "activate" means
/// (open the file, run an action, etc.).
pub fn activate_event(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &crate::widgets::WidgetPanelState,
) -> Option<(String, serde_json::Value)> {
    let WidgetSpec::Tree { item_keys, .. } = spec else {
        return None;
    };
    let sel = resolve(spec, widget_key, &panel.instance_states).selected;
    if sel < 0 {
        return None;
    }
    let item_key = item_keys.get(sel as usize).cloned().unwrap_or_default();
    Some(("activate".into(), json!({ "index": sel, "key": item_key, })))
}

/// If the focused row is checkable (parent tree has `checkable:
/// true` *and* the row's `checked` is `Some(_)`), the `toggle` event
/// with the inverted value — mirroring what a click on the row's
/// `[v]`/`[ ]` glyph would do. `None` lets the caller fall back to
/// `activate`.
fn toggle_if_checkable_event(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &crate::widgets::WidgetPanelState,
) -> Option<(String, serde_json::Value)> {
    let WidgetSpec::Tree {
        nodes,
        item_keys,
        checkable,
        ..
    } = spec
    else {
        return None;
    };
    if !checkable {
        return None;
    }
    let sel = resolve(spec, widget_key, &panel.instance_states).selected;
    if sel < 0 {
        return None;
    }
    // No checkbox glyph on this row — let activate fire.
    let cur_checked = nodes.get(sel as usize).and_then(|n| n.checked)?;
    let new_checked = !cur_checked;
    let item_key = item_keys.get(sel as usize).cloned().unwrap_or_default();
    Some((
        "toggle".into(),
        json!({ "index": sel, "key": item_key, "checked": new_checked, }),
    ))
}

/// Indices of the tree nodes visible under the current expansion set:
/// a node shows iff every ancestor on its depth path is expanded.
/// Shared by the renderer-side wheel bound and the app-side selection
/// movement / paging (`app/widget_runtime.rs`).
pub fn collect_visible_tree_indices(
    nodes: &[fresh_core::api::TreeNode],
    item_keys: &[String],
    expanded: &std::collections::HashSet<String>,
) -> Vec<usize> {
    let mut ancestor_open: Vec<bool> = Vec::new();
    let mut visible: Vec<usize> = Vec::with_capacity(nodes.len());
    for (i, node) in nodes.iter().enumerate() {
        let depth = node.depth as usize;
        ancestor_open.truncate(depth);
        if ancestor_open.iter().all(|open| *open) {
            visible.push(i);
        }
        let key = item_keys.get(i).cloned().unwrap_or_default();
        let is_open = if node.has_children {
            !key.is_empty() && expanded.contains(&key)
        } else {
            true
        };
        ancestor_open.push(is_open);
    }
    visible
}
