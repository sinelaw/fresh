//! `Tree` — disclosure tree with optional checkboxes and card density.

use std::collections::{HashMap, HashSet};

use fresh_core::api::{OverlayColorSpec, TreeNode, WidgetSpec};
use fresh_core::text_property::TextPropertyEntry;
use serde_json::json;

use super::WidgetImpl;
use crate::widgets::registry::{HitArea, PaintedWindow, WidgetInstanceState};
use crate::widgets::render::{
    apply_hover_band, ensure_trailing_newline, mark_list_card_selected, render_tree_row,
    tree_max_scroll, tree_node_is_card, tree_node_rows, CollectedOutput, RenderContext,
    KEY_FOCUSED_BG,
};

pub struct Tree;

/// Columns one pan keystroke moves.
///
/// `less(1)`'s left/right step, which is the closest thing to a convention a
/// terminal has for panning by key — and it has to be wider than the wheel's
/// three-column notch, because a wheel is turned in handfuls and a key is
/// pressed one press at a time. The extremes have `S-Home` / `S-End`, so this
/// only has to be comfortable for reading around a match.
const PAN_COLUMNS: i32 = 8;

impl WidgetImpl for Tree {
    fn on_wheel(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        viewport: super::Viewport,
        delta: i32,
    ) -> bool {
        let WidgetSpec::Tree {
            item_height,
            card_borders,
            checkable,
            nodes,
            item_keys,
            ..
        } = spec
        else {
            return false;
        };
        if nodes.is_empty() {
            return false;
        }
        let item_height = (*item_height).max(1);
        let expanded = resolve(spec, widget_key, &panel.instance_states).expanded;
        let visible_indices = collect_visible_tree_indices(nodes, item_keys, &expanded);
        if visible_indices.is_empty() {
            return false;
        }
        // **A Tree's offset counts rows, not nodes** — line-level
        // scrolling, so a bordered card can be partially clipped at the
        // viewport edges — which is why the bound is computed against
        // `viewport.rows` and not the item window beside it. Compute
        // per-node heights and the clamp with the renderer's own helpers
        // so the wheel can't disagree with what will actually be
        // painted. Mirror the renderer's normalization: bordered-card
        // layout only engages for multi-row items.
        let card_borders = *card_borders && item_height > 1;
        let heights: Vec<u32> = visible_indices
            .iter()
            .map(|&abs| {
                crate::widgets::render::tree_node_rows(
                    &nodes[abs],
                    *checkable,
                    item_height,
                    card_borders,
                )
            })
            .collect();
        let max_scroll = crate::widgets::render::tree_max_scroll(&heights, viewport.rows);
        let cur_scroll = panel.painted.get(widget_key).map(|w| w.offset).unwrap_or(0);
        let new_scroll = (cur_scroll as i32 + delta).clamp(0, max_scroll as i32) as u32;
        if new_scroll == cur_scroll {
            return false;
        }
        // Mouse scroll moves the *view* only — the selection stays put
        // (and may scroll out of view). `user_scrolled` tells the
        // renderer not to snap the offset back to the selection, and it
        // survives a plugin `SetSelectedIndex` that re-pins the same
        // selection. The offset is the painter's window; the latch is
        // the tree's own fold.
        panel.window_mut(widget_key, viewport).offset = new_scroll;
        panel.latch_user_scrolled(widget_key);
        true
    }

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
                let delta = match key {
                    "S-Left" => Some(-PAN_COLUMNS),
                    "S-Right" => Some(PAN_COLUMNS),
                    // Home is where each row's content says it should rest —
                    // its own match — not the head of the line. The head is a
                    // few more `S-Left`s away, and a reader who wants the
                    // match back should not have to pan to find it.
                    "S-Home" => None,
                    // Far enough that the per-row clamp lands every row on
                    // its own tail; `pan_h` bounds the stored value.
                    _ => Some(i32::MAX / 4),
                };
                if !panel.pan_h(widget_key, delta) {
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

    fn collect(
        &self,
        spec: &WidgetSpec,
        prev: &HashMap<String, WidgetInstanceState>,
        next_state: &mut HashMap<String, WidgetInstanceState>,
        ctx: RenderContext<'_>,
        panel_width: u32,
    ) -> CollectedOutput {
        let WidgetSpec::Tree {
            nodes,
            item_keys,
            selected_index,
            visible_rows,
            expanded_keys,
            checkable,
            item_height,
            card_borders,
            indent_cols,
            key: tree_key,
        } = spec
        else {
            return CollectedOutput::default();
        };
        render_widget_tree(
            nodes,
            item_keys,
            *selected_index,
            *visible_rows,
            expanded_keys,
            *checkable,
            *item_height,
            *card_borders,
            *indent_cols,
            tree_key.as_deref(),
            prev,
            next_state,
            ctx,
            panel_width,
        )
    }
}

/// A `Tree`'s state, once the spec and the instance map have been
/// reconciled. As with [`crate::widgets::kinds::list::Resolved`], the
/// window is absent: that is the painter's, and lives in
/// [`crate::widgets::PaintedWindow`].
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

#[allow(clippy::too_many_arguments)]
fn render_widget_tree(
    nodes: &[TreeNode],
    item_keys: &[String],
    selected_index: i32,
    spec_visible_rows: Option<u32>,
    expanded_keys: &[String],
    checkable: bool,
    item_height: u32,
    card_borders: bool,
    indent_cols: u32,
    tree_key: Option<&str>,
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    ctx: RenderContext<'_>,
    panel_width: u32,
) -> CollectedOutput {
    let mut out = CollectedOutput::default();
    // Resolve the row window: explicit spec value pins it; omitted
    // auto-sizes from the host's height budget; no budget → legacy
    // default, flagged for `collect_col`'s fill pass. Same protocol
    // as `collect_list`.
    let visible_rows = match (spec_visible_rows, ctx.avail_height) {
        (Some(v), _) => v,
        (None, Some(budget)) => budget.max(1),
        (None, None) => {
            out.wants_fill = true;
            fresh_core::api::LEGACY_VISIBLE_ROWS_FALLBACK
        }
    };
    // Fixed rows per node. `1` is the classic single-line tree; a
    // larger value renders every node as a card of this many rows.
    // Windowing/scroll stay in *node* units so single-line trees (the
    // vast majority) are wholly unaffected. With `card_borders`, rows
    // per node vary: card nodes take `item_height + 2` (border rows),
    // non-card nodes a single row — see `tree_node_rows`.
    let item_height = item_height.max(1);
    let card_borders = card_borders && item_height > 1;
    // The selection and the expanded set are the tree's own, through
    // the one resolver every reader uses; the spec is their seed until
    // a handler decides otherwise.
    let Resolved {
        selected: prev_sel,
        expanded: prev_expanded,
        user_scrolled,
    } = resolve_seeded(selected_index, expanded_keys, tree_key.unwrap_or(""), prev);
    // The offset is the *last paint's*, not the tree's: the scroll fold
    // reads back its own previous value and republishes it below.
    let prev_scroll = ctx.painted(tree_key).map(|w| w.offset).unwrap_or(0);
    // Sideways is the reader's, not the paint's — one delta for the whole
    // tree, clamped per row against that row's own length so rows of
    // different lengths slide together rather than drifting apart.
    let h_pan = ctx.h_pan(tree_key);

    // Compute the visible (un-collapsed) flat slice of the
    // full `nodes` list. A node at depth d is visible iff
    // every ancestor (the most recent earlier node at depth
    // d-1, that node's most recent earlier at d-2, etc.) is
    // expanded. Walk linearly tracking ancestor expansion at
    // each depth — set ancestor[d] = is_expanded(node) when
    // we visit a node at depth d, and consider a node
    // visible iff ancestor[0..node.depth] are all true.
    //
    // O(N * max_depth) — fine; trees in this editor are
    // shallow (filesystem trees, search-results trees).
    let mut ancestor_open: Vec<bool> = Vec::new();
    let mut visible_indices: Vec<usize> = Vec::with_capacity(nodes.len());
    for (i, node) in nodes.iter().enumerate() {
        let depth = node.depth as usize;
        // Truncate the ancestor stack to this node's depth.
        ancestor_open.truncate(depth);
        let visible = ancestor_open.iter().all(|open| *open);
        if visible {
            visible_indices.push(i);
        }
        // Push this node's own openness onto the stack so
        // descendants see it. The node is "open" iff it has
        // children AND its key is in expanded_keys; leaves
        // act like open nodes (their nonexistent descendants
        // can't be hidden anyway).
        let key = item_keys.get(i).cloned().unwrap_or_default();
        let is_open = if node.has_children {
            !key.is_empty() && prev_expanded.contains(&key)
        } else {
            true
        };
        ancestor_open.push(is_open);
    }

    // Clamp the previous selection to a visible index. The
    // selected_index in the spec/instance state references
    // the *absolute* `nodes` index; if that node is now
    // hidden (parent collapsed), find the closest visible
    // node at-or-before it. If no visible nodes, -1.
    // Rows each visible node occupies. Without `card_borders` every
    // node is a fixed `item_height`-row band, so all the node-unit
    // scroll math below reduces to the original behaviour exactly.
    // With it, card nodes grow two border rows and non-card nodes
    // (folder headers) shrink to one row.
    let heights: Vec<u32> = visible_indices
        .iter()
        .map(|&abs| tree_node_rows(&nodes[abs], checkable, item_height, card_borders))
        .collect();
    let clamp_to_visible = |abs: i32| -> i32 {
        if abs < 0 || nodes.is_empty() {
            return -1;
        }
        let abs = abs.min((nodes.len() as i32) - 1) as usize;
        if let Ok(_pos) = visible_indices.binary_search(&abs) {
            return abs as i32;
        }
        // Not visible — fall back to the nearest earlier
        // visible node, else the first visible node, else -1.
        let earlier = visible_indices.iter().rev().find(|&&v| v <= abs);
        if let Some(&v) = earlier {
            return v as i32;
        }
        visible_indices.first().map(|&v| v as i32).unwrap_or(-1)
    };
    let effective_sel_abs = clamp_to_visible(prev_sel);
    // Find the position of the selected absolute index in
    // visible_indices — that's its "visible-window position"
    // used for scroll math.
    let sel_visible_pos: i32 = if effective_sel_abs < 0 {
        -1
    } else {
        visible_indices
            .iter()
            .position(|&v| v == effective_sel_abs as usize)
            .map(|p| p as i32)
            .unwrap_or(-1)
    };

    // Compute scroll. The offset is in *rows* into the flattened row
    // list of the visible (un-collapsed) nodes — not node units — so the
    // wheel scrolls line by line and a tall bordered card can sit
    // partially clipped at either viewport edge. For uniform single-row
    // trees rows and nodes coincide, so the classic paths are unchanged.
    //
    // Once the user has scrolled by mouse (`user_scrolled`), respect
    // the stored offset as-is — the selected node may sit off-screen.
    // Selection moves (keyboard/click/plugin) clear the flag, re-arming
    // keep-selection-visible. Same contract as the List path.
    let row_starts: Vec<u32> = heights
        .iter()
        .scan(0u32, |acc, &h| {
            let start = *acc;
            *acc += h;
            Some(start)
        })
        .collect();
    let total_rows: u32 = heights.iter().sum();
    let mut scroll = prev_scroll;
    if sel_visible_pos >= 0 && !user_scrolled {
        let sel = sel_visible_pos as usize;
        let sel_start = row_starts[sel];
        let sel_end = sel_start + heights[sel];
        if sel_start < scroll {
            scroll = sel_start;
        } else if sel_end > scroll + visible_rows {
            // Scroll just enough that the whole selected node shows; a
            // node taller than the viewport anchors to its top row.
            scroll = sel_end.saturating_sub(visible_rows).min(sel_start);
        }
    }
    let max_scroll = tree_max_scroll(&heights, visible_rows);
    if scroll > max_scroll {
        scroll = max_scroll;
    }

    // **The walk carries this widget's state; it does not decide it.**
    // Same contract as `collect_list` and `collect_dropdown`: the
    // clamped selection is a derivation [`resolve`] reapplies on every
    // read, the offset is the paint's window and leaves below under
    // that name, and an untouched tree contributes no entry at all —
    // while a stored one has to survive, because `update_side_effects`
    // replaces the whole map.
    if let Some(k) = tree_key.filter(|k| !k.is_empty()) {
        if let Some(stored) = prev.get(k) {
            next_state.insert(k.to_string(), stored.clone());
        }
        // The window this paint used. `items` is the node budget the
        // pager moves in — the row budget divided by the rows one node
        // occupies, which for bordered cards of unequal height is the
        // conservative estimate paging has always used.
        let per_node = if card_borders {
            item_height + 2
        } else {
            item_height
        };
        out.painted.insert(
            k.to_string(),
            PaintedWindow {
                rows: visible_rows,
                items: visible_rows / per_node.max(1),
                offset: scroll,
            },
        );
    }

    // Render the visible window: rows `[scroll, scroll + budget)`.
    // Nodes straddling either edge are emitted and then clipped to the
    // window, so a card can be partially visible at the top and bottom.
    let budget = visible_rows.max(1);
    let start_node = row_starts
        .partition_point(|&s| s <= scroll)
        .saturating_sub(1);
    let mut rows_emitted: u32 = 0;
    for (vis_pos, &abs_idx) in visible_indices.iter().enumerate().skip(start_node) {
        if rows_emitted >= budget {
            break;
        }
        // Rows of this node hidden above the window (>0 only for the
        // first node, when `scroll` lands inside it).
        let clip_top = scroll.saturating_sub(row_starts[vis_pos]) as usize;
        let entries_before = out.entries.len();
        let hits_before = out.hits.len();
        // Apply pad/truncate hints and convert any char-unit
        // overlays to byte offsets *before* the disclosure
        // prefix is prepended; render_tree_row then byte-shifts
        // the (now byte-unit) overlays uniformly.
        let mut node = nodes[abs_idx].clone();
        node.text.normalize_widths();
        for line in node.extra_lines.iter_mut() {
            line.normalize_widths();
        }
        let item_key = item_keys.get(abs_idx).cloned().unwrap_or_default();
        let is_expanded =
            node.has_children && !item_key.is_empty() && prev_expanded.contains(&item_key);
        let rendered = render_tree_row(
            &node,
            is_expanded,
            checkable,
            item_height,
            card_borders,
            panel_width,
            indent_cols,
            h_pan,
        );
        let mut entry = rendered.entry;
        let is_selected = abs_idx as i32 == effective_sel_abs;
        // Bordered-card nodes mark selection the way the pre-tree card
        // list did — a heavy box frame via `mark_list_card_selected`,
        // no background band (it reads garish over a multi-row card).
        // The heavy glyphs double as the marker
        // `paint_dock_seamless_active_tab` keys on to merge the active
        // dock card into the editor, so a bg-only highlight here would
        // (and once did — issue seen after the folder-tree redesign)
        // silently lose that seamless-tab treatment.
        let as_card = card_borders && tree_node_is_card(&node, checkable);
        // Non-card rows: a highlight band filling the whole row.
        let select_style = |e: &mut TextPropertyEntry| {
            let mut style = e.style.clone().unwrap_or_default();
            style.bg = Some(OverlayColorSpec::theme_key(KEY_FOCUSED_BG));
            style.extend_to_line_end = true;
            e.style = Some(style);
        };
        let mark_selected = |e: &mut TextPropertyEntry| {
            if as_card {
                mark_list_card_selected(e);
            } else {
                select_style(e);
            }
        };
        // The pointer highlights the row it's on — including every
        // continuation row of a card, which selects as one unit and so
        // must light as one. Selection outranks hover: a selected row
        // keeps its stronger band (or its card frame) rather than being
        // repainted the moment the pointer crosses it.
        let is_hovered_row = !is_selected && ctx.is_row_hovered(tree_key, &item_key);
        if is_selected {
            mark_selected(&mut entry);
        } else if is_hovered_row {
            apply_hover_band(&mut entry);
        }
        let row_byte_end = entry.text.len();
        ensure_trailing_newline(&mut entry);
        out.entries.push(entry);
        let hit_row = (out.entries.len() - 1) as u32;
        // Tree hits use the *tree's* spec key for `widget_key` (so
        // click-to-focus works the same as Toggle/Button — the tree is
        // tabbable). The per-row key travels in the payload.
        let tree_spec_key = tree_key.unwrap_or("").to_string();
        // Continuation rows of a card (item_height > 1). The primary row
        // owns expand/toggle, but every continuation row carries its own
        // `select` hit — a card selects as a unit, so clicking its branch
        // or PR line must behave like clicking its title line (the web
        // renderer already treats the whole card as one click target).
        // They also take the selection highlight so the card highlights
        // as a block.
        for mut extra in rendered.extra_entries {
            if is_selected {
                mark_selected(&mut extra);
            } else if is_hovered_row {
                apply_hover_band(&mut extra);
            }
            let extra_byte_end = extra.text.len();
            ensure_trailing_newline(&mut extra);
            out.entries.push(extra);
            if extra_byte_end > 0 {
                out.hits.push(HitArea {
                    overlay: false,
                    buffer_row: (out.entries.len() - 1) as u32,
                    byte_start: 0,
                    byte_end: extra_byte_end,
                    event: crate::widgets::WidgetEvent {
                        row_target: true,
                        context_click: true,
                        widget_key: tree_spec_key.clone(),
                        widget_kind: "tree",
                        payload: json!({
                            "index": abs_idx as i64,
                            "key": item_key.clone(),
                        }),
                        event_type: "select",
                        owner_key: None,
                    },
                });
            }
        }
        // Disclosure hit (only when has_children) — fires
        // `expand`. The host toggles instance-state
        // `expanded_keys` and re-renders before firing the
        // event; the plugin only listens if it cares about
        // expansion changes.
        if let Some(disc_range) = rendered.disclosure_range {
            out.hits.push(HitArea {
                overlay: false,
                buffer_row: hit_row,
                byte_start: disc_range.0,
                byte_end: disc_range.1,
                event: crate::widgets::WidgetEvent {
                    row_target: false,
                    context_click: false,
                    widget_key: tree_spec_key.clone(),
                    widget_kind: "tree",
                    payload: json!({
                        "index": abs_idx as i64,
                        "key": item_key.clone(),
                        "expanded": !is_expanded,
                    }),
                    event_type: "expand",
                    owner_key: None,
                },
            });
        }
        // Checkbox hit (when the parent Tree is checkable
        // *and* this node has Some(_) checked) — fires
        // `toggle` with the *new* checked value. The host
        // does not mutate the spec; the plugin owns the
        // truth and pushes the new state back via
        // `WidgetMutation::SetCheckedKeys`.
        if let Some(cb_range) = rendered.checkbox_range {
            let new_checked = !nodes[abs_idx].checked.unwrap_or(false);
            out.hits.push(HitArea {
                overlay: false,
                buffer_row: hit_row,
                byte_start: cb_range.0,
                byte_end: cb_range.1,
                event: crate::widgets::WidgetEvent {
                    row_target: false,
                    context_click: false,
                    widget_key: tree_spec_key.clone(),
                    widget_kind: "tree",
                    payload: json!({
                        "index": abs_idx as i64,
                        "key": item_key.clone(),
                        "checked": new_checked,
                    }),
                    event_type: "toggle",
                    owner_key: None,
                },
            });
        }
        // Row body hit — fires `select`. Spans whatever's
        // left of the row text after the disclosure +
        // checkbox prefix.
        let body_start = match (rendered.checkbox_range, rendered.disclosure_range) {
            (Some((_, end)), _) => end + 1, // +1 for the trailing space after [v]
            (None, Some((_, end))) => end,
            (None, None) => 0,
        };
        if body_start < row_byte_end {
            out.hits.push(HitArea {
                overlay: false,
                buffer_row: hit_row,
                byte_start: body_start,
                byte_end: row_byte_end,
                event: crate::widgets::WidgetEvent {
                    row_target: true,
                    context_click: true,
                    widget_key: tree_spec_key.clone(),
                    widget_kind: "tree",
                    payload: json!({
                        "index": abs_idx as i64,
                        "key": item_key.clone(),
                    }),
                    event_type: "select",
                    owner_key: None,
                },
            });
        }

        // Clip this node's rows to the viewport window: drop `clip_top`
        // rows hidden above it and anything past the remaining budget
        // below, shifting the surviving rows' hits up accordingly and
        // discarding hits whose row was clipped away (a hidden
        // disclosure glyph must not stay clickable).
        let node_rows = out.entries.len() - entries_before;
        let keep_from = entries_before + clip_top.min(node_rows);
        let remaining = (budget - rows_emitted) as usize;
        let keep_to = (keep_from + remaining).min(out.entries.len());
        if keep_from > entries_before || keep_to < out.entries.len() {
            let kept: Vec<TextPropertyEntry> = out
                .entries
                .drain(entries_before..)
                .enumerate()
                .filter_map(|(i, e)| {
                    let row = entries_before + i;
                    (row >= keep_from && row < keep_to).then_some(e)
                })
                .collect();
            out.entries.extend(kept);
            let clip = (keep_from - entries_before) as u32;
            let kept_hits: Vec<HitArea> = out
                .hits
                .drain(hits_before..)
                .filter_map(|mut h| {
                    let row = h.buffer_row as usize;
                    if row >= keep_from && row < keep_to {
                        h.buffer_row -= clip;
                        Some(h)
                    } else {
                        None
                    }
                })
                .collect();
            out.hits.extend(kept_hits);
        }
        rows_emitted += (out.entries.len() - entries_before) as u32;
    }

    // Surface a scroll region so the host paints a draggable overlay
    // scrollbar when the tree overflows — mirroring the List path, so the
    // dock's session tree gets the same hover scrollbar the card list had.
    // Emitted whenever the tree is keyed (not only on overflow) so wheel
    // routing can hit-test the pointer against the tree's geometry too.
    // Totals are in rows (matching the row-based scroll offset), so the
    // thumb size/position track line-level scrolling exactly.
    if tree_key.filter(|k| !k.is_empty()).is_some() {
        out.self_scroll = Some(crate::widgets::layout_box::BoxScroll {
            total: total_rows as usize,
            visible: rows_emitted as usize,
            offset: scroll as usize,
        });
    }

    out
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
