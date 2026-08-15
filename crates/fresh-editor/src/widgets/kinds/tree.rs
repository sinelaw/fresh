//! `Tree` — disclosure tree with optional checkboxes and card density.

use std::collections::{HashMap, HashSet};

use fresh_core::api::{OverlayColorSpec, TreeNode, WidgetSpec};
use fresh_core::text_property::TextPropertyEntry;
use serde_json::json;

use super::WidgetImpl;
use crate::widgets::registry::{HitArea, WidgetInstanceState};
use crate::widgets::render::{
    apply_hover_band, ensure_trailing_newline, mark_list_card_selected, render_tree_row,
    tree_max_scroll, tree_node_is_card, tree_node_rows, CollectedOutput, RenderContext,
    ScrollRegion, KEY_FOCUSED_BG,
};

pub(crate) struct Tree;

impl WidgetImpl for Tree {
    fn on_wheel(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        delta: i32,
    ) -> bool {
        let WidgetSpec::Tree {
            visible_rows,
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
        let visible_rows = panel.effective_visible_rows(widget_key, *visible_rows);
        let item_height = (*item_height).max(1);
        let (cur_sel, cur_scroll, expanded) = match panel.instance_states.get(widget_key) {
            Some(WidgetInstanceState::Tree {
                selected_index,
                scroll_offset,
                expanded_keys,
                ..
            }) => (*selected_index, *scroll_offset, expanded_keys.clone()),
            _ => (-1, 0, std::collections::HashSet::<String>::new()),
        };
        let visible_indices = collect_visible_tree_indices(nodes, item_keys, &expanded);
        if visible_indices.is_empty() {
            return false;
        }
        // Scroll offset and clamp are in *row* units (line-level
        // scrolling — a bordered card can be partially clipped at the
        // viewport edges). Compute per-node heights and the clamp with
        // the renderer's own helpers so the wheel can't disagree with
        // what will actually be painted. Mirror the renderer's
        // normalization: bordered-card layout only engages for
        // multi-row items.
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
        let max_scroll = crate::widgets::render::tree_max_scroll(&heights, visible_rows);
        let new_scroll = (cur_scroll as i32 + delta).clamp(0, max_scroll as i32) as u32;
        if new_scroll == cur_scroll {
            return false;
        }
        // Mouse scroll moves the *view* only — the selection stays put
        // (and may scroll out of view). `user_scrolled` tells the
        // renderer not to snap the offset back to the selection, and it
        // survives a plugin `SetSelectedIndex` that re-pins the same
        // selection.
        panel.instance_states.insert(
            widget_key.to_string(),
            WidgetInstanceState::Tree {
                scroll_offset: new_scroll,
                selected_index: cur_sel,
                expanded_keys: expanded,
                user_scrolled: true,
            },
        );
        true
    }

    fn box_meta(&self, spec: &WidgetSpec) -> super::BoxMeta {
        let mut m = super::BoxMeta::plain("tree");
        if let WidgetSpec::Tree { key: Some(k), .. } = spec {
            if !k.is_empty() {
                m.key = Some(k.clone());
                m.focusable = true;
                m.scrollable = true;
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
    if let Some(k) = tree_key {
        if !k.is_empty() {
            out.effective_rows.insert(k.to_string(), visible_rows);
        }
    }
    // Fixed rows per node. `1` is the classic single-line tree; a
    // larger value renders every node as a card of this many rows.
    // Windowing/scroll stay in *node* units so single-line trees (the
    // vast majority) are wholly unaffected. With `card_borders`, rows
    // per node vary: card nodes take `item_height + 2` (border rows),
    // non-card nodes a single row — see `tree_node_rows`.
    let item_height = item_height.max(1);
    let card_borders = card_borders && item_height > 1;
    // Look up host-owned instance state (scroll, selection,
    // expanded set). Spec values are initial-only.
    let prev_state = tree_key.filter(|k| !k.is_empty()).and_then(|k| prev.get(k));
    let (prev_scroll, prev_sel, prev_expanded, user_scrolled) = match prev_state {
        Some(WidgetInstanceState::Tree {
            scroll_offset,
            selected_index,
            expanded_keys,
            user_scrolled,
        }) => (
            *scroll_offset,
            *selected_index,
            expanded_keys.clone(),
            *user_scrolled,
        ),
        _ => {
            // First render: seed expanded_keys from spec.
            let seeded: HashSet<String> = expanded_keys.iter().cloned().collect();
            (0, selected_index, seeded, false)
        }
    };

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

    // Persist instance state.
    if let Some(k) = tree_key.filter(|k| !k.is_empty()) {
        next_state.insert(
            k.to_string(),
            WidgetInstanceState::Tree {
                scroll_offset: scroll,
                selected_index: effective_sel_abs,
                expanded_keys: prev_expanded.clone(),
                user_scrolled,
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
                    widget_key: tree_spec_key.clone(),
                    widget_kind: "tree",
                    buffer_row: (out.entries.len() - 1) as u32,
                    byte_start: 0,
                    byte_end: extra_byte_end,
                    payload: json!({
                        "index": abs_idx as i64,
                        "key": item_key.clone(),
                    }),
                    event_type: "select",
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
                widget_key: tree_spec_key.clone(),
                widget_kind: "tree",
                buffer_row: hit_row,
                byte_start: disc_range.0,
                byte_end: disc_range.1,
                payload: json!({
                    "index": abs_idx as i64,
                    "key": item_key.clone(),
                    "expanded": !is_expanded,
                }),
                event_type: "expand",
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
                widget_key: tree_spec_key.clone(),
                widget_kind: "tree",
                buffer_row: hit_row,
                byte_start: cb_range.0,
                byte_end: cb_range.1,
                payload: json!({
                    "index": abs_idx as i64,
                    "key": item_key.clone(),
                    "checked": new_checked,
                }),
                event_type: "toggle",
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
                widget_key: tree_spec_key.clone(),
                widget_kind: "tree",
                buffer_row: hit_row,
                byte_start: body_start,
                byte_end: row_byte_end,
                payload: json!({
                    "index": abs_idx as i64,
                    "key": item_key.clone(),
                }),
                event_type: "select",
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
    if let Some(k) = tree_key.filter(|k| !k.is_empty()) {
        out.scroll_regions.push(ScrollRegion {
            list_key: k.to_string(),
            buffer_row: 0,
            col_in_row: 0,
            width_cols: panel_width,
            height_rows: rows_emitted,
            total: total_rows as usize,
            visible: rows_emitted as usize,
            scroll: scroll as usize,
        });
    }

    out
}

/// Indices of the tree nodes visible under the current expansion set:
/// a node shows iff every ancestor on its depth path is expanded.
/// Shared by the renderer-side wheel bound and the app-side selection
/// movement / paging (`app/widget_runtime.rs`).
pub(crate) fn collect_visible_tree_indices(
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
