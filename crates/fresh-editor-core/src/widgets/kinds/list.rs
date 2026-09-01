//! `List` — virtual-scrolled select list (classic rows or card items).

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;
use fresh_core::text_property::TextPropertyEntry;
use serde_json::json;

use super::WidgetImpl;
use crate::widgets::registry::{HitArea, PaintedWindow, WidgetInstanceState};
use crate::widgets::render::{
    apply_hover_band, blank_list_row, ensure_trailing_newline, mark_list_card_selected,
    mark_list_row_selected, render_collected, CollectedOutput, RenderContext,
};

pub struct List;

impl WidgetImpl for List {
    fn on_wheel(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        viewport: super::Viewport,
        delta: i32,
    ) -> bool {
        let total = total_items(spec);
        if total == 0 {
            return false;
        }
        // The window arrives in items already — a `List`'s offset counts
        // items, so the row-to-item division that used to happen here is
        // the resolver's, once, for every seam that needs it. A list that
        // already shows everything (max_scroll == 0, e.g. the Git Log,
        // which sets visible_rows == commit count and scrolls via its
        // enclosing pane) still reports "can't scroll" and lets the wheel
        // bubble to that pane rather than swallowing it.
        let visible_items = viewport.items.max(1);
        let max_scroll = total.saturating_sub(visible_items);
        let cur_scroll = panel.painted.get(widget_key).map(|w| w.offset).unwrap_or(0);
        let new_scroll = (cur_scroll as i64 + delta as i64).clamp(0, max_scroll as i64) as u32;
        if new_scroll == cur_scroll {
            return false;
        }
        // Wheel scrolls the *view* only — the selection stays put (and
        // may leave the visible window); `user_scrolled` tells the
        // renderer not to snap the offset back to it. The offset is the
        // painter's window and the latch is the widget's own fold, so
        // the notch writes one of each.
        panel.window_mut(widget_key, viewport).offset = new_scroll;
        panel.latch_user_scrolled(widget_key);
        true
    }

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
        key: &str,
        fx: &mut super::KeyFx,
    ) -> super::KeyDisposition {
        match key {
            "Up" | "Down" => {
                let delta = if key == "Up" { -1 } else { 1 };
                select_move(spec, widget_key, panel, delta, fx);
            }
            "PageUp" | "PageDown" => {
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
                let delta = if key == "PageUp" { -page } else { page };
                select_move(spec, widget_key, panel, delta, fx);
            }
            "Enter" | "Space" => {
                if let Some(ev) = activate_event(spec, widget_key, panel) {
                    fx.events.push(ev);
                }
            }
            _ => return super::KeyDisposition::Pass,
        }
        super::KeyDisposition::Consumed
    }
    /// Pointer model: a row click syncs the host-owned selection to
    /// the clicked index — preserving scroll, re-arming
    /// scroll-follows-selection (a deliberate selection snaps a
    /// scrolled-away view back) — then lets the recorded `select`
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
                        user_scrolled: false,
                    },
                );
            }
        }
        super::PointerDisposition::Default
    }

    fn collect(
        &self,
        spec: &WidgetSpec,
        prev: &HashMap<String, WidgetInstanceState>,
        next_state: &mut HashMap<String, WidgetInstanceState>,
        ctx: RenderContext<'_>,
        panel_width: u32,
    ) -> CollectedOutput {
        let WidgetSpec::List {
            items,
            item_specs,
            item_keys,
            selected_index,
            visible_rows,
            key: list_key,
            ..
        } = spec
        else {
            return CollectedOutput::default();
        };
        collect_list(
            items,
            item_specs,
            item_keys,
            *selected_index,
            *visible_rows,
            list_key.as_deref(),
            prev,
            next_state,
            ctx,
            panel_width,
        )
    }
}

/// A `List`'s state, once the spec and the instance map have been
/// reconciled. The window is deliberately absent: that is the painter's,
/// and [`crate::widgets::PaintedWindow`] is where it lives.
pub struct Resolved {
    /// The selection clamped into the current dataset, or `-1` for none
    /// (an empty list, or a list nobody has selected in).
    pub selected: i32,
    /// Whether the user has taken the window off the selection by mouse.
    pub user_scrolled: bool,
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
    let (stored, user_scrolled) = match key.filter(|k| !k.is_empty()).and_then(|k| prev.get(k)) {
        Some(WidgetInstanceState::List {
            selected_index,
            user_scrolled,
        }) => (*selected_index, *user_scrolled),
        _ => (spec_selected, false),
    };
    Resolved {
        selected: clamp_selection(stored, total),
        user_scrolled,
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
/// range), re-arming scroll-follows-selection, and queue `select` —
/// but only when the index actually moved: a clamped move at the
/// list's top/bottom edge still repaints (re-arming `user_scrolled`
/// snaps a scrolled-away view back to the selection) but must not
/// spam the plugin with same-index selections — each one re-runs the
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
            // Keyboard nav re-arms scroll-follows-selection so the
            // renderer brings the new selection back into view.
            user_scrolled: false,
        },
    );
    if new_sel != cur_sel {
        fx.events
            .push(("select".into(), json!({ "index": new_sel, "key": new_key })));
    }
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

/// Pre-render every card item-spec into its own block of entries at the
/// given column width. Returns the rendered cards alongside the uniform
/// card height (the tallest card, minimum 1) that the list's selection
/// and scroll math are expressed in.
fn render_list_cards(
    item_specs: &[WidgetSpec],
    prev: &HashMap<String, WidgetInstanceState>,
    ctx: RenderContext<'_>,
    width: u32,
) -> (Vec<Vec<TextPropertyEntry>>, u32) {
    let mut rendered_cards: Vec<Vec<TextPropertyEntry>> = Vec::with_capacity(item_specs.len());
    let mut item_height: u32 = 1;
    for item_spec in item_specs.iter() {
        let mut scratch = HashMap::new();
        let card_entries = render_collected(item_spec, prev, &mut scratch, ctx, width).entries;
        item_height = item_height.max((card_entries.len() as u32).max(1));
        rendered_cards.push(card_entries);
    }
    (rendered_cards, item_height)
}

/// Resolved geometry for one [`collect_list`] render: where the
/// scroll window sits and how items map to rows. All selection /
/// scroll values are in *item* units; the card path maps each item
/// to a fixed band of `item_height` rows.
struct ListLayout {
    /// Total number of items (classic rows or card specs).
    total: u32,
    /// Selection clamped to the current dataset (-1 when none / empty).
    effective_sel: i32,
    /// First visible item index.
    scroll: u32,
    /// Number of items that fit in the available height.
    visible_items: u32,
    /// Uniform card height in rows (1 for the classic path).
    item_height: u32,
    /// Pre-rendered card blocks (empty for the classic path).
    rendered_cards: Vec<Vec<TextPropertyEntry>>,
}

/// Resolve the prior host-owned scroll/selection state, (re-)render
/// any card blocks, and compute the visible window for a List. Pure
/// bookkeeping — it neither emits rows nor persists state.
#[allow(clippy::too_many_arguments)]
fn plan_list_layout(
    items_len: usize,
    item_specs: &[WidgetSpec],
    selected_index: i32,
    visible_rows: u32,
    list_key: Option<&str>,
    prev: &HashMap<String, WidgetInstanceState>,
    ctx: RenderContext<'_>,
    panel_width: u32,
) -> ListLayout {
    let use_specs = !item_specs.is_empty();
    let total = if use_specs {
        item_specs.len() as u32
    } else {
        items_len as u32
    };
    // Available height, in terminal rows.
    let avail_rows = visible_rows.max(1);

    // The selection and the user-scroll latch are the widget's, and
    // come through the one resolver every reader uses — clamped to the
    // current dataset, which may have shrunk since the last paint.
    let Resolved {
        selected: effective_sel,
        user_scrolled,
    } = resolve(total, selected_index, list_key, prev);
    // The offset is the *last paint's*, not the widget's: the scroll
    // fold reads back its own previous value and republishes it below.
    let prev_scroll = ctx.painted(list_key).map(|w| w.offset).unwrap_or(0);

    // Pre-render the card blocks (if any) so we know the uniform card
    // height; the visible-item count and all the scroll math derive
    // from it. Nested hits/embeds/overlays/scroll are dropped: a card
    // is a single `select` target (interactive widgets nested in a
    // card aren't routed yet).
    let mut rendered_cards: Vec<Vec<TextPropertyEntry>> = Vec::new();
    let mut item_height: u32 = 1;
    if use_specs {
        (rendered_cards, item_height) = render_list_cards(item_specs, prev, ctx, panel_width);
    }
    // How many items fit, and the per-item scroll window.
    let visible_items = if use_specs {
        (avail_rows / item_height).max(1)
    } else {
        avail_rows
    };

    // When the card list overflows, the host paints a scrollbar in the
    // rightmost column — which would sit on top of each card's right
    // border. Re-render the cards one column narrower so they leave
    // that column free. (Row count is width-independent, so
    // `item_height` stays valid.)
    if use_specs && total > visible_items && panel_width > 1 {
        (rendered_cards, _) = render_list_cards(item_specs, prev, ctx, panel_width - 1);
    }

    // Compute scroll. Normally we auto-clamp to keep the selection in
    // view, but once the user has scrolled by mouse (`user_scrolled`)
    // we respect their offset as-is so the selected card can sit
    // off-screen — only the range clamp below still applies. Selection
    // moves (keyboard/click/plugin) clear `user_scrolled`, re-arming
    // this follow behaviour.
    let mut scroll = prev_scroll;
    if effective_sel >= 0 && !user_scrolled {
        let sel = effective_sel as u32;
        if sel < scroll {
            scroll = sel;
        }
        if sel >= scroll + visible_items {
            scroll = sel + 1 - visible_items;
        }
    }
    let max_scroll = total.saturating_sub(visible_items);
    if scroll > max_scroll {
        scroll = max_scroll;
    }

    ListLayout {
        total,
        effective_sel,
        scroll,
        visible_items,
        item_height,
        rendered_cards,
    }
}

#[allow(clippy::too_many_arguments)]
fn collect_list(
    items: &[TextPropertyEntry],
    item_specs: &[WidgetSpec],
    item_keys: &[String],
    selected_index: i32,
    spec_visible_rows: Option<u32>,
    list_key: Option<&str>,
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    ctx: RenderContext<'_>,
    panel_width: u32,
) -> CollectedOutput {
    let mut entries: Vec<TextPropertyEntry> = Vec::new();
    let mut hits: Vec<HitArea> = Vec::new();
    let mut self_scroll: Option<crate::widgets::layout_box::BoxScroll> = None;
    // Resolve the row window: an explicit spec value pins it exactly
    // as before; an omitted one auto-sizes from the host's height
    // budget (threaded down like `panel_width`, resolved to leftover
    // rows by `collect_col`'s fill pass). No budget → legacy default,
    // flagged so an enclosing Col with a real height can re-render
    // this subtree with one.
    let mut wants_fill = false;
    let visible_rows = match (spec_visible_rows, ctx.avail_height) {
        (Some(v), _) => v,
        (None, Some(budget)) => budget.max(1),
        (None, None) => {
            wants_fill = true;
            fresh_core::api::LEGACY_VISIBLE_ROWS_FALLBACK
        }
    };

    // Two layouts share one selection/scroll model:
    //   * classic — one `items` `TextPropertyEntry` per row;
    //   * cards    — one `item_specs` `WidgetSpec` per item,
    //                each rendered into a multi-row block (a
    //                rounded `LabeledSection` "pill", say).
    // Selection, scroll, `visible_rows`, and clicks are always
    // in *item* units; the card path just maps an item to a
    // fixed band of `item_height` rows instead of one row.
    let use_specs = !item_specs.is_empty();
    // Available height, in terminal rows.
    let avail_rows = visible_rows.max(1);
    let ListLayout {
        total,
        effective_sel,
        scroll,
        visible_items,
        item_height,
        rendered_cards,
    } = plan_list_layout(
        items.len(),
        item_specs,
        selected_index,
        visible_rows,
        list_key,
        prev,
        ctx,
        panel_width,
    );

    // **The walk carries this widget's state; it does not decide it.**
    //
    // It used to write the *resolved* triple back — the clamped index, the
    // measured card height and the folded offset — which made the render
    // walk an authority on state that `select_move`, the pointer path and
    // the wheel also write, and made every other reader depend on a paint
    // having happened first. Two of those three were never storage at all:
    // they are the paint's window, and they leave below under that name.
    // The third, the index, is clamped by [`resolve`] on every read, so a
    // reader gets the same answer whether or not the walk wrote it down.
    //
    // What the pass-through still buys is collection — `update_side_effects`
    // replaces the whole map, so a widget the spec no longer contains loses
    // its state, and one this walk did not mention would lose it too. An
    // absent entry stays absent: the spec is the seed until a handler makes
    // a decision. Same contract as `collect_dropdown`.
    if let Some(k) = list_key.filter(|k| !k.is_empty()) {
        if let Some(stored) = prev.get(k) {
            next_state.insert(k.to_string(), stored.clone());
        }
    }

    let start = scroll as usize;
    let end = ((scroll + visible_items) as usize).min(total as usize);

    let rows_emitted: u32 = if use_specs {
        // Each item occupies a band of `item_height` rows; shorter
        // cards pad within their band so every card lines up. A
        // `select` hit covers every row, so a click anywhere on
        // the card selects it. When the list height isn't a whole
        // multiple of the card height, the next item below the
        // fold is rendered *partially* into the leftover rows
        // (rather than a blank gap) so it's clear there's more to
        // scroll.
        let mut emitted = 0u32;
        let last = if end < total as usize { end + 1 } else { end };
        'cards: for (offset, card) in rendered_cards[start..last].iter().enumerate() {
            let i = start + offset;
            let is_selected = i as i32 == effective_sel;
            let item_key = item_keys.get(i).cloned().unwrap_or_default();
            // A list row carries its *own* item key as the hit's widget
            // key (unlike a tree, where every row shares the tree's), so
            // the pointer resolves to one card without any extra plumbing.
            let is_hovered_row = !is_selected && ctx.is_hovered(Some(item_key.as_str()));
            for r in 0..item_height as usize {
                if emitted >= avail_rows {
                    break 'cards;
                }
                let mut entry = card.get(r).cloned().unwrap_or_else(blank_list_row);
                entry.normalize_widths();
                if is_selected {
                    mark_list_card_selected(&mut entry);
                } else if is_hovered_row {
                    apply_hover_band(&mut entry);
                }
                let byte_end = entry.text.len();
                ensure_trailing_newline(&mut entry);
                let hit_row = entries.len() as u32;
                entries.push(entry);
                hits.push(HitArea {
                    overlay: false,
                    buffer_row: hit_row,
                    byte_start: 0,
                    byte_end,
                    event: crate::widgets::WidgetEvent {
                        row_target: true,
                        context_click: true,
                        widget_key: item_key.clone(),
                        widget_kind: "list",
                        payload: json!({
                            "index": i as i64,
                            "key": item_key,
                            "list_key": list_key,
                        }),
                        event_type: "select",
                        // The row's widget_key is the per-item key (hover
                        // and pointer resolution use it); the List itself
                        // owns the hit — focus, selection state, and the
                        // fired event target it.
                        owner_key: list_key.map(str::to_string),
                    },
                });
                emitted += 1;
            }
        }
        emitted
    } else {
        // Classic one-row-per-item path.
        for (offset, item) in items[start..end.min(items.len())].iter().enumerate() {
            let i = start + offset;
            let mut entry = item.clone();
            entry.normalize_widths();
            let item_key = item_keys.get(i).cloned().unwrap_or_default();
            if i as i32 == effective_sel {
                mark_list_row_selected(&mut entry);
            } else if ctx.is_hovered(Some(item_key.as_str())) {
                apply_hover_band(&mut entry);
            }
            let byte_end = entry.text.len();
            ensure_trailing_newline(&mut entry);
            entries.push(entry);
            let hit_row = (entries.len() - 1) as u32;
            hits.push(HitArea {
                overlay: false,
                buffer_row: hit_row,
                byte_start: 0,
                byte_end,
                event: crate::widgets::WidgetEvent {
                    row_target: true,
                    context_click: true,
                    widget_key: item_key.clone(),
                    widget_kind: "list",
                    payload: json!({
                        "index": i as i64,
                        "key": item_key,
                        // The List's own spec key, so a click handler can
                        // update the host-owned selection instance state
                        // (keyed by this) — the item key in `key` is not
                        // enough to find the widget. Null for keyless lists.
                        "list_key": list_key,
                    }),
                    event_type: "select",
                    owner_key: list_key.map(str::to_string),
                },
            });
        }
        (end - start) as u32
    };

    // Pad to the advertised height with blank rows so the List
    // occupies its full `visible_rows` (keeps a sibling pane's
    // bottom border aligned). Padding rows aren't clickable.
    for _ in rows_emitted..avail_rows {
        entries.push(blank_list_row());
    }

    // Surface the list's geometry + scroll state. The host paints a
    // draggable scrollbar for lists that overflow (`total > visible`),
    // and mouse-wheel routing hit-tests the pointer against the region
    // either way — a wheel over a list that fits must not scroll a
    // sibling list elsewhere on the panel. Totals are in items;
    // height_rows is the painted band so the thumb spans it.
    if let Some(k) = list_key {
        let _ = k;
        self_scroll = Some(crate::widgets::layout_box::BoxScroll {
            total: total as usize,
            visible: visible_items as usize,
            offset: scroll as usize,
        });
    }

    // The window this paint used, published under the name it deserves:
    // the row budget, how many items that showed, where the window sat,
    // and the measured card band. Every one of them is a derivation over
    // geometry or a fold the painter owns — see `PaintedWindow`.
    let mut painted = HashMap::new();
    if let Some(k) = list_key {
        if !k.is_empty() {
            painted.insert(
                k.to_string(),
                PaintedWindow {
                    rows: visible_rows,
                    items: visible_items,
                    offset: scroll,
                },
            );
        }
    }
    CollectedOutput {
        entries,
        hits,
        wants_fill,
        painted,
        focus_cursor: None,
        embeds: Vec::new(),
        overlays: Vec::new(),
        self_scroll,
        popups: Vec::new(),
        // List items are virtualized rows, not independently
        // addressable boxes — the List's own box (pushed by
        // `render_collected`) is the dispatch target; row-level
        // targeting stays on `HitArea`s.
        boxes: Vec::new(),
    }
}
