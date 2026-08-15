//! `List` — virtual-scrolled select list (classic rows or card items).

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;
use fresh_core::text_property::TextPropertyEntry;
use serde_json::json;

use super::WidgetImpl;
use crate::widgets::registry::{HitArea, WidgetInstanceState};
use crate::widgets::render::{
    apply_hover_band, blank_list_row, ensure_trailing_newline, mark_list_card_selected,
    mark_list_row_selected, render_collected, CollectedOutput, RenderContext, ScrollRegion,
};

pub(crate) struct List;

impl WidgetImpl for List {
    fn on_wheel(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        delta: i32,
    ) -> bool {
        let WidgetSpec::List {
            visible_rows,
            items,
            item_specs,
            ..
        } = spec
        else {
            return false;
        };
        let total = if item_specs.is_empty() {
            items.len()
        } else {
            item_specs.len()
        } as u32;
        if total == 0 {
            return false;
        }
        let visible_rows = panel.effective_visible_rows(widget_key, *visible_rows);
        let (cur_sel, cur_scroll, item_height) = match panel.instance_states.get(widget_key) {
            Some(WidgetInstanceState::List {
                selected_index,
                scroll_offset,
                item_height,
                ..
            }) => (*selected_index, *scroll_offset, (*item_height).max(1)),
            _ => (-1, 0, 1),
        };
        // Convert the row-denominated viewport into a per-item window so
        // the bound is right for card lists (item_height > 1), and so a
        // list that already shows everything (max_scroll == 0, e.g. the
        // Git Log which sets visible_rows == commit count and scrolls via
        // its enclosing pane) reports "can't scroll" and lets the wheel
        // bubble to that pane rather than swallowing it.
        let visible_items = (visible_rows.max(1) / item_height).max(1);
        let max_scroll = total.saturating_sub(visible_items);
        let new_scroll = (cur_scroll as i64 + delta as i64).clamp(0, max_scroll as i64) as u32;
        if new_scroll == cur_scroll {
            return false;
        }
        // Wheel scrolls the *view* only — the selection stays put (and
        // may leave the visible window); `user_scrolled` tells the
        // renderer not to snap the offset back to it.
        panel.instance_states.insert(
            widget_key.to_string(),
            WidgetInstanceState::List {
                scroll_offset: new_scroll,
                selected_index: cur_sel,
                item_height,
                user_scrolled: true,
            },
        );
        true
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
    /// Whether the host last scrolled by mouse (suppresses follow).
    user_scrolled: bool,
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

    // Look up host-owned scroll + selected index from prev state
    // (becomes authoritative after first render). The spec's
    // `selected_index` is initial-only on first mount.
    let (prev_scroll, prev_sel, user_scrolled) = list_key
        .and_then(|k| prev.get(k))
        .and_then(|s| match s {
            WidgetInstanceState::List {
                scroll_offset,
                selected_index,
                user_scrolled,
                ..
            } => Some((*scroll_offset, *selected_index, *user_scrolled)),
            _ => None,
        })
        .unwrap_or((0, selected_index, false));
    // Clamp the previous selection to the current dataset size —
    // items may have shrunk between renders. Out-of-range selections
    // collapse to the last item, or -1 if the list is now empty.
    let effective_sel = if prev_sel < 0 || total == 0 {
        -1
    } else if (prev_sel as u32) >= total {
        (total - 1) as i32
    } else {
        prev_sel
    };

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
        user_scrolled,
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
    let mut scroll_regions: Vec<ScrollRegion> = Vec::new();
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
        user_scrolled,
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

    // Persist scroll + selection for the next render.
    // Lists without a `key` lose state across updates.
    if let Some(k) = list_key {
        next_state.insert(
            k.to_string(),
            WidgetInstanceState::List {
                scroll_offset: scroll,
                selected_index: effective_sel,
                item_height,
                user_scrolled,
            },
        );
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
                    widget_key: item_key.clone(),
                    widget_kind: "list",
                    buffer_row: hit_row,
                    byte_start: 0,
                    byte_end,
                    payload: json!({
                        "index": i as i64,
                        "key": item_key,
                        "list_key": list_key,
                    }),
                    event_type: "select",
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
                widget_key: item_key.clone(),
                widget_kind: "list",
                buffer_row: hit_row,
                byte_start: 0,
                byte_end,
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
        scroll_regions.push(ScrollRegion {
            list_key: k.to_string(),
            buffer_row: 0,
            col_in_row: 0,
            width_cols: panel_width,
            height_rows: avail_rows,
            total: total as usize,
            visible: visible_items as usize,
            scroll: scroll as usize,
        });
    }

    let mut effective_rows = HashMap::new();
    if let Some(k) = list_key {
        if !k.is_empty() {
            effective_rows.insert(k.to_string(), visible_rows);
        }
    }
    CollectedOutput {
        entries,
        hits,
        wants_fill,
        effective_rows,
        focus_cursor: None,
        embeds: Vec::new(),
        overlays: Vec::new(),
        scroll_regions,
        dropdown_popups: Vec::new(),
        // List items are virtualized rows, not independently
        // addressable boxes — the List's own box (pushed by
        // `render_collected`) is the dispatch target; row-level
        // targeting stays on `HitArea`s.
        boxes: Vec::new(),
    }
}
