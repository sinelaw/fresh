//! Render a `WidgetSpec` tree into `Vec<TextPropertyEntry>`.
//!
//! This is the path from declarative spec to the bytes the existing
//! virtual-buffer pipeline already knows how to display. By going
//! through `TextPropertyEntry`, widgets paint via exactly the same
//! renderer that today's `setVirtualBufferContent` uses — no parallel
//! render path. This is what makes the new widget API additive: the
//! buffer mid-bytes are indistinguishable from hand-rolled output.
//!
//! v1 dispatches on four kinds:
//!   * `Row` — children laid out left-to-right within a single line
//!     (the result is one `TextPropertyEntry`).
//!   * `Col` — children stacked vertically (the result is one
//!     `TextPropertyEntry` per child output line).
//!   * `HintBar` — keyboard-hint footer (one `TextPropertyEntry`).
//!   * `Raw` — pass-through (zero interpretation; plugin's entries
//!     flow through unchanged).
//!
//! Future kinds (`Toggle`, `Button`, `TextInput`, `List`, `Tree`,
//! `Layer`, `Transient`, `Table`) extend the dispatch without
//! changing the public function signature.

use crate::widgets::registry::{HitArea, WidgetInstanceState};
use fresh_core::api::{
    ButtonKind, HintEntry, OverlayColorSpec, OverlayOptions, TreeNode, WidgetSpec,
};
use fresh_core::text_property::{InlineOverlay, OffsetUnit, TextPropertyEntry};
use serde_json::json;
use std::collections::{HashMap, HashSet};

// Theme keys used by the v1 widget renderers. Centralized so future
// "role-based" theming (§7 of the design doc) has one place to
// substitute the role→key mapping.
const KEY_HELP_KEY_FG: &str = "ui.help_key_fg";
const KEY_TOGGLE_ON_FG: &str = "ui.tab_active_fg";
const KEY_FOCUSED_FG: &str = "ui.menu_active_fg";
const KEY_FOCUSED_BG: &str = "ui.menu_active_bg";
const KEY_DANGER_FG: &str = "ui.status_error_indicator_fg";
const KEY_INPUT_BG: &str = "ui.prompt_bg";
const KEY_PLACEHOLDER_FG: &str = "ui.menu_disabled_fg";

/// Where the host should place the buffer's hardware cursor — the
/// terminal's blinking caret — when a `TextInput` is focused. Built
/// by the renderer; the dispatcher translates `(buffer_row,
/// byte_in_row)` to an absolute byte position in the virtual buffer
/// and sets the panel buffer's primary cursor there. When a
/// non-text widget is focused (Toggle / Button / List) or the
/// panel has no tabbable widgets, this is `None` and the host
/// hides the cursor entirely.
#[derive(Debug, Clone, Copy)]
pub struct FocusCursor {
    pub buffer_row: u32,
    pub byte_in_row: u32,
}

/// What a single render of a `WidgetSpec` produces.
///
/// * `entries` — the bytes for `set_virtual_buffer_content`.
/// * `hits` — click rectangles for the `WidgetRegistry` so a later
///   `mouse_click` dispatches a semantic `widget_event`.
/// * `instance_states` — next-tick widget instance state (List
///   scroll offsets / selection, TextInput value+cursor, …).
/// * `focus_key` — currently focused widget key, clamped to a
///   tabbable that exists in the spec (or `""` when there are no
///   tabbables).
/// * `tabbable` — focusable widget keys collected in declaration
///   order. The Tab-cycle command finds the current `focus_key`'s
///   index in this list to advance it.
/// * `focus_cursor` — when a `TextInput` is focused, where the
///   terminal cursor should land. Replaces the previous
///   "overlay-as-cursor" hack — the actual hardware cursor blinks
///   at the right byte, with no theme-color guesswork.
pub struct RenderOutput {
    pub entries: Vec<TextPropertyEntry>,
    pub hits: Vec<HitArea>,
    pub instance_states: HashMap<String, WidgetInstanceState>,
    pub focus_key: String,
    pub tabbable: Vec<String>,
    pub focus_cursor: Option<FocusCursor>,
}

/// Render a spec to a [`RenderOutput`].
///
/// `prev` is the previous render's instance state (or empty on
/// first mount). `prev_focus_key` is the previous render's focus
/// key (or `""`); the renderer keeps it if it matches a tabbable in
/// the new spec, otherwise falls back to the first tabbable.
/// `panel_width` is the buffer's column width — used by `Row` to
/// size flex `Spacer`s. Pass `u32::MAX` to disable flex (children
/// won't be padded).
pub fn render_spec(
    spec: &WidgetSpec,
    prev: &HashMap<String, WidgetInstanceState>,
    prev_focus_key: &str,
    panel_width: u32,
) -> RenderOutput {
    // Walk the spec to collect tabbable keys, then resolve the
    // active focus key. This must happen before the entry pass so
    // that widget arms know whether they're focused.
    let mut tabbable = Vec::new();
    collect_tabbable(spec, &mut tabbable);
    let focus_key = if !prev_focus_key.is_empty() && tabbable.iter().any(|k| k == prev_focus_key) {
        prev_focus_key.to_string()
    } else {
        tabbable.first().cloned().unwrap_or_default()
    };

    let mut next_state = HashMap::new();
    let (entries, hits, focus_cursor) =
        render_collected(spec, prev, &mut next_state, &focus_key, panel_width);
    RenderOutput {
        entries,
        hits,
        instance_states: next_state,
        focus_key,
        tabbable,
        focus_cursor,
    }
}

/// One position in a Row's two-pass layout. Used internally to
/// defer flex-spacer sizing until after we know all the inline
/// children's natural widths.
enum RowPiece {
    Inline {
        entry: TextPropertyEntry,
        hits: Vec<HitArea>,
        /// Some when this inline child was a focused TextInput.
        /// `byte_in_row` is the cursor's offset within the *child's*
        /// text — the Row collapse pass shifts it by the merged
        /// inline_shift before publishing.
        focus_cursor: Option<FocusCursor>,
    },
    Block {
        entries: Vec<TextPropertyEntry>,
        hits: Vec<HitArea>,
        focus_cursor: Option<FocusCursor>,
    },
    Flex,
}

/// Strip a trailing `'\n'` from `entry.text` if present (overlays /
/// hits aren't affected because the newline is at the very end and
/// no overlay should span it). Used to prepare an inline-rendered
/// child for Row inline-collapse, where individual newlines would
/// split the merged row across multiple buffer lines.
fn strip_trailing_newline(entry: &mut TextPropertyEntry) {
    if entry.text.ends_with('\n') {
        entry.text.pop();
    }
}

/// Append a single trailing newline to `entry.text` if it doesn't
/// already end with one. Each top-level entry needs to end with
/// `\n` so it occupies its own line in the underlying virtual
/// buffer (the buffer's line model is byte-driven; without `\n`
/// adjacent entries concatenate into one logical line).
fn ensure_trailing_newline(entry: &mut TextPropertyEntry) {
    if !entry.text.ends_with('\n') {
        entry.text.push('\n');
    }
}

/// Walk a spec tree and append tabbable widget keys (`Toggle`,
/// `Button`, `TextInput`, `List`, `Tree` with a non-empty `key`) in
/// declaration order. Layout containers (`Row`, `Col`) recurse;
/// `Raw`, `Spacer`, `HintBar` skip.
fn collect_tabbable(spec: &WidgetSpec, out: &mut Vec<String>) {
    match spec {
        WidgetSpec::Row { children, .. } | WidgetSpec::Col { children, .. } => {
            for c in children {
                collect_tabbable(c, out);
            }
        }
        WidgetSpec::Toggle { key: Some(k), .. }
        | WidgetSpec::Button { key: Some(k), .. }
        | WidgetSpec::Text { key: Some(k), .. }
        | WidgetSpec::List { key: Some(k), .. }
        | WidgetSpec::Tree { key: Some(k), .. }
            if !k.is_empty() =>
        {
            out.push(k.clone());
        }
        _ => {}
    }
}

/// Internal renderer. Returns the entries and the hit areas
/// produced by `spec` *as if* it were rendered at row 0; callers
/// (Col, Row block path) shift `buffer_row` upward by their own
/// row offset before forwarding. `prev` is read-only previous
/// instance state; `next_state` accumulates the post-render state
/// the host should persist. `focus_key` is the panel's currently
/// focused widget key — widget arms compare against their own
/// `key` to decide whether to render with focus styling, ignoring
/// the spec's `focused` field. (Plugin-passed `focused` is the
/// initial-only hint that becomes redundant once the host's focus
/// key takes over.)
fn render_collected(
    spec: &WidgetSpec,
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    focus_key: &str,
    panel_width: u32,
) -> (Vec<TextPropertyEntry>, Vec<HitArea>, Option<FocusCursor>) {
    let mut entries: Vec<TextPropertyEntry> = Vec::new();
    let mut hits: Vec<HitArea> = Vec::new();
    // At most one TextInput is focused per panel, so the cursor
    // position bubbles up through containers as a single Option.
    let mut focus_cursor: Option<FocusCursor> = None;
    match spec {
        WidgetSpec::Row { children, .. } => {
            // Two-pass layout for Row:
            //  1. Walk children, render each. Track flex spacers
            //     by index in the accumulator; their text starts
            //     empty and grows in pass 2.
            //  2. Compute leftover width = panel_width - sum of
            //     non-flex widths; distribute evenly across flex
            //     slots; expand each flex spacer's text + shift
            //     subsequent overlays / hits accordingly.
            //
            // Multi-line children (Raw with N>1, nested Col)
            // flush the row accumulator and pass through unchanged
            // — flex layout only spans inline-sized children.
            let mut row_pieces: Vec<RowPiece> = Vec::new();
            for child in children {
                if let WidgetSpec::Spacer { flex: true, .. } = child {
                    row_pieces.push(RowPiece::Flex);
                    continue;
                }
                let (child_entries, child_hits, child_focus) =
                    render_collected(child, prev, next_state, focus_key, panel_width);
                if child_entries.is_empty() {
                    debug_assert!(child_hits.is_empty(), "empty children produce no hits");
                    continue;
                }
                if child_entries.len() == 1 {
                    let mut entry = child_entries.into_iter().next().unwrap();
                    // Inline children can't carry their own newlines
                    // — that would split the merged Row across
                    // buffer lines. The Row's final merged entry
                    // gets exactly one newline appended below.
                    strip_trailing_newline(&mut entry);
                    row_pieces.push(RowPiece::Inline {
                        entry,
                        hits: child_hits,
                        focus_cursor: child_focus,
                    });
                } else {
                    row_pieces.push(RowPiece::Block {
                        entries: child_entries,
                        hits: child_hits,
                        focus_cursor: child_focus,
                    });
                }
            }

            // Compute flex sizing.
            let inline_natural: usize = row_pieces
                .iter()
                .filter_map(|p| match p {
                    RowPiece::Inline { entry, .. } => Some(entry.text.len()),
                    _ => None,
                })
                .sum();
            let flex_count = row_pieces
                .iter()
                .filter(|p| matches!(p, RowPiece::Flex))
                .count();
            let flex_total = (panel_width as usize).saturating_sub(inline_natural);
            // Distribute leftover evenly. With multiple flex slots,
            // the leftover bytes spread as evenly as possible (any
            // remainder lands in the first slot).
            let (flex_each, flex_extra) = match flex_total.checked_div(flex_count) {
                Some(each) => (each, flex_total % flex_count),
                None => (0, 0),
            };

            // Pass 2: assemble. Accumulate inline pieces (with
            // collapsed flex spacers) into one entry; flush block
            // pieces. Track byte-shift so child hits' offsets stay
            // correct.
            let mut acc: Option<TextPropertyEntry> = None;
            let mut flex_seen = 0usize;
            for piece in row_pieces {
                match piece {
                    RowPiece::Inline {
                        mut entry,
                        hits: child_hits,
                        focus_cursor: child_focus,
                    } => {
                        let inline_shift = match acc.as_ref() {
                            Some(e) => e.text.len(),
                            None => 0,
                        };
                        for mut h in child_hits {
                            h.byte_start += inline_shift;
                            h.byte_end += inline_shift;
                            hits.push(h);
                        }
                        if let Some(mut fc) = child_focus {
                            // buffer_row stays 0 — caller shifts.
                            fc.byte_in_row += inline_shift as u32;
                            focus_cursor = Some(fc);
                        }
                        match acc.as_mut() {
                            Some(merged) => merge_inline(merged, &mut entry),
                            None => acc = Some(entry),
                        }
                    }
                    RowPiece::Flex => {
                        // Materialize the flex spacer as N spaces.
                        let n = flex_each + if flex_seen < flex_extra { 1 } else { 0 };
                        flex_seen += 1;
                        if n > 0 {
                            let mut text = String::with_capacity(n);
                            for _ in 0..n {
                                text.push(' ');
                            }
                            let entry = TextPropertyEntry {
                                text,
                                properties: Default::default(),
                                style: None,
                                inline_overlays: Vec::new(),
                                segments: Vec::new(),
                                pad_to_chars: None,
                                truncate_to_chars: None,
                            };
                            match acc.as_mut() {
                                Some(merged) => {
                                    let mut e = entry;
                                    merge_inline(merged, &mut e);
                                }
                                None => acc = Some(entry),
                            }
                        }
                    }
                    RowPiece::Block {
                        entries: block_entries,
                        hits: child_hits,
                        focus_cursor: child_focus,
                    } => {
                        if let Some(mut merged) = acc.take() {
                            ensure_trailing_newline(&mut merged);
                            entries.push(merged);
                        }
                        let row_offset = entries.len() as u32;
                        for mut h in child_hits {
                            h.buffer_row += row_offset;
                            hits.push(h);
                        }
                        if let Some(mut fc) = child_focus {
                            fc.buffer_row += row_offset;
                            focus_cursor = Some(fc);
                        }
                        entries.extend(block_entries);
                    }
                }
            }
            if let Some(mut merged) = acc {
                ensure_trailing_newline(&mut merged);
                entries.push(merged);
            }
        }
        WidgetSpec::Col { children, .. } => {
            for child in children {
                let (child_entries, child_hits, child_focus) =
                    render_collected(child, prev, next_state, focus_key, panel_width);
                let row_offset = entries.len() as u32;
                for mut h in child_hits {
                    h.buffer_row += row_offset;
                    hits.push(h);
                }
                if let Some(mut fc) = child_focus {
                    fc.buffer_row += row_offset;
                    focus_cursor = Some(fc);
                }
                entries.extend(child_entries);
            }
        }
        WidgetSpec::HintBar {
            entries: hint_entries,
            ..
        } => {
            let mut entry = render_hint_bar(hint_entries);
            ensure_trailing_newline(&mut entry);
            entries.push(entry);
            // No hits — HintBar is read-only in v1. (When the
            // keymap layer arrives, individual entries become
            // clickable command targets.)
        }
        WidgetSpec::Toggle {
            checked,
            label,
            focused,
            key,
        } => {
            // Host-managed focus overrides the spec's `focused`
            // when this widget has a key and is the panel's focused
            // widget. Plugin-passed `focused` is ignored when the
            // host owns focus (i.e. the panel has any tabbable
            // widgets); without it, the renderer falls back to the
            // spec value (legacy path).
            let is_focused = match key.as_deref() {
                Some(k) if !k.is_empty() => k == focus_key,
                _ => *focused,
            };
            let mut entry = render_toggle(*checked, label, is_focused);
            let byte_end = entry.text.len();
            hits.push(HitArea {
                widget_key: key.clone().unwrap_or_default(),
                widget_kind: "toggle",
                buffer_row: 0,
                byte_start: 0,
                byte_end,
                payload: json!({ "checked": !*checked }),
                event_type: "toggle",
            });
            ensure_trailing_newline(&mut entry);
            entries.push(entry);
        }
        WidgetSpec::Button {
            label,
            focused,
            intent,
            key,
        } => {
            let is_focused = match key.as_deref() {
                Some(k) if !k.is_empty() => k == focus_key,
                _ => *focused,
            };
            let mut entry = render_button(label, is_focused, *intent);
            let byte_end = entry.text.len();
            hits.push(HitArea {
                widget_key: key.clone().unwrap_or_default(),
                widget_kind: "button",
                buffer_row: 0,
                byte_start: 0,
                byte_end,
                payload: json!({}),
                event_type: "activate",
            });
            ensure_trailing_newline(&mut entry);
            entries.push(entry);
        }
        WidgetSpec::Spacer { cols, flex, .. } => {
            // Top-level / Col context: flex Spacers don't fill at
            // this level (no Row to absorb their flexibility), so
            // they fall back to `cols`. Row uses a separate code
            // path that sees the Spacer spec directly and handles
            // flex sizing — see RowPiece::Flex.
            let _ = flex;
            let cols = (*cols).min(4096) as usize;
            let mut text = String::with_capacity(cols + 1);
            for _ in 0..cols {
                text.push(' ');
            }
            let mut entry = TextPropertyEntry {
                text,
                properties: Default::default(),
                style: None,
                inline_overlays: Vec::new(),
                segments: Vec::new(),
                pad_to_chars: None,
                truncate_to_chars: None,
            };
            ensure_trailing_newline(&mut entry);
            entries.push(entry);
        }
        WidgetSpec::List {
            items,
            item_keys,
            selected_index,
            visible_rows,
            key: list_key,
        } => {
            // Look up host-owned scroll + selected index from prev
            // state (becomes authoritative after first render).
            // Spec's `selected_index` is initial-only on first
            // mount; subsequent updates read instance state.
            let total = items.len() as u32;
            let visible = (*visible_rows).max(1);
            let (prev_scroll, prev_sel) = list_key
                .as_deref()
                .and_then(|k| prev.get(k))
                .and_then(|s| match s {
                    WidgetInstanceState::List {
                        scroll_offset,
                        selected_index,
                    } => Some((*scroll_offset, *selected_index)),
                    _ => None,
                })
                .unwrap_or((0, *selected_index));
            // Clamp the previous selection to the current dataset
            // size — items may have shrunk between renders (e.g.
            // search results changed). Out-of-range selections
            // collapse to the last item, or -1 if the list is
            // now empty.
            let effective_sel = if prev_sel < 0 || total == 0 {
                -1
            } else if (prev_sel as u32) >= total {
                (total - 1) as i32
            } else {
                prev_sel
            };

            // Compute scroll: auto-clamp to keep selection in view
            // and never extend past the dataset end.
            let mut scroll = prev_scroll;
            if effective_sel >= 0 {
                let sel = effective_sel as u32;
                if sel < scroll {
                    scroll = sel;
                }
                if sel >= scroll + visible {
                    scroll = sel + 1 - visible;
                }
            }
            let max_scroll = total.saturating_sub(visible);
            if scroll > max_scroll {
                scroll = max_scroll;
            }
            // Persist scroll + selection for the next render.
            // Lists without a `key` lose state across updates.
            if let Some(k) = list_key.as_deref() {
                next_state.insert(
                    k.to_string(),
                    WidgetInstanceState::List {
                        scroll_offset: scroll,
                        selected_index: effective_sel,
                    },
                );
            }

            // Render the visible window, emitting one entry + one
            // hit area per visible item. Selected row gets the
            // menu_active_bg + extend_to_line_end style. Hit-area
            // payload uses the *absolute* item index so the plugin
            // never needs to translate window-relative coordinates.
            let start = scroll as usize;
            let end = ((scroll + visible) as usize).min(items.len());
            for (offset, item) in items[start..end].iter().enumerate() {
                let i = start + offset;
                let mut entry = item.clone();
                entry.normalize_widths();
                let is_selected = i as i32 == effective_sel;
                if is_selected {
                    let mut style = entry.style.unwrap_or_default();
                    style.bg = Some(OverlayColorSpec::theme_key(KEY_FOCUSED_BG));
                    style.extend_to_line_end = true;
                    entry.style = Some(style);
                }
                let byte_end = entry.text.len();
                ensure_trailing_newline(&mut entry);
                entries.push(entry);
                let item_key = item_keys.get(i).cloned().unwrap_or_default();
                let hit_row = (entries.len() - 1) as u32;
                hits.push(HitArea {
                    widget_key: item_key.clone(),
                    widget_kind: "list",
                    buffer_row: hit_row,
                    byte_start: 0,
                    byte_end,
                    payload: json!({
                        "index": i as i64,
                        "key": item_key,
                    }),
                    event_type: "select",
                });
            }
        }
        WidgetSpec::Tree {
            nodes,
            item_keys,
            selected_index,
            visible_rows,
            expanded_keys,
            checkable,
            key: tree_key,
        } => {
            // Look up host-owned instance state (scroll, selection,
            // expanded set). Spec values are initial-only.
            let prev_state = tree_key
                .as_deref()
                .filter(|k| !k.is_empty())
                .and_then(|k| prev.get(k));
            let (prev_scroll, prev_sel, prev_expanded) = match prev_state {
                Some(WidgetInstanceState::Tree {
                    scroll_offset,
                    selected_index,
                    expanded_keys,
                }) => (*scroll_offset, *selected_index, expanded_keys.clone()),
                _ => {
                    // First render: seed expanded_keys from spec.
                    let seeded: HashSet<String> = expanded_keys.iter().cloned().collect();
                    (0, *selected_index, seeded)
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
            let total_visible = visible_indices.len() as u32;
            let visible = (*visible_rows).max(1);
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

            // Compute scroll: same auto-clamp logic as List, but
            // operating on the visible-windowed indices.
            let mut scroll = prev_scroll;
            if sel_visible_pos >= 0 {
                let sel = sel_visible_pos as u32;
                if sel < scroll {
                    scroll = sel;
                }
                if sel >= scroll + visible {
                    scroll = sel + 1 - visible;
                }
            }
            let max_scroll = total_visible.saturating_sub(visible);
            if scroll > max_scroll {
                scroll = max_scroll;
            }

            // Persist instance state.
            if let Some(k) = tree_key.as_deref().filter(|k| !k.is_empty()) {
                next_state.insert(
                    k.to_string(),
                    WidgetInstanceState::Tree {
                        scroll_offset: scroll,
                        selected_index: effective_sel_abs,
                        expanded_keys: prev_expanded.clone(),
                    },
                );
            }

            // Render the visible window.
            let start = scroll as usize;
            let end = ((scroll + visible) as usize).min(visible_indices.len());
            for &abs_idx in &visible_indices[start..end] {
                // Apply pad/truncate hints and convert any char-unit
                // overlays to byte offsets *before* the disclosure
                // prefix is prepended; render_tree_row then byte-shifts
                // the (now byte-unit) overlays uniformly.
                let mut node = nodes[abs_idx].clone();
                node.text.normalize_widths();
                let item_key = item_keys.get(abs_idx).cloned().unwrap_or_default();
                let is_expanded =
                    node.has_children && !item_key.is_empty() && prev_expanded.contains(&item_key);
                let rendered = render_tree_row(&node, is_expanded, *checkable);
                let mut entry = rendered.entry;
                let is_selected = abs_idx as i32 == effective_sel_abs;
                if is_selected {
                    let mut style = entry.style.unwrap_or_default();
                    style.bg = Some(OverlayColorSpec::theme_key(KEY_FOCUSED_BG));
                    style.extend_to_line_end = true;
                    entry.style = Some(style);
                }
                let row_byte_end = entry.text.len();
                ensure_trailing_newline(&mut entry);
                entries.push(entry);
                let hit_row = (entries.len() - 1) as u32;
                // Disclosure hit (only when has_children) — fires
                // `expand`. The host toggles instance-state
                // `expanded_keys` and re-renders before firing the
                // event; the plugin only listens if it cares about
                // expansion changes.
                // Tree hits use the *tree's* spec key for
                // `widget_key` (so click-to-focus works the same
                // as Toggle/Button — the tree is tabbable). The
                // per-row key travels in the payload.
                let tree_spec_key = tree_key.clone().unwrap_or_default();
                if let Some(disc_range) = rendered.disclosure_range {
                    hits.push(HitArea {
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
                    hits.push(HitArea {
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
                    hits.push(HitArea {
                        widget_key: tree_spec_key,
                        widget_kind: "tree",
                        buffer_row: hit_row,
                        byte_start: body_start,
                        byte_end: row_byte_end,
                        payload: json!({
                            "index": abs_idx as i64,
                            "key": item_key,
                        }),
                        event_type: "select",
                    });
                }
            }
        }
        WidgetSpec::Text {
            value,
            cursor_byte,
            focused,
            label,
            placeholder,
            rows,
            field_width,
            max_visible_chars,
            key,
        } => {
            let is_focused = match key.as_deref() {
                Some(k) if !k.is_empty() => k == focus_key,
                _ => *focused,
            };
            // Host-owned value/cursor (+ scroll, multi-line only):
            // read instance state if it exists; else seed from spec
            // on first render. See WidgetInstanceState::Text doc.
            let (effective_value, effective_cursor_byte, prev_scroll) = match key
                .as_deref()
                .filter(|k| !k.is_empty())
                .and_then(|k| prev.get(k))
            {
                Some(WidgetInstanceState::Text {
                    value,
                    cursor_byte,
                    scroll,
                }) => (value.clone(), *cursor_byte as i32, *scroll),
                _ => (value.clone(), *cursor_byte, 0),
            };
            let effective_cursor = if is_focused {
                effective_cursor_byte
            } else {
                -1
            };
            // `rows == 0` shouldn't happen because of serde's
            // default = 1, but if it slips through (raw struct
            // construction in tests, etc.) treat it as single-line.
            let multiline = *rows > 1;
            let new_scroll;
            if multiline {
                let rendered = render_text_area(
                    &effective_value,
                    effective_cursor,
                    is_focused,
                    label,
                    placeholder.as_deref(),
                    *rows,
                    *field_width,
                    prev_scroll,
                    panel_width,
                );
                new_scroll = rendered.scroll_row;
                if let (Some(buffer_row), Some(byte_in_row)) =
                    (rendered.cursor_buffer_row, rendered.cursor_byte_in_row)
                {
                    focus_cursor = Some(FocusCursor {
                        buffer_row,
                        byte_in_row: byte_in_row as u32,
                    });
                }
                for mut e in rendered.entries {
                    ensure_trailing_newline(&mut e);
                    entries.push(e);
                }
            } else {
                let rendered = render_text_input(
                    &effective_value,
                    effective_cursor,
                    is_focused,
                    label,
                    placeholder.as_deref(),
                    *max_visible_chars,
                    *field_width,
                );
                new_scroll = 0;
                if let Some(byte_in_row) = rendered.cursor_byte_in_entry {
                    focus_cursor = Some(FocusCursor {
                        buffer_row: 0,
                        byte_in_row: byte_in_row as u32,
                    });
                }
                let mut entry = rendered.entry;
                ensure_trailing_newline(&mut entry);
                entries.push(entry);
            }
            // Persist instance state for next render. Cursor clamps
            // into `[0, value.len()]`; `scroll` carries the
            // renderer's auto-clamped first-visible-row for
            // multi-line, or `0` for single-line.
            if let Some(k) = key.as_deref().filter(|k| !k.is_empty()) {
                let cb = effective_cursor_byte
                    .max(0)
                    .min(effective_value.len() as i32) as u32;
                next_state.insert(
                    k.to_string(),
                    WidgetInstanceState::Text {
                        value: effective_value.clone(),
                        cursor_byte: cb,
                        scroll: new_scroll,
                    },
                );
            }
        }
        WidgetSpec::Raw {
            entries: raw_entries,
            ..
        } => {
            // Raw is the migration escape hatch: the plugin's own
            // bytes flow through unchanged. The plugin still owns
            // mouse clicks within Raw regions (via the existing
            // `mouse_click` hook); the widget runtime intentionally
            // emits no hit areas here. We *do* ensure each Raw
            // entry ends with a newline so it occupies its own
            // buffer line — plugins that already include `\n` are
            // unaffected.
            for raw_entry in raw_entries {
                let mut e = raw_entry.clone();
                e.normalize_widths();
                ensure_trailing_newline(&mut e);
                entries.push(e);
            }
        }
    }
    (entries, hits, focus_cursor)
}

/// Render a HintBar into a single `TextPropertyEntry`.
///
/// Layout: `<keys> <label>  <keys> <label>  …`. The key portion of
/// each entry is highlighted with the `ui.help_key_fg` theme key;
/// labels use the buffer's default foreground.
///
/// This replaces the per-plugin hand-rolled footer at e.g.
/// `crates/fresh-editor/plugins/search_replace.ts:535–541`,
/// `audit_mode.ts:1068–1158`, `pkg.ts:2136–2145`.
pub fn render_hint_bar(entries: &[HintEntry]) -> TextPropertyEntry {
    let separator = "  ";
    let mut text = String::new();
    let mut overlays = Vec::new();
    for (i, entry) in entries.iter().enumerate() {
        if i > 0 {
            text.push_str(separator);
        }
        let key_start = text.len();
        text.push_str(&entry.keys);
        let key_end = text.len();
        if key_end > key_start {
            overlays.push(InlineOverlay {
                start: key_start,
                end: key_end,
                style: OverlayOptions {
                    fg: Some(OverlayColorSpec::theme_key(KEY_HELP_KEY_FG)),
                    bold: true,
                    ..Default::default()
                },
                properties: Default::default(),
                unit: OffsetUnit::Byte,
            });
        }
        if !entry.label.is_empty() {
            text.push(' ');
            text.push_str(&entry.label);
        }
    }
    TextPropertyEntry {
        text,
        properties: Default::default(),
        style: None,
        inline_overlays: overlays,
        segments: Vec::new(),
        pad_to_chars: None,
        truncate_to_chars: None,
    }
}

/// Render a `Toggle` to a single `TextPropertyEntry`.
///
/// Layout: `[v] label` when checked, `[ ] label` when not. The check
/// glyph is colored via `ui.tab_active_fg` when checked (no override
/// when unchecked). When focused, the entire entry is given a focused
/// fg/bg pair (`ui.menu_active_fg`/`ui.menu_active_bg`) plus bold —
/// matching the Settings UI's selected-control affordance.
pub fn render_toggle(checked: bool, label: &str, focused: bool) -> TextPropertyEntry {
    let glyph = if checked { "[v]" } else { "[ ]" };
    let mut text = String::with_capacity(glyph.len() + 1 + label.len());
    text.push_str(glyph);
    text.push(' ');
    text.push_str(label);

    let mut overlays = Vec::new();

    // Check-glyph color (only when checked — leaves default fg
    // when unchecked, which is what plugins do today).
    if checked {
        overlays.push(InlineOverlay {
            start: 0,
            end: glyph.len(),
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(KEY_TOGGLE_ON_FG)),
                bold: true,
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }

    // Focused: full-entry fg/bg + bold.
    if focused {
        overlays.push(InlineOverlay {
            start: 0,
            end: text.len(),
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(KEY_FOCUSED_FG)),
                bg: Some(OverlayColorSpec::theme_key(KEY_FOCUSED_BG)),
                bold: true,
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }

    TextPropertyEntry {
        text,
        properties: Default::default(),
        style: None,
        inline_overlays: overlays,
        segments: Vec::new(),
        pad_to_chars: None,
        truncate_to_chars: None,
    }
}

/// Render a `Button` to a single `TextPropertyEntry`.
///
/// Layout: `[ Label ]` (with explicit space padding so the label
/// is visually inset from the brackets). Styling depends on `kind`
/// and `focused`:
///
/// * `Normal`  — default fg; focused → fg/bg flip + bold.
/// * `Primary` — bold; focused → fg/bg flip.
/// * `Danger`  — red fg (theme `ui.status_error_indicator_fg`);
///   focused → bold.
pub fn render_button(label: &str, focused: bool, kind: ButtonKind) -> TextPropertyEntry {
    let text = format!("[ {} ]", label);
    let mut overlays = Vec::new();

    let base_style = match kind {
        ButtonKind::Normal => OverlayOptions::default(),
        ButtonKind::Primary => OverlayOptions {
            bold: true,
            ..Default::default()
        },
        ButtonKind::Danger => OverlayOptions {
            fg: Some(OverlayColorSpec::theme_key(KEY_DANGER_FG)),
            ..Default::default()
        },
    };

    let style = if focused {
        OverlayOptions {
            fg: Some(OverlayColorSpec::theme_key(KEY_FOCUSED_FG)),
            bg: Some(OverlayColorSpec::theme_key(KEY_FOCUSED_BG)),
            bold: true,
            ..base_style
        }
    } else {
        base_style
    };

    // Only emit an overlay if the style is non-default — keeps the
    // serialized entry tight.
    if style.fg.is_some()
        || style.bg.is_some()
        || style.bold
        || style.italic
        || style.underline
        || style.strikethrough
    {
        overlays.push(InlineOverlay {
            start: 0,
            end: text.len(),
            style,
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }

    TextPropertyEntry {
        text,
        properties: Default::default(),
        style: None,
        inline_overlays: overlays,
        segments: Vec::new(),
        pad_to_chars: None,
        truncate_to_chars: None,
    }
}

/// Output of `render_tree_row` — the rendered entry plus the byte
/// range covered by the disclosure glyph (when present) so the
/// caller can emit a separate hit area for click-to-expand.
pub struct RenderedTreeRow {
    pub entry: TextPropertyEntry,
    /// Byte range within `entry.text` of the disclosure glyph
    /// (`▶`/`▼`). `None` for leaf nodes (no glyph rendered).
    pub disclosure_range: Option<(usize, usize)>,
    /// Byte range within `entry.text` of the checkbox glyph
    /// (`[v]` / `[ ]`). `None` when the parent Tree is not
    /// `checkable`, or when this node has `checked: None`. The
    /// caller emits a `toggle` hit area over this range.
    pub checkbox_range: Option<(usize, usize)>,
}

/// Render a single `TreeNode` row.
///
/// Layout: `<indent><disclosure><space>[<checkbox><space>]<node-text>`
/// where:
/// * `indent` = `depth * 2` spaces.
/// * `disclosure` = `▶` (collapsed) / `▼` (expanded) for internal
///   nodes; two spaces (alignment) for leaves.
/// * `checkbox` = `[v]` (checked) / `[ ]` (unchecked) when the
///   parent Tree opted into `checkable: true` *and* this node has
///   `checked: Some(_)`; otherwise omitted entirely.
/// * `<node-text>` is the plugin's pre-rendered row content, with
///   its inline overlays byte-shifted by the prefix length.
///
/// The disclosure glyph is colored with `ui.help_key_fg`; the
/// checkbox glyph reuses `ui.tab_active_fg` (the same key the
/// `Toggle` widget uses for its checked-state glyph) so it reads
/// as a control surface against the row's text.
pub fn render_tree_row(node: &TreeNode, expanded: bool, checkable: bool) -> RenderedTreeRow {
    let indent_cols = (node.depth as usize) * 2;
    let disclosure_glyph: &str = if node.has_children {
        if expanded {
            "▼"
        } else {
            "▶"
        }
    } else {
        // Two spaces — same display width as the glyph plus space,
        // keeping leaf rows aligned with their internal siblings.
        "  "
    };
    // `disclosure_glyph` (▶/▼) is 1 column wide; we want the row
    // text to start at the same column whether or not the row is
    // a leaf. With glyph + one separator space, that's 2 cols. The
    // leaf branch uses two literal spaces for the same width.
    let separator: &str = if node.has_children { " " } else { "" };

    let checkbox_glyph: Option<&'static str> = if checkable {
        match node.checked {
            Some(true) => Some("[v]"),
            Some(false) => Some("[ ]"),
            None => None,
        }
    } else {
        None
    };
    let checkbox_extra = checkbox_glyph.map(|g| g.len() + 1).unwrap_or(0);

    let mut text = String::with_capacity(
        indent_cols
            + disclosure_glyph.len()
            + separator.len()
            + checkbox_extra
            + node.text.text.len(),
    );
    for _ in 0..indent_cols {
        text.push(' ');
    }
    let disc_start = text.len();
    text.push_str(disclosure_glyph);
    let disc_end = text.len();
    text.push_str(separator);
    let checkbox_range = if let Some(g) = checkbox_glyph {
        let cb_start = text.len();
        text.push_str(g);
        let cb_end = text.len();
        text.push(' ');
        Some((cb_start, cb_end))
    } else {
        None
    };
    let body_start = text.len();
    text.push_str(&node.text.text);

    // Carry over the plugin's inline overlays, shifted right by
    // `body_start` so they land on the correct bytes after the
    // prefix.
    let mut overlays: Vec<InlineOverlay> = node
        .text
        .inline_overlays
        .iter()
        .map(|o| {
            let mut shifted = o.clone();
            shifted.start += body_start;
            shifted.end += body_start;
            shifted
        })
        .collect();

    // Disclosure glyph color — only on internal nodes, where the
    // glyph is a real character (not just two spaces).
    if node.has_children {
        overlays.push(InlineOverlay {
            start: disc_start,
            end: disc_end,
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(KEY_HELP_KEY_FG)),
                bold: true,
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }
    // Checkbox glyph color — bright for checked, dim for unchecked,
    // matching the Toggle widget's convention.
    if let Some((cb_start, cb_end)) = checkbox_range {
        let theme_key = match node.checked {
            Some(true) => KEY_TOGGLE_ON_FG,
            _ => KEY_PLACEHOLDER_FG,
        };
        overlays.push(InlineOverlay {
            start: cb_start,
            end: cb_end,
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(theme_key)),
                bold: matches!(node.checked, Some(true)),
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }

    let disclosure_range = if node.has_children {
        Some((disc_start, disc_end))
    } else {
        None
    };
    let entry = TextPropertyEntry {
        text,
        // The plugin's own row-level properties (e.g. file-row
        // metadata) carry through unchanged so existing
        // mouse_click handlers still see them.
        properties: node.text.properties.clone(),
        style: node.text.style.clone(),
        inline_overlays: overlays,
        // segments / pad / truncate hints are consumed by the
        // caller before render_tree_row is invoked (see
        // normalize_widths in the Tree match arm). The output
        // entry's text is already final, so these are cleared.
        segments: Vec::new(),
        pad_to_chars: None,
        truncate_to_chars: None,
    };
    RenderedTreeRow {
        entry,
        disclosure_range,
        checkbox_range,
    }
}

/// Output of `render_text_input` — the rendered entry plus the
/// byte offset within `entry.text` where the host should place the
/// hardware cursor when this input is focused.
pub struct RenderedTextInput {
    pub entry: TextPropertyEntry,
    /// Byte offset within `entry.text` where the cursor lands.
    /// When the input is unfocused or has no cursor, `None`.
    pub cursor_byte_in_entry: Option<usize>,
}

/// Render a `TextInput`.
///
/// Layout: `Label: [<inner>]` (or `[<inner>]` with no label).
/// `<inner>` is exactly `field_width` chars wide when
/// `field_width > 0` — short values pad with trailing spaces, long
/// values head-truncate with `…` so the cursor (typically near the
/// tail) stays visible. With `field_width == 0` the input grows
/// with the value (legacy behaviour, also used by tests).
///
/// Placeholder: when unfocused and empty, the placeholder string
/// is shown in `ui.menu_disabled_fg`. Focused inputs always show
/// their (possibly empty) value, never the placeholder.
///
/// Focused-bg: the bracketed region gets `ui.prompt_bg` so the
/// field visually reads as the active editing target.
///
/// **No cursor overlay**: this renderer does not paint the cursor
/// itself — it returns the byte offset where the host should drop
/// the *real* hardware cursor (the terminal's blinking caret). The
/// dispatcher uses that offset to position
/// `SplitViewState::cursors.primary` and flip `show_cursors=true`
/// on the panel buffer. Result: the cursor is always visible
/// regardless of theme contrast, blinks correctly, and matches
/// every other text-input field in the editor.
pub fn render_text_input(
    value: &str,
    cursor_byte: i32,
    focused: bool,
    label: &str,
    placeholder: Option<&str>,
    max_visible_chars: u32,
    field_width: u32,
) -> RenderedTextInput {
    let show_placeholder = !focused && value.is_empty() && placeholder.is_some();

    // Compute the user-cursor's char position within `value`. We
    // operate in bytes here, which is correct for the cursor on
    // ASCII; multibyte chars resolve via is_char_boundary checks.
    let raw_cursor_byte = if cursor_byte < 0 {
        value.len()
    } else {
        (cursor_byte as usize).min(value.len())
    };

    // Build `<inner>` plus the byte offset of the cursor *within*
    // `<inner>` (not yet including `[`/label offsets). This is the
    // single place where field-width truncation/padding lives.
    let (inner, cursor_in_inner) = if show_placeholder {
        // Placeholder doesn't carry a cursor (never focused here).
        (placeholder.unwrap_or("").to_string(), None)
    } else if field_width > 0 {
        // Constant-width. Visible value occupies `target` chars;
        // when focused we add one trailing pad space so the cursor
        // never lands on the closing bracket. Result inner width:
        //   focused   → target + 1
        //   unfocused → target
        let target = field_width as usize;
        let pad_extra = if focused { 1 } else { 0 };
        let total_inner = target + pad_extra;
        let value_chars: Vec<char> = value.chars().collect();
        if value_chars.len() <= target {
            // Short or exact-fit value: pad with trailing spaces
            // to total_inner. Cursor at byte k of value lands at
            // byte k of inner.
            let mut padded = value.to_string();
            while padded.chars().count() < total_inner {
                padded.push(' ');
            }
            (padded, Some(raw_cursor_byte))
        } else {
            // Long value: head-truncate to fit `target - 1` value
            // chars + 1 ellipsis. When focused, append a trailing
            // pad space (cursor parks there at end-of-value).
            let keep = target - 1;
            let drop_chars = value_chars.len() - keep;
            let mut dropped_bytes = 0usize;
            for ch in value_chars.iter().take(drop_chars) {
                dropped_bytes += ch.len_utf8();
            }
            let tail = &value[dropped_bytes..];
            let mut s = String::with_capacity("…".len() + tail.len() + pad_extra);
            s.push('…');
            s.push_str(tail);
            for _ in 0..pad_extra {
                s.push(' ');
            }
            // Cursor: if it sits in the dropped prefix, clamp to
            // right after the `…` glyph; otherwise translate
            // through the truncation.
            let cursor_in_inner = if raw_cursor_byte < dropped_bytes {
                "…".len()
            } else {
                "…".len() + (raw_cursor_byte - dropped_bytes)
            };
            (s, Some(cursor_in_inner))
        }
    } else if max_visible_chars > 0 && value.chars().count() > max_visible_chars as usize {
        // Legacy max_visible_chars path: tail-truncate with `…`
        // (drops the *tail*, not the head — matches the original
        // cursor-invisible v1 behaviour for callers still using it).
        let chars: Vec<char> = value.chars().collect();
        let take = (max_visible_chars as usize).saturating_sub(1);
        let start = chars.len().saturating_sub(take);
        let tail: String = chars[start..].iter().collect();
        let s = format!("…{}", tail);
        (s, Some(raw_cursor_byte.min(value.len())))
    } else {
        // No fixed width and no truncation: render the value as-is.
        // When focused we still need somewhere for the cursor to
        // land at end-of-value — append a trailing space so the
        // cursor sits on it instead of overlapping the closing
        // bracket.
        let mut s = value.to_string();
        if focused {
            s.push(' ');
        }
        (s, Some(raw_cursor_byte))
    };

    // Compose the final text: optional label, `[`, inner, `]`.
    let mut text = String::new();
    if !label.is_empty() {
        text.push_str(label);
        text.push(' ');
    }
    let bracket_open_byte = text.len();
    text.push('[');
    let inner_byte_start = text.len();
    text.push_str(&inner);
    let inner_byte_end = text.len();
    text.push(']');
    let bracket_close_byte = text.len();

    let mut overlays = Vec::new();

    if show_placeholder {
        overlays.push(InlineOverlay {
            start: inner_byte_start,
            end: inner_byte_end,
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(KEY_PLACEHOLDER_FG)),
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }

    if focused {
        overlays.push(InlineOverlay {
            start: bracket_open_byte,
            end: bracket_close_byte,
            style: OverlayOptions {
                bg: Some(OverlayColorSpec::theme_key(KEY_INPUT_BG)),
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }

    let cursor_byte_in_entry = if focused {
        cursor_in_inner.map(|c| inner_byte_start + c)
    } else {
        None
    };

    RenderedTextInput {
        entry: TextPropertyEntry {
            text,
            properties: Default::default(),
            style: None,
            inline_overlays: overlays,
            segments: Vec::new(),
            pad_to_chars: None,
            truncate_to_chars: None,
        },
        cursor_byte_in_entry,
    }
}

/// Output of `render_text_area`. One entry per visible row of the
/// editing region, plus optionally one preceding label row.
pub struct RenderedTextArea {
    /// The label row (if any) followed by `visible_rows` rows of
    /// editing content. Empty `value` lines are rendered as blank
    /// padded rows so the widget always occupies its full visual
    /// height.
    pub entries: Vec<TextPropertyEntry>,
    /// Auto-clamped scroll row (first visible line of `value`)
    /// after this render. Persisted into instance state by the
    /// caller.
    pub scroll_row: u32,
    /// Buffer row (within `entries`) where the host should drop
    /// the hardware cursor when focused. `None` when unfocused or
    /// when `value` is empty and the placeholder is showing.
    pub cursor_buffer_row: Option<u32>,
    /// Byte offset within the cursor's row text where the cursor
    /// lands. Pairs with `cursor_buffer_row`.
    pub cursor_byte_in_row: Option<usize>,
}

/// Render a multi-line `TextArea`.
///
/// Layout:
/// * If `label` is non-empty, one `Label:` row precedes the editing
///   region.
/// * Then exactly `visible_rows` rows of editing content. Lines of
///   `value` between `[scroll_row, scroll_row + visible_rows)` are
///   rendered; rows beyond the value are blanks (padded so the
///   editing region's input-bg block keeps its rectangular shape).
/// * The editing region uses `field_width` columns when set; `0`
///   means "use up to `panel_width`". Long lines are truncated with
///   `…` at the right when they exceed the field width — this is
///   different from `TextInput`'s head-truncation, because the
///   cursor is no longer pinned to end-of-value (it can be
///   anywhere within multi-line content).
/// * When focused, every visible content row gets the
///   `ui.prompt_bg` overlay extended to the field width so the
///   editing region reads as a single block.
/// * Placeholder: shown on the *first* row only when unfocused and
///   `value` is empty.
///
/// Cursor: returns the visible row index (relative to `entries`)
/// and byte offset within that row's text. The auto-clamp policy:
/// keep the cursor's line in view by adjusting `scroll_row` when
/// the cursor's line falls outside `[scroll_row, scroll_row +
/// visible_rows)`.
#[allow(clippy::too_many_arguments)]
pub fn render_text_area(
    value: &str,
    cursor_byte: i32,
    focused: bool,
    label: &str,
    placeholder: Option<&str>,
    visible_rows: u32,
    field_width: u32,
    prev_scroll: u32,
    panel_width: u32,
) -> RenderedTextArea {
    // Resolve effective field width: caller's value if set, else
    // `panel_width` (or a small default if the panel is unsized).
    let target_width: usize = if field_width > 0 {
        field_width as usize
    } else if panel_width != u32::MAX && panel_width > 0 {
        panel_width as usize
    } else {
        40
    };

    // Split value into lines (without the `\n`). Empty value still
    // produces one (empty) line — matching how a single-line
    // editor would treat an empty buffer.
    let mut lines: Vec<&str> = value.split('\n').collect();
    if lines.is_empty() {
        lines.push("");
    }

    // Cursor → (line_index, byte_in_line). When `cursor_byte` is
    // negative (no cursor), we still compute a line for scroll
    // bookkeeping but don't emit a focus_cursor.
    let raw_cursor_byte = if cursor_byte < 0 {
        value.len()
    } else {
        (cursor_byte as usize).min(value.len())
    };
    let (cursor_line, cursor_col) = byte_to_line_col(value, raw_cursor_byte);

    // Auto-clamp scroll: keep cursor's line in [scroll_row,
    // scroll_row + visible_rows). On first render, prev_scroll == 0.
    let visible_rows_usize = visible_rows.max(1) as usize;
    let mut scroll_row = prev_scroll as usize;
    if cursor_line < scroll_row {
        scroll_row = cursor_line;
    } else if cursor_line >= scroll_row + visible_rows_usize {
        scroll_row = cursor_line + 1 - visible_rows_usize;
    }
    // Don't scroll past the last line.
    let max_scroll = lines.len().saturating_sub(visible_rows_usize);
    if scroll_row > max_scroll {
        scroll_row = max_scroll;
    }

    let show_placeholder =
        !focused && value.is_empty() && placeholder.is_some() && !placeholder.unwrap().is_empty();

    let mut entries: Vec<TextPropertyEntry> = Vec::new();
    let mut cursor_buffer_row: Option<u32> = None;
    let mut cursor_byte_in_row: Option<usize> = None;

    if !label.is_empty() {
        let mut text = String::with_capacity(label.len() + 2);
        text.push_str(label);
        text.push(':');
        entries.push(TextPropertyEntry {
            text,
            properties: Default::default(),
            style: None,
            inline_overlays: Vec::new(),
            segments: Vec::new(),
            pad_to_chars: None,
            truncate_to_chars: None,
        });
    }
    let label_offset: u32 = entries.len() as u32;

    for row_in_view in 0..visible_rows_usize {
        let line_idx = scroll_row + row_in_view;
        let mut row_text;
        let mut overlays: Vec<InlineOverlay> = Vec::new();

        if line_idx < lines.len() {
            row_text = pad_or_truncate_line(lines[line_idx], target_width);
        } else {
            row_text = " ".repeat(target_width);
        }

        // Placeholder shows on the first row only.
        if show_placeholder && row_in_view == 0 {
            let ph = placeholder.unwrap();
            row_text = pad_or_truncate_line(ph, target_width);
            overlays.push(InlineOverlay {
                start: 0,
                end: row_text.len(),
                style: OverlayOptions {
                    fg: Some(OverlayColorSpec::theme_key(KEY_PLACEHOLDER_FG)),
                    ..Default::default()
                },
                properties: Default::default(),
                unit: OffsetUnit::Byte,
            });
        }

        // Focused-bg covers the full row width — the editing
        // region reads as a single block.
        if focused {
            overlays.push(InlineOverlay {
                start: 0,
                end: row_text.len(),
                style: OverlayOptions {
                    bg: Some(OverlayColorSpec::theme_key(KEY_INPUT_BG)),
                    ..Default::default()
                },
                properties: Default::default(),
                unit: OffsetUnit::Byte,
            });
        }

        // Drop the cursor on this row if it matches.
        if focused && line_idx == cursor_line && cursor_byte >= 0 {
            // The cursor's byte column on its line. If the line was
            // truncated, the cursor may have shifted past the
            // visible region — clamp to the last visible byte so
            // the hardware cursor stays in the row.
            let col_in_line = cursor_col.min(row_text.len());
            cursor_buffer_row = Some(label_offset + row_in_view as u32);
            cursor_byte_in_row = Some(col_in_line);
        }

        entries.push(TextPropertyEntry {
            text: row_text,
            properties: Default::default(),
            style: None,
            inline_overlays: overlays,
            segments: Vec::new(),
            pad_to_chars: None,
            truncate_to_chars: None,
        });
    }

    RenderedTextArea {
        entries,
        scroll_row: scroll_row as u32,
        cursor_buffer_row,
        cursor_byte_in_row,
    }
}

/// Translate a byte offset in `value` to (line_index, byte_in_line).
fn byte_to_line_col(value: &str, byte: usize) -> (usize, usize) {
    let byte = byte.min(value.len());
    let mut line = 0usize;
    let mut line_start = 0usize;
    for (i, &b) in value.as_bytes().iter().enumerate().take(byte) {
        if b == b'\n' {
            line += 1;
            line_start = i + 1;
        }
    }
    (line, byte - line_start)
}

/// Pad `line` with trailing spaces to `target` chars, or
/// tail-truncate with `…` if it overflows. Operates on chars to keep
/// the visual width predictable for ASCII; multibyte chars count as
/// one char each (terminal column width != char count for CJK, but
/// that's an acceptable v1 limitation matching `TextInput`).
fn pad_or_truncate_line(line: &str, target: usize) -> String {
    let chars: Vec<char> = line.chars().collect();
    if chars.len() <= target {
        let mut out = line.to_string();
        let pad = target - chars.len();
        for _ in 0..pad {
            out.push(' ');
        }
        out
    } else {
        let keep = target.saturating_sub(1);
        let mut out: String = chars.iter().take(keep).collect();
        out.push('…');
        out
    }
}

/// Merge `next` into `merged` for the inline-row collapse path.
/// `next`'s overlays are byte-shifted to account for the merged
/// text length so far.
fn merge_inline(merged: &mut TextPropertyEntry, next: &mut TextPropertyEntry) {
    let shift = merged.text.len();
    merged.text.push_str(&next.text);
    for overlay in next.inline_overlays.drain(..) {
        merged.inline_overlays.push(InlineOverlay {
            start: overlay.start + shift,
            end: overlay.end + shift,
            style: overlay.style,
            properties: overlay.properties,
            unit: overlay.unit,
        });
    }
    // `style` and `properties` from `next` are dropped — Row inline
    // collapse only preserves inline_overlays. Whole-entry style on
    // an inline-row child has no meaningful semantics here; if a
    // plugin needs whole-line styling it should produce a Col with
    // the styled child as its sole element.
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Most existing tests don't care about the new focus_key /
    /// tabbable fields. Wrap the no-focus-needed render path so
    /// they keep destructuring a 3-tuple; new tests destructure
    /// `RenderOutput` directly.
    fn render_no_focus(
        spec: &WidgetSpec,
        prev: &HashMap<String, WidgetInstanceState>,
    ) -> (
        Vec<TextPropertyEntry>,
        Vec<HitArea>,
        HashMap<String, WidgetInstanceState>,
    ) {
        // u32::MAX disables flex sizing (no leftover to distribute).
        let out = render_spec(spec, prev, "", u32::MAX);
        (out.entries, out.hits, out.instance_states)
    }

    #[test]
    fn hint_bar_renders_entries_with_key_overlays() {
        let entries = vec![
            HintEntry {
                keys: "Tab".into(),
                label: "next".into(),
            },
            HintEntry {
                keys: "Esc".into(),
                label: "close".into(),
            },
        ];
        let entry = render_hint_bar(&entries);
        assert_eq!(entry.text, "Tab next  Esc close");
        assert_eq!(entry.inline_overlays.len(), 2);
        // First overlay covers "Tab" (bytes 0..3).
        assert_eq!(entry.inline_overlays[0].start, 0);
        assert_eq!(entry.inline_overlays[0].end, 3);
        // Second overlay covers "Esc" (bytes 10..13).
        assert_eq!(entry.inline_overlays[1].start, 10);
        assert_eq!(entry.inline_overlays[1].end, 13);
    }

    #[test]
    fn hint_bar_omits_label_when_empty() {
        let entries = vec![HintEntry {
            keys: "?".into(),
            label: "".into(),
        }];
        let entry = render_hint_bar(&entries);
        assert_eq!(entry.text, "?");
    }

    #[test]
    fn col_stacks_children_top_to_bottom() {
        let spec = WidgetSpec::Col {
            children: vec![
                WidgetSpec::HintBar {
                    entries: vec![HintEntry {
                        keys: "A".into(),
                        label: "alpha".into(),
                    }],
                    key: None,
                },
                WidgetSpec::HintBar {
                    entries: vec![HintEntry {
                        keys: "B".into(),
                        label: "beta".into(),
                    }],
                    key: None,
                },
            ],
            key: None,
        };
        let (out, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(out.len(), 2);
        assert_eq!(out[0].text, "A alpha\n");
        assert_eq!(out[1].text, "B beta\n");
        assert!(hits.is_empty(), "HintBar emits no hit areas in v1");
    }

    #[test]
    fn raw_passes_through_unchanged() {
        let spec = WidgetSpec::Raw {
            entries: vec![TextPropertyEntry::text("hello")],
            key: None,
        };
        let (out, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].text, "hello\n");
        assert!(hits.is_empty());
    }

    #[test]
    fn toggle_checked_emits_glyph_overlay() {
        let entry = render_toggle(true, "Case", false);
        assert_eq!(entry.text, "[v] Case");
        // One overlay for the glyph, no focused overlay.
        assert_eq!(entry.inline_overlays.len(), 1);
        assert_eq!(entry.inline_overlays[0].start, 0);
        assert_eq!(entry.inline_overlays[0].end, 3);
    }

    #[test]
    fn toggle_unchecked_no_glyph_overlay() {
        let entry = render_toggle(false, "Case", false);
        assert_eq!(entry.text, "[ ] Case");
        assert_eq!(entry.inline_overlays.len(), 0);
    }

    #[test]
    fn toggle_focused_adds_full_entry_overlay() {
        let entry = render_toggle(true, "Case", true);
        // Glyph overlay + focused overlay.
        assert_eq!(entry.inline_overlays.len(), 2);
        // Focused overlay spans the full entry.
        assert_eq!(entry.inline_overlays[1].start, 0);
        assert_eq!(entry.inline_overlays[1].end, entry.text.len());
        assert!(entry.inline_overlays[1].style.bold);
    }

    #[test]
    fn button_normal_unfocused_has_no_overlay() {
        let entry = render_button("Replace All", false, ButtonKind::Normal);
        assert_eq!(entry.text, "[ Replace All ]");
        assert!(entry.inline_overlays.is_empty());
    }

    #[test]
    fn button_primary_is_bold() {
        let entry = render_button("Submit", false, ButtonKind::Primary);
        assert_eq!(entry.inline_overlays.len(), 1);
        assert!(entry.inline_overlays[0].style.bold);
    }

    #[test]
    fn button_danger_uses_error_theme_key() {
        let entry = render_button("Delete", false, ButtonKind::Danger);
        assert_eq!(entry.inline_overlays.len(), 1);
        let fg = entry.inline_overlays[0].style.fg.as_ref().unwrap();
        assert_eq!(fg.as_theme_key(), Some("ui.status_error_indicator_fg"));
    }

    #[test]
    fn button_focused_overrides_with_menu_active_keys() {
        let entry = render_button("OK", true, ButtonKind::Normal);
        let style = &entry.inline_overlays[0].style;
        assert_eq!(
            style.fg.as_ref().and_then(|c| c.as_theme_key()),
            Some("ui.menu_active_fg")
        );
        assert_eq!(
            style.bg.as_ref().and_then(|c| c.as_theme_key()),
            Some("ui.menu_active_bg")
        );
        assert!(style.bold);
    }

    #[test]
    fn flex_spacer_fills_remaining_row_width() {
        let spec = WidgetSpec::Row {
            children: vec![
                WidgetSpec::Toggle {
                    checked: false,
                    label: "A".into(),
                    focused: false,
                    key: None,
                },
                WidgetSpec::Spacer {
                    cols: 0,
                    flex: true,
                    key: None,
                },
                WidgetSpec::Button {
                    label: "B".into(),
                    focused: false,
                    intent: ButtonKind::Normal,
                    key: None,
                },
            ],
            key: None,
        };
        // Toggle "[ ] A" = 5 bytes; Button "[ B ]" = 5 bytes;
        // panel_width = 30 → flex fills 20 spaces. Plus a trailing
        // newline added by the Row's terminator.
        let out = render_spec(&spec, &HashMap::new(), "", 30);
        assert_eq!(out.entries.len(), 1);
        let text = &out.entries[0].text;
        assert_eq!(text.len(), 31);
        assert!(text.starts_with("[ ] A"));
        assert!(text.ends_with("[ B ]\n"));
        let button_hit = out.hits.iter().find(|h| h.widget_kind == "button").unwrap();
        assert_eq!(button_hit.byte_start, 25);
        assert_eq!(button_hit.byte_end, 30);
    }

    #[test]
    fn flex_spacer_with_no_leftover_collapses_to_zero() {
        let spec = WidgetSpec::Row {
            children: vec![
                WidgetSpec::Toggle {
                    checked: false,
                    label: "A".into(),
                    focused: false,
                    key: None,
                },
                WidgetSpec::Spacer {
                    cols: 0,
                    flex: true,
                    key: None,
                },
                WidgetSpec::Toggle {
                    checked: false,
                    label: "B".into(),
                    focused: false,
                    key: None,
                },
            ],
            key: None,
        };
        // Both toggles use 5+5=10 bytes; panel_width=10 → flex=0.
        let out = render_spec(&spec, &HashMap::new(), "", 10);
        assert_eq!(out.entries[0].text, "[ ] A[ ] B\n");
    }

    #[test]
    fn spacer_in_row_pads_with_spaces() {
        let spec = WidgetSpec::Row {
            children: vec![
                WidgetSpec::Toggle {
                    checked: false,
                    label: "A".into(),
                    focused: false,
                    key: None,
                },
                WidgetSpec::Spacer {
                    cols: 4,
                    flex: false,
                    key: None,
                },
                WidgetSpec::Button {
                    label: "Go".into(),
                    focused: false,
                    intent: ButtonKind::Normal,
                    key: None,
                },
            ],
            key: None,
        };
        let (out, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].text, "[ ] A    [ Go ]\n");
    }

    #[test]
    fn row_collapses_inline_children_with_shifted_overlays() {
        let spec = WidgetSpec::Row {
            children: vec![
                WidgetSpec::HintBar {
                    entries: vec![HintEntry {
                        keys: "Tab".into(),
                        label: "x".into(),
                    }],
                    key: None,
                },
                WidgetSpec::HintBar {
                    entries: vec![HintEntry {
                        keys: "Esc".into(),
                        label: "y".into(),
                    }],
                    key: None,
                },
            ],
            key: None,
        };
        let (out, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(out.len(), 1);
        // Two adjacent HintBars are concatenated; the second's overlay shifts.
        assert_eq!(out[0].text, "Tab xEsc y\n");
        assert_eq!(out[0].inline_overlays.len(), 2);
        assert_eq!(out[0].inline_overlays[1].start, 5);
        assert_eq!(out[0].inline_overlays[1].end, 8);
    }

    // -------------------------------------------------------------
    // Hit-area tests
    // -------------------------------------------------------------

    #[test]
    fn toggle_emits_hit_area_with_toggle_payload() {
        let spec = WidgetSpec::Toggle {
            checked: false,
            label: "Case".into(),
            focused: false,
            key: Some("case".into()),
        };
        let (_entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(hits.len(), 1);
        let h = &hits[0];
        assert_eq!(h.widget_key, "case");
        assert_eq!(h.widget_kind, "toggle");
        assert_eq!(h.event_type, "toggle");
        assert_eq!(h.buffer_row, 0);
        assert_eq!(h.byte_start, 0);
        assert_eq!(h.byte_end, "[ ] Case".len());
        assert_eq!(h.payload, json!({"checked": true}));
    }

    #[test]
    fn button_emits_hit_area_with_activate_payload() {
        let spec = WidgetSpec::Button {
            label: "Replace All".into(),
            focused: false,
            intent: ButtonKind::Primary,
            key: Some("replace".into()),
        };
        let (_entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(hits.len(), 1);
        let h = &hits[0];
        assert_eq!(h.widget_key, "replace");
        assert_eq!(h.widget_kind, "button");
        assert_eq!(h.event_type, "activate");
        assert_eq!(h.byte_end, "[ Replace All ]".len());
        assert_eq!(h.payload, json!({}));
    }

    #[test]
    fn row_inline_collapse_shifts_hit_byte_offsets() {
        let spec = WidgetSpec::Row {
            children: vec![
                WidgetSpec::Toggle {
                    checked: true,
                    label: "A".into(),
                    focused: false,
                    key: Some("a".into()),
                },
                WidgetSpec::Spacer {
                    cols: 2,
                    flex: false,
                    key: None,
                },
                WidgetSpec::Toggle {
                    checked: false,
                    label: "B".into(),
                    focused: false,
                    key: Some("b".into()),
                },
            ],
            key: None,
        };
        let (entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        // One merged row with text "[v] A  [ ] B"
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].text, "[v] A  [ ] B\n");
        assert_eq!(hits.len(), 2);
        assert_eq!(hits[0].widget_key, "a");
        assert_eq!(hits[0].buffer_row, 0);
        assert_eq!(hits[0].byte_start, 0);
        assert_eq!(hits[0].byte_end, 5); // "[v] A".len()
                                         // Second toggle shifts past first toggle ("[v] A".len() = 5)
                                         // + spacer ("  ".len() = 2) = 7.
        assert_eq!(hits[1].widget_key, "b");
        assert_eq!(hits[1].buffer_row, 0);
        assert_eq!(hits[1].byte_start, 7);
        assert_eq!(hits[1].byte_end, 12);
    }

    #[test]
    fn col_stacks_hit_rows() {
        let spec = WidgetSpec::Col {
            children: vec![
                WidgetSpec::Toggle {
                    checked: false,
                    label: "row0".into(),
                    focused: false,
                    key: Some("k0".into()),
                },
                WidgetSpec::Toggle {
                    checked: true,
                    label: "row1".into(),
                    focused: false,
                    key: Some("k1".into()),
                },
            ],
            key: None,
        };
        let (_entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(hits.len(), 2);
        assert_eq!(hits[0].buffer_row, 0);
        assert_eq!(hits[1].buffer_row, 1);
    }

    // -------------------------------------------------------------
    // Focus management
    // -------------------------------------------------------------

    #[test]
    fn collect_tabbable_visits_widgets_with_keys_in_declaration_order() {
        let spec = WidgetSpec::Col {
            children: vec![
                WidgetSpec::HintBar {
                    entries: vec![],
                    key: Some("hb".into()),
                },
                WidgetSpec::Row {
                    children: vec![
                        WidgetSpec::Toggle {
                            checked: false,
                            label: "T".into(),
                            focused: false,
                            key: Some("t".into()),
                        },
                        WidgetSpec::Spacer {
                            cols: 1,
                            flex: false,
                            key: None,
                        },
                        WidgetSpec::Button {
                            label: "B".into(),
                            focused: false,
                            intent: ButtonKind::Normal,
                            key: Some("b".into()),
                        },
                    ],
                    key: None,
                },
                WidgetSpec::Text {
                    value: "".into(),
                    cursor_byte: -1,
                    focused: false,
                    label: "".into(),
                    placeholder: None,
                    rows: 1,
                    field_width: 0,
                    max_visible_chars: 0,
                    key: Some("ti".into()),
                },
                WidgetSpec::Toggle {
                    checked: false,
                    label: "no key".into(),
                    focused: false,
                    key: None,
                },
            ],
            key: None,
        };
        let mut tabbable = Vec::new();
        collect_tabbable(&spec, &mut tabbable);
        // HintBar without a key isn't tabbable; tabbables are
        // Toggle/Button/TextInput/List with non-empty keys.
        assert_eq!(tabbable, vec!["t", "b", "ti"]);
    }

    #[test]
    fn first_render_focuses_first_tabbable() {
        let spec = WidgetSpec::Row {
            children: vec![
                WidgetSpec::Toggle {
                    checked: false,
                    label: "A".into(),
                    focused: false,
                    key: Some("a".into()),
                },
                WidgetSpec::Toggle {
                    checked: false,
                    label: "B".into(),
                    focused: false,
                    key: Some("b".into()),
                },
            ],
            key: None,
        };
        let out = render_spec(&spec, &HashMap::new(), "", u32::MAX);
        assert_eq!(out.focus_key, "a");
        assert_eq!(out.tabbable, vec!["a", "b"]);
    }

    #[test]
    fn render_preserves_focus_key_across_re_renders() {
        let spec = WidgetSpec::Row {
            children: vec![
                WidgetSpec::Toggle {
                    checked: false,
                    label: "A".into(),
                    focused: false,
                    key: Some("a".into()),
                },
                WidgetSpec::Toggle {
                    checked: false,
                    label: "B".into(),
                    focused: false,
                    key: Some("b".into()),
                },
            ],
            key: None,
        };
        let out = render_spec(&spec, &HashMap::new(), "b", u32::MAX);
        assert_eq!(out.focus_key, "b");
    }

    #[test]
    fn render_clamps_stale_focus_key_to_first_tabbable() {
        // Previous render focused "stale", but the new spec doesn't
        // have any widget with that key — fall back to the first
        // tabbable.
        let spec = WidgetSpec::Toggle {
            checked: false,
            label: "Only".into(),
            focused: false,
            key: Some("only".into()),
        };
        let out = render_spec(&spec, &HashMap::new(), "stale", u32::MAX);
        assert_eq!(out.focus_key, "only");
    }

    #[test]
    fn focused_widget_renders_with_focused_styling() {
        let spec = WidgetSpec::Row {
            children: vec![
                WidgetSpec::Toggle {
                    checked: false,
                    label: "A".into(),
                    focused: false,
                    key: Some("a".into()),
                },
                WidgetSpec::Toggle {
                    checked: false,
                    label: "B".into(),
                    focused: false,
                    key: Some("b".into()),
                },
            ],
            key: None,
        };
        let out = render_spec(&spec, &HashMap::new(), "b", u32::MAX);
        assert_eq!(out.entries.len(), 1, "row collapses inline");
        // Two overlays expected from the focused B: one for B's
        // glyph (none, since unchecked) — actually unchecked emits
        // no glyph overlay. So only the focused-style overlay.
        // Find the focused overlay by its menu_active_bg key.
        let entry = &out.entries[0];
        let focused_overlay = entry
            .inline_overlays
            .iter()
            .find(|o| {
                o.style.bg.as_ref().and_then(|c| c.as_theme_key()) == Some("ui.menu_active_bg")
            })
            .expect("focused overlay present on B");
        // B's text is "[ ] B", starting after "[ ] A".len()==5 + spacer 0 (no spacer here).
        // Inline collapse: A is "[ ] A" then immediately "[ ] B" = 10 bytes.
        assert_eq!(focused_overlay.start, 5);
        assert_eq!(focused_overlay.end, 10);
    }

    #[test]
    fn no_tabbables_yields_empty_focus_key() {
        let spec = WidgetSpec::Col {
            children: vec![WidgetSpec::HintBar {
                entries: vec![],
                key: None,
            }],
            key: None,
        };
        let out = render_spec(&spec, &HashMap::new(), "", u32::MAX);
        assert_eq!(out.focus_key, "");
        assert!(out.tabbable.is_empty());
    }

    // -------------------------------------------------------------
    // List
    // -------------------------------------------------------------

    #[test]
    fn list_emits_one_entry_and_one_hit_per_item() {
        let spec = WidgetSpec::List {
            items: vec![
                TextPropertyEntry::text("alpha"),
                TextPropertyEntry::text("beta"),
                TextPropertyEntry::text("gamma"),
            ],
            item_keys: vec!["a".into(), "b".into(), "c".into()],
            selected_index: -1,
            visible_rows: 10,
            key: None,
        };
        let (entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(entries.len(), 3);
        assert_eq!(hits.len(), 3);
        for (i, h) in hits.iter().enumerate() {
            assert_eq!(h.buffer_row, i as u32);
            assert_eq!(h.widget_kind, "list");
            assert_eq!(h.event_type, "select");
            assert_eq!(h.payload["index"], i);
        }
        assert_eq!(hits[0].widget_key, "a");
        assert_eq!(hits[2].widget_key, "c");
    }

    #[test]
    fn list_applies_selection_bg_to_selected_row() {
        let spec = WidgetSpec::List {
            items: vec![
                TextPropertyEntry::text("first"),
                TextPropertyEntry::text("second"),
            ],
            item_keys: vec!["x".into(), "y".into()],
            selected_index: 1,
            visible_rows: 10,
            key: None,
        };
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert!(entries[0].style.is_none(), "unselected row keeps no style");
        let style = entries[1].style.as_ref().expect("selected row gets style");
        assert_eq!(
            style.bg.as_ref().and_then(|c| c.as_theme_key()),
            Some("ui.menu_active_bg"),
        );
        assert!(style.extend_to_line_end);
    }

    #[test]
    fn list_inside_col_offsets_hit_rows_by_preceding_lines() {
        let spec = WidgetSpec::Col {
            children: vec![
                WidgetSpec::HintBar {
                    entries: vec![HintEntry {
                        keys: "h".into(),
                        label: "header".into(),
                    }],
                    key: None,
                },
                WidgetSpec::List {
                    items: vec![
                        TextPropertyEntry::text("row0"),
                        TextPropertyEntry::text("row1"),
                    ],
                    item_keys: vec!["a".into(), "b".into()],
                    selected_index: -1,
                    visible_rows: 10,
                    key: None,
                },
            ],
            key: None,
        };
        let (entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(entries.len(), 3);
        assert_eq!(hits.len(), 2);
        // List rows land at buffer_row 1 and 2 (after the HintBar).
        assert_eq!(hits[0].buffer_row, 1);
        assert_eq!(hits[1].buffer_row, 2);
    }

    #[test]
    fn list_payload_includes_absolute_index_and_key() {
        let spec = WidgetSpec::List {
            items: vec![TextPropertyEntry::text("only")],
            item_keys: vec!["match:42".into()],
            selected_index: 0,
            visible_rows: 10,
            key: None,
        };
        let (_entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(hits[0].payload["index"], 0);
        assert_eq!(hits[0].payload["key"], "match:42");
    }

    #[test]
    fn list_with_missing_key_emits_empty_widget_key() {
        let spec = WidgetSpec::List {
            items: vec![TextPropertyEntry::text("a"), TextPropertyEntry::text("b")],
            // Only one key for two items — second hit gets an empty key.
            item_keys: vec!["only".into()],
            selected_index: -1,
            visible_rows: 10,
            key: None,
        };
        let (_, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(hits[0].widget_key, "only");
        assert_eq!(hits[1].widget_key, "");
    }

    fn make_list(selected: i32, visible: u32, total: usize, key: Option<&str>) -> WidgetSpec {
        let items = (0..total)
            .map(|i| TextPropertyEntry::text(format!("row{}", i)))
            .collect();
        let item_keys = (0..total).map(|i| format!("k{}", i)).collect();
        WidgetSpec::List {
            items,
            item_keys,
            selected_index: selected,
            visible_rows: visible,
            key: key.map(|s| s.to_string()),
        }
    }

    #[test]
    fn list_renders_only_visible_window() {
        let spec = make_list(-1, 3, 10, Some("L"));
        let (entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(entries.len(), 3);
        assert_eq!(hits.len(), 3);
        // First three items, absolute indices 0..2.
        assert_eq!(hits[0].payload["index"], 0);
        assert_eq!(hits[2].payload["index"], 2);
    }

    #[test]
    fn list_scrolls_to_keep_selected_below_window_in_view() {
        // 10 items, visible=3, select index 5: scroll should be 3
        // (so selected lands at the bottom of the window). On
        // *first* render (empty prev), the spec's selected_index
        // seeds instance state.
        let spec = make_list(5, 3, 10, Some("L"));
        let (_entries, hits, state) = render_no_focus(&spec, &HashMap::new());
        // Visible window is items 3..6 → hits index 3, 4, 5.
        assert_eq!(hits.len(), 3);
        assert_eq!(hits[0].payload["index"], 3);
        assert_eq!(hits[2].payload["index"], 5);
        let scroll = match state.get("L").unwrap() {
            WidgetInstanceState::List { scroll_offset, .. } => *scroll_offset,
            _ => unreachable!(),
        };
        assert_eq!(scroll, 3);
    }

    #[test]
    fn list_scrolls_to_keep_selected_above_window_in_view() {
        // Previous render scrolled to 5 with selection at 5; user
        // pressed Up enough times that select_move set instance
        // state's selection to 1; renderer should scroll back up
        // to 1. (Spec's selected_index is initial-only; instance
        // state is authoritative once present.)
        let mut prev = HashMap::new();
        prev.insert(
            "L".into(),
            WidgetInstanceState::List {
                scroll_offset: 5,
                selected_index: 1,
            },
        );
        // Spec's selected_index doesn't matter (instance state wins).
        let spec = make_list(99, 3, 10, Some("L"));
        let (_entries, hits, state) = render_no_focus(&spec, &prev);
        assert_eq!(hits[0].payload["index"], 1);
        let scroll = match state.get("L").unwrap() {
            WidgetInstanceState::List { scroll_offset, .. } => *scroll_offset,
            _ => unreachable!(),
        };
        assert_eq!(scroll, 1);
    }

    #[test]
    fn list_scroll_preserved_when_selection_remains_in_view() {
        // Previous render scrolled to 4 with selection at 4; user
        // moved selection to 5 (still in window 4..6); scroll stays.
        let mut prev = HashMap::new();
        prev.insert(
            "L".into(),
            WidgetInstanceState::List {
                scroll_offset: 4,
                selected_index: 5,
            },
        );
        let spec = make_list(99, 3, 10, Some("L"));
        let (_entries, hits, state) = render_no_focus(&spec, &prev);
        assert_eq!(hits[0].payload["index"], 4);
        let scroll = match state.get("L").unwrap() {
            WidgetInstanceState::List { scroll_offset, .. } => *scroll_offset,
            _ => unreachable!(),
        };
        assert_eq!(scroll, 4);
    }

    #[test]
    fn list_clamps_scroll_to_max_when_dataset_is_smaller_than_old_offset() {
        // Previous scroll past the end of a now-shorter dataset
        // clamps to max_scroll = total - visible.
        let mut prev = HashMap::new();
        prev.insert(
            "L".into(),
            WidgetInstanceState::List {
                scroll_offset: 8,
                selected_index: -1,
            },
        );
        let spec = make_list(-1, 3, 5, Some("L"));
        let (entries, _hits, state) = render_no_focus(&spec, &prev);
        assert_eq!(entries.len(), 3);
        let scroll = match state.get("L").unwrap() {
            WidgetInstanceState::List { scroll_offset, .. } => *scroll_offset,
            _ => unreachable!(),
        };
        // total=5, visible=3 → max=2.
        assert_eq!(scroll, 2);
    }

    #[test]
    fn list_does_not_scroll_when_total_smaller_than_visible() {
        let spec = make_list(-1, 10, 3, Some("L"));
        let (entries, _hits, state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(entries.len(), 3, "all items fit");
        let scroll = match state.get("L").unwrap() {
            WidgetInstanceState::List { scroll_offset, .. } => *scroll_offset,
            _ => unreachable!(),
        };
        assert_eq!(scroll, 0);
    }

    #[test]
    fn list_without_key_does_not_persist_state() {
        let spec = make_list(5, 3, 10, None);
        let (_entries, _hits, state) = render_no_focus(&spec, &HashMap::new());
        assert!(
            state.is_empty(),
            "Lists without a `key` opt out of state preservation"
        );
    }

    // -------------------------------------------------------------
    // TextInput
    // -------------------------------------------------------------

    #[test]
    fn text_input_renders_value_in_brackets() {
        let entry = render_text_input("hello", -1, false, "", None, 0, 0).entry;
        assert_eq!(entry.text, "[hello]");
        assert!(entry.inline_overlays.is_empty());
    }

    #[test]
    fn text_input_with_label_prefixes_with_label_space() {
        let entry = render_text_input("foo", -1, false, "Search:", None, 0, 0).entry;
        assert_eq!(entry.text, "Search: [foo]");
    }

    #[test]
    fn text_input_focused_adds_input_bg_overlay() {
        let entry = render_text_input("x", -1, true, "", None, 0, 0).entry;
        // Focused → input-bg overlay (no cursor since cursor_byte < 0).
        assert_eq!(entry.inline_overlays.len(), 1);
        let bg = entry.inline_overlays[0].style.bg.as_ref().unwrap();
        assert_eq!(bg.as_theme_key(), Some("ui.prompt_bg"));
    }

    #[test]
    fn text_input_cursor_byte_in_entry_at_value_position() {
        // Cursor mid-value: returned byte points at the position
        // *within entry.text*. text = "[abc ]" (focused → trailing
        // pad space). 'a' at byte 1, 'b' at 2, 'c' at 3 — so a
        // cursor at value-byte 1 lands at entry-byte 2.
        let r = render_text_input("abc", 1, true, "", None, 0, 0);
        assert_eq!(r.cursor_byte_in_entry, Some(2));
    }

    #[test]
    fn text_input_cursor_at_end_lands_on_padding_space_not_bracket() {
        // Cursor at end-of-value: with focused + no field_width,
        // a trailing pad space is appended so the cursor never
        // overlaps the closing bracket. text = "[ab ]" → cursor
        // at value-byte 2 lands at entry-byte 3 (the space), not
        // at byte 4 (the `]`).
        let r = render_text_input("ab", 2, true, "", None, 0, 0);
        assert_eq!(r.entry.text, "[ab ]");
        assert_eq!(r.cursor_byte_in_entry, Some(3));
        assert_ne!(r.cursor_byte_in_entry, Some(4), "must not overlap ]");
    }

    #[test]
    fn text_input_unfocused_empty_shows_placeholder_in_muted() {
        let entry = render_text_input("", -1, false, "", Some("type here"), 0, 0).entry;
        assert_eq!(entry.text, "[type here]");
        // One overlay for the placeholder muted color.
        assert_eq!(entry.inline_overlays.len(), 1);
        let fg = entry.inline_overlays[0].style.fg.as_ref().unwrap();
        assert_eq!(fg.as_theme_key(), Some("ui.menu_disabled_fg"));
    }

    #[test]
    fn text_input_focused_empty_does_not_show_placeholder() {
        let entry = render_text_input("", -1, true, "", Some("type here"), 0, 0).entry;
        // No placeholder when focused. Empty + focused + no
        // field_width → trailing pad space so the cursor has
        // somewhere to sit. text = "[ ]".
        assert_eq!(entry.text, "[ ]");
    }

    #[test]
    fn text_input_field_width_pads_short_value_unfocused() {
        // field_width=10, unfocused → inner is 10 chars, no extra
        // pad (cursor not visible anyway).
        let r = render_text_input("hi", 2, false, "", None, 0, 10);
        assert_eq!(r.entry.text, "[hi        ]");
    }

    #[test]
    fn text_input_field_width_focused_adds_cursor_park_space() {
        // field_width=10, focused, value fills exactly 10 → inner
        // is 11 chars (10 + 1 cursor-park space) so the cursor at
        // end-of-value never lands on `]`.
        let r = render_text_input("0123456789", 10, true, "", None, 0, 10);
        assert_eq!(r.entry.text, "[0123456789 ]");
        // Cursor at byte 10 of value → byte 10 of inner → byte 11
        // of entry.text (after `[`). That's the cursor-park space,
        // not `]` (which lives at byte 12).
        assert_eq!(r.cursor_byte_in_entry, Some(11));
        assert_ne!(r.cursor_byte_in_entry, Some(12), "must not land on ]");
    }

    #[test]
    fn text_input_field_width_head_truncates_long_value() {
        // 30-char value, field_width=10, unfocused → keep last 9
        // chars + `…`; no pad space.
        let r = render_text_input("0123456789abcdefghijklmnopqrst", 30, false, "", None, 0, 10);
        assert!(r.entry.text.contains("…lmnopqrst"));
    }

    #[test]
    fn text_input_field_width_clamps_cursor_in_dropped_prefix() {
        // Long value, field_width=5, focused, cursor at byte 0 (in
        // dropped prefix) → clamped to right after the `…`.
        let r = render_text_input("abcdefghij", 0, true, "", None, 0, 5);
        // Inner = `…fghij ` (1 ellipsis + 4 tail chars + 1 pad).
        // Cursor at "right after `…`" = byte 3 of inner (3 = `…`'s
        // UTF-8 byte length). entry.text has `[` before, so
        // absolute byte = 1 + 3 = 4.
        assert_eq!(r.cursor_byte_in_entry, Some(1 + "…".len()));
    }

    #[test]
    fn text_input_truncates_long_value_keeping_tail_visible() {
        let value: String = "0123456789abcdefghij".to_string();
        let entry = render_text_input(&value, -1, false, "", None, 6, 0).entry;
        // Tail-truncated to "…fghij" (max=6, take=5 chars).
        assert_eq!(entry.text, "[…fghij]");
    }

    #[test]
    fn raw_inside_col_offsets_following_hits() {
        let spec = WidgetSpec::Col {
            children: vec![
                WidgetSpec::Raw {
                    entries: vec![
                        TextPropertyEntry::text("line0"),
                        TextPropertyEntry::text("line1"),
                        TextPropertyEntry::text("line2"),
                    ],
                    key: None,
                },
                WidgetSpec::Toggle {
                    checked: false,
                    label: "after raw".into(),
                    focused: false,
                    key: Some("post".into()),
                },
            ],
            key: None,
        };
        let (entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(entries.len(), 4);
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].buffer_row, 3);
    }

    // -------------------------------------------------------------
    // Tree
    // -------------------------------------------------------------

    fn tnode(text: &str, depth: u32, has_children: bool) -> TreeNode {
        TreeNode {
            text: TextPropertyEntry::text(text),
            depth,
            has_children,
            checked: None,
        }
    }

    fn make_tree(
        nodes: Vec<TreeNode>,
        item_keys: Vec<&str>,
        selected: i32,
        visible: u32,
        expanded: Vec<&str>,
        key: Option<&str>,
    ) -> WidgetSpec {
        WidgetSpec::Tree {
            nodes,
            item_keys: item_keys.iter().map(|s| s.to_string()).collect(),
            selected_index: selected,
            visible_rows: visible,
            expanded_keys: expanded.iter().map(|s| s.to_string()).collect(),
            checkable: false,
            key: key.map(|s| s.to_string()),
        }
    }

    #[test]
    fn tree_row_renders_disclosure_glyph_for_internal_collapsed() {
        let r = render_tree_row(&tnode("file.txt", 0, true), false, false);
        assert!(r.entry.text.starts_with('\u{25B6}'), "starts with ▶");
        assert!(r.entry.text.contains("file.txt"));
        assert!(r.disclosure_range.is_some());
    }

    #[test]
    fn tree_row_renders_disclosure_glyph_for_internal_expanded() {
        let r = render_tree_row(&tnode("file.txt", 0, true), true, false);
        assert!(r.entry.text.starts_with('\u{25BC}'), "starts with ▼");
    }

    #[test]
    fn tree_row_leaf_uses_two_spaces_no_disclosure_hit() {
        let r = render_tree_row(&tnode("match", 0, false), false, false);
        // No glyph, just spaces for alignment.
        assert!(r.entry.text.starts_with("  "));
        assert!(r.entry.text.contains("match"));
        assert!(r.disclosure_range.is_none());
    }

    #[test]
    fn tree_row_indents_by_depth_times_two() {
        let r = render_tree_row(&tnode("nested", 2, false), false, false);
        // depth=2 → 4 leading spaces, then 2 alignment spaces, then "nested".
        assert!(r.entry.text.starts_with("      nested"));
    }

    #[test]
    fn tree_row_shifts_plugin_overlays_by_prefix() {
        let mut node = tnode("hello", 1, false);
        node.text.inline_overlays.push(InlineOverlay {
            start: 0,
            end: 5,
            style: OverlayOptions {
                bold: true,
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
        let r = render_tree_row(&node, false, false);
        // depth=1 → 2 indent + 2 alignment = 4 prefix bytes (ASCII).
        // The plugin's [0..5] becomes [4..9].
        let plugin_overlay = r
            .entry
            .inline_overlays
            .iter()
            .find(|o| o.style.bold)
            .expect("bold overlay carried through");
        assert_eq!(plugin_overlay.start, 4);
        assert_eq!(plugin_overlay.end, 9);
    }

    #[test]
    fn tree_row_omits_checkbox_when_not_checkable() {
        // Even with `checked: Some(_)`, no glyph if `checkable: false`.
        let mut node = tnode("file.rs", 0, false);
        node.checked = Some(true);
        let r = render_tree_row(&node, false, false);
        assert!(r.checkbox_range.is_none());
        assert!(!r.entry.text.contains("[v]"));
        assert!(!r.entry.text.contains("[ ]"));
    }

    #[test]
    fn tree_row_omits_checkbox_when_checked_is_none() {
        // `checkable: true` but `checked: None` → still no glyph.
        // Lets a checkable tree mix non-checkbox-bearing nodes
        // (e.g. a separator or header) with checkbox rows.
        let node = tnode("section", 0, false);
        let r = render_tree_row(&node, false, true);
        assert!(r.checkbox_range.is_none());
        assert!(!r.entry.text.contains("[v]"));
        assert!(!r.entry.text.contains("[ ]"));
    }

    #[test]
    fn tree_row_renders_checked_glyph_after_disclosure() {
        let mut node = tnode("file.rs", 0, true);
        node.checked = Some(true);
        let r = render_tree_row(&node, true, true);
        assert!(r.checkbox_range.is_some(), "checkbox range emitted");
        let (cb_start, cb_end) = r.checkbox_range.unwrap();
        // Layout: ▼(3 bytes UTF-8) + " " + [v] + " " + body
        assert_eq!(&r.entry.text[cb_start..cb_end], "[v]");
        assert!(r.entry.text.contains("[v] file.rs"));
    }

    #[test]
    fn tree_row_renders_unchecked_glyph_for_leaf() {
        let mut node = tnode("match-row", 1, false);
        node.checked = Some(false);
        let r = render_tree_row(&node, false, true);
        let (cb_start, cb_end) = r
            .checkbox_range
            .expect("checkbox range for leaf with checked: Some");
        assert_eq!(&r.entry.text[cb_start..cb_end], "[ ]");
        // depth=1 → 2-space indent; leaf-alignment → 2 spaces; then `[ ]` + " ".
        assert!(r.entry.text.starts_with("    [ ] match-row"));
    }

    #[test]
    fn tree_row_checkbox_glyph_byte_range_addresses_correct_text() {
        // Sanity: byte_start..byte_end must extract the glyph
        // verbatim (no UTF-8 boundary issues from the disclosure).
        let mut node = tnode("path/with/é", 0, true);
        node.checked = Some(true);
        let r = render_tree_row(&node, false, true);
        let (cb_start, cb_end) = r.checkbox_range.unwrap();
        assert!(r.entry.text.is_char_boundary(cb_start));
        assert!(r.entry.text.is_char_boundary(cb_end));
        assert_eq!(&r.entry.text[cb_start..cb_end], "[v]");
    }

    #[test]
    fn tree_node_pad_to_chars_pads_text_before_prefix_offset_shift() {
        // depth=0 prefix is "▶ " (1 codepoint glyph + 1 space).
        // Plugin sends body "x" with pad_to_chars=5; renderer pads
        // body to "x    " then prepends prefix.
        let mut node = tnode("x", 0, true);
        node.text.pad_to_chars = Some(5);
        let spec = make_tree(vec![node], vec!["x"], -1, 10, vec!["x"], Some("T"));
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(entries.len(), 1);
        // The full row is prefix + padded body + trailing newline.
        // Body region must be "x    " (5 columns).
        let trimmed = entries[0].text.trim_end_matches('\n');
        assert!(
            trimmed.ends_with("x    "),
            "row should end with the padded body, got {trimmed:?}"
        );
    }

    #[test]
    fn tree_node_truncate_to_chars_cuts_body_before_prefix_offset_shift() {
        let mut node = tnode("abcdefghij", 0, false);
        node.text.truncate_to_chars = Some(6);
        let spec = make_tree(vec![node], vec!["x"], -1, 10, vec![], Some("T"));
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        let trimmed = entries[0].text.trim_end_matches('\n');
        // With budget=6, truncation produces "abc..." (3 head chars
        // + ellipsis), then prefix is prepended.
        assert!(
            trimmed.ends_with("abc..."),
            "row should end with truncated body, got {trimmed:?}"
        );
    }

    #[test]
    fn tree_node_char_unit_overlay_resolves_against_padded_text_and_shifts_by_prefix() {
        // Body text "x" padded to 5 codepoints — the host pads to
        // "x    " before resolving overlays. A char-unit overlay at
        // [0..5] must end up covering the full padded body in bytes,
        // shifted right by the prefix length.
        let mut node = tnode("x", 0, false);
        node.text.pad_to_chars = Some(5);
        node.text.inline_overlays.push(InlineOverlay {
            start: 0,
            end: 5,
            style: OverlayOptions {
                bold: true,
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Char,
        });
        let spec = make_tree(vec![node], vec!["x"], -1, 10, vec![], Some("T"));
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        let entry = &entries[0];
        let bold = entry
            .inline_overlays
            .iter()
            .find(|o| o.style.bold)
            .expect("bold overlay carried through");
        // depth=0, leaf → prefix is two spaces (no glyph). Body
        // starts at byte 2 and is 5 bytes (ASCII pad), so [2..7].
        assert_eq!(bold.start, 2);
        assert_eq!(bold.end, 7);
    }

    #[test]
    fn tree_node_char_unit_overlay_with_multibyte_body_resolves_correctly() {
        // Body text "éxé" — 3 codepoints, 5 bytes. A char-unit
        // overlay at [1..2] (just the "x") becomes byte [3..4]
        // within the body, then shifted by leaf prefix (2 bytes).
        let mut node = tnode("éxé", 0, false);
        node.text.inline_overlays.push(InlineOverlay {
            start: 1,
            end: 2,
            style: OverlayOptions {
                bold: true,
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Char,
        });
        let spec = make_tree(vec![node], vec!["x"], -1, 10, vec![], Some("T"));
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        let entry = &entries[0];
        let bold = entry
            .inline_overlays
            .iter()
            .find(|o| o.style.bold)
            .expect("bold overlay carried through");
        // Prefix is 2 bytes (two ASCII spaces), char→byte [1..2]
        // resolves to body byte [2..3], then shift +2 → [4..5].
        let trimmed = entry.text.trim_end_matches('\n');
        assert_eq!(bold.start, 4);
        assert_eq!(bold.end, 5);
        assert_eq!(&trimmed[bold.start..bold.end], "x");
    }

    #[test]
    fn tree_node_segments_concatenate_into_row_text_with_per_segment_overlays() {
        let mut node = tnode("", 0, false);
        node.text.segments = vec![
            fresh_core::text_property::StyledSegment {
                text: "AB".to_string(),
                style: None,
                overlays: vec![],
            },
            fresh_core::text_property::StyledSegment {
                text: " ".to_string(),
                style: None,
                overlays: vec![],
            },
            fresh_core::text_property::StyledSegment {
                text: "CD".to_string(),
                style: Some(OverlayOptions {
                    bold: true,
                    ..Default::default()
                }),
                overlays: vec![],
            },
        ];
        let spec = make_tree(vec![node], vec!["x"], -1, 10, vec![], Some("T"));
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        let trimmed = entries[0].text.trim_end_matches('\n');
        // Leaf row: 2-space prefix + concatenated segments.
        assert!(
            trimmed.ends_with("AB CD"),
            "row should end with concatenated segments, got {trimmed:?}"
        );
        let bold = entries[0]
            .inline_overlays
            .iter()
            .find(|o| o.style.bold)
            .expect("styled segment overlay carried through");
        // Bold covers the third segment only ("CD" at byte 5..7
        // after 2-byte prefix + "AB " = 3 bytes).
        assert_eq!(&trimmed[bold.start..bold.end], "CD");
    }

    #[test]
    fn tree_node_segment_nested_overlay_shifts_to_segment_position() {
        // Build a row whose third segment carries a nested overlay
        // covering chars [0..3] within itself ("CDE"). The host
        // shifts those by the segment's start in the entry; final
        // bytes resolve against the assembled text.
        let mut node = tnode("", 0, false);
        node.text.segments = vec![
            fresh_core::text_property::StyledSegment {
                text: "AB".to_string(),
                style: None,
                overlays: vec![],
            },
            fresh_core::text_property::StyledSegment {
                text: " - ".to_string(),
                style: None,
                overlays: vec![],
            },
            fresh_core::text_property::StyledSegment {
                text: "CDEFG".to_string(),
                style: None,
                overlays: vec![InlineOverlay {
                    start: 0,
                    end: 3,
                    style: OverlayOptions {
                        bold: true,
                        ..Default::default()
                    },
                    properties: Default::default(),
                    unit: OffsetUnit::Char,
                }],
            },
        ];
        let spec = make_tree(vec![node], vec!["x"], -1, 10, vec![], Some("T"));
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        let trimmed = entries[0].text.trim_end_matches('\n');
        let bold = entries[0]
            .inline_overlays
            .iter()
            .find(|o| o.style.bold)
            .expect("nested overlay carried through");
        assert_eq!(&trimmed[bold.start..bold.end], "CDE");
    }

    #[test]
    fn tree_node_segments_with_pad_pad_after_concatenation() {
        let mut node = tnode("", 0, false);
        node.text.segments = vec![fresh_core::text_property::StyledSegment {
            text: "ab".to_string(),
            style: None,
            overlays: vec![],
        }];
        node.text.pad_to_chars = Some(5);
        let spec = make_tree(vec![node], vec!["x"], -1, 10, vec![], Some("T"));
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        let trimmed = entries[0].text.trim_end_matches('\n');
        // Two-space leaf prefix + "ab" + three padding spaces = "  ab   ".
        assert!(
            trimmed.ends_with("ab   "),
            "row should be padded after segment concat, got {trimmed:?}"
        );
    }

    #[test]
    fn tree_renders_only_top_level_when_nothing_expanded() {
        let spec = make_tree(
            vec![
                tnode("a", 0, true),
                tnode("a.0", 1, false),
                tnode("a.1", 1, false),
                tnode("b", 0, true),
                tnode("b.0", 1, false),
            ],
            vec!["a", "a.0", "a.1", "b", "b.0"],
            -1,
            10,
            vec![], // none expanded
            Some("T"),
        );
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        // Only the two top-level nodes are visible.
        assert_eq!(entries.len(), 2);
        assert!(entries[0].text.contains('a'));
        assert!(entries[1].text.contains('b'));
    }

    #[test]
    fn tree_renders_children_of_expanded_nodes() {
        let spec = make_tree(
            vec![
                tnode("a", 0, true),
                tnode("a.0", 1, false),
                tnode("a.1", 1, false),
                tnode("b", 0, true),
                tnode("b.0", 1, false),
            ],
            vec!["a", "a.0", "a.1", "b", "b.0"],
            -1,
            10,
            vec!["a"],
            Some("T"),
        );
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        // a, a.0, a.1, b — b's child stays hidden.
        assert_eq!(entries.len(), 4);
    }

    #[test]
    fn tree_emits_two_hits_per_internal_row_one_per_leaf() {
        // a (internal, expanded) + a.0 (leaf) → 2 hits for a (disclosure + body)
        // and 1 hit for a.0 (body only).
        let spec = make_tree(
            vec![tnode("a", 0, true), tnode("a.0", 1, false)],
            vec!["a", "a.0"],
            -1,
            10,
            vec!["a"],
            Some("T"),
        );
        let (_entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(hits.len(), 3);
        // First hit: disclosure on the internal node.
        assert_eq!(hits[0].event_type, "expand");
        assert_eq!(hits[0].widget_kind, "tree");
        assert_eq!(hits[1].event_type, "select");
        assert_eq!(hits[2].event_type, "select");
    }

    #[test]
    fn tree_hits_carry_tree_spec_key_and_per_item_key_in_payload() {
        let spec = make_tree(
            vec![tnode("only", 0, false)],
            vec!["only-key"],
            -1,
            10,
            vec![],
            Some("matchTree"),
        );
        let (_entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(hits[0].widget_key, "matchTree");
        assert_eq!(hits[0].payload["key"], "only-key");
        assert_eq!(hits[0].payload["index"], 0);
    }

    #[test]
    fn tree_persists_expanded_keys_in_instance_state() {
        let spec = make_tree(
            vec![tnode("a", 0, true), tnode("a.0", 1, false)],
            vec!["a", "a.0"],
            -1,
            10,
            vec!["a"],
            Some("T"),
        );
        let (_, _, state) = render_no_focus(&spec, &HashMap::new());
        match state.get("T").unwrap() {
            WidgetInstanceState::Tree { expanded_keys, .. } => {
                assert!(expanded_keys.contains("a"));
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn tree_instance_state_overrides_spec_expanded_keys() {
        // Previous instance state has b expanded but spec says a.
        // Instance state wins (spec is initial-only after first render).
        let mut prev = HashMap::new();
        prev.insert(
            "T".into(),
            WidgetInstanceState::Tree {
                scroll_offset: 0,
                selected_index: -1,
                expanded_keys: ["b".to_string()].iter().cloned().collect(),
            },
        );
        let spec = make_tree(
            vec![
                tnode("a", 0, true),
                tnode("a.0", 1, false),
                tnode("b", 0, true),
                tnode("b.0", 1, false),
            ],
            vec!["a", "a.0", "b", "b.0"],
            -1,
            10,
            vec!["a"], // initial-only — ignored after first render
            Some("T"),
        );
        let (entries, _hits, _state) = render_no_focus(&spec, &prev);
        // Should render: a (collapsed), b, b.0 — three rows. a.0 hidden.
        assert_eq!(entries.len(), 3);
    }

    #[test]
    fn tree_selected_row_gets_focused_bg() {
        let spec = make_tree(
            vec![tnode("a", 0, false), tnode("b", 0, false)],
            vec!["a", "b"],
            1,
            10,
            vec![],
            Some("T"),
        );
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert!(entries[0].style.is_none());
        let style = entries[1].style.as_ref().expect("selected gets style");
        assert_eq!(
            style.bg.as_ref().and_then(|c| c.as_theme_key()),
            Some("ui.menu_active_bg")
        );
        assert!(style.extend_to_line_end);
    }

    #[test]
    fn tree_clamps_selection_to_visible_when_selected_node_is_hidden() {
        // selected_index = 1 (a.0), but `a` is collapsed → a.0 hidden.
        // The renderer falls back to the nearest earlier visible
        // node (a, idx 0).
        let spec = make_tree(
            vec![tnode("a", 0, true), tnode("a.0", 1, false)],
            vec!["a", "a.0"],
            1,
            10,
            vec![], // a not expanded
            Some("T"),
        );
        let (_entries, _hits, state) = render_no_focus(&spec, &HashMap::new());
        match state.get("T").unwrap() {
            WidgetInstanceState::Tree { selected_index, .. } => {
                assert_eq!(*selected_index, 0);
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn tree_scrolls_to_keep_selection_in_visible_window() {
        // 6 visible rows total, visible_rows=3, selected at flat
        // position 4 → scroll should be 2 (so selected lands at the
        // bottom of the window).
        let spec = make_tree(
            vec![
                tnode("0", 0, false),
                tnode("1", 0, false),
                tnode("2", 0, false),
                tnode("3", 0, false),
                tnode("4", 0, false),
                tnode("5", 0, false),
            ],
            vec!["k0", "k1", "k2", "k3", "k4", "k5"],
            4,
            3,
            vec![],
            Some("T"),
        );
        let (entries, _hits, state) = render_no_focus(&spec, &HashMap::new());
        // Visible window: items 2..5 → 3 rows.
        assert_eq!(entries.len(), 3);
        match state.get("T").unwrap() {
            WidgetInstanceState::Tree { scroll_offset, .. } => assert_eq!(*scroll_offset, 2),
            _ => unreachable!(),
        }
    }

    #[test]
    fn tree_tabbable_keys_include_tree_with_key() {
        let spec = WidgetSpec::Col {
            children: vec![
                WidgetSpec::Toggle {
                    checked: false,
                    label: "T".into(),
                    focused: false,
                    key: Some("toggle".into()),
                },
                make_tree(
                    vec![tnode("a", 0, false)],
                    vec!["a"],
                    -1,
                    10,
                    vec![],
                    Some("tree"),
                ),
            ],
            key: None,
        };
        let mut tabbable = Vec::new();
        collect_tabbable(&spec, &mut tabbable);
        assert_eq!(tabbable, vec!["toggle", "tree"]);
    }

    // -------------------------------------------------------------
    // TextArea
    // -------------------------------------------------------------

    fn make_text_area(
        value: &str,
        cursor_byte: i32,
        focused: bool,
        rows: u32,
        field_width: u32,
        key: Option<&str>,
    ) -> WidgetSpec {
        WidgetSpec::Text {
            value: value.into(),
            cursor_byte,
            focused,
            label: String::new(),
            placeholder: None,
            // Force multi-line behaviour even when the test passes
            // `rows: 1` — the previous TextArea-specific tests
            // exercise the multi-line code path through this
            // helper.
            rows: rows.max(2),
            field_width,
            max_visible_chars: 0,
            key: key.map(|s| s.into()),
        }
    }

    #[test]
    fn text_area_renders_visible_rows_count() {
        // Single line value, but rows=3 → 3 entries (line + 2
        // blanks).
        let spec = make_text_area("hi", -1, false, 3, 10, Some("ta"));
        let prev = HashMap::new();
        let out = render_spec(&spec, &prev, "", 80);
        assert_eq!(out.entries.len(), 3);
    }

    #[test]
    fn text_area_pads_short_lines_to_field_width() {
        let spec = make_text_area("hi", -1, false, 1, 6, Some("ta"));
        let prev = HashMap::new();
        let out = render_spec(&spec, &prev, "", 80);
        // First (only visible) row: "hi" padded to 6 chars → "hi    \n"
        let first = &out.entries[0];
        assert_eq!(first.text, "hi    \n");
    }

    #[test]
    fn text_area_truncates_long_line_with_ellipsis() {
        let spec = make_text_area("abcdefghi", -1, false, 1, 5, Some("ta"));
        let prev = HashMap::new();
        let out = render_spec(&spec, &prev, "", 80);
        // 9 chars trimmed to 5 → "abcd…\n".
        assert_eq!(out.entries[0].text, "abcd…\n");
    }

    #[test]
    fn text_area_focused_adds_input_bg_overlay_per_row() {
        let spec = make_text_area("a\nb", -1, true, 3, 4, Some("ta"));
        let prev = HashMap::new();
        let out = render_spec(&spec, &prev, "ta", 80);
        for entry in &out.entries {
            let has_bg = entry.inline_overlays.iter().any(|o| {
                o.style
                    .bg
                    .as_ref()
                    .and_then(|c| c.as_theme_key())
                    .map(|k| k == "ui.prompt_bg")
                    .unwrap_or(false)
            });
            assert!(has_bg, "every focused row gets input-bg");
        }
    }

    #[test]
    fn text_area_publishes_focus_cursor_at_value_position() {
        // value="ab\ncd", cursor at byte 4 (col 1 on line 1, char
        // 'd' position).
        let spec = make_text_area("ab\ncd", 4, true, 3, 6, Some("ta"));
        let prev = HashMap::new();
        let out = render_spec(&spec, &prev, "ta", 80);
        let fc = out.focus_cursor.expect("focused → cursor published");
        // Line 1 is the second visible row → buffer_row 1.
        assert_eq!(fc.buffer_row, 1);
        // Col 1 on the rendered row.
        assert_eq!(fc.byte_in_row, 1);
    }

    #[test]
    fn text_area_label_offsets_cursor_buffer_row() {
        // With a label, the editing region starts on row 1, so a
        // cursor on line 0 of the value lands on row 1 of the
        // buffer.
        let spec = WidgetSpec::Text {
            value: "hi".into(),
            cursor_byte: 1,
            focused: true,
            label: "Note".into(),
            placeholder: None,
            rows: 2,
            field_width: 6,
            max_visible_chars: 0,
            key: Some("ta".into()),
        };
        let prev = HashMap::new();
        let out = render_spec(&spec, &prev, "ta", 80);
        // entries[0] is the label row, entries[1..] are content.
        assert!(out.entries[0].text.starts_with("Note:"));
        let fc = out.focus_cursor.unwrap();
        assert_eq!(fc.buffer_row, 1);
    }

    #[test]
    fn text_area_persists_value_and_cursor_in_instance_state() {
        let spec = make_text_area("abc", 2, true, 2, 8, Some("ta"));
        let prev = HashMap::new();
        let out = render_spec(&spec, &prev, "ta", 80);
        match out.instance_states.get("ta") {
            Some(WidgetInstanceState::Text {
                value, cursor_byte, ..
            }) => {
                assert_eq!(value, "abc");
                assert_eq!(*cursor_byte, 2);
            }
            other => panic!("expected Text instance state, got {:?}", other),
        }
    }

    #[test]
    fn text_area_instance_state_overrides_spec_value() {
        // Plugin's spec says "old" but instance state has "new" —
        // the renderer reads from instance state.
        let spec = make_text_area("old", 0, true, 2, 8, Some("ta"));
        let mut prev = HashMap::new();
        prev.insert(
            "ta".into(),
            WidgetInstanceState::Text {
                value: "new".into(),
                cursor_byte: 3,
                scroll: 0,
            },
        );
        let out = render_spec(&spec, &prev, "ta", 80);
        // The first row should now read "new" (not "old").
        assert!(out.entries[0].text.starts_with("new"));
    }

    #[test]
    fn text_area_scroll_clamps_to_keep_cursor_visible() {
        // 5-line value, rows=2. Cursor on line 4 (last). On first
        // render the renderer should auto-scroll so line 4 is
        // visible.
        let spec = make_text_area("a\nb\nc\nd\ne", 8, true, 2, 4, Some("ta"));
        // byte 8 is on the 5th line (line index 4).
        let prev = HashMap::new();
        let out = render_spec(&spec, &prev, "ta", 80);
        match out.instance_states.get("ta") {
            Some(WidgetInstanceState::Text { scroll, .. }) => {
                assert_eq!(*scroll, 3, "scroll so lines 3..5 are visible");
            }
            _ => panic!("expected Text instance state"),
        }
    }

    #[test]
    fn text_area_unfocused_empty_shows_placeholder_in_first_row() {
        // Test the renderer directly (focused=false). Host-owned
        // focus would otherwise auto-focus the only tabbable
        // widget — see `text_area_publishes_focus_cursor_at_value_position`
        // for the focused path.
        let r = render_text_area("", -1, false, "", Some("write here"), 2, 12, 0, 80);
        assert!(r.entries[0].text.starts_with("write here"));
        // Placeholder uses the muted-fg overlay.
        let fg = r.entries[0]
            .inline_overlays
            .iter()
            .find_map(|o| o.style.fg.as_ref())
            .and_then(|c| c.as_theme_key());
        assert_eq!(fg, Some("ui.menu_disabled_fg"));
    }

    #[test]
    fn text_area_tabbable_keys_include_text_area_with_key() {
        let spec = WidgetSpec::Col {
            children: vec![
                WidgetSpec::Toggle {
                    checked: false,
                    label: "T".into(),
                    focused: false,
                    key: Some("toggle".into()),
                },
                make_text_area("", -1, false, 3, 10, Some("note")),
            ],
            key: None,
        };
        let mut tabbable = Vec::new();
        collect_tabbable(&spec, &mut tabbable);
        assert_eq!(tabbable, vec!["toggle", "note"]);
    }
}
