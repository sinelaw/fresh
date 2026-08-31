//! `DualList` — two-column ordered-subset picker.

use std::collections::HashMap;

use fresh_core::api::{DualListOption, OverlayColorSpec, OverlayOptions, WidgetSpec};
use serde_json::json;

use super::WidgetImpl;
use crate::widgets::registry::{HitArea, WidgetInstanceState};
use fresh_core::text_property::{InlineOverlay, OffsetUnit, TextPropertyEntry};

use crate::widgets::render::{
    cell, dual_available_values, dual_col_width, dual_cursor_marker, dual_label,
    dual_sanitize_included, ensure_trailing_newline, CollectedOutput, RenderContext,
    DUAL_COLUMN_ACTIVE, DUAL_CURSOR_IDLE, DUAL_GUTTER_BLANK, KEY_COMPLETION_DIM_FG, KEY_FOCUSED_BG,
    KEY_FOCUSED_FG, KEY_PLACEHOLDER_FG, KEY_SECTION_LABEL_FG,
};

pub(crate) struct DualList;

impl WidgetImpl for DualList {
    fn box_meta(&self, spec: &WidgetSpec) -> super::BoxMeta {
        let mut m = super::BoxMeta::plain("dual_list");
        if let WidgetSpec::DualList { key: Some(k), .. } = spec {
            if !k.is_empty() {
                m.key = Some(k.clone());
                m.focusable = true;
            }
        }
        m
    }

    /// The DualList's whole keyboard vocabulary is self-contained
    /// state mutation, so it lives here: arrows drive the active
    /// column's cursor, Left/Right switch columns, Space moves the
    /// focused item across, PageUp/PageDown reorder within Included.
    /// Enter commits form-like — the kind requests a focus advance
    /// rather than reaching into panel policy.
    fn on_key(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        key: &str,
        fx: &mut super::KeyFx,
    ) -> super::KeyDisposition {
        let op = match key {
            "Up" => DualOp::CursorMove(-1),
            "Down" => DualOp::CursorMove(1),
            "PageUp" => DualOp::Reorder(-1),
            "PageDown" => DualOp::Reorder(1),
            "Left" => DualOp::SwitchColumn(false),
            "Right" => DualOp::SwitchColumn(true),
            "Space" => DualOp::MoveAcross,
            "Enter" => {
                fx.focus_advance = Some(1);
                return super::KeyDisposition::Consumed;
            }
            _ => return super::KeyDisposition::Pass,
        };
        apply_op(spec, widget_key, panel, op, fx);
        super::KeyDisposition::Consumed
    }

    /// Pointer model: a cell click makes the clicked column active
    /// and moves its cursor to the clicked row — cursor/active state
    /// only, no `change` (the included set is unchanged), and the
    /// recorded hit event is swallowed. The set itself moves through
    /// the keyboard vocabulary (`apply_op`).
    fn on_pointer(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        event_type: &str,
        payload: &serde_json::Value,
        _fx: &mut super::PointerFx,
    ) -> super::PointerDisposition {
        if event_type != "dual_focus" {
            return super::PointerDisposition::Default;
        }
        let to_included = payload.get("column").and_then(|v| v.as_str()) == Some("included");
        let index = payload.get("index").and_then(|v| v.as_i64()).unwrap_or(0) as usize;
        pointer_focus_cell(spec, widget_key, panel, to_included, index);
        super::PointerDisposition::Consumed
    }

    fn collect(
        &self,
        spec: &WidgetSpec,
        prev: &HashMap<String, WidgetInstanceState>,
        next_state: &mut HashMap<String, WidgetInstanceState>,
        ctx: RenderContext<'_>,
        panel_width: u32,
    ) -> CollectedOutput {
        let WidgetSpec::DualList {
            options,
            included,
            excluded,
            label,
            focused,
            active_included,
            available_cursor,
            included_cursor,
            hint,
            visible_rows,
            key,
        } = spec
        else {
            return CollectedOutput::default();
        };
        collect_dual_list(
            options,
            DualListSeed {
                included,
                excluded,
                active_included: *active_included,
                available_cursor: *available_cursor as usize,
                included_cursor: *included_cursor as usize,
            },
            label,
            hint,
            *focused,
            *visible_rows,
            key.as_deref(),
            prev,
            next_state,
            ctx,
            panel_width,
        )
    }
}

/// A `DualList` interaction, resolved from a keystroke by
/// [`DualList::on_key`].
enum DualOp {
    /// Make the Included column active (`true`) or Available (`false`).
    SwitchColumn(bool),
    /// Move the active column's cursor by `delta`.
    CursorMove(i32),
    /// Move the focused item across columns (add if Available is
    /// active, remove if Included is active).
    MoveAcross,
    /// Reorder the focused Included item by `delta` (no-op unless the
    /// Included column is active).
    Reorder(i32),
}

/// Click on a cell: make the clicked column active and move its
/// cursor to the clicked row, re-deriving the live column contents
/// from the spec so cursor clamping matches what's on screen.
fn pointer_focus_cell(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    to_included: bool,
    index: usize,
) {
    let WidgetSpec::DualList {
        options, excluded, ..
    } = spec
    else {
        return;
    };
    let (mut included, mut avail_cur, mut incl_cur) = match panel.instance_states.get(widget_key) {
        Some(WidgetInstanceState::DualList {
            included,
            available_cursor,
            included_cursor,
            ..
        }) => (
            included.clone(),
            *available_cursor as usize,
            *included_cursor as usize,
        ),
        _ => (Vec::new(), 0, 0),
    };
    included = dual_sanitize_included(options, &included);
    let available = dual_available_values(options, &included, excluded);
    if to_included {
        if index < included.len() {
            incl_cur = index;
        }
    } else if index < available.len() {
        avail_cur = index;
    }
    panel.instance_states.insert(
        widget_key.to_string(),
        WidgetInstanceState::DualList {
            included,
            active_included: to_included,
            available_cursor: avail_cur as u32,
            included_cursor: incl_cur as u32,
        },
    );
}

/// Step a column cursor by `delta`, clamped to `[0, len)`. Empty
/// column stays at 0.
fn step_cursor(cursor: usize, delta: i32, len: usize) -> usize {
    if len == 0 {
        return 0;
    }
    let raw = cursor as i32 + delta;
    raw.clamp(0, len as i32 - 1) as usize
}

/// Apply one op against the host-owned instance state: load (or seed
/// from the spec), mutate, store back, and queue `change` with the
/// new included set when it actually changed. The single mutation
/// path for the DualList's keyboard model — the click path only syncs
/// cursors (`handle_widget_dual_click`) and never changes the set.
fn apply_op(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    op: DualOp,
    fx: &mut super::KeyFx,
) {
    if widget_key.is_empty() {
        return;
    }
    let WidgetSpec::DualList {
        options,
        excluded,
        included: spec_included,
        ..
    } = spec
    else {
        return;
    };
    let (mut included, mut active_included, mut avail_cur, mut incl_cur) =
        match panel.instance_states.get(widget_key) {
            Some(WidgetInstanceState::DualList {
                included,
                active_included,
                available_cursor,
                included_cursor,
            }) => (
                included.clone(),
                *active_included,
                *available_cursor as usize,
                *included_cursor as usize,
            ),
            _ => (spec_included.clone(), false, 0usize, 0usize),
        };
    included = dual_sanitize_included(options, &included);
    let mut available = dual_available_values(options, &included, excluded);
    let clamp = |c: usize, len: usize| if len == 0 { 0 } else { c.min(len - 1) };
    avail_cur = clamp(avail_cur, available.len());
    incl_cur = clamp(incl_cur, included.len());

    let before = included.clone();
    match op {
        DualOp::SwitchColumn(to_included) => active_included = to_included,
        DualOp::CursorMove(delta) => {
            if active_included {
                incl_cur = step_cursor(incl_cur, delta, included.len());
            } else {
                avail_cur = step_cursor(avail_cur, delta, available.len());
            }
        }
        DualOp::MoveAcross => {
            if active_included {
                // Remove the focused included item back to Available.
                if incl_cur < included.len() {
                    included.remove(incl_cur);
                    incl_cur = clamp(incl_cur, included.len());
                }
            } else {
                // Add the focused available item to the Included list.
                if avail_cur < available.len() {
                    included.push(available[avail_cur].clone());
                    available = dual_available_values(options, &included, excluded);
                    avail_cur = clamp(avail_cur, available.len());
                }
            }
        }
        DualOp::Reorder(delta) => {
            // Only meaningful in the Included column.
            if active_included && !included.is_empty() {
                let target = incl_cur as i32 + delta;
                if target >= 0 && (target as usize) < included.len() {
                    included.swap(incl_cur, target as usize);
                    incl_cur = target as usize;
                }
            }
        }
    }
    let changed = included != before;
    panel.instance_states.insert(
        widget_key.to_string(),
        WidgetInstanceState::DualList {
            included: included.clone(),
            active_included,
            available_cursor: avail_cur as u32,
            included_cursor: incl_cur as u32,
        },
    );
    if changed {
        fx.events
            .push(("change".into(), json!({ "included": included })));
    }
}

/// Spec-supplied starting state for a `DualList`: what the host asks for
/// before the widget has instance state of its own — and, for a host that
/// owns the control's state itself (the Settings dialog re-emits its model
/// every frame and keeps none), on every frame.
pub(crate) struct DualListSeed<'a> {
    pub included: &'a [String],
    /// Options of this spec that a sibling control has claimed, so the
    /// Available column must not offer them.
    pub excluded: &'a [String],
    pub active_included: bool,
    pub available_cursor: usize,
    pub included_cursor: usize,
}

/// A `DualList`'s live model: the two columns as they will be drawn, which
/// one the keyboard is driving, and where each column's cursor sits.
pub(crate) struct Resolved {
    /// The Included column, in host order, sanitized against this spec.
    pub included: Vec<String>,
    /// The Available column: the options neither included nor excluded.
    pub available: Vec<String>,
    /// Which column the keyboard drives.
    pub active_included: bool,
    /// Cursor rows, each clamped into its own column.
    pub available_cursor: usize,
    pub included_cursor: usize,
    /// Whether the widget holds focus. Every marker the control draws is
    /// `focused && …` — an unfocused DualList shows no cursor and no active
    /// column — so focus is resolved once, here, rather than re-derived by
    /// each caller alongside the state it belongs with.
    pub focused: bool,
}

impl Resolved {
    /// How many body rows the control draws: one per row of the longer
    /// column, but never fewer than the spec's `visible_rows`, so the
    /// control keeps its height as items move across instead of resizing
    /// the panel under the pointer.
    pub(crate) fn body_rows(&self, visible_rows: u32) -> usize {
        self.available
            .len()
            .max(self.included.len())
            .max(visible_rows as usize)
    }
}

/// **Where a `DualList`'s columns and cursors actually come from.**
///
/// Instance state is authoritative after first render; the spec's
/// `included`/`active_included`/cursor fields are a seed, used on the first
/// frame and on every frame for a host that owns the control's state itself.
///
/// Whichever the source, the included set is sanitized against *this* spec's
/// options and each cursor is clamped into its own column: the option set can
/// change under a stored state, and a cursor left past the end of a shortened
/// column would mark a row that no longer exists.
///
/// Pulled out of the collector because the *description* needs the same
/// answer, and a second copy of these rules is a second place for them to
/// drift. Pure, and in particular it never writes `next_state` — the
/// collector persists the answer, so instance state keeps one writer — which
/// is what lets a description call it.
pub(crate) fn resolve(
    options: &[DualListOption],
    seed: &DualListSeed<'_>,
    key: Option<&str>,
    prev: &HashMap<String, WidgetInstanceState>,
    focused: bool,
) -> Resolved {
    let seed_state = || {
        (
            seed.included.to_vec(),
            seed.active_included,
            seed.available_cursor,
            seed.included_cursor,
        )
    };
    let (included, active_included, mut avail_cur, mut incl_cur) = match key {
        Some(k) if !k.is_empty() => match prev.get(k) {
            Some(WidgetInstanceState::DualList {
                included,
                active_included,
                available_cursor,
                included_cursor,
            }) => (
                included.clone(),
                *active_included,
                *available_cursor as usize,
                *included_cursor as usize,
            ),
            _ => seed_state(),
        },
        _ => seed_state(),
    };
    let included = dual_sanitize_included(options, &included);
    let available = dual_available_values(options, &included, seed.excluded);
    if !available.is_empty() {
        avail_cur = avail_cur.min(available.len() - 1);
    } else {
        avail_cur = 0;
    }
    if !included.is_empty() {
        incl_cur = incl_cur.min(included.len() - 1);
    } else {
        incl_cur = 0;
    }
    Resolved {
        included,
        available,
        active_included,
        available_cursor: avail_cur,
        included_cursor: incl_cur,
        focused,
    }
}

/// The header row. Each title carries the same two-column gutter its cells
/// do, and the column the keyboard is driving is marked with `▾ ` plus the
/// accent fg — so "which side am I on?" survives both a monochrome terminal
/// and a color-only reading.
pub(crate) fn header_row(st: &Resolved, col_w: usize) -> TextPropertyEntry {
    let avail_active = st.focused && !st.active_included;
    let incl_active = st.focused && st.active_included;
    let avail_head = format!(
        "{}{}",
        if avail_active {
            DUAL_COLUMN_ACTIVE
        } else {
            DUAL_GUTTER_BLANK
        },
        cell("Available", col_w)
    );
    let incl_head = format!(
        "{}{}",
        if incl_active {
            DUAL_COLUMN_ACTIVE
        } else {
            DUAL_GUTTER_BLANK
        },
        cell("Included", col_w)
    );
    let header = format!("{avail_head}  {incl_head}");
    let head_left = 0..avail_head.len();
    let head_right = (avail_head.len() + 2)..header.len();
    let mut header_entry = TextPropertyEntry::text(&header);
    for (range, active) in [(head_left, avail_active), (head_right, incl_active)] {
        header_entry.inline_overlays.push(InlineOverlay {
            start: range.start,
            end: range.end,
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(if active {
                    KEY_SECTION_LABEL_FG
                } else {
                    KEY_COMPLETION_DIM_FG
                })),
                bold: active,
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }
    ensure_trailing_newline(&mut header_entry);
    header_entry
}

/// One body row of a `DualList`: two cells side by side, and where each of
/// them sits in the row's text.
pub(crate) struct Row {
    /// The row as the collector pushes it, trailing newline included.
    pub entry: TextPropertyEntry,
    /// The Available cell's byte range, present only when a value occupies
    /// the cell — an empty cell answers no press, so it gets no hit area.
    pub available: Option<(usize, usize)>,
    /// The Included cell's byte range, on the same terms.
    pub included: Option<(usize, usize)>,
}

/// One body row: the two columns' cells side by side, each with its own
/// cursor gutter.
///
/// The active column's cursor row gets the filled `▸ `, the idle column's the
/// hollow `▹ ` so both cursors are readable at once — the idle marker is what
/// tells you where Left/Right will drop you. Rows that hold no cursor still
/// reserve the two columns, so nothing shifts as the cursor moves.
///
/// The cell ranges come back with the row rather than being left for the
/// caller to measure back out of the rendered text: the press target is the
/// cell this function just laid out, and re-deriving a range by scanning
/// output is the duplication that put hover and the context menu on different
/// rows elsewhere in this migration.
pub(crate) fn body_row(options: &[DualListOption], st: &Resolved, i: usize, col_w: usize) -> Row {
    let left_val = st.available.get(i);
    let right_val = st.included.get(i);
    let left = left_val.map(|v| dual_label(options, v)).unwrap_or("");
    let right = right_val.map(|v| dual_label(options, v)).unwrap_or("");
    let left_gutter = dual_cursor_marker(
        st.focused && left_val.is_some() && i == st.available_cursor,
        !st.active_included,
    );
    let right_gutter = dual_cursor_marker(
        st.focused && right_val.is_some() && i == st.included_cursor,
        st.active_included,
    );
    let left_cell = format!("{left_gutter}{}", cell(left, col_w));
    let right_cell = format!("{right_gutter}{}", cell(right, col_w));
    let text = format!("{}  {}", left_cell, right_cell);
    let left_start = 0usize;
    let left_end = left_cell.len();
    let right_start = left_end + 2;
    let right_end = right_start + right_cell.len();

    let mut entry = TextPropertyEntry::text(&text);
    // Cursor highlight, spanning the marker *and* the label so the
    // whole cell reads as one selected row. The active column gets
    // the full fg/bg flip; the idle column gets a dimmed marker
    // only (below) so the two never compete for attention.
    if st.focused {
        let (hs, he) = if st.active_included {
            if right_val.is_some() && i == st.included_cursor {
                (right_start, right_end)
            } else {
                (0, 0)
            }
        } else if left_val.is_some() && i == st.available_cursor {
            (left_start, left_end)
        } else {
            (0, 0)
        };
        if he > hs {
            entry.inline_overlays.push(InlineOverlay {
                start: hs,
                end: he,
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
        // Idle-column marker: dimmed, no background, so it reads
        // as "the other cursor is parked here".
        let idle = if st.active_included {
            (left_val.is_some() && i == st.available_cursor).then_some(left_start)
        } else {
            (right_val.is_some() && i == st.included_cursor).then_some(right_start)
        };
        if let Some(start) = idle {
            entry.inline_overlays.push(InlineOverlay {
                start,
                end: start + DUAL_CURSOR_IDLE.len(),
                style: OverlayOptions {
                    fg: Some(OverlayColorSpec::theme_key(KEY_COMPLETION_DIM_FG)),
                    ..Default::default()
                },
                properties: Default::default(),
                unit: OffsetUnit::Byte,
            });
        }
    }
    ensure_trailing_newline(&mut entry);
    Row {
        entry,
        available: left_val.is_some().then_some((left_start, left_end)),
        included: right_val.is_some().then_some((right_start, right_end)),
    }
}

#[allow(clippy::too_many_arguments)]
fn collect_dual_list(
    options: &[DualListOption],
    seed: DualListSeed<'_>,
    label: &str,
    hint: &str,
    focused: bool,
    visible_rows: u32,
    key: Option<&str>,
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    ctx: RenderContext<'_>,
    panel_width: u32,
) -> CollectedOutput {
    let mut out = CollectedOutput::default();
    // A keyed widget takes focus from the host's resolved focus key; an
    // unkeyed one falls back to the spec's initial-only `focused` hint.
    let is_focused = if key.is_some_and(|k| !k.is_empty()) {
        ctx.is_focused(key)
    } else {
        focused
    };
    let st = resolve(options, &seed, key, prev, is_focused);
    // **The walk carries this widget's state; it does not decide it.**
    //
    // What it used to write back was `resolve`'s answer — the sanitized
    // included set and both clamped cursors — and every one of those is
    // recomputed on each read by the same function, which the painter and the
    // description both call. Persisting a derivation stored nothing a reader
    // could not work out, and it made the render walk a second writer of
    // fields `on_key` and `on_pointer` own.
    //
    // The pass-through keeps the entry alive across the whole-map replace in
    // `update_side_effects`; an absent one stays absent, so a control nobody
    // has touched still reads its columns from the spec. Same rule as
    // `kinds::dropdown` and `kinds::number`, for the same reason.
    if let Some(k) = key.filter(|k| !k.is_empty()) {
        if let Some(stored) = prev.get(k) {
            next_state.insert(k.to_string(), stored.clone());
        }
    }

    let col_w = dual_col_width(panel_width);
    let widget_key = key.unwrap_or("").to_string();

    if let Some(e) = label_row(label) {
        out.entries.push(e);
    }
    let header_row_idx = out.entries.len() as u32;
    out.entries.push(header_row(&st, col_w));

    for i in 0..st.body_rows(visible_rows) {
        let Row {
            entry,
            available,
            included,
        } = body_row(options, &st, i, col_w);
        let row = header_row_idx + 1 + i as u32;
        // Click hit areas: clicking a cell focuses that column +
        // cursor row.
        for (range, column) in [(available, "available"), (included, "included")] {
            let Some((byte_start, byte_end)) = range else {
                continue;
            };
            out.hits.push(HitArea {
                row_target: false,
                context_click: false,
                overlay: false,
                widget_key: widget_key.clone(),
                widget_kind: "dual_list",
                buffer_row: row,
                byte_start,
                byte_end,
                payload: json!({ "column": column, "index": i }),
                event_type: "dual_focus",
                owner_key: None,
            });
        }
        out.entries.push(entry);
    }

    // Key hint under the columns. The control's bindings (Shift+←→ to
    // move an item across, Shift+↑↓ to reorder) aren't guessable from
    // its shape, so the host supplies a localized one-liner and it
    // rides with the control instead of only in a panel footer.
    if let Some(e) = hint_row(hint) {
        out.entries.push(e);
    }
    out
}

/// The optional label above the columns. `None` when the plugin gave none,
/// which is what "empty = omitted" means — an empty row is not the same as no
/// row, because the column band below it would start one line lower.
pub(crate) fn label_row(label: &str) -> Option<TextPropertyEntry> {
    if label.is_empty() {
        return None;
    }
    let mut e = TextPropertyEntry::text(label);
    ensure_trailing_newline(&mut e);
    Some(e)
}

/// The key hint under the columns.
///
/// The control's bindings — Shift+←/→ to move an item across, Shift+↑/↓ to
/// reorder — are not guessable from its shape, so the host supplies a
/// localized one-liner and it rides *with the control* rather than living only
/// in a panel footer, where a control on a surface with no footer would lose
/// it. Indented by the same blank gutter the cells reserve, so it lines up
/// under the Available column rather than under the cursor channel.
pub(crate) fn hint_row(hint: &str) -> Option<TextPropertyEntry> {
    if hint.is_empty() {
        return None;
    }
    let text = format!("{DUAL_GUTTER_BLANK}{hint}");
    let mut e = TextPropertyEntry::text(&text);
    e.inline_overlays.push(InlineOverlay {
        start: 0,
        end: text.len(),
        style: OverlayOptions {
            fg: Some(OverlayColorSpec::theme_key(KEY_PLACEHOLDER_FG)),
            ..Default::default()
        },
        properties: Default::default(),
        unit: OffsetUnit::Byte,
    });
    ensure_trailing_newline(&mut e);
    Some(e)
}

/// Kind policy for the plugin `SetDualIncluded` mutation: drop values
/// not in THIS spec's option set and preserve the cursors/active pane
/// from the previous state. The mutation arm in `plugin_dispatch` is
/// a pure delegation.
pub(crate) fn set_included_state(
    spec: &WidgetSpec,
    prev: Option<&crate::widgets::WidgetInstanceState>,
    included: &[String],
) -> crate::widgets::WidgetInstanceState {
    let sanitized = match spec {
        WidgetSpec::DualList { options, .. } => {
            crate::widgets::dual_sanitize_included(options, included)
        }
        _ => included.to_vec(),
    };
    let (active, avail_cur, incl_cur) = match prev {
        Some(crate::widgets::WidgetInstanceState::DualList {
            active_included,
            available_cursor,
            included_cursor,
            ..
        }) => (*active_included, *available_cursor, *included_cursor),
        _ => (false, 0, 0),
    };
    crate::widgets::WidgetInstanceState::DualList {
        included: sanitized,
        active_included: active,
        available_cursor: avail_cur,
        included_cursor: incl_cur,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::widgets::render::DUAL_CURSOR_ACTIVE;

    fn opts(values: &[&str]) -> Vec<DualListOption> {
        values
            .iter()
            .map(|v| DualListOption {
                value: v.to_string(),
                label: format!("{v} label"),
            })
            .collect()
    }

    fn seed<'a>(included: &'a [String], excluded: &'a [String]) -> DualListSeed<'a> {
        DualListSeed {
            included,
            excluded,
            active_included: false,
            available_cursor: 0,
            included_cursor: 0,
        }
    }

    /// Stored state outranks the seed, but is never trusted about *this*
    /// spec: options can change under it, so an included value that is no
    /// longer an option goes, and a cursor left past the end of the column it
    /// shortened would otherwise mark a row that does not exist.
    #[test]
    fn stored_state_outranks_the_seed_and_is_clamped_into_its_columns() {
        let options = opts(&["a", "b", "c"]);
        let spec_included = vec!["a".to_string()];
        let mut prev = HashMap::new();
        prev.insert(
            "k".to_string(),
            WidgetInstanceState::DualList {
                included: vec!["b".to_string(), "gone".to_string()],
                active_included: true,
                available_cursor: 9,
                included_cursor: 9,
            },
        );
        let st = resolve(&options, &seed(&spec_included, &[]), Some("k"), &prev, true);
        assert_eq!(st.included, vec!["b".to_string()]);
        assert_eq!(st.available, vec!["a".to_string(), "c".to_string()]);
        assert_eq!(st.included_cursor, 0);
        assert_eq!(st.available_cursor, 1);
        assert!(st.active_included);
    }

    /// A row answers a press only where a value sits, and reports the range
    /// it laid the cell out at rather than leaving the caller to measure it
    /// back out of the text. An empty row still reserves both gutters, so
    /// nothing shifts as the cursor moves.
    #[test]
    fn a_row_reports_a_press_target_only_where_a_value_sits() {
        let options = opts(&["a"]);
        let st = resolve(&options, &seed(&[], &[]), None, &HashMap::new(), true);

        let filled = body_row(&options, &st, 0, 8);
        let (start, end) = filled.available.expect("the Available cell holds a value");
        assert_eq!(
            &filled.entry.text[start..end],
            format!("{DUAL_CURSOR_ACTIVE}{}", cell("a label", 8))
        );
        assert_eq!(filled.included, None);

        let empty = body_row(&options, &st, 1, 8);
        assert_eq!(empty.available, None);
        assert_eq!(empty.included, None);
        assert_eq!(
            empty.entry.text.trim_end_matches('\n').chars().count(),
            filled.entry.text.trim_end_matches('\n').chars().count()
        );
    }
}
