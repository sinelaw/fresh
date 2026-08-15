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

struct DualListSeed<'a> {
    included: &'a [String],
    excluded: &'a [String],
    active_included: bool,
    available_cursor: usize,
    included_cursor: usize,
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
    let excluded = seed.excluded;
    // A keyed widget takes focus from the host's resolved focus key; an
    // unkeyed one falls back to the spec's initial-only `focused` hint.
    let is_focused = if key.is_some_and(|k| !k.is_empty()) {
        ctx.is_focused(key)
    } else {
        focused
    };
    let seed_state = || {
        (
            seed.included.to_vec(),
            seed.active_included,
            seed.available_cursor,
            seed.included_cursor,
        )
    };
    // Instance state is authoritative after first render.
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
    let available = dual_available_values(options, &included, excluded);
    // Clamp cursors into their columns.
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
    if let Some(k) = key {
        if !k.is_empty() {
            next_state.insert(
                k.to_string(),
                WidgetInstanceState::DualList {
                    included: included.clone(),
                    active_included,
                    available_cursor: avail_cur as u32,
                    included_cursor: incl_cur as u32,
                },
            );
        }
    }

    let col_w = dual_col_width(panel_width);
    let widget_key = key.unwrap_or("").to_string();

    // Optional label row.
    if !label.is_empty() {
        let mut e = TextPropertyEntry::text(label);
        ensure_trailing_newline(&mut e);
        out.entries.push(e);
    }
    // Header row. Each title carries the same two-column gutter its
    // cells do, and the column the keyboard is driving is marked with
    // `▾ ` plus the accent fg — so "which side am I on?" survives both
    // a monochrome terminal and a color-only reading.
    let avail_active = is_focused && !active_included;
    let incl_active = is_focused && active_included;
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
    let header_row = out.entries.len() as u32;
    out.entries.push(header_entry);

    // Body rows — one per max(available, included), at least
    // `visible_rows`.
    let body_rows = available
        .len()
        .max(included.len())
        .max(visible_rows as usize);
    for i in 0..body_rows {
        let left_val = available.get(i);
        let right_val = included.get(i);
        let left = left_val.map(|v| dual_label(options, v)).unwrap_or("");
        let right = right_val.map(|v| dual_label(options, v)).unwrap_or("");
        // Per-column cursor gutter. The active column's cursor row
        // gets the filled `▸ `, the idle column's the hollow `▹ ` so
        // both cursors are readable at once — the idle marker is what
        // tells you where Left/Right will drop you. Rows that hold no
        // cursor still reserve the two columns, so nothing shifts as
        // the cursor moves.
        let left_gutter = dual_cursor_marker(
            is_focused && left_val.is_some() && i == avail_cur,
            !active_included,
        );
        let right_gutter = dual_cursor_marker(
            is_focused && right_val.is_some() && i == incl_cur,
            active_included,
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
        if is_focused {
            let (hs, he) = if active_included {
                if right_val.is_some() && i == incl_cur {
                    (right_start, right_end)
                } else {
                    (0, 0)
                }
            } else if left_val.is_some() && i == avail_cur {
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
            let idle = if active_included {
                (left_val.is_some() && i == avail_cur).then_some(left_start)
            } else {
                (right_val.is_some() && i == incl_cur).then_some(right_start)
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
        let row = header_row + 1 + i as u32;
        // Click hit areas: clicking a cell focuses that column +
        // cursor row.
        if left_val.is_some() {
            out.hits.push(HitArea {
                overlay: false,
                widget_key: widget_key.clone(),
                widget_kind: "dual_list",
                buffer_row: row,
                byte_start: left_start,
                byte_end: left_end,
                payload: json!({ "column": "available", "index": i }),
                event_type: "dual_focus",
            });
        }
        if right_val.is_some() {
            out.hits.push(HitArea {
                overlay: false,
                widget_key: widget_key.clone(),
                widget_kind: "dual_list",
                buffer_row: row,
                byte_start: right_start,
                byte_end: right_end,
                payload: json!({ "column": "included", "index": i }),
                event_type: "dual_focus",
            });
        }
        out.entries.push(entry);
    }

    // Key hint under the columns. The control's bindings (Shift+←→ to
    // move an item across, Shift+↑↓ to reorder) aren't guessable from
    // its shape, so the host supplies a localized one-liner and it
    // rides with the control instead of only in a panel footer.
    if !hint.is_empty() {
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
        out.entries.push(e);
    }
    out
}
