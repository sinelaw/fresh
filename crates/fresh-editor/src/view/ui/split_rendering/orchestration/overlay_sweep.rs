//! Per-row, per-cell line-sweep over the viewport's overlays.
//!
//! Consumer side of the overlay pipeline. `super::overlays` builds
//! the `DecorationContext` (the full set of overlays clipped to the
//! viewport + a position-sorted index); `OverlayActiveSet` walks
//! that set monotonically by byte position as `render_view_lines`
//! iterates cells, so each cell knows which overlays cover its byte
//! and each row knows which overlays appeared anywhere along it
//! (fuel for the `extend_to_line_end` tail-fill).

use crate::view::overlay::Overlay;
use std::ops::Range;

/// Line-sweep over the viewport's overlays.
///
/// Driving model:
/// * One instance per `render_view_lines` call; sweep state is
///   monotonic in byte position across all view lines in the call
///   (overlays don't re-activate just because we moved to the next
///   row, mirroring how the underlying `Overlay` ranges are stored
///   in buffer-byte coordinates).
/// * Per visual row, the caller invokes `enter_row(wrap_continuation)`
///   to reset the "which overlays appeared on THIS row" set.
/// * Per cell (when the cell has a source byte), the caller invokes
///   `advance_to(bp)`; the sweep drops entries whose `range.end <=
///   bp`, admits any new entries whose `range.start <= bp` from the
///   sorted position index, and keeps `active` priority-ascending so
///   `compute_char_style` overlays them in last-write-wins z-order.
/// * After the cell loop, the row-fill block calls `fill_overlay()`
///   to pick the highest-priority `extend_to_line_end` overlay that
///   touched this row.
pub(super) struct OverlayActiveSet<'a> {
    overlays: &'a [(Overlay, Range<usize>)],
    /// Indices into `overlays`, sorted by `range.start` ascending —
    /// produced once per render call by the decorations pass.
    position_index: &'a [usize],
    /// `(range_end, overlay_index, overlay)` for every overlay whose
    /// range currently covers `last_bp`. Kept priority-ascending so
    /// the apply loop in `compute_char_style` lands higher-priority
    /// last (last write wins).
    active: Vec<(usize, usize, &'a Overlay)>,
    /// Flat slice mirror of `active` for `compute_char_style`. Only
    /// rebuilt when `active` actually mutates.
    active_refs: Vec<&'a Overlay>,
    /// Next overlay (in `position_index`) to consider for admission.
    next_pos: usize,
    /// Byte the sweep was last advanced to. Subsequent cells at the
    /// same byte (e.g. multi-cell chars) short-circuit.
    last_bp: Option<usize>,
    /// Overlay indices that appeared in `active` at any point during
    /// the current visual row — fuel for the `extend_to_line_end`
    /// tail-fill. Cleared on `enter_row`. On wrap continuations it
    /// is pre-seeded from the still-active overlays so an overlay
    /// activated on the first row of a wrap survives onto its
    /// continuation rows.
    row_touched: Vec<usize>,
}

impl<'a> OverlayActiveSet<'a> {
    pub(super) fn new(
        overlays: &'a [(Overlay, Range<usize>)],
        position_index: &'a [usize],
    ) -> Self {
        Self {
            overlays,
            position_index,
            active: Vec::new(),
            active_refs: Vec::new(),
            next_pos: 0,
            last_bp: None,
            row_touched: Vec::new(),
        }
    }

    /// Begin a new visual row. `wrap_continuation` is true when the
    /// current view line is `LineStart::AfterBreak`; on those rows
    /// inherited overlays (still in `active` from the previous row)
    /// must paint the tail fill.
    ///
    /// `row_start_byte` is the row's first source byte (when it has
    /// one). A multi-row overlay is admitted into `active` on the row
    /// its range *starts* on and stays active thereafter — so on later
    /// rows nothing re-admits it and, without seeding, `row_touched`
    /// never saw it again: a range-wide `extend_to_line_end` overlay
    /// tail-filled only its first line. Seed from the overlays whose
    /// range strictly covers the row's start byte. The strict
    /// `range.end > start` bound keeps the historical guard intact: an
    /// overlay whose `end` was bumped exactly to the next line's start
    /// (live_diff's empty-line `end = start + 1`) does not bleed onto
    /// that line.
    pub(super) fn enter_row(&mut self, wrap_continuation: bool, row_start_byte: Option<usize>) {
        self.row_touched.clear();
        if wrap_continuation {
            self.row_touched
                .extend(self.active.iter().map(|(_, idx, _)| *idx));
        } else if let Some(start) = row_start_byte {
            self.row_touched.extend(
                self.active
                    .iter()
                    .filter(|(_, idx, _)| {
                        let range = &self.overlays[*idx].1;
                        range.start <= start && range.end > start
                    })
                    .map(|(_, idx, _)| *idx),
            );
        }
    }

    /// Advance the sweep to byte position `bp`. No-op when `bp`
    /// matches `last_bp` (the cell loop hits this on every cell
    /// that maps to the same source byte).
    pub(super) fn advance_to(&mut self, bp: usize) {
        if self.last_bp == Some(bp) {
            return;
        }
        let mut dirty = false;
        if self.active.iter().any(|(end, _, _)| *end <= bp) {
            self.active.retain(|(end, _, _)| *end > bp);
            dirty = true;
        }
        while self.next_pos < self.position_index.len() {
            let idx = self.position_index[self.next_pos];
            let (overlay, range) = &self.overlays[idx];
            if range.start > bp {
                break;
            }
            // Include only when `[start, end)` is non-empty and bp is
            // inside. Zero-width overlays (start == end) are filtered
            // out, matching the prior `Range::contains` semantics.
            if range.end > bp {
                let pri = overlay.priority;
                let pos = self
                    .active
                    .iter()
                    .position(|(_, _, o)| o.priority > pri)
                    .unwrap_or(self.active.len());
                self.active.insert(pos, (range.end, idx, overlay));
                dirty = true;
                if !self.row_touched.contains(&idx) {
                    self.row_touched.push(idx);
                }
            }
            self.next_pos += 1;
        }
        if dirty {
            self.active_refs.clear();
            self.active_refs
                .extend(self.active.iter().map(|(_, _, o)| *o));
        }
        self.last_bp = Some(bp);
    }

    /// Active overlays at the byte `advance_to` was last called with,
    /// in priority-ascending order. Use for per-cell style composition.
    pub(super) fn at_cursor(&self) -> &[&'a Overlay] {
        &self.active_refs
    }

    /// Highest-priority overlay with `extend_to_line_end` that touched
    /// the current row. Drives the post-cell-loop tail-fill bg.
    pub(super) fn fill_overlay(&self) -> Option<&'a Overlay> {
        self.row_touched
            .iter()
            .map(|&idx| &self.overlays[idx].0)
            .filter(|o| o.extend_to_line_end)
            .max_by_key(|o| o.priority)
    }
}
