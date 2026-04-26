//! Whole-buffer visual-row index.
//!
//! A second-tier cache sitting on `EditorState` alongside
//! [`LineWrapCache`](crate::view::line_wrap_cache::LineWrapCache).
//! Where `LineWrapCache` answers per-line questions ("layout for line K"),
//! this index answers whole-buffer questions:
//!
//!   * `total_rows()` — total visual row count (O(1)).
//!   * `position_at_row(r)` — `(line_idx, line_start_byte, offset_in_line)`
//!     for any visual row (O(log N_lines) via `partition_point`).
//!   * `line_first_row(i)` — cumulative visual row at the start of line `i`
//!     (O(1)).
//!
//! Storage: a parallel pair of vectors with `N_lines + 1` entries.
//!
//!     prefix_sums[i] = sum of visual row counts of logical lines 0..i
//!     line_starts[i] = byte offset where logical line i begins
//!
//!     prefix_sums[N] = total visual rows
//!     line_starts[N] = buffer length (sentinel)
//!
//! Population: derived from `LineWrapCache`. Each entry `prefix_sums[i+1]
//! - prefix_sums[i]` equals `cache_entry.len()` for line `i`. On a miss
//! the build path falls through to `compute_line_layout` (same miss
//! handler the per-line cache uses), so the row counts always match the
//! pipeline output. No second wrap implementation; no drift.
//!
//! Invalidation: keyed on the same pipeline-input version + geometry
//! that determines per-line row counts. Any version bump or width /
//! gutter / wrap-flag change → key changes → stale index becomes
//! unreachable. Build is lazy on next query.
//!
//! Replaces three independent O(N_lines) folds:
//!   * scroll math's `build_visual_row_map` (per mouse event)
//!   * scrollbar render's `scrollbar_visual_row_counts` (per frame)
//!   * `ensure_visible` wrapped scroll-up walk (per keystroke)

use crate::state::EditorState;
use crate::view::line_wrap_cache::{
    count_visual_rows_for_text, pipeline_inputs_version, CacheViewMode, LineWrapKey, WrapGeometry,
};

/// All inputs that determine the per-line visual row counts a buffer
/// produces.  Identical to `LineWrapKey`'s geometry-related fields
/// minus `line_start` (which varies across lines).  Mutating any of
/// these → different key → stale index becomes unreachable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VisualRowIndexKey {
    pub pipeline_inputs_version: u64,
    pub view_mode: CacheViewMode,
    pub effective_width: u32,
    pub gutter_width: u16,
    pub wrap_column: Option<u32>,
    pub hanging_indent: bool,
    pub line_wrap_enabled: bool,
}

impl VisualRowIndexKey {
    /// Build the matching per-line `LineWrapKey` for a given line start.
    fn line_key(&self, line_start: usize) -> LineWrapKey {
        LineWrapKey {
            pipeline_inputs_version: self.pipeline_inputs_version,
            view_mode: self.view_mode,
            line_start,
            effective_width: self.effective_width,
            gutter_width: self.gutter_width,
            wrap_column: self.wrap_column,
            hanging_indent: self.hanging_indent,
            line_wrap_enabled: self.line_wrap_enabled,
        }
    }

    fn geom(&self) -> WrapGeometry {
        WrapGeometry {
            effective_width: self.effective_width as usize,
            gutter_width: self.gutter_width as usize,
            hanging_indent: self.hanging_indent,
            wrap_column: self.wrap_column,
            line_wrap_enabled: self.line_wrap_enabled,
            view_mode: self.view_mode,
        }
    }
}

/// The index itself: prefix sums + line-start byte offsets, plus the
/// key the index was built for so callers can detect staleness.
#[derive(Debug, Clone, Default)]
pub struct VisualRowIndex {
    key: Option<VisualRowIndexKey>,
    /// `prefix_sums[i]` = total visual rows in lines 0..i.
    /// `prefix_sums.last()` = total visual rows in the buffer.
    /// Length = N_lines + 1.
    prefix_sums: Vec<u32>,
    /// `line_starts[i]` = byte offset where logical line `i` begins.
    /// `line_starts.last()` = buffer length (sentinel).
    /// Length = N_lines + 1.
    line_starts: Vec<usize>,
}

impl VisualRowIndex {
    pub fn is_built_for(&self, key: &VisualRowIndexKey) -> bool {
        self.key.as_ref() == Some(key)
    }

    /// Discard any cached state — used when the editor knows the
    /// underlying buffer changed in ways the key wouldn't catch (e.g.
    /// buffer swap).
    pub fn clear(&mut self) {
        self.key = None;
        self.prefix_sums.clear();
        self.line_starts.clear();
    }

    /// Number of logical lines covered (== `prefix_sums.len() - 1`).
    pub fn line_count(&self) -> usize {
        self.prefix_sums.len().saturating_sub(1)
    }

    /// Total visual rows across all logical lines (O(1)).
    pub fn total_rows(&self) -> u32 {
        *self.prefix_sums.last().unwrap_or(&0)
    }

    /// First visual row of logical line `line_idx` (O(1)).
    pub fn line_first_row(&self, line_idx: usize) -> u32 {
        *self.prefix_sums.get(line_idx).unwrap_or(&self.total_rows())
    }

    /// Visual row count of logical line `line_idx` (O(1)).
    pub fn line_row_count(&self, line_idx: usize) -> u32 {
        let next = self
            .prefix_sums
            .get(line_idx + 1)
            .copied()
            .unwrap_or_else(|| self.total_rows());
        next - self.line_first_row(line_idx)
    }

    /// Byte offset of the start of logical line `line_idx` (O(1)).
    pub fn line_start_byte(&self, line_idx: usize) -> usize {
        *self.line_starts.get(line_idx).unwrap_or(&0)
    }

    /// Find the logical line that contains byte offset `byte` (O(log N)).
    /// Returns `(line_idx, line_start_byte)`.  Bytes past the last line
    /// resolve to the last line.
    pub fn line_for_byte(&self, byte: usize) -> (usize, usize) {
        let n = self.line_count();
        if n == 0 {
            return (0, 0);
        }
        // `line_starts[N]` is the buffer-length sentinel; clamp so we
        // never return that index.  Largest i in 0..N such that
        // line_starts[i] <= byte.
        let p = self.line_starts.partition_point(|&s| s <= byte);
        let i = p.saturating_sub(1).min(n - 1);
        (i, self.line_starts[i])
    }

    /// Convert an absolute visual row to `(line_idx, line_start_byte,
    /// offset_in_line)`.  Saturates to the last valid row if `row` is
    /// out of range.  O(log N).
    pub fn position_at_row(&self, row: u32) -> (usize, usize, usize) {
        if self.prefix_sums.is_empty() {
            return (0, 0, 0);
        }
        let total = self.total_rows();
        let target = row.min(total.saturating_sub(1));
        // Largest i such that prefix_sums[i] <= target.
        let p = self.prefix_sums.partition_point(|&s| s <= target);
        let i = p.saturating_sub(1).min(self.line_count().saturating_sub(1));
        let offset = (target - self.prefix_sums[i]) as usize;
        (i, self.line_starts[i], offset)
    }
}

/// Ensure `state.visual_row_index` is built for `key`.  Cheap if it
/// already matches; otherwise re-walks all lines to compute per-line
/// visual-row counts.
///
/// On hit in `LineWrapCache` (renderer-populated entries for visible
/// lines), reads `entry.len()` for free.  On miss, runs the cheap
/// count-only path (`count_visual_rows_for_text` — wrap + tally,
/// skipping `ViewLineIterator` materialization and per-char `Vec<ViewLine>`
/// allocation).  Does **not** write back into `LineWrapCache` on miss:
/// the index has its own `prefix_sums` storage and doesn't need the
/// cache for its own answers, and skipping the put avoids the per-char
/// allocation churn the profile flagged (`ViewLineIterator::next` 7.4%,
/// `Vec<ViewLine>` growth ~10–15%).  Off-screen lines that are needed
/// by other consumers later will be filled by the renderer's writeback
/// when they become visible.
///
/// Skips the build entirely when the buffer is empty or `line_count()`
/// is unavailable.  Callers that need a guaranteed-built index should
/// check `is_built_for(key)` after this returns.
pub fn ensure_built(state: &mut EditorState, key: &VisualRowIndexKey) {
    if state.visual_row_index.is_built_for(key) {
        return;
    }

    let buffer_len = state.buffer.len();
    let line_count = state
        .buffer
        .line_count()
        .unwrap_or_else(|| (buffer_len / state.buffer.estimated_line_length()).max(1));
    if line_count == 0 {
        // No lines yet — store an empty index keyed so we don't rebuild
        // every call.
        state.visual_row_index = VisualRowIndex {
            key: Some(*key),
            prefix_sums: vec![0],
            line_starts: vec![0],
        };
        return;
    }

    let effective_width = key.effective_width as usize;
    let gutter_width = key.gutter_width as usize;
    let hanging_indent = key.hanging_indent;

    // Build into local Vecs first so we don't fight the borrow checker
    // when re-borrowing `state` per line.
    let mut prefix_sums: Vec<u32> = Vec::with_capacity(line_count + 1);
    let mut line_starts: Vec<usize> = Vec::with_capacity(line_count + 1);
    let mut running: u32 = 0;
    prefix_sums.push(0);

    for line_idx in 0..line_count {
        let line_start = state
            .buffer
            .line_start_offset(line_idx)
            .unwrap_or(buffer_len);
        line_starts.push(line_start);

        let line_key = key.line_key(line_start);
        let rows: u32 = if let Some(cached) = state.line_wrap_cache.get(&line_key) {
            // Renderer (or a previous full-fidelity miss handler) put
            // a real layout here — read its row count for free.
            (cached.len() as u32).max(1)
        } else if !key.line_wrap_enabled {
            // Without wrap, every logical line is exactly one visual row.
            // Don't bother running the pipeline.
            1
        } else {
            // Cache miss: compute the row count via the cheapest
            // pipeline tap — wrap-only, no ViewLine materialization.
            // We deliberately skip `LineWrapCache.put()` here: the
            // index stores `prefix_sums` standalone, and writing back
            // would cost the per-char `Vec<ViewLine>` allocation the
            // profile flagged.  When the line later becomes visible,
            // the renderer's writeback will fill the cache with the
            // full-fidelity layout.
            let Some(bytes) = state.buffer.get_line(line_idx) else {
                // Best-effort: missing line still counts as 1 row.
                running = running.saturating_add(1);
                prefix_sums.push(running);
                continue;
            };
            let line_content = String::from_utf8_lossy(&bytes);
            let trimmed = line_content.trim_end_matches('\n').trim_end_matches('\r');
            count_visual_rows_for_text(trimmed, effective_width, gutter_width, hanging_indent)
        };

        running = running.saturating_add(rows);
        prefix_sums.push(running);
    }
    line_starts.push(buffer_len);

    state.visual_row_index = VisualRowIndex {
        key: Some(*key),
        prefix_sums,
        line_starts,
    };
}

/// Convenience: build the index for `state` from a `WrapGeometry`,
/// using the state's current pipeline-input versions.
pub fn ensure_built_from_geom(state: &mut EditorState, geom: &WrapGeometry) {
    let key = VisualRowIndexKey {
        pipeline_inputs_version: pipeline_inputs_version(
            state.buffer.version(),
            state.soft_breaks.version(),
            state.conceals.version(),
        ),
        view_mode: geom.view_mode,
        effective_width: geom.effective_width as u32,
        gutter_width: geom.gutter_width as u16,
        wrap_column: geom.wrap_column,
        hanging_indent: geom.hanging_indent,
        line_wrap_enabled: geom.line_wrap_enabled,
    };
    ensure_built(state, &key);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn idx_with(prefix: Vec<u32>, starts: Vec<usize>) -> VisualRowIndex {
        VisualRowIndex {
            key: Some(VisualRowIndexKey {
                pipeline_inputs_version: 0,
                view_mode: CacheViewMode::Source,
                effective_width: 80,
                gutter_width: 6,
                wrap_column: None,
                hanging_indent: false,
                line_wrap_enabled: true,
            }),
            prefix_sums: prefix,
            line_starts: starts,
        }
    }

    #[test]
    fn empty_index_total_is_zero() {
        let idx = VisualRowIndex::default();
        assert_eq!(idx.total_rows(), 0);
        assert_eq!(idx.line_count(), 0);
    }

    #[test]
    fn single_line_one_row() {
        let idx = idx_with(vec![0, 1], vec![0, 10]);
        assert_eq!(idx.total_rows(), 1);
        assert_eq!(idx.line_count(), 1);
        assert_eq!(idx.line_first_row(0), 0);
        assert_eq!(idx.line_row_count(0), 1);
        assert_eq!(idx.position_at_row(0), (0, 0, 0));
    }

    #[test]
    fn multi_line_no_wrap() {
        // 3 lines, 1 row each.
        let idx = idx_with(vec![0, 1, 2, 3], vec![0, 10, 20, 30]);
        assert_eq!(idx.total_rows(), 3);
        assert_eq!(idx.line_count(), 3);
        assert_eq!(idx.position_at_row(0), (0, 0, 0));
        assert_eq!(idx.position_at_row(1), (1, 10, 0));
        assert_eq!(idx.position_at_row(2), (2, 20, 0));
        // Out-of-range saturates to the last row.
        assert_eq!(idx.position_at_row(99), (2, 20, 0));
    }

    #[test]
    fn wrapped_line_offsets() {
        // Line 0: 1 row.  Line 1: 3 rows (wrapped).  Line 2: 2 rows.
        let idx = idx_with(vec![0, 1, 4, 6], vec![0, 10, 200, 300]);
        assert_eq!(idx.total_rows(), 6);
        assert_eq!(idx.line_row_count(0), 1);
        assert_eq!(idx.line_row_count(1), 3);
        assert_eq!(idx.line_row_count(2), 2);
        // Row 0 → line 0, offset 0.
        assert_eq!(idx.position_at_row(0), (0, 0, 0));
        // Rows 1..4 → line 1, offsets 0..3.
        assert_eq!(idx.position_at_row(1), (1, 10, 0));
        assert_eq!(idx.position_at_row(2), (1, 10, 1));
        assert_eq!(idx.position_at_row(3), (1, 10, 2));
        // Rows 4..6 → line 2, offsets 0..2.
        assert_eq!(idx.position_at_row(4), (2, 200, 0));
        assert_eq!(idx.position_at_row(5), (2, 200, 1));
    }

    #[test]
    fn line_for_byte_resolves_to_containing_line() {
        let idx = idx_with(vec![0, 1, 2, 3], vec![0, 10, 20, 30]);
        assert_eq!(idx.line_for_byte(0), (0, 0));
        assert_eq!(idx.line_for_byte(5), (0, 0));
        assert_eq!(idx.line_for_byte(10), (1, 10));
        assert_eq!(idx.line_for_byte(15), (1, 10));
        assert_eq!(idx.line_for_byte(20), (2, 20));
        assert_eq!(idx.line_for_byte(29), (2, 20));
        // Past last line start: maps to last line index.
        assert_eq!(idx.line_for_byte(99), (2, 20));
    }

    #[test]
    fn is_built_for_detects_key_mismatch() {
        let idx = idx_with(vec![0, 1], vec![0, 10]);
        let mut k = idx.key.unwrap();
        assert!(idx.is_built_for(&k));
        k.effective_width += 1;
        assert!(!idx.is_built_for(&k));
    }

    #[test]
    fn clear_resets_to_default() {
        let mut idx = idx_with(vec![0, 1, 2, 3], vec![0, 10, 20, 30]);
        idx.clear();
        assert_eq!(idx.total_rows(), 0);
        assert_eq!(idx.line_count(), 0);
        assert!(idx.key.is_none());
    }
}
