//! Conceal range infrastructure
//!
//! Provides a system for hiding or replacing ranges of buffer text during rendering.
//! Used for Typora-style "seamless canvas" markdown: hiding syntax markers like `**`
//! around bold text, `[](url)` around links, etc.
//!
//! ## Architecture
//!
//! Follows the same pattern as OverlayManager:
//! 1. Plugins add conceal ranges via `addConceal(bufferId, namespace, start, end, options)`
//! 2. Ranges are stored with marker-based position tracking (auto-adjust on edits)
//! 3. During token pipeline, concealed byte ranges are filtered/replaced
//!
//! ## Integration Point
//!
//! Conceal ranges are applied to the token stream in `split_rendering.rs` after
//! plugin view transforms but before wrapping. This means:
//! - Plugin transforms see the original (unconcealed) tokens
//! - Concealment happens transparently at the editor level
//! - Wrapping operates on the concealed (shorter) lines

use crate::model::marker::{MarkerId, MarkerList};
use fresh_core::overlay::OverlayNamespace;
use std::collections::HashMap;
use std::ops::Range;

/// A conceal range hides or replaces a byte range during rendering
#[derive(Debug, Clone)]
pub struct ConcealRange {
    /// Namespace for bulk operations (shared with overlay namespace system)
    pub namespace: OverlayNamespace,

    /// Start marker (left affinity - stays before inserted text)
    pub start_marker: MarkerId,

    /// End marker (right affinity - moves after inserted text)
    pub end_marker: MarkerId,

    /// Optional replacement text to show instead of the concealed content.
    /// If None, the range is simply hidden (zero-width).
    pub replacement: Option<String>,
}

impl ConcealRange {
    /// Get the current byte range by resolving markers
    pub fn range(&self, marker_list: &MarkerList) -> Range<usize> {
        let start = marker_list.get_position(self.start_marker).unwrap_or(0);
        let end = marker_list.get_position(self.end_marker).unwrap_or(0);
        start..end
    }

    /// Check if this range overlaps with another range
    pub fn overlaps(&self, range: &Range<usize>, marker_list: &MarkerList) -> bool {
        let self_range = self.range(marker_list);
        self_range.start < range.end && range.start < self_range.end
    }
}

/// Manages conceal ranges for a buffer
#[derive(Debug, Clone)]
pub struct ConcealManager {
    ranges: Vec<ConcealRange>,
    /// `MarkerId -> index into ranges` for O(log N + k) `remove_in_range`.
    /// Both endpoints of each range are registered. Kept in sync with
    /// every push / swap_remove on `ranges`.
    marker_to_idx: HashMap<MarkerId, usize>,
    /// Monotonic counter bumped on every mutation. Consumers that cache derived
    /// data (e.g. `LineWrapCache`) fold this into their key so any mutation
    /// invalidates stale entries automatically.
    version: u32,
}

impl ConcealManager {
    /// Create a new empty conceal manager
    pub fn new() -> Self {
        Self {
            ranges: Vec::new(),
            marker_to_idx: HashMap::new(),
            version: 0,
        }
    }

    /// Monotonic version, bumped on every mutation.
    pub fn version(&self) -> u32 {
        self.version
    }

    /// Add a conceal range
    pub fn add(
        &mut self,
        marker_list: &mut MarkerList,
        namespace: OverlayNamespace,
        range: Range<usize>,
        replacement: Option<String>,
    ) {
        let start_marker = marker_list.create(range.start, true); // left affinity
        let end_marker = marker_list.create(range.end, false); // right affinity

        let idx = self.ranges.len();
        self.marker_to_idx.insert(start_marker, idx);
        self.marker_to_idx.insert(end_marker, idx);
        self.ranges.push(ConcealRange {
            namespace,
            start_marker,
            end_marker,
            replacement,
        });
        self.version = self.version.wrapping_add(1);
    }

    /// Remove all conceal ranges in a namespace
    pub fn clear_namespace(&mut self, namespace: &OverlayNamespace, marker_list: &mut MarkerList) {
        // Collect indices and markers up-front, then remove via swap_remove
        // (descending order so indices stay valid).
        let mut indices: Vec<usize> = self
            .ranges
            .iter()
            .enumerate()
            .filter_map(|(i, r)| (&r.namespace == namespace).then_some(i))
            .collect();
        if indices.is_empty() {
            return;
        }
        indices.sort_unstable_by(|a, b| b.cmp(a));
        for idx in indices {
            self.swap_remove_at(idx, marker_list);
        }
        self.version = self.version.wrapping_add(1);
    }

    /// Remove all conceal ranges that overlap with a byte range and clean up their markers
    pub fn remove_in_range(&mut self, range: &Range<usize>, marker_list: &mut MarkerList) {
        // O(log N + k): query the marker tree for endpoints near `range`,
        // map back to entries, then verify each candidate's true range
        // since `query_range` is closed-interval and the marker at the
        // exact upper bound represents a one-past-the-end position.
        // Spanning conceals (start < range.start && end > range.end) are
        // not detected; document the precondition in the type's contract.
        if range.start >= range.end {
            return;
        }
        let hits = marker_list.query_range(range.start, range.end);
        if hits.is_empty() {
            return;
        }
        let mut candidates: Vec<usize> = hits
            .iter()
            .filter_map(|(mid, _, _)| self.marker_to_idx.get(mid).copied())
            .collect();
        candidates.sort_unstable();
        candidates.dedup();

        let mut to_remove: Vec<usize> = candidates
            .into_iter()
            .filter(|&idx| {
                let r = &self.ranges[idx];
                let start = marker_list.get_position(r.start_marker).unwrap_or(0);
                let end = marker_list.get_position(r.end_marker).unwrap_or(0);
                start < range.end && range.start < end
            })
            .collect();
        if to_remove.is_empty() {
            return;
        }
        // Descending so swap_remove doesn't shift earlier indices.
        to_remove.sort_unstable_by(|a, b| b.cmp(a));
        for idx in to_remove {
            self.swap_remove_at(idx, marker_list);
        }
        self.version = self.version.wrapping_add(1);
    }

    /// Clear all conceal ranges and their markers
    pub fn clear(&mut self, marker_list: &mut MarkerList) {
        let had_any = !self.ranges.is_empty();
        for range in &self.ranges {
            marker_list.delete(range.start_marker);
            marker_list.delete(range.end_marker);
        }
        self.ranges.clear();
        self.marker_to_idx.clear();
        if had_any {
            self.version = self.version.wrapping_add(1);
        }
    }

    /// Swap-remove the entry at `idx`, deleting its markers and patching
    /// `marker_to_idx` for whatever entry got swapped in. Does NOT bump
    /// the version — callers do that at batch boundaries.
    fn swap_remove_at(&mut self, idx: usize, marker_list: &mut MarkerList) {
        let removed = self.ranges.swap_remove(idx);
        self.marker_to_idx.remove(&removed.start_marker);
        self.marker_to_idx.remove(&removed.end_marker);
        marker_list.delete(removed.start_marker);
        marker_list.delete(removed.end_marker);
        if let Some(moved) = self.ranges.get(idx) {
            self.marker_to_idx.insert(moved.start_marker, idx);
            self.marker_to_idx.insert(moved.end_marker, idx);
        }
    }

    /// Query conceal ranges that overlap a viewport range.
    /// Returns ranges sorted by start position for efficient token filtering.
    pub fn query_viewport(
        &self,
        start: usize,
        end: usize,
        marker_list: &MarkerList,
    ) -> Vec<(Range<usize>, Option<&str>)> {
        let mut results: Vec<(Range<usize>, Option<&str>)> = self
            .ranges
            .iter()
            .filter_map(|r| {
                let range = r.range(marker_list);
                if range.start < end && start < range.end {
                    Some((range, r.replacement.as_deref()))
                } else {
                    None
                }
            })
            .collect();

        // Sort by start position for sequential processing
        results.sort_by_key(|(range, _)| range.start);

        // Debug: log conceal ranges being applied during render
        if !results.is_empty() {
            let summary: Vec<String> = results
                .iter()
                .map(|(r, repl)| format!("{}..{}={}", r.start, r.end, repl.unwrap_or("hide")))
                .collect();
            tracing::trace!(
                "[conceal] query_viewport({start}..{end}): {} ranges: {}",
                results.len(),
                summary.join(", ")
            );
        }

        results
    }

    /// Check if a byte position is inside any conceal range.
    /// Returns the conceal info if concealed.
    pub fn is_concealed(
        &self,
        position: usize,
        marker_list: &MarkerList,
    ) -> Option<(Range<usize>, Option<&str>)> {
        for r in &self.ranges {
            let range = r.range(marker_list);
            if range.contains(&position) {
                return Some((range, r.replacement.as_deref()));
            }
        }
        None
    }

    /// Returns true if there are no conceal ranges
    pub fn is_empty(&self) -> bool {
        self.ranges.is_empty()
    }

    /// Test-only: assert `marker_to_idx` is consistent with `ranges`.
    /// Panics on any divergence. Used by property tests.
    #[cfg(test)]
    fn check_invariants(&self) {
        assert_eq!(
            self.marker_to_idx.len(),
            self.ranges.len() * 2,
            "marker_to_idx size != 2 * ranges.len()"
        );
        for (i, r) in self.ranges.iter().enumerate() {
            assert_eq!(
                self.marker_to_idx.get(&r.start_marker).copied(),
                Some(i),
                "start_marker {:?} of range {} mismapped",
                r.start_marker,
                i,
            );
            assert_eq!(
                self.marker_to_idx.get(&r.end_marker).copied(),
                Some(i),
                "end_marker {:?} of range {} mismapped",
                r.end_marker,
                i,
            );
        }
    }
}

impl Default for ConcealManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ns() -> OverlayNamespace {
        OverlayNamespace::from_string("test".to_string())
    }

    #[test]
    fn test_conceal_remove_in_range_keeps_only_disjoint() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(200);
        let mut manager = ConcealManager::new();

        manager.add(&mut marker_list, ns(), 0..5, None);
        manager.add(&mut marker_list, ns(), 10..20, None);
        manager.add(&mut marker_list, ns(), 30..40, None);
        manager.add(&mut marker_list, ns(), 50..60, None);

        manager.remove_in_range(&(15..35), &mut marker_list);

        let kept: Vec<_> = manager
            .query_viewport(0, 1000, &marker_list)
            .into_iter()
            .map(|(r, _)| r)
            .collect();
        assert_eq!(kept, vec![0..5, 50..60]);
    }

    #[test]
    fn test_conceal_remove_in_range_deletes_markers_and_bumps_version() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);
        let mut manager = ConcealManager::new();

        manager.add(&mut marker_list, ns(), 10..20, None);
        let v0 = manager.version();

        manager.remove_in_range(&(0..50), &mut marker_list);
        assert!(manager.is_empty());
        assert_ne!(manager.version(), v0);
    }

    #[test]
    fn test_conceal_remove_in_range_no_match_keeps_version() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);
        let mut manager = ConcealManager::new();

        manager.add(&mut marker_list, ns(), 10..20, None);
        let v0 = manager.version();

        manager.remove_in_range(&(50..60), &mut marker_list);
        assert!(!manager.is_empty());
        assert_eq!(manager.version(), v0);
    }

    #[test]
    fn test_conceal_remove_in_range_endpoint_semantics() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);
        let mut manager = ConcealManager::new();

        manager.add(&mut marker_list, ns(), 10..20, None);

        manager.remove_in_range(&(20..30), &mut marker_list);
        assert!(!manager.is_empty());
        manager.remove_in_range(&(0..10), &mut marker_list);
        assert!(!manager.is_empty());
        manager.remove_in_range(&(19..21), &mut marker_list);
        assert!(manager.is_empty());
    }

    /// Mirrors the production cycle in `markdown_compose.ts`: for each line
    /// in a `lines_changed` batch, clear conceals in the line's byte range,
    /// then re-add the line's conceals. Steady-state entry count holds
    /// throughout, exactly like the plugin's per-line rebuild.
    ///
    /// Run with:
    ///   cargo nextest run -p fresh-editor --no-capture \
    ///     view::conceal::tests::perf_full_buffer_rebuild_pass
    #[test]
    fn perf_full_buffer_rebuild_pass() {
        const LINES: usize = 500;
        const LINE_BYTES: usize = 50;
        const CONCEALS_PER_LINE: usize = 5;

        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(LINES * LINE_BYTES);
        let mut manager = ConcealManager::new();

        let conceal_byte = |line: usize, k: usize| -> usize {
            line * LINE_BYTES + k * (LINE_BYTES / CONCEALS_PER_LINE)
        };

        // Populate steady state (mirrors first-render: every line has its conceals).
        for line in 0..LINES {
            for k in 0..CONCEALS_PER_LINE {
                let s = conceal_byte(line, k);
                manager.add(&mut marker_list, ns(), s..(s + 2), None);
            }
        }
        let initial = LINES * CONCEALS_PER_LINE;

        // One full-buffer `lines_changed` pass: per line, clear + re-add.
        let start = std::time::Instant::now();
        for line in 0..LINES {
            let line_range = (line * LINE_BYTES)..((line + 1) * LINE_BYTES);
            manager.remove_in_range(&line_range, &mut marker_list);
            for k in 0..CONCEALS_PER_LINE {
                let s = conceal_byte(line, k);
                manager.add(&mut marker_list, ns(), s..(s + 2), None);
            }
        }
        let elapsed = start.elapsed();

        eprintln!(
            "[perf] conceal full-buffer rebuild ({LINES} lines, {} entries steady): \
             {:?} total, {:?}/line",
            initial,
            elapsed,
            elapsed / LINES as u32,
        );
        // Steady state preserved.
        let still_present = manager
            .query_viewport(0, LINES * LINE_BYTES, &marker_list)
            .len();
        assert_eq!(still_present, initial);
    }

    mod proptests {
        use super::*;
        use proptest::prelude::*;

        #[derive(Debug, Clone)]
        enum Op {
            // Length capped so the spanning-conceal precondition holds:
            // every conceal is shorter than the smallest query range.
            Add {
                start: usize,
                len: usize,
                ns_idx: u8,
            },
            RemoveInRange {
                start: usize,
                end: usize,
            },
            ClearNamespace {
                ns_idx: u8,
            },
        }

        const BUFFER_SIZE: usize = 200;
        const MAX_CONCEAL_LEN: usize = 4;
        const MIN_QUERY_LEN: usize = MAX_CONCEAL_LEN + 1;

        fn arb_op() -> impl Strategy<Value = Op> {
            prop_oneof![
                3 => (0..(BUFFER_SIZE - MAX_CONCEAL_LEN), 1..=MAX_CONCEAL_LEN, 0u8..3u8)
                    .prop_map(|(start, len, ns_idx)| Op::Add { start, len, ns_idx }),
                2 => (0..BUFFER_SIZE, MIN_QUERY_LEN..=BUFFER_SIZE)
                    .prop_map(|(start, qlen)| {
                        let s = start.min(BUFFER_SIZE - 1);
                        let e = (s + qlen).min(BUFFER_SIZE);
                        Op::RemoveInRange { start: s, end: e }
                    }),
                1 => (0u8..3u8).prop_map(|ns_idx| Op::ClearNamespace { ns_idx }),
            ]
        }

        fn nsf(idx: u8) -> OverlayNamespace {
            OverlayNamespace::from_string(format!("ns{idx}"))
        }

        proptest! {
            /// Invariants must hold after every sequence of operations.
            /// Plus: after `remove_in_range(r)`, no surviving conceal's
            /// range overlaps `r` — given the precondition that conceals
            /// are no longer than the query range.
            #[test]
            fn prop_marker_index_consistent(ops in prop::collection::vec(arb_op(), 0..40)) {
                let mut marker_list = MarkerList::new();
                marker_list.set_buffer_size(BUFFER_SIZE);
                let mut manager = ConcealManager::new();

                for op in ops {
                    match op {
                        Op::Add { start, len, ns_idx } => {
                            manager.add(&mut marker_list, nsf(ns_idx), start..(start + len), None);
                        }
                        Op::RemoveInRange { start, end } => {
                            manager.remove_in_range(&(start..end), &mut marker_list);
                            for (rng, _) in manager.query_viewport(0, BUFFER_SIZE, &marker_list) {
                                let overlaps = rng.start < end && start < rng.end;
                                prop_assert!(
                                    !overlaps,
                                    "conceal {:?} survived remove_in_range({start}..{end})",
                                    rng,
                                );
                            }
                        }
                        Op::ClearNamespace { ns_idx } => {
                            manager.clear_namespace(&nsf(ns_idx), &mut marker_list);
                        }
                    }
                    manager.check_invariants();
                }
            }
        }
    }
}
