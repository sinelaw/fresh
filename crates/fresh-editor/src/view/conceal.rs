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
}

impl ConcealManager {
    /// Create a new empty conceal manager
    pub fn new() -> Self {
        Self { ranges: Vec::new() }
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

        self.ranges.push(ConcealRange {
            namespace,
            start_marker,
            end_marker,
            replacement,
        });
    }

    /// Remove all conceal ranges in a namespace
    pub fn clear_namespace(&mut self, namespace: &OverlayNamespace, marker_list: &mut MarkerList) {
        // Collect markers to delete
        let markers_to_delete: Vec<_> = self
            .ranges
            .iter()
            .filter(|r| &r.namespace == namespace)
            .flat_map(|r| vec![r.start_marker, r.end_marker])
            .collect();

        // Remove ranges
        self.ranges.retain(|r| &r.namespace != namespace);

        // Delete markers
        for marker_id in markers_to_delete {
            marker_list.delete(marker_id);
        }
    }

    /// Remove all conceal ranges that overlap with a byte range and clean up their markers
    pub fn remove_in_range(&mut self, range: &Range<usize>, marker_list: &mut MarkerList) {
        let markers_to_delete: Vec<_> = self
            .ranges
            .iter()
            .filter(|r| r.overlaps(range, marker_list))
            .flat_map(|r| vec![r.start_marker, r.end_marker])
            .collect();

        self.ranges.retain(|r| !r.overlaps(range, marker_list));

        for marker_id in markers_to_delete {
            marker_list.delete(marker_id);
        }
    }

    /// Clear all conceal ranges and their markers
    pub fn clear(&mut self, marker_list: &mut MarkerList) {
        for range in &self.ranges {
            marker_list.delete(range.start_marker);
            marker_list.delete(range.end_marker);
        }
        self.ranges.clear();
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
            tracing::info!(
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
}

impl Default for ConcealManager {
    fn default() -> Self {
        Self::new()
    }
}
