//! Soft break infrastructure
//!
//! Provides a marker-based system for injecting soft line breaks during rendering.
//! Used for compose-mode word wrapping: plugins register break points at byte positions,
//! and markers auto-adjust on buffer edits so breaks survive without async round-trips.
//!
//! ## Architecture
//!
//! Follows the same pattern as ConcealManager:
//! 1. Plugins add soft breaks via `addSoftBreak(bufferId, namespace, position, indent)`
//! 2. Break positions are stored with marker-based tracking (auto-adjust on edits)
//! 3. During the token pipeline, breaks are injected into the token stream
//!
//! ## Integration Point
//!
//! Soft breaks are applied to the token stream in `split_rendering.rs` BEFORE
//! conceal ranges and wrapping. This means:
//! - Concealment operates on the already-broken lines
//! - The wrapping transform sees pre-broken content

use crate::model::marker::{MarkerId, MarkerList};
use fresh_core::overlay::OverlayNamespace;

/// A soft break point that injects a line break during rendering
#[derive(Debug, Clone)]
pub struct SoftBreakPoint {
    /// Namespace for bulk operations (shared with overlay namespace system)
    pub namespace: OverlayNamespace,

    /// Marker at the break position (right affinity — shifts with inserted text)
    pub marker_id: MarkerId,

    /// Number of hanging indent spaces to insert after the break
    pub indent: u16,
}

impl SoftBreakPoint {
    /// Get the current byte position by resolving the marker
    pub fn position(&self, marker_list: &MarkerList) -> usize {
        marker_list.get_position(self.marker_id).unwrap_or(0)
    }

    /// Check if this break point falls within a byte range
    pub fn in_range(&self, start: usize, end: usize, marker_list: &MarkerList) -> bool {
        let pos = self.position(marker_list);
        pos >= start && pos < end
    }
}

/// Manages soft break points for a buffer
#[derive(Debug, Clone)]
pub struct SoftBreakManager {
    breaks: Vec<SoftBreakPoint>,
    /// Monotonic counter bumped on every mutation. Consumers that cache derived
    /// data (e.g. `LineWrapCache`) fold this into their key so any mutation
    /// invalidates stale entries automatically.
    version: u32,
}

impl SoftBreakManager {
    /// Create a new empty soft break manager
    pub fn new() -> Self {
        Self {
            breaks: Vec::new(),
            version: 0,
        }
    }

    /// Monotonic version, bumped on every mutation.
    pub fn version(&self) -> u32 {
        self.version
    }

    /// Add a soft break point
    pub fn add(
        &mut self,
        marker_list: &mut MarkerList,
        namespace: OverlayNamespace,
        position: usize,
        indent: u16,
    ) {
        let marker_id = marker_list.create(position, false); // right affinity

        self.breaks.push(SoftBreakPoint {
            namespace,
            marker_id,
            indent,
        });
        self.version = self.version.wrapping_add(1);
    }

    /// Remove all soft breaks in a namespace
    pub fn clear_namespace(&mut self, namespace: &OverlayNamespace, marker_list: &mut MarkerList) {
        let markers_to_delete: Vec<_> = self
            .breaks
            .iter()
            .filter(|b| &b.namespace == namespace)
            .map(|b| b.marker_id)
            .collect();

        let before = self.breaks.len();
        self.breaks.retain(|b| &b.namespace != namespace);

        for marker_id in markers_to_delete {
            marker_list.delete(marker_id);
        }
        if self.breaks.len() != before {
            self.version = self.version.wrapping_add(1);
        }
    }

    /// Remove all soft breaks that fall within a byte range and clean up their markers
    pub fn remove_in_range(&mut self, start: usize, end: usize, marker_list: &mut MarkerList) {
        // Resolve each break's marker position once per call (single retain pass).
        let before = self.breaks.len();
        let mut markers_to_delete = Vec::new();
        self.breaks.retain(|b| {
            let pos = marker_list.get_position(b.marker_id).unwrap_or(0);
            let in_range = pos >= start && pos < end;
            if in_range {
                markers_to_delete.push(b.marker_id);
            }
            !in_range
        });

        for marker_id in markers_to_delete {
            marker_list.delete(marker_id);
        }
        if self.breaks.len() != before {
            self.version = self.version.wrapping_add(1);
        }
    }

    /// Clear all soft breaks and their markers
    pub fn clear(&mut self, marker_list: &mut MarkerList) {
        let had_any = !self.breaks.is_empty();
        for bp in &self.breaks {
            marker_list.delete(bp.marker_id);
        }
        self.breaks.clear();
        if had_any {
            self.version = self.version.wrapping_add(1);
        }
    }

    /// Query soft breaks that fall within a viewport range.
    /// Returns sorted `(position, indent)` pairs for efficient token processing.
    pub fn query_viewport(
        &self,
        start: usize,
        end: usize,
        marker_list: &MarkerList,
    ) -> Vec<(usize, u16)> {
        let mut results: Vec<(usize, u16)> = self
            .breaks
            .iter()
            .filter_map(|b| {
                let pos = b.position(marker_list);
                if pos >= start && pos < end {
                    Some((pos, b.indent))
                } else {
                    None
                }
            })
            .collect();

        // Sort by position for sequential processing
        results.sort_by_key(|(pos, _)| *pos);

        results
    }

    /// Returns true if there are no soft breaks
    pub fn is_empty(&self) -> bool {
        self.breaks.is_empty()
    }
}

impl Default for SoftBreakManager {
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
    fn test_soft_break_remove_in_range_keeps_only_outside() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(200);
        let mut manager = SoftBreakManager::new();

        manager.add(&mut marker_list, ns(), 5, 0);
        manager.add(&mut marker_list, ns(), 25, 0);
        manager.add(&mut marker_list, ns(), 45, 0);
        manager.add(&mut marker_list, ns(), 65, 0);

        // Remove [20..50): 25 and 45 are inside, 5 and 65 stay.
        manager.remove_in_range(20, 50, &mut marker_list);

        let kept: Vec<_> = manager
            .query_viewport(0, 1000, &marker_list)
            .into_iter()
            .map(|(p, _)| p)
            .collect();
        assert_eq!(kept, vec![5, 65]);
    }

    #[test]
    fn test_soft_break_remove_in_range_endpoint_semantics() {
        // Half-open: pos == start removed, pos == end kept.
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);
        let mut manager = SoftBreakManager::new();

        manager.add(&mut marker_list, ns(), 10, 0);
        manager.add(&mut marker_list, ns(), 20, 0);

        manager.remove_in_range(10, 20, &mut marker_list);
        let kept: Vec<_> = manager
            .query_viewport(0, 1000, &marker_list)
            .into_iter()
            .map(|(p, _)| p)
            .collect();
        assert_eq!(kept, vec![20]);
    }

    #[test]
    fn test_soft_break_remove_in_range_bumps_version_only_on_change() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);
        let mut manager = SoftBreakManager::new();

        manager.add(&mut marker_list, ns(), 10, 0);
        let v0 = manager.version();

        manager.remove_in_range(50, 60, &mut marker_list);
        assert_eq!(manager.version(), v0);

        manager.remove_in_range(0, 50, &mut marker_list);
        assert!(manager.is_empty());
        assert_ne!(manager.version(), v0);
    }
}
