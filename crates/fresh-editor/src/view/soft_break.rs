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
}

impl SoftBreakManager {
    /// Create a new empty soft break manager
    pub fn new() -> Self {
        Self { breaks: Vec::new() }
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
    }

    /// Remove all soft breaks in a namespace
    pub fn clear_namespace(&mut self, namespace: &OverlayNamespace, marker_list: &mut MarkerList) {
        let markers_to_delete: Vec<_> = self
            .breaks
            .iter()
            .filter(|b| &b.namespace == namespace)
            .map(|b| b.marker_id)
            .collect();

        self.breaks.retain(|b| &b.namespace != namespace);

        for marker_id in markers_to_delete {
            marker_list.delete(marker_id);
        }
    }

    /// Remove all soft breaks that fall within a byte range and clean up their markers
    pub fn remove_in_range(&mut self, start: usize, end: usize, marker_list: &mut MarkerList) {
        let markers_to_delete: Vec<_> = self
            .breaks
            .iter()
            .filter(|b| b.in_range(start, end, marker_list))
            .map(|b| b.marker_id)
            .collect();

        self.breaks.retain(|b| !b.in_range(start, end, marker_list));

        for marker_id in markers_to_delete {
            marker_list.delete(marker_id);
        }
    }

    /// Clear all soft breaks and their markers
    pub fn clear(&mut self, marker_list: &mut MarkerList) {
        for bp in &self.breaks {
            marker_list.delete(bp.marker_id);
        }
        self.breaks.clear();
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
