use crate::model::marker::{MarkerId, MarkerList};
use ratatui::style::{Color, Style};
use std::ops::Range;
use std::sync::atomic::{AtomicU64, Ordering};

/// Opaque handle for an overlay, returned to plugins for later removal.
/// Internally a String (can be UUID, composite key, etc.) but plugins treat it as opaque.
/// This is stable across text edits (unlike line-number-based IDs).
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct OverlayHandle(pub String);

impl OverlayHandle {
    /// Generate a new unique handle
    pub fn new() -> Self {
        static NEXT_HANDLE: AtomicU64 = AtomicU64::new(1);
        Self(format!(
            "ovl_{}",
            NEXT_HANDLE.fetch_add(1, Ordering::Relaxed)
        ))
    }

    /// Create a handle from a string (for internal use)
    pub fn from_string(s: String) -> Self {
        Self(s)
    }

    /// Get the internal string representation
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl Default for OverlayHandle {
    fn default() -> Self {
        Self::new()
    }
}

/// Namespace for grouping overlays (for efficient bulk removal).
/// Plugins create a namespace once and use it for all their overlays.
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct OverlayNamespace(pub String);

impl OverlayNamespace {
    /// Generate a new unique namespace
    pub fn new() -> Self {
        static NEXT_NAMESPACE: AtomicU64 = AtomicU64::new(1);
        Self(format!(
            "ns_{}",
            NEXT_NAMESPACE.fetch_add(1, Ordering::Relaxed)
        ))
    }

    /// Create a namespace from a string (for plugin registration)
    pub fn from_string(s: String) -> Self {
        Self(s)
    }

    /// Get the internal string representation
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl Default for OverlayNamespace {
    fn default() -> Self {
        Self::new()
    }
}

/// Overlay face - defines the visual appearance of an overlay
#[derive(Debug, Clone, PartialEq)]
pub enum OverlayFace {
    /// Underline with a specific style
    Underline { color: Color, style: UnderlineStyle },
    /// Background color
    Background { color: Color },
    /// Foreground (text) color
    Foreground { color: Color },
    /// Combined style with multiple attributes
    Style { style: Style },
}

/// Style of underline
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnderlineStyle {
    /// Straight line
    Straight,
    /// Wavy/squiggly line (for errors)
    Wavy,
    /// Dotted line
    Dotted,
    /// Dashed line
    Dashed,
}

/// Priority for overlay z-ordering
/// Higher priority overlays are rendered on top of lower priority ones
pub type Priority = i32;

/// An overlay represents a visual decoration over a range of text
/// Uses markers for content-anchored positions that automatically adjust with edits
#[derive(Debug, Clone)]
pub struct Overlay {
    /// Unique handle for this overlay (opaque, for removal by handle)
    pub handle: OverlayHandle,

    /// Namespace this overlay belongs to (for bulk removal)
    pub namespace: Option<OverlayNamespace>,

    /// Start marker (left affinity - stays before inserted text)
    pub start_marker: MarkerId,

    /// End marker (right affinity - moves after inserted text)
    pub end_marker: MarkerId,

    /// Visual appearance of the overlay
    pub face: OverlayFace,

    /// Priority for z-ordering (higher = on top)
    pub priority: Priority,

    /// Optional tooltip/message to show when hovering over this overlay
    pub message: Option<String>,

    /// Whether to extend the overlay's background to the end of the visual line
    /// Used for full-width line highlighting (e.g., in diff views)
    pub extend_to_line_end: bool,
}

impl Overlay {
    /// Create a new overlay with markers at the given range
    ///
    /// # Arguments
    /// * `marker_list` - MarkerList to create markers in
    /// * `range` - Byte range for the overlay
    /// * `face` - Visual appearance
    ///
    /// Returns the overlay (which contains its handle for later removal)
    pub fn new(marker_list: &mut MarkerList, range: Range<usize>, face: OverlayFace) -> Self {
        let start_marker = marker_list.create(range.start, true); // left affinity
        let end_marker = marker_list.create(range.end, false); // right affinity

        Self {
            handle: OverlayHandle::new(),
            namespace: None,
            start_marker,
            end_marker,
            face,
            priority: 0,
            message: None,
            extend_to_line_end: false,
        }
    }

    /// Create an overlay with a namespace (for bulk removal)
    pub fn with_namespace(
        marker_list: &mut MarkerList,
        range: Range<usize>,
        face: OverlayFace,
        namespace: OverlayNamespace,
    ) -> Self {
        let mut overlay = Self::new(marker_list, range, face);
        overlay.namespace = Some(namespace);
        overlay
    }

    /// Create an overlay with a specific priority
    pub fn with_priority(
        marker_list: &mut MarkerList,
        range: Range<usize>,
        face: OverlayFace,
        priority: Priority,
    ) -> Self {
        let mut overlay = Self::new(marker_list, range, face);
        overlay.priority = priority;
        overlay
    }

    /// Add a message/tooltip to this overlay
    pub fn with_message(mut self, message: String) -> Self {
        self.message = Some(message);
        self
    }

    /// Set the priority
    pub fn with_priority_value(mut self, priority: Priority) -> Self {
        self.priority = priority;
        self
    }

    /// Set the namespace
    pub fn with_namespace_value(mut self, namespace: OverlayNamespace) -> Self {
        self.namespace = Some(namespace);
        self
    }

    /// Set whether to extend the overlay to the end of the visual line
    pub fn with_extend_to_line_end(mut self, extend: bool) -> Self {
        self.extend_to_line_end = extend;
        self
    }

    /// Get the current byte range by resolving markers
    /// This is called once per frame during rendering setup
    pub fn range(&self, marker_list: &MarkerList) -> Range<usize> {
        let start = marker_list.get_position(self.start_marker).unwrap_or(0);
        let end = marker_list.get_position(self.end_marker).unwrap_or(0);
        start..end
    }

    /// Check if this overlay contains a position
    pub fn contains(&self, position: usize, marker_list: &MarkerList) -> bool {
        self.range(marker_list).contains(&position)
    }

    /// Check if this overlay overlaps with a range
    pub fn overlaps(&self, range: &Range<usize>, marker_list: &MarkerList) -> bool {
        let self_range = self.range(marker_list);
        self_range.start < range.end && range.start < self_range.end
    }
}

/// Manages overlays for a buffer
/// Overlays are sorted by priority for efficient rendering
#[derive(Debug, Clone)]
pub struct OverlayManager {
    /// All active overlays, indexed for O(1) lookup by handle
    overlays: Vec<Overlay>,
}

impl OverlayManager {
    /// Create a new empty overlay manager
    pub fn new() -> Self {
        Self {
            overlays: Vec::new(),
        }
    }

    /// Add an overlay and return its handle for later removal
    pub fn add(&mut self, overlay: Overlay) -> OverlayHandle {
        let handle = overlay.handle.clone();
        self.overlays.push(overlay);
        // Keep sorted by priority (ascending - lower priority first)
        self.overlays.sort_by_key(|o| o.priority);
        handle
    }

    /// Remove an overlay by its handle
    pub fn remove_by_handle(
        &mut self,
        handle: &OverlayHandle,
        marker_list: &mut MarkerList,
    ) -> bool {
        if let Some(pos) = self.overlays.iter().position(|o| &o.handle == handle) {
            let overlay = self.overlays.remove(pos);
            marker_list.delete(overlay.start_marker);
            marker_list.delete(overlay.end_marker);
            true
        } else {
            false
        }
    }

    /// Remove all overlays in a namespace
    pub fn clear_namespace(&mut self, namespace: &OverlayNamespace, marker_list: &mut MarkerList) {
        // Collect markers to delete
        let markers_to_delete: Vec<_> = self
            .overlays
            .iter()
            .filter(|o| o.namespace.as_ref() == Some(namespace))
            .flat_map(|o| vec![o.start_marker, o.end_marker])
            .collect();

        // Remove overlays
        self.overlays
            .retain(|o| o.namespace.as_ref() != Some(namespace));

        // Delete markers
        for marker_id in markers_to_delete {
            marker_list.delete(marker_id);
        }
    }

    /// Replace overlays in a namespace that overlap a range with new overlays.
    ///
    /// This preserves overlays outside the range, which helps avoid flicker and
    /// unnecessary marker churn during viewport-only updates.
    pub fn replace_range_in_namespace(
        &mut self,
        namespace: &OverlayNamespace,
        range: &Range<usize>,
        mut new_overlays: Vec<Overlay>,
        marker_list: &mut MarkerList,
    ) {
        let mut markers_to_delete = Vec::new();

        self.overlays.retain(|overlay| {
            let in_namespace = overlay.namespace.as_ref() == Some(namespace);
            if in_namespace && overlay.overlaps(range, marker_list) {
                markers_to_delete.push(overlay.start_marker);
                markers_to_delete.push(overlay.end_marker);
                false
            } else {
                true
            }
        });

        for marker_id in markers_to_delete {
            marker_list.delete(marker_id);
        }

        if !new_overlays.is_empty() {
            self.overlays.append(&mut new_overlays);
            self.overlays.sort_by_key(|o| o.priority);
        }
    }

    /// Remove all overlays in a range and clean up their markers
    pub fn remove_in_range(&mut self, range: &Range<usize>, marker_list: &mut MarkerList) {
        // Collect markers to delete
        let markers_to_delete: Vec<_> = self
            .overlays
            .iter()
            .filter(|o| o.overlaps(range, marker_list))
            .flat_map(|o| vec![o.start_marker, o.end_marker])
            .collect();

        // Remove overlays
        self.overlays.retain(|o| !o.overlaps(range, marker_list));

        // Delete markers
        for marker_id in markers_to_delete {
            marker_list.delete(marker_id);
        }
    }

    /// Clear all overlays and their markers
    pub fn clear(&mut self, marker_list: &mut MarkerList) {
        // Delete all markers
        for overlay in &self.overlays {
            marker_list.delete(overlay.start_marker);
            marker_list.delete(overlay.end_marker);
        }

        self.overlays.clear();
    }

    /// Get all overlays at a specific position, sorted by priority
    pub fn at_position(&self, position: usize, marker_list: &MarkerList) -> Vec<&Overlay> {
        self.overlays
            .iter()
            .filter(|o| {
                let range = o.range(marker_list);
                range.contains(&position)
            })
            .collect()
    }

    /// Get all overlays that overlap with a range, sorted by priority
    pub fn in_range(&self, range: &Range<usize>, marker_list: &MarkerList) -> Vec<&Overlay> {
        self.overlays
            .iter()
            .filter(|o| o.overlaps(range, marker_list))
            .collect()
    }

    /// Query overlays in a viewport range efficiently using the marker interval tree
    ///
    /// This is much faster than calling `at_position()` for every character in the range.
    /// Returns overlays with their resolved byte ranges.
    ///
    /// # Performance
    /// - Old approach: O(N * M) where N = positions to check, M = overlay count
    /// - This approach: O(log M + k) where k = overlays in viewport (typically 2-10)
    pub fn query_viewport(
        &self,
        start: usize,
        end: usize,
        marker_list: &MarkerList,
    ) -> Vec<(&Overlay, Range<usize>)> {
        use std::collections::HashMap;

        // Query the marker interval tree once for all markers in viewport
        // This is O(log N + k) where k = markers in viewport
        let visible_markers = marker_list.query_range(start, end);

        // Build a quick lookup map: marker_id -> position
        let marker_positions: HashMap<_, _> = visible_markers
            .into_iter()
            .map(|(id, start, _end)| (id, start))
            .collect();

        // Find overlays whose markers are in the viewport
        // Only resolve positions for overlays that are actually visible
        self.overlays
            .iter()
            .filter_map(|overlay| {
                // Try to get positions from our viewport query results
                let start_pos = marker_positions.get(&overlay.start_marker)?;
                let end_pos = marker_positions.get(&overlay.end_marker)?;

                let range = *start_pos..*end_pos;

                // Only include if actually overlaps viewport
                if range.start < end && range.end > start {
                    Some((overlay, range))
                } else {
                    None
                }
            })
            .collect()
    }

    /// Get overlay by handle
    pub fn get_by_handle(&self, handle: &OverlayHandle) -> Option<&Overlay> {
        self.overlays.iter().find(|o| &o.handle == handle)
    }

    /// Get mutable overlay by handle
    pub fn get_by_handle_mut(&mut self, handle: &OverlayHandle) -> Option<&mut Overlay> {
        self.overlays.iter_mut().find(|o| &o.handle == handle)
    }

    /// Get total number of overlays
    pub fn len(&self) -> usize {
        self.overlays.len()
    }

    /// Check if there are any overlays
    pub fn is_empty(&self) -> bool {
        self.overlays.is_empty()
    }

    /// Get all overlays (for rendering)
    pub fn all(&self) -> &[Overlay] {
        &self.overlays
    }
}

impl Default for OverlayManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Helper functions for creating common overlay types
impl Overlay {
    /// Create an error underline overlay (wavy red line)
    pub fn error(
        marker_list: &mut MarkerList,
        range: Range<usize>,
        message: Option<String>,
    ) -> Self {
        let mut overlay = Self::with_priority(
            marker_list,
            range,
            OverlayFace::Underline {
                color: Color::Red,
                style: UnderlineStyle::Wavy,
            },
            10, // Higher priority for errors
        );
        overlay.message = message;
        overlay
    }

    /// Create a warning underline overlay (wavy yellow line)
    pub fn warning(
        marker_list: &mut MarkerList,
        range: Range<usize>,
        message: Option<String>,
    ) -> Self {
        let mut overlay = Self::with_priority(
            marker_list,
            range,
            OverlayFace::Underline {
                color: Color::Yellow,
                style: UnderlineStyle::Wavy,
            },
            5, // Medium priority for warnings
        );
        overlay.message = message;
        overlay
    }

    /// Create an info underline overlay (wavy blue line)
    pub fn info(
        marker_list: &mut MarkerList,
        range: Range<usize>,
        message: Option<String>,
    ) -> Self {
        let mut overlay = Self::with_priority(
            marker_list,
            range,
            OverlayFace::Underline {
                color: Color::Blue,
                style: UnderlineStyle::Wavy,
            },
            3, // Lower priority for info
        );
        overlay.message = message;
        overlay
    }

    /// Create a hint underline overlay (dotted gray line)
    pub fn hint(
        marker_list: &mut MarkerList,
        range: Range<usize>,
        message: Option<String>,
    ) -> Self {
        let mut overlay = Self::with_priority(
            marker_list,
            range,
            OverlayFace::Underline {
                color: Color::Gray,
                style: UnderlineStyle::Dotted,
            },
            1, // Lowest priority for hints
        );
        overlay.message = message;
        overlay
    }

    /// Create a selection highlight overlay
    pub fn selection(marker_list: &mut MarkerList, range: Range<usize>) -> Self {
        Self::with_priority(
            marker_list,
            range,
            OverlayFace::Background {
                color: Color::Rgb(38, 79, 120), // VSCode-like selection color
            },
            -10, // Very low priority so it's under other overlays
        )
    }

    /// Create a search result highlight overlay
    pub fn search_match(marker_list: &mut MarkerList, range: Range<usize>) -> Self {
        Self::with_priority(
            marker_list,
            range,
            OverlayFace::Background {
                color: Color::Rgb(72, 72, 0), // Yellow-ish highlight
            },
            -5, // Low priority
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_overlay_creation_with_markers() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);

        let overlay = Overlay::new(
            &mut marker_list,
            5..10,
            OverlayFace::Background { color: Color::Red },
        );

        assert_eq!(marker_list.get_position(overlay.start_marker), Some(5));
        assert_eq!(marker_list.get_position(overlay.end_marker), Some(10));
        assert_eq!(overlay.range(&marker_list), 5..10);
    }

    #[test]
    fn test_overlay_adjusts_with_insert() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);

        let overlay = Overlay::new(
            &mut marker_list,
            10..20,
            OverlayFace::Background { color: Color::Red },
        );

        // Insert before overlay
        marker_list.adjust_for_insert(5, 10);

        // Overlay should have moved forward
        assert_eq!(overlay.range(&marker_list), 20..30);
    }

    #[test]
    fn test_overlay_adjusts_with_delete() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);

        let overlay = Overlay::new(
            &mut marker_list,
            20..30,
            OverlayFace::Background { color: Color::Red },
        );

        // Delete before overlay
        marker_list.adjust_for_delete(5, 10);

        // Overlay should have moved backward
        assert_eq!(overlay.range(&marker_list), 10..20);
    }

    #[test]
    fn test_overlay_manager_add_remove() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);
        let mut manager = OverlayManager::new();

        let overlay = Overlay::new(
            &mut marker_list,
            5..10,
            OverlayFace::Background { color: Color::Red },
        );

        let handle = manager.add(overlay);
        assert_eq!(manager.len(), 1);

        manager.remove_by_handle(&handle, &mut marker_list);
        assert_eq!(manager.len(), 0);
    }

    #[test]
    fn test_overlay_namespace_clear() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);
        let mut manager = OverlayManager::new();

        let ns = OverlayNamespace::from_string("todo".to_string());

        // Add overlays in namespace
        let overlay1 = Overlay::with_namespace(
            &mut marker_list,
            5..10,
            OverlayFace::Background { color: Color::Red },
            ns.clone(),
        );
        let overlay2 = Overlay::with_namespace(
            &mut marker_list,
            15..20,
            OverlayFace::Background { color: Color::Blue },
            ns.clone(),
        );
        // Add overlay without namespace
        let overlay3 = Overlay::new(
            &mut marker_list,
            25..30,
            OverlayFace::Background {
                color: Color::Green,
            },
        );

        manager.add(overlay1);
        manager.add(overlay2);
        manager.add(overlay3);
        assert_eq!(manager.len(), 3);

        // Clear only the namespace
        manager.clear_namespace(&ns, &mut marker_list);
        assert_eq!(manager.len(), 1); // Only overlay3 remains
    }

    #[test]
    fn test_overlay_priority_sorting() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);
        let mut manager = OverlayManager::new();

        manager.add(Overlay::with_priority(
            &mut marker_list,
            5..10,
            OverlayFace::Background { color: Color::Red },
            10,
        ));
        manager.add(Overlay::with_priority(
            &mut marker_list,
            5..10,
            OverlayFace::Background { color: Color::Blue },
            5,
        ));
        manager.add(Overlay::with_priority(
            &mut marker_list,
            5..10,
            OverlayFace::Background {
                color: Color::Green,
            },
            15,
        ));

        let overlays = manager.at_position(7, &marker_list);
        assert_eq!(overlays.len(), 3);
        // Should be sorted by priority (low to high)
        assert_eq!(overlays[0].priority, 5);
        assert_eq!(overlays[1].priority, 10);
        assert_eq!(overlays[2].priority, 15);
    }

    #[test]
    fn test_overlay_contains_and_overlaps() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);

        let overlay = Overlay::new(
            &mut marker_list,
            10..20,
            OverlayFace::Background { color: Color::Red },
        );

        assert!(!overlay.contains(9, &marker_list));
        assert!(overlay.contains(10, &marker_list));
        assert!(overlay.contains(15, &marker_list));
        assert!(overlay.contains(19, &marker_list));
        assert!(!overlay.contains(20, &marker_list));

        assert!(!overlay.overlaps(&(0..10), &marker_list));
        assert!(overlay.overlaps(&(5..15), &marker_list));
        assert!(overlay.overlaps(&(15..25), &marker_list));
        assert!(!overlay.overlaps(&(20..30), &marker_list));
    }
}
