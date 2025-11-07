use crate::marker::{MarkerId, MarkerList};
use ratatui::style::{Color, Style};
use std::ops::Range;

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
    /// Start marker (left affinity - stays before inserted text)
    pub start_marker: MarkerId,

    /// End marker (right affinity - moves after inserted text)
    pub end_marker: MarkerId,

    /// Visual appearance of the overlay
    pub face: OverlayFace,

    /// Priority for z-ordering (higher = on top)
    pub priority: Priority,

    /// Optional identifier for this overlay (for removal/updates)
    pub id: Option<String>,

    /// Optional tooltip/message to show when hovering over this overlay
    pub message: Option<String>,
}

impl Overlay {
    /// Create a new overlay with markers at the given range
    ///
    /// # Arguments
    /// * `marker_list` - MarkerList to create markers in
    /// * `range` - Byte range for the overlay
    /// * `face` - Visual appearance
    pub fn new(marker_list: &mut MarkerList, range: Range<usize>, face: OverlayFace) -> Self {
        let start_marker = marker_list.create(range.start, true); // left affinity
        let end_marker = marker_list.create(range.end, false); // right affinity

        Self {
            start_marker,
            end_marker,
            face,
            priority: 0,
            id: None,
            message: None,
        }
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

    /// Create an overlay with an ID (for later reference)
    pub fn with_id(
        marker_list: &mut MarkerList,
        range: Range<usize>,
        face: OverlayFace,
        id: String,
    ) -> Self {
        let mut overlay = Self::new(marker_list, range, face);
        overlay.id = Some(id);
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
    /// All active overlays
    overlays: Vec<Overlay>,
}

impl OverlayManager {
    /// Create a new empty overlay manager
    pub fn new() -> Self {
        Self {
            overlays: Vec::new(),
        }
    }

    /// Add an overlay
    pub fn add(&mut self, overlay: Overlay) {
        self.overlays.push(overlay);
        // Keep sorted by priority (ascending - lower priority first)
        self.overlays.sort_by_key(|o| o.priority);
    }

    /// Remove all overlays with a specific ID and clean up their markers
    pub fn remove_by_id(&mut self, id: &str, marker_list: &mut MarkerList) {
        // Collect markers to delete
        let markers_to_delete: Vec<_> = self
            .overlays
            .iter()
            .filter(|o| o.id.as_deref() == Some(id))
            .flat_map(|o| vec![o.start_marker, o.end_marker])
            .collect();

        // Remove overlays
        self.overlays.retain(|o| o.id.as_deref() != Some(id));

        // Delete markers
        for marker_id in markers_to_delete {
            marker_list.delete(marker_id);
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
        self.overlays
            .retain(|o| !o.overlaps(range, marker_list));

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

    /// Get overlay by ID
    pub fn get_by_id(&self, id: &str) -> Option<&Overlay> {
        self.overlays.iter().find(|o| o.id.as_deref() == Some(id))
    }

    /// Get mutable overlay by ID
    pub fn get_by_id_mut(&mut self, id: &str) -> Option<&mut Overlay> {
        self.overlays
            .iter_mut()
            .find(|o| o.id.as_deref() == Some(id))
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

        let overlay = Overlay::new(&mut marker_list, 5..10, OverlayFace::Background {
            color: Color::Red,
        });

        assert_eq!(marker_list.get_position(overlay.start_marker), Some(5));
        assert_eq!(marker_list.get_position(overlay.end_marker), Some(10));
        assert_eq!(overlay.range(&marker_list), 5..10);
    }

    #[test]
    fn test_overlay_adjusts_with_insert() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);

        let overlay = Overlay::new(&mut marker_list, 10..20, OverlayFace::Background {
            color: Color::Red,
        });

        // Insert before overlay
        marker_list.adjust_for_insert(5, 10);

        // Overlay should have moved forward
        assert_eq!(overlay.range(&marker_list), 20..30);
    }

    #[test]
    fn test_overlay_adjusts_with_delete() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);

        let overlay = Overlay::new(&mut marker_list, 20..30, OverlayFace::Background {
            color: Color::Red,
        });

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

        let overlay = Overlay::with_id(
            &mut marker_list,
            5..10,
            OverlayFace::Background { color: Color::Red },
            "test-1".to_string(),
        );

        manager.add(overlay);
        assert_eq!(manager.len(), 1);

        manager.remove_by_id("test-1", &mut marker_list);
        assert_eq!(manager.len(), 0);
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

        let overlay = Overlay::new(&mut marker_list, 10..20, OverlayFace::Background {
            color: Color::Red,
        });

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
