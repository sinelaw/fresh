use ratatui::style::{Color, Modifier, Style};
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
/// This is similar to Emacs overlays - a general-purpose primitive for text decoration
#[derive(Debug, Clone)]
pub struct Overlay {
    /// Byte range in the buffer this overlay covers
    pub range: Range<usize>,

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
    /// Create a new overlay with default priority (0)
    pub fn new(range: Range<usize>, face: OverlayFace) -> Self {
        Self {
            range,
            face,
            priority: 0,
            id: None,
            message: None,
        }
    }

    /// Create an overlay with a specific priority
    pub fn with_priority(range: Range<usize>, face: OverlayFace, priority: Priority) -> Self {
        Self {
            range,
            face,
            priority,
            id: None,
            message: None,
        }
    }

    /// Create an overlay with an ID (for later reference)
    pub fn with_id(range: Range<usize>, face: OverlayFace, id: String) -> Self {
        Self {
            range,
            face,
            priority: 0,
            id: Some(id),
            message: None,
        }
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

    /// Check if this overlay contains a position
    pub fn contains(&self, position: usize) -> bool {
        self.range.contains(&position)
    }

    /// Check if this overlay overlaps with a range
    pub fn overlaps(&self, range: &Range<usize>) -> bool {
        self.range.start < range.end && range.start < self.range.end
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

    /// Remove all overlays with a specific ID
    pub fn remove_by_id(&mut self, id: &str) {
        self.overlays.retain(|o| o.id.as_deref() != Some(id));
    }

    /// Remove all overlays in a range
    pub fn remove_in_range(&mut self, range: &Range<usize>) {
        self.overlays.retain(|o| !o.overlaps(range));
    }

    /// Clear all overlays
    pub fn clear(&mut self) {
        self.overlays.clear();
    }

    /// Get all overlays at a specific position, sorted by priority
    pub fn at_position(&self, position: usize) -> Vec<&Overlay> {
        self.overlays
            .iter()
            .filter(|o| o.contains(position))
            .collect()
    }

    /// Get all overlays that overlap with a range, sorted by priority
    pub fn in_range(&self, range: &Range<usize>) -> Vec<&Overlay> {
        self.overlays
            .iter()
            .filter(|o| o.overlaps(range))
            .collect()
    }

    /// Get overlay by ID
    pub fn get_by_id(&self, id: &str) -> Option<&Overlay> {
        self.overlays.iter().find(|o| o.id.as_deref() == Some(id))
    }

    /// Get mutable overlay by ID
    pub fn get_by_id_mut(&mut self, id: &str) -> Option<&mut Overlay> {
        self.overlays.iter_mut().find(|o| o.id.as_deref() == Some(id))
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
    pub fn error(range: Range<usize>, message: Option<String>) -> Self {
        let mut overlay = Self::with_priority(
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
    pub fn warning(range: Range<usize>, message: Option<String>) -> Self {
        let mut overlay = Self::with_priority(
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
    pub fn info(range: Range<usize>, message: Option<String>) -> Self {
        let mut overlay = Self::with_priority(
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
    pub fn hint(range: Range<usize>, message: Option<String>) -> Self {
        let mut overlay = Self::with_priority(
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
    pub fn selection(range: Range<usize>) -> Self {
        Self::with_priority(
            range,
            OverlayFace::Background {
                color: Color::Rgb(38, 79, 120), // VSCode-like selection color
            },
            -10, // Very low priority so it's under other overlays
        )
    }

    /// Create a search result highlight overlay
    pub fn search_match(range: Range<usize>) -> Self {
        Self::with_priority(
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
    fn test_overlay_contains() {
        let overlay = Overlay::new(5..10, OverlayFace::Background { color: Color::Red });

        assert!(!overlay.contains(4));
        assert!(overlay.contains(5));
        assert!(overlay.contains(7));
        assert!(overlay.contains(9));
        assert!(!overlay.contains(10));
    }

    #[test]
    fn test_overlay_overlaps() {
        let overlay = Overlay::new(5..10, OverlayFace::Background { color: Color::Red });

        assert!(!overlay.overlaps(&(0..5)));
        assert!(overlay.overlaps(&(0..6)));
        assert!(overlay.overlaps(&(5..10)));
        assert!(overlay.overlaps(&(7..12)));
        assert!(overlay.overlaps(&(9..15)));
        assert!(!overlay.overlaps(&(10..15)));
    }

    #[test]
    fn test_overlay_manager_add_and_query() {
        let mut manager = OverlayManager::new();

        manager.add(Overlay::new(5..10, OverlayFace::Background { color: Color::Red }));
        manager.add(Overlay::new(15..20, OverlayFace::Background { color: Color::Blue }));

        assert_eq!(manager.len(), 2);

        let overlays = manager.at_position(7);
        assert_eq!(overlays.len(), 1);
        assert_eq!(overlays[0].range, 5..10);

        let overlays = manager.at_position(17);
        assert_eq!(overlays.len(), 1);
        assert_eq!(overlays[0].range, 15..20);

        let overlays = manager.at_position(12);
        assert_eq!(overlays.len(), 0);
    }

    #[test]
    fn test_overlay_manager_priority_sorting() {
        let mut manager = OverlayManager::new();

        manager.add(Overlay::with_priority(
            5..10,
            OverlayFace::Background { color: Color::Red },
            10,
        ));
        manager.add(Overlay::with_priority(
            5..10,
            OverlayFace::Background { color: Color::Blue },
            5,
        ));
        manager.add(Overlay::with_priority(
            5..10,
            OverlayFace::Background { color: Color::Green },
            15,
        ));

        let overlays = manager.at_position(7);
        assert_eq!(overlays.len(), 3);
        // Should be sorted by priority (low to high)
        assert_eq!(overlays[0].priority, 5);
        assert_eq!(overlays[1].priority, 10);
        assert_eq!(overlays[2].priority, 15);
    }

    #[test]
    fn test_overlay_manager_remove_by_id() {
        let mut manager = OverlayManager::new();

        manager.add(Overlay::with_id(
            5..10,
            OverlayFace::Background { color: Color::Red },
            "error-1".to_string(),
        ));
        manager.add(Overlay::with_id(
            15..20,
            OverlayFace::Background { color: Color::Blue },
            "error-2".to_string(),
        ));

        assert_eq!(manager.len(), 2);

        manager.remove_by_id("error-1");
        assert_eq!(manager.len(), 1);

        let overlays = manager.at_position(7);
        assert_eq!(overlays.len(), 0);

        let overlays = manager.at_position(17);
        assert_eq!(overlays.len(), 1);
    }

    #[test]
    fn test_overlay_manager_remove_in_range() {
        let mut manager = OverlayManager::new();

        manager.add(Overlay::new(5..10, OverlayFace::Background { color: Color::Red }));
        manager.add(Overlay::new(15..20, OverlayFace::Background { color: Color::Blue }));
        manager.add(Overlay::new(25..30, OverlayFace::Background { color: Color::Green }));

        assert_eq!(manager.len(), 3);

        manager.remove_in_range(&(0..12));
        assert_eq!(manager.len(), 2);

        let overlays = manager.at_position(7);
        assert_eq!(overlays.len(), 0);

        let overlays = manager.at_position(17);
        assert_eq!(overlays.len(), 1);
    }

    #[test]
    fn test_overlay_helpers() {
        let error = Overlay::error(5..10, Some("error message".to_string()));
        assert_eq!(error.priority, 10);
        assert_eq!(error.message, Some("error message".to_string()));

        let warning = Overlay::warning(5..10, Some("warning message".to_string()));
        assert_eq!(warning.priority, 5);

        let info = Overlay::info(5..10, None);
        assert_eq!(info.priority, 3);

        let selection = Overlay::selection(5..10);
        assert_eq!(selection.priority, -10);
    }
}
