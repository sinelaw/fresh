//! Layout utilities for hit testing
//!
//! This module provides shared utilities for UI layout and hit testing,
//! extracted from settings/layout.rs to be reusable across components.

use ratatui::layout::Rect;

/// Check if a point is within a rectangle
///
/// This is the fundamental hit testing primitive used by all UI components.
#[inline]
pub fn point_in_rect(rect: Rect, x: u16, y: u16) -> bool {
    x >= rect.x && x < rect.x + rect.width && y >= rect.y && y < rect.y + rect.height
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_point_in_rect_inside() {
        let rect = Rect::new(10, 20, 30, 40);
        assert!(point_in_rect(rect, 10, 20)); // top-left corner
        assert!(point_in_rect(rect, 25, 40)); // middle
        assert!(point_in_rect(rect, 39, 59)); // just inside bottom-right
    }

    #[test]
    fn test_point_in_rect_outside() {
        let rect = Rect::new(10, 20, 30, 40);
        assert!(!point_in_rect(rect, 9, 20)); // left of rect
        assert!(!point_in_rect(rect, 10, 19)); // above rect
        assert!(!point_in_rect(rect, 40, 20)); // right edge (exclusive)
        assert!(!point_in_rect(rect, 10, 60)); // bottom edge (exclusive)
    }

    #[test]
    fn test_point_in_rect_zero_size() {
        let rect = Rect::new(10, 20, 0, 0);
        assert!(!point_in_rect(rect, 10, 20));
    }
}
