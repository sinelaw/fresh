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

/// Trait for layout structs that support hit testing
///
/// Implement this trait for layout structs returned by render functions
/// to provide uniform hit testing across different UI components.
///
/// # Example
///
/// ```ignore
/// pub struct MyLayout {
///     pub button_area: Rect,
///     pub list_areas: Vec<Rect>,
/// }
///
/// pub enum MyHit {
///     Button,
///     ListItem(usize),
/// }
///
/// impl HitTest for MyLayout {
///     type Hit = MyHit;
///
///     fn hit_test(&self, x: u16, y: u16) -> Option<MyHit> {
///         if point_in_rect(self.button_area, x, y) {
///             return Some(MyHit::Button);
///         }
///         for (i, area) in self.list_areas.iter().enumerate() {
///             if point_in_rect(*area, x, y) {
///                 return Some(MyHit::ListItem(i));
///             }
///         }
///         None
///     }
///
///     fn contains(&self, x: u16, y: u16) -> bool {
///         // Check if point is in any tracked area
///         self.hit_test(x, y).is_some()
///     }
/// }
/// ```
pub trait HitTest {
    /// The hit result type (e.g., DropdownHit, MenuHit)
    type Hit;

    /// Test if point hits any element, returning hit info
    fn hit_test(&self, x: u16, y: u16) -> Option<Self::Hit>;

    /// Check if point is within component bounds
    fn contains(&self, x: u16, y: u16) -> bool;
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
