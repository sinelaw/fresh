//! Focus management utilities
//!
//! This module provides a generic focus manager for cycling through
//! focusable elements, extracted from the settings panel focus logic.

/// Manages focus cycling through a list of elements
///
/// This is a generic version of the focus panel cycling pattern used in
/// settings. It handles wrapping at boundaries and provides methods for
/// navigating forward, backward, and jumping to specific elements.
///
/// # Example
///
/// ```
/// use fresh_editor::view::ui::FocusManager;
///
/// #[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// enum Panel { Left, Center, Right }
///
/// let mut focus = FocusManager::new(vec![Panel::Left, Panel::Center, Panel::Right]);
///
/// assert_eq!(focus.current(), Some(Panel::Left));
/// focus.focus_next();
/// assert_eq!(focus.current(), Some(Panel::Center));
/// focus.focus_next();
/// assert_eq!(focus.current(), Some(Panel::Right));
/// focus.focus_next(); // wraps around
/// assert_eq!(focus.current(), Some(Panel::Left));
/// ```
#[derive(Debug, Clone)]
pub struct FocusManager<T> {
    elements: Vec<T>,
    current: usize,
}

impl<T> Default for FocusManager<T> {
    fn default() -> Self {
        Self {
            elements: Vec::new(),
            current: 0,
        }
    }
}

impl<T: Copy + Eq> FocusManager<T> {
    /// Create a new focus manager with the given elements
    ///
    /// Focus starts at the first element (index 0).
    pub fn new(elements: Vec<T>) -> Self {
        Self {
            elements,
            current: 0,
        }
    }

    /// Get the currently focused element
    pub fn current(&self) -> Option<T> {
        self.elements.get(self.current).copied()
    }

    /// Get the current index
    pub fn current_index(&self) -> usize {
        self.current
    }

    /// Move focus to the next element, wrapping at the end
    ///
    /// Returns the newly focused element.
    pub fn focus_next(&mut self) -> Option<T> {
        if self.elements.is_empty() {
            return None;
        }
        self.current = (self.current + 1) % self.elements.len();
        self.current()
    }

    /// Move focus to the previous element, wrapping at the beginning
    ///
    /// Returns the newly focused element.
    pub fn focus_prev(&mut self) -> Option<T> {
        if self.elements.is_empty() {
            return None;
        }
        self.current = (self.current + self.elements.len() - 1) % self.elements.len();
        self.current()
    }

    /// Set focus to a specific element
    ///
    /// Returns true if the element was found and focused, false otherwise.
    pub fn set(&mut self, element: T) -> bool {
        if let Some(idx) = self.elements.iter().position(|&e| e == element) {
            self.current = idx;
            true
        } else {
            false
        }
    }

    /// Set focus by index
    ///
    /// Returns true if the index was valid, false otherwise.
    pub fn set_index(&mut self, index: usize) -> bool {
        if index < self.elements.len() {
            self.current = index;
            true
        } else {
            false
        }
    }

    /// Check if the given element is currently focused
    pub fn is_current(&self, element: T) -> bool {
        self.current() == Some(element)
    }

    /// Get the number of elements
    pub fn len(&self) -> usize {
        self.elements.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.elements.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum TestPanel {
        A,
        B,
        C,
    }

    #[test]
    fn test_new_starts_at_first() {
        let focus = FocusManager::new(vec![TestPanel::A, TestPanel::B, TestPanel::C]);
        assert_eq!(focus.current(), Some(TestPanel::A));
        assert_eq!(focus.current_index(), 0);
    }

    #[test]
    fn test_focus_next_cycles() {
        let mut focus = FocusManager::new(vec![TestPanel::A, TestPanel::B, TestPanel::C]);

        assert_eq!(focus.focus_next(), Some(TestPanel::B));
        assert_eq!(focus.focus_next(), Some(TestPanel::C));
        assert_eq!(focus.focus_next(), Some(TestPanel::A)); // wrap
    }

    #[test]
    fn test_focus_prev_cycles() {
        let mut focus = FocusManager::new(vec![TestPanel::A, TestPanel::B, TestPanel::C]);

        assert_eq!(focus.focus_prev(), Some(TestPanel::C)); // wrap
        assert_eq!(focus.focus_prev(), Some(TestPanel::B));
        assert_eq!(focus.focus_prev(), Some(TestPanel::A));
    }

    #[test]
    fn test_set_element() {
        let mut focus = FocusManager::new(vec![TestPanel::A, TestPanel::B, TestPanel::C]);

        assert!(focus.set(TestPanel::C));
        assert_eq!(focus.current(), Some(TestPanel::C));
        assert_eq!(focus.current_index(), 2);
    }

    #[test]
    fn test_set_index() {
        let mut focus = FocusManager::new(vec![TestPanel::A, TestPanel::B, TestPanel::C]);

        assert!(focus.set_index(1));
        assert_eq!(focus.current(), Some(TestPanel::B));

        assert!(!focus.set_index(10)); // out of bounds
        assert_eq!(focus.current(), Some(TestPanel::B)); // unchanged
    }

    #[test]
    fn test_is_current() {
        let focus = FocusManager::new(vec![TestPanel::A, TestPanel::B, TestPanel::C]);

        assert!(focus.is_current(TestPanel::A));
        assert!(!focus.is_current(TestPanel::B));
    }

    #[test]
    fn test_empty_manager() {
        let mut focus: FocusManager<TestPanel> = FocusManager::new(vec![]);

        assert_eq!(focus.current(), None);
        assert_eq!(focus.focus_next(), None);
        assert_eq!(focus.focus_prev(), None);
        assert!(focus.is_empty());
    }

    #[test]
    fn test_single_element() {
        let mut focus = FocusManager::new(vec![TestPanel::A]);

        assert_eq!(focus.current(), Some(TestPanel::A));
        assert_eq!(focus.focus_next(), Some(TestPanel::A)); // stays same
        assert_eq!(focus.focus_prev(), Some(TestPanel::A)); // stays same
    }
}
