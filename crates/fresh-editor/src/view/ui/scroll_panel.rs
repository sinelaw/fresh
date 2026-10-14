//! Scroll bookkeeping for a list the tree does not own.
//!
//! **One consumer, one struct.** This was a panel: `ScrollablePanel::render`
//! walked a list of variable-height items, called back per item and drew the
//! scrollbar itself, and `ScrollItem` was how an item stated its height so the
//! panel could bound the scroll and find the row a focused item sat on.
//!
//! Everything a retained tree can answer for itself has gone. The settings
//! tree and its body describe their rows and the fold paints them, so the
//! painter went first (`render`, `ScrollablePanelLayout`, `ItemLayoutInfo`,
//! `RenderInfo`); then the window became the list element's own, and with it
//! the offset — which left `ScrollablePanel::ensure_focused_visible` walking
//! `ScrollItem::height` over every row to compute a number nothing read, a
//! second copy of the heights the tree had already measured. `ScrollablePanel`,
//! `ScrollItem` and `FocusRegion` went with it.
//!
//! [`ScrollState`] is what remains: three `u16`s and the arithmetic over them,
//! for the keybinding editor's table, whose rows are not the tree's yet.

/// Pure scroll state - knows nothing about content
#[derive(Debug, Clone, Copy, Default)]
pub struct ScrollState {
    /// Scroll offset in rows (not items)
    pub offset: u16,
    /// Viewport height
    pub viewport: u16,
    /// Total content height
    pub content_height: u16,
}

impl ScrollState {
    /// Create new scroll state
    pub fn new(viewport: u16) -> Self {
        Self {
            offset: 0,
            viewport,
            content_height: 0,
        }
    }

    /// Update viewport height
    pub fn set_viewport(&mut self, height: u16) {
        self.viewport = height;
        self.clamp_offset();
    }

    /// Update content height (call when items change)
    #[cfg(test)]
    pub fn set_content_height(&mut self, height: u16) {
        self.content_height = height;
        self.clamp_offset();
    }

    /// Maximum scroll offset
    pub fn max_offset(&self) -> u16 {
        self.content_height.saturating_sub(self.viewport)
    }

    /// Clamp offset to valid range
    fn clamp_offset(&mut self) {
        self.offset = self.offset.min(self.max_offset());
    }

    /// Scroll to ensure a region is visible
    /// If region is taller than viewport, shows the top
    pub fn ensure_visible(&mut self, y: u16, height: u16) {
        if y < self.offset {
            // Region is above viewport - scroll up
            self.offset = y;
        } else if y + height > self.offset + self.viewport {
            // Region is below viewport - scroll down
            if height > self.viewport {
                // Oversized item - show top
                self.offset = y;
            } else {
                self.offset = y + height - self.viewport;
            }
        }
        self.clamp_offset();
    }

    /// Scroll by delta rows (positive = down, negative = up)
    pub fn scroll_by(&mut self, delta: i16) {
        if delta < 0 {
            self.offset = self.offset.saturating_sub((-delta) as u16);
        } else {
            self.offset = self.offset.saturating_add(delta as u16);
        }
        self.clamp_offset();
    }

    /// Check if scrolling is needed
    pub fn needs_scrollbar(&self) -> bool {
        self.content_height > self.viewport
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scroll_state_basic() {
        let mut state = ScrollState::new(10);
        state.set_content_height(100);

        assert_eq!(state.viewport, 10);
        assert_eq!(state.content_height, 100);
        assert_eq!(state.max_offset(), 90);
        assert!(state.needs_scrollbar());
    }

    #[test]
    fn test_scroll_state_no_scrollbar_needed() {
        let mut state = ScrollState::new(100);
        state.set_content_height(50);

        assert!(!state.needs_scrollbar());
        assert_eq!(state.max_offset(), 0);
    }

    #[test]
    fn test_scroll_by() {
        let mut state = ScrollState::new(10);
        state.set_content_height(100);

        state.scroll_by(5);
        assert_eq!(state.offset, 5);

        state.scroll_by(-3);
        assert_eq!(state.offset, 2);

        // Can't scroll past 0
        state.scroll_by(-10);
        assert_eq!(state.offset, 0);

        // Can't scroll past max
        state.scroll_by(200);
        assert_eq!(state.offset, 90);
    }

    #[test]
    fn test_ensure_visible_above_viewport() {
        let mut state = ScrollState::new(10);
        state.set_content_height(100);
        state.offset = 50;

        // Ensure item at y=20 (above viewport) is visible
        state.ensure_visible(20, 5);
        assert_eq!(state.offset, 20);
    }

    #[test]
    fn test_ensure_visible_below_viewport() {
        let mut state = ScrollState::new(10);
        state.set_content_height(100);
        state.offset = 0;

        // Ensure item at y=50 is visible (need to scroll down)
        state.ensure_visible(50, 5);
        assert_eq!(state.offset, 45); // 50 + 5 - 10 = 45
    }

    #[test]
    fn test_ensure_visible_oversized_item() {
        let mut state = ScrollState::new(10);
        state.set_content_height(100);
        state.offset = 0;

        // Ensure item at y=50 with height 20 (larger than viewport)
        state.ensure_visible(50, 20);
        assert_eq!(state.offset, 50); // Show top of item
    }

    #[test]
    fn test_ensure_visible_already_visible() {
        let mut state = ScrollState::new(10);
        state.set_content_height(100);
        state.offset = 20;

        // Item at y=22 is already visible
        state.ensure_visible(22, 3);
        assert_eq!(state.offset, 20); // No change
    }
}
