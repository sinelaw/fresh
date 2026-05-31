//! Reusable scrollable panel for lists with variable-height items
//!
//! This module provides a `ScrollablePanel` that handles:
//! - Row-based scrolling (not item-based) for variable-height items
//! - Automatic ensure-visible for focused items
//! - Sub-focus support for navigating within large items (e.g., TextList rows)
//! - Scrollbar rendering with proper thumb sizing
//!
//! Inspired by patterns from Flutter (Sliver), WPF (ScrollViewer), Qt (QAbstractScrollArea).
//!
//! # Usage Flow
//!
//! 1. **Define items** - Implement `ScrollItem` for your item type. Sizing is
//!    width-aware: callers pass the column count available to the item, which
//!    lets each item compute its own height (e.g. text wrapping):
//!    ```ignore
//!    impl ScrollItem for MyItem {
//!        fn height(&self, width: u16) -> u16 { ... }
//!        fn focus_regions(&self, width: u16) -> Vec<FocusRegion> { ... } // optional
//!    }
//!    ```
//!
//! 2. **Store state** - Keep a `ScrollablePanel` in your component state
//!
//! 3. **On selection change** - Call `ensure_focused_visible()` to scroll the
//!    focused item into view:
//!    ```ignore
//!    panel.ensure_focused_visible(&items, selected_index, sub_focus, width);
//!    ```
//!
//! 4. **On render** - Update viewport, then call `render()` with a callback:
//!    ```ignore
//!    panel.set_viewport(available_height);
//!    panel.update_content_height(&items, available_width);
//!    let layout = panel.render(frame, area, &items, |f, rect, item, idx| {
//!        render_my_item(f, rect, item, idx)
//!    }, theme);
//!    ```
//!
//! 5. **Use layout** - The returned `ScrollablePanelLayout` contains:
//!    - `content_area` - Area used for content (excluding scrollbar)
//!    - `scrollbar_area` - Scrollbar rect if visible (for drag hit testing)
//!    - `item_layouts` - Per-item layout info from your render callback
//!
//! # Sub-focus
//!
//! For items with internal navigation (e.g., a list of strings), implement
//! `focus_regions()` to return focusable sub-areas. Then pass the sub-focus
//! ID to `ensure_focused_visible()` to scroll that specific region into view.

use ratatui::layout::Rect;
use ratatui::Frame;

use super::scrollbar::{render_scrollbar, ScrollbarColors, ScrollbarState};
use crate::view::theme::Theme;

/// A focusable region within an item
#[derive(Debug, Clone, Copy)]
pub struct FocusRegion {
    /// Identifier for this region (e.g., row index within a TextList)
    pub id: usize,
    /// Y offset within the parent item
    pub y_offset: u16,
    /// Height of this region
    pub height: u16,
}

/// Trait for items that can be displayed in a scrollable panel.
///
/// Width is bubbled down on every query so items can size themselves to the
/// available columns (e.g. wrap a multi-line description). There is no cached
/// width on the item: the panel always asks with the current `width`.
pub trait ScrollItem {
    /// Total height of this item in terminal rows when laid out at `width` columns.
    fn height(&self, width: u16) -> u16;

    /// Optional: sub-focus regions within this item.
    /// Used for items with internal navigation (e.g., TextList rows).
    /// Y offsets are absolute within the item (i.e. row 0 is the top of the
    /// area allocated to the item, including any chrome above the content).
    fn focus_regions(&self, _width: u16) -> Vec<FocusRegion> {
        Vec::new()
    }
}

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

    /// Scroll to a ratio (0.0 = top, 1.0 = bottom)
    pub fn scroll_to_ratio(&mut self, ratio: f32) {
        let ratio = ratio.clamp(0.0, 1.0);
        self.offset = (ratio * self.max_offset() as f32) as u16;
    }

    /// Check if scrolling is needed
    pub fn needs_scrollbar(&self) -> bool {
        self.content_height > self.viewport
    }

    /// Convert to ScrollbarState for rendering
    pub fn to_scrollbar_state(&self) -> ScrollbarState {
        ScrollbarState::new(
            self.content_height as usize,
            self.viewport as usize,
            self.offset as usize,
        )
    }
}

/// Layout info returned by ScrollablePanel::render
#[derive(Debug, Clone)]
pub struct ScrollablePanelLayout<L> {
    /// Content area (excluding scrollbar)
    pub content_area: Rect,
    /// Scrollbar area (if visible)
    pub scrollbar_area: Option<Rect>,
    /// Per-item layouts with their indices and Y positions
    pub item_layouts: Vec<ItemLayoutInfo<L>>,
}

/// Layout info for a single item
#[derive(Debug, Clone)]
pub struct ItemLayoutInfo<L> {
    /// Item index
    pub index: usize,
    /// Y position in content coordinates (before scroll)
    pub content_y: u16,
    /// Rendered area on screen
    pub area: Rect,
    /// Custom layout data from render callback
    pub layout: L,
}

/// Info passed to render callback for partial item rendering
#[derive(Debug, Clone, Copy)]
pub struct RenderInfo {
    /// Screen area to render into
    pub area: Rect,
    /// Number of rows to skip at top of item (for partial visibility)
    pub skip_top: u16,
    /// Item index
    pub index: usize,
}

/// Manages scrolling for a list of items
#[derive(Debug, Clone, Default)]
pub struct ScrollablePanel {
    /// Scroll state
    pub scroll: ScrollState,
}

impl ScrollablePanel {
    /// Create new scrollable panel
    pub fn new() -> Self {
        Self {
            scroll: ScrollState::default(),
        }
    }

    /// Create with initial viewport height
    pub fn with_viewport(viewport: u16) -> Self {
        Self {
            scroll: ScrollState::new(viewport),
        }
    }

    /// Update scroll state for new viewport size
    pub fn set_viewport(&mut self, height: u16) {
        self.scroll.set_viewport(height);
    }

    /// Get current viewport height
    pub fn viewport_height(&self) -> usize {
        self.scroll.viewport as usize
    }

    /// Calculate total content height from items at the given area width.
    /// Handles the circular dependency between scrollbar presence and item height.
    pub fn update_content_height<I: ScrollItem>(&mut self, items: &[I], area_width: u16) {
        // First pass: assume no scrollbar
        let height1: u16 = items.iter().map(|i| i.height(area_width)).sum();
        self.scroll.set_content_height(height1);
        
        // If a scrollbar is needed, it reduces width, which might change height
        if self.scroll.needs_scrollbar() && area_width > 0 {
            let height2: u16 = items.iter().map(|i| i.height(area_width - 1)).sum();
            self.scroll.set_content_height(height2);
        }
    }

    /// Get the effective content width given an outer area width
    pub fn content_width(&self, area_width: u16) -> u16 {
        if self.scroll.needs_scrollbar() {
            area_width.saturating_sub(1)
        } else {
            area_width
        }
    }

    /// Get Y offset for an item by index at the given effective content width.
    pub fn item_y_offset<I: ScrollItem>(&self, items: &[I], index: usize, content_width: u16) -> u16 {
        items[..index].iter().map(|i| i.height(content_width)).sum()
    }

    /// Ensure focused item (and optional sub-region) is visible at the given outer width.
    pub fn ensure_focused_visible<I: ScrollItem>(
        &mut self,
        items: &[I],
        focused_index: usize,
        sub_focus: Option<usize>,
        area_width: u16,
    ) {
        if focused_index >= items.len() {
            return;
        }

        // Must sync content height first to know if scrollbar is present
        self.update_content_height(items, area_width);
        
        let content_width = self.content_width(area_width);

        // Calculate Y offset of focused item
        let item_y = self.item_y_offset(items, focused_index, content_width);
        let item = &items[focused_index];
        let item_h = item.height(content_width);

        // If sub-focus specified, use that region
        let (focus_y, focus_h) = if let Some(sub_id) = sub_focus {
            let regions = item.focus_regions(content_width);
            if let Some(region) = regions.iter().find(|r| r.id == sub_id) {
                (item_y + region.y_offset, region.height)
            } else {
                (item_y, item_h)
            }
        } else {
            (item_y, item_h)
        };
        self.scroll.ensure_visible(focus_y, focus_h);
    }

    /// Render visible items and scrollbar
    ///
    /// # Arguments
    /// * `frame` - The ratatui frame
    /// * `area` - Total area for the panel (including scrollbar)
    /// * `items` - Slice of items to render
    /// * `render_item` - Callback to render each item, receives (frame, RenderInfo, item).
    ///   RenderInfo contains area, skip_top (rows to skip for partial visibility), and index.
    /// * `theme` - Theme for scrollbar colors
    ///
    /// # Returns
    /// Layout info for hit testing
    pub fn render<I, F, L>(
        &self,
        frame: &mut Frame,
        area: Rect,
        items: &[I],
        render_item: F,
        theme: &Theme,
    ) -> ScrollablePanelLayout<L>
    where
        I: ScrollItem,
        F: Fn(&mut Frame, RenderInfo, &I) -> L,
    {
        let scrollbar_width = if self.scroll.needs_scrollbar() { 1 } else { 0 };
        let content_area = Rect::new(
            area.x,
            area.y,
            area.width.saturating_sub(scrollbar_width),
            area.height,
        );
        let item_width = content_area.width;

        let mut layouts = Vec::new();
        let mut content_y = 0u16; // Y in content coordinates
        let mut render_y = area.y; // Y on screen

        for (idx, item) in items.iter().enumerate() {
            let item_h = item.height(item_width);

            // Skip items entirely before scroll offset
            if content_y + item_h <= self.scroll.offset {
                content_y += item_h;
                continue;
            }

            // Stop if we're past the viewport
            if render_y >= area.y + area.height {
                break;
            }

            // Calculate visible portion of item
            let skip_top = self.scroll.offset.saturating_sub(content_y);
            let available_h = (area.y + area.height).saturating_sub(render_y);
            let visible_h = (item_h - skip_top).min(available_h);

            if visible_h > 0 {
                let item_area = Rect::new(content_area.x, render_y, content_area.width, visible_h);
                let info = RenderInfo {
                    area: item_area,
                    skip_top,
                    index: idx,
                };
                let layout = render_item(frame, info, item);
                layouts.push(ItemLayoutInfo {
                    index: idx,
                    content_y,
                    area: item_area,
                    layout,
                });
            }

            render_y += visible_h;
            content_y += item_h;
        }

        // Render scrollbar if needed
        let scrollbar_area = if self.scroll.needs_scrollbar() {
            let sb_area = Rect::new(area.x + content_area.width, area.y, 1, area.height);
            let scrollbar_state = self.scroll.to_scrollbar_state();
            let scrollbar_colors = ScrollbarColors::from_theme(theme);
            render_scrollbar(frame, sb_area, &scrollbar_state, &scrollbar_colors);
            Some(sb_area)
        } else {
            None
        };

        ScrollablePanelLayout {
            content_area,
            scrollbar_area,
            item_layouts: layouts,
        }
    }

    /// Render without scrollbar (for when scrollbar is managed externally)
    pub fn render_content_only<I, F, L>(
        &self,
        frame: &mut Frame,
        area: Rect,
        items: &[I],
        render_item: F,
    ) -> Vec<ItemLayoutInfo<L>>
    where
        I: ScrollItem,
        F: Fn(&mut Frame, RenderInfo, &I) -> L,
    {
        let mut layouts = Vec::new();
        let mut content_y = 0u16;
        let mut render_y = area.y;
        let item_width = area.width;

        for (idx, item) in items.iter().enumerate() {
            let item_h = item.height(item_width);

            if content_y + item_h <= self.scroll.offset {
                content_y += item_h;
                continue;
            }

            if render_y >= area.y + area.height {
                break;
            }

            let skip_top = self.scroll.offset.saturating_sub(content_y);
            let available_h = (area.y + area.height).saturating_sub(render_y);
            let visible_h = (item_h - skip_top).min(available_h);

            if visible_h > 0 {
                let item_area = Rect::new(area.x, render_y, area.width, visible_h);
                let info = RenderInfo {
                    area: item_area,
                    skip_top,
                    index: idx,
                };
                let layout = render_item(frame, info, item);
                layouts.push(ItemLayoutInfo {
                    index: idx,
                    content_y,
                    area: item_area,
                    layout,
                });
            }

            render_y += visible_h;
            content_y += item_h;
        }

        layouts
    }

    // Scroll operations
    pub fn scroll_up(&mut self, rows: u16) {
        self.scroll.scroll_by(-(rows as i16));
    }

    pub fn scroll_down(&mut self, rows: u16) {
        self.scroll.scroll_by(rows as i16);
    }

    pub fn scroll_to_ratio(&mut self, ratio: f32) {
        self.scroll.scroll_to_ratio(ratio);
    }

    /// Get current scroll offset
    pub fn offset(&self) -> u16 {
        self.scroll.offset
    }

    /// Check if scrollbar is needed
    pub fn needs_scrollbar(&self) -> bool {
        self.scroll.needs_scrollbar()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestItem {
        height: u16,
    }

    impl ScrollItem for TestItem {
        fn height(&self, _width: u16) -> u16 {
            self.height
        }
    }

    impl ScrollItem for fn(u16) -> u16 {
        fn height(&self, width: u16) -> u16 {
            self(width)
        }
    }

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

    #[test]
    fn test_scroll_to_ratio() {
        let mut state = ScrollState::new(10);
        state.set_content_height(100);

        state.scroll_to_ratio(0.0);
        assert_eq!(state.offset, 0);

        state.scroll_to_ratio(1.0);
        assert_eq!(state.offset, 90);

        state.scroll_to_ratio(0.5);
        assert_eq!(state.offset, 45);
    }

    /// Test items are width-agnostic, so the width arg is ignored — pass any value.
    const TEST_WIDTH: u16 = 80;

    #[test]
    fn test_panel_update_content_height() {
        let mut panel = ScrollablePanel::new();
        let items = vec![
            TestItem { height: 3 },
            TestItem { height: 5 },
            TestItem { height: 2 },
        ];

        panel.update_content_height(&items, TEST_WIDTH);
        assert_eq!(panel.scroll.content_height, 10);
    }

    #[test]
    fn test_panel_item_y_offset() {
        let panel = ScrollablePanel::new();
        let items = vec![
            TestItem { height: 3 },
            TestItem { height: 5 },
            TestItem { height: 2 },
        ];

        assert_eq!(panel.item_y_offset(&items, 0, TEST_WIDTH), 0);
        assert_eq!(panel.item_y_offset(&items, 1, TEST_WIDTH), 3);
        assert_eq!(panel.item_y_offset(&items, 2, TEST_WIDTH), 8);
    }

    #[test]
    fn test_panel_ensure_focused_visible() {
        let mut panel = ScrollablePanel::with_viewport(5);
        let items = vec![
            TestItem { height: 3 },
            TestItem { height: 3 },
            TestItem { height: 3 },
            TestItem { height: 3 },
        ];
        panel.update_content_height(&items, TEST_WIDTH);

        // Focus on item 2 (y=6, h=3) - needs scroll
        panel.ensure_focused_visible(&items, 2, None, TEST_WIDTH);
        // Item 2 ends at y=9, viewport=5, so offset should be 9-5=4
        assert_eq!(panel.scroll.offset, 4);
    }

    struct TestItemWithRegions {
        height: u16,
        regions: Vec<FocusRegion>,
    }

    impl ScrollItem for TestItemWithRegions {
        fn height(&self, _width: u16) -> u16 {
            self.height
        }

        fn focus_regions(&self, _width: u16) -> Vec<FocusRegion> {
            self.regions.clone()
        }
    }

    #[test]
    fn test_panel_ensure_focused_visible_with_subfocus() {
        let mut panel = ScrollablePanel::with_viewport(5);
        let items = vec![TestItemWithRegions {
            height: 10,
            regions: vec![
                FocusRegion {
                    id: 0,
                    y_offset: 0,
                    height: 1,
                },
                FocusRegion {
                    id: 1,
                    y_offset: 3,
                    height: 1,
                },
                FocusRegion {
                    id: 2,
                    y_offset: 7,
                    height: 1,
                },
            ],
        }];
        panel.update_content_height(&items, TEST_WIDTH);

        // Focus on sub-region 2 (y_offset=7 within item, so absolute y=7)
        panel.ensure_focused_visible(&items, 0, Some(2), TEST_WIDTH);
        // Region at y=7, h=1, viewport=5, so offset should be 7+1-5=3
        assert_eq!(panel.scroll.offset, 3);
    }

    /// Regression test for the scrollbar-width mismatch bug.
    ///
    /// ## Background
    ///
    /// `ScrollablePanel::render()` reserves one column for the scrollbar when
    /// `needs_scrollbar()` is true, so items are rendered at `area_width - 1`.
    /// Before the fix, `ensure_focused_visible` computed Y-offsets using the
    /// *full* `area_width`, not the narrower render width. When items are
    /// taller at the narrow width (because description text wraps onto an
    /// extra line), the cumulative Y position of items deep in the list drifts
    /// below the offset that `ensure_focused_visible` calculated — leaving the
    /// target item off-screen after the jump.
    ///
    /// ## Scenario
    ///
    /// - `area_width = 10`, `viewport = 5`
    /// - 4 "wide" items: height 2 at w ≥ 10, height 3 at w < 10
    /// - 1 "target" item (index 4): fixed height 2 at any width
    ///
    /// Content height at w=10:  4×2 + 2 = 10  →  needs_scrollbar (10 > 5)
    /// Render width:            10 − 1 = 9
    /// Content height at w=9:   4×3 + 2 = 14
    ///
    /// Y-position of item 4 at render width 9: 3+3+3+3 = **12**
    ///
    /// | Path          | offset computed | item 4 visible?            |
    /// |---------------|-----------------|----------------------------|
    /// | Bug (w=10)    | 8+2−5 = 5       | 12..14 ∉ [5..10) → **NO** |
    /// | Fix (w=9)     | 12+2−5 = 9      | 12..14 ⊆ [9..14) → **YES**|
    #[test]
    fn test_ensure_focused_visible_uses_render_width_when_scrollbar_present() {
        let area_width = 10u16;
        let viewport = 5u16;

        // 4 items that grow by 1 row when the scrollbar steals a column,
        // plus the target item at index 4 (fixed height).
        let mut items: Vec<fn(u16) -> u16> = vec![|w| if w >= 10 { 2 } else { 3 }; 4];
        items.push(|_| 2);

        let mut panel = ScrollablePanel::new();
        panel.set_viewport(viewport);
        panel.update_content_height(&items, area_width);

        // The scrollbar must be active for the bug to manifest.
        assert!(
            panel.needs_scrollbar(),
            "content height ({}) should exceed viewport ({}) to trigger the scrollbar",
            panel.scroll.content_height,
            viewport
        );

        // Ask the panel to make item 4 visible.
        panel.ensure_focused_visible(&items, 4, None, area_width);

        // The render engine will use (area_width - 1) = 9 for item heights.
        let render_width = area_width - 1;
        let item4_y: u16 = items[..4].iter().map(|i| i.height(render_width)).sum();
        let item4_h = items[4].height(render_width);
        let offset = panel.offset();

        // Item 4 must lie fully within the rendered viewport.
        assert!(
            offset <= item4_y && item4_y + item4_h <= offset + viewport,
            "Item 4 at render-y={item4_y}..{} must be visible in viewport \
             [{offset}..{}), but offset was computed as {offset}",
            item4_y + item4_h,
            offset + viewport,
        );
    }
}
