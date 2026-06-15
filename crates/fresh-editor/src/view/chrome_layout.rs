//! Chrome layout engine (PoC): turn a [`ChromeSnapshot`] + window size into
//! pixel geometry for the **complete chrome set** — menu bar, per-pane tab
//! bars, split dividers, status bar, and popups — plus pane-relative
//! hit-testing and a back-channel event vocabulary.
//!
//! This is the second half of the chrome seam from
//! `docs/internal/NON_TERMINAL_UI_RESEARCH.md`. The first half
//! ([`super::chrome_snapshot`]) describes the chrome *semantically*; this half
//! lays it out in *pixels* the way a GUI frontend would, independently of any
//! GPU draw calls. Everything here is pure geometry + hit-testing, so it is
//! fully unit-tested without a display. The remaining step — issuing the
//! actual glyph/quad draws for these rects on the wgpu surface — is the only
//! part that needs visual verification (`fresh --gui`).
//!
//! Mapping to the design's Neovim-style externalization:
//!   - `menubar`  → native menu bar,
//!   - `panes[].tabbar` / tab rects → `ext_tabline` (native tabs, close hit-areas),
//!   - `dividers` → `ext_multigrid` window placement (draggable split handles),
//!   - `popups`   → `ext_popupmenu` / native modal dialogs,
//!   - `status`   → status line.
//!
//! Hit-testing returns a [`ChromeHit`]; [`ChromeHit::on_click`] maps it to a
//! [`ChromeEvent`] — the normalized back-channel a backend sends to the core
//! (the research's "backend owns pixel→logical, core owns focus" split). Split
//! geometry mirrors `view::split::split_rect` (Horizontal = top/bottom with a
//! horizontal divider; Vertical = left/right with a vertical divider).

use fresh_core::SplitDirection;

use super::chrome_snapshot::ChromeSnapshot;
use crate::view::split::SplitNode;

/// A pixel-space rectangle (origin top-left, y-down).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PxRect {
    pub x: f32,
    pub y: f32,
    pub w: f32,
    pub h: f32,
}

impl PxRect {
    pub fn new(x: f32, y: f32, w: f32, h: f32) -> Self {
        Self { x, y, w, h }
    }
    /// True if `(px, py)` falls inside this rect (half-open on the far edges).
    pub fn contains(&self, px: f32, py: f32) -> bool {
        px >= self.x && px < self.x + self.w && py >= self.y && py < self.y + self.h
    }
}

/// Font/cell metrics + chrome band thicknesses a GUI provides. Defaults are
/// reasonable for a typical monospace GUI; a real frontend measures its font.
#[derive(Debug, Clone, Copy)]
pub struct Metrics {
    pub cell_w: f32,
    pub cell_h: f32,
    pub menubar_h: f32,
    pub tabbar_h: f32,
    pub status_h: f32,
    pub divider_px: f32,
    /// Horizontal padding inside a menu item / tab, each side.
    pub item_pad_x: f32,
    /// Width of a tab's close hit-area (right edge of the tab).
    pub close_w: f32,
}

impl Metrics {
    /// Derive bands from a cell size (e.g. 8×17 px for JetBrains Mono 14pt).
    pub fn from_cell(cell_w: f32, cell_h: f32) -> Self {
        Self {
            cell_w,
            cell_h,
            menubar_h: (cell_h * 1.6).round(),
            tabbar_h: (cell_h * 1.6).round(),
            status_h: cell_h,
            divider_px: 4.0,
            item_pad_x: cell_w,
            close_w: cell_w * 2.0,
        }
    }
}

impl Default for Metrics {
    fn default() -> Self {
        Self::from_cell(8.0, 17.0)
    }
}

/// One top-level menu item in the menu bar.
#[derive(Debug, Clone, PartialEq)]
pub struct MenuItemRect {
    pub index: usize,
    pub label: String,
    pub rect: PxRect,
}

/// One tab within a pane's tab bar (with its close hit-area).
#[derive(Debug, Clone, PartialEq)]
pub struct TabRect {
    pub label: String,
    pub buffer_id: Option<usize>,
    pub active: bool,
    pub rect: PxRect,
    pub close: PxRect,
}

/// A leaf pane: its tab bar strip, its content area, and its tabs.
#[derive(Debug, Clone, PartialEq)]
pub struct PaneRect {
    pub index: usize,
    pub buffer_id: Option<usize>,
    pub active: bool,
    pub tabbar: PxRect,
    pub content: PxRect,
    pub tabs: Vec<TabRect>,
}

/// Orientation of a draggable split divider.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Orientation {
    /// A horizontal bar between a top and bottom pane (a `Horizontal` split).
    Horizontal,
    /// A vertical bar between a left and right pane (a `Vertical` split).
    Vertical,
}

/// A draggable split divider.
#[derive(Debug, Clone, PartialEq)]
pub struct DividerRect {
    pub index: usize,
    pub rect: PxRect,
    pub orientation: Orientation,
}

/// A popup/overlay placement (centered, palette-style — anchors are an
/// enrichment once the snapshot carries them).
#[derive(Debug, Clone, PartialEq)]
pub struct PopupRect {
    pub index: usize,
    pub kind: String,
    pub rect: PxRect,
}

/// The complete laid-out chrome for one frame.
#[derive(Debug, Clone, PartialEq)]
pub struct ChromeLayout {
    pub viewport: PxRect,
    pub menubar: Vec<MenuItemRect>,
    pub menubar_band: PxRect,
    pub panes: Vec<PaneRect>,
    pub dividers: Vec<DividerRect>,
    pub status: PxRect,
    pub popups: Vec<PopupRect>,
}

/// What a point hit. Returned by [`ChromeLayout::hit`].
#[derive(Debug, Clone, PartialEq)]
pub enum ChromeHit {
    Menu { index: usize },
    Tab { pane: usize, buffer_id: Option<usize> },
    TabClose { pane: usize, buffer_id: Option<usize> },
    Divider { index: usize, orientation: Orientation },
    Status,
    Popup { index: usize },
    PaneContent { pane: usize, buffer_id: Option<usize> },
}

/// The normalized back-channel event a backend sends to the core in response
/// to a chrome interaction (the "backend owns pixels, core owns focus" seam).
#[derive(Debug, Clone, PartialEq)]
pub enum ChromeEvent {
    OpenMenu { index: usize },
    SelectTab { buffer_id: usize },
    CloseTab { buffer_id: usize },
    FocusPane { buffer_id: usize },
    /// User pressed on a divider to begin a drag (resize handled by follow-up
    /// motion the backend reports as it drags).
    BeginDividerDrag { index: usize, orientation: Orientation },
    DismissPopup { index: usize },
}

impl ChromeHit {
    /// Map a click on this hit to the back-channel event the core acts on.
    /// `None` for hits that are not click-actionable on their own (e.g. plain
    /// status-bar background).
    pub fn on_click(&self) -> Option<ChromeEvent> {
        match *self {
            ChromeHit::Menu { index } => Some(ChromeEvent::OpenMenu { index }),
            ChromeHit::Tab { buffer_id: Some(b), .. } => Some(ChromeEvent::SelectTab { buffer_id: b }),
            ChromeHit::TabClose { buffer_id: Some(b), .. } => Some(ChromeEvent::CloseTab { buffer_id: b }),
            ChromeHit::Divider { index, orientation } => {
                Some(ChromeEvent::BeginDividerDrag { index, orientation })
            }
            ChromeHit::PaneContent { buffer_id: Some(b), .. } => {
                Some(ChromeEvent::FocusPane { buffer_id: b })
            }
            ChromeHit::Popup { index } => Some(ChromeEvent::DismissPopup { index }),
            _ => None,
        }
    }
}

impl ChromeLayout {
    /// Compute the full chrome layout for `snapshot` in a `width`×`height`
    /// pixel window, using `metrics`.
    pub fn compute(snapshot: &ChromeSnapshot, width: f32, height: f32, metrics: &Metrics) -> Self {
        let viewport = PxRect::new(0.0, 0.0, width, height);

        // 1. Menu bar across the top.
        let menubar_band = PxRect::new(0.0, 0.0, width, metrics.menubar_h);
        let mut menubar = Vec::with_capacity(snapshot.menubar.len());
        let mut mx = 0.0_f32;
        for (index, label) in snapshot.menubar.iter().enumerate() {
            let w = label.chars().count() as f32 * metrics.cell_w + metrics.item_pad_x * 2.0;
            menubar.push(MenuItemRect {
                index,
                label: label.clone(),
                rect: PxRect::new(mx, 0.0, w, metrics.menubar_h),
            });
            mx += w;
        }

        // 2. Status bar across the bottom.
        let status = PxRect::new(0.0, height - metrics.status_h, width, metrics.status_h);

        // 3. The middle content area hosts the split tree.
        let content = PxRect::new(
            0.0,
            menubar_band.h,
            width,
            (height - menubar_band.h - status.h).max(0.0),
        );

        let mut panes = Vec::new();
        let mut dividers = Vec::new();
        layout_node(
            &snapshot.split_layout,
            content,
            snapshot.active_buffer,
            metrics,
            &mut panes,
            &mut dividers,
        );

        // 4. Popups: centered, palette-style, sized from the viewport.
        let popups = snapshot
            .overlays
            .iter()
            .enumerate()
            .map(|(index, kind)| {
                let w = (width * 0.5).min(720.0);
                let h = (height * 0.5).min(420.0);
                PopupRect {
                    index,
                    kind: kind.clone(),
                    rect: PxRect::new((width - w) * 0.5, (height - h) * 0.35, w, h),
                }
            })
            .collect();

        ChromeLayout {
            viewport,
            menubar,
            menubar_band,
            panes,
            dividers,
            status,
            popups,
        }
    }

    /// Hit-test a pixel point against the chrome, top-most first: popups →
    /// menu bar → dividers → status → per-pane tabs/close → pane content.
    pub fn hit(&self, x: f32, y: f32) -> Option<ChromeHit> {
        // Popups are modal/topmost.
        for p in &self.popups {
            if p.rect.contains(x, y) {
                return Some(ChromeHit::Popup { index: p.index });
            }
        }
        for m in &self.menubar {
            if m.rect.contains(x, y) {
                return Some(ChromeHit::Menu { index: m.index });
            }
        }
        // Dividers sit above pane content so a drag grabs the handle, not text.
        for d in &self.dividers {
            if d.rect.contains(x, y) {
                return Some(ChromeHit::Divider {
                    index: d.index,
                    orientation: d.orientation,
                });
            }
        }
        if self.status.contains(x, y) {
            return Some(ChromeHit::Status);
        }
        for pane in &self.panes {
            for tab in &pane.tabs {
                if tab.close.contains(x, y) {
                    return Some(ChromeHit::TabClose {
                        pane: pane.index,
                        buffer_id: tab.buffer_id,
                    });
                }
                if tab.rect.contains(x, y) {
                    return Some(ChromeHit::Tab {
                        pane: pane.index,
                        buffer_id: tab.buffer_id,
                    });
                }
            }
            if pane.content.contains(x, y) {
                return Some(ChromeHit::PaneContent {
                    pane: pane.index,
                    buffer_id: pane.buffer_id,
                });
            }
        }
        None
    }
}

/// Recurse the split tree into pane + divider rects, mirroring
/// `view::split::split_rect` (1 separator between children; ratio gives the
/// first child's share).
fn layout_node(
    node: &SplitNode,
    rect: PxRect,
    active_buffer: usize,
    metrics: &Metrics,
    panes: &mut Vec<PaneRect>,
    dividers: &mut Vec<DividerRect>,
) {
    match node {
        SplitNode::Leaf { buffer_id, .. } => {
            push_pane(Some(buffer_id.0), rect, active_buffer, metrics, panes);
        }
        SplitNode::Grouped { layout, .. } => {
            // A grouped subtree expands into the parent's area when active.
            layout_node(layout, rect, active_buffer, metrics, panes, dividers);
        }
        SplitNode::Split {
            direction,
            first,
            second,
            ratio,
            ..
        } => {
            let d = metrics.divider_px;
            let idx = dividers.len();
            match direction {
                SplitDirection::Horizontal => {
                    // top / bottom, horizontal divider between them
                    let usable = (rect.h - d).max(0.0);
                    let first_h = (usable * *ratio).round();
                    let top = PxRect::new(rect.x, rect.y, rect.w, first_h);
                    let bar = PxRect::new(rect.x, rect.y + first_h, rect.w, d);
                    let bottom =
                        PxRect::new(rect.x, rect.y + first_h + d, rect.w, (usable - first_h).max(0.0));
                    dividers.push(DividerRect {
                        index: idx,
                        rect: bar,
                        orientation: Orientation::Horizontal,
                    });
                    layout_node(first, top, active_buffer, metrics, panes, dividers);
                    layout_node(second, bottom, active_buffer, metrics, panes, dividers);
                }
                SplitDirection::Vertical => {
                    // left / right, vertical divider between them
                    let usable = (rect.w - d).max(0.0);
                    let first_w = (usable * *ratio).round();
                    let left = PxRect::new(rect.x, rect.y, first_w, rect.h);
                    let bar = PxRect::new(rect.x + first_w, rect.y, d, rect.h);
                    let right =
                        PxRect::new(rect.x + first_w + d, rect.y, (usable - first_w).max(0.0), rect.h);
                    dividers.push(DividerRect {
                        index: idx,
                        rect: bar,
                        orientation: Orientation::Vertical,
                    });
                    layout_node(first, left, active_buffer, metrics, panes, dividers);
                    layout_node(second, right, active_buffer, metrics, panes, dividers);
                }
            }
        }
    }
}

/// Emit one pane: a tab-bar strip on top, content below, and (for now) one tab
/// for the pane's buffer. Multi-buffer-per-split tab lists are an enrichment.
fn push_pane(
    buffer_id: Option<usize>,
    rect: PxRect,
    active_buffer: usize,
    metrics: &Metrics,
    panes: &mut Vec<PaneRect>,
) {
    let index = panes.len();
    let tabbar = PxRect::new(rect.x, rect.y, rect.w, metrics.tabbar_h.min(rect.h));
    let content = PxRect::new(
        rect.x,
        rect.y + tabbar.h,
        rect.w,
        (rect.h - tabbar.h).max(0.0),
    );
    let active = buffer_id == Some(active_buffer);
    let label = match buffer_id {
        Some(b) => format!("buffer#{b}"),
        None => "(group)".to_string(),
    };
    let tab_w =
        (label.chars().count() as f32 * metrics.cell_w + metrics.item_pad_x * 2.0 + metrics.close_w)
            .min(tabbar.w);
    let tab_rect = PxRect::new(tabbar.x, tabbar.y, tab_w, tabbar.h);
    let close = PxRect::new(
        tab_rect.x + tab_rect.w - metrics.close_w,
        tab_rect.y,
        metrics.close_w,
        tab_rect.h,
    );
    panes.push(PaneRect {
        index,
        buffer_id,
        active,
        tabbar,
        content,
        tabs: vec![TabRect {
            label,
            buffer_id,
            active,
            rect: tab_rect,
            close,
        }],
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    fn snap(layout: SplitNode, dividers: usize, active: usize) -> ChromeSnapshot {
        ChromeSnapshot {
            menubar: vec!["File".into(), "Edit".into(), "View".into()],
            split_layout: layout,
            tabs: vec![],
            overlays: vec![],
            divider_count: dividers,
            active_buffer: active,
        }
    }

    fn leaf(buf: usize, id: usize) -> SplitNode {
        SplitNode::leaf(fresh_core::BufferId(buf), fresh_core::SplitId(id))
    }

    #[test]
    fn single_pane_fills_content_band() {
        let s = snap(leaf(1, 0), 0, 1);
        let m = Metrics::from_cell(8.0, 16.0);
        let l = ChromeLayout::compute(&s, 800.0, 600.0, &m);

        assert_eq!(l.menubar.len(), 3);
        assert_eq!(l.dividers.len(), 0);
        assert_eq!(l.panes.len(), 1);
        assert!(l.panes[0].active);
        // Bands stack with no gaps/overlap: menubar | panes | status.
        assert_eq!(l.menubar_band.h, m.menubar_h);
        let pane = &l.panes[0];
        assert!((pane.tabbar.y - m.menubar_h).abs() < 0.01, "tab bar sits under the menu bar");
        assert!(
            (pane.content.y + pane.content.h - l.status.y).abs() < 0.01,
            "content meets the status bar exactly"
        );
        assert!((l.status.y - (600.0 - m.status_h)).abs() < 0.01);
    }

    #[test]
    fn vertical_split_makes_two_panes_and_a_vertical_divider() {
        // left | right (Vertical split)
        let tree = SplitNode::split(
            SplitDirection::Vertical,
            leaf(1, 1),
            leaf(2, 2),
            0.5,
            fresh_core::SplitId(0),
        );
        let s = snap(tree, 1, 2);
        let m = Metrics::from_cell(8.0, 16.0);
        let l = ChromeLayout::compute(&s, 800.0, 600.0, &m);

        assert_eq!(l.panes.len(), 2);
        assert_eq!(l.dividers.len(), 1);
        assert_eq!(l.dividers[0].orientation, Orientation::Vertical);
        // Pane 2 holds the active buffer.
        assert!(!l.panes[0].active && l.panes[1].active);
        // Left pane is left of the divider; right pane is right of it; no overlap.
        let div = l.dividers[0].rect;
        assert!(l.panes[0].content.x + l.panes[0].content.w <= div.x + 0.01);
        assert!(l.panes[1].content.x >= div.x + div.w - 0.01);
    }

    #[test]
    fn horizontal_split_divider_is_horizontal() {
        let tree = SplitNode::split(
            SplitDirection::Horizontal,
            leaf(1, 1),
            leaf(2, 2),
            0.3,
            fresh_core::SplitId(0),
        );
        let s = snap(tree, 1, 1);
        let l = ChromeLayout::compute(&s, 800.0, 600.0, &Metrics::default());
        assert_eq!(l.dividers.len(), 1);
        assert_eq!(l.dividers[0].orientation, Orientation::Horizontal);
        // Top pane ends above the divider; bottom pane starts below it.
        let div = l.dividers[0].rect;
        assert!(l.panes[0].content.y + l.panes[0].content.h <= div.y + 0.01);
        assert!(l.panes[1].tabbar.y >= div.y + div.h - 0.01);
    }

    #[test]
    fn hit_testing_covers_every_chrome_element() {
        let tree = SplitNode::split(
            SplitDirection::Vertical,
            leaf(1, 1),
            leaf(2, 2),
            0.5,
            fresh_core::SplitId(0),
        );
        let mut s = snap(tree, 1, 1);
        s.overlays = vec!["Popup".into()];
        let m = Metrics::from_cell(8.0, 16.0);
        let l = ChromeLayout::compute(&s, 800.0, 600.0, &m);

        // Menu bar: click inside the first menu item.
        let file = &l.menubar[0].rect;
        assert_eq!(
            l.hit(file.x + 1.0, file.y + 1.0),
            Some(ChromeHit::Menu { index: 0 })
        );

        // Divider handle. Probe just below the menu bar, above the centered
        // popup (the popup is modal/topmost and would otherwise cover the
        // divider's midpoint — which is the correct precedence, exercised
        // separately below).
        let div = l.dividers[0].rect;
        match l.hit(div.x + div.w * 0.5, l.menubar_band.h + 5.0) {
            Some(ChromeHit::Divider { orientation: Orientation::Vertical, .. }) => {}
            other => panic!("expected vertical divider hit, got {other:?}"),
        }

        // Status bar — but it's covered by the centered popup only in the
        // middle, so hit the far-left of the status band.
        assert_eq!(l.hit(2.0, 599.0), Some(ChromeHit::Status));

        // Popup is modal/topmost over the center.
        match l.hit(400.0, 300.0) {
            Some(ChromeHit::Popup { index: 0 }) => {}
            other => panic!("expected popup hit at center, got {other:?}"),
        }

        // A tab and its close button in pane 0.
        let tab = l.panes[0].tabs[0].clone();
        match l.hit(tab.rect.x + 1.0, tab.rect.y + 1.0) {
            Some(ChromeHit::Tab { pane: 0, buffer_id: Some(1) }) => {}
            other => panic!("expected tab hit, got {other:?}"),
        }
        match l.hit(tab.close.x + 1.0, tab.close.y + 1.0) {
            Some(ChromeHit::TabClose { pane: 0, buffer_id: Some(1) }) => {}
            other => panic!("expected tab-close hit, got {other:?}"),
        }
    }

    #[test]
    fn hits_map_to_back_channel_events() {
        assert_eq!(
            ChromeHit::Menu { index: 2 }.on_click(),
            Some(ChromeEvent::OpenMenu { index: 2 })
        );
        assert_eq!(
            ChromeHit::Tab { pane: 0, buffer_id: Some(7) }.on_click(),
            Some(ChromeEvent::SelectTab { buffer_id: 7 })
        );
        assert_eq!(
            ChromeHit::TabClose { pane: 1, buffer_id: Some(3) }.on_click(),
            Some(ChromeEvent::CloseTab { buffer_id: 3 })
        );
        assert_eq!(
            ChromeHit::Divider { index: 0, orientation: Orientation::Vertical }.on_click(),
            Some(ChromeEvent::BeginDividerDrag { index: 0, orientation: Orientation::Vertical })
        );
        assert_eq!(ChromeHit::Status.on_click(), None);
    }
}
