//! Where the frame put each pane — read once, off the `Ui` that laid the
//! frame out.
//!
//! **One source of pane geometry per frame.** The render path used to lay
//! the pane grid out more than once and three different ways: `Editor::render`
//! laid out the shell tree, `SplitManager::get_leaves_with_rects` laid the
//! *same* grid out again in a scratch `Ui<()>` to answer where the panes are,
//! and the macro-replay path did a third — a `layout_only` of the shell, then
//! `compute_content_layout`, which ran the scratch grid inside it. The scratch
//! grid was a second implementation of the pane layout; where it disagreed
//! with the tree by a cell, the painter's clip papered over it.
//!
//! [`PaneRects`] is the one answer. It is read off the shell's `Ui` right
//! after the frame's layout — [`Ui::frame`](fresh_ui::Ui::frame) on the render
//! path, [`Ui::layout_only`](fresh_ui::Ui::layout_only) on the replay path —
//! and handed to everything that used to ask the scratch grid: the plugin
//! `lines_changed` hooks, the body painter, and the layout-only pass. The keys
//! it reads are the ones the description applies: [`leaf_key`] for the box a
//! pane fills and [`content_key`] for the content slot inside it, past the
//! strip and beside the scrollbar.
//!
//! **Never last frame's rect.** Every chrome toggle — the dock, the explorer,
//! the menu bar, a separator drag — changes pane widths between frames, and a
//! pane laid out at the old width is a visibly wrong frame, not a short one.
//! The rects here are this frame's, from this frame's layout.
//!
//! The session preview is the one caller with no tree to read: it paints
//! *another window's* grid offscreen, where this window's tree has no nodes.
//! [`PaneRects::offscreen`] lays that grid out once, from the same description
//! the frame uses (`splits::overlay`), and reads it the same way — one layout
//! of one description, rather than a scratch grid plus a per-pane interior.

use std::collections::HashMap;

use ratatui::layout::Rect;

use crate::model::event::{BufferId, LeafId};
use crate::view::split::SplitNode;

use super::msg::UiMsg;
use super::splits::{content_key, leaf_key, overlay, Splits};

/// The two rectangles a pane is: the box it fills, and its content slot.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PaneRect {
    /// The pane's box (`leaf_key`): what its `Host` is painted into, strip and
    /// scrollbars included.
    pub pane: Rect,
    /// The content slot (`content_key`): past the strip, beside the vertical
    /// bar, above the horizontal one. What the buffer is laid out in.
    pub content: Rect,
}

/// Where the frame put every pane it placed, by the pane.
///
/// Outer panes and the panels inside an active buffer group alike — a group's
/// leaves are panes of the same grid, mounted in their outer pane's content
/// slot, and the tree keys them the same way.
#[derive(Clone, Debug, Default)]
pub struct PaneRects {
    by_pane: HashMap<LeafId, PaneRect>,
}

impl PaneRects {
    /// Read off `ui` — the tree that laid this frame out — for each of `panes`
    /// the tree placed. `size` is the frame's area; the rectangles come back
    /// in screen coordinates.
    ///
    /// A pane with no node — one hidden behind a maximized sibling — is simply
    /// absent. A zero-size pane is *not*: the scratch grid answered a zero
    /// rectangle for it and the painter painted nothing in it, and this keeps
    /// that shape rather than dropping the pane from the frame's list.
    pub fn read(
        ui: &fresh_ui::Ui<UiMsg>,
        panes: impl IntoIterator<Item = LeafId>,
        size: Rect,
    ) -> Self {
        let at = |key: fresh_ui::Key| -> Option<Rect> {
            let e = ui.find_by_key(&key)?;
            Some(super::screen_rect(ui.rect_of(e), size))
        };
        let by_pane = panes
            .into_iter()
            .filter_map(|id| {
                let pane = at(leaf_key(id))?;
                // The interior always keys its content slot; a pane with a
                // box and no content node would be a description bug, and
                // a zero rect there is what the painter does with it.
                let content = at(content_key(id)).unwrap_or_default();
                Some((id, PaneRect { pane, content }))
            })
            .collect();
        Self { by_pane }
    }

    /// The same grid laid out offscreen at `area`, for a window the frame has
    /// no nodes for.
    ///
    /// The session preview paints another window's panes into a rectangle of
    /// this one's frame. That grid is described by the same `overlay` the
    /// frame mounts, laid out once here at the preview's box, so the preview's
    /// panes come off one layout of one description — the rule the render
    /// path follows, applied to the one grid the render path's tree cannot
    /// hold.
    pub fn offscreen(s: &Splits, area: Rect) -> Self {
        stats::note_offscreen_grid();
        let mut ui: fresh_ui::Ui<UiMsg> = fresh_ui::Ui::new();
        ui.frame(overlay(s), fresh_ui::Size::new(area.width, area.height));
        Self::read(&ui, panes_of(s), area)
    }

    /// The box `pane` fills, if the tree placed it.
    pub fn pane(&self, pane: LeafId) -> Option<Rect> {
        self.by_pane.get(&pane).map(|r| r.pane)
    }

    /// `pane`'s content slot, if the tree placed it.
    pub fn content(&self, pane: LeafId) -> Option<Rect> {
        self.by_pane.get(&pane).map(|r| r.content)
    }

    /// `leaves`, each with the box it fills — the shape
    /// `SplitManager::get_visible_buffers` answered in, so the callers that
    /// took that list take this one.
    ///
    /// A leaf the tree did not place gets a zero rectangle, as the scratch
    /// grid gave one it could not find; the set of leaves is the caller's,
    /// and `SplitManager::visible_leaves` already leaves out what a maximized
    /// pane hides.
    pub fn visible(&self, leaves: &[(LeafId, BufferId)]) -> Vec<(LeafId, BufferId, Rect)> {
        leaves
            .iter()
            .map(|(leaf, buffer)| (*leaf, *buffer, self.pane(*leaf).unwrap_or_default()))
            .collect()
    }
}

/// Every pane `s` describes: the grid's leaves — only the maximized one, when
/// one is — and the panels of each pane's active group.
///
/// The same rule as `SplitManager::visible_leaves` plus `Window::pane_groups`,
/// stated over the description because that is what the offscreen layout has.
fn panes_of(s: &Splits) -> Vec<LeafId> {
    let mut out: Vec<LeafId> = match s.maximized.and_then(|id| s.root.find(id.0)) {
        Some(SplitNode::Leaf { split_id, .. }) => vec![*split_id],
        _ => s
            .root
            .visible_leaves()
            .into_iter()
            .map(|(l, _)| l)
            .collect(),
    };
    for g in s.groups.values() {
        out.extend(g.visible_leaves().into_iter().map(|(l, _)| l));
    }
    out
}

/// How many times the pane grid was laid out, per frame.
///
/// The instrument Stage 2 of the retained-mode plan asks for: a count of
/// shell layout passes and of scratch-grid layouts, so a test can pin "one
/// shell layout, no scratch grid" per frame and per replayed action, and so
/// the next path to lay the grid out a second time fails a test rather than
/// hiding behind the clip.
///
/// Thread-local, because the editor renders on one thread and tests run one
/// editor per thread. Counted only with debug assertions on; the release
/// build pays nothing.
pub mod stats {
    #[cfg(debug_assertions)]
    use std::cell::Cell;

    #[cfg(debug_assertions)]
    thread_local! {
        static SHELL_LAYOUTS: Cell<u32> = const { Cell::new(0) };
        static SCRATCH_GRIDS: Cell<u32> = const { Cell::new(0) };
        static OFFSCREEN_GRIDS: Cell<u32> = const { Cell::new(0) };
    }

    /// The counts since the last [`take`].
    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
    pub struct LayoutCounts {
        /// Layouts of the shell tree: `Ui::frame` on the render path,
        /// `Ui::layout_only` on the replay path. One per frame is the rule.
        pub shell: u32,
        /// Scratch layouts of the pane grid in a `Ui<()>` —
        /// `SplitNode::get_leaves_with_rects`. Zero on the render path is
        /// the rule; its remaining callers are model queries and tests.
        pub scratch_grids: u32,
        /// Offscreen layouts of another window's grid, for the session
        /// preview ([`super::PaneRects::offscreen`]). One per embedded
        /// window painted.
        pub offscreen_grids: u32,
    }

    #[inline]
    pub fn note_shell_layout() {
        #[cfg(debug_assertions)]
        SHELL_LAYOUTS.with(|c| c.set(c.get().saturating_add(1)));
    }

    #[inline]
    pub fn note_scratch_grid() {
        #[cfg(debug_assertions)]
        SCRATCH_GRIDS.with(|c| c.set(c.get().saturating_add(1)));
    }

    #[inline]
    pub fn note_offscreen_grid() {
        #[cfg(debug_assertions)]
        OFFSCREEN_GRIDS.with(|c| c.set(c.get().saturating_add(1)));
    }

    /// The counts since the last call, which resets them.
    #[cfg(debug_assertions)]
    pub fn take() -> LayoutCounts {
        LayoutCounts {
            shell: SHELL_LAYOUTS.with(|c| c.replace(0)),
            scratch_grids: SCRATCH_GRIDS.with(|c| c.replace(0)),
            offscreen_grids: OFFSCREEN_GRIDS.with(|c| c.replace(0)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::event::SplitDirection;
    use crate::view::shell::splits::PaneChrome;
    use fresh_core::SplitId;

    fn leaf(n: usize) -> SplitNode {
        SplitNode::leaf(BufferId(n), SplitId(n))
    }

    fn split(dir: SplitDirection, a: SplitNode, b: SplitNode, ratio: f32, id: usize) -> SplitNode {
        SplitNode::split(dir, a, b, ratio, SplitId(id))
    }

    fn id(n: usize) -> LeafId {
        LeafId(SplitId(n))
    }

    fn chrome(tabs: bool, vscroll: bool, hscroll: bool) -> PaneChrome {
        PaneChrome {
            tabs,
            vscroll,
            hscroll,
        }
    }

    /// The tree's answer is the scratch grid's answer, pane for pane, for the
    /// box and for the content slot — which is what lets the scratch grid go.
    #[test]
    fn the_tree_places_panes_where_the_scratch_grid_did() {
        let root = split(
            SplitDirection::Vertical,
            leaf(0),
            split(SplitDirection::Horizontal, leaf(1), leaf(2), 0.3, 11),
            0.6,
            10,
        );
        let s = Splits {
            root: root.clone(),
            maximized: None,
            chrome: [
                (id(0), chrome(true, true, true)),
                (id(1), chrome(true, false, false)),
                (id(2), chrome(false, true, false)),
            ]
            .into_iter()
            .collect(),
            controls: Default::default(),
            groups: Default::default(),
            interiors: Default::default(),
        };
        let area = Rect::new(3, 2, 100, 40);
        let _ = stats::take();
        let rects = PaneRects::offscreen(&s, area);

        let want = root.get_leaves_with_rects(area);
        assert_eq!(want.len(), 3);
        // The instrument sees both: the one offscreen layout, and the oracle's
        // scratch grid.
        assert_eq!(
            stats::take(),
            stats::LayoutCounts {
                shell: 0,
                scratch_grids: 1,
                offscreen_grids: 1,
            }
        );
        for (leaf, _, r) in want {
            assert_eq!(rects.pane(leaf), Some(r), "{leaf:?}'s box");
            let c = s.chrome[&leaf];
            let content = crate::view::ui::split_rendering::layout::split_layout(leaf, r, c);
            assert_eq!(
                rects.content(leaf),
                Some(content.content_rect),
                "{leaf:?}'s content slot"
            );
        }
    }

    /// A maximized pane is the only one placed; the others are absent, not
    /// zero — and `visible` gives a leaf the tree did not place a zero rect,
    /// as the scratch grid did.
    #[test]
    fn a_maximized_pane_is_the_only_one_placed() {
        let s = Splits {
            root: split(SplitDirection::Vertical, leaf(0), leaf(1), 0.5, 10),
            maximized: Some(id(1)),
            chrome: Default::default(),
            controls: Default::default(),
            groups: Default::default(),
            interiors: Default::default(),
        };
        let area = Rect::new(0, 0, 80, 24);
        let rects = PaneRects::offscreen(&s, area);
        assert_eq!(rects.pane(id(1)), Some(area));
        assert_eq!(rects.pane(id(0)), None);
        assert_eq!(
            rects.visible(&[(id(0), BufferId(0))]),
            vec![(id(0), BufferId(0), Rect::default())]
        );
    }

    /// A group's panels are panes of the same grid: placed inside the outer
    /// pane's content slot, and readable by their own keys.
    #[test]
    fn a_groups_panels_are_placed_inside_the_outer_panes_content() {
        let group = SplitNode::Grouped {
            split_id: id(50),
            name: "g".into(),
            layout: Box::new(split(SplitDirection::Vertical, leaf(20), leaf(21), 0.5, 30)),
            active_inner_leaf: id(20),
        };
        let s = Splits {
            root: leaf(0),
            maximized: None,
            chrome: [
                (id(0), chrome(true, true, false)),
                (id(20), chrome(false, true, false)),
                (id(21), chrome(false, false, false)),
            ]
            .into_iter()
            .collect(),
            controls: Default::default(),
            groups: [(id(0), group.clone())].into_iter().collect(),
            interiors: Default::default(),
        };
        let area = Rect::new(0, 0, 80, 24);
        let rects = PaneRects::offscreen(&s, area);
        let outer = rects.content(id(0)).expect("the outer content slot");
        assert_eq!(outer, Rect::new(0, 1, 79, 23));
        let want = group.get_leaves_with_rects(outer);
        assert_eq!(want.len(), 2);
        for (leaf, _, r) in want {
            assert_eq!(rects.pane(leaf), Some(r), "{leaf:?} inside the outer pane");
        }
        // A panel with a scrollbar column gives it up from its own box.
        let inner = rects.pane(id(20)).unwrap();
        assert_eq!(
            rects.content(id(20)),
            Some(Rect::new(inner.x, inner.y, inner.width - 1, inner.height))
        );
    }
}
