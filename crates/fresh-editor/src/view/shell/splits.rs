//! The split grid, as a description.
//!
//! **One implementation of where the panes are.** `SplitNode` carried its own
//! layout engine — `get_leaves_with_rects` recursing over ratios and reserving
//! a cell per separator — and everything downstream was keyed on the
//! rectangles it produced: the separator drags, the per-pane scrollbars, the
//! tab strips, click-to-byte. That engine is this description now, and the
//! model's queries are reads of it (`SplitManager::get_visible_buffers`).
//!
//! The rule itself does not move. `split_rect_ext` converts a ratio to cells
//! and pins the first child so its sibling keeps `MIN_PANE_{WIDTH,HEIGHT}` —
//! app logic keyed on the available extent, the same shape as the dock's
//! bail-out. What `layout_reader` adds is the extent: `build()` cannot read
//! geometry, and this is the library's answer for app logic that needs it.
//!
//! No gestures and no paint here: the nodes carry keys and nothing else, so
//! this description can be laid out by the model with `M = ()` as easily as by
//! the shell. The dividers' drags and the panes' content are the editor's, and
//! are added where the grid is mounted.

use std::collections::HashMap;
use std::rc::Rc;

use fresh_ui::{
    col, gesture, layout_reader, row, Event, GestureKind, Key, LayoutInfo, MouseButton, Node,
    PointerMode, Sizing,
};

use crate::model::event::{ContainerId, LeafId, SplitDirection};
use crate::view::split::{split_rect_ext, SplitNode};

use super::msg::{UiFact, UiMsg};

/// A pane's key, by the leaf it shows.
pub fn leaf_key(id: LeafId) -> Key {
    Key::Pair("pane".into(), id.0 .0 as u64)
}

/// A divider's key, by the container it splits.
pub fn divider_key(id: ContainerId) -> Key {
    Key::Pair("divider".into(), id.0 .0 as u64)
}

/// The grid for a split tree, with `maximized` taking the whole box when set.
pub fn grid<M: 'static>(root: &SplitNode, maximized: Option<LeafId>) -> Node<M> {
    if let Some(id) = maximized {
        if let Some(SplitNode::Leaf { split_id, .. }) = root.find(id.into()) {
            // A maximized pane is the whole box and there are no separators —
            // the same two facts `get_visible_buffers` and `get_separators`
            // state separately.
            return pane(*split_id);
        }
    }
    node_of(root)
}

fn pane<M: 'static>(id: LeafId) -> Node<M> {
    row().key(leaf_key(id))
}

fn node_of<M: 'static>(n: &SplitNode) -> Node<M> {
    match n {
        SplitNode::Leaf { split_id, .. } => pane(*split_id),
        SplitNode::Grouped { layout, .. } => node_of(layout),
        SplitNode::Split {
            direction,
            first,
            second,
            ratio,
            split_id,
            fixed_first,
            fixed_second,
        } => {
            let (dir, ratio) = (*direction, *ratio);
            let (ff, fs, id) = (*fixed_first, *fixed_second, *split_id);
            let (a, b) = (first.clone(), second.clone());
            // The cell counts need the extent, and `build` has none. The rule
            // is `split_rect_ext`'s — one copy, the model's own.
            layout_reader(move |info: LayoutInfo| {
                let whole = ratatui::layout::Rect::new(
                    0,
                    0,
                    info.constraints.max_w,
                    info.constraints.max_h,
                );
                let (ra, rb) = split_rect_ext(whole, dir, ratio, ff, fs);
                let (first, second) = (node_of::<M>(&a), node_of::<M>(&b));
                let divider = row().key(divider_key(id));
                match dir {
                    SplitDirection::Vertical => row().children([
                        first.w(Sizing::Cells(ra.width)),
                        divider.w(Sizing::Cells(1)),
                        second.w(Sizing::Cells(rb.width)),
                    ]),
                    SplitDirection::Horizontal => col().children([
                        first.h(Sizing::Cells(ra.height)),
                        divider.h(Sizing::Cells(1)),
                        second.h(Sizing::Cells(rb.height)),
                    ]),
                }
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::event::BufferId;
    use fresh_core::SplitId;
    use fresh_ui::{Size, Ui};
    use ratatui::layout::Rect;

    fn leaf(n: usize) -> SplitNode {
        SplitNode::leaf(BufferId(n), SplitId(n))
    }

    fn split(dir: SplitDirection, a: SplitNode, b: SplitNode, ratio: f32, id: usize) -> SplitNode {
        SplitNode::split(dir, a, b, ratio, SplitId(id))
    }

    /// Every shape worth checking: a lone pane, each direction, an uneven
    /// ratio, nesting both ways, and a `Grouped` node standing in for its own
    /// layout.
    fn shapes() -> Vec<SplitNode> {
        use SplitDirection::{Horizontal, Vertical};
        vec![
            leaf(0),
            split(Vertical, leaf(0), leaf(1), 0.5, 10),
            split(Horizontal, leaf(0), leaf(1), 0.5, 10),
            split(Vertical, leaf(0), leaf(1), 0.25, 10),
            split(Horizontal, leaf(0), leaf(1), 0.8, 10),
            split(
                Vertical,
                split(Horizontal, leaf(0), leaf(1), 0.5, 11),
                leaf(2),
                0.5,
                10,
            ),
            split(
                Horizontal,
                leaf(0),
                split(
                    Vertical,
                    leaf(1),
                    split(Horizontal, leaf(2), leaf(3), 0.4, 12),
                    0.6,
                    11,
                ),
                0.3,
                10,
            ),
        ]
    }

    fn tree_rects(root: &SplitNode, at: Rect) -> Vec<(LeafId, Rect)> {
        let mut ui: Ui<()> = Ui::new();
        ui.frame(grid::<()>(root, None), Size::new(at.width, at.height));
        let mut out: Vec<(LeafId, Rect)> = Vec::new();
        for (id, _, _) in root.reference_leaves_with_rects(at) {
            let e = ui.find_by_key(&leaf_key(id));
            let r = e.map(|e| ui.rect_of(e)).unwrap_or_default();
            out.push((
                id,
                Rect::new(at.x + r.x.max(0) as u16, at.y + r.y.max(0) as u16, r.w, r.h),
            ));
        }
        out
    }

    /// **The tree lays the grid out exactly as the model always did.**
    ///
    /// This is the swap's whole safety argument: `get_leaves_with_rects` is a
    /// layout engine, and it is being replaced by one. It shares the rule
    /// (`split_rect_ext`), so a divergence here would be the *structure*
    /// disagreeing — a reserved separator cell, or which child takes the
    /// remainder.
    #[test]
    fn the_grid_places_every_pane_where_the_model_does() {
        for (i, root) in shapes().iter().enumerate() {
            for (w, h) in [(80u16, 24u16), (200, 60), (40, 12), (31, 9), (120, 40)] {
                let at = Rect::new(0, 0, w, h);
                let want: Vec<(LeafId, Rect)> = root
                    .reference_leaves_with_rects(at)
                    .into_iter()
                    .map(|(id, _, r)| (id, r))
                    .collect();
                assert_eq!(tree_rects(root, at), want, "shape {i} at {w}x{h}");
            }
        }
    }

    /// And at an offset: the model partitions the rectangle it is given, so
    /// the tree's answer is its own rectangle plus the frame's origin.
    #[test]
    fn an_offset_box_moves_every_pane_with_it() {
        let root = split(SplitDirection::Vertical, leaf(0), leaf(1), 0.5, 10);
        let at = Rect::new(7, 3, 60, 20);
        let want: Vec<(LeafId, Rect)> = root
            .reference_leaves_with_rects(at)
            .into_iter()
            .map(|(id, _, r)| (id, r))
            .collect();
        assert_eq!(tree_rects(&root, at), want);
    }

    /// A maximized pane is the whole box, and the separators go with it —
    /// two facts `get_visible_buffers` and `get_separators` state apart.
    #[test]
    fn a_maximized_pane_takes_the_whole_box() {
        let root = split(SplitDirection::Vertical, leaf(0), leaf(1), 0.5, 10);
        let mut ui: Ui<()> = Ui::new();
        ui.frame(
            grid::<()>(&root, Some(LeafId(SplitId(1)))),
            Size::new(80, 24),
        );
        let r = ui.rect_of(
            ui.find_by_key(&leaf_key(LeafId(SplitId(1))))
                .expect("the pane"),
        );
        assert_eq!((r.x, r.y, r.w, r.h), (0, 0, 80, 24));
        assert!(
            ui.find_by_key(&leaf_key(LeafId(SplitId(0)))).is_none(),
            "the other pane is not shown at all"
        );
    }

    /// What one grid layout costs — the number the swap turns on, since the
    /// model's queries become this and some of them are per-frame.
    ///
    /// Reported, not bounded tightly: a wall-clock threshold is a flake
    /// waiting for a loaded runner. `--nocapture` to read it.
    #[test]
    fn a_grid_layout_is_cheap_enough_to_be_the_query() {
        use std::time::Instant;
        let root = split(
            SplitDirection::Horizontal,
            leaf(0),
            split(SplitDirection::Vertical, leaf(1), leaf(2), 0.5, 11),
            0.3,
            10,
        );
        const N: u32 = 2_000;
        let t = Instant::now();
        for _ in 0..N {
            let mut ui: Ui<()> = Ui::new();
            ui.frame(grid::<()>(&root, None), Size::new(200, 60));
            std::hint::black_box(ui.find_by_key(&leaf_key(LeafId(SplitId(2)))));
        }
        let per = t.elapsed() / N;
        println!("grid layout (3 panes, cold): {per:?}");
        assert!(per.as_millis() < 10, "a grid layout took {per:?}");
    }

    /// **A pane divides itself exactly as the painter's arithmetic did.**
    ///
    /// Every combination of the three flags, at sizes down to one that starves
    /// the content entirely — the horizontal bar's width is the part a reader
    /// gets wrong from the picture, since it stops short of the vertical
    /// bar's column instead of running under it.
    #[test]
    fn a_pane_divides_itself_the_way_the_painter_did() {
        use crate::view::ui::split_rendering::layout::{reference_split_layout, split_layout};
        for tabs in [true, false] {
            for vs in [true, false] {
                for hs in [true, false] {
                    for at in [
                        Rect::new(0, 0, 80, 24),
                        Rect::new(7, 3, 40, 12),
                        Rect::new(0, 0, 3, 2),
                        Rect::new(12, 5, 200, 60),
                    ] {
                        let id = LeafId(SplitId(9));
                        let c = PaneChrome {
                            tabs,
                            vscroll: vs,
                            hscroll: hs,
                        };
                        let got = split_layout(id, at, c);
                        let want = reference_split_layout(at, tabs, vs, hs);
                        assert_eq!(
                            (
                                got.tabs_rect,
                                got.content_rect,
                                got.scrollbar_rect,
                                got.horizontal_scrollbar_rect
                            ),
                            (
                                want.tabs_rect,
                                want.content_rect,
                                want.scrollbar_rect,
                                want.horizontal_scrollbar_rect
                            ),
                            "tabs={tabs} vscroll={vs} hscroll={hs} at {at:?}"
                        );
                    }
                }
            }
        }
    }

    /// **The right-click clear fires even when the click is consumed.**
    ///
    /// That is the whole point of it, and the thing the `LayoutBox` at z 200
    /// could not do: the legacy walk runs only when the tree declines the
    /// event, so a right-click any migrated surface took skipped the guard.
    /// The theme inspector's own capture listener `stop()`s the click, which
    /// makes it the sharpest case available in a bare frame.
    #[test]
    fn a_right_click_clears_the_tab_menus_even_when_something_eats_it() {
        use crate::view::shell::frame::{frame_tree, Frame};
        use fresh_ui::{Input, Mods, Point, Size, Ui};
        let facts = |mods: Mods| -> Vec<UiFact> {
            let mut ui: Ui<UiMsg> = Ui::new();
            ui.frame(frame_tree(Frame::default()), Size::new(80, 24));
            ui.dispatch(Input::press(Point::new(40, 12), MouseButton::Right, mods))
                .msgs
                .into_iter()
                .filter_map(|m| match m {
                    UiMsg::Ui(f) => Some(f),
                    _ => None,
                })
                .collect()
        };
        assert_eq!(
            facts(Mods::NONE),
            vec![UiFact::ClearTabMenus],
            "a plain right-click clears them and goes on"
        );
        assert_eq!(
            facts(Mods::CTRL),
            vec![UiFact::ClearTabMenus, UiFact::ThemeInspect { x: 40, y: 12 },],
            "and so does one the inspector stops"
        );
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(frame_tree(Frame::default()), Size::new(80, 24));
        let left: Vec<UiFact> = ui
            .dispatch(Input::press(
                Point::new(40, 12),
                MouseButton::Left,
                Mods::NONE,
            ))
            .msgs
            .into_iter()
            .filter_map(|m| match m {
                UiMsg::Ui(f) => Some(f),
                _ => None,
            })
            .collect();
        assert!(
            !left.contains(&UiFact::ClearTabMenus),
            "a left click is how they open, not how they close"
        );
    }

    /// **An inner panel is a pane with two of its three parts off.**
    ///
    /// A buffer group's panel is laid out inside its outer pane's interior, so
    /// it has no strip and no bottom bar — only the scrollbar its own content
    /// earns. `render_content` wrote those four rectangles by hand right where
    /// it branched on the panel; this is that branch, and it is the same
    /// `pane_interior` every other pane gets.
    #[test]
    fn an_inner_panel_is_a_pane_with_two_parts_off() {
        use crate::view::ui::split_rendering::layout::split_layout;
        for vs in [true, false] {
            for at in [
                Rect::new(0, 0, 80, 24),
                Rect::new(7, 3, 40, 12),
                Rect::new(0, 0, 1, 1),
            ] {
                let got = split_layout(
                    LeafId(SplitId(9)),
                    at,
                    PaneChrome {
                        tabs: false,
                        vscroll: vs,
                        hscroll: false,
                    },
                );
                let bar = at.width.min(vs as u16);
                assert_eq!(
                    got.content_rect,
                    Rect::new(at.x, at.y, at.width - bar, at.height),
                    "the panel's whole area but the bar's column, vscroll={vs} at {at:?}"
                );
                assert_eq!(
                    got.scrollbar_rect,
                    Rect::new(at.x + at.width - bar, at.y, bar, at.height),
                    "and the bar beside it, vscroll={vs} at {at:?}"
                );
                assert_eq!(got.tabs_rect.height, 0, "no strip");
                assert_eq!(got.horizontal_scrollbar_rect.height, 0, "no bottom bar");
            }
        }
    }

    /// **The one case where the arithmetic was wrong, and the layout is not.**
    ///
    /// A pane one row tall with both a tab strip and a horizontal scrollbar
    /// wants two rows and has one. The painter derived the bar's `y` from the
    /// bottom — `y + height - 1` — without noticing the tabs had already taken
    /// that row, so it produced a rectangle *overlapping* the tab strip and
    /// drew the bar over it. A column starves its last child instead: there is
    /// no room, so the bar gets none.
    ///
    /// Kept as a test rather than folded into the sweep above, because it is a
    /// deliberate divergence and the sweep is a parity claim.
    #[test]
    fn a_starved_pane_no_longer_draws_its_scrollbar_over_its_tabs() {
        use crate::view::ui::split_rendering::layout::{reference_split_layout, split_layout};
        let at = Rect::new(0, 0, 1, 1);
        let old = reference_split_layout(at, true, true, true);
        let new = split_layout(
            LeafId(SplitId(9)),
            at,
            PaneChrome {
                tabs: true,
                vscroll: true,
                hscroll: true,
            },
        );
        assert_eq!(
            old.tabs_rect,
            Rect::new(0, 0, 1, 1),
            "the tabs take the row"
        );
        assert_eq!(
            old.horizontal_scrollbar_rect,
            Rect::new(0, 0, 0, 1),
            "and the old bar was on the same row"
        );
        assert_eq!(
            new.horizontal_scrollbar_rect.height, 0,
            "there is no room for it, so it has none"
        );
    }

    /// The dividers land on the cells the model reserves for them.
    #[test]
    fn the_dividers_are_where_the_separators_are() {
        for (i, root) in shapes().iter().enumerate() {
            for (w, h) in [(80u16, 24u16), (200, 60), (41, 13)] {
                let at = Rect::new(0, 0, w, h);
                let mut ui: Ui<()> = Ui::new();
                ui.frame(grid::<()>(root, None), Size::new(w, h));
                for (id, dir, x, y, len) in root.get_separators_with_ids(at) {
                    let e = ui
                        .find_by_key(&divider_key(id))
                        .unwrap_or_else(|| panic!("shape {i}: no divider for {id:?}"));
                    let r = ui.rect_of(e);
                    let want = match dir {
                        SplitDirection::Horizontal => (x, y, len, 1),
                        SplitDirection::Vertical => (x, y, 1, len),
                    };
                    assert_eq!(
                        (r.x as u16, r.y as u16, r.w, r.h),
                        want,
                        "shape {i} at {w}x{h}, divider {id:?}"
                    );
                }
            }
        }
    }
}

/// What the shell needs to state about the grid.
#[derive(Clone, Debug, PartialEq)]
pub struct Splits {
    pub root: SplitNode,
    pub maximized: Option<LeafId>,
    /// Which chrome each visible pane has. Resolved once, by the editor, and
    /// read by both halves of the frame — the description below and the
    /// painter that fills the `Host` leaf under it — so a pane's strip cannot
    /// be a row tall in one and absent in the other.
    pub chrome: std::collections::HashMap<LeafId, PaneChrome>,
}

/// The grid mounted over the body's `Host` leaf: geometry and the dividers'
/// gestures, painting nothing.
///
/// The panes are `Ignore` — not there at all, as far as the pointer is
/// concerned — because the body's clicks are still the legacy walk's: placing
/// a caret, selecting a word, hitting a tab strip. What the tree takes is the
/// divider, and it takes it *because the node knows which container it is*.
/// `handle_click_split_separator` searched a recorded list of separator
/// rectangles to answer that, comparing a click against each in turn.
pub fn overlay(s: &Splits) -> Node<UiMsg> {
    dress(
        grid::<UiMsg>(&s.root, s.maximized),
        &s.root,
        s.maximized,
        &s.chrome,
    )
}

/// Walk the built grid and give each node its pointer role.
///
/// Done as a second pass rather than inside `grid` so the description stays
/// message-agnostic: the model lays the same grid out with `M = ()`, and a
/// gesture would make that impossible.
fn dress(
    n: Node<UiMsg>,
    root: &SplitNode,
    maximized: Option<LeafId>,
    chrome: &HashMap<LeafId, PaneChrome>,
) -> Node<UiMsg> {
    // The grid is built by `layout_reader`s, so its structure is not walkable
    // before layout. Instead the dressing is applied by rebuilding: the same
    // recursion, with roles.
    let _ = n;
    if let Some(id) = maximized {
        if let Some(SplitNode::Leaf { split_id, .. }) = root.find(id.into()) {
            return pane_inert::<UiMsg>().children([pane_interior::<UiMsg>(
                *split_id,
                chrome.get(split_id).copied().unwrap_or_default(),
            )]);
        }
        return pane_inert::<UiMsg>();
    }
    dressed(root, chrome)
}

fn pane_inert<M: 'static>() -> Node<M> {
    row().pointer_mode(PointerMode::Ignore)
}

fn dressed(n: &SplitNode, chrome: &HashMap<LeafId, PaneChrome>) -> Node<UiMsg> {
    match n {
        SplitNode::Leaf { split_id, .. } => {
            pane_inert::<UiMsg>().children([pane_interior::<UiMsg>(
                *split_id,
                chrome.get(split_id).copied().unwrap_or_default(),
            )])
        }
        SplitNode::Grouped { layout, .. } => dressed(layout, chrome),
        SplitNode::Split {
            direction,
            first,
            second,
            ratio,
            split_id,
            fixed_first,
            fixed_second,
        } => {
            let (dir, ratio) = (*direction, *ratio);
            let (ff, fs, id) = (*fixed_first, *fixed_second, *split_id);
            let (a, b) = (first.clone(), second.clone());
            let chrome = chrome.clone();
            layout_reader(move |info: LayoutInfo| {
                let whole = ratatui::layout::Rect::new(
                    0,
                    0,
                    info.constraints.max_w,
                    info.constraints.max_h,
                );
                let (ra, rb) = split_rect_ext(whole, dir, ratio, ff, fs);
                let (first, second) = (dressed(&a, &chrome), dressed(&b, &chrome));
                let div = divider(id, dir);
                match dir {
                    SplitDirection::Vertical => {
                        row().pointer_mode(PointerMode::Transparent).children([
                            first.w(Sizing::Cells(ra.width)),
                            div.w(Sizing::Cells(1)),
                            second.w(Sizing::Cells(rb.width)),
                        ])
                    }
                    SplitDirection::Horizontal => {
                        col().pointer_mode(PointerMode::Transparent).children([
                            first.h(Sizing::Cells(ra.height)),
                            div.h(Sizing::Cells(1)),
                            second.h(Sizing::Cells(rb.height)),
                        ])
                    }
                }
            })
        }
    }
}

/// One divider: it starts the width drag, and it says when it is hovered.
///
/// It paints nothing — the split renderer still draws the separator glyph and
/// its hover highlight, from `separator_areas`, which is itself a read of this
/// same layout.
fn divider(id: ContainerId, dir: SplitDirection) -> Node<UiMsg> {
    gesture(row())
        .key(divider_key(id))
        .on(
            GestureKind::Press,
            Rc::new(move |e: &Event| {
                if e.button != MouseButton::Left {
                    return None;
                }
                e.stop();
                Some(UiMsg::Ui(UiFact::SeparatorPress {
                    container: id,
                    direction: dir,
                    x: e.pos.x.max(0) as u16,
                    y: e.pos.y.max(0) as u16,
                }))
            }),
        )
        .on_enter(Rc::new(move |_: &Event| {
            Some(UiMsg::Ui(UiFact::SeparatorHover(Some((id, dir)))))
        }))
        .on_leave(Rc::new(move |_: &Event| {
            Some(UiMsg::Ui(UiFact::SeparatorHover(None)))
        }))
}

/// **A right-click anywhere clears the two left-click-only menus**, then lets
/// the click go on to whatever it was aimed at.
///
/// The "+" new-tab menu and the close-split confirmation open on a left click
/// and have no right-click behaviour of their own, so a right-click aimed
/// past them should dismiss them the way clicking elsewhere does — including
/// the right-click that *opens* a tab's context menu, which is aimed at a tab
/// with the "+" menu still hanging over it.
///
/// A capture-phase listener that does not `stop()`: it runs before anything
/// under the pointer sees the click, and the click continues. It was a
/// full-screen box in the legacy walk at the top of the z band — but that walk
/// runs only when the tree declines the event, so the guard silently did not
/// fire for a right-click any migrated surface took. Here it always does.
pub fn tab_menu_guard(frame: Node<UiMsg>) -> Node<UiMsg> {
    gesture(frame).on_capture(
        GestureKind::Press,
        Rc::new(|e: &Event| {
            (e.button == MouseButton::Right).then_some(UiMsg::Ui(UiFact::ClearTabMenus))
        }),
    )
}

// ── the pane's interior ─────────────────────────────────────────────────────

/// The parts of a pane, by role and by the pane they belong to.
///
/// Keyed per leaf because `find_by_key` takes the first match in tree order: a
/// grid of panes puts several interiors in one tree, and a bare `"pane_tabs"`
/// would name whichever pane came first.
pub fn tabs_key(id: LeafId) -> Key {
    Key::Pair("pane_tabs".into(), id.0 .0 as u64)
}
pub fn content_key(id: LeafId) -> Key {
    Key::Pair("pane_content".into(), id.0 .0 as u64)
}
pub fn vscroll_key(id: LeafId) -> Key {
    Key::Pair("pane_vscroll".into(), id.0 .0 as u64)
}
pub fn hscroll_key(id: LeafId) -> Key {
    Key::Pair("pane_hscroll".into(), id.0 .0 as u64)
}

/// Which of a pane's three chrome parts exist.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct PaneChrome {
    pub tabs: bool,
    pub vscroll: bool,
    pub hscroll: bool,
}

/// What a pane is, in the parts that decide its chrome.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PaneKind {
    /// A panel inside an active buffer group. It is laid out *within* the
    /// outer pane's interior, so it has no strip and no bottom bar of its
    /// own — only the scrollbar its own content earns.
    pub inner_group_leaf: bool,
    /// The view asked for no tab strip (a group's panel, a plugin's dock).
    pub suppress_chrome: bool,
    /// The buffer scrolls at all. A `Fixed` panel does not, and never had a
    /// bar in either direction.
    pub scrollable: bool,
    /// A terminal streaming its live PTY grid gives up the scrollbar column so
    /// the grid can use it. A terminal held in read-only scrollback keeps it,
    /// per split, so one terminal in two panes can differ (fresh#2595).
    pub terminal_live_grid: bool,
}

impl Default for PaneKind {
    fn default() -> Self {
        Self {
            inner_group_leaf: false,
            suppress_chrome: false,
            scrollable: true,
            terminal_live_grid: false,
        }
    }
}

impl PaneChrome {
    /// **The rule, stated once.** What the window offers, narrowed by what
    /// this pane is.
    ///
    /// `window` is the frame-wide half — the tab bar's visibility and the two
    /// scrollbar config flags — which is why it is the same type: at the
    /// window level these are "offered", at the pane level "present".
    pub fn resolve(window: PaneChrome, pane: PaneKind) -> Self {
        PaneChrome {
            tabs: window.tabs && !pane.inner_group_leaf && !pane.suppress_chrome,
            vscroll: window.vscroll && pane.scrollable && !pane.terminal_live_grid,
            hscroll: window.hscroll && pane.scrollable && !pane.inner_group_leaf,
        }
    }
}

/// How a pane divides itself: a tab strip on top, the content with its
/// vertical scrollbar beside it, and a horizontal scrollbar under both.
///
/// Which of the three exist is per-pane state — a buffer group's panel
/// suppresses its tab strip, a terminal showing its live PTY grid gives up its
/// scrollbar column so the grid can use it, a non-scrollable panel never had
/// one — so they arrive as flags, resolved before the description is built.
///
/// The horizontal bar stops short of the vertical one's column rather than
/// running under it. That is the painter's arithmetic and it is the one thing
/// here a reader would get wrong from the picture alone.
pub fn pane_interior<M: 'static>(id: LeafId, c: PaneChrome) -> Node<M> {
    let cells = |on: bool| Sizing::Cells(on as u16);
    col().children([
        row().key(tabs_key(id)).h(cells(c.tabs)),
        row().flex(1).children([
            row().key(content_key(id)).flex(1),
            row().key(vscroll_key(id)).w(cells(c.vscroll)),
        ]),
        row().h(cells(c.hscroll)).children([
            row().key(hscroll_key(id)).flex(1),
            // The column the vertical bar occupies, kept clear.
            row().w(cells(c.vscroll)),
        ]),
    ])
}
