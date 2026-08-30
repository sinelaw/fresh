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

use std::rc::Rc;

use fresh_ui::{
    col, gesture, host, layout_reader, row, stack, Axis, Event, GestureKind, Key, LayoutInfo,
    MouseButton, Node, PointerMode, Sizing,
};

use crate::app::types::HoverTarget;
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

    /// Every pane the fold reaches, and the rectangle it is handed — the
    /// paint half's answer to [`tree_rects`]'s.
    fn panes_folded(s: &Splits, at: Rect) -> Vec<(LeafId, Rect)> {
        use crate::view::shell::fold::{fold_band, Band, Caret, HostPainter, Paints};
        use crate::view::shell::frame::HostTarget;

        #[derive(Default)]
        struct Panes(Vec<(LeafId, Rect)>);
        impl HostPainter for Panes {
            fn paint_host(
                &mut self,
                target: HostTarget,
                rect: Rect,
                _buf: &mut ratatui::buffer::Buffer,
                _caret: &mut Caret,
            ) {
                if let HostTarget::Pane(leaf) = target {
                    self.0.push((leaf, rect));
                }
            }
        }

        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(overlay(s), Size::new(at.width, at.height));
        let mut buf = ratatui::buffer::Buffer::empty(at);
        let mut out = Panes::default();
        fold_band(
            ui.spec(),
            &mut buf,
            &|_: &fresh_ui::ThemeKey| ratatui::style::Style::default(),
            &mut out,
            Band::Background,
            Paints::All,
        );
        out.0
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

    /// **A buffer group's dividers are the pane's own dividers.**
    ///
    /// A group's layout lives in a side map on the window, not in the split
    /// tree, and was dispatched into the pane's interior only at paint time —
    /// so its separators stayed recorded rectangles (`chrome:group_separators`)
    /// long after the main tree's became nodes, and a drag on one had to
    /// search that list to find which container it was. Mounted in the pane's
    /// content, they are ordinary dividers, at the cells the painter draws
    /// them at: it derives those from `get_separators_with_ids(content_rect)`,
    /// and the content node *is* that rectangle.
    #[test]
    fn a_groups_dividers_land_where_its_separators_are_drawn() {
        let group = SplitNode::Grouped {
            split_id: LeafId(SplitId(50)),
            name: "g".into(),
            layout: Box::new(split(SplitDirection::Vertical, leaf(20), leaf(21), 0.5, 30)),
            active_inner_leaf: LeafId(SplitId(20)),
        };
        let host = LeafId(SplitId(0));
        let with_tabs = PaneChrome {
            tabs: true,
            vscroll: true,
            hscroll: false,
        };
        let s = Splits {
            root: leaf(0),
            maximized: None,
            chrome: [(host, with_tabs)].into_iter().collect(),
            controls: Default::default(),
            groups: [(host, group.clone())].into_iter().collect(),
        };
        let (w, h) = (80u16, 24u16);
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(overlay(&s), Size::new(w, h));

        // The pane's content rect, as the interior divides it: a strip row off
        // the top and the scrollbar column off the right.
        let content = ui.rect_of(ui.find_by_key(&content_key(host)).expect("the content"));
        assert_eq!(
            (content.x, content.y, content.w, content.h),
            (0, 1, w - 1, h - 1),
            "past the strip and beside the bar"
        );

        let at = Rect::new(content.x as u16, content.y as u16, content.w, content.h);
        let SplitNode::Grouped { layout, .. } = &group else {
            unreachable!()
        };
        let want = layout.get_separators_with_ids(at);
        assert_eq!(want.len(), 1, "one separator inside the group");
        for (id, _dir, x, y, len) in want {
            let r = ui.rect_of(ui.find_by_key(&divider_key(id)).expect("the divider"));
            assert_eq!(
                (r.x, r.y, r.h),
                (x as i32, y as i32, len),
                "{id:?} where the painter draws it"
            );
        }
    }

    /// **Every pane paints into the rectangle layout gave it.**
    ///
    /// The paint half of the split, and the claim that makes it worth making:
    /// the body was one `Host` that the split renderer filled with every pane
    /// at once, laying them out a second time from `SplitManager` — so a pane
    /// was painted at one engine's rectangle and clicked at another's, and
    /// the two merely agreed. The fold reaches one pane at a time now, and
    /// what it hands each is this description's own rectangle.
    #[test]
    fn the_fold_reaches_every_pane_at_its_own_rect() {
        for (i, root) in shapes().iter().enumerate() {
            for (w, h) in [(80u16, 24u16), (200, 60), (31, 9)] {
                let at = Rect::new(0, 0, w, h);
                let s = Splits {
                    root: root.clone(),
                    maximized: None,
                    chrome: Default::default(),
                    controls: Default::default(),
                    groups: Default::default(),
                };
                let want: Vec<(LeafId, Rect)> = root
                    .reference_leaves_with_rects(at)
                    .into_iter()
                    .map(|(id, _, r)| (id, r))
                    .collect();
                assert_eq!(panes_folded(&s, at), want, "shape {i} at {w}x{h}");
            }
        }
    }

    /// **A maximized pane is the only one the fold reaches.**
    ///
    /// The same two facts `get_visible_buffers` states when a pane is
    /// maximized: it is the whole box, and it is alone.
    #[test]
    fn a_maximized_pane_is_the_only_host_in_the_grid() {
        let root = split(SplitDirection::Vertical, leaf(0), leaf(1), 0.5, 10);
        let at = Rect::new(0, 0, 80, 24);
        let s = Splits {
            root,
            maximized: Some(LeafId(SplitId(1))),
            chrome: Default::default(),
            controls: Default::default(),
            groups: Default::default(),
        };
        assert_eq!(panes_folded(&s, at), vec![(LeafId(SplitId(1)), at)]);
    }

    /// **A group's panels are hosts of their own, inside their pane's.**
    ///
    /// `expand_visible_buffers` lays a group out in its pane's *content*
    /// rectangle — past the strip and the scrollbar column — and paints one
    /// entry per inner leaf. Those entries are these hosts, and the pane's own
    /// still comes first: it is their ancestor, and it is the one that paints
    /// the group tab.
    #[test]
    fn a_groups_panels_are_hosts_inside_their_panes() {
        use crate::view::ui::split_rendering::layout::split_layout;
        let inner = split(SplitDirection::Vertical, leaf(20), leaf(21), 0.5, 30);
        let group = SplitNode::Grouped {
            split_id: LeafId(SplitId(50)),
            name: "g".into(),
            layout: Box::new(inner.clone()),
            active_inner_leaf: LeafId(SplitId(20)),
        };
        let host_leaf = LeafId(SplitId(0));
        let chrome = PaneChrome {
            tabs: true,
            vscroll: true,
            hscroll: false,
        };
        let at = Rect::new(0, 0, 80, 24);
        let s = Splits {
            root: leaf(0),
            maximized: None,
            chrome: [(host_leaf, chrome)].into_iter().collect(),
            controls: Default::default(),
            groups: [(host_leaf, group)].into_iter().collect(),
        };

        let content = split_layout(host_leaf, at, chrome).content_rect;
        let mut want = vec![(host_leaf, at)];
        want.extend(
            inner
                .reference_leaves_with_rects(content)
                .into_iter()
                .map(|(id, _, r)| (id, r)),
        );
        assert_eq!(panes_folded(&s, at), want);
    }

    /// **The two strip buttons are where the painter draws their glyphs.**
    ///
    /// The cluster is `[gap] > [□] [×] [trail]` at the right end of the strip,
    /// and the painter walks it with a running `cx`. The description walks the
    /// same shape as a row of one-cell nodes, so this is the parity claim that
    /// replaces `close_split_areas` and `maximize_split_areas`: those were the
    /// painter's `cx` recorded as rectangles, and a press was a comparison
    /// against them.
    #[test]
    fn the_strip_buttons_land_where_the_painter_draws_them() {
        use crate::view::ui::split_rendering::layout::split_layout;
        use crate::view::ui::tabs::split_control_reserve;
        for (maximize, close) in [(true, true), (true, false), (false, true), (false, false)] {
            let controls = PaneControls { maximize, close };
            assert_eq!(
                controls.reserve(),
                split_control_reserve(maximize, close),
                "the description reserves what the painter reserves"
            );
            if controls.reserve() == 0 {
                continue;
            }
            let chrome = PaneChrome {
                tabs: true,
                vscroll: true,
                hscroll: false,
            };
            let pane = LeafId(SplitId(0));
            for at in [Rect::new(0, 0, 80, 24), Rect::new(5, 2, 40, 12)] {
                let s = Splits {
                    root: leaf(0),
                    maximized: None,
                    chrome: [(pane, chrome)].into_iter().collect(),
                    controls,
                    groups: Default::default(),
                };
                let mut ui: Ui<UiMsg> = Ui::new();
                ui.frame(overlay(&s), Size::new(at.width, at.height));

                // The painter's own arithmetic, from `render_split_tab_bar`.
                let strip = split_layout(pane, at, chrome).tabs_rect;
                let cluster_x = strip.x + strip.width.saturating_sub(controls.reserve());
                // It skips the gap, then the reserved `>` column.
                let mut cx = cluster_x + 2;
                let mut want: Vec<(Key, u16)> = Vec::new();
                if maximize {
                    want.push((maximize_key(pane), cx));
                    cx += 1;
                }
                if close {
                    want.push((close_key(pane), cx));
                }

                for (key, x) in want {
                    let e = ui.find_by_key(&key).expect("the button");
                    let r = ui.rect_of(e);
                    assert_eq!(
                        (at.x + r.x.max(0) as u16, at.y + r.y.max(0) as u16, r.w, r.h),
                        (x, strip.y, 1, 1),
                        "{key:?} at {at:?}, maximize={maximize} close={close}"
                    );
                }
            }
        }
    }

    /// **A pane's host id can never be mistaken for a region's.**
    ///
    /// Regions are the seven fixed slots, numbered 1..=7; `LeafId`s come from
    /// a dense counter that starts at the same place, so the two id spaces
    /// would overlap on the very first pane. The tag is what keeps the fold's
    /// "this id names nothing" assertion able to mean it.
    #[test]
    fn a_panes_host_id_is_never_a_regions() {
        use crate::view::shell::frame::{pane_host_id, HostRegion, HostTarget};
        for r in HostRegion::ALL {
            assert_eq!(
                HostTarget::from_host_id(r.into()),
                Some(HostTarget::Region(r)),
                "{r:?} still resolves to itself"
            );
        }
        for n in [0usize, 1, 4, 7, 63, 4096] {
            let leaf = LeafId(SplitId(n));
            assert_eq!(
                HostTarget::from_host_id(pane_host_id(leaf)),
                Some(HostTarget::Pane(leaf)),
                "pane {n} round-trips"
            );
            assert!(
                HostRegion::from_host_id(pane_host_id(leaf)).is_none(),
                "pane {n} is not a region"
            );
        }
    }

    /// **A press on a pane's strip names that pane, because it is that pane's.**
    ///
    /// Two `LayoutBox`es covered the tab row — the strip at z 60 and the split
    /// controls at 70 — and both recovered the pane by comparing the cell
    /// against every recorded `bar_area` in turn (`tab_bar_split_at`). A node
    /// knows which pane it belongs to; what is left to hit-test is the strip's
    /// *interior*, which is the tab renderer's layout and stays there.
    #[test]
    fn a_press_on_a_strip_names_the_pane_it_belongs_to() {
        use fresh_ui::{Input, Mods, Point};
        let root = split(SplitDirection::Vertical, leaf(0), leaf(1), 0.5, 10);
        let with_tabs = PaneChrome {
            tabs: true,
            vscroll: false,
            hscroll: false,
        };
        let s = Splits {
            root: root.clone(),
            maximized: None,
            chrome: [
                (LeafId(SplitId(0)), with_tabs),
                (LeafId(SplitId(1)), with_tabs),
            ]
            .into_iter()
            .collect(),
            controls: Default::default(),
            groups: Default::default(),
        };
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(overlay(&s), Size::new(80, 24));

        // Each pane's strip is its own top row, where the interior puts it.
        // The right pane starts at 41: the divider takes the column at 40.
        for (leaf_id, want_x) in [(LeafId(SplitId(0)), 0u16), (LeafId(SplitId(1)), 41)] {
            let r = ui.rect_of(ui.find_by_key(&tabs_key(leaf_id)).expect("a strip"));
            assert_eq!(
                (r.x, r.y, r.h),
                (want_x as i32, 0, 1),
                "{leaf_id:?}'s strip is its top row"
            );
        }

        let press = |ui: &mut Ui<UiMsg>, x: u16| -> Vec<UiFact> {
            ui.dispatch(Input::press(
                Point::new(x as i32, 0),
                MouseButton::Left,
                Mods::NONE,
            ))
            .msgs
            .into_iter()
            .filter_map(|m| match m {
                UiMsg::Ui(f) if f != UiFact::ClearTabMenus => Some(f),
                _ => None,
            })
            .collect()
        };
        assert_eq!(
            press(&mut ui, 5),
            vec![UiFact::PaneTabsPress {
                pane: LeafId(SplitId(0)),
                x: 5,
                y: 0
            }],
            "the left strip"
        );
        assert_eq!(
            press(&mut ui, 60),
            vec![UiFact::PaneTabsPress {
                pane: LeafId(SplitId(1)),
                x: 60,
                y: 0
            }],
            "and the right one"
        );
    }

    /// A pane with no strip has no row for one, so its top row is content —
    /// the flag decides that, not a recorded rectangle.
    #[test]
    fn a_pane_with_no_strip_gives_its_top_row_to_the_content() {
        use fresh_ui::{Input, Mods, Point};
        let s = Splits {
            root: leaf(0),
            maximized: None,
            chrome: [(LeafId(SplitId(0)), PaneChrome::default())]
                .into_iter()
                .collect(),
            controls: Default::default(),
            groups: Default::default(),
        };
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(overlay(&s), Size::new(80, 24));
        let r = ui.rect_of(
            ui.find_by_key(&tabs_key(LeafId(SplitId(0))))
                .expect("a strip"),
        );
        assert_eq!(r.h, 0, "no row for it");
        let said: Vec<UiFact> = ui
            .dispatch(Input::press(
                Point::new(5, 0),
                MouseButton::Left,
                Mods::NONE,
            ))
            .msgs
            .into_iter()
            .filter_map(|m| match m {
                UiMsg::Ui(f) if f != UiFact::ClearTabMenus => Some(f),
                _ => None,
            })
            .collect();
        assert_eq!(
            said,
            vec![UiFact::PaneContentPress {
                pane: LeafId(SplitId(0)),
                x: 5,
                y: 0,
                clicks: 1,
                mods: fresh_ui::Mods::NONE,
            }],
            "the content's, not the strip's"
        );
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

/// Which of the two buttons the strips carry, this frame.
///
/// **Frame-wide, not per-pane**, because that is how the painter decides them:
/// `show_maximize = has_multiple_splits || is_maximized` and `show_close =
/// has_multiple_splits && !is_maximized`, neither of which mentions a
/// particular pane. Every strip in a frame therefore reserves the same number
/// of columns for them — which is the property that lets the cluster be a
/// fixed-width row rather than something measured.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct PaneControls {
    pub maximize: bool,
    pub close: bool,
}

impl PaneControls {
    /// The columns the cluster occupies: `[gap][>][□?][×?][trail]`.
    ///
    /// The `>` overflow slot is reserved whether or not the tabs overflow, so
    /// the cluster never shifts as they scroll — which is why this does not
    /// depend on anything the tab renderer measures. It is
    /// `tabs::split_control_reserve`, and the parity test says so.
    pub fn reserve(self) -> u16 {
        match self.maximize || self.close {
            false => 0,
            true => 1 + 1 + self.maximize as u16 + self.close as u16 + 1,
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
    /// Which buttons this frame's strips carry. See [`PaneControls`].
    pub controls: PaneControls,
    /// The buffer group each pane is showing, if any, by the pane it is shown
    /// in. A group's layout lives in a side map on the window rather than in
    /// the split tree, and is dispatched at render time into the pane's
    /// interior — so the grid a pane holds is stated here rather than found by
    /// walking `root`.
    pub groups: std::collections::HashMap<LeafId, SplitNode>,
}

/// The grid mounted over the body's `Host` leaf: geometry and the dividers'
/// gestures, painting nothing.
///
/// The panes are `Transparent`: what the tree claims inside one, it claims, and
/// everything else carries on to the legacy walk — placing a caret, selecting
/// a word, the scrollbars. What the tree takes so far is the divider and the
/// tab strip, and it takes each *because the node knows which pane or
/// container it is*. `handle_click_split_separator` and `tab_bar_split_at`
/// both answered that by comparing a cell against a recorded list of
/// rectangles, one at a time.
pub fn overlay(s: &Splits) -> Node<UiMsg> {
    let s = Rc::new(s.clone());
    dress(grid::<UiMsg>(&s.root, s.maximized), &s)
}

/// Walk the built grid and give each node its pointer role.
///
/// Done as a second pass rather than inside `grid` so the description stays
/// message-agnostic: the model lays the same grid out with `M = ()`, and a
/// gesture would make that impossible.
fn dress(n: Node<UiMsg>, s: &Rc<Splits>) -> Node<UiMsg> {
    // The grid is built by `layout_reader`s, so its structure is not walkable
    // before layout. Instead the dressing is applied by rebuilding: the same
    // recursion, with roles.
    let _ = n;
    if let Some(id) = s.maximized {
        if let Some(SplitNode::Leaf { split_id, .. }) = s.root.find(id.into()) {
            return live_pane(*split_id, s);
        }
        return pane_inert::<UiMsg>();
    }
    dressed(&s.root, s)
}

/// A pane, as far as the pointer is concerned: `Transparent`, so its interior
/// is reachable and everything it does not claim carries on behind it.
///
/// It was `Ignore` — not there at all — while the pane held nothing but the
/// painter's cells. Now the strip is a node inside it, and `Ignore` would hide
/// that node along with the pane.
///
/// A stack, because a pane is two things over one rectangle: the painter's
/// cells and the geometry that answers for them.
fn pane_inert<M: 'static>() -> Node<M> {
    stack().pointer_mode(PointerMode::Transparent)
}

/// One pane: the painter's `Host` leaf, with the pane's own geometry over it.
///
/// **A pane is its own host.** The body used to be a single `Host` that the
/// split renderer filled with every pane at once, laying them out a second
/// time from `SplitManager` — so the rectangle a pane was *painted* at and the
/// rectangle it was *clicked* at came from two engines that merely agreed.
/// Now the fold reaches one pane at a time and hands each the rectangle layout
/// gave it, and there is one answer to where a pane is.
///
/// The `Host` is under the interior rather than over it because the interior
/// paints nothing: it is the strip's gestures, the scrollbars' and the
/// content's, over cells that are still the painter's.
fn live_pane(id: LeafId, s: &Rc<Splits>) -> Node<UiMsg> {
    let chrome = s.chrome.get(&id).copied().unwrap_or_default();
    pane_inert::<UiMsg>().key(leaf_key(id)).children([
        host(super::frame::pane_host_id(id)),
        live_interior(id, chrome, s),
    ])
}

/// `s` is shared rather than cloned: a `layout_reader` outlives the build, so
/// every `Split` node needs its own handle on what the panes below it hold.
fn dressed(n: &SplitNode, s: &Rc<Splits>) -> Node<UiMsg> {
    match n {
        SplitNode::Leaf { split_id, .. } => live_pane(*split_id, s),
        SplitNode::Grouped { layout, .. } => dressed(layout, s),
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
            let s = s.clone();
            layout_reader(move |info: LayoutInfo| {
                let whole = ratatui::layout::Rect::new(
                    0,
                    0,
                    info.constraints.max_w,
                    info.constraints.max_h,
                );
                let (ra, rb) = split_rect_ext(whole, dir, ratio, ff, fs);
                let (first, second) = (dressed(&a, &s), dressed(&b, &s));
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

/// One divider: it takes the pointer for the whole drag, and it says when it
/// is hovered.
///
/// It paints nothing — the split renderer still draws the separator glyph and
/// its hover highlight, from `separator_areas`, which is itself a read of this
/// same layout.
fn divider(id: ContainerId, dir: SplitDirection) -> Node<UiMsg> {
    super::grip::draggable(
        super::msg::Grip::Separator,
        row(),
        Rc::new(move |e: &Event| {
            Some(UiMsg::Ui(UiFact::SeparatorPress {
                container: id,
                direction: dir,
                x: e.pos.x.max(0) as u16,
                y: e.pos.y.max(0) as u16,
            }))
        }),
    )
    // On the outside: the gesture node `draggable` returns is the one that
    // hit-tests, and the one `separator_areas` reads back.
    .key(divider_key(id))
    .on_enter(Rc::new(move |_: &Event| {
        Some(UiMsg::Ui(UiFact::SeparatorHover(Some((id, dir)))))
    }))
    .on_leave(Rc::new(move |_: &Event| {
        Some(UiMsg::Ui(UiFact::SeparatorHover(None)))
    }))
}

/// Every container the description could have put a divider node for.
///
/// Ids and directions only — both are model facts. Where each divider *is* is
/// layout's answer, read back by [`separator_rects`].
fn container_ids(n: &SplitNode, out: &mut Vec<(ContainerId, SplitDirection)>) {
    match n {
        SplitNode::Leaf { .. } => {}
        SplitNode::Grouped { layout, .. } => container_ids(layout, out),
        SplitNode::Split {
            direction,
            first,
            second,
            split_id,
            ..
        } => {
            out.push((*split_id, *direction));
            container_ids(first, out);
            container_ids(second, out);
        }
    }
}

/// Where each split separator is, read off the tree.
///
/// **This replaces a second layout.** `SplitManager::get_separators_with_ids`
/// walked the split tree computing rectangles from ratios and fixed extents —
/// the same arithmetic `node_of` hands to `split_rect_ext`, run again against
/// a rectangle the caller had to supply — and the grouped subtrees' separators
/// were not in it at all: the painter recorded those as it drew them and the
/// two lists were concatenated. Goal 5 allows one source of geometry.
///
/// A container with no divider element — the frame is maximized, or the pane
/// hosting the group is not visible — simply has no rectangle, and drops out.
/// That is the same answer the maximized early-return gave, derived instead of
/// stated.
pub fn separator_rects(
    ui: &fresh_ui::Ui<UiMsg>,
    s: &Splits,
    size: ratatui::layout::Rect,
) -> Vec<(ContainerId, SplitDirection, u16, u16, u16)> {
    let mut ids = Vec::new();
    container_ids(&s.root, &mut ids);
    // A pane showing a buffer group holds that group's own grid, whose
    // containers are nodes here like any other.
    for g in s.groups.values() {
        container_ids(g, &mut ids);
    }
    ids.into_iter()
        .filter_map(|(id, dir)| {
            let r = super::rect_of(ui, &divider_key(id), size)?;
            // The length runs along the separator: a horizontal split's line is
            // as wide as the box, a vertical one as tall.
            let length = match dir {
                SplitDirection::Horizontal => r.width,
                SplitDirection::Vertical => r.height,
            };
            Some((id, dir, r.x, r.y, length))
        })
        .collect()
}

/// A pane's interior with the parts that answer for themselves wired up.
///
/// The shape is `pane_interior`'s — one statement of it, which the model also
/// lays out with `M = ()` to derive rectangles. What the shell adds is the
/// strip's gestures.
fn live_interior(id: LeafId, c: PaneChrome, s: &Rc<Splits>) -> Node<UiMsg> {
    // A pane showing an active buffer group holds that group's own grid in its
    // content — laid out inside the pane's interior, past the strip and the
    // scrollbar column, which is exactly where the painter puts it. That was
    // the migration's standing boundary: the group's panels and dividers lived
    // in a side map, so their separators stayed recorded rectangles while the
    // main tree's became nodes. Nested here, they are the same nodes.
    let content = match s.groups.get(&id) {
        Some(g) => dressed(g, s),
        None => content_surface(id),
    };
    pane_interior(
        id,
        c,
        PaneSlots {
            tabs: tab_strip(id),
            controls: live_controls(id, s.controls),
            content,
            vscroll: scrollbar(id, Axis::Vertical),
            hscroll: scrollbar(id, Axis::Horizontal),
        },
    )
}

/// The control cluster with its two buttons answering for themselves.
///
/// Each button is a node that knows its pane, so the press carries no
/// coordinates at all — `handle_click_split_controls` opened by comparing the
/// cell against every pane's two recorded rectangles to recover exactly this.
/// The hover is the buttons' own too: `on_enter` / `on_leave` rather than a
/// third scan of the same lists.
fn live_controls(id: LeafId, c: PaneControls) -> Node<UiMsg> {
    let button = |n: Node<UiMsg>, target: HoverTarget, fact: UiFact| {
        let pressed = fact.clone();
        gesture(n)
            .on(
                GestureKind::Press,
                Rc::new(move |e: &Event| {
                    if e.button != MouseButton::Left {
                        return None;
                    }
                    e.stop();
                    Some(UiMsg::Ui(pressed.clone()))
                }),
            )
            .on_enter(Rc::new(move |_: &Event| {
                Some(UiMsg::Ui(UiFact::Hover(Some(target.clone()))))
            }))
            .on_leave(Rc::new(|_: &Event| Some(UiMsg::Ui(UiFact::Hover(None)))))
    };
    controls(
        id,
        c,
        button(
            row(),
            HoverTarget::MaximizeSplitButton(id),
            UiFact::PaneMaximize(id),
        ),
        button(
            row(),
            HoverTarget::CloseSplitButton(id),
            UiFact::PaneClose(id),
        ),
    )
}

/// A pane's content: where the text is, and where a click places the caret.
///
/// **The cells stay the painter's.** What the node supplies is which pane was
/// clicked and where, so the handlers behind it stop scanning every recorded
/// content rectangle to answer that. What they still take is the rectangle
/// itself, because click-to-byte is a projection through the view pipeline —
/// and that rectangle is this node's own, read back from the tree.
///
/// A **right** press is deliberately not claimed: it belongs to the base
/// surface's dismissal of the tab context menu, which is the only thing left
/// on the legacy walk that a right-click over a pane should reach.
fn content_surface(id: LeafId) -> Node<UiMsg> {
    let at = |e: &Event| (e.pos.x.max(0) as u16, e.pos.y.max(0) as u16);
    gesture(row())
        .on(
            GestureKind::Press,
            Rc::new(move |e: &Event| {
                if e.button != MouseButton::Left {
                    return None;
                }
                let (x, y) = at(e);
                e.stop();
                Some(UiMsg::Ui(UiFact::PaneContentPress {
                    pane: id,
                    x,
                    y,
                    clicks: e.clicks,
                    mods: e.mods,
                }))
            }),
        )
        .on(
            GestureKind::Wheel,
            Rc::new(move |e: &Event| {
                let (x, y) = at(e);
                e.stop();
                Some(UiMsg::Ui(pane_wheel(id, x, y, e.delta, e.axis)))
            }),
        )
}

/// The right-hand control cluster of a pane's strip: `[gap] > [□] [×] [trail]`.
///
/// **Message-agnostic, like the rest of the interior.** The cells are the tab
/// renderer's; what the nodes carry is where each button is and which pane it
/// belongs to. `close_split_areas` and `maximize_split_areas` were those two
/// facts recorded as rectangles and compared against a cell.
/// The two buttons are slots, on the same terms as [`PaneSlots`]: the model
/// asks for the shape with bare `row()`s to get the rectangles, and the shell
/// puts gestures in. The keys are applied here either way.
pub fn controls<M: 'static>(
    id: LeafId,
    c: PaneControls,
    maximize: Node<M>,
    close: Node<M>,
) -> Node<M> {
    if c.reserve() == 0 {
        return row().w(Sizing::Cells(0));
    }
    let one = Sizing::Cells(1);
    let mut cells: Vec<Node<M>> = vec![
        // The gap, then the `>` overflow slot — reserved whether or not the
        // tabs overflow, so the cluster does not shift as they scroll.
        row().w(one),
        row().w(one),
    ];
    if c.maximize {
        cells.push(maximize.key(maximize_key(id)).w(one));
    }
    if c.close {
        cells.push(close.key(close_key(id)).w(one));
    }
    // The trailing blank the painter leaves.
    cells.push(row().w(one));
    row().children(cells)
}

/// One of a pane's scrollbars.
///
/// **The pane is the node's; the bar's geometry stays recorded.** Where the
/// thumb is, and how wide the content is, are reads of the scroll state at
/// paint time — genuinely recorded. Which pane the pointer is over was *also*
/// recovered from a recorded rectangle, by asking every pane's bar in turn
/// whether it contained the point, and that is what the key replaces.
fn scrollbar(id: LeafId, axis: Axis) -> Node<UiMsg> {
    let at = |e: &Event| (e.pos.x.max(0) as u16, e.pos.y.max(0) as u16);
    let bar = gesture(row())
        .on(
            GestureKind::Press,
            Rc::new(move |e: &Event| {
                if e.button != MouseButton::Left {
                    return None;
                }
                let (x, y) = at(e);
                e.stop();
                Some(UiMsg::Ui(UiFact::PaneScrollbarPress {
                    pane: id,
                    axis,
                    x,
                    y,
                }))
            }),
        )
        .on(
            GestureKind::Wheel,
            Rc::new(move |e: &Event| {
                let (x, y) = at(e);
                e.stop();
                Some(UiMsg::Ui(pane_wheel(id, x, y, e.delta, e.axis)))
            }),
        );
    // Only the vertical bar names a hover target: it has a draggable thumb and
    // a track that pages, and the highlight follows the pointer between them.
    match axis {
        Axis::Horizontal => bar,
        Axis::Vertical => bar
            .on(
                GestureKind::Move,
                Rc::new(move |e: &Event| {
                    Some(UiMsg::Ui(UiFact::PaneScrollbarHover(Some((
                        id,
                        e.pos.y.max(0) as u16,
                    )))))
                }),
            )
            .on_enter(Rc::new(move |e: &Event| {
                Some(UiMsg::Ui(UiFact::PaneScrollbarHover(Some((
                    id,
                    e.pos.y.max(0) as u16,
                )))))
            }))
            .on_leave(Rc::new(move |_: &Event| {
                Some(UiMsg::Ui(UiFact::PaneScrollbarHover(None)))
            })),
    }
}

/// A wheel notch over a pane, by axis. One statement of it, because the
/// pane's parts each report the wheel and every one of them means the same
/// thing: move this pane's surface.
fn pane_wheel(id: LeafId, x: u16, y: u16, delta: i32, axis: Axis) -> UiFact {
    match axis {
        Axis::Vertical => UiFact::PaneWheel {
            pane: id,
            x,
            y,
            delta,
        },
        Axis::Horizontal => UiFact::PanePan { pane: id, delta },
    }
}

/// The tab strip, as one node per pane.
///
/// **The strip is the node; its interior is still the painter's.** The tabs,
/// the close buttons, the "+" and the scroll arrows are laid out by the tab
/// renderer and hit-tested against what it recorded, so what moves here is
/// *which pane's strip the pointer is on* — which the node knows because it is
/// that pane's — and the ordering that two `LayoutBox`es used to express by
/// their `z`: the split controls are drawn on top of the tab row, so they are
/// asked first.
fn tab_strip(id: LeafId) -> Node<UiMsg> {
    let at = |e: &Event| (e.pos.x.max(0) as u16, e.pos.y.max(0) as u16);
    gesture(row())
        .on(
            GestureKind::Press,
            Rc::new(move |e: &Event| {
                let (x, y) = at(e);
                e.stop();
                Some(UiMsg::Ui(match e.button {
                    MouseButton::Left => UiFact::PaneTabsPress { pane: id, x, y },
                    // Right-click on a tab raises its context menu. The clear
                    // half — a right-click anywhere else dismisses it — stays
                    // on the legacy walk's base surface, which this claim
                    // keeps out of the way of.
                    MouseButton::Right => UiFact::PaneTabsSecondary { pane: id, x, y },
                    _ => return None,
                }))
            }),
        )
        .on(
            GestureKind::Move,
            Rc::new(move |e: &Event| {
                let (x, y) = at(e);
                Some(UiMsg::Ui(UiFact::PaneTabsHover(Some((id, x, y)))))
            }),
        )
        .on_enter(Rc::new(move |e: &Event| {
            let (x, y) = at(e);
            Some(UiMsg::Ui(UiFact::PaneTabsHover(Some((id, x, y)))))
        }))
        .on_leave(Rc::new(move |_: &Event| {
            Some(UiMsg::Ui(UiFact::PaneTabsHover(None)))
        }))
        .on(
            GestureKind::Wheel,
            Rc::new(move |e: &Event| {
                let (x, y) = at(e);
                e.stop();
                // A wheel over a horizontal strip pans it, on either axis: up
                // and left walk toward the first tab, down and right toward
                // the last. The vertical one also dismisses the transient
                // popups and fires the plugin hook, as it did on the box.
                Some(UiMsg::Ui(match e.axis {
                    fresh_ui::Axis::Vertical => UiFact::PaneTabsWheel {
                        pane: id,
                        x,
                        y,
                        delta: e.delta,
                    },
                    fresh_ui::Axis::Horizontal => UiFact::PaneTabsPan {
                        pane: id,
                        delta: e.delta,
                    },
                }))
            }),
        )
}

/// **A right-click anywhere clears the transient tab menus**, then lets the
/// click go on to whatever it was aimed at.
///
/// Three of them: the "+" new-tab menu, the close-split confirmation, and a
/// tab's own context menu. None has right-click behaviour of its own, so a
/// right-click aimed past any of them should dismiss it the way clicking
/// elsewhere does — including the right-click that *opens* a tab's context
/// menu, which is aimed at a tab with the "+" menu possibly hanging over it.
///
/// A capture-phase listener that does not `stop()`: it runs before anything
/// under the pointer sees the click, and the click continues. So a right-press
/// on a tab clears all three here and the strip opens the new one after,
/// which is what the base surface's "clear unless it was a tab" fork was
/// spelling out as two branches of one statement.
///
/// It was a full-screen box in the legacy walk at the top of the z band — but
/// that walk runs only when the tree declines the event, so the guard silently
/// did not fire for a right-click any migrated surface took. Here it always
/// does.
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
/// The `□` / `⧉` button at the right end of a pane's strip.
pub fn maximize_key(id: LeafId) -> Key {
    Key::Pair("pane_maximize".into(), id.0 .0 as u64)
}
/// The `×` beside it.
pub fn close_key(id: LeafId) -> Key {
    Key::Pair("pane_close".into(), id.0 .0 as u64)
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
pub fn pane_interior<M: 'static>(id: LeafId, c: PaneChrome, s: PaneSlots<M>) -> Node<M> {
    let cells = |on: bool| Sizing::Cells(on as u16);
    col().children([
        // The strip is the tabs and, at its right end, the control cluster.
        // The tabs take what is left, which is the `tabs_rect.width - reserve`
        // the tab renderer is given.
        // The strip is the tabs and, at its right end, the control cluster.
        // The key is the row's, because the row is the strip: the tabs take
        // what the cluster leaves, which is the `tabs_rect.width - reserve`
        // the tab renderer is given.
        row()
            .key(tabs_key(id))
            .h(cells(c.tabs))
            .children([s.tabs.flex(1), s.controls]),
        row().flex(1).children([
            s.content.key(content_key(id)).flex(1),
            s.vscroll.key(vscroll_key(id)).w(cells(c.vscroll)),
        ]),
        row().h(cells(c.hscroll)).children([
            s.hscroll.key(hscroll_key(id)).flex(1),
            // The column the vertical bar occupies, kept clear.
            row().w(cells(c.vscroll)),
        ]),
    ])
}

/// What goes in each of a pane's four slots.
///
/// `row()` everywhere is the bare shape: only the rectangles are wanted, which
/// is how the model asks this description for them. The shell puts nodes with
/// gestures and children in instead. The keys are applied by `pane_interior`
/// either way, so a caller cannot forget one.
pub struct PaneSlots<M> {
    pub tabs: Node<M>,
    /// The right-hand control cluster, *inside* the strip. Its children carry
    /// the widths; this slot is whatever they come to.
    pub controls: Node<M>,
    pub content: Node<M>,
    pub vscroll: Node<M>,
    pub hscroll: Node<M>,
}

impl<M: 'static> Default for PaneSlots<M> {
    fn default() -> Self {
        Self {
            tabs: row(),
            controls: row(),
            content: row(),
            vscroll: row(),
            hscroll: row(),
        }
    }
}
