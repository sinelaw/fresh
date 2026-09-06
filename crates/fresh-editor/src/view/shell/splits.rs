//! The split grid, as a description.
//!
//! **One implementation of where the panes are.** `SplitNode` carried its own
//! layout engine — `get_leaves_with_rects` recursing over ratios and reserving
//! a cell per separator — and everything downstream was keyed on the
//! rectangles it produced: the separator drags, the per-pane scrollbars, the
//! tab strips, click-to-byte. That engine is this description now, laid out
//! once per frame as part of the shell tree, and where the panes are is read
//! off that layout (`view::shell::geometry::PaneRects`) — the model no longer
//! lays anything out to answer.
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
    col, gesture, layout_reader, row, stack, text, Axis, Event, GestureKind, Key, LayoutInfo,
    MouseButton, Node, PointerMode, Sizing,
};

use crate::app::shell_host::shell_theme::pair;
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
            // the same two facts `SplitManager::visible_leaves` and
            // `get_separators` state separately.
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
        use crate::view::shell::fold::{fold_band, Band, HostPainter, Paints};
        use crate::view::shell::frame::HostTarget;

        #[derive(Default)]
        struct Panes(Vec<(LeafId, Rect)>);
        impl HostPainter for Panes {
            fn paint_host(
                &mut self,
                target: HostTarget,
                rect: Rect,
                _buf: &mut ratatui::buffer::Buffer,
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
            None,
        );
        out.0
    }

    /// **The tree lays the grid out exactly as the model always did.**
    ///
    /// This is the swap's whole safety argument: the model's walk
    /// (`reference_leaves_with_rects`, the engine that was) is a layout
    /// engine, and it was replaced by one. The two share the rule
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
    /// two facts `SplitManager::visible_leaves` and `get_separators` state
    /// apart.
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
            active: None,
            chrome: [(host, with_tabs)].into_iter().collect(),
            controls: Default::default(),
            groups: [(host, group.clone())].into_iter().collect(),
            interiors: Default::default(),
            strips: Default::default(),
            hover: None,
            drop_zone: None,
            hosts: Default::default(),
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
                    active: None,
                    chrome: Default::default(),
                    controls: Default::default(),
                    groups: Default::default(),
                    interiors: Default::default(),
                    strips: Default::default(),
                    hover: None,
                    drop_zone: None,
                    hosts: Default::default(),
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
    /// The same two facts `SplitManager::visible_leaves` and the grid state
    /// when a pane is maximized: it is alone, and it is the whole box.
    #[test]
    fn a_maximized_pane_is_the_only_host_in_the_grid() {
        let root = split(SplitDirection::Vertical, leaf(0), leaf(1), 0.5, 10);
        let at = Rect::new(0, 0, 80, 24);
        let s = Splits {
            root,
            maximized: Some(LeafId(SplitId(1))),
            active: None,
            chrome: Default::default(),
            controls: Default::default(),
            groups: Default::default(),
            interiors: Default::default(),
            strips: Default::default(),
            hover: None,
            drop_zone: None,
            hosts: Default::default(),
        };
        assert_eq!(panes_folded(&s, at), vec![(LeafId(SplitId(1)), at)]);
    }

    /// **A group's panels are the hosts; the pane showing the group has
    /// none.**
    ///
    /// `expand_visible_buffers` lays a group out in its pane's *content*
    /// rectangle — past the strip and the scrollbar column — and paints one
    /// entry per inner leaf. Those entries are these leaves, each at its own
    /// content slot. The outer pane paints nothing of its own any more: its
    /// strip is nodes and its content is the group's grid, so there is no
    /// host for it to be.
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
            active: None,
            chrome: [(host_leaf, chrome)].into_iter().collect(),
            controls: Default::default(),
            groups: [(host_leaf, group)].into_iter().collect(),
            interiors: Default::default(),
            strips: Default::default(),
            hover: None,
            drop_zone: None,
            hosts: Default::default(),
        };

        let content = split_layout(host_leaf, at, chrome).content_rect;
        // An inner leaf with no chrome of its own is all content.
        let want: Vec<(LeafId, Rect)> = inner
            .reference_leaves_with_rects(content)
            .into_iter()
            .map(|(id, _, r)| (id, r))
            .collect();
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
                    active: None,
                    chrome: [(pane, chrome)].into_iter().collect(),
                    controls,
                    groups: Default::default(),
                    interiors: Default::default(),
                    strips: Default::default(),
                    hover: None,
                    drop_zone: None,
                    hosts: Default::default(),
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
    /// A pane's host id carries its tag: `LeafId`s are dense small integers,
    /// and an untagged small integer names no leaf — the fold's "this id
    /// names nothing" assertion means it.
    #[test]
    fn a_panes_host_id_round_trips_and_a_bare_number_names_nothing() {
        use crate::view::shell::frame::{pane_host_id, HostTarget};
        for n in [0usize, 1, 4, 7, 63, 4096] {
            let leaf = LeafId(SplitId(n));
            assert_eq!(
                HostTarget::from_host_id(pane_host_id(leaf)),
                Some(HostTarget::Pane(leaf)),
                "pane {n} round-trips"
            );
            assert_eq!(
                HostTarget::from_host_id(fresh_ui::HostId(n as u64)),
                None,
                "{n} bare is nobody's"
            );
        }
    }

    /// **A drop zone is a wash over the target pane's content, and the leaf
    /// under it is the same element.** A tab dragged to a pane's right edge
    /// washes the near half of its content in the drop zone's keys, bordered;
    /// the content slot is a stack of its own whether or not a zone is over
    /// it, so the leaf that took the drag's capture is the leaf the next
    /// frame mounts.
    #[test]
    fn a_drop_zone_washes_half_the_content_and_keeps_the_leaf() {
        use crate::app::types::TabDropZone;
        let leaf = LeafId(SplitId(1));
        let with = |zone: Option<TabDropZone>| Splits {
            root: SplitNode::leaf(BufferId(1), SplitId(1)),
            maximized: None,
            active: Some(leaf),
            chrome: Default::default(),
            controls: PaneControls {
                maximize: false,
                close: false,
            },
            groups: Default::default(),
            interiors: Default::default(),
            strips: Default::default(),
            hover: None,
            drop_zone: zone,
            hosts: Default::default(),
        };
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(overlay(&with(None)), Size::new(40, 10));
        let content = ui.find_by_key(&content_key(leaf)).expect("the content");
        let spec = ui.frame(
            overlay(&with(Some(TabDropZone::SplitRight(leaf)))),
            Size::new(40, 10),
        );
        let washes: Vec<_> = spec
            .items
            .iter()
            .filter(|i| i.draw == fresh_ui::Draw::Wash)
            .map(|i| (i.rect, i.theme.as_str().to_string()))
            .collect();
        assert_eq!(
            washes,
            vec![(
                fresh_ui::Rect::new(20, 0, 20, 10),
                crate::app::shell_host::shell_theme::attrs(
                    "ui.tab_drop_zone_border",
                    "ui.tab_drop_zone_bg",
                    &["bold"]
                )
            )],
            "the right half, in the zone's keys"
        );
        assert!(
            spec.items
                .iter()
                .any(|i| matches!(i.draw, fresh_ui::Draw::Border(_))
                    && i.rect == fresh_ui::Rect::new(20, 0, 20, 10)),
            "bordered"
        );
        assert_eq!(
            ui.find_by_key(&content_key(leaf)),
            Some(content),
            "the leaf is the same element under the zone"
        );
    }

    /// **A press on a pane's tab names that pane, because it is that pane's.**
    ///
    /// Two `LayoutBox`es covered the tab row — the strip at z 60 and the split
    /// controls at 70 — and both recovered the pane by comparing the cell
    /// against every recorded `bar_area` in turn (`tab_bar_split_at`), then
    /// the tab against the tab renderer's record. A tab is a node that knows
    /// its pane and its target; the press names both.
    #[test]
    fn a_press_on_a_tab_names_the_pane_it_belongs_to() {
        use crate::view::shell::tabs::{tab_key, Strip, Tab};
        use crate::view::split::TabTarget;
        use fresh_ui::{Input, Mods, Point};
        let root = split(SplitDirection::Vertical, leaf(0), leaf(1), 0.5, 10);
        let with_tabs = PaneChrome {
            tabs: true,
            vscroll: false,
            hscroll: false,
        };
        let strip = |n: usize| Strip {
            tabs: vec![Tab {
                target: TabTarget::Buffer(BufferId(n)),
                name: format!("file_{n}.rs"),
                modified: false,
                preview: false,
                binary: false,
            }],
            active: Some(TabTarget::Buffer(BufferId(n))),
            active_pane: n == 0,
            hover: None,
            offset: 0,
            preview_label: String::new(),
        };
        let s = Splits {
            root: root.clone(),
            maximized: None,
            active: None,
            chrome: [
                (LeafId(SplitId(0)), with_tabs),
                (LeafId(SplitId(1)), with_tabs),
            ]
            .into_iter()
            .collect(),
            controls: Default::default(),
            groups: Default::default(),
            interiors: Default::default(),
            strips: [
                (LeafId(SplitId(0)), strip(0)),
                (LeafId(SplitId(1)), strip(1)),
            ]
            .into_iter()
            .collect(),
            hover: None,
            drop_zone: None,
            hosts: Default::default(),
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
            let tab = ui.rect_of(
                ui.find_by_key(&tab_key(leaf_id, TabTarget::Buffer(BufferId(leaf_id.0 .0))))
                    .expect("its tab"),
            );
            assert_eq!(
                tab.x, want_x as i32,
                "the first tab starts at the strip's left edge"
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
            vec![UiFact::PaneTabPress {
                pane: LeafId(SplitId(0)),
                target: TabTarget::Buffer(BufferId(0)),
                x: 5,
                y: 0
            }],
            "the left strip"
        );
        // The release ends the press's capture before the next press.
        ui.dispatch(Input::release(
            Point::new(5, 0),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert_eq!(
            press(&mut ui, 45),
            vec![UiFact::PaneTabPress {
                pane: LeafId(SplitId(1)),
                target: TabTarget::Buffer(BufferId(1)),
                x: 45,
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
            active: None,
            chrome: [(LeafId(SplitId(0)), PaneChrome::default())]
                .into_iter()
                .collect(),
            controls: Default::default(),
            groups: Default::default(),
            interiors: Default::default(),
            strips: Default::default(),
            hover: None,
            drop_zone: None,
            hosts: Default::default(),
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
                // A fresh leaf with no rows settled answers its top.
                byte: Some(0),
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
// Not `PartialEq`, for the reason `Frame` is not: a pane can hold a mounted
// plugin panel, and a `WidgetSpec` is `Clone + Debug` and not comparable.
// Nothing compared two of these — the derive outlived its last caller.
#[derive(Clone, Debug)]
pub struct Splits {
    pub root: SplitNode,
    pub maximized: Option<LeafId>,
    /// The pane the keyboard belongs to when no panel or overlay holds it:
    /// its content is the base's marked focus holder (see
    /// [`content_leaf`]).
    pub active: Option<LeafId>,
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
    /// The described plugin panel each pane is showing, by the pane showing
    /// it.
    ///
    /// **A mounted panel is a subtree in the pane's content slot** — that is
    /// C.5, and this map is the whole of what the grid needs to know about it.
    /// The virtual buffer stays as a *text mirror* for search, copy and the
    /// `lines_changed` hooks; what it stops being is the rendering path.
    ///
    /// Absent for a pane showing a buffer, and absent for a mounted panel the
    /// adapter does not cover — a `WindowEmbed` panel keeps its painter whole,
    /// by the same `widgets::covered` gate the dock and the floating panel
    /// pass through. The painter is told the same thing, so a pane described
    /// here is a pane whose text pass does not run.
    pub interiors: std::collections::HashMap<LeafId, super::panel::Interior>,
    /// Each pane's tab strip, as content: its tabs, which is active, how far
    /// it is scrolled. A pane with a strip row and no entry here lays out an
    /// empty strip. See `shell::tabs`.
    pub strips: std::collections::HashMap<LeafId, super::tabs::Strip>,
    /// The shell's hover, for the strip's cluster: `□` and `×` read it to
    /// light up, as every other described button does.
    pub hover: Option<HoverTarget>,
    /// Where a dragged tab would land, while one is being dragged past the
    /// threshold: the target pane's content, or half of it, wears a wash and
    /// a border over the text (`drop_zone_node`).
    pub drop_zone: Option<crate::app::types::TabDropZone>,
    /// Each pane's leaf, by the pane: the handle the window keeps for as
    /// long as the pane exists, so every frame mounts the same object
    /// (`buffer_host::PaneHandle`). A pane with no handle here — an
    /// offscreen grid, a test — gets a fresh one, which is a leaf that
    /// answers no byte.
    pub hosts: std::collections::HashMap<LeafId, super::buffer_host::PaneHandle>,
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

/// Build the grid a second time, with each node's pointer role on it.
///
/// **`n` is unused.** The first statement is `let _ = n;`, and everything below
/// is rebuilt from `s.root`; [`overlay`] is the only caller and the
/// `grid::<UiMsg>(...)` it passes is dropped on the floor. The signature is a
/// leftover from the shape this was going to have — walk the built grid,
/// annotate it — which it cannot have: the grid is made of `layout_reader`s, so
/// it has no structure to walk until layout has run, and by then it is too late
/// to attach a gesture. Dressing is therefore a rebuild, not a pass over
/// something built, and nothing here reads its argument. Dropping the parameter
/// is a pure refactor with no other call site to update.
///
/// The split between the two recursions is still worth having, and it is not
/// what `n` is for: it keeps the description message-agnostic, because the
/// model lays the same grid out with `M = ()` and a gesture in `grid` would
/// make that impossible.
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

/// One pane: its interior, with the pane's content leaf in the content slot.
///
/// **A pane is its own host, and the host is its content.** The body used to
/// be a single `Host` that the split renderer filled with every pane at once,
/// laying them out a second time from `SplitManager` — so the rectangle a
/// pane was *painted* at and the rectangle it was *clicked* at came from two
/// engines that merely agreed. Then each pane carried a plain `Host` under
/// its interior, spanning strip, content and bars alike. Now the leaf is the
/// content slot itself (`buffer_host::BufferHost`): the fold hands the text
/// pipeline the content rectangle layout gave the leaf, the leaf answers the
/// byte under a cell, and the strip and the bars beside it are nodes of
/// their own.
fn live_pane(id: LeafId, s: &Rc<Splits>) -> Node<UiMsg> {
    let chrome = s.chrome.get(&id).copied().unwrap_or_default();
    pane_inert::<UiMsg>()
        .key(leaf_key(id))
        .child(live_interior(id, chrome, s))
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
                let lit = matches!(
                    s.hover,
                    Some(HoverTarget::SplitSeparator(c, d)) if c == id && d == dir
                );
                let div = divider(id, dir, lit);
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

/// One divider: its line, in the separator's colour or the hover colour
/// while the pointer is on it; it takes the pointer for the whole drag, and
/// it says when it is hovered.
///
/// **The line is the node's.** The split renderer used to draw the glyphs
/// and the editor repainted them in the hover colour from a record of where
/// the nodes were (`separator_areas`); the node draws its own line, and the
/// hover is a theme the description chooses, so there is nothing to record
/// and nothing to repaint.
fn divider(id: ContainerId, dir: SplitDirection, lit: bool) -> Node<UiMsg> {
    let ink = pair(
        if lit {
            "ui.split_separator_hover_fg"
        } else {
            "ui.split_separator_fg"
        },
        "editor.bg",
    );
    let line: Node<UiMsg> = match dir {
        SplitDirection::Horizontal => layout_reader(move |info: LayoutInfo| {
            text("─".repeat(usize::from(info.constraints.max_w))).theme(ink.clone())
        }),
        SplitDirection::Vertical => layout_reader(move |info: LayoutInfo| {
            col().children(
                (0..info.constraints.max_h)
                    .map(|_| text("│").theme(ink.clone()))
                    .collect::<Vec<_>>(),
            )
        }),
    };
    super::grip::draggable(
        super::msg::Grip::Separator,
        line,
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
    separator_rects_of(ui, &s.root, s.groups.values(), size)
}

/// [`separator_rects`] from the grid's model facts rather than a
/// description: the root and the active groups' grids.
pub fn separator_rects_of<'a>(
    ui: &fresh_ui::Ui<UiMsg>,
    root: &SplitNode,
    groups: impl IntoIterator<Item = &'a SplitNode>,
    size: ratatui::layout::Rect,
) -> Vec<(ContainerId, SplitDirection, u16, u16, u16)> {
    let mut ids = Vec::new();
    container_ids(root, &mut ids);
    // A pane showing a buffer group holds that group's own grid, whose
    // containers are nodes here like any other.
    for g in groups {
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
    // The pane's handle: its content leaf and its bars' leaves, kept by the
    // window for as long as the pane exists. A pane with none — an offscreen
    // grid, a test — gets a fresh one, whose leaves answer no byte and paint
    // no bar.
    let handle = s
        .hosts
        .get(&id)
        .cloned()
        .unwrap_or_else(|| super::buffer_host::PaneHandle::new(id));
    let content = match (s.groups.get(&id), s.interiors.get(&id)) {
        (Some(g), _) => dressed(g, s).key(content_key(id)),
        // **A described mounted panel replaces the content surface, it does
        // not sit on top of one.** The surface's whole job is to say which
        // pane was pressed and where, so a click can be turned into a caret
        // position through the view pipeline; a panel has no caret to place
        // and no byte to map to, and every one of its rows answers for itself.
        (None, Some(i)) => panel_content(id, i.clone(), s.active == Some(id)),
        (None, None) => content_leaf(id, handle.clone(), s.active == Some(id)),
    };
    // The content slot is a stack of its own — always, so the leaf's element
    // is the same whether or not something is over it — and a tab drag's
    // drop zone is the one thing over it.
    let zone = s
        .drop_zone
        .filter(|z| z.split_id() == id)
        .map(drop_zone_node);
    let content = stack().children(Some(content).into_iter().chain(zone));
    pane_interior(
        id,
        c,
        PaneSlots {
            tabs: tab_strip(id, s),
            // The cluster is laid out with the tabs, inside the strip's own
            // reader — it needs to know whether the tabs overflow to show
            // its `>`, and only the strip's layout knows.
            controls: row().w(Sizing::Cells(0)),
            content,
            vscroll: scrollbar(id, Axis::Vertical, &handle, &s.hover),
            hscroll: scrollbar(id, Axis::Horizontal, &handle, &s.hover),
        },
    )
}

/// One flank of a composed panel's paper-on-desk margin.
///
/// `[desk][paper]` on the left, `[paper][desk]` on the right — the layout
/// `render_compose_margins` paints for a composed *buffer*, said as two boxes
/// instead of two `Block`s. Nothing here is hittable: a press in the margin is
/// still a press on the pane, and the pane's own gesture is what should get it.
fn compose_margin(width: u16, height: u16, desk_first: bool) -> Node<UiMsg> {
    use crate::app::shell_host::shell_theme::Ink;
    let paper = width.min(1);
    let desk = width - paper;
    let edge = || {
        row()
            .w(Sizing::Cells(paper))
            .h(Sizing::Cells(height))
            .theme(super::widgets::pane_surface().to_string())
    };
    let felt = || {
        row()
            .w(Sizing::Cells(desk))
            .h(Sizing::Cells(height))
            .theme(Ink::keys("editor.fg", "ui.compose_margin_bg").to_string())
    };
    let flank = row()
        .w(Sizing::Cells(width))
        .h(Sizing::Cells(height))
        .pointer_mode(PointerMode::Ignore);
    if desk_first {
        flank.child(felt()).child(edge())
    } else {
        flank.child(edge()).child(felt())
    }
}

/// A mounted plugin panel, as the pane's content.
///
/// Laid out inside a `layout_reader` for both extents, and that is the point
/// rather than a convenience: **a pane's row budget is the rectangle it is
/// being given**, not a number the last paint recorded. The dock and the
/// floating panel know their inner height as state — it is the box they were
/// placed in — so they hand it down on the `Interior`; a pane's arrives here,
/// which is why [`super::panel::Interior::avail_height`] is `None` for one.
fn panel_content(id: LeafId, i: super::panel::Interior, active: bool) -> Node<UiMsg> {
    // Where the tree's focus rests when this is the active pane and no
    // widget of the panel holds it: on the panel itself, as the base's own
    // surface does. A widget the registry names is marked by `widgets::node`
    // (through `Ctx::keyboard`), and the innermost mark wins.
    let rests_here = active && !super::widgets::marks(&i.spec, &i.focus_key);
    let page = i.page.clone();
    let reading = i.reading;
    let compose = i.compose;
    let body = fresh_ui::layout_reader(move |info: fresh_ui::LayoutInfo| {
        // **The whole pane, not `widget_panel_width`'s.** The runtime lays a
        // mounted spec two columns short — "reserve 2 cols for gutter /
        // scrollbar / border padding the renderer adds", in its own words —
        // because the *painter* drew the panel's scrollbar outside the text it
        // had wrapped. A described panel's bar is its list's own, and a
        // viewport reserves the column itself, so taking those two again is
        // counting them twice: the bar would float two columns inboard with
        // dead space between it and the divider, which is what it did until
        // this line said otherwise.
        //
        // The dock's `DIVIDER_COLS` is not the counter-example it looks like.
        // There the two columns are *taken* — one by the painter's divider,
        // one by the slack it wraps against — and the description may not use
        // them. Here nothing takes them.
        let pane_w = info.constraints.max_w.max(1);
        let pane_h = info.constraints.max_h;
        // **A composed panel is a column in the pane, not the pane.** The
        // margins either side are the ones the buffer painter drew for a
        // composed buffer (`render_compose_margins`), described here because a
        // described panel never reaches that painter: desk outside, one column
        // of paper edge inside, the page between them. Without them the page
        // filled the pane and every width the plugin asked the host for — a
        // `flexSpacer`'s fill, a rule, a right-aligned control — was the
        // terminal's.
        //
        // **The column, less the one the plugin was told to hold back.**
        // `widget_panel_width` hands the plugin `composeWidth - 1`, because a
        // row filling the render area exactly wraps, and the page lays its
        // rows out to that number — so the node gets the same one, or a
        // right-aligned control comes out a column past where the plugin put
        // everything else. The held-back column joins the right margin, which
        // is where the composed buffer's painter left it too.
        let (left_pad, right_pad, inner_w) = match compose.map(|cw| cw.max(11)) {
            Some(cw) if cw < pane_w => {
                let inner = cw - 1;
                let left = (pane_w - cw) / 2;
                (left, pane_w - left - inner, inner)
            }
            _ => (0, 0, pane_w),
        };
        let widgets = super::widgets::node(
            &i.spec,
            inner_w,
            &super::widgets::Ctx {
                slot: super::widgets::Slot::Pane(id),
                states: &i.states,
                focus_key: i.focus_key.clone(),
                keyboard: active,
                hovered_key: i.hovered_key.clone(),
                marker_gutter: i.marker_gutter,
                hovered_item_key: i.hovered_item_key.clone(),
                hovered_popup_row: i.hovered_popup_row.clone(),
                // **A page's lists take their natural height**: the page is
                // the window, not each list. Every other panel's lists window
                // themselves to the pane.
                avail_height: match page {
                    Some(_) => None,
                    None => Some(info.constraints.max_h as u32),
                },
                scrollbar_reveal: i.scrollbar_reveal,
                // Not `panel_surface`: a mounted panel's rows were buffer
                // text on the editor's own ground. See `widgets::pane_surface`.
                surface: super::widgets::pane_surface(),
                markdown: i.markdown.as_ref().map(|m| m.ctx()),
            },
        )
        .w(Sizing::Cells(inner_w));
        let content = match &page {
            // **A page scrolls as a whole.** Its content is as tall as it
            // is, inside one viewport the pane sizes; the wheel, the bar and
            // the anchor's commands move the window, and nothing inside it
            // windows itself.
            Some(anchor) => fresh_ui::viewport(
                fresh_ui::row().children([
                    match reading {
                        // **A page's reader is drawn by the description**, because
                        // there is nothing else on screen that could: the pane
                        // shows this tree rather than the mirror buffer, so the
                        // mirror's cursor is a report and not a caret. A
                        // zero-width marker at the reader's cell, laid over the
                        // content and scrolled with it, is that caret — the same
                        // marker a focused field places, at coordinates the host
                        // owns instead of a byte the field owns.
                        Some((at_row, at_col)) => stack().children([
                            widgets,
                            // **Nothing here is hittable.** A stack's children all
                            // get the whole rect and the later one is hit first, so
                            // a caret layer that took hits would swallow every
                            // press on the page — which is how the reader gets to
                            // a control in the first place.
                            col()
                                .pointer_mode(PointerMode::Ignore)
                                .child(row().h(Sizing::Cells(at_row.min(u16::MAX as u32) as u16)))
                                .child(
                                    row()
                                        .h(Sizing::Cells(1))
                                        .child(row().w(Sizing::Cells(at_col)))
                                        .child(
                                            text("")
                                                .key(super::widgets::caret_key(
                                                    super::widgets::Slot::Pane(id),
                                                ))
                                                .w(Sizing::Cells(0))
                                                .h(Sizing::Cells(1))
                                                .cursor_byte(0),
                                        ),
                                ),
                        ]),
                        None => widgets,
                    }
                    .w(Sizing::Cells(inner_w)),
                    // **The window is wider than the page.** A viewport stretches
                    // its child to its own width, and the window reaches the
                    // pane's edge so its bar can hang there — so the page is
                    // packed to the left of a row that fills the window, and the
                    // slack beside it is the right margin showing through.
                    fresh_ui::row()
                        .w(Sizing::Cells(right_pad))
                        .pointer_mode(PointerMode::Ignore),
                ]),
            )
            .scrollbar()
            // **The same bar the pane draws**, in the same two colours: an
            // unthemed one falls back to the editor's own ground, which paints
            // the track in the background it sits on — a thumb floating on
            // nothing, with no track to click. See `splits::scrollbar`.
            .scrollbar_theme(pair("ui.scrollbar_thumb_fg", "ui.scrollbar_track_fg"))
            .anchor_to(anchor.clone())
            // **The bar hangs on the pane's edge, not the column's.** A
            // composed buffer's bar does, because the painter drew it in the
            // pane's own scrollbar column outside the margins; a viewport
            // draws its bar at its own right edge, so a window that stopped
            // where the text stops put the bar fifteen columns inboard with
            // the desk beside it. The window reaches the pane's edge instead
            // and the right margin is painted *under* it — which also leaves
            // the window's left edge where the text starts, so the row and
            // column a press lands on are still the page's own.
            .w(Sizing::Cells(inner_w.saturating_add(right_pad)))
            .h(Sizing::Flex(1)),
            // **The panel is the height of the pane, not of its content.** A
            // `Tree` or `List` the plugin left auto-sized (`visible_rows:
            // None`) is a `flex(1)` inside the panel's column, and flex
            // divides what is *left* of a bounded extent — a column that
            // sized itself to its content would hand the list every row it
            // asked for and let the pane clip the overflow, so the match list
            // would have no window to scroll and no bar to say how far
            // through it you are.
            None => widgets.h(Sizing::Flex(1)),
        };
        // **The pane's ground is the panel's too.** The buffer painter filled
        // the content rect with the editor's own background before it drew a
        // line of text; a described panel reaches no painter, so the rows it
        // does not cover were the terminal's default ground rather than the
        // theme's — invisible on a black terminal and wrong on any other.
        let ground = super::widgets::pane_surface().to_string();
        if left_pad == 0 && right_pad == 0 {
            return content.theme(ground);
        }
        // The margins are a layer of their own, under the content: the page's
        // window overlaps the right one so its bar can reach the pane's edge,
        // and a flank drawn beside the window instead would be the thing the
        // bar is drawn over.
        stack()
            .w(Sizing::Cells(pane_w))
            .h(Sizing::Flex(1))
            .theme(ground)
            .child(
                row()
                    .w(Sizing::Cells(pane_w))
                    .h(Sizing::Cells(pane_h))
                    .pointer_mode(PointerMode::Ignore)
                    .child(compose_margin(left_pad, pane_h, true))
                    .child(row().w(Sizing::Cells(inner_w)))
                    .child(compose_margin(right_pad, pane_h, false)),
            )
            .child(
                row()
                    .w(Sizing::Cells(pane_w))
                    .h(Sizing::Flex(1))
                    .child(row().w(Sizing::Cells(left_pad)))
                    .child(content),
            )
    });
    // **The press stays the pane's, unchanged.** The first instinct here was
    // that a panel has no byte to put a caret at, so a press only needed to
    // move the keyboard — and that is wrong, because the mirror still has
    // bytes and the plugin API depends on it. `git_log`'s own comment says so:
    // "Selection is cursor-driven (see the `cursor_moved` handler), so the
    // List's `select` event is ignored — a row click places the buffer cursor,
    // and `cursor_moved` mirrors it into the selection." Half a press left its
    // log clickable in the sense that the hit arrived, and dead in the sense
    // that nothing happened.
    //
    // So this is `content_leaf`'s press verbatim: the caret it places is
    // invisible now — the text pass does not run and the hardware caret comes
    // from the description's own marker — and that is exactly right. The
    // mirror is where a plugin reads a click's *line* from, and it goes on
    // being that.
    //
    // The **wheel** is deliberately not taken, where `content_leaf` takes
    // it: the panel's lists are viewports, and `fresh-ui` chains a notch into
    // one only when nothing claimed it. The dock learned this the same way.
    //
    // A **right** press is left alone for the reason it was before: it belongs
    // to the base surface's dismissal of the tab context menu.
    let pressed = gesture(body).on(
        GestureKind::Press,
        Rc::new(move |e: &Event| {
            if e.button != MouseButton::Left {
                return None;
            }
            e.stop();
            Some(UiMsg::Ui(UiFact::PaneContentPress {
                pane: id,
                // A panel has no byte to place a caret at.
                byte: None,
                x: e.pos.x.max(0) as u16,
                y: e.pos.y.max(0) as u16,
                clicks: e.clicks,
                mods: e.mods,
            }))
        }),
    );
    // **The panel's interior: its ring's root, its keymap, and the seam its
    // keys cross.** The same [`super::panel::interior`] the dock's is: the
    // content slot's key (`content_key`, which `interior_key(Slot::Pane)`
    // resolves to) names it, so the ring the host's focus advance walks is
    // read off the tree (`Ui::next_in`); the plugin's mode is a keymap on
    // the capture leg, so a key the mode binds is its action before any
    // widget sees it; Tab is declined to the tree's ring, confined to this
    // panel by the pane's keyboard layer (`frame::frame_tree`, `pane_keys`)
    // while the pane is active; and every other key is `PanelKey(Pane)`,
    // which the host hands to the editor's own keyboard — the mode's text
    // input and the rest of the buffer's route, as before.
    super::panel::interior(
        super::widgets::Slot::Pane(id),
        i.keymap.clone(),
        rests_here,
        pressed,
    )
}

/// The pane's content: the buffer's leaf, with the gestures a pane's
/// content answers and the focus the base rests on.
///
/// **A press names the byte, not the cell.** The leaf answers
/// `text_byte_at` from the rows its last text pass drew, so the press
/// carries the byte the caret goes to (`Event::text_byte`), and the press
/// captures the pointer: a selection drag is this gesture's own moves
/// (`PaneContentDrag`) and its release (`PaneContentRelease`), wherever the
/// pointer goes — the same mechanism a scrollbar thumb and a tab use, in
/// place of a drag flag the legacy walk ranked against nine others. The
/// cell still rides along for what is not the buffer's: a live terminal
/// grid's forwarding, the plugin hook, the gutter's fold toggle.
///
/// The leaf is the base's marked focus holder when this is the active pane
/// (`autofocus`): where the tree's focus rests when no chrome scope is up.
fn content_leaf(id: LeafId, handle: super::buffer_host::PaneHandle, active: bool) -> Node<UiMsg> {
    let at = |e: &Event| (e.pos.x.max(0) as u16, e.pos.y.max(0) as u16);
    let surface = gesture(handle.node())
        .on(
            GestureKind::Press,
            Rc::new(move |e: &Event| {
                if e.button != MouseButton::Left {
                    return None;
                }
                let (x, y) = at(e);
                e.capture_pointer();
                e.stop();
                Some(UiMsg::Ui(UiFact::PaneContentPress {
                    pane: id,
                    byte: e.text_byte,
                    x,
                    y,
                    clicks: e.clicks,
                    mods: e.mods,
                }))
            }),
        )
        .on(
            GestureKind::Move,
            Rc::new(move |e: &Event| {
                // A move is the drag only while the leaf holds the pointer
                // its press took; bare motion across the text changes
                // nothing anybody draws and says nothing.
                if !e.captured {
                    return None;
                }
                let (x, y) = at(e);
                Some(UiMsg::Ui(UiFact::PaneContentDrag { pane: id, x, y }))
            }),
        )
        .on(
            GestureKind::Release,
            Rc::new(move |e: &Event| {
                if e.button != MouseButton::Left {
                    return None;
                }
                Some(UiMsg::Ui(UiFact::PaneContentRelease { pane: id }))
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
    // **The leaf's keyboard.** Every key that reaches the content while it
    // holds focus is the editor's — Tab included, which is the buffer's
    // indent and never the ring's traversal — and the leaf claims it so no
    // pipeline behind the tree has to be the default owner of a key nobody
    // took (design §3.7.5).
    let n = fresh_ui::focusable(surface)
        .key(content_key(id))
        .skip_traversal()
        .on_key(move |e: &Event| {
            e.stop();
            Some(UiMsg::Ui(UiFact::PaneKey { pane: id }))
        });
    // **The pane's context is the leaf's settled fact.** A terminal taking
    // the keyboard raw and a composite buffer resolve their keys in their own
    // sections; the chain names the pane's content, and `get_key_context`
    // asks the pane's handle which (`PaneHandle::context`), as the PTY gate
    // asks it for raw input. Not a node above the leaf: the leaf's element
    // must be the same whatever mode it is in, so a drag that parks a live
    // terminal in scroll-back keeps the capture it took.
    match active {
        true => n.autofocus(),
        false => n,
    }
}

/// One of a pane's scrollbars: the bar's leaf, with the gestures a bar
/// answers.
///
/// **The bar is the tree's, thumb and all.** The leaf paints an ordinary
/// `Draw::Scrollbar` from the facts the editor settled before the frame
/// (`Editor::settle_pane_bars`), sized to the track layout gives it — where
/// the painter used to draw the bar into the pane's cells and file the
/// thumb's extent for the press to read back. A press reads the thumb from
/// the same facts and the same arithmetic (`Draw::scrollbar_thumb`), so the
/// two cannot disagree. The thumb lights under the pointer through the
/// bar's theme; a track cell under it is a mark on the facts.
fn scrollbar(
    id: LeafId,
    axis: Axis,
    handle: &super::buffer_host::PaneHandle,
    hover: &Option<HoverTarget>,
) -> Node<UiMsg> {
    let at = |e: &Event| (e.pos.x.max(0) as u16, e.pos.y.max(0) as u16);
    let thumb_lit = matches!(hover, Some(HoverTarget::ScrollbarThumb(p)) if *p == id);
    let theme = pair(
        if thumb_lit {
            "ui.scrollbar_thumb_hover_fg"
        } else {
            "ui.scrollbar_thumb_fg"
        },
        "ui.scrollbar_track_fg",
    );
    let bar = gesture(handle.bar_node(axis).theme(theme))
        .on(
            GestureKind::Press,
            Rc::new(move |e: &Event| {
                if e.button != MouseButton::Left {
                    return None;
                }
                let (x, y) = at(e);
                // **The whole drag mechanism, in one call** — the same one
                // `grip::draggable` makes. A thumb drag leaves the bar's own
                // column on its first step, and capture is what keeps the
                // moves and the release coming back here instead of being
                // routed by a flag the ladder read on every event.
                e.capture_pointer();
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
            GestureKind::Release,
            Rc::new(move |e: &Event| {
                e.stop();
                Some(UiMsg::Ui(UiFact::PaneScrollbarRelease { pane: id, axis }))
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
    // **One `Move`, two meanings, and the state says which.** A captured bar
    // is being dragged; an uncaptured one is being hovered, and the vertical
    // bar's highlight follows the pointer between its thumb and its track.
    // Emitting one fact and letting the applier read the drag flag is the
    // shape `GripDrag` already has, and it is why the bar does not need to
    // know whether it holds the pointer.
    let bar = bar.on(
        GestureKind::Move,
        Rc::new(move |e: &Event| {
            let (x, y) = at(e);
            Some(UiMsg::Ui(UiFact::PaneScrollbarDrag {
                pane: id,
                axis,
                x,
                y,
            }))
        }),
    );
    // Only the vertical bar names a hover target: it has a draggable thumb and
    // a track that pages, and the highlight follows the pointer between them.
    match axis {
        Axis::Horizontal => bar,
        Axis::Vertical => bar
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

/// The tab strip, as one node per pane: its tabs, described
/// (`shell::tabs`), under the strip's own wheel.
///
/// The tabs, their close buttons, the `+`, the arrows and the cluster are
/// each a node that answers its own press and reports its own hover. What
/// the strip itself answers is the wheel — a notch anywhere on the row pans
/// the tabs — and a press on its ground, which stops here so that the buffer
/// beneath never sees a click aimed at the bar between two tabs.
fn tab_strip(id: LeafId, s: &Rc<Splits>) -> Node<UiMsg> {
    let at = |e: &Event| (e.pos.x.max(0) as u16, e.pos.y.max(0) as u16);
    let strip = s.strips.get(&id).cloned().unwrap_or_default();
    let cluster = super::tabs::Cluster {
        controls: s.controls,
        maximized: s.maximized.is_some(),
        hover_maximize: s.hover == Some(HoverTarget::MaximizeSplitButton(id)),
        hover_close: s.hover == Some(HoverTarget::CloseSplitButton(id)),
    };
    gesture(super::tabs::strip(id, &strip, cluster))
        .on(
            GestureKind::Press,
            Rc::new(|e: &Event| {
                // The ground between the tabs: a left press is spent here,
                // and a right one raises no menu and stays available to the
                // base surface's clear.
                if e.button == MouseButton::Left {
                    e.stop();
                }
                None
            }),
        )
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
/// Where a dragged tab would land, over the target pane's content: the
/// whole content for a strip or centre drop, the near half for an edge drop
/// — the same halves the painter carved, with the same floors — as a
/// bordered box whose ground is a wash, so the text under it stays readable
/// through the highlight.
fn drop_zone_node(zone: crate::app::types::TabDropZone) -> Node<UiMsg> {
    use crate::app::shell_host::shell_theme::attrs;
    use crate::app::types::TabDropZone as Z;
    let zone_box = || {
        col()
            .theme(attrs(
                "ui.tab_drop_zone_border",
                "ui.tab_drop_zone_bg",
                &["bold"],
            ))
            .wash()
            .border()
    };
    match zone {
        Z::TabBar(..) | Z::SplitCenter(_) => zone_box(),
        Z::SplitLeft(_) => row().children([zone_box().w(Sizing::Pct(50)).min_w(3), row().flex(1)]),
        Z::SplitRight(_) => row().children([row().flex(1), zone_box().w(Sizing::Pct(50)).min_w(3)]),
        Z::SplitTop(_) => col().children([zone_box().h(Sizing::Pct(50)).min_h(2), col().flex(1)]),
        Z::SplitBottom(_) => {
            col().children([col().flex(1), zone_box().h(Sizing::Pct(50)).min_h(2)])
        }
    }
}

pub fn content_key(id: LeafId) -> Key {
    Key::Pair("pane_content".into(), id.0 .0 as u64)
}

/// The pane a content key names, if `k` is one.
pub fn pane_of_content_key(k: &Key) -> Option<LeafId> {
    match k {
        Key::Pair(name, n) if &**name == "pane_content" => {
            Some(LeafId(fresh_core::SplitId(*n as usize)))
        }
        _ => None,
    }
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
            // The content names itself (`content_key`): a leaf's context
            // is a keyed node *above* it on the chain (`content_leaf`), and
            // a key applied here would sit on that node and take its name.
            s.content.flex(1),
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
/// gestures and children in instead. The strip's and the bars' keys are
/// applied by `pane_interior` either way, so a caller cannot forget one; the
/// content carries its own (`content_key`), because what a caller puts there
/// may be a keyed chain — a pane's context above its leaf — and one key on
/// the slot would rename its top.
pub struct PaneSlots<M> {
    pub tabs: Node<M>,
    /// The right-hand control cluster, *inside* the strip. Its children carry
    /// the widths; this slot is whatever they come to.
    pub controls: Node<M>,
    pub content: Node<M>,
    pub vscroll: Node<M>,
    pub hscroll: Node<M>,
}

impl<M: 'static> PaneSlots<M> {
    /// The bare shape: rectangles only, for the model that lays a pane out
    /// to read them back. The content row is keyed here because the content
    /// names itself in the shell's tree, and the model's must answer to the
    /// same name.
    pub fn bare(id: LeafId) -> Self {
        Self {
            tabs: row(),
            controls: row(),
            content: row().key(content_key(id)),
            vscroll: row(),
            hscroll: row(),
        }
    }
}
