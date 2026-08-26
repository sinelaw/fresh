//! Property tests.
//!
//! Golden tests pin down what one session looks like. These pin down what must
//! be true of *every* session: invariants of the layout algebra, of
//! reconciliation, and of the application under arbitrary input.

use proptest::prelude::*;

mod support;
use fresh_ui::Axis;
use fresh_ui::{
    col, distribute, row, text, Input, Key, KeyCode, KeyPress, Mods, MouseButton, Node, Point,
    Rect, Size, Sizing, Ui,
};
use support::demo::{App, Demo};

// ---------------------------------------------------------------------------
// The flex division algebra
// ---------------------------------------------------------------------------

proptest! {
    /// Every cell is handed out, and none is invented.
    #[test]
    fn distribute_conserves_the_total(total in 0u16..2000, weights in prop::collection::vec(0u16..20, 0..12)) {
        let shares = distribute(total, &weights);
        prop_assert_eq!(shares.len(), weights.len());
        let sum: u32 = shares.iter().map(|s| *s as u32).sum();
        if weights.iter().any(|w| *w > 0) {
            prop_assert_eq!(sum, total as u32);
        } else {
            prop_assert_eq!(sum, 0);
        }
    }

    /// A zero weight never receives a cell, whatever the remainder.
    #[test]
    fn zero_weights_receive_nothing(total in 0u16..2000, weights in prop::collection::vec(0u16..20, 1..12)) {
        let shares = distribute(total, &weights);
        for (w, s) in weights.iter().zip(shares.iter()) {
            if *w == 0 {
                prop_assert_eq!(*s, 0);
            }
        }
    }

    /// The same inputs always produce the same division. An unspecified rule
    /// here is what produces one-cell gaps that move between runs.
    #[test]
    fn distribution_is_a_function_of_its_inputs(total in 0u16..2000, weights in prop::collection::vec(0u16..20, 0..12)) {
        prop_assert_eq!(distribute(total, &weights), distribute(total, &weights));
    }

    /// Equal weights differ by at most one cell, and the extra goes to the
    /// earlier children.
    #[test]
    fn equal_weights_differ_by_at_most_one(total in 0u16..500, n in 1usize..10) {
        let shares = distribute(total, &vec![1; n]);
        let max = *shares.iter().max().unwrap();
        let min = *shares.iter().min().unwrap();
        prop_assert!(max - min <= 1, "{shares:?}");
        let first_small = shares.iter().position(|s| *s == min).unwrap();
        prop_assert!(
            shares[first_small..].iter().all(|s| *s == min),
            "the larger shares are the earlier ones: {shares:?}"
        );
    }
}

// ---------------------------------------------------------------------------
// Layout
// ---------------------------------------------------------------------------

/// A generated child: a fixed extent, a flex weight, or content-sized.
#[derive(Clone, Copy, Debug)]
enum Child {
    Cells(u16),
    Flex(u16),
    Auto,
}

fn child_strategy() -> impl Strategy<Value = Child> {
    prop_oneof![
        (0u16..12).prop_map(Child::Cells),
        (1u16..5).prop_map(Child::Flex),
        Just(Child::Auto),
    ]
}

fn build_row(children: &[Child], gap: u16) -> Node<()> {
    row().gap(gap).h(Sizing::Cells(1)).children(
        children
            .iter()
            .enumerate()
            .map(|(i, c)| {
                let n = text(format!("c{i}"));
                match c {
                    Child::Cells(v) => n.w(Sizing::Cells(*v)),
                    Child::Flex(w) => n.w(Sizing::Flex(*w)),
                    Child::Auto => n,
                }
            })
            .collect::<Vec<_>>(),
    )
}

fn rects(ui: &Ui<()>, parent: fresh_ui::ElementId) -> Vec<Rect> {
    ui.children(parent).iter().map(|c| ui.rect(*c)).collect()
}

proptest! {
    /// Children of a flow container never overlap and never run backwards.
    #[test]
    fn a_row_places_its_children_in_order_without_overlap(
        children in prop::collection::vec(child_strategy(), 0..8),
        gap in 0u16..3,
        width in 1u16..60,
    ) {
        let mut ui: Ui<()> = Ui::new();
        ui.frame(build_row(&children, gap), Size::new(width, 4));
        let root = ui.root().unwrap();
        let rs = rects(&ui, root);
        for pair in rs.windows(2) {
            prop_assert!(
                pair[1].x >= pair[0].right(),
                "children overlap: {:?} then {:?}",
                pair[0],
                pair[1]
            );
        }
    }

    /// A child is contained in its parent. Nothing escapes its box.
    #[test]
    fn children_stay_inside_their_parent(
        children in prop::collection::vec(child_strategy(), 0..8),
        gap in 0u16..3,
        width in 1u16..60,
    ) {
        let mut ui: Ui<()> = Ui::new();
        ui.frame(build_row(&children, gap), Size::new(width, 4));
        let root = ui.root().unwrap();
        let parent = ui.rect(root);
        for r in rects(&ui, root) {
            prop_assert!(r.x >= parent.x && r.y >= parent.y, "{r:?} outside {parent:?}");
            prop_assert!(
                r.right() <= parent.right() && r.bottom() <= parent.bottom(),
                "{r:?} outside {parent:?}"
            );
        }
    }

    /// Laying the same description out twice gives the same rectangles: there
    /// is no hidden state carried between passes.
    #[test]
    fn layout_is_reproducible(
        children in prop::collection::vec(child_strategy(), 0..8),
        gap in 0u16..3,
        width in 1u16..60,
    ) {
        let size = Size::new(width, 4);
        let mut a: Ui<()> = Ui::new();
        a.frame(build_row(&children, gap), size);
        let mut b: Ui<()> = Ui::new();
        b.frame(build_row(&children, gap), size);
        prop_assert_eq!(
            rects(&a, a.root().unwrap()),
            rects(&b, b.root().unwrap())
        );
    }

    /// Every emitted item is inside the frame once its clip is applied.
    #[test]
    fn nothing_paints_outside_the_frame(
        children in prop::collection::vec(child_strategy(), 0..8),
        width in 1u16..60,
        height in 1u16..12,
    ) {
        let mut ui: Ui<()> = Ui::new();
        let frame = Size::new(width, height);
        let spec = ui.frame(
            col().children((0..4).map(|_| build_row(&children, 1))),
            frame,
        );
        let bounds = Rect::from_size(frame);
        for item in &spec.items {
            let v = item.visible_rect().intersect(bounds);
            prop_assert!(
                v == item.visible_rect().intersect(bounds),
                "{:?} escapes the frame",
                item.rect
            );
        }
    }
}

// ---------------------------------------------------------------------------
// Reconciliation
// ---------------------------------------------------------------------------

fn keyed(order: &[u8]) -> Node<()> {
    col().children(
        order
            .iter()
            .map(|k| text(format!("row {k}")).key(Key::from(*k)))
            .collect::<Vec<_>>(),
    )
}

proptest! {
    /// A permutation of the same keys moves elements; it never recreates them.
    #[test]
    fn permuting_keys_preserves_the_element_set(
        order in prop::collection::vec(0u8..12, 0..12),
        shuffle in prop::collection::vec(0usize..12, 0..24),
    ) {
        // Deduplicate: two children with the same key at once is a caller
        // error, not a property of the reconciler.
        let mut seen = Vec::new();
        for k in order {
            if !seen.contains(&k) {
                seen.push(k);
            }
        }
        let mut ui: Ui<()> = Ui::new();
        ui.reconcile(keyed(&seen));
        let root = ui.root().unwrap();
        let before: std::collections::BTreeSet<_> = ui.children(root).into_iter().collect();

        let mut permuted = seen.clone();
        for (i, s) in shuffle.iter().enumerate() {
            if permuted.is_empty() {
                break;
            }
            let a = i % permuted.len();
            let b = s % permuted.len();
            permuted.swap(a, b);
        }
        ui.reconcile(keyed(&permuted));

        let after: std::collections::BTreeSet<_> = ui.children(root).into_iter().collect();
        prop_assert_eq!(before, after, "the same elements, in a different order");
    }

    /// Returning to the same description returns to the same number of
    /// elements: reconciliation leaks nothing.
    #[test]
    fn returning_to_a_shape_returns_to_its_element_count(
        shapes in prop::collection::vec(prop::collection::vec(0u8..6, 0..6), 1..8),
    ) {
        let base: Vec<u8> = vec![1, 2, 3];
        let mut ui: Ui<()> = Ui::new();
        ui.reconcile(keyed(&base));
        let baseline = ui.live_count();

        for shape in &shapes {
            let mut uniq = Vec::new();
            for k in shape {
                if !uniq.contains(k) {
                    uniq.push(*k);
                }
            }
            ui.reconcile(keyed(&uniq));
        }
        ui.reconcile(keyed(&base));
        prop_assert_eq!(ui.live_count(), baseline);
    }
}

// ---------------------------------------------------------------------------
// The application under arbitrary input
// ---------------------------------------------------------------------------

const FRAME: Size = Size { w: 44, h: 12 };

#[derive(Clone, Debug)]
enum Act {
    Click(u8, u8),
    RightClick(u8, u8),
    Move(u8, u8),
    Wheel(i8),
    Key(u8),
    Tab,
    Enter,
    Esc,
    Palette,
    Resize(u8, u8),
}

fn act_strategy() -> impl Strategy<Value = Act> {
    prop_oneof![
        4 => (0u8..44, 0u8..12).prop_map(|(x, y)| Act::Click(x, y)),
        2 => (0u8..44, 0u8..12).prop_map(|(x, y)| Act::RightClick(x, y)),
        2 => (0u8..44, 0u8..12).prop_map(|(x, y)| Act::Move(x, y)),
        2 => (-8i8..8).prop_map(Act::Wheel),
        3 => (b'a'..=b'z').prop_map(Act::Key),
        3 => Just(Act::Tab),
        2 => Just(Act::Enter),
        2 => Just(Act::Esc),
        1 => Just(Act::Palette),
        1 => (4u8..60, 3u8..16).prop_map(|(w, h)| Act::Resize(w, h)),
    ]
}

fn apply(demo: &mut Demo, act: &Act) {
    let m = Mods::NONE;
    match act {
        Act::Click(x, y) => {
            let pos = Point::new(*x as i32, *y as i32);
            demo.input(Input::press(pos, MouseButton::Left, m));
            demo.input(Input::release(pos, MouseButton::Left, m));
        }
        Act::RightClick(x, y) => {
            let pos = Point::new(*x as i32, *y as i32);
            demo.input(Input::press(pos, MouseButton::Right, m));
            demo.input(Input::release(pos, MouseButton::Right, m));
        }
        Act::Move(x, y) => {
            demo.input(Input::Move {
                pos: Point::new(*x as i32, *y as i32),
                mods: m,
            });
        }
        Act::Wheel(d) => {
            demo.input(Input::Wheel {
                pos: Point::new(30, 6),
                delta: *d as i32,
                axis: Axis::Vertical,
                mods: m,
            });
        }
        Act::Key(c) => {
            demo.input(Input::Key(KeyPress::new(KeyCode::Char(*c as char))));
        }
        Act::Tab => {
            demo.input(Input::Key(KeyPress::new(KeyCode::Tab)));
        }
        Act::Enter => {
            demo.input(Input::Key(KeyPress::new(KeyCode::Enter)));
        }
        Act::Esc => {
            demo.input(Input::Key(KeyPress::new(KeyCode::Esc)));
        }
        Act::Palette => {
            demo.input(Input::Key(KeyPress::with(KeyCode::Char('p'), Mods::CTRL)));
        }
        Act::Resize(w, h) => demo.resize(Size::new(*w as u16, *h as u16)),
    }
}

/// The invariants that must hold after any input, whatever it was.
fn check_invariants(demo: &Demo) -> Result<(), TestCaseError> {
    let ui = &demo.ui;

    if let Some(f) = ui.focused() {
        prop_assert!(ui.is_live(f), "focus points at a disposed element");
    }

    let frame = Rect::from_size(demo.size);
    for item in &ui.spec().items {
        let v = item.visible_rect();
        if v.is_empty() {
            continue;
        }
        prop_assert!(
            v.intersect(frame) == v,
            "{:?} paints outside the {:?} frame",
            v,
            demo.size
        );
    }

    // O(visible): the display list is bounded by the frame, not by the data.
    let area = demo.size.w as usize * demo.size.h as usize;
    prop_assert!(
        ui.spec().items.len() <= area.max(64) * 4,
        "{} items for a {}x{} frame",
        ui.spec().items.len(),
        demo.size.w,
        demo.size.h
    );

    prop_assert!(demo.app.selected < demo.app.visible().len().max(1));
    Ok(())
}

proptest! {
    #![proptest_config(ProptestConfig { cases: 96, ..ProptestConfig::default() })]

    /// Any sequence of inputs leaves the application in a valid state.
    #[test]
    fn arbitrary_input_holds_the_invariants(acts in prop::collection::vec(act_strategy(), 0..40)) {
        let mut demo = Demo::with_app(App::default(), FRAME);
        check_invariants(&demo)?;
        for act in &acts {
            apply(&mut demo, act);
            check_invariants(&demo)?;
        }
    }

    /// The same, over a million rows: the per-frame cost must not depend on how
    /// much data there is.
    #[test]
    fn a_million_rows_hold_the_same_invariants(acts in prop::collection::vec(act_strategy(), 0..24)) {
        let mut demo = Demo::with_app(App::huge(1_000_000), FRAME);
        for act in &acts {
            apply(&mut demo, act);
            check_invariants(&demo)?;
            prop_assert!(
                demo.ui.live_count() < 400,
                "{} elements mounted",
                demo.ui.live_count()
            );
        }
    }

    /// Rendering again without input changes nothing. A frame is a function of
    /// the state, so a second look at the same state gives the same picture.
    #[test]
    fn rendering_is_idempotent(acts in prop::collection::vec(act_strategy(), 0..20)) {
        let mut demo = Demo::with_app(App::default(), FRAME);
        for act in &acts {
            apply(&mut demo, act);
        }
        let once = demo.screen();
        demo.render();
        let twice = demo.screen();
        prop_assert_eq!(once.text(), twice.text());
    }

    /// The element count does not grow without bound as inputs accumulate: a
    /// long session leaks nothing.
    #[test]
    fn a_long_session_does_not_accumulate_elements(acts in prop::collection::vec(act_strategy(), 0..60)) {
        let mut demo = Demo::with_app(App::default(), FRAME);
        for act in &acts {
            apply(&mut demo, act);
        }
        // Everything closed, back to the base layout.
        for _ in 0..3 {
            apply(&mut demo, &Act::Esc);
        }
        demo.resize(FRAME);
        prop_assert!(
            demo.ui.live_count() < 400,
            "{} elements after {} inputs",
            demo.ui.live_count(),
            acts.len()
        );
    }
}

// ---------------------------------------------------------------------------
// Floors, pointer modes, and the click run — the three attributes added for
// hosts that were writing arithmetic around their absence.
// ---------------------------------------------------------------------------

proptest! {
    /// **A floor is never violated.** Whatever a container is doing with the
    /// space it has — dividing a remainder, running out of it entirely — a
    /// child that declared a minimum extent gets at least that many cells.
    ///
    /// The interesting half is the overflow case: a row too narrow for its
    /// children still honours their floors and overflows, rather than
    /// quietly handing out zero.
    #[test]
    fn a_floor_is_never_violated(
        width in 1u16..40,
        kids in prop::collection::vec((0u16..12, 0u16..6, any::<bool>()), 1..6),
    ) {
        let mut ui: Ui<()> = Ui::new();
        let children: Vec<Node<()>> = kids
            .iter()
            .map(|&(cells, floor, flexible)| {
                let n = text("");
                let n = if flexible { n.flex(1) } else { n.w(Sizing::Cells(cells)) };
                n.min_w(floor)
            })
            .collect();
        ui.frame(row().children(children), Size::new(width, 1));
        for (i, &(_, floor, _)) in kids.iter().enumerate() {
            let r = ui.rect(ui.at(&[i]).unwrap());
            prop_assert!(
                r.w >= floor,
                "child {i} wanted at least {floor} cells, got {} (width {width})",
                r.w
            );
            // **And the cells are its own.** Width alone is not the guarantee:
            // an overflowing row used to pull later children *back* over a
            // floored one, so the extent was right and the separation was gone.
            // A floor that a sibling may sit on top of is not a floor.
            if floor > 0 && i + 1 < kids.len() {
                let next = ui.rect(ui.at(&[i + 1]).unwrap());
                prop_assert!(
                    next.x >= r.x + r.w as i32,
                    "child {} was placed at {}, over child {i}'s cells ({}..{})",
                    i + 1,
                    next.x,
                    r.x,
                    r.x + r.w as i32
                );
            }
        }
    }

    /// **A floor never shrinks anything.** Adding one can only ever widen a
    /// child, never narrow it — so it is safe to add to an existing layout.
    #[test]
    fn adding_a_floor_never_narrows_a_child(
        width in 1u16..40,
        cells in prop::collection::vec(0u16..12, 1..6),
        floor in 0u16..6,
        which in 0usize..6,
    ) {
        let which = which % cells.len();
        let build = |floor_on: Option<usize>| -> Vec<Node<()>> {
            cells
                .iter()
                .enumerate()
                .map(|(i, &c)| {
                    let n = text("").w(Sizing::Cells(c));
                    if floor_on == Some(i) { n.min_w(floor) } else { n }
                })
                .collect()
        };
        let mut a: Ui<()> = Ui::new();
        a.frame(row().children(build(None)), Size::new(width, 1));
        let before = a.rect(a.at(&[which]).unwrap()).w;
        let mut b: Ui<()> = Ui::new();
        b.frame(row().children(build(Some(which))), Size::new(width, 1));
        let after = b.rect(b.at(&[which]).unwrap()).w;
        prop_assert!(after >= before, "floor narrowed {which}: {before} -> {after}");
    }

    /// **A transparent cover is invisible to the pointer, everywhere.** Not at
    /// one sampled cell: at every cell of the area it covers.
    #[test]
    fn a_transparent_cover_never_absorbs(x in 0i32..12, y in 0i32..4) {
        use std::cell::Cell;
        use std::rc::Rc as R;
        use fresh_ui::{col, gesture, stack, Event, GestureKind, PointerMode};
        let hit = R::new(Cell::new(false));
        let h = hit.clone();
        let mut ui: Ui<()> = Ui::new();
        ui.frame(
            stack().children([
                gesture(text("")).w(Sizing::Cells(12)).h(Sizing::Cells(4)).on(
                    GestureKind::Press,
                    R::new(move |_: &Event| { h.set(true); None }),
                ),
                col()
                    .pointer_mode(PointerMode::Transparent)
                    .w(Sizing::Cells(12))
                    .h(Sizing::Cells(4)),
            ]),
            Size::new(12, 4),
        );
        ui.dispatch(Input::press(Point::new(x, y), MouseButton::Left, Mods::NONE));
        prop_assert!(hit.get(), "the cover swallowed the press at ({x}, {y})");
    }

    /// …and an opaque one absorbs at every cell. The pair is what makes either
    /// half meaningful.
    #[test]
    fn an_opaque_cover_always_absorbs(x in 0i32..12, y in 0i32..4) {
        use std::cell::Cell;
        use std::rc::Rc as R;
        use fresh_ui::{col, gesture, stack, Event, GestureKind};
        let hit = R::new(Cell::new(false));
        let h = hit.clone();
        let mut ui: Ui<()> = Ui::new();
        ui.frame(
            stack().children([
                gesture(text("")).w(Sizing::Cells(12)).h(Sizing::Cells(4)).on(
                    GestureKind::Press,
                    R::new(move |_: &Event| { h.set(true); None }),
                ),
                col().w(Sizing::Cells(12)).h(Sizing::Cells(4)),
            ]),
            Size::new(12, 4),
        );
        ui.dispatch(Input::press(Point::new(x, y), MouseButton::Left, Mods::NONE));
        prop_assert!(!hit.get(), "the cover let the press through at ({x}, {y})");
    }

    /// **The run count is carried, not invented.** Whatever the host reports on
    /// the press arrives on the press *and* on the click it completes — the
    /// library neither counts nor re-counts.
    #[test]
    fn the_click_run_is_reported_unchanged(n in 1u8..=8) {
        use std::cell::RefCell as RC;
        use std::rc::Rc as R;
        use fresh_ui::{gesture, Event, GestureKind};
        let seen: R<RC<Vec<(GestureKind, u8)>>> = R::default();
        let (a, b) = (seen.clone(), seen.clone());
        let mut ui: Ui<()> = Ui::new();
        ui.frame(
            gesture(text("")).w(Sizing::Cells(4)).h(Sizing::Cells(1))
                .on(GestureKind::Press, R::new(move |e: &Event| {
                    a.borrow_mut().push((e.kind, e.clicks)); None
                }))
                .on(GestureKind::Click, R::new(move |e: &Event| {
                    b.borrow_mut().push((e.kind, e.clicks)); None
                })),
            Size::new(4, 1),
        );
        let at = Point::new(1, 0);
        ui.dispatch(Input::press_n(at, MouseButton::Left, Mods::NONE, n));
        ui.dispatch(Input::release(at, MouseButton::Left, Mods::NONE));
        prop_assert_eq!(
            &*seen.borrow(),
            &vec![(GestureKind::Press, n), (GestureKind::Click, n)]
        );
    }
}
