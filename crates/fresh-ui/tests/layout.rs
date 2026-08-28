//! Layout (plan phase L3). Golden rectangles, boundaries, the cache, and the
//! determinism of flex division.

use fresh_ui::{
    col, distribute, layout_reader, row, text, viewport, Align, BuildCx, Component, ComponentExt,
    Constraints, Draw, Key, Node, Rect, Size, Sizing, Ui,
};

const FRAME: Size = Size { w: 80, h: 24 };

fn ui() -> Ui<()> {
    Ui::new()
}

/// The tutorial's screen: a one-row title, then a row of a fixed sidebar and a
/// flexible body.
fn screen() -> Node<()> {
    col().children([
        text("My Tasks").h(Sizing::Cells(1)),
        row()
            .flex(1)
            .children([text("tags").w(Sizing::Cells(20)), text("body").flex(1)]),
    ])
}

#[test]
fn constraints_resolve_to_the_documented_rectangles() {
    let mut ui = ui();
    ui.frame(screen(), FRAME);

    assert_eq!(ui.rect(ui.root().unwrap()), Rect::new(0, 0, 80, 24));
    assert_eq!(
        ui.rect(ui.at(&[0]).unwrap()),
        Rect::new(0, 0, 80, 1),
        "title"
    );
    assert_eq!(
        ui.rect(ui.at(&[1]).unwrap()),
        Rect::new(0, 1, 80, 23),
        "body row"
    );
    assert_eq!(
        ui.rect(ui.at(&[1, 0]).unwrap()),
        Rect::new(0, 1, 20, 23),
        "sidebar"
    );
    assert_eq!(
        ui.rect(ui.at(&[1, 1]).unwrap()),
        Rect::new(20, 1, 60, 23),
        "body"
    );
}

#[test]
fn auto_height_inside_a_flex_column_inside_a_modal() {
    // The modal is a fixed 40x10 region; inside it a column with a flexible
    // header area and a list that takes only what its rows need.
    let mut ui = ui();
    ui.frame(
        col().w(Sizing::Cells(40)).h(Sizing::Cells(10)).children([
            text("header").flex(1),
            col().children([text("a"), text("b"), text("c")]),
        ]),
        FRAME,
    );

    let list = ui.at(&[1]).unwrap();
    assert_eq!(
        ui.size_of(list),
        Size::new(40, 3),
        "Auto is exactly the rows"
    );
    assert_eq!(
        ui.rect(ui.at(&[0]).unwrap()),
        Rect::new(0, 0, 40, 7),
        "flex takes the rest"
    );
    assert_eq!(ui.rect(list), Rect::new(0, 7, 40, 3));
}

#[test]
fn every_child_flex_in_zero_space_gives_every_child_zero() {
    let mut ui = ui();
    ui.frame(
        col().h(Sizing::Cells(0)).children([row()
            .flex(1)
            .children([text("a").flex(1), text("b").flex(1)])]),
        FRAME,
    );
    let r = ui.at(&[0]).unwrap();
    assert_eq!(ui.size_of(r).h, 0);
    assert_eq!(ui.size_of(ui.at(&[0, 0]).unwrap()).h, 0);
    assert_eq!(ui.size_of(ui.at(&[0, 1]).unwrap()).h, 0);
}

#[test]
fn text_wraps_to_a_width_a_sibling_determined() {
    let mut ui = ui();
    let spec = ui
        .frame(
            row()
                .w(Sizing::Cells(30))
                .h(Sizing::Cells(10))
                // Start, not the default Stretch, so the text keeps its own
                // height instead of filling the row.
                .align(Align::Start)
                .children([
                    text("gutter").w(Sizing::Cells(10)),
                    text("one two three four five six seven")
                        .wrap()
                        .w(Sizing::Flex(1)),
                ]),
            FRAME,
        )
        .clone();

    let wrapped = ui.at(&[1]).unwrap();
    // 20 columns are left after the gutter; the height follows from the width.
    assert_eq!(ui.rect(wrapped), Rect::new(10, 0, 20, 2));

    // The rows layout reserved are exactly the rows paint emits.
    let lines = spec
        .items
        .iter()
        .find_map(|i| match &i.draw {
            fresh_ui::Draw::Lines(l) if i.id == wrapped => Some(l.clone()),
            _ => None,
        })
        .expect("the wrapped text");
    assert_eq!(
        lines.iter().map(|l| l.to_string()).collect::<Vec<_>>(),
        vec!["one two three four", "five six seven"]
    );
}

#[test]
fn flex_remainders_are_deterministic() {
    // 10 cells over three equal children: 4, 3, 3 — the extra cell goes to the
    // earliest child, on every run.
    assert_eq!(distribute(10, &[1, 1, 1]), vec![4, 3, 3]);
    assert_eq!(distribute(10, &[1, 1, 1]), vec![4, 3, 3]);
    assert_eq!(distribute(7, &[2, 1]), vec![5, 2]);
    assert_eq!(distribute(0, &[1, 1]), vec![0, 0]);
    assert_eq!(distribute(5, &[0, 0]), vec![0, 0], "no weight, no cells");

    let mut ui = ui();
    ui.frame(
        row().w(Sizing::Cells(10)).h(Sizing::Cells(1)).children([
            text("a").w(Sizing::Flex(1)),
            text("b").w(Sizing::Flex(1)),
            text("c").w(Sizing::Flex(1)),
        ]),
        FRAME,
    );
    assert_eq!(ui.rect(ui.at(&[0]).unwrap()), Rect::new(0, 0, 4, 1));
    assert_eq!(ui.rect(ui.at(&[1]).unwrap()), Rect::new(4, 0, 3, 1));
    assert_eq!(ui.rect(ui.at(&[2]).unwrap()), Rect::new(7, 0, 3, 1));
}

// -- boundaries and the cache -----------------------------------------------

struct Label(&'static str);

impl Component<()> for Label {
    type State = u32;
    fn build(&self, s: &u32, _cx: &mut BuildCx<'_, ()>) -> Node<()> {
        text(format!("{} {}", self.0, s))
    }
}

fn boxed_label() -> Node<()> {
    col().children([
        text("top").h(Sizing::Cells(1)),
        // A fixed-size box: nothing inside it can change its size.
        col()
            .w(Sizing::Cells(20))
            .h(Sizing::Cells(3))
            .child(Label("n").node()),
        text("bottom").h(Sizing::Cells(1)),
    ])
}

#[test]
fn a_dirty_node_inside_a_fixed_box_relayouts_that_box_and_nothing_above() {
    let mut ui = ui();
    ui.frame(boxed_label(), FRAME);

    let root = ui.root().unwrap();
    let fixed = ui.at(&[1]).unwrap();
    let label = ui.at(&[1, 0]).unwrap();
    let sibling = ui.at(&[0]).unwrap();

    let before_root = ui.layouts(root);
    let before_sibling = ui.layouts(sibling);
    let before_fixed = ui.layouts(fixed);

    ui.set_state::<u32>(label, |s| *s += 1);
    ui.tick();

    assert_eq!(
        ui.layouts(root),
        before_root,
        "the root was not measured again"
    );
    assert_eq!(
        ui.layouts(sibling),
        before_sibling,
        "nor an unrelated sibling"
    );
    assert!(ui.layouts(fixed) > before_fixed, "the boundary was");
}

#[test]
fn a_clean_node_handed_equal_constraints_is_not_visited() {
    let mut ui = ui();
    ui.frame(screen(), FRAME);
    let sidebar = ui.at(&[1, 0]).unwrap();
    let body = ui.at(&[1, 1]).unwrap();
    let before_sidebar = ui.layouts(sidebar);
    let before_body = ui.layouts(body);

    // Same description, same frame: layout has nothing to do.
    ui.frame(screen(), FRAME);
    assert_eq!(
        ui.layouts(sidebar),
        before_sidebar,
        "cached result returned"
    );
    assert_eq!(ui.layouts(body), before_body);

    // A narrower frame changes what the flexible child is given, so it is
    // measured again — while the fixed-width sidebar, whose constraints are
    // unchanged, still is not.
    ui.frame(screen(), Size::new(70, 24));
    assert!(ui.layouts(body) > before_body, "the flexible child");
    assert_eq!(
        ui.layouts(sidebar),
        before_sidebar,
        "the fixed one is untouched"
    );
}

// -- constraint-dependent structure ------------------------------------------

#[test]
fn a_layout_reader_receives_its_constraints_during_the_pass() {
    use std::cell::RefCell;
    use std::rc::Rc;

    let seen: Rc<RefCell<Vec<Constraints>>> = Rc::default();
    let s = seen.clone();

    let mut ui = ui();
    ui.frame(
        row().children([
            text("gutter").w(Sizing::Cells(12)),
            layout_reader(move |info| {
                s.borrow_mut().push(info.constraints);
                // How many rows fit is a function of the constraints, so the
                // structure can only be decided here.
                col().children((0..info.constraints.max_h.min(4)).map(|i| text(format!("row {i}"))))
            })
            .flex(1),
        ]),
        Size::new(40, 3),
    );

    let got = seen.borrow().clone();
    assert_eq!(got.len(), 1);
    assert_eq!(got[0].max_w, 28, "the width left after the gutter");
    assert_eq!(got[0].max_h, 3);
    assert_eq!(
        ui.children(ui.at(&[1, 0]).unwrap()).len(),
        3,
        "three rows fit"
    );
}

#[test]
#[should_panic(expected = "geometry is not readable during build")]
fn geometry_is_not_readable_during_build() {
    use std::cell::RefCell;
    use std::rc::Rc;

    struct Peeker(Rc<RefCell<Option<*const Ui<()>>>>);

    // A component that reaches for geometry mid-build. The reference is
    // obtained the only way a component could: through shared state the
    // application handed it.
    impl Component<()> for Peeker {
        type State = ();
        fn build(&self, _s: &(), cx: &mut BuildCx<'_, ()>) -> Node<()> {
            let p = self.0.borrow().expect("wired");
            // SAFETY-free equivalent of an application handing a component a
            // handle it should not use here; the assert is what is under test.
            let ui: &Ui<()> = unsafe { &*p };
            let _ = ui.rect(cx.id());
            text("x")
        }
    }

    let cell: Rc<RefCell<Option<*const Ui<()>>>> = Rc::default();
    let mut ui: Ui<()> = Ui::new();
    *cell.borrow_mut() = Some(&ui as *const _);
    ui.frame(Peeker(cell).node(), FRAME);
}

#[test]
fn alignment_places_children_across_the_cross_axis() {
    let mut ui = ui();
    ui.frame(
        col()
            .w(Sizing::Cells(20))
            .h(Sizing::Cells(3))
            .align(Align::Center)
            .children([text("ab"), text("cdef")]),
        FRAME,
    );
    assert_eq!(ui.rect(ui.at(&[0]).unwrap()), Rect::new(9, 0, 2, 1));
    assert_eq!(ui.rect(ui.at(&[1]).unwrap()), Rect::new(8, 1, 4, 1));
}

#[test]
fn padding_border_and_gap_come_out_of_the_content_box() {
    let mut ui = ui();
    ui.frame(
        col()
            .w(Sizing::Cells(20))
            .h(Sizing::Cells(10))
            .pad(2, 1)
            .border()
            .gap(1)
            .children([text("a").h(Sizing::Cells(1)), text("b").h(Sizing::Cells(1))]),
        FRAME,
    );
    // pad 2/1 plus a 1-cell border on each side.
    assert_eq!(ui.rect(ui.at(&[0]).unwrap()), Rect::new(3, 2, 14, 1));
    assert_eq!(
        ui.rect(ui.at(&[1]).unwrap()),
        Rect::new(3, 4, 14, 1),
        "one row of gap"
    );
}

#[test]
fn a_viewport_clips_its_children_and_scrolls_them() {
    let mut ui = ui();
    let rows: Vec<Node<()>> = (0..20).map(|i| text(format!("row {i}"))).collect();
    ui.frame(
        viewport(col().children(rows))
            .w(Sizing::Cells(20))
            .h(Sizing::Cells(5)),
        FRAME,
    );
    let vp = ui.root().unwrap();
    assert_eq!(ui.size_of(vp), Size::new(20, 5));
    assert_eq!(
        ui.scroll(vp).1,
        Size::new(20, 20),
        "content is taller than the window"
    );
    assert_eq!(ui.rect(ui.at(&[0, 0]).unwrap()), Rect::new(0, 0, 20, 1));

    ui.scroll_to(vp, fresh_ui::Point::new(0, 6));
    ui.tick();
    assert_eq!(
        ui.rect(ui.at(&[0, 0]).unwrap()),
        Rect::new(0, -6, 20, 1),
        "scrolled above"
    );
    assert!(ui
        .rect(ui.at(&[0, 0]).unwrap())
        .intersect(ui.clip(ui.at(&[0, 0]).unwrap()))
        .is_empty());
    assert_eq!(
        ui.rect(ui.at(&[0, 6]).unwrap()),
        Rect::new(0, 0, 20, 1),
        "now at the top"
    );
}

// ---------------------------------------------------------------------------
// Floors: an extent that a shrinking container cannot take away
// ---------------------------------------------------------------------------

/// A `Flex` gap takes what is left — and what is left can be nothing.
///
/// This is the behaviour a floor exists to override, pinned first so the next
/// test means something.
#[test]
fn a_flex_gap_closes_completely_when_there_is_nothing_left() {
    let mut ui = ui();
    ui.frame(
        row().children([
            text("aaaaaaaa").w(Sizing::Cells(8)),
            row().flex(1),
            text("bb").w(Sizing::Cells(2)),
        ]),
        Size::new(10, 1),
    );
    assert_eq!(ui.rect(ui.at(&[1]).unwrap()).w, 0);
}

/// With a floor it does not close: the gap keeps its cells and the row
/// overflows, rather than the separation silently disappearing.
///
/// Overflowing is the honest outcome. A caller that writes `min_w(3)` is
/// saying three cells matter more than fitting; the alternative — quietly
/// giving zero — is the case that ships as a rendering bug nobody can see in
/// the description.
#[test]
fn a_floor_keeps_a_flex_gap_open_when_the_row_is_too_narrow() {
    let mut ui = ui();
    ui.frame(
        row().children([
            text("aaaaaaaa").w(Sizing::Cells(8)),
            row().flex(1).min_w(3),
            text("bb").w(Sizing::Cells(2)),
        ]),
        Size::new(10, 1),
    );
    assert_eq!(ui.rect(ui.at(&[1]).unwrap()).w, 3);
}

/// A floor is a floor, not a size: with room to spare the flex share wins.
#[test]
fn a_floor_does_not_cap_a_gap_that_has_room() {
    let mut ui = ui();
    ui.frame(
        row().children([
            text("aa").w(Sizing::Cells(2)),
            row().flex(1).min_w(3),
            text("bb").w(Sizing::Cells(2)),
        ]),
        Size::new(20, 1),
    );
    assert_eq!(ui.rect(ui.at(&[1]).unwrap()).w, 16);
}

/// It applies to the main axis of whichever direction the container runs, so
/// the same attribute reads correctly in a column.
#[test]
fn a_column_honours_the_height_floor() {
    let mut ui = ui();
    ui.frame(
        col().children([
            text("a").h(Sizing::Cells(8)),
            col().flex(1).min_h(3),
            text("b").h(Sizing::Cells(2)),
        ]),
        Size::new(10, 10),
    );
    assert_eq!(ui.rect(ui.at(&[1]).unwrap()).h, 3);
}

/// A floor on a node that is not flexible at all still holds: `Auto` content
/// narrower than the floor is padded out to it.
#[test]
fn a_floor_widens_content_that_would_be_narrower() {
    let mut ui = ui();
    ui.frame(
        row().children([text("ab").min_w(6), text("cd")]),
        Size::new(20, 1),
    );
    assert_eq!(ui.rect(ui.at(&[0]).unwrap()).w, 6);
    assert_eq!(ui.rect(ui.at(&[1]).unwrap()).x, 6);
}

/// The floor survives the compositions a real list is made of: a viewport
/// around a column of rows, each row ending in a badge pushed right by a
/// floored gap.
///
/// Isolated tests measure a `row()` whose parent hands it a definite width.
/// A list row's width arrives through a viewport and a constraint-dependent
/// builder, and that is the path a host actually uses — so it is the path the
/// floor has to hold on.
#[test]
fn a_floor_holds_inside_a_viewport() {
    let mut ui = ui();
    ui.frame(
        viewport(col().children([row().h(Sizing::Cells(1)).children([
            text("a rather long row title"),
            row().flex(1).min_w(3),
            text("#2").w(Sizing::Cells(2)),
        ])])),
        Size::new(20, 4),
    );
    let gap = ui.at(&[0, 0, 1]).expect("the gap");
    assert!(
        ui.rect(gap).w >= 3,
        "the gap collapsed to {} inside a viewport",
        ui.rect(gap).w
    );
}

/// A second frame re-measures every dirty boundary, not just the root.
///
/// The mark that a node needs layout stops at the nearest relayout boundary —
/// that is the point of boundaries. So re-measuring the *root* does not stand
/// in for the rest of the dirty list: the root walk hits each boundary's cache
/// and short-circuits above it, leaving it holding the previous frame's
/// measurement. `TextRender` shapes its rows during measure and paints from
/// them, so a boundary that was skipped paints last frame's text.
///
/// A layer is what made this reachable in practice: it dirties the root, the
/// root has no cached constraints to re-enter on, and the pass used to drop
/// the rest of the list on that account. The editor's status bar then painted
/// a stale row for every frame in which a menu was open.
#[test]
fn a_stale_boundary_is_re_measured_even_when_the_root_is_too() {
    let tree = |msg: &str, overlay: bool| {
        // `h(Cells(1))` hands the row tight constraints, which makes it a
        // relayout boundary — the marker from the text below stops there.
        // The inner `col` matters: it sits between the root and the boundary
        // and is itself clean, so a walk from the root stops there. Without
        // something in between, the root's own re-measure would reach the
        // boundary by accident and the bug would not show.
        let base = col().children([col().flex(1).children([
            row().h(Sizing::Cells(1)).children([text(msg.to_string())]),
            text("body").flex(1),
        ])]);
        if overlay {
            base.child(
                fresh_ui::layer()
                    .anchor(fresh_ui::Anchor::Screen(Align::Center))
                    .child(text("overlay")),
            )
        } else {
            base
        }
    };
    let lines = |ui: &Ui<()>| -> Vec<String> {
        ui.spec()
            .items
            .iter()
            .filter_map(|i| match &i.draw {
                fresh_ui::Draw::Lines(l) => l.first().map(|s| s.to_string()),
                _ => None,
            })
            .collect()
    };

    // One `Ui` across both frames: the second frame is a reconcile, which is
    // how a host drives it.
    let mut ui = ui();
    ui.frame(tree("before", true), FRAME);
    assert!(lines(&ui).contains(&"before".to_string()));

    ui.frame(tree("after", false), FRAME);
    assert!(
        lines(&ui).contains(&"after".to_string()),
        "the boundary painted a stale row: {:?}",
        lines(&ui)
    );
}

/// **Priority decides who yields, declaration order decides who is placed
/// where.** The two were the same thing before: a row sizes its children
/// against the space that is left, in the order they were declared, so the
/// first-declared won every contest.
///
/// The status bar is the case this exists for. Its rule is "reserve the right
/// side, then spend what is left on the left side" — a precedence, which flex
/// cannot say, so the editor computed the answer outside layout and passed in
/// widths (`left_budget`). Here the same rule is two `priority` calls.
#[test]
fn a_higher_priority_child_is_sized_before_a_lower_one() {
    let row_of = |left_priority: u8, right_priority: u8| {
        row().children([
            text("a-very-long-left-hand-label")
                .w(Sizing::Cells(26))
                .priority(left_priority),
            text("RIGHT").w(Sizing::Cells(5)).priority(right_priority),
        ])
    };

    // Declaration order alone: the left child takes what it asked for and the
    // right one gets the remainder — three cells of a five-cell ask.
    let mut plain = ui();
    plain.frame(row_of(0, 0), Size::new(29, 1));
    assert_eq!(plain.rect(plain.at(&[0]).unwrap()).w, 26);
    assert_eq!(
        plain.rect(plain.at(&[1]).unwrap()).w,
        3,
        "the right side yielded"
    );

    // The same tree, with the right side reserved first. It keeps all five
    // cells and the left side truncates instead — and it is still drawn on
    // the right, because placement did not change.
    let mut reserved = ui();
    reserved.frame(row_of(0, 1), Size::new(29, 1));
    let left = reserved.rect(reserved.at(&[0]).unwrap());
    let right = reserved.rect(reserved.at(&[1]).unwrap());
    assert_eq!(right.w, 5, "the reserved side keeps its width");
    assert_eq!(left.w, 24, "the left side spends what is left");
    assert!(left.x < right.x, "placement is still declaration order");
}

/// The default costs nothing: with no priority set anywhere, layout is
/// byte-identical to what it was before the concept existed.
#[test]
fn equal_priorities_keep_declaration_order() {
    let mut ui = ui();
    ui.frame(
        row().children([
            text("aaaa").w(Sizing::Cells(4)).priority(3),
            text("bbbb").w(Sizing::Cells(4)).priority(3),
            text("cccc").w(Sizing::Cells(4)).priority(3),
        ]),
        Size::new(9, 1),
    );
    assert_eq!(ui.rect(ui.at(&[0]).unwrap()).w, 4);
    assert_eq!(ui.rect(ui.at(&[1]).unwrap()).w, 4);
    assert_eq!(ui.rect(ui.at(&[2]).unwrap()).w, 1, "the last one yields");
}

// -- Elide -------------------------------------------------------------------

/// **A run that is given less than it measured at says so.**
///
/// Non-wrapping text is clipped by the enclosing box, which loses the fact that
/// anything was cut. That was survivable while a host truncated its own strings
/// first — but `priority` moved the decision into layout, so by the time the
/// width is known the host is no longer in the loop. The mark has to come from
/// the run.
#[test]
fn an_elided_run_marks_the_end_it_cut() {
    use fresh_ui::Elide;
    let painted = |e: Elide| {
        let mut ui: Ui<()> = Ui::new();
        let spec = ui
            .frame(
                row()
                    .w(Sizing::Cells(6))
                    .child(text("abcdefghij").elide(e).key(Key::from(1u64))),
                Size::new(6, 1),
            )
            .clone();
        spec.items
            .iter()
            .find(|i| i.key.as_ref() == Some(&Key::from(1u64)))
            .and_then(|i| match &i.draw {
                Draw::Lines(l) => l.first().map(|s| s.to_string()),
                _ => None,
            })
            .expect("the run painted")
    };
    // The head survives, and the cut is visible at the end.
    assert_eq!(painted(Elide::Tail), "abcde…");
    // The tail survives — a path keeps its filename.
    assert_eq!(painted(Elide::Head), "…fghij");
    // Unasked for, nothing changes: the box clips, exactly as before.
    assert_eq!(painted(Elide::None), "abcdefghij");
}

/// **Cells, not characters.** Counting `char`s puts the mark in the wrong place
/// for a wide glyph and can cut one in half. Four double-width glyphs in five
/// cells is two glyphs and the mark.
#[test]
fn eliding_counts_cells_and_never_splits_a_glyph() {
    use fresh_ui::Elide;
    let mut ui: Ui<()> = Ui::new();
    let spec = ui
        .frame(
            row()
                .w(Sizing::Cells(5))
                .child(text("漢字漢字").elide(Elide::Tail).key(Key::from(1u64))),
            Size::new(5, 1),
        )
        .clone();
    let painted = spec
        .items
        .iter()
        .find(|i| i.key.as_ref() == Some(&Key::from(1u64)))
        .and_then(|i| match &i.draw {
            Draw::Lines(l) => l.first().map(|s| s.to_string()),
            _ => None,
        })
        .expect("the run painted");
    assert_eq!(painted, "漢字…");
}

/// **A layer can be as wide as what it hangs off.**
///
/// A dropdown matches its button; a command palette matches the row it sits
/// above. Neither is expressible any other way: a layer measures against the
/// whole frame, so `flex` inside it reaches the frame's edge, and a cell count
/// is the caller measuring the anchor by hand — which is the arithmetic
/// anchoring exists to remove.
#[test]
fn a_stretched_layer_takes_its_anchor_width() {
    use fresh_ui::{layer, Anchor, Place};
    let build = |stretch: bool| {
        let mut ui: Ui<()> = Ui::new();
        let l = layer()
            .key(Key::from(9u64))
            .anchor(Anchor::Node(Key::from(1u64)))
            .place(Place::Above)
            .child(col().theme("x").child(text("hi")));
        let l = if stretch { l.stretch_to_anchor() } else { l };
        let spec = ui
            .frame(
                col().children([
                    row().h(Sizing::Cells(4)),
                    row()
                        .key(Key::from(1u64))
                        .w(Sizing::Cells(30))
                        .h(Sizing::Cells(1))
                        .theme("row")
                        .child(l),
                ]),
                Size::new(60, 10),
            )
            .clone();
        let r = spec
            .index
            .iter()
            .find(|(k, _)| *k == Key::from(9u64))
            .unwrap()
            .1
            .clone();
        spec.items[r.start].rect
    };
    // Left to itself, the layer is the width of its content.
    assert_eq!(build(false).w, 2, "\"hi\" is two cells");
    // Stretched, it is the width of the row it is placed above — and still
    // only as tall as it needs.
    let s = build(true);
    assert_eq!((s.w, s.h), (30, 1), "the anchor's width, its own height");
}

/// **A caret is a cell; a click position is a point.**
///
/// `Place::Below` an `Anchor::Point` lands on the point's own row, because a
/// point resolves to a zero-size rect and below a zero-height thing is itself.
/// That is right for what a point names — the context menu opens `Over` a click
/// position and wants exactly that. A caret occupies a cell, and a completion
/// popup placed below one must clear the character it is completing.
#[test]
fn below_a_cell_is_the_row_after_it_and_below_a_point_is_its_own() {
    use fresh_ui::{layer, Anchor, Place};
    let at = |a: Anchor| {
        let mut ui: Ui<()> = Ui::new();
        let spec = ui
            .frame(
                col().child(
                    layer()
                        .key(Key::from(3u64))
                        .anchor(a)
                        .place(Place::Below)
                        .child(col().theme("x").child(text("hi"))),
                ),
                Size::new(40, 12),
            )
            .clone();
        spec.index
            .iter()
            .find(|(k, _)| *k == Key::from(3u64))
            .and_then(|(_, r)| spec.items.get(r.start))
            .map(|i| i.rect.y)
            .expect("placed")
    };
    assert_eq!(
        at(Anchor::Point(5, 4)),
        4,
        "below a point is the point's row"
    );
    assert_eq!(
        at(Anchor::Cell(5, 4)),
        5,
        "below a cell is the row after it"
    );
}

/// **A layer can align within its anchor, not only match it.**
///
/// A notification hangs off the right edge of the row above it; a dropdown
/// matches its button's width. Both are relationships to the anchor, and
/// `stretch_to_anchor` was only the `Stretch` one — with no way to say the
/// others, a caller is back to measuring the anchor and subtracting.
#[test]
fn a_layer_aligns_on_the_axis_its_placement_leaves_free() {
    use fresh_ui::{layer, Align, Anchor, Place};
    let at = |align: Option<Align>| {
        let l = layer()
            .key(Key::from(4u64))
            .anchor(Anchor::Node(Key::from(1u64)))
            .place(Place::Above)
            .child(col().theme("x").child(text("hi")));
        let l = match align {
            Some(a) => l.align_to_anchor(a),
            None => l,
        };
        let mut ui: Ui<()> = Ui::new();
        let spec = ui
            .frame(
                col().children([
                    row().h(Sizing::Cells(4)),
                    row()
                        .key(Key::from(1u64))
                        .w(Sizing::Cells(30))
                        .h(Sizing::Cells(1))
                        .theme("row")
                        .child(l),
                ]),
                Size::new(60, 10),
            )
            .clone();
        let i = spec
            .index
            .iter()
            .find(|(k, _)| *k == Key::from(4u64))
            .and_then(|(_, r)| spec.items.get(r.start))
            .expect("placed");
        (i.rect.x, i.rect.w)
    };
    // Unaligned: the placement's own origin, the anchor's left edge.
    assert_eq!(at(None), (0, 2));
    assert_eq!(at(Some(Align::Start)), (0, 2));
    // "hi" is two cells; the anchor is thirty.
    assert_eq!(at(Some(Align::Center)), (14, 2));
    assert_eq!(
        at(Some(Align::End)),
        (28, 2),
        "flush with the anchor's right"
    );
    // Stretch is the case `stretch_to_anchor` already spelled, unchanged.
    assert_eq!(at(Some(Align::Stretch)), (0, 30));
}

/// **A host can say where something inside it is, and an anchor can name it.**
///
/// The tree hands a host leaf a rectangle and knows nothing about its interior,
/// so a layer anchored to a text caret names a thing the tree cannot locate.
/// The alternative is a screen coordinate in the description, which is the
/// caller doing the layout engine's job and carrying a number that goes stale.
/// So the owner of the space answers where the thing is, and the description
/// still names it.
#[test]
fn a_layer_can_anchor_to_a_rectangle_the_host_published() {
    let caret = Key::Str("caret".into());
    let popup = Key::Str("popup".into());
    let tree = || -> Node<()> {
        col().child(fresh_ui::host(1u64).flex(1)).child(
            fresh_ui::layer()
                .key(popup.clone())
                .anchor(fresh_ui::Anchor::Node(caret.clone()))
                .place(fresh_ui::Place::Below)
                .child(col().w(Sizing::Cells(10)).h(Sizing::Cells(3)).theme("p")),
        )
    };
    let rect_of = |ui: &Ui<()>| ui.find_by_key(&popup).map(|id| ui.rect_of(id)).unwrap();

    // No element and no published rectangle: the anchor falls back, as a name
    // nobody answers always has.
    let mut ui: Ui<()> = Ui::new();
    ui.frame(tree(), FRAME);
    let fallback = rect_of(&ui);

    // The host says where its caret is; the layer lands on the row below it.
    ui.set_host_anchor(caret.clone(), Rect::new(30, 8, 1, 1));
    ui.place_layers(FRAME);
    let placed = rect_of(&ui);
    assert_eq!((placed.x, placed.y), (30, 9), "below the published cell");
    assert_ne!(placed, fallback, "publishing an anchor moved the layer");

    // It moves with the caret, without the description changing.
    ui.set_host_anchor(caret.clone(), Rect::new(12, 3, 1, 1));
    ui.place_layers(FRAME);
    assert_eq!((rect_of(&ui).x, rect_of(&ui).y), (12, 4));

    // Anchors are per-frame: a new frame with nothing published falls back
    // again rather than reusing a caret that has since moved.
    ui.frame(tree(), FRAME);
    assert_eq!(rect_of(&ui), fallback, "a stale caret is worse than none");
}

/// An element carrying the key wins: a published rectangle fills a gap, it
/// cannot shadow a real node.
#[test]
fn an_element_outranks_a_published_anchor_for_the_same_key() {
    let k = Key::Str("thing".into());
    let popup = Key::Str("popup".into());
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col()
            .child(col().key(k.clone()).w(Sizing::Cells(4)).h(Sizing::Cells(2)))
            .child(
                fresh_ui::layer()
                    .key(popup.clone())
                    .anchor(fresh_ui::Anchor::Node(k.clone()))
                    .place(fresh_ui::Place::Below)
                    .child(col().w(Sizing::Cells(6)).h(Sizing::Cells(1)).theme("p")),
            ),
        FRAME,
    );
    let with_element = ui.find_by_key(&popup).map(|id| ui.rect_of(id)).unwrap();
    ui.set_host_anchor(k.clone(), Rect::new(50, 20, 1, 1));
    ui.place_layers(FRAME);
    assert_eq!(
        ui.find_by_key(&popup).map(|id| ui.rect_of(id)).unwrap(),
        with_element,
        "the real node still answers for its own key"
    );
}

/// **A layer can be confined to a region rather than to the frame.**
///
/// A surface that floats over everything belongs to the frame. Some do not: a
/// popup hanging off a status bar must not put its right border on the
/// editor's scrollbar, and clamping to the frame lands it exactly there.
/// Shrinking the layer is not the same statement — that changes how big the
/// box is, when what is wanted is where it may go.
#[test]
fn a_layer_confined_to_a_region_clamps_to_that_region() {
    let area = Key::Str("area".into());
    let popup = Key::Str("popup".into());
    // The layer wants to sit at x=70 and is 20 wide, so it overhangs both the
    // frame (80) and the region (79) and has to be pulled back.
    let tree = |within: Option<Key>| -> Node<()> {
        let mut l = fresh_ui::layer()
            .key(popup.clone())
            .anchor(fresh_ui::Anchor::Point(70, 5))
            .place(fresh_ui::Place::Over)
            .fit(fresh_ui::Fit::CLAMP);
        if let Some(k) = within {
            l = l.within(k);
        }
        col().child(l.child(col().w(Sizing::Cells(20)).h(Sizing::Cells(2)).theme("p")))
    };
    let x_of = |ui: &Ui<()>| ui.find_by_key(&popup).map(|id| ui.rect_of(id)).unwrap().x;

    let mut ui: Ui<()> = Ui::new();
    ui.frame(tree(None), FRAME);
    assert_eq!(x_of(&ui), 60, "clamped to the frame's right edge");

    // The same layer, told it may only occupy the frame less its last column.
    let mut ui: Ui<()> = Ui::new();
    ui.frame(tree(Some(area.clone())), FRAME);
    ui.set_host_anchor(area.clone(), Rect::new(0, 0, FRAME.w - 1, FRAME.h));
    ui.place_layers(FRAME);
    assert_eq!(x_of(&ui), 59, "the reserved column is left alone");
}

/// A region also moves where a screen-anchored layer centres, and where it can
/// start: the bounds are the whole coordinate space the placement works in, not
/// just a right-hand limit.
#[test]
fn a_region_moves_the_origin_as_well_as_the_limit() {
    let area = Key::Str("area".into());
    let popup = Key::Str("popup".into());
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col().child(
            fresh_ui::layer()
                .key(popup.clone())
                .anchor(fresh_ui::Anchor::Screen(Align::Center))
                .within(area.clone())
                .child(col().w(Sizing::Cells(10)).h(Sizing::Cells(2)).theme("p")),
        ),
        FRAME,
    );
    // A region 40 wide starting at x=20: the centre is 20 + (40-10)/2 = 35.
    ui.set_host_anchor(area.clone(), Rect::new(20, 4, 40, 10));
    ui.place_layers(FRAME);
    let r = ui.find_by_key(&popup).map(|id| ui.rect_of(id)).unwrap();
    assert_eq!((r.x, r.y), (35, 4 + (10 - 2) / 2));
}
