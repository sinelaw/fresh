//! Layout (plan phase L3). Golden rectangles, boundaries, the cache, and the
//! determinism of flex division.

use fresh_ui::{
    col, distribute, layout_reader, row, text, viewport, Align, BuildCx, Component, ComponentExt,
    Constraints, Node, Rect, Size, Sizing, Ui,
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
