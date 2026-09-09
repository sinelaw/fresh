//! Scheduler exit criteria (plan phase L2).

use std::cell::RefCell;
use std::rc::Rc;

use fresh_ui::{
    col, focusable, text, BuildCx, Component, ComponentExt, Handler, Node, Size, Sizing, Ui,
    Updater,
};

// -- components --------------------------------------------------------------

struct Child;

impl Component<()> for Child {
    type State = u32;
    fn build(&self, s: &u32, _cx: &mut BuildCx<'_, ()>) -> Node<()> {
        text(format!("{s}"))
    }
}

struct Parent;

impl Component<()> for Parent {
    type State = u32;
    fn build(&self, _s: &u32, _cx: &mut BuildCx<'_, ()>) -> Node<()> {
        col().child(Child.node())
    }
}

/// Shows its child while its state is true.
struct Toggle;

impl Component<()> for Toggle {
    type State = bool;
    fn build(&self, show: &bool, _cx: &mut BuildCx<'_, ()>) -> Node<()> {
        col().child_if(*show, || Child.node())
    }
}

struct Bad;

impl Component<()> for Bad {
    type State = u32;
    fn build(&self, _s: &u32, cx: &mut BuildCx<'_, ()>) -> Node<()> {
        cx.updater::<u32>().set(|s| *s += 1);
        text("unreachable")
    }
}

/// Hands its updater and a click handler back to the test, standing in for the
/// dispatch layer that will do this from a real event.
#[derive(Clone, Default)]
struct Wires {
    updater: Rc<RefCell<Option<Updater<u32>>>>,
    handler: Rc<RefCell<Option<Handler<()>>>>,
}

struct Wired(Wires);

impl Component<()> for Wired {
    type State = u32;
    fn build(&self, s: &u32, cx: &mut BuildCx<'_, ()>) -> Node<()> {
        *self.0.updater.borrow_mut() = Some(cx.updater::<u32>());
        *self.0.handler.borrow_mut() = Some(cx.set_state(|s: &mut u32| *s += 100));
        text(format!("{s}"))
    }
}

// -- cases -------------------------------------------------------------------

#[test]
fn parent_and_child_dirty_in_one_tick_build_once_each_parent_first() {
    let mut ui: Ui<()> = Ui::new();
    ui.reconcile(Parent.node());
    let p = ui.root().unwrap();
    let c = ui.at(&[0, 0]).unwrap();

    ui.trace(true);
    ui.set_state::<u32>(c, |s| *s += 1);
    ui.set_state::<u32>(p, |s| *s += 1);
    ui.flush();

    // The parent rebuilds first and reconciles the child as part of that pass,
    // which clears the child's mark; the drain then finds nothing left to do.
    assert_eq!(ui.take_build_log(), vec![p, c]);
}

#[test]
fn an_element_disposed_by_its_parents_rebuild_is_not_rebuilt() {
    let mut ui: Ui<()> = Ui::new();
    ui.reconcile(Toggle.node());
    let t = ui.root().unwrap();
    ui.set_state::<bool>(t, |s| *s = true);
    ui.flush();
    let c = ui.at(&[0, 0]).unwrap();

    ui.trace(true);
    ui.set_state::<u32>(c, |s| *s += 1);
    ui.set_state::<bool>(t, |s| *s = false);
    ui.flush();

    assert_eq!(ui.take_build_log(), vec![t]);
    assert!(!ui.is_live(c), "and it is gone by the end of the flush");
}

#[test]
#[should_panic(expected = "set_state during build at Bad")]
fn set_state_during_build_panics_naming_the_element() {
    let mut ui: Ui<()> = Ui::new();
    ui.reconcile(Bad.node());
}

#[test]
fn set_state_from_a_handler_coalesces_into_the_next_flush() {
    let wires = Wires::default();
    let mut ui: Ui<()> = Ui::new();
    ui.reconcile(Wired(wires.clone()).node());
    let id = ui.root().unwrap();

    let updater = wires.updater.borrow().clone().unwrap();
    let handler = wires.handler.borrow().clone().unwrap();

    // Three separate updates from outside a build.
    updater.set(|s| *s += 1);
    updater.set(|s| *s += 10);
    handler(&fresh_ui::Event::synthetic(
        fresh_ui::GestureKind::Click,
        id,
    ));

    assert_eq!(
        *ui.state::<u32>(id).unwrap(),
        0,
        "updates mark; they do not propagate"
    );

    ui.trace(true);
    ui.flush();

    assert_eq!(
        *ui.state::<u32>(id).unwrap(),
        111,
        "all three applied, in order"
    );
    assert_eq!(
        ui.take_build_log(),
        vec![id],
        "coalesced into a single build"
    );
}

#[test]
fn a_mark_on_an_element_that_flush_disposes_is_dropped_silently() {
    let mut ui: Ui<()> = Ui::new();
    ui.reconcile(Toggle.node());
    let t = ui.root().unwrap();
    ui.set_state::<bool>(t, |s| *s = true);
    ui.flush();
    let c = ui.at(&[0, 0]).unwrap();

    ui.set_state::<u32>(c, |s| *s += 1);
    ui.reconcile(col()); // a whole new root type: everything below goes away
    ui.flush(); // must not observe the stale mark
    assert!(!ui.is_live(c));
}

// -- layout_only: geometry without the frame ---------------------------------
//
// A host that only wants a rectangle out of a description asks with
// `layout_only`. The tests below pin the two halves of what that means: the
// geometry is the geometry a real frame would have produced, and none of the
// things a frame *does* have happened. Each one is a side effect a caller once
// paid per question asked, not per frame shown — the editor's macro replay
// asks once per replayed action.

const GEOM: Size = Size { w: 20, h: 6 };

/// A column of three one-cell rows, the second of which asks for focus.
fn rows() -> Node<()> {
    col().children([
        focusable(text("one")).key("one").h(Sizing::Cells(1)),
        focusable(text("two"))
            .key("two")
            .h(Sizing::Cells(1))
            .autofocus(),
        focusable(text("three")).key("three").h(Sizing::Cells(1)),
    ])
}

#[test]
fn layout_only_lays_out_a_new_description() {
    let mut probe: Ui<()> = Ui::new();
    probe.layout_only(rows(), GEOM);

    let mut framed: Ui<()> = Ui::new();
    framed.frame(rows(), GEOM);

    for i in 0..3 {
        assert_eq!(
            probe.rect_of(probe.at(&[i]).unwrap()),
            framed.rect_of(framed.at(&[i]).unwrap()),
            "row {i}: the rectangle a frame would have given"
        );
    }
}

#[test]
fn layout_only_does_not_move_focus() {
    let mut ui: Ui<()> = Ui::new();
    ui.layout_only(rows(), GEOM);
    assert_eq!(
        ui.focused(),
        None,
        "autofocus is a reaction to a frame, not part of computing one"
    );

    // And the reaction is not lost — the frame that does happen still fires it.
    ui.frame(rows(), GEOM);
    assert_eq!(ui.focused(), Some(ui.at(&[1]).unwrap()));
}

#[test]
fn layout_only_leaves_a_queued_scroll_command_queued() {
    use fresh_ui::{viewport, Point};

    let anchor = fresh_ui::behavior::anchor::Anchor::new();
    let mk = |a: std::rc::Rc<fresh_ui::behavior::anchor::Anchor>| -> Node<()> {
        viewport(col().children((0..40).map(|i| text(format!("row {i}")))))
            .anchor_to(a)
            .h(Sizing::Cells(4))
    };

    let mut ui: Ui<()> = Ui::new();
    ui.frame(mk(anchor.clone()), GEOM);
    let vp = ui.root().unwrap();
    assert_eq!(ui.scroll(vp).0, Point::ZERO);

    anchor.scroll_to(Point::new(0, 10));
    ui.layout_only(mk(anchor.clone()), GEOM);
    assert_eq!(
        ui.scroll(vp).0,
        Point::ZERO,
        "a question about geometry does not move a viewport"
    );

    ui.frame(mk(anchor.clone()), GEOM);
    assert_eq!(
        ui.scroll(vp).0,
        Point::new(0, 10),
        "the command waited for a frame rather than being dropped"
    );
}

#[test]
fn layout_only_does_not_advance_a_ticker() {
    struct Ticked(Rc<RefCell<u32>>);

    impl Component<()> for Ticked {
        type State = ();
        fn init(&self, cx: &mut fresh_ui::InitCx<'_, ()>) {
            let n = self.0.clone();
            cx.register(fresh_ui::behavior::Ticker::new(move || {
                *n.borrow_mut() += 1
            }));
        }
        fn build(&self, _s: &(), _cx: &mut BuildCx<'_, ()>) -> Node<()> {
            text("tick")
        }
    }

    let ticks = Rc::new(RefCell::new(0));
    let mut ui: Ui<()> = Ui::new();
    ui.frame(Ticked(ticks.clone()).node(), GEOM);
    let after_frame = *ticks.borrow();

    ui.layout_only(Ticked(ticks.clone()).node(), GEOM);
    assert_eq!(
        *ticks.borrow(),
        after_frame,
        "a ticker counts frames; a geometry query is not one"
    );
}

#[test]
fn layout_only_leaves_the_display_list_alone() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(col().child(text("drawn")), GEOM);
    let painted = ui.spec().items.len();

    ui.layout_only(rows(), GEOM);
    assert_eq!(
        ui.spec().items.len(),
        painted,
        "the display list still describes what is on the screen"
    );
}

// -- lay_out / paint: a frame in two halves -----------------------------------
//
// A host leaf's paint may depend on a pass the application runs over the
// laid-out geometry — a text pane formats its rows for the rectangle layout
// gave it, and its caret is an answer of that pass. `lay_out` and `paint` let
// the host run that pass between the two, so the leaf paints from what the
// pass settled rather than a frame behind it.

/// A leaf whose paint places the cursor wherever the shared cell says.
struct Caret(Rc<RefCell<Option<fresh_ui::Point>>>);

impl fresh_ui::RenderObject for Caret {
    fn layout(&mut self, c: fresh_ui::Constraints, _cx: &mut dyn fresh_ui::LayoutCx) -> Size {
        c.constrain(c.max())
    }
    fn paint(&self, g: fresh_ui::Geom, out: &mut fresh_ui::DrawList) {
        out.push(fresh_ui::Draw::Fill, g);
        if let Some(at) = *self.0.borrow() {
            out.set_cursor(fresh_ui::Point::new(g.rect.x + at.x, g.rect.y + at.y));
        }
    }
    fn hit(&self, _local: fresh_ui::Point) -> fresh_ui::Hit {
        fresh_ui::Hit::Opaque
    }
    fn render_name(&self) -> &'static str {
        "Caret"
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

impl fresh_ui::HostLeaf for Caret {}

#[test]
fn a_pass_between_lay_out_and_paint_reaches_the_frames_own_display_list() {
    let caret: Rc<RefCell<Option<fresh_ui::Point>>> = Rc::new(RefCell::new(None));
    let shared = caret.clone();
    let factory: fresh_ui::HostObject = Rc::new(move || Box::new(Caret(shared.clone())));
    let tree = || {
        col().children([
            text("title").h(Sizing::Cells(1)),
            fresh_ui::host_object(factory.clone()).key("pane"),
        ])
    };

    let mut ui: Ui<()> = Ui::new();
    ui.lay_out(tree(), GEOM);
    // The pass: read where layout put the leaf, decide where the caret is.
    let rect = ui.rect_of(ui.find_by_key(&"pane".into()).unwrap());
    assert_eq!(
        rect,
        fresh_ui::Rect::new(0, 1, 20, 5),
        "laid out, not painted"
    );
    *caret.borrow_mut() = Some(fresh_ui::Point::new(3, 2));

    let spec = ui.paint();
    assert_eq!(
        spec.cursor.map(|c| c.pos),
        Some(fresh_ui::Point::new(3, 3)),
        "the display list carries what the pass settled, in this frame"
    );
    assert_eq!(
        ui.spec()
            .items
            .iter()
            .filter(|i| i.draw == fresh_ui::Draw::Fill)
            .count(),
        1,
        "and the leaf painted once"
    );
}

#[test]
fn lay_out_settles_what_a_frame_would() {
    let mut ui: Ui<()> = Ui::new();
    ui.lay_out(rows(), GEOM);
    assert_eq!(
        ui.focused(),
        ui.find_by_key(&"two".into()),
        "autofocus is a reaction to a frame, and this is a frame's first half"
    );
    assert!(
        ui.spec().items.is_empty(),
        "nothing is painted until the second half"
    );
    ui.paint();
    assert!(!ui.spec().items.is_empty());
}
