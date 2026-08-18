//! Hit-testing and propagation (plan phase L5).

use std::cell::RefCell;
use std::rc::Rc;

use fresh_ui::{
    col, gesture, stack, text, viewport, BuildCx, Component, ComponentExt, Event, GestureKind,
    Input, Mods, MouseButton, Node, Point, PointerMode, Size, Sizing, Ui,
};

const FRAME: Size = Size { w: 20, h: 10 };
type Log = Rc<RefCell<Vec<String>>>;

fn note(log: &Log, s: String) -> Option<()> {
    log.borrow_mut().push(s);
    None
}

/// Logs every phase it sees, on both passes.
fn traced(name: &'static str, log: &Log, child: Node<()>) -> Node<()> {
    let a = log.clone();
    let b = log.clone();
    gesture(child)
        .on_capture(
            GestureKind::Click,
            Rc::new(move |e: &Event| note(&a, format!("{name} {:?}", e.phase))),
        )
        .on(
            GestureKind::Click,
            Rc::new(move |e: &Event| note(&b, format!("{name} {:?}", e.phase))),
        )
}

fn click(ui: &mut Ui<()>, x: i32, y: i32) -> Vec<()> {
    let pos = Point::new(x, y);
    let mut out = ui.dispatch(Input::Press {
        pos,
        button: MouseButton::Left,
        mods: Mods::NONE,
    });
    out.extend(ui.dispatch(Input::Release {
        pos,
        button: MouseButton::Left,
        mods: Mods::NONE,
    }));
    out
}

#[test]
fn capture_runs_root_to_target_and_bubble_runs_target_to_root() {
    let log: Log = Rc::default();
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        traced(
            "outer",
            &log,
            col().child(traced("inner", &log, text("hit"))),
        ),
        FRAME,
    );
    click(&mut ui, 1, 0);

    // The deepest element hit is the TextRun, which carries no listeners, so
    // both gestures see the walk pass through them rather than stop.
    assert_eq!(
        *log.borrow(),
        vec![
            "outer Capture",
            "inner Capture",
            "inner Bubble",
            "outer Bubble",
        ]
    );
}

#[test]
fn the_deepest_hit_element_sees_the_target_phase() {
    let log: Log = Rc::default();
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col().child(
            traced("leaf", &log, Node::nil())
                .w(Sizing::Cells(5))
                .h(Sizing::Cells(1)),
        ),
        FRAME,
    );
    click(&mut ui, 1, 0);
    assert_eq!(*log.borrow(), vec!["leaf Target", "leaf Target"]);
}

#[test]
fn stopping_ends_propagation() {
    let log: Log = Rc::default();
    let l = log.clone();
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        traced(
            "outer",
            &log,
            col().child(gesture(text("hit")).on(
                GestureKind::Click,
                Rc::new(move |e: &Event| {
                    e.stop();
                    note(&l, "inner claimed".into())
                }),
            )),
        ),
        FRAME,
    );
    click(&mut ui, 1, 0);

    assert_eq!(
        *log.borrow(),
        vec!["outer Capture", "inner claimed"],
        "the outer bubble listener never runs"
    );
}

#[test]
fn an_opaque_sibling_above_blocks_what_is_behind() {
    let log: Log = Rc::default();
    let mut ui: Ui<()> = Ui::new();
    // A stack paints in order, so the second child is on top.
    ui.frame(
        stack().children([
            traced("behind", &log, text("xxxxx"))
                .w(Sizing::Cells(5))
                .h(Sizing::Cells(1)),
            traced("front", &log, text("yy"))
                .w(Sizing::Cells(2))
                .h(Sizing::Cells(1)),
        ]),
        FRAME,
    );

    click(&mut ui, 0, 0);
    assert!(log.borrow().iter().any(|s| s.starts_with("front")));
    assert!(!log.borrow().iter().any(|s| s.starts_with("behind")));

    // Past the front child's width, the one behind is reachable again.
    log.borrow_mut().clear();
    click(&mut ui, 4, 0);
    assert!(log.borrow().iter().any(|s| s.starts_with("behind")));
}

#[test]
fn a_transparent_region_runs_its_handlers_then_lets_the_hit_continue() {
    let log: Log = Rc::default();
    let mut ui: Ui<()> = Ui::new();
    let l = log.clone();
    ui.frame(
        stack().children([
            traced("behind", &log, text("xxxxxxxx"))
                .w(Sizing::Cells(8))
                .h(Sizing::Cells(1)),
            // Covers the same area, but claims none of it for itself.
            gesture(Node::nil())
                .pointer_mode(PointerMode::Transparent)
                .w(Sizing::Cells(8))
                .h(Sizing::Cells(1))
                .on(
                    GestureKind::Click,
                    Rc::new(move |_: &Event| note(&l, "front".into())),
                ),
        ]),
        FRAME,
    );
    click(&mut ui, 2, 0);
    // Its own handlers run, and *then* the hit continues to what is behind:
    // two stacked paths, in front-to-back order.
    assert_eq!(
        *log.borrow(),
        vec!["front", "behind Capture", "behind Bubble"]
    );
}

#[test]
fn capture_survives_the_pointer_leaving_the_rectangle() {
    let moves: Rc<RefCell<Vec<Point>>> = Rc::default();
    let m = moves.clone();
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col().children([
            gesture(text("grip"))
                .on(
                    GestureKind::Press,
                    Rc::new(move |e: &Event| {
                        e.capture_pointer();
                        None
                    }),
                )
                .on(
                    GestureKind::Move,
                    Rc::new(move |e: &Event| {
                        m.borrow_mut().push(e.pos);
                        None
                    }),
                ),
            text("elsewhere"),
        ]),
        FRAME,
    );

    ui.dispatch(Input::Press {
        pos: Point::new(1, 0),
        button: MouseButton::Left,
        mods: Mods::NONE,
    });
    assert!(ui.captured().is_some());

    // Far outside the grip's own rectangle, and on top of another element.
    ui.dispatch(Input::Move {
        pos: Point::new(5, 6),
        mods: Mods::NONE,
    });
    assert_eq!(*moves.borrow(), vec![Point::new(5, 6)]);

    ui.dispatch(Input::Release {
        pos: Point::new(5, 6),
        button: MouseButton::Left,
        mods: Mods::NONE,
    });
    assert!(ui.captured().is_none(), "release ends the drag");
}

#[test]
fn a_viewport_at_its_bound_lets_the_wheel_through() {
    let mut ui: Ui<()> = Ui::new();
    let inner_rows: Vec<Node<()>> = (0..4).map(|i| text(format!("in {i}"))).collect();
    let outer_rows: Vec<Node<()>> = (0..40).map(|i| text(format!("out {i}"))).collect();

    ui.frame(
        viewport(
            col().children(
                outer_rows
                    .into_iter()
                    .take(5)
                    .chain(std::iter::once(
                        viewport(col().children(inner_rows))
                            .w(Sizing::Cells(10))
                            .h(Sizing::Cells(2)),
                    ))
                    .chain((0..40).map(|i| text(format!("tail {i}"))))
                    .collect::<Vec<_>>(),
            ),
        ),
        FRAME,
    );

    let outer = ui.root().unwrap();
    let inner = ui.at(&[0, 5]).unwrap();

    // Over the inner viewport: it scrolls first.
    let over_inner = Point::new(1, 5);
    ui.dispatch(Input::Wheel {
        pos: over_inner,
        delta: 1,
        mods: Mods::NONE,
    });
    ui.tick();
    assert_eq!(ui.scroll(inner).0.y, 1);
    assert_eq!(ui.scroll(outer).0.y, 0, "the outer one has not moved");

    // Drive the inner one to its bound, then one more notch chains outward.
    ui.dispatch(Input::Wheel {
        pos: over_inner,
        delta: 1,
        mods: Mods::NONE,
    });
    ui.tick();
    assert_eq!(
        ui.scroll(inner).0.y,
        2,
        "at the bottom of four rows in a two-row window"
    );

    ui.dispatch(Input::Wheel {
        pos: over_inner,
        delta: 1,
        mods: Mods::NONE,
    });
    ui.tick();
    assert_eq!(ui.scroll(inner).0.y, 2, "still at its bound");
    assert_eq!(
        ui.scroll(outer).0.y,
        1,
        "so the wheel chained to the outer viewport"
    );
}

#[test]
fn preventing_the_default_suppresses_scrolling_without_claiming() {
    let mut ui: Ui<()> = Ui::new();
    let rows: Vec<Node<()>> = (0..40).map(|i| text(format!("row {i}"))).collect();
    ui.frame(
        viewport(gesture(col().children(rows)).on(
            GestureKind::Wheel,
            Rc::new(|e: &Event| {
                e.prevent_default();
                None
            }),
        )),
        FRAME,
    );
    let vp = ui.root().unwrap();
    ui.dispatch(Input::Wheel {
        pos: Point::new(1, 1),
        delta: 1,
        mods: Mods::NONE,
    });
    ui.tick();
    assert_eq!(ui.scroll(vp).0.y, 0);
}

// -- retargeting -------------------------------------------------------------

struct Button2(&'static str);

impl Component<()> for Button2 {
    type State = ();
    fn build(&self, _s: &(), _cx: &mut BuildCx<'_, ()>) -> Node<()> {
        gesture(text(self.0))
    }
}

#[test]
fn propagation_out_of_a_component_reports_the_component_as_the_target() {
    let seen: Rc<RefCell<Vec<(String, String)>>> = Rc::default();
    let s = seen.clone();
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        gesture(col().child(Button2("press me").node())).on(
            GestureKind::Click,
            Rc::new(move |e: &Event| {
                s.borrow_mut()
                    .push((format!("{:?}", e.target), format!("{:?}", e.current)));
                None
            }),
        ),
        FRAME,
    );
    click(&mut ui, 1, 0);

    let component = ui.at(&[0, 0]).unwrap();
    let outer = ui.root().unwrap();
    assert_eq!(
        *seen.borrow(),
        vec![(format!("{component:?}"), format!("{outer:?}"))],
        "the listener sees the button, not the TextRun inside it"
    );
}

#[test]
fn hover_transitions_fire_once_on_entering_and_leaving() {
    let log: Log = Rc::default();
    let (a, b) = (log.clone(), log.clone());
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col().children([
            gesture(text("one"))
                .on(
                    GestureKind::Enter,
                    Rc::new(move |_: &Event| note(&a, "enter".into())),
                )
                .on(
                    GestureKind::Leave,
                    Rc::new(move |_: &Event| note(&b, "leave".into())),
                ),
            text("two"),
        ]),
        FRAME,
    );

    ui.dispatch(Input::Move {
        pos: Point::new(1, 0),
        mods: Mods::NONE,
    });
    ui.dispatch(Input::Move {
        pos: Point::new(2, 0),
        mods: Mods::NONE,
    });
    assert_eq!(
        *log.borrow(),
        vec!["enter"],
        "moving within does not re-enter"
    );

    ui.dispatch(Input::Move {
        pos: Point::new(1, 1),
        mods: Mods::NONE,
    });
    assert_eq!(*log.borrow(), vec!["enter", "leave"]);
}

#[test]
fn pressing_and_dragging_the_scrollbar_gutter_scrolls_the_viewport() {
    let mut ui: Ui<()> = Ui::new();
    let rows: Vec<Node<()>> = (0..40).map(|i| text(format!("row {i}"))).collect();
    // Content taller than the window, with a scrollbar: the last column is the
    // gutter the framework drives.
    ui.frame(viewport(col().children(rows)).scrollbar(), FRAME);
    let vp = ui.root().unwrap();
    assert_eq!(ui.scroll(vp).0.y, 0);

    let gutter = FRAME.w as i32 - 1;
    // Press near the bottom of the track: the window jumps toward the end.
    ui.dispatch(Input::Press {
        pos: Point::new(gutter, FRAME.h as i32 - 1),
        button: MouseButton::Left,
        mods: Mods::NONE,
    });
    ui.tick();
    let jumped = ui.scroll(vp).0.y;
    assert!(jumped > 0, "a press on the gutter scrolled ({jumped})");

    // Still holding, drag to the top: the window follows back.
    ui.dispatch(Input::Move {
        pos: Point::new(gutter, 0),
        mods: Mods::NONE,
    });
    ui.tick();
    assert_eq!(ui.scroll(vp).0.y, 0, "dragging to the top scrolled back");

    // After release the gutter no longer drives scrolling.
    ui.dispatch(Input::Release {
        pos: Point::new(gutter, 0),
        button: MouseButton::Left,
        mods: Mods::NONE,
    });
    ui.dispatch(Input::Move {
        pos: Point::new(gutter, FRAME.h as i32 - 1),
        mods: Mods::NONE,
    });
    ui.tick();
    assert_eq!(
        ui.scroll(vp).0.y,
        0,
        "a bare move after release does not scroll"
    );
}
