//! Hit-testing and propagation (plan phase L5).

use std::cell::RefCell;
use std::rc::Rc;

use fresh_ui::Axis;
use fresh_ui::{
    col, gesture, row, stack, text, viewport, BuildCx, Component, ComponentExt, Event, GestureKind,
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
    let mut out = ui
        .dispatch(Input::press(pos, MouseButton::Left, Mods::NONE))
        .msgs;
    out.extend(
        ui.dispatch(Input::release(pos, MouseButton::Left, Mods::NONE))
            .msgs,
    );
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

/// **An observer above a transparent region hears the event once.**
///
/// Stacked paths share their upper reaches. Walking each in full offered the
/// event to those shared ancestors once per path, so a capture-phase observer
/// near the root — the way an application watches a channel without claiming
/// it — fired two, three, however many times the point happened to stack. The
/// extra paths exist for the elements *behind* the transparent one; the ones
/// above it are the same elements either way.
#[test]
fn an_observer_above_a_transparent_region_hears_one_event_once() {
    let log: Log = Rc::default();
    let mut ui: Ui<()> = Ui::new();
    let l = log.clone();
    ui.frame(
        gesture(
            stack().children([
                traced("behind", &log, text("xxxxxxxx"))
                    .w(Sizing::Cells(8))
                    .h(Sizing::Cells(1)),
                gesture(Node::nil())
                    .pointer_mode(PointerMode::Transparent)
                    .w(Sizing::Cells(8))
                    .h(Sizing::Cells(1)),
            ]),
        )
        .on_capture(
            GestureKind::Click,
            Rc::new(move |_: &Event| note(&l, "observer".into())),
        ),
        FRAME,
    );
    click(&mut ui, 2, 0);
    assert_eq!(
        log.borrow().iter().filter(|s| *s == "observer").count(),
        1,
        "one click, one call: {:?}",
        log.borrow()
    );
    // And what the extra path is for still happens.
    assert!(log.borrow().iter().any(|s| s.starts_with("behind")));
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

    ui.dispatch(Input::press(
        Point::new(1, 0),
        MouseButton::Left,
        Mods::NONE,
    ));
    assert!(ui.captured().is_some());

    // Far outside the grip's own rectangle, and on top of another element.
    ui.dispatch(Input::Move {
        pos: Point::new(5, 6),
        mods: Mods::NONE,
    });
    assert_eq!(*moves.borrow(), vec![Point::new(5, 6)]);

    ui.dispatch(Input::release(
        Point::new(5, 6),
        MouseButton::Left,
        Mods::NONE,
    ));
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
        axis: Axis::Vertical,
        mods: Mods::NONE,
    });
    ui.tick();
    assert_eq!(ui.scroll(inner).0.y, 1);
    assert_eq!(ui.scroll(outer).0.y, 0, "the outer one has not moved");

    // Drive the inner one to its bound, then one more notch chains outward.
    ui.dispatch(Input::Wheel {
        pos: over_inner,
        delta: 1,
        axis: Axis::Vertical,
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
        axis: Axis::Vertical,
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
        axis: Axis::Vertical,
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
    ui.dispatch(Input::press(
        Point::new(gutter, FRAME.h as i32 - 1),
        MouseButton::Left,
        Mods::NONE,
    ));
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
    ui.dispatch(Input::release(
        Point::new(gutter, 0),
        MouseButton::Left,
        Mods::NONE,
    ));
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

/// **A card list's track is grabbable along its whole length.**
///
/// An item-scrolling viewport counts its offset, its content and its window in
/// *items*, while the bar beside it is measured in cells. The two agree only
/// when an item is one cell tall, and the hit path read the track's height as
/// the window — so for a list of five-row cards the thumb it computed was
/// twice the length of the one painted, and a press in the top two-thirds of
/// the track read as landing *inside* it. A press inside the thumb picks it up
/// where it was touched and moves nothing, so clicking the track did nothing
/// at all.
#[test]
fn pressing_the_track_of_an_item_scrolling_viewport_scrolls_it() {
    // Eight five-cell items in a ten-cell frame: a window of two items, and
    // six items of travel behind it.
    let built = || {
        let rows: Vec<Node<()>> = (0..2).map(|i| text(format!("item {i}"))).collect();
        viewport(col().children(rows))
            .items(8)
            .item_rows(5)
            .scrollbar()
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(built(), FRAME);

    // The painted thumb is the window's share of the track, not the track.
    let bar = ui
        .spec()
        .items
        .iter()
        .find_map(|i| match i.draw {
            fresh_ui::Draw::Scrollbar {
                offset,
                content,
                window,
            } => Some((offset, content, u32::from(window), i.rect.h)),
            _ => None,
        })
        .expect("eight items in a window of two overflow");
    let (_, len) = fresh_ui::Draw::scrollbar_thumb(bar.0, bar.1, bar.2, bar.3);
    assert!(
        len < FRAME.h,
        "a thumb that fills its track cannot be dragged ({len} of {})",
        FRAME.h
    );

    // And a press on the bare track below it jumps the window.
    let vp = ui.root().unwrap();
    assert_eq!(ui.scroll(vp).0.y, 0);
    ui.dispatch(Input::press(
        Point::new(FRAME.w as i32 - 1, len as i32),
        MouseButton::Left,
        Mods::NONE,
    ));
    ui.tick();
    assert!(
        ui.scroll(vp).0.y > 0,
        "a press on the track scrolled ({})",
        ui.scroll(vp).0.y
    );
}

/// The chain honours the axis it is given — and the built-in `Viewport` has
/// nothing to move along `x`.
///
/// The scroll model has always been two-dimensional: a viewport's offset and
/// its maximum are both points, and the maximum's `x` is computed. But a
/// viewport lays its child out under `Constraints::new(0, w, ..)`, bounding the
/// child to the window's own width, so the content can never be wider than the
/// window and that maximum is always zero. `Viewport` is a vertical scroller by
/// construction.
///
/// That is deliberate and unchanged here. What this test pins is the boundary:
/// the routing understands both axes, so a horizontally scrollable viewport
/// would work the day one exists — and until then a horizontal wheel reaching a
/// viewport correctly does nothing rather than scrolling the wrong way.
#[test]
fn a_viewport_has_no_horizontal_extent_to_scroll() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        viewport(text("x".repeat(60)))
            .w(Sizing::Cells(10))
            .h(Sizing::Cells(1)),
        FRAME,
    );
    let vp = ui.root().unwrap();
    let (offset, content) = ui.scroll(vp);
    assert_eq!(offset, Point::new(0, 0));
    assert!(
        content.w <= 10,
        "the child is bounded to the window's width, so there is no overflow \
         to scroll: content {content:?}"
    );

    ui.dispatch(Input::Wheel {
        pos: Point::new(2, 0),
        delta: 3,
        axis: Axis::Horizontal,
        mods: Mods::NONE,
    });
    assert_eq!(
        ui.scroll(vp).0,
        Point::new(0, 0),
        "nothing to scroll, and in particular the vertical offset is untouched"
    );
}

/// A vertical wheel still scrolls, and leaves `x` alone.
#[test]
fn a_vertical_wheel_moves_only_y() {
    let mut ui: Ui<()> = Ui::new();
    let rows: Vec<Node<()>> = (0..40).map(|i| text(format!("row {i}"))).collect();
    ui.frame(
        viewport(col().children(rows))
            .w(Sizing::Cells(10))
            .h(Sizing::Cells(4)),
        FRAME,
    );
    let vp = ui.root().unwrap();

    ui.dispatch(Input::Wheel {
        pos: Point::new(2, 2),
        delta: 2,
        axis: Axis::Vertical,
        mods: Mods::NONE,
    });
    assert_eq!(ui.scroll(vp).0, Point::new(0, 2));
}

/// A `Wheel` listener can tell the axes apart, so a widget that handles its own
/// scrolling is not forced to guess.
#[test]
fn a_wheel_listener_sees_the_axis() {
    let seen: Rc<RefCell<Vec<(i32, Axis)>>> = Rc::new(RefCell::new(Vec::new()));
    let s = seen.clone();
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        gesture(text("row")).on(
            GestureKind::Wheel,
            Rc::new(move |e: &Event| {
                s.borrow_mut().push((e.delta, e.axis));
                e.stop();
                None
            }),
        ),
        FRAME,
    );

    for (delta, axis) in [(1, Axis::Vertical), (-2, Axis::Horizontal)] {
        ui.dispatch(Input::Wheel {
            pos: Point::new(1, 0),
            delta,
            axis,
            mods: Mods::NONE,
        });
    }
    assert_eq!(
        *seen.borrow(),
        vec![(1, Axis::Vertical), (-2, Axis::Horizontal)]
    );
}

// -- what a dispatch reports -------------------------------------------------

/// Claiming and saying something are different answers, and a host routing
/// between this tree and an older pipeline of its own needs both.
#[test]
fn a_claim_is_reported_separately_from_the_messages() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        gesture(text("row")).on(
            GestureKind::Press,
            Rc::new(|e: &Event| {
                // Claims, and has nothing to say about it.
                e.stop();
                None
            }),
        ),
        FRAME,
    );
    let d = ui.dispatch(Input::press(
        Point::new(1, 0),
        MouseButton::Left,
        Mods::NONE,
    ));
    assert!(d.msgs.is_empty(), "the handler returned no message");
    assert!(d.claimed, "but it claimed the press");
}

/// And the converse: a message without a claim.
#[test]
fn a_message_without_a_claim_is_reported_as_unclaimed() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        gesture(text("row")).on(GestureKind::Press, Rc::new(|_: &Event| Some(()))),
        FRAME,
    );
    let d = ui.dispatch(Input::press(
        Point::new(1, 0),
        MouseButton::Left,
        Mods::NONE,
    ));
    assert_eq!(d.msgs.len(), 1);
    assert!(!d.claimed, "producing a message is not claiming the event");
}

// ---------------------------------------------------------------------------
// A pointer mode on any node, not only a gesture
// ---------------------------------------------------------------------------

/// A container that draws nothing still absorbs a press by default — "a region
/// that draws is a region that hits" describes the intent, not the mechanism,
/// and the mechanism has always been "everything hits unless it says
/// otherwise". This pins the default so the opt-out below is meaningful.
#[test]
fn a_plain_container_over_a_target_absorbs_the_press() {
    let log: Log = Rc::default();
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        stack().children([
            traced("behind", &log, text("xxxxxxxx"))
                .w(Sizing::Cells(8))
                .h(Sizing::Cells(1)),
            // No theme, no listeners, nothing drawn: still a surface.
            col().w(Sizing::Cells(5)).h(Sizing::Cells(1)),
        ]),
        FRAME,
    );
    click(&mut ui, 2, 0);
    assert!(
        !log.borrow().iter().any(|s| s.starts_with("behind")),
        "the plain container absorbed it: {:?}",
        log.borrow()
    );
    // …and the target really is reachable, so the assertion above is about the
    // container and not about a click that landed on nothing.
    log.borrow_mut().clear();
    click(&mut ui, 6, 0);
    assert!(
        log.borrow().iter().any(|s| s.starts_with("behind")),
        "past the container, the target answers: {:?}",
        log.borrow()
    );
}

/// …and the opt-out reaches an ordinary container, not just a `Gesture`.
///
/// This is the shape every overlay strip takes: a full-size column positioning
/// a one-row band over content that must stay reachable. Before `pointer_mode`
/// applied to any node it could not be written — wrapping the column in a
/// transparent gesture made the *wrapper* transparent and left the column
/// itself absorbing everything.
#[test]
fn a_transparent_container_lets_the_press_reach_what_is_behind() {
    let log: Log = Rc::default();
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        stack().children([
            traced("behind", &log, text("xxxxx"))
                .w(Sizing::Cells(5))
                .h(Sizing::Cells(1)),
            col()
                .pointer_mode(PointerMode::Transparent)
                .w(Sizing::Cells(5))
                .h(Sizing::Cells(1)),
        ]),
        FRAME,
    );
    click(&mut ui, 2, 0);
    assert!(
        log.borrow().iter().any(|s| s.starts_with("behind")),
        "got {:?}",
        log.borrow()
    );
}

/// Transparency is per node, not inherited: a strip can pass presses through
/// while one control inside it still takes them. That is the whole point — an
/// overlay is mostly decoration with a button in it.
#[test]
fn an_opaque_child_of_a_transparent_strip_still_absorbs() {
    let log: Log = Rc::default();
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        stack().children([
            traced("behind", &log, text("xxxxxxxx"))
                .w(Sizing::Cells(8))
                .h(Sizing::Cells(1)),
            col()
                .pointer_mode(PointerMode::Transparent)
                .w(Sizing::Cells(8))
                .h(Sizing::Cells(1))
                .children([fresh_ui::row()
                    .pointer_mode(PointerMode::Transparent)
                    .h(Sizing::Cells(1))
                    .children([
                        // The spacer has to say so too — transparency is a
                        // property of a node, not something its parent grants
                        // it. Leave this opaque and the strip swallows its own
                        // left half.
                        fresh_ui::row()
                            .pointer_mode(PointerMode::Transparent)
                            .w(Sizing::Cells(4)),
                        traced("button", &log, text("ok")).w(Sizing::Cells(2)),
                    ])]),
        ]),
        FRAME,
    );
    click(&mut ui, 1, 0);
    assert!(
        log.borrow().iter().any(|s| s.starts_with("behind"))
            && !log.borrow().iter().any(|s| s.starts_with("button")),
        "off the button: {:?}",
        log.borrow()
    );
    log.borrow_mut().clear();
    click(&mut ui, 5, 0);
    assert!(
        log.borrow().iter().any(|s| s.starts_with("button")),
        "on the button: {:?}",
        log.borrow()
    );
}

/// A bound is a statement about input as well as paint. The same over-constrained
/// row as in `paint.rs`: the status slot is placed on the frame's own column, so
/// without a bound a press there reaches the slot instead of the box that drew
/// the frame — a resize grip losing its column to a one-cell label.
#[test]
fn a_bound_keeps_a_press_from_reaching_what_was_clipped_away() {
    let log: Log = Rc::default();
    let mut ui: Ui<()> = Ui::new();
    let build =
        |log: &Log, clip: bool| {
            col().w(Sizing::Cells(10)).border().clip(clip).child(
                row().h(Sizing::Cells(1)).children([
                    text("a-name!").w(Sizing::Cells(7)),
                    row().flex(1).min_w(1),
                    traced("slot", log, text("M").w(Sizing::Cells(1))),
                ]),
            )
        };

    // Unbounded, x=9 is the slot's.
    ui.frame(build(&log, false), FRAME);
    click(&mut ui, 9, 1);
    assert!(
        log.borrow().iter().any(|s| s.starts_with("slot")),
        "without a bound the escaped cell is the slot's"
    );

    // Bounded — the default under `border()` — it is not.
    log.borrow_mut().clear();
    let mut ui: Ui<()> = Ui::new();
    ui.frame(build(&log, true), FRAME);
    click(&mut ui, 9, 1);
    assert!(
        log.borrow().is_empty(),
        "a bounded child takes no input outside the bound: {:?}",
        log.borrow()
    );

    // And the cells it legitimately occupies still answer.
    log.borrow_mut().clear();
    click(&mut ui, 3, 1);
    assert!(
        log.borrow().is_empty(),
        "the name's cells are not the slot's"
    );
}

/// **The thumb's top lands on the row pressed.**
///
/// Its travel is the part of the track it can reach — `track - len`, not the
/// whole track — and dividing by the track instead leaves the thumb short of
/// the row by up to `len` cells and the last row of the track unable to reach
/// the end of the content. A one-cell thumb hides the bug (`track - 1` and
/// `track - len` agree there), so this uses a window big enough for a thumb
/// several rows tall.
#[test]
fn pressing_a_track_row_puts_the_thumb_on_that_row() {
    let mut ui: Ui<()> = Ui::new();
    // Twice the window's height of content: the thumb is half the track.
    let rows: Vec<Node<()>> = (0..(FRAME.h as usize * 2))
        .map(|i| text(format!("row {i}")))
        .collect();
    ui.frame(viewport(col().children(rows)).scrollbar(), FRAME);

    let thumb = |ui: &Ui<()>| -> (u16, u16) {
        let bar = ui
            .spec()
            .items
            .iter()
            .find_map(|i| match i.draw {
                fresh_ui::Draw::Scrollbar {
                    offset,
                    content,
                    window,
                } => Some((offset, content, u32::from(window), i.rect.h)),
                _ => None,
            })
            .expect("an overflowing viewport shows a bar");
        fresh_ui::Draw::scrollbar_thumb(bar.0, bar.1, bar.2, bar.3)
    };

    let (_, len) = thumb(&ui);
    assert!(
        len > 1,
        "the thumb must be taller than one cell to be a test"
    );
    let gutter = FRAME.w as i32 - 1;
    let press_at = |ui: &mut Ui<()>, row: u16| {
        ui.dispatch(Input::press(
            Point::new(gutter, row as i32),
            MouseButton::Left,
            Mods::NONE,
        ));
        ui.dispatch(Input::release(
            Point::new(gutter, row as i32),
            MouseButton::Left,
            Mods::NONE,
        ));
        ui.tick();
    };
    // **Bare track only.** A press that lands *on* the thumb picks it up
    // where it was touched and moves nothing — see
    // `a_press_inside_the_thumb_grabs_it_rather_than_jumping` — so the rows
    // the resting thumb covers are not track presses and cannot be asserted
    // about here. Row 0 is inside it, and doubles as the reset: from anywhere
    // else it is bare track, and jumping there returns the thumb to the top.
    for target in len..=(FRAME.h - len) {
        press_at(&mut ui, 0);
        assert_eq!(thumb(&ui).0, 0, "row 0 is bare track once scrolled away");
        press_at(&mut ui, target);
        assert_eq!(
            thumb(&ui).0,
            target,
            "pressing track row {target} must put the thumb there"
        );
    }
}

/// **Closing a layer is not always the whole gesture.**
///
/// A click outside a menu is spent closing it — the menu was in the way. A
/// tooltip is in the way of nothing: clicking into the document while one is
/// showing should hide it *and* put the caret where the click landed, or the
/// tooltip has charged the user a click to get rid of it.
#[test]
fn a_pass_through_dismissal_leaves_the_press_for_what_it_was_aimed_at() {
    let log: Log = Rc::new(RefCell::new(Vec::new()));
    let build = |pass: bool, log: &Log| -> Node<()> {
        let l = log.clone();
        let d = match pass {
            true => fresh_ui::Dismiss::OUTSIDE_POINTER.passing_through(),
            false => fresh_ui::Dismiss::OUTSIDE_POINTER,
        };
        let ll = log.clone();
        stack().children([
            // What the press was aimed at, behind the layer.
            gesture(col().theme("doc")).on(
                GestureKind::Press,
                Rc::new(move |_: &Event| note(&ll, "document".into())),
            ),
            fresh_ui::layer()
                .anchor(fresh_ui::Anchor::Point(0, 0))
                .place(fresh_ui::Place::Over)
                .dismiss(d)
                .on_dismiss_handler(Rc::new(move |_: &Event| note(&l, "dismissed".into())))
                .child(col().w(Sizing::Cells(4)).h(Sizing::Cells(2)).theme("pop")),
        ])
    };

    // The claim is what the *host* is told — a host with its own pipeline
    // behind this tree reads it to decide whether the press is still going.
    // The tree's own handlers run either way, which is why the document's
    // press is logged in both.
    let press = |ui: &mut Ui<()>| {
        ui.dispatch(Input::press(
            Point::new(10, 6),
            MouseButton::Left,
            Mods::NONE,
        ))
        .claimed
    };

    // Spending the press: the host is told it is gone.
    let mut ui: Ui<()> = Ui::new();
    ui.frame(build(false, &log), FRAME);
    assert!(press(&mut ui), "closing the layer was the whole gesture");
    assert_eq!(*log.borrow(), vec!["dismissed", "document"]);

    // Passing it through: dismissed, and the host still has the press.
    log.borrow_mut().clear();
    let mut ui: Ui<()> = Ui::new();
    ui.frame(build(true, &log), FRAME);
    assert!(!press(&mut ui), "the press is still going somewhere");
    assert_eq!(*log.borrow(), vec!["dismissed", "document"]);
}

/// **And the same rule for the key that dismissed it.**
///
/// `pass_through` said "the input that dismissed this layer goes on to whatever
/// it was aimed at", and the pointer honoured it while the keyboard did not:
/// any dismissal at all reported the key as claimed. So a tooltip that hides on
/// the next keystroke ate that keystroke — the tooltip charging the user a key
/// to get rid of it, which is exactly the case the flag exists for.
#[test]
fn a_pass_through_dismissal_leaves_the_key_for_what_it_was_aimed_at() {
    let build = |pass: bool| -> Node<()> {
        let d = match pass {
            true => fresh_ui::Dismiss::ANY_KEY.passing_through(),
            false => fresh_ui::Dismiss::ANY_KEY,
        };
        stack().children([
            col().theme("doc"),
            fresh_ui::layer()
                .anchor(fresh_ui::Anchor::Point(0, 0))
                .place(fresh_ui::Place::Over)
                .dismiss(d)
                .on_dismiss_handler(Rc::new(|_: &Event| None))
                .child(col().w(Sizing::Cells(4)).h(Sizing::Cells(2)).theme("pop")),
        ])
    };
    let typed = |ui: &mut Ui<()>| {
        ui.dispatch(Input::Key(fresh_ui::KeyPress::with(
            fresh_ui::KeyCode::Char('j'),
            Mods::NONE,
        )))
        .claimed
    };

    let mut ui: Ui<()> = Ui::new();
    ui.frame(build(false), FRAME);
    assert!(typed(&mut ui), "closing the layer was the whole keystroke");

    let mut ui: Ui<()> = Ui::new();
    ui.frame(build(true), FRAME);
    assert!(!typed(&mut ui), "the key is still going somewhere");
}

/// **The thing a menu hangs off is not outside it.**
///
/// A press on the trigger that opened a list is one gesture — close it — and
/// every menu on every platform reads it that way. Counting the trigger as
/// outside makes the press do two things: the dismissal closes the layer, and
/// the trigger's own press, which runs immediately after, toggles it straight
/// back open. The list never closes.
///
/// Only [`Anchor::Node`] is honoured, because a parent is wherever the caller
/// happened to attach the layer — as often a whole panel body as a single row
/// — and suppressing the dismissal over a body would leave no outside at all.
#[test]
fn a_press_on_the_node_a_layer_is_anchored_to_does_not_dismiss_it() {
    let log: Log = Rc::new(RefCell::new(Vec::new()));
    let anchor = fresh_ui::Key::Str("trigger-row".into());
    let build = |log: &Log, anchor: &fresh_ui::Key| -> Node<()> {
        let l = log.clone();
        let t = log.clone();
        stack().children([
            // Somewhere barren, to prove the dismissal still works at all.
            col().theme("doc"),
            // The trigger and its list, under one named row — which is what
            // the editor's dropdown pop-over does.
            row().key(anchor.clone()).h(Sizing::Cells(1)).children([
                gesture(text("[value]")).on(
                    GestureKind::Press,
                    Rc::new(move |_: &Event| note(&t, "trigger".into())),
                ),
                fresh_ui::layer()
                    .anchor(fresh_ui::Anchor::Node(anchor.clone()))
                    .place(fresh_ui::Place::Below)
                    .dismiss(fresh_ui::Dismiss::OUTSIDE_POINTER)
                    .on_dismiss_handler(Rc::new(move |_: &Event| note(&l, "dismissed".into())))
                    .child(col().w(Sizing::Cells(6)).h(Sizing::Cells(3)).theme("pop")),
            ]),
        ])
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(build(&log, &anchor), FRAME);

    // The trigger sits at row 0; pressing it reaches the trigger and does not
    // dismiss, so the host's own toggle closes the list exactly once.
    ui.dispatch(Input::press(
        Point::new(2, 0),
        MouseButton::Left,
        Mods::NONE,
    ));
    assert!(
        log.borrow().contains(&"trigger".to_string()),
        "the trigger saw its own press: {:?}",
        log.borrow()
    );
    assert!(
        !log.borrow().contains(&"dismissed".to_string()),
        "and the layer did not also dismiss itself: {:?}",
        log.borrow()
    );

    // Somewhere barren still dismisses — the exclusion is the anchor, not the
    // whole frame.
    log.borrow_mut().clear();
    ui.dispatch(Input::press(
        Point::new(18, 8),
        MouseButton::Left,
        Mods::NONE,
    ));
    assert!(
        log.borrow().contains(&"dismissed".to_string()),
        "a press away from both closes it: {:?}",
        log.borrow()
    );
}

// -- Event::text_byte ---------------------------------------------------------

/// A press on text reports the byte, not the column.
///
/// The two are the same number only while every character is one byte and one
/// cell. `名前` is two characters, six bytes and four cells, so a caller that
/// used the column would be three bytes out by the fourth cell — which is
/// exactly the arithmetic this field exists to remove from callers.
#[test]
fn a_press_on_text_reports_the_byte_under_it() {
    let seen: Rc<RefCell<Vec<Option<usize>>>> = Rc::new(RefCell::new(Vec::new()));
    let sink = seen.clone();
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col().child(gesture(text("名前: value")).on(
            GestureKind::Press,
            Rc::new(move |e: &Event| {
                sink.borrow_mut().push(e.text_byte);
                None
            }),
        )),
        FRAME,
    );
    // Cells:  0,1 = 名   2,3 = 前   4 = ':'  5 = ' '  6.. = "value"
    // Bytes:  0    = 名   3    = 前   6 = ':'  7 = ' '  8.. = "value"
    for (col, want) in [(0, 0), (1, 0), (2, 3), (3, 3), (4, 6), (6, 8), (8, 10)] {
        ui.dispatch(Input::press(
            Point::new(col, 0),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert_eq!(
            seen.borrow().last().copied().flatten(),
            Some(want),
            "column {col} should be byte {want}"
        );
    }
}

/// Past the end of the run is the end of the run, and a press on something
/// that is not text has no byte at all.
#[test]
fn text_byte_is_absent_where_there_is_no_text() {
    let seen: Rc<RefCell<Vec<Option<usize>>>> = Rc::new(RefCell::new(Vec::new()));
    let sink = seen.clone();
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col()
            .child(gesture(text("ab")).w(Sizing::Cells(10)).on(
                GestureKind::Press,
                Rc::new({
                    let sink = sink.clone();
                    move |e: &Event| {
                        sink.borrow_mut().push(e.text_byte);
                        None
                    }
                }),
            ))
            .child(
                gesture(col().w(Sizing::Cells(10)).h(Sizing::Cells(1)))
                    .on(
                        GestureKind::Press,
                        Rc::new(move |e: &Event| {
                            sink.borrow_mut().push(e.text_byte);
                            None
                        }),
                    )
                    .h(Sizing::Cells(1)),
            ),
        FRAME,
    );
    ui.dispatch(Input::press(
        Point::new(7, 0),
        MouseButton::Left,
        Mods::NONE,
    ));
    assert_eq!(
        seen.borrow().last().copied().flatten(),
        Some(2),
        "a press past the last character is the end of the string"
    );
    ui.dispatch(Input::press(
        Point::new(3, 1),
        MouseButton::Left,
        Mods::NONE,
    ));
    assert_eq!(
        seen.borrow().last().copied().flatten(),
        None,
        "a box is not text and has no byte"
    );
}

/// **A press on wrapped text reports a byte too.**
///
/// A row of wrapped text is not a slice of the source — the break ate the
/// space between "world" and "here" — so the byte under a press is not the
/// column plus the bytes of the rows above it. The run says which bytes each
/// row is, and the press resolves against the row it landed on; the caller
/// never has to know a wrap happened.
#[test]
fn a_press_on_a_wrapped_run_reports_the_byte_on_the_row_it_landed_on() {
    let seen: Rc<RefCell<Vec<Option<usize>>>> = Rc::new(RefCell::new(Vec::new()));
    let sink = seen.clone();
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col().child(
            gesture(text("hello world here").wrap().w(Sizing::Cells(11)))
                .on(
                    GestureKind::Press,
                    Rc::new(move |e: &Event| {
                        sink.borrow_mut().push(e.text_byte);
                        None
                    }),
                )
                .h(Sizing::Cells(2)),
        ),
        FRAME,
    );
    // Rows: "hello world" (bytes 0..11) and "here" (bytes 12..16). Byte 11 is
    // the space the break ate and no row shows it.
    for (at, want) in [
        (Point::new(0, 0), 0),
        (Point::new(6, 0), 6),
        (Point::new(10, 0), 10),
        (Point::new(0, 1), 12),
        (Point::new(3, 1), 15),
        (Point::new(4, 1), 16),
    ] {
        ui.dispatch(Input::press(at, MouseButton::Left, Mods::NONE));
        assert_eq!(
            seen.borrow().last().copied().flatten(),
            Some(want),
            "a press at {at:?} should be byte {want}"
        );
    }
}
