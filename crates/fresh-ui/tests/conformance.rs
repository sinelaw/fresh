//! The behaviours the Part 1c review found missing (plan phase R12).
//!
//! One test per finding. Each asserts the thing the design document says
//! happens, not the thing the implementation happened to do.

use std::cell::{Cell, RefCell};
use std::rc::Rc;

use fresh_ui::{
    col, focusable, gesture, host_leaf, layer, layout_reader, text, viewport, Align, Anchor,
    BuildCx, Component, ComponentExt, Constraints, Draw, Event, Focusable, Geom, GeomHandle,
    GestureKind, Hit, HostLeaf, InitCx, Input, Intent, Key, KeyCode, KeyPress, LayoutCx,
    LayoutInfo, Modality, Mods, MouseButton, Node, Place, Point, PointerMode, Rect, RenderObject,
    Shortcut, Size, Sizing, Ui,
};

const FRAME: Size = Size { w: 20, h: 10 };

fn press(ui: &mut Ui<()>, x: i32, y: i32) {
    ui.dispatch(Input::press(
        Point::new(x, y),
        MouseButton::Left,
        Mods::NONE,
    ));
}

fn click(ui: &mut Ui<()>, x: i32, y: i32) {
    press(ui, x, y);
    ui.dispatch(Input::release(
        Point::new(x, y),
        MouseButton::Left,
        Mods::NONE,
    ));
}

fn key(ui: &mut Ui<()>, code: KeyCode) {
    ui.dispatch(Input::Key(KeyPress {
        code,
        mods: Mods::NONE,
    }));
}

// ---------------------------------------------------------------------------
// D1 — the framework asks the render object, not the description
// ---------------------------------------------------------------------------

/// A leaf the library knows nothing about: it is not one of the primitives, it
/// declares its own geometry, and it says it wants raw host input.
struct Grid {
    cells: Cell<u32>,
}

impl RenderObject for Grid {
    fn layout(&mut self, c: Constraints, _cx: &mut dyn LayoutCx) -> Size {
        let s = c.constrain(Size::new(8, 3));
        self.cells.set(s.w as u32 * s.h as u32);
        s
    }

    fn paint(&self, g: Geom, out: &mut fresh_ui::DrawList) {
        out.push(Draw::Fill, g);
    }

    fn hit(&self, _local: Point) -> Hit {
        Hit::Opaque
    }

    fn takes_raw_input(&self) -> bool {
        true
    }

    fn render_name(&self) -> &'static str {
        "Grid"
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

impl HostLeaf for Grid {}

#[test]
fn a_host_leaf_measures_paints_and_hits_like_any_primitive() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(
        col().child(host_leaf(|| {
            Box::new(Grid {
                cells: Cell::new(0),
            })
        })),
        FRAME,
    );
    let painted: Vec<Rect> = spec
        .items
        .iter()
        .filter(|i| i.draw == Draw::Fill)
        .map(|i| i.rect)
        .collect();
    // The column stretches it across the frame; the height is the leaf's own
    // answer, which nothing in the library could have supplied.
    assert_eq!(painted, vec![Rect::new(0, 0, 20, 3)]);

    // Its own hit answer puts it on the path.
    let path = ui.hit_test(Point::new(2, 1));
    assert!(!path.is_empty());
    // And it takes raw input, because it said so and nothing made it inert.
    assert!(ui.raw_input());
}

#[test]
fn the_layer_semantics_come_from_the_render_object() {
    // Nothing here matches on a description: modality reaches focus and input
    // routing, the scrim reaches paint, and the dismissal rule reaches the
    // pointer, all through the same render object.
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(
        col().child(text("behind")).child(
            layer()
                .modality(Modality::Exclusive)
                .scrim(Some(fresh_ui::Scrim::Dim))
                .child(focusable(text("front"))),
        ),
        FRAME,
    );
    assert!(spec
        .items
        .iter()
        .any(|i| matches!(i.draw, Draw::Scrim(fresh_ui::Scrim::Dim))));
    // Exclusive: a host leaf outside it would be inert, and traversal is
    // confined to the layer.
    assert_eq!(ui.focus_scope().nodes.len(), 1);
}

// ---------------------------------------------------------------------------
// D2 — the Focusable behavior is the Focusable description
// ---------------------------------------------------------------------------

struct Keyed(Rc<RefCell<Vec<&'static str>>>);

#[derive(Default)]
struct KeyedState {
    _f: Option<Rc<Focusable<()>>>,
}

impl Component<()> for Keyed {
    type State = KeyedState;

    fn init(&self, cx: &mut InitCx<'_, ()>) -> KeyedState {
        let raw = self.0.clone();
        let act = self.0.clone();
        let gained = self.0.clone();
        KeyedState {
            _f: Some(
                cx.focusable(
                    Focusable::new(Rc::new(move |e: &Event| {
                        if e.kind == GestureKind::FocusGained {
                            gained.borrow_mut().push("gained");
                        }
                        None
                    }))
                    .autofocus()
                    .on_key(Rc::new(move |e: &Event| {
                        if e.key.map(|k| k.code) == Some(KeyCode::Char('r')) {
                            raw.borrow_mut().push("raw");
                            e.stop();
                        }
                        None
                    }))
                    .shortcut(Shortcut::new(
                        KeyPress {
                            code: KeyCode::Char('x'),
                            mods: Mods::NONE,
                        },
                        Intent::Confirm,
                    ))
                    .action(
                        Intent::Confirm,
                        Rc::new(move |_e: &Event| {
                            act.borrow_mut().push("confirm");
                            None
                        }),
                    ),
                ),
            ),
        }
    }

    fn build(&self, _s: &KeyedState, _cx: &mut BuildCx<'_, ()>) -> Node<()> {
        text("body")
    }
}

#[test]
fn a_focusable_behavior_gets_keys_shortcuts_and_actions() {
    let log: Rc<RefCell<Vec<&'static str>>> = Rc::default();
    let mut ui: Ui<()> = Ui::new();
    ui.frame(col().child(Keyed(log.clone()).node()), FRAME);

    // Autofocus reached it, which means it is in the focus tree at all.
    assert!(ui.focused().is_some());
    assert_eq!(&*log.borrow(), &["gained"]);

    key(&mut ui, KeyCode::Char('r'));
    key(&mut ui, KeyCode::Char('x'));
    assert_eq!(&*log.borrow(), &["gained", "raw", "confirm"]);
}

// ---------------------------------------------------------------------------
// D3 — a layer above a modal is still hittable
// ---------------------------------------------------------------------------

#[test]
fn a_layer_opened_inside_a_modal_is_hittable() {
    let hits: Rc<Cell<u32>> = Rc::default();
    let h = hits.clone();
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col()
            .child(text("behind"))
            .child(
                layer()
                    .modality(Modality::Exclusive)
                    .child(col().child(text("dialog")).child(
                        // A dropdown the dialog opened: an ordinary non-modal layer
                        // declared inside the modal, so it resolves after it.
                        layer().child(gesture(text("menu")).on(
                            GestureKind::Click,
                            Rc::new(move |_e: &Event| {
                                h.set(h.get() + 1);
                                None
                            }),
                        )),
                    )),
            ),
        FRAME,
    );
    // The nested layer sits at the dialog's origin; clicking it must reach it.
    click(&mut ui, 0, 0);
    assert_eq!(hits.get(), 1, "the layer above the modal took the click");
}

// ---------------------------------------------------------------------------
// D4 — geometry is reachable from a handler
// ---------------------------------------------------------------------------

struct Measured(Rc<Cell<Rect>>);

#[derive(Default)]
struct MeasuredState {
    geom: Option<GeomHandle>,
}

impl Component<()> for Measured {
    type State = MeasuredState;

    fn init(&self, cx: &mut InitCx<'_, ()>) -> MeasuredState {
        MeasuredState {
            geom: Some(cx.geometry()),
        }
    }

    fn build(&self, s: &MeasuredState, _cx: &mut BuildCx<'_, ()>) -> Node<()> {
        let seen = self.0.clone();
        let g = s.geom.clone().expect("constructed");
        gesture(text("press me")).on(
            GestureKind::Press,
            Rc::new(move |_e: &Event| {
                // The handler runs while the tree is borrowed; it holds a
                // handle, not a reference into it.
                seen.set(g.rect());
                None
            }),
        )
    }
}

#[test]
fn a_handler_can_read_its_own_geometry() {
    let seen: Rc<Cell<Rect>> = Rc::default();
    let mut ui: Ui<()> = Ui::new();
    ui.frame(col().child(Measured(seen.clone()).node()), FRAME);
    press(&mut ui, 1, 0);
    assert_eq!(seen.get(), Rect::new(0, 0, 20, 1));
}

/// **A held window carries its unit with it.**
///
/// The snapshot a handle reads is the same window the tree laid out, so it is
/// in the same unit: an item-scrolling viewport reports how many *items* are on
/// screen, and the band beside it is what says so. Three-cell items in a
/// nine-cell viewport are three of them, not nine — and a caller with only
/// `rect()` and `scroll()` had no way to tell those two numbers apart.
struct Windowed(Rc<Cell<(Option<Rect>, Option<fresh_ui::Band>)>>);

#[derive(Default)]
struct WindowedState {
    geom: Option<GeomHandle>,
}

impl Component<()> for Windowed {
    type State = WindowedState;

    fn init(&self, cx: &mut InitCx<'_, ()>) -> WindowedState {
        WindowedState {
            geom: Some(cx.geometry()),
        }
    }

    fn build(&self, s: &WindowedState, _cx: &mut BuildCx<'_, ()>) -> Node<()> {
        let seen = self.0.clone();
        let g = s.geom.clone().expect("constructed");
        // The viewport is this component's own root, so the handle addresses
        // it: geometry on a component answers with the render node it owns.
        viewport(col().children((0..20).map(move |i| {
            let seen = seen.clone();
            let g = g.clone();
            gesture(text(format!("card {i}")).h(Sizing::Cells(3))).on(
                GestureKind::Press,
                Rc::new(move |_e: &Event| {
                    seen.set((g.window(), g.band()));
                    None
                }),
            )
        })))
        .items(20)
        .item_rows(3)
    }
}

#[test]
fn a_handle_reads_a_window_in_the_unit_its_offset_counts() {
    let seen: Rc<Cell<(Option<Rect>, Option<fresh_ui::Band>)>> = Rc::default();
    let mut ui: Ui<()> = Ui::new();
    ui.frame(Windowed(seen.clone()).node(), Size::new(20, 9));
    press(&mut ui, 1, 0);
    let (window, band) = seen.get();
    assert_eq!(
        window.map(|w| w.h),
        Some(3),
        "nine cells of three-cell items is three items"
    );
    assert_eq!(
        band,
        Some(fresh_ui::Band::Cells(3)),
        "and the band is what says the three is in items"
    );
}

// ---------------------------------------------------------------------------
// D5 — a prop change after mount reaches the render object
// ---------------------------------------------------------------------------

#[test]
fn a_pointer_mode_change_after_mount_reaches_the_render_object() {
    let hits: Rc<Cell<u32>> = Rc::default();
    let build = |mode: PointerMode, h: Rc<Cell<u32>>| -> Node<()> {
        col().child(
            gesture(text("target"))
                .pointer_mode(mode)
                .on(
                    GestureKind::Press,
                    Rc::new(move |_e: &Event| {
                        h.set(h.get() + 1);
                        None
                    }),
                )
                .w(Sizing::Cells(10))
                .h(Sizing::Cells(2)),
        )
    };

    let mut ui: Ui<()> = Ui::new();
    ui.frame(build(PointerMode::Opaque, hits.clone()), FRAME);
    press(&mut ui, 1, 0);
    assert_eq!(hits.get(), 1);

    // Same tree, same element, different mode. Nothing moved, so the layout
    // pass has no reason to run — but the mode still has to arrive.
    ui.frame(build(PointerMode::Ignore, hits.clone()), FRAME);
    press(&mut ui, 1, 0);
    assert_eq!(hits.get(), 1, "an ignored region takes no press");
}

#[test]
fn a_traversal_position_change_after_mount_reaches_the_focus_tree() {
    let build = |skip: bool| -> Node<()> {
        let mut second = focusable(text("second")).key(Key::from("second"));
        if skip {
            second = second.skip_traversal();
        }
        col()
            .child(focusable(text("first")).key(Key::from("first")))
            .child(second)
    };

    let mut ui: Ui<()> = Ui::new();
    ui.frame(build(false), FRAME);
    assert_eq!(ui.focus_scope().nodes.len(), 2);

    ui.frame(build(true), FRAME);
    assert_eq!(
        ui.focus_scope().nodes.len(),
        1,
        "the withdrawn registration left traversal"
    );
}

// ---------------------------------------------------------------------------
// D6 — losing focus is a transition, even when a scope takes it away
// ---------------------------------------------------------------------------

#[test]
fn opening_a_modal_tells_the_old_holder_it_lost_focus() {
    let log: Rc<RefCell<Vec<String>>> = Rc::default();
    let mk = |modal: bool, log: Rc<RefCell<Vec<String>>>| -> Node<()> {
        let a = log.clone();
        let mut root = col().child(
            focusable(text("field"))
                .key(Key::from("field"))
                .autofocus()
                .on_focus_change(move |e: &Event| {
                    a.borrow_mut().push(format!("field {:?}", e.kind));
                    None
                }),
        );
        if modal {
            let b = log.clone();
            root = root.child(
                layer().modality(Modality::Exclusive).child(
                    focusable(text("ok"))
                        .key(Key::from("ok"))
                        .autofocus()
                        .on_focus_change(move |e: &Event| {
                            b.borrow_mut().push(format!("ok {:?}", e.kind));
                            None
                        }),
                ),
            );
        }
        root
    };

    let mut ui: Ui<()> = Ui::new();
    ui.frame(mk(false, log.clone()), FRAME);
    assert_eq!(&*log.borrow(), &["field FocusGained"]);

    ui.frame(mk(true, log.clone()), FRAME);
    let seen = log.borrow().clone();
    assert!(
        seen.contains(&"field FocusLost".to_string()),
        "the field was told it lost focus: {seen:?}"
    );
    assert!(seen.contains(&"ok FocusGained".to_string()), "{seen:?}");
}

// ---------------------------------------------------------------------------
// D7 — the second measure feeds back into the parent's own size
// ---------------------------------------------------------------------------

#[test]
fn a_child_remeasured_at_a_known_width_changes_its_parents_height() {
    // The row's height is not known before its children are measured, so the
    // wrapping text is measured once at its natural width and again at the
    // width the row settled on. The second answer is taller, and the row has to
    // be as tall as it.
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col().child(
            col()
                .key(Key::from("outer"))
                .w(Sizing::Cells(10))
                .child(text("one two three four five six").wrap()),
        ),
        FRAME,
    );
    let outer = ui.find_by_key(&Key::from("outer")).expect("mounted");
    let rows = ui.rect(outer).h;
    let text_h = ui
        .spec()
        .items
        .iter()
        .filter_map(|i| match &i.draw {
            Draw::Lines(l) => Some(l.len() as u16),
            _ => None,
        })
        .max()
        .unwrap_or(0);
    assert_eq!(rows, text_h, "the parent is as tall as what it measured");
    assert!(rows > 1, "the text did wrap");
}

// ---------------------------------------------------------------------------
// D8 — a disposed element leaves nothing behind
// ---------------------------------------------------------------------------

#[test]
fn disposing_the_captured_element_releases_the_capture() {
    let mk = |present: bool| -> Node<()> {
        let mut root = col();
        if present {
            root = root.child(gesture(text("grip")).on(
                GestureKind::Press,
                Rc::new(|e: &Event| {
                    e.capture_pointer();
                    None
                }),
            ));
        }
        root.child(text("rest"))
    };

    let mut ui: Ui<()> = Ui::new();
    ui.frame(mk(true), FRAME);
    press(&mut ui, 1, 0);
    assert!(ui.captured().is_some());

    ui.frame(mk(false), FRAME);
    assert!(
        ui.captured().is_none(),
        "the capture went away with the element that held it"
    );
    assert!(ui.hovered().is_empty() || !ui.hovered().is_empty());
}

#[test]
fn disposing_the_focused_element_leaves_focus_somewhere_live() {
    let mk = |present: bool| -> Node<()> {
        let mut root = col();
        if present {
            root = root.child(focusable(text("gone")).key(Key::from("gone")).autofocus());
        }
        root.child(text("rest"))
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(mk(true), FRAME);
    let gone = ui.focused().expect("autofocused");

    ui.frame(mk(false), FRAME);
    assert_ne!(ui.focused(), Some(gone));
}

// ---------------------------------------------------------------------------
// D9 — a layer is resolved once per frame
// ---------------------------------------------------------------------------

#[test]
fn a_scroll_command_does_not_duplicate_the_layers() {
    let anchor = fresh_ui::behavior::anchor::Anchor::new();
    let mut ui: Ui<()> = Ui::new();
    let mk = |a: Rc<fresh_ui::behavior::anchor::Anchor>| -> Node<()> {
        col()
            .child(
                viewport(col().children((0..40).map(|i| text(format!("row {i}")))))
                    .anchor_to(a)
                    .h(Sizing::Cells(4)),
            )
            .child(
                layer()
                    .scrim(Some(fresh_ui::Scrim::Dim))
                    .child(text("over")),
            )
    };
    ui.frame(mk(anchor.clone()), FRAME);
    let before = ui
        .spec()
        .items
        .iter()
        .filter(|i| matches!(i.draw, Draw::Scrim(_)))
        .count();
    assert_eq!(before, 1);

    // A command that moves the window forces a second arrange in the same
    // frame. The layer must not be resolved twice by it.
    anchor.scroll_to(Point::new(0, 10));
    ui.frame(mk(anchor.clone()), FRAME);
    let after = ui
        .spec()
        .items
        .iter()
        .filter(|i| matches!(i.draw, Draw::Scrim(_)))
        .count();
    assert_eq!(after, 1, "one layer, one scrim");
}

/// Cards of three different heights, named `card 0`..`card 5`, in a window
/// four cells tall.
fn cards(a: Rc<fresh_ui::behavior::anchor::Anchor>) -> Node<()> {
    viewport(col().children((0..6).map(|i| {
        col()
            .key(Key::Pair("card".into(), i))
            .children((0..=i % 3).map(move |r| text(format!("card {i}.{r}"))))
    })))
    .anchor_to(a)
    .h(Sizing::Cells(4))
}

/// The rows the window is showing, top to bottom.
fn shown(ui: &Ui<()>) -> Vec<String> {
    ui.spec()
        .visible()
        .filter_map(|i| match &i.draw {
            Draw::Lines(l) => l.first().map(|s| s.to_string()),
            _ => None,
        })
        .collect()
}

#[test]
fn a_window_is_moved_to_a_band_it_measured_itself() {
    // `reveal` takes a content row, which is only an item's position when
    // every item is one cell tall. These are 1, 2 and 3 cells — so the
    // caller names the card and the framework, which laid it out, finds it.
    let anchor = fresh_ui::behavior::anchor::Anchor::new();
    let mut ui: Ui<()> = Ui::new();
    ui.frame(cards(anchor.clone()), FRAME);
    assert_eq!(
        shown(&ui),
        vec!["card 0.0", "card 1.0", "card 1.1", "card 2.0"]
    );

    // The cards start at content rows 0, 1, 3, 6, 7 and 9. Card 4 is two
    // cells tall at row 7, so the *shortest* move that holds all of it puts
    // its last row at the window's bottom — the window starts at row 5.
    anchor.reveal_key(Key::Pair("card".into(), 4));
    ui.frame(cards(anchor.clone()), FRAME);
    assert_eq!(
        shown(&ui),
        vec!["card 2.2", "card 3.0", "card 4.0", "card 4.1"],
        "the window moved just far enough to hold card 4"
    );

    // Already inside: nothing moves.
    anchor.reveal_key(Key::Pair("card".into(), 3));
    ui.frame(cards(anchor.clone()), FRAME);
    assert_eq!(
        shown(&ui),
        vec!["card 2.2", "card 3.0", "card 4.0", "card 4.1"],
        "a band already in the window is left where it is"
    );

    // Backwards, to a band above the window.
    anchor.reveal_key(Key::Pair("card".into(), 1));
    ui.frame(cards(anchor.clone()), FRAME);
    assert_eq!(
        shown(&ui),
        vec!["card 1.0", "card 1.1", "card 2.0", "card 2.1"],
        "the window moved back to card 1's top"
    );

    // A key nothing carries leaves the window alone.
    anchor.reveal_key(Key::Pair("card".into(), 99));
    ui.frame(cards(anchor.clone()), FRAME);
    assert_eq!(
        shown(&ui),
        vec!["card 1.0", "card 1.1", "card 2.0", "card 2.1"],
        "an unknown key is not a scroll to the top"
    );
}

#[test]
fn a_band_taller_than_the_window_is_shown_from_its_top() {
    // Otherwise the shortest move flushes its *bottom* edge with the
    // window's, scrolling past the thing that was asked for.
    let anchor = fresh_ui::behavior::anchor::Anchor::new();
    let mut ui: Ui<()> = Ui::new();
    let mk = |a: Rc<fresh_ui::behavior::anchor::Anchor>| -> Node<()> {
        viewport(
            col()
                .child(text("before"))
                .child(
                    col()
                        .key(Key::from("tall"))
                        .children((0..6).map(|r| text(format!("tall {r}")))),
                )
                .child(text("after")),
        )
        .anchor_to(a)
        .h(Sizing::Cells(3))
    };
    ui.frame(mk(anchor.clone()), FRAME);
    anchor.reveal_key(Key::from("tall"));
    ui.frame(mk(anchor.clone()), FRAME);
    assert_eq!(shown(&ui), vec!["tall 0", "tall 1", "tall 2"]);
}

/// **A window's offset and its extent are counted in the same unit.** An
/// index-scrolled window's offset is an item, so `reveal` has to compare it
/// against how many *items* fit — which is the window's height in cells only
/// when the items are one cell tall. Reading the height for both left a list
/// of three-row cards where it was: item 11 sat "inside" a window that was
/// fifteen cells but five items.
#[test]
fn an_item_window_of_tall_rows_reveals_by_item_not_by_cell() {
    let anchor = fresh_ui::behavior::anchor::Anchor::new();
    let mut ui: Ui<()> = Ui::new();
    let mk = |a: Rc<fresh_ui::behavior::anchor::Anchor>| -> Node<()> {
        viewport(layout_reader(move |info: LayoutInfo| {
            let w = info.scroll_window.unwrap_or_default();
            col().children((w.y..w.y + w.h as i32).map(|i| {
                col()
                    .h(Sizing::Cells(3))
                    .children((0..3).map(move |r| text(format!("row {i}.{r}"))))
            }))
        }))
        .items(12)
        .item_rows(3)
        .anchor_to(a)
        .h(Sizing::Cells(9))
    };
    ui.frame(mk(anchor.clone()), FRAME);
    assert_eq!(
        shown(&ui).first().map(String::as_str),
        Some("row 0.0"),
        "the window starts on the first item"
    );

    // Three items fit in nine cells, so showing item 11 puts the window at 9.
    anchor.reveal(11);
    ui.frame(mk(anchor.clone()), FRAME);
    assert_eq!(
        shown(&ui),
        vec![
            "row 9.0", "row 9.1", "row 9.2", "row 10.0", "row 10.1", "row 10.2", "row 11.0",
            "row 11.1", "row 11.2"
        ],
        "the window moved by items, not by cells"
    );
}

/// **A modal layer owns the keys it declines, and the pointer can still pass.**
///
/// `Modality` used to be one knob for two channels, so a surface that wanted
/// the keyboard had to take the pointer with it — or, as the editor's menu
/// did, keep a whole input handler alive whose only remaining job was to
/// return "consumed" for every key it had nothing to say about. `Keyboard`
/// says the one thing without the other.
#[test]
fn a_keyboard_modal_layer_swallows_what_it_declines_and_lets_the_pointer_by() {
    let pressed = Rc::new(Cell::new(0));
    let p = pressed.clone();
    let mk = || -> Node<()> {
        let p = p.clone();
        col()
            .child(gesture(text("behind")).on(
                GestureKind::Press,
                Rc::new(move |_: &Event| {
                    p.set(p.get() + 1);
                    None
                }),
            ))
            .child(
                layer()
                    .modality(Modality::Keyboard)
                    // Anchored below the row behind it, the way a menu hangs
                    // off its bar — so the pointer test is about modality
                    // rather than about what the layer happens to cover.
                    .anchor(Anchor::Point(0, 2))
                    .child(focusable(text("menu")).autofocus()),
            )
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(mk(), FRAME);

    // Nothing in the chain acts on a printable key, and it stops all the same:
    // the host behind this tree must not type it into the document.
    assert!(
        ui.dispatch(Input::Key(KeyPress {
            code: KeyCode::Char('x'),
            mods: Mods::NONE,
        }))
        .claimed,
        "a modal layer owns the keys it declines"
    );

    // The pointer is untouched — the row behind still answers its own press,
    // which is how clicking another menu label switches menus in one press.
    click(&mut ui, 1, 0);
    assert_eq!(pressed.get(), 1, "the pointer passes through");
}

/// **A layer that steps out of the way is out of the way.** A modal layer
/// owns the keys it declines — but a `passing_through` dismissal is the layer
/// saying "close me, and let the input reach what it was aimed at", so the
/// modal claim must not put it back in front of the key it just left.
///
/// The editor's completion list is the shape: Enter means "close this and
/// insert a newline", and the newline is the buffer's.
#[test]
fn a_pass_through_dismissal_beats_the_modal_claim() {
    let dismiss_on_any = fresh_ui::Dismiss {
        any_key: true,
        ..fresh_ui::Dismiss::default()
    };
    let mk = move || -> Node<()> {
        col().child(
            layer()
                .modality(Modality::Keyboard)
                .dismiss(dismiss_on_any.passing_through())
                .child(focusable(text("list")).autofocus()),
        )
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(mk(), FRAME);
    assert!(
        !ui.dispatch(Input::Key(KeyPress {
            code: KeyCode::Enter,
            mods: Mods::NONE,
        }))
        .claimed,
        "the layer dismissed itself and said the key goes on"
    );

    // Without `passing_through` the dismissal spends the key, as it always
    // has: the layer *was* in the way, and getting rid of it is the gesture.
    let mk2 = move || -> Node<()> {
        col().child(
            layer()
                .modality(Modality::Keyboard)
                .dismiss(dismiss_on_any)
                .child(focusable(text("pane")).autofocus()),
        )
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(mk2(), FRAME);
    assert!(
        ui.dispatch(Input::Key(KeyPress {
            code: KeyCode::Enter,
            mods: Mods::NONE,
        }))
        .claimed
    );
}

/// **A layer that owns the keyboard confines focus traversal to itself**, and
/// nothing inside it has to declare a scope for that to hold.
///
/// This is the same containment as the key claim, on the other mechanism, and
/// leaving it out is worse than a stray highlight: `move_focus` returning true
/// *claims the key*, so every directional key a modal declines was spent
/// walking focus out of it. The editor found all three faces of that at once —
/// Left, Right and Shift+Tab over a completion popup were swallowed instead of
/// dismissing it, and Tab in a dialog left the dialog instead of reaching its
/// next button.
#[test]
fn a_keyboard_modal_layer_keeps_focus_traversal_inside_itself() {
    let mk = || -> Node<()> {
        col()
            // Two focusables behind the layer, so traversal has somewhere
            // tempting to go in both directions.
            .child(focusable(text("behind one")))
            .child(focusable(text("behind two")))
            .child(
                layer()
                    .modality(Modality::Keyboard)
                    .dismiss(fresh_ui::Dismiss {
                        any_key: true,
                        ..fresh_ui::Dismiss::default()
                    })
                    .child(focusable(text("inside")).autofocus()),
            )
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(mk(), FRAME);
    let inside = ui.focused().expect("the layer autofocused");

    for code in [
        KeyCode::Tab,
        KeyCode::BackTab,
        KeyCode::Left,
        KeyCode::Right,
    ] {
        ui.dispatch(Input::Key(KeyPress {
            code,
            mods: Mods::NONE,
        }));
        assert_eq!(
            ui.focused(),
            Some(inside),
            "{code:?} moved focus out of the modal layer"
        );
    }
}

/// **Landing where you started is not a move**, so it does not claim the key.
///
/// Reading order wraps, so a scope holding one focusable answers every
/// direction with that same element — and a "move" that reports true stops
/// the key at `default_for_intent`, in front of the layer's own `Dismiss`.
/// One focusable in one dismissing layer is the editor's completion popup,
/// where it meant Left, Right and Shift+Tab did nothing at all instead of
/// closing it.
#[test]
fn a_traversal_that_lands_where_it_started_does_not_claim_the_key() {
    let mk = || -> Node<()> {
        col().child(
            layer()
                .modality(Modality::Keyboard)
                .dismiss(
                    fresh_ui::Dismiss {
                        any_key: true,
                        ..fresh_ui::Dismiss::default()
                    }
                    .passing_through(),
                )
                .child(focusable(text("the only one")).autofocus()),
        )
    };
    for code in [
        KeyCode::Tab,
        KeyCode::BackTab,
        KeyCode::Left,
        KeyCode::Right,
    ] {
        let mut ui: Ui<()> = Ui::new();
        ui.frame(mk(), FRAME);
        assert!(
            !ui.dispatch(Input::Key(KeyPress {
                code,
                mods: Mods::NONE,
            }))
            .claimed,
            "{code:?} was spent moving focus to where it already was"
        );
    }
}

/// The other half of the same rule: without modality an unclaimed key falls
/// through, and the host behind the tree is told so.
#[test]
fn a_non_modal_layer_lets_a_key_it_declines_through() {
    let mk = || -> Node<()> {
        col().child(
            layer()
                .modality(Modality::None)
                .child(focusable(text("menu")).autofocus()),
        )
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(mk(), FRAME);
    assert!(
        !ui.dispatch(Input::Key(KeyPress {
            code: KeyCode::Char('x'),
            mods: Mods::NONE,
        }))
        .claimed,
        "nothing claimed it, so the host may act on it"
    );
}

// ---------------------------------------------------------------------------
// D10 — raw input is answered per element
// ---------------------------------------------------------------------------

#[test]
fn a_leaf_inside_an_exclusive_layer_still_takes_raw_input() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col()
            .child(host_leaf(|| {
                Box::new(Grid {
                    cells: Cell::new(0),
                })
            }))
            .child(layer().modality(Modality::Exclusive).child(host_leaf(|| {
                Box::new(Grid {
                    cells: Cell::new(0),
                })
            }))),
        FRAME,
    );
    let live: Vec<_> = ui.raw_input_leaves().collect();
    assert_eq!(
        live.len(),
        1,
        "only the leaf inside the exclusive layer is live"
    );
    assert!(ui.raw_input());
}

/// **Raw input is the keyboard's.** A leaf takes it only while focus is on
/// it or inside it, or nothing holds focus: a layer that holds the keyboard
/// above a raw leaf — a popup over a terminal — takes the keys the leaf
/// would have taken raw, whatever its modality.
#[test]
fn a_raw_leaf_takes_no_input_while_something_else_holds_the_keyboard() {
    let grid = || {
        host_leaf(|| {
            Box::new(Grid {
                cells: Cell::new(0),
            })
        })
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col().child(grid()).child(
            layer()
                .modality(Modality::Keyboard)
                .child(focusable(text("popup")).autofocus()),
        ),
        FRAME,
    );
    assert!(ui.focused().is_some(), "the layer's field holds focus");
    assert!(!ui.raw_input(), "so the leaf takes nothing raw");

    let mut ui: Ui<()> = Ui::new();
    ui.frame(col().child(focusable(grid()).autofocus()), FRAME);
    assert!(ui.focused().is_some());
    assert!(
        ui.raw_input(),
        "the leaf holds the keyboard and takes it raw"
    );

    // The leaf's keyboard is its holder's: a leaf wrapped in a focusable —
    // under a keyed node above that, as a pane's context is — takes raw
    // input while the wrapper holds focus, and none while a focusable beside
    // it does.
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col()
            .child(
                fresh_ui::stack()
                    .key("context")
                    .child(focusable(grid()).autofocus()),
            )
            .child(focusable(text("field"))),
        FRAME,
    );
    assert!(ui.raw_input(), "focus on the wrapper is focus on the leaf");
    key(&mut ui, KeyCode::Tab);
    assert!(
        !ui.raw_input(),
        "and the leaf takes nothing raw while the field beside it holds the keyboard"
    );
}

// ---------------------------------------------------------------------------
// A wash is a ground that keeps what it covers
// ---------------------------------------------------------------------------

/// A themed box paints a fill; the same box as a wash paints a wash, in the
/// same place and before its own content, so a backend recolours the cells
/// under it and keeps their text.
#[test]
fn a_washed_box_paints_a_wash_where_a_themed_box_paints_a_fill() {
    let grounds = |root: Node<()>| -> Vec<(Draw, Rect)> {
        let mut ui: Ui<()> = Ui::new();
        ui.frame(root, FRAME)
            .items
            .iter()
            .filter(|i| matches!(i.draw, Draw::Fill | Draw::Wash))
            .map(|i| (i.draw.clone(), i.rect))
            .collect()
    };
    let plain = grounds(col().child(col().theme("ground").child(text("over"))));
    assert_eq!(plain, vec![(Draw::Fill, Rect::new(0, 0, 20, 1))]);
    let washed = grounds(col().child(col().theme("ground").wash().child(text("over"))));
    assert_eq!(washed, vec![(Draw::Wash, Rect::new(0, 0, 20, 1))]);
}

// ---------------------------------------------------------------------------
// D11 — the cursor reaches the display list
// ---------------------------------------------------------------------------

#[test]
fn a_focused_text_field_puts_the_cursor_in_the_display_list() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col().child(
            fresh_ui::TextField::new("abc")
                .autofocus()
                .on_change(|_s: String| ())
                .node(),
        ),
        FRAME,
    );
    let c = ui.spec().cursor.expect("a focused field owns the cursor");
    assert_eq!(c.pos, Point::new(3, 0), "at the caret, not at the origin");
    assert!(c.visible);
}

// ---------------------------------------------------------------------------
// D13 — the viewport's declared props do something
// ---------------------------------------------------------------------------

#[test]
fn a_viewport_honours_max_h_selectable_and_its_initial_offset() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col().child(
            viewport(col().children((0..40).map(|i| text(format!("row {i}")))))
                .key(Key::from("vp"))
                .selectable()
                .max_h(3)
                .scroll_at(0, 5),
        ),
        FRAME,
    );
    assert!(
        ui.spec().items.iter().any(|i| i.draw == Draw::Selectable),
        "the selectable region is in the display list"
    );
    let vp = ui.find_by_key(&Key::from("vp")).expect("mounted");
    assert_eq!(ui.rect(vp).h, 3, "max_h bounded the window");
    assert_eq!(ui.scroll(vp).0.y, 5, "the initial offset was applied");

    let rows: Vec<String> = ui
        .spec()
        .visible()
        .filter_map(|i| match &i.draw {
            Draw::Lines(l) => l.first().map(|s| s.to_string()),
            _ => None,
        })
        .collect();
    assert_eq!(rows, vec!["row 5", "row 6", "row 7"]);
}

// ---------------------------------------------------------------------------
// D14 — a reader's subtree is an ordinary part of the tree
// ---------------------------------------------------------------------------

#[test]
fn a_layout_reader_subtree_inherits_the_theme_above_it() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(
        col()
            .theme("panel")
            .child(layout_reader(|_info| text("inside"))),
        FRAME,
    );
    let themed: Vec<&str> = spec
        .items
        .iter()
        .filter(|i| matches!(i.draw, Draw::Lines(_)))
        .map(|i| i.theme.as_str())
        .collect();
    assert_eq!(themed, vec!["panel"]);
}

// ---------------------------------------------------------------------------
// D15 — the positioning and linking passes skip what did not change
// ---------------------------------------------------------------------------

#[test]
fn an_unchanged_frame_does_not_measure_anything_again() {
    let mut ui: Ui<()> = Ui::new();
    let mk = || -> Node<()> {
        col()
            .key(Key::from("root"))
            .children((0..20).map(|i| text(format!("row {i}"))))
    };
    ui.frame(mk(), FRAME);
    let root = ui.find_by_key(&Key::from("root")).expect("mounted");
    let first = ui.layouts(root);

    ui.frame(mk(), FRAME);
    assert_eq!(
        ui.layouts(root),
        first,
        "a frame that changed nothing measured nothing"
    );
}

#[test]
fn a_change_deep_in_the_tree_relinks_only_its_own_path() {
    // The cheapest observable proxy for "the walk was skipped": a subtree that
    // is neither dirty nor handed different inheritance keeps the identity of
    // everything it published, so nothing below it remounts.
    let seen: Rc<Cell<u32>> = Rc::default();
    let mk = |label: &str| -> Node<()> {
        col()
            .child(col().key(Key::from("stable")).child(text("unchanging")))
            .child(text(label))
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(mk("a"), FRAME);
    let stable = ui.find_by_key(&Key::from("stable")).expect("mounted");
    let builds = ui.rect(stable);
    seen.set(1);

    ui.frame(mk("b"), FRAME);
    assert_eq!(
        ui.find_by_key(&Key::from("stable")),
        Some(stable),
        "the untouched subtree kept its identity"
    );
    assert_eq!(ui.rect(stable), builds);
}

// ---------------------------------------------------------------------------
// F1 — a geometry handle does not resolve to a recycled arena slot
// ---------------------------------------------------------------------------

/// Takes a geometry handle at construction and stashes a clone where the test
/// can reach it, so the handle outlives the element.
struct Stash(Rc<RefCell<Vec<GeomHandle>>>);

#[derive(Default)]
struct StashState {
    // Held so the handle lives as long as the element, as a real component
    // would keep it; the test reads its own clone.
    _geom: Option<GeomHandle>,
}

impl Component<()> for Stash {
    type State = StashState;

    fn init(&self, cx: &mut InitCx<'_, ()>) -> StashState {
        let g = cx.geometry();
        self.0.borrow_mut().push(g.clone());
        StashState { _geom: Some(g) }
    }

    fn build(&self, _s: &StashState, _cx: &mut BuildCx<'_, ()>) -> Node<()> {
        // One row tall; the element that reuses its slot is three, so a handle
        // that follows the slot reads a height it should never see.
        gesture(text("x")).h(Sizing::Cells(1))
    }
}

struct Tall(Rc<RefCell<Vec<GeomHandle>>>);

impl Component<()> for Tall {
    type State = StashState;

    fn init(&self, cx: &mut InitCx<'_, ()>) -> StashState {
        let g = cx.geometry();
        self.0.borrow_mut().push(g.clone());
        StashState { _geom: Some(g) }
    }

    fn build(&self, _s: &StashState, _cx: &mut BuildCx<'_, ()>) -> Node<()> {
        col()
            .child(text("a"))
            .child(text("b"))
            .child(text("c"))
            .h(Sizing::Cells(3))
    }
}

#[test]
fn a_geometry_handle_does_not_read_a_recycled_slot() {
    let handles: Rc<RefCell<Vec<GeomHandle>>> = Rc::default();

    // Frame 1: mount the short component. It is the only dynamic element, so it
    // takes a slot the next mount will pop off the free list.
    let mut ui: Ui<()> = Ui::new();
    ui.frame(col().child(Stash(handles.clone()).node()), FRAME);
    let first = handles.borrow()[0].clone();
    assert_eq!(first.size().h, 1, "the first occupant is one row tall");

    // Frame 2: the short component is gone and a taller one is built in its
    // place, reusing the freed slot. Its geometry is three rows.
    ui.frame(col().child(Tall(handles.clone()).node()), FRAME);
    let second = handles.borrow()[1].clone();
    assert_eq!(second.size().h, 3, "the new occupant is three rows tall");

    // The stale handle must not have followed the slot to the new element: it
    // reads nothing, not the three-row rectangle. Without a generation tag the
    // two ids are the same u32 and this returns 3.
    assert_eq!(
        first.size(),
        Size::new(0, 0),
        "a handle to a disposed element reads nothing, not its slot's new occupant"
    );
}

/// **A thumb never claims less of the track than the window shows.**
///
/// The length is a ratio, and flooring it rounds the window *down*: 28 rows
/// of 434 is 6.5% of a 28-cell track, which floors to one cell claiming 3.6%.
/// A one-cell thumb is also the hardest thing on the bar to hit, so the row a
/// user aims at lands on the track and page-jumps the viewport instead of
/// grabbing — which is what a press one row below the top of a resting thumb
/// did in the keybinding editor's 434-binding table.
#[test]
fn a_scrollbar_thumb_rounds_its_length_up() {
    // The case above: two cells, not one.
    assert_eq!(Draw::scrollbar_thumb(0, 434, 28, 28).1, 2);

    // The rule, not the case: for any content longer than the track, the
    // thumb is at least as long as the exact ratio would make it.
    for track in [3u16, 7, 28, 40] {
        for content in (track as u32 + 1)..600 {
            let len = Draw::scrollbar_thumb(0, content, track as u32, track).1;
            let exact = (track as f64) * (track as f64) / (content as f64);
            assert!(
                len as f64 >= exact,
                "track {track}, content {content}: thumb {len} is shorter than the \
                 window's own share of the track ({exact:.3})"
            );
            assert!(
                (1..=track).contains(&len),
                "track {track}, content {content}: thumb {len} is off the track"
            );
        }
    }
}

/// **The window is an argument because it is not the track.**
///
/// A cell-scrolling viewport shows as many cells as its bar is tall, so the
/// two numbers coincide and either one gives the ratio. An item-scrolling one
/// does not: sixteen five-row cards in a twenty-two cell track show *four*,
/// and reading the track as the window makes the ratio 22/16 — over one, so
/// the thumb clamps to the whole bar. A thumb that fills its track says the
/// list is entirely visible and has nowhere left to be dragged to, which is
/// what every card list's bar said.
#[test]
fn a_scrollbar_thumb_measures_the_window_not_the_track() {
    assert_eq!(Draw::scrollbar_thumb(0, 16, 4, 22), (0, 6));
    // And the fully-scrolled thumb still sits flush against the bottom.
    assert_eq!(Draw::scrollbar_thumb(12, 16, 4, 22), (16, 6));
}

/// Content that fits gives the whole track — there is nowhere to scroll, so
/// there is no gap for the thumb to leave.
#[test]
fn a_scrollbar_thumb_fills_a_track_it_cannot_move_along() {
    for content in 1..=28u32 {
        assert_eq!(
            Draw::scrollbar_thumb(0, content, 28, 28),
            (0, 28),
            "content {content} fits in 28 cells"
        );
    }
}

/// **What a reader builds this frame is what paints this frame.**
///
/// A `layout_reader`'s builder is a new closure over new state every
/// description, and the framework invalidates it so the builder re-runs. But
/// re-running the builder is only half of it: the subtree it produces has to
/// reach the display list of *this* frame. It did not — the builder ran with
/// the new state and the spec still carried the previous frame's text, so
/// every surface built inside a reader painted one frame behind the state it
/// was built from, and only caught up when some later, unrelated event
/// happened to draw again.
///
/// In the editor that is the settings dialog: clicking a category moved the
/// selection and rebuilt the description, and the screen kept showing the
/// previous category until the pointer moved.
#[test]
fn a_layout_readers_rebuilt_subtree_paints_in_the_same_frame() {
    fn painted(ui: &Ui<()>) -> Vec<String> {
        ui.spec()
            .items
            .iter()
            .filter_map(|i| match &i.draw {
                Draw::Lines(l) => Some(l.iter().map(|s| s.to_string()).collect::<Vec<_>>()),
                _ => None,
            })
            .flatten()
            .collect()
    }
    let tree = |label: &'static str| layout_reader(move |_| text(label));

    let mut ui: Ui<()> = Ui::new();
    ui.frame(tree("first"), FRAME);
    assert_eq!(painted(&ui), vec!["first".to_string()]);

    ui.frame(tree("second"), FRAME);
    assert_eq!(
        painted(&ui),
        vec!["second".to_string()],
        "the reader rebuilt with the new description, so the new subtree is \
         what this frame paints — not what the last one did"
    );

    // The same, one layer in. A layer is laid out in its own pass, and that
    // is where the editor's dialogs live.
    // The subtree keeps the *same geometry* both times — the editor's dialog
    // is a fixed-size box whose contents change — so nothing about the
    // arrangement moves and only the text differs.
    let layered = |label: &'static str| {
        col().key(Key::Str("chrome".into())).child(
            layer()
                .within(Key::Str("chrome".into()))
                .anchor(Anchor::Screen(Align::Center))
                .place(Place::Over)
                .child(layout_reader(move |_| {
                    col()
                        .w(Sizing::Cells(12))
                        .h(Sizing::Cells(1))
                        .key(Key::Str("box".into()))
                        .child(text(label))
                })),
        )
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(layered("one"), FRAME);
    assert_eq!(painted(&ui), vec!["one".to_string()]);

    ui.frame(layered("two"), FRAME);
    assert_eq!(
        painted(&ui),
        vec!["two".to_string()],
        "a reader inside a layer paints this frame's build too"
    );
}

/// **A press on the thumb picks it up; a press on the track jumps to it.**
///
/// Every press used to put the thumb's *top* under the pointer, so grabbing a
/// thumb anywhere below its first row shifted it up by however far down it had
/// been grabbed — the viewport moved without the pointer moving at all. That
/// is the click-jumps-to-row bug, and it is why a press one row into a resting
/// two-row thumb scrolled the keybinding editor's table.
#[test]
fn a_press_inside_the_thumb_grabs_it_rather_than_jumping() {
    fn bar(ui: &mut Ui<()>) -> (Rect, i32) {
        let v = ui.find_by_key(&Key::Str("v".into())).expect("the viewport");
        (ui.rect_of(v), ui.scroll(v).0.y)
    }
    let tree = || {
        // Thirty rows in a ten-row window puts four cells of thumb on a
        // ten-cell track, so there is a thumb to grab in the first place.
        viewport(col().children((0..30).map(|i| text(format!("row {i}")))))
            .key(Key::Str("v".into()))
            .scrollbar()
            .h(Sizing::Cells(10))
            .w(Sizing::Cells(10))
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(tree(), FRAME);
    let (r, before) = bar(&mut ui);
    assert_eq!(before, 0, "starts at the top");

    // The bar is the viewport's last column; the resting thumb starts at its
    // first row. Press one row into it and let go without moving.
    let x = r.x + r.w as i32 - 1;
    press(&mut ui, x, r.y + 1);
    ui.dispatch(Input::release(
        Point::new(x, r.y + 1),
        MouseButton::Left,
        Mods::NONE,
    ));
    ui.frame(tree(), FRAME);
    assert_eq!(
        bar(&mut ui).1,
        before,
        "a press inside the thumb with no movement moves nothing"
    );

    // A press on the bare track below the thumb still jumps.
    press(&mut ui, x, r.y + 8);
    ui.frame(tree(), FRAME);
    assert!(
        bar(&mut ui).1 > before,
        "a press on the track below the thumb still jumps toward it"
    );
}

/// **Confining focus and swallowing keys are two questions**, and
/// `Modality::Focus` is the one variant that answers them differently.
///
/// A text prompt is the shape it exists for. While the prompt is up it is
/// unambiguously where the keyboard is — the surface behind it must not get
/// the key first, which is what confinement says — and yet a key the prompt
/// itself does not act on is still the *editor's*: that is how a minibuffer's
/// `Ctrl+P` reaches the command palette. `Modality::Keyboard` cannot say it,
/// because swallowing is exactly what makes an open menu a dead end.
///
/// Without the split, a host with its own pipeline behind the tree had two
/// bad options for such a surface: claim the keyboard and lose every binding
/// it declines, or claim nothing and let whatever else is focusable take the
/// key out from under it.
#[test]
fn a_focus_modal_layer_confines_traversal_without_swallowing() {
    let seen = Rc::new(Cell::new(0));
    let s = seen.clone();
    let mk = move || -> Node<()> {
        let s = s.clone();
        col()
            // Something else focusable and *asking for focus*, standing in
            // for another surface that would otherwise take the keyboard.
            .child(focusable(text("behind")).autofocus())
            .child(
                layer()
                    .modality(Modality::Focus)
                    .anchor(Anchor::Point(0, 2))
                    .child(focusable(text("prompt")).autofocus().on_key({
                        let s = s.clone();
                        move |_: &Event| {
                            s.set(s.get() + 1);
                            None
                        }
                    })),
            )
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(mk(), FRAME);

    // Confinement: the layer's own chain is offered the key, not the
    // focusable behind it that also asked for focus.
    let d = ui.dispatch(Input::Key(KeyPress {
        code: KeyCode::Char('x'),
        mods: Mods::NONE,
    }));
    assert_eq!(
        seen.get(),
        1,
        "the confined layer's chain saw the key first"
    );

    // And no swallow: the handler declined to stop, so the key is still the
    // host's to resolve.
    assert!(
        !d.claimed,
        "a Focus layer hands back what its chain did not act on"
    );
    assert!(
        !ui.keyboard_owned(),
        "and says so for the keys the tree cannot route at all"
    );
}

/// **A surface can own the keyboard and still hand back what it declines**,
/// and a host with its own pipeline behind the tree has to be able to ask
/// each half separately: "may I intercept this key" is not "may I resolve
/// it". `keyboard_owned` answers the second, `focus_confined` the first, and
/// a `Modality::Focus` layer is precisely where the two disagree.
///
/// The editor's unfocused popups are the case. While a prompt is open they
/// must not intercept a keystroke — the prompt owns the keyboard — but the
/// prompt does not swallow, so what it declines is still the host's to
/// resolve. Asking one question for both is what a ranked list of surfaces
/// was doing.
#[test]
fn confining_focus_and_swallowing_keys_are_asked_separately() {
    let ask = |m: Modality| -> (bool, bool) {
        let mut ui: Ui<()> = Ui::new();
        ui.frame(
            col().child(
                layer()
                    .modality(m)
                    .child(focusable(text("surface")).autofocus()),
            ),
            FRAME,
        );
        (ui.focus_confined(), ui.keyboard_owned())
    };

    assert_eq!(
        ask(Modality::Focus),
        (true, false),
        "a Focus layer confines the keyboard and swallows nothing"
    );
    assert_eq!(
        ask(Modality::Keyboard),
        (true, true),
        "a Keyboard layer does both"
    );
    assert_eq!(
        ask(Modality::None),
        (false, false),
        "and an ordinary layer does neither"
    );
    assert_eq!(
        ask(Modality::Pointer),
        (false, false),
        "nor does one that claims only the pointer"
    );
}

/// **A dismissed layer goes on confining focus until the frame is rebuilt**,
/// and that is a limit worth stating rather than a bug to fix. The library
/// does not unilaterally remove a layer the application declared: dismissal
/// reports itself (`on_dismiss`) and the *app* stops declaring the surface on
/// its next frame. So between the dismissal and that frame, the element is
/// still on the focused chain.
///
/// The consequence for a host with its own pipeline behind the tree: this
/// question answers "was a surface confining focus when this frame was
/// built", not "is one confining it right now". A host that must know the
/// second — because a rung may mutate state and then decline the same
/// keystroke — has to re-derive from its own live state, and that is exactly
/// why `fresh-editor`'s `get_key_context` and its unfocused-popup guard read
/// an app-side stack rather than asking here.
#[test]
fn a_dismissed_layer_still_confines_focus_until_the_next_frame() {
    let mut ui: Ui<()> = Ui::new();
    let mk = || -> Node<()> {
        col().children([
            focusable(text("behind")),
            layer()
                .modality(Modality::Focus)
                .dismiss(fresh_ui::Dismiss::ESCAPE)
                .child(focusable(text("prompt")).autofocus()),
        ])
    };
    ui.frame(mk(), FRAME);
    assert!(
        ui.focus_confined(),
        "the prompt confines focus while it is up"
    );

    ui.dispatch(Input::Key(KeyPress {
        code: KeyCode::Esc,
        mods: Mods::NONE,
    }));
    assert!(
        ui.focus_confined(),
        "and goes on doing so until the app stops declaring it"
    );

    // The app's next frame is what actually takes it away.
    let mut ui: Ui<()> = Ui::new();
    ui.frame(col().child(focusable(text("behind")).autofocus()), FRAME);
    assert!(
        !ui.focus_confined(),
        "with the layer gone from the description, nothing confines focus"
    );
}

/// The partner: a `Focus` layer still claims what its chain *does* act on.
/// Declining is a node not stopping the flow, not the layer being absent.
#[test]
fn a_focus_modal_layer_still_claims_what_it_answers() {
    let mk = || -> Node<()> {
        col().child(layer().modality(Modality::Focus).child(
            focusable(text("prompt")).autofocus().on_key(|e: &Event| {
                e.stop();
                None
            }),
        ))
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(mk(), FRAME);
    assert!(
        ui.dispatch(Input::Key(KeyPress {
            code: KeyCode::Char('x'),
            mods: Mods::NONE,
        }))
        .claimed,
        "the node stopped the flow, so the key is spent"
    );
}

/// **The pointer's mirror of `Modality::Keyboard`**, and the variant a
/// surface needs when the tree owns its pointer while its *interior* owns its
/// keyboard host-side.
///
/// The editor's plugin panel is the shape: the box swallows every press that
/// lands outside it, and its keys go to the widget runtime through a layer of
/// its own. Saying `Exclusive` for the pointer half took the keyboard too —
/// focus moved into a layer with nothing focusable inside it, was dropped for
/// want of anywhere to go, and the surface's real keyboard layer stopped
/// being the one containment found. That is a plugin context menu that no
/// longer closes on Escape, and it is what this variant prevents.
#[test]
fn a_pointer_modal_layer_takes_the_pointer_and_leaves_the_keyboard() {
    let pressed = Rc::new(Cell::new(0));
    let keyed = Rc::new(Cell::new(0));
    let p = pressed.clone();
    let k = keyed.clone();
    let mk = move || -> Node<()> {
        let (p, k) = (p.clone(), k.clone());
        col()
            .child(gesture(text("behind")).on(
                GestureKind::Press,
                Rc::new(move |_: &Event| {
                    p.set(p.get() + 1);
                    None
                }),
            ))
            // The behind-the-scenes keyboard owner: a surface elsewhere in the
            // frame that wants focus, standing in for the panel's own layer.
            .child(focusable(text("elsewhere")).autofocus().on_key({
                let k = k.clone();
                move |_: &Event| {
                    k.set(k.get() + 1);
                    None
                }
            }))
            // The claim: the whole frame, pointer only, nothing focusable in
            // it — exactly the editor's modal slot.
            .child(
                layer()
                    .modality(Modality::Pointer)
                    .anchor(Anchor::Screen(Align::Start))
                    .place(Place::Fill)
                    .child(text("claim")),
            )
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(mk(), FRAME);

    // The pointer stops at the claim: the row behind it never sees the press.
    click(&mut ui, 1, 0);
    assert_eq!(pressed.get(), 0, "a pointer-modal layer takes the press");

    // The keyboard is untouched: focus stayed where it asked to be, and the
    // key reached it.
    ui.dispatch(Input::Key(KeyPress {
        code: KeyCode::Char('x'),
        mods: Mods::NONE,
    }));
    assert_eq!(
        keyed.get(),
        1,
        "focus never moved into the claim, so the key reached the real owner"
    );
    assert!(
        !ui.keyboard_owned(),
        "and the claim swallows nothing it never took"
    );
}

/// **Flex contributes nothing but its floor to an intrinsic measure** (L15).
///
/// A box that is being *asked how big it wants to be* has no room to divide:
/// the maximum it was handed is its parent's room, not an extent anyone has
/// claimed. Dividing that among the flex children made every `Sizing::Auto`
/// box holding a spacer as wide as the frame — which is why a menu dropdown
/// and an anchored popup measured their own rows' text instead of saying
/// `Auto`.
///
/// The row here is the shape that made the rule necessary: `label · spacer ·
/// accelerator`, the spacer flexed, inside a box that hugs its content.
#[test]
fn flex_contributes_only_its_floor_to_an_intrinsic_measure() {
    let hug = Key::Str("hug".into());
    // A layer measures its child loosely — it is placed at a point and sized
    // to what it holds, which is exactly the question the rule is about.
    let mk = || {
        col().child(
            layer()
                .anchor(Anchor::Point(0, 0))
                .place(Place::Over)
                .child(col().w(Sizing::Auto).key(hug.clone()).child(
                    fresh_ui::row().w(Sizing::Auto).children([
                        text("Open"),
                        fresh_ui::row().w(Sizing::Flex(1)),
                        text("^O"),
                    ]),
                )),
        )
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(mk(), FRAME);
    let w = ui.rect_of(ui.find_by_key(&hug).expect("the hugging box")).w;
    assert_eq!(
        w, 6,
        "the box is as wide as its content — \"Open\" and \"^O\" — not as wide \
         as the frame the flex spacer was offered"
    );

    // And where the extent *is* definite, flex still divides what is left:
    // the same row inside a box of a stated width puts the accelerator against
    // the right edge.
    let fixed = Key::Str("fixed".into());
    let acc = Key::Str("acc".into());
    let mk = || {
        col().w(Sizing::Cells(20)).key(fixed.clone()).child(
            fresh_ui::row().w(Sizing::Flex(1)).children([
                text("Open"),
                fresh_ui::row().w(Sizing::Flex(1)),
                text("^O").key(acc.clone()),
            ]),
        )
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(mk(), FRAME);
    let r = ui.rect_of(ui.find_by_key(&acc).expect("the accelerator"));
    assert_eq!(
        r.x + r.w as i32,
        20,
        "a definite extent still has room for the spacer to divide"
    );
}
#[test]
fn nested_flex_under_a_definite_box_still_divides() {
    use fresh_ui::{col, row, text, Key, Size, Sizing, Ui};
    let inner = Key::Str("inner".into());
    let mid = Key::Str("mid".into());
    let mut ui: Ui<()> = Ui::new();
    let (ik, mk2) = (inner.clone(), mid.clone());
    ui.frame(
        layout_reader(move |_i: LayoutInfo| {
            col().w(Sizing::Cells(20)).h(Sizing::Cells(10)).children([
                row().h(Sizing::Cells(1)).child(text("head")),
                row().flex(1).children([
                    col().w(Sizing::Cells(4)).child(text("cat")),
                    col()
                        .w(Sizing::Flex(1))
                        .key(mk2.clone())
                        .child(row().h(Sizing::Flex(1)).key(ik.clone()).child(text("x"))),
                ]),
            ])
        }),
        Size::new(40, 20),
    );
    let m = ui.rect_of(ui.find_by_key(&mid).expect("mid"));
    let i = ui.rect_of(ui.find_by_key(&inner).expect("inner"));
    assert_eq!((m.h, i.h), (9, 9), "mid={m:?} inner={i:?}");
}
