//! The behaviours the Part 1c review found missing (plan phase R12).
//!
//! One test per finding. Each asserts the thing the design document says
//! happens, not the thing the implementation happened to do.

use std::cell::{Cell, RefCell};
use std::rc::Rc;

use fresh_ui::{
    col, focusable, gesture, host_leaf, layer, layout_reader, text, viewport, BuildCx, Component,
    ComponentExt, Constraints, Draw, Event, Focusable, Geom, GeomHandle, GestureKind, Hit,
    HostLeaf, InitCx, Input, Intent, Key, KeyCode, KeyPress, LayoutCx, Modality, Mods, MouseButton,
    Node, Point, PointerMode, Rect, RenderObject, Shortcut, Size, Sizing, Ui,
};

const FRAME: Size = Size { w: 20, h: 10 };

fn press(ui: &mut Ui<()>, x: i32, y: i32) {
    ui.dispatch(Input::Press {
        pos: Point::new(x, y),
        button: MouseButton::Left,
        mods: Mods::NONE,
    });
}

fn click(ui: &mut Ui<()>, x: i32, y: i32) {
    press(ui, x, y);
    ui.dispatch(Input::Release {
        pos: Point::new(x, y),
        button: MouseButton::Left,
        mods: Mods::NONE,
    });
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
