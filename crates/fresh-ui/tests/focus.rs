//! Focus, traversal and intents (plan phase L6).

use std::cell::RefCell;
use std::rc::Rc;

use fresh_ui::{
    col, focusable, layer, row, text, Align, Anchor, BuildCx, Component, ComponentExt, Directional,
    Event, FocusDir, Input, Intent, KeyCode, KeyPress, Modality, Mods, Node, SelectionOnFocus,
    Size, Sizing, Ui,
};

const FRAME: Size = Size { w: 30, h: 10 };
type Log = Rc<RefCell<Vec<String>>>;

fn field(name: &'static str) -> Node<()> {
    focusable(text(name)).key(name).h(Sizing::Cells(1))
}

fn form() -> Node<()> {
    col().children([field("one"), field("two"), field("three")])
}

#[test]
fn tab_walks_the_focusables_in_reading_order_and_wraps() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(form(), FRAME);
    let ids: Vec<_> = (0..3).map(|i| ui.at(&[i]).unwrap()).collect();

    let tab = Input::Key(KeyPress::new(KeyCode::Tab));
    ui.dispatch(tab);
    assert_eq!(ui.focused(), Some(ids[0]));
    ui.dispatch(tab);
    assert_eq!(ui.focused(), Some(ids[1]));
    ui.dispatch(tab);
    assert_eq!(ui.focused(), Some(ids[2]));
    ui.dispatch(tab);
    assert_eq!(ui.focused(), Some(ids[0]), "wraps at the end");

    ui.dispatch(Input::Key(KeyPress::new(KeyCode::BackTab)));
    assert_eq!(ui.focused(), Some(ids[2]));
}

#[test]
fn explicit_ordinals_override_position() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col().children([
            focusable(text("a")).ordinal(3),
            focusable(text("b")).ordinal(1),
            focusable(text("c")).ordinal(2),
        ]),
        FRAME,
    );
    let order = ui.focus_scope().ordered();
    assert_eq!(
        order,
        vec![
            ui.at(&[1]).unwrap(),
            ui.at(&[2]).unwrap(),
            ui.at(&[0]).unwrap()
        ]
    );
}

#[test]
fn traversal_skips_what_asked_to_be_skipped() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col().children([
            focusable(text("a")),
            focusable(text("b")).skip_traversal(),
            focusable(text("c")),
        ]),
        FRAME,
    );
    let ids = ui.focus_scope().ordered();
    assert_eq!(ids.len(), 2);
    assert_eq!(ids, vec![ui.at(&[0]).unwrap(), ui.at(&[2]).unwrap()]);
}

#[test]
fn a_directional_policy_moves_by_geometry() {
    let mut ui: Ui<()> = Ui::new();
    ui.set_traversal_policy(Box::new(Directional));
    ui.frame(
        col().children([
            row().h(Sizing::Cells(1)).children([
                focusable(text("tl")).w(Sizing::Cells(10)),
                focusable(text("tr")).w(Sizing::Cells(10)),
            ]),
            row().h(Sizing::Cells(1)).children([
                focusable(text("bl")).w(Sizing::Cells(10)),
                focusable(text("br")).w(Sizing::Cells(10)),
            ]),
        ]),
        FRAME,
    );
    let tl = ui.at(&[0, 0]).unwrap();
    let tr = ui.at(&[0, 1]).unwrap();
    let bl = ui.at(&[1, 0]).unwrap();

    ui.request_focus(tl, SelectionOnFocus::None);
    ui.dispatch(Input::Key(KeyPress::new(KeyCode::Right)));
    assert_eq!(ui.focused(), Some(tr));
    ui.request_focus(tl, SelectionOnFocus::None);
    ui.dispatch(Input::Key(KeyPress::new(KeyCode::Down)));
    assert_eq!(ui.focused(), Some(bl));
}

// -- preservation ------------------------------------------------------------

struct Counter;

impl Component<()> for Counter {
    type State = u32;
    fn build(&self, s: &u32, _cx: &mut BuildCx<'_, ()>) -> Node<()> {
        col().children([field("one"), text(format!("count {s}")), field("two")])
    }
}

#[test]
fn focus_survives_reconciliation() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(Counter.node(), FRAME);
    let two = ui.at(&[0, 2]).unwrap();
    ui.request_focus(two, SelectionOnFocus::Preserve);

    // Rebuild the whole tree, repeatedly.
    for _ in 0..5 {
        ui.frame(Counter.node(), FRAME);
    }
    ui.set_state::<u32>(ui.root().unwrap(), |s| *s += 1);
    ui.tick();

    assert_eq!(
        ui.focused(),
        Some(two),
        "the element is the same, so focus is"
    );
    assert_eq!(ui.at(&[0, 2]), Some(two));
}

#[test]
fn focus_moves_lost_before_gained() {
    let log: Log = Rc::default();
    let (a, b) = (log.clone(), log.clone());
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col().children([
            focusable(text("one")).on_focus_change(move |e: &Event| {
                a.borrow_mut().push(format!("one {:?}", e.kind));
                None
            }),
            focusable(text("two")).on_focus_change(move |e: &Event| {
                b.borrow_mut().push(format!("two {:?}", e.kind));
                None
            }),
        ]),
        FRAME,
    );
    let one = ui.at(&[0]).unwrap();
    let two = ui.at(&[1]).unwrap();
    ui.request_focus(one, SelectionOnFocus::None);
    log.borrow_mut().clear();
    ui.request_focus(two, SelectionOnFocus::None);

    assert_eq!(*log.borrow(), vec!["one FocusLost", "two FocusGained"]);
}

#[test]
fn a_focus_request_carries_what_to_do_with_the_selection() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(form(), FRAME);
    let one = ui.at(&[0]).unwrap();

    ui.request_focus(one, SelectionOnFocus::Caret(4));
    assert_eq!(ui.focus_selection(), SelectionOnFocus::Caret(4));

    ui.dispatch(Input::Key(KeyPress::new(KeyCode::Tab)));
    assert_eq!(
        ui.focus_selection(),
        SelectionOnFocus::SelectAll,
        "tabbing to a field selects it; clicking into one would not"
    );
}

#[test]
fn autofocus_gives_focus_to_the_first_element_that_asked() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col().children([focusable(text("a")), focusable(text("b")).autofocus()]),
        FRAME,
    );
    assert_eq!(ui.focused(), Some(ui.at(&[1]).unwrap()));
}

// -- scopes ------------------------------------------------------------------

#[test]
fn a_modal_layer_confines_traversal_to_itself() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col().children([
            field("behind one"),
            field("behind two"),
            layer()
                .anchor(Anchor::Screen(Align::Center))
                .modality(Modality::Exclusive)
                .child(col().children([field("modal one"), field("modal two")])),
        ]),
        FRAME,
    );

    let reachable = ui.focus_scope().ordered();
    assert_eq!(reachable.len(), 2, "only the modal's fields");
    assert_eq!(reachable[0], ui.at(&[2, 0, 0]).unwrap());

    let tab = Input::Key(KeyPress::new(KeyCode::Tab));
    ui.dispatch(tab);
    ui.dispatch(tab);
    ui.dispatch(tab);
    assert!(
        reachable.contains(&ui.focused().unwrap()),
        "tabbing never leaves the modal"
    );
}

// -- intents -----------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Msg {
    ClosePrompt,
    ClearSelection,
}

#[test]
fn the_same_intent_resolves_to_different_actions_by_focus_position() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        col().children([
            // The buffer clears its selection on Cancel.
            focusable(text("buffer"))
                .key("buffer")
                .action(Intent::Cancel, |_| Msg::ClearSelection),
            // The prompt closes itself on the same intent.
            focusable(text("prompt"))
                .key("prompt")
                .action(Intent::Cancel, |_| Msg::ClosePrompt),
        ]),
        FRAME,
    );
    let buffer = ui.at(&[0]).unwrap();
    let prompt = ui.at(&[1]).unwrap();
    let esc = Input::Key(KeyPress::new(KeyCode::Esc));

    ui.request_focus(buffer, SelectionOnFocus::None);
    assert_eq!(ui.dispatch(esc), vec![Msg::ClearSelection]);

    ui.request_focus(prompt, SelectionOnFocus::None);
    assert_eq!(ui.dispatch(esc), vec![Msg::ClosePrompt]);
}

#[test]
fn an_action_on_an_ancestor_catches_what_the_focused_element_declines() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        focusable(col().child(focusable(text("inner")).key("inner")))
            .action(Intent::Cancel, |_| Msg::ClosePrompt),
        FRAME,
    );
    let inner = ui.at(&[0, 0]).unwrap();
    ui.request_focus(inner, SelectionOnFocus::None);
    assert_eq!(
        ui.dispatch(Input::Key(KeyPress::new(KeyCode::Esc))),
        vec![Msg::ClosePrompt]
    );
}

#[test]
fn a_subtree_can_read_a_chord_differently_from_the_global_map() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        focusable(text("vi mode"))
            .shortcut(KeyPress::char('q'), Intent::Cancel)
            .action(Intent::Cancel, |_| Msg::ClosePrompt),
        FRAME,
    );
    let f = ui.root().unwrap();
    ui.request_focus(f, SelectionOnFocus::None);
    assert_eq!(
        ui.dispatch(Input::Key(KeyPress::char('q'))),
        vec![Msg::ClosePrompt]
    );
}

#[test]
fn raw_key_listeners_run_before_intents_and_can_claim() {
    let mut ui: Ui<Msg> = Ui::new();
    let seen: Rc<RefCell<Vec<char>>> = Rc::default();
    let s = seen.clone();
    ui.frame(
        focusable(text("field"))
            .on_key(move |e: &Event| {
                if let Some(KeyPress {
                    code: KeyCode::Char(c),
                    ..
                }) = e.key
                {
                    s.borrow_mut().push(c);
                    e.stop();
                }
                None
            })
            .action(Intent::Cancel, |_| Msg::ClosePrompt),
        FRAME,
    );
    let f = ui.root().unwrap();
    ui.request_focus(f, SelectionOnFocus::None);

    ui.dispatch(Input::Key(KeyPress::char('x')));
    assert_eq!(*seen.borrow(), vec!['x']);
    // Esc is not a Char, so the raw listener declines and the intent resolves.
    assert_eq!(
        ui.dispatch(Input::Key(KeyPress::new(KeyCode::Esc))),
        vec![Msg::ClosePrompt]
    );
}

#[test]
fn focus_within_is_invalidated_only_below_the_common_ancestor() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        focusable(
            col().children([
                focusable(col().child(field("a")))
                    .focus_within()
                    .key("left"),
                focusable(col().child(field("b")))
                    .focus_within()
                    .key("right"),
            ]),
        )
        .focus_within()
        .key("root"),
        FRAME,
    );

    let top = ui.root().unwrap();
    let left = ui.at(&[0, 0]).unwrap();
    let right = ui.at(&[0, 1]).unwrap();
    let a = ui.at(&[0, 0, 0, 0]).unwrap();
    let b = ui.at(&[0, 1, 0, 0]).unwrap();

    ui.request_focus(a, SelectionOnFocus::None);
    ui.tick();
    assert!(ui.has_focus_within(left));
    assert!(!ui.has_focus_within(right));

    let top_builds = ui.builds(top);
    ui.request_focus(b, SelectionOnFocus::None);
    ui.tick();

    assert!(ui.has_focus_within(right));
    assert!(!ui.has_focus_within(left));
    assert_eq!(
        ui.builds(top),
        top_builds,
        "the common ancestor's answer did not change, so it was not rebuilt"
    );
}

#[test]
fn a_dismissible_layer_closes_on_escape() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        col().children([
            text("behind"),
            layer()
                .anchor(Anchor::Screen(Align::Center))
                .dismiss(fresh_ui::Dismiss::ESCAPE)
                .on_dismiss(|_| Msg::ClosePrompt)
                .child(text("menu")),
        ]),
        FRAME,
    );
    assert_eq!(
        ui.dispatch(Input::Key(KeyPress::new(KeyCode::Esc))),
        vec![Msg::ClosePrompt]
    );
}

#[test]
fn a_dismissible_layer_closes_on_a_click_outside_it() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        col().children([
            text("behind"),
            layer()
                .anchor(Anchor::Point(10, 5))
                .dismiss(fresh_ui::Dismiss::OUTSIDE_POINTER)
                .on_dismiss(|_| Msg::ClosePrompt)
                .child(text("menu")),
        ]),
        FRAME,
    );
    // Inside the menu: nothing.
    let inside = ui.dispatch(Input::press(
        fresh_ui::Point::new(11, 5),
        fresh_ui::MouseButton::Left,
        Mods::NONE,
    ));
    assert!(inside.is_empty());

    let outside = ui.dispatch(Input::press(
        fresh_ui::Point::new(1, 1),
        fresh_ui::MouseButton::Left,
        Mods::NONE,
    ));
    assert_eq!(outside, vec![Msg::ClosePrompt]);
}

#[test]
fn clicking_a_focusable_can_move_focus_to_it() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col().children([
            focusable(text("one")).on_key(|_| None),
            focusable(fresh_ui::gesture(text("two")).on(
                fresh_ui::GestureKind::Click,
                Rc::new(|e: &Event| {
                    e.request_focus(SelectionOnFocus::Caret(0));
                    None
                }),
            )),
        ]),
        FRAME,
    );
    let pos = fresh_ui::Point::new(1, 1);
    ui.dispatch(Input::press(pos, fresh_ui::MouseButton::Left, Mods::NONE));
    ui.dispatch(Input::release(pos, fresh_ui::MouseButton::Left, Mods::NONE));

    assert_eq!(ui.focused(), Some(ui.at(&[1, 0]).unwrap()));
    assert_eq!(ui.focus_selection(), SelectionOnFocus::Caret(0));
}

#[test]
fn moving_focus_directly_is_available_without_a_key() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(form(), FRAME);
    assert!(ui.move_focus(FocusDir::Next));
    assert_eq!(ui.focused(), Some(ui.at(&[0]).unwrap()));
}

/// **A direction needs somewhere to move from; Tab does not.**
///
/// `dispatch_key` resolves a key to an `Intent` and falls back to default
/// traversal even when nothing is focused, so an arrow key used to move focus
/// "from nowhere" onto the first focusable — and *claim* the key. In a host
/// that offers its keys to this tree before its own handlers, that key was the
/// application's: the editor's command palette lost a `Right` to it the moment
/// its frame gained a focusable, moving focus instead of the text cursor.
///
/// Tab is the gesture that means "enter the interface", and still does.
#[test]
fn an_arrow_key_with_nothing_focused_is_not_traversal() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        col().children([focusable(text("one")), focusable(text("two"))]),
        FRAME,
    );
    assert!(ui.focused().is_none(), "nothing is focused to begin with");

    for code in [KeyCode::Right, KeyCode::Left, KeyCode::Up, KeyCode::Down] {
        let got = ui.dispatch(Input::Key(KeyPress::new(code)));
        assert!(
            !got.claimed,
            "{code:?} with nothing focused belongs to the application"
        );
        assert!(ui.focused().is_none(), "{code:?} moved focus from nowhere");
    }

    // Tab does enter, and from then on the directions work as they always did.
    assert!(ui.dispatch(Input::Key(KeyPress::new(KeyCode::Tab))).claimed);
    assert!(ui.focused().is_some(), "Tab enters the interface");
    assert!(
        ui.dispatch(Input::Key(KeyPress::new(KeyCode::Down)))
            .claimed,
        "with a starting point, a direction traverses"
    );
}

// -- a layer that names its focus scope ---------------------------------------

/// The shape a panel's keyboard needs: a `Modality::Focus` layer that ranks
/// early, holding no content, and the content elsewhere in the tree.
///
/// `sink` stands for the fallback key handler the layer carries; `body` is the
/// panel, declared after it so it paints later.
fn scoped_panel(scope: bool) -> Node<()> {
    let keys = layer()
        .anchor(Anchor::Screen(Align::Start))
        .modality(Modality::Focus)
        .child(focusable(text("sink")).key("sink"));
    let keys = match scope {
        true => keys.scope_at("body".into()),
        false => keys,
    };
    col()
        .child(keys)
        .child(col().key("body").children([field("one"), field("two")]))
}

/// **Without a scope, confinement is containment and the content is outside
/// it.** This is the state the editor's plugin panels are in: every widget is
/// focusable and none is reachable, because the keyboard layer holds one node
/// and traversal is confined to the layer.
#[test]
fn a_keyboard_layer_confines_traversal_to_itself() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(scoped_panel(false), FRAME);
    let reachable: Vec<_> = ui
        .focus_scope()
        .ordered()
        .into_iter()
        .filter_map(|e| ui.key_of(e))
        .collect();
    assert_eq!(
        reachable,
        vec!["sink".into()],
        "the layer's own subtree is all traversal can reach"
    );
}

/// Naming a scope moves the confinement to the content, leaving the rank where
/// it was declared.
#[test]
fn a_named_scope_confines_traversal_to_the_content() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(scoped_panel(true), FRAME);
    let reachable: Vec<_> = ui
        .focus_scope()
        .ordered()
        .into_iter()
        .filter_map(|e| ui.key_of(e))
        .collect();
    assert_eq!(
        reachable,
        vec!["one".into(), "two".into()],
        "the named element's focusables, and not the layer's sink"
    );

    // A scope that opens takes focus, and the scope is the content now, so
    // the frame lands on the first control rather than on the sink.
    assert_eq!(ui.key_of(ui.focused().unwrap()), Some("one".into()));
    let tab = Input::Key(KeyPress::new(KeyCode::Tab));
    ui.dispatch(tab);
    assert_eq!(ui.key_of(ui.focused().unwrap()), Some("two".into()));
    ui.dispatch(tab);
    assert_eq!(
        ui.key_of(ui.focused().unwrap()),
        Some("one".into()),
        "wraps inside the scope rather than escaping to the sink"
    );
}

/// The containment questions the host asks must follow the scope too.
///
/// Focus is outside the layer's own subtree by construction here, so an
/// ancestor walk alone would report that no keyboard layer is up — and the
/// host would go on resolving keys the panel had claimed.
#[test]
fn a_scoped_layer_still_answers_the_containment_questions() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(scoped_panel(true), FRAME);
    assert_eq!(ui.key_of(ui.focused().unwrap()), Some("one".into()));
    assert!(
        ui.focus_confined(),
        "focus is inside the scope the keyboard layer named"
    );
}

/// A scope naming a key nothing carries confines nothing — the same answer as
/// not naming one, rather than an empty ring.
#[test]
fn a_scope_naming_nothing_falls_back_to_the_layer() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col()
            .child(
                layer()
                    .anchor(Anchor::Screen(Align::Start))
                    .modality(Modality::Focus)
                    .scope_at("absent".into())
                    .child(focusable(text("sink")).key("sink")),
            )
            .child(col().key("body").children([field("one")])),
        FRAME,
    );
    let reachable: Vec<_> = ui
        .focus_scope()
        .ordered()
        .into_iter()
        .filter_map(|e| ui.key_of(e))
        .collect();
    assert_eq!(reachable, vec!["sink".into()]);
}

/// **A scope whose content has nothing focusable still holds the keyboard.**
///
/// `skip_traversal` says Tab does not *stop* on a node; it does not say focus
/// may never *rest* there. `focus_scope` reads it as the former when it builds
/// the traversal set, so a scope root marked skippable is absent from its own
/// scope — and when its descendants are unfocusable too, the set is empty.
/// Focus was then dropped, and with focus nowhere the containment questions
/// answer that no keyboard layer is up at all, so the layer's keys leak to
/// whatever is behind it.
///
/// The editor reached this with a plugin panel whose interior is described but
/// holds no focusable control. The symptom was a dialog that would not close:
/// Escape never reached the panel, and the scrim it had raised outlived it,
/// swallowing clicks meant for the menu bar underneath.
#[test]
fn an_empty_scope_keeps_the_keyboard_at_its_root() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col()
            .child(
                layer()
                    .anchor(Anchor::Screen(Align::Start))
                    .modality(Modality::Focus)
                    .scope_at("body".into()),
            )
            // The scope root is focusable but skipped, exactly as a panel's
            // fallback key handler is, and nothing inside it takes focus.
            .child(
                focusable(col().child(text("nothing focusable")))
                    .key("body")
                    .skip_traversal(),
            ),
        FRAME,
    );
    let root = ui.find_by_key(&"body".into()).expect("the scope root");
    assert_eq!(
        ui.focused(),
        Some(root),
        "focus rests on the scope root rather than being dropped"
    );
    assert!(
        ui.focus_scope().ordered().is_empty(),
        "and it is still not a Tab stop"
    );
}

// -- the key capture leg ------------------------------------------------------

/// A capture listener sees the key before the focused element, and can decline
/// it without swallowing the rest.
///
/// This is what lets a surface reserve one key. Before it existed, the only
/// way to pre-empt a focused control was a bubble listener that stopped
/// everything — which is why a plugin panel that wants `Enter` ended up
/// claiming every key its widgets would have handled.
#[test]
fn a_capture_listener_runs_before_the_focused_element() {
    let log: Log = Rc::new(RefCell::new(Vec::new()));
    let (a, b) = (log.clone(), log.clone());
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        focusable(
            col().child(focusable(text("field")).key("field").on_key(move |_| {
                b.borrow_mut().push("field".into());
                None
            })),
        )
        .skip_traversal()
        .on_key_capture(move |_| {
            a.borrow_mut().push("root capture".into());
            None
        }),
        FRAME,
    );
    ui.dispatch(Input::Key(KeyPress::new(KeyCode::Tab)));
    log.borrow_mut().clear();
    ui.dispatch(Input::Key(KeyPress::new(KeyCode::Enter)));
    assert_eq!(
        *log.borrow(),
        vec!["root capture".to_string(), "field".to_string()],
        "down leg first, then the focused element"
    );
}

/// Stopping on the capture leg keeps the key from the focused element.
#[test]
fn a_capture_listener_that_stops_pre_empts_the_control() {
    let log: Log = Rc::new(RefCell::new(Vec::new()));
    let (a, b) = (log.clone(), log.clone());
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        focusable(
            col().child(focusable(text("field")).key("field").on_key(move |_| {
                b.borrow_mut().push("field".into());
                None
            })),
        )
        .skip_traversal()
        .on_key_capture(move |e: &Event| {
            a.borrow_mut().push("root capture".into());
            e.stop();
            None
        }),
        FRAME,
    );
    ui.dispatch(Input::Key(KeyPress::new(KeyCode::Tab)));
    log.borrow_mut().clear();
    ui.dispatch(Input::Key(KeyPress::new(KeyCode::Enter)));
    assert_eq!(
        *log.borrow(),
        vec!["root capture".to_string()],
        "the focused element never saw it"
    );
}

/// **A key the ring cannot serve is handed back by one modality and swallowed
/// by the other.** Which of the two a surface declared decides whether it may
/// decline a key at all.
///
/// Declining is how a surface lets the tree's ring resolve a key: the fallback
/// returns without `stop`, `propagate_key` reports nothing claimed, and
/// `default_for_intent` moves focus. When the scope holds one focusable there
/// is nowhere to move, `move_focus` answers false — and what happens to the key
/// then is the layer's modality, not the fallback's choice.
///
/// `Modality::Focus` confines traversal without swallowing, so the key leaves
/// `dispatch` unclaimed and the host's own pipeline still answers it. That is
/// what lets a plugin panel's fallback decline Tab unconditionally: a panel
/// holding one widget, or none, still hands Tab back to the router that had it
/// before.
///
/// `Modality::Keyboard` swallows what nothing acted on — that is what makes an
/// open menu a dead end for a stray key — so the same declined Tab is claimed
/// by the layer with no message, no move and no host. A surface declared that
/// way can only decline a key it knows the ring will serve, which is not
/// something a description can know about itself.
#[test]
fn a_key_the_ring_cannot_serve_is_handed_back_only_by_a_focus_layer() {
    // One focusable under a fallback that declines Tab and claims the rest —
    // the shape both the plugin panel and the settings dialog have.
    let one_focusable = |m: Modality| {
        let mut ui: Ui<()> = Ui::new();
        ui.frame(
            col().child(
                layer()
                    .anchor(Anchor::Screen(Align::Start))
                    .modality(m)
                    .child(
                        focusable(col().child(field("only")))
                            .key("fallback")
                            .skip_traversal()
                            .on_key(|e: &Event| {
                                if e.key.is_some_and(|k| k.code == KeyCode::Tab) {
                                    return None;
                                }
                                e.stop();
                                None
                            }),
                    ),
            ),
            FRAME,
        );
        // The first Tab settles focus on the only focusable; the second is the
        // one with nowhere to go.
        ui.dispatch(Input::Key(KeyPress::new(KeyCode::Tab)));
        let landed = ui.focused();
        let d = ui.dispatch(Input::Key(KeyPress::new(KeyCode::Tab)));
        assert_eq!(ui.focused(), landed, "there was nowhere else to go");
        assert!(d.msgs.is_empty(), "and nothing was said about it");
        d.claimed
    };
    assert!(
        !one_focusable(Modality::Focus),
        "a focus layer hands back the Tab it could not serve"
    );
    assert!(
        one_focusable(Modality::Keyboard),
        "a keyboard layer swallows it, and the host never hears it"
    );
}
