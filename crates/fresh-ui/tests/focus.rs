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
    // As a terminal reports it: BackTab with Shift still on.
    ui.dispatch(Input::Key(KeyPress::with(KeyCode::BackTab, Mods::SHIFT)));
    assert_eq!(ui.focused(), Some(ids[1]));
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

/// **A `layout_reader` relinks its own subtree; it does not decide its focus
/// parent's whole child list.**
///
/// A reader's nearest focus ancestor is almost never its own node — it is
/// whatever focusable encloses it — and that focusable's children include the
/// widgets beside the reader and every *other* reader under it. Replacing
/// that list with one reader's contribution made the last reader to rebuild
/// the only one in the scope, so a scope holding two readers ended the frame
/// with whatever the second one contributed: nothing, here, and traversal had
/// nowhere to go while two focusables sat in the tree pointing at it.
///
/// The editor's plugin panels are exactly this shape — a described interior
/// wrapping a reader, with more readers inside the description for the
/// windowed lists — which is how a picker full of buttons answered Tab by
/// doing nothing at all.
#[test]
fn a_second_reader_does_not_empty_its_focus_parents_ring() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        focusable(col().children([
            fresh_ui::layout_reader(|_: fresh_ui::LayoutInfo| {
                col().children([field("one"), field("two")])
            }),
            // Nothing focusable, and it relinks after the one above.
            fresh_ui::layout_reader(|_: fresh_ui::LayoutInfo| text("tail")),
        ]))
        .key("scope")
        .skip_traversal(),
        FRAME,
    );

    let one = ui
        .find_by_key(&fresh_ui::Key::Str("one".into()))
        .expect("the first reader's subtree is in the tree");
    let two = ui
        .find_by_key(&fresh_ui::Key::Str("two".into()))
        .expect("and so is the rest of it");

    let tab = Input::Key(KeyPress::new(KeyCode::Tab));
    ui.dispatch(tab);
    assert_eq!(ui.focused(), Some(one), "the scope's ring is not empty");
    ui.dispatch(tab);
    assert_eq!(ui.focused(), Some(two), "and Tab walks all of it");
}

// -- a mark that moves ----------------------------------------------------------
//
// The fourth case of `apply_autofocus`. A description can *say* where focus
// is by marking a control `autofocus`; the tree lands there when the scope
// opens, and follows when the mark moves — while the ring's own moves are
// left alone, because a mark that has not moved is not a decision.

fn marked(name: &'static str, mark: bool) -> Node<&'static str> {
    let n = focusable(text(name))
        .key(name)
        .h(Sizing::Cells(1))
        .on_focus_change(move |e: &Event| {
            (e.kind == fresh_ui::GestureKind::FocusGained).then_some(name)
        });
    match mark {
        true => n.autofocus(),
        false => n,
    }
}

/// The dock's shape: a keyboard layer that names an in-flow interior as its
/// scope, so the layer ranks early and the content paints late.
fn panel(fields: Vec<Node<&'static str>>) -> Node<&'static str> {
    col().children([
        layer()
            .anchor(Anchor::Screen(Align::Start))
            .modality(Modality::Focus)
            .scope_at("scope".into()),
        focusable(col().children(fields))
            .key("scope")
            .skip_traversal(),
    ])
}

#[test]
fn a_mark_that_moves_inside_the_scope_moves_focus_with_one_gain() {
    let mut ui: Ui<&'static str> = Ui::new();
    ui.frame(panel(vec![marked("a", true), marked("b", false)]), FRAME);
    assert_eq!(
        ui.focused(),
        ui.find_by_key(&"a".into()),
        "entered on the mark"
    );
    ui.take_messages();

    ui.frame(panel(vec![marked("a", false), marked("b", true)]), FRAME);
    assert_eq!(
        ui.focused(),
        ui.find_by_key(&"b".into()),
        "the mark moved; focus followed"
    );
    assert_eq!(ui.take_messages(), vec!["b"], "one landing, one gain");
}

#[test]
fn a_mark_that_stays_does_not_undo_the_rings_own_move() {
    let mut ui: Ui<&'static str> = Ui::new();
    ui.frame(panel(vec![marked("a", true), marked("b", false)]), FRAME);
    ui.dispatch(Input::Key(KeyPress::new(KeyCode::Tab)));
    assert_eq!(
        ui.focused(),
        ui.find_by_key(&"b".into()),
        "Tab moved the ring"
    );
    ui.take_messages();

    // The description has not caught up: it still marks `a`.
    ui.frame(panel(vec![marked("a", true), marked("b", false)]), FRAME);
    assert_eq!(
        ui.focused(),
        ui.find_by_key(&"b".into()),
        "a stale mark is not a decision; the ring's move stands"
    );
    assert!(ui.take_messages().is_empty());
}

#[test]
fn a_mark_on_an_element_first_built_this_frame_lands_on_it() {
    let mut ui: Ui<&'static str> = Ui::new();
    ui.frame(panel(vec![marked("a", true)]), FRAME);
    assert_eq!(ui.focused(), ui.find_by_key(&"a".into()));
    ui.take_messages();

    // The frame that first builds `c` is the one that marks it — the shape
    // of a dropdown row that is described and focused in one plugin turn.
    ui.frame(panel(vec![marked("a", false), marked("c", true)]), FRAME);
    assert_eq!(
        ui.focused(),
        ui.find_by_key(&"c".into()),
        "landed on the frame that built the element, with no replay"
    );
    assert_eq!(ui.take_messages(), vec!["c"]);
}

#[test]
fn a_mark_moving_outside_the_active_scope_moves_nothing() {
    let group = |name: &'static str, fields: Vec<Node<&'static str>>| {
        focusable(col().children(fields))
            .key(name)
            .skip_traversal()
            .focus_scope()
    };
    let mut ui: Ui<&'static str> = Ui::new();
    ui.frame(
        col().children([
            group("one", vec![marked("x", false), marked("y", false)]),
            group("two", vec![marked("p", true), marked("q", false)]),
        ]),
        FRAME,
    );
    let x = ui.find_by_key(&"x".into()).unwrap();
    ui.request_focus(x, SelectionOnFocus::None);
    ui.take_messages();

    // `two`'s mark moves while focus is confined to `one`.
    ui.frame(
        col().children([
            group("one", vec![marked("x", false), marked("y", false)]),
            group("two", vec![marked("p", false), marked("q", true)]),
        ]),
        FRAME,
    );
    assert_eq!(
        ui.focused(),
        Some(x),
        "a mark outside the confinement is entry-only"
    );
    assert!(ui.take_messages().is_empty());
}

#[test]
fn a_mark_that_goes_away_rests_focus_on_the_scope() {
    let mut ui: Ui<&'static str> = Ui::new();
    ui.frame(panel(vec![marked("a", true), marked("b", false)]), FRAME);
    assert_eq!(ui.focused(), ui.find_by_key(&"a".into()));
    ui.take_messages();

    ui.frame(panel(vec![marked("a", false), marked("b", false)]), FRAME);
    assert_eq!(
        ui.focused(),
        ui.find_by_key(&"scope".into()),
        "nothing marked is a state the tree holds on the scope's own element"
    );
    // And the ring starts from outside: the first Tab reaches the first
    // focusable rather than skipping it.
    ui.dispatch(Input::Key(KeyPress::new(KeyCode::Tab)));
    assert_eq!(ui.focused(), ui.find_by_key(&"a".into()));
}

// -- observed, not claimed -------------------------------------------------------

/// **A key can be observed without being claimed.** A subtree whose keys are
/// bound outside the tree — a plugin panel whose Tab is the plugin's own —
/// needs propagation and the tree's intent resolution to end at it while the
/// key still reaches the host. `Flow::Stop` would swallow it; `Flow::Continue`
/// would let traversal have it. Observing is the third answer.
#[test]
fn an_observed_key_ends_propagation_and_traversal_but_is_not_claimed() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        focusable(col().children([field("one"), field("two")]))
            .key("interior")
            .skip_traversal()
            .on_key(|e: &Event| {
                e.observe();
                None
            }),
        FRAME,
    );
    let one = ui.find_by_key(&"one".into()).unwrap();
    ui.request_focus(one, SelectionOnFocus::None);
    let d = ui.dispatch(Input::Key(KeyPress::new(KeyCode::Tab)));
    assert_eq!(ui.focused(), Some(one), "traversal did not move focus");
    assert!(!d.claimed, "and the key is still the host's");
    // A stop recorded first stands: observing cannot un-claim.
    let mut ui2: Ui<()> = Ui::new();
    ui2.frame(
        focusable(
            col().child(focusable(text("inner")).key("inner").on_key(|e: &Event| {
                e.stop();
                None
            })),
        )
        .key("outer")
        .skip_traversal()
        .on_key(|e: &Event| {
            e.observe();
            None
        }),
        FRAME,
    );
    let inner = ui2.find_by_key(&"inner".into()).unwrap();
    ui2.request_focus(inner, SelectionOnFocus::None);
    let d = ui2.dispatch(Input::Key(KeyPress::new(KeyCode::Enter)));
    assert!(
        d.claimed,
        "the inner stop claimed it before the outer observer ran"
    );
}

/// A focusable built inside a `layout_reader` is an ordinary ring member: a
/// mark on it lands, stays across frames, and its keys reach it.
///
/// The editor's settings dialog builds its whole box inside a reader (the
/// box is sized from the frame), and its focus seams once sat *outside* the
/// reader on the belief that a registration made during the layout pass was
/// gone by the next frame. Autofocus settles after layout, and a keyed
/// element is the same element from one layout to the next, so it is not.
#[test]
fn a_marked_focusable_inside_a_layout_reader_holds_focus_across_frames() {
    let log: Log = Rc::new(RefCell::new(Vec::new()));
    let build = {
        let log = log.clone();
        move || {
            let log = log.clone();
            layer()
                .anchor(Anchor::Screen(Align::Center))
                .modality(Modality::Keyboard)
                .scope_at("box".into())
                .child(fresh_ui::layout_reader(move |_info| {
                    let log = log.clone();
                    col()
                        .key("box")
                        .w(Sizing::Cells(10))
                        .h(Sizing::Cells(3))
                        .children([
                            field("a"),
                            focusable(text("b"))
                                .key("b")
                                .h(Sizing::Cells(1))
                                .autofocus()
                                .on_key(move |e: &Event| {
                                    let code = e.key?.code;
                                    if code != KeyCode::Enter {
                                        return None;
                                    }
                                    log.borrow_mut().push(format!("{code:?}"));
                                    e.stop();
                                    None
                                }),
                            field("c"),
                        ])
                }))
        }
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(build(), FRAME);
    let b = ui.find_by_key(&"b".into()).expect("b");
    assert_eq!(
        ui.focused(),
        Some(b),
        "the mark lands on the frame it is built"
    );
    ui.frame(build(), FRAME);
    ui.frame(build(), FRAME);
    assert_eq!(ui.find_by_key(&"b".into()), Some(b), "the same element");
    assert_eq!(ui.focused(), Some(b), "and still focused three frames on");
    ui.dispatch(Input::Key(KeyPress::new(KeyCode::Enter)));
    assert_eq!(log.borrow().as_slice(), ["Enter"], "its keys reach it");
    ui.dispatch(Input::Key(KeyPress::new(KeyCode::Tab)));
    assert_eq!(
        ui.focused(),
        ui.find_by_key(&"c".into()),
        "and the ring walks the reader's siblings"
    );
}

/// A mark that moves *between* two `layout_reader` subtrees — one nested in
/// the other — is followed on the frame that carries it, not the next one.
///
/// The editor's settings dialog is a reader (the box) holding a reader (the
/// footer row); a fact moving from a card to a footer button is exactly
/// this. The readers rebuild during the layout pass, so the second reader's
/// mark exists only after the layout dirt it raised is flushed; settling
/// focus before that flush finds no mark and rests on the scope.
#[test]
fn a_mark_moving_into_a_nested_layout_reader_is_followed_on_that_frame() {
    let build = |mark_button: bool| {
        layer()
            .anchor(Anchor::Screen(Align::Center))
            .modality(Modality::Keyboard)
            .scope_at("box".into())
            .child(fresh_ui::layout_reader(move |_info| {
                let field = focusable(text("a")).key("a").h(Sizing::Cells(1));
                let field = match mark_button {
                    false => field.autofocus(),
                    true => field,
                };
                focusable(col().children([
                    field,
                    fresh_ui::layout_reader(move |_info| {
                        let b = focusable(text("b")).key("b").h(Sizing::Cells(1));
                        match mark_button {
                            true => b.autofocus(),
                            false => b,
                        }
                    }),
                ]))
                .w(Sizing::Cells(10))
                .h(Sizing::Cells(3))
                .key("box")
                .skip_traversal()
            }))
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(build(false), FRAME);
    assert_eq!(ui.focused(), ui.find_by_key(&"a".into()));
    ui.frame(build(true), FRAME);
    assert_eq!(
        ui.focused(),
        ui.find_by_key(&"b".into()),
        "the mark moved into the nested reader, and focus followed this frame"
    );
}

/// A group is entered at the stop it names, from either direction, and
/// stepped through in reading order once inside.
#[test]
fn a_group_is_entered_at_its_entry_stop_and_walked_inside() {
    let build = || {
        col().children([
            field("before"),
            focusable(col().children([field("g0"), field("g1"), field("g2")]))
                .skip_traversal()
                .enters_at("g1".into()),
            field("after"),
        ])
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(build(), FRAME);
    let at = |ui: &Ui<()>, k: &str| ui.find_by_key(&k.into()).unwrap();
    let tab = Input::Key(KeyPress::new(KeyCode::Tab));
    let back = Input::Key(KeyPress::new(KeyCode::BackTab));
    ui.dispatch(tab);
    assert_eq!(ui.focused(), Some(at(&ui, "before")));
    ui.dispatch(tab);
    assert_eq!(
        ui.focused(),
        Some(at(&ui, "g1")),
        "entered at the entry, not the first"
    );
    ui.dispatch(tab);
    assert_eq!(ui.focused(), Some(at(&ui, "g2")), "inside, reading order");
    ui.dispatch(tab);
    assert_eq!(ui.focused(), Some(at(&ui, "after")), "and out at the end");
    ui.dispatch(back);
    assert_eq!(
        ui.focused(),
        Some(at(&ui, "g1")),
        "entered at the entry backwards too"
    );
    ui.dispatch(back);
    assert_eq!(ui.focused(), Some(at(&ui, "g0")));
    ui.dispatch(back);
    assert_eq!(ui.focused(), Some(at(&ui, "before")));
}

/// An entry that names nothing on the ring is no entry: the group is entered
/// where it would have been.
#[test]
fn a_group_whose_entry_is_not_a_stop_is_entered_in_order() {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col().children([
            field("before"),
            focusable(col().children([field("g0"), text("plain").key("plain")]))
                .skip_traversal()
                .enters_at("plain".into()),
        ]),
        FRAME,
    );
    let tab = Input::Key(KeyPress::new(KeyCode::Tab));
    ui.dispatch(tab);
    ui.dispatch(tab);
    assert_eq!(ui.focused(), ui.find_by_key(&"g0".into()));
}

/// **A layer that stops confining releases what it held.** The editor's dock:
/// its interior is a subtree that stays described whether or not the dock
/// holds the keyboard, and the keyboard layer that scopes to it comes and
/// goes with the host's focus fact. While the layer is up, the dock's marked
/// widget holds focus. When the layer goes and the mark moves to the pane
/// behind, focus must go with it — the dock's widget is still there, still
/// focusable, and still where focus was left, so nothing but this rule moves
/// it.
#[test]
fn a_layer_that_stops_confining_releases_focus_to_the_enclosing_mark() {
    let build = |dock_keys: bool| {
        let pane = focusable(text("pane"))
            .key("pane")
            .skip_traversal()
            .h(Sizing::Cells(1));
        let pane = match dock_keys {
            true => pane,
            false => pane.autofocus(),
        };
        let widget = focusable(text("w")).key("w").h(Sizing::Cells(1));
        let widget = match dock_keys {
            true => widget.autofocus(),
            false => widget,
        };
        let dock = col().key("dock").children([widget, field("x")]);
        let root = col().children([dock, pane]);
        match dock_keys {
            true => root.child(
                layer()
                    .anchor(Anchor::Screen(Align::Start))
                    .modality(Modality::Focus)
                    .scope_at("dock".into()),
            ),
            false => root,
        }
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(build(true), FRAME);
    let w = ui.find_by_key(&"w".into()).expect("w");
    assert_eq!(ui.focused(), Some(w), "the confined mark holds focus");
    // Tab inside the confinement: the ring's own move, which the mark must
    // not undo on the next frame.
    ui.dispatch(Input::Key(KeyPress::new(KeyCode::Tab)));
    let x = ui.find_by_key(&"x".into()).expect("x");
    assert_eq!(ui.focused(), Some(x));
    ui.frame(build(true), FRAME);
    assert_eq!(ui.focused(), Some(x), "a stale mark does not undo Tab");

    // The layer goes and the mark moves to the pane.
    ui.frame(build(false), FRAME);
    let pane = ui.find_by_key(&"pane".into()).expect("pane");
    assert_eq!(
        ui.focused(),
        Some(pane),
        "focus left behind by a gone confinement goes to the enclosing mark"
    );
    // And stays there across frames that change nothing.
    ui.frame(build(false), FRAME);
    assert_eq!(ui.focused(), Some(pane));
}

/// Outside every confinement the whole tree is the scope: a mark moving from
/// one subtree to another at the root is followed the same way a mark moving
/// inside a dialog is.
#[test]
fn a_mark_moving_between_subtrees_at_the_root_is_followed() {
    let build = |which: &'static str| {
        let one = focusable(text("one")).key("one").h(Sizing::Cells(1));
        let two = focusable(text("two")).key("two").h(Sizing::Cells(1));
        let (one, two) = match which {
            "one" => (one.autofocus(), two),
            _ => (one, two.autofocus()),
        };
        col().children([col().child(one), col().child(two)])
    };
    let mut ui: Ui<()> = Ui::new();
    ui.frame(build("one"), FRAME);
    let one = ui.find_by_key(&"one".into()).expect("one");
    assert_eq!(ui.focused(), Some(one));
    ui.frame(build("two"), FRAME);
    let two = ui.find_by_key(&"two".into()).expect("two");
    assert_eq!(
        ui.focused(),
        Some(two),
        "the root's mark moved, focus follows"
    );
    // Tab is the ring's move; a mark that has not moved leaves it alone.
    ui.dispatch(Input::Key(KeyPress::new(KeyCode::Tab)));
    assert_eq!(ui.focused(), Some(one));
    ui.frame(build("two"), FRAME);
    assert_eq!(ui.focused(), Some(one), "a stale mark does not undo Tab");
}
