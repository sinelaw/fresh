//! Scoped persistence: what a document switch does to element state.
//!
//! `Persisted` is the library's answer to a question every multi-document host
//! asks — *this subtree is going away and coming back; what survives?* Its own
//! documentation states the contract: a value is read at construction and
//! written back at teardown, and "keys are anchored to the enclosing
//! `PersistenceScope` rather than to tree position, so moving a widget does not
//! lose its value and two widgets at the same position under different
//! documents do not share one."
//!
//! Every claim in that sentence is load-bearing for a host that keys a document
//! subtree and swaps it, and none of them was covered. These tests are the
//! cover. They are deliberately written against the *host's* usage shape —
//! a keyed subtree per document, a scope ambient at its root, identical keys
//! inside — rather than against the behavior in isolation, because the failure
//! modes are all about ordering and identity between two subtrees rather than
//! about one value round-tripping.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use fresh_ui::ambient::{provide, scope};
use fresh_ui::behavior::{Persisted, Store, PERSISTENCE_SCOPE};
use fresh_ui::key::Key;
use fresh_ui::{col, text, BuildCx, Component, ComponentExt, InitCx, Node, Size, Ui};

// ---------------------------------------------------------------------------
// A host store, and a log of what the library asked it to do
// ---------------------------------------------------------------------------

/// The host's side of persistence, plus a transcript.
///
/// The transcript is most of the point: two of the four things under test are
/// about *ordering* between one subtree's teardown and the next one's
/// construction, and a store that only remembers its final contents cannot
/// tell a correct order from a lucky one.
#[derive(Default)]
struct Spy {
    values: RefCell<HashMap<String, Rc<dyn std::any::Any>>>,
    log: RefCell<Vec<String>>,
}

impl Spy {
    fn log(&self) -> Vec<String> {
        self.log.borrow().clone()
    }
}

impl Store for Spy {
    fn get(&self, key: &str) -> Option<Rc<dyn std::any::Any>> {
        let v = self.values.borrow().get(key).cloned();
        self.log
            .borrow_mut()
            .push(format!("get {key} -> {}", v.is_some()));
        v
    }

    fn set(&self, key: &str, value: Rc<dyn std::any::Any>) {
        self.log.borrow_mut().push(format!("set {key}"));
        self.values.borrow_mut().insert(key.into(), value);
    }
}

// ---------------------------------------------------------------------------
// A leaf that keeps one scoped value
// ---------------------------------------------------------------------------

/// A component holding one persisted `String`, under whatever scope encloses
/// it. Its own key is the same in every document, on purpose: that collision
/// is what the scope has to survive.
struct Note {
    /// What this instance writes into its value once mounted. `None` leaves
    /// whatever was restored, which is how the tests read a value back.
    write: Option<&'static str>,
}

/// `Option`, because `Component::State` must be `Default` and a live
/// behavior handle has no default — the same shape `tests/behavior.rs` uses.
#[derive(Default)]
struct NoteState {
    value: Option<Rc<Persisted<String>>>,
}

impl Component<()> for Note {
    type State = NoteState;

    fn init(&self, cx: &mut InitCx<'_, ()>) -> NoteState {
        // The scope is read by the caller — "the framework does not guess it"
        // — so a component that forgets this line silently shares one value
        // across every document. See the last test.
        let scope = cx
            .read(&PERSISTENCE_SCOPE)
            .map(|s| (*s).clone())
            .unwrap_or_else(|| "<none>".into());
        let value = cx.register(Persisted::new(&scope, "note", String::new()));
        if let Some(w) = self.write {
            value.set(w.into());
        }
        NoteState { value: Some(value) }
    }

    fn build(&self, s: &NoteState, _cx: &mut BuildCx<'_, ()>) -> Node<()> {
        text(s.value.as_ref().map(|v| v.get()).unwrap_or_default())
    }
}

/// One document: `scope()` provides the persistence scope and keys the
/// subtree by the same id, which is the pairing this whole file is about.
fn document(id: &str, write: Option<&'static str>) -> Node<()> {
    scope(
        id,
        col().child(Note { write }.node().key(Key::Str("note".into()))),
    )
}

fn shown(ui: &Ui<()>) -> String {
    ui.spec()
        .in_flow()
        .iter()
        .filter_map(|i| match &i.draw {
            fresh_ui::render::spec::Draw::Lines(l) => Some(l.join("")),
            _ => None,
        })
        .collect::<Vec<_>>()
        .join("")
}

fn ui_with(store: &Rc<Spy>) -> Ui<()> {
    let mut ui: Ui<()> = Ui::new();
    ui.set_store(store.clone() as Rc<dyn Store>);
    ui
}

// ---------------------------------------------------------------------------

/// **A key change discards a subtree, and discarding it writes the value back.**
///
/// The whole scheme rests on this: a host that swaps documents by changing one
/// key gets teardown for free. If `teardown` only ran when the `Ui` dropped,
/// every switch would lose the value and the loss would look exactly like
/// "persistence is not wired up yet".
#[test]
fn switching_documents_writes_the_outgoing_value_back() {
    let spy = Rc::new(Spy::default());
    let mut ui = ui_with(&spy);

    ui.frame(document("alpha", Some("alpha's note")), Size::new(40, 4));
    assert_eq!(shown(&ui), "alpha's note");

    // The switch: a different key at the same position.
    ui.frame(document("beta", Some("beta's note")), Size::new(40, 4));
    assert_eq!(shown(&ui), "beta's note");

    assert!(
        spy.log().contains(&"set alpha/note".to_string()),
        "alpha's note must be written back when its subtree goes away, log: {:?}",
        spy.log()
    );
}

/// **A value is in the store as soon as it is set, not when its element dies.**
///
/// This is the ordering hazard, and the reason `Persisted::set` writes
/// through. Disposal is deferred to the end of a flush — reconcile is
/// transactional and must be unwindable — so the *replacement* subtree is
/// constructed before the outgoing one is disposed. Under a teardown-only
/// write the log read `get alpha`, `get beta`, `set alpha`: anything mounted
/// in the same flush saw the store before the outgoing value reached it, and a
/// widget whose move reconciliation could not match by key read the default
/// and then had the old instance's value written over the top of it.
///
/// Writing on change removes the question entirely: the store is current
/// whenever anyone looks, whatever order disposal runs in.
#[test]
fn a_value_reaches_the_store_when_it_is_set() {
    let spy = Rc::new(Spy::default());
    let mut ui = ui_with(&spy);

    ui.frame(document("alpha", Some("written")), Size::new(40, 4));
    assert!(
        spy.log().contains(&"set alpha/note".to_string()),
        "the value must be in the store while its element is still alive, log: {:?}",
        spy.log()
    );

    // And it is there before anything the next flush constructs asks for it.
    ui.frame(document("beta", Some("other")), Size::new(40, 4));
    let log = spy.log();
    let set_alpha = log.iter().position(|l| l == "set alpha/note").unwrap();
    let get_beta = log
        .iter()
        .position(|l| l.starts_with("get beta/note"))
        .unwrap();
    assert!(
        set_alpha < get_beta,
        "alpha's value is stored before beta is built, log: {log:?}"
    );
}

/// **A document's value comes back when the document does.**
///
/// The round trip, through a discard — which is the case a host actually hits,
/// and the one that distinguishes "persisted" from "the element happened to
/// survive".
#[test]
fn a_documents_value_survives_being_switched_away_from() {
    let spy = Rc::new(Spy::default());
    let mut ui = ui_with(&spy);

    ui.frame(document("alpha", Some("kept")), Size::new(40, 4));
    ui.frame(document("beta", Some("beta's own")), Size::new(40, 4));
    // Back to alpha, writing nothing: what shows is what was restored.
    ui.frame(document("alpha", None), Size::new(40, 4));

    assert_eq!(
        shown(&ui),
        "kept",
        "alpha's note should be restored from the store, log: {:?}",
        spy.log()
    );
}

/// **Two documents at the same position, with the same key, do not share.**
///
/// This is the sentence in `Persisted`'s own documentation, and it is the one
/// a multi-document host stakes everything on. The two notes here are the same
/// component, at the same position, under the same key — the collision is
/// deliberate, because per-document ids that restart at 1 produce exactly it.
#[test]
fn two_documents_do_not_share_one_value() {
    let spy = Rc::new(Spy::default());
    let mut ui = ui_with(&spy);

    ui.frame(document("alpha", Some("alpha")), Size::new(40, 4));
    ui.frame(document("beta", Some("beta")), Size::new(40, 4));
    ui.frame(document("alpha", None), Size::new(40, 4));
    assert_eq!(shown(&ui), "alpha");
    ui.frame(document("beta", None), Size::new(40, 4));
    assert_eq!(shown(&ui), "beta");

    let values = spy.values.borrow();
    assert!(
        values.contains_key("alpha/note") && values.contains_key("beta/note"),
        "each document keeps its own entry, got {:?}",
        values.keys().collect::<Vec<_>>()
    );
}

/// **Only the visible document is mounted.**
///
/// The efficiency claim a host relies on when it decides *not* to keep every
/// document's subtree alive: switching does not leave the outgoing document
/// costing anything. If both stayed mounted, the display list would carry both
/// notes.
#[test]
fn the_document_that_is_not_shown_costs_nothing() {
    let spy = Rc::new(Spy::default());
    let mut ui = ui_with(&spy);

    ui.frame(document("alpha", Some("alpha")), Size::new(40, 4));
    ui.frame(document("beta", Some("beta")), Size::new(40, 4));

    let out = shown(&ui);
    assert!(
        out.contains("beta") && !out.contains("alpha"),
        "only the mounted document contributes to the display list, got {out:?}"
    );
}

/// **Rebuilding the same document, frame after frame, is not a scope change.**
///
/// The case a host that rebuilds its whole description every frame is in all
/// the time — which the cost model invites, since a rebuild costs one
/// allocation per node. Each frame hands `provide` a *fresh* `Rc` holding an
/// equal value. Under pointer identity that is a change: every dependent is
/// marked dirty every frame, and a `Persisted` beneath it trips the "ambient
/// read in `init()` changed" assertion on the second frame having never
/// actually changed. `scope()` provides through `provide_eq`, so an equal
/// value re-provided is not a change.
#[test]
fn rebuilding_the_same_document_is_not_a_scope_change() {
    let spy = Rc::new(Spy::default());
    let mut ui = ui_with(&spy);

    ui.frame(document("alpha", Some("written")), Size::new(40, 4));
    ui.frame(document("alpha", None), Size::new(40, 4));
    ui.frame(document("alpha", None), Size::new(40, 4));

    assert_eq!(shown(&ui), "written");
    let gets = spy
        .log()
        .iter()
        .filter(|l| l.starts_with("get alpha/note"))
        .count();
    assert_eq!(
        gets,
        1,
        "one construction across three frames of the same document, log: {:?}",
        spy.log()
    );
}

/// **The trap `scope()` exists to close.**
///
/// Plain `provide` compares by pointer, so a fresh `Rc` of an equal value is a
/// change; `Persisted` reads its scope in `init()`, which the library forbids
/// changing for a live element. The two meet on the second frame. This is not
/// a bug in either — it is the pairing being separable — and it is why the
/// boundary is one node.
#[test]
#[should_panic(expected = "only read it in init()")]
fn provide_by_pointer_identity_and_a_constructor_read_do_not_mix() {
    let spy = Rc::new(Spy::default());
    let mut ui = ui_with(&spy);
    let doc = |id: &str| {
        provide(
            &PERSISTENCE_SCOPE,
            Rc::new(id.to_string()),
            col().child(Note { write: None }.node().key(Key::Str("note".into()))),
        )
    };
    ui.frame(doc("alpha"), Size::new(40, 4));
    ui.frame(doc("alpha"), Size::new(40, 4));
}

// ---------------------------------------------------------------------------
// `MemStore`: the floor every host stands on, and the one operation a map lacks
// ---------------------------------------------------------------------------

/// A host that installs the shipped store gets the round trip, with no store
/// of its own to write. This is the same switch as
/// `a_documents_value_survives_being_switched_away_from`, driven through
/// `MemStore` instead of the spy — because the spy is a test double and the
/// thing hosts will actually use is this.
#[test]
fn the_shipped_store_carries_a_value_across_a_switch() {
    use fresh_ui::behavior::MemStore;
    let store = Rc::new(MemStore::new());
    let mut ui: Ui<()> = Ui::new();
    ui.set_store(store.clone() as Rc<dyn Store>);

    ui.frame(document("alpha", Some("kept")), Size::new(40, 4));
    ui.frame(document("beta", None), Size::new(40, 4));
    assert_eq!(shown(&ui), "", "beta has its own value, not alpha's");
    ui.frame(document("alpha", None), Size::new(40, 4));
    assert_eq!(shown(&ui), "kept", "alpha's value came back");
}

/// **The operation a plain map does not have.** A scope's values are dead when
/// the thing it names is gone, and the tree cannot know that — an unmounted
/// subtree is exactly the case where the values must be *kept*. So the host
/// says so, and `forget_scope` is how.
///
/// The separator is the part worth pinning: prefix-matching on the bare scope
/// name would take `window:10` down with `window:1`.
#[test]
fn forgetting_a_scope_takes_that_scope_and_no_neighbour() {
    use fresh_ui::behavior::MemStore;
    let store = MemStore::new();
    store.set("window:1/scroll", Rc::new(7u32));
    store.set("window:1/filter", Rc::new(String::from("x")));
    store.set("window:10/scroll", Rc::new(9u32));
    store.set("window:2/scroll", Rc::new(3u32));
    assert_eq!(store.len(), 4);

    store.forget_scope("window:1");

    assert_eq!(store.len(), 2, "both of window:1's values went");
    assert!(store.get("window:1/scroll").is_none());
    assert!(store.get("window:1/filter").is_none());
    assert!(
        store.get("window:10/scroll").is_some(),
        "window:10 is not inside window:1"
    );
    assert!(store.get("window:2/scroll").is_some());
}

/// A forgotten scope is a *new* document when it comes back: the next mount
/// reads the default, not the value the last one left. Which is the whole
/// point — a closed workspace's scroll offset must not be waiting for the next
/// workspace that happens to reuse its id.
#[test]
fn a_forgotten_scope_comes_back_empty() {
    use fresh_ui::behavior::MemStore;
    let store = Rc::new(MemStore::new());
    let mut ui: Ui<()> = Ui::new();
    ui.set_store(store.clone() as Rc<dyn Store>);

    ui.frame(document("alpha", Some("kept")), Size::new(40, 4));
    ui.frame(document("beta", None), Size::new(40, 4));
    store.forget_scope("alpha");
    ui.frame(document("alpha", None), Size::new(40, 4));

    assert_eq!(shown(&ui), "", "the value was forgotten, not restored");
}
