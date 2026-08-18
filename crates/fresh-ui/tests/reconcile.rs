//! Reconciler exit criteria.
//!
//! Every case here is stated in the implementation plan as a required L1
//! behaviour. They are the semantics every later phase is built on.

use std::panic::AssertUnwindSafe;
use std::rc::Rc;

mod support;
use fresh_ui::{col, row, shared_rc, text, BuildCx, Component, ComponentExt, Node, Ui};
use support::fake::Recorder;

fn ui() -> (Recorder, Ui<()>) {
    let rec = Recorder::new();
    let ui = Ui::with_renderer(Box::new(rec.clone()));
    (rec, ui)
}

// -- components used by the cases -------------------------------------------

#[derive(Default, PartialEq, Eq, Debug)]
struct Count {
    n: u32,
}

struct Counter {
    label: &'static str,
}

impl Component<()> for Counter {
    type State = Count;
    fn build(&self, s: &Count, _cx: &mut BuildCx<'_, ()>) -> Node<()> {
        text(format!("{} {}", self.label, s.n))
    }
}

struct Boom(bool);

impl Component<()> for Boom {
    type State = ();
    fn build(&self, _s: &(), _cx: &mut BuildCx<'_, ()>) -> Node<()> {
        assert!(!self.0, "boom");
        text("ok")
    }
}

// -- cases -------------------------------------------------------------------

#[test]
fn unkeyed_children_reorder_updates_in_place_positionally() {
    let (rec, mut ui) = ui();
    ui.reconcile(col().children([text("a"), text("b"), text("c")]));
    let root = ui.root().unwrap();
    let before = ui.children(root);

    rec.clear();
    ui.reconcile(col().children([text("c"), text("b"), text("a")]));

    assert_eq!(ui.children(root), before, "position is the implicit key");
    assert_eq!(rec.creates().len(), 0);
    assert_eq!(rec.disposes().len(), 0);
    // The root and all three children are updated in place.
    assert_eq!(rec.updates().len(), 4);
}

#[test]
fn keyed_children_reorder_moves_the_same_elements() {
    let (rec, mut ui) = ui();
    let k = |s: &'static str| text(s).key(s);
    ui.reconcile(col().children([k("a"), k("b"), k("c")]));
    let root = ui.root().unwrap();
    let before = ui.children(root);

    rec.clear();
    ui.reconcile(col().children([k("c"), k("a"), k("b")]));

    assert_eq!(ui.children(root), vec![before[2], before[0], before[1]]);
    assert_eq!(rec.disposes().len(), 0);
    assert_eq!(rec.creates().len(), 0);
}

#[test]
fn changing_a_key_remounts_and_drops_the_state() {
    let (rec, mut ui) = ui();
    ui.reconcile(col().children([Counter { label: "x" }.node().key("k1")]));
    let root = ui.root().unwrap();
    let before = ui.children(root)[0];

    ui.set_state::<Count>(before, |s| s.n = 7);
    ui.flush();
    assert_eq!(ui.state::<Count>(before).unwrap().n, 7);

    rec.clear();
    ui.reconcile(col().children([Counter { label: "x" }.node().key("k2")]));
    let after = ui.children(root)[0];

    assert_ne!(before, after);
    assert_eq!(
        ui.state::<Count>(after).unwrap().n,
        0,
        "a remount is a first mount"
    );
    // The component and the TextRun it built, on both sides.
    assert_eq!(rec.creates().len(), 2);
    assert_eq!(rec.disposes().len(), 2);
    assert!(!ui.is_live(before));
}

#[test]
fn changing_the_type_remounts() {
    let (rec, mut ui) = ui();
    ui.reconcile(col().children([text("a")]));
    let root = ui.root().unwrap();
    let before = ui.children(root)[0];

    rec.clear();
    ui.reconcile(col().children([row()]));

    assert_ne!(ui.children(root)[0], before);
    assert_eq!(rec.creates().len(), 1);
    assert_eq!(rec.disposes().len(), 1);
}

#[test]
fn removing_a_child_from_the_middle_disposes_exactly_one() {
    let (rec, mut ui) = ui();
    ui.reconcile(col().children([text("a"), text("b"), text("c")]));

    rec.clear();
    ui.reconcile(col().children([text("a"), text("c")]));

    assert_eq!(rec.disposes().len(), 1);
    assert_eq!(rec.creates().len(), 0);
}

#[test]
fn nested_remount_disposes_children_before_parents_once_each() {
    let (rec, mut ui) = ui();
    ui.reconcile(col().child(col().child(col().child(text("leaf")))));
    let a = ui.at(&[0]).unwrap();
    let b = ui.at(&[0, 0]).unwrap();
    let c = ui.at(&[0, 0, 0]).unwrap();

    rec.clear();
    ui.reconcile(col().child(text("x")));

    let ids: Vec<_> = rec.disposes().iter().map(|o| o.id()).collect();
    assert_eq!(ids, vec![c, b, a], "teardown runs children before parents");
}

#[test]
fn the_same_shared_instance_skips_the_subtree() {
    let (_rec, mut ui) = ui();
    let shared: Rc<Node<()>> = Rc::new(Counter { label: "i" }.node());

    ui.trace(true);
    ui.reconcile(col().child(shared_rc(shared.clone())));
    assert_eq!(ui.take_build_log().len(), 1, "mounting builds once");

    ui.reconcile(col().child(shared_rc(shared.clone())));
    assert!(
        ui.take_build_log().is_empty(),
        "same instance, nothing below is touched"
    );

    // A different instance of an identical description is not the same
    // instance: structural equality is deliberately not a skip rule.
    ui.reconcile(col().child(shared_rc(Rc::new(Counter { label: "i" }.node()))));
    assert_eq!(ui.take_build_log().len(), 1);
}

#[test]
fn a_panic_part_way_through_leaves_the_last_committed_tree() {
    let (rec, mut ui) = ui();
    ui.reconcile(col().children([text("a"), Boom(false).node()]));
    let shape = ui.shape();
    let live = ui.live_count();

    rec.clear();
    let hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(|_| {}));
    let outcome = std::panic::catch_unwind(AssertUnwindSafe(|| {
        ui.reconcile(col().children([text("b"), Boom(true).node()]))
    }));
    std::panic::set_hook(hook);

    assert!(outcome.is_err());
    assert_eq!(ui.live_count(), live, "nothing was left behind");
    assert_eq!(ui.shape(), shape, "the committed structure is intact");
    // Elements created before the panic were rolled back, so create and
    // dispose stay balanced.
    assert_eq!(rec.creates().len(), rec.disposes().len());

    // And the tree is still usable.
    ui.reconcile(col().children([text("c"), Boom(false).node()]));
    assert_eq!(ui.shape(), shape);
}
