//! Scheduler exit criteria (plan phase L2).

use std::cell::RefCell;
use std::rc::Rc;

use fresh_ui::{col, text, BuildCx, Component, ComponentExt, Handler, Node, Ui, Updater};

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
    ui.frame(Parent.node());
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
    ui.frame(Toggle.node());
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
    ui.frame(Bad.node());
}

#[test]
fn set_state_from_a_handler_coalesces_into_the_next_flush() {
    let wires = Wires::default();
    let mut ui: Ui<()> = Ui::new();
    ui.frame(Wired(wires.clone()).node());
    let id = ui.root().unwrap();

    let updater = wires.updater.borrow().clone().unwrap();
    let handler = wires.handler.borrow().clone().unwrap();

    // Three separate updates from outside a build.
    updater.set(|s| *s += 1);
    updater.set(|s| *s += 10);
    handler(&Default::default());

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
    ui.frame(Toggle.node());
    let t = ui.root().unwrap();
    ui.set_state::<bool>(t, |s| *s = true);
    ui.flush();
    let c = ui.at(&[0, 0]).unwrap();

    ui.set_state::<u32>(c, |s| *s += 1);
    ui.frame(col()); // a whole new root type: everything below goes away
    ui.flush(); // must not observe the stale mark
    assert!(!ui.is_live(c));
}
