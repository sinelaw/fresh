//! Behavior registration and teardown fan-out (plan phase L2a).

use std::cell::RefCell;
use std::rc::Rc;

use fresh_ui::{col, text, Behavior, BuildCx, Component, ComponentExt, InitCx, Node, Ui};

/// Appends its name to a shared log when it is torn down. The log is the whole
/// point: order is the thing being asserted.
struct Probe {
    name: &'static str,
    log: Rc<RefCell<Vec<&'static str>>>,
}

impl Behavior for Probe {
    fn teardown(&self) {
        self.log.borrow_mut().push(self.name);
    }

    fn behavior_name(&self) -> &'static str {
        self.name
    }
}

type Log = Rc<RefCell<Vec<&'static str>>>;

#[derive(Default)]
struct Held {
    _first: Option<Rc<Probe>>,
    _second: Option<Rc<Probe>>,
}

/// Registers two behaviors, and renders a child that registers a third.
struct Outer(Log);

impl Component<()> for Outer {
    type State = Held;

    fn init(&self, cx: &mut InitCx<'_, ()>) -> Held {
        Held {
            _first: Some(cx.register(Probe {
                name: "outer.a",
                log: self.0.clone(),
            })),
            _second: Some(cx.register(Probe {
                name: "outer.b",
                log: self.0.clone(),
            })),
        }
    }

    fn build(&self, _s: &Held, _cx: &mut BuildCx<'_, ()>) -> Node<()> {
        col().child(Inner(self.0.clone()).node())
    }
}

struct Inner(Log);

impl Component<()> for Inner {
    type State = Held;

    fn init(&self, cx: &mut InitCx<'_, ()>) -> Held {
        Held {
            _first: Some(cx.register(Probe {
                name: "inner",
                log: self.0.clone(),
            })),
            _second: None,
        }
    }

    fn build(&self, _s: &Held, _cx: &mut BuildCx<'_, ()>) -> Node<()> {
        text("leaf")
    }
}

#[test]
fn behaviors_tear_down_children_first_and_in_reverse_registration_order() {
    let log: Log = Rc::default();
    let mut ui: Ui<()> = Ui::new();
    ui.reconcile(col().child(Outer(log.clone()).node()));
    assert!(
        log.borrow().is_empty(),
        "nothing is torn down while mounted"
    );

    // Replace the subtree: everything below goes away.
    ui.reconcile(col().child(text("gone")));

    assert_eq!(
        *log.borrow(),
        vec!["inner", "outer.b", "outer.a"],
        "children before parents; within one state object, reverse registration order"
    );
}

#[test]
fn a_behavior_is_torn_down_exactly_once() {
    let log: Log = Rc::default();
    let mut ui: Ui<()> = Ui::new();
    ui.reconcile(col().child(Outer(log.clone()).node()));
    ui.reconcile(col().child(text("gone")));
    ui.reconcile(col());
    ui.flush();

    assert_eq!(log.borrow().iter().filter(|n| **n == "inner").count(), 1);
    assert_eq!(log.borrow().len(), 3);
}

#[test]
fn a_rebuild_does_not_tear_anything_down() {
    let log: Log = Rc::default();
    let mut ui: Ui<()> = Ui::new();
    ui.reconcile(col().child(Outer(log.clone()).node()));
    let outer = ui.at(&[0]).unwrap();

    for _ in 0..3 {
        ui.reconcile(col().child(Outer(log.clone()).node()));
    }
    ui.mark(outer);
    ui.flush();

    assert!(
        log.borrow().is_empty(),
        "init runs once per mount, not per build"
    );
    assert!(ui.builds(outer) > 1);
    assert_eq!(ui.behaviors(outer), vec!["outer.a", "outer.b"]);
}
