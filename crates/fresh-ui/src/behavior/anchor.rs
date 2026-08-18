//! `Anchor`: addressing a mounted surface by handle.
//!
//! The owner constructs one, passes it down in a description, and the child
//! binds it. Commands are method calls forwarded to the bound element, applied
//! by the framework between frames — never synchronously, so the frozen-tree
//! invariant holds.
//!
//! Scope is restricted the same way `Controller`'s is: a command may touch only
//! framework-owned state of its target — a scroll offset, focus — never
//! controlled application state, which would violate the data-flow rule.
//!
//! This is not an id side table: binding is explicit, the state stays on the
//! render object, and the anchor maps handle to element rather than identity to
//! state.

use std::cell::{Cell, RefCell};
use std::rc::Rc;

use crate::element::ElementId;
use crate::render::geom::Point;

use super::Behavior;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Command {
    ScrollTo(Point),
    /// Move the window so that this index is inside it.
    Reveal(u32),
}

#[derive(Default)]
pub struct Anchor {
    bound: Cell<Option<ElementId>>,
    queue: RefCell<Vec<Command>>,
}

impl Anchor {
    pub fn new() -> Rc<Anchor> {
        Rc::new(Anchor::default())
    }

    /// The element this anchor addresses, once it has mounted.
    pub fn target(&self) -> Option<ElementId> {
        self.bound.get()
    }

    pub(crate) fn bind(&self, id: ElementId) {
        self.bound.set(Some(id));
    }

    pub(crate) fn take(&self) -> Vec<Command> {
        std::mem::take(&mut self.queue.borrow_mut())
    }

    /// Move the target's window to an absolute offset.
    pub fn scroll_to(&self, p: Point) {
        self.queue.borrow_mut().push(Command::ScrollTo(p));
    }

    /// Move the target's window so that `index` is inside it, by the shortest
    /// distance. Nothing happens if it already is.
    pub fn reveal(&self, index: u32) {
        self.queue.borrow_mut().push(Command::Reveal(index));
    }
}

impl Behavior for Anchor {
    fn behavior_name(&self) -> &'static str {
        "Anchor"
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
