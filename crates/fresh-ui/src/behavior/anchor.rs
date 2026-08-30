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

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Command {
    ScrollTo(Point),
    /// Move the window so that this index is inside it.
    Reveal(u32),
    /// Move the window so that the descendant with this key is inside it.
    RevealKey(crate::key::Key),
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

    /// Move the target's window so that the descendant carrying `key` is
    /// inside it, by the shortest distance. Nothing happens if it already is,
    /// or if nothing under the target carries that key.
    ///
    /// **The companion [`Anchor::reveal`] cannot answer for content whose
    /// items differ in height**: it takes an index and treats it as a content
    /// row, which is the same thing only when every row is one cell. A column
    /// of cards — a settings page, a diff, a feed — knows *which* card to
    /// show and has no idea what row it landed on; the framework, which laid
    /// the column out, does. So the caller names the card and the window is
    /// moved by what was measured, rather than by a second measurement the
    /// caller would have to keep in step.
    ///
    /// A band taller than the window is shown from its top: the alternative
    /// (its bottom edge flush with the window's) scrolls past the very thing
    /// the caller asked to see.
    pub fn reveal_key(&self, key: impl Into<crate::key::Key>) {
        self.queue
            .borrow_mut()
            .push(Command::RevealKey(key.into()));
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
