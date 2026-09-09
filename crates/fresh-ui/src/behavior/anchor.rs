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
    /// Move the window so that the descendant with this key is at its top.
    TopKey(crate::key::Key),
    /// Move the window so that one row *inside* the keyed descendant is in
    /// it: `(key, rows from that band's top)`.
    RevealKeyAt(crate::key::Key, u32),
    RevealByte(crate::key::Key, usize),
    /// Move the window by this many rows (negative: up), clamped.
    ScrollBy(i32),
    /// Move the window by this many of its own heights (negative: up),
    /// clamped — a page key, in whatever unit the window counts.
    ScrollByPages(i32),
    /// Move the window to the end of its content.
    ScrollToEnd,
}

#[derive(Debug, Default)]
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

    /// Move the target's window by `rows` (negative: up), clamped to its
    /// content. The arrow keys of a page that scrolls as a whole.
    pub fn scroll_by(&self, rows: i32) {
        self.queue.borrow_mut().push(Command::ScrollBy(rows));
    }

    /// Move the target's window by `pages` of its own height (negative:
    /// up), clamped. The page keys of a page that scrolls as a whole.
    pub fn scroll_by_pages(&self, pages: i32) {
        self.queue.borrow_mut().push(Command::ScrollByPages(pages));
    }

    /// Move the target's window to the end of its content.
    pub fn scroll_to_end(&self) {
        self.queue.borrow_mut().push(Command::ScrollToEnd);
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
        self.queue.borrow_mut().push(Command::RevealKey(key.into()));
    }

    /// Move the target's window so that the descendant carrying `key` is at
    /// the *top* of it, however far that is.
    ///
    /// The difference from [`Anchor::reveal_key`] is what "show me this" means
    /// when the caller is jumping rather than following. Revealing moves as
    /// little as possible, which leaves the target at the bottom edge — right
    /// for a cursor that walked there, wrong for "take me to this section",
    /// where everything under the heading is what the reader wants and a
    /// tight viewport clips it away entirely.
    pub fn top_key(&self, key: impl Into<crate::key::Key>) {
        self.queue.borrow_mut().push(Command::TopKey(key.into()));
    }

    /// Move the target's window so that the row `row` rows into the keyed
    /// descendant's band is inside it.
    ///
    /// **For an offset the caller owns inside content the framework placed.**
    /// A text editor knows its caret is on its own line seventeen; it does
    /// not know, and should not compute, which row of the column that lands
    /// on. Naming the band and the offset within it splits the question at
    /// the one place each side has an answer for.
    pub fn reveal_key_at(&self, key: impl Into<crate::key::Key>, row: u32) {
        self.queue
            .borrow_mut()
            .push(Command::RevealKeyAt(key.into(), row));
    }

    /// Move the target's window so that the row holding `byte` of the keyed
    /// **wrapped text run** is inside it.
    ///
    /// **The same split as [`Anchor::reveal_key_at`], one step further** (L5).
    /// There the caller owns an offset *in rows* inside content the framework
    /// placed; here it does not have rows at all. A document view knows where
    /// its caret is as a byte of the string it handed over, and which row that
    /// byte landed on is the wrap's answer — it changes with the width, and
    /// the caller would have to re-shape the text to find it, which is the
    /// second layout this design exists to remove. So the caller names the
    /// byte and the framework, which shaped the rows, does the rest.
    ///
    /// The keyed descendant must be a text run; naming anything else moves
    /// nothing, as an unknown key does.
    pub fn reveal_byte(&self, key: impl Into<crate::key::Key>, byte: usize) {
        self.queue
            .borrow_mut()
            .push(Command::RevealByte(key.into(), byte));
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
