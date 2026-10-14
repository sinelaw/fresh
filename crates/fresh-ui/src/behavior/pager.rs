//! `Pager`: a list's answer to "a page from here".
//!
//! **A page is layout's number.** How far PageDown moves a selection is the
//! height of the window the list was given, in items — which is known when the
//! list is laid out and nowhere earlier. A host that sized a page from a
//! panel's rectangle, or from a height it wrote down while describing the
//! frame, was doing layout's work a second time and could disagree with it
//! (a bordered box, a header row, cards several rows tall).
//!
//! So the list records the window its layout placed, and the owner asks the
//! pager where a page from the selection lands. The owner still decides what
//! a key means — its keymap is the one place a key becomes an action — and
//! still owns the selection; the pager owns only the arithmetic between the
//! selection and the window.

use std::cell::{Cell, RefCell};
use std::rc::Rc;

/// The window a list was last laid out with, and the page arithmetic over it.
/// Constructed by the owner, handed to [`List::pager`](crate::List::pager).
#[derive(Debug, Default)]
pub struct Pager {
    /// Items in the window the list was last laid out with; `None` until it
    /// has been laid out.
    rows: Cell<Option<usize>>,
}

/// A handle equals only itself: two pagers are the same pager or different
/// ones, whatever windows they last saw — so a description that carries one
/// compares unchanged while it hands down the same handle.
impl PartialEq for Pager {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Pager {
    pub fn new() -> Rc<Pager> {
        Rc::new(Pager::default())
    }

    /// Called by the list, at layout, with the number of items its window
    /// holds.
    pub(crate) fn record(&self, rows: usize) {
        self.rows.set(Some(rows.max(1)));
    }

    /// The list is gone: there is no window, so no page.
    pub(crate) fn forget(&self) {
        self.rows.set(None);
    }

    /// How many items the window held when the list was last laid out;
    /// `None` when it has not been, or is gone. For an owner that keeps a
    /// window of its own in step with the one drawn.
    pub fn window(&self) -> Option<usize> {
        self.rows.get()
    }

    /// The item `pages` pages from `from` (negative: up) in a list of `len`
    /// items, clamped to the list: a page past the end lands on the last
    /// item, and one before the start on the first.
    ///
    /// `None` when the list has not been laid out — it is not on screen, so
    /// there is no page to speak of — or has no items.
    pub fn target(&self, from: usize, pages: i32, len: usize) -> Option<usize> {
        let rows = self.rows.get()?;
        let last = len.checked_sub(1)?;
        let step = rows.saturating_mul(pages.unsigned_abs() as usize);
        Some(match pages < 0 {
            true => from.min(last).saturating_sub(step),
            false => from.saturating_add(step).min(last),
        })
    }
}

/// A list's hold on the pager it records into — its own, or its owner's —
/// so that a list leaving the tree takes its window with it. A pager that
/// went on answering from the last window it saw would page a list nobody
/// can see (the narrow Settings layout draws its categories as a strip, not
/// this list) by a height that is no longer anyone's.
#[derive(Debug, Default)]
pub(crate) struct PagerSlot {
    own: Rc<Pager>,
    current: RefCell<Option<Rc<Pager>>>,
}

impl PagerSlot {
    /// The pager this build records into: `owner`'s when it passed one,
    /// else the list's own. A pager the list stops recording into forgets
    /// the window it had.
    pub(crate) fn bind(&self, owner: Option<Rc<Pager>>) -> Rc<Pager> {
        let next = owner.unwrap_or_else(|| self.own.clone());
        let prev = self.current.replace(Some(next.clone()));
        if let Some(prev) = prev.filter(|p| !Rc::ptr_eq(p, &next)) {
            prev.forget();
        }
        next
    }
}

impl super::Behavior for PagerSlot {
    fn teardown(&self) {
        if let Some(p) = self.current.take() {
            p.forget();
        }
    }

    fn behavior_name(&self) -> &'static str {
        "PagerSlot"
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
