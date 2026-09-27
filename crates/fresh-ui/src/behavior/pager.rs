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

use std::cell::Cell;
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
