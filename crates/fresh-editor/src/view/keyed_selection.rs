//! A list's selection, held by the selected item's key.
//!
//! **The key is the selection; the index is where it was.** A model that
//! holds its selection as an index keeps pointing at the same *position*
//! when the list changes under it — a suggestion streamed in above the
//! selected one, a file created while the browser is open, a Settings
//! category expanded above the cursor — and the selection silently lands on
//! a different item. Held by key, it stays on its item wherever that item
//! goes, and an item that is gone leaves nothing selected rather than
//! whatever now sits where it was.
//!
//! The index the key was last found at is kept as a hint, so reading the
//! selection is a comparison while the list is still: a walk happens only
//! when the item has moved.

use std::cell::Cell;

/// A selection over a list whose items are keyed by `K`. See the module
/// docs.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KeyedSelection<K = String> {
    key: Option<K>,
    hint: Cell<usize>,
}

impl<K> Default for KeyedSelection<K> {
    fn default() -> Self {
        Self {
            key: None,
            hint: Cell::new(0),
        }
    }
}

impl<K: PartialEq> KeyedSelection<K> {
    /// Nothing selected.
    pub fn none() -> Self {
        Self::default()
    }

    /// The item at `index`, whose key is `key`.
    pub fn at(index: usize, key: impl Into<K>) -> Self {
        Self {
            key: Some(key.into()),
            hint: Cell::new(index),
        }
    }

    /// The key of the selected item, if any.
    pub fn key(&self) -> Option<&K> {
        self.key.as_ref()
    }

    /// Where the selected item is in a list of `len` items keyed by
    /// `key_of`: `None` when nothing is selected or the item is gone.
    pub fn index<'a, Q>(&self, len: usize, key_of: impl Fn(usize) -> &'a Q) -> Option<usize>
    where
        K: std::borrow::Borrow<Q>,
        Q: PartialEq + ?Sized + 'a,
    {
        self.find(len, |i, key| key_of(i) == key.borrow())
    }

    /// [`Self::index`] for a list whose keys are computed rather than
    /// stored: `is(i, key)` says whether item `i` has the selected key.
    pub fn find(&self, len: usize, is: impl Fn(usize, &K) -> bool) -> Option<usize> {
        let key = self.key.as_ref()?;
        let hint = self.hint.get();
        if hint < len && is(hint, key) {
            return Some(hint);
        }
        let found = (0..len).find(|&i| is(i, key))?;
        self.hint.set(found);
        Some(found)
    }

    /// Where the selected item was last seen. A list that must always have
    /// a selection re-selects here, clamped, when its item is gone.
    pub fn last_index(&self) -> usize {
        self.hint.get()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_selection_follows_its_item_and_lets_go_when_it_is_gone() {
        let before = ["a", "b", "c"];
        let sel: KeyedSelection = KeyedSelection::at(2, "c");
        assert_eq!(sel.index(before.len(), |i| before[i]), Some(2));

        // Something arrives above it: the selection moves down with "c".
        let after = ["new", "a", "b", "c"];
        assert_eq!(sel.index(after.len(), |i| after[i]), Some(3));

        // "c" leaves: nothing is selected, not whatever took its place.
        let gone = ["new", "a", "b", "d"];
        assert_eq!(sel.index(gone.len(), |i| gone[i]), None);
        assert_eq!(
            KeyedSelection::<String>::none().index(3, |i| before[i]),
            None
        );
    }
}
