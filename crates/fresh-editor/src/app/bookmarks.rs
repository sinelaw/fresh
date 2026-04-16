//! Self-contained bookmark storage.
//!
//! A bookmark remembers a `(buffer_id, byte_position)` under a single-char
//! register. The state is deliberately minimal: lookup, insert, remove,
//! iterate. Cross-cutting work — checking that a bookmark's target buffer
//! still exists, jumping the cursor, rendering a list — lives on `Editor`.

use std::collections::HashMap;

use crate::model::event::BufferId;

/// A bookmark: a position within a specific buffer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Bookmark {
    pub buffer_id: BufferId,
    pub position: usize,
}

/// Owner of the register -> bookmark map.
#[derive(Debug, Default)]
pub(crate) struct BookmarkState {
    bookmarks: HashMap<char, Bookmark>,
}

impl BookmarkState {
    /// Store (or replace) a bookmark under `key`.
    pub(crate) fn set(&mut self, key: char, bookmark: Bookmark) {
        self.bookmarks.insert(key, bookmark);
    }

    /// Retrieve the bookmark under `key`, if any.
    pub(crate) fn get(&self, key: char) -> Option<Bookmark> {
        self.bookmarks.get(&key).copied()
    }

    /// Remove the bookmark under `key`. Returns `true` if one existed.
    pub(crate) fn remove(&mut self, key: char) -> bool {
        self.bookmarks.remove(&key).is_some()
    }

    /// True when no bookmarks are set.
    pub(crate) fn is_empty(&self) -> bool {
        self.bookmarks.is_empty()
    }

    /// Iterate over all (key, bookmark) pairs — used by serialization and
    /// the `list_bookmarks` orchestrator on `Editor`.
    pub(crate) fn iter(&self) -> impl Iterator<Item = (char, Bookmark)> + '_ {
        self.bookmarks.iter().map(|(k, v)| (*k, *v))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn bm(buffer: usize, pos: usize) -> Bookmark {
        Bookmark {
            buffer_id: BufferId(buffer),
            position: pos,
        }
    }

    #[test]
    fn default_is_empty() {
        let s = BookmarkState::default();
        assert!(s.is_empty());
        assert_eq!(s.get('a'), None);
    }

    #[test]
    fn set_then_get_roundtrips() {
        let mut s = BookmarkState::default();
        s.set('1', bm(7, 42));
        assert!(!s.is_empty());
        assert_eq!(s.get('1'), Some(bm(7, 42)));
    }

    #[test]
    fn set_replaces_existing_bookmark_under_same_key() {
        let mut s = BookmarkState::default();
        s.set('1', bm(7, 42));
        s.set('1', bm(3, 99));
        assert_eq!(s.get('1'), Some(bm(3, 99)));
    }

    #[test]
    fn remove_returns_true_when_bookmark_existed() {
        let mut s = BookmarkState::default();
        s.set('1', bm(7, 42));
        assert!(s.remove('1'));
        assert_eq!(s.get('1'), None);
    }

    #[test]
    fn remove_returns_false_when_no_bookmark() {
        let mut s = BookmarkState::default();
        assert!(!s.remove('1'));
    }

    #[test]
    fn iter_yields_all_entries() {
        let mut s = BookmarkState::default();
        s.set('a', bm(1, 10));
        s.set('b', bm(2, 20));
        let mut seen: Vec<(char, Bookmark)> = s.iter().collect();
        seen.sort_by_key(|(k, _)| *k);
        assert_eq!(seen, vec![('a', bm(1, 10)), ('b', bm(2, 20))]);
    }
}
