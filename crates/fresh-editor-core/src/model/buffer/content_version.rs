//! The buffer's version counter, and how far into the buffer each recent
//! version change reached.
//!
//! `version()` says *that* the content changed; a cache derived from a prefix
//! of the buffer also needs to know *where*, or every keystroke forces it to
//! rebuild from byte 0. Every content mutation already funnels through one
//! `bump` here (that is what keeps `version()` honest), so recording the lowest
//! offset each change touched costs nothing at the call sites and cannot miss
//! one: a path that changes content without bumping would already be a stale
//! `version()` bug.
//!
//! A stamp also carries a per-buffer instance number, because a buffer is
//! sometimes *replaced* wholesale (reload, revert) by a fresh one whose
//! version restarts at 0 — the same number as the stamp of the buffer it
//! replaced, with entirely different content.

use std::collections::VecDeque;
use std::sync::atomic::{AtomicU64, Ordering};

/// Source of [`ContentStamp::instance`]: unique per buffer for the process.
static NEXT_INSTANCE: AtomicU64 = AtomicU64::new(1);

/// How many recent changes are remembered. A reader that falls further
/// behind than this gets the conservative answer (nothing is known to be
/// unchanged), which is always correct, just slower for it.
const RECORDED_CHANGES: usize = 64;

/// Where a buffer's content stood at one moment. Hand it back to
/// [`TextBuffer::unchanged_prefix_since`](super::TextBuffer::unchanged_prefix_since)
/// to learn how much of the buffer is still byte-for-byte what it was then.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ContentStamp {
    instance: u64,
    version: u64,
}

#[derive(Debug)]
pub(super) struct ContentVersion {
    instance: u64,
    version: u64,
    /// `(version the change produced, lowest offset it may have touched)`,
    /// oldest first.
    changes: VecDeque<(u64, usize)>,
    /// Lowest version whose later changes are all still in `changes`.
    horizon: u64,
}

impl ContentVersion {
    pub(super) fn new() -> Self {
        Self {
            instance: NEXT_INSTANCE.fetch_add(1, Ordering::Relaxed),
            version: 0,
            changes: VecDeque::new(),
            horizon: 0,
        }
    }

    pub(super) fn get(&self) -> u64 {
        self.version
    }

    /// Advance the version for a change that left every byte before `floor`
    /// as it was. `usize::MAX` for a change that touched no bytes at all (a
    /// format change), `0` when the change can't say.
    pub(super) fn bump(&mut self, floor: usize) {
        self.version = self.version.wrapping_add(1);
        self.changes.push_back((self.version, floor));
        while self.changes.len() > RECORDED_CHANGES {
            if let Some((version, _)) = self.changes.pop_front() {
                self.horizon = version;
            }
        }
    }

    pub(super) fn stamp(&self) -> ContentStamp {
        ContentStamp {
            instance: self.instance,
            version: self.version,
        }
    }

    /// Every byte before the returned offset is what it was when `stamp`
    /// was taken. `usize::MAX` when nothing changed; `0` when the stamp is
    /// from another buffer or too old to answer.
    pub(super) fn unchanged_prefix_since(&self, stamp: ContentStamp) -> usize {
        if stamp.instance != self.instance || stamp.version > self.version {
            return 0;
        }
        if stamp.version < self.horizon {
            return 0;
        }
        self.changes
            .iter()
            .filter(|(version, _)| *version > stamp.version)
            .map(|(_, floor)| *floor)
            .min()
            .unwrap_or(usize::MAX)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_prefix_ends_at_the_lowest_change_since_the_stamp() {
        let mut v = ContentVersion::new();
        v.bump(500);
        let stamp = v.stamp();
        assert_eq!(v.unchanged_prefix_since(stamp), usize::MAX);
        v.bump(300);
        v.bump(900);
        assert_eq!(v.unchanged_prefix_since(stamp), 300);
        // A change from before the stamp does not count.
        let later = v.stamp();
        v.bump(700);
        assert_eq!(v.unchanged_prefix_since(later), 700);
    }

    #[test]
    fn a_stamp_older_than_the_record_answers_nothing_is_known() {
        let mut v = ContentVersion::new();
        let stamp = v.stamp();
        for _ in 0..RECORDED_CHANGES {
            v.bump(1000);
        }
        assert_eq!(v.unchanged_prefix_since(stamp), 1000);
        v.bump(1000);
        assert_eq!(v.unchanged_prefix_since(stamp), 0);
    }

    #[test]
    fn a_stamp_from_a_replaced_buffer_answers_nothing_is_known() {
        let old = ContentVersion::new();
        let new = ContentVersion::new();
        // Both at version 0, which is exactly the collision a reload makes.
        assert_eq!(new.unchanged_prefix_since(old.stamp()), 0);
    }
}
