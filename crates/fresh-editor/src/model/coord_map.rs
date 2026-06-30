//! Version-keyed edit-delta ring for mapping a byte coordinate captured at an
//! older buffer version forward to the current version.
//!
//! # Why this exists
//!
//! `lines_changed` hooks fire fire-and-forget: a plugin reads the byte ranges
//! they carry *later*, on its own thread, after more edits have already landed
//! on the editor thread. Any raw byte coordinate the plugin then echoes back
//! (to clear or locate a decoration) is therefore stale.
//!
//! Decorations themselves are marker-backed and auto-shift, so *anchored*
//! positions ride edits for free. What does **not** ride is a loose coordinate
//! the plugin uses to find the row it wants to act on. This ring lets the
//! editor remap such a coordinate from the version it was captured at to the
//! current version, reusing the exact same shift math the marker tree applies
//! to a right-gravity marker.
//!
//! # The one invariant
//!
//! The ring is fed at the **marker-adjustment chokepoint** — the same call
//! sites that invoke `MarkerList::adjust_for_insert/adjust_for_delete`
//! (`EditorState::apply_insert`, `apply_delete`, and
//! `replay_bulk_marker_adjustments`). Because every edit that shifts a marker
//! also appends to this ring, ring coverage equals marker coverage by
//! construction: the two can never disagree about which edits happened.
//!
//! # Barriers and eviction
//!
//! Some bulk applies restore an opaque buffer snapshot with no per-edit triples
//! (hot-exit recovery, logs loaded from disk). Those bump `version()` but can't
//! be replayed, so they push a *barrier*: any map whose source version precedes
//! the barrier returns `None`. Capacity eviction works the same way — once an
//! old delta is dropped, versions that would have needed it return `None`. In
//! both cases `None` means "force a full refresh", which is exactly what bulk
//! edits already trigger (they clear ephemeral decorations and re-fire
//! `lines_changed`).

use std::collections::VecDeque;

/// One recorded edit: at `version` (the buffer version *after* the edit landed)
/// the range `[pos, pos + removed)` was replaced by `inserted` bytes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct EditDelta {
    version: u64,
    pos: usize,
    removed: usize,
    inserted: usize,
}

/// Default ring capacity. Each entry is a handful of words; the staleness
/// window we need to cover is a single storm of edits between a render and the
/// plugin processing it (tens to low hundreds), so this is generous.
const DEFAULT_CAP: usize = 1024;

/// Forward coordinate mapper backed by a capped, version-keyed edit ring.
pub struct CoordMap {
    /// Edits in application order: chronological across versions, and within a
    /// single bulk edit in the descending-position order
    /// `replay_bulk_marker_adjustments` records them. Replaying a coordinate
    /// through them in stored order reproduces the marker-tree result exactly.
    deltas: VecDeque<EditDelta>,
    /// Lowest version that can still be mapped from. A request with
    /// `from_version < min_mappable` returns `None`. Raised both by capacity
    /// eviction (to the evicted delta's version) and by barriers.
    min_mappable: u64,
    cap: usize,
}

impl Default for CoordMap {
    fn default() -> Self {
        Self::new(DEFAULT_CAP)
    }
}

impl CoordMap {
    pub fn new(cap: usize) -> Self {
        Self {
            deltas: VecDeque::new(),
            min_mappable: 0,
            cap: cap.max(1),
        }
    }

    /// Record an insertion of `len` bytes at `pos`, producing buffer
    /// `version`.
    pub fn record_insert(&mut self, version: u64, pos: usize, len: usize) {
        self.push(EditDelta {
            version,
            pos,
            removed: 0,
            inserted: len,
        });
    }

    /// Record a deletion of `len` bytes at `pos`, producing buffer `version`.
    pub fn record_delete(&mut self, version: u64, pos: usize, len: usize) {
        self.push(EditDelta {
            version,
            pos,
            removed: len,
            inserted: 0,
        });
    }

    /// Record one bulk-edit tuple. Several of these may share a `version` (a
    /// whole bulk edit is atomic with respect to version); record them in the
    /// same descending-position order `replay_bulk_marker_adjustments` uses.
    pub fn record_replace(&mut self, version: u64, pos: usize, removed: usize, inserted: usize) {
        self.push(EditDelta {
            version,
            pos,
            removed,
            inserted,
        });
    }

    /// Record an opaque content change at `version` that cannot be replayed
    /// (snapshot restore with no edit triples). Any later map from a version
    /// before `version` returns `None`.
    pub fn record_barrier(&mut self, version: u64) {
        // A barrier at version V means the edit producing V is unmappable, so
        // the floor becomes V: from_version >= V is still fine (it doesn't
        // include the V edit), from_version < V is not.
        self.min_mappable = self.min_mappable.max(version);
    }

    fn push(&mut self, delta: EditDelta) {
        if delta.removed == 0 && delta.inserted == 0 {
            return;
        }
        self.deltas.push_back(delta);
        while self.deltas.len() > self.cap {
            if let Some(evicted) = self.deltas.pop_front() {
                // Dropping the edit at `evicted.version` means any source
                // version below it can no longer be fully replayed.
                self.min_mappable = self.min_mappable.max(evicted.version);
            }
        }
    }

    /// Map `coord`, valid as of `from_version`, forward to the current version.
    ///
    /// Returns `None` when `from_version` precedes the mappable floor (evicted
    /// or barriered) — the caller should force a full refresh rather than act
    /// on a guessed position.
    pub fn map(&self, coord: usize, from_version: u64) -> Option<usize> {
        if from_version < self.min_mappable {
            return None;
        }
        let mut c = coord;
        for d in &self.deltas {
            if d.version <= from_version {
                continue;
            }
            c = apply_delta(c, d.pos, d.removed, d.inserted);
        }
        Some(c)
    }

    /// Number of retained deltas (for tests/diagnostics).
    #[cfg(test)]
    pub fn len(&self) -> usize {
        self.deltas.len()
    }
}

/// Shift a single coordinate through one edit that replaced `[pos, pos+removed)`
/// with `inserted` bytes. Right-gravity at the boundary: a coordinate sitting
/// exactly at an insertion point moves forward with the inserted text, matching
/// how the marker tree shifts a right-gravity marker. A coordinate strictly
/// inside a deleted range collapses to the deletion start.
///
/// A same-position replacement (both lengths non-zero) is reduced to its **net**
/// delta first, exactly as `replay_bulk_marker_adjustments` collapses a merged
/// delete+insert tuple before calling `marker_list.adjust_*`. Without this a
/// coordinate strictly inside the replaced span would be mapped to a different
/// byte than its own auto-shifting marker, breaking the "exact same shift math"
/// guarantee for stale coordinates landing inside a bulk replacement.
fn apply_delta(coord: usize, pos: usize, removed: usize, inserted: usize) -> usize {
    let (removed, inserted) = if removed > 0 && inserted > 0 {
        if inserted >= removed {
            (0, inserted - removed)
        } else {
            (removed - inserted, 0)
        }
    } else {
        (removed, inserted)
    };
    let mut c = coord;
    if removed > 0 {
        let end = pos + removed;
        if c >= end {
            c -= removed;
        } else if c > pos {
            c = pos;
        }
    }
    if inserted > 0 && c >= pos {
        c += inserted;
    }
    c
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identity_when_no_edits() {
        let m = CoordMap::default();
        assert_eq!(m.map(42, 0), Some(42));
    }

    #[test]
    fn identity_when_from_version_is_current() {
        let mut m = CoordMap::default();
        m.record_insert(1, 0, 5); // version 1
        m.record_insert(2, 0, 5); // version 2
                                  // Captured at the latest version: nothing newer to replay.
        assert_eq!(m.map(10, 2), Some(10));
    }

    #[test]
    fn insert_before_shifts_right() {
        let mut m = CoordMap::default();
        m.record_insert(1, 5, 3); // insert 3 bytes at 5
        assert_eq!(m.map(10, 0), Some(13));
        assert_eq!(
            m.map(5, 0),
            Some(8),
            "right-gravity: coord at insert point moves"
        );
        assert_eq!(m.map(4, 0), Some(4), "before the insert: unchanged");
    }

    #[test]
    fn delete_before_shifts_left() {
        let mut m = CoordMap::default();
        m.record_delete(1, 2, 3); // remove [2,5)
        assert_eq!(m.map(10, 0), Some(7));
        assert_eq!(m.map(2, 0), Some(2), "at delete start: unchanged");
        assert_eq!(m.map(5, 0), Some(2), "at delete end: shifts to start");
    }

    #[test]
    fn coord_inside_deleted_range_collapses_to_start() {
        let mut m = CoordMap::default();
        m.record_delete(1, 4, 6); // remove [4,10)
        assert_eq!(m.map(7, 0), Some(4));
    }

    #[test]
    fn only_replays_newer_than_from_version() {
        let mut m = CoordMap::default();
        m.record_insert(1, 0, 100); // version 1 — already reflected in a v1 coord
        m.record_insert(2, 0, 10); // version 2 — the only edit after v1
        assert_eq!(m.map(50, 1), Some(60));
    }

    #[test]
    fn bulk_descending_tuples_match_marker_result() {
        // A bulk edit at version 1 with two inserts, recorded descending by
        // position as replay_bulk_marker_adjustments iterates.
        let mut m = CoordMap::default();
        m.record_replace(1, 10, 0, 3);
        m.record_replace(1, 5, 0, 3);
        assert_eq!(m.map(7, 0), Some(10), "shifted only by the lower insert");
        assert_eq!(m.map(12, 0), Some(18), "shifted by both inserts");
    }

    #[test]
    fn undo_as_forward_delta_nets_out() {
        // Edit then its inverse, both appended forward. A coord outside the
        // edited region maps back to itself; no index rewind needed.
        let mut m = CoordMap::default();
        m.record_insert(1, 5, 4); // insert 4 at 5  (version 1)
        m.record_delete(2, 5, 4); // undo: remove [5,9) (version 2)
        assert_eq!(m.map(20, 0), Some(20));
    }

    #[test]
    fn eviction_raises_floor_and_returns_none() {
        let mut m = CoordMap::new(2);
        m.record_insert(1, 0, 1);
        m.record_insert(2, 0, 1);
        m.record_insert(3, 0, 1); // evicts version-1 delta
        assert_eq!(m.len(), 2);
        assert_eq!(
            m.map(0, 0),
            None,
            "from before the evicted edit: unmappable"
        );
        assert_eq!(m.map(0, 1), Some(2), "from the evicted version's floor: ok");
    }

    #[test]
    fn barrier_blocks_older_versions() {
        let mut m = CoordMap::default();
        m.record_insert(1, 0, 5);
        m.record_barrier(2); // opaque snapshot restore at version 2
        m.record_insert(3, 0, 5);
        assert_eq!(m.map(10, 1), None, "straddles the barrier: refresh");
        assert_eq!(m.map(10, 2), Some(15), "after the barrier: mappable again");
    }

    #[test]
    fn replacement_shifts_by_net_delta() {
        // A bulk replacement tuple replaces [10, 16) (6 bytes) with 2 bytes
        // (net -4). Coordinates are shifted by the NET delta, matching the
        // marker tree's adjust_for_delete(pos, net), including one strictly
        // inside the replaced span.
        let mut m = CoordMap::default();
        m.record_replace(1, 10, 6, 2);
        assert_eq!(m.map(20, 0), Some(16), "past the span: 20 - (6-2)");
        assert_eq!(m.map(9, 0), Some(9), "before the span: unchanged");
        // Inside [10,16): collapses through the net deletion, not to pos+inserted.
        assert_eq!(
            m.map(13, 0),
            Some(10),
            "inside the span: clamps to net-delete start"
        );

        // Net-positive replacement: replace 2 bytes with 6 (net +4).
        let mut g = CoordMap::default();
        g.record_replace(1, 10, 2, 6);
        assert_eq!(g.map(20, 0), Some(24), "past the span: 20 + (6-2)");
        assert_eq!(
            g.map(11, 0),
            Some(15),
            "inside the span: net insert shifts by +4"
        );
        assert_eq!(g.map(9, 0), Some(9), "before the span: unchanged");
    }
}
