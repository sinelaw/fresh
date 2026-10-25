/// Scroll synchronization for side-by-side diff views
///
/// This module implements marker-based sync anchors for synchronized scrolling
/// between two panes showing different versions of a file (e.g., old vs new in a diff).
///
/// Key design principles:
/// - Single source of truth: `scroll_line` is the authoritative position
/// - Sync anchors mark corresponding lines between buffers (e.g., hunk boundaries)
/// - Synchronization happens at render time, not via async commands
/// - No feedback loops because only one position is tracked
use crate::model::event::SplitId;
use serde::{Deserialize, Serialize};

/// A sync anchor linking corresponding line positions in two buffers
///
/// Anchors are placed at diff hunk boundaries where both buffers
/// have a known correspondence (e.g., start of context, end of hunk).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SyncAnchor {
    /// Line number in the left (primary) buffer
    pub left_line: usize,
    /// Line number in the right (secondary) buffer
    pub right_line: usize,
}

/// A unique identifier for a scroll sync group
pub type ScrollSyncGroupId = u32;

/// A group of two splits that scroll together with anchor-based synchronization
///
/// Unlike the simple sync_group which applies the same scroll delta to all splits,
/// this uses sync anchors to correctly handle buffers with different line counts.
#[derive(Debug, Clone)]
pub struct ScrollSyncGroup {
    /// Unique ID for this sync group
    pub id: ScrollSyncGroupId,
    /// The left (primary) split - scroll position is tracked in this split's line space
    pub left_split: SplitId,
    /// The right (secondary) split - position is derived from anchors
    pub right_split: SplitId,
    /// Single source of truth: scroll position in left buffer's line space
    /// Both splits derive their viewport position from this value
    pub scroll_line: usize,
    /// Sync anchors ordered by left_line
    /// These mark corresponding positions between the two buffers
    pub anchors: Vec<SyncAnchor>,
    /// Where the last row-space sync left the panes, for a group whose two
    /// panes show the same buffer (markdown's source beside its preview).
    pub row_sync: Option<RowSyncState>,
}

impl ScrollSyncGroup {
    /// Create a new scroll sync group
    pub fn new(id: ScrollSyncGroupId, left_split: SplitId, right_split: SplitId) -> Self {
        Self {
            id,
            left_split,
            right_split,
            scroll_line: 0,
            anchors: vec![SyncAnchor {
                left_line: 0,
                right_line: 0,
            }],
            row_sync: None,
        }
    }

    /// Set the sync anchors (replacing any existing ones)
    /// Anchors should be sorted by left_line
    pub fn set_anchors(&mut self, anchors: Vec<SyncAnchor>) {
        self.anchors = anchors;
        // Ensure there's always at least the origin anchor
        if self.anchors.is_empty() {
            self.anchors.push(SyncAnchor {
                left_line: 0,
                right_line: 0,
            });
        }
    }

    /// Check if a split is part of this sync group
    pub fn contains_split(&self, split_id: SplitId) -> bool {
        self.left_split == split_id || self.right_split == split_id
    }

    /// Check if a split is the left (primary) split
    pub fn is_left_split(&self, split_id: SplitId) -> bool {
        self.left_split == split_id
    }

    /// Convert a line number from left buffer space to right buffer space
    pub fn left_to_right_line(&self, left_line: usize) -> usize {
        map_through_anchors(&self.anchors, left_line, |a| (a.left_line, a.right_line))
    }

    /// Convert a line number from right buffer space to left buffer space
    pub fn right_to_left_line(&self, right_line: usize) -> usize {
        map_through_anchors(&self.anchors, right_line, |a| (a.right_line, a.left_line))
    }

    /// Update scroll position from a scroll delta on the given split
    /// Returns true if this group handled the scroll
    pub fn apply_scroll_delta(&mut self, split_id: SplitId, delta_lines: isize) -> bool {
        if !self.contains_split(split_id) {
            return false;
        }

        // Apply delta to scroll_line (which is always in left buffer space)
        let new_scroll = if delta_lines >= 0 {
            self.scroll_line.saturating_add(delta_lines as usize)
        } else {
            self.scroll_line.saturating_sub(delta_lines.unsigned_abs())
        };

        self.scroll_line = new_scroll;
        true
    }

    /// Set scroll position directly
    /// The line number should be in left buffer space
    pub fn set_scroll_line(&mut self, line: usize) {
        self.scroll_line = line;
    }

    /// Get the scroll line for the left split
    #[cfg(test)]
    pub fn left_scroll_line(&self) -> usize {
        self.scroll_line
    }

    /// Get the scroll line for the right split (derived via anchors)
    #[cfg(test)]
    pub fn right_scroll_line(&self) -> usize {
        self.left_to_right_line(self.scroll_line)
    }
}

/// `line` on one side mapped to the other through `anchors`, read as
/// `(this side, other side)` by `sides`: from the last anchor at or above it
/// (with none, from the top of both), line for line — but never past the next
/// anchor's line on the other side. Between two anchors the other side may
/// have fewer lines (a change only this side has): it holds at the edge of
/// the change until this side reaches the anchor after it, rather than
/// running on into lines that don't match.
pub fn map_through_anchors(
    anchors: &[SyncAnchor],
    line: usize,
    sides: impl Fn(&SyncAnchor) -> (usize, usize),
) -> usize {
    let (from, to) = anchors
        .iter()
        .map(&sides)
        .rfind(|(this, _)| *this <= line)
        .unwrap_or((0, 0));
    let mapped = to.saturating_add(line - from);
    match anchors.iter().map(&sides).find(|(this, _)| *this > line) {
        Some((_, next)) => mapped.min(next.max(to)),
        None => mapped,
    }
}

/// Where the last row-space sync of a same-buffer group left both panes.
///
/// Kept so a sync never moves the partner against the direction the user
/// scrolled, and so that focusing the other pane — which makes *it* the one
/// that drives — does not re-derive a position nobody scrolled to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RowSyncState {
    /// The pane that drove the sync.
    pub active: SplitId,
    /// Its top row then.
    pub active_top: u32,
    /// The partner's top row the sync gave it.
    pub other_top: u32,
}

/// The screen row at which a same-buffer pair is lined up, for a driving pane
/// whose top row is `top`, scrolling at most to `max_top`, `height` rows tall.
///
/// Both panes show the same text, one of them taller (a rendered preview's
/// table frames and list spacing), so only one row of the window can show the
/// same line in both. Lining up the top row leaves everything below it drifting
/// apart by the whole window's difference; lining up the middle hides the top of
/// the document in the taller pane while the other shows line 1. So the aligned
/// row slides: row 0 while the document's top is in view, the middle of the
/// window through the body, the last row once its end is — each pane then shows
/// its own start and end, and the drift is split above and below the middle.
pub fn focus_row(top: u32, max_top: u32, height: u32) -> u32 {
    let last = height.saturating_sub(1);
    if max_top == 0 {
        return 0;
    }
    if max_top < last {
        // A document barely taller than the window: no room for the two ramps,
        // so slide evenly across it.
        return ((top.min(max_top) as u64 * last as u64) / max_top as u64) as u32;
    }
    let half = last / 2;
    let to_end = max_top.saturating_sub(top);
    if top < half {
        top
    } else if to_end < last - half {
        last - to_end
    } else {
        half
    }
}

/// The partner's top row for a driving pane whose top row is `top`.
///
/// `map` takes a driving-pane row to the partner row showing the same text.
/// The partner's top is chosen so the row [`focus_row`] picks shows the same
/// line in both panes, clamped to what the partner can scroll to. The two ends
/// are pinned: the top of one pane shows the top of the other, the end the end.
pub fn synced_top(
    top: u32,
    max_top: u32,
    height: u32,
    other_max_top: u32,
    map: impl Fn(u32) -> u32,
) -> u32 {
    if top == 0 {
        return 0;
    }
    if top >= max_top {
        return other_max_top;
    }
    let focus = focus_row(top, max_top, height);
    map(top + focus).saturating_sub(focus).min(other_max_top)
}

/// The driving pane's top row for which [`synced_top`] — the curve `curve` —
/// puts the partner at `other_top`: the inverse of that curve, for when the
/// partner is the one scrolled.
///
/// One curve relates the pair whichever pane is scrolled, so scrolling one and
/// then the other never jumps. The partner can stop between two of the curve's
/// values (it is taller, so it has rows the curve steps over); the driver then
/// takes the last top at or before it, and moving on in either pane carries on
/// along the curve in the same direction.
pub fn inverse_synced_top(other_top: u32, max_top: u32, curve: impl Fn(u32) -> u32) -> u32 {
    // The largest top whose partner is at or above `other_top`. The curve is
    // monotonic, so a binary search finds it.
    let (mut lo, mut hi) = (0u32, max_top);
    while lo < hi {
        let mid = lo + (hi - lo).div_ceil(2);
        if curve(mid) <= other_top {
            lo = mid;
        } else {
            hi = mid - 1;
        }
    }
    lo
}

/// The row of `to` showing what row `row` of `from` shows: two layouts of the
/// same buffer, as two panes draw it.
///
/// Lines are where the two agree, so the row's line is found in `from` and its
/// place inside the line carried over in proportion: the third of five rows a
/// line takes in one pane is the row three fifths of the way through it in the
/// other. Proportion keeps the map monotonic — a later row never maps to an
/// earlier one — so the partner never scrolls back while the driver goes on.
pub fn map_row(
    from: &crate::view::wrap_index::WrapIndex,
    to: &crate::view::wrap_index::WrapIndex,
    buffer: &crate::model::buffer::Buffer,
    row: u32,
) -> u32 {
    let line = from.byte_of_row(buffer, row).line;
    let first = from.line_first_row(line);
    let from_rows = from.line_wrap(line).map_or(1, |l| l.total_rows()).max(1);
    let to_rows = to.line_wrap(line).map_or(1, |l| l.total_rows());
    let within = row.saturating_sub(first).min(from_rows - 1);
    to.line_first_row(line) + ((within as u64 * to_rows as u64) / from_rows as u64) as u32
}

/// Manager for scroll sync groups
#[derive(Debug, Default)]
pub struct ScrollSyncManager {
    /// Active scroll sync groups
    groups: Vec<ScrollSyncGroup>,
}

impl ScrollSyncManager {
    /// Create a new scroll sync manager
    pub fn new() -> Self {
        Self { groups: Vec::new() }
    }

    /// Create a scroll sync group with a plugin-provided ID
    /// Returns true if created successfully, false if ID already exists
    pub fn create_group_with_id(
        &mut self,
        id: ScrollSyncGroupId,
        left_split: SplitId,
        right_split: SplitId,
    ) -> bool {
        // Check if ID already exists
        if self.groups.iter().any(|g| g.id == id) {
            return false;
        }

        let group = ScrollSyncGroup::new(id, left_split, right_split);
        self.groups.push(group);
        true
    }

    /// Remove a scroll sync group by ID
    pub fn remove_group(&mut self, id: ScrollSyncGroupId) -> bool {
        if let Some(pos) = self.groups.iter().position(|g| g.id == id) {
            self.groups.remove(pos);
            true
        } else {
            false
        }
    }

    /// Drop every group with a split `is_live` rejects: a group whose pane
    /// closed syncs nothing, and its id would refuse a new group.
    pub fn retain_live_groups(&mut self, is_live: impl Fn(SplitId) -> bool) {
        self.groups
            .retain(|g| is_live(g.left_split) && is_live(g.right_split));
    }

    /// Get a mutable reference to a group by ID
    pub fn get_group_mut(&mut self, id: ScrollSyncGroupId) -> Option<&mut ScrollSyncGroup> {
        self.groups.iter_mut().find(|g| g.id == id)
    }

    /// Find the group containing a specific split
    pub fn find_group_for_split(&self, split_id: SplitId) -> Option<&ScrollSyncGroup> {
        self.groups.iter().find(|g| g.contains_split(split_id))
    }

    /// Find the group containing a specific split (mutable)
    pub fn find_group_for_split_mut(&mut self, split_id: SplitId) -> Option<&mut ScrollSyncGroup> {
        self.groups.iter_mut().find(|g| g.contains_split(split_id))
    }

    /// Check if a split is in any scroll sync group
    pub fn is_split_synced(&self, split_id: SplitId) -> bool {
        self.groups.iter().any(|g| g.contains_split(split_id))
    }

    /// Get all groups (for iteration during render)
    pub fn groups(&self) -> &[ScrollSyncGroup] {
        &self.groups
    }

    /// Apply scroll delta to the group containing the split
    /// Returns true if a group handled the scroll
    pub fn apply_scroll_delta(&mut self, split_id: SplitId, delta_lines: isize) -> bool {
        if let Some(group) = self.find_group_for_split_mut(split_id) {
            group.apply_scroll_delta(split_id, delta_lines);
            true
        } else {
            false
        }
    }

    /// Set anchors for a group
    pub fn set_anchors(&mut self, group_id: ScrollSyncGroupId, anchors: Vec<SyncAnchor>) {
        if let Some(group) = self.get_group_mut(group_id) {
            group.set_anchors(anchors);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_left_to_right_line_simple() {
        let mut group = ScrollSyncGroup::new(1, SplitId(1), SplitId(2));
        group.set_anchors(vec![
            SyncAnchor {
                left_line: 0,
                right_line: 0,
            },
            SyncAnchor {
                left_line: 10,
                right_line: 10,
            },
            SyncAnchor {
                left_line: 20,
                right_line: 25,
            }, // Right has 5 extra lines
        ]);

        // Before any anchors
        assert_eq!(group.left_to_right_line(0), 0);
        assert_eq!(group.left_to_right_line(5), 5);

        // After second anchor (1:1 mapping)
        assert_eq!(group.left_to_right_line(10), 10);
        assert_eq!(group.left_to_right_line(15), 15);

        // After third anchor (offset by 5)
        assert_eq!(group.left_to_right_line(20), 25);
        assert_eq!(group.left_to_right_line(25), 30);
    }

    /// With one anchor per change, at its end, a side scrolled through a
    /// change only it has holds the other side at the change's edge; past it,
    /// the two move together again.
    #[test]
    fn a_one_sided_change_holds_the_other_side_at_its_edge() {
        let mut group = ScrollSyncGroup::new(1, SplitId(1), SplitId(2));
        // The left has 100 lines at line 10 the right doesn't.
        group.set_anchors(vec![SyncAnchor {
            left_line: 110,
            right_line: 10,
        }]);
        assert_eq!(group.left_to_right_line(5), 5);
        assert_eq!(group.left_to_right_line(10), 10);
        assert_eq!(group.left_to_right_line(60), 10);
        assert_eq!(group.left_to_right_line(110), 10);
        assert_eq!(group.left_to_right_line(115), 15);
        assert_eq!(group.right_to_left_line(5), 5);
        assert_eq!(group.right_to_left_line(10), 110);
        assert_eq!(group.right_to_left_line(15), 115);

        // And the mirror: the right has the extra lines.
        group.set_anchors(vec![SyncAnchor {
            left_line: 10,
            right_line: 110,
        }]);
        assert_eq!(group.right_to_left_line(60), 10);
        assert_eq!(group.right_to_left_line(115), 15);
        assert_eq!(group.left_to_right_line(10), 110);
    }

    #[test]
    fn test_right_to_left_line() {
        let mut group = ScrollSyncGroup::new(1, SplitId(1), SplitId(2));
        group.set_anchors(vec![
            SyncAnchor {
                left_line: 0,
                right_line: 0,
            },
            SyncAnchor {
                left_line: 10,
                right_line: 15,
            }, // Right has 5 extra lines
        ]);

        // Before anchor
        assert_eq!(group.right_to_left_line(0), 0);
        assert_eq!(group.right_to_left_line(5), 5);

        // After anchor
        assert_eq!(group.right_to_left_line(15), 10);
        assert_eq!(group.right_to_left_line(20), 15);
    }

    #[test]
    fn focus_row_slides_from_the_top_through_the_middle_to_the_end() {
        // 100 rows to scroll through in a 21-row window: the ramps are 10 rows.
        assert_eq!(focus_row(0, 100, 21), 0);
        assert_eq!(focus_row(4, 100, 21), 4);
        assert_eq!(focus_row(10, 100, 21), 10);
        assert_eq!(focus_row(50, 100, 21), 10);
        assert_eq!(focus_row(90, 100, 21), 10);
        assert_eq!(focus_row(95, 100, 21), 15);
        assert_eq!(focus_row(100, 100, 21), 20);
        // Every step moves the aligned row by at most one, never back.
        for top in 0..100 {
            let (a, b) = (focus_row(top, 100, 21), focus_row(top + 1, 100, 21));
            assert!(b == a || b == a + 1, "top {top}: {a} -> {b}");
        }
        // Too short for both ramps: an even slide, still end to end.
        assert_eq!(focus_row(0, 5, 21), 0);
        assert_eq!(focus_row(5, 5, 21), 20);
        assert_eq!(focus_row(0, 0, 21), 0);
    }

    #[test]
    fn synced_top_pins_the_ends_and_never_steps_back() {
        // A partner twice as tall: row r shows what row r/2 shows here.
        let double = |r: u32| r * 2;
        let (max_top, height, other_max) = (80, 21, 180);
        assert_eq!(synced_top(0, max_top, height, other_max, double), 0);
        assert_eq!(
            synced_top(max_top, max_top, height, other_max, double),
            other_max
        );
        let mut last = 0;
        for top in 1..=max_top {
            let t = synced_top(top, max_top, height, other_max, double);
            assert!(t >= last, "top {top}: partner went back from {last} to {t}");
            last = t;
        }
        // In the body the middle row lines up: row 50+10 here is row 120 there,
        // which the partner shows 10 rows down.
        assert_eq!(synced_top(50, max_top, height, other_max, double), 110);
    }

    #[test]
    fn inverse_synced_top_undoes_the_curve() {
        let (max_top, height, other_max) = (80, 21, 180);
        let curve = |top: u32| synced_top(top, max_top, height, other_max, |r| r * 2);
        for top in 0..=max_top {
            assert_eq!(inverse_synced_top(curve(top), max_top, curve), top);
        }
        // Between two of the curve's values: the earlier top.
        let (a, b) = (curve(40), curve(41));
        assert!(b > a + 1);
        assert_eq!(inverse_synced_top(a + 1, max_top, curve), 40);
        assert_eq!(inverse_synced_top(other_max, max_top, curve), max_top);
    }

    #[test]
    fn test_scroll_delta() {
        let mut group = ScrollSyncGroup::new(1, SplitId(1), SplitId(2));
        group.set_anchors(vec![
            SyncAnchor {
                left_line: 0,
                right_line: 0,
            },
            SyncAnchor {
                left_line: 50,
                right_line: 60,
            },
        ]);

        // Initial position
        assert_eq!(group.left_scroll_line(), 0);
        assert_eq!(group.right_scroll_line(), 0);

        // Scroll down 10 lines
        group.apply_scroll_delta(SplitId(1), 10);
        assert_eq!(group.left_scroll_line(), 10);
        assert_eq!(group.right_scroll_line(), 10);

        // Scroll to position past anchor
        group.set_scroll_line(55);
        assert_eq!(group.left_scroll_line(), 55);
        assert_eq!(group.right_scroll_line(), 65); // 60 + 5
    }
}
