//! Cursor-dependent activation for conceals and soft breaks.
//!
//! A decoration tagged with a [`ScopedActivation`] is filtered at *query*
//! time against the rendering split's cursor positions, instead of being
//! deleted and recreated by the plugin on every cursor move. This is what
//! lets cursor movement leave the marker set — and therefore the
//! `LineWrapCache` / `WrapIndex` versions — completely untouched.
//!
//! **The scope is a range of the document, so it is carried by markers**, like
//! every other range here. It used to be stored as a fixed length relative to
//! the decoration's own anchor, on the reasoning that an edit inside the line
//! re-fires `lines_changed` and the plugin rebuilds the decoration anyway. It
//! does — but only after a round trip through the plugin thread, and frames are
//! rendered before that lands. In between, the scope was as long as it had been
//! when it was emitted while the text inside it had grown, so a cursor could
//! sit *past* a scope that should still contain it. What that looked like in
//! markdown compose: type at the end of `# a` and the `# ` concealed itself for
//! a frame and then came back, because for that frame no cursor was "in" the
//! heading's line. Markers grow with the text, so what is asked is where the
//! scope is *now*.
//!
//! Two point markers, not one span marker. `MarkerList::create_span` exists for
//! overlap queries and its *end* is not reliable under editing: the interval
//! tree is a BST on interval start, and its edit adjustment descends right from
//! a node whose start precedes the edit — so a span sitting to the left of that
//! node, but reaching past the edit, never has its end shifted. (Overlay ranges
//! hit the same wall, which is why `overlay.rs` keeps its own
//! `start_marker`/`end_marker` and documents the span as "never read for
//! positions".) A point marker is always reached, because a marker at or after
//! the edit is always to the right in that BST.

use crate::model::marker::{MarkerId, MarkerList};
use fresh_core::api::MarkerActivation;

/// Cursor-scope rule stored on a conceal range or soft break.
/// `None` on the owning entry means "always active".
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ScopedActivation {
    /// `true`: active only while a cursor IS in scope (`if-cursor-in`).
    /// `false`: active only while NO cursor is in scope (`unless-cursor-in`).
    pub if_cursor_in: bool,
    /// Start of the scope, inclusive.
    ///
    /// **The owner deletes both ends.** They are markers like the decoration's
    /// own, and leak the same way if a removal path forgets them — see
    /// `ConcealRanges::swap_remove_at` and `SoftBreakPoints::swap_remove_at`,
    /// which are the only places either is destroyed.
    pub scope_start: MarkerId,
    /// End of the scope, exclusive. Right gravity, so text typed at the end of
    /// the scope extends it instead of falling outside it — that is the whole
    /// point of anchoring the scope.
    pub scope_end: MarkerId,
}

impl ScopedActivation {
    /// Convert a wire-format rule (absolute scope bytes) into the stored
    /// form, anchored at `anchor` (the decoration's position at emission
    /// time). A scope starting after the anchor is clamped to it — plugins
    /// scope a decoration to its own span or line, both of which start at or
    /// before the decoration.
    pub fn from_absolute(
        rule: &MarkerActivation,
        anchor: usize,
        marker_list: &mut MarkerList,
    ) -> Self {
        let start = rule.scope_start.min(anchor);
        let end = rule.scope_end.max(start);
        Self {
            if_cursor_in: rule.if_cursor_in,
            scope_start: marker_list.create(start),
            scope_end: marker_list.create(end),
        }
    }

    /// Release the scope's markers. Called by the owning decoration's removal
    /// path, which is the only thing that knows when the rule is gone.
    pub fn release(&self, marker_list: &mut MarkerList) {
        marker_list.delete(self.scope_start);
        marker_list.delete(self.scope_end);
    }

    /// Whether the decoration is active given the rendering split's cursor
    /// byte positions.
    ///
    /// A scope whose markers have gone answers "no cursor in it", which leaves
    /// an `unless-cursor-in` decoration active — the concealed form, i.e. what
    /// the document looks like when nothing is being edited.
    #[inline]
    pub fn is_active(&self, marker_list: &MarkerList, cursors: &[usize]) -> bool {
        let start = marker_list.get_position(self.scope_start).unwrap_or(0);
        let end = marker_list.get_position(self.scope_end).unwrap_or(0);
        let cursor_in = cursors.iter().any(|&p| p >= start && p < end);
        cursor_in == self.if_cursor_in
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rule(if_cursor_in: bool, start: usize, end: usize) -> MarkerActivation {
        MarkerActivation {
            if_cursor_in,
            scope_start: start,
            scope_end: end,
        }
    }

    #[test]
    fn unless_cursor_in_active_without_cursor() {
        let mut markers = MarkerList::new();
        let a = ScopedActivation::from_absolute(&rule(false, 10, 20), 12, &mut markers);
        assert!(a.is_active(&markers, &[]));
        assert!(a.is_active(&markers, &[5, 25]));
        assert!(!a.is_active(&markers, &[10]));
        assert!(!a.is_active(&markers, &[19]));
        assert!(a.is_active(&markers, &[20])); // end is exclusive
    }

    #[test]
    fn if_cursor_in_active_only_with_cursor() {
        let mut markers = MarkerList::new();
        let a = ScopedActivation::from_absolute(&rule(true, 10, 20), 12, &mut markers);
        assert!(!a.is_active(&markers, &[]));
        assert!(a.is_active(&markers, &[15]));
        assert!(!a.is_active(&markers, &[20]));
    }

    #[test]
    fn the_scope_moves_with_an_edit_before_it() {
        let mut markers = MarkerList::new();
        // Emitted with scope [10, 20). Five bytes inserted ahead of it shift
        // the whole scope along, so the same text is still covered.
        let a = ScopedActivation::from_absolute(&rule(false, 10, 20), 12, &mut markers);
        markers.adjust_for_insert(0, 5);
        assert!(!a.is_active(&markers, &[15])); // shifted scope [15, 25)
        assert!(a.is_active(&markers, &[10])); // now outside
    }

    /// The regression: text typed *inside* the scope widens it.
    ///
    /// The line's own text grew, so the position one past it is still on the
    /// line. Held as a fixed length this is where the scope fell short, and a
    /// cursor at the end of what it had just typed read as outside — which
    /// flipped an `unless-cursor-in` conceal on for a frame.
    ///
    /// The decoration's own markers are created here too, because the shape of
    /// the marker tree is what the span-marker version of this got wrong: with
    /// neighbours in the tree, a span's end stopped being adjusted at all.
    #[test]
    fn the_scope_grows_with_an_edit_inside_it() {
        let mut markers = MarkerList::new();
        // `# a` as the whole buffer: the heading's conceal covers `# ` and is
        // scoped to the line, one past its last byte.
        let _conceal_start = markers.create(0);
        let _conceal_end = markers.create(2);
        let a = ScopedActivation::from_absolute(&rule(false, 0, 4), 0, &mut markers);
        assert!(!a.is_active(&markers, &[3]), "the cursor is on the line");

        // Type one character at the end of the line. The cursor lands on 4,
        // which the scope has to have grown to contain.
        markers.adjust_for_insert(3, 1);
        assert!(
            !a.is_active(&markers, &[4]),
            "a cursor at the end of the text it just typed is still on that line"
        );

        // And it keeps up with a burst, which is what outran the one byte of
        // slack the emitted scope used to carry.
        markers.adjust_for_insert(4, 6);
        assert!(!a.is_active(&markers, &[10]));
    }

    /// Deleting inside the scope shrinks it, so the cursor leaves it at the
    /// right byte rather than a stale one.
    #[test]
    fn the_scope_shrinks_with_a_deletion_inside_it() {
        let mut markers = MarkerList::new();
        let a = ScopedActivation::from_absolute(&rule(false, 0, 10), 0, &mut markers);
        markers.adjust_for_delete(4, 3); // scope [0, 7)
        assert!(!a.is_active(&markers, &[6]));
        assert!(a.is_active(&markers, &[7]));
    }

    /// Released markers leave the decoration in its resting form rather than
    /// reading someone else's positions.
    #[test]
    fn a_released_scope_reads_as_empty() {
        let mut markers = MarkerList::new();
        let a = ScopedActivation::from_absolute(&rule(false, 10, 20), 12, &mut markers);
        a.release(&mut markers);
        assert!(a.is_active(&markers, &[15]), "unless-cursor-in stays on");
    }
}
