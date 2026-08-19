//! Grapheme cluster utilities for proper cursor movement and text editing
//!
//! This module provides functions for navigating and editing text by grapheme clusters
//! rather than Unicode code points. This is essential for proper handling of:
//! - Thai and other Southeast Asian scripts (base + combining marks)
//! - Emoji with modifiers (skin tones, ZWJ sequences)
//! - Latin text with combining diacritics (e.g., ñ as n + combining tilde)
//!
//! A grapheme cluster is what a user perceives as a single character.
//! For example, Thai "ที่" looks like one character but is 3 Unicode code points.

use unicode_segmentation::{GraphemeCursor, UnicodeSegmentation};

/// Snap `pos` down to a UTF-8 code-point boundary so it can be handed to
/// [`GraphemeCursor`], which requires boundary-aligned offsets. A position
/// inside a code point can never be a grapheme boundary, so snapping down
/// preserves the semantics of every caller below.
#[inline]
fn snap_to_char_boundary(s: &str, mut pos: usize) -> usize {
    pos = pos.min(s.len());
    while pos > 0 && !s.is_char_boundary(pos) {
        pos -= 1;
    }
    pos
}

/// Find the byte position of the previous grapheme cluster boundary.
///
/// Given a position within a string, returns the byte offset where the
/// previous grapheme cluster starts.
///
/// # Examples
/// ```ignore
/// let s = "ที่นี่"; // Thai text with combining marks
/// let pos = prev_grapheme_boundary(s, 9); // After first grapheme
/// assert_eq!(pos, 0); // Start of string
/// ```
#[inline]
pub fn prev_grapheme_boundary(s: &str, pos: usize) -> usize {
    if pos == 0 || s.is_empty() {
        return 0;
    }

    // A position INSIDE a code point is inside that code point's
    // cluster: the boundary strictly before it is the containing
    // cluster's START, which the snapped char boundary may already be
    // (single-code-point cluster).  Stepping `prev_boundary` from the
    // snapped position instead would overshoot one cluster left.
    let snapped = snap_to_char_boundary(s, pos);
    if snapped < pos {
        return snap_to_grapheme_boundary(s, snapped);
    }
    let mut cursor = GraphemeCursor::new(snapped, s.len(), true);
    // With the whole string as a single chunk (chunk_start == 0) the cursor
    // never needs pre-context, so this cannot fail.
    cursor.prev_boundary(s, 0).ok().flatten().unwrap_or(0)
}

/// Find the byte position of the next grapheme cluster boundary.
///
/// Given a position within a string, returns the byte offset after the
/// current grapheme cluster ends.
///
/// # Examples
/// ```ignore
/// let s = "ที่นี่"; // Thai text with combining marks
/// let pos = next_grapheme_boundary(s, 0); // At start
/// assert_eq!(pos, 9); // After first grapheme cluster "ที่"
/// ```
#[inline]
pub fn next_grapheme_boundary(s: &str, pos: usize) -> usize {
    if pos >= s.len() || s.is_empty() {
        return s.len();
    }

    // A mid-cluster position (snapped or not) advances to the end of the
    // containing cluster — the next boundary after it.
    let pos = snap_to_char_boundary(s, pos);
    let mut cursor = GraphemeCursor::new(pos, s.len(), true);
    cursor.next_boundary(s, 0).ok().flatten().unwrap_or(s.len())
}

/// Get the grapheme cluster at the given position.
///
/// Returns the grapheme cluster that starts at or contains the given byte position,
/// along with its start and end byte offsets.
///
/// Returns `None` if the position is at or beyond the end of the string.
#[inline]
pub fn grapheme_at(s: &str, pos: usize) -> Option<(&str, usize, usize)> {
    if pos >= s.len() || s.is_empty() {
        return None;
    }

    let start = snap_to_grapheme_boundary(s, pos);
    let end = next_grapheme_boundary(s, start);
    Some((&s[start..end], start, end))
}

/// Snap a byte position **down** to the nearest grapheme-cluster
/// boundary at or before it.
///
/// Unlike [`prev_grapheme_boundary`], a `pos` that already sits on a
/// boundary is returned unchanged — this is the "don't land inside a
/// cluster" clamp used when placing the cursor from an externally
/// computed byte offset (e.g. a mouse click), not a movement operation.
///
/// # Examples
/// ```ignore
/// let s = "aที่b"; // 'a'(1) + Thai cluster(9) + 'b'(1)
/// assert_eq!(snap_to_grapheme_boundary(s, 1), 1);  // already a boundary
/// assert_eq!(snap_to_grapheme_boundary(s, 5), 1);  // inside the cluster → its start
/// assert_eq!(snap_to_grapheme_boundary(s, 10), 10); // cluster end / 'b' start
/// ```
#[inline]
pub fn snap_to_grapheme_boundary(s: &str, pos: usize) -> usize {
    if pos == 0 || s.is_empty() {
        return 0;
    }
    if pos >= s.len() {
        return s.len();
    }
    let pos = snap_to_char_boundary(s, pos);
    let mut cursor = GraphemeCursor::new(pos, s.len(), true);
    match cursor.is_boundary(s, 0) {
        Ok(true) => pos,
        // Not a boundary → start of the containing cluster.
        _ => cursor.prev_boundary(s, 0).ok().flatten().unwrap_or(0),
    }
}

/// Count the number of grapheme clusters in a string.
///
/// This is what users would count as "characters".
#[inline]
pub fn grapheme_count(s: &str) -> usize {
    s.graphemes(true).count()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ascii_navigation() {
        let s = "hello";
        assert_eq!(prev_grapheme_boundary(s, 0), 0);
        assert_eq!(prev_grapheme_boundary(s, 1), 0);
        assert_eq!(prev_grapheme_boundary(s, 3), 2);
        assert_eq!(next_grapheme_boundary(s, 0), 1);
        assert_eq!(next_grapheme_boundary(s, 4), 5);
        assert_eq!(next_grapheme_boundary(s, 5), 5);
    }

    #[test]
    fn test_thai_navigation() {
        // Thai "ที่" = ท (3 bytes) + ี (3 bytes) + ่ (3 bytes) = 9 bytes, 1 grapheme
        let s = "ที่";
        assert_eq!(s.len(), 9);
        assert_eq!(grapheme_count(s), 1);

        // From start, next should jump to end (past the whole grapheme)
        assert_eq!(next_grapheme_boundary(s, 0), 9);

        // From end, prev should jump to start
        assert_eq!(prev_grapheme_boundary(s, 9), 0);

        // From middle of grapheme, next should still go to end
        assert_eq!(next_grapheme_boundary(s, 3), 9);

        // From middle of grapheme, prev should go to start
        assert_eq!(prev_grapheme_boundary(s, 3), 0);
    }

    #[test]
    fn test_snap_to_grapheme_boundary() {
        // ASCII: every byte is a boundary, so nothing moves.
        assert_eq!(snap_to_grapheme_boundary("hello", 3), 3);
        assert_eq!(snap_to_grapheme_boundary("hello", 0), 0);
        assert_eq!(snap_to_grapheme_boundary("hello", 99), 5);

        // Thai cluster (9 bytes) + 'x': interior offsets snap to the
        // cluster start; the boundary at its end is preserved.
        let s = "ที่x";
        assert_eq!(snap_to_grapheme_boundary(s, 0), 0);
        assert_eq!(snap_to_grapheme_boundary(s, 3), 0); // mid-cluster
        assert_eq!(snap_to_grapheme_boundary(s, 6), 0); // mid-cluster
        assert_eq!(snap_to_grapheme_boundary(s, 9), 9); // cluster end / 'x' start
        assert_eq!(snap_to_grapheme_boundary(s, 10), 10); // end of string
    }

    #[test]
    fn test_thai_multiple_graphemes() {
        // "ที่นี่" = 2 grapheme clusters, each 9 bytes
        let s = "ที่นี่";
        assert_eq!(s.len(), 18);
        assert_eq!(grapheme_count(s), 2);

        // Navigation from start
        assert_eq!(next_grapheme_boundary(s, 0), 9);
        assert_eq!(next_grapheme_boundary(s, 9), 18);

        // Navigation from end
        assert_eq!(prev_grapheme_boundary(s, 18), 9);
        assert_eq!(prev_grapheme_boundary(s, 9), 0);
    }

    #[test]
    fn test_emoji_navigation() {
        // Family emoji with ZWJ
        let s = "👨‍👩‍👧";
        assert_eq!(grapheme_count(s), 1);

        // Should treat as single grapheme
        assert_eq!(next_grapheme_boundary(s, 0), s.len());
        assert_eq!(prev_grapheme_boundary(s, s.len()), 0);
    }

    #[test]
    fn test_combining_diacritics() {
        // "é" as e + combining acute accent
        let s = "e\u{0301}"; // e + ́
        assert_eq!(s.chars().count(), 2); // 2 code points
        assert_eq!(grapheme_count(s), 1); // 1 grapheme

        assert_eq!(next_grapheme_boundary(s, 0), s.len());
        assert_eq!(prev_grapheme_boundary(s, s.len()), 0);
    }

    #[test]
    fn test_mixed_content() {
        // ASCII + Thai + ASCII
        let s = "aที่b";
        // 'a' (1) + ที่ (9) + 'b' (1) = 11 bytes
        assert_eq!(s.len(), 11);
        assert_eq!(grapheme_count(s), 3);

        assert_eq!(next_grapheme_boundary(s, 0), 1); // past 'a'
        assert_eq!(next_grapheme_boundary(s, 1), 10); // past Thai
        assert_eq!(next_grapheme_boundary(s, 10), 11); // past 'b'

        assert_eq!(prev_grapheme_boundary(s, 11), 10); // before 'b'
        assert_eq!(prev_grapheme_boundary(s, 10), 1); // before Thai
        assert_eq!(prev_grapheme_boundary(s, 1), 0); // before 'a'
    }

    #[test]
    fn test_grapheme_at() {
        let s = "aที่b";

        let (g, start, end) = grapheme_at(s, 0).unwrap();
        assert_eq!(g, "a");
        assert_eq!((start, end), (0, 1));

        let (g, start, end) = grapheme_at(s, 1).unwrap();
        assert_eq!(g, "ที่");
        assert_eq!((start, end), (1, 10));

        let (g, start, end) = grapheme_at(s, 5).unwrap(); // middle of Thai
        assert_eq!(g, "ที่");
        assert_eq!((start, end), (1, 10));

        let (g, start, end) = grapheme_at(s, 10).unwrap();
        assert_eq!(g, "b");
        assert_eq!((start, end), (10, 11));

        assert!(grapheme_at(s, 11).is_none()); // past end
    }

    #[test]
    fn test_empty_string() {
        let s = "";
        assert_eq!(prev_grapheme_boundary(s, 0), 0);
        assert_eq!(next_grapheme_boundary(s, 0), 0);
        assert_eq!(grapheme_count(s), 0);
        assert!(grapheme_at(s, 0).is_none());
    }
}
