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

use unicode_segmentation::UnicodeSegmentation;

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

    let pos = pos.min(s.len());

    // Find all grapheme boundaries up to our position
    let mut last_boundary = 0;
    for (idx, _) in s.grapheme_indices(true) {
        if idx >= pos {
            break;
        }
        last_boundary = idx;
    }

    last_boundary
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

    // Find the grapheme that contains our position, then return its end
    for (idx, grapheme) in s.grapheme_indices(true) {
        let end = idx + grapheme.len();
        if idx >= pos {
            // This grapheme starts at or after our position
            return end;
        }
        if end > pos {
            // Our position is within this grapheme
            return end;
        }
    }

    s.len()
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

    for (idx, grapheme) in s.grapheme_indices(true) {
        let end = idx + grapheme.len();
        if idx <= pos && pos < end {
            return Some((grapheme, idx, end));
        }
    }

    None
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
