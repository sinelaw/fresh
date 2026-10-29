//! Text-based reference highlighting (WASM-compatible)
//!
//! When the cursor is on a word, all occurrences of that word in the viewport
//! are highlighted. This module provides simple whole-word text matching
//! without requiring tree-sitter.
//!
//! # Design
//!
//! - Pure text matching (no AST analysis)
//! - Efficient viewport-based search
//! - Respects word boundaries (won't match partial words)

use crate::model::buffer::Buffer;
use crate::primitives::highlight_types::HighlightSpan;
use crate::primitives::word_navigation::{
    char_at, char_before, is_unicode_word_char, is_word_char_at, is_word_char_before,
};
use ratatui::style::Color;
use std::ops::Range;

/// Default highlight color for word occurrences
pub const DEFAULT_HIGHLIGHT_COLOR: Color = Color::Rgb(60, 60, 80);

/// Text-based reference highlighter (WASM-compatible)
///
/// Highlights all occurrences of the word under cursor using simple
/// text matching. This is the fallback mode when tree-sitter is not available.
pub struct TextReferenceHighlighter {
    /// Color for occurrence highlights
    pub highlight_color: Color,
    /// Minimum word length to trigger highlighting
    pub min_word_length: usize,
    /// Whether highlighting is enabled
    pub enabled: bool,
}

impl Default for TextReferenceHighlighter {
    fn default() -> Self {
        Self {
            highlight_color: DEFAULT_HIGHLIGHT_COLOR,
            min_word_length: 2,
            enabled: true,
        }
    }
}

impl TextReferenceHighlighter {
    /// Create a new text reference highlighter
    pub fn new() -> Self {
        Self::default()
    }

    /// Create with custom highlight color
    pub fn with_color(color: Color) -> Self {
        Self {
            highlight_color: color,
            ..Self::default()
        }
    }

    /// Highlight occurrences of word under cursor
    ///
    /// Returns highlight spans for all whole-word matches of the word
    /// at `cursor_position` within the viewport range.
    pub fn highlight_occurrences(
        &self,
        buffer: &Buffer,
        cursor_position: usize,
        viewport_start: usize,
        viewport_end: usize,
    ) -> Vec<HighlightSpan> {
        if !self.enabled {
            return Vec::new();
        }

        word_occurrences(
            buffer,
            cursor_position,
            viewport_start..viewport_end,
            self.min_word_length,
        )
        .into_iter()
        .map(|range| HighlightSpan {
            range,
            color: self.highlight_color,
            bg: None,
            category: None,
        })
        .collect()
    }
}

/// Each whole-word occurrence, overlapping `range`, of the word at
/// `cursor_position` — if it is at least `min_word_length` bytes long.
pub fn word_occurrences(
    buffer: &Buffer,
    cursor_position: usize,
    range: Range<usize>,
    min_word_length: usize,
) -> Vec<Range<usize>> {
    let Some(word_range) = word_at(buffer, cursor_position) else {
        return Vec::new();
    };
    let word_bytes = buffer.slice_bytes(word_range);
    match std::str::from_utf8(&word_bytes) {
        Ok(word) if word.len() >= min_word_length => {
            find_matches_in_range(buffer, word, range.start, range.end, true)
        }
        _ => Vec::new(),
    }
}

/// The word under the cursor at `position` (or just before it at the end
/// of the buffer); `None` when the cursor is not on a word character. A word
/// is a run of Unicode word characters, as the regex `\w` counts them
/// ([`is_unicode_word_char`]), so `caf` is not a word inside `café`.
pub fn word_at(buffer: &Buffer, position: usize) -> Option<Range<usize>> {
    let len = buffer.len();
    let on_word = if position < len {
        is_word_char_at(buffer, position)
    } else {
        position == len && is_word_char_before(buffer, position)
    };
    if !on_word {
        return None;
    }
    // Out from the cursor a character at a time, as far as a long word can
    // be; a byte that is no valid character just ends the word
    const MAX_WORD: usize = 1000;
    let mut start = position;
    while let Some(c) = char_before(buffer, start).filter(|&c| is_unicode_word_char(c)) {
        if position - start >= MAX_WORD {
            break;
        }
        start -= c.len_utf8();
    }
    let mut end = position;
    while let Some(c) = char_at(buffer, end).filter(|&c| is_unicode_word_char(c)) {
        if end - position >= MAX_WORD {
            break;
        }
        end += c.len_utf8();
    }
    (start < end).then_some(start..end)
}

/// The most text a match search covers: beyond it (a huge single-line
/// file), nothing is highlighted rather than scanning it all.
pub const MAX_SEARCH_RANGE: usize = 1024 * 1024;

/// All literal matches of `needle` overlapping the `start..end` byte range
/// (resident text only). With `whole_word`, matches that continue into a
/// surrounding word are rejected (the cursor-word highlight); without it
/// every match counts (the selection highlight).
pub fn find_matches_in_range(
    buffer: &Buffer,
    needle: &str,
    start: usize,
    end: usize,
    whole_word: bool,
) -> Vec<Range<usize>> {
    // Skip if search range is too large (e.g., huge single-line files)
    if end.saturating_sub(start) > MAX_SEARCH_RANGE {
        return Vec::new();
    }

    let mut occurrences = Vec::new();

    // Get the text in the range (with some padding for edge matches),
    // kept on character boundaries so the slice stays valid UTF-8.
    let search_start = buffer.snap_to_char_boundary(start.saturating_sub(needle.len()));
    let padded_end = (end + needle.len()).min(buffer.len());
    let search_end = {
        let snapped = buffer.snap_to_char_boundary(padded_end);
        if snapped == padded_end {
            padded_end
        } else {
            buffer.next_char_boundary(snapped)
        }
    };

    let bytes = buffer.slice_bytes(search_start..search_end);
    let text = match std::str::from_utf8(&bytes) {
        Ok(s) => s,
        Err(_) => return occurrences,
    };

    // Use match_indices for single-pass searching (creates StrSearcher once)
    // This is much more efficient than repeatedly calling find() which creates
    // a new searcher each time
    for (rel_pos, _) in text.match_indices(needle) {
        let abs_start = search_start + rel_pos;
        let abs_end = abs_start + needle.len();

        // Check if this is a whole word match (not part of a larger word)
        let is_word_start = !whole_word || !is_word_char_before(buffer, abs_start);

        let is_word_end = !whole_word || !is_word_char_at(buffer, abs_end);

        if is_word_start && is_word_end {
            // Only include if it overlaps with the actual viewport
            if abs_start < end && abs_end > start {
                occurrences.push(abs_start..abs_end);
            }
        }
    }

    occurrences
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::filesystem::NoopFileSystem;
    use std::sync::Arc;

    fn make_buffer(content: &str) -> Buffer {
        let fs = Arc::new(NoopFileSystem);
        let mut buf = Buffer::empty(fs);
        buf.insert(0, content);
        buf
    }

    #[test]
    fn word_at_takes_whole_unicode_words() {
        let text = "x café, naïve_1 日本語 e\u{301}t";
        let buffer = Buffer::from_str_test(text);
        let word = |pos: usize| word_at(&buffer, pos).map(|r| &text[r]);
        // Anywhere in a word, its start, its end, on the accented letter
        for (needle, word_text) in [
            ("café", "café"),
            ("é", "café"),
            ("naïve", "naïve_1"),
            ("ïve", "naïve_1"),
            ("_1", "naïve_1"),
            ("本", "日本語"),
            ("e\u{301}", "e\u{301}t"),
            ("\u{301}", "e\u{301}t"),
        ] {
            assert_eq!(
                word(text.find(needle).unwrap()),
                Some(word_text),
                "{needle:?}"
            );
        }
        // Off any word: punctuation, space; at the end, the word before it
        assert_eq!(word(text.find(',').unwrap()), None);
        assert_eq!(word(1), None);
        assert_eq!(word(text.len()), Some("e\u{301}t"));
        assert_eq!(word(text.len() + 1), None);
        let empty = Buffer::from_str_test("");
        assert_eq!(word_at(&empty, 0), None);
        let trailing_space = Buffer::from_str_test("ab ");
        assert_eq!(word_at(&trailing_space, 3), None);
    }

    /// A byte that is no valid UTF-8 near the cursor ends a word, and does
    /// not stop the word under the cursor being found.
    #[test]
    fn word_at_finds_words_beside_invalid_bytes() {
        let fs = Arc::new(NoopFileSystem);
        let mut buffer = Buffer::empty(fs);
        buffer.insert_bytes(0, b"ab\xFFcafe\xE9 x caf".to_vec());
        let cafe = 3;
        assert_eq!(word_at(&buffer, cafe + 1), Some(cafe..cafe + 4));
        assert_eq!(word_at(&buffer, 0), Some(0..2));
        assert_eq!(word_at(&buffer, 2), None);
        assert_eq!(
            word_at(&buffer, buffer.len()),
            Some(buffer.len() - 3..buffer.len())
        );
    }

    /// The text of each whole-word occurrence of the word at `cursor`.
    fn occurrences(text: &str, cursor: usize) -> Vec<&str> {
        let buffer = Buffer::from_str_test(text);
        word_occurrences(&buffer, cursor, 0..buffer.len(), 2)
            .into_iter()
            .map(|range| &text[range])
            .collect()
    }

    /// Letters past ASCII are word characters, as for the regex `\b`: a
    /// word is never matched inside a longer one that goes on with one
    /// (issue #3645).
    #[test]
    fn non_ascii_letters_are_word_characters() {
        let text = "café caf cafe";
        let caf = text.find(" caf ").unwrap() + 1;
        assert_eq!(occurrences(text, caf), vec!["caf"]);
        // On the `é` itself, and on the letters before it: the whole word
        assert_eq!(occurrences(text, text.find('é').unwrap()), vec!["café"]);
        assert_eq!(occurrences(text, 0), vec!["café"]);
    }

    #[test]
    fn non_ascii_words_match_whole() {
        // (text, cursor, the word under it, how many times it occurs whole):
        // Cyrillic, a decomposed accent (`e` + U+0301, a mark, which `\w`
        // includes), and CJK
        let decomposed = "cafe\u{301} cafe cafe\u{301}";
        for (text, cursor, word, expected) in [
            ("при привет при", 0, "при", 2),
            (decomposed, 0, "cafe\u{301}", 2),
            (
                decomposed,
                decomposed.find(" cafe ").unwrap() + 1,
                "cafe",
                1,
            ),
            ("日本 日本語 日本", 0, "日本", 2),
            ("naïve x naïve", "naïve x naïve".len(), "naïve", 2),
        ] {
            assert_eq!(
                occurrences(text, cursor),
                vec![word; expected],
                "{text:?}, cursor at {cursor}"
            );
        }
    }

    #[test]
    fn test_highlight_word_occurrences() {
        let buffer = make_buffer("foo bar foo baz foo");
        let highlighter = TextReferenceHighlighter::new();

        // Cursor on first "foo"
        let spans = highlighter.highlight_occurrences(&buffer, 1, 0, buffer.len());
        assert_eq!(spans.len(), 3); // Three occurrences of "foo"
    }

    #[test]
    fn test_no_partial_matches() {
        let buffer = make_buffer("foobar foo barfoo");
        let highlighter = TextReferenceHighlighter::new();

        // Cursor on standalone "foo"
        let spans = highlighter.highlight_occurrences(&buffer, 8, 0, buffer.len());
        assert_eq!(spans.len(), 1); // Only the standalone "foo", not "foobar" or "barfoo"
    }

    #[test]
    fn test_minimum_word_length() {
        let buffer = make_buffer("a a a a");
        let highlighter = TextReferenceHighlighter::new();

        // Single-character word should not be highlighted (min_word_length = 2)
        let spans = highlighter.highlight_occurrences(&buffer, 0, 0, buffer.len());
        assert_eq!(spans.len(), 0);
    }

    #[test]
    fn test_disabled_highlighting() {
        let buffer = make_buffer("foo foo foo");
        let mut highlighter = TextReferenceHighlighter::new();
        highlighter.enabled = false;

        let spans = highlighter.highlight_occurrences(&buffer, 0, 0, buffer.len());
        assert_eq!(spans.len(), 0);
    }
}
