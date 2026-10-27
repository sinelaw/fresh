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
use crate::primitives::word_navigation::{find_word_end, find_word_start, is_word_char};
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
/// of the buffer); `None` when the cursor is not on a word character.
pub fn word_at(buffer: &Buffer, position: usize) -> Option<Range<usize>> {
    let buf_len = buffer.len();
    if position > buf_len {
        return None;
    }

    // Check if cursor is on a word character
    // Need to handle cursor at end of buffer
    let is_on_word = if position < buf_len {
        let byte_at_pos = buffer.slice_bytes(position..position + 1);
        byte_at_pos
            .first()
            .map(|&b| is_word_char(b))
            .unwrap_or(false)
    } else if position > 0 {
        // Cursor at end of buffer - check previous character
        let byte_before = buffer.slice_bytes(position - 1..position);
        byte_before
            .first()
            .map(|&b| is_word_char(b))
            .unwrap_or(false)
    } else {
        false
    };

    if !is_on_word && position > 0 {
        // Check if we're just after a word AND the cursor is at end of buffer
        // or the character before was a word char but current is not
        // This handles cursor positioned right after a word (e.g., at end of "foo|")
        let byte_before = buffer.slice_bytes(position.saturating_sub(1)..position);
        let is_after_word = byte_before
            .first()
            .map(|&b| is_word_char(b))
            .unwrap_or(false);

        // Only use "word before cursor" if we're at end of buffer
        // Otherwise, cursor on whitespace/punctuation should not highlight
        if is_after_word && position >= buf_len {
            // Use the word before cursor
            let start = find_word_start(buffer, position.saturating_sub(1));
            let end = position;
            if start < end {
                return Some(start..end);
            }
        }
        return None;
    }

    if !is_on_word {
        return None;
    }

    // Find word boundaries
    let start = find_word_start(buffer, position);
    let end = find_word_end(buffer, position);

    if start < end {
        Some(start..end)
    } else {
        None
    }
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
        let is_word_start = !whole_word || abs_start == 0 || {
            let prev_byte = buffer.slice_bytes(abs_start - 1..abs_start);
            prev_byte.first().map(|&b| !is_word_char(b)).unwrap_or(true)
        };

        let is_word_end = !whole_word || abs_end >= buffer.len() || {
            let next_byte = buffer.slice_bytes(abs_end..abs_end + 1);
            next_byte.first().map(|&b| !is_word_char(b)).unwrap_or(true)
        };

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
