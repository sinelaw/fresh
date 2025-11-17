//! Semantic highlighting for word occurrences under cursor
//!
//! When the cursor is on a word/identifier, all occurrences of that word
//! in the current viewport are highlighted with a subtle background color.
//!
//! # Design
//! - Uses the same `HighlightSpan` approach as syntax highlighting for efficiency
//! - Computed on-demand during rendering (no persistent markers)
//! - Only highlights occurrences within the visible viewport
//!
//! # Future Enhancement: Tree-sitter Scope-Aware Highlighting
//! Currently uses text matching to find occurrences. A better approach would be
//! to use tree-sitter's "locals" queries to find only semantically-related
//! identifiers (same variable binding). This would:
//! - Not highlight `x` in one function when cursor is on `x` in another function
//! - Respect lexical scoping rules
//! - Match how VSCode's documentHighlight works
//!
//! This would require:
//! - Running tree-sitter locals queries for each language
//! - Building a symbol table mapping definitions to references
//! - Tracking scope boundaries

use crate::highlighter::HighlightSpan;
use crate::text_buffer::Buffer;
use crate::word_navigation::{find_word_end, find_word_start, is_word_char};
use ratatui::style::Color;
use std::ops::Range;

/// Default subtle background color for occurrence highlights
/// A dark gray that's visible but not distracting
pub const DEFAULT_HIGHLIGHT_COLOR: Color = Color::Rgb(60, 60, 80);

/// Semantic highlighter for word occurrences
pub struct SemanticHighlighter {
    /// Color for occurrence highlights
    pub highlight_color: Color,
    /// Minimum word length to trigger highlighting
    pub min_word_length: usize,
    /// Whether semantic highlighting is enabled
    pub enabled: bool,
}

impl SemanticHighlighter {
    /// Create a new semantic highlighter with default settings
    pub fn new() -> Self {
        Self {
            highlight_color: DEFAULT_HIGHLIGHT_COLOR,
            min_word_length: 2,
            enabled: true,
        }
    }

    /// Set the highlight color
    pub fn with_color(mut self, color: Color) -> Self {
        self.highlight_color = color;
        self
    }

    /// Set the minimum word length
    pub fn with_min_length(mut self, length: usize) -> Self {
        self.min_word_length = length;
        self
    }

    /// Get highlights for word occurrences in the viewport
    ///
    /// # Arguments
    /// * `buffer` - The text buffer
    /// * `cursor_position` - Current cursor byte position
    /// * `viewport_start` - Start byte offset of visible viewport
    /// * `viewport_end` - End byte offset of visible viewport
    ///
    /// # Returns
    /// Vector of highlight spans for all occurrences of the word under cursor
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

        // Find the word under the cursor
        let word_range = match self.get_word_at_position(buffer, cursor_position) {
            Some(range) => range,
            None => return Vec::new(),
        };

        // Get the word text
        let word_bytes = buffer.slice_bytes(word_range.clone());
        let word = match std::str::from_utf8(&word_bytes) {
            Ok(s) => s.to_string(),
            Err(_) => return Vec::new(),
        };

        // Check minimum length
        if word.len() < self.min_word_length {
            return Vec::new();
        }

        // Find all occurrences in the viewport
        let occurrences = self.find_occurrences_in_range(buffer, &word, viewport_start, viewport_end);

        // Convert to highlight spans
        occurrences
            .into_iter()
            .map(|range| HighlightSpan {
                range,
                color: self.highlight_color,
            })
            .collect()
    }

    /// Get the word range at the given position
    ///
    /// Returns None if the cursor is not on a word character.
    fn get_word_at_position(&self, buffer: &Buffer, position: usize) -> Option<Range<usize>> {
        let buf_len = buffer.len();
        if position > buf_len {
            return None;
        }

        // Check if cursor is on a word character
        // Need to handle cursor at end of buffer
        let is_on_word = if position < buf_len {
            let byte_at_pos = buffer.slice_bytes(position..position + 1);
            byte_at_pos.first().map(|&b| is_word_char(b)).unwrap_or(false)
        } else if position > 0 {
            // Cursor at end of buffer - check previous character
            let byte_before = buffer.slice_bytes(position - 1..position);
            byte_before.first().map(|&b| is_word_char(b)).unwrap_or(false)
        } else {
            false
        };

        if !is_on_word && position > 0 {
            // Check if we're just after a word AND the cursor is at end of buffer
            // or the character before was a word char but current is not
            // This handles cursor positioned right after a word (e.g., at end of "foo|")
            let byte_before = buffer.slice_bytes(position.saturating_sub(1)..position);
            let is_after_word = byte_before.first().map(|&b| is_word_char(b)).unwrap_or(false);

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

    /// Find all whole-word occurrences of a word in a byte range
    fn find_occurrences_in_range(
        &self,
        buffer: &Buffer,
        word: &str,
        start: usize,
        end: usize,
    ) -> Vec<Range<usize>> {
        let mut occurrences = Vec::new();

        // Get the text in the range (with some padding for edge words)
        let search_start = start.saturating_sub(word.len());
        let search_end = (end + word.len()).min(buffer.len());

        let bytes = buffer.slice_bytes(search_start..search_end);
        let text = match std::str::from_utf8(&bytes) {
            Ok(s) => s,
            Err(_) => return occurrences,
        };

        // Find all occurrences
        let word_bytes = word.as_bytes();
        let mut search_pos = 0;

        while let Some(rel_pos) = text[search_pos..].find(word) {
            let abs_start = search_start + search_pos + rel_pos;
            let abs_end = abs_start + word.len();

            // Check if this is a whole word match (not part of a larger word)
            let is_word_start = abs_start == 0 || {
                let prev_byte = buffer.slice_bytes(abs_start - 1..abs_start);
                prev_byte.first().map(|&b| !is_word_char(b)).unwrap_or(true)
            };

            let is_word_end = abs_end >= buffer.len() || {
                let next_byte = buffer.slice_bytes(abs_end..abs_end + 1);
                next_byte.first().map(|&b| !is_word_char(b)).unwrap_or(true)
            };

            if is_word_start && is_word_end {
                // Only include if it overlaps with the actual viewport
                if abs_start < end && abs_end > start {
                    occurrences.push(abs_start..abs_end);
                }
            }

            search_pos += rel_pos + 1;
        }

        occurrences
    }
}

impl Default for SemanticHighlighter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_word_at_position() {
        let buffer = Buffer::from_str_test("hello world test");
        let highlighter = SemanticHighlighter::new();

        // Middle of "hello"
        let range = highlighter.get_word_at_position(&buffer, 2).unwrap();
        assert_eq!(range, 0..5);

        // Start of "world"
        let range = highlighter.get_word_at_position(&buffer, 6).unwrap();
        assert_eq!(range, 6..11);

        // On space (not a word)
        let range = highlighter.get_word_at_position(&buffer, 5);
        assert!(range.is_none());
    }

    #[test]
    fn test_find_occurrences() {
        let buffer = Buffer::from_str_test("foo bar foo baz foo");
        let highlighter = SemanticHighlighter::new();

        let occurrences = highlighter.find_occurrences_in_range(&buffer, "foo", 0, buffer.len());
        assert_eq!(occurrences.len(), 3);
        assert_eq!(occurrences[0], 0..3);
        assert_eq!(occurrences[1], 8..11);
        assert_eq!(occurrences[2], 16..19);
    }

    #[test]
    fn test_whole_word_only() {
        let buffer = Buffer::from_str_test("foobar foo foobaz");
        let highlighter = SemanticHighlighter::new();

        let occurrences = highlighter.find_occurrences_in_range(&buffer, "foo", 0, buffer.len());
        // Should only find the standalone "foo", not "foobar" or "foobaz"
        assert_eq!(occurrences.len(), 1);
        assert_eq!(occurrences[0], 7..10);
    }

    #[test]
    fn test_highlight_occurrences() {
        let buffer = Buffer::from_str_test("let foo = 1;\nlet bar = foo;\nlet baz = foo;");
        let highlighter = SemanticHighlighter::new();

        // Cursor on first 'foo' at position 4
        let spans = highlighter.highlight_occurrences(&buffer, 4, 0, buffer.len());

        // Should find 3 occurrences of 'foo'
        assert_eq!(spans.len(), 3);
    }

    #[test]
    fn test_min_word_length() {
        let buffer = Buffer::from_str_test("a b c a b c");
        let highlighter = SemanticHighlighter::new().with_min_length(2);

        // Single character 'a' at position 0 should not be highlighted
        let spans = highlighter.highlight_occurrences(&buffer, 0, 0, buffer.len());
        assert_eq!(spans.len(), 0);
    }

    #[test]
    fn test_disabled() {
        let buffer = Buffer::from_str_test("hello hello hello");
        let mut highlighter = SemanticHighlighter::new();
        highlighter.enabled = false;

        let spans = highlighter.highlight_occurrences(&buffer, 0, 0, buffer.len());
        assert_eq!(spans.len(), 0);
    }

    #[test]
    fn test_cursor_at_end_of_buffer() {
        let buffer = Buffer::from_str_test("foo bar foo");
        let highlighter = SemanticHighlighter::new();

        // Cursor at end of buffer (after last "foo")
        let spans = highlighter.highlight_occurrences(&buffer, buffer.len(), 0, buffer.len());
        // Should find both "foo" occurrences
        assert_eq!(spans.len(), 2);
    }

    #[test]
    fn test_cursor_on_word() {
        let buffer = Buffer::from_str_test("foo bar foo");
        let highlighter = SemanticHighlighter::new();

        // Cursor on first character of "foo"
        let spans = highlighter.highlight_occurrences(&buffer, 0, 0, buffer.len());
        // Should find both "foo" occurrences
        assert_eq!(spans.len(), 2);
    }

    #[test]
    fn test_viewport_limiting() {
        let buffer = Buffer::from_str_test("foo bar foo baz foo");
        let highlighter = SemanticHighlighter::new();

        // Only search in viewport 4..12 (should find middle "foo" only)
        let spans = highlighter.highlight_occurrences(&buffer, 8, 4, 12);
        assert_eq!(spans.len(), 1);
        assert_eq!(spans[0].range, 8..11);
    }
}
