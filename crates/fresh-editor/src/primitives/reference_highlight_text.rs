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
        let occurrences =
            self.find_occurrences_in_range(buffer, &word, viewport_start, viewport_end);

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
    fn get_word_at_position(&self, buffer: &Buffer, position: usize) -> Option<Range<usize>> {
        let buf_len = buffer.len();
        if position > buf_len {
            return None;
        }

        // Check if cursor is on a word character
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
            // Check if we're just after a word at end of buffer
            let byte_before = buffer.slice_bytes(position.saturating_sub(1)..position);
            let is_after_word = byte_before
                .first()
                .map(|&b| is_word_char(b))
                .unwrap_or(false);

            if is_after_word && position >= buf_len {
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

    /// Maximum search range (1MB) to avoid performance issues
    const MAX_SEARCH_RANGE: usize = 1024 * 1024;

    /// Find all whole-word occurrences in a byte range
    fn find_occurrences_in_range(
        &self,
        buffer: &Buffer,
        word: &str,
        start: usize,
        end: usize,
    ) -> Vec<Range<usize>> {
        // Skip if search range is too large
        if end.saturating_sub(start) > Self::MAX_SEARCH_RANGE {
            return Vec::new();
        }

        let mut occurrences = Vec::new();

        // Get the text with padding for edge words
        let search_start = start.saturating_sub(word.len());
        let search_end = (end + word.len()).min(buffer.len());

        let bytes = buffer.slice_bytes(search_start..search_end);
        let text = match std::str::from_utf8(&bytes) {
            Ok(s) => s,
            Err(_) => return occurrences,
        };

        // Use match_indices for efficient single-pass searching
        for (rel_pos, _) in text.match_indices(word) {
            let abs_start = search_start + rel_pos;
            let abs_end = abs_start + word.len();

            // Check if this is a whole word match
            let is_word_start = abs_start == 0 || {
                let prev_byte = buffer.slice_bytes(abs_start - 1..abs_start);
                prev_byte.first().map(|&b| !is_word_char(b)).unwrap_or(true)
            };

            let is_word_end = abs_end >= buffer.len() || {
                let next_byte = buffer.slice_bytes(abs_end..abs_end + 1);
                next_byte.first().map(|&b| !is_word_char(b)).unwrap_or(true)
            };

            if is_word_start && is_word_end {
                // Only include if within viewport
                if abs_start < end && abs_end > start {
                    occurrences.push(abs_start..abs_end);
                }
            }
        }

        occurrences
    }
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
