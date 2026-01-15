//! Unified visual layout calculations for text display
//!
//! This module provides consistent handling of visual column calculations
//! across all editor operations: rendering, mouse clicks, and cursor navigation.
//!
//! Key concepts:
//! - **Character index**: Position in the character sequence (0, 1, 2, ...)
//! - **Visual column**: Screen column position accounting for char widths
//! - **Source byte**: Byte offset in the source buffer
//!
//! Handles:
//! - ANSI escape sequences (zero visual width)
//! - Double-width characters (CJK, emoji)
//! - Tab expansion
//! - Zero-width Unicode characters

use crate::primitives::ansi::AnsiParser;
use crate::primitives::display_width::char_width;

/// Standard tab width for terminal display
pub const TAB_WIDTH: usize = 8;

/// Calculate tab expansion width at a given column
#[inline]
pub fn tab_expansion_width(col: usize) -> usize {
    TAB_WIDTH - (col % TAB_WIDTH)
}

/// Per-line mappings that support all visual layout operations with O(1) lookups
#[derive(Debug, Clone, Default)]
pub struct LineMappings {
    /// Source byte for each character (indexed by char position)
    /// Length == number of characters in processed text
    pub char_source_bytes: Vec<Option<usize>>,

    /// Visual column for each character (indexed by char position)
    /// For zero-width chars, this is the same as the previous char's visual column
    pub char_visual_cols: Vec<usize>,

    /// Character index at each visual column (indexed by visual column)
    /// For double-width chars, consecutive visual columns map to the same char
    /// Length == total visual width of line
    pub visual_to_char: Vec<usize>,

    /// Total visual width of the line
    pub total_visual_width: usize,
}

impl LineMappings {
    /// Get source byte for a character at the given index
    #[inline]
    pub fn source_byte_at_char(&self, char_idx: usize) -> Option<usize> {
        self.char_source_bytes.get(char_idx).copied().flatten()
    }

    /// Get visual column for a character at the given index
    #[inline]
    pub fn visual_col_at_char(&self, char_idx: usize) -> usize {
        self.char_visual_cols.get(char_idx).copied().unwrap_or(0)
    }

    /// Get character index at a given visual column (O(1) for mouse clicks)
    #[inline]
    pub fn char_at_visual_col(&self, visual_col: usize) -> usize {
        self.visual_to_char
            .get(visual_col)
            .copied()
            .unwrap_or_else(|| {
                // Past end of line - return last char index
                self.char_source_bytes.len().saturating_sub(1)
            })
    }

    /// Get source byte at a given visual column (O(1) for mouse clicks)
    #[inline]
    pub fn source_byte_at_visual_col(&self, visual_col: usize) -> Option<usize> {
        let char_idx = self.char_at_visual_col(visual_col);
        self.source_byte_at_char(char_idx)
    }

    /// Get the source byte at the end of the line
    #[inline]
    pub fn line_end_byte(&self) -> usize {
        self.char_source_bytes
            .iter()
            .rev()
            .find_map(|&b| b)
            .map(|b| b + 1) // One past last char
            .unwrap_or(0)
    }
}

/// Builder for constructing LineMappings incrementally
#[derive(Debug)]
pub struct LineMappingsBuilder {
    mappings: LineMappings,
    current_visual_col: usize,
    ansi_parser: Option<AnsiParser>,
}

impl LineMappingsBuilder {
    /// Create a new builder, optionally with ANSI parsing enabled
    pub fn new(has_ansi: bool) -> Self {
        Self {
            mappings: LineMappings::default(),
            current_visual_col: 0,
            ansi_parser: if has_ansi {
                Some(AnsiParser::new())
            } else {
                None
            },
        }
    }

    /// Add a character to the mappings
    ///
    /// Returns the visual width of the character (0 for ANSI/zero-width, 1-2 for visible chars)
    pub fn add_char(&mut self, ch: char, source_byte: Option<usize>) -> usize {
        // Check if this is part of an ANSI escape sequence
        if let Some(ref mut parser) = self.ansi_parser {
            if parser.parse_char(ch).is_none() {
                // ANSI escape character - zero visual width
                let _char_idx = self.mappings.char_source_bytes.len();
                self.mappings.char_source_bytes.push(source_byte);
                self.mappings.char_visual_cols.push(self.current_visual_col);
                // No entry in visual_to_char for zero-width chars
                return 0;
            }
        }

        // Regular character (possibly zero-width Unicode)
        let width = if ch == '\t' {
            tab_expansion_width(self.current_visual_col)
        } else {
            char_width(ch)
        };

        let char_idx = self.mappings.char_source_bytes.len();
        self.mappings.char_source_bytes.push(source_byte);
        self.mappings.char_visual_cols.push(self.current_visual_col);

        // Add visual column entries for this character
        for _ in 0..width {
            self.mappings.visual_to_char.push(char_idx);
        }

        self.current_visual_col += width;
        width
    }

    /// Add a tab character with custom expansion
    pub fn add_tab(&mut self, source_byte: Option<usize>) -> usize {
        let width = tab_expansion_width(self.current_visual_col);
        let char_idx = self.mappings.char_source_bytes.len();

        self.mappings.char_source_bytes.push(source_byte);
        self.mappings.char_visual_cols.push(self.current_visual_col);

        for _ in 0..width {
            self.mappings.visual_to_char.push(char_idx);
        }

        self.current_visual_col += width;
        width
    }

    /// Get the current visual column
    pub fn current_visual_col(&self) -> usize {
        self.current_visual_col
    }

    /// Finish building and return the mappings
    pub fn finish(mut self) -> LineMappings {
        self.mappings.total_visual_width = self.current_visual_col;
        self.mappings
    }
}

/// Calculate visual width of a string, handling ANSI escapes and tabs
///
/// This is the canonical function for visual width calculation.
/// Use this instead of `str_width()` when the text may contain ANSI codes or tabs.
pub fn visual_width(s: &str, start_col: usize) -> usize {
    if !s.contains('\x1b') && !s.contains('\t') {
        // Fast path: no special handling needed
        return crate::primitives::display_width::str_width(s);
    }

    let mut col = start_col;
    let mut parser = AnsiParser::new();

    for ch in s.chars() {
        if parser.parse_char(ch).is_none() {
            continue; // ANSI escape char, skip
        }
        if ch == '\t' {
            col += tab_expansion_width(col);
        } else {
            col += char_width(ch);
        }
    }

    col - start_col
}

/// Convert byte offset to visual column (ANSI-aware, tab-aware)
///
/// Given a byte offset within the string, returns the visual column at that position.
pub fn byte_to_visual_col(s: &str, byte_offset: usize) -> usize {
    let clamped_offset = byte_offset.min(s.len());

    if !s.contains('\x1b') && !s.contains('\t') {
        // Fast path: just calculate width of the prefix
        return crate::primitives::display_width::str_width(&s[..clamped_offset]);
    }

    let mut col = 0;
    let mut current_byte = 0;
    let mut parser = AnsiParser::new();

    for ch in s.chars() {
        if current_byte >= clamped_offset {
            break;
        }

        if parser.parse_char(ch).is_some() {
            // Visible character
            if ch == '\t' {
                col += tab_expansion_width(col);
            } else {
                col += char_width(ch);
            }
        }
        // ANSI chars don't add to visual column

        current_byte += ch.len_utf8();
    }

    col
}

/// Convert visual column to byte offset (ANSI-aware, tab-aware)
///
/// Given a visual column, returns the byte offset of the character at or after that column.
/// If the visual column is beyond the string's width, returns the string's length.
pub fn visual_col_to_byte(s: &str, target_visual_col: usize) -> usize {
    if !s.contains('\x1b') && !s.contains('\t') {
        // Fast path: use simple character iteration (no ANSI, no tabs)
        let mut col = 0;
        for (byte_idx, ch) in s.char_indices() {
            let width = char_width(ch);
            // Check if target falls within this character's visual range [col, col+width)
            if target_visual_col < col + width {
                return byte_idx;
            }
            col += width;
        }
        return s.len();
    }

    let mut col = 0;
    let mut parser = AnsiParser::new();

    for (byte_idx, ch) in s.char_indices() {
        if parser.parse_char(ch).is_some() {
            // Visible character - check if target falls within this char's range
            let width = if ch == '\t' {
                tab_expansion_width(col)
            } else {
                char_width(ch)
            };

            // Target is within [col, col+width) range of this character
            if target_visual_col < col + width {
                return byte_idx;
            }

            col += width;
        }
        // ANSI chars: don't add to visual column, don't match target
    }

    s.len()
}

/// Build complete line mappings from text and source byte information
///
/// This is used when constructing ViewLine during token processing.
pub fn build_line_mappings(
    text: &str,
    source_bytes: impl Iterator<Item = Option<usize>>,
    has_ansi: bool,
) -> LineMappings {
    let mut builder = LineMappingsBuilder::new(has_ansi);
    let mut source_iter = source_bytes;

    for ch in text.chars() {
        let source_byte = source_iter.next().flatten();
        builder.add_char(ch, source_byte);
    }

    builder.finish()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_visual_width_ascii() {
        assert_eq!(visual_width("Hello", 0), 5);
        assert_eq!(visual_width("", 0), 0);
    }

    #[test]
    fn test_visual_width_with_tabs() {
        // Tab at column 0 expands to 8 spaces
        assert_eq!(visual_width("\t", 0), 8);
        // Tab at column 4 expands to 4 spaces
        assert_eq!(visual_width("1234\t", 0), 8);
        // "12" (2) + tab (6 to reach 8) = 8
        assert_eq!(visual_width("12\t", 0), 8);
    }

    #[test]
    fn test_visual_width_with_ansi() {
        // ANSI escape sequences should have zero width
        assert_eq!(visual_width("\x1b[31mRed\x1b[0m", 0), 3);
        assert_eq!(visual_width("\x1b[1;31;4mBold\x1b[0m", 0), 4);
    }

    #[test]
    fn test_visual_width_cjk() {
        // CJK characters are 2 columns each
        assert_eq!(visual_width("你好", 0), 4);
        assert_eq!(visual_width("Hello你好", 0), 9);
    }

    #[test]
    fn test_byte_to_visual_col_simple() {
        let s = "Hello";
        assert_eq!(byte_to_visual_col(s, 0), 0);
        assert_eq!(byte_to_visual_col(s, 1), 1);
        assert_eq!(byte_to_visual_col(s, 5), 5);
    }

    #[test]
    fn test_byte_to_visual_col_with_ansi() {
        // "\x1b[31m" is 5 bytes, "Red" is 3 bytes
        let s = "\x1b[31mRed";
        assert_eq!(byte_to_visual_col(s, 0), 0); // At ESC
        assert_eq!(byte_to_visual_col(s, 5), 0); // At 'R' (ANSI prefix has 0 width)
        assert_eq!(byte_to_visual_col(s, 6), 1); // At 'e'
        assert_eq!(byte_to_visual_col(s, 8), 3); // Past end
    }

    #[test]
    fn test_byte_to_visual_col_with_cjk() {
        // "你" is 3 bytes and 2 columns
        let s = "a你b";
        assert_eq!(byte_to_visual_col(s, 0), 0); // 'a'
        assert_eq!(byte_to_visual_col(s, 1), 1); // '你' start
        assert_eq!(byte_to_visual_col(s, 4), 3); // 'b'
    }

    #[test]
    fn test_visual_col_to_byte_simple() {
        let s = "Hello";
        assert_eq!(visual_col_to_byte(s, 0), 0);
        assert_eq!(visual_col_to_byte(s, 3), 3);
        assert_eq!(visual_col_to_byte(s, 5), 5);
        assert_eq!(visual_col_to_byte(s, 10), 5); // Past end
    }

    #[test]
    fn test_visual_col_to_byte_with_ansi() {
        // "\x1b[31m" is 5 bytes, "Red" is 3 bytes
        let s = "\x1b[31mRed";
        assert_eq!(visual_col_to_byte(s, 0), 5); // Visual col 0 = 'R' at byte 5
        assert_eq!(visual_col_to_byte(s, 1), 6); // Visual col 1 = 'e' at byte 6
        assert_eq!(visual_col_to_byte(s, 3), 8); // Past end
    }

    #[test]
    fn test_visual_col_to_byte_with_cjk() {
        // "a你b" - 'a' at 0, '你' at 1-3, 'b' at 4
        let s = "a你b";
        assert_eq!(visual_col_to_byte(s, 0), 0); // 'a'
        assert_eq!(visual_col_to_byte(s, 1), 1); // '你' (both cols 1 and 2 map to byte 1)
        assert_eq!(visual_col_to_byte(s, 2), 1); // Still '你'
        assert_eq!(visual_col_to_byte(s, 3), 4); // 'b'
    }

    #[test]
    fn test_line_mappings_builder_simple() {
        let mut builder = LineMappingsBuilder::new(false);
        builder.add_char('H', Some(0));
        builder.add_char('i', Some(1));

        let mappings = builder.finish();

        assert_eq!(mappings.char_source_bytes.len(), 2);
        assert_eq!(mappings.visual_to_char.len(), 2);
        assert_eq!(mappings.source_byte_at_char(0), Some(0));
        assert_eq!(mappings.source_byte_at_char(1), Some(1));
        assert_eq!(mappings.char_at_visual_col(0), 0);
        assert_eq!(mappings.char_at_visual_col(1), 1);
    }

    #[test]
    fn test_line_mappings_builder_with_cjk() {
        let mut builder = LineMappingsBuilder::new(false);
        builder.add_char('a', Some(0)); // 1 column
        builder.add_char('你', Some(1)); // 2 columns
        builder.add_char('b', Some(4)); // 1 column

        let mappings = builder.finish();

        assert_eq!(mappings.char_source_bytes.len(), 3);
        assert_eq!(mappings.visual_to_char.len(), 4); // 1 + 2 + 1

        // Click on visual col 0 -> char 0 ('a')
        assert_eq!(mappings.source_byte_at_visual_col(0), Some(0));
        // Click on visual col 1 -> char 1 ('你')
        assert_eq!(mappings.source_byte_at_visual_col(1), Some(1));
        // Click on visual col 2 -> still char 1 ('你')
        assert_eq!(mappings.source_byte_at_visual_col(2), Some(1));
        // Click on visual col 3 -> char 2 ('b')
        assert_eq!(mappings.source_byte_at_visual_col(3), Some(4));
    }

    #[test]
    fn test_line_mappings_builder_with_ansi() {
        let mut builder = LineMappingsBuilder::new(true);

        // Simulate "\x1b[31mA" - ANSI prefix (5 chars) + 'A'
        builder.add_char('\x1b', Some(0));
        builder.add_char('[', Some(1));
        builder.add_char('3', Some(2));
        builder.add_char('1', Some(3));
        builder.add_char('m', Some(4));
        builder.add_char('A', Some(5));

        let mappings = builder.finish();

        // 6 characters total
        assert_eq!(mappings.char_source_bytes.len(), 6);
        // But only 1 visual column (only 'A' is visible)
        assert_eq!(mappings.visual_to_char.len(), 1);
        assert_eq!(mappings.total_visual_width, 1);

        // All chars have correct source bytes
        assert_eq!(mappings.source_byte_at_char(0), Some(0)); // ESC
        assert_eq!(mappings.source_byte_at_char(5), Some(5)); // 'A'

        // Visual col 0 maps to char 5 ('A')
        assert_eq!(mappings.char_at_visual_col(0), 5);
        assert_eq!(mappings.source_byte_at_visual_col(0), Some(5));
    }

    #[test]
    fn test_line_mappings_cursor_on_ansi() {
        let mut builder = LineMappingsBuilder::new(true);

        // "\x1b[31mHi" - cursor at byte 0 (ESC) should work
        builder.add_char('\x1b', Some(0));
        builder.add_char('[', Some(1));
        builder.add_char('3', Some(2));
        builder.add_char('1', Some(3));
        builder.add_char('m', Some(4));
        builder.add_char('H', Some(5));
        builder.add_char('i', Some(6));

        let mappings = builder.finish();

        // Can look up source byte for any char, including ANSI
        assert_eq!(mappings.source_byte_at_char(0), Some(0)); // ESC at byte 0
        assert_eq!(mappings.source_byte_at_char(1), Some(1)); // '[' at byte 1

        // Visual column of ANSI chars is 0 (same as where 'H' will be displayed)
        assert_eq!(mappings.visual_col_at_char(0), 0);
        assert_eq!(mappings.visual_col_at_char(4), 0);
        assert_eq!(mappings.visual_col_at_char(5), 0); // 'H'
        assert_eq!(mappings.visual_col_at_char(6), 1); // 'i'
    }
}
