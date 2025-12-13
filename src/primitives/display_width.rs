//! Display width calculation for Unicode text
//!
//! This module provides utilities for calculating the visual display width
//! of characters and strings on a terminal. This is essential for proper
//! cursor positioning, line wrapping, and UI layout with CJK characters,
//! emoji, and other double-width or zero-width characters.

use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

/// Calculate the display width of a single character.
///
/// Returns 0 for control characters and zero-width characters,
/// 2 for CJK/fullwidth characters and emoji,
/// 1 for most other characters.
#[inline]
pub fn char_width(c: char) -> usize {
    // unicode_width returns None for control characters
    c.width().unwrap_or(0)
}

/// Calculate the display width of a string.
///
/// This is the sum of display widths of all characters in the string.
/// Use this instead of `.chars().count()` when calculating visual layout.
#[inline]
pub fn str_width(s: &str) -> usize {
    s.width()
}

/// Extension trait for convenient width calculation on string types.
pub trait DisplayWidth {
    /// Returns the display width (number of terminal columns) of this string.
    fn display_width(&self) -> usize;
}

impl DisplayWidth for str {
    #[inline]
    fn display_width(&self) -> usize {
        str_width(self)
    }
}

impl DisplayWidth for String {
    #[inline]
    fn display_width(&self) -> usize {
        str_width(self)
    }
}

/// Calculate the visual column (display width) at a given byte offset within a string.
///
/// Returns the sum of display widths of all characters before the given byte offset.
#[inline]
pub fn visual_column_at_byte(s: &str, byte_offset: usize) -> usize {
    s[..byte_offset.min(s.len())].chars().map(char_width).sum()
}

/// Convert a visual column to a byte offset within a string.
///
/// Returns the byte offset of the character that starts at or after the given visual column.
/// If the visual column is beyond the string's width, returns the string's length.
/// This ensures the result is always at a valid UTF-8 character boundary.
#[inline]
pub fn byte_offset_at_visual_column(s: &str, visual_col: usize) -> usize {
    let mut current_col = 0;
    for (byte_idx, ch) in s.char_indices() {
        if current_col >= visual_col {
            return byte_idx;
        }
        current_col += char_width(ch);
    }
    s.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ascii_width() {
        assert_eq!(str_width("Hello"), 5);
        assert_eq!(str_width(""), 0);
        assert_eq!(str_width(" "), 1);
    }

    #[test]
    fn test_cjk_width() {
        // Chinese characters are 2 columns each
        assert_eq!(str_width("你好"), 4);
        assert_eq!(str_width("你好世界"), 8);

        // Japanese
        assert_eq!(str_width("月"), 2);
        assert_eq!(str_width("日本"), 4);

        // Korean
        assert_eq!(str_width("한글"), 4);
    }

    #[test]
    fn test_emoji_width() {
        // Most emoji are 2 columns
        assert_eq!(str_width("🚀"), 2);
        assert_eq!(str_width("🎉"), 2);
        assert_eq!(str_width("🚀🎉"), 4);
    }

    #[test]
    fn test_mixed_width() {
        // ASCII + CJK
        assert_eq!(str_width("Hello你好"), 5 + 4);
        assert_eq!(str_width("a你b"), 1 + 2 + 1);

        // ASCII + emoji
        assert_eq!(str_width("Hi🚀"), 2 + 2);
    }

    #[test]
    fn test_char_width() {
        assert_eq!(char_width('a'), 1);
        assert_eq!(char_width('你'), 2);
        assert_eq!(char_width('🚀'), 2);
    }

    #[test]
    fn test_zero_width() {
        // Control characters
        assert_eq!(char_width('\0'), 0);
        assert_eq!(char_width('\t'), 0); // Tab is control char, terminal handles it specially

        // Zero-width space
        assert_eq!(char_width('\u{200B}'), 0);
    }

    #[test]
    fn test_display_width_trait() {
        let s = "你好";
        assert_eq!(s.display_width(), 4);

        let string = String::from("Hello🚀");
        assert_eq!(string.display_width(), 7);
    }
}
