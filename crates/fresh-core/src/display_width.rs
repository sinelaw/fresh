//! Display width calculation for Unicode text.
//!
//! The single source of truth for "how many terminal columns does this text
//! occupy", backed by the `unicode-width` crate. Used for cursor positioning,
//! line wrapping, and UI layout with CJK characters, emoji, and other
//! double-width or zero-width characters.
//!
//! This lives in `fresh-core` so that both the editor (layout/rendering) and
//! the plugin runtime (the `charWidth` / `stringWidth` plugin APIs) compute
//! width with the *same* logic — plugins must not re-derive their own width
//! tables, or their measurements drift from how the editor actually lays out
//! cells.

use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

/// Display width of a single character, in terminal columns.
///
/// Returns 0 for control and zero-width characters, 2 for CJK/fullwidth
/// characters and most emoji, and 1 for everything else.
#[inline]
pub fn char_width(c: char) -> usize {
    // unicode_width returns None for control characters.
    c.width().unwrap_or(0)
}

/// Display width of a string, in terminal columns (the sum of its characters'
/// widths). Use this instead of `.chars().count()` for visual layout.
#[inline]
pub fn str_width(s: &str) -> usize {
    s.width()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ascii() {
        assert_eq!(str_width("Hello"), 5);
        assert_eq!(str_width(""), 0);
        assert_eq!(char_width('a'), 1);
    }

    #[test]
    fn cjk_and_emoji_are_two_columns() {
        assert_eq!(char_width('你'), 2);
        assert_eq!(char_width('🚀'), 2);
        assert_eq!(str_width("你好"), 4);
        assert_eq!(str_width("Hi🚀"), 4);
    }

    #[test]
    fn control_and_zero_width_are_zero() {
        assert_eq!(char_width('\0'), 0);
        assert_eq!(char_width('\t'), 0);
        assert_eq!(char_width('\u{200B}'), 0); // zero-width space
    }
}
