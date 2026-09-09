//! Display width calculation for Unicode text
//!
//! This module provides utilities for calculating the visual display width
//! of characters and strings on a terminal. This is essential for proper
//! cursor positioning, line wrapping, and UI layout with CJK characters,
//! emoji, and other double-width or zero-width characters.

// `char_width` / `str_width` are the single source of truth in `fresh-core`,
// shared with the plugin runtime's `charWidth` / `stringWidth` APIs so plugins
// measure width exactly the way the editor lays out cells. The editor-specific
// byte/column helpers below build on them.
pub use fresh_core::display_width::{char_width, str_width};

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

/// Convert a visual column to a byte offset, resolving to the grapheme
/// cluster whose cell **contains** the column (click semantics).
///
/// Differs from [`byte_offset_at_visual_column`] in two ways, both of
/// which matter when mapping a mouse click to a caret position:
///
/// * It advances one **grapheme cluster** at a time (measuring each with
///   [`str_width`], the same measure the renderer uses), so the returned
///   offset is always on a cluster boundary — a click never lands between
///   the codepoints of an emoji ZWJ sequence, a flag, or a Thai
///   combining cluster.
/// * It returns the cluster *containing* `visual_col` rather than the
///   first cluster starting at or after it, so clicking the right half of
///   a double-width glyph selects that glyph, not the next one.
///
/// Past the end of the string, returns the string's length.
#[inline]
pub fn grapheme_byte_at_visual_column(s: &str, visual_col: usize) -> usize {
    use unicode_segmentation::UnicodeSegmentation;
    let mut byte = 0;
    let mut col = 0usize;
    for cluster in s.graphemes(true) {
        let w = str_width(cluster);
        if col + w > visual_col {
            return byte;
        }
        col += w;
        byte += cluster.len();
    }
    byte
}

/// Visual column of a byte offset in its line, wide-char aware.
///
/// Returns `None` when the offset's line can't be resolved. The offset may
/// sit anywhere in the line, including on the line ending (which yields the
/// width of the full line content).
pub fn visual_column_of(buffer: &crate::model::buffer::Buffer, offset: usize) -> Option<usize> {
    let line = buffer.get_line_number(offset);
    let line_start = buffer.line_start_offset(line)?;
    let content = buffer.get_line(line)?;
    let text = String::from_utf8_lossy(&content);
    Some(visual_column_at_byte(
        &text,
        offset.saturating_sub(line_start),
    ))
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
    fn test_grapheme_byte_at_visual_column() {
        // ASCII: column maps to byte one-to-one; past end clamps to len.
        assert_eq!(grapheme_byte_at_visual_column("abcdef", 0), 0);
        assert_eq!(grapheme_byte_at_visual_column("abcdef", 3), 3);
        assert_eq!(grapheme_byte_at_visual_column("abcdef", 99), 6);

        // Wide CJK: each ideograph is 3 bytes, 2 columns. Clicking either
        // half of 中 resolves to its start (contains-semantics).
        let s = "中文x"; // 中(0..3) 文(3..6) x(6)
        assert_eq!(grapheme_byte_at_visual_column(s, 0), 0);
        assert_eq!(grapheme_byte_at_visual_column(s, 1), 0); // right half of 中
        assert_eq!(grapheme_byte_at_visual_column(s, 2), 3); // start of 文
        assert_eq!(grapheme_byte_at_visual_column(s, 4), 6); // the 'x'

        // Multi-codepoint clusters never split: decomposed "é" (e + U+0301,
        // 3 bytes, 1 column) and a 9-byte Thai cluster.
        assert_eq!(grapheme_byte_at_visual_column("e\u{301}z", 1), 3);
        assert_eq!(grapheme_byte_at_visual_column("ที่z", 1), 9);
    }

    #[test]
    fn test_display_width_trait() {
        let s = "你好";
        assert_eq!(s.display_width(), 4);

        let string = String::from("Hello🚀");
        assert_eq!(string.display_width(), 7);
    }
}
