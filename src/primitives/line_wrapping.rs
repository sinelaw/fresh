//! Clean line wrapping implementation
//!
//! Pipeline: Line text → Wrapping transformation → Wrapped line segments → Rendering/Cursor
//!
//! This module provides a single source of truth for how lines wrap,
//! ensuring rendering and cursor positioning always agree.

use crate::primitives::display_width::char_width;

/// Represents a single wrapped segment of a logical line
#[derive(Debug, Clone)]
pub struct WrappedSegment {
    /// The text content of this segment (without gutter spaces)
    pub text: String,
    /// Whether this is a continuation of a previous segment (not the first segment)
    pub is_continuation: bool,
    /// Start position of this segment in the original line (character offset, not byte offset)
    pub start_char_offset: usize,
    /// End position of this segment in the original line (character offset, not byte offset)
    pub end_char_offset: usize,
}

/// Configuration for line wrapping
#[derive(Debug, Clone)]
pub struct WrapConfig {
    /// Width available for text on the first line (terminal_width - scrollbar - gutter)
    pub first_line_width: usize,
    /// Width available for text on continuation lines (first_line_width - gutter_indent)
    pub continuation_line_width: usize,
    /// Width of gutter (for continuation line indentation)
    pub gutter_width: usize,
}

impl WrapConfig {
    /// Create a new wrap configuration
    ///
    /// # Arguments
    /// * `content_area_width` - Width of the content area (after UI elements like tabs/status bar, but including scrollbar and gutter)
    /// * `gutter_width` - Width of the line number gutter
    /// * `has_scrollbar` - Whether to reserve a column for scrollbar
    pub fn new(content_area_width: usize, gutter_width: usize, has_scrollbar: bool) -> Self {
        let scrollbar_width = if has_scrollbar { 1 } else { 0 };
        // Calculate the width available for text content
        // Both first line and continuation lines have the same text width
        // (continuation lines just have visual indentation, not less text space)
        let text_area_width = content_area_width
            .saturating_sub(scrollbar_width)
            .saturating_sub(gutter_width);

        Self {
            first_line_width: text_area_width,
            continuation_line_width: text_area_width, // Same width, not reduced!
            gutter_width,
        }
    }

    /// Create a "no wrap" configuration (infinite width)
    /// This treats the line as having unlimited width, so it never wraps
    pub fn no_wrap(gutter_width: usize) -> Self {
        Self {
            first_line_width: usize::MAX,
            continuation_line_width: usize::MAX,
            gutter_width,
        }
    }
}

/// Wrap a single line of text into segments
///
/// This is the core wrapping transformation. It takes raw text and produces
/// a list of wrapped segments that both rendering and cursor positioning can use.
///
/// # Arguments
/// * `text` - The line text to wrap
/// * `config` - Wrapping configuration
///
/// # Returns
/// A vector of WrappedSegment, one per visual line
pub fn wrap_line(text: &str, config: &WrapConfig) -> Vec<WrappedSegment> {
    let mut segments = Vec::new();

    if text.is_empty() {
        // Empty line = one empty segment
        return vec![WrappedSegment {
            text: String::new(),
            is_continuation: false,
            start_char_offset: 0,
            end_char_offset: 0,
        }];
    }

    let chars: Vec<char> = text.chars().collect();
    let mut pos = 0; // Position in chars array
    let mut is_first = true;

    while pos < chars.len() {
        let width = if is_first {
            config.first_line_width
        } else {
            config.continuation_line_width
        };

        // Track where this segment starts in the original text
        let segment_start_char = pos;

        // If we only had whitespace and nothing else, we're done
        if pos >= chars.len() {
            break;
        }

        // Take characters until we reach the visual width limit
        let mut segment_visual_width = 0;
        let segment_text_start = pos;

        while pos < chars.len() {
            let c = chars[pos];
            let c_width = char_width(c);

            // Check if adding this character would exceed the width
            // (but always include at least one character per segment to avoid infinite loops)
            if segment_visual_width + c_width > width && pos > segment_text_start {
                break;
            }

            segment_visual_width += c_width;
            pos += 1;
        }

        // Extract the text for this segment
        let segment_text: String = chars[segment_text_start..pos].iter().collect();

        segments.push(WrappedSegment {
            text: segment_text,
            is_continuation: !is_first,
            start_char_offset: segment_start_char,
            end_char_offset: pos,
        });

        is_first = false;
    }

    // Always return at least one segment
    if segments.is_empty() {
        segments.push(WrappedSegment {
            text: String::new(),
            is_continuation: false,
            start_char_offset: 0,
            end_char_offset: 0,
        });
    }

    segments
}

/// Given a character position within a line, find which wrapped segment it's in
/// and the position within that segment
///
/// # Arguments
/// * `char_pos` - Character position in the ORIGINAL text (including skipped whitespace)
/// * `segments` - The wrapped segments
///
/// # Returns
/// (segment_index, column_in_segment)
pub fn char_position_to_segment(char_pos: usize, segments: &[WrappedSegment]) -> (usize, usize) {
    if segments.is_empty() {
        return (0, 0);
    }

    // Find which segment contains this position by checking the char offset ranges
    for (seg_idx, segment) in segments.iter().enumerate() {
        // Check if char_pos falls within this segment's range in the original text
        if char_pos >= segment.start_char_offset && char_pos < segment.end_char_offset {
            // Position is in this segment
            // Calculate the column within the segment's text
            // Note: segment.text may be shorter than the range if we skipped whitespace
            let offset_in_range = char_pos - segment.start_char_offset;

            // Find how much whitespace was skipped at the start of this segment
            let segment_text_len = segment.text.chars().count();
            let range_len = segment.end_char_offset - segment.start_char_offset;
            let whitespace_skipped = range_len - segment_text_len;

            // The column is the position minus the skipped whitespace
            let col = offset_in_range.saturating_sub(whitespace_skipped);
            return (seg_idx, col);
        }
    }

    // Position is at or past the end - put it at the end of last segment
    let last_idx = segments.len() - 1;
    let last_len = segments[last_idx].text.chars().count();
    (last_idx, last_len)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_wrap_empty_line() {
        let config = WrapConfig::new(60, 8, true);
        let segments = wrap_line("", &config);

        assert_eq!(segments.len(), 1);
        assert_eq!(segments[0].text, "");
        assert!(!segments[0].is_continuation);
    }

    #[test]
    fn test_wrap_short_line() {
        let config = WrapConfig::new(60, 8, true);
        let text = "Hello world";
        let segments = wrap_line(text, &config);

        assert_eq!(segments.len(), 1);
        assert_eq!(segments[0].text, text);
        assert!(!segments[0].is_continuation);
    }

    #[test]
    fn test_wrap_long_line() {
        // Terminal: 60 cols, Gutter: 8, Scrollbar: 1
        // Available width: 60 - 1 (scrollbar) - 8 (gutter) = 51 chars
        // BOTH first line and continuation lines: 51 chars (same width!)
        let config = WrapConfig::new(60, 8, true);

        let text = "A fast, lightweight terminal text editor written in Rust. Handles files of any size with instant startup, low memory usage, and modern IDE features.";
        let segments = wrap_line(text, &config);

        // Expected segments based on 51 character width (both first and continuation):
        const SEG0: &str = "A fast, lightweight terminal text editor written in";
        const SEG1: &str = " Rust. Handles files of any size with instant start";
        const SEG2: &str = "up, low memory usage, and modern IDE features.";

        assert_eq!(segments.len(), 3);

        assert_eq!(segments[0].text, SEG0);
        assert!(!segments[0].is_continuation);

        assert_eq!(segments[1].text, SEG1);
        assert!(segments[1].is_continuation);

        assert_eq!(segments[2].text, SEG2);
        assert!(segments[2].is_continuation);

        // Test char_position_to_segment with various positions

        // Position 0 (start of text) -> segment 0, column 0
        assert_eq!(char_position_to_segment(0, &segments), (0, 0));

        // Position in middle of first segment
        assert_eq!(char_position_to_segment(25, &segments), (0, 25));

        // Position at end of first segment (where 'H' is)
        assert_eq!(
            char_position_to_segment(SEG0.chars().count() - 1, &segments),
            (0, SEG0.chars().count() - 1)
        );

        // Position at start of second segment (where 'a' in "andles" is)
        assert_eq!(
            char_position_to_segment(SEG0.chars().count(), &segments),
            (1, 0)
        );

        // Position in middle of second segment
        let pos_in_seg1 = SEG0.chars().count() + 30;
        assert_eq!(char_position_to_segment(pos_in_seg1, &segments), (1, 30));

        // Position at start of third segment
        let seg2_start = SEG0.chars().count() + SEG1.chars().count();
        assert_eq!(char_position_to_segment(seg2_start, &segments), (2, 0));

        // Position at end of text (in third segment)
        let text_len = text.chars().count();
        assert_eq!(
            char_position_to_segment(text_len, &segments),
            (2, SEG2.chars().count())
        );

        // Position beyond end of text
        assert_eq!(
            char_position_to_segment(text_len + 10, &segments),
            (2, SEG2.chars().count())
        );
    }

    #[test]
    fn test_wrap_with_leading_space() {
        let config = WrapConfig::new(60, 8, true);
        // With our config: 60 - 1 (scrollbar) - 8 (gutter) = 51 chars per line

        // Create text that wraps such that continuation starts with space
        let text = format!("{} {}", "A".repeat(51), "B".repeat(50));
        let segments = wrap_line(&text, &config);

        println!("segments: {:?}", segments);
        assert_eq!(segments.len(), 2);
        assert_eq!(
            segments[0].text.chars().count(),
            51,
            "First segment should be 51 chars"
        );
        assert!(segments[1].is_continuation);

        // Second segment starts with space, then B's (51 chars total)
        assert_eq!(
            segments[1].text.chars().count(),
            51,
            "Continuation should also be 51 chars"
        );
    }

    #[test]
    fn test_wrap_exact_width() {
        let config = WrapConfig::new(60, 8, true);
        println!(
            "Config: first={}, cont={}",
            config.first_line_width, config.continuation_line_width
        );

        // Create text that's longer than one line (2 full lines worth)
        let text = "A".repeat(config.first_line_width * 2);
        let segments = wrap_line(&text, &config);

        println!("Number of segments: {}", segments.len());
        for (i, seg) in segments.iter().enumerate() {
            println!(
                "Segment {}: len={}, start={}, end={}",
                i,
                seg.text.len(),
                seg.start_char_offset,
                seg.end_char_offset
            );
        }

        assert_eq!(
            segments[0].text.len(),
            config.first_line_width,
            "First segment should have first_line_width characters"
        );
        if segments.len() > 1 {
            assert_eq!(
                segments[1].text.len(),
                config.continuation_line_width,
                "Second segment should have continuation_line_width characters (same as first!)"
            );
        }
    }

    #[test]
    fn test_wrap_with_real_text() {
        let config = WrapConfig::new(60, 8, true);
        println!(
            "Config: first={}, cont={}",
            config.first_line_width, config.continuation_line_width
        );

        let text = "The quick brown fox jumps over the lazy dog and runs through the forest, exploring ancient trees and mysterious pathways that wind between towering oaks.";
        println!("Text len: {}", text.len());
        println!("Text[48..55]: {:?}", &text[48..55]);

        let segments = wrap_line(text, &config);

        for (i, seg) in segments.iter().enumerate() {
            println!(
                "Segment {}: len={}, start={}, end={}, text[..10]={:?}",
                i,
                seg.text.len(),
                seg.start_char_offset,
                seg.end_char_offset,
                &seg.text[..seg.text.len().min(10)]
            );
        }

        assert_eq!(
            segments[0].text.len(),
            config.first_line_width,
            "First segment should have {} chars but has {}",
            config.first_line_width,
            segments[0].text.len()
        );
    }

    #[test]
    fn test_wrap_config_widths() {
        // Test that WrapConfig calculates widths correctly
        let config = WrapConfig::new(60, 8, true);

        println!(
            "Config: first_line_width={}, continuation_line_width={}, gutter_width={}",
            config.first_line_width, config.continuation_line_width, config.gutter_width
        );

        // Terminal: 60, scrollbar: 1, gutter: 8
        // Available width: 60 - 1 - 8 = 51 chars
        // BOTH first line and continuation lines should have 51 chars of TEXT
        // (continuation lines have visual indentation, but same text width)
        assert_eq!(config.first_line_width, 51);
        assert_eq!(
            config.continuation_line_width, 51,
            "Continuation lines should have same text width as first line!"
        );

        let text = "The quick brown fox jumps over the lazy dog and runs through the forest, exploring ancient trees and mysterious pathways that wind between towering oaks.";
        let segments = wrap_line(text, &config);

        println!("Text length: {}", text.len());
        println!("Number of segments: {}", segments.len());

        for (i, seg) in segments.iter().enumerate() {
            println!(
                "Segment {}: start={}, end={}, len={}, is_continuation={}",
                i,
                seg.start_char_offset,
                seg.end_char_offset,
                seg.text.len(),
                seg.is_continuation
            );
            println!("  Text: {:?}", &seg.text[..seg.text.len().min(40)]);
        }

        // Check position 51 (should be first char of segment 1)
        let (seg_idx, col_in_seg) = char_position_to_segment(51, &segments);
        println!(
            "Position 51: segment_idx={}, col_in_segment={}",
            seg_idx, col_in_seg
        );
        assert_eq!(seg_idx, 1, "Position 51 should be in segment 1");
        assert_eq!(col_in_seg, 0, "Position 51 should be at start of segment 1");
    }

    // ==========================================================================
    // Tests for double-width character handling (CJK, emoji, etc.)
    // These tests verify that wrap_line correctly uses visual display width
    // instead of character count for CJK, emoji, and other wide characters.
    // ==========================================================================

    use crate::primitives::display_width::str_width;

    /// Test that str_width correctly identifies double-width characters
    #[test]
    fn test_visual_width_calculation() {
        // ASCII - each char is 1 column
        assert_eq!(str_width("Hello"), 5);

        // Chinese - each char is 2 columns
        assert_eq!(
            str_width("你好"),
            4,
            "Two Chinese characters should be 4 columns"
        );

        // Emoji - each is 2 columns
        assert_eq!(str_width("🚀"), 2, "Rocket emoji should be 2 columns");
        assert_eq!(str_width("🚀🎉"), 4, "Two emoji should be 4 columns");

        // Mixed
        assert_eq!(
            str_width("Hello你好"),
            5 + 4,
            "Hello (5) + 你好 (4) = 9 columns"
        );
        assert_eq!(
            str_width("a🚀b"),
            1 + 2 + 1,
            "a (1) + 🚀 (2) + b (1) = 4 columns"
        );

        // Japanese
        assert_eq!(
            str_width("月"),
            2,
            "Japanese Moon character should be 2 columns"
        );
    }

    /// BUG TEST: wrap_line counts characters, not visual width
    /// This test FAILS because wrap_line uses chars().count() instead of visual width
    #[test]
    fn test_wrap_line_double_width_characters() {
        // Create a narrow terminal: 20 columns total
        // After gutter (8) and scrollbar (1), we have 11 columns for text
        let config = WrapConfig::new(20, 8, true);
        assert_eq!(
            config.first_line_width, 11,
            "Should have 11 columns for text"
        );

        // Create text with Chinese characters
        // "你好世界啊" = 5 characters, but 10 visual columns
        let chinese_text = "你好世界啊";
        assert_eq!(chinese_text.chars().count(), 5, "5 Chinese characters");
        assert_eq!(str_width(chinese_text), 10, "10 visual columns");

        let _segments = wrap_line(chinese_text, &config);

        // Current BUGGY behavior: wrap_line thinks 5 chars < 11 width, so no wrap
        // Expected behavior: 10 visual columns < 11 width, so should fit in one line
        // This test passes because the text fits even with buggy counting

        // Now test a case that SHOULD wrap but DOESN'T due to the bug:
        // "你好世界啊你好" = 7 characters (14 visual columns)
        let chinese_text_long = "你好世界啊你好";
        assert_eq!(chinese_text_long.chars().count(), 7, "7 Chinese characters");
        assert_eq!(str_width(chinese_text_long), 14, "14 visual columns");

        let segments_long = wrap_line(chinese_text_long, &config);

        // BUG: wrap_line thinks 7 chars < 11 width, so it doesn't wrap!
        // But 14 visual columns > 11 column width, so it SHOULD wrap!
        //
        // Expected: 2 segments (wraps after ~5-6 chars to stay within 11 visual columns)
        // Actual:   1 segment (treats 7 chars as fitting in 11 "columns")
        assert_eq!(
            segments_long.len(),
            2,
            "BUG: 14 visual columns should wrap at 11 column width! \
             wrap_line is counting characters ({}) instead of visual width ({}).",
            chinese_text_long.chars().count(),
            str_width(chinese_text_long)
        );
    }

    /// BUG TEST: wrap_line with emoji doesn't account for visual width
    #[test]
    fn test_wrap_line_emoji_str_width() {
        // 11 columns available for text
        let config = WrapConfig::new(20, 8, true);
        assert_eq!(config.first_line_width, 11);

        // "🚀🎉🔥🌟🎄🎊" = 6 emoji characters, but 12 visual columns
        // Note: Using emoji that are all in the Misc Symbols & Pictographs range
        let emoji_text = "🚀🎉🔥🌟🎄🎊";
        assert_eq!(emoji_text.chars().count(), 6, "6 emoji characters");
        assert_eq!(str_width(emoji_text), 12, "12 visual columns");

        let segments = wrap_line(emoji_text, &config);

        // BUG: wrap_line thinks 6 chars < 11 width, so no wrap
        // But 12 visual columns > 11 column width, so it SHOULD wrap!
        assert_eq!(
            segments.len(),
            2,
            "BUG: 12 visual columns should wrap at 11 column width! \
             wrap_line is counting emoji as 1 column each instead of 2."
        );
    }

    /// BUG TEST: Mixed ASCII and double-width characters
    #[test]
    fn test_wrap_line_mixed_ascii_and_cjk() {
        // 11 columns available for text
        let config = WrapConfig::new(20, 8, true);
        assert_eq!(config.first_line_width, 11);

        // "Hello你好" = 7 characters, but 9 visual columns (5 + 4)
        // This should fit in 11 columns
        let mixed_short = "Hello你好";
        assert_eq!(mixed_short.chars().count(), 7);
        assert_eq!(str_width(mixed_short), 9);

        let segments_short = wrap_line(mixed_short, &config);
        assert_eq!(segments_short.len(), 1, "9 visual columns should fit in 11");

        // "Hello你好世" = 8 characters, but 11 visual columns (5 + 6)
        // This should JUST fit in 11 columns
        let mixed_exact = "Hello你好世";
        assert_eq!(mixed_exact.chars().count(), 8);
        assert_eq!(str_width(mixed_exact), 11);

        let segments_exact = wrap_line(mixed_exact, &config);
        assert_eq!(
            segments_exact.len(),
            1,
            "11 visual columns should fit exactly in 11"
        );

        // "Hello你好世界" = 9 characters, but 13 visual columns (5 + 8)
        // This should wrap!
        let mixed_long = "Hello你好世界";
        assert_eq!(mixed_long.chars().count(), 9);
        assert_eq!(str_width(mixed_long), 13);

        let segments_long = wrap_line(mixed_long, &config);
        // BUG: wrap_line thinks 9 chars < 11 width, so no wrap
        // But 13 visual columns > 11 column width!
        assert_eq!(
            segments_long.len(),
            2,
            "BUG: 13 visual columns ({} chars) should wrap at 11 column width! \
             wrap_line is not accounting for double-width characters.",
            mixed_long.chars().count()
        );
    }

    /// Test demonstrating the fundamental issue: chars().count() vs visual width
    #[test]
    fn test_chars_count_vs_visual_width_bug() {
        // This test demonstrates WHY the bug exists
        let chinese = "你好世界"; // 4 characters, 8 visual columns
        let ascii = "HelloWor"; // 8 characters, 8 visual columns

        // Both should take the same visual space on screen
        assert_eq!(str_width(chinese), str_width(ascii), "Same visual width");

        // But chars().count() gives DIFFERENT values
        assert_eq!(chinese.chars().count(), 4);
        assert_eq!(ascii.chars().count(), 8);

        // The bug: wrap_line uses chars().count() for width calculation
        // So it thinks "你好世界" takes 4 columns and "HelloWor" takes 8 columns
        // But they both take 8 visual columns on screen!

        let config = WrapConfig::new(20, 8, true); // 11 columns for text

        let chinese_segments = wrap_line(chinese, &config);
        let ascii_segments = wrap_line(ascii, &config);

        // Both have 8 visual columns, both should fit in 11 columns (no wrap)
        // ASCII: works correctly (8 chars counted as 8 columns)
        assert_eq!(ascii_segments.len(), 1, "ASCII text should not wrap");

        // Chinese: ALSO works in this case (4 chars < 11), but for wrong reason!
        // wrap_line thinks it's 4 columns, not 8
        assert_eq!(chinese_segments.len(), 1, "Chinese text should not wrap");

        // Now test where the bug becomes visible:
        // "你好世界你好" = 6 chars, 12 visual columns
        let chinese_long = "你好世界你好";
        assert_eq!(chinese_long.chars().count(), 6);
        assert_eq!(str_width(chinese_long), 12);

        // "HelloWorldAB" = 12 chars, 12 visual columns
        let ascii_long = "HelloWorldAB";
        assert_eq!(ascii_long.chars().count(), 12);
        assert_eq!(str_width(ascii_long), 12);

        // Same visual width, but...
        let chinese_long_segments = wrap_line(chinese_long, &config);
        let ascii_long_segments = wrap_line(ascii_long, &config);

        // ASCII wraps correctly: 12 > 11, so 2 segments
        assert_eq!(
            ascii_long_segments.len(),
            2,
            "ASCII 12 columns should wrap at 11"
        );

        // Chinese now wraps correctly using visual width!
        assert_eq!(
            chinese_long_segments.len(),
            2,
            "Chinese text with 12 visual columns should wrap at 11 column width"
        );
    }
}

// ==========================================================================
// Property-based tests for Unicode handling
// ==========================================================================

#[cfg(test)]
mod proptests {
    use super::*;
    use crate::primitives::display_width::str_width;
    use proptest::prelude::*;

    /// Strategy to generate strings with various Unicode characters
    fn unicode_string_strategy() -> impl Strategy<Value = String> {
        prop::collection::vec(
            prop_oneof![
                // ASCII characters
                "[a-zA-Z0-9 ]{1,5}",
                // Chinese characters
                "[你好世界月日本中文]{1,3}",
                // Japanese hiragana/katakana
                "[あいうえおアイウエオ]{1,3}",
                // Korean
                "[한글테스트]{1,2}",
                // Emoji (common ones that are reliably 2-width)
                Just("🚀".to_string()),
                Just("🎉".to_string()),
                Just("🔥".to_string()),
                Just("❤".to_string()),
                // Mixed
                Just("a你b".to_string()),
                Just("Hello世界".to_string()),
            ],
            1..5,
        )
        .prop_map(|parts| parts.join(""))
    }

    proptest! {
        /// Property: Segment visual widths should not exceed configured width
        /// (except when a single character is wider than the width)
        #[test]
        fn prop_segment_width_respects_config(
            text in unicode_string_strategy(),
            width in 5usize..50,
        ) {
            let config = WrapConfig {
                first_line_width: width,
                continuation_line_width: width,
                gutter_width: 0,
            };

            let segments = wrap_line(&text, &config);

            for (i, segment) in segments.iter().enumerate() {
                let seg_width = str_width(&segment.text);

                // Each segment should fit within the width limit,
                // OR be a single character that's wider than the limit
                let char_count = segment.text.chars().count();
                if char_count > 1 {
                    prop_assert!(
                        seg_width <= width,
                        "Segment {} has visual width {} > config width {}. Text: {:?}",
                        i, seg_width, width, segment.text
                    );
                }
            }
        }

        /// Property: Concatenating all segments should give back the original text
        #[test]
        fn prop_segments_reconstruct_original(text in unicode_string_strategy()) {
            let config = WrapConfig::new(20, 0, false);
            let segments = wrap_line(&text, &config);

            let reconstructed: String = segments.iter().map(|s| s.text.as_str()).collect();

            prop_assert_eq!(
                reconstructed, text,
                "Segments should reconstruct to original text"
            );
        }

        /// Property: Total visual width should be preserved across wrapping
        #[test]
        fn prop_total_visual_width_preserved(text in unicode_string_strategy()) {
            let config = WrapConfig::new(15, 0, false);
            let segments = wrap_line(&text, &config);

            let original_width = str_width(&text);
            let segments_width: usize = segments.iter().map(|s| str_width(&s.text)).sum();

            prop_assert_eq!(
                segments_width, original_width,
                "Total visual width should be preserved"
            );
        }

        /// Property: Character offsets should be monotonically increasing and valid
        #[test]
        fn prop_char_offsets_valid(text in unicode_string_strategy()) {
            let config = WrapConfig::new(10, 0, false);
            let segments = wrap_line(&text, &config);

            let text_char_count = text.chars().count();
            let mut prev_end = 0;

            for (i, segment) in segments.iter().enumerate() {
                // Start should equal previous end (no gaps)
                prop_assert_eq!(
                    segment.start_char_offset, prev_end,
                    "Segment {} start should equal previous end",
                    i
                );

                // End should be > start (unless empty)
                if !segment.text.is_empty() {
                    prop_assert!(
                        segment.end_char_offset > segment.start_char_offset,
                        "Non-empty segment {} should have end > start",
                        i
                    );
                }

                // End should not exceed text length
                prop_assert!(
                    segment.end_char_offset <= text_char_count,
                    "Segment {} end {} exceeds text char count {}",
                    i, segment.end_char_offset, text_char_count
                );

                prev_end = segment.end_char_offset;
            }

            // Last segment should end at text end
            if !segments.is_empty() && !text.is_empty() {
                prop_assert_eq!(
                    segments.last().unwrap().end_char_offset,
                    text_char_count,
                    "Last segment should end at text end"
                );
            }
        }

        /// Property: For equal visual width strings, wrap_line should produce
        /// segments with similar characteristics regardless of character encoding
        #[test]
        fn prop_visual_width_consistency(
            n_ascii in 1usize..10,
            _n_wide in 1usize..5,
        ) {
            // Create ASCII string with n_ascii * 2 characters (each 1 column)
            let ascii_text: String = "A".repeat(n_ascii * 2);
            // Create wide string with n_wide * 2 characters (each 2 columns, so n_wide * 4 visual width)
            // Actually, to match visual width, we need n_ascii wide chars
            let wide_text: String = "你".repeat(n_ascii);

            // Both should have same visual width
            let ascii_width = str_width(&ascii_text);
            let wide_width = str_width(&wide_text);
            prop_assert_eq!(ascii_width, wide_width, "Visual widths should match");

            // With same config, both should produce same number of segments
            let config = WrapConfig::new(15, 0, false);
            let ascii_segments = wrap_line(&ascii_text, &config);
            let wide_segments = wrap_line(&wide_text, &config);

            prop_assert_eq!(
                ascii_segments.len(),
                wide_segments.len(),
                "Equal visual width text should produce same number of segments. \
                 ASCII '{}' ({} width) -> {} segments, Wide '{}' ({} width) -> {} segments",
                ascii_text, ascii_width, ascii_segments.len(),
                wide_text, wide_width, wide_segments.len()
            );
        }
    }
}
