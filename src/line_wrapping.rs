//! Clean line wrapping implementation
//!
//! Pipeline: Line text → Wrapping transformation → Wrapped line segments → Rendering/Cursor
//!
//! This module provides a single source of truth for how lines wrap,
//! ensuring rendering and cursor positioning always agree.

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
        let text_area_width = content_area_width.saturating_sub(scrollbar_width).saturating_sub(gutter_width);
        let first_line_width = text_area_width;
        let continuation_line_width = text_area_width.saturating_sub(gutter_width);

        Self {
            first_line_width,
            continuation_line_width,
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

        // Take up to width characters for this segment
        let mut segment_len = 0;
        let segment_text_start = pos;

        while segment_len < width && pos < chars.len() {
            segment_len += 1;
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
        assert_eq!(segments[0].is_continuation, false);
    }

    #[test]
    fn test_wrap_short_line() {
        let config = WrapConfig::new(60, 8, true);
        let text = "Hello world";
        let segments = wrap_line(text, &config);

        assert_eq!(segments.len(), 1);
        assert_eq!(segments[0].text, text);
        assert_eq!(segments[0].is_continuation, false);
    }

    #[test]
    fn test_wrap_long_line() {
        // Terminal: 60 cols, Gutter: 8, Scrollbar: 1
        // First line: 60 - 1 = 59 chars (see WrapConfig::new implementation)
        // Continuation: 59 chars (same as first line)
        let config = WrapConfig::new(60, 8, true);

        let text = "A fast, lightweight terminal text editor written in Rust. Handles files of any size with instant startup, low memory usage, and modern IDE features.";
        let segments = wrap_line(text, &config);

        // Expected segments based on 59 character width:
        const SEG0: &str = "A fast, lightweight terminal text editor written in Rust. H";
        const SEG1: &str = "andles files of any size with instant startup, low memory u";
        const SEG2: &str = "sage, and modern IDE features.";

        assert_eq!(segments.len(), 3);

        assert_eq!(segments[0].text, SEG0);
        assert_eq!(segments[0].is_continuation, false);

        assert_eq!(segments[1].text, SEG1);
        assert_eq!(segments[1].is_continuation, true);

        assert_eq!(segments[2].text, SEG2);
        assert_eq!(segments[2].is_continuation, true);

        // Test char_position_to_segment with various positions

        // Position 0 (start of text) -> segment 0, column 0
        assert_eq!(char_position_to_segment(0, &segments), (0, 0));

        // Position in middle of first segment
        assert_eq!(char_position_to_segment(25, &segments), (0, 25));

        // Position at end of first segment (where 'H' is)
        assert_eq!(char_position_to_segment(SEG0.chars().count() - 1, &segments), (0, SEG0.chars().count() - 1));

        // Position at start of second segment (where 'a' in "andles" is)
        assert_eq!(char_position_to_segment(SEG0.chars().count(), &segments), (1, 0));

        // Position in middle of second segment
        let pos_in_seg1 = SEG0.chars().count() + 30;
        assert_eq!(char_position_to_segment(pos_in_seg1, &segments), (1, 30));

        // Position at start of third segment
        let seg2_start = SEG0.chars().count() + SEG1.chars().count();
        assert_eq!(char_position_to_segment(seg2_start, &segments), (2, 0));

        // Position at end of text
        let text_len = text.chars().count();
        assert_eq!(char_position_to_segment(text_len, &segments), (2, SEG2.chars().count()));

        // Position beyond end of text
        assert_eq!(char_position_to_segment(text_len + 10, &segments), (2, SEG2.chars().count()));
    }

    #[test]
    fn test_wrap_with_leading_space() {
        let config = WrapConfig::new(60, 8, true);

        // Create text that wraps such that continuation starts with space
        let text = "A".repeat(51) + " " + &"B".repeat(50);
        let segments = wrap_line(&text, &config);

	println!("segments: {:?}", segments);
        assert_eq!(segments.len(), 2);
        assert_eq!(segments[0].text.chars().count(), 59); // All A's
        assert_eq!(segments[1].is_continuation, true);

        // Second segment should skip the leading space and have B's
        assert!(segments[1].text.starts_with('B'), "Continuation should skip leading space");
        assert_eq!(segments[1].text.chars().count(), 43);
    }

    #[test]
    fn test_wrap_exact_width() {
        let config = WrapConfig::new(60, 8, true);
        println!("Config: first={}, cont={}", config.first_line_width, config.continuation_line_width);

        // Create text that's longer than one line
        let text = "A".repeat(100);
        let segments = wrap_line(&text, &config);

        println!("Number of segments: {}", segments.len());
        for (i, seg) in segments.iter().enumerate() {
            println!("Segment {}: len={}, start={}, end={}", i, seg.text.len(), seg.start_char_offset, seg.end_char_offset);
        }

        assert_eq!(segments[0].text.len(), config.first_line_width, "First segment should have first_line_width characters");
        if segments.len() > 1 {
            assert_eq!(segments[1].text.len(), config.continuation_line_width, "Second segment should have continuation_line_width characters");
        }
    }

    #[test]
    fn test_wrap_with_real_text() {
        let config = WrapConfig::new(60, 8, true);
        println!("Config: first={}, cont={}", config.first_line_width, config.continuation_line_width);

        let text = "The quick brown fox jumps over the lazy dog and runs through the forest, exploring ancient trees and mysterious pathways that wind between towering oaks.";
        println!("Text len: {}", text.len());
        println!("Text[48..55]: {:?}", &text[48..55]);

        let segments = wrap_line(&text, &config);

        for (i, seg) in segments.iter().enumerate() {
            println!("Segment {}: len={}, start={}, end={}, text[..10]={:?}",
                     i, seg.text.len(), seg.start_char_offset, seg.end_char_offset,
                     &seg.text[..seg.text.len().min(10)]);
        }

        assert_eq!(segments[0].text.len(), config.first_line_width,
                   "First segment should have {} chars but has {}", config.first_line_width, segments[0].text.len());
    }

    #[test]
    fn test_wrap_config_widths() {
        // Test that WrapConfig calculates widths correctly
        let config = WrapConfig::new(60, 8, true);

        println!("Config: first_line_width={}, continuation_line_width={}, gutter_width={}",
                 config.first_line_width, config.continuation_line_width, config.gutter_width);

        // Terminal: 60, scrollbar: 1, gutter: 8
        // First line: 60 - 1 - 8 = 51
        // Continuation: 51 - 8 = 43
        assert_eq!(config.first_line_width, 51);
        assert_eq!(config.continuation_line_width, 43);

        let text = "The quick brown fox jumps over the lazy dog and runs through the forest, exploring ancient trees and mysterious pathways that wind between towering oaks.";
        let segments = wrap_line(text, &config);

        println!("Text length: {}", text.len());
        println!("Number of segments: {}", segments.len());

        for (i, seg) in segments.iter().enumerate() {
            println!("Segment {}: start={}, end={}, len={}, is_continuation={}",
                     i, seg.start_char_offset, seg.end_char_offset, seg.text.len(), seg.is_continuation);
            println!("  Text: {:?}", &seg.text[..seg.text.len().min(40)]);
        }

        // Check position 51 (should be first char of segment 1)
        let (seg_idx, col_in_seg) = char_position_to_segment(51, &segments);
        println!("Position 51: segment_idx={}, col_in_segment={}", seg_idx, col_in_seg);
        assert_eq!(seg_idx, 1, "Position 51 should be in segment 1");
        assert_eq!(col_in_seg, 0, "Position 51 should be at start of segment 1");
    }

}
