//! Bracket matching highlight using the overlay system
//!
//! This module manages bracket pair highlighting through overlays.
//! When the cursor is on a bracket, the matching bracket is highlighted.
//! Optional rainbow colors can be applied based on nesting depth.

use crate::model::buffer::Buffer;
use crate::model::marker::MarkerList;
use crate::view::overlay::{Overlay, OverlayFace, OverlayManager, OverlayNamespace};
use crate::view::theme::Theme;
use ratatui::style::Color;
use std::ops::Range;

/// Default rainbow bracket colors (cycle through these based on nesting depth)
pub const DEFAULT_BRACKET_COLORS: [Color; 6] = [
    Color::Rgb(255, 215, 0),   // Gold
    Color::Rgb(218, 112, 214), // Orchid
    Color::Rgb(50, 205, 50),   // Lime Green
    Color::Rgb(30, 144, 255),  // Dodger Blue
    Color::Rgb(255, 127, 80),  // Coral
    Color::Rgb(147, 112, 219), // Medium Purple
];

/// Namespace for bracket highlight overlays
pub fn bracket_highlight_namespace() -> OverlayNamespace {
    OverlayNamespace::from_string("bracket-highlight".to_string())
}

/// Namespace for rainbow bracket colorization overlays
pub fn bracket_colorization_namespace() -> OverlayNamespace {
    OverlayNamespace::from_string("bracket-colorization".to_string())
}

/// Bracket types we match
const BRACKET_PAIRS: &[(char, char)] = &[('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')];

/// Maximum number of bytes to scan for bracket matching/nesting depth.
/// Prevents O(n) scans on huge files from hanging the editor.
pub(crate) const MAX_BRACKET_SEARCH_BYTES: usize = 1_000_000;

/// Chunk size for bulk reads during bracket scanning.
const BRACKET_SCAN_CHUNK: usize = 16 * 1024;

/// Check if a character is an opening bracket
fn is_opening_bracket(ch: char) -> bool {
    BRACKET_PAIRS.iter().any(|(open, _)| *open == ch)
}

/// Check if a character is a closing bracket
fn is_closing_bracket(ch: char) -> bool {
    BRACKET_PAIRS.iter().any(|(_, close)| *close == ch)
}

/// Get the opening bracket for a closing bracket
fn opening_for_closing(ch: char) -> Option<char> {
    BRACKET_PAIRS
        .iter()
        .find_map(|(open, close)| if *close == ch { Some(*open) } else { None })
}

/// Whether `pos` falls inside any of the given byte ranges.
///
/// `ranges` must be sorted by `start` and non-overlapping (the syntax
/// highlighter produces spans in this order), which lets us binary-search.
/// These are the comment/string ranges where brackets are prose/data rather
/// than structural punctuation, so they are excluded from bracket matching
/// and rainbow colorization (issue #2405).
fn pos_in_ranges(ranges: &[Range<usize>], pos: usize) -> bool {
    ranges
        .binary_search_by(|r| {
            if pos < r.start {
                std::cmp::Ordering::Greater
            } else if pos >= r.end {
                std::cmp::Ordering::Less
            } else {
                std::cmp::Ordering::Equal
            }
        })
        .is_ok()
}

/// Get the matching bracket pair for a character
fn get_bracket_pair(ch: char) -> Option<(char, char, bool)> {
    for &(open, close) in BRACKET_PAIRS {
        if ch == open {
            return Some((open, close, true)); // forward search
        }
        if ch == close {
            return Some((open, close, false)); // backward search
        }
    }
    None
}

/// Manager for bracket highlight overlays
pub struct BracketHighlightOverlay {
    /// Whether bracket highlighting is enabled
    pub enabled: bool,
    /// Whether to use rainbow colors based on nesting depth
    pub rainbow_enabled: bool,
    /// Colors to use for rainbow brackets (cycles through)
    pub rainbow_colors: [Color; 6],
    /// Default bracket match highlight color (when rainbow is disabled)
    pub match_color: Color,
    /// Last cursor position where we computed brackets
    last_cursor_pos: Option<usize>,
}

impl BracketHighlightOverlay {
    /// Create a new bracket highlight overlay manager
    pub fn new() -> Self {
        Self {
            enabled: true,
            rainbow_enabled: true,
            rainbow_colors: DEFAULT_BRACKET_COLORS,
            match_color: Color::Rgb(255, 215, 0), // Gold
            last_cursor_pos: None,
        }
    }

    /// Update bracket highlights based on cursor position
    ///
    /// `skip_ranges` are byte ranges (comments and strings, sorted by start)
    /// whose brackets must be ignored — they are prose/data, not structural
    /// punctuation (issue #2405).
    ///
    /// Returns true if overlays were updated
    #[allow(clippy::too_many_arguments)]
    pub fn update(
        &mut self,
        buffer: &Buffer,
        overlays: &mut OverlayManager,
        marker_list: &mut MarkerList,
        theme: &Theme,
        cursor_position: usize,
        viewport_start: usize,
        viewport_end: usize,
        skip_ranges: &[Range<usize>],
    ) -> bool {
        if !self.enabled && !self.rainbow_enabled {
            return false;
        }

        let new_match_color = theme.bracket_match_fg;
        let new_rainbow_colors = [
            theme.bracket_rainbow_1,
            theme.bracket_rainbow_2,
            theme.bracket_rainbow_3,
            theme.bracket_rainbow_4,
            theme.bracket_rainbow_5,
            theme.bracket_rainbow_6,
        ];
        let colors_changed =
            self.match_color != new_match_color || self.rainbow_colors != new_rainbow_colors;
        if colors_changed {
            self.match_color = new_match_color;
            self.rainbow_colors = new_rainbow_colors;
        }

        let mut updated = false;

        // Update full rainbow bracket colorization
        if self.rainbow_enabled {
            updated |= self.update_colorization(
                buffer,
                overlays,
                marker_list,
                viewport_start,
                viewport_end,
                skip_ranges,
            );
        } else {
            updated |= self.clear_colorization(overlays, marker_list);
        }

        // Check if cursor position changed
        if !self.enabled {
            return updated;
        }

        if self.last_cursor_pos == Some(cursor_position) && !colors_changed {
            return updated;
        }
        self.last_cursor_pos = Some(cursor_position);
        updated = true;

        // Clear existing bracket overlays
        let ns = bracket_highlight_namespace();
        overlays.clear_namespace(&ns, marker_list);

        // Check if cursor is on a bracket
        let buf_len = buffer.len();
        if cursor_position >= buf_len {
            return true;
        }

        let bytes = buffer.slice_bytes(cursor_position..cursor_position + 1);
        if bytes.is_empty() {
            return true;
        }

        let ch = bytes[0] as char;

        // Get bracket pair info
        let (opening, closing, forward) = match get_bracket_pair(ch) {
            Some(pair) => pair,
            None => return true, // Not on a bracket
        };

        // A bracket inside a comment or string is not structural punctuation,
        // so don't highlight it (issue #2405).
        if pos_in_ranges(skip_ranges, cursor_position) {
            return true;
        }

        // Calculate nesting depth at cursor position for rainbow colors
        let depth = if self.rainbow_enabled {
            self.calculate_nesting_depth(buffer, cursor_position, forward, skip_ranges)
        } else {
            0
        };

        // Find matching bracket
        let matching_pos = self.find_matching_bracket(
            buffer,
            cursor_position,
            opening,
            closing,
            forward,
            skip_ranges,
        );

        // Determine color based on depth
        let color = if self.rainbow_enabled {
            self.rainbow_colors[depth % self.rainbow_colors.len()]
        } else {
            self.match_color
        };

        // Create overlay for the bracket at cursor
        let cursor_face = OverlayFace::Foreground { color };
        let cursor_overlay = Overlay::with_namespace(
            marker_list,
            cursor_position..cursor_position + 1,
            cursor_face,
            ns.clone(),
        )
        .with_priority_value(10);
        overlays.add(cursor_overlay);

        // Create overlay for the matching bracket if found
        if let Some(match_pos) = matching_pos {
            let match_face = OverlayFace::Foreground { color };
            let match_overlay = Overlay::with_namespace(
                marker_list,
                match_pos..match_pos + 1,
                match_face,
                ns.clone(),
            )
            .with_priority_value(10);
            overlays.add(match_overlay);
        }

        updated
    }

    /// Calculate the nesting depth of a bracket at a position.
    ///
    /// Brackets inside `skip_ranges` (comments/strings) are ignored so the
    /// depth reflects only structural punctuation (issue #2405).
    fn calculate_nesting_depth(
        &self,
        buffer: &Buffer,
        position: usize,
        is_opening: bool,
        skip_ranges: &[Range<usize>],
    ) -> usize {
        // Track nesting depth across all bracket types so rainbow colors follow
        // overall nesting level. Bound the scan to avoid O(n) work on huge files.
        let scan_start = position.saturating_sub(MAX_BRACKET_SEARCH_BYTES);
        let mut stack: Vec<char> = Vec::new();
        let mut pos = scan_start;

        while pos < position {
            let chunk_end = (pos + BRACKET_SCAN_CHUNK).min(position);
            let chunk = buffer.slice_bytes(pos..chunk_end);
            for (i, &b) in chunk.iter().enumerate() {
                if pos_in_ranges(skip_ranges, pos + i) {
                    continue;
                }
                let c = b as char;
                if is_opening_bracket(c) {
                    stack.push(c);
                } else if is_closing_bracket(c) {
                    if let Some(expected_open) = opening_for_closing(c) {
                        if stack.last() == Some(&expected_open) {
                            stack.pop();
                        }
                    }
                }
            }
            pos = chunk_end;
        }

        // For opening brackets, depth is the current stack size.
        // For closing brackets, depth is the stack size minus one (matching opening).
        if is_opening {
            stack.len()
        } else {
            stack.len().saturating_sub(1)
        }
    }

    /// Find the matching bracket (bounded to MAX_BRACKET_SEARCH_BYTES).
    ///
    /// Brackets inside `skip_ranges` (comments/strings) are ignored so the
    /// match reflects only structural punctuation (issue #2405).
    fn find_matching_bracket(
        &self,
        buffer: &Buffer,
        position: usize,
        opening: char,
        closing: char,
        forward: bool,
        skip_ranges: &[Range<usize>],
    ) -> Option<usize> {
        let buffer_len = buffer.len();
        let open = opening as u8;
        let close = closing as u8;
        let mut depth: i32 = 1;

        if forward {
            let search_limit = (position + 1 + MAX_BRACKET_SEARCH_BYTES).min(buffer_len);
            let mut pos = position + 1;
            while pos < search_limit {
                let chunk_end = (pos + BRACKET_SCAN_CHUNK).min(search_limit);
                let chunk = buffer.slice_bytes(pos..chunk_end);
                for (i, &b) in chunk.iter().enumerate() {
                    if pos_in_ranges(skip_ranges, pos + i) {
                        continue;
                    }
                    if b == open {
                        depth += 1;
                    } else if b == close {
                        depth -= 1;
                        if depth == 0 {
                            return Some(pos + i);
                        }
                    }
                }
                pos = chunk_end;
            }
        } else {
            let search_limit = position.saturating_sub(MAX_BRACKET_SEARCH_BYTES);
            let mut pos = position;
            while pos > search_limit {
                let chunk_start = pos.saturating_sub(BRACKET_SCAN_CHUNK).max(search_limit);
                let chunk = buffer.slice_bytes(chunk_start..pos);
                for (i, &b) in chunk.iter().enumerate().rev() {
                    if pos_in_ranges(skip_ranges, chunk_start + i) {
                        continue;
                    }
                    if b == close {
                        depth += 1;
                    } else if b == open {
                        depth -= 1;
                        if depth == 0 {
                            return Some(chunk_start + i);
                        }
                    }
                }
                pos = chunk_start;
            }
        }

        None
    }

    /// Force clear all highlights (e.g., when switching buffers)
    pub fn clear(&mut self, overlays: &mut OverlayManager, marker_list: &mut MarkerList) {
        let highlight_ns = bracket_highlight_namespace();
        overlays.clear_namespace(&highlight_ns, marker_list);
        let color_ns = bracket_colorization_namespace();
        overlays.clear_namespace(&color_ns, marker_list);
        self.last_cursor_pos = None;
    }

    /// Force recalculation on next update
    pub fn invalidate(&mut self) {
        self.last_cursor_pos = None;
    }

    fn clear_colorization(
        &mut self,
        overlays: &mut OverlayManager,
        marker_list: &mut MarkerList,
    ) -> bool {
        let ns = bracket_colorization_namespace();
        overlays.clear_namespace(&ns, marker_list);
        true
    }

    fn update_colorization(
        &mut self,
        buffer: &Buffer,
        overlays: &mut OverlayManager,
        marker_list: &mut MarkerList,
        viewport_start: usize,
        viewport_end: usize,
        skip_ranges: &[Range<usize>],
    ) -> bool {
        if viewport_start >= viewport_end || buffer.is_empty() {
            return self.clear_colorization(overlays, marker_list);
        }

        let viewport_size = viewport_end.saturating_sub(viewport_start);
        let scan_start = viewport_start.saturating_sub(viewport_size);
        let scan_end = viewport_end.min(buffer.len());
        if scan_start >= scan_end {
            return self.clear_colorization(overlays, marker_list);
        }

        let bytes = buffer.slice_bytes(scan_start..scan_end);
        if bytes.is_empty() {
            return self.clear_colorization(overlays, marker_list);
        }

        let ns = bracket_colorization_namespace();
        let mut stack: Vec<char> = Vec::new();
        let mut new_overlays = Vec::new();

        for (idx, byte) in bytes.iter().enumerate() {
            let pos = scan_start + idx;
            let c = *byte as char;

            // Brackets inside comments/strings are prose/data, not structural
            // punctuation — don't colorize them and don't let them affect the
            // nesting depth of real brackets (issue #2405).
            if pos_in_ranges(skip_ranges, pos) {
                continue;
            }

            if is_opening_bracket(c) {
                let depth = stack.len();
                stack.push(c);
                if pos >= viewport_start {
                    let color = self.rainbow_colors[depth % self.rainbow_colors.len()];
                    let face = OverlayFace::Foreground { color };
                    let overlay =
                        Overlay::with_namespace(marker_list, pos..pos + 1, face, ns.clone())
                            .with_priority_value(6);
                    new_overlays.push(overlay);
                }
                continue;
            }

            if is_closing_bracket(c) {
                let depth = stack.len().saturating_sub(1);
                if let Some(expected_open) = opening_for_closing(c) {
                    if stack.last() == Some(&expected_open) {
                        stack.pop();
                    }
                }
                if pos >= viewport_start {
                    let color = self.rainbow_colors[depth % self.rainbow_colors.len()];
                    let face = OverlayFace::Foreground { color };
                    let overlay =
                        Overlay::with_namespace(marker_list, pos..pos + 1, face, ns.clone())
                            .with_priority_value(6);
                    new_overlays.push(overlay);
                }
            }
        }

        overlays.replace_range_in_namespace(&ns, &(0..buffer.len()), new_overlays, marker_list);
        true
    }
}

impl Default for BracketHighlightOverlay {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::buffer::Buffer;

    #[test]
    fn test_bracket_pair_detection() {
        assert!(is_opening_bracket('('));
        assert!(is_opening_bracket('['));
        assert!(is_opening_bracket('{'));
        assert!(!is_opening_bracket(')'));
        assert!(!is_opening_bracket('a'));

        assert!(is_closing_bracket(')'));
        assert!(is_closing_bracket(']'));
        assert!(is_closing_bracket('}'));
        assert!(!is_closing_bracket('('));
        assert!(!is_closing_bracket('a'));
    }

    #[test]
    fn test_get_bracket_pair() {
        assert_eq!(get_bracket_pair('('), Some(('(', ')', true)));
        assert_eq!(get_bracket_pair(')'), Some(('(', ')', false)));
        assert_eq!(get_bracket_pair('['), Some(('[', ']', true)));
        assert_eq!(get_bracket_pair(']'), Some(('[', ']', false)));
        assert_eq!(get_bracket_pair('a'), None);
    }

    #[test]
    fn test_find_matching_bracket_forward() {
        let buffer = Buffer::from_str_test("(hello)");
        let overlay = BracketHighlightOverlay::new();

        let result = overlay.find_matching_bracket(&buffer, 0, '(', ')', true, &[]);
        assert_eq!(result, Some(6));
    }

    #[test]
    fn test_find_matching_bracket_backward() {
        let buffer = Buffer::from_str_test("(hello)");
        let overlay = BracketHighlightOverlay::new();

        let result = overlay.find_matching_bracket(&buffer, 6, '(', ')', false, &[]);
        assert_eq!(result, Some(0));
    }

    #[test]
    fn test_find_matching_bracket_nested() {
        let buffer = Buffer::from_str_test("((inner))");
        let overlay = BracketHighlightOverlay::new();

        // Outer opening bracket should match outer closing
        let result = overlay.find_matching_bracket(&buffer, 0, '(', ')', true, &[]);
        assert_eq!(result, Some(8));

        // Inner opening bracket should match inner closing
        let result = overlay.find_matching_bracket(&buffer, 1, '(', ')', true, &[]);
        assert_eq!(result, Some(7));
    }

    #[test]
    fn test_nesting_depth() {
        let buffer = Buffer::from_str_test("((()))");
        let overlay = BracketHighlightOverlay::new();

        // Outermost opening bracket: depth 0
        assert_eq!(overlay.calculate_nesting_depth(&buffer, 0, true, &[]), 0);

        // Second level opening bracket: depth 1
        assert_eq!(overlay.calculate_nesting_depth(&buffer, 1, true, &[]), 1);

        // Third level opening bracket: depth 2
        assert_eq!(overlay.calculate_nesting_depth(&buffer, 2, true, &[]), 2);
    }

    #[test]
    fn test_nesting_depth_mixed_types() {
        let buffer = Buffer::from_str_test("({[]})");
        let overlay = BracketHighlightOverlay::new();

        assert_eq!(overlay.calculate_nesting_depth(&buffer, 0, true, &[]), 0);
        assert_eq!(overlay.calculate_nesting_depth(&buffer, 1, true, &[]), 1);
        assert_eq!(overlay.calculate_nesting_depth(&buffer, 2, true, &[]), 2);
        assert_eq!(overlay.calculate_nesting_depth(&buffer, 3, false, &[]), 2);
        assert_eq!(overlay.calculate_nesting_depth(&buffer, 4, false, &[]), 1);
        assert_eq!(overlay.calculate_nesting_depth(&buffer, 5, false, &[]), 0);
    }

    #[test]
    fn test_pos_in_ranges() {
        let ranges = [2..5, 8..10];
        assert!(!pos_in_ranges(&ranges, 1));
        assert!(pos_in_ranges(&ranges, 2)); // inclusive start
        assert!(pos_in_ranges(&ranges, 4));
        assert!(!pos_in_ranges(&ranges, 5)); // exclusive end
        assert!(!pos_in_ranges(&ranges, 6));
        assert!(pos_in_ranges(&ranges, 9));
        assert!(!pos_in_ranges(&ranges, 10));
        assert!(!pos_in_ranges(&[], 0));
    }

    #[test]
    fn test_find_matching_bracket_skips_comment_bracket() {
        // `( # ) )` — the first `)` sits inside a "comment" range and must be
        // ignored, so the opening `(` matches the second `)`.
        let buffer = Buffer::from_str_test("(a)b)");
        let overlay = BracketHighlightOverlay::new();

        // Without skipping, `(` at 0 matches the `)` at 2.
        assert_eq!(
            overlay.find_matching_bracket(&buffer, 0, '(', ')', true, &[]),
            Some(2)
        );

        // Treat byte 2 (the first `)`) as inside a comment: it should be
        // skipped, so the match becomes the `)` at 4.
        assert_eq!(
            overlay.find_matching_bracket(&buffer, 0, '(', ')', true, &[2..3]),
            Some(4)
        );
    }

    #[test]
    fn test_nesting_depth_skips_comment_brackets() {
        // `((x))` with the inner `(` at byte 1 treated as a comment bracket.
        let buffer = Buffer::from_str_test("((x))");
        let overlay = BracketHighlightOverlay::new();

        // Normally the bracket at byte 2 would be depth 2.
        assert_eq!(overlay.calculate_nesting_depth(&buffer, 2, true, &[]), 2);

        // With byte 1 skipped, only the outer `(` counts -> depth 1.
        assert_eq!(
            overlay.calculate_nesting_depth(&buffer, 2, true, &[1..2]),
            1
        );
    }
}
