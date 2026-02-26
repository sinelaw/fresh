//! Bracket matching highlight using the overlay system
//!
//! This module manages bracket pair highlighting through overlays.
//! When the cursor is on a bracket, the matching bracket is highlighted.
//! Optional rainbow colors can be applied based on nesting depth.

use crate::model::buffer::Buffer;
use crate::model::marker::MarkerList;
use crate::view::overlay::{Overlay, OverlayFace, OverlayManager, OverlayNamespace};
use ratatui::style::Color;

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

/// Bracket types we match
const BRACKET_PAIRS: &[(char, char)] = &[('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')];

/// Maximum number of bytes to scan for bracket matching/nesting depth.
/// Prevents O(n) scans on huge files from hanging the editor.
const MAX_BRACKET_SEARCH_BYTES: usize = 1_000_000;

/// Chunk size for bulk reads during bracket scanning.
const BRACKET_SCAN_CHUNK: usize = 16 * 1024;

/// Check if a character is an opening bracket
#[cfg(test)]
fn is_opening_bracket(ch: char) -> bool {
    BRACKET_PAIRS.iter().any(|(open, _)| *open == ch)
}

/// Check if a character is a closing bracket
#[cfg(test)]
fn is_closing_bracket(ch: char) -> bool {
    BRACKET_PAIRS.iter().any(|(_, close)| *close == ch)
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
    pub rainbow_colors: Vec<Color>,
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
            rainbow_colors: DEFAULT_BRACKET_COLORS.to_vec(),
            match_color: Color::Rgb(255, 215, 0), // Gold
            last_cursor_pos: None,
        }
    }

    /// Update bracket highlights based on cursor position
    ///
    /// Returns true if overlays were updated
    pub fn update(
        &mut self,
        buffer: &Buffer,
        overlays: &mut OverlayManager,
        marker_list: &mut MarkerList,
        cursor_position: usize,
    ) -> bool {
        if !self.enabled {
            return false;
        }

        // Check if cursor position changed
        if self.last_cursor_pos == Some(cursor_position) {
            return false;
        }
        self.last_cursor_pos = Some(cursor_position);

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

        // Calculate nesting depth at cursor position for rainbow colors
        let depth = if self.rainbow_enabled {
            self.calculate_nesting_depth(buffer, cursor_position, opening, closing, forward)
        } else {
            0
        };

        // Find matching bracket
        let matching_pos =
            self.find_matching_bracket(buffer, cursor_position, opening, closing, forward);

        // Determine color based on depth
        let color = if self.rainbow_enabled && !self.rainbow_colors.is_empty() {
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

        true
    }

    /// Calculate the nesting depth of a bracket at a position
    fn calculate_nesting_depth(
        &self,
        buffer: &Buffer,
        position: usize,
        opening: char,
        closing: char,
        is_opening: bool,
    ) -> usize {
        // Only scan up to MAX_BRACKET_SEARCH_BYTES backwards to avoid O(n) on huge files.
        let scan_start = position.saturating_sub(MAX_BRACKET_SEARCH_BYTES);
        let open = opening as u8;
        let close = closing as u8;
        let mut depth: usize = 0;
        let mut pos = scan_start;

        while pos < position {
            let chunk_end = (pos + BRACKET_SCAN_CHUNK).min(position);
            let chunk = buffer.slice_bytes(pos..chunk_end);
            for &b in &chunk {
                if b == open {
                    depth += 1;
                } else if b == close {
                    depth = depth.saturating_sub(1);
                }
            }
            pos = chunk_end;
        }

        if is_opening {
            depth
        } else {
            depth.saturating_sub(1)
        }
    }

    /// Find the matching bracket (bounded to MAX_BRACKET_SEARCH_BYTES)
    fn find_matching_bracket(
        &self,
        buffer: &Buffer,
        position: usize,
        opening: char,
        closing: char,
        forward: bool,
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
        let ns = bracket_highlight_namespace();
        overlays.clear_namespace(&ns, marker_list);
        self.last_cursor_pos = None;
    }

    /// Force recalculation on next update
    pub fn invalidate(&mut self) {
        self.last_cursor_pos = None;
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

        let result = overlay.find_matching_bracket(&buffer, 0, '(', ')', true);
        assert_eq!(result, Some(6));
    }

    #[test]
    fn test_find_matching_bracket_backward() {
        let buffer = Buffer::from_str_test("(hello)");
        let overlay = BracketHighlightOverlay::new();

        let result = overlay.find_matching_bracket(&buffer, 6, '(', ')', false);
        assert_eq!(result, Some(0));
    }

    #[test]
    fn test_find_matching_bracket_nested() {
        let buffer = Buffer::from_str_test("((inner))");
        let overlay = BracketHighlightOverlay::new();

        // Outer opening bracket should match outer closing
        let result = overlay.find_matching_bracket(&buffer, 0, '(', ')', true);
        assert_eq!(result, Some(8));

        // Inner opening bracket should match inner closing
        let result = overlay.find_matching_bracket(&buffer, 1, '(', ')', true);
        assert_eq!(result, Some(7));
    }

    #[test]
    fn test_nesting_depth() {
        let buffer = Buffer::from_str_test("((()))");
        let overlay = BracketHighlightOverlay::new();

        // Outermost opening bracket: depth 0
        assert_eq!(
            overlay.calculate_nesting_depth(&buffer, 0, '(', ')', true),
            0
        );

        // Second level opening bracket: depth 1
        assert_eq!(
            overlay.calculate_nesting_depth(&buffer, 1, '(', ')', true),
            1
        );

        // Third level opening bracket: depth 2
        assert_eq!(
            overlay.calculate_nesting_depth(&buffer, 2, '(', ')', true),
            2
        );
    }
}
