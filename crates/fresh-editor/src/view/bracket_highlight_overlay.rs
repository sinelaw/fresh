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

/// The `config.editor` bracket toggles, resolved for one frame.
///
/// Carried through the render pipeline instead of being latched onto the
/// per-buffer overlay at construction time, so a settings change takes effect
/// on the next frame for every buffer without a separate invalidation pass.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BracketHighlightSettings {
    /// `editor.highlight_matching_brackets`: highlight the pair the cursor
    /// sits on. Also the master switch — `rainbow_brackets` documents that it
    /// requires this to be enabled.
    pub matching: bool,
    /// `editor.rainbow_brackets`: color brackets by nesting depth.
    pub rainbow: bool,
}

impl BracketHighlightSettings {
    /// Build from the editor config, applying the documented dependency of
    /// `rainbow_brackets` on `highlight_matching_brackets`.
    pub fn from_config(editor: &crate::config::EditorConfig) -> Self {
        Self {
            matching: editor.highlight_matching_brackets,
            rainbow: editor.highlight_matching_brackets && editor.rainbow_brackets,
        }
    }
}

impl Default for BracketHighlightSettings {
    fn default() -> Self {
        Self {
            matching: true,
            rainbow: true,
        }
    }
}

/// Manager for bracket highlight overlays
pub struct BracketHighlightOverlay {
    /// Colors to use for rainbow brackets (cycles through)
    pub rainbow_colors: [Color; 6],
    /// Default bracket match highlight color (when rainbow is disabled)
    pub match_color: Color,
    /// Last cursor position where we computed brackets
    last_cursor_pos: Option<usize>,
    /// Whether depth-colorization overlays are currently present. Lets a
    /// disabled pass clear them exactly once instead of re-clearing (and
    /// reporting "updated") on every frame.
    colorization_active: bool,
    /// What the last colorization pass was computed from, and the byte range
    /// it wrote into. A frame whose inputs match this does no work at all:
    /// the overlays it would produce are already in place, and re-deriving
    /// them costs the whole overlay set (see `update_colorization`).
    colorization_cache: Option<ColorizationCache>,
}

/// Inputs a colorization pass depends on, plus the range it wrote.
struct ColorizationCache {
    /// Bytes scanned for brackets.
    scan: Range<usize>,
    /// Byte range the overlays were written into — the union of this pass's
    /// scan range and the previous one's, which is what the next replace has
    /// to cover to retract what scrolled out of view.
    written: Range<usize>,
    buffer_version: u64,
    viewport: Range<usize>,
    colors: [Color; 6],
    skip_ranges: Vec<Range<usize>>,
    /// The overlay set's removal counters when this pass ran — global, and
    /// for the colorization namespace. Anything that could have removed
    /// these overlays (a buffer switch, a plugin's `clearAllOverlays`, a
    /// clear of this namespace) moves one of them, and the cached overlays
    /// can no longer be assumed present.
    removal_epochs: (u64, u64),
}

impl BracketHighlightOverlay {
    /// Create a new bracket highlight overlay manager
    pub fn new() -> Self {
        Self {
            rainbow_colors: DEFAULT_BRACKET_COLORS,
            match_color: Color::Rgb(255, 215, 0), // Gold
            last_cursor_pos: None,
            colorization_active: false,
            colorization_cache: None,
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
        settings: BracketHighlightSettings,
        cursor_position: usize,
        viewport_start: usize,
        viewport_end: usize,
        skip_ranges: &[Range<usize>],
    ) -> bool {
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
        if settings.rainbow {
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

        // Turning matching off has to retract the pair overlays a previously
        // enabled pass left behind — `last_cursor_pos` is Some exactly when
        // such overlays may exist, so this clears once and then no-ops.
        if !settings.matching {
            if self.last_cursor_pos.take().is_some() {
                overlays.clear_namespace(&bracket_highlight_namespace(), marker_list);
                updated = true;
            }
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
        let depth = if settings.rainbow {
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
        let color = if settings.rainbow {
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
        self.colorization_active = false;
    }

    /// Force recalculation on next update
    pub fn invalidate(&mut self) {
        self.last_cursor_pos = None;
    }

    /// Drop the depth-colorization overlays. Returns whether anything was
    /// actually retracted, so a permanently-disabled setting doesn't report a
    /// change on every frame.
    fn clear_colorization(
        &mut self,
        overlays: &mut OverlayManager,
        marker_list: &mut MarkerList,
    ) -> bool {
        if !self.colorization_active {
            return false;
        }
        let ns = bracket_colorization_namespace();
        overlays.clear_namespace(&ns, marker_list);
        self.colorization_active = false;
        // The overlays this cache describes are gone, so the next enabled
        // pass must rebuild rather than recognise its own inputs.
        self.colorization_cache = None;
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

        let ns = bracket_colorization_namespace();

        // Nothing this pass reads has moved since the last one, so the
        // overlays it would produce are the ones already on the buffer.
        // Without this a parked cursor re-derives them every frame, and the
        // replace below is what makes that expensive on a heavily-decorated
        // buffer (a review diff carries an overlay per line).
        if let Some(cache) = &self.colorization_cache {
            if cache.scan == (scan_start..scan_end)
                && cache.buffer_version == buffer.version()
                && cache.viewport == (viewport_start..viewport_end)
                && cache.colors == self.rainbow_colors
                && cache.skip_ranges == skip_ranges
                && cache.removal_epochs == overlays.removal_epochs_for(&ns)
            {
                return false;
            }
        }

        let bytes = buffer.slice_bytes(scan_start..scan_end);
        if bytes.is_empty() {
            return self.clear_colorization(overlays, marker_list);
        }

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

        self.colorization_active = !new_overlays.is_empty();
        // Replace over the scanned bytes plus whatever the previous pass
        // wrote — enough to retract the overlays that scrolled out of view,
        // and nothing more. Passing `0..buffer.len()` here (as this used to)
        // makes the replace's marker-tree query return *every* marker in the
        // buffer, so the cost of one frame grew with the whole overlay set
        // rather than with the viewport: ~230ms per frame on a 20k-line
        // review diff, against ~3ms for the rest of the frame.
        let written = match &self.colorization_cache {
            // An edit moved every marker after it, so the overlays this
            // namespace already owns are no longer where the cached range
            // says they are — one of them can have been pushed clear of it.
            // Retract across the buffer for that pass; edits are rare next
            // to frames, and this is the range the pass always used before.
            Some(cache) if cache.buffer_version != buffer.version() => 0..buffer.len(),
            Some(cache) => cache.written.start.min(scan_start)..cache.written.end.max(scan_end),
            None => scan_start..scan_end,
        };
        overlays.replace_range_in_namespace(&ns, &written, new_overlays, marker_list);
        self.colorization_cache = Some(ColorizationCache {
            scan: scan_start..scan_end,
            // The next pass has to cover what this one wrote, but not what
            // the one before it did — that has just been retracted.
            written: scan_start..scan_end,
            buffer_version: buffer.version(),
            viewport: viewport_start..viewport_end,
            colors: self.rainbow_colors,
            skip_ranges: skip_ranges.to_vec(),
            removal_epochs: overlays.removal_epochs_for(&ns),
        });
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

    /// Run one colorization pass over the whole buffer as the renderer
    /// would, returning whether it did any work.
    fn colorize_frame(
        overlay: &mut BracketHighlightOverlay,
        buffer: &Buffer,
        overlays: &mut OverlayManager,
        markers: &mut MarkerList,
    ) -> bool {
        overlay.update_colorization(buffer, overlays, markers, 0, buffer.len(), &[])
    }

    /// A frame that changed nothing the pass reads must do nothing. Without
    /// this the pass re-derives its overlays every frame, and the replace
    /// that publishes them scans the buffer's whole overlay set — which is
    /// what made a large review diff cost ~230ms per frame instead of ~3ms.
    #[test]
    fn colorization_skips_a_frame_whose_inputs_have_not_moved() {
        let buffer = Buffer::from_str_test("fn a() { b(); }\n");
        let mut overlays = OverlayManager::new();
        let mut markers = MarkerList::new();
        let mut overlay = BracketHighlightOverlay::new();

        assert!(
            colorize_frame(&mut overlay, &buffer, &mut overlays, &mut markers),
            "the first pass has overlays to publish"
        );
        let after_first = overlays.len();
        assert!(after_first > 0, "brackets in view should be colorized");

        assert!(
            !colorize_frame(&mut overlay, &buffer, &mut overlays, &mut markers),
            "an unchanged frame must not re-derive the colorization"
        );
        assert_eq!(
            overlays.len(),
            after_first,
            "and must leave the overlays it published alone"
        );
    }

    /// The cache is a claim about overlays still being on the buffer, so
    /// anything that removes them has to break it — otherwise the brackets
    /// stay uncolored until some unrelated input happens to move.
    #[test]
    fn colorization_rebuilds_after_the_overlays_are_cleared() {
        let buffer = Buffer::from_str_test("fn a() { b(); }\n");
        let mut overlays = OverlayManager::new();
        let mut markers = MarkerList::new();
        let mut overlay = BracketHighlightOverlay::new();

        colorize_frame(&mut overlay, &buffer, &mut overlays, &mut markers);
        let published = overlays.len();
        assert!(published > 0);

        // Something else wipes the buffer's overlays — a buffer switch, or a
        // plugin calling `clearAllOverlays`.
        overlays.clear(&mut markers);
        assert_eq!(overlays.len(), 0);

        assert!(
            colorize_frame(&mut overlay, &buffer, &mut overlays, &mut markers),
            "a cleared buffer must be re-colorized, not assumed current"
        );
        assert_eq!(overlays.len(), published);
    }

    /// An edit moves the buffer version, and the brackets with it.
    #[test]
    fn colorization_rebuilds_after_an_edit() {
        let mut buffer = Buffer::from_str_test("fn a() { b(); }\n");
        let mut overlays = OverlayManager::new();
        let mut markers = MarkerList::new();
        let mut overlay = BracketHighlightOverlay::new();

        colorize_frame(&mut overlay, &buffer, &mut overlays, &mut markers);
        buffer.insert(0, "{}\n");

        assert!(
            colorize_frame(&mut overlay, &buffer, &mut overlays, &mut markers),
            "an edited buffer must be re-colorized"
        );
    }

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
