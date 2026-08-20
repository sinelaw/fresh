//! Reference highlighting using the overlay system
//!
//! This module manages occurrence highlighting through overlays that
//! automatically adjust their positions when text is edited. Unlike the
//! old cache-based approach, overlays use markers that move with the text.
//!
//! What gets highlighted depends on whether there is a selection, following
//! VSCode/Zed: with no selection the word under the cursor drives the
//! highlight, and while a selection is active it takes over — the cursor-word
//! highlight is suppressed and the occurrences of the *selected text* are
//! highlighted instead.

use crate::model::buffer::Buffer;
use crate::model::marker::MarkerList;
use crate::primitives::reference_highlighter::{
    is_matchable_selection, ReferenceHighlighter, MAX_SELECTION_MATCH_BYTES,
};
use crate::view::overlay::{Overlay, OverlayFace, OverlayManager, OverlayNamespace};
use ratatui::style::Color;
use std::ops::Range;
use std::time::{Duration, Instant};

/// Default debounce delay for reference highlighting (150ms)
pub const DEFAULT_DEBOUNCE_MS: u64 = 150;

/// Namespace for reference highlight overlays
pub fn reference_highlight_namespace() -> OverlayNamespace {
    OverlayNamespace::from_string("reference-highlight".to_string())
}

/// What the occurrence highlight is currently keyed on
#[derive(Clone, Debug, PartialEq, Eq)]
enum HighlightTarget {
    /// No selection: the word under the cursor.
    Word(String),
    /// An active selection takes over from the cursor word. `text` is empty
    /// when the selection is not matchable (multi-line, blank, or oversized);
    /// the target still exists so the cursor-word highlight stays suppressed.
    Selection { text: String, range: Range<usize> },
}

/// Manager for reference highlight overlays
///
/// Tracks the current highlight target (cursor word, or the active selection)
/// and manages overlays that highlight all of its occurrences. Overlays
/// automatically adjust positions via markers.
pub struct ReferenceHighlightOverlay {
    /// The target currently highlighted (overlays exist for it)
    current_target: Option<HighlightTarget>,
    /// The target we're waiting to highlight (pending debounce)
    pending_target: Option<HighlightTarget>,
    /// When the target changed (for debouncing)
    target_changed_at: Option<Instant>,
    /// Debounce delay before updating highlights
    debounce_delay: Duration,
    /// Whether highlighting is enabled
    pub enabled: bool,
}

impl ReferenceHighlightOverlay {
    /// Create a new reference highlight overlay manager
    pub fn new() -> Self {
        Self {
            current_target: None,
            pending_target: None,
            target_changed_at: None,
            debounce_delay: Duration::from_millis(DEFAULT_DEBOUNCE_MS),
            enabled: true,
        }
    }

    /// Create with custom debounce delay
    pub fn with_debounce(delay_ms: u64) -> Self {
        Self {
            debounce_delay: Duration::from_millis(delay_ms),
            ..Self::new()
        }
    }

    /// Update reference highlights based on cursor position and selection
    ///
    /// This should be called on each render. It will:
    /// 1. Determine the highlight target (the selection, else the cursor word)
    /// 2. Debounce rapid cursor/selection movements
    /// 3. Update overlays when debounce period elapses
    ///
    /// `selection` is the primary cursor's selection range, if any. While it
    /// is set the cursor-word highlight is dropped immediately — a stale word
    /// highlight next to a selection makes the extent of the selection
    /// impossible to read (issue #3011).
    ///
    /// Returns true if overlays were updated
    #[allow(clippy::too_many_arguments)]
    pub fn update(
        &mut self,
        buffer: &Buffer,
        overlays: &mut OverlayManager,
        marker_list: &mut MarkerList,
        highlighter: &mut ReferenceHighlighter,
        cursor_position: usize,
        selection: Option<Range<usize>>,
        viewport_start: usize,
        viewport_end: usize,
        context_bytes: usize,
        highlight_color: Color,
    ) -> bool {
        if !self.enabled {
            return false;
        }

        let now = Instant::now();

        let target = match selection {
            Some(range) if !range.is_empty() => {
                Some(selection_target(buffer, range, highlighter.min_word_length))
            }
            _ => get_word_at_position(buffer, cursor_position).map(HighlightTarget::Word),
        };

        // A selection must not leave the previous cursor-word highlight on
        // screen while its own matches wait out the debounce.
        let mut updated = false;
        if matches!(target, Some(HighlightTarget::Selection { .. }))
            && matches!(self.current_target, Some(HighlightTarget::Word(_)))
        {
            overlays.clear_namespace(&reference_highlight_namespace(), marker_list);
            self.current_target = None;
            updated = true;
        }

        // Check if the target changed from what we're tracking
        if target != self.pending_target {
            // Target changed - record time and new pending target
            self.target_changed_at = Some(now);
            self.pending_target = target;
            // Keep showing current overlays (they auto-adjust via markers)
            return updated;
        }

        // Target is same as pending - check if we should apply
        if let Some(changed_at) = self.target_changed_at {
            if now.duration_since(changed_at) >= self.debounce_delay {
                // Debounce period elapsed - update overlays
                self.current_target = self.pending_target.clone();
                self.target_changed_at = None;

                self.apply_highlights(
                    buffer,
                    overlays,
                    marker_list,
                    highlighter,
                    cursor_position,
                    viewport_start,
                    viewport_end,
                    context_bytes,
                    highlight_color,
                );
                return true;
            }
        }

        updated
    }

    /// Apply highlights for the current target (cursor word or selection)
    #[allow(clippy::too_many_arguments)]
    fn apply_highlights(
        &self,
        buffer: &Buffer,
        overlays: &mut OverlayManager,
        marker_list: &mut MarkerList,
        highlighter: &mut ReferenceHighlighter,
        cursor_position: usize,
        viewport_start: usize,
        viewport_end: usize,
        context_bytes: usize,
        highlight_color: Color,
    ) {
        let ns = reference_highlight_namespace();

        // Clear existing reference highlight overlays
        overlays.clear_namespace(&ns, marker_list);

        highlighter.highlight_color = highlight_color;

        // Compute occurrences of whatever the current target is
        let spans = match &self.current_target {
            // Nothing under the cursor and no selection - we're done
            None => return,
            Some(HighlightTarget::Word(_)) => highlighter.highlight_occurrences(
                buffer,
                cursor_position,
                viewport_start,
                viewport_end,
                context_bytes,
            ),
            Some(HighlightTarget::Selection { text, range }) => {
                highlighter.selection_matches(buffer, text, range, viewport_start, viewport_end)
            }
        };

        // Create overlays for each occurrence.
        //
        // `ThemedStyle` rather than a plain `Background`: the highlight has to
        // be a background the eye actually catches, which means it cannot hug
        // `editor.bg`, which in turn means it lands where syntax foregrounds
        // live. `fg_on_low_contrast` repairs exactly the cells that would
        // otherwise become unreadable, so visibility and legibility stop being
        // a trade (#3011).
        for span in spans {
            let face = OverlayFace::ThemedStyle {
                fallback_style: ratatui::style::Style::default().bg(span.color),
                fg_theme: None,
                bg_theme: Some("ui.semantic_highlight_bg".to_string()),
                fg_on_collision_only: false,
                fg_on_low_contrast: true,
            };
            let overlay = Overlay::with_namespace(marker_list, span.range, face, ns.clone())
                .with_priority_value(5) // Lower priority than diagnostics
                .with_theme_key("ui.semantic_highlight_bg");

            overlays.add(overlay);
        }
    }

    /// Check if a redraw is needed (debounce timer pending)
    pub fn needs_redraw(&self) -> Option<Duration> {
        self.target_changed_at.map(|changed_at| {
            let elapsed = changed_at.elapsed();
            if elapsed >= self.debounce_delay {
                Duration::ZERO
            } else {
                self.debounce_delay - elapsed
            }
        })
    }

    /// Force clear all highlights (e.g., when switching buffers)
    pub fn clear(&mut self, overlays: &mut OverlayManager, marker_list: &mut MarkerList) {
        let ns = reference_highlight_namespace();
        overlays.clear_namespace(&ns, marker_list);
        self.current_target = None;
        self.pending_target = None;
        self.target_changed_at = None;
    }

    /// Check if currently debouncing
    pub fn is_debouncing(&self) -> bool {
        self.target_changed_at.is_some()
    }

    /// Get the debounce delay
    pub fn debounce_delay(&self) -> Duration {
        self.debounce_delay
    }
}

impl Default for ReferenceHighlightOverlay {
    fn default() -> Self {
        Self::new()
    }
}

/// Build the highlight target for an active selection
///
/// Oversized selections keep a target (so the cursor-word highlight stays
/// suppressed) but carry no text: reading them in full would be a scan
/// proportional to the selection, not to the viewport.
fn selection_target(buffer: &Buffer, range: Range<usize>, min_len: usize) -> HighlightTarget {
    let text = if range.len() <= MAX_SELECTION_MATCH_BYTES {
        let bytes = buffer.slice_bytes(range.clone());
        String::from_utf8(bytes)
            .ok()
            .filter(|text| is_matchable_selection(text, min_len))
            .unwrap_or_default()
    } else {
        String::new()
    };

    HighlightTarget::Selection { text, range }
}

/// Get the word at the given position in the buffer
fn get_word_at_position(buffer: &crate::model::buffer::Buffer, position: usize) -> Option<String> {
    use crate::primitives::word_navigation::{find_word_end, find_word_start, is_word_char};

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
    } else {
        false
    };

    if !is_on_word {
        return None;
    }

    // Find word boundaries
    let start = find_word_start(buffer, position);
    let end = find_word_end(buffer, position);

    if start < end {
        let word_bytes = buffer.slice_bytes(start..end);
        std::str::from_utf8(&word_bytes).ok().map(|s| s.to_string())
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::buffer::Buffer;

    /// Text used by the selection tests. "beta" appears on both lines, so a
    /// cursor-word highlight and a selection highlight cover different cells.
    const SAMPLE: &str = "alpha beta gamma alpha\nbeta alpha delta beta\n";

    /// Drive the debounced pass to completion for one cursor/selection state
    /// and return the ranges it highlighted.
    fn highlighted_ranges(
        manager: &mut ReferenceHighlightOverlay,
        buffer: &Buffer,
        overlays: &mut OverlayManager,
        markers: &mut MarkerList,
        highlighter: &mut ReferenceHighlighter,
        cursor: usize,
        selection: Option<Range<usize>>,
    ) -> Vec<Range<usize>> {
        // First pass records the target, second applies it (debounce is 0).
        for _ in 0..2 {
            manager.update(
                buffer,
                overlays,
                markers,
                highlighter,
                cursor,
                selection.clone(),
                0,
                buffer.len(),
                100_000,
                Color::Rgb(60, 60, 80),
            );
        }

        let mut ranges: Vec<Range<usize>> = overlays
            .query_viewport(0, buffer.len(), markers)
            .into_iter()
            .filter(|(overlay, _)| {
                overlay.namespace.as_ref() == Some(&reference_highlight_namespace())
            })
            .map(|(_, range)| range)
            .collect();
        ranges.sort_by_key(|r| r.start);
        ranges
    }

    /// Issue #3011: while a selection is active the cursor-word highlight has
    /// to give way to the selection's own matches — otherwise the highlighted
    /// word and the selection overlap and the selection's extent is unreadable.
    #[test]
    fn selection_replaces_word_highlight_with_selection_matches() {
        let buffer = Buffer::from_str_test(SAMPLE);
        let mut overlays = OverlayManager::new();
        let mut markers = MarkerList::new();
        let mut highlighter = ReferenceHighlighter::new();
        let mut manager = ReferenceHighlightOverlay::with_debounce(0);

        // Cursor inside "beta" on line 1, no selection: every "beta" highlights.
        let word_ranges = highlighted_ranges(
            &mut manager,
            &buffer,
            &mut overlays,
            &mut markers,
            &mut highlighter,
            7,
            None,
        );
        assert_eq!(word_ranges, vec![6..10, 23..27, 40..44]);

        // Select "bet" out of the first "beta": the word highlight is gone and
        // the other "bet" runs are highlighted instead (the selection itself
        // is left to the selection background).
        let selection_ranges = highlighted_ranges(
            &mut manager,
            &buffer,
            &mut overlays,
            &mut markers,
            &mut highlighter,
            9,
            Some(6..9),
        );
        assert_eq!(selection_ranges, vec![23..26, 40..43]);
    }

    /// The stale word highlight must not survive even one frame of the
    /// selection: it is dropped before the new matches' debounce runs.
    #[test]
    fn word_highlight_is_dropped_as_soon_as_a_selection_appears() {
        let buffer = Buffer::from_str_test(SAMPLE);
        let mut overlays = OverlayManager::new();
        let mut markers = MarkerList::new();
        let mut highlighter = ReferenceHighlighter::new();
        let mut manager = ReferenceHighlightOverlay::with_debounce(0);

        for _ in 0..2 {
            manager.update(
                &buffer,
                &mut overlays,
                &mut markers,
                &mut highlighter,
                7,
                None,
                0,
                buffer.len(),
                100_000,
                Color::Rgb(60, 60, 80),
            );
        }
        assert_ne!(overlays.len(), 0, "cursor word should be highlighted");

        manager.update(
            &buffer,
            &mut overlays,
            &mut markers,
            &mut highlighter,
            9,
            Some(6..9),
            0,
            buffer.len(),
            100_000,
            Color::Rgb(60, 60, 80),
        );
        assert_eq!(
            overlays.len(),
            0,
            "the cursor-word highlight must be cleared on the first frame of a selection, \
             before the selection's own matches are computed"
        );
    }

    /// Multi-line selections only suppress: matching a multi-line run would
    /// paint whole regions of the viewport.
    ///
    /// The fixture repeats the selected two-line run, so without the guard
    /// the second copy would be highlighted — a fixture where the run occurs
    /// only once would pass with or without it.
    #[test]
    fn multiline_selection_only_suppresses() {
        let buffer = Buffer::from_str_test("beta gamma\nalpha delta\nbeta gamma\nalpha delta\n");
        let mut overlays = OverlayManager::new();
        let mut markers = MarkerList::new();
        let mut highlighter = ReferenceHighlighter::new();
        let mut manager = ReferenceHighlightOverlay::with_debounce(0);

        // "beta gamma\nalpha" — repeated verbatim at 23..39.
        let selection = 0..16;
        assert_eq!(
            String::from_utf8(buffer.slice_bytes(23..39)).unwrap(),
            String::from_utf8(buffer.slice_bytes(selection.clone())).unwrap(),
            "fixture must repeat the selected run, or the guard is untested"
        );

        let ranges = highlighted_ranges(
            &mut manager,
            &buffer,
            &mut overlays,
            &mut markers,
            &mut highlighter,
            selection.end,
            Some(selection),
        );
        assert!(ranges.is_empty(), "got {ranges:?}");
    }

    /// A one-character selection must not turn into an overlay per occurrence.
    #[test]
    fn single_character_selection_only_suppresses() {
        let buffer = Buffer::from_str_test(SAMPLE);
        let mut overlays = OverlayManager::new();
        let mut markers = MarkerList::new();
        let mut highlighter = ReferenceHighlighter::new();
        let mut manager = ReferenceHighlightOverlay::with_debounce(0);

        // "a" of "alpha" — present dozens of times in the fixture.
        let ranges = highlighted_ranges(
            &mut manager,
            &buffer,
            &mut overlays,
            &mut markers,
            &mut highlighter,
            1,
            Some(0..1),
        );
        assert!(ranges.is_empty(), "got {ranges:?}");
    }

    /// An oversized selection is never read into memory, so it cannot be
    /// matched — but it still suppresses the cursor-word highlight.
    #[test]
    fn oversized_selection_only_suppresses() {
        let long = "beta ".repeat(MAX_SELECTION_MATCH_BYTES);
        let buffer = Buffer::from_str_test(&long);
        let mut overlays = OverlayManager::new();
        let mut markers = MarkerList::new();
        let mut highlighter = ReferenceHighlighter::new();
        let mut manager = ReferenceHighlightOverlay::with_debounce(0);

        let selection = 0..(MAX_SELECTION_MATCH_BYTES + 1);
        let ranges = highlighted_ranges(
            &mut manager,
            &buffer,
            &mut overlays,
            &mut markers,
            &mut highlighter,
            selection.end,
            Some(selection),
        );
        assert!(ranges.is_empty(), "got {ranges:?}");
    }

    #[test]
    fn test_get_word_at_position() {
        let buffer = Buffer::from_str_test("hello world test");

        // Middle of "hello"
        let word = get_word_at_position(&buffer, 2);
        assert_eq!(word, Some("hello".to_string()));

        // On space - no word
        let word = get_word_at_position(&buffer, 5);
        assert_eq!(word, None);

        // Start of "world"
        let word = get_word_at_position(&buffer, 6);
        assert_eq!(word, Some("world".to_string()));
    }
}
