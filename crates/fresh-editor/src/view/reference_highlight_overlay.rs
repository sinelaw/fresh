//! Reference highlighting using the overlay system
//!
//! This module manages word occurrence highlighting through overlays that
//! automatically adjust their positions when text is edited. Unlike the
//! old cache-based approach, overlays use markers that move with the text.

use crate::model::buffer::Buffer;
use crate::model::marker::MarkerList;
use crate::primitives::reference_highlighter::ReferenceHighlighter;
use crate::view::overlay::{Overlay, OverlayFace, OverlayManager, OverlayNamespace};
use ratatui::style::Color;
use std::time::{Duration, Instant};

/// Default debounce delay for reference highlighting (150ms)
pub const DEFAULT_DEBOUNCE_MS: u64 = 150;

/// Namespace for reference highlight overlays
pub fn reference_highlight_namespace() -> OverlayNamespace {
    OverlayNamespace::from_string("reference-highlight".to_string())
}

/// Manager for reference highlight overlays
///
/// Tracks the current word under cursor and manages overlays that highlight
/// all occurrences. Overlays automatically adjust positions via markers.
pub struct ReferenceHighlightOverlay {
    /// The word currently highlighted (overlays exist for this word)
    current_word: Option<String>,
    /// The word we're waiting to highlight (pending debounce)
    pending_word: Option<String>,
    /// When cursor moved to a different word (for debouncing)
    word_changed_at: Option<Instant>,
    /// Debounce delay before updating highlights
    debounce_delay: Duration,
    /// Whether highlighting is enabled
    pub enabled: bool,
}

impl ReferenceHighlightOverlay {
    /// Create a new reference highlight overlay manager
    pub fn new() -> Self {
        Self {
            current_word: None,
            pending_word: None,
            word_changed_at: None,
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

    /// Update reference highlights based on cursor position
    ///
    /// This should be called on each render. It will:
    /// 1. Check if cursor is on a different word
    /// 2. Debounce rapid cursor movements
    /// 3. Update overlays when debounce period elapses
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
        viewport_start: usize,
        viewport_end: usize,
        context_bytes: usize,
        highlight_color: Color,
    ) -> bool {
        if !self.enabled {
            return false;
        }

        let now = Instant::now();

        // Get the word under cursor
        let word_under_cursor = get_word_at_position(buffer, cursor_position);

        // Check if word changed from what we're tracking
        let word_changed = word_under_cursor != self.pending_word;

        if word_changed {
            // Word changed - record time and new pending word
            self.word_changed_at = Some(now);
            self.pending_word = word_under_cursor;
            // Keep showing current overlays (they auto-adjust via markers)
            return false;
        }

        // Word is same as pending - check if we should apply
        if let Some(changed_at) = self.word_changed_at {
            if now.duration_since(changed_at) >= self.debounce_delay {
                // Debounce period elapsed - update overlays
                self.current_word = self.pending_word.clone();
                self.word_changed_at = None;

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

        false
    }

    /// Apply highlights for the current word
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

        // If no word under cursor, we're done
        if self.current_word.is_none() {
            return;
        }

        // Compute occurrences
        highlighter.highlight_color = highlight_color;
        let spans = highlighter.highlight_occurrences(
            buffer,
            cursor_position,
            viewport_start,
            viewport_end,
            context_bytes,
        );

        // Create overlays for each occurrence
        for span in spans {
            let face = OverlayFace::Background { color: span.color };
            let overlay = Overlay::with_namespace(marker_list, span.range, face, ns.clone())
                .with_priority_value(5); // Lower priority than diagnostics

            overlays.add(overlay);
        }
    }

    /// Check if a redraw is needed (debounce timer pending)
    pub fn needs_redraw(&self) -> Option<Duration> {
        self.word_changed_at.map(|changed_at| {
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
        self.current_word = None;
        self.pending_word = None;
        self.word_changed_at = None;
    }

    /// Check if currently debouncing
    pub fn is_debouncing(&self) -> bool {
        self.word_changed_at.is_some()
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
