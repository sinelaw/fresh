//! Debounced cache for semantic highlighting
//!
//! Semantic highlighting (finding all occurrences of the word under cursor)
//! can be expensive due to tree-sitter parsing. This module provides a
//! debounced cache that only recomputes highlights after the cursor has
//! been stable for a configurable delay.

use crate::model::buffer::Buffer;
use crate::primitives::highlighter::HighlightSpan;
use crate::primitives::reference_highlighter::ReferenceHighlighter;
use ratatui::style::Color;
use std::time::{Duration, Instant};

/// Default debounce delay for semantic highlighting (150ms)
pub const DEFAULT_DEBOUNCE_MS: u64 = 150;

/// Debounced cache for semantic highlight spans
pub struct ReferenceHighlightCache {
    /// Cached highlight spans from the last computation
    cached_spans: Vec<HighlightSpan>,
    /// Cursor position when cache was computed
    cached_cursor: Option<usize>,
    /// Viewport range when cache was computed (start, end)
    cached_viewport: Option<(usize, usize)>,
    /// When the cursor position last changed
    cursor_changed_at: Option<Instant>,
    /// Debounce delay before computing highlights
    debounce_delay: Duration,
}

impl ReferenceHighlightCache {
    /// Create a new cache with default debounce delay
    pub fn new() -> Self {
        Self {
            cached_spans: Vec::new(),
            cached_cursor: None,
            cached_viewport: None,
            cursor_changed_at: None,
            debounce_delay: Duration::from_millis(DEFAULT_DEBOUNCE_MS),
        }
    }

    /// Create a new cache with custom debounce delay
    pub fn with_debounce(delay_ms: u64) -> Self {
        Self {
            debounce_delay: Duration::from_millis(delay_ms),
            ..Self::new()
        }
    }

    /// Get semantic highlights, using cache when appropriate
    ///
    /// Returns cached highlights if cursor hasn't moved to a new word.
    /// Recomputes when cursor moves to a different word and debounce period elapses.
    ///
    /// # Arguments
    /// * `highlighter` - The semantic highlighter to use for computation
    /// * `buffer` - The text buffer
    /// * `cursor_position` - Current cursor byte position
    /// * `viewport_start` - Start byte offset of visible viewport
    /// * `viewport_end` - End byte offset of visible viewport
    /// * `context_bytes` - Number of bytes before/after viewport to parse for context
    /// * `highlight_color` - Color to use for highlights
    ///
    /// # Returns
    /// A tuple of (highlights, needs_redraw) where needs_redraw is true if
    /// the caller should schedule a redraw after debounce_delay.
    pub fn get_highlights(
        &mut self,
        highlighter: &mut ReferenceHighlighter,
        buffer: &Buffer,
        cursor_position: usize,
        viewport_start: usize,
        viewport_end: usize,
        context_bytes: usize,
        highlight_color: Color,
    ) -> &[HighlightSpan] {
        let now = Instant::now();
        let viewport = (viewport_start, viewport_end);

        // First call - compute immediately
        if self.cached_cursor.is_none() {
            self.cached_cursor = Some(cursor_position);
            self.cached_viewport = Some(viewport);
            highlighter.highlight_color = highlight_color;
            self.cached_spans = highlighter.highlight_occurrences(
                buffer,
                cursor_position,
                viewport_start,
                viewport_end,
                context_bytes,
            );
            return &self.cached_spans;
        }

        // Check if cursor position changed
        let cursor_changed = self.cached_cursor != Some(cursor_position);
        let viewport_changed = self.cached_viewport != Some(viewport);

        if cursor_changed {
            // Cursor moved - record the time but DON'T clear cache yet
            // This keeps old highlights visible during rapid movement
            self.cursor_changed_at = Some(now);
            self.cached_cursor = Some(cursor_position);

            if viewport_changed {
                self.cached_viewport = Some(viewport);
            }

            // Return existing cache (may be stale, but better than nothing)
            return &self.cached_spans;
        }

        // Cursor hasn't changed - check if we should compute
        if let Some(changed_at) = self.cursor_changed_at {
            if now.duration_since(changed_at) >= self.debounce_delay {
                // Debounce period elapsed - compute highlights
                highlighter.highlight_color = highlight_color;
                self.cached_spans = highlighter.highlight_occurrences(
                    buffer,
                    cursor_position,
                    viewport_start,
                    viewport_end,
                    context_bytes,
                );
                self.cached_viewport = Some(viewport);
                // Clear the changed_at so we don't recompute every frame
                self.cursor_changed_at = None;
            }
            // else: still in debounce period, return current cache
        } else if viewport_changed {
            // Viewport changed but cursor didn't - recompute for new viewport
            highlighter.highlight_color = highlight_color;
            self.cached_spans = highlighter.highlight_occurrences(
                buffer,
                cursor_position,
                viewport_start,
                viewport_end,
                context_bytes,
            );
            self.cached_viewport = Some(viewport);
        }

        &self.cached_spans
    }

    /// Check if a redraw is needed (debounce timer pending)
    ///
    /// Returns Some(remaining_time) if a redraw should be scheduled,
    /// None if no redraw is needed.
    pub fn needs_redraw(&self) -> Option<Duration> {
        self.cursor_changed_at.map(|changed_at| {
            let elapsed = changed_at.elapsed();
            if elapsed >= self.debounce_delay {
                Duration::ZERO
            } else {
                self.debounce_delay - elapsed
            }
        })
    }

    /// Invalidate the cache (e.g., when buffer content changes)
    pub fn invalidate(&mut self) {
        self.cached_spans.clear();
        self.cached_cursor = None;
        self.cached_viewport = None;
        self.cursor_changed_at = None;
    }

    /// Check if highlights are currently being debounced
    pub fn is_debouncing(&self) -> bool {
        self.cursor_changed_at.is_some()
    }

    /// Get the current debounce delay
    pub fn debounce_delay(&self) -> Duration {
        self.debounce_delay
    }

    /// Set a new debounce delay
    pub fn set_debounce_delay(&mut self, delay: Duration) {
        self.debounce_delay = delay;
    }
}

impl Default for ReferenceHighlightCache {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::buffer::Buffer;
    use crate::primitives::reference_highlighter::ReferenceHighlighter;
    use std::thread::sleep;

    #[test]
    fn test_raw_highlighter_works() {
        // Verify the underlying highlighter works without cache
        let mut highlighter = ReferenceHighlighter::new();
        let buffer = Buffer::from_str_test("hello world hello");

        let spans = highlighter.highlight_occurrences(&buffer, 0, 0, 17, 1000);
        assert!(
            !spans.is_empty(),
            "Raw highlighter should find 'hello' occurrences"
        );
    }

    #[test]
    fn test_cache_computes_on_first_call() {
        let mut cache = ReferenceHighlightCache::with_debounce(100);
        let mut highlighter = ReferenceHighlighter::new();
        let buffer = Buffer::from_str_test("hello world hello");
        let color = Color::Rgb(60, 60, 80);

        // First call - should compute immediately
        let spans = cache.get_highlights(&mut highlighter, &buffer, 0, 0, 17, 1000, color);
        assert!(!spans.is_empty(), "Should compute on first call");
    }

    #[test]
    fn test_cache_returns_stale_during_debounce() {
        let mut cache = ReferenceHighlightCache::with_debounce(100);
        let mut highlighter = ReferenceHighlighter::new();
        let buffer = Buffer::from_str_test("hello world hello");
        let color = Color::Rgb(60, 60, 80);

        // First call at position 0 (on "hello") - computes
        let spans = cache.get_highlights(&mut highlighter, &buffer, 0, 0, 17, 1000, color);
        assert!(!spans.is_empty(), "Should compute on first call");
        let first_len = spans.len();

        // Move cursor to position 6 (on "world") - returns stale cache
        let spans = cache.get_highlights(&mut highlighter, &buffer, 6, 0, 17, 1000, color);
        assert_eq!(
            spans.len(),
            first_len,
            "Should return stale cache during debounce"
        );

        // Check that we need a redraw
        assert!(
            cache.needs_redraw().is_some(),
            "Should signal need for redraw"
        );
    }

    #[test]
    fn test_cache_computes_after_debounce() {
        let mut cache = ReferenceHighlightCache::with_debounce(10); // 10ms for fast test
        let mut highlighter = ReferenceHighlighter::new();
        let buffer = Buffer::from_str_test("hello world hello");
        let color = Color::Rgb(60, 60, 80);

        // First call at position 0 (on "hello")
        let spans = cache.get_highlights(&mut highlighter, &buffer, 0, 0, 17, 1000, color);
        let first_count = spans.len();
        assert!(!spans.is_empty(), "Should compute on first call");

        // Move to position 6 (on "world")
        let _ = cache.get_highlights(&mut highlighter, &buffer, 6, 0, 17, 1000, color);

        // Wait for debounce
        sleep(Duration::from_millis(20));

        // Same position - should compute now (world has only 1 occurrence)
        let spans = cache.get_highlights(&mut highlighter, &buffer, 6, 0, 17, 1000, color);
        assert!(!spans.is_empty(), "Should have highlights after debounce");
        // "hello" appears twice, "world" appears once
        assert!(
            spans.len() != first_count || spans.len() == 1,
            "Should have recomputed for new word"
        );
    }

    #[test]
    fn test_cache_invalidation() {
        let mut cache = ReferenceHighlightCache::with_debounce(10);
        let mut highlighter = ReferenceHighlighter::new();
        let buffer = Buffer::from_str_test("hello world hello");
        let color = Color::Rgb(60, 60, 80);

        // Populate cache
        let spans = cache.get_highlights(&mut highlighter, &buffer, 0, 0, 17, 1000, color);
        assert!(!spans.is_empty());

        // Invalidate
        cache.invalidate();

        // Next call should recompute (first call after invalidation)
        let spans = cache.get_highlights(&mut highlighter, &buffer, 0, 0, 17, 1000, color);
        assert!(!spans.is_empty(), "Should recompute after invalidation");
    }

    #[test]
    fn test_needs_redraw() {
        let mut cache = ReferenceHighlightCache::with_debounce(50);
        let mut highlighter = ReferenceHighlighter::new();
        let buffer = Buffer::from_str_test("hello world");
        let color = Color::Rgb(60, 60, 80);

        // Initially no redraw needed
        assert!(cache.needs_redraw().is_none());

        // First call
        let _ = cache.get_highlights(&mut highlighter, &buffer, 0, 0, 11, 1000, color);

        // No redraw needed after first compute
        assert!(cache.needs_redraw().is_none());

        // Move cursor
        let _ = cache.get_highlights(&mut highlighter, &buffer, 6, 0, 11, 1000, color);

        // Now needs redraw
        let remaining = cache.needs_redraw();
        assert!(remaining.is_some());
        assert!(remaining.unwrap() <= Duration::from_millis(50));
    }
}
