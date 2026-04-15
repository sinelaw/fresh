//! Span / style primitive helpers used across the split renderer.
//!
//! This module is **self-sustaining**: it has no dependency on any shared
//! render-time "mega struct" and only uses ratatui + a narrow set of crate
//! primitives. Every helper takes typed inputs and returns typed outputs.

use crate::primitives::display_width::char_width;
use crate::primitives::highlighter::HighlightSpan;
use crate::view::overlay::{Overlay, OverlayFace};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::Span;
use std::ops::Range;

/// Compute character-level diff between two strings, returning ranges of changed characters.
/// Returns a tuple of (old_changed_ranges, new_changed_ranges) where each range indicates
/// character indices that differ between the strings.
pub(super) fn compute_inline_diff(
    old_text: &str,
    new_text: &str,
) -> (Vec<Range<usize>>, Vec<Range<usize>>) {
    let old_chars: Vec<char> = old_text.chars().collect();
    let new_chars: Vec<char> = new_text.chars().collect();

    let mut old_ranges = Vec::new();
    let mut new_ranges = Vec::new();

    // Find common prefix
    let prefix_len = old_chars
        .iter()
        .zip(new_chars.iter())
        .take_while(|(a, b)| a == b)
        .count();

    // Find common suffix (from the non-prefix part)
    let old_remaining = old_chars.len() - prefix_len;
    let new_remaining = new_chars.len() - prefix_len;
    let suffix_len = old_chars
        .iter()
        .rev()
        .zip(new_chars.iter().rev())
        .take(old_remaining.min(new_remaining))
        .take_while(|(a, b)| a == b)
        .count();

    // The changed range is between prefix and suffix
    let old_start = prefix_len;
    let old_end = old_chars.len().saturating_sub(suffix_len);
    let new_start = prefix_len;
    let new_end = new_chars.len().saturating_sub(suffix_len);

    if old_start < old_end {
        old_ranges.push(old_start..old_end);
    }
    if new_start < new_end {
        new_ranges.push(new_start..new_end);
    }

    (old_ranges, new_ranges)
}

/// Append a styled span to `spans` and mirror visual columns into `map`.
///
/// One map entry is pushed per visual column (not per character): double-width
/// characters contribute 2 entries, zero-width characters contribute 0.
pub(super) fn push_span_with_map(
    spans: &mut Vec<Span<'static>>,
    map: &mut Vec<Option<usize>>,
    text: String,
    style: Style,
    source: Option<usize>,
) {
    if text.is_empty() {
        return;
    }
    for ch in text.chars() {
        let width = char_width(ch);
        for _ in 0..width {
            map.push(source);
        }
    }
    spans.push(Span::styled(text, style));
}

/// Debug tag style - dim/muted color to distinguish from actual content.
pub(super) fn debug_tag_style() -> Style {
    Style::default()
        .fg(Color::DarkGray)
        .add_modifier(Modifier::DIM)
}

/// Push a debug tag span (no map entries since these aren't real content).
pub(super) fn push_debug_tag(
    spans: &mut Vec<Span<'static>>,
    map: &mut Vec<Option<usize>>,
    text: String,
) {
    if text.is_empty() {
        return;
    }
    // Debug tags don't map to source positions - they're visual-only
    for ch in text.chars() {
        let width = char_width(ch);
        for _ in 0..width {
            map.push(None);
        }
    }
    spans.push(Span::styled(text, debug_tag_style()));
}

/// Accumulator for building spans - collects characters with the same style
/// into a single span, flushing when the style changes. This is important for
/// proper rendering of combining characters (like Thai diacritics) which
/// must be in the same string as their base character.
pub(super) struct SpanAccumulator {
    text: String,
    style: Style,
    first_source: Option<usize>,
}

impl SpanAccumulator {
    pub(super) fn new() -> Self {
        Self {
            text: String::new(),
            style: Style::default(),
            first_source: None,
        }
    }

    /// Add a character. If the style matches, append to current span.
    /// If style differs, flush the current span first and start a new one.
    pub(super) fn push(
        &mut self,
        ch: char,
        style: Style,
        source: Option<usize>,
        spans: &mut Vec<Span<'static>>,
        map: &mut Vec<Option<usize>>,
    ) {
        // If we have accumulated text and the style changed, flush first
        if !self.text.is_empty() && style != self.style {
            self.flush(spans, map);
        }

        // Start new accumulation if empty
        if self.text.is_empty() {
            self.style = style;
            self.first_source = source;
        }

        self.text.push(ch);

        // Update map for this character's visual width
        let width = char_width(ch);
        for _ in 0..width {
            map.push(source);
        }
    }

    /// Flush accumulated text as a span.
    pub(super) fn flush(&mut self, spans: &mut Vec<Span<'static>>, _map: &mut Vec<Option<usize>>) {
        if !self.text.is_empty() {
            spans.push(Span::styled(std::mem::take(&mut self.text), self.style));
            self.first_source = None;
        }
    }
}

/// Context for tracking active spans in debug mode.
#[derive(Default)]
pub(super) struct DebugSpanTracker {
    /// Currently active highlight span (byte range)
    active_highlight: Option<Range<usize>>,
    /// Currently active overlay spans (byte ranges)
    active_overlays: Vec<Range<usize>>,
}

impl DebugSpanTracker {
    /// Get opening tags for spans that start at this byte position.
    pub(super) fn get_opening_tags(
        &mut self,
        byte_pos: Option<usize>,
        highlight_spans: &[HighlightSpan],
        viewport_overlays: &[(Overlay, Range<usize>)],
    ) -> Vec<String> {
        let mut tags = Vec::new();

        if let Some(bp) = byte_pos {
            // Check if we're entering a new highlight span
            if let Some(span) = highlight_spans.iter().find(|s| s.range.start == bp) {
                tags.push(format!("<hl:{}-{}>", span.range.start, span.range.end));
                self.active_highlight = Some(span.range.clone());
            }

            // Check if we're entering new overlay spans
            for (overlay, range) in viewport_overlays.iter() {
                if range.start == bp {
                    let overlay_type = match &overlay.face {
                        OverlayFace::Underline { .. } => "ul",
                        OverlayFace::Background { .. } => "bg",
                        OverlayFace::Foreground { .. } => "fg",
                        OverlayFace::Style { .. } => "st",
                        OverlayFace::ThemedStyle { .. } => "ts",
                    };
                    tags.push(format!("<{}:{}-{}>", overlay_type, range.start, range.end));
                    self.active_overlays.push(range.clone());
                }
            }
        }

        tags
    }

    /// Get closing tags for spans that end at this byte position.
    pub(super) fn get_closing_tags(&mut self, byte_pos: Option<usize>) -> Vec<String> {
        let mut tags = Vec::new();

        if let Some(bp) = byte_pos {
            // Check if we're exiting the active highlight span
            if let Some(ref range) = self.active_highlight {
                if bp >= range.end {
                    tags.push("</hl>".to_string());
                    self.active_highlight = None;
                }
            }

            // Check if we're exiting any overlay spans
            let mut closed_indices = Vec::new();
            for (i, range) in self.active_overlays.iter().enumerate() {
                if bp >= range.end {
                    tags.push("</ov>".to_string());
                    closed_indices.push(i);
                }
            }
            // Remove closed overlays (in reverse order to preserve indices)
            for i in closed_indices.into_iter().rev() {
                self.active_overlays.remove(i);
            }
        }

        tags
    }
}

/// Advance a cursor through sorted, non-overlapping spans to find the color at `byte_pos`.
/// Returns the color if `byte_pos` falls inside a span, and advances `cursor` past any
/// spans that end before `byte_pos` so subsequent calls are O(1) amortized.
#[inline]
pub(super) fn span_color_at(
    spans: &[HighlightSpan],
    cursor: &mut usize,
    byte_pos: usize,
) -> Option<Color> {
    while *cursor < spans.len() {
        let span = &spans[*cursor];
        if span.range.end <= byte_pos {
            *cursor += 1;
        } else if span.range.start > byte_pos {
            return None;
        } else {
            return Some(span.color);
        }
    }
    None
}

/// Like `span_color_at` but also returns the theme key for the highlight category.
pub(super) fn span_info_at(
    spans: &[HighlightSpan],
    cursor: &mut usize,
    byte_pos: usize,
) -> (Option<Color>, Option<&'static str>, Option<&'static str>) {
    while *cursor < spans.len() {
        let span = &spans[*cursor];
        if span.range.end <= byte_pos {
            *cursor += 1;
        } else if span.range.start > byte_pos {
            return (None, None, None);
        } else {
            let theme_key = span.category.as_ref().map(|c| c.theme_key());
            let display_name = span.category.as_ref().map(|c| c.display_name());
            return (Some(span.color), theme_key, display_name);
        }
    }
    (None, None, None)
}

/// Collapse a `Vec<(char, Style)>` into run-length encoded styled spans.
pub(super) fn compress_chars(chars: Vec<(char, Style)>) -> Vec<Span<'static>> {
    if chars.is_empty() {
        return vec![];
    }

    let mut spans = Vec::new();
    let mut current_style = chars[0].1;
    let mut current_text = String::new();
    current_text.push(chars[0].0);

    for (ch, style) in chars.into_iter().skip(1) {
        if style == current_style {
            current_text.push(ch);
        } else {
            spans.push(Span::styled(current_text.clone(), current_style));
            current_text.clear();
            current_text.push(ch);
            current_style = style;
        }
    }

    spans.push(Span::styled(current_text, current_style));
    spans
}
