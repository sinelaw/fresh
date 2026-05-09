//! Text properties for embedding metadata in text ranges
//!
//! This module provides Emacs-style text properties that allow embedding
//! arbitrary metadata (like source locations, severity levels, etc.) in
//! specific ranges of text. This is essential for virtual buffers where
//! each line might represent a diagnostic, search result, or other structured data.

use crate::api::OverlayOptions;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::ops::Range;

/// A text property that associates metadata with a range of text
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, ts_rs::TS)]
#[ts(export)]
pub struct TextProperty {
    /// Start byte offset (inclusive)
    pub start: usize,
    /// End byte offset (exclusive)
    pub end: usize,
    /// Arbitrary properties as key-value pairs
    #[ts(type = "Record<string, any>")]
    pub properties: HashMap<String, serde_json::Value>,
}

impl TextProperty {
    /// Create a new text property for a range
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start,
            end,
            properties: HashMap::new(),
        }
    }

    /// Add a property
    pub fn with_property(mut self, key: impl Into<String>, value: serde_json::Value) -> Self {
        self.properties.insert(key.into(), value);
        self
    }

    /// Set multiple properties at once
    pub fn with_properties(mut self, props: HashMap<String, serde_json::Value>) -> Self {
        self.properties.extend(props);
        self
    }

    /// Check if this property range contains a byte position
    pub fn contains(&self, pos: usize) -> bool {
        pos >= self.start && pos < self.end
    }

    /// Check if this property range overlaps with another range
    pub fn overlaps(&self, range: &Range<usize>) -> bool {
        self.start < range.end && self.end > range.start
    }

    /// Get a property value by key
    pub fn get(&self, key: &str) -> Option<&serde_json::Value> {
        self.properties.get(key)
    }

    /// Get a property as a specific type
    pub fn get_as<T: for<'de> Deserialize<'de>>(&self, key: &str) -> Option<T> {
        self.properties
            .get(key)
            .and_then(|v| serde_json::from_value(v.clone()).ok())
    }
}

/// Unit for `InlineOverlay` `start` / `end` offsets.
///
/// Plugins emitting overlays for text whose byte/codepoint counts
/// match (pure ASCII) can stay on the `Byte` default and avoid
/// per-overlay UTF-8 arithmetic. Plugins working with text that
/// may contain multi-byte characters can emit offsets in `Char`
/// units and let the host convert them to byte offsets at
/// consumption time — which is free in Rust against the entry's
/// final text.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize, ts_rs::TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub enum OffsetUnit {
    /// UTF-8 byte offsets within the entry's text. Default.
    #[default]
    Byte,
    /// Unicode codepoint (scalar value) offsets within the entry's
    /// text. Converted to byte offsets at consumption time.
    Char,
}

fn is_byte_unit(u: &OffsetUnit) -> bool {
    matches!(u, OffsetUnit::Byte)
}

/// An inline overlay specifying styling for a sub-range within a text entry
#[derive(Debug, Clone, Serialize, Deserialize, ts_rs::TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct InlineOverlay {
    /// Start offset within the entry's text. See `unit`.
    pub start: usize,
    /// End offset within the entry's text (exclusive). See `unit`.
    pub end: usize,
    /// Styling options for this range
    #[ts(type = "Partial<OverlayOptions>")]
    pub style: OverlayOptions,
    /// Optional properties for this sub-range (e.g., click target metadata)
    #[ts(type = "Record<string, any>")]
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub properties: HashMap<String, serde_json::Value>,
    /// Unit for `start` / `end`. Defaults to `byte`.
    #[serde(default, skip_serializing_if = "is_byte_unit")]
    pub unit: OffsetUnit,
}

/// One styled segment of a `TextPropertyEntry` built via the
/// `segments` field. Plugins use segments to describe row content
/// structurally — a sequence of (text, optional style, optional
/// nested overlays) — instead of pre-rendering the text and
/// computing byte/char offsets for overlays themselves. The host
/// concatenates segment text and emits the corresponding overlays
/// during `normalize_widths`.
#[derive(Debug, Clone, Serialize, Deserialize, ts_rs::TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct StyledSegment {
    /// Verbatim text for this segment.
    pub text: String,
    /// When set, the host emits an `InlineOverlay` covering this
    /// segment's text in the final entry.
    #[ts(type = "Partial<OverlayOptions>")]
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub style: Option<OverlayOptions>,
    /// Additional overlays inside this segment. Offsets are in
    /// the overlay's own `unit`, relative to the segment's start
    /// (NOT the final entry text); the host shifts them by the
    /// segment's position during concatenation.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub overlays: Vec<InlineOverlay>,
}

/// An entry with text and its properties
#[derive(Debug, Clone, Serialize, Deserialize, ts_rs::TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct TextPropertyEntry {
    /// The text content. When `segments` is non-empty `text` is
    /// rebuilt from concatenating segment text during
    /// `normalize_widths` and any value supplied here is replaced.
    pub text: String,
    /// Properties for this text
    #[ts(type = "Record<string, any>")]
    #[serde(default)]
    pub properties: HashMap<String, serde_json::Value>,
    /// Optional whole-entry styling
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub style: Option<OverlayOptions>,
    /// Optional sub-range styling within this entry
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub inline_overlays: Vec<InlineOverlay>,
    /// Optional segment list. When non-empty the host concatenates
    /// segment text into `text` and pushes one `InlineOverlay`
    /// (in `Char` units) per styled segment plus the segment's
    /// nested `overlays` shifted by its position. Resolved before
    /// truncate/pad/char-byte conversion in `normalize_widths`.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub segments: Vec<StyledSegment>,
    /// Pad `text` with spaces to this many display columns
    /// (Unicode codepoints). No-op when `text` already has at
    /// least this many codepoints. Applied before overlays are
    /// resolved.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub pad_to_chars: Option<u32>,
    /// Truncate `text` to at most this many display columns
    /// (Unicode codepoints). When the budget is greater than
    /// 3 the truncated tail is replaced with `...`; when it is
    /// 3 or less the text is cut at exactly the budget. Applied
    /// before overlays are resolved.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub truncate_to_chars: Option<u32>,
}

impl TextPropertyEntry {
    /// Create a new entry with just text
    pub fn text(text: impl Into<String>) -> Self {
        Self {
            text: text.into(),
            properties: HashMap::new(),
            style: None,
            inline_overlays: Vec::new(),
            segments: Vec::new(),
            pad_to_chars: None,
            truncate_to_chars: None,
        }
    }

    /// Resolve `segments` (if any) into `text` plus inline overlays,
    /// then apply `truncate_to_chars`, then `pad_to_chars`, then
    /// convert any `unit: Char` overlays to byte offsets against the
    /// resulting `text`. Idempotent: an entry with no segments,
    /// pad/truncate hints, or char-unit overlays is left untouched.
    ///
    /// Truncation rounds the byte cut to a UTF-8 codepoint boundary.
    /// Char-offset overlays beyond the resulting codepoint count are
    /// clamped to that count.
    pub fn normalize_widths(&mut self) {
        if !self.segments.is_empty() {
            // Segments are authoritative: replace any pre-existing
            // `text`. Per-segment style becomes a Char-unit overlay
            // covering the segment; nested overlays shift by the
            // segment's start in their declared unit.
            let segments = std::mem::take(&mut self.segments);
            self.text.clear();
            let mut char_cursor: usize = 0;
            let mut byte_cursor: usize = 0;
            for seg in segments {
                let seg_chars = seg.text.chars().count();
                let seg_bytes = seg.text.len();
                if let Some(style) = seg.style {
                    self.inline_overlays.push(InlineOverlay {
                        start: char_cursor,
                        end: char_cursor + seg_chars,
                        style,
                        properties: HashMap::new(),
                        unit: OffsetUnit::Char,
                    });
                }
                for mut o in seg.overlays {
                    match o.unit {
                        OffsetUnit::Char => {
                            o.start += char_cursor;
                            o.end += char_cursor;
                        }
                        OffsetUnit::Byte => {
                            o.start += byte_cursor;
                            o.end += byte_cursor;
                        }
                    }
                    self.inline_overlays.push(o);
                }
                self.text.push_str(&seg.text);
                char_cursor += seg_chars;
                byte_cursor += seg_bytes;
            }
        }

        if let Some(max_chars) = self.truncate_to_chars {
            let max = max_chars as usize;
            let cur = self.text.chars().count();
            if cur > max {
                if max <= 3 {
                    let cut_byte = self
                        .text
                        .char_indices()
                        .nth(max)
                        .map(|(b, _)| b)
                        .unwrap_or(self.text.len());
                    self.text.truncate(cut_byte);
                } else {
                    let keep = max - 3;
                    let cut_byte = self
                        .text
                        .char_indices()
                        .nth(keep)
                        .map(|(b, _)| b)
                        .unwrap_or(self.text.len());
                    self.text.truncate(cut_byte);
                    self.text.push_str("...");
                }
            }
        }

        if let Some(min_chars) = self.pad_to_chars {
            let cur = self.text.chars().count();
            let target = min_chars as usize;
            if target > cur {
                let pad = target - cur;
                self.text.reserve(pad);
                for _ in 0..pad {
                    self.text.push(' ');
                }
            }
        }

        let needs_conversion = self
            .inline_overlays
            .iter()
            .any(|o| matches!(o.unit, OffsetUnit::Char));
        if needs_conversion {
            // Build a codepoint-index → byte-index lookup over the
            // final text. One pass; subsequent overlay lookups are
            // O(1) into the table.
            let mut char_to_byte: Vec<usize> = self.text.char_indices().map(|(b, _)| b).collect();
            char_to_byte.push(self.text.len());
            for o in &mut self.inline_overlays {
                if matches!(o.unit, OffsetUnit::Char) {
                    let s = o.start.min(char_to_byte.len() - 1);
                    let e = o.end.min(char_to_byte.len() - 1);
                    o.start = char_to_byte[s];
                    o.end = char_to_byte[e];
                    o.unit = OffsetUnit::Byte;
                }
            }
        }
    }

    /// Add a property
    pub fn with_property(mut self, key: impl Into<String>, value: serde_json::Value) -> Self {
        self.properties.insert(key.into(), value);
        self
    }

    /// Set multiple properties
    pub fn with_properties(mut self, props: HashMap<String, serde_json::Value>) -> Self {
        self.properties = props;
        self
    }

    /// Set whole-entry styling
    pub fn with_style(mut self, style: OverlayOptions) -> Self {
        self.style = Some(style);
        self
    }

    /// Add a sub-range inline overlay
    pub fn with_inline_overlay(mut self, start: usize, end: usize, style: OverlayOptions) -> Self {
        self.inline_overlays.push(InlineOverlay {
            start,
            end,
            style,
            properties: HashMap::new(),
            unit: OffsetUnit::Byte,
        });
        self
    }

    /// Push a styled segment. After `normalize_widths` runs, the
    /// segment becomes part of `text` plus a Char-unit
    /// `InlineOverlay` covering it (when `style` is set).
    pub fn with_segment(mut self, text: impl Into<String>, style: Option<OverlayOptions>) -> Self {
        self.segments.push(StyledSegment {
            text: text.into(),
            style,
            overlays: Vec::new(),
        });
        self
    }
}

#[cfg(test)]
mod normalize_tests {
    use super::*;

    fn entry(text: &str) -> TextPropertyEntry {
        TextPropertyEntry::text(text)
    }

    #[test]
    fn pad_to_chars_pads_short_ascii_text() {
        let mut e = entry("hi");
        e.pad_to_chars = Some(5);
        e.normalize_widths();
        assert_eq!(e.text, "hi   ");
    }

    #[test]
    fn pad_to_chars_is_noop_when_text_already_wider() {
        let mut e = entry("longer than five");
        e.pad_to_chars = Some(5);
        e.normalize_widths();
        assert_eq!(e.text, "longer than five");
    }

    #[test]
    fn pad_to_chars_counts_codepoints_not_bytes() {
        // 'é' is two UTF-8 bytes but one codepoint.
        let mut e = entry("éé");
        e.pad_to_chars = Some(4);
        e.normalize_widths();
        assert_eq!(e.text, "éé  ");
    }

    #[test]
    fn truncate_to_chars_appends_ellipsis_when_budget_over_three() {
        let mut e = entry("abcdefghij");
        e.truncate_to_chars = Some(6);
        e.normalize_widths();
        assert_eq!(e.text, "abc...");
    }

    #[test]
    fn truncate_to_chars_cuts_without_ellipsis_when_budget_three_or_less() {
        let mut e = entry("abcdef");
        e.truncate_to_chars = Some(3);
        e.normalize_widths();
        assert_eq!(e.text, "abc");
    }

    #[test]
    fn truncate_to_chars_respects_codepoint_boundary() {
        // 'é' is two UTF-8 bytes; cutting at byte 1 would split it.
        let mut e = entry("éééé");
        e.truncate_to_chars = Some(2);
        e.normalize_widths();
        assert_eq!(e.text, "éé");
    }

    #[test]
    fn truncate_then_pad_combines_correctly() {
        let mut e = entry("abcdefghij");
        e.truncate_to_chars = Some(6);
        e.pad_to_chars = Some(8);
        e.normalize_widths();
        assert_eq!(e.text, "abc...  ");
    }

    #[test]
    fn char_unit_overlay_converted_to_byte_offsets_against_ascii() {
        let mut e = entry("hello world");
        e.inline_overlays.push(InlineOverlay {
            start: 6,
            end: 11,
            style: OverlayOptions::default(),
            properties: HashMap::new(),
            unit: OffsetUnit::Char,
        });
        e.normalize_widths();
        let o = &e.inline_overlays[0];
        assert_eq!(o.start, 6);
        assert_eq!(o.end, 11);
        assert_eq!(o.unit, OffsetUnit::Byte);
    }

    #[test]
    fn char_unit_overlay_converted_to_byte_offsets_with_multibyte_chars() {
        // "éxé" = é(2) x(1) é(2) = 5 bytes, 3 codepoints
        let mut e = entry("éxé");
        e.inline_overlays.push(InlineOverlay {
            start: 1,
            end: 2,
            style: OverlayOptions::default(),
            properties: HashMap::new(),
            unit: OffsetUnit::Char,
        });
        e.normalize_widths();
        let o = &e.inline_overlays[0];
        assert_eq!(o.start, 2);
        assert_eq!(o.end, 3);
        assert_eq!(o.unit, OffsetUnit::Byte);
        assert_eq!(&e.text[o.start..o.end], "x");
    }

    #[test]
    fn char_unit_overlay_after_pad_indexes_into_padded_text() {
        let mut e = entry("hi");
        e.pad_to_chars = Some(6);
        e.inline_overlays.push(InlineOverlay {
            start: 0,
            end: 6,
            style: OverlayOptions::default(),
            properties: HashMap::new(),
            unit: OffsetUnit::Char,
        });
        e.normalize_widths();
        let o = &e.inline_overlays[0];
        assert_eq!(o.start, 0);
        assert_eq!(o.end, 6);
    }

    #[test]
    fn char_unit_overlay_after_truncate_clamps_to_remaining_text() {
        let mut e = entry("abcdefghij");
        e.truncate_to_chars = Some(6); // becomes "abc..."
        e.inline_overlays.push(InlineOverlay {
            start: 0,
            end: 100, // overshoots — clamp to text length in codepoints
            style: OverlayOptions::default(),
            properties: HashMap::new(),
            unit: OffsetUnit::Char,
        });
        e.normalize_widths();
        let o = &e.inline_overlays[0];
        assert_eq!(o.start, 0);
        assert_eq!(o.end, e.text.len());
    }

    #[test]
    fn byte_unit_overlay_unchanged_by_normalize() {
        let mut e = entry("hello");
        e.inline_overlays.push(InlineOverlay {
            start: 1,
            end: 4,
            style: OverlayOptions::default(),
            properties: HashMap::new(),
            unit: OffsetUnit::Byte,
        });
        e.normalize_widths();
        let o = &e.inline_overlays[0];
        assert_eq!(o.start, 1);
        assert_eq!(o.end, 4);
        assert_eq!(o.unit, OffsetUnit::Byte);
    }

    fn styled(text: &str, fg_marker_bold: bool) -> StyledSegment {
        StyledSegment {
            text: text.to_string(),
            style: if fg_marker_bold {
                Some(OverlayOptions {
                    bold: true,
                    ..Default::default()
                })
            } else {
                None
            },
            overlays: Vec::new(),
        }
    }

    #[test]
    fn segments_concatenate_into_text() {
        let mut e = entry("ignored");
        e.segments = vec![
            styled("hello", false),
            styled(" ", false),
            styled("world", false),
        ];
        e.normalize_widths();
        assert_eq!(e.text, "hello world");
        assert!(e.segments.is_empty(), "segments consumed");
    }

    #[test]
    fn styled_segments_emit_char_unit_overlays_for_styled_segments_only() {
        let mut e = entry("");
        e.segments = vec![
            styled("AB", false),
            styled("CD", true), // bold
            styled("EF", false),
            styled("GH", true), // bold
        ];
        e.normalize_widths();
        // After char→byte conversion (all ASCII so identity).
        assert_eq!(e.text, "ABCDEFGH");
        let bold: Vec<_> = e.inline_overlays.iter().filter(|o| o.style.bold).collect();
        assert_eq!(bold.len(), 2);
        assert_eq!((bold[0].start, bold[0].end), (2, 4));
        assert_eq!((bold[1].start, bold[1].end), (6, 8));
    }

    #[test]
    fn styled_segments_with_multibyte_text_emit_correct_byte_overlays() {
        // "éé" + "x" + "éé" = chars [0..2, 2..3, 3..5], bytes [0..4, 4..5, 5..9].
        let mut e = entry("");
        e.segments = vec![styled("éé", false), styled("x", true), styled("éé", false)];
        e.normalize_widths();
        assert_eq!(e.text, "ééxéé");
        let bold = e
            .inline_overlays
            .iter()
            .find(|o| o.style.bold)
            .expect("styled middle segment");
        assert_eq!((bold.start, bold.end), (4, 5));
        assert_eq!(&e.text[bold.start..bold.end], "x");
    }

    #[test]
    fn segment_nested_overlays_shift_by_segment_position_in_their_unit() {
        let mut e = entry("");
        e.segments = vec![
            StyledSegment {
                text: "abc".to_string(),
                style: None,
                overlays: vec![],
            },
            StyledSegment {
                text: "éé".to_string(),
                style: None,
                overlays: vec![InlineOverlay {
                    start: 1,
                    end: 2,
                    style: OverlayOptions {
                        bold: true,
                        ..Default::default()
                    },
                    properties: HashMap::new(),
                    unit: OffsetUnit::Char,
                }],
            },
        ];
        e.normalize_widths();
        // "abcéé" — segment2 starts at char 3, byte 3.
        // Nested overlay [1..2] in segment2 → entry chars [4..5].
        // Char→byte conversion: char 4 = byte 5, char 5 = byte 7.
        let bold = e
            .inline_overlays
            .iter()
            .find(|o| o.style.bold)
            .expect("nested overlay");
        assert_eq!(&e.text[bold.start..bold.end], "é");
    }

    #[test]
    fn segments_then_pad_works() {
        let mut e = entry("");
        e.segments = vec![styled("ab", true)];
        e.pad_to_chars = Some(5);
        e.normalize_widths();
        assert_eq!(e.text, "ab   ");
        let bold = e
            .inline_overlays
            .iter()
            .find(|o| o.style.bold)
            .expect("segment overlay");
        assert_eq!((bold.start, bold.end), (0, 2));
    }

    #[test]
    fn segments_then_truncate_clamps_overlapping_overlay() {
        let mut e = entry("");
        e.segments = vec![styled("abcdefghij", true)];
        e.truncate_to_chars = Some(5);
        e.normalize_widths();
        // Truncated to "ab..." (budget>3).
        assert_eq!(e.text, "ab...");
        let bold = e
            .inline_overlays
            .iter()
            .find(|o| o.style.bold)
            .expect("segment overlay");
        // Bold overlay covered chars [0..10] originally; clamped to
        // the new text length (5 codepoints / 5 bytes ASCII).
        assert_eq!(bold.end, e.text.len());
    }

    #[test]
    fn segments_replace_pre_existing_text() {
        let mut e = entry("should be discarded");
        e.segments = vec![styled("only this", false)];
        e.normalize_widths();
        assert_eq!(e.text, "only this");
    }
}
