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

/// An entry with text and its properties
#[derive(Debug, Clone, Serialize, Deserialize, ts_rs::TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct TextPropertyEntry {
    /// The text content
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
            pad_to_chars: None,
            truncate_to_chars: None,
        }
    }

    /// Apply `truncate_to_chars`, then `pad_to_chars`, then convert
    /// any `unit: Char` overlays to byte offsets against the
    /// resulting `text`. Idempotent: an entry with neither
    /// pad/truncate hints nor char-unit overlays is left untouched.
    ///
    /// Truncation rounds the byte cut to a UTF-8 codepoint boundary.
    /// Char-offset overlays beyond the resulting codepoint count are
    /// clamped to that count.
    pub fn normalize_widths(&mut self) {
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
}
