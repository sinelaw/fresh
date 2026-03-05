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

/// An inline overlay specifying styling for a sub-range within a text entry
#[derive(Debug, Clone, Serialize, Deserialize, ts_rs::TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub struct InlineOverlay {
    /// Start byte offset within the entry's text
    pub start: usize,
    /// End byte offset within the entry's text (exclusive)
    pub end: usize,
    /// Styling options for this range
    #[ts(type = "Partial<OverlayOptions>")]
    pub style: OverlayOptions,
    /// Optional properties for this sub-range (e.g., click target metadata)
    #[ts(type = "Record<string, any>")]
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub properties: HashMap<String, serde_json::Value>,
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
}

impl TextPropertyEntry {
    /// Create a new entry with just text
    pub fn text(text: impl Into<String>) -> Self {
        Self {
            text: text.into(),
            properties: HashMap::new(),
            style: None,
            inline_overlays: Vec::new(),
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
        });
        self
    }
}
