//! Text properties for embedding metadata in text ranges
//!
//! This module provides Emacs-style text properties that allow embedding
//! arbitrary metadata (like source locations, severity levels, etc.) in
//! specific ranges of text. This is essential for virtual buffers where
//! each line might represent a diagnostic, search result, or other structured data.

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

/// An entry with text and its properties
#[derive(Debug, Clone, Serialize, Deserialize, ts_rs::TS)]
#[ts(export)]
pub struct TextPropertyEntry {
    /// The text content
    pub text: String,
    /// Properties for this text
    #[ts(type = "Record<string, any>")]
    pub properties: HashMap<String, serde_json::Value>,
}

impl TextPropertyEntry {
    /// Create a new entry with just text
    pub fn text(text: impl Into<String>) -> Self {
        Self {
            text: text.into(),
            properties: HashMap::new(),
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
}
