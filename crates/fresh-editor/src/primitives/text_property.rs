//! Text properties for embedding metadata in text ranges
//!
//! This module provides Emacs-style text properties that allow embedding
//! arbitrary metadata (like source locations, severity levels, etc.) in
//! specific ranges of text. This is essential for virtual buffers where
//! each line might represent a diagnostic, search result, or other structured data.

use std::ops::Range;
use std::sync::atomic::{AtomicU64, Ordering};

/// Source of [`TextPropertyManager::version`] values.
///
/// Process-global rather than per-manager because the version has to
/// survive a manager being *replaced*, which is how a buffer's properties
/// are actually set: `set_virtual_buffer_content` assigns a whole new
/// manager built by `from_entries`. A per-manager counter starting at zero
/// makes the version equal the property count, so a consumer comparing
/// versions would see "unchanged" whenever a re-render happened to produce
/// the same number of properties — a plugin panel repainting one row with
/// new values, a list whose selection moved — and would serve the previous
/// render's properties indefinitely.
static NEXT_VERSION: AtomicU64 = AtomicU64::new(1);

fn next_version() -> u64 {
    NEXT_VERSION.fetch_add(1, Ordering::Relaxed)
}

// Re-export types from fresh-core for shared type usage
pub use fresh_core::text_property::{TextProperty, TextPropertyEntry};

/// A collected overlay from inline styling in TextPropertyEntry,
/// with byte offsets converted to absolute positions in the full text.
#[derive(Debug, Clone)]
pub struct CollectedOverlay {
    /// Absolute byte range in the assembled text
    pub range: Range<usize>,
    /// The overlay styling options
    pub options: fresh_core::api::OverlayOptions,
}

/// Manager for text properties in a buffer
///
/// Stores and queries text properties efficiently. Properties can overlap
/// and are sorted by start position for fast lookup.
#[derive(Debug, Clone)]
pub struct TextPropertyManager {
    /// All properties, sorted by start position
    properties: Vec<TextProperty>,
    /// Changes on every mutation, and on every fresh manager. Consumers
    /// that copy the whole set — the plugin state snapshot does, once per
    /// tick per buffer — compare this instead of copying again, which
    /// matters because a plugin-drawn buffer can carry a property per
    /// line. Values come from a process-global counter so that two
    /// managers never share one; see [`NEXT_VERSION`].
    version: u64,
}

impl Default for TextPropertyManager {
    fn default() -> Self {
        Self::new()
    }
}

impl TextPropertyManager {
    /// Create a new empty property manager
    pub fn new() -> Self {
        Self {
            properties: Vec::new(),
            version: next_version(),
        }
    }

    /// Add a text property
    pub fn add(&mut self, property: TextProperty) {
        // Insert in sorted order by start position
        let pos = self
            .properties
            .binary_search_by_key(&property.start, |p| p.start)
            .unwrap_or_else(|e| e);
        self.properties.insert(pos, property);
        self.version = next_version();
    }

    /// Get all properties at a specific byte position
    pub fn get_at(&self, pos: usize) -> Vec<&TextProperty> {
        self.properties.iter().filter(|p| p.contains(pos)).collect()
    }

    /// Get all properties overlapping a range
    pub fn get_overlapping(&self, range: &Range<usize>) -> Vec<&TextProperty> {
        self.properties
            .iter()
            .filter(|p| p.overlaps(range))
            .collect()
    }

    /// Clear all properties
    pub fn clear(&mut self) {
        self.properties.clear();
        self.version = next_version();
    }

    /// Remove all properties in a range
    pub fn remove_in_range(&mut self, range: &Range<usize>) {
        self.properties
            .retain(|p| !p.overlaps(range) && !range.contains(&p.start));
        self.version = next_version();
    }

    /// Get all properties
    pub fn all(&self) -> &[TextProperty] {
        &self.properties
    }

    /// Check if there are any properties
    pub fn is_empty(&self) -> bool {
        self.properties.is_empty()
    }

    /// Get the number of properties
    pub fn len(&self) -> usize {
        self.properties.len()
    }

    /// Set all properties at once (replaces existing)
    pub fn set_all(&mut self, properties: Vec<TextProperty>) {
        self.properties = properties;
        // Ensure sorted by start position
        self.properties.sort_by_key(|p| p.start);
        self.version = next_version();
    }

    /// A value that changes on every mutation and is unique to this
    /// manager, so a consumer holding a copy of `all()` can tell whether
    /// it is stale without comparing the properties themselves — including
    /// when the whole manager was replaced by a different one.
    pub fn version(&self) -> u64 {
        self.version
    }

    /// Merge properties from another source
    ///
    /// This is useful when setting buffer content with properties.
    /// Returns the assembled text, the property manager, and any collected
    /// inline overlay specifications (with absolute byte offsets).
    pub fn from_entries(entries: Vec<TextPropertyEntry>) -> (String, Self, Vec<CollectedOverlay>) {
        let mut text = String::new();
        let mut manager = Self::new();
        let mut collected_overlays = Vec::new();
        let mut offset = 0;

        for entry in entries {
            let start = offset;
            let entry_len = entry.text.len();
            text.push_str(&entry.text);
            let end = offset + entry_len;

            if !entry.properties.is_empty() {
                let property = TextProperty {
                    start,
                    end,
                    properties: entry.properties,
                };
                manager.add(property);
            }

            // Collect whole-entry style
            if let Some(style) = entry.style {
                collected_overlays.push(CollectedOverlay {
                    range: start..end,
                    options: style,
                });
            }

            // Collect sub-range inline overlays, converting to absolute offsets
            for inline in entry.inline_overlays {
                let abs_start = start + inline.start.min(entry_len);
                let abs_end = start + inline.end.min(entry_len);
                if abs_start < abs_end {
                    collected_overlays.push(CollectedOverlay {
                        range: abs_start..abs_end,
                        options: inline.style,
                    });
                    // Create a TextProperty for inline overlays with properties
                    if !inline.properties.is_empty() {
                        let property = TextProperty {
                            start: abs_start,
                            end: abs_end,
                            properties: inline.properties,
                        };
                        manager.add(property);
                    }
                }
            }

            offset = end;
        }

        (text, manager, collected_overlays)
    }
}

#[cfg(test)]
mod tests {
    /// The version has to distinguish two *different* managers, not just
    /// two states of one, because replacing the manager wholesale is how a
    /// buffer's properties are set (`set_virtual_buffer_content`). A
    /// per-manager counter starting at zero made the version equal the
    /// property count, so a panel repainting the same number of rows with
    /// new values looked unchanged and its reader kept the old values.
    #[test]
    fn version_distinguishes_managers_with_the_same_shape() {
        let entry = |file: &str| TextPropertyEntry {
            properties: [("file".to_string(), serde_json::json!(file))]
                .into_iter()
                .collect(),
            ..TextPropertyEntry::text("row\n")
        };

        let (_, first, _) = TextPropertyManager::from_entries(vec![entry("a.rs")]);
        let (_, second, _) = TextPropertyManager::from_entries(vec![entry("b.rs")]);

        assert_eq!(first.len(), second.len(), "same shape, different content");
        assert_ne!(
            first.version(),
            second.version(),
            "a replacement manager must not reuse the version of the one it replaces"
        );
    }

    #[test]
    fn version_changes_on_every_mutation() {
        let mut manager = TextPropertyManager::new();
        let mut seen = vec![manager.version()];
        manager.add(TextProperty {
            start: 0,
            end: 1,
            properties: Default::default(),
        });
        seen.push(manager.version());
        manager.set_all(Vec::new());
        seen.push(manager.version());
        manager.clear();
        seen.push(manager.version());

        let mut unique = seen.clone();
        unique.sort_unstable();
        unique.dedup();
        assert_eq!(unique.len(), seen.len(), "versions repeated: {seen:?}");
    }

    use super::*;
    use serde::Deserialize;
    use serde_json::json;

    #[test]
    fn test_text_property_contains() {
        let prop = TextProperty::new(10, 20);
        assert!(prop.contains(10));
        assert!(prop.contains(15));
        assert!(prop.contains(19));
        assert!(!prop.contains(9));
        assert!(!prop.contains(20));
    }

    #[test]
    fn test_text_property_overlaps() {
        let prop = TextProperty::new(10, 20);
        assert!(prop.overlaps(&(5..15)));
        assert!(prop.overlaps(&(15..25)));
        assert!(prop.overlaps(&(10..20)));
        assert!(prop.overlaps(&(12..18)));
        assert!(!prop.overlaps(&(0..10)));
        assert!(!prop.overlaps(&(20..30)));
    }

    #[test]
    fn test_text_property_with_properties() {
        let prop = TextProperty::new(0, 10)
            .with_property("severity", json!("error"))
            .with_property(
                "location",
                json!({"file": "test.rs", "line": 42, "column": 5}),
            );

        assert_eq!(prop.get("severity"), Some(&json!("error")));
        assert_eq!(
            prop.get("location"),
            Some(&json!({"file": "test.rs", "line": 42, "column": 5}))
        );
        assert_eq!(prop.get("nonexistent"), None);
    }

    #[test]
    fn test_text_property_get_as() {
        let prop = TextProperty::new(0, 10)
            .with_property("count", json!(42))
            .with_property(
                "location",
                json!({"file": "test.rs", "line": 42, "column": 5}),
            );

        let count: Option<i64> = prop.get_as("count");
        assert_eq!(count, Some(42));

        #[derive(Debug, Deserialize, PartialEq)]
        struct Location {
            file: String,
            line: u32,
            column: u32,
        }

        let loc: Option<Location> = prop.get_as("location");
        assert_eq!(
            loc,
            Some(Location {
                file: "test.rs".to_string(),
                line: 42,
                column: 5,
            })
        );
    }

    #[test]
    fn test_manager_add_and_get_at() {
        let mut manager = TextPropertyManager::new();

        manager.add(TextProperty::new(0, 10).with_property("id", json!("first")));
        manager.add(TextProperty::new(5, 15).with_property("id", json!("second")));
        manager.add(TextProperty::new(20, 30).with_property("id", json!("third")));

        // Position 7 is covered by first and second
        let props = manager.get_at(7);
        assert_eq!(props.len(), 2);
        assert_eq!(props[0].get("id"), Some(&json!("first")));
        assert_eq!(props[1].get("id"), Some(&json!("second")));

        // Position 25 is covered by third only
        let props = manager.get_at(25);
        assert_eq!(props.len(), 1);
        assert_eq!(props[0].get("id"), Some(&json!("third")));

        // Position 17 is not covered by any
        let props = manager.get_at(17);
        assert_eq!(props.len(), 0);
    }

    #[test]
    fn test_manager_get_overlapping() {
        let mut manager = TextPropertyManager::new();

        manager.add(TextProperty::new(0, 10).with_property("id", json!("first")));
        manager.add(TextProperty::new(20, 30).with_property("id", json!("second")));

        // Range overlaps with first
        let props = manager.get_overlapping(&(5..15));
        assert_eq!(props.len(), 1);
        assert_eq!(props[0].get("id"), Some(&json!("first")));

        // Range overlaps with second
        let props = manager.get_overlapping(&(25..35));
        assert_eq!(props.len(), 1);
        assert_eq!(props[0].get("id"), Some(&json!("second")));

        // Range overlaps with neither
        let props = manager.get_overlapping(&(12..18));
        assert_eq!(props.len(), 0);

        // Range overlaps with both
        let props = manager.get_overlapping(&(0..30));
        assert_eq!(props.len(), 2);
    }

    #[test]
    fn test_manager_from_entries() {
        let entries = vec![
            TextPropertyEntry::text("Error at line 42\n")
                .with_property("severity", json!("error"))
                .with_property("line", json!(42)),
            TextPropertyEntry::text("Warning at line 100\n")
                .with_property("severity", json!("warning"))
                .with_property("line", json!(100)),
        ];

        let (text, manager, _overlays) = TextPropertyManager::from_entries(entries);

        assert_eq!(text, "Error at line 42\nWarning at line 100\n");
        assert_eq!(manager.len(), 2);

        // First property covers "Error at line 42\n" (17 bytes)
        let first_props = manager.get_at(0);
        assert_eq!(first_props.len(), 1);
        assert_eq!(first_props[0].get("severity"), Some(&json!("error")));
        assert_eq!(first_props[0].get("line"), Some(&json!(42)));
        assert_eq!(first_props[0].start, 0);
        assert_eq!(first_props[0].end, 17);

        // Second property covers "Warning at line 100\n" (20 bytes)
        let second_props = manager.get_at(17);
        assert_eq!(second_props.len(), 1);
        assert_eq!(second_props[0].get("severity"), Some(&json!("warning")));
        assert_eq!(second_props[0].get("line"), Some(&json!(100)));
        assert_eq!(second_props[0].start, 17);
        assert_eq!(second_props[0].end, 37);
    }

    #[test]
    fn test_manager_clear() {
        let mut manager = TextPropertyManager::new();
        manager.add(TextProperty::new(0, 10));
        manager.add(TextProperty::new(20, 30));

        assert_eq!(manager.len(), 2);
        manager.clear();
        assert_eq!(manager.len(), 0);
        assert!(manager.is_empty());
    }

    #[test]
    fn test_manager_remove_in_range() {
        let mut manager = TextPropertyManager::new();
        manager.add(TextProperty::new(0, 10).with_property("id", json!("first")));
        manager.add(TextProperty::new(20, 30).with_property("id", json!("second")));
        manager.add(TextProperty::new(40, 50).with_property("id", json!("third")));

        // Remove properties overlapping with range 15-35
        manager.remove_in_range(&(15..35));

        // Should have removed second (20-30 overlaps with 15-35)
        assert_eq!(manager.len(), 2);
        let all = manager.all();
        assert_eq!(all[0].get("id"), Some(&json!("first")));
        assert_eq!(all[1].get("id"), Some(&json!("third")));
    }
}
