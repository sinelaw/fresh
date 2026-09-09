//! Text properties for embedding metadata in text ranges
//!
//! This module provides Emacs-style text properties that allow embedding
//! arbitrary metadata (like source locations, severity levels, etc.) in
//! specific ranges of text. This is essential for virtual buffers where
//! each line might represent a diagnostic, search result, or other structured data.

use std::ops::Range;
use std::sync::Arc;

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
#[derive(Debug, Clone, Default)]
pub struct TextPropertyManager {
    /// All properties, sorted by start position.
    ///
    /// Shared rather than owned so that handing the set to a reader — the
    /// plugin state snapshot takes one per buffer, on every tick — is a
    /// refcount bump instead of a copy. A plugin-drawn buffer can carry a
    /// property per line, so copying was the tick's whole cost on a large
    /// review; sharing removes the copy rather than trying to skip it,
    /// which is a freshness problem nobody has to get right.
    ///
    /// Immutable: the manager is replaced wholesale (see
    /// `set_virtual_buffer_content`), and the mutators below rebuild it.
    /// That is not a hot path — `from_entries` assembles a `Vec` and seals
    /// it once.
    properties: Arc<[TextProperty]>,
    /// Widest `end - start` in the set.
    ///
    /// Properties are sorted by start but may overlap, so a lookup at a
    /// byte cannot stop at the first earlier property that misses it — an
    /// earlier one may still be long enough to cover it. Knowing the
    /// widest bounds how far back that walk has to go: nothing starting
    /// before `pos - max_span` can reach `pos`. Without it the walk is the
    /// whole set, which on a plugin-composed buffer is one property per
    /// row.
    max_span: usize,
}

impl TextPropertyManager {
    /// Create a new empty property manager
    pub fn new() -> Self {
        Self {
            properties: Arc::from(Vec::new()),
            max_span: 0,
        }
    }

    /// Seal `properties` (already sorted by start) as the new set.
    /// The single place `max_span` is derived, so it cannot drift.
    fn seal(&mut self, properties: Vec<TextProperty>) {
        self.max_span = properties
            .iter()
            .map(|p| p.end.saturating_sub(p.start))
            .max()
            .unwrap_or(0);
        self.properties = Arc::from(properties);
    }

    /// Index of the first property starting after `pos`. Everything that
    /// can contain `pos` lies before it.
    fn first_after(&self, pos: usize) -> usize {
        self.properties.partition_point(|p| p.start <= pos)
    }

    /// The lowest index a property containing a byte at or after `from`
    /// can have. Paired with [`Self::first_after`] this bounds a lookup to
    /// the properties that can actually reach it.
    fn lowest_reaching(&self, from: usize) -> usize {
        let earliest = from.saturating_sub(self.max_span);
        self.properties.partition_point(|p| p.start < earliest)
    }

    /// Add a text property
    pub fn add(&mut self, property: TextProperty) {
        let mut properties = self.properties.to_vec();
        // Insert in sorted order by start position
        let pos = properties
            .binary_search_by_key(&property.start, |p| p.start)
            .unwrap_or_else(|e| e);
        properties.insert(pos, property);
        self.seal(properties);
    }

    /// Get all properties at a specific byte position
    pub fn get_at(&self, pos: usize) -> Vec<&TextProperty> {
        self.properties[self.lowest_reaching(pos)..self.first_after(pos)]
            .iter()
            .filter(|p| p.contains(pos))
            .collect()
    }

    /// Get all properties overlapping a range
    pub fn get_overlapping(&self, range: &Range<usize>) -> Vec<&TextProperty> {
        // An empty range is not short-circuited: `overlaps` reports a
        // property straddling the point as overlapping it, and callers
        // already see that answer. The window below reproduces it —
        // `end - 1` keeps properties starting at `end` out, which is what
        // `start < range.end` would have rejected anyway.
        let lo = self.lowest_reaching(range.start);
        let hi = self.first_after(range.end.saturating_sub(1));
        self.properties[lo..hi]
            .iter()
            .filter(|p| p.overlaps(range))
            .collect()
    }

    /// Clear all properties
    pub fn clear(&mut self) {
        self.seal(Vec::new());
    }

    /// Remove all properties in a range
    pub fn remove_in_range(&mut self, range: &Range<usize>) {
        let mut properties = self.properties.to_vec();
        properties.retain(|p| !p.overlaps(range) && !range.contains(&p.start));
        self.seal(properties);
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
    pub fn set_all(&mut self, mut properties: Vec<TextProperty>) {
        // Ensure sorted by start position
        properties.sort_by_key(|p| p.start);
        self.seal(properties);
    }

    /// The property set, for a reader that wants to hold on to it. Cloning
    /// the returned handle is a refcount bump, so a per-tick consumer can
    /// take one every tick without the cost tracking the buffer's content.
    pub fn shared(&self) -> Arc<[TextProperty]> {
        Arc::clone(&self.properties)
    }

    /// Merge properties from another source
    ///
    /// This is useful when setting buffer content with properties.
    /// Returns the assembled text, the property manager, and any collected
    /// inline overlay specifications (with absolute byte offsets).
    pub fn from_entries(entries: Vec<TextPropertyEntry>) -> (String, Self, Vec<CollectedOverlay>) {
        let mut text = String::new();
        let mut properties: Vec<TextProperty> = Vec::new();
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
                properties.push(property);
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
                        properties.push(property);
                    }
                }
            }

            offset = end;
        }

        // Entries are walked in order and each property starts at its
        // entry's offset, so this is already sorted by `start`; sealing it
        // here is the one place the set is built.
        let mut manager = Self::new();
        manager.seal(properties);
        (text, manager, collected_overlays)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    /// Handing the property set to a reader must not copy it: that copy,
    /// once per buffer per tick in the plugin state snapshot, was the whole
    /// per-tick cost of a large review. Sharing also removes the freshness
    /// question — an earlier attempt skipped the copy when a version looked
    /// unchanged, and the version turned out to be the property count, so a
    /// panel repainting the same number of rows with new values served the
    /// old ones.
    #[test]
    fn shared_hands_out_the_same_allocation() {
        let entry = |file: &str| TextPropertyEntry {
            properties: [("file".to_string(), serde_json::json!(file))]
                .into_iter()
                .collect(),
            ..TextPropertyEntry::text("row\n")
        };
        let (_, manager, _) = TextPropertyManager::from_entries(vec![entry("a.rs")]);

        let first = manager.shared();
        let second = manager.shared();
        assert!(
            Arc::ptr_eq(&first, &second),
            "each hand-off must be the same allocation, not a copy"
        );
        assert_eq!(first.len(), 1);
    }

    /// A reader holding an older set keeps seeing what it was handed, and
    /// the next hand-off is the new content — the property the snapshot
    /// relies on now that it re-shares every tick instead of comparing.
    #[test]
    fn a_replacement_hands_out_the_new_content() {
        let entry = |file: &str| TextPropertyEntry {
            properties: [("file".to_string(), serde_json::json!(file))]
                .into_iter()
                .collect(),
            ..TextPropertyEntry::text("row\n")
        };

        let (_, first_manager, _) = TextPropertyManager::from_entries(vec![entry("a.rs")]);
        let held = first_manager.shared();

        // Same shape, different content — the case a count-based freshness
        // check could not see.
        let (_, second_manager, _) = TextPropertyManager::from_entries(vec![entry("b.rs")]);
        let fresh = second_manager.shared();

        assert_eq!(held.len(), fresh.len(), "same shape");
        assert!(!Arc::ptr_eq(&held, &fresh), "different sets");
        assert_eq!(
            fresh[0].properties.get("file").and_then(|v| v.as_str()),
            Some("b.rs"),
            "the new set must carry the new content"
        );
        assert_eq!(
            held[0].properties.get("file").and_then(|v| v.as_str()),
            Some("a.rs"),
            "the held set is unaffected by the replacement"
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

    /// The bounded lookups must answer exactly what a scan of the whole
    /// set answers. The bound is `max_span`, so the shapes that could
    /// break it are the ones where an early property is far wider than
    /// its neighbours and still reaches a much later byte.
    #[test]
    fn bounded_lookup_matches_a_full_scan() {
        let prop = |start: usize, end: usize, id: &str| {
            let mut p = TextProperty::new(start, end);
            p.properties.insert("id".to_string(), json!(id));
            p
        };
        // Rows, plus a wide property covering all of them, plus one
        // nested inside a row — overlapping, nested and adjacent at once.
        let mut props = vec![prop(0, 400, "wide")];
        for row in 0..40 {
            props.push(prop(row * 10, row * 10 + 10, "row"));
        }
        props.push(prop(205, 207, "inner"));
        props.push(prop(600, 610, "far"));

        let mut manager = TextPropertyManager::new();
        manager.set_all(props.clone());

        let scan_at = |pos: usize| {
            let mut v: Vec<&str> = props
                .iter()
                .filter(|p| p.contains(pos))
                .filter_map(|p| p.get("id").and_then(|v| v.as_str()))
                .collect();
            v.sort_unstable();
            v
        };
        for pos in 0..640 {
            let mut got: Vec<&str> = manager
                .get_at(pos)
                .into_iter()
                .filter_map(|p| p.get("id").and_then(|v| v.as_str()))
                .collect();
            got.sort_unstable();
            assert_eq!(got, scan_at(pos), "get_at({pos})");
        }

        for start in (0..640).step_by(7) {
            for len in [0usize, 1, 5, 33] {
                let range = start..start + len;
                let mut want: Vec<&str> = props
                    .iter()
                    .filter(|p| p.overlaps(&range))
                    .filter_map(|p| p.get("id").and_then(|v| v.as_str()))
                    .collect();
                let mut got: Vec<&str> = manager
                    .get_overlapping(&range)
                    .into_iter()
                    .filter_map(|p| p.get("id").and_then(|v| v.as_str()))
                    .collect();
                want.sort_unstable();
                got.sort_unstable();
                assert_eq!(got, want, "get_overlapping({start}..{})", start + len);
            }
        }
    }
}
