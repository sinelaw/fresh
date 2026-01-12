//! Virtual text rendering infrastructure
//!
//! Provides a system for rendering virtual text that doesn't exist in the buffer.
//! Used for inlay hints (type annotations, parameter names), git blame headers, etc.
//!
//! Two types of virtual text are supported:
//! - **Inline**: Text inserted before/after a character (e.g., `: i32` type hints)
//! - **Line**: Full lines inserted above/below a position (e.g., git blame headers)
//!
//! Virtual text is rendered during the render phase by reading from VirtualTextManager.
//! The buffer content remains unchanged - we just inject extra styled text during rendering.
//!
//! ## Architecture
//!
//! This follows an Emacs-like model where:
//! 1. Plugins add virtual text in response to buffer changes (async, fire-and-forget)
//! 2. Virtual text is stored persistently with marker-based position tracking
//! 3. Render loop reads virtual text synchronously from memory (no async waiting)
//!
//! This ensures frame coherence: render always sees a consistent snapshot of virtual text.

use ratatui::style::Style;
use std::collections::HashMap;

use crate::model::marker::{MarkerId, MarkerList};

/// Position relative to the character at the marker position
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VirtualTextPosition {
    // ─── Inline positions (within a line) ───
    /// Render before the character (e.g., parameter hints: `/*count=*/5`)
    BeforeChar,
    /// Render after the character (e.g., type hints: `x: i32`)
    AfterChar,

    // ─── Line positions (full lines) ───
    /// Render as a full line ABOVE the line containing this position
    /// Used for git blame headers, section separators, etc.
    /// These lines do NOT get line numbers in the gutter.
    LineAbove,
    /// Render as a full line BELOW the line containing this position
    /// Used for inline documentation, fold previews, etc.
    /// These lines do NOT get line numbers in the gutter.
    LineBelow,
}

impl VirtualTextPosition {
    /// Returns true if this is a line-level position (LineAbove/LineBelow)
    pub fn is_line(&self) -> bool {
        matches!(self, Self::LineAbove | Self::LineBelow)
    }

    /// Returns true if this is an inline position (BeforeChar/AfterChar)
    pub fn is_inline(&self) -> bool {
        matches!(self, Self::BeforeChar | Self::AfterChar)
    }
}

/// Namespace for grouping virtual texts (for efficient bulk removal).
/// Similar to OverlayNamespace - plugins create a namespace once and use it for all their virtual texts.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VirtualTextNamespace(pub String);

impl VirtualTextNamespace {
    /// Create a namespace from a string (for plugin registration)
    pub fn from_string(s: String) -> Self {
        Self(s)
    }

    /// Get the internal string representation
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// A piece of virtual text to render at a specific position
#[derive(Debug, Clone)]
pub struct VirtualText {
    /// Marker tracking the position (auto-adjusts on edits)
    pub marker_id: MarkerId,
    /// Text to display (for LineAbove/LineBelow, this is the full line content)
    pub text: String,
    /// Styling (typically dimmed/gray for hints)
    pub style: Style,
    /// Where to render relative to the marker position
    pub position: VirtualTextPosition,
    /// Priority for ordering multiple items at same position (higher = later)
    pub priority: i32,
    /// Optional string identifier for this virtual text (for plugin use)
    pub string_id: Option<String>,
    /// Optional namespace for bulk removal (like Overlay's namespace)
    pub namespace: Option<VirtualTextNamespace>,
}

/// Unique identifier for a virtual text entry
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VirtualTextId(pub u64);

/// Manages virtual text entries for a buffer
///
/// Uses the marker system for position tracking, so virtual text automatically
/// adjusts when the buffer is edited.
pub struct VirtualTextManager {
    /// Map from virtual text ID to virtual text entry
    texts: HashMap<VirtualTextId, VirtualText>,
    /// Next ID to assign
    next_id: u64,
}

impl VirtualTextManager {
    /// Create a new empty manager
    pub fn new() -> Self {
        Self {
            texts: HashMap::new(),
            next_id: 0,
        }
    }

    /// Add a virtual text entry
    ///
    /// # Arguments
    /// * `marker_list` - The marker list to create a position marker in
    /// * `position` - Byte offset in the buffer
    /// * `text` - Text to display
    /// * `style` - Styling for the text
    /// * `vtext_position` - Whether to render before or after the character
    /// * `priority` - Ordering priority (higher = later in render order)
    ///
    /// # Returns
    /// The ID of the created virtual text entry
    pub fn add(
        &mut self,
        marker_list: &mut MarkerList,
        position: usize,
        text: String,
        style: Style,
        vtext_position: VirtualTextPosition,
        priority: i32,
    ) -> VirtualTextId {
        // Create marker at position
        // Use right affinity (false) so the marker stays with the following character
        let marker_id = marker_list.create(position, false);

        let id = VirtualTextId(self.next_id);
        self.next_id += 1;

        self.texts.insert(
            id,
            VirtualText {
                marker_id,
                text,
                style,
                position: vtext_position,
                priority,
                string_id: None,
                namespace: None,
            },
        );

        id
    }

    /// Add a virtual text entry with a string identifier
    ///
    /// This is useful for plugins that need to track and remove virtual texts by name.
    #[allow(clippy::too_many_arguments)]
    pub fn add_with_id(
        &mut self,
        marker_list: &mut MarkerList,
        position: usize,
        text: String,
        style: Style,
        vtext_position: VirtualTextPosition,
        priority: i32,
        string_id: String,
    ) -> VirtualTextId {
        let marker_id = marker_list.create(position, false);

        let id = VirtualTextId(self.next_id);
        self.next_id += 1;

        self.texts.insert(
            id,
            VirtualText {
                marker_id,
                text,
                style,
                position: vtext_position,
                priority,
                string_id: Some(string_id),
                namespace: None,
            },
        );

        id
    }

    /// Add a virtual line (LineAbove or LineBelow) with namespace for bulk removal
    ///
    /// This is the primary API for features like git blame headers.
    ///
    /// # Arguments
    /// * `marker_list` - The marker list to create a position marker in
    /// * `position` - Byte offset in the buffer (anchors the line to this position)
    /// * `text` - Full line content to display
    /// * `style` - Styling for the line
    /// * `placement` - LineAbove or LineBelow
    /// * `namespace` - Namespace for bulk removal (e.g., "git-blame")
    /// * `priority` - Ordering when multiple lines at same position
    #[allow(clippy::too_many_arguments)]
    pub fn add_line(
        &mut self,
        marker_list: &mut MarkerList,
        position: usize,
        text: String,
        style: Style,
        placement: VirtualTextPosition,
        namespace: VirtualTextNamespace,
        priority: i32,
    ) -> VirtualTextId {
        debug_assert!(
            placement.is_line(),
            "add_line requires LineAbove or LineBelow"
        );

        let marker_id = marker_list.create(position, false);

        let id = VirtualTextId(self.next_id);
        self.next_id += 1;

        self.texts.insert(
            id,
            VirtualText {
                marker_id,
                text,
                style,
                position: placement,
                priority,
                string_id: None,
                namespace: Some(namespace),
            },
        );

        id
    }

    /// Remove a virtual text entry by its string identifier
    pub fn remove_by_id(&mut self, marker_list: &mut MarkerList, string_id: &str) -> bool {
        // Find the entry with matching string_id
        let to_remove: Vec<VirtualTextId> = self
            .texts
            .iter()
            .filter_map(|(id, vtext)| {
                if vtext.string_id.as_deref() == Some(string_id) {
                    Some(*id)
                } else {
                    None
                }
            })
            .collect();

        let mut removed = false;
        for id in to_remove {
            if let Some(vtext) = self.texts.remove(&id) {
                marker_list.delete(vtext.marker_id);
                removed = true;
            }
        }
        removed
    }

    /// Remove all virtual text entries whose string_id starts with the given prefix
    pub fn remove_by_prefix(&mut self, marker_list: &mut MarkerList, prefix: &str) {
        // Collect markers to delete
        let markers_to_delete: Vec<(VirtualTextId, MarkerId)> = self
            .texts
            .iter()
            .filter_map(|(id, vtext)| {
                if let Some(ref sid) = vtext.string_id {
                    if sid.starts_with(prefix) {
                        return Some((*id, vtext.marker_id));
                    }
                }
                None
            })
            .collect();

        // Delete markers and remove entries
        for (id, marker_id) in markers_to_delete {
            marker_list.delete(marker_id);
            self.texts.remove(&id);
        }
    }

    /// Remove a virtual text entry
    pub fn remove(&mut self, marker_list: &mut MarkerList, id: VirtualTextId) -> bool {
        if let Some(vtext) = self.texts.remove(&id) {
            marker_list.delete(vtext.marker_id);
            true
        } else {
            false
        }
    }

    /// Clear all virtual text entries
    pub fn clear(&mut self, marker_list: &mut MarkerList) {
        for vtext in self.texts.values() {
            marker_list.delete(vtext.marker_id);
        }
        self.texts.clear();
    }

    /// Get the number of virtual text entries
    pub fn len(&self) -> usize {
        self.texts.len()
    }

    /// Check if there are no virtual text entries
    pub fn is_empty(&self) -> bool {
        self.texts.is_empty()
    }

    /// Query virtual texts in a byte range
    ///
    /// Returns a vector of (byte_position, &VirtualText) pairs, sorted by:
    /// 1. Byte position (ascending)
    /// 2. Priority (ascending, so higher priority renders later)
    ///
    /// # Arguments
    /// * `marker_list` - The marker list to query positions from
    /// * `start` - Start byte offset (inclusive)
    /// * `end` - End byte offset (exclusive)
    pub fn query_range(
        &self,
        marker_list: &MarkerList,
        start: usize,
        end: usize,
    ) -> Vec<(usize, &VirtualText)> {
        let mut results: Vec<(usize, &VirtualText)> = self
            .texts
            .values()
            .filter_map(|vtext| {
                let pos = marker_list.get_position(vtext.marker_id)?;
                if pos >= start && pos < end {
                    Some((pos, vtext))
                } else {
                    None
                }
            })
            .collect();

        // Sort by position, then by priority
        results.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.priority.cmp(&b.1.priority)));

        results
    }

    /// Build a lookup map for efficient per-character access during rendering
    ///
    /// Returns a HashMap where keys are byte positions and values are vectors
    /// of virtual texts at that position, sorted by priority.
    pub fn build_lookup(
        &self,
        marker_list: &MarkerList,
        start: usize,
        end: usize,
    ) -> HashMap<usize, Vec<&VirtualText>> {
        let mut lookup: HashMap<usize, Vec<&VirtualText>> = HashMap::new();

        for vtext in self.texts.values() {
            if let Some(pos) = marker_list.get_position(vtext.marker_id) {
                if pos >= start && pos < end {
                    lookup.entry(pos).or_default().push(vtext);
                }
            }
        }

        // Sort each position's texts by priority
        for texts in lookup.values_mut() {
            texts.sort_by_key(|vt| vt.priority);
        }

        lookup
    }

    /// Clear all virtual texts in a namespace
    ///
    /// This is the primary way plugins remove their virtual texts (e.g., before updating blame data).
    pub fn clear_namespace(
        &mut self,
        marker_list: &mut MarkerList,
        namespace: &VirtualTextNamespace,
    ) {
        let to_remove: Vec<VirtualTextId> = self
            .texts
            .iter()
            .filter_map(|(id, vtext)| {
                if vtext.namespace.as_ref() == Some(namespace) {
                    Some(*id)
                } else {
                    None
                }
            })
            .collect();

        for id in to_remove {
            if let Some(vtext) = self.texts.remove(&id) {
                marker_list.delete(vtext.marker_id);
            }
        }
    }

    /// Query only virtual LINES (LineAbove/LineBelow) in a byte range
    ///
    /// Used by the render pipeline to inject header/footer lines.
    /// Returns (byte_position, &VirtualText) pairs sorted by position then priority.
    pub fn query_lines_in_range(
        &self,
        marker_list: &MarkerList,
        start: usize,
        end: usize,
    ) -> Vec<(usize, &VirtualText)> {
        let mut results: Vec<(usize, &VirtualText)> = self
            .texts
            .values()
            .filter(|vtext| vtext.position.is_line())
            .filter_map(|vtext| {
                let pos = marker_list.get_position(vtext.marker_id)?;
                if pos >= start && pos < end {
                    Some((pos, vtext))
                } else {
                    None
                }
            })
            .collect();

        // Sort by position, then by priority
        results.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.priority.cmp(&b.1.priority)));

        results
    }

    /// Query only INLINE virtual texts (BeforeChar/AfterChar) in a byte range
    ///
    /// Used by the render pipeline to inject inline hints.
    pub fn query_inline_in_range(
        &self,
        marker_list: &MarkerList,
        start: usize,
        end: usize,
    ) -> Vec<(usize, &VirtualText)> {
        let mut results: Vec<(usize, &VirtualText)> = self
            .texts
            .values()
            .filter(|vtext| vtext.position.is_inline())
            .filter_map(|vtext| {
                let pos = marker_list.get_position(vtext.marker_id)?;
                if pos >= start && pos < end {
                    Some((pos, vtext))
                } else {
                    None
                }
            })
            .collect();

        // Sort by position, then by priority
        results.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.priority.cmp(&b.1.priority)));

        results
    }

    /// Build a lookup map for virtual LINES, keyed by the line's anchor byte position
    ///
    /// For each source line, the renderer can quickly check if there are
    /// LineAbove or LineBelow virtual texts anchored to positions within that line.
    pub fn build_lines_lookup(
        &self,
        marker_list: &MarkerList,
        start: usize,
        end: usize,
    ) -> HashMap<usize, Vec<&VirtualText>> {
        let mut lookup: HashMap<usize, Vec<&VirtualText>> = HashMap::new();

        for vtext in self.texts.values() {
            if !vtext.position.is_line() {
                continue;
            }
            if let Some(pos) = marker_list.get_position(vtext.marker_id) {
                if pos >= start && pos < end {
                    lookup.entry(pos).or_default().push(vtext);
                }
            }
        }

        // Sort each position's texts by priority
        for texts in lookup.values_mut() {
            texts.sort_by_key(|vt| vt.priority);
        }

        lookup
    }
}

impl Default for VirtualTextManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::style::Color;

    fn hint_style() -> Style {
        Style::default().fg(Color::DarkGray)
    }

    #[test]
    fn test_new_manager() {
        let manager = VirtualTextManager::new();
        assert_eq!(manager.len(), 0);
        assert!(manager.is_empty());
    }

    #[test]
    fn test_add_virtual_text() {
        let mut marker_list = MarkerList::new();
        let mut manager = VirtualTextManager::new();

        let id = manager.add(
            &mut marker_list,
            10,
            ": i32".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );

        assert_eq!(manager.len(), 1);
        assert!(!manager.is_empty());
        assert_eq!(id.0, 0);
    }

    #[test]
    fn test_remove_virtual_text() {
        let mut marker_list = MarkerList::new();
        let mut manager = VirtualTextManager::new();

        let id = manager.add(
            &mut marker_list,
            10,
            ": i32".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );

        assert_eq!(manager.len(), 1);

        let removed = manager.remove(&mut marker_list, id);
        assert!(removed);
        assert_eq!(manager.len(), 0);

        // Marker should also be removed
        assert_eq!(marker_list.marker_count(), 0);
    }

    #[test]
    fn test_remove_nonexistent() {
        let mut marker_list = MarkerList::new();
        let mut manager = VirtualTextManager::new();

        let removed = manager.remove(&mut marker_list, VirtualTextId(999));
        assert!(!removed);
    }

    #[test]
    fn test_clear() {
        let mut marker_list = MarkerList::new();
        let mut manager = VirtualTextManager::new();

        manager.add(
            &mut marker_list,
            10,
            ": i32".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );
        manager.add(
            &mut marker_list,
            20,
            ": String".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );

        assert_eq!(manager.len(), 2);
        assert_eq!(marker_list.marker_count(), 2);

        manager.clear(&mut marker_list);

        assert_eq!(manager.len(), 0);
        assert_eq!(marker_list.marker_count(), 0);
    }

    #[test]
    fn test_query_range() {
        let mut marker_list = MarkerList::new();
        let mut manager = VirtualTextManager::new();

        // Add three virtual texts at positions 10, 20, 30
        manager.add(
            &mut marker_list,
            10,
            ": i32".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );
        manager.add(
            &mut marker_list,
            20,
            ": String".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );
        manager.add(
            &mut marker_list,
            30,
            ": bool".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );

        // Query range [15, 35) should return positions 20 and 30
        let results = manager.query_range(&marker_list, 15, 35);
        assert_eq!(results.len(), 2);
        assert_eq!(results[0].0, 20);
        assert_eq!(results[0].1.text, ": String");
        assert_eq!(results[1].0, 30);
        assert_eq!(results[1].1.text, ": bool");

        // Query range [0, 15) should return position 10
        let results = manager.query_range(&marker_list, 0, 15);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].0, 10);
        assert_eq!(results[0].1.text, ": i32");
    }

    #[test]
    fn test_query_empty_range() {
        let mut marker_list = MarkerList::new();
        let mut manager = VirtualTextManager::new();

        manager.add(
            &mut marker_list,
            10,
            ": i32".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );

        // Query range with no virtual texts
        let results = manager.query_range(&marker_list, 100, 200);
        assert!(results.is_empty());
    }

    #[test]
    fn test_priority_ordering() {
        let mut marker_list = MarkerList::new();
        let mut manager = VirtualTextManager::new();

        // Add multiple virtual texts at the same position with different priorities
        manager.add(
            &mut marker_list,
            10,
            "low".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );
        manager.add(
            &mut marker_list,
            10,
            "high".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            10,
        );
        manager.add(
            &mut marker_list,
            10,
            "medium".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            5,
        );

        let results = manager.query_range(&marker_list, 0, 20);
        assert_eq!(results.len(), 3);
        // Should be sorted by priority: 0, 5, 10
        assert_eq!(results[0].1.text, "low");
        assert_eq!(results[1].1.text, "medium");
        assert_eq!(results[2].1.text, "high");
    }

    #[test]
    fn test_build_lookup() {
        let mut marker_list = MarkerList::new();
        let mut manager = VirtualTextManager::new();

        manager.add(
            &mut marker_list,
            10,
            ": i32".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );
        manager.add(
            &mut marker_list,
            10,
            " = 5".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            1,
        );
        manager.add(
            &mut marker_list,
            20,
            ": String".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );

        let lookup = manager.build_lookup(&marker_list, 0, 30);

        assert_eq!(lookup.len(), 2); // Two unique positions

        let at_10 = lookup.get(&10).unwrap();
        assert_eq!(at_10.len(), 2);
        assert_eq!(at_10[0].text, ": i32"); // priority 0
        assert_eq!(at_10[1].text, " = 5"); // priority 1

        let at_20 = lookup.get(&20).unwrap();
        assert_eq!(at_20.len(), 1);
        assert_eq!(at_20[0].text, ": String");
    }

    #[test]
    fn test_position_tracking_after_insert() {
        let mut marker_list = MarkerList::new();
        let mut manager = VirtualTextManager::new();

        manager.add(
            &mut marker_list,
            10,
            ": i32".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );

        // Insert 5 bytes before position 10
        marker_list.adjust_for_insert(5, 5);

        // Virtual text should now be at position 15
        let results = manager.query_range(&marker_list, 0, 20);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].0, 15);
    }

    #[test]
    fn test_position_tracking_after_delete() {
        let mut marker_list = MarkerList::new();
        let mut manager = VirtualTextManager::new();

        manager.add(
            &mut marker_list,
            20,
            ": i32".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );

        // Delete 5 bytes before position 20 (at position 10)
        marker_list.adjust_for_delete(10, 5);

        // Virtual text should now be at position 15
        let results = manager.query_range(&marker_list, 0, 20);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].0, 15);
    }

    #[test]
    fn test_before_and_after_positions() {
        let mut marker_list = MarkerList::new();
        let mut manager = VirtualTextManager::new();

        manager.add(
            &mut marker_list,
            10,
            "/*param=*/".to_string(),
            hint_style(),
            VirtualTextPosition::BeforeChar,
            0,
        );
        manager.add(
            &mut marker_list,
            10,
            ": Type".to_string(),
            hint_style(),
            VirtualTextPosition::AfterChar,
            0,
        );

        let lookup = manager.build_lookup(&marker_list, 0, 20);
        let at_10 = lookup.get(&10).unwrap();

        assert_eq!(at_10.len(), 2);
        // Both at same position, check they have different positions
        let before = at_10
            .iter()
            .find(|vt| vt.position == VirtualTextPosition::BeforeChar);
        let after = at_10
            .iter()
            .find(|vt| vt.position == VirtualTextPosition::AfterChar);

        assert!(before.is_some());
        assert!(after.is_some());
        assert_eq!(before.unwrap().text, "/*param=*/");
        assert_eq!(after.unwrap().text, ": Type");
    }
}
