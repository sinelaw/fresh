use crate::model::marker::{MarkerId, MarkerList};
use ratatui::style::{Color, Style};
use std::collections::HashMap;
use std::ops::Range;

// Re-export types from fresh-core for shared type usage
pub use fresh_core::overlay::{OverlayHandle, OverlayNamespace};

/// Overlay face - defines the visual appearance of an overlay
#[derive(Debug, Clone, PartialEq)]
pub enum OverlayFace {
    /// Underline with a specific style
    Underline { color: Color, style: UnderlineStyle },
    /// Background color
    Background { color: Color },
    /// Foreground (text) color
    Foreground { color: Color },
    /// Combined style with multiple attributes (fully resolved colors)
    Style { style: Style },
    /// Style with theme key references - resolved at render time
    ///
    /// Theme keys like "ui.status_bar_fg" or "editor.selection_bg"
    /// are resolved when rendering, so overlays automatically update
    /// when the theme changes.
    ThemedStyle {
        /// Fallback style with RGB colors (used if theme keys don't resolve)
        fallback_style: Style,
        /// Theme key for foreground color (e.g., "ui.status_bar_fg")
        fg_theme: Option<String>,
        /// Theme key for background color (e.g., "editor.selection_bg")
        bg_theme: Option<String>,
    },
}

impl OverlayFace {
    /// Create an OverlayFace from OverlayOptions
    ///
    /// If the options contain theme key references, creates a ThemedStyle
    /// for runtime resolution. Otherwise creates a fully resolved Style.
    pub fn from_options(options: &fresh_core::api::OverlayOptions) -> Self {
        use crate::view::theme::named_color_from_str;
        use ratatui::style::Modifier;

        let mut style = Style::default();

        if let Some(ref fg) = options.fg {
            if let Some((r, g, b)) = fg.as_rgb() {
                style = style.fg(Color::Rgb(r, g, b));
            } else if let Some(key) = fg.as_theme_key() {
                if let Some(color) = named_color_from_str(key) {
                    style = style.fg(color);
                }
            }
        }

        if let Some(ref bg) = options.bg {
            if let Some((r, g, b)) = bg.as_rgb() {
                style = style.bg(Color::Rgb(r, g, b));
            } else if let Some(key) = bg.as_theme_key() {
                if let Some(color) = named_color_from_str(key) {
                    style = style.bg(color);
                }
            }
        }

        let mut modifiers = Modifier::empty();
        if options.bold {
            modifiers |= Modifier::BOLD;
        }
        if options.italic {
            modifiers |= Modifier::ITALIC;
        }
        if options.underline {
            modifiers |= Modifier::UNDERLINED;
        }
        if options.strikethrough {
            modifiers |= Modifier::CROSSED_OUT;
        }
        if !modifiers.is_empty() {
            style = style.add_modifier(modifiers);
        }

        // Only treat as theme keys if they're NOT recognized named colors
        // (named colors were already resolved to concrete Color values above)
        let fg_theme = options
            .fg
            .as_ref()
            .and_then(|c| c.as_theme_key())
            .filter(|key| named_color_from_str(key).is_none())
            .map(String::from);
        let bg_theme = options
            .bg
            .as_ref()
            .and_then(|c| c.as_theme_key())
            .filter(|key| named_color_from_str(key).is_none())
            .map(String::from);

        if fg_theme.is_some() || bg_theme.is_some() {
            OverlayFace::ThemedStyle {
                fallback_style: style,
                fg_theme,
                bg_theme,
            }
        } else {
            OverlayFace::Style { style }
        }
    }
}

/// Style of underline
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnderlineStyle {
    /// Straight line
    Straight,
    /// Wavy/squiggly line (for errors)
    Wavy,
    /// Dotted line
    Dotted,
    /// Dashed line
    Dashed,
}

/// Priority for overlay z-ordering
/// Higher priority overlays are rendered on top of lower priority ones
pub type Priority = i32;

/// An overlay represents a visual decoration over a range of text
/// Uses markers for content-anchored positions that automatically adjust with edits
#[derive(Debug, Clone)]
pub struct Overlay {
    /// Unique handle for this overlay (opaque, for removal by handle)
    pub handle: OverlayHandle,

    /// Namespace this overlay belongs to (for bulk removal)
    pub namespace: Option<OverlayNamespace>,

    /// Start marker (left affinity - stays before inserted text)
    pub start_marker: MarkerId,

    /// End marker (right affinity - moves after inserted text)
    pub end_marker: MarkerId,

    /// Visual appearance of the overlay
    pub face: OverlayFace,

    /// Priority for z-ordering (higher = on top)
    pub priority: Priority,

    /// Optional tooltip/message to show when hovering over this overlay
    pub message: Option<String>,

    /// Whether to extend the overlay's background to the end of the visual line
    /// Used for full-width line highlighting (e.g., in diff views)
    pub extend_to_line_end: bool,

    /// Optional URL for OSC 8 terminal hyperlinks.
    /// When set, the rendered text in this overlay becomes a clickable hyperlink.
    pub url: Option<String>,

    /// Theme key that produced this overlay's primary color (e.g. "diagnostic.warning_bg").
    /// Recorded at creation time so the theme inspector can show the exact key
    /// without reverse-mapping colors.
    pub theme_key: Option<&'static str>,
}

impl Overlay {
    /// Create a new overlay with markers at the given range
    ///
    /// # Arguments
    /// * `marker_list` - MarkerList to create markers in
    /// * `range` - Byte range for the overlay
    /// * `face` - Visual appearance
    ///
    /// Returns the overlay (which contains its handle for later removal)
    pub fn new(marker_list: &mut MarkerList, range: Range<usize>, face: OverlayFace) -> Self {
        let start_marker = marker_list.create(range.start, true); // left affinity
        let end_marker = marker_list.create(range.end, false); // right affinity

        Self {
            handle: OverlayHandle::new(),
            namespace: None,
            start_marker,
            end_marker,
            face,
            priority: 0,
            message: None,
            extend_to_line_end: false,
            url: None,
            theme_key: None,
        }
    }

    /// Create an overlay with a namespace (for bulk removal)
    pub fn with_namespace(
        marker_list: &mut MarkerList,
        range: Range<usize>,
        face: OverlayFace,
        namespace: OverlayNamespace,
    ) -> Self {
        let mut overlay = Self::new(marker_list, range, face);
        overlay.namespace = Some(namespace);
        overlay
    }

    /// Create an overlay with a specific priority
    pub fn with_priority(
        marker_list: &mut MarkerList,
        range: Range<usize>,
        face: OverlayFace,
        priority: Priority,
    ) -> Self {
        let mut overlay = Self::new(marker_list, range, face);
        overlay.priority = priority;
        overlay
    }

    /// Add a message/tooltip to this overlay
    pub fn with_message(mut self, message: String) -> Self {
        self.message = Some(message);
        self
    }

    /// Set the priority
    pub fn with_priority_value(mut self, priority: Priority) -> Self {
        self.priority = priority;
        self
    }

    /// Set the namespace
    pub fn with_namespace_value(mut self, namespace: OverlayNamespace) -> Self {
        self.namespace = Some(namespace);
        self
    }

    /// Set whether to extend the overlay to the end of the visual line
    pub fn with_extend_to_line_end(mut self, extend: bool) -> Self {
        self.extend_to_line_end = extend;
        self
    }

    /// Set the theme key that produced this overlay's color
    pub fn with_theme_key(mut self, key: &'static str) -> Self {
        self.theme_key = Some(key);
        self
    }

    /// Get the current byte range by resolving markers
    /// This is called once per frame during rendering setup
    pub fn range(&self, marker_list: &MarkerList) -> Range<usize> {
        let start = marker_list.get_position(self.start_marker).unwrap_or(0);
        let end = marker_list.get_position(self.end_marker).unwrap_or(0);
        start..end
    }

    /// Check if this overlay contains a position
    pub fn contains(&self, position: usize, marker_list: &MarkerList) -> bool {
        self.range(marker_list).contains(&position)
    }

    /// Check if this overlay overlaps with a range
    pub fn overlaps(&self, range: &Range<usize>, marker_list: &MarkerList) -> bool {
        let self_range = self.range(marker_list);
        self_range.start < range.end && range.start < self_range.end
    }
}

/// Manages overlays for a buffer
/// Overlays are sorted by priority for efficient rendering
#[derive(Debug, Clone)]
pub struct OverlayManager {
    /// All active overlays, indexed for O(1) lookup by handle
    overlays: Vec<Overlay>,
    /// `MarkerId -> index into overlays` for O(log N + k) `remove_in_range`.
    /// Both endpoints of each overlay are registered. Kept in sync with
    /// every push / swap_remove on `overlays`, and rebuilt after any sort.
    marker_to_idx: HashMap<MarkerId, usize>,
}

impl OverlayManager {
    /// Create a new empty overlay manager
    pub fn new() -> Self {
        Self {
            overlays: Vec::new(),
            marker_to_idx: HashMap::new(),
        }
    }

    /// Add an overlay and return its handle for later removal
    pub fn add(&mut self, overlay: Overlay) -> OverlayHandle {
        let handle = overlay.handle.clone();
        // Binary-search the priority-ordered insertion point and shift in
        // place. Avoids the O(n²·log n) sort-on-every-add the prior impl
        // had — the docstring on `extend` warned about this.
        let priority = overlay.priority;
        let pos = self.overlays.partition_point(|o| o.priority <= priority);
        self.overlays.insert(pos, overlay);
        // Every entry from `pos` onward shifted by one — re-index that tail.
        // Tail length is small when adds are append-shaped (the common case
        // for plugins that emit per-line clear+rebuild).
        for (i, o) in self.overlays.iter().enumerate().skip(pos) {
            self.marker_to_idx.insert(o.start_marker, i);
            self.marker_to_idx.insert(o.end_marker, i);
        }
        handle
    }

    /// Append many overlays at once, sorting a single time at the end.
    ///
    /// `add` re-sorts the whole vector on every insertion, which is O(n² log n)
    /// when a caller has N overlays to add. Use this instead when rebuilding an
    /// overlay set from scratch (e.g. `set_virtual_buffer_content`), where the
    /// caller already owns the full list up front.
    pub fn extend<I: IntoIterator<Item = Overlay>>(&mut self, overlays: I) {
        self.overlays.extend(overlays);
        self.overlays.sort_by_key(|o| o.priority);
        self.rebuild_marker_index();
    }

    /// Remove an overlay by its handle
    pub fn remove_by_handle(
        &mut self,
        handle: &OverlayHandle,
        marker_list: &mut MarkerList,
    ) -> bool {
        if let Some(pos) = self.overlays.iter().position(|o| &o.handle == handle) {
            let overlay = self.overlays.remove(pos);
            self.marker_to_idx.remove(&overlay.start_marker);
            self.marker_to_idx.remove(&overlay.end_marker);
            // Vec::remove shifts every subsequent entry down by one — repair.
            for (i, o) in self.overlays.iter().enumerate().skip(pos) {
                self.marker_to_idx.insert(o.start_marker, i);
                self.marker_to_idx.insert(o.end_marker, i);
            }
            marker_list.delete(overlay.start_marker);
            marker_list.delete(overlay.end_marker);
            true
        } else {
            false
        }
    }

    /// Remove all overlays in a namespace
    pub fn clear_namespace(&mut self, namespace: &OverlayNamespace, marker_list: &mut MarkerList) {
        let mut indices: Vec<usize> = self
            .overlays
            .iter()
            .enumerate()
            .filter_map(|(i, o)| (o.namespace.as_ref() == Some(namespace)).then_some(i))
            .collect();
        if indices.is_empty() {
            return;
        }
        indices.sort_unstable_by(|a, b| b.cmp(a));
        for idx in indices {
            self.swap_remove_at(idx, marker_list);
        }
        // Restore priority order after swap_removes.
        self.overlays.sort_by_key(|o| o.priority);
        self.rebuild_marker_index();
    }

    /// Replace overlays in a namespace that overlap a range with new overlays.
    ///
    /// This preserves overlays outside the range, which helps avoid flicker and
    /// unnecessary marker churn during viewport-only updates.
    pub fn replace_range_in_namespace(
        &mut self,
        namespace: &OverlayNamespace,
        range: &Range<usize>,
        mut new_overlays: Vec<Overlay>,
        marker_list: &mut MarkerList,
    ) {
        // Find overlays in this namespace that overlap the range. Use the
        // marker-tree to narrow candidates; verify each candidate's true
        // range and namespace before removing.
        if range.start < range.end {
            let hits = marker_list.query_range(range.start, range.end);
            let mut candidates: Vec<usize> = hits
                .iter()
                .filter_map(|(mid, _, _)| self.marker_to_idx.get(mid).copied())
                .collect();
            candidates.sort_unstable();
            candidates.dedup();
            let mut to_remove: Vec<usize> = candidates
                .into_iter()
                .filter(|&idx| {
                    let o = &self.overlays[idx];
                    if o.namespace.as_ref() != Some(namespace) {
                        return false;
                    }
                    let start = marker_list.get_position(o.start_marker).unwrap_or(0);
                    let end = marker_list.get_position(o.end_marker).unwrap_or(0);
                    start < range.end && range.start < end
                })
                .collect();
            to_remove.sort_unstable_by(|a, b| b.cmp(a));
            for idx in to_remove {
                self.swap_remove_at(idx, marker_list);
            }
        }

        if !new_overlays.is_empty() {
            self.overlays.append(&mut new_overlays);
        }
        self.overlays.sort_by_key(|o| o.priority);
        self.rebuild_marker_index();
    }

    /// Remove all overlays in a range and clean up their markers
    pub fn remove_in_range(&mut self, range: &Range<usize>, marker_list: &mut MarkerList) {
        // O(log N + k) for the lookup; restoring the priority-sorted
        // invariant after `swap_remove` is O(N) (adaptive sort on a
        // near-sorted vec) plus O(N) marker_to_idx rebuild. For typical
        // markdown_compose workloads where overlays in a buffer share
        // the same priority, the adaptive sort is a no-op pass.
        // Spanning overlays (start < range.start && end > range.end) are
        // not detected — same precondition as ConcealManager.
        if range.start >= range.end {
            return;
        }
        let hits = marker_list.query_range(range.start, range.end);
        if hits.is_empty() {
            return;
        }
        let mut candidates: Vec<usize> = hits
            .iter()
            .filter_map(|(mid, _, _)| self.marker_to_idx.get(mid).copied())
            .collect();
        candidates.sort_unstable();
        candidates.dedup();

        let mut to_remove: Vec<usize> = candidates
            .into_iter()
            .filter(|&idx| {
                let o = &self.overlays[idx];
                let start = marker_list.get_position(o.start_marker).unwrap_or(0);
                let end = marker_list.get_position(o.end_marker).unwrap_or(0);
                start < range.end && range.start < end
            })
            .collect();
        if to_remove.is_empty() {
            return;
        }
        to_remove.sort_unstable_by(|a, b| b.cmp(a));
        for idx in to_remove {
            self.swap_remove_at(idx, marker_list);
        }
        // Restore priority order broken by swap_removes.
        self.overlays.sort_by_key(|o| o.priority);
        self.rebuild_marker_index();
    }

    /// Clear all overlays and their markers
    pub fn clear(&mut self, marker_list: &mut MarkerList) {
        for overlay in &self.overlays {
            marker_list.delete(overlay.start_marker);
            marker_list.delete(overlay.end_marker);
        }
        self.overlays.clear();
        self.marker_to_idx.clear();
    }

    /// Swap-remove the entry at `idx`, deleting its markers and patching
    /// `marker_to_idx` for whatever entry got swapped in. Caller is
    /// responsible for restoring sort order if needed.
    fn swap_remove_at(&mut self, idx: usize, marker_list: &mut MarkerList) {
        let removed = self.overlays.swap_remove(idx);
        self.marker_to_idx.remove(&removed.start_marker);
        self.marker_to_idx.remove(&removed.end_marker);
        marker_list.delete(removed.start_marker);
        marker_list.delete(removed.end_marker);
        if let Some(moved) = self.overlays.get(idx) {
            self.marker_to_idx.insert(moved.start_marker, idx);
            self.marker_to_idx.insert(moved.end_marker, idx);
        }
    }

    /// Rebuild `marker_to_idx` from the current `overlays` order.
    /// Called after sorts that scramble indices.
    fn rebuild_marker_index(&mut self) {
        self.marker_to_idx.clear();
        for (i, o) in self.overlays.iter().enumerate() {
            self.marker_to_idx.insert(o.start_marker, i);
            self.marker_to_idx.insert(o.end_marker, i);
        }
    }

    /// Get all overlays at a specific position, sorted by priority
    pub fn at_position(&self, position: usize, marker_list: &MarkerList) -> Vec<&Overlay> {
        self.overlays
            .iter()
            .filter(|o| {
                let range = o.range(marker_list);
                range.contains(&position)
            })
            .collect()
    }

    /// Get all overlays that overlap with a range, sorted by priority
    pub fn in_range(&self, range: &Range<usize>, marker_list: &MarkerList) -> Vec<&Overlay> {
        self.overlays
            .iter()
            .filter(|o| o.overlaps(range, marker_list))
            .collect()
    }

    /// Query overlays in a viewport range efficiently using the marker interval tree
    ///
    /// This is much faster than calling `at_position()` for every character in the range.
    /// Returns overlays with their resolved byte ranges.
    ///
    /// # Performance
    /// - Old approach: O(N * M) where N = positions to check, M = overlay count
    /// - This approach: O(log M + k) where k = overlays in viewport (typically 2-10)
    pub fn query_viewport(
        &self,
        start: usize,
        end: usize,
        marker_list: &MarkerList,
    ) -> Vec<(&Overlay, Range<usize>)> {
        use std::collections::HashMap;

        // Query the marker interval tree once for all markers in viewport
        // This is O(log N + k) where k = markers in viewport
        let visible_markers = marker_list.query_range(start, end);

        // Build a quick lookup map: marker_id -> position
        let marker_positions: HashMap<_, _> = visible_markers
            .into_iter()
            .map(|(id, start, _end)| (id, start))
            .collect();

        // Find overlays whose markers overlap with the viewport.
        // At least one marker must be in the viewport, but the other may be
        // outside (e.g. a multi-line overlay partially scrolled out of view).
        // For the out-of-viewport marker, fall back to resolving its position
        // directly from the marker list.
        self.overlays
            .iter()
            .filter_map(|overlay| {
                let start_in_vp = marker_positions.get(&overlay.start_marker).copied();
                let end_in_vp = marker_positions.get(&overlay.end_marker).copied();

                // At least one marker must be in the viewport for the overlay
                // to be visible at all
                if start_in_vp.is_none() && end_in_vp.is_none() {
                    return None;
                }

                // For the marker outside the viewport, resolve its position directly
                let start_pos =
                    start_in_vp.or_else(|| marker_list.get_position(overlay.start_marker))?;
                let end_pos = end_in_vp.or_else(|| marker_list.get_position(overlay.end_marker))?;

                let range = start_pos..end_pos;

                // Only include if actually overlaps viewport.
                // For zero-width ranges (e.g. diagnostics at a single position),
                // check that the point is within [start, end] (inclusive).
                // For non-zero ranges, check standard overlap: start < end && end > start.
                let included = if range.start == range.end {
                    range.start >= start && range.start <= end
                } else {
                    range.start < end && range.end > start
                };

                if included {
                    Some((overlay, range))
                } else {
                    None
                }
            })
            .collect()
    }

    /// Get overlay by handle
    pub fn get_by_handle(&self, handle: &OverlayHandle) -> Option<&Overlay> {
        self.overlays.iter().find(|o| &o.handle == handle)
    }

    /// Get mutable overlay by handle
    pub fn get_by_handle_mut(&mut self, handle: &OverlayHandle) -> Option<&mut Overlay> {
        self.overlays.iter_mut().find(|o| &o.handle == handle)
    }

    /// Get total number of overlays
    pub fn len(&self) -> usize {
        self.overlays.len()
    }

    /// Check if there are any overlays
    pub fn is_empty(&self) -> bool {
        self.overlays.is_empty()
    }

    /// Get all overlays (for rendering)
    pub fn all(&self) -> &[Overlay] {
        &self.overlays
    }

    /// Test-only: assert `marker_to_idx` is consistent with `overlays`,
    /// and that priorities are non-decreasing along the vector.
    /// Panics on any divergence. Used by property tests.
    #[cfg(test)]
    fn check_invariants(&self) {
        assert_eq!(
            self.marker_to_idx.len(),
            self.overlays.len() * 2,
            "marker_to_idx size != 2 * overlays.len()"
        );
        for (i, o) in self.overlays.iter().enumerate() {
            assert_eq!(
                self.marker_to_idx.get(&o.start_marker).copied(),
                Some(i),
                "start_marker {:?} of overlay {} mismapped",
                o.start_marker,
                i,
            );
            assert_eq!(
                self.marker_to_idx.get(&o.end_marker).copied(),
                Some(i),
                "end_marker {:?} of overlay {} mismapped",
                o.end_marker,
                i,
            );
        }
        // Priority order — only enforceable when nothing is mid-cycle.
        // Tests check this via `assert_priority_sorted` after points
        // where the invariant is supposed to hold (e.g. after `add`).
    }

    /// Test-only: assert overlays are non-decreasing by priority.
    #[cfg(test)]
    fn assert_priority_sorted(&self) {
        for w in self.overlays.windows(2) {
            assert!(
                w[0].priority <= w[1].priority,
                "priority order broken: {} after {}",
                w[1].priority,
                w[0].priority,
            );
        }
    }
}

impl Default for OverlayManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Helper functions for creating common overlay types
impl Overlay {
    /// Create an error underline overlay (wavy red line)
    pub fn error(
        marker_list: &mut MarkerList,
        range: Range<usize>,
        message: Option<String>,
    ) -> Self {
        let mut overlay = Self::with_priority(
            marker_list,
            range,
            OverlayFace::Underline {
                color: Color::Red,
                style: UnderlineStyle::Wavy,
            },
            10, // Higher priority for errors
        );
        overlay.message = message;
        overlay
    }

    /// Create a warning underline overlay (wavy yellow line)
    pub fn warning(
        marker_list: &mut MarkerList,
        range: Range<usize>,
        message: Option<String>,
    ) -> Self {
        let mut overlay = Self::with_priority(
            marker_list,
            range,
            OverlayFace::Underline {
                color: Color::Yellow,
                style: UnderlineStyle::Wavy,
            },
            5, // Medium priority for warnings
        );
        overlay.message = message;
        overlay
    }

    /// Create an info underline overlay (wavy blue line)
    pub fn info(
        marker_list: &mut MarkerList,
        range: Range<usize>,
        message: Option<String>,
    ) -> Self {
        let mut overlay = Self::with_priority(
            marker_list,
            range,
            OverlayFace::Underline {
                color: Color::Blue,
                style: UnderlineStyle::Wavy,
            },
            3, // Lower priority for info
        );
        overlay.message = message;
        overlay
    }

    /// Create a hint underline overlay (dotted gray line)
    pub fn hint(
        marker_list: &mut MarkerList,
        range: Range<usize>,
        message: Option<String>,
    ) -> Self {
        let mut overlay = Self::with_priority(
            marker_list,
            range,
            OverlayFace::Underline {
                color: Color::Gray,
                style: UnderlineStyle::Dotted,
            },
            1, // Lowest priority for hints
        );
        overlay.message = message;
        overlay
    }

    /// Create a selection highlight overlay
    pub fn selection(marker_list: &mut MarkerList, range: Range<usize>) -> Self {
        let mut overlay = Self::with_priority(
            marker_list,
            range,
            OverlayFace::Background {
                color: Color::Rgb(38, 79, 120), // VSCode-like selection color
            },
            -10, // Very low priority so it's under other overlays
        );
        overlay.theme_key = Some("editor.selection_bg");
        overlay
    }

    /// Create a search result highlight overlay
    pub fn search_match(marker_list: &mut MarkerList, range: Range<usize>) -> Self {
        let mut overlay = Self::with_priority(
            marker_list,
            range,
            OverlayFace::Background {
                color: Color::Rgb(72, 72, 0), // Yellow-ish highlight
            },
            -5, // Low priority
        );
        overlay.theme_key = Some("search.match_bg");
        overlay
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_overlay_creation_with_markers() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);

        let overlay = Overlay::new(
            &mut marker_list,
            5..10,
            OverlayFace::Background { color: Color::Red },
        );

        assert_eq!(marker_list.get_position(overlay.start_marker), Some(5));
        assert_eq!(marker_list.get_position(overlay.end_marker), Some(10));
        assert_eq!(overlay.range(&marker_list), 5..10);
    }

    #[test]
    fn test_overlay_adjusts_with_insert() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);

        let overlay = Overlay::new(
            &mut marker_list,
            10..20,
            OverlayFace::Background { color: Color::Red },
        );

        // Insert before overlay
        marker_list.adjust_for_insert(5, 10);

        // Overlay should have moved forward
        assert_eq!(overlay.range(&marker_list), 20..30);
    }

    #[test]
    fn test_overlay_adjusts_with_delete() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);

        let overlay = Overlay::new(
            &mut marker_list,
            20..30,
            OverlayFace::Background { color: Color::Red },
        );

        // Delete before overlay
        marker_list.adjust_for_delete(5, 10);

        // Overlay should have moved backward
        assert_eq!(overlay.range(&marker_list), 10..20);
    }

    #[test]
    fn test_overlay_manager_add_remove() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);
        let mut manager = OverlayManager::new();

        let overlay = Overlay::new(
            &mut marker_list,
            5..10,
            OverlayFace::Background { color: Color::Red },
        );

        let handle = manager.add(overlay);
        assert_eq!(manager.len(), 1);

        manager.remove_by_handle(&handle, &mut marker_list);
        assert_eq!(manager.len(), 0);
    }

    #[test]
    fn test_overlay_namespace_clear() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);
        let mut manager = OverlayManager::new();

        let ns = OverlayNamespace::from_string("todo".to_string());

        // Add overlays in namespace
        let overlay1 = Overlay::with_namespace(
            &mut marker_list,
            5..10,
            OverlayFace::Background { color: Color::Red },
            ns.clone(),
        );
        let overlay2 = Overlay::with_namespace(
            &mut marker_list,
            15..20,
            OverlayFace::Background { color: Color::Blue },
            ns.clone(),
        );
        // Add overlay without namespace
        let overlay3 = Overlay::new(
            &mut marker_list,
            25..30,
            OverlayFace::Background {
                color: Color::Green,
            },
        );

        manager.add(overlay1);
        manager.add(overlay2);
        manager.add(overlay3);
        assert_eq!(manager.len(), 3);

        // Clear only the namespace
        manager.clear_namespace(&ns, &mut marker_list);
        assert_eq!(manager.len(), 1); // Only overlay3 remains
    }

    #[test]
    fn test_overlay_priority_sorting() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);
        let mut manager = OverlayManager::new();

        manager.add(Overlay::with_priority(
            &mut marker_list,
            5..10,
            OverlayFace::Background { color: Color::Red },
            10,
        ));
        manager.add(Overlay::with_priority(
            &mut marker_list,
            5..10,
            OverlayFace::Background { color: Color::Blue },
            5,
        ));
        manager.add(Overlay::with_priority(
            &mut marker_list,
            5..10,
            OverlayFace::Background {
                color: Color::Green,
            },
            15,
        ));

        let overlays = manager.at_position(7, &marker_list);
        assert_eq!(overlays.len(), 3);
        // Should be sorted by priority (low to high)
        assert_eq!(overlays[0].priority, 5);
        assert_eq!(overlays[1].priority, 10);
        assert_eq!(overlays[2].priority, 15);
    }

    #[test]
    fn test_overlay_contains_and_overlaps() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);

        let overlay = Overlay::new(
            &mut marker_list,
            10..20,
            OverlayFace::Background { color: Color::Red },
        );

        assert!(!overlay.contains(9, &marker_list));
        assert!(overlay.contains(10, &marker_list));
        assert!(overlay.contains(15, &marker_list));
        assert!(overlay.contains(19, &marker_list));
        assert!(!overlay.contains(20, &marker_list));

        assert!(!overlay.overlaps(&(0..10), &marker_list));
        assert!(overlay.overlaps(&(5..15), &marker_list));
        assert!(overlay.overlaps(&(15..25), &marker_list));
        assert!(!overlay.overlaps(&(20..30), &marker_list));
    }

    #[test]
    fn test_overlay_remove_in_range_keeps_only_disjoint() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(200);
        let mut manager = OverlayManager::new();

        manager.add(Overlay::new(
            &mut marker_list,
            0..5,
            OverlayFace::Background { color: Color::Red },
        ));
        manager.add(Overlay::new(
            &mut marker_list,
            10..20,
            OverlayFace::Background { color: Color::Blue },
        ));
        manager.add(Overlay::new(
            &mut marker_list,
            30..40,
            OverlayFace::Background {
                color: Color::Green,
            },
        ));
        manager.add(Overlay::new(
            &mut marker_list,
            50..60,
            OverlayFace::Background {
                color: Color::Yellow,
            },
        ));

        // Range 15..35 overlaps overlays #2 (10..20) and #3 (30..40), leaves #1 and #4.
        manager.remove_in_range(&(15..35), &mut marker_list);

        let kept: Vec<_> = manager
            .all()
            .iter()
            .map(|o| o.range(&marker_list))
            .collect();
        assert_eq!(kept, vec![0..5, 50..60]);
    }

    #[test]
    fn test_overlay_remove_in_range_deletes_markers() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);
        let mut manager = OverlayManager::new();

        let overlay = Overlay::new(
            &mut marker_list,
            10..20,
            OverlayFace::Background { color: Color::Red },
        );
        let start_id = overlay.start_marker;
        let end_id = overlay.end_marker;
        manager.add(overlay);

        manager.remove_in_range(&(0..50), &mut marker_list);

        assert_eq!(manager.len(), 0);
        assert_eq!(marker_list.get_position(start_id), None);
        assert_eq!(marker_list.get_position(end_id), None);
    }

    #[test]
    fn test_overlay_remove_in_range_endpoint_semantics() {
        // Touching at a single endpoint must NOT remove (start == range.end or end == range.start).
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);
        let mut manager = OverlayManager::new();

        manager.add(Overlay::new(
            &mut marker_list,
            10..20,
            OverlayFace::Background { color: Color::Red },
        ));

        manager.remove_in_range(&(20..30), &mut marker_list);
        assert_eq!(manager.len(), 1);
        manager.remove_in_range(&(0..10), &mut marker_list);
        assert_eq!(manager.len(), 1);
        manager.remove_in_range(&(19..21), &mut marker_list);
        assert_eq!(manager.len(), 0);
    }

    /// Mirrors the production cycle: per line in `lines_changed`, clear
    /// overlays in the line's byte range, then re-add the line's overlays.
    /// Steady-state count holds throughout. Same shape as the matching
    /// conceal perf test for direct comparison.
    ///
    /// Run with:
    ///   cargo nextest run -p fresh-editor --no-capture \
    ///     view::overlay::tests::perf_full_buffer_rebuild_pass
    #[test]
    fn perf_full_buffer_rebuild_pass() {
        const LINES: usize = 500;
        const LINE_BYTES: usize = 50;
        const OVERLAYS_PER_LINE: usize = 5;

        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(LINES * LINE_BYTES);
        let mut manager = OverlayManager::new();

        let overlay_byte = |line: usize, k: usize| -> usize {
            line * LINE_BYTES + k * (LINE_BYTES / OVERLAYS_PER_LINE)
        };
        let make_overlay = |ml: &mut MarkerList, line: usize, k: usize| {
            let s = overlay_byte(line, k);
            Overlay::new(
                ml,
                s..(s + 2),
                OverlayFace::Background { color: Color::Red },
            )
        };

        // Populate steady state.
        for line in 0..LINES {
            for k in 0..OVERLAYS_PER_LINE {
                let o = make_overlay(&mut marker_list, line, k);
                manager.add(o);
            }
        }
        let initial = LINES * OVERLAYS_PER_LINE;

        // One full-buffer `lines_changed` pass: per line, clear + re-add.
        let start = std::time::Instant::now();
        for line in 0..LINES {
            let line_range = (line * LINE_BYTES)..((line + 1) * LINE_BYTES);
            manager.remove_in_range(&line_range, &mut marker_list);
            for k in 0..OVERLAYS_PER_LINE {
                let o = make_overlay(&mut marker_list, line, k);
                manager.add(o);
            }
        }
        let elapsed = start.elapsed();

        eprintln!(
            "[perf] overlay full-buffer rebuild ({LINES} lines, {} entries steady): \
             {:?} total, {:?}/line",
            initial,
            elapsed,
            elapsed / LINES as u32,
        );
        assert_eq!(manager.len(), initial);
    }

    mod proptests {
        use super::*;
        use proptest::prelude::*;

        #[derive(Debug, Clone)]
        enum Op {
            Add {
                start: usize,
                len: usize,
                priority: i32,
                ns_idx: u8,
            },
            RemoveInRange {
                start: usize,
                end: usize,
            },
            ClearNamespace {
                ns_idx: u8,
            },
            ReplaceRange {
                start: usize,
                end: usize,
                ns_idx: u8,
                /// New overlays to insert in the same range; same shape
                /// as `Add` but len capped to satisfy precondition.
                new_overlays: Vec<(usize, usize, i32)>,
            },
        }

        const BUFFER_SIZE: usize = 200;
        const MAX_OVERLAY_LEN: usize = 4;
        const MIN_QUERY_LEN: usize = MAX_OVERLAY_LEN + 1;

        fn arb_overlay_spec() -> impl Strategy<Value = (usize, usize, i32)> {
            (
                0..(BUFFER_SIZE - MAX_OVERLAY_LEN),
                1..=MAX_OVERLAY_LEN,
                -5i32..=5i32,
            )
        }

        fn arb_op() -> impl Strategy<Value = Op> {
            prop_oneof![
                3 => arb_overlay_spec().prop_flat_map(|(start, len, priority)| {
                    (Just(start), Just(len), Just(priority), 0u8..3u8)
                }).prop_map(|(start, len, priority, ns_idx)| Op::Add {
                    start, len, priority, ns_idx,
                }),
                2 => (0..BUFFER_SIZE, MIN_QUERY_LEN..=BUFFER_SIZE)
                    .prop_map(|(start, qlen)| {
                        let s = start.min(BUFFER_SIZE - 1);
                        let e = (s + qlen).min(BUFFER_SIZE);
                        Op::RemoveInRange { start: s, end: e }
                    }),
                1 => (0u8..3u8).prop_map(|ns_idx| Op::ClearNamespace { ns_idx }),
                1 => (
                    0..BUFFER_SIZE,
                    MIN_QUERY_LEN..=BUFFER_SIZE,
                    0u8..3u8,
                    prop::collection::vec(arb_overlay_spec(), 0..4),
                )
                    .prop_map(|(start, qlen, ns_idx, new_overlays)| {
                        let s = start.min(BUFFER_SIZE - 1);
                        let e = (s + qlen).min(BUFFER_SIZE);
                        Op::ReplaceRange { start: s, end: e, ns_idx, new_overlays }
                    }),
            ]
        }

        fn nsf(idx: u8) -> OverlayNamespace {
            OverlayNamespace::from_string(format!("ns{idx}"))
        }

        proptest! {
            /// Invariants must hold after every sequence of operations.
            /// Plus: after `remove_in_range(r)`, no surviving overlay's
            /// range overlaps `r`. Plus: after `add` / `extend` /
            /// `clear_namespace` / `replace_range_in_namespace`, the
            /// vector is sorted by priority. Note: priority order may be
            /// transiently broken right after `remove_in_range` until the
            /// next `add` — production callers always pair these.
            #[test]
            fn prop_marker_index_consistent(ops in prop::collection::vec(arb_op(), 0..30)) {
                let mut marker_list = MarkerList::new();
                marker_list.set_buffer_size(BUFFER_SIZE);
                let mut manager = OverlayManager::new();

                for op in ops {
                    match op {
                        Op::Add { start, len, priority, ns_idx } => {
                            let o = Overlay::with_namespace(
                                &mut marker_list,
                                start..(start + len),
                                OverlayFace::Background { color: Color::Red },
                                nsf(ns_idx),
                            );
                            let mut o = o;
                            o.priority = priority;
                            manager.add(o);
                            manager.check_invariants();
                            manager.assert_priority_sorted();
                        }
                        Op::RemoveInRange { start, end } => {
                            manager.remove_in_range(&(start..end), &mut marker_list);
                            for (o, rng) in manager.query_viewport(start, end, &marker_list) {
                                let overlaps = rng.start < end && start < rng.end;
                                prop_assert!(
                                    !overlaps,
                                    "overlay {:?} (handle {:?}) survived remove_in_range({start}..{end})",
                                    rng, o.handle,
                                );
                            }
                            manager.check_invariants();
                        }
                        Op::ClearNamespace { ns_idx } => {
                            manager.clear_namespace(&nsf(ns_idx), &mut marker_list);
                            manager.check_invariants();
                            manager.assert_priority_sorted();
                        }
                        Op::ReplaceRange { start, end, ns_idx, new_overlays } => {
                            let new: Vec<Overlay> = new_overlays.into_iter().map(|(s, l, p)| {
                                let mut o = Overlay::with_namespace(
                                    &mut marker_list,
                                    s..(s + l),
                                    OverlayFace::Background { color: Color::Blue },
                                    nsf(ns_idx),
                                );
                                o.priority = p;
                                o
                            }).collect();
                            manager.replace_range_in_namespace(
                                &nsf(ns_idx),
                                &(start..end),
                                new,
                                &mut marker_list,
                            );
                            manager.check_invariants();
                            manager.assert_priority_sorted();
                        }
                    }
                }
            }
        }
    }
}
