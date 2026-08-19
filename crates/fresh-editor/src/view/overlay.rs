use crate::model::marker::{MarkerId, MarkerList};
use ratatui::style::{Color, Style};
use std::collections::{HashMap, HashSet};
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
        /// When `true`, apply `fg` only on cells whose existing fg
        /// equals the resolved bg (a same-colour collision).
        fg_on_collision_only: bool,
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
        if options.reversed {
            // The widget block caret rides this: without it, a panel's
            // markdown document showed no caret at all in buffer-mounted
            // panels (the reversed cell was silently dropped here).
            modifiers |= Modifier::REVERSED;
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
                fg_on_collision_only: options.fg_on_collision_only,
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

    /// Start marker. Right gravity, like every `MarkerList::create` marker.
    pub start_marker: MarkerId,

    /// End marker, also right gravity: text typed at the end extends it.
    pub end_marker: MarkerId,

    /// Spatial index only: one marker covering the overlay's whole extent,
    /// so the marker tree's interval-overlap query can return this overlay
    /// for a viewport it *spans* — the case two endpoint markers cannot
    /// answer, because both sit outside such a window.
    ///
    /// Never read for positions; `start_marker`/`end_marker` stay
    /// authoritative. It is created with right gravity while a fixed-end
    /// overlay's real end has left gravity, so the span can sit a byte wide
    /// of the true range at an edit boundary. That is deliberate and safe in
    /// one direction only: the span is a superset, so it can over-offer a
    /// candidate (harmless — `query_viewport` re-checks every candidate
    /// against the real markers) but never hide one.
    pub span_marker: MarkerId,

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
        let start_marker = marker_list.create(range.start); // left affinity
        let end_marker = marker_list.create(range.end); // right affinity
        let span_marker = marker_list.create_span(range.start, range.end);

        Self {
            handle: OverlayHandle::new(),
            namespace: None,
            start_marker,
            end_marker,
            span_marker,
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

    /// Like [`with_namespace`], but the end marker uses left gravity so the
    /// overlay does not grow when text is inserted immediately after it.
    ///
    /// Used for search-match highlights, which must stay anchored to the matched
    /// text and not absorb adjacent typing (issue #2053).
    ///
    /// [`with_namespace`]: Overlay::with_namespace
    pub fn with_namespace_fixed_end(
        marker_list: &mut MarkerList,
        range: Range<usize>,
        face: OverlayFace,
        namespace: OverlayNamespace,
    ) -> Self {
        let start_marker = marker_list.create(range.start); // left affinity
        let end_marker = marker_list.create_left_gravity(range.end);
        let span_marker = marker_list.create_span(range.start, range.end);

        Self {
            handle: OverlayHandle::new(),
            namespace: Some(namespace),
            start_marker,
            end_marker,
            span_marker,
            face,
            priority: 0,
            message: None,
            extend_to_line_end: false,
            url: None,
            theme_key: None,
        }
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
    /// Bumped by every removal that is not confined to a single namespace.
    /// A producer that caches "the overlays I derived last time are still on
    /// the buffer" stores this with its own inputs, so a clear from anywhere
    /// else invalidates that belief — see `BracketHighlightOverlay`.
    removal_epoch: u64,
    /// Per-namespace removal counters, for the same purpose at finer grain:
    /// one producer clearing its own namespace every frame (bracket matching
    /// does, on each cursor move) must not invalidate another's cache.
    namespace_removal_epochs: HashMap<OverlayNamespace, u64>,
    /// `namespace -> indices into overlays`. Clearing or replacing a
    /// namespace reaches its own overlays directly instead of walking the
    /// buffer's whole set to find them.
    namespace_to_idx: HashMap<OverlayNamespace, HashSet<usize>>,
    /// `handle -> index into overlays`, so removing the one overlay a caller
    /// holds a handle for is a lookup rather than a search.
    handle_to_idx: HashMap<OverlayHandle, usize>,
}

impl OverlayManager {
    /// Create a new empty overlay manager
    pub fn new() -> Self {
        Self {
            overlays: Vec::new(),
            marker_to_idx: HashMap::new(),
            removal_epoch: 0,
            namespace_removal_epochs: HashMap::new(),
            namespace_to_idx: HashMap::new(),
            handle_to_idx: HashMap::new(),
        }
    }

    /// Add an overlay and return its handle for later removal. O(1): the
    /// set is unordered, and every consumer that needs overlays in priority
    /// order sorts the handful it actually draws (see `query_viewport`'s
    /// callers). Keeping the whole vector priority-sorted instead cost an
    /// insertion shift plus a re-index of everything after it, on every
    /// add — which is once or twice per cursor move, against a buffer that
    /// can hold an overlay per line.
    pub fn add(&mut self, overlay: Overlay) -> OverlayHandle {
        let handle = overlay.handle.clone();
        let idx = self.overlays.len();
        self.overlays.push(overlay);
        self.index_entry(idx);
        handle
    }

    /// Append many overlays at once.
    pub fn extend<I: IntoIterator<Item = Overlay>>(&mut self, overlays: I) {
        let first_new = self.overlays.len();
        self.overlays.extend(overlays);
        for idx in first_new..self.overlays.len() {
            self.index_entry(idx);
        }
    }

    /// Record the entry at `idx` in every index. The entry must already be
    /// in `overlays` at that position.
    fn index_entry(&mut self, idx: usize) {
        let overlay = &self.overlays[idx];
        self.marker_to_idx.insert(overlay.start_marker, idx);
        self.marker_to_idx.insert(overlay.end_marker, idx);
        self.marker_to_idx.insert(overlay.span_marker, idx);
        self.handle_to_idx.insert(overlay.handle.clone(), idx);
        // Own the namespace key only when it is new. This runs once per
        // overlay on the bulk path (`extend` over a whole diff), where
        // `entry(ns.clone())` would allocate a `String` per overlay to
        // insert a key that is already there.
        if let Some(ns) = &overlay.namespace {
            if let Some(slots) = self.namespace_to_idx.get_mut(ns) {
                slots.insert(idx);
            } else {
                self.namespace_to_idx
                    .entry(ns.clone())
                    .or_default()
                    .insert(idx);
            }
        }
    }

    /// Drop the entry at `idx` from every index. Does not touch `overlays`.
    fn unindex_entry(&mut self, idx: usize, overlay: &Overlay) {
        self.marker_to_idx.remove(&overlay.start_marker);
        self.marker_to_idx.remove(&overlay.end_marker);
        self.marker_to_idx.remove(&overlay.span_marker);
        self.handle_to_idx.remove(&overlay.handle);
        if let Some(ns) = &overlay.namespace {
            if let Some(slots) = self.namespace_to_idx.get_mut(ns) {
                slots.remove(&idx);
                if slots.is_empty() {
                    self.namespace_to_idx.remove(ns);
                }
            }
        }
    }

    /// Move the index entries of the overlay now sitting at `idx` (it was at
    /// `from`, before a swap-remove pulled it down).
    fn reindex_moved(&mut self, idx: usize, from: usize) {
        let overlay = &self.overlays[idx];
        self.marker_to_idx.insert(overlay.start_marker, idx);
        self.marker_to_idx.insert(overlay.end_marker, idx);
        self.marker_to_idx.insert(overlay.span_marker, idx);
        // Both keys are already in their maps, so re-point them in place:
        // `self.overlays` and the index maps are disjoint fields, so the
        // borrows coexist and neither key needs cloning.
        if let Some(slot) = self.handle_to_idx.get_mut(&overlay.handle) {
            *slot = idx;
        }
        if let Some(ns) = &overlay.namespace {
            if let Some(slots) = self.namespace_to_idx.get_mut(ns) {
                slots.remove(&from);
                slots.insert(idx);
            }
        }
    }

    /// Remove an overlay by its handle. O(1): the handle index says where
    /// it is instead of the vector being searched for it.
    pub fn remove_by_handle(
        &mut self,
        handle: &OverlayHandle,
        marker_list: &mut MarkerList,
    ) -> bool {
        let Some(idx) = self.handle_to_idx.get(handle).copied() else {
            return false;
        };
        match self.overlays[idx].namespace.clone() {
            Some(ns) => self.bump_namespace_epoch(&ns),
            None => self.removal_epoch = self.removal_epoch.wrapping_add(1),
        }
        self.swap_remove_at(idx, marker_list);
        true
    }

    /// Remove all overlays in a namespace. O(k) in that namespace's own
    /// overlays, whatever else the buffer carries.
    ///
    /// This is a hot path, not a housekeeping one: a plugin that repaints a
    /// one-row highlight — the review diff's cursor line — clears and
    /// re-adds its namespace on every cursor move. While this walked the
    /// whole overlay set to find its two entries, an unrelated 20 000-line
    /// diff in the same buffer made every arrow key pay for all of it.
    pub fn clear_namespace(&mut self, namespace: &OverlayNamespace, marker_list: &mut MarkerList) {
        self.bump_namespace_epoch(namespace);
        let Some(slots) = self.namespace_to_idx.remove(namespace) else {
            return;
        };
        // Descending, so each swap-remove only ever pulls down an entry from
        // beyond the ones still to be removed.
        let mut slots: Vec<usize> = slots.into_iter().collect();
        slots.sort_unstable_by(|a, b| b.cmp(a));
        for idx in slots {
            self.swap_remove_at(idx, marker_list);
        }
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
        self.bump_namespace_epoch(namespace);
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
                    if start < end {
                        // Healthy overlay: remove on genuine half-open overlap.
                        start < range.end && range.start < end
                    } else {
                        // Collapsed (start == end) or inverted (start > end)
                        // overlay. These arise when an edit erases the overlay's
                        // anchored text — the markers clamp to the edit point and
                        // a later insert can even push them past each other
                        // (issue #2414). A strict overlap test never matches a
                        // zero-length span, so the dead overlay would linger and
                        // surface as a phantom search match. Treat it as a point
                        // and remove it whenever it lands inside the replaced
                        // range, so it is dropped rather than recreated.
                        let lo = start.min(end);
                        let hi = start.max(end);
                        lo <= range.end && range.start <= hi
                    }
                })
                .collect();
            to_remove.sort_unstable_by(|a, b| b.cmp(a));
            for idx in to_remove {
                self.swap_remove_at(idx, marker_list);
            }
        }

        if !new_overlays.is_empty() {
            let first_new = self.overlays.len();
            self.overlays.append(&mut new_overlays);
            for idx in first_new..self.overlays.len() {
                self.index_entry(idx);
            }
        }
    }

    /// Remove all overlays in a range and clean up their markers
    pub fn remove_in_range(&mut self, range: &Range<usize>, marker_list: &mut MarkerList) {
        self.removal_epoch = self.removal_epoch.wrapping_add(1);
        // O(log N + k) throughout: the marker tree narrows the candidates
        // and each removal patches only its own index entries.
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
        // Descending, so a swap-remove never moves an entry still queued
        // for removal.
        to_remove.sort_unstable_by(|a, b| b.cmp(a));
        for idx in to_remove {
            self.swap_remove_at(idx, marker_list);
        }
    }

    /// Like [`remove_in_range`], but only removes overlays belonging to
    /// `namespace`. Overlays in other namespaces (e.g. editor-owned LSP
    /// diagnostics) that happen to overlap the range are left intact.
    ///
    /// [`remove_in_range`]: Self::remove_in_range
    pub fn remove_in_range_for_namespace(
        &mut self,
        range: &Range<usize>,
        namespace: &OverlayNamespace,
        marker_list: &mut MarkerList,
    ) {
        self.bump_namespace_epoch(namespace);
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
                if o.namespace.as_ref() != Some(namespace) {
                    return false;
                }
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
    }

    /// Clear all overlays and their markers
    pub fn clear(&mut self, marker_list: &mut MarkerList) {
        self.removal_epoch = self.removal_epoch.wrapping_add(1);
        for overlay in &self.overlays {
            marker_list.delete(overlay.start_marker);
            marker_list.delete(overlay.end_marker);
            // The span marker too. Leaving it behind orphans one interval
            // per overlay in the tree forever, and every query here now
            // goes through that tree — `set_virtual_buffer_content` clears
            // and refills on each content set, so the orphans would grow
            // the `k` in O(log N + k) without bound across reloads.
            marker_list.delete(overlay.span_marker);
        }
        self.overlays.clear();
        self.marker_to_idx.clear();
        self.handle_to_idx.clear();
        self.namespace_to_idx.clear();
    }

    /// Swap-remove the entry at `idx`, deleting its markers and re-pointing
    /// every index at whatever entry got swapped in. The set is unordered,
    /// so there is nothing left for the caller to restore.
    fn swap_remove_at(&mut self, idx: usize, marker_list: &mut MarkerList) {
        let removed = self.overlays.swap_remove(idx);
        self.unindex_entry(idx, &removed);
        marker_list.delete(removed.start_marker);
        marker_list.delete(removed.end_marker);
        marker_list.delete(removed.span_marker);
        // `swap_remove` pulled the last entry down into `idx` — unless the
        // removed entry *was* last, in which case nothing moved.
        let moved_from = self.overlays.len();
        if idx < moved_from {
            self.reindex_moved(idx, moved_from);
        }
    }

    /// Get all overlays at a specific position, sorted by priority.
    pub fn at_position(&self, position: usize, marker_list: &MarkerList) -> Vec<&Overlay> {
        let mut hits: Vec<&Overlay> = self
            .candidates_in(position..position + 1, marker_list)
            .filter(|o| o.range(marker_list).contains(&position))
            .collect();
        hits.sort_by_key(|o| o.priority);
        hits
    }

    /// Get all overlays that overlap with a range, sorted by priority.
    pub fn in_range(&self, range: &Range<usize>, marker_list: &MarkerList) -> Vec<&Overlay> {
        let mut hits: Vec<&Overlay> = self
            .candidates_in(range.clone(), marker_list)
            .filter(|o| o.overlaps(range, marker_list))
            .collect();
        hits.sort_by_key(|o| o.priority);
        hits
    }

    /// Overlays whose span marker touches `range`, as a superset to filter.
    /// The marker tree answers this in O(log N + k) — the vector is never
    /// walked, which is what keeps these off the buffer's total overlay
    /// count.
    fn candidates_in<'a>(
        &'a self,
        range: Range<usize>,
        marker_list: &MarkerList,
    ) -> impl Iterator<Item = &'a Overlay> + 'a {
        let mut idxs: Vec<usize> = marker_list
            .query_range(range.start, range.end)
            .iter()
            .filter_map(|(mid, _, _)| self.marker_to_idx.get(mid).copied())
            .collect();
        idxs.sort_unstable();
        idxs.dedup();
        idxs.into_iter()
            .filter_map(move |idx| self.overlays.get(idx))
    }

    /// The overlays in one namespace. O(k), for callers that would otherwise
    /// filter `all()` — a per-frame walk of everything to count a handful.
    pub fn in_namespace<'a>(
        &'a self,
        namespace: &OverlayNamespace,
    ) -> impl Iterator<Item = &'a Overlay> + 'a {
        self.namespace_to_idx
            .get(namespace)
            .into_iter()
            .flat_map(move |slots| slots.iter().filter_map(move |&idx| self.overlays.get(idx)))
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
        // Ask the marker tree which overlays touch the window at all. Every
        // overlay indexes its whole extent as one span marker, so this is an
        // ordinary interval-overlap query and it answers for all three
        // shapes: starting inside, ending inside, and spanning the window
        // with both endpoints outside. That last one is why this cannot be
        // driven from the endpoint markers alone, and why this used to walk
        // every overlay in the buffer instead — on a large diff that was
        // ~22 000 overlays scanned to find the ~270 on screen, once per
        // frame, and it is what made arrow keys, filter typing and divider
        // drags all queue behind the redraw.
        let hits = marker_list.query_range(start, end);

        // A candidate can arrive up to three times (start, end and span
        // markers all land in the window for a short overlay), so fold to
        // distinct overlay indices before resolving anything.
        let mut candidates: Vec<usize> = hits
            .iter()
            .filter_map(|(id, _, _)| self.marker_to_idx.get(id).copied())
            .collect();
        candidates.sort_unstable();
        candidates.dedup();

        // The span marker is only an index — deliberately a superset, so it
        // can offer a candidate that does not really overlap. The real
        // markers decide, exactly as before.
        candidates
            .into_iter()
            .filter_map(|idx| {
                let overlay = self.overlays.get(idx)?;
                let start_pos = marker_list.get_position(overlay.start_marker)?;
                let end_pos = marker_list.get_position(overlay.end_marker)?;
                let range = start_pos..end_pos;

                // Zero-width ranges (a diagnostic at a single position) are
                // inclusive at both ends; everything else is a standard
                // half-open overlap test.
                let included = if range.start == range.end {
                    range.start >= start && range.start <= end
                } else {
                    range.start < end && range.end > start
                };

                included.then_some((overlay, range))
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
    /// Counters a cache compares against to know whether the overlays it
    /// produced are still on the buffer: the global one moves on any
    /// removal that is not namespace-scoped, the second only on removals
    /// within `namespace`. See `removal_epoch`.
    pub fn removal_epochs_for(&self, namespace: &OverlayNamespace) -> (u64, u64) {
        (
            self.removal_epoch,
            self.namespace_removal_epochs
                .get(namespace)
                .copied()
                .unwrap_or(0),
        )
    }

    fn bump_namespace_epoch(&mut self, namespace: &OverlayNamespace) {
        let counter = self
            .namespace_removal_epochs
            .entry(namespace.clone())
            .or_insert(0);
        *counter = counter.wrapping_add(1);
    }

    /// Every overlay on the buffer, in no particular order — insertion
    /// shuffles under `swap_remove`, and nothing needs a global order (the
    /// renderer sorts what it draws). Prefer [`in_namespace`] or
    /// [`query_viewport`] where they fit: they answer from an index instead
    /// of handing out the whole set to be filtered.
    ///
    /// [`in_namespace`]: Self::in_namespace
    /// [`query_viewport`]: Self::query_viewport
    pub fn all(&self) -> &[Overlay] {
        &self.overlays
    }

    /// Test-only: assert every index agrees with `overlays`. Panics on any
    /// divergence. Used by property tests — with the vector unordered and
    /// removals swapping entries around, index consistency is the whole
    /// safety property, so it is checked after each operation.
    #[cfg(test)]
    fn check_invariants(&self) {
        // Three markers per overlay: the two authoritative endpoints plus
        // the span that indexes its extent for `query_viewport`. All three
        // must map to the overlay's slot, or a viewport query resolves the
        // wrong overlay — or silently drops one.
        assert_eq!(
            self.marker_to_idx.len(),
            self.overlays.len() * 3,
            "marker_to_idx size != 3 * overlays.len()"
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
            assert_eq!(
                self.marker_to_idx.get(&o.span_marker).copied(),
                Some(i),
                "span_marker {:?} of overlay {} mismapped",
                o.span_marker,
                i,
            );
        }
        // One handle per overlay, each pointing at its own slot: this is
        // what `remove_by_handle` trusts instead of searching.
        assert_eq!(
            self.handle_to_idx.len(),
            self.overlays.len(),
            "handle_to_idx size != overlays.len()"
        );
        for (i, o) in self.overlays.iter().enumerate() {
            assert_eq!(
                self.handle_to_idx.get(&o.handle).copied(),
                Some(i),
                "handle {:?} of overlay {} mismapped",
                o.handle,
                i,
            );
        }
        // The namespace index must name exactly the namespaced overlays and
        // no stale slot: `clear_namespace` removes what it lists here
        // without looking at anything else.
        let mut expected: HashMap<&OverlayNamespace, HashSet<usize>> = HashMap::new();
        for (i, o) in self.overlays.iter().enumerate() {
            if let Some(ns) = &o.namespace {
                expected.entry(ns).or_default().insert(i);
            }
        }
        assert_eq!(
            self.namespace_to_idx.len(),
            expected.len(),
            "namespace_to_idx tracks {} namespaces, expected {}",
            self.namespace_to_idx.len(),
            expected.len(),
        );
        for (ns, slots) in &expected {
            assert_eq!(
                self.namespace_to_idx.get(*ns),
                Some(slots),
                "namespace {ns:?} indexed as {:?}, expected {slots:?}",
                self.namespace_to_idx.get(*ns),
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

    /// Every removal path must delete all three of an overlay's markers.
    /// `clear()` used to delete only the two endpoints, orphaning one
    /// interval per overlay in the tree — invisible while queries walked
    /// the overlay vector, but every query is driven by the marker tree
    /// now, and `set_virtual_buffer_content` clears and refills on every
    /// content set.
    #[test]
    fn clear_deletes_every_marker_it_created() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(1000);
        let mut mgr = OverlayManager::new();
        for i in 0..10 {
            mgr.add(Overlay::new(
                &mut marker_list,
                (i * 10)..(i * 10 + 5),
                OverlayFace::Background { color: Color::Red },
            ));
        }
        assert_eq!(
            marker_list.query_range(0, 1000).len(),
            30,
            "three markers per overlay: start, end, span"
        );

        mgr.clear(&mut marker_list);

        assert_eq!(mgr.len(), 0);
        assert_eq!(
            marker_list.query_range(0, 1000).len(),
            0,
            "clear() must leave no marker behind"
        );
    }

    /// Clearing a namespace must not disturb overlays outside it, and must
    /// leave every index consistent for the ones that remain.
    ///
    /// This used to walk the whole overlay set to find the namespace's
    /// members, then sort and re-index everything afterwards to repair the
    /// priority order `swap_remove` scrambled — so a one-overlay clear cost
    /// time proportional to the buffer, on every cursor move, for any plugin
    /// that repaints a highlight. The clear is now driven from the namespace
    /// index and the set is unordered, so what has to hold is that the
    /// survivors and the indexes still agree.
    #[test]
    fn test_clear_namespace_leaves_indexes_consistent() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(10_000);
        let mut mgr = OverlayManager::new();
        let bulk = OverlayNamespace("bulk".to_string());
        let scratch = OverlayNamespace("scratch".to_string());

        // Interleave two namespaces across a range of priorities so a
        // scrambling removal would be visible in the survivors' order.
        for i in 0..50usize {
            mgr.add(
                Overlay::with_namespace(
                    &mut marker_list,
                    (i * 10)..(i * 10 + 4),
                    OverlayFace::Background { color: Color::Red },
                    bulk.clone(),
                )
                .with_priority_value(i as Priority),
            );
            mgr.add(
                Overlay::with_namespace(
                    &mut marker_list,
                    (i * 10 + 5)..(i * 10 + 8),
                    OverlayFace::Background { color: Color::Blue },
                    scratch.clone(),
                )
                .with_priority_value(i as Priority),
            );
        }

        mgr.clear_namespace(&scratch, &mut marker_list);

        assert_eq!(mgr.overlays.len(), 50, "only the bulk namespace survives");
        assert!(
            mgr.overlays
                .iter()
                .all(|o| o.namespace.as_ref() == Some(&bulk)),
            "no scratch overlay may survive"
        );
        mgr.check_invariants();
        assert!(
            mgr.namespace_to_idx.get(&scratch).is_none(),
            "the cleared namespace leaves no index entry behind"
        );
        // Every surviving overlay must be findable at its recorded index.
        for (i, o) in mgr.overlays.iter().enumerate() {
            assert_eq!(
                mgr.marker_to_idx.get(&o.start_marker),
                Some(&i),
                "start marker index stale for overlay {i}"
            );
            assert_eq!(
                mgr.marker_to_idx.get(&o.end_marker),
                Some(&i),
                "end marker index stale for overlay {i}"
            );
        }
        // The index must not still reference the removed overlays — three
        // markers each (start, end, span), for the 50 that survive.
        assert_eq!(
            mgr.marker_to_idx.len(),
            150,
            "index holds exactly three markers per surviving overlay"
        );
        for (i, o) in mgr.overlays.iter().enumerate() {
            assert_eq!(
                mgr.marker_to_idx.get(&o.span_marker),
                Some(&i),
                "span marker index stale for overlay {i}"
            );
        }
        // Clearing an absent namespace is a no-op.
        mgr.clear_namespace(&scratch, &mut marker_list);
        assert_eq!(mgr.overlays.len(), 50);
    }

    /// `query_viewport` is driven by the marker tree, so it must return every
    /// overlay touching the window regardless of how the window sits inside
    /// it — and must not depend on how much else the buffer carries.
    ///
    /// The spanning shape is the one that forced the old full scan: both
    /// endpoints lie outside the window, so no query over endpoint markers
    /// can find it. The span marker is what makes it answerable.
    #[test]
    fn test_query_viewport_finds_every_overlap_shape() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100_000);
        let mut mgr = OverlayManager::new();
        let face = || OverlayFace::Background { color: Color::Red };

        let starts_inside = mgr.add(Overlay::new(&mut marker_list, 4_950..5_010, face()));
        let ends_inside = mgr.add(Overlay::new(&mut marker_list, 4_990..5_050, face()));
        let wholly_inside = mgr.add(Overlay::new(&mut marker_list, 5_001..5_002, face()));
        let spans = mgr.add(Overlay::new(&mut marker_list, 1_000..9_000, face()));
        let before = mgr.add(Overlay::new(&mut marker_list, 10..20, face()));
        let after = mgr.add(Overlay::new(&mut marker_list, 90_000..90_010, face()));

        // Bury them in unrelated overlays: the answer must not change.
        for i in 0..5_000 {
            let at = 20_000 + i * 10;
            mgr.add(Overlay::new(&mut marker_list, at..at + 4, face()));
        }

        let found: Vec<_> = mgr
            .query_viewport(5_000, 5_005, &marker_list)
            .into_iter()
            .map(|(o, _)| o.handle.clone())
            .collect();

        for (label, handle) in [
            ("starts inside", &starts_inside),
            ("ends inside", &ends_inside),
            ("wholly inside", &wholly_inside),
            ("spans the window", &spans),
        ] {
            assert!(
                found.contains(handle),
                "{label} overlay missing from viewport query"
            );
        }
        for (label, handle) in [("before", &before), ("after", &after)] {
            assert!(
                !found.contains(handle),
                "{label} overlay must not be returned"
            );
        }
        assert_eq!(found.len(), 4, "no unrelated overlay should be returned");
    }

    /// An overlay taller than the viewport still renders.
    ///
    /// `query_viewport` used to require at least one of an overlay's markers
    /// to land inside the queried range. An overlay that spans the viewport
    /// has neither — its start is above the top and its end below the bottom —
    /// so it was dropped even though it covers every visible line. Anything
    /// taller than the window hit this: a code-tour step range, a long
    /// diagnostic, a large diff hunk.
    #[test]
    fn test_query_viewport_keeps_overlay_spanning_the_viewport() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(1000);
        let mut manager = OverlayManager::new();

        // Overlay 100..900; the viewport 400..500 sits entirely inside it.
        manager.add(Overlay::new(
            &mut marker_list,
            100..900,
            OverlayFace::Background { color: Color::Red },
        ));

        let found = manager.query_viewport(400, 500, &marker_list);
        assert_eq!(
            found.len(),
            1,
            "an overlay spanning the whole viewport must still be returned"
        );
        assert_eq!(found[0].1, 100..900, "with its full resolved range");
    }

    /// The neighbouring cases the span fix must not break — in particular the
    /// disjoint ones, which would leak through if the overlap test below the
    /// marker lookup were not doing the real work.
    #[test]
    fn test_query_viewport_boundaries() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(1000);
        let mut manager = OverlayManager::new();
        manager.add(Overlay::new(
            &mut marker_list,
            100..200,
            OverlayFace::Background { color: Color::Red },
        ));

        // Containing the overlay, and overlapping each edge — all visible.
        assert_eq!(manager.query_viewport(50, 250, &marker_list).len(), 1);
        assert_eq!(manager.query_viewport(150, 250, &marker_list).len(), 1);
        assert_eq!(manager.query_viewport(50, 150, &marker_list).len(), 1);
        // Disjoint on either side — not visible.
        assert!(manager.query_viewport(300, 400, &marker_list).is_empty());
        assert!(manager.query_viewport(10, 90, &marker_list).is_empty());
    }

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
                        }
                    }
                }
            }
        }
    }
}
