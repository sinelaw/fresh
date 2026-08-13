//! Soft break infrastructure
//!
//! Provides a marker-based system for injecting soft line breaks during rendering.
//! Used for compose-mode word wrapping: plugins register break points at byte positions,
//! and markers auto-adjust on buffer edits so breaks survive without async round-trips.
//!
//! ## Architecture
//!
//! Follows the same pattern as ConcealManager:
//! 1. Plugins add soft breaks via `addSoftBreak(bufferId, namespace, position, indent)`
//! 2. Break positions are stored with marker-based tracking (auto-adjust on edits)
//! 3. During the token pipeline, breaks are injected into the token stream
//!
//! ## Integration Point
//!
//! Soft breaks are applied to the token stream in `split_rendering.rs` BEFORE
//! conceal ranges and wrapping. This means:
//! - Concealment operates on the already-broken lines
//! - The wrapping transform sees pre-broken content

use crate::model::marker::{MarkerId, MarkerList};
use crate::view::activation::ScopedActivation;
use crate::view::theme::Theme;
use fresh_core::api::MarkerActivation;
use fresh_core::overlay::OverlayNamespace;
use ratatui::style::Style;
use std::collections::HashMap;

/// A glyph run drawn at the head of a continuation row, stored the way
/// `VirtualText` stores its styling: concrete colours in `style` as the
/// fallback, theme keys kept separate so they re-resolve on every frame
/// and follow live theme changes.
///
/// Its display width never exceeds the break's `indent` — the manager
/// widens `indent` to fit at insert time (see [`SoftBreakManager::add_full`]),
/// which is what lets every width consumer keep reading `indent` alone.
#[derive(Debug, Clone)]
pub struct SoftBreakPrefix {
    pub text: String,
    pub style: Style,
    pub fg_theme_key: Option<String>,
    pub bg_theme_key: Option<String>,
}

impl SoftBreakPrefix {
    /// Resolve the on-screen `Style` against a live theme. Theme keys win
    /// over the fallback's colours; modifiers from the fallback survive.
    /// Same contract as [`crate::view::virtual_text::VirtualText::resolved_style`].
    pub fn resolved_style(&self, theme: &Theme) -> Style {
        let mut style = self.style;
        if let Some(key) = &self.fg_theme_key {
            if let Some(color) = theme.resolve_theme_key(key) {
                style = style.fg(color);
            }
        }
        if let Some(key) = &self.bg_theme_key {
            if let Some(color) = theme.resolve_theme_key(key) {
                style = style.bg(color);
            }
        }
        style
    }
}

/// A soft break point that injects a line break during rendering
#[derive(Debug, Clone)]
pub struct SoftBreakPoint {
    /// Namespace for bulk operations (shared with overlay namespace system)
    pub namespace: OverlayNamespace,

    /// Marker at the break position (right affinity — shifts with inserted text)
    pub marker_id: MarkerId,

    /// Total width of the continuation row's indent, in columns. The single
    /// number every width consumer reads; `prefix` is drawn inside it.
    pub indent: u16,

    /// Optional cursor-dependent activation rule, scope stored relative to
    /// the break marker. `None` = always active. Filtered at query time so
    /// cursor movement never mutates the marker set (see view/activation.rs).
    pub activation: Option<ScopedActivation>,

    /// Optional glyph run drawn at the head of the continuation row.
    /// `None` = the whole indent renders blank.
    pub prefix: Option<SoftBreakPrefix>,
}

/// One soft break resolved for a render pass: where to break, how wide the
/// continuation indent is, and what (if anything) to draw in it.
///
/// The token-producing transform takes these rather than raw
/// `SoftBreakPoint`s so measurement-only callers — the wrap index, the row
/// count cache — can build the same stream from plain `(position, indent)`
/// pairs via [`SoftBreakRender::plain`].
#[derive(Debug, Clone, PartialEq)]
pub struct SoftBreakRender {
    pub position: usize,
    pub indent: u16,
    /// Continuation-row glyphs and their theme-resolved style. The style is
    /// `None` when the caller has no theme (measurement paths), which
    /// changes colour only — never width.
    pub prefix: Option<(String, Option<Style>)>,
}

impl SoftBreakRender {
    /// A break with a blank continuation indent.
    pub fn plain(position: usize, indent: u16) -> Self {
        Self {
            position,
            indent,
            prefix: None,
        }
    }
}

impl SoftBreakPoint {
    /// Get the current byte position by resolving the marker
    pub fn position(&self, marker_list: &MarkerList) -> usize {
        marker_list.get_position(self.marker_id).unwrap_or(0)
    }

    /// Check if this break point falls within a byte range
    pub fn in_range(&self, start: usize, end: usize, marker_list: &MarkerList) -> bool {
        let pos = self.position(marker_list);
        pos >= start && pos < end
    }
}

/// Manages soft break points for a buffer
#[derive(Debug, Clone)]
pub struct SoftBreakManager {
    breaks: Vec<SoftBreakPoint>,
    /// `MarkerId -> index into breaks` for O(log N + k) `remove_in_range`.
    /// Each break has exactly one marker. Kept in sync with every push /
    /// swap_remove on `breaks`.
    marker_to_idx: HashMap<MarkerId, usize>,
    /// Monotonic counter bumped on every mutation. Consumers that cache derived
    /// data (e.g. `LineWrapCache`) fold this into their key so any mutation
    /// invalidates stale entries automatically.
    version: u32,
}

impl SoftBreakManager {
    /// Create a new empty soft break manager
    pub fn new() -> Self {
        Self {
            breaks: Vec::new(),
            marker_to_idx: HashMap::new(),
            version: 0,
        }
    }

    /// Monotonic version, bumped on every mutation.
    pub fn version(&self) -> u32 {
        self.version
    }

    /// Add a soft break point
    pub fn add(
        &mut self,
        marker_list: &mut MarkerList,
        namespace: OverlayNamespace,
        position: usize,
        indent: u16,
    ) {
        self.add_with_activation(marker_list, namespace, position, indent, None);
    }

    /// Add a soft break point with an optional cursor-dependent activation
    /// rule (wire format, absolute scope bytes — converted to the
    /// marker-relative form here).
    pub fn add_with_activation(
        &mut self,
        marker_list: &mut MarkerList,
        namespace: OverlayNamespace,
        position: usize,
        indent: u16,
        activation: Option<MarkerActivation>,
    ) {
        self.add_full(marker_list, namespace, position, indent, activation, None);
    }

    /// As [`add_with_activation`](Self::add_with_activation), plus a glyph
    /// run for the continuation row.
    ///
    /// `indent` is widened to the prefix's display width when the caller
    /// asks for a prefix wider than the indent it declared. `indent` is the
    /// only width the scroll math and the wrap index ever see, so letting a
    /// prefix overflow it would put the renderer and those consumers on
    /// different column counts for the same row.
    pub fn add_full(
        &mut self,
        marker_list: &mut MarkerList,
        namespace: OverlayNamespace,
        position: usize,
        indent: u16,
        activation: Option<MarkerActivation>,
        prefix: Option<SoftBreakPrefix>,
    ) {
        let marker_id = marker_list.create(position);

        let indent = match &prefix {
            Some(p) => indent.max(
                u16::try_from(fresh_core::display_width::str_width(&p.text)).unwrap_or(u16::MAX),
            ),
            None => indent,
        };

        let idx = self.breaks.len();
        self.marker_to_idx.insert(marker_id, idx);
        self.breaks.push(SoftBreakPoint {
            namespace,
            marker_id,
            indent,
            activation: activation.map(|rule| ScopedActivation::from_absolute(&rule, position)),
            prefix,
        });
        self.version = self.version.wrapping_add(1);
    }

    /// Remove all soft breaks in a namespace
    pub fn clear_namespace(&mut self, namespace: &OverlayNamespace, marker_list: &mut MarkerList) {
        let mut indices: Vec<usize> = self
            .breaks
            .iter()
            .enumerate()
            .filter_map(|(i, b)| (&b.namespace == namespace).then_some(i))
            .collect();
        if indices.is_empty() {
            return;
        }
        indices.sort_unstable_by(|a, b| b.cmp(a));
        for idx in indices {
            self.swap_remove_at(idx, marker_list);
        }
        self.version = self.version.wrapping_add(1);
    }

    /// Remove all soft breaks that fall within a byte range and clean up their markers
    pub fn remove_in_range(&mut self, start: usize, end: usize, marker_list: &mut MarkerList) {
        // O(log N + k): query the marker tree for points in `[start, end]`,
        // map back to entries, verify position is in `[start, end)` (the
        // marker tree query is closed; soft-break membership is half-open).
        if start >= end {
            return;
        }
        let hits = marker_list.query_range(start, end);
        if hits.is_empty() {
            return;
        }
        let mut to_remove: Vec<usize> = hits
            .iter()
            .filter_map(|(mid, pos, _)| {
                if *pos < end {
                    self.marker_to_idx.get(mid).copied()
                } else {
                    None
                }
            })
            .collect();
        to_remove.sort_unstable();
        to_remove.dedup();
        if to_remove.is_empty() {
            return;
        }
        // Descending so swap_remove doesn't shift earlier indices.
        to_remove.sort_unstable_by(|a, b| b.cmp(a));
        for idx in to_remove {
            self.swap_remove_at(idx, marker_list);
        }
        self.version = self.version.wrapping_add(1);
    }

    /// Clear all soft breaks and their markers
    pub fn clear(&mut self, marker_list: &mut MarkerList) {
        let had_any = !self.breaks.is_empty();
        for bp in &self.breaks {
            marker_list.delete(bp.marker_id);
        }
        self.breaks.clear();
        self.marker_to_idx.clear();
        if had_any {
            self.version = self.version.wrapping_add(1);
        }
    }

    /// Swap-remove the entry at `idx`, deleting its marker and patching
    /// `marker_to_idx` for whatever entry got swapped in.
    fn swap_remove_at(&mut self, idx: usize, marker_list: &mut MarkerList) {
        let removed = self.breaks.swap_remove(idx);
        self.marker_to_idx.remove(&removed.marker_id);
        marker_list.delete(removed.marker_id);
        if let Some(moved) = self.breaks.get(idx) {
            self.marker_to_idx.insert(moved.marker_id, idx);
        }
    }

    /// Query soft breaks that fall within a viewport range.
    /// Returns sorted `(position, indent)` pairs for efficient token processing.
    ///
    /// `cursors` are the rendering split's cursor byte positions, used to
    /// evaluate cursor-dependent activation rules. Pass `&[]` for
    /// cursor-blind consumers (scroll math, `WrapIndex`) — they see
    /// the canonical "no cursor anywhere" rendering.
    pub fn query_viewport(
        &self,
        start: usize,
        end: usize,
        marker_list: &MarkerList,
        cursors: &[usize],
    ) -> Vec<(usize, u16)> {
        let mut results: Vec<(usize, u16)> = self
            .breaks
            .iter()
            .filter_map(|b| {
                let pos = b.position(marker_list);
                if let Some(a) = &b.activation {
                    if !a.is_active(pos, cursors) {
                        return None;
                    }
                }
                if pos >= start && pos < end {
                    Some((pos, b.indent))
                } else {
                    None
                }
            })
            .collect();

        // Sort by position for sequential processing
        results.sort_by_key(|(pos, _)| *pos);

        results
    }

    /// As [`query_viewport`](Self::query_viewport), but carrying each break's
    /// continuation prefix — what the token transform needs to actually draw
    /// the row.
    ///
    /// `theme` is `Some` on the drawing path and `None` on measurement paths
    /// (the per-line layout the scroll math reads). The prefix text is
    /// emitted either way; only its colour depends on the theme, so both
    /// paths lay out identical columns. Same split as
    /// [`crate::view::ui::split_rendering::transforms::resolve_inline_hints`].
    pub fn query_viewport_rendered(
        &self,
        start: usize,
        end: usize,
        marker_list: &MarkerList,
        cursors: &[usize],
        theme: Option<&Theme>,
    ) -> Vec<SoftBreakRender> {
        let mut results: Vec<SoftBreakRender> = self
            .breaks
            .iter()
            .filter_map(|b| {
                let pos = b.position(marker_list);
                if let Some(a) = &b.activation {
                    if !a.is_active(pos, cursors) {
                        return None;
                    }
                }
                if pos < start || pos >= end {
                    return None;
                }
                Some(SoftBreakRender {
                    position: pos,
                    indent: b.indent,
                    prefix: b
                        .prefix
                        .as_ref()
                        .map(|p| (p.text.clone(), theme.map(|t| p.resolved_style(t)))),
                })
            })
            .collect();

        results.sort_by_key(|b| b.position);

        results
    }

    /// Earliest soft break in `start..end` whose cursor-dependent activation
    /// currently differs from the canonical evaluation. See
    /// `ConcealManager::earliest_cursor_divergence` — same contract.
    pub fn earliest_cursor_divergence(
        &self,
        start: usize,
        end: usize,
        marker_list: &MarkerList,
        cursors: &[usize],
    ) -> Option<usize> {
        self.breaks
            .iter()
            .filter_map(|b| {
                let a = b.activation.as_ref()?;
                let pos = b.position(marker_list);
                if pos < start || pos >= end {
                    return None;
                }
                (a.is_active(pos, cursors) != a.is_active(pos, &[])).then_some(pos)
            })
            .min()
    }

    /// Number of stored soft breaks. Every viewport query walks all of them
    /// (see [`query_viewport`](Self::query_viewport)), so this is the size of
    /// a per-frame scan.
    pub fn len(&self) -> usize {
        self.breaks.len()
    }

    /// Returns true if there are no soft breaks
    pub fn is_empty(&self) -> bool {
        self.breaks.is_empty()
    }

    /// Test-only: assert `marker_to_idx` is consistent with `breaks`.
    /// Panics on any divergence. Used by property tests.
    #[cfg(test)]
    fn check_invariants(&self) {
        assert_eq!(
            self.marker_to_idx.len(),
            self.breaks.len(),
            "marker_to_idx size != breaks size"
        );
        for (i, b) in self.breaks.iter().enumerate() {
            let mapped = self.marker_to_idx.get(&b.marker_id).copied();
            assert_eq!(
                mapped,
                Some(i),
                "marker {:?} should map to idx {} but maps to {:?}",
                b.marker_id,
                i,
                mapped
            );
        }
    }
}

impl Default for SoftBreakManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ns() -> OverlayNamespace {
        OverlayNamespace::from_string("test".to_string())
    }

    #[test]
    fn test_soft_break_remove_in_range_keeps_only_outside() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(200);
        let mut manager = SoftBreakManager::new();

        manager.add(&mut marker_list, ns(), 5, 0);
        manager.add(&mut marker_list, ns(), 25, 0);
        manager.add(&mut marker_list, ns(), 45, 0);
        manager.add(&mut marker_list, ns(), 65, 0);

        // Remove [20..50): 25 and 45 are inside, 5 and 65 stay.
        manager.remove_in_range(20, 50, &mut marker_list);

        let kept: Vec<_> = manager
            .query_viewport(0, 1000, &marker_list, &[])
            .into_iter()
            .map(|(p, _)| p)
            .collect();
        assert_eq!(kept, vec![5, 65]);
    }

    fn prefix(text: &str) -> SoftBreakPrefix {
        SoftBreakPrefix {
            text: text.to_string(),
            style: Style::default(),
            fg_theme_key: None,
            bg_theme_key: None,
        }
    }

    /// `indent` is the only width the scroll math and the wrap index ever
    /// read, so a prefix wider than the indent the plugin declared has to
    /// grow it — otherwise the renderer would draw a row wider than every
    /// other consumer believes it to be.
    #[test]
    fn test_soft_break_prefix_widens_a_too_narrow_indent() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);
        let mut manager = SoftBreakManager::new();

        manager.add_full(&mut marker_list, ns(), 10, 1, None, Some(prefix("▌ ")));

        let breaks = manager.query_viewport(0, 100, &marker_list, &[]);
        assert_eq!(breaks, vec![(10, 2)]);
    }

    /// An indent wider than the prefix is left alone — the prefix is drawn
    /// inside it and the rest pads out.
    #[test]
    fn test_soft_break_prefix_narrower_than_indent_keeps_the_indent() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);
        let mut manager = SoftBreakManager::new();

        manager.add_full(&mut marker_list, ns(), 10, 6, None, Some(prefix("▌ ")));

        let breaks = manager.query_viewport(0, 100, &marker_list, &[]);
        assert_eq!(breaks, vec![(10, 6)]);
    }

    /// The width-only and render queries must report the same columns for
    /// the same break: they feed the scroll math and the renderer, and a
    /// disagreement between those two is what puts the cursor on the wrong
    /// row.
    #[test]
    fn test_soft_break_queries_agree_on_indent() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);
        let mut manager = SoftBreakManager::new();

        manager.add_full(&mut marker_list, ns(), 10, 1, None, Some(prefix("▌ ")));
        manager.add(&mut marker_list, ns(), 40, 3);

        let widths = manager.query_viewport(0, 100, &marker_list, &[]);
        let rendered = manager.query_viewport_rendered(0, 100, &marker_list, &[], None);

        let pairs: Vec<(usize, u16)> = rendered.iter().map(|b| (b.position, b.indent)).collect();
        assert_eq!(widths, pairs);
        assert_eq!(
            rendered[0].prefix.as_ref().map(|(t, _)| t.as_str()),
            Some("▌ ")
        );
        assert_eq!(rendered[1].prefix, None);
    }

    #[test]
    fn test_soft_break_remove_in_range_endpoint_semantics() {
        // Half-open: pos == start removed, pos == end kept.
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);
        let mut manager = SoftBreakManager::new();

        manager.add(&mut marker_list, ns(), 10, 0);
        manager.add(&mut marker_list, ns(), 20, 0);

        manager.remove_in_range(10, 20, &mut marker_list);
        let kept: Vec<_> = manager
            .query_viewport(0, 1000, &marker_list, &[])
            .into_iter()
            .map(|(p, _)| p)
            .collect();
        assert_eq!(kept, vec![20]);
    }

    #[test]
    fn test_soft_break_remove_in_range_bumps_version_only_on_change() {
        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(100);
        let mut manager = SoftBreakManager::new();

        manager.add(&mut marker_list, ns(), 10, 0);
        let v0 = manager.version();

        manager.remove_in_range(50, 60, &mut marker_list);
        assert_eq!(manager.version(), v0);

        manager.remove_in_range(0, 50, &mut marker_list);
        assert!(manager.is_empty());
        assert_ne!(manager.version(), v0);
    }

    /// Mirrors the production cycle: per line in `lines_changed`, clear
    /// soft breaks in the line's byte range, then re-add the line's
    /// soft breaks. Same shape as the matching conceal/overlay perf
    /// tests for direct comparison.
    ///
    /// Run with:
    ///   cargo nextest run -p fresh-editor --no-capture \
    ///     view::soft_break::tests::perf_full_buffer_rebuild_pass
    #[test]
    fn perf_full_buffer_rebuild_pass() {
        const LINES: usize = 500;
        const LINE_BYTES: usize = 50;
        const BREAKS_PER_LINE: usize = 5;

        let mut marker_list = MarkerList::new();
        marker_list.set_buffer_size(LINES * LINE_BYTES);
        let mut manager = SoftBreakManager::new();

        let break_byte = |line: usize, k: usize| -> usize {
            line * LINE_BYTES + k * (LINE_BYTES / BREAKS_PER_LINE)
        };

        // Populate steady state.
        for line in 0..LINES {
            for k in 0..BREAKS_PER_LINE {
                manager.add(&mut marker_list, ns(), break_byte(line, k), 0);
            }
        }
        let initial = LINES * BREAKS_PER_LINE;

        // One full-buffer `lines_changed` pass: per line, clear + re-add.
        let start = std::time::Instant::now();
        for line in 0..LINES {
            let line_start = line * LINE_BYTES;
            let line_end = line_start + LINE_BYTES;
            manager.remove_in_range(line_start, line_end, &mut marker_list);
            for k in 0..BREAKS_PER_LINE {
                manager.add(&mut marker_list, ns(), break_byte(line, k), 0);
            }
        }
        let elapsed = start.elapsed();

        eprintln!(
            "[perf] soft_break full-buffer rebuild ({LINES} lines, {} entries steady): \
             {:?} total, {:?}/line",
            initial,
            elapsed,
            elapsed / LINES as u32,
        );
        let still_present = manager
            .query_viewport(0, LINES * LINE_BYTES, &marker_list, &[])
            .len();
        assert_eq!(still_present, initial);
    }

    mod proptests {
        use super::*;
        use proptest::prelude::*;

        #[derive(Debug, Clone)]
        enum Op {
            Add { pos: usize, indent: u16, ns_idx: u8 },
            RemoveInRange { start: usize, end: usize },
            ClearNamespace { ns_idx: u8 },
        }

        const BUFFER_SIZE: usize = 200;

        fn arb_op() -> impl Strategy<Value = Op> {
            prop_oneof![
                3 => (0..BUFFER_SIZE, 0u16..8u16, 0u8..3u8)
                    .prop_map(|(pos, indent, ns_idx)| Op::Add { pos, indent, ns_idx }),
                2 => (0..BUFFER_SIZE, 0..BUFFER_SIZE)
                    .prop_map(|(a, b)| {
                        let (s, e) = if a <= b { (a, b) } else { (b, a) };
                        Op::RemoveInRange { start: s, end: e }
                    }),
                1 => (0u8..3u8).prop_map(|ns_idx| Op::ClearNamespace { ns_idx }),
            ]
        }

        fn nsf(idx: u8) -> OverlayNamespace {
            OverlayNamespace::from_string(format!("ns{idx}"))
        }

        proptest! {
            /// Invariants must hold after every sequence of operations.
            /// Plus: after `remove_in_range(r)`, no surviving entry's
            /// position lies in `[r.start, r.end)`.
            #[test]
            fn prop_marker_index_consistent(ops in prop::collection::vec(arb_op(), 0..40)) {
                let mut marker_list = MarkerList::new();
                marker_list.set_buffer_size(BUFFER_SIZE);
                let mut manager = SoftBreakManager::new();

                for op in ops {
                    match op {
                        Op::Add { pos, indent, ns_idx } => {
                            manager.add(&mut marker_list, nsf(ns_idx), pos, indent);
                        }
                        Op::RemoveInRange { start, end } => {
                            manager.remove_in_range(start, end, &mut marker_list);
                            // No surviving entry inside the removed range.
                            for (p, _) in manager.query_viewport(0, BUFFER_SIZE, &marker_list, &[]) {
                                prop_assert!(
                                    !(p >= start && p < end),
                                    "entry at {p} survived remove_in_range({start}..{end})",
                                );
                            }
                        }
                        Op::ClearNamespace { ns_idx } => {
                            manager.clear_namespace(&nsf(ns_idx), &mut marker_list);
                        }
                    }
                    manager.check_invariants();
                }
            }
        }
    }
}
