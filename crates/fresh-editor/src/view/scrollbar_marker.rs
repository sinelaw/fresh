//! Plugin-owned markers painted on a split's vertical scrollbar track
//! ("overview ruler" marks), and their projection onto the track.
//!
//! # Why byte anchors
//!
//! Markers are stored as byte offsets in a private [`MarkerList`], exactly
//! like gutter indicators ([`crate::view::margin::MarginManager`]). Two
//! properties fall out of that choice:
//!
//! * **They survive edits.** The interval tree shifts every anchor in
//!   O(log n) per edit, so markers stay glued to their content between plugin
//!   refreshes — through typing, undo, and LSP workspace edits.
//! * **They exist in every file-size regime.** On a large file opened before
//!   the incremental line scan has run, `Buffer::line_count()` is `None` and
//!   there is no line coordinate at all. Bytes are always available.
//!
//! # Why the projection is O(track height)
//!
//! A scrollbar track is one column wide and at most terminal-height tall
//! (~24–200 cells), however large the file. So the render-side representation
//! is a fixed-length array of `track_height` cells — never a per-line array.
//! Building it is O(M) in the *marker* count; painting is O(track_height).
//! Neither term is proportional to file size.
//!
//! The buckets are cached and rebuilt only when their [`ProjectionKey`] changes
//! (marker set, buffer content, geometry, or basis), so a steady-state frame
//! costs one key comparison. This is the same version-keyed staleness idiom as
//! [`crate::view::line_wrap_cache::LineWrapCache`].

use std::collections::{BTreeMap, HashSet};

use fresh_core::api::{OverlayColorSpec, ScrollbarMarker};

use crate::model::marker::{MarkerId, MarkerList};

/// Per-namespace cap on stored markers. The track oversamples wildly at this
/// count already (≤ ~200 cells), so the cap exists to bound *set-time* cost
/// and memory, not fidelity. Mirrors the spirit of `SearchState::MAX_MATCHES`.
pub const MAX_MARKERS_PER_NAMESPACE: usize = 20_000;

/// A stored marker. Colors keep their [`OverlayColorSpec`] form so theme keys
/// resolve at paint time and markers follow theme switches for free.
#[derive(Debug, Clone)]
struct MarkerEntry {
    start: MarkerId,
    /// `None` for a point marker.
    end: Option<MarkerId>,
    color: OverlayColorSpec,
    priority: i32,
}

/// The coordinate basis a scrollbar is currently using.
///
/// This mirrors the three regimes of
/// [`scrollbar_line_counts`](crate::view::ui::split_rendering) exactly. It is
/// produced by the same function that computes the thumb's counts and handed
/// to the marker projection, so markers and thumb cannot disagree about which
/// coordinate space they live in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MarkerBasis {
    /// Exact wrapped-row counts from the visual-row index (small wrapped
    /// buffers only — building that index is O(all lines)).
    VisualRows { total: u64 },
    /// Logical line numbers.
    LogicalLines { total: u64 },
    /// Raw byte offsets. The only basis available on a large file whose line
    /// scan has not run, and O(1) per marker.
    Bytes { total: u64 },
}

impl MarkerBasis {
    fn total(&self) -> u64 {
        match *self {
            MarkerBasis::VisualRows { total }
            | MarkerBasis::LogicalLines { total }
            | MarkerBasis::Bytes { total } => total,
        }
    }

    /// Discriminant used in the bucket cache key.
    fn tag(&self) -> u8 {
        match self {
            MarkerBasis::VisualRows { .. } => 0,
            MarkerBasis::LogicalLines { .. } => 1,
            MarkerBasis::Bytes { .. } => 2,
        }
    }
}

/// Marker storage for one buffer.
///
/// Namespaces are a flat per-buffer string space, as with overlays and gutter
/// indicators. A [`BTreeMap`] keeps iteration order deterministic so two
/// markers of equal priority on the same cell always resolve the same way.
#[derive(Debug, Default)]
pub struct ScrollbarMarkerManager {
    markers: MarkerList,
    namespaces: BTreeMap<String, Vec<MarkerEntry>>,
    version: u64,
}

impl ScrollbarMarkerManager {
    pub fn new() -> Self {
        Self::default()
    }

    /// Bumped on every mutation; part of the bucket cache key.
    pub fn version(&self) -> u64 {
        self.version
    }

    pub fn is_empty(&self) -> bool {
        self.namespaces.values().all(|v| v.is_empty())
    }

    /// Total stored markers across all namespaces.
    pub fn len(&self) -> usize {
        self.namespaces.values().map(|v| v.len()).sum()
    }

    /// Shift all anchors for an insertion. O(log n).
    pub fn adjust_for_insert(&mut self, position: usize, length: usize) {
        self.markers.adjust_for_insert(position, length);
    }

    /// Shift all anchors for a deletion. O(log n).
    pub fn adjust_for_delete(&mut self, position: usize, length: usize) {
        self.markers.adjust_for_delete(position, length);
    }

    /// Shift all anchors for a whole bulk edit at once.
    /// See [`MarkerList::adjust_for_bulk_edits`].
    pub fn adjust_for_bulk_edits(&mut self, edits: &[(usize, usize, usize)]) {
        self.markers.adjust_for_bulk_edits(edits);
    }

    /// Replace a namespace's entire marker set.
    ///
    /// Returns the number of markers actually stored — less than
    /// `resolved.len()` when the cap truncated the set.
    pub fn set_markers(&mut self, namespace: &str, resolved: Vec<ResolvedMarker>) -> usize {
        self.drop_namespace_markers(namespace);
        let stored = self.insert_entries(namespace, resolved);
        self.version = self.version.wrapping_add(1);
        stored
    }

    /// Replace only the markers of `namespace` currently anchored in
    /// `[start, end)`, leaving the rest of the namespace untouched.
    ///
    /// The victims are found with an O(log n + k) interval-tree query; the
    /// per-namespace retain is O(M) in that namespace's size. This runs on a
    /// plugin command, never per frame.
    pub fn set_markers_in_range(
        &mut self,
        namespace: &str,
        start: usize,
        end: usize,
        resolved: Vec<ResolvedMarker>,
    ) -> usize {
        let in_range: HashSet<MarkerId> = self
            .markers
            .query_range(start, end)
            .into_iter()
            .filter(|(_, s, _)| *s >= start && *s < end.max(start))
            .map(|(id, _, _)| id)
            .collect();

        if let Some(entries) = self.namespaces.get_mut(namespace) {
            let mut doomed = Vec::new();
            entries.retain(|e| {
                if in_range.contains(&e.start) {
                    doomed.push(e.clone());
                    false
                } else {
                    true
                }
            });
            for e in doomed {
                self.markers.delete(e.start);
                if let Some(end_id) = e.end {
                    self.markers.delete(end_id);
                }
            }
        }

        let stored = self.insert_entries(namespace, resolved);
        self.version = self.version.wrapping_add(1);
        stored
    }

    /// Remove every marker in a namespace.
    pub fn clear_namespace(&mut self, namespace: &str) {
        self.drop_namespace_markers(namespace);
        self.namespaces.remove(namespace);
        self.version = self.version.wrapping_add(1);
    }

    fn drop_namespace_markers(&mut self, namespace: &str) {
        if let Some(entries) = self.namespaces.get_mut(namespace) {
            for e in entries.drain(..) {
                self.markers.delete(e.start);
                if let Some(end_id) = e.end {
                    self.markers.delete(end_id);
                }
            }
        }
    }

    fn insert_entries(&mut self, namespace: &str, resolved: Vec<ResolvedMarker>) -> usize {
        let entries = self.namespaces.entry(namespace.to_string()).or_default();
        let room = MAX_MARKERS_PER_NAMESPACE.saturating_sub(entries.len());
        let take = resolved.len().min(room);

        for m in resolved.into_iter().take(take) {
            // Right gravity, so text typed at the marked position pushes the
            // marker along with its content.
            let start = self.markers.create(m.start);
            let end = m
                .end
                .filter(|e| *e > m.start)
                .map(|e| self.markers.create(e));
            entries.push(MarkerEntry {
                start,
                end,
                color: m.color,
                priority: m.priority,
            });
        }
        take
    }

    /// Current byte anchors of every marker, with style. Used by the
    /// projection; also the unit-test observation point.
    ///
    /// O(M log n).
    pub fn resolved(&self) -> Vec<ResolvedMarker> {
        let mut out = Vec::with_capacity(self.len());
        for entries in self.namespaces.values() {
            for e in entries {
                let Some(start) = self.markers.get_position(e.start) else {
                    continue;
                };
                let end = e.end.and_then(|id| self.markers.get_position(id));
                out.push(ResolvedMarker {
                    start,
                    end,
                    color: e.color.clone(),
                    priority: e.priority,
                });
            }
        }
        out
    }
}

/// A marker in plain byte coordinates — the form the manager stores and the
/// projection consumes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedMarker {
    pub start: usize,
    pub end: Option<usize>,
    pub color: OverlayColorSpec,
    pub priority: i32,
}

impl ResolvedMarker {
    /// Convert a plugin-supplied marker to byte coordinates.
    ///
    /// `line_to_byte` is only consulted for the `line` convenience field; a
    /// marker whose line cannot be resolved (large file with no line scan yet)
    /// is dropped rather than silently anchored at byte 0 — the mistake
    /// `handle_set_line_indicator` makes with `unwrap_or(0)`.
    pub fn from_api(
        marker: &ScrollbarMarker,
        mut line_to_byte: impl FnMut(usize) -> Option<usize>,
    ) -> Option<Self> {
        let start = match (marker.position, marker.line) {
            (Some(p), _) => p as usize,
            (None, Some(l)) => line_to_byte(l as usize)?,
            (None, None) => return None,
        };
        // `end_line` is inclusive, so its *start* byte is the right end
        // coordinate: the projection maps it to that line's row, which is the
        // last row the streak should cover. An unresolvable end line degrades
        // to a point marker rather than dropping the marker entirely.
        let end = match (marker.end, marker.end_line) {
            (Some(e), _) => Some(e as usize),
            (None, Some(l)) => line_to_byte(l as usize),
            (None, None) => None,
        };
        Some(Self {
            start,
            end,
            color: marker.color.clone(),
            priority: marker.priority.unwrap_or(0),
        })
    }
}

/// Marks the editor contributes itself, alongside the plugin-owned ones.
///
/// Today that is the unsaved-change diff (`Buffer::diff_since_saved`): the
/// same byte ranges that draw the gutter's blue bar, in the same colour, so a
/// change reads identically on both surfaces. Unlike plugin markers these are
/// not anchored — they are derived from the buffer's own save snapshot and
/// recomputed whenever [`ProjectionKey::core_version`] moves, so there is
/// nothing to keep glued through edits.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CoreMarks {
    /// Half-open byte ranges in the current buffer.
    pub ranges: Vec<std::ops::Range<usize>>,
    pub color: OverlayColorSpec,
    pub priority: i32,
}

impl CoreMarks {
    /// The exact `(start, last covered byte)` pairs the projection looks up.
    ///
    /// A range is half-open, so its last covered byte is `end - 1`; projecting
    /// `end` itself would spill the streak onto the row after the change
    /// whenever a range stops on a row boundary.
    ///
    /// Callers that must pre-resolve coordinates — the logical-line basis
    /// builds a byte→line map before projecting — have to seed that map from
    /// *these* bytes. Deriving them in one place is what keeps the two sides
    /// from disagreeing: a lookup miss would answer row 0 and pile marks at
    /// the top of the track.
    pub fn endpoints(&self) -> impl Iterator<Item = (usize, usize)> + '_ {
        self.ranges
            .iter()
            .map(|r| (r.start, r.end.saturating_sub(1).max(r.start)))
    }
}

/// Cache key for a projected bucket column.
///
/// Public so a caller can probe [`ScrollbarMarkerBuckets::cached`] *before*
/// computing the inputs a rebuild needs — gathering [`CoreMarks`] costs a
/// whole-buffer diff, which must not happen on a steady-state frame.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ProjectionKey {
    marker_version: u64,
    /// Buffer/decoration pipeline version — anchors move when the buffer
    /// changes even if the marker set itself did not.
    content_version: u64,
    /// Version of the editor-contributed marks. Separate from
    /// `content_version` because a save changes what they cover without
    /// changing the content.
    core_version: u64,
    track_height: u16,
    basis_tag: u8,
    basis_total: u64,
}

impl ProjectionKey {
    pub fn new(
        manager: &ScrollbarMarkerManager,
        content_version: u64,
        core_version: u64,
        basis: MarkerBasis,
        track_height: usize,
    ) -> Self {
        Self {
            marker_version: manager.version(),
            content_version,
            core_version,
            track_height: track_height.min(u16::MAX as usize) as u16,
            basis_tag: basis.tag(),
            basis_total: basis.total(),
        }
    }
}

/// One track cell's winning marker.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MarkerCell {
    pub color: OverlayColorSpec,
    pub priority: i32,
}

/// Projected marker column for one scrollbar geometry.
///
/// Holds a small FIFO of recently used projections so two splits of the same
/// buffer at different heights don't thrash a single slot (the
/// `LineWrapCache` pattern in miniature).
#[derive(Debug, Default)]
pub struct ScrollbarMarkerBuckets {
    entries: Vec<(ProjectionKey, Vec<Option<MarkerCell>>)>,
    stats: ProjectionStats,
}

/// How much projecting has actually cost this buffer.
///
/// A rebuild walks every stored marker, so `markers_walked / marker count` is
/// how many times the whole set has been re-projected — the number that
/// separates "projected once, then cached" from "re-projected on every frame
/// of a scroll". Kept per-buckets rather than in a process-wide counter so
/// concurrent tests (and split panes) observe only their own work.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct ProjectionStats {
    pub rebuilds: u64,
    pub markers_walked: u64,
}

/// How many distinct scrollbar geometries to keep projected at once.
const MAX_CACHED_PROJECTIONS: usize = 4;

impl ScrollbarMarkerBuckets {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn clear(&mut self) {
        self.entries.clear();
    }

    /// Projection work done for this buffer so far. See [`ProjectionStats`].
    pub fn stats(&self) -> ProjectionStats {
        self.stats
    }

    fn lookup(&self, key: &ProjectionKey) -> Option<usize> {
        self.entries.iter().position(|(k, _)| k == key)
    }

    /// The projection for `key`, if it is still cached.
    ///
    /// Lets a caller skip gathering the rebuild's inputs on a hit — see
    /// [`CoreMarks`].
    pub fn cached(&self, key: &ProjectionKey) -> Option<&[Option<MarkerCell>]> {
        self.lookup(key).map(|i| self.entries[i].1.as_slice())
    }

    /// Most recently projected column for a track of this height, if one is
    /// cached.
    ///
    /// Read-only companion to [`project`], for consumers that run after the
    /// render pass has already projected (the web UI builds its scene from
    /// data rather than from painted cells, so it needs the same column the
    /// terminal just painted — without re-deriving the basis).
    pub fn latest_for_height(&self, track_height: usize) -> Option<&[Option<MarkerCell>]> {
        let h = track_height.min(u16::MAX as usize) as u16;
        self.entries
            .iter()
            .rev()
            .find(|(k, _)| k.track_height == h)
            .map(|(_, cells)| cells.as_slice())
    }
}

/// Project markers onto a track column, reusing the cached projection when
/// nothing that affects it has changed.
///
/// `row_of_byte` maps a byte offset to a coordinate in `basis`'s space; the
/// caller supplies the regime-appropriate lookup (identity for bytes,
/// `get_line_number` for lines, the visual-row index for wrapped rows).
///
/// # Geometry
///
/// A marker at coordinate `c` is painted at row `c * H / total`. That is not
/// an approximation of the thumb's mapping — it is the same mapping. With
/// thumb size `ts = V/T * H` (`V` = viewport height, `T` = total), the thumb
/// top for a viewport starting at `c` is
///
/// ```text
/// c * (H - ts) / (T - V) = c * H * (1 - V/T) / (T - V) = c * H / T
/// ```
///
/// so a marker sits exactly on the thumb's top row when its content is
/// scrolled to the top of the viewport, and the thumb's span covers exactly
/// the markers whose content is on screen. They diverge only where the
/// renderer clamps thumb size (minimum 1 row, maximum 80% of track), which is
/// sub-cell at realistic sizes.
pub fn project<'a>(
    manager: &ScrollbarMarkerManager,
    buckets: &'a mut ScrollbarMarkerBuckets,
    key: ProjectionKey,
    core: Option<&CoreMarks>,
    basis: MarkerBasis,
    track_height: usize,
    row_of_byte: impl FnMut(usize) -> u64,
) -> &'a [Option<MarkerCell>] {
    if let Some(idx) = buckets.lookup(&key) {
        return &buckets.entries[idx].1;
    }

    let cells = build_cells(manager, core, basis, track_height, row_of_byte);
    buckets.stats.rebuilds += 1;
    buckets.stats.markers_walked += manager.len() as u64;

    if buckets.entries.len() >= MAX_CACHED_PROJECTIONS {
        buckets.entries.remove(0);
    }
    buckets.entries.push((key, cells));
    let last = buckets.entries.len() - 1;
    &buckets.entries[last].1
}

fn build_cells(
    manager: &ScrollbarMarkerManager,
    core: Option<&CoreMarks>,
    basis: MarkerBasis,
    track_height: usize,
    mut row_of_byte: impl FnMut(usize) -> u64,
) -> Vec<Option<MarkerCell>> {
    let mut cells: Vec<Option<MarkerCell>> = vec![None; track_height];
    if track_height == 0 {
        return cells;
    }
    let total = basis.total().max(1);
    let h = track_height as u64;

    // Core marks are laid down first, so a plugin marker of *equal* priority
    // painted after them wins the cell — the same order of precedence the
    // gutter applies between the blue unsaved bar and a plugin indicator.
    // Core ranges project through `endpoints()` — the same pairs any caller
    // that pre-resolves coordinates seeded its map from. (Plugin markers keep
    // their documented exclusive-`end` projection: their ends are
    // line-relative, where the difference is sub-cell.)
    let core_marks = core.into_iter().flat_map(|c| {
        c.endpoints()
            .map(move |(s, e)| (s, Some(e), &c.color, c.priority))
    });
    let plugin_marks = manager
        .resolved()
        .into_iter()
        .map(|m| (m.start, m.end, m.color, m.priority));

    // `resolved()` returns owned colours while `core` lends them, so the two
    // sources are walked as one sequence of (start, end, colour, priority)
    // with the colour cloned once, at the cell it wins.
    let mut paint = |start: usize, end: Option<usize>, color: &OverlayColorSpec, priority: i32| {
        let start_row = (row_of_byte(start).min(total.saturating_sub(1)) * h / total) as usize;
        let end_row = match end {
            Some(e) if e > start => {
                (row_of_byte(e).min(total.saturating_sub(1)) * h / total) as usize
            }
            _ => start_row,
        };

        // `max(start_row)` is a guard, not arithmetic: a `row_of_byte` that
        // answered non-monotonically (a missing entry in a pre-resolved map,
        // say) would otherwise index a backwards range and panic — in the
        // render path, where a wrong mark is survivable and a crash is not.
        let last = end_row.max(start_row).min(track_height - 1);
        for cell in cells[start_row.min(last)..=last].iter_mut() {
            let better = match cell {
                Some(existing) => priority >= existing.priority,
                None => true,
            };
            if better {
                *cell = Some(MarkerCell {
                    color: color.clone(),
                    priority,
                });
            }
        }
    };

    for (start, end, color, priority) in core_marks {
        paint(start, end, color, priority);
    }
    for (start, end, color, priority) in plugin_marks {
        paint(start, end, &color, priority);
    }

    cells
}

#[cfg(test)]
mod tests {
    use super::*;

    fn red() -> OverlayColorSpec {
        OverlayColorSpec::Rgb(255, 0, 0)
    }
    fn blue() -> OverlayColorSpec {
        OverlayColorSpec::Rgb(0, 0, 255)
    }

    fn point(start: usize, priority: i32, color: OverlayColorSpec) -> ResolvedMarker {
        ResolvedMarker {
            start,
            end: None,
            color,
            priority,
        }
    }

    #[test]
    fn set_markers_replaces_previous_set() {
        let mut mgr = ScrollbarMarkerManager::new();
        mgr.set_markers("ns", vec![point(10, 0, red()), point(20, 0, red())]);
        assert_eq!(mgr.len(), 2);

        mgr.set_markers("ns", vec![point(30, 0, blue())]);
        assert_eq!(mgr.len(), 1);
        assert_eq!(mgr.resolved()[0].start, 30);
    }

    #[test]
    fn namespaces_are_independent() {
        let mut mgr = ScrollbarMarkerManager::new();
        mgr.set_markers("a", vec![point(10, 0, red())]);
        mgr.set_markers("b", vec![point(20, 0, blue())]);
        mgr.clear_namespace("a");
        let got = mgr.resolved();
        assert_eq!(got.len(), 1);
        assert_eq!(got[0].start, 20);
    }

    /// The whole robustness claim: anchors ride buffer edits without the
    /// plugin republishing.
    #[test]
    fn markers_shift_with_edits() {
        let mut mgr = ScrollbarMarkerManager::new();
        mgr.set_markers("ns", vec![point(100, 0, red())]);

        mgr.adjust_for_insert(0, 10);
        assert_eq!(mgr.resolved()[0].start, 110);

        mgr.adjust_for_delete(0, 20);
        assert_eq!(mgr.resolved()[0].start, 90);
    }

    #[test]
    fn range_replace_leaves_markers_outside_the_range() {
        let mut mgr = ScrollbarMarkerManager::new();
        mgr.set_markers(
            "ns",
            vec![
                point(10, 0, red()),
                point(100, 0, red()),
                point(500, 0, red()),
            ],
        );

        // Republish only [50, 200): the 10 and 500 markers must survive.
        mgr.set_markers_in_range("ns", 50, 200, vec![point(150, 0, blue())]);

        let mut starts: Vec<usize> = mgr.resolved().iter().map(|m| m.start).collect();
        starts.sort_unstable();
        assert_eq!(starts, vec![10, 150, 500]);
    }

    #[test]
    fn range_replace_on_empty_region_only_adds() {
        let mut mgr = ScrollbarMarkerManager::new();
        mgr.set_markers("ns", vec![point(10, 0, red())]);
        mgr.set_markers_in_range("ns", 100, 200, vec![point(150, 0, blue())]);
        assert_eq!(mgr.len(), 2);
    }

    #[test]
    fn per_namespace_cap_truncates() {
        let mut mgr = ScrollbarMarkerManager::new();
        let many: Vec<_> = (0..MAX_MARKERS_PER_NAMESPACE + 500)
            .map(|i| point(i * 4, 0, red()))
            .collect();
        let stored = mgr.set_markers("ns", many);
        assert_eq!(stored, MAX_MARKERS_PER_NAMESPACE);
        assert_eq!(mgr.len(), MAX_MARKERS_PER_NAMESPACE);
    }

    #[test]
    fn from_api_drops_unresolvable_line_instead_of_anchoring_at_zero() {
        let m = ScrollbarMarker {
            position: None,
            line: Some(42),
            end: None,
            end_line: None,
            color: red(),
            priority: None,
        };
        assert!(ResolvedMarker::from_api(&m, |_| None).is_none());
        assert_eq!(
            ResolvedMarker::from_api(&m, |l| Some(l * 10))
                .unwrap()
                .start,
            420
        );
    }

    /// A line-coordinate producer (a `git diff` parser) can span a hunk with
    /// one marker instead of one per line.
    #[test]
    fn from_api_resolves_an_inclusive_end_line() {
        let m = ScrollbarMarker {
            position: None,
            line: Some(10),
            end: None,
            end_line: Some(14),
            color: red(),
            priority: None,
        };
        let r = ResolvedMarker::from_api(&m, |l| Some(l * 10)).unwrap();
        assert_eq!((r.start, r.end), (100, Some(140)));
    }

    /// An explicit byte `end` wins, matching how `position` beats `line`.
    #[test]
    fn from_api_prefers_byte_end_over_end_line() {
        let m = ScrollbarMarker {
            position: Some(0),
            line: None,
            end: Some(55),
            end_line: Some(14),
            color: red(),
            priority: None,
        };
        let r = ResolvedMarker::from_api(&m, |l| Some(l * 10)).unwrap();
        assert_eq!(r.end, Some(55));
    }

    #[test]
    fn from_api_prefers_byte_position_over_line() {
        let m = ScrollbarMarker {
            position: Some(7),
            line: Some(42),
            end: None,
            end_line: None,
            color: red(),
            priority: None,
        };
        assert_eq!(
            ResolvedMarker::from_api(&m, |_| Some(999)).unwrap().start,
            7
        );
    }

    // --- projection ---

    fn project_bytes(
        mgr: &ScrollbarMarkerManager,
        total: u64,
        track: usize,
    ) -> Vec<Option<MarkerCell>> {
        project_bytes_with(mgr, None, total, track)
    }

    fn project_bytes_with(
        mgr: &ScrollbarMarkerManager,
        core: Option<&CoreMarks>,
        total: u64,
        track: usize,
    ) -> Vec<Option<MarkerCell>> {
        let mut buckets = ScrollbarMarkerBuckets::new();
        let basis = MarkerBasis::Bytes { total };
        let key = ProjectionKey::new(mgr, 0, 0, basis, track);
        project(mgr, &mut buckets, key, core, basis, track, |b| b as u64).to_vec()
    }

    #[test]
    fn projection_places_markers_proportionally() {
        let mut mgr = ScrollbarMarkerManager::new();
        // total 1000 bytes, track 10 rows: byte 0 → row 0, 500 → row 5,
        // 999 → row 9.
        mgr.set_markers(
            "ns",
            vec![
                point(0, 0, red()),
                point(500, 0, red()),
                point(999, 0, red()),
            ],
        );
        let cells = project_bytes(&mgr, 1000, 10);
        assert!(cells[0].is_some());
        assert!(cells[5].is_some());
        assert!(cells[9].is_some());
        assert!(cells[1].is_none());
    }

    #[test]
    fn projection_is_file_size_independent() {
        // The same relative positions in a 4 GB file land on the same rows.
        let mut mgr = ScrollbarMarkerManager::new();
        let total: u64 = 4_000_000_000;
        mgr.set_markers(
            "ns",
            vec![
                point(0, 0, red()),
                point((total / 2) as usize, 0, red()),
                point((total - 1) as usize, 0, red()),
            ],
        );
        let cells = project_bytes(&mgr, total, 10);
        assert!(cells[0].is_some());
        assert!(cells[5].is_some());
        assert!(cells[9].is_some());
    }

    #[test]
    fn range_markers_fill_a_streak() {
        let mut mgr = ScrollbarMarkerManager::new();
        mgr.set_markers(
            "ns",
            vec![ResolvedMarker {
                start: 200,
                end: Some(600),
                color: red(),
                priority: 0,
            }],
        );
        let cells = project_bytes(&mgr, 1000, 10);
        for (row, cell) in cells.iter().enumerate().take(7).skip(2) {
            assert!(cell.is_some(), "row {row} should be marked");
        }
        assert!(cells[0].is_none());
        assert!(cells[7].is_none());
    }

    #[test]
    fn higher_priority_wins_a_shared_cell() {
        let mut mgr = ScrollbarMarkerManager::new();
        mgr.set_markers("a", vec![point(500, 5, red())]);
        mgr.set_markers("b", vec![point(505, 1, blue())]);
        let cells = project_bytes(&mgr, 1000, 10);
        assert_eq!(cells[5].as_ref().unwrap().color, red());
    }

    #[test]
    fn projection_is_cached_until_markers_change() {
        let mut mgr = ScrollbarMarkerManager::new();
        mgr.set_markers("ns", vec![point(500, 0, red())]);
        let mut buckets = ScrollbarMarkerBuckets::new();

        let basis = MarkerBasis::Bytes { total: 1000 };
        let mut calls = 0usize;
        for _ in 0..5 {
            let key = ProjectionKey::new(&mgr, 0, 0, basis, 10);
            project(&mgr, &mut buckets, key, None, basis, 10, |b| {
                calls += 1;
                b as u64
            });
        }
        assert_eq!(calls, 1, "steady-state frames must not re-project");

        mgr.set_markers("ns", vec![point(600, 0, red())]);
        let key = ProjectionKey::new(&mgr, 0, 0, basis, 10);
        project(&mgr, &mut buckets, key, None, basis, 10, |b| {
            calls += 1;
            b as u64
        });
        assert_eq!(calls, 2, "a marker change must invalidate the projection");
    }

    #[test]
    fn projection_reruns_when_content_version_changes() {
        let mgr = {
            let mut m = ScrollbarMarkerManager::new();
            m.set_markers("ns", vec![point(500, 0, red())]);
            m
        };
        let mut buckets = ScrollbarMarkerBuckets::new();
        let basis = MarkerBasis::Bytes { total: 1000 };
        let mut calls = 0usize;
        for v in [0u64, 0, 1] {
            let key = ProjectionKey::new(&mgr, v, 0, basis, 10);
            project(&mgr, &mut buckets, key, None, basis, 10, |b| {
                calls += 1;
                b as u64
            });
        }
        assert_eq!(calls, 2);
    }

    #[test]
    fn distinct_track_heights_both_stay_cached() {
        let mut mgr = ScrollbarMarkerManager::new();
        mgr.set_markers("ns", vec![point(500, 0, red())]);
        let mut buckets = ScrollbarMarkerBuckets::new();
        let basis = MarkerBasis::Bytes { total: 1000 };
        let mut calls = 0usize;
        // Two splits at different heights, alternating frames.
        for _ in 0..4 {
            for h in [10usize, 25] {
                let key = ProjectionKey::new(&mgr, 0, 0, basis, h);
                project(&mgr, &mut buckets, key, None, basis, h, |b| {
                    calls += 1;
                    b as u64
                });
            }
        }
        assert_eq!(calls, 2, "each geometry projects once, then stays cached");
    }

    #[test]
    fn empty_track_and_empty_basis_do_not_panic() {
        let mut mgr = ScrollbarMarkerManager::new();
        mgr.set_markers("ns", vec![point(0, 0, red())]);
        assert!(project_bytes(&mgr, 0, 0).is_empty());
        assert_eq!(project_bytes(&mgr, 0, 3).len(), 3);
    }

    #[test]
    fn marker_past_end_of_basis_clamps_to_last_row() {
        let mut mgr = ScrollbarMarkerManager::new();
        mgr.set_markers("ns", vec![point(10_000, 0, red())]);
        let cells = project_bytes(&mgr, 1000, 10);
        assert!(cells[9].is_some());
    }

    // --- editor-contributed marks ---

    fn core(ranges: &[std::ops::Range<usize>], priority: i32) -> CoreMarks {
        CoreMarks {
            ranges: ranges.to_vec(),
            color: blue(),
            priority,
        }
    }

    fn core_one(start: usize, end: usize, priority: i32) -> CoreMarks {
        core(std::slice::from_ref(&(start..end)), priority)
    }

    /// The unsaved-change case: no plugin has set anything, and the editor's
    /// own ranges still reach the track.
    #[test]
    fn core_marks_paint_with_no_plugin_markers_at_all() {
        let mgr = ScrollbarMarkerManager::new();
        let cells = project_bytes_with(&mgr, Some(&core_one(500, 510, 5)), 1000, 10);
        assert_eq!(cells[5].as_ref().unwrap().color, blue());
        assert!(cells[0].is_none() && cells[9].is_none());
    }

    /// A range spanning a chunk of the file paints a proportional streak, not
    /// a dot — a big unsaved edit should read as a big mark.
    #[test]
    fn core_range_paints_every_row_it_spans() {
        let mgr = ScrollbarMarkerManager::new();
        let cells = project_bytes_with(&mgr, Some(&core_one(200, 600, 5)), 1000, 10);
        let painted: Vec<usize> = cells
            .iter()
            .enumerate()
            .filter(|(_, c)| c.is_some())
            .map(|(i, _)| i)
            .collect();
        // Bytes 200..=599 of 1000 over a 10-row track: rows 2 through 5. Row 6
        // belongs to byte 600, which the half-open range does not cover.
        assert_eq!(painted, vec![2, 3, 4, 5]);
    }

    /// Precedence mirrors the gutter: a plugin marker of equal-or-higher
    /// priority takes the cell, so a git hunk (10) is never hidden by the
    /// unsaved-change mark (5) they both land on.
    #[test]
    fn plugin_marker_outranks_a_core_mark_on_the_same_cell() {
        let mut mgr = ScrollbarMarkerManager::new();
        mgr.set_markers("ns", vec![point(500, 10, red())]);
        let cells = project_bytes_with(&mgr, Some(&core_one(500, 501, 5)), 1000, 10);
        assert_eq!(cells[5].as_ref().unwrap().color, red());
    }

    /// ...and a lower-priority plugin marker does not displace a core mark.
    #[test]
    fn core_mark_holds_its_cell_against_a_lower_priority_marker() {
        let mut mgr = ScrollbarMarkerManager::new();
        mgr.set_markers("ns", vec![point(500, 1, red())]);
        let cells = project_bytes_with(&mgr, Some(&core_one(500, 501, 5)), 1000, 10);
        assert_eq!(cells[5].as_ref().unwrap().color, blue());
    }

    /// The core version is keyed separately from the content version because a
    /// save changes what the marks cover without touching the buffer.
    #[test]
    fn projection_reruns_when_the_core_version_changes() {
        let mgr = ScrollbarMarkerManager::new();
        let mut buckets = ScrollbarMarkerBuckets::new();
        let basis = MarkerBasis::Bytes { total: 1000 };
        let marks = core_one(500, 501, 5);
        let mut calls = 0usize;
        for core_version in [0u64, 0, 1] {
            let key = ProjectionKey::new(&mgr, 0, core_version, basis, 10);
            project(&mgr, &mut buckets, key, Some(&marks), basis, 10, |b| {
                calls += 1;
                b as u64
            });
        }
        // A one-byte range needs a single coordinate lookup, so two calls
        // across the three frames means exactly two rebuilds.
        assert_eq!(calls, 2, "a save must invalidate the projection");
    }
}
