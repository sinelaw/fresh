//! Line-wrap row-count cache.
//!
//! A bounded per-buffer cache that answers the question "how many visual
//! rows does this logical line wrap to, under these pipeline inputs?"
//!
//! See `docs/internal/line-wrap-cache-plan.md` for the design.  The key
//! ideas, very briefly:
//!
//! * The cache is populated from two sides: the renderer writes entries
//!   as a side effect of running the full pipeline on a visible frame,
//!   and the scroll-math path writes entries by running a one-line
//!   "mini-pipeline" on demand.  Both paths invoke the same underlying
//!   pipeline functions, so the values agree by construction.
//!
//! * Invalidation is implicit: the key includes a
//!   `pipeline_inputs_version` derived from the buffer's and the two
//!   plugin managers' version counters, along with every geometry / view
//!   parameter the pipeline reads.  Mutating any of those produces a
//!   different key for future queries, and old entries age out via FIFO.
//!
//! * Memory is bounded.  The FIFO queue is capped at `capacity`; when
//!   `capacity` is reached on insert, the oldest inserted key is
//!   evicted.  Stale entries never produce wrong answers — they're just
//!   never looked up.
//!
//! Structural invariant maintained at all times:
//!
//!     self.map.len() == self.order.len() <= self.capacity

use crate::state::EditorState;
use crate::view::ui::split_rendering::base_tokens::build_base_tokens;
use crate::view::ui::split_rendering::transforms::{
    apply_conceal_ranges, apply_soft_breaks, apply_wrapping_transform,
};
use fresh_core::api::ViewTokenWireKind;
use std::collections::{HashMap, VecDeque};

/// Default capacity.  At ~80 bytes/entry this is ~650 KB max, comfortably
/// inside a per-buffer memory budget.
pub const DEFAULT_CAPACITY: usize = 8192;

/// View mode the pipeline is running in.  Conceals and some plugin-
/// rendered content only apply in Compose.  Kept as a small plain enum
/// so the key stays cheap to hash.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum CacheViewMode {
    Source,
    Compose,
}

/// Full set of inputs that determine a single logical line's wrapped
/// visual-row count.  Every mutable input must be represented here — if
/// the caller forgets one, stale entries can be returned.
///
/// The `pipeline_inputs_version` folds in the buffer version plus the
/// soft-break and conceal managers' versions (see
/// `LineWrapCache::pipeline_inputs_version`).  The remaining fields are
/// geometry / viewport config.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct LineWrapKey {
    pub pipeline_inputs_version: u64,
    pub view_mode: CacheViewMode,
    pub line_start: usize,
    pub effective_width: u32,
    pub gutter_width: u16,
    pub wrap_column: Option<u32>,
    pub hanging_indent: bool,
    pub line_wrap_enabled: bool,
}

/// Derive the combined pipeline-inputs version from the three source
/// versions.  Any change to any of them flips the combined value.  This
/// is not a hash — it's a packed integer with enough bit-budget to make
/// accidental collisions astronomically unlikely in a single session.
///
/// * `buffer_version` gets the low 32 bits (wrapped to u32).  Buffer edits
///   are the most frequent source of change.
/// * `soft_breaks_version` is shifted up 32 bits.
/// * `conceal_version` is shifted up 48 bits.
///
/// Collisions would require one of the three to wrap its counter AND the
/// others to land on exactly the same values — not a concern for a u32
/// counter in a single session.
#[inline]
pub fn pipeline_inputs_version(
    buffer_version: u64,
    soft_breaks_version: u32,
    conceal_version: u32,
) -> u64 {
    (buffer_version & 0xFFFF_FFFF)
        ^ ((soft_breaks_version as u64) << 32)
        ^ ((conceal_version as u64) << 48)
}

/// Bounded FIFO cache from `LineWrapKey` to visual row count.
///
/// FIFO (not LRU) because the dominant access pattern is sequential
/// scrolling: each line is queried a few times in close succession, then
/// rarely again.  FIFO is simpler to reason about and matches this
/// pattern well enough.  If future profiling shows churn we can swap the
/// eviction policy — the external API doesn't change.
#[derive(Debug, Clone)]
pub struct LineWrapCache {
    map: HashMap<LineWrapKey, u32>,
    order: VecDeque<LineWrapKey>,
    capacity: usize,
}

impl Default for LineWrapCache {
    fn default() -> Self {
        Self::with_capacity(DEFAULT_CAPACITY)
    }
}

impl LineWrapCache {
    pub fn with_capacity(capacity: usize) -> Self {
        assert!(capacity > 0, "LineWrapCache capacity must be > 0");
        Self {
            map: HashMap::with_capacity(capacity),
            order: VecDeque::with_capacity(capacity),
            capacity,
        }
    }

    pub fn len(&self) -> usize {
        debug_assert_eq!(self.map.len(), self.order.len(),
            "LineWrapCache invariant: map.len() == order.len()");
        self.map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn capacity(&self) -> usize {
        self.capacity
    }

    /// Look up a cached value.  Returns `None` on miss.
    pub fn get(&self, key: &LineWrapKey) -> Option<u32> {
        self.map.get(key).copied()
    }

    /// Query by key; on miss, run `compute` and store its result.  This
    /// is the primary entry point for both the renderer's write path and
    /// the scroll-math miss handler.
    ///
    /// Returns the (possibly just-computed) value.  The `compute` closure
    /// is called at most once per cache miss; hits do not invoke it.
    pub fn get_or_insert_with<F>(&mut self, key: LineWrapKey, compute: F) -> u32
    where
        F: FnOnce() -> u32,
    {
        if let Some(&v) = self.map.get(&key) {
            return v;
        }
        let v = compute();
        self.insert_fresh(key, v);
        v
    }

    /// Unconditionally store a value for `key`.  If `key` is already
    /// present, its value is updated in place and its insertion order is
    /// **not** changed (this keeps the FIFO queue simple — re-inserts
    /// don't refresh age).
    ///
    /// The renderer-side writeback uses this after a render pass: it
    /// just-computed each visible line's row count and wants to make
    /// sure the cache holds it.
    pub fn put(&mut self, key: LineWrapKey, value: u32) {
        if let Some(slot) = self.map.get_mut(&key) {
            *slot = value;
            return;
        }
        self.insert_fresh(key, value);
    }

    /// Remove all entries.  Called on config changes that we can't express
    /// through the key (none today, but it's useful for tests and for
    /// plugin-lifecycle events in the future).
    pub fn clear(&mut self) {
        self.map.clear();
        self.order.clear();
    }

    /// Insert a never-before-seen key, evicting oldest first if at capacity.
    ///
    /// Must only be called when `key` is not already in `self.map`.
    fn insert_fresh(&mut self, key: LineWrapKey, value: u32) {
        debug_assert!(!self.map.contains_key(&key));
        if self.map.len() >= self.capacity {
            if let Some(oldest) = self.order.pop_front() {
                self.map.remove(&oldest);
            }
        }
        self.map.insert(key, value);
        self.order.push_back(key);
        debug_assert_eq!(self.map.len(), self.order.len());
        debug_assert!(self.map.len() <= self.capacity);
    }
}

/// Geometry + view config inputs to the wrap pipeline that aren't carried
/// by `EditorState`.  Bundled so the plumbing through call sites doesn't
/// grow a laundry list of parameters.
#[derive(Debug, Clone, Copy)]
pub struct WrapGeometry {
    pub effective_width: usize,
    pub gutter_width: usize,
    pub hanging_indent: bool,
    pub wrap_column: Option<u32>,
    pub line_wrap_enabled: bool,
    pub view_mode: CacheViewMode,
}

impl WrapGeometry {
    /// Build a cache key for a logical line at `line_start` under these
    /// geometry and pipeline-input versions.
    pub fn key(&self, line_start: usize, pipeline_inputs_version: u64) -> LineWrapKey {
        LineWrapKey {
            pipeline_inputs_version,
            view_mode: self.view_mode,
            line_start,
            effective_width: self.effective_width as u32,
            gutter_width: self.gutter_width as u16,
            wrap_column: self.wrap_column,
            hanging_indent: self.hanging_indent,
            line_wrap_enabled: self.line_wrap_enabled,
        }
    }
}

/// Run the same pipeline the renderer runs, scoped to exactly one logical
/// line starting at `line_start`, and return the visual-row count for that
/// line.  Used by the cache miss handler.
///
/// When `geom.line_wrap_enabled` is false, returns 1 without running the
/// pipeline — an unwrapped line is always one visual row.
///
/// The four pipeline steps mirror `view_data::build_view_data`:
///   1. `build_base_tokens(top_byte=line_start, count=1)`
///   2. `apply_soft_breaks` (Compose mode, when any soft breaks overlap the line)
///   3. `apply_conceal_ranges` (Compose mode, when any conceals overlap the line)
///   4. `apply_wrapping_transform`
/// Then count `Break` tokens before the first `Newline` (which closes this
/// logical line) and add 1 for the row the line itself occupies.
pub fn count_visual_rows_via_pipeline(
    state: &mut EditorState,
    line_start: usize,
    line_end: usize,
    geom: &WrapGeometry,
) -> u32 {
    if !geom.line_wrap_enabled {
        return 1;
    }

    let is_binary = state.buffer.is_binary();
    let line_ending = state.buffer.line_ending();
    let estimated_line_length = state.buffer.estimated_line_length();

    // Step 1: build tokens for just this one logical line.
    let mut tokens = build_base_tokens(
        &mut state.buffer,
        line_start,
        estimated_line_length,
        1, // just this one logical line
        is_binary,
        line_ending,
        &[], // no fold skip ranges — folds affect what's rendered, not per-line wrap count
    );

    let is_compose = matches!(geom.view_mode, CacheViewMode::Compose);

    // Step 2: soft breaks (Compose mode only; same gating as the renderer).
    if is_compose && !state.soft_breaks.is_empty() {
        let sb = state
            .soft_breaks
            .query_viewport(line_start, line_end, &state.marker_list);
        if !sb.is_empty() {
            tokens = apply_soft_breaks(tokens, &sb);
        }
    }

    // Step 3: conceal ranges (Compose mode only).
    if is_compose && !state.conceals.is_empty() {
        let cr = state
            .conceals
            .query_viewport(line_start, line_end, &state.marker_list);
        if !cr.is_empty() {
            tokens = apply_conceal_ranges(tokens, &cr);
        }
    }

    // Step 4: wrap.
    tokens = apply_wrapping_transform(
        tokens,
        geom.effective_width,
        geom.gutter_width,
        geom.hanging_indent,
    );

    // Count non-empty visual rows before the first Newline.
    //
    // `build_base_tokens` may emit tokens for more than one logical line
    // because its internal cap is `visible_count + 4`; the first Newline
    // closes the logical line we care about.
    //
    // `apply_wrapping_transform` can emit a *trailing* Break when the last
    // chunk fills `effective_width` exactly — that Break is width-triggered
    // and is followed by nothing of substance, so it doesn't represent a
    // real wrap. We track "did this row have any content" and only count
    // rows that did.
    let mut rows: u32 = 0;
    let mut row_has_content = false;
    for t in &tokens {
        match &t.kind {
            ViewTokenWireKind::Newline => break,
            ViewTokenWireKind::Break => {
                if row_has_content {
                    rows += 1;
                }
                row_has_content = false;
            }
            ViewTokenWireKind::Text(s) => {
                if !s.is_empty() {
                    row_has_content = true;
                }
            }
            ViewTokenWireKind::Space | ViewTokenWireKind::BinaryByte(_) => {
                row_has_content = true;
            }
        }
    }
    if row_has_content {
        rows += 1;
    }
    rows.max(1)
}

/// Combined version of all pipeline inputs on the given state.  Fold into
/// a `LineWrapKey` to make stale entries unreachable on any mutation.
#[inline]
pub fn state_pipeline_inputs_version(state: &EditorState) -> u64 {
    pipeline_inputs_version(
        state.buffer.version(),
        state.soft_breaks.version(),
        state.conceals.version(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn key(line_start: usize, version: u64) -> LineWrapKey {
        LineWrapKey {
            pipeline_inputs_version: version,
            view_mode: CacheViewMode::Source,
            line_start,
            effective_width: 80,
            gutter_width: 6,
            wrap_column: None,
            hanging_indent: false,
            line_wrap_enabled: true,
        }
    }

    #[test]
    fn empty_cache_is_empty() {
        let cache = LineWrapCache::default();
        assert!(cache.is_empty());
        assert_eq!(cache.len(), 0);
    }

    #[test]
    fn get_or_insert_caches_on_miss() {
        let mut cache = LineWrapCache::with_capacity(4);
        let mut compute_calls = 0;
        let v = cache.get_or_insert_with(key(100, 1), || {
            compute_calls += 1;
            7
        });
        assert_eq!(v, 7);
        assert_eq!(compute_calls, 1);
        assert_eq!(cache.len(), 1);
    }

    #[test]
    fn repeat_lookup_is_a_hit() {
        let mut cache = LineWrapCache::with_capacity(4);
        let mut compute_calls = 0;
        cache.get_or_insert_with(key(100, 1), || {
            compute_calls += 1;
            7
        });
        let v = cache.get_or_insert_with(key(100, 1), || {
            compute_calls += 1;
            99 // wrong value, should not be invoked
        });
        assert_eq!(v, 7);
        assert_eq!(compute_calls, 1, "second lookup should be a hit");
    }

    #[test]
    fn different_versions_are_separate_entries() {
        let mut cache = LineWrapCache::with_capacity(4);
        cache.get_or_insert_with(key(100, 1), || 3);
        cache.get_or_insert_with(key(100, 2), || 5);
        assert_eq!(cache.get(&key(100, 1)), Some(3));
        assert_eq!(cache.get(&key(100, 2)), Some(5));
        assert_eq!(cache.len(), 2);
    }

    #[test]
    fn evicts_oldest_when_capacity_reached() {
        let mut cache = LineWrapCache::with_capacity(3);
        cache.get_or_insert_with(key(100, 1), || 1);
        cache.get_or_insert_with(key(200, 1), || 2);
        cache.get_or_insert_with(key(300, 1), || 3);
        assert_eq!(cache.len(), 3);
        // Inserting a fourth evicts the oldest (line_start=100).
        cache.get_or_insert_with(key(400, 1), || 4);
        assert_eq!(cache.len(), 3);
        assert_eq!(cache.get(&key(100, 1)), None, "oldest evicted");
        assert_eq!(cache.get(&key(200, 1)), Some(2));
        assert_eq!(cache.get(&key(300, 1)), Some(3));
        assert_eq!(cache.get(&key(400, 1)), Some(4));
    }

    #[test]
    fn structural_invariant_holds_under_many_inserts() {
        let mut cache = LineWrapCache::with_capacity(16);
        for i in 0..200u64 {
            cache.get_or_insert_with(key(i as usize, i), || i as u32);
            assert!(cache.len() <= 16);
            assert_eq!(cache.len(), cache.map.len());
            assert_eq!(cache.len(), cache.order.len());
        }
    }

    #[test]
    fn put_overwrites_existing_value_without_reordering() {
        let mut cache = LineWrapCache::with_capacity(3);
        cache.get_or_insert_with(key(100, 1), || 1);
        cache.get_or_insert_with(key(200, 1), || 2);
        cache.get_or_insert_with(key(300, 1), || 3);
        // Overwrite middle.
        cache.put(key(200, 1), 42);
        assert_eq!(cache.get(&key(200, 1)), Some(42));
        // Inserting a new entry still evicts 100 (oldest), not 200.
        cache.get_or_insert_with(key(400, 1), || 4);
        assert_eq!(cache.get(&key(100, 1)), None);
        assert_eq!(cache.get(&key(200, 1)), Some(42));
        assert_eq!(cache.get(&key(400, 1)), Some(4));
    }

    #[test]
    fn clear_empties_cache() {
        let mut cache = LineWrapCache::with_capacity(4);
        cache.get_or_insert_with(key(100, 1), || 1);
        cache.get_or_insert_with(key(200, 1), || 2);
        cache.clear();
        assert!(cache.is_empty());
        assert_eq!(cache.get(&key(100, 1)), None);
    }

    #[test]
    fn pipeline_inputs_version_changes_when_any_source_changes() {
        let a = pipeline_inputs_version(100, 5, 3);
        assert_ne!(a, pipeline_inputs_version(101, 5, 3), "buffer bump changes version");
        assert_ne!(a, pipeline_inputs_version(100, 6, 3), "soft-break bump changes version");
        assert_ne!(a, pipeline_inputs_version(100, 5, 4), "conceal bump changes version");
    }

    #[test]
    #[should_panic]
    fn zero_capacity_rejected() {
        LineWrapCache::with_capacity(0);
    }
}
