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
        debug_assert_eq!(
            self.map.len(),
            self.order.len(),
            "LineWrapCache invariant: map.len() == order.len()"
        );
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

/// Count visual rows for a single line's text under the renderer's
/// wrap algorithm.  Pure function of (text, geometry).
///
/// Behaves exactly like the renderer's per-logical-line wrap count:
/// runs `apply_wrapping_transform` on a single-`Text`-token input and
/// tallies non-empty rows.  A trailing `Break` emitted when the last
/// chunk exactly fills the effective width is followed by nothing
/// meaningful and does not count as a row.
pub fn count_visual_rows_for_text(
    line_text: &str,
    effective_width: usize,
    gutter_width: usize,
    hanging_indent: bool,
) -> u32 {
    use crate::view::ui::split_rendering::transforms::apply_wrapping_transform;
    use fresh_core::api::ViewTokenWire;

    let tokens = vec![ViewTokenWire {
        source_offset: Some(0),
        kind: ViewTokenWireKind::Text(line_text.to_string()),
        style: None,
    }];
    let wrapped = apply_wrapping_transform(tokens, effective_width, gutter_width, hanging_indent);
    let mut rows: u32 = 0;
    let mut row_has_content = false;
    for t in &wrapped {
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
        assert_ne!(
            a,
            pipeline_inputs_version(101, 5, 3),
            "buffer bump changes version"
        );
        assert_ne!(
            a,
            pipeline_inputs_version(100, 6, 3),
            "soft-break bump changes version"
        );
        assert_ne!(
            a,
            pipeline_inputs_version(100, 5, 4),
            "conceal bump changes version"
        );
    }

    #[test]
    #[should_panic]
    fn zero_capacity_rejected() {
        LineWrapCache::with_capacity(0);
    }

    // -------------------------------------------------------------------
    // Layer 4: wrap-function invariants.
    //
    // These hold for any correct wrap regardless of cache state. A cache
    // bug that corrupts a stored value would eventually violate one of
    // them via the cache-backed path (e.g. width-monotonicity).
    // -------------------------------------------------------------------

    /// An empty line wraps to exactly one visual row.
    #[test]
    fn empty_line_is_one_row() {
        for width in [5usize, 10, 42, 80, 120] {
            assert_eq!(count_visual_rows_for_text("", width, 0, false), 1);
            assert_eq!(count_visual_rows_for_text("", width, 6, false), 1);
        }
    }

    /// A line whose visual width fits inside the available width wraps to
    /// exactly one row.  Tests a few short ASCII strings at a few widths.
    #[test]
    fn line_that_fits_is_one_row() {
        // "hello world" = 11 chars; at effective_width=80, gutter=6 →
        // available width = 74 > 11, must be 1 row.
        for text in ["hello", "hello world", "a b c d"] {
            assert_eq!(count_visual_rows_for_text(text, 80, 6, false), 1);
        }
    }

    /// Width monotonicity: widening `effective_width` never *increases*
    /// the row count.
    ///
    /// For a fixed text, any correct wrap satisfies
    ///     w1 <= w2  →  rows(w1) >= rows(w2).
    #[test]
    fn width_monotonicity() {
        let texts = [
            "",
            "short",
            "a b c d e f g h i j k l m n o",
            "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz",
            "word00 word01 word02 word03 word04 word05 word06 word07",
        ];
        let gutter = 2usize;
        for text in &texts {
            let mut prev_rows: Option<u32> = None;
            // effective_width must be > gutter to leave any available
            // width; start well above.
            for w in [10usize, 15, 20, 30, 50, 80, 120, 200] {
                let rows = count_visual_rows_for_text(text, w, gutter, false);
                if let Some(prev) = prev_rows {
                    assert!(
                        rows <= prev,
                        "width monotonicity violated: rows({} chars, w={}) = {} > rows at prev w = {}. \
                         text={:?}",
                        text.len(),
                        w,
                        rows,
                        prev,
                        text,
                    );
                }
                prev_rows = Some(rows);
            }
        }
    }

    /// No row count is ever zero — even pathologically narrow widths or
    /// unusual inputs return at least 1.
    #[test]
    fn row_count_is_always_at_least_one() {
        let cases = [
            ("", 80usize),
            ("x", 80),
            ("", 2), // near-minimum width
            ("abc", 3),
            (
                "a very long line with lots of words that will definitely wrap",
                20,
            ),
        ];
        for (text, w) in cases {
            assert!(
                count_visual_rows_for_text(text, w, 0, false) >= 1,
                "row count < 1 for text={:?}, width={}",
                text,
                w,
            );
        }
    }

    /// Adding characters never *decreases* the row count at a fixed width.
    ///
    /// Subset-superset property: if `a` is a prefix of `b`, `rows(a) <=
    /// rows(b)`.  A cache that returned a stale value for a shortened
    /// line would fail this.
    #[test]
    fn prefix_never_has_more_rows() {
        let base = "aaaaaaaaaa bbbbbbbbbb cccccccccc dddddddddd eeeeeeeeee";
        let width = 20usize;
        let gutter = 2usize;
        let mut prev_rows: u32 = 0;
        for len in (0..=base.len()).step_by(5) {
            let prefix = &base[..len];
            let rows = count_visual_rows_for_text(prefix, width, gutter, false);
            assert!(
                rows >= prev_rows,
                "prefix property violated: len={}, rows={}, prev_rows={}",
                len,
                rows,
                prev_rows,
            );
            prev_rows = rows;
        }
    }

    /// Deterministic: same input → same output, always.
    #[test]
    fn count_is_deterministic() {
        let text = "word00 word01 word02 word03 word04 word05 word06 word07 word08 word09";
        let w = 30usize;
        let g = 4usize;
        let r1 = count_visual_rows_for_text(text, w, g, false);
        for _ in 0..16 {
            let r = count_visual_rows_for_text(text, w, g, false);
            assert_eq!(r, r1, "non-deterministic row count");
        }
    }

    // -------------------------------------------------------------------
    // Layer 3 (partial): shadow-model property test.
    //
    // A "shadow" cache always recomputes from the pure `count_visual_rows
    // _for_text` function; the "real" cache uses `LineWrapCache`. A
    // mutation-free op stream with random (text, width) probes must
    // always agree between real and shadow — otherwise the cache is
    // returning a value inconsistent with fresh computation. Covers the
    // insert / hit / evict surfaces on the cache primitive without
    // running the full editor pipeline.
    //
    // Full plugin-state shadow (buffer edits, soft-break injection,
    // conceals, view-mode toggles) lives in an e2e-level test — this
    // layer is the pure-primitive check.
    // -------------------------------------------------------------------

    #[test]
    fn shadow_agreement_pure_primitive() {
        // Deterministic "random" inputs from simple counters, so this is
        // reproducible without a proptest dep.
        let texts: Vec<String> = (0..30)
            .map(|i| {
                let n = (i * 7 + 3) % 120 + 5;
                let seed = [b'a', b'b', b'c', b' ', b'd', b'e', b'f', b' ', b'1', b'2'];
                (0..n).map(|k| seed[k % seed.len()] as char).collect()
            })
            .collect();
        let widths: [usize; 5] = [12, 20, 42, 80, 120];

        // Op stream: pick (text_idx, width_idx) pairs, query both real
        // and shadow.
        let mut real = LineWrapCache::with_capacity(16);
        for step in 0..400usize {
            let t_idx = (step * 37 + 11) % texts.len();
            let w_idx = (step * 5 + 3) % widths.len();
            let text = &texts[t_idx];
            let width = widths[w_idx];

            let shadow_val = count_visual_rows_for_text(text, width, 2, false);

            let key = LineWrapKey {
                pipeline_inputs_version: 0,
                view_mode: CacheViewMode::Source,
                line_start: t_idx, // stand-in for byte; distinct per text
                effective_width: width as u32,
                gutter_width: 2,
                wrap_column: None,
                hanging_indent: false,
                line_wrap_enabled: true,
            };
            let real_val =
                real.get_or_insert_with(key, || count_visual_rows_for_text(text, width, 2, false));

            assert_eq!(
                real_val, shadow_val,
                "shadow disagreement at step {step}: text_idx={t_idx}, width={width}, \
                 real={real_val}, shadow={shadow_val}",
            );
            assert!(real.len() <= 16, "cache exceeded capacity");
        }
    }

    /// Version-bump invalidation: entries stored under version V are
    /// NEVER returned when a lookup is built at version V+1.  The
    /// old entry sits in memory until FIFO evicts it, but no caller
    /// should ever get the stale value.
    #[test]
    fn version_bump_makes_old_entry_unreachable() {
        let mut cache = LineWrapCache::with_capacity(16);
        let key_v0 = LineWrapKey {
            pipeline_inputs_version: 100,
            view_mode: CacheViewMode::Source,
            line_start: 42,
            effective_width: 80,
            gutter_width: 6,
            wrap_column: None,
            hanging_indent: false,
            line_wrap_enabled: true,
        };
        cache.get_or_insert_with(key_v0, || 5);
        assert_eq!(cache.get(&key_v0), Some(5));

        let key_v1 = LineWrapKey {
            pipeline_inputs_version: 101,
            ..key_v0
        };
        assert_eq!(
            cache.get(&key_v1),
            None,
            "v1 lookup must miss even though v0 entry is still present"
        );

        // Miss path stores under v1; v0 remains in the map, untouched.
        let mut miss_called = 0;
        let v = cache.get_or_insert_with(key_v1, || {
            miss_called += 1;
            7
        });
        assert_eq!(v, 7);
        assert_eq!(miss_called, 1);
        assert_eq!(cache.get(&key_v1), Some(7));
        assert_eq!(
            cache.get(&key_v0),
            Some(5),
            "v0 entry preserved until evicted"
        );
    }

    /// All geometry dimensions in the key are distinct — changing any one
    /// produces a miss.
    #[test]
    fn every_key_dimension_separates_entries() {
        let base = LineWrapKey {
            pipeline_inputs_version: 1,
            view_mode: CacheViewMode::Source,
            line_start: 10,
            effective_width: 80,
            gutter_width: 6,
            wrap_column: None,
            hanging_indent: false,
            line_wrap_enabled: true,
        };

        // Vary each field in turn; each variation must be a distinct key.
        let variations: [LineWrapKey; 8] = [
            LineWrapKey {
                pipeline_inputs_version: 2,
                ..base
            },
            LineWrapKey {
                view_mode: CacheViewMode::Compose,
                ..base
            },
            LineWrapKey {
                line_start: 11,
                ..base
            },
            LineWrapKey {
                effective_width: 81,
                ..base
            },
            LineWrapKey {
                gutter_width: 7,
                ..base
            },
            LineWrapKey {
                wrap_column: Some(70),
                ..base
            },
            LineWrapKey {
                hanging_indent: true,
                ..base
            },
            LineWrapKey {
                line_wrap_enabled: false,
                ..base
            },
        ];

        let mut cache = LineWrapCache::with_capacity(16);
        cache.get_or_insert_with(base, || 1);
        for (i, v) in variations.iter().enumerate() {
            assert_ne!(*v, base, "variation {i} shouldn't equal base");
            assert_eq!(
                cache.get(v),
                None,
                "variation {i} unexpectedly hit base entry"
            );
            cache.get_or_insert_with(*v, || 2 + i as u32);
        }
        // Base entry is still reachable.
        assert_eq!(cache.get(&base), Some(1));
        // Each variation stored its own value.
        for (i, v) in variations.iter().enumerate() {
            assert_eq!(cache.get(v), Some(2 + i as u32));
        }
    }
}
