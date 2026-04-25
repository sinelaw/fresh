//! Line-wrap pipeline-output cache.
//!
//! A bounded per-buffer cache from `LineWrapKey` to `Arc<Vec<ViewLine>>` —
//! the final output of the render pipeline for a single logical line.
//!
//! See `docs/internal/line-wrap-cache-plan.md` for the full design.  In
//! brief:
//!
//! * **Single source of truth.**  The value stored is what the renderer
//!   actually produces.  Every consumer that needs to know "how many
//!   visual rows?", "where does byte X land visually?", "what byte is at
//!   visual column N?" reads the same `ViewLine` structures via the
//!   methods `ViewLine` already exposes (`source_byte_at_char`,
//!   `char_at_visual_col`, `source_byte_at_visual_col`, `visual_col_at_char`,
//!   `visual_width`).  No second implementation to drift from.
//!
//! * **Two writers, one pipeline.**  The renderer populates cache entries
//!   as a side effect of its normal per-frame work; the miss handler in
//!   this module runs the same four-step pipeline scoped to a single
//!   logical line.  Same inputs → same output.
//!
//! * **Invalidation by key.**  The key includes `pipeline_inputs_version`
//!   (a packed u64 derived from `buffer.version()`, `SoftBreakManager::
//!   version()`, and `ConcealManager::version()`) plus every geometry /
//!   view dimension the pipeline reads.  Mutating any of those produces a
//!   different key; old entries become unreachable and age out via FIFO
//!   eviction.  There is no active invalidate step.
//!
//! * **Byte-budget eviction.**  Because `Vec<ViewLine>` sizes vary from
//!   a few hundred bytes for a short line to megabytes for a long line
//!   wrapping into thousands of rows, count-based eviction is the wrong
//!   metric.  The cache tracks approximate total memory and evicts
//!   oldest-first when a new insert would exceed the byte budget.
//!
//! Structural invariants maintained at all times:
//!
//!     self.map.len() == self.order.len()
//!     self.current_bytes <= self.byte_budget  (after any insert)

use crate::state::EditorState;
use crate::view::ui::split_rendering::base_tokens::build_base_tokens;
use crate::view::ui::split_rendering::transforms::{
    apply_conceal_ranges, apply_soft_breaks, apply_wrapping_transform,
};
use crate::view::ui::view_pipeline::{ViewLine, ViewLineIterator};
use fresh_core::api::ViewTokenWireKind;
use std::collections::{HashMap, VecDeque};
use std::sync::Arc;

/// Default byte budget: 8 MiB.  Comfortably holds the full layout for a
/// small-to-medium buffer, a handful of huge lines, or any interactive
/// scroll span.  A single 200 KB line wrapping to ~2000 rows takes
/// roughly 2 MB in its `Vec<ViewLine>` form, so the budget can absorb
/// several such lines before churning.
pub const DEFAULT_BYTE_BUDGET: usize = 8 * 1024 * 1024;

/// View mode the pipeline is running in.  Conceals and some plugin-
/// rendered content only apply in Compose.  Kept as a small plain enum
/// so the key stays cheap to hash.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum CacheViewMode {
    Source,
    Compose,
}

/// Full set of inputs that determine a single logical line's wrapped
/// layout.  Every mutable input must be represented here — if the
/// caller forgets one, stale entries can be returned.
///
/// The `pipeline_inputs_version` folds in the buffer version plus the
/// soft-break and conceal managers' versions (see
/// [`pipeline_inputs_version`]).  The remaining fields are geometry /
/// viewport config.
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
/// * `buffer_version` gets the low 32 bits (wrapped to u32).  Buffer
///   edits are the most frequent source of change.
/// * `soft_breaks_version` is shifted up 32 bits.
/// * `conceal_version` is shifted up 48 bits.
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

/// Estimate the in-memory size of a `Vec<ViewLine>` for byte-budget
/// accounting.  Rough but stable — we'd rather over- than under-estimate
/// so the budget stays honest.
///
/// Per `ViewLine`:
///   - `text` (String): bytes in the rendered text
///   - `char_source_bytes` (Vec<Option<usize>>): 16 bytes × chars
///   - `char_styles` (Vec<Option<ViewTokenStyle>>): ~32 bytes × chars
///   - `char_visual_cols` (Vec<usize>): 8 bytes × chars
///   - `visual_to_char` (Vec<usize>): 8 bytes × visual cols
///   - overhead (HashSet, enum, bool, alignment padding): ~64 bytes
///
/// Round up to `visual_width * 64 + text.len() + 96` for simplicity.
fn estimate_view_lines_bytes(lines: &[ViewLine]) -> usize {
    let mut total = 48; // Arc + Vec overhead
    for line in lines {
        let chars = line.char_source_bytes.len();
        let visual = line.visual_to_char.len();
        total += line.text.len() + chars * 56 + visual * 8 + 96;
    }
    total
}

/// Bounded FIFO cache from `LineWrapKey` to `Arc<Vec<ViewLine>>`.
///
/// FIFO (not LRU) because the dominant access pattern is sequential
/// scrolling: each line is queried a few times in close succession, then
/// rarely again.  FIFO is simpler to reason about and matches this
/// pattern well enough.  If future profiling shows churn we can swap the
/// eviction policy — the external API doesn't change.
#[derive(Debug, Clone)]
pub struct LineWrapCache {
    map: HashMap<LineWrapKey, Arc<Vec<ViewLine>>>,
    order: VecDeque<LineWrapKey>,
    byte_budget: usize,
    current_bytes: usize,
}

impl Default for LineWrapCache {
    fn default() -> Self {
        Self::with_byte_budget(DEFAULT_BYTE_BUDGET)
    }
}

impl LineWrapCache {
    pub fn with_byte_budget(byte_budget: usize) -> Self {
        assert!(byte_budget > 0, "LineWrapCache byte_budget must be > 0");
        Self {
            map: HashMap::new(),
            order: VecDeque::new(),
            byte_budget,
            current_bytes: 0,
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

    pub fn byte_budget(&self) -> usize {
        self.byte_budget
    }

    pub fn current_bytes(&self) -> usize {
        self.current_bytes
    }

    /// Look up a cached value.  Returns `None` on miss.  The returned
    /// `Arc` is a cheap clone; callers can hold it without copying the
    /// underlying `Vec<ViewLine>`.
    pub fn get(&self, key: &LineWrapKey) -> Option<Arc<Vec<ViewLine>>> {
        self.map.get(key).cloned()
    }

    /// Query by key; on miss, run `compute` and store its result.  The
    /// primary entry point for both the renderer's write path and the
    /// scroll-math miss handler.
    ///
    /// Returns the (possibly just-computed) value as an `Arc`.  The
    /// `compute` closure is called at most once per cache miss; hits do
    /// not invoke it.
    pub fn get_or_insert_with<F>(&mut self, key: LineWrapKey, compute: F) -> Arc<Vec<ViewLine>>
    where
        F: FnOnce() -> Vec<ViewLine>,
    {
        if let Some(v) = self.map.get(&key) {
            return v.clone();
        }
        let value = Arc::new(compute());
        self.insert_fresh(key, value.clone());
        value
    }

    /// Unconditionally store a value for `key`.  If `key` is already
    /// present, its value is replaced in place and its FIFO position is
    /// **not** changed (this keeps the queue simple — re-inserts don't
    /// refresh age).  Byte-budget accounting is updated.
    pub fn put(&mut self, key: LineWrapKey, value: Arc<Vec<ViewLine>>) {
        if let Some(existing) = self.map.get_mut(&key) {
            let old_bytes = estimate_view_lines_bytes(existing);
            let new_bytes = estimate_view_lines_bytes(&value);
            *existing = value;
            self.current_bytes = self.current_bytes + new_bytes - old_bytes.min(self.current_bytes);
            return;
        }
        self.insert_fresh(key, value);
    }

    /// Remove all entries.  Used by tests and by future
    /// plugin-lifecycle events.
    pub fn clear(&mut self) {
        self.map.clear();
        self.order.clear();
        self.current_bytes = 0;
    }

    /// Insert a never-before-seen key, evicting oldest-first until the
    /// new entry fits inside `byte_budget`.
    fn insert_fresh(&mut self, key: LineWrapKey, value: Arc<Vec<ViewLine>>) {
        debug_assert!(!self.map.contains_key(&key));
        let new_bytes = estimate_view_lines_bytes(&value);

        // Evict until (current_bytes + new_bytes) fits.  Always keep at
        // least one slot — if the single new entry alone exceeds the
        // budget, we still accept it (the cache was asked to hold it;
        // the alternative is silently dropping data the caller just
        // paid to compute).
        while self.current_bytes + new_bytes > self.byte_budget && !self.order.is_empty() {
            if let Some(oldest_key) = self.order.pop_front() {
                if let Some(oldest_val) = self.map.remove(&oldest_key) {
                    let shed = estimate_view_lines_bytes(&oldest_val);
                    self.current_bytes = self.current_bytes.saturating_sub(shed);
                }
            }
        }

        self.map.insert(key, value);
        self.order.push_back(key);
        self.current_bytes += new_bytes;
        debug_assert_eq!(self.map.len(), self.order.len());
    }
}

/// Materialise a line's layout as `Vec<ViewLine>` from plain text
/// alone — no buffer iteration, no soft breaks, no conceals.
///
/// Useful at sites that have `line_text: &str` in hand and can't
/// easily reach `EditorState` (or are inside a `line_iterator` borrow).
/// The produced `ViewLine`s match the renderer's word-boundary wrap
/// on the same text at the same geometry, so row counts and cursor
/// mappings agree with `layout_for_line` in the absence of soft
/// breaks / conceals.  When soft breaks or conceals ARE active for
/// the line, callers should prefer `layout_for_line` to get accurate
/// layout.
pub fn layout_for_plain_text(
    line_text: &str,
    effective_width: usize,
    gutter_width: usize,
    hanging_indent: bool,
    tab_size: usize,
) -> Vec<ViewLine> {
    use crate::view::ui::view_pipeline::LineStart;
    use fresh_core::api::ViewTokenWire;
    let tokens = vec![ViewTokenWire {
        source_offset: Some(0),
        kind: ViewTokenWireKind::Text(line_text.to_string()),
        style: None,
    }];
    let wrapped = apply_wrapping_transform(tokens, effective_width, gutter_width, hanging_indent);
    let mut lines: Vec<ViewLine> =
        ViewLineIterator::new(&wrapped, false, true, tab_size, false).collect();
    // Invariant: every logical line is at least one visual row.  An
    // empty input produces zero ViewLines through the iterator; emit
    // one placeholder so callers (scrollbar row counts, scroll math)
    // see consistent ≥1 results matching `compute_line_layout`.
    if lines.is_empty() {
        lines.push(ViewLine {
            text: String::new(),
            source_start_byte: Some(0),
            char_source_bytes: Vec::new(),
            char_styles: Vec::new(),
            char_visual_cols: Vec::new(),
            visual_to_char: Vec::new(),
            tab_starts: std::collections::HashSet::new(),
            line_start: LineStart::Beginning,
            ends_with_newline: false,
        });
    }
    lines
}

/// Look up a line's layout in the cache, running the mini-pipeline to
/// fill on miss.  The primary read-path entry point for consumers that
/// need full `ViewLine` layout (not just row count).
///
/// Guarantees that the value returned matches what the renderer would
/// produce for the same line under the same pipeline inputs: same
/// function chain is called either way, so cache hit and miss are
/// indistinguishable to the caller.
pub fn layout_for_line(
    state: &mut EditorState,
    line_start: usize,
    line_end: usize,
    geom: &WrapGeometry,
) -> Arc<Vec<ViewLine>> {
    let version = pipeline_inputs_version(
        state.buffer.version(),
        state.soft_breaks.version(),
        state.conceals.version(),
    );
    let key = geom.key(line_start, version);
    if let Some(cached) = state.line_wrap_cache.get(&key) {
        return cached;
    }
    let layout = compute_line_layout(state, line_start, line_end, geom);
    let arc = Arc::new(layout);
    state.line_wrap_cache.put(key, arc.clone());
    arc
}

/// Given a logical line's layout and a character position within the
/// LOGICAL line (not the ViewLine), return `(segment_idx,
/// col_in_segment)` — the index of the `ViewLine` the character falls
/// into, and the visual column within that `ViewLine`.
///
/// Replaces `primitives::line_wrapping::char_position_to_segment` for
/// callers that have a cached `Vec<ViewLine>`.
///
/// The trick: continuation `ViewLine`s can carry hanging-indent
/// characters at their start whose `source_offset` is `None` (they
/// don't correspond to any source byte).  Those chars must NOT count
/// toward the source-character position we're walking past.  So we
/// sum *source* characters per row (char_source_bytes entries that
/// are `Some(_)`) to find the row containing `char_pos_in_line`, and
/// within that row we locate the specific char whose source_offset
/// matches.
///
/// If `layout` is empty, returns `(0, 0)`.  If the position is past
/// the end of the last row, returns the last row with the last
/// visual column of that row.
pub fn char_position_in_layout(layout: &[ViewLine], char_pos_in_line: usize) -> (usize, usize) {
    if layout.is_empty() {
        return (0, 0);
    }
    let mut source_chars_consumed = 0usize;
    for (i, line) in layout.iter().enumerate() {
        let source_chars_in_row = line
            .char_source_bytes
            .iter()
            .filter(|b| b.is_some())
            .count();
        if char_pos_in_line < source_chars_consumed + source_chars_in_row {
            // The target source-char is in this row.  Find the
            // `char_idx` whose position-among-source-chars equals
            // the within-row offset, then convert to visual column.
            let within_row = char_pos_in_line - source_chars_consumed;
            let mut source_count = 0usize;
            for (char_idx, byte) in line.char_source_bytes.iter().enumerate() {
                if byte.is_some() {
                    if source_count == within_row {
                        return (i, line.visual_col_at_char(char_idx));
                    }
                    source_count += 1;
                }
            }
            // Fallback: shouldn't happen given the length check above,
            // but don't return garbage if it does.
            return (i, line.visual_width().saturating_sub(1));
        }
        source_chars_consumed += source_chars_in_row;
    }
    // Past the end: return the last row's last visual column.  (A
    // cursor one past the last source char on the last row lands
    // here.)
    let last_idx = layout.len() - 1;
    let last = &layout[last_idx];
    let last_col = last.visual_width().saturating_sub(1);
    (last_idx, last_col)
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

/// Run the same pipeline the renderer runs, scoped to exactly one
/// logical line starting at `line_start`, and return the rendered
/// [`ViewLine`]s for that line.  Used by the cache miss handler.
///
/// When `geom.line_wrap_enabled` is false, returns a single
/// placeholder `ViewLine` — an unwrapped line always occupies exactly
/// one visual row.  (Callers that only need a count can read
/// `.len()`; callers that need coordinate mappings would not query
/// this path with wrapping off.)
///
/// The four pipeline steps mirror `view_data::build_view_data`:
///   1. `build_base_tokens(top_byte=line_start, count=1)`
///   2. `apply_soft_breaks` (Compose mode, when any soft breaks overlap)
///   3. `apply_conceal_ranges` (Compose mode, when any conceals overlap)
///   4. `apply_wrapping_transform`
/// followed by `ViewLineIterator::collect()` to materialise the
/// `Vec<ViewLine>`.
///
/// The result is what the renderer would produce for this single
/// logical line — the single source of truth the cache exists to
/// share.
pub fn compute_line_layout(
    state: &mut EditorState,
    line_start: usize,
    line_end: usize,
    geom: &WrapGeometry,
) -> Vec<ViewLine> {
    let is_binary = state.buffer.is_binary();
    let line_ending = state.buffer.line_ending();
    let estimated_line_length = state.buffer.estimated_line_length();
    let tab_size = state.buffer_settings.tab_size;

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

    // Step 4: wrap (only when line-wrap is actually enabled).  When
    // disabled, pass tokens through unchanged; ViewLineIterator will
    // still yield one ViewLine per Newline boundary.
    if geom.line_wrap_enabled {
        tokens = apply_wrapping_transform(
            tokens,
            geom.effective_width,
            geom.gutter_width,
            geom.hanging_indent,
        );
    }

    // Materialise the ViewLines.  `build_base_tokens` may emit tokens
    // for more than one logical line; collect only the first logical
    // line's ViewLines (those up to and including the first Newline).
    let all_lines: Vec<ViewLine> =
        ViewLineIterator::new(&tokens, is_binary, !is_binary, tab_size, false).collect();

    // The `ViewLineIterator` produces one `ViewLine` per visual row.
    // The Newline tokens inside split the stream at logical-line
    // boundaries: every `ViewLine` after the first whose `line_start`
    // is `AfterSourceNewline` begins a NEW logical line, which we
    // don't want.  Keep only rows up to (but not including) the first
    // such transition.
    let mut result = Vec::with_capacity(all_lines.len().min(8));
    for (i, line) in all_lines.into_iter().enumerate() {
        use crate::view::ui::view_pipeline::LineStart;
        if i > 0 && matches!(line.line_start, LineStart::AfterSourceNewline) {
            break;
        }
        result.push(line);
    }
    if result.is_empty() {
        // Defensive: even a completely empty logical line corresponds
        // to exactly one visual row.  The iterator should always
        // produce at least one, but be safe.
        result.push(ViewLine {
            text: String::new(),
            source_start_byte: Some(line_start),
            char_source_bytes: Vec::new(),
            char_styles: Vec::new(),
            char_visual_cols: Vec::new(),
            visual_to_char: Vec::new(),
            tab_starts: std::collections::HashSet::new(),
            line_start: crate::view::ui::view_pipeline::LineStart::Beginning,
            ends_with_newline: false,
        });
    }
    result
}

/// Row count only.  Thin wrapper over [`compute_line_layout`] for
/// callers that need just the visual-row count — scroll math,
/// thumb-size math.  Prefer calling through the cache
/// (`get_or_insert_with(key, || compute_line_layout(...)).len()`).
pub fn count_visual_rows_via_pipeline(
    state: &mut EditorState,
    line_start: usize,
    line_end: usize,
    geom: &WrapGeometry,
) -> u32 {
    compute_line_layout(state, line_start, line_end, geom).len() as u32
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

/// Build a placeholder `Vec<ViewLine>` of a given row count for cache
/// consumers that only need `.len()` (e.g. scroll math's count-only
/// queries, or the per-viewport row-count memoization).  The returned
/// `ViewLine`s have empty char/visual mappings — they carry no real
/// layout information.
///
/// This exists because the cache is typed on `Vec<ViewLine>` so the
/// cross-consumer path can share real layout, but some call sites
/// don't yet have access to `EditorState` (needed by
/// [`compute_line_layout`]).  When those sites are migrated to take
/// `&mut EditorState`, this helper can go away.
pub fn placeholder_layout_for_row_count(n: u32) -> Vec<ViewLine> {
    use crate::view::ui::view_pipeline::LineStart;
    (0..n)
        .map(|_| ViewLine {
            text: String::new(),
            source_start_byte: None,
            char_source_bytes: Vec::new(),
            char_styles: Vec::new(),
            char_visual_cols: Vec::new(),
            visual_to_char: Vec::new(),
            tab_starts: std::collections::HashSet::new(),
            line_start: LineStart::Beginning,
            ends_with_newline: false,
        })
        .collect()
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
    use crate::view::ui::view_pipeline::LineStart;

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

    /// Build a dummy `Vec<ViewLine>` of length `n` for primitive tests
    /// that only care about how the cache stores / evicts values, not
    /// about the actual pipeline output.  Each `ViewLine` is empty
    /// apart from its row identity.
    fn dummy_lines(n: u32) -> Vec<ViewLine> {
        (0..n)
            .map(|_| ViewLine {
                text: String::new(),
                source_start_byte: Some(0),
                char_source_bytes: Vec::new(),
                char_styles: Vec::new(),
                char_visual_cols: Vec::new(),
                visual_to_char: Vec::new(),
                tab_starts: std::collections::HashSet::new(),
                line_start: LineStart::Beginning,
                ends_with_newline: false,
            })
            .collect()
    }

    /// Roomy byte budget for tests that shouldn't evict.
    const ROOMY: usize = 1024 * 1024;
    /// Tight byte budget that evicts after a handful of empty lines.
    /// Each empty `ViewLine` is ~96 bytes plus 48 Vec/Arc overhead, so
    /// this budget holds roughly 3 entries.
    const TIGHT: usize = 500;

    #[test]
    fn empty_cache_is_empty() {
        let cache = LineWrapCache::default();
        assert!(cache.is_empty());
        assert_eq!(cache.len(), 0);
        assert_eq!(cache.current_bytes(), 0);
    }

    #[test]
    fn get_or_insert_caches_on_miss() {
        let mut cache = LineWrapCache::with_byte_budget(ROOMY);
        let mut compute_calls = 0;
        let v = cache.get_or_insert_with(key(100, 1), || {
            compute_calls += 1;
            dummy_lines(7)
        });
        assert_eq!(v.len(), 7);
        assert_eq!(compute_calls, 1);
        assert_eq!(cache.len(), 1);
    }

    #[test]
    fn repeat_lookup_is_a_hit() {
        let mut cache = LineWrapCache::with_byte_budget(ROOMY);
        let mut compute_calls = 0;
        cache.get_or_insert_with(key(100, 1), || {
            compute_calls += 1;
            dummy_lines(7)
        });
        let v = cache.get_or_insert_with(key(100, 1), || {
            compute_calls += 1;
            dummy_lines(99) // wrong value, should not be invoked
        });
        assert_eq!(v.len(), 7);
        assert_eq!(compute_calls, 1, "second lookup should be a hit");
    }

    #[test]
    fn different_versions_are_separate_entries() {
        let mut cache = LineWrapCache::with_byte_budget(ROOMY);
        cache.get_or_insert_with(key(100, 1), || dummy_lines(3));
        cache.get_or_insert_with(key(100, 2), || dummy_lines(5));
        assert_eq!(cache.get(&key(100, 1)).map(|v| v.len()), Some(3));
        assert_eq!(cache.get(&key(100, 2)).map(|v| v.len()), Some(5));
        assert_eq!(cache.len(), 2);
    }

    #[test]
    fn evicts_oldest_when_byte_budget_reached() {
        let mut cache = LineWrapCache::with_byte_budget(TIGHT);
        cache.get_or_insert_with(key(100, 1), || dummy_lines(1));
        cache.get_or_insert_with(key(200, 1), || dummy_lines(1));
        cache.get_or_insert_with(key(300, 1), || dummy_lines(1));
        // Adding a fourth tiny entry should evict at least the oldest
        // (line_start=100) to stay within the budget.
        cache.get_or_insert_with(key(400, 1), || dummy_lines(1));
        assert!(cache.current_bytes() <= TIGHT);
        assert_eq!(cache.get(&key(100, 1)).is_none(), true, "oldest evicted");
        // Later entries still reachable.
        assert!(cache.get(&key(400, 1)).is_some());
    }

    #[test]
    fn structural_invariant_holds_under_many_inserts() {
        let mut cache = LineWrapCache::with_byte_budget(TIGHT);
        for i in 0..200u64 {
            cache.get_or_insert_with(key(i as usize, i), || dummy_lines(1));
            assert_eq!(cache.len(), cache.map.len());
            assert_eq!(cache.len(), cache.order.len());
            assert_eq!(cache.current_bytes <= cache.byte_budget, true);
        }
    }

    #[test]
    fn put_overwrites_existing_value_without_reordering() {
        let mut cache = LineWrapCache::with_byte_budget(ROOMY);
        cache.get_or_insert_with(key(100, 1), || dummy_lines(1));
        cache.get_or_insert_with(key(200, 1), || dummy_lines(1));
        cache.get_or_insert_with(key(300, 1), || dummy_lines(1));
        // Overwrite middle with a different-sized value.
        cache.put(key(200, 1), Arc::new(dummy_lines(42)));
        assert_eq!(cache.get(&key(200, 1)).map(|v| v.len()), Some(42));
        // key=100 is still the oldest in the FIFO.
        cache.get_or_insert_with(key(400, 1), || dummy_lines(1));
        // With ROOMY budget nothing's evicted yet; all present.
        for k in [100usize, 200, 300, 400] {
            assert!(cache.get(&key(k, 1)).is_some(), "k={k} should be present");
        }
    }

    #[test]
    fn clear_empties_cache() {
        let mut cache = LineWrapCache::with_byte_budget(ROOMY);
        cache.get_or_insert_with(key(100, 1), || dummy_lines(1));
        cache.get_or_insert_with(key(200, 1), || dummy_lines(1));
        cache.clear();
        assert!(cache.is_empty());
        assert_eq!(cache.current_bytes(), 0);
        assert!(cache.get(&key(100, 1)).is_none());
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
    fn zero_byte_budget_rejected() {
        LineWrapCache::with_byte_budget(0);
    }

    /// Even if a single new entry's estimated size exceeds the budget,
    /// the cache accepts it rather than silently dropping data the
    /// caller just paid to compute.  Later inserts will still evict it
    /// like any other FIFO entry.
    #[test]
    fn oversize_entry_is_accepted_then_agable() {
        let mut cache = LineWrapCache::with_byte_budget(TIGHT);
        // dummy_lines(50) is ~7 KB per line × 50 = ~350 KB... no, empty
        // ViewLines are ~96 bytes each, so 50 × 96 ≈ 5 KB.  That
        // exceeds TIGHT (500 bytes).
        cache.get_or_insert_with(key(1, 1), || dummy_lines(50));
        assert!(cache.get(&key(1, 1)).is_some());
        // Inserting a second entry evicts the oversize one.
        cache.get_or_insert_with(key(2, 1), || dummy_lines(1));
        assert!(cache.get(&key(1, 1)).is_none());
        assert!(cache.get(&key(2, 1)).is_some());
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

        // Cache stores Vec<ViewLine>, so the shadow compares the LENGTH
        // (row count) the cache would expose with a fresh recompute.
        // The full-pipeline shadow (ViewLine coordinates agreeing with
        // the renderer) lives in e2e tests; this primitive-level shadow
        // checks that the FIFO / byte-budget machinery doesn't corrupt
        // stored values across inserts and evictions.
        //
        // Real cache values are built from `dummy_lines(shadow_count)`
        // so the cache value's length equals the shadow row count.
        let mut real = LineWrapCache::with_byte_budget(TIGHT);
        for step in 0..400usize {
            let t_idx = (step * 37 + 11) % texts.len();
            let w_idx = (step * 5 + 3) % widths.len();
            let text = &texts[t_idx];
            let width = widths[w_idx];

            let shadow_rows = count_visual_rows_for_text(text, width, 2, false);

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
            let real_val = real.get_or_insert_with(key, || dummy_lines(shadow_rows));
            assert_eq!(
                real_val.len() as u32,
                shadow_rows,
                "shadow disagreement at step {step}: text_idx={t_idx}, width={width}, \
                 real={}, shadow={shadow_rows}",
                real_val.len(),
            );
            assert!(
                real.current_bytes() <= real.byte_budget(),
                "cache exceeded byte budget"
            );
        }
    }

    /// Version-bump invalidation: entries stored under version V are
    /// NEVER returned when a lookup is built at version V+1.  The
    /// old entry sits in memory until FIFO evicts it, but no caller
    /// should ever get the stale value.
    #[test]
    fn version_bump_makes_old_entry_unreachable() {
        let mut cache = LineWrapCache::with_byte_budget(ROOMY);
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
        cache.get_or_insert_with(key_v0, || dummy_lines(5));
        assert_eq!(cache.get(&key_v0).map(|v| v.len()), Some(5));

        let key_v1 = LineWrapKey {
            pipeline_inputs_version: 101,
            ..key_v0
        };
        assert!(
            cache.get(&key_v1).is_none(),
            "v1 lookup must miss even though v0 entry is still present"
        );

        // Miss path stores under v1; v0 remains in the map, untouched.
        let mut miss_called = 0;
        let v = cache.get_or_insert_with(key_v1, || {
            miss_called += 1;
            dummy_lines(7)
        });
        assert_eq!(v.len(), 7);
        assert_eq!(miss_called, 1);
        assert_eq!(cache.get(&key_v1).map(|v| v.len()), Some(7));
        assert_eq!(
            cache.get(&key_v0).map(|v| v.len()),
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

        let mut cache = LineWrapCache::with_byte_budget(ROOMY);
        cache.get_or_insert_with(base, || dummy_lines(1));
        for (i, v) in variations.iter().enumerate() {
            assert_ne!(*v, base, "variation {i} shouldn't equal base");
            assert!(
                cache.get(v).is_none(),
                "variation {i} unexpectedly hit base entry"
            );
            cache.get_or_insert_with(*v, || dummy_lines(2 + i as u32));
        }
        // Base entry is still reachable.
        assert_eq!(cache.get(&base).map(|v| v.len()), Some(1));
        // Each variation stored its own value (distinguished by length).
        for (i, v) in variations.iter().enumerate() {
            assert_eq!(cache.get(v).map(|v| v.len()), Some(2 + i));
        }
    }
}
