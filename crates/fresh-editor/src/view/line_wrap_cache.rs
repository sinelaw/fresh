//! Wrap-layout keys, plain-text layout helpers and row counting.
//!
//! * [`LineWrapKey`] names a logical line's layout under a given geometry
//!   and pipeline-input version ([`PipelineInputs`]); any change produces a
//!   different key, so stale entries become unreachable without an explicit
//!   invalidation step.
//! * [`RowCountCache`] memoizes "how many visual rows does this line take?"
//!   by that key for the viewport's scroll hot paths.
//! * `layout_for_plain_text*`, `count_visual_rows_for_text*` and
//!   `byte_position_in_layout` lay out or measure a line from its text alone.
//!
//! The full per-line layout the renderer produces is read from the wrap
//! index (`wrap_index`), not cached here.

use crate::view::ui::view_pipeline::{ViewLine, ViewLineIterator};
use fresh_core::api::ViewTokenWireKind;
use std::collections::{HashMap, VecDeque};

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
/// `pipeline_inputs_version` is the buffer version, and that is
/// sufficient: the only live consumer is [`RowCountCache`], whose value
/// is a pure function of the line's raw text and the geometry fields
/// below.  Decorations never enter it — `Viewport`'s row counting adds
/// virtual rows *outside* the cache and returns before consulting it at
/// all when soft breaks apply — so a decoration version could not stale
/// an entry.  Layout that does model decorations goes through
/// [`WrapIndex`](crate::view::wrap_index::WrapIndex), which keys on the
/// full [`PipelineInputs`] instead.
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
    /// Terminal-grid wrap mode (fresh#2649): rows break at exact column
    /// boundaries (`effective_width` columns) with no word-boundary
    /// preference, no hanging indent, and no gutter — matching the live
    /// PTY grid so entering scroll-back doesn't reflow. Only terminal
    /// buffers set this; it keys separately from word-wrap entries at the
    /// same geometry.
    pub grid_wrap: bool,
    /// Signature of the cursor positions inside this line (see
    /// [`cursor_sig_for_line`]). Cursor-dependent conceal/soft-break
    /// activation makes the cursor line's layout a function of where the
    /// cursors sit within it; folding that into the key means a cursor
    /// move invalidates at most the two lines whose signature changed
    /// while every other entry stays valid. Cursor-blind consumers
    /// (`WrapIndex`, scroll math) always use `0` — the canonical
    /// "no cursor anywhere" layout.
    pub cursor_sig: u64,
}

/// The versions of everything that feeds line layout, kept apart.
///
/// This replaces a packed-XOR `u64`: equality still answers "is anything
/// stale", but the components stay visible, and *which* one moved is the
/// load-bearing distinction — a buffer edit is repaired locally by
/// [`WrapIndex::damage_bytes`](crate::view::wrap_index::WrapIndex::damage_bytes),
/// while a decoration change is repaired by diffing the old decoration
/// snapshot against the new one. A packed integer could express neither
/// without unpacking tricks, and its collision story ("astronomically
/// unlikely") is replaced by plain field equality.
///
/// `virtual_text` is folded in so that adding / removing plugin virtual
/// lines (e.g. markdown_compose's table borders, git blame headers)
/// invalidates the same consumers the other sources do — `WrapIndex` adds
/// virtual line counts to its prefix sums and would otherwise serve a
/// stale total when the plugin re-tiles a table.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct PipelineInputs {
    pub buffer: u64,
    pub soft_breaks: u32,
    pub conceals: u32,
    pub virtual_text: u32,
}

impl PipelineInputs {
    /// Do the decoration components (everything except the buffer text)
    /// match? The buffer half has its own repair channel, so this is the
    /// question `ensure_built` asks to pick diff-repair over rebuild.
    #[cfg(test)]
    pub fn decorations_match(&self, other: &PipelineInputs) -> bool {
        self.soft_breaks == other.soft_breaks
            && self.conceals == other.conceals
            && self.virtual_text == other.virtual_text
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
    use crate::view::wrap_machine::WrapRule;
    layout_for_plain_text_under(
        line_text,
        WrapRule::Word {
            content_width: effective_width,
            gutter_width,
            hanging_indent,
        },
        tab_size,
    )
}

/// Materialise a plain line's layout under any wrap rule: run the machine,
/// then turn its token stream into `ViewLine`s.  The single body behind
/// [`layout_for_plain_text`] and [`layout_for_plain_text_grid`] — the two
/// differ only in which rule they hand the machine.
fn layout_for_plain_text_under(
    line_text: &str,
    rule: crate::view::wrap_machine::WrapRule,
    tab_size: usize,
) -> Vec<ViewLine> {
    use crate::view::ui::view_pipeline::LineStart;
    use crate::view::wrap_machine::WrapMachine;
    use fresh_core::api::ViewTokenWire;
    let tokens = vec![ViewTokenWire {
        source_offset: Some(0),
        kind: ViewTokenWireKind::Text(line_text.to_string()),
        style: None,
    }];
    let wrapped = WrapMachine::run(tokens, rule).tokens;
    let mut lines: Vec<ViewLine> =
        ViewLineIterator::new(&wrapped, false, true, tab_size, false).collect();
    // Invariant: every logical line is at least one visual row.  An
    // empty input produces zero ViewLines through the iterator; emit
    // one placeholder so callers (scrollbar row counts, scroll math)
    // see consistent ≥1 results.
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
            virtual_gutter_glyph: None,
            virtual_line_style: None,
        });
    }
    lines
}

/// The visual row of `byte_in_line` within a laid-out logical line, and the
/// visual column it sits at.
///
/// The byte-oriented counterpart of [`char_position_in_layout`], which walks
/// *drawn* characters: a wrap breaking on a space consumes it, so that count
/// falls one behind the source per wrapped row. Callers pass a byte offset,
/// which is a character index only for single-byte text that never wraps on
/// whitespace — deep in one enormous line the drift is the difference between
/// the viewport agreeing with what was drawn and not (issue #1806). Rows carry
/// real byte offsets, so ask them.
///
/// If `layout` is empty, returns `(0, 0)`. A byte past the end of the last row
/// returns that row and the visual column of its last *source* character —
/// which is not the same as [`char_position_in_layout`]'s last visual column
/// when the row ends in injected content or a wide glyph.
pub fn byte_position_in_layout(layout: &[ViewLine], byte_in_line: usize) -> (usize, usize) {
    if layout.is_empty() {
        return (0, 0);
    }
    // Rows ascend, so it is the last one starting at or before the byte. A row
    // drawing no source of its own leaves the answer with the row above.
    let mut row_idx = 0;
    for (i, row) in layout.iter().enumerate() {
        match row.char_source_bytes.iter().find_map(|b| *b) {
            Some(first) if first <= byte_in_line => row_idx = i,
            Some(_) => break,
            None => {}
        }
    }
    let row = &layout[row_idx];
    let mut col = 0;
    for (char_idx, source) in row.char_source_bytes.iter().enumerate() {
        match source {
            Some(b) if *b <= byte_in_line => col = row.visual_col_at_char(char_idx),
            Some(_) => break,
            None => {}
        }
    }
    (row_idx, col)
}

/// Row counts keyed by [`LineWrapKey`], for the consumers that only ever
/// ask "how many rows?" — the viewport's scroll hot paths.
///
/// The value is a plain `u32`: storing counts as `Vec<ViewLine>` of that
/// length, as this memo used to,
/// allocated thousands of empty `ViewLine`s per miss on a long line and
/// needed a byte-budget evictor to keep them in check. Counts are uniform,
/// so a plain entry cap is the right bound.
#[derive(Debug, Clone)]
pub struct RowCountCache {
    map: HashMap<LineWrapKey, u32>,
    order: VecDeque<LineWrapKey>,
    capacity: usize,
}

impl RowCountCache {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            map: HashMap::new(),
            order: VecDeque::new(),
            capacity: capacity.max(1),
        }
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    /// Read the cached count, or compute and store one. FIFO eviction once
    /// `capacity` entries are held.
    pub fn get_or_insert_with<F: FnOnce() -> u32>(&mut self, key: LineWrapKey, compute: F) -> u32 {
        if let Some(&n) = self.map.get(&key) {
            return n;
        }
        let n = compute();
        while self.map.len() >= self.capacity {
            match self.order.pop_front() {
                Some(oldest) => {
                    self.map.remove(&oldest);
                }
                None => break,
            }
        }
        self.order.push_back(key);
        self.map.insert(key, n);
        n
    }

    pub fn clear(&mut self) {
        self.map.clear();
        self.order.clear();
    }
}

/// Count visual rows for a single line's text after applying the
/// plugin's soft breaks AND the renderer's word-wrap.  Mirrors the
/// renderer's full pipeline (`apply_soft_breaks` → `apply_wrapping_transform`)
/// so the scroll math agrees row-for-row with the rendered output even
/// when the plugin has injected breaks at narrower-than-viewport
/// widths (e.g. markdown_compose's per-paragraph wrap).
///
/// `soft_breaks_in_line` is the slice of `(byte_position, indent)` pairs
/// for breaks falling **inside** `[line_start, line_start + line_text.len())`.
/// Callers should pre-filter from the buffer-wide list.
///
/// When `soft_breaks_in_line` is empty this is a thin wrapper over
/// [`count_visual_rows_for_text`].
pub fn count_visual_rows_for_text_with_soft_breaks(
    line_text: &str,
    line_start: usize,
    soft_breaks_in_line: &[(usize, u16)],
    effective_width: usize,
    gutter_width: usize,
    hanging_indent: bool,
) -> u32 {
    if soft_breaks_in_line.is_empty() {
        return count_visual_rows_for_text(
            line_text,
            effective_width,
            gutter_width,
            hanging_indent,
        );
    }

    let mut total: u32 = 0;
    let mut prev_end: usize = 0; // byte offset within `line_text`
    let mut prev_indent: u16 = 0;

    for &(pos, indent) in soft_breaks_in_line {
        // Defensive: callers pre-filter, but ignore anything out of
        // range so a stale break list can't OOB-slice the line.
        if pos < line_start {
            continue;
        }
        let rel = pos - line_start;
        if rel >= line_text.len() {
            continue;
        }
        if rel < prev_end {
            // Break list is sorted; this would only fire on a
            // duplicate or a not-byte-aligned offset.  Skip rather
            // than panic.
            continue;
        }
        if !line_text.is_char_boundary(rel) {
            // Stale break list: an edit earlier in the line shifted
            // the text under positions computed against the old
            // content, so the offset can land mid-char.
            continue;
        }
        let segment = &line_text[prev_end..rel];
        total = total.saturating_add(count_segment_rows_with_indent(
            segment,
            prev_indent,
            effective_width,
            gutter_width,
            hanging_indent,
        ));
        // The renderer's `apply_soft_breaks` consumes the Space token
        // *at* the break position when one is present (see
        // transforms.rs::apply_soft_breaks).  Skip exactly one
        // character at `rel` to mirror that — UTF-8 safe.
        let consumed = line_text[rel..]
            .chars()
            .next()
            .map(|c| c.len_utf8())
            .unwrap_or(0);
        prev_end = (rel + consumed).min(line_text.len());
        prev_indent = indent;
    }
    let segment = &line_text[prev_end..];
    total = total.saturating_add(count_segment_rows_with_indent(
        segment,
        prev_indent,
        effective_width,
        gutter_width,
        hanging_indent,
    ));
    total.max(1)
}

/// Helper for [`count_visual_rows_for_text_with_soft_breaks`]:
/// row count for one inter-break segment with `leading_indent`
/// columns reserved at the front.  An empty segment still occupies
/// one visual row (matches the renderer, which emits a trailing
/// `Break` for the broken position).
fn count_segment_rows_with_indent(
    segment: &str,
    leading_indent: u16,
    effective_width: usize,
    gutter_width: usize,
    hanging_indent: bool,
) -> u32 {
    if segment.is_empty() && leading_indent == 0 {
        return 1;
    }
    if leading_indent == 0 {
        return count_visual_rows_for_text(segment, effective_width, gutter_width, hanging_indent);
    }
    // Prepend the indent columns; this lets the renderer's word-wrap
    // see the same `current_line_width` it would after
    // `apply_soft_breaks` injected indent Spaces.
    let mut prefixed = String::with_capacity(leading_indent as usize + segment.len());
    for _ in 0..leading_indent {
        prefixed.push(' ');
    }
    prefixed.push_str(segment);
    count_visual_rows_for_text(&prefixed, effective_width, gutter_width, hanging_indent)
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
    use crate::view::wrap_machine::{WrapMachine, WrapRule};
    use fresh_core::api::ViewTokenWire;

    let tokens = vec![ViewTokenWire {
        source_offset: Some(0),
        kind: ViewTokenWireKind::Text(line_text.to_string()),
        style: None,
    }];
    let out = WrapMachine::run(
        tokens,
        WrapRule::Word {
            content_width: effective_width,
            gutter_width,
            hanging_indent,
        },
    );
    (out.rows.len() as u32).max(1)
}

/// Row count under the terminal-grid rule (fresh#2649).
///
/// Drives the same machine as the renderer, so the count and the drawn rows
/// cannot disagree — the divergence that made scroll-back stick.
pub fn count_visual_rows_for_text_grid(line_text: &str, cols: usize) -> u32 {
    use crate::view::wrap_machine::{WrapMachine, WrapRule};
    use fresh_core::api::ViewTokenWire;

    if cols == 0 {
        return 1;
    }
    let tokens = vec![ViewTokenWire {
        source_offset: Some(0),
        kind: ViewTokenWireKind::Text(line_text.to_string()),
        style: None,
    }];
    let out = WrapMachine::run(tokens, WrapRule::Grid { cols });
    (out.rows.len() as u32).max(1)
}

/// Absolute source byte of each grid-wrap visual row of `line_text` at `cols`
/// columns, where `line_start` is the line's byte offset.
///
/// Grid mode's answer to "which byte is the row at the top of the viewport?".
/// Word-wrap now answers that by walking rows from a byte
/// ([`crate::view::row_walk`]) rather than by laying out the whole line.
///
/// Drives the same machine as the renderer, so the byte mapping and the drawn
/// rows cannot disagree (fresh#2649).
#[cfg(test)]
pub fn grid_segment_source_bytes(line_text: &str, line_start: usize, cols: usize) -> Vec<usize> {
    use crate::view::wrap_machine::{WrapMachine, WrapRule};
    use fresh_core::api::ViewTokenWire;

    if cols == 0 {
        return vec![line_start];
    }
    let tokens = vec![ViewTokenWire {
        source_offset: Some(line_start),
        kind: ViewTokenWireKind::Text(line_text.to_string()),
        style: None,
    }];
    let out = WrapMachine::run(tokens, WrapRule::Grid { cols });
    let mut rows: Vec<usize> = out
        .rows
        .iter()
        .map(|r| r.source_byte.unwrap_or(line_start))
        .collect();
    if rows.is_empty() {
        rows.push(line_start);
    }
    rows
}

/// Grid-mode counterpart of [`layout_for_plain_text`]: materialise a line's
/// layout as `Vec<ViewLine>` under terminal-grid wrapping at `cols` columns.
/// Matches the renderer's `apply_grid_wrapping_transform` output for the
/// same text, so row counts and cursor mappings agree with what is drawn.
pub fn layout_for_plain_text_grid(line_text: &str, cols: usize, tab_size: usize) -> Vec<ViewLine> {
    use crate::view::wrap_machine::WrapRule;
    layout_for_plain_text_under(line_text, WrapRule::Grid { cols }, tab_size)
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
            grid_wrap: false,
            cursor_sig: 0,
        }
    }

    #[test]
    fn row_count_cache_serves_hits_and_evicts_oldest_first() {
        let mut cache = RowCountCache::with_capacity(2);
        let mut misses = 0;
        let count = |cache: &mut RowCountCache, line: usize, n: u32, misses: &mut usize| {
            cache.get_or_insert_with(key(line, 1), || {
                *misses += 1;
                n
            })
        };

        assert_eq!(count(&mut cache, 0, 3, &mut misses), 3);
        assert_eq!(count(&mut cache, 10, 5, &mut misses), 5);
        assert_eq!(misses, 2);

        // A repeat is served from the map, not recomputed.
        assert_eq!(count(&mut cache, 0, 999, &mut misses), 3);
        assert_eq!(misses, 2);

        // The third distinct key evicts the oldest (line 0), not line 10.
        assert_eq!(count(&mut cache, 20, 7, &mut misses), 7);
        assert_eq!(cache.len(), 2);
        assert_eq!(count(&mut cache, 10, 999, &mut misses), 5);
        assert_eq!(misses, 3);
        assert_eq!(count(&mut cache, 0, 42, &mut misses), 42);
        assert_eq!(misses, 4);

        cache.clear();
        assert!(cache.is_empty());
    }

    /// A different `pipeline_inputs_version` is a different key, so an edit
    /// makes stale counts unreachable rather than needing invalidation.
    #[test]
    fn row_count_cache_is_keyed_on_the_pipeline_version() {
        let mut cache = RowCountCache::with_capacity(8);
        assert_eq!(cache.get_or_insert_with(key(0, 1), || 3), 3);
        assert_eq!(cache.get_or_insert_with(key(0, 2), || 9), 9);
        assert_eq!(cache.get_or_insert_with(key(0, 1), || 0), 3);
    }

    #[test]
    fn pipeline_inputs_distinguishes_the_buffer_from_the_decorations() {
        let a = PipelineInputs {
            buffer: 100,
            soft_breaks: 5,
            conceals: 3,
            virtual_text: 7,
        };
        assert_ne!(a, PipelineInputs { buffer: 101, ..a });
        assert!(
            a.decorations_match(&PipelineInputs { buffer: 101, ..a }),
            "a buffer edit alone leaves the decoration half current"
        );
        for changed in [
            PipelineInputs {
                soft_breaks: 6,
                ..a
            },
            PipelineInputs { conceals: 4, ..a },
            PipelineInputs {
                virtual_text: 8,
                ..a
            },
        ] {
            assert_ne!(a, changed);
            assert!(!a.decorations_match(&changed));
        }
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
    // Terminal-grid wrap (fresh#2649).
    // -------------------------------------------------------------------

    /// Grid counting basics: exact-column breaks, no word-boundary
    /// preference, ANSI escapes zero-width, wide chars two cells.
    #[test]
    fn grid_count_basics() {
        // Empty line is one row at any width.
        assert_eq!(count_visual_rows_for_text_grid("", 20), 1);
        // Exact fit stays one row; one more char wraps.
        assert_eq!(count_visual_rows_for_text_grid(&"a".repeat(20), 20), 1);
        assert_eq!(count_visual_rows_for_text_grid(&"a".repeat(21), 20), 2);
        assert_eq!(count_visual_rows_for_text_grid(&"a".repeat(40), 20), 2);
        assert_eq!(count_visual_rows_for_text_grid(&"a".repeat(41), 20), 3);
        // Words are split mid-word at the exact column — 6 words of 5
        // chars minus the trailing space = 29 visible cells at width 10.
        assert_eq!(
            count_visual_rows_for_text_grid("word1 word2 word3 word4 word5", 10),
            3
        );
        // SGR color codes are zero width.
        let colored = format!("\x1b[31m{}\x1b[0m", "x".repeat(20));
        assert_eq!(count_visual_rows_for_text_grid(&colored, 20), 1);
        // Wide chars take two cells: 10 CJK chars = 20 cells.
        let cjk = "\u{4e16}".repeat(10);
        assert_eq!(count_visual_rows_for_text_grid(&cjk, 20), 1);
        assert_eq!(count_visual_rows_for_text_grid(&cjk, 19), 2);
    }

    /// A wide char that would straddle the boundary moves to the next row
    /// whole (like the grid's early wrap) — the count follows the same
    /// walk, not a `ceil(total/cols)` shortcut.
    #[test]
    fn grid_count_wide_char_straddles_boundary() {
        // 19 narrow cells then a wide char at width 20: the wide char
        // doesn't fit in the single remaining cell → next row.
        let text = format!("{}\u{4e16}", "a".repeat(19));
        assert_eq!(count_visual_rows_for_text_grid(&text, 20), 2);
    }

    /// The transform-backed layout (what the renderer draws), the
    /// allocation-free count (what scroll math uses), and the segment
    /// byte map (what PageUp/Down use) must agree row-for-row — the
    /// single-row-model invariant that keeps terminal scroll-back
    /// scrolling stable (fresh#2649 symptom 2).
    #[test]
    fn grid_layout_count_and_segments_agree() {
        let texts: Vec<String> = vec![
            String::new(),
            "short".into(),
            "a".repeat(99),
            "a".repeat(100),
            "a".repeat(101),
            "word1 word2 word3 word4 word5 word6 word7 word8".into(),
            format!("\x1b[31mred{}\x1b[0m tail", "x".repeat(50)),
            format!("{}{}", "\u{4e16}".repeat(13), "mixed latin \u{e9}\u{5d0}"),
            format!("prompt$ {}", "argword ".repeat(30)),
        ];
        for text in &texts {
            for cols in [7usize, 10, 20, 33, 99] {
                let count = count_visual_rows_for_text_grid(text, cols) as usize;
                let layout = layout_for_plain_text_grid(text, cols, 4);
                assert_eq!(
                    layout.len(),
                    count,
                    "layout rows != counted rows for cols={cols} text={text:?}"
                );
                let segs = grid_segment_source_bytes(text, 0, cols);
                assert_eq!(
                    segs.len(),
                    count,
                    "segment starts != counted rows for cols={cols} text={text:?}"
                );
                // Each row's first source byte must match the segment map.
                for (i, row) in layout.iter().enumerate() {
                    if let Some(first) = row.char_source_bytes.iter().find_map(|b| *b) {
                        assert_eq!(
                            first, segs[i],
                            "row {i} first byte mismatch for cols={cols} text={text:?}"
                        );
                    }
                    // No visible row is wider than the grid... except a
                    // trailing newline cell which ViewLine carries.
                    let vis = row
                        .visual_width()
                        .saturating_sub(usize::from(row.ends_with_newline));
                    assert!(
                        vis <= cols,
                        "row {i} wider than grid ({vis} > {cols}) for text={text:?}"
                    );
                }
            }
        }
    }

    /// A soft-break offset that lands inside a multi-byte char must be
    /// skipped like the other malformed break positions, not panic the
    /// slice.  This happens when the break list is stale: the plugin
    /// recomputes breaks asynchronously, so an insert earlier in the
    /// line shifts the text under positions computed against the old
    /// content.
    #[test]
    fn stale_soft_break_inside_multibyte_char_does_not_panic() {
        // "decorative wave " is 16 bytes, so '—' occupies bytes
        // 16..19 of the line; a break at rel=17 is mid-char — exactly
        // what a one-byte-stale break list yields after inserting one
        // byte before the break.
        let text = "decorative wave \u{2014} a rising sea of glyphs";
        let line_start = 1043usize;
        let breaks = [(line_start + 17, 0u16)];
        let rows =
            count_visual_rows_for_text_with_soft_breaks(text, line_start, &breaks, 80, 6, false);
        assert!(rows >= 1);
    }

    /// Property test for `count_visual_rows_for_text_with_soft_breaks`:
    /// deterministic fuzz over multi-byte / grapheme-cluster texts,
    /// geometries, and adversarial break lists (mid-char, mid-cluster,
    /// out-of-range, unsorted, duplicates).  Deterministic LCG, so
    /// reproducible without a proptest dep.
    ///
    /// Properties:
    ///   1. never panics, result >= 1, and is deterministic;
    ///   2. rows are bounded above by chars + indents + breaks + 1
    ///      (each counted row contains at least one char);
    ///   3. with a *well-formed* break list (sorted, in-range, on char
    ///      boundaries), rows >= breaks + 1 — every segment occupies
    ///      at least one row.
    #[test]
    fn soft_break_row_count_properties() {
        let mut state: u64 = 0x9E37_79B9_7F4A_7C15;
        let mut next = move || {
            state = state
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            (state >> 33) as usize
        };
        // Building blocks: wrap-relevant ASCII, 1..4-byte scalars, and
        // multi-scalar grapheme clusters (combining marks, ZWJ
        // sequences, regional-indicator flags, variation selectors),
        // plus zero-width and RTL scalars.  Adjacent picks can also
        // merge into larger clusters (e.g. emoji + skin tone).
        let palette: &[&str] = &[
            "a",
            "b",
            "c",
            " ",
            " ",
            "\t",
            "-",
            "\u{e9}",                                      // é, 2-byte
            "\u{5d0}",                                     // א, RTL Hebrew
            "\u{2014}",                                    // —, 3-byte
            "\u{4e16}",                                    // 世, wide CJK
            "\u{1f680}",                                   // 🚀, 4-byte
            "e\u{301}",                                    // e + combining acute
            "\u{928}\u{93f}",                              // Devanagari नि
            "\u{1f468}\u{200d}\u{1f469}\u{200d}\u{1f467}", // ZWJ family
            "\u{1f1ee}\u{1f1f1}",                          // regional-indicator flag
            "\u{1f44d}\u{1f3fb}",                          // thumbs-up + skin tone
            "\u{2764}\u{fe0f}",                            // heart + VS16
            "\u{200d}",                                    // lone zero-width joiner
            "\u{200b}",                                    // zero-width space
        ];

        for _iter in 0..2000 {
            let n_pieces = next() % 60;
            let text: String = (0..n_pieces)
                .map(|_| palette[next() % palette.len()])
                .collect();
            let line_start = next() % 5000;
            let width = 2 + next() % 119;
            let gutter = next() % 11;
            let hanging = next() % 2 == 0;

            // Adversarial breaks: positions roam past both ends of the
            // line, indents are occasionally huge, order is unsorted.
            let n_breaks = next() % 8;
            let breaks: Vec<(usize, u16)> = (0..n_breaks)
                .map(|_| {
                    let pos = (line_start + next() % (text.len() + 10)).saturating_sub(5);
                    let indent = if next() % 10 == 0 {
                        500
                    } else {
                        (next() % 12) as u16
                    };
                    (pos, indent)
                })
                .collect();

            let rows = count_visual_rows_for_text_with_soft_breaks(
                &text, line_start, &breaks, width, gutter, hanging,
            );
            let again = count_visual_rows_for_text_with_soft_breaks(
                &text, line_start, &breaks, width, gutter, hanging,
            );
            assert_eq!(
                rows, again,
                "non-deterministic: text={text:?} breaks={breaks:?}"
            );
            assert!(rows >= 1, "zero rows: text={text:?} breaks={breaks:?}");
            let indent_sum: usize = breaks.iter().map(|&(_, i)| i as usize).sum();
            let bound = (text.chars().count() + indent_sum + breaks.len() + 1) as u32;
            assert!(
                rows <= bound,
                "rows={rows} > bound={bound}: text={text:?} breaks={breaks:?} \
                 width={width} gutter={gutter} hanging={hanging}",
            );

            // Well-formed list: distinct sorted char boundaries inside
            // the line.  Lower bound: each segment is >= 1 row.
            let mut good: Vec<(usize, u16)> = Vec::new();
            for (b, _) in text.char_indices() {
                if b > 0 && next() % 4 == 0 {
                    good.push((line_start + b, (next() % 8) as u16));
                }
            }
            good.sort_unstable();
            let rows = count_visual_rows_for_text_with_soft_breaks(
                &text, line_start, &good, width, gutter, hanging,
            );
            assert!(
                rows as usize > good.len(),
                "rows={rows} < segments={}: text={text:?} breaks={good:?} \
                 width={width} gutter={gutter} hanging={hanging}",
                good.len() + 1,
            );
        }
    }
}
