# Line-Wrap Cache — Plan

## Status note

This plan was initially implemented as a *row-count* cache: the value type was
`u32` and the remaining `wrap_line` callers (cursor navigation, scrollbar
thumb sizing) stayed on the old char-wrap algorithm.  That partial
implementation is what landed in commits `a44c82b` through `f0fac48` on
`claude/fix-scroll-wrapped-lines-Cfgmx`.

This revision redefines the cache to hold the **full pipeline output**
(`Vec<ViewLine>` per logical line) so that every caller that today reaches for
`wrap_line` can be migrated to consume cached layout from a single source.  The
user-reported bugs were already fixed by the partial implementation; this
revision is about genuinely delivering "one source of truth" instead of "one
row count".

## Problem recap

The editor had two independent wrap implementations drifting by 0–1 rows on
real text:

| Used by | Function | Algorithm |
|---|---|---|
| Renderer | `apply_wrapping_transform` | word-boundary aware (UAX #29, 16-col lookback, grapheme-split fallback) |
| Scroll math / cursor nav / scrollbar thumb | `wrap_line` | pure char-width hard wrap |

Plus a gutter-width drift (`scrollbar_math` had a hardcoded digit floor of 4;
`Viewport::gutter_width` used `MIN_LINE_NUMBER_DIGITS = 2`).

User-visible consequences:

1. **Over-scroll into empty viewport.** `scroll_down_visual`'s within-line
   fast path advanced `top_view_line_offset` without re-clamping.  Already
   fixed by a re-clamp call.
2. **Under-scroll — last line never reachable.** Real word-wrapped text
   wrapped into more rows than `wrap_line` counted, so `max_scroll_row` was
   too small.

Both covered by sweep tests in `scroll_wrapped_reach_last_line.rs`.

A third consequence became apparent later:

3. **Cursor navigation and scrollbar-thumb sizing** still use `wrap_line`.
   The thumb's size and the cursor's visual position can disagree with the
   rendered content by a row on word-wrapped buffers.

## The real design

Cache the output of the render pipeline at the per-logical-line granularity:

```
    raw line bytes
          │
          ▼
  build_base_tokens        ← viewport-scoped: takes (start_byte, line_count)
          │
          ▼
    apply_soft_breaks      ← reads SoftBreakManager for the range
          │
          ▼
   apply_conceal_ranges    ← reads ConcealManager for the range (Compose mode)
          │
          ▼
 apply_wrapping_transform  ← uses effective_width, gutter_width, hanging_indent
          │
          ▼
    ViewLineIterator       ← wraps tokens into visual rows
          │
          ▼
   Arc<Vec<ViewLine>>      ← CACHE VALUE: one entry per logical line
```

**One entry = one logical line's worth of `ViewLine`s.**  Row count is
`cached.len()`.  Every byte ↔ visual-col / visual-row ↔ byte mapping comes
straight off the cached `ViewLine`s via the methods they already expose
(`source_byte_at_char`, `char_at_visual_col`, `source_byte_at_visual_col`,
`visual_col_at_char`, `visual_width`).

## Cache shape

```rust
struct LineWrapKey {
    pipeline_inputs_version: u64,  // buffer + soft-breaks + conceal versions
    view_mode: CacheViewMode,
    line_start: usize,
    effective_width: u32,
    gutter_width: u16,
    wrap_column: Option<u32>,
    hanging_indent: bool,
    line_wrap_enabled: bool,
}

struct LineWrapCache {
    map: HashMap<LineWrapKey, Arc<Vec<ViewLine>>>,
    order: VecDeque<LineWrapKey>,
    byte_budget: usize,        // total bytes of cached ViewLine data
    current_bytes: usize,      // running total
}
```

Entries cloned via `Arc` — no deep copies when scroll math asks for a line
that's already in cache.

### Eviction

Because `Vec<ViewLine>` sizes vary wildly (a 10-char line's entry is a few
hundred bytes; a 200 KB line wrapping to 2000 rows is on the order of 2 MB),
**count-based** eviction is the wrong metric.  Use a **byte budget**:

- `byte_budget` default: 8 MiB.  Enough to hold the full layout for a small-
  to-medium buffer, a handful of huge lines, or any interactive span.
- `current_bytes` tracks approximate accumulated entry size (summed
  `visual_width() * per_row_overhead` plus per-line constant).
- When inserting an entry that pushes `current_bytes` past the budget, evict
  from the FIFO front until the new entry fits.

### Writers

1. **Renderer writeback** (`view_data::build_view_data`).  After the
   pipeline runs on the visible window, slice the resulting tokens /
   ViewLines by source line and store each logical line's ViewLines under
   its key.  Skipped when a plugin `view_transform` is active (the tokens
   aren't reproducible from raw line text via the mini-pipeline).

2. **Miss handler** (in `line_wrap_cache`).  Run the same 4-step pipeline
   scoped to exactly one logical line and collect `ViewLine`s.  Invoked
   by scroll math / cursor nav when they query a line the renderer
   hasn't visited yet.

Both paths run the same pipeline; values agree by construction.

### Readers / migrated callers

Every `wrap_line` consumer outside tests:

| Caller | Today | After |
|---|---|---|
| `Viewport::count_visual_rows_for_line` | wrap → `.len()` | `cache.get_or_compute(key).len()` |
| `Viewport::ensure_visible` (5 sites) | wrap → inspect `WrappedSegment.start_char_offset` | walk cached `ViewLine`s; use `source_byte_at_char`, `char_at_visual_col` |
| `Viewport::cursor_screen_position` | wrap → compute cursor col | `visual_col_at_char` / `source_byte_at_char` |
| `scrollbar::scrollbar_visual_row_counts` (2 sites) | wrap every line, sum | iterate buffer lines, sum `cache.get_or_compute(key).len()` |
| `scrollbar_math::build_visual_row_map` | already cached but value is `u32` | now value is `Arc<Vec<ViewLine>>`, length is the same row count |
| `view_data::build_view_data` (the renderer itself) | always re-runs pipeline for visible window | may consult cache per logical line; run pipeline only on miss |

Last row is the architectural win: the renderer becomes a cache *consumer*
as well as a *producer*.  Unchanged lines don't need to be re-wrapped per
frame.

## Invalidation

Unchanged from the row-count plan.  The combined `pipeline_inputs_version`
(u64 derived from `buffer.version()` + `soft_breaks.version()` +
`conceal.version()`) makes stale entries unreachable after any pipeline-
input mutation.  Width / gutter / hanging-indent / view-mode changes flip
those key dimensions directly.  Entries age out via FIFO when the byte
budget is exceeded.

## What goes away

- `primitives::line_wrapping::wrap_line` — no non-test callers.
- `WrappedSegment` — replaced by direct `ViewLine` reads.
- `compute_wrap_row_count_for_text` (added in the partial implementation) —
  subsumed by cache miss handler returning `Vec<ViewLine>`.
- `Viewport::wrap_row_cache` (the small ad-hoc per-viewport cache) —
  subsumed by the EditorState-level cache once readers are migrated.

## Huge-file behavior

Same as before — files over the `large_file_threshold_bytes` (1 MB) bypass
all the wrap math via byte-based scroll.  The cache is never consulted.
Mouse wheel on huge files only wraps the lines actually scrolled through.

## Plugin `view_transform`

Still bypassed.  Plugin tokens aren't reproducible from raw line text in
the miss handler.  Renderer writes are also skipped under
`view_transform.is_some()`, and scroll math for view-transform buffers
goes through `ViewLineIterator` on the plugin's own tokens (the existing
`scroll_view_lines` path).

## Testing strategy

### Layer 1 — cache primitive

Unit tests in `line_wrap_cache.rs`:

- FIFO invariants: `map.len() == order.len()`; capacity / byte-budget
  respected across interleaved insert / clear calls.
- Distinct-key separation: varying any key dimension produces a miss.
- Version bumping: old-version entries unreachable after version change.
- Re-query is a hit (compute closure not invoked).
- Eviction: oldest entry evicted when a new insert exceeds the budget.

### Layer 2 — mini-pipeline equivalence (ViewLine-level)

`count_visual_rows_via_pipeline` returns `Vec<ViewLine>` that agrees
with the renderer's per-line slice of its full-pipeline output.  Proptested
over random text, widths, soft breaks, conceals.

### Layer 3 — shadow-model property test

Random op stream (edits, soft-break / conceal ops, resizes, view-mode
toggles) driven against both the real cache and a no-cache recompute
reference.  Every query must agree.  Expanded from the row-count version
to compare `ViewLine::char_source_bytes`, `char_visual_cols`, etc. —
not just length.

### Layer 4 — wrap-function invariants

Width monotonicity, empty line = 1 row, prefix doesn't reduce row count,
etc.  Hold at the `Vec<ViewLine>`-length level.

### Layer 5 — render-vs-reader agreement

After the renderer runs, every cached entry's `Vec<ViewLine>` must match
a fresh mini-pipeline on the same line.  Covers the writeback invariant.

### Layer 6 — behavioral e2e

- Scroll sweeps (existing `scroll_wrapped_reach_last_line.rs`).
- Single-long-line perf test (`scroll_single_long_line_perf.rs`).
- Mid-scroll resize, mid-drag edit (existing
  `line_wrap_cache_consistency.rs`).
- **New**: cursor-position / click-target tests.  With a word-wrapped
  buffer, the visual row reported by `cursor_screen_position` must match
  the row the renderer actually drew the cursor on.
- **New**: scrollbar thumb size consistency.  For a buffer with `N`
  visual rows (per the renderer), the thumb's reported `total_rows`
  equals `N`.

### Layer 7 — stress / fuzz

Optional.  Random op streams for N minutes, assert invariants throughout.

## Implementation order

Phases, each landing as a self-contained commit:

1. **Plan doc (this revision).**
2. **Change cache value type** from `u32` to `Arc<Vec<ViewLine>>`; update
   the primitive + its unit tests; update the miss handler to return
   ViewLines; update existing row-count consumers to use `.len()`.
3. **Renderer writeback update**: write `Arc<Vec<ViewLine>>` entries
   instead of row counts.
4. **Migrate `ensure_visible`** — the big one.  Replace `wrap_line` +
   `WrappedSegment` math with cached-ViewLine reads using the existing
   `ViewLine` methods.
5. **Migrate `cursor_screen_position` and `scrollbar_visual_row_counts`.**
6. **Drop `wrap_line` from non-test code.**  Confirm only test callers
   remain.
7. **Render-consumer path** (optional, follow-up): make
   `build_view_data` consult the cache for each logical line instead of
   always re-running the pipeline.  First render fills; subsequent
   unchanged frames skip the pipeline for unchanged lines.
8. **New tests** (Layer 6 additions): cursor / click-target parity,
   scrollbar-thumb consistency.

## Deeper issue not addressed

`apply_wrapping_transform` has an O(n²) hot path on long single tokens
(per-chunk `split_word_bound_indices` scanning from byte 0).  The cache
hides this after the first hit, but the first hit on a 200 KB line is
still ~600 ms in debug.  A proper fix makes `apply_wrapping_transform`
linear by walking word-boundary indices once as a monotonic cursor.
Out of scope for this branch; tracked separately.

## Partial-implementation rollback

If the ViewLine migration proves intractable mid-refactor, rolling back
to the row-count implementation shipped in commits `197ea4e`–`f0fac48`
is straightforward — the public API of `LineWrapCache` stays the same
shape, only the value type changes.  All the correctness fixes
(reclamp, gutter unification, `apply_wrapping_transform` swap) are
independent and stay in place either way.
