# Line-Wrap Cache — Plan

## Problem

The editor has two independent wrap implementations:

| Used by | Function | Algorithm | Effective width |
|---|---|---|---|
| Renderer | `split_rendering::transforms::apply_wrapping_transform` | word-boundary with 16-col lookback, falls back to grapheme split | `content_width - 1` (reserves a column so the EOL cursor never lands on the scrollbar) |
| Scroll math | `primitives::line_wrapping::wrap_line` | pure char-width hard wrap, no word boundaries | `content_width` (no EOL reservation) |

Plus two independent gutter estimates that had drifted — `Viewport::gutter_width` uses `MIN_LINE_NUMBER_DIGITS = 2` as the digit floor; `scrollbar_math::estimated_gutter_width` had `4` hardcoded (already patched to match).

Consequences observed as user-visible bugs:

1. **Over-scroll into empty viewport.** `Viewport::scroll_down_visual`'s within-line fast path advanced `top_view_line_offset` without re-clamping. Once offset pushed past the wrap count, the viewport rendered the tail segment at the top with `~` (past-EOF marker) rows below. Already fixed with a re-clamp call.
2. **Under-scroll — last line never visible.** For real word-wrapped text, `wrap_line` reports fewer visual rows than `apply_wrapping_transform`. Max-scroll is too small; mouse wheel, scrollbar drag, and PageDown all stop short of the real end. Only the Down-arrow cursor path (which re-checks visibility against the rendered view lines) can reach the end.

Both bugs are reproduced by sweep tests in `crates/fresh-editor/tests/e2e/scroll_wrapped_reach_last_line.rs` across multiple terminal widths and heights.

## Why a wrap-step-only cache was the wrong layer

A first sketch cached the output of `apply_wrapping_transform` keyed on wrap geometry only. That meant every other pipeline input (plugin soft breaks, conceal ranges, view mode) needed an "escape hatch" — a branch that bypassed the cache because the key didn't cover that dimension. Every escape hatch is a bug waiting to happen: if the caller forgets to check the bypass condition, the cache returns stale data.

The real layer to cache at is the **output of the whole render pipeline** — "for this logical line, under these plugin states and this geometry, how many visual rows does the pipeline produce?" Every pipeline input goes into the key, nothing bypasses.

## The render pipeline (what the cache must account for)

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
   apply_conceal_ranges    ← reads ConcealManager for the range (Compose mode only)
          │
          ▼
 apply_wrapping_transform  ← uses effective_width, gutter_width, hanging_indent
          │
          ▼
  count Break tokens between Newlines = visual row count per logical line
```

Every one of these steps affects the final row count. The cache key must cover every input any step reads.

## Approach: B4 (shared pipeline-output cache)

A per-buffer, bounded LRU cache keyed by the full set of pipeline inputs for a single logical line. The renderer writes to it as a side effect of its normal work; scroll math reads from it and fills missing entries on demand by running a **mini-pipeline** — the same four steps, scoped to just the one line being queried.

### Cache shape

The pipeline reads from three mutable sources: the buffer, `SoftBreakManager`, `ConcealManager`. Any of them changing could change the output. Rather than three separate version counters in the key, we derive a single `pipeline_inputs_version: u64` at query time:

```rust
fn pipeline_inputs_version(state: &EditorState) -> u64 {
    // Each manager exposes a monotonic u32; pack into a single u64.
    // (32 bits of buffer version is plenty for a single session.)
    let buf = state.buffer.version() as u32;
    let sb  = state.soft_breaks.version();
    let cn  = state.conceals.version();
    // Mix with a simple rotating XOR so any of the three changing bumps the combined value.
    (buf as u64) ^ ((sb as u64) << 21) ^ ((cn as u64) << 42)
}
```

Any mutation of any of the three sources flips the combined value, which makes the key change, which makes old entries unreachable. Per-manager counters stay private; the key carries one u64.

```rust
struct LineWrapKey {
    pipeline_inputs_version: u64,  // derived from (buffer.version, soft_breaks.version, conceals.version)
    view_mode: ViewMode,           // Compose vs Source — conceals/soft-breaks only apply in Compose
    line_start: usize,
    effective_width: u32,
    gutter_width: u16,
    wrap_column: Option<u32>,
    hanging_indent: bool,
    line_wrap_enabled: bool,
}

struct LineWrapCache {
    map: HashMap<LineWrapKey, u32>,   // row_count
    order: VecDeque<LineWrapKey>,     // FIFO eviction
    capacity: usize,                   // default 8192
}
```

- Lives on `EditorState`, sibling of `ScrollbarRowCache`.
- Cap: 8192 entries ≈ 700 KB worst case.
- **Invariant:** `map.len() == order.len() <= capacity` at all times.

### Cache write by the renderer

In `split_rendering::view_data::build_view_data`, after `apply_wrapping_transform` runs on the visible window's tokens:

1. Walk the wrapped token stream.
2. A `Newline` token closes the current logical line. Between Newlines, count `Break` tokens → visual row count for that logical line.
3. Identify the logical line's `line_start` byte from the first token's `source_offset` after each Newline (or the initial `viewport.top_byte` for the first line in the window).
4. Insert `(all key dimensions) → row_count` into the cache.

Runs once per render pass on the visible tokens only. Work already dominated by the wrap step — essentially free.

### Cache read by scroll math — mini-pipeline miss handler

Three current call sites compute per-line row counts with `wrap_line`:

- `Viewport::count_visual_rows_for_line` (used by `scroll_down_visual`, `scroll_up_visual`, `apply_visual_scroll_limit`, `find_max_visual_scroll_position`, `set_top_byte_with_limit`).
- `app::scrollbar_math::build_visual_row_map` (used by `scrollbar_jump_visual` and `scrollbar_drag_relative_visual` for small files only).
- `view::ui::split_rendering::scrollbar::scrollbar_visual_row_counts` (thumb sizing on small files).

Each becomes a cache query. On miss:

```rust
// Run the same 4-step pipeline the renderer runs, but for just this one line.
let tokens = build_base_tokens(buffer, line_start, est_len, /*count=*/ 1, ...);
let tokens = apply_soft_breaks(tokens, &soft_breaks_in_range(state, line_start, line_end));
let tokens = apply_conceal_ranges(tokens, &conceals_in_range(state, line_start, line_end));
let tokens = apply_wrapping_transform(tokens, effective_width, gutter_width, hanging_indent);
let count = count_breaks(&tokens) + 1;
cache.put(key, count);
count
```

Each pipeline step is already byte-range-scoped — passing `[line_start, line_end)` as a 1-line window works out of the box.

### View transforms: naturally inert, no explicit bypass

When a plugin `view_transform` is active, scroll math takes a different path entirely — `handle_mouse_scroll` runs `ViewLineIterator` directly on the plugin's tokens via `scroll_view_lines`. It never consults the wrap-row cache. So the cache is inert for view-transform buffers without needing an explicit bypass flag; it simply isn't queried.

(If we wanted to cache plugin-transformed row counts too, we'd need a monotonic `version()` on each plugin's view-transform output, since plugin state is opaque. Out of scope.)

### Single-source-of-truth invariant

Every "how many visual rows does this line wrap to" query in the codebase ultimately runs the same four-step pipeline:

- Render path: full pipeline on the visible window, cache-write side effect.
- Scroll-math path: mini-pipeline on one line, cache-write side effect.
- All cached reads afterward.

Any `(line_start, ... all key dimensions)` tuple is wrapped at most once while it lives in the cache. Whichever path hits it first pays; all later paths read.

## Invalidation — there is no explicit invalidate step

The word "invalidation" is misleading shorthand. The cache is a `HashMap<Key, u32>` + a `VecDeque<Key>` for FIFO. A lookup either hits (returns stored value) or misses (computes, stores, returns). **Nothing ever gets actively "invalidated."**

"Invalidating line 5" means: arrange for future lookups of line 5 to use a **different key**, so the previously-stored entry is never matched again. The old entry stays in the HashMap (occupying memory) until FIFO eviction retires it. Because the key has the version mixed in, a mutation to any pipeline input changes the key → old entries become unreachable.

**Worked example:**

```
Time 0: pipeline_inputs_version = 0x...A. Cache stores:
  {(v=0x...A, line_start=100, width=60, ...) → 4}

Time 1: user types a char on line 5. buffer.version() bumps.
        pipeline_inputs_version is now 0x...B.

Time 2: scroll math queries line 5's row count.
        Key built at v=0x...B: (v=0x...B, line_start=100, width=60, ...)
        Lookup in map: NOT FOUND → miss.
        Runs mini-pipeline → stores {(v=0x...B, ...) → 5}.

Time 3: the old entry at v=0x...A is still in the HashMap, unreachable.
        No query will ever build a key with v=0x...A again.

Time 4: many more inserts → cache hits `capacity`.
        FIFO evicts oldest entries. Eventually the v=0x...A entry goes.
        Memory freed.
```

**Two practical consequences:**

1. **Stale entries never cause wrong answers.** They're never returned — no query builds a key that matches them. The only cost of a stale entry is memory, and memory is bounded by `capacity`.
2. **Overinvalidation is cheap.** When any pipeline input changes, entries for *all* lines become unreachable — even lines whose text didn't change. The next access to each unchanged line triggers one mini-pipeline recompute, which is bounded and fast. We avoid the actual hard cache-invalidation problem (edit-range-scoped invalidation) by accepting this recompute cost.

**What goes into the version** — every pipeline input is covered:

| Input | Reacts to | How it gets into the key |
|---|---|---|
| buffer text | any edit | `buffer.version()` → `pipeline_inputs_version` |
| soft breaks | plugin mutates `SoftBreakManager` | `soft_breaks.version()` → `pipeline_inputs_version` |
| conceals | plugin mutates `ConcealManager` | `conceals.version()` → `pipeline_inputs_version` |
| view mode | Compose ↔ Source toggle | `view_mode` directly in key |
| `line_start` byte | upstream edits shift subsequent lines | Directly in key (new `line_start` auto-misses) |
| `effective_width` | terminal resize, `wrap_column` config | Directly in key |
| `gutter_width` | logical-line-count digit rollover, plugin adds/removes indicator columns | Directly in key |
| `wrap_column` | explicit config change | Directly in key |
| `hanging_indent` | `viewport.wrap_indent` toggle | Directly in key |
| `line_wrap_enabled` | line-wrap toggle | Directly in key (and `false` skips cache — 1 row per logical line is trivial) |

**Required plumbing:**

- `SoftBreakManager`: expose `fn version(&self) -> u32`, bumped by every mutating method. `u32` wraps at ~4B mutations per session — fine.
- `ConcealManager`: same.
- `EditorState` (or `LineWrapCache` directly): read both versions at key-build time and fold into `pipeline_inputs_version` with `buffer.version()`.

**Failure modes and how they're prevented:**

- *Stale returns* → prevented by putting every mutable input in the key (directly or via the version).
- *Unbounded growth* → prevented by FIFO cap. Stale entries age out even if never re-queried.
- *Drift between renderer writes and miss-handler writes* → prevented because both use the same pipeline functions on the same inputs. Same inputs → same output by construction.
- *Key/order desync* (internal cache bug) → `map` and `order` must stay in lockstep. Enforced by the cache's API contract and tested (see Testing section).

## Testing strategy

Caches are where correctness drifts go to hide, so we need coverage at multiple layers.

### Layer 1 — Unit tests on the cache primitive

Pure mechanical tests of the `LineWrapCache` structure, no editor involved.

- **Structural invariant**: `map.len() == order.len() <= capacity` after any sequence of `get_or_insert` calls. Property-tested (proptest) on random sequences of insert/query operations.
- **FIFO order**: inserting `capacity + 1` distinct keys evicts exactly the oldest one. Subsequent query for the evicted key is a miss, triggers recompute.
- **Re-query is a hit**: a second query for the same key returns without invoking the compute closure (tracked via a side-effect counter in the closure).
- **Distinct keys never collide**: hashing/equality tests over all field permutations.

### Layer 2 — Mini-pipeline equivalence

The miss-handler must return the same row count as the full renderer pipeline for the same line under the same inputs.

- **Given**: a buffer, an `EditorState` with arbitrary soft breaks / conceals, a viewport geometry.
- **Assert**: for every logical line, `mini_pipeline(line) == count_breaks_between_newlines(full_pipeline(whole_buffer), line)`. I.e. the 1-line mini-pipeline agrees with the renderer's per-line segmentation.
- Run as a proptest over random buffer text, random soft-break positions, random conceal ranges, random (width, gutter, hanging_indent).

### Layer 3 — Shadow-model property tests

This is the strongest correctness check. A "shadow" cache always recomputes from scratch on every query (no caching). The real cache and the shadow are driven by the same random op stream; their outputs must agree at every step.

```rust
enum Op {
    QueryRow { line_idx: usize },
    EditInsert { byte: usize, text: String },
    EditDelete { range: Range<usize> },
    AddSoftBreak { byte: usize },
    RemoveSoftBreak { byte: usize },
    AddConceal { range: Range<usize> },
    RemoveConceal { range: Range<usize> },
    ToggleViewMode,
    ResizeTerminal { width: u16 },
    ToggleLineWrap,
}

proptest! {
    fn cache_matches_shadow(ops: Vec<Op>, initial_buffer: String, geometry: Geometry) {
        let mut real = RealCacheHarness::new(&initial_buffer, geometry);
        let mut shadow = ShadowHarness::new(&initial_buffer, geometry);
        for op in ops {
            real.apply(&op);
            shadow.apply(&op);
            if let Op::QueryRow { line_idx } = op {
                prop_assert_eq!(real.query(line_idx), shadow.query(line_idx));
            }
        }
    }
}
```

The shadow is effectively a reference implementation that calls the full render pipeline on demand (no caching, no side effects). Any divergence between real and shadow is a cache bug — a stale return, a miscomputed miss, a missed invalidation.

Coverage targets for the random op generator:
- Edits near/on wrap boundaries.
- Soft breaks that fall on wrap boundaries, on a line that's already cached, before/after the line.
- Conceal ranges that span a wrap boundary, that span multiple logical lines.
- ViewMode toggles between arbitrary states.
- Resizes that cross digit-count boundaries (causing `gutter_width` change).

### Layer 4 — Invariants on the wrap function itself

Regardless of cache, the underlying row count should satisfy:

- **Width monotonicity**: for fixed text, increasing `effective_width` never increases row count. (`w' ≥ w → rows(w') ≤ rows(w)`.) A cache bug that corrupts values would eventually violate this.
- **Empty line**: empty logical line → exactly 1 row.
- **No-wrap when it fits**: if `visual_width(line_text) <= effective_width`, row count is 1.
- **Upper bound**: row count ≤ `ceil(visual_width(line_text) / min_row_width)` for some reasonable `min_row_width`.
- **Newline-free content**: `rows(text_with_no_embedded_newline)` has no soft break consequences from the soft-break subsystem unless a soft break is registered.

Proptested against `apply_wrapping_transform` directly (no cache), and separately against the cache-backed path.

### Layer 5 — Render-vs-scroll agreement

The cross-consumer invariant: whatever the renderer paints on screen for the visible window must match what scroll math thinks is there.

- Rendered frame → count visible visual rows per logical line from the painted `Vec<ViewLine>`.
- Scroll math's cache → query row count for each of those logical lines.
- Assert equality.

Run this after every operation in the property test's op stream, not just at the end, to catch transient drift.

### Layer 6 — Behavioral e2e (what we already have)

The sweep tests in `crates/fresh-editor/tests/e2e/scroll_wrapped_reach_last_line.rs` are behavioral contracts — "scrolling a buffer of long wrapped lines must reach the last line at all representative widths × heights." These stay as regression guards.

Add scenario tests for:
- Plugin soft break injected mid-scroll: scrollbar-drag-to-bottom after injection still reaches the real bottom.
- Conceal range added/removed under the cursor: row counts update, cursor stays visually anchored.
- ViewMode toggle during scroll: row counts recompute; scroll position remains coherent.
- Terminal resize mid-drag: scroll math adapts; no stuck state.
- Cache-pressure scenario: open a buffer larger than `capacity` lines, scroll-sweep from top to bottom and back — assert no visual artifacts, no panics, no off-by-ones.

### Layer 7 — Stress + fuzz (optional, longer horizon)

- A fuzzer that feeds random edit/scroll op streams and checks for panics, assertion failures, and render-vs-scroll divergence.
- Long-running "monkey" test: random operations for N minutes, assert cache invariants hold throughout.

### Coverage matrix

| | Layer 1 | Layer 2 | Layer 3 | Layer 4 | Layer 5 | Layer 6 | Layer 7 |
|---|---|---|---|---|---|---|---|
| FIFO correctness | ✓ | | ✓ | | | | ✓ |
| Mini-pipeline ≡ renderer | | ✓ | ✓ | | ✓ | | ✓ |
| Invalidation on edit | | | ✓ | | ✓ | ✓ | ✓ |
| Invalidation on soft-break change | | | ✓ | | ✓ | ✓ | ✓ |
| Invalidation on conceal change | | | ✓ | | ✓ | ✓ | ✓ |
| Width monotonicity | | | | ✓ | | | |
| Cross-consumer drift | | | | | ✓ | ✓ | ✓ |
| User-facing bugs (original tests) | | | | | | ✓ | |
| Unknown edge cases | | | ✓ | ✓ | | | ✓ |

Layers 1–5 should land with the implementation. Layer 6 extends existing e2e tests. Layer 7 is optional follow-up.

## Huge-file behavior

Unchanged, because the paths that would iterate whole-file wrap math already branch on `large_file_threshold_bytes` (1 MB) and fall back to byte-based math that never touches the cache:

- `handle_scrollbar_drag_relative` and `handle_scrollbar_jump` in `app/scrollbar_input.rs` branch at `buffer_len <= large_file_threshold`. The `else` arms compute `bytes_per_pixel` directly. Cache never consulted.
- `scrollbar_visual_row_counts` early-returns `(0, 0)` for large files. Cache never consulted.
- Mouse wheel goes through `scroll_down_visual`, which wraps only the lines actually scrolled through (bounded per event). On a huge file the cache accumulates at most N entries per scroll event.
- PageDown moves the cursor by logical lines; `ensure_visible` wraps a handful of lines to check visibility.

Net effect on huge files: identical behavior, slightly less wrap work (cache hits on lines revisited), zero unbounded memory growth.

## Trade-offs

Pipeline-output cache vs the simpler wrap-step cache:

| | Wrap-step-only cache | Pipeline-output cache (chosen) |
|---|---|---|
| Correct under soft breaks | No (needed bypass branch) | Yes |
| Correct under conceals | No (also bypass; today's scroll math is wrong here) | Yes |
| Handles view transforms | Explicit bypass | Natural (different scroll path never queries) |
| Miss-handler cost | 1 × `apply_wrapping_transform` | 4 steps: `build_base_tokens` + `apply_soft_breaks` + `apply_conceal_ranges` + `apply_wrapping_transform` |
| Code reuse | One renderer function shared | Entire pipeline shared |
| Key dimensions | 5 | 10 |
| Escape hatches | Several | None |

The miss-handler cost difference matters most on the first scrollbar-drag sweep of a small-file buffer (~12K lines). Under the chosen plan that sweep is roughly 2–4× slower than today's `wrap_line` sweep. Subsequent drags and all renders are cache hits. An initial drag at ~10–30 ms/k-lines is tolerable for the correctness it buys.

## Call-site changes

1. `view/ui/split_rendering/mod.rs`: `pub(crate) mod transforms`, `pub(crate) mod base_tokens`, `pub(crate) mod view_data` (or an equivalent re-export of the mini-pipeline helpers).
2. Visibility bumps on `apply_wrapping_transform`, `build_base_tokens`, `apply_soft_breaks`, `apply_conceal_ranges` to `pub(crate)`.
3. `state.rs`: add `line_wrap_cache: LineWrapCache` field on `EditorState`, sibling of `scrollbar_row_cache`.
4. `state/soft_breaks.rs` (or wherever `SoftBreakManager` lives): add `version: u32` field + `fn version(&self) -> u32`; bump on every mutating method.
5. `state/conceals.rs`: same pattern on `ConcealManager`.
6. New module `view/line_wrap_cache.rs`:
   - `LineWrapCache` struct + bounded-FIFO internals.
   - `LineWrapKey` struct.
   - `count_visual_rows_for_line_via_pipeline(state, buffer, line_start, geometry) -> usize` — the miss-path mini-pipeline helper.
7. `view/viewport.rs`:
   - `count_visual_rows_for_line` takes a `&mut LineWrapCache` and the full `&EditorState` (to read soft-break/conceal versions + managers for the miss path).
   - Callers (`scroll_down_visual`, `scroll_up_visual`, `apply_visual_scroll_limit`, `find_max_visual_scroll_position`, `set_top_byte_with_limit`) thread these through.
8. `app/scrollbar_math.rs`: `build_visual_row_map` takes the cache + state reference. `scrollbar_jump_visual` and `scrollbar_drag_relative_visual` signatures extend accordingly.
9. `app/scrollbar_input.rs`: pass the cache + state from `editor.buffers[buffer_id]` into the scrollbar_math calls.
10. `view/ui/split_rendering/scrollbar.rs`: `scrollbar_visual_row_counts` reads from the cache for small files.
11. `view/ui/split_rendering/view_data.rs`: after `apply_wrapping_transform`, walk the wrapped tokens and populate the cache for each logical line in the visible window.

## Fall-back revert strategy

If this refactor turns out to be too invasive, the minimum-viable fix is still:

- Keep Fix 1 (`scroll_down_visual` reclamp) — already committed.
- Keep the gutter-width unification in `scrollbar_math` — already committed.
- Add the `-1` cursor-reservation adjustment in all scroll-math `WrapConfig` builders.
- Leave `wrap_line` in place; accept the char-wrap vs word-wrap discrepancy as a known limitation documented here.

This would fix Bug 2 for homogeneous-character lines but not for real word-wrapped text (which is the reported user scenario). So this is a fallback, not the real fix.

## Out-of-scope follow-ups

- Plugin-view-transform-aware caching. Would need per-plugin `version()` + an opaque "plugin output is a function of X" contract. Not worth the surface area.
- Replacing `wrap_line` entirely in `primitives/line_wrapping.rs`. Has many non-scroll callers (cursor hit-testing, visual layout) that want char-level semantics; changing it is a separate refactor.
- Moving to Alt B5 (`ViewLines` as the coordinate system — `top_byte` derived from a ViewLine index). Architecturally cleanest long-term answer; too invasive for a bug fix.
- Edit-range-scoped invalidation (vs bumping `buffer_version` globally). Reduces overinvalidation on heavy-edit workloads; not needed at current cache sizes.
