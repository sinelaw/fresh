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

```rust
struct LineWrapKey {
    buffer_version: u64,
    soft_breaks_version: u32,
    conceal_version: u32,
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
- Cap: 8192 entries ≈ 800 KB worst case (slightly larger key than the first draft).

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

## Invalidation

Every input the pipeline reads is in the key. Invalidation happens naturally via key mismatch + FIFO eviction:

| Input | Reacts to | How invalidation happens |
|---|---|---|
| `buffer_version` | any buffer edit | Key bumps → old entries unreachable → FIFO evicts |
| `soft_breaks_version` | plugin mutates `SoftBreakManager` | Same |
| `conceal_version` | plugin mutates `ConcealManager` | Same |
| `view_mode` | Compose ↔ Source toggle | Same |
| `line_start` | upstream edits shift subsequent lines | Queries use new `line_start` → miss → recompute |
| `effective_width` | terminal resize, `wrap_column` config | Same |
| `gutter_width` | logical-line-count digit rollover (9→10, 99→100…), plugin adds/removes indicator columns | Same |
| `wrap_column` | explicit config change | Same |
| `hanging_indent` | `viewport.wrap_indent` toggle | Same |
| `line_wrap_enabled` | line-wrap toggle | Same (and line_wrap_enabled=false skips cache — 1 row per logical line is trivial) |

**Overinvalidation is intentional.** When line 5 gets edited, `buffer_version` bumps and entries for lines 1–4, 6+ all become logically dead even though their text didn't change. They age out via FIFO and recompute on next access. Refining this to edit-byte-range tracking is the actual hard cache-invalidation problem; we avoid it by making recomputation cheap and bounded.

**Required plumbing** for the new key dimensions:

- `SoftBreakManager`: expose `fn version(&self) -> u32`, bumped on any mutation. A `u32` wraps at 4B edits — fine.
- `ConcealManager`: same.
- `EditorState`: read both versions when building the key.

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
