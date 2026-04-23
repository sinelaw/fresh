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

## Approach: B4 (shared line-wrap cache populated by both paths)

A per-buffer, bounded LRU cache keyed by the exact inputs that determine a line's wrapped row count. The renderer writes to it as a side effect of rendering; scroll math reads from it and fills missing entries on demand. Same function computes the value regardless of which caller triggers it first — so drift is impossible and no line is wrapped twice per `(line, geometry)`.

### Cache shape

```rust
struct LineWrapKey {
    buffer_version: u64,
    line_start: usize,
    effective_width: u32,
    gutter_width: u16,
    hanging_indent: bool,
}

struct LineWrapCache {
    map: HashMap<LineWrapKey, u32>,   // row_count
    order: VecDeque<LineWrapKey>,     // FIFO eviction
    capacity: usize,                   // default 8192
}
```

- Lives on `EditorState`, sibling of `ScrollbarRowCache`.
- Key includes `buffer_version`: an edit naturally invalidates all old entries (they become unreachable by future lookups and age out via FIFO).
- Width/gutter/hanging-indent changes just yield a different key — old entries age out.
- Cap: 8192 entries ≈ 700 KB worst case.

### Cache write by the renderer (free work)

In `split_rendering::view_data::build_view_data`, after `apply_wrapping_transform` has run on the visible window's tokens:

1. Walk the wrapped token stream.
2. A `Newline` token closes the current logical line; between Newlines, count `Break` tokens → visual row count for that logical line.
3. Identify the logical line's `line_start` byte from the first token's `source_offset` after each Newline (or the initial `viewport.top_byte` for the first line).
4. Insert `(line_start, effective_width, gutter_width, hanging_indent, buffer_version) → row_count` into the cache.

This runs once per render pass. It's O(viewport tokens), i.e. already dominated by the wrapping pass — essentially free.

### Cache read by scroll math

Three call sites currently compute per-line row counts with `wrap_line`:

- `Viewport::count_visual_rows_for_line` (used by `scroll_down_visual`, `scroll_up_visual`, `apply_visual_scroll_limit`, `find_max_visual_scroll_position`, `set_top_byte_with_limit`).
- `app::scrollbar_math::build_visual_row_map` (used by `scrollbar_jump_visual` and `scrollbar_drag_relative_visual` for small files only).
- `view::ui::split_rendering::scrollbar::scrollbar_visual_row_counts` (thumb sizing on small files).

Each becomes:

```rust
cache.get_or_compute(key, || count_visual_rows_for_line_text(
    text, effective_width, gutter_width, hanging_indent,
))
```

where `count_visual_rows_for_line_text` is a new helper that builds a single-`Text`-token input and runs `apply_wrapping_transform` — the same function the renderer uses.

### Single-source-of-truth invariant

Every line-wrap query in the codebase, whether rendering or scroll-math, ultimately hits `apply_wrapping_transform` (directly in the render path, via the cache miss path elsewhere). There is no second implementation to drift from.

Any `(line_start, effective_width, gutter_width, hanging_indent, buffer_version)` tuple is wrapped at most once while it lives in the cache. Whichever consumer hits it first pays; all later consumers read.

## Huge-file behavior

Huge-file scrolling is unchanged because the paths that would iterate whole-file wrap math already branch on `large_file_threshold_bytes` (1 MB) and fall back to byte-based math that doesn't touch the cache:

- `handle_scrollbar_drag_relative` and `handle_scrollbar_jump` in `app/scrollbar_input.rs` branch at `buffer_len <= large_file_threshold`. The `else` arms compute `bytes_per_pixel` and never call `scrollbar_math`. Cache never touched.
- `scrollbar_visual_row_counts` early-returns `(0, 0)` for large files. Cache never touched.
- Mouse wheel goes through `scroll_down_visual`, which only wraps the lines actually scrolled through (bounded per event). On a huge file the cache accumulates at most N entries per scroll event, never the whole file.
- PageDown moves the cursor by logical lines; `ensure_visible` wraps a handful of lines to check visibility.

Net effect on huge files: identical behavior, slightly less wrap work (cache hits on lines revisited), zero unbounded memory growth.

## Call-site changes

1. `view/ui/split_rendering/mod.rs`: `pub(crate) mod transforms` (already done).
2. `view/ui/split_rendering/transforms.rs`: `apply_wrapping_transform` `pub(super)` → `pub(crate)` (already done).
3. New module `view/line_wrap_cache.rs`:
   - `LineWrapCache` struct + methods.
   - `count_visual_rows_for_line_text(text, effective_width, gutter_width, hanging_indent) -> usize` helper.
4. `state.rs`: add `line_wrap_cache: LineWrapCache` field on `EditorState`, initialized to default.
5. `view/viewport.rs`:
   - `count_visual_rows_for_line` takes `&mut LineWrapCache` (or a `CountContext` struct with all inputs).
   - Callers (`scroll_down_visual`, etc.) thread the cache through.
6. `app/scrollbar_math.rs`: `build_visual_row_map` takes `&mut LineWrapCache`. `scrollbar_jump_visual` and `scrollbar_drag_relative_visual` signatures extend accordingly.
7. `app/scrollbar_input.rs`: pass the cache from `editor.buffers[buffer_id].line_wrap_cache` into the scrollbar_math calls.
8. `view/ui/split_rendering/scrollbar.rs`: `scrollbar_visual_row_counts` reads from cache for small files.
9. `view/ui/split_rendering/view_data.rs`: after `apply_wrapping_transform`, walk the wrapped tokens and populate the cache for each logical line in the visible window.

## Fall-back revert strategy

If this refactor turns out to be too invasive, the minimum-viable fix is still:

- Keep Fix 1 (`scroll_down_visual` reclamp) — already committed.
- Keep the gutter-width unification in `scrollbar_math` — already committed.
- Add the `-1` cursor-reservation adjustment in all scroll-math `WrapConfig` builders.
- Leave `wrap_line` in place; accept the char-wrap vs word-wrap discrepancy as a known limitation documented here.

This would fix Bug 2 for homogeneous-character lines but not for real word-wrapped text (which is the reported user scenario). So this is a fallback, not the real fix.

## Out-of-scope follow-ups

- Replacing `wrap_line` entirely in `primitives/line_wrapping.rs`. Has many non-scroll callers (cursor hit-testing, visual layout) that want char-level semantics; changing it is a separate refactor with its own test surface.
- Moving to Alt B5 (`ViewLines` as the coordinate system — `top_byte` derived from a ViewLine index). Architecturally the cleanest long-term answer; too invasive for a bug fix.
