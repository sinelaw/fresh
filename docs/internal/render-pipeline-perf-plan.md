# Render-Pipeline Perf Plan

## Context

A 90.7s `perf` capture (~59K cpu_core samples) places **`render_view_lines`
at 20.77 % of total CPU**, dwarfing every other symbol. The cost is
concentrated in a single inlined statement inside `compute_char_style`
(`crates/fresh-editor/src/view/ui/split_rendering/char_style.rs:66-74`):

```rust
let overlays: Vec<&Overlay> = ctx.viewport_overlays
    .iter()
    .filter(|(_, range)| range.contains(&bp))
    .map(|(overlay, _)| overlay)
    .collect();
```

Of the 20.77 %:

- **10.81 %** filter+find iteration (6.37 % is `Range::contains` alone)
- **9.42 %** Vec allocation/extend for the per-cell `Vec<&Overlay>`

Per visible cell, the renderer linear-scans the full viewport overlay slice
*and* heap-allocates a tiny vector that is consumed once and dropped. The
same shape — *"answer `which intervals contain this byte/line` per inner-loop
element"* — recurs across the rendering pipeline.

## The pattern

The render path repeatedly performs **interval stabbing** inside its inner
loops:

| Site | Frequency | Predicate | Today |
|---|---|---|---|
| `viewport_overlays` (`char_style.rs:67`) | per cell | `range.contains(&bp)` | linear scan + Vec collect |
| `selection_ranges` (`render_line.rs:467`) | per cell | `range.contains(&bp)` | `iter().any` |
| `block_selections` (`render_line.rs:449`) | per cell | line/col rectangle | `iter().any` |
| `conceal_ranges` (`transforms.rs:393-400`) | per char of every Text token | `start <= b < end` | linear scan via closure |
| `is_hidden_byte` (`folding.rs:152-156`) | per ViewLine | `start_byte <= b < end_byte` | `iter().any` |

There is precedent for the right shape:

- `span_color_at` / `span_info_at` (`spans.rs:265-302`) already use a stateful
  `&mut cursor` over a sorted-by-position spans slice. `highlight_spans` and
  `semantic_token_spans` therefore *don't* appear in the perf hot list despite
  being touched on every cell.
- `apply_soft_breaks` (`transforms.rs:328`) already advances `break_idx`
  monotonically through a sorted break list.

The plan generalises that idiom — sweep-line / merge-iterate — across the
remaining stabbing sites, then attacks the larger structural waste in the
fold path.

## Goal

1. Eliminate per-cell `Vec` allocation in the style-resolution hot path.
2. Replace per-cell linear scans over interval lists with O(1) amortised
   active-set updates.
3. Stop materialising hidden ViewLines that fold post-filtering will discard.

Success is measured against a re-captured `perf` profile in the same scenario:
`render_view_lines` should drop from ~21 % to a single-digit share, with the
overlay-stab and Vec-allocation chunks gone entirely.

## Phase 1 — Overlay sweep in `compute_char_style`

**Files touched**: `orchestration/overlays.rs`, `orchestration/render_line.rs`,
`char_style.rs`, `orchestration/contexts.rs`.

The wrinkle vs. the existing `span_*` cursor pattern is **multi-overlap +
priority ordering**: multiple overlays can cover the same byte, and the apply
loop expects them in priority-ascending order so "last write wins" produces
the correct z-order.

### Design

In `overlays.rs`, after the existing priority sort
(`overlays.rs:168`), build a parallel **position index**: a
`Vec<usize>` of indices into `viewport_overlays`, sorted by `range.start`.
Pass both into `DecorationContext` (no semantic change for current callers).

In `render_line.rs`, maintain alongside `hl_cursor`/`sem_cursor`:

- `active_overlays: SmallVec<[usize; 8]>` — indices currently covering
  `byte_pos`, kept sorted by priority via insertion-sort on add.
- `next_overlay_in_pos: usize` — pointer into the position-sorted index.

Per cell, **only when `byte_pos` actually changes**:

1. Drop entries from `active_overlays` whose `range.end <= bp`.
2. While `position_index[next_overlay_in_pos]` has `range.start <= bp`:
   insert into `active_overlays` by priority; advance `next_overlay_in_pos`.

`compute_char_style` accepts `active_overlays: &[&Overlay]` instead of
filtering `viewport_overlays`. The match-and-apply loop is unchanged.

### Edge cases

- **`byte_pos: Option<usize>`** (virtual text, ANSI escape continuation
  cells): skip sweep updates; pass an empty `active_overlays` slice to match
  current behaviour where `bp = None` short-circuits to `Vec::new()`.
- **View-line transitions** within one `render_view_lines` call: `bp` jumps
  forward but stays monotonic; sweep state persists across lines.
- **Overlays that span multiple view lines**: handled naturally by the active
  set.
- **Overlays with `range.start == range.end`** (zero-width): currently filtered
  by `Range::contains`; preserve by including only when `start <= bp < end`.

### Cost model

- **Setup**: O(N log N) sort of `position_index` per render.
- **Per cell**: O(1) amortised. Each overlay enters and exits `active_overlays`
  exactly once per render. Worst-case insertion is O(k) for active depth `k`
  (typically ≤ 5).
- **Allocation**: zero per cell. `active_overlays` is a `SmallVec` reused
  across the entire `render_view_lines` call.

### `extend_to_line_end` follow-up (same phase)

`render_line.rs:971-1002` re-scans `viewport_overlays` per line searching for
the highest-priority overlay with `extend_to_line_end` overlapping the line's
byte range. Same active set serves this lookup: at end-of-line, scan
`active_overlays` (small) instead of the full viewport list.

## Phase 2 — Selection + block-selection sweep

**Files touched**: `orchestration/render_line.rs`,
`orchestration/contexts.rs` (`SelectionContext`).

### Linear selection ranges (`render_line.rs:467`)

`selection_ranges` is normally non-overlapping and sorted by start. Replace
`selection_ranges.iter().any(|range| range.contains(&bp))` with a
`sel_cursor: usize` advanced exactly like `hl_cursor`. Single-overlap variant
of the overlay sweep — strictly simpler.

### Block selections (`render_line.rs:449`)

`block_selections: &[(start_line, start_col, end_line, end_col)]` — the
predicate is `gutter_num` ∈ [start_line, end_line] ∧ `byte_index` ∈
[start_col, end_col]. Sort by `start_line`; maintain
`active_block_selections` updated as `gutter_num` advances per ViewLine
(once per line, not once per cell). Per cell, scan only the active set for
the column predicate.

## Phase 3 — Conceal-range sweep in `apply_conceal_ranges`

**File**: `crates/fresh-editor/src/view/ui/split_rendering/transforms.rs`.

`apply_conceal_ranges` defines `is_concealed` (`transforms.rs:393-400`) as a
closure that linear-scans `conceal_ranges` for **every character of every
Text token**. Because the function walks tokens in source-byte order, and
conceal ranges arrive sorted (or are trivially sortable), the closure
becomes a stateful cursor:

```rust
let mut conceal_cursor: usize = 0;
let active = |b: usize| -> Option<usize> {
    while conceal_cursor < conceal_ranges.len()
        && conceal_ranges[conceal_cursor].0.end <= b {
        conceal_cursor += 1;
    }
    let (range, _) = conceal_ranges.get(conceal_cursor)?;
    (range.start <= b && b < range.end).then_some(conceal_cursor)
};
```

Drops the per-character scan to O(1) amortised. Note the existing
`emitted_replacements: HashSet<usize>` is keyed by conceal index — preserved
unchanged.

## Phase 4 — Fold-hidden-line sweep in `apply_folding`

**File**: `crates/fresh-editor/src/view/ui/split_rendering/folding.rs`.

`is_hidden_byte` (`folding.rs:152-156`) is `ranges.iter().any(...)` per
ViewLine. Sort `collapsed_ranges` by `start_byte` once; advance a cursor as
the iteration of `lines` walks forward in source-byte order. This is a
small fix — the *real* fold win is Phase 5.

## Phase 5 — Push fold-aware skipping into `ViewLineIterator`

**Files**: `crates/fresh-editor/src/view/ui/view_pipeline.rs`,
`crates/fresh-editor/src/view/ui/split_rendering/folding.rs`,
`crates/fresh-editor/src/view/ui/split_rendering/orchestration/`
(layout / line-budget calculation),
`crates/fresh-editor/src/view/ui/split_rendering/mod.rs` (entry).

### What the current pipeline does

1. `fold_adjusted_visible_count` (`folding.rs:28-78`) **inflates the line
   budget** so the view pipeline produces enough ViewLines to cover hidden
   content too — otherwise the visible portion would come up short.
2. The full pipeline runs for every ViewLine the budget asks for: base
   tokens, wrapping, conceals, char styles, char_source_bytes, the per-line
   `text` clone — all materialised.
3. `apply_folding` then **post-filters**: walks every produced `ViewLine`,
   calls `is_hidden_byte`, drops hidden ones.

So folded text is a *filter-out*, not a *skip-over*. Wasted upstream work
scales with the fold mass, not the visible cell count, and can dwarf the
per-cell hot path on heavily-folded buffers (large JSON, code with many
collapsed regions).

### Target architecture

Teach the iterator that produces ViewLines to **skip hidden source ranges
at the source level**, so hidden bytes are never tokenised.

1. **`FoldSkipSet`**: a sorted, non-overlapping `Vec<Range<usize>>` derived
   once per render from `FoldManager::resolved_ranges`. Lives alongside the
   buffer in the iterator's input.

2. **`ViewLineIterator` accepts a `FoldSkipSet`** (or `&[Range<usize>]`).
   When the iterator's source-byte cursor enters a skip range, it advances
   the cursor to `range.end` (the next visible byte) before producing the
   next ViewLine. Wrapping/conceal transforms upstream stay unchanged
   because they operate on the token stream — the skip happens at iterator
   construction-time of source positions, not after token production.

3. **Header replacement still emitted**: the fold-header line that owns the
   collapse remains visible, with placeholder text appended (current
   `append_fold_placeholder` logic moves into the iterator's first-pass
   handling for header bytes).

4. **`fold_adjusted_visible_count` collapses to identity** — once hidden
   bytes are skipped at the source, the visible-line budget no longer needs
   to be inflated. The function is removed; callers pass through
   `visible_count` directly.

5. **`apply_folding`'s line-filter step is deleted**. The placeholder
   `append_fold_placeholder` work moves up into the iterator (or into a
   tiny post-pass that operates only on header lines, identified by
   `collapsed_header_bytes`).

### Migration sequence

This is the largest change in the plan. Do it in order:

1. **5a**: Introduce `FoldSkipSet` as a typed input on
   `ViewLineIterator::new`, plumbed but **empty by default** (no behaviour
   change). Add unit tests for the iterator that drive it with non-empty
   skip sets and assert source-byte continuity across skip boundaries.

2. **5b**: Implement the skip logic in the iterator's source-byte advance
   path. Verify visual equivalence by running with the new path enabled
   only when `FoldManager::is_empty()` is false; the post-filter remains in
   place as a belt-and-suspenders correctness check (it should now be a
   no-op).

3. **5c**: Remove `apply_folding`'s line-filter loop; keep only the
   header-placeholder application (move it into a small helper that takes
   only header-byte → placeholder).

4. **5d**: Remove `fold_adjusted_visible_count`; restore the natural
   `visible_count` plumbing.

5. **5e**: Re-run `perf`. Folded buffers should now show fold-mass-invariant
   render cost.

### Risks (specific to Phase 5)

- **`view_pipeline.rs` and the wrap/conceal transforms assume a contiguous
  source-byte stream.** The skip happens *before* tokenisation but *inside*
  the iterator, so the contract is preserved: each emitted ViewLine still
  references monotonically increasing source bytes — they just have gaps.
  All downstream consumers already tolerate gaps (LineStart variants,
  `char_source_bytes: Vec<Option<usize>>`).
- **Cursor / click resolution across folded ranges.** `screen_to_buffer_position`
  and friends rely on `view_line_mappings.line_end_byte`. The iterator must
  set `line_end_byte` to the byte *after* the last hidden byte for the
  fold-header line, so `Down` from the header moves past the fold (parity
  with current behaviour).
- **Indent-folding indicator code in `fold_indicators_for_viewport`** still
  needs to see the *unfolded* line budget for its lookahead. Solve by
  computing indicators against the buffer directly, not against
  post-iterator ViewLines (it largely already does — verify and lock in).

## Smaller, allocation-side wins (independent of phases above)

Visible in the same profile, separable PRs:

- **`render_line.rs:201`** — `current_view_line.text.clone()` per ViewLine.
  Only needed for `contains('\x1b')` and `chars()`; both work on `&str`.
  Drop the clone.
- **`render_line.rs:214`** — `line_chars_for_ws: Vec<char> = line_content.chars().collect()`
  per ViewLine, only used for `.position`/`.rposition`. Inline on the
  iterator.
- **`render_line.rs:592`** — `indicator_buf = ch.to_string()` per cell.
  Replace with `encode_utf8` into a stack `[u8; 4]`.
- **`render_line.rs:864`** — per-line `line_view_map.iter().enumerate()` scan
  for cursor x. Track during the main per-cell loop where `bp` is already
  in hand.

Combined these account for an additional several percent of CPU spread
across `String::clone`, `RawVec::finish_grow`, `cfree` and `realloc` in the
profile — none individually large, all easy.

## Risks & mitigations

| Risk | Phase | Mitigation |
|---|---|---|
| Active-set update at line transitions miscounts overlay depth | 1 | Unit tests in `char_style.rs` that drive sweep across multi-line overlays. |
| Selection sweep breaks when ranges are not actually sorted | 2 | Add a debug-build assertion in the renderer; sort defensively if needed (selections are small). |
| Conceal cursor drifts when a token's bytes don't strictly increase (e.g. virtual tokens) | 3 | Skip cursor update for tokens with `source_offset == None` — same shape as today's closure. |
| Fold post-filter removed before iterator skip is correct | 5c | Keep post-filter in place during 5a/5b; assert it removes zero lines before deleting in 5c. |
| Cursor `Down` behaviour over folded ranges regresses | 5 | Add e2e tests for cursor traversal across collapsed folds before any iterator changes; use them as the regression gate. |
| `fold_indicators_for_viewport` lookahead breaks once budget no longer inflates | 5d | Move indicator detection to operate on buffer directly, decoupled from ViewLine count. |

## Success criteria

- `render_view_lines` ≤ 8 % of CPU on the same workload (down from 20.77 %).
- `compute_char_style`'s `Vec<&Overlay>` allocation no longer present in
  flame-graph (zero `RawVec::finish_grow` attributable to `compute_char_style`).
- All existing tests pass at each phase boundary; per-phase commits.
- A heavily-folded benchmark buffer (≥ 80 % bytes inside collapsed ranges)
  shows render time within ~1.5× of the same buffer fully expanded
  (currently it can be far worse because hidden lines are still tokenised).
- No new public API surface; all changes contained within
  `view/ui/split_rendering/`, `view/ui/view_pipeline.rs`, and
  `view/folding.rs`.
