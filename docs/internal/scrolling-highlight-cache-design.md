# Scrolling-Friendly Highlight Cache

## Status: Phase 1 shipped; Phase 2 deferred; Phases 3/4 in progress

**Phase 1 (whole-file cache for small files)** is implemented and eliminates
the original bottleneck (63 % of P-core cycles in `full_parse` per scroll
frame on small files). After the first parse, scrolling is filter-only.

**Phase 2 (worker thread + atomic snapshot)** is deferred. The synchronous
implementation in Phase 1 already gives ~zero render-thread CPU on
steady-state scroll for small files. Moving parsing off-thread further
would require either making `Buffer` cross-thread shareable or copying
bytes per parse pass — both substantive architectural changes whose
marginal benefit (smoother first-paint, no jank during pathological edits)
doesn't currently justify the concurrency surface area. Revisit when:
- File-open latency on large files becomes a user-visible problem.
- Pathological-edit jank is reported in practice.

**Phase 3 (large-file unification)** and **Phase 4** (scope cache, grammar
gen counter) build directly on Phase 1.

Successor to `syntax-highlighting-checkpoint-design.md`. The checkpoint
machinery (markers + saved `(ParseState, ScopeStack)`) stays. What changes is
how the cache is sized, who owns its mutation, and what the render thread does
when the cache doesn't cover the viewport.

## Problem

Profiling a mouse-scroll session (`perf record --call-graph fp -e cycles:u`,
~25 s of continuous wheel scrolling, 31 K samples on the P-cores) shows:

```
fresh::run_event_loop_common
  73.7%  Terminal::draw
   72.4%  Editor::render
    65.9%  compute_buffer_layout
     63.4%  HighlightEngine::highlight_viewport
      63.3%  TextMateEngine::full_parse        ← every scroll frame
       52.8%  ParseState::parse_line
       8.9%   scope_stack_to_category
```

The cause is in `highlight_viewport` (`crates/fresh-editor/src/primitives/highlight_engine.rs:413-485`).
The cache-hit gate requires `cache.range.end >= parse_end`, where
`parse_end = viewport_end + 10 KB` (`highlight_context_bytes`, default 10 K).
After one parse, `cache.range.end == viewport_end + 10 KB`. A forward scroll by
any amount Δ > 0 sets the new `parse_end = viewport_end + Δ + 10 KB`, the gate
fails, and we fall through to `full_parse` — which **replaces** the cache with
a new ±10 KB window centred on the new viewport, discarding the previous
spans even though the unchanged regions were still correct. Upward scroll
fails the symmetric `cache.range.start <= desired_parse_start` predicate and
behaves the same way.

So the cache is rebuilt-from-checkpoint on every scroll tick. The 10 KB
context padding is one-shot pre-warm that's discarded the moment the user
moves.

## Goal

Steady-state scrolling on a small file should do **zero parse work**. Edits
should pay only for the convergence region, off the render thread.

## Design

### Cache shape

```
struct TextMateCache {
    range: Range<usize>,          // bytes covered by `spans`
    spans: Vec<CachedSpan>,       // sorted by start, no overlaps
    tail_state: Option<(ParseState, ScopeStack)>,  // saved at range.end
    buffer_version: u64,          // matches buffer at last splice
}
```

`tail_state` is new — required so the worker can extend the cache forward
without re-parsing from a checkpoint.

The engine holds the cache behind an atomic snapshot:

```
cache: arc_swap::ArcSwap<TextMateCache>
```

### Single-writer worker

A dedicated worker thread per `TextMateEngine` (or one shared pool, scoped per
buffer) is the **only** code path that mutates the cache. It owns:

- The current `TextMateCache`.
- The `checkpoint_markers` / `checkpoint_states` machinery (already exists).
- A command channel: `Invalidate { dirty_from }`, `EnsureRangeParsed { upto }`,
  `Stop`.
- A cancellation generation counter shared with the foreground (`Arc<AtomicU64>`),
  bumped on each edit.

Worker loop:

```
loop {
    cmd = chan.recv();                       // block when idle
    match cmd {
        Invalidate{dirty_from}        => apply_dirty(dirty_from);
        EnsureRangeParsed{upto}       => parse_until(upto);
        Stop                          => return;
    }
    // After every command, opportunistically extend cache toward EOF
    // (or toward `target_end`, see Large Files below) at idle priority,
    // checking the cancel generation between checkpoints.
}
```

After each completed pass (small splice or window extension), the worker
publishes a new `Arc<TextMateCache>` via `ArcSwap::store`.

### Render path

`highlight_viewport` becomes pure read:

```
let cache = self.cache.load();             // Arc<TextMateCache>, lock-free
if cache.range covers viewport {
    return filter_cached_spans(...);       // existing helper
}
// Fast scroll past dirty tail OR cold-start before worker filled enough
worker.send(EnsureRangeParsed{ upto: viewport_end });
return filter_cached_spans(...);           // may be partially stale for ≤1 frame
```

The render thread never parses, never blocks. If the cache is briefly
incomplete for the viewport, we ship one frame with whatever spans exist
(`notify_insert/delete` already keep span byte offsets aligned with the
buffer, so unchanged regions are correct; only the not-yet-reparsed dirty
tail is stale). The worker delivers correct spans by the next frame.

### Edit path

`notify_insert` / `notify_delete` keep their current behaviour (shift span
offsets, extend/contract `cache.range`, update `dirty_from`), then bump the
cancel generation and post `Invalidate{dirty_from}` to the worker. Edits do
not block.

### Convergence and budget

Worker handling of `Invalidate`:

1. Resume parsing from the nearest checkpoint before `dirty_from`.
2. At each subsequent checkpoint, compare the freshly-computed
   `(ParseState, ScopeStack)` against the stored snapshot.
3. **Converged**: splice new spans into the cache up to the convergence
   point. `dirty_from = None`. Done.
4. **Budget hit** (`BUDGET` bytes parsed without convergence — start with
   ~64 KB, tune later): splice new spans up to the current position, set
   `dirty_from = current_position`, publish, then continue parsing on the
   next loop iteration. This prevents pathological edits (unmatched `/*`
   that re-scopes the entire tail) from blocking the worker on a single
   command.
5. The cancel generation may advance mid-pass (another edit arrived); abort
   and restart from the new `dirty_from`.

### Buffer access

The worker reads the buffer through the same handle pattern other background
work uses. Edits go through `notify_insert/notify_delete` first, then post
the channel message; ordering guarantees the worker sees `dirty_from <=
edit_position` when it next reads the buffer. The cancel generation guards
against the worker reading bytes that are about to change.

## Large files (`> MAX_PARSE_BYTES`)

Same code paths, with one knob.

The cache structure, the snapshot, the worker, the render path, the edit
path — all unchanged. The only difference is the **target range** the worker
tries to keep covered. Today's threshold (`MAX_PARSE_BYTES = 1 MB`) gates
"parse the whole file" against "parse a viewport-centred window."

Add a single `target_end_for(viewport: Range<usize>, buffer_len: usize)`:

```
fn target_end_for(viewport: Range<usize>, buffer_len: usize) -> usize {
    if buffer_len <= MAX_PARSE_BYTES {
        buffer_len                                // small file: whole-file
    } else {
        (viewport.end + LARGE_FILE_LOOKAHEAD)     // e.g. 256 KB ahead
            .min(buffer_len)
    }
}

fn target_start_for(viewport: Range<usize>, _: usize) -> usize {
    if buffer_len <= MAX_PARSE_BYTES {
        0
    } else {
        viewport.start.saturating_sub(LARGE_FILE_LOOKBEHIND)  // e.g. 64 KB
    }
}
```

The worker, when idle, parses toward `target_end_for(current_viewport, …)`
and prunes spans before `target_start_for(...)` to keep memory bounded. The
viewport position is published by the render thread (an `AtomicUsize` pair
or a small mailbox; the worker reads the latest, no contention).

Because the cache is a contiguous range and span byte offsets stay aligned
with the buffer through `notify_insert/notify_delete`, sliding the window
forward and dropping stale spans behind is the same span-vector mutation as
splicing. No new code path.

When the user scrolls far past the lookahead, the render path falls into
`EnsureRangeParsed{upto: viewport_end}` exactly as in the small-file case.
The worker re-anchors from a checkpoint near the new viewport. The same
"render stale for ≤1 frame, snap correct" behaviour applies.

Concretely, `target_end_for` and `target_start_for` are the **only**
file-size branch in the design. Everything else — the snapshot, the worker
loop, the convergence/budget logic, the cancel generation, the render path —
runs unchanged.

### Memory bounds

- Small file (≤ 1 MB): cache holds spans for the whole file.
  ~24 B/span × 5–20 spans/line × 10K lines ≈ 1–5 MB.
- Large file (> 1 MB): cache holds spans for
  `[target_start .. target_end]`, ~320 KB worth of source. ~80 KB–400 KB
  of spans.

Memory cost scales with the visible+lookahead window, not file size.

### Edits in large files

`try_partial_update`'s convergence pass naturally bounds work to the
"distance from the edit until parse state stabilises", regardless of file
size. The budget cap protects against pathological non-convergence in both
modes. The window-trim step runs after convergence, so an edit far inside
the cached window is just a normal splice; an edit *outside* the cached
window (rare during typing) is dropped on the floor — `dirty_from` is set,
but the next pass hits a region already outside `target_start..target_end`
and clears `dirty_from` without doing work.

## What gets simpler

- The current `cache_covers_viewport` / `exact_cache_hit` gate collapses to
  "does the cache range cover the viewport?". `highlight_context_bytes`
  (10 KB) becomes irrelevant for the small-file path and is folded into
  `LARGE_FILE_LOOKAHEAD`/`LOOKBEHIND` for the large-file path.
- The render-thread `full_parse` call site goes away. `full_parse` itself
  moves into the worker as `worker_extend(start, end)`.
- The byte-0-resume fallback in `find_parse_resume_point` becomes a worker
  cold-start detail; the render path never reaches it.

## What stays

- `notify_insert` / `notify_delete` and their span-shifting logic.
- Checkpoint markers + saved states (`checkpoint_markers`,
  `checkpoint_states`).
- `try_partial_update`'s convergence algorithm.
- `scope_stack_to_category` (but see adjacent fix below).

## Adjacent fixes

These were surfaced by the same profile and are independent of the cache
redesign. Worth folding in while touching the engine.

1. **Cache `Scope → Option<HighlightCategory>`** in
   `scope_stack_to_category` (`highlight_engine.rs:954-962`). Syntect's
   `Scope` atoms are append-only-interned globally, so the mapping never
   needs invalidation. A `HashMap<Scope, Option<HighlightCategory>>` per
   engine eliminates the `~9%` cost of `scope.build_string()` per token.
2. **Grammar-snapshot rebuild bug** in
   `app/plugin_dispatch.rs:68-81`. The cheap-check compares
   `available_syntaxes().len()` (syntect-only count) against the snapshot's
   count built from `available_grammar_info()` (catalog count, includes
   tree-sitter-only languages, excludes Plain Text). The two never match,
   so the rebuild fires on every event tick. Replace with a `catalog_gen:
   u64` generation counter on `GrammarRegistry`, bumped on every catalog
   mutation; snapshot stores the last-seen gen.

## Out of scope

- Tree-sitter's highlight path. Tree-sitter is incremental and sub-ms per
  edit; it doesn't share the bottleneck. The cache machinery should still
  live at the `HighlightEngine` level so both backends benefit if a
  tree-sitter span cache is wanted later, but the worker thread treatment
  is only justified for syntect.
- Async parsing on file open. The worker already gives us this for free —
  open posts an `EnsureRangeParsed{upto: viewport_end}`, the worker
  catches up to fill the rest. No special open-time path.

## Open questions

- Worker-per-engine vs. shared pool. Per-engine is simpler; pool is more
  efficient when many buffers are open. Start per-engine; revisit if the
  thread count becomes a problem.
- `BUDGET` and `LARGE_FILE_LOOKAHEAD` defaults. Pick conservatively
  (~64 KB / ~256 KB), measure, tune.
- Buffer-handle ergonomics for the worker. The buffer is mutated only on
  the foreground; worker reads bytes via the existing snapshot/handle
  primitives (TBD, depends on what's already exposed).

## Expected outcome

- Steady-state scroll on small file: ~0% CPU in `full_parse`. Render
  reduces to span filtering + decoration assembly. Should comfortably hit
  refresh-rate frame budget on the render thread.
- Steady-state scroll on large file: ~0% CPU on the render thread for
  scrolls within the lookahead window. Brief one-frame staleness when
  scrolling past the window.
- Edit on small file: bounded splice on the worker, render reads a
  near-up-to-date snapshot. Pathological edits never block the render.
- Edit on large file: identical behaviour, the budgeted partial pass
  doesn't care about file size.
