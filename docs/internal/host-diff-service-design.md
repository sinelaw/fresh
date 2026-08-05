# Host diff service: baselines as buffers, one diff engine, one cache

Status: proposal (follow-up to `live-diff-scalable-diff-design.md`)

## The landscape today — four diff paths, three coordinate systems

The editor currently computes "what changed" in four unrelated ways:

1. **Native unsaved-changes stripe** (gutter blue `│`, priority 5, plus
   scrollbar marks). `Persistence` keeps `saved_root: Arc<PieceTreeNode>` —
   the piece-tree root at last save, an O(1) snapshot. `diff_since_saved`
   (`model/buffer/persistence.rs:266`) walks the saved and live trees in
   parallel, skipping identical subtrees by `Arc::ptr_eq`, and emits
   **document-absolute byte ranges** (`PieceTreeDiff`). It is
   *identity*-based (a re-typed identical byte is a different `Added`
   piece), corrected by a byte-compare verify pass only when the changed
   span is ≤ 64 KiB (`MAX_VERIFY_BYTES`). Three consumers re-run it
   independently, up to **3× per frame per buffer**: the gutter
   (`folding.rs::diff_indicators_for_viewport`), the scrollbar
   (`scrollbar.rs::unsaved_change_marks` — only its *projection* is
   cached, keyed on `(version, save_state_version, basis, track_height)`),
   and the plugin snapshot (`buffer_saved_diffs`, rebuilt every tick in
   `populate_plugin_state_snapshot`).
2. **git_gutter.ts** (priority 10): spawns
   `git diff -U0 HEAD -- <file>`, string-parses the unified output, and
   marks **disk vs HEAD** in *line* coordinates. It never sees the live
   buffer; between saves its markers drift on anchors.
3. **live_diff.ts** (priority 9): configurable reference (HEAD / disk /
   any ref) vs the **live buffer**, diffed by the native
   `computeLineDiff` patience diff (`fresh-plugin-runtime/src/diff.rs`),
   in *line* coordinates — but both full texts cross the plugin bridge
   every recompute.
4. **Composite/review buffers**: the host does *alignment only*
   (`LineAlignment::from_hunks`); the hunks are produced by
   `audit_mode.ts` string-parsing `git diff` output, like git_gutter.

Plus one fossil: `model/line_diff.rs`, a dense-LCS saved-vs-current
line diff with **zero non-test callers**. It should be deleted.

Every row of that list wants the same thing — "hunks between the live
buffer and some reference" — and each solved reference-loading,
diffing, coordinates, caching and invalidation separately. CONTRIBUTING
§12 ("when a bug recurs, centralize") applies squarely.

## Design in one sentence

**A baseline is a hidden read-only `TextBuffer`; the diff service diffs
two `TextBuffer`s; every consumer — native stripe, git gutter, live
diff, review alignment — reads one memoized `DiffResult` per
(buffer, baseline) pair.**

This is the formulation that fits the existing architecture with no new
concepts, because everything needed already exists as a first-class
citizen:

- **Loading a reference is opening a buffer.** Disk baselines load via
  the buffer's own `filesystem()` (never the active window's — the
  buffer may belong to another authority); git baselines via
  `process_spawner.spawn_to_file("git", ["show", "<oid>:<path>"], cwd,
  tmp)` — stdout streamed to disk, never into memory (the shipped
  git-log pattern) — then opened file-backed. Both routes are
  remote-authority-correct by construction, and both inherit
  lazy chunk loading, large-file mode, and encoding handling for free.
  A 200 MB reference costs what opening a 200 MB file costs: almost
  nothing until read.
- **Snapshotting is `piece_tree.root()`** — one `Arc::clone`. The
  saved baseline needs no loading at all: it *is* `saved_root`, already
  maintained through chunk loads and pristine rebuilds
  (`rebuild_with_pristine_saved_root`).
- **Reading slices of a baseline is reading a buffer** —
  `iter_lines_from`, `get_text_range_mut`, viewport-lazy. No new API
  shape for "give me the deleted lines' text".
- **Showing a baseline side-by-side is a composite source pane** —
  hidden virtual/file-backed buffers already back composite panes.

## The engine: two tiers, one result

```rust
pub struct DiffResult {
    /// Content-accurate line hunks (patience). Empty ⇔ equal.
    pub hunks: Vec<LineDiffHunk>,          // existing fresh-core type
    /// Byte projection of `hunks` against the *new* side, for byte-basis
    /// consumers (scrollbar Bytes basis, large-file gutter).
    pub byte_ranges: Vec<Range<usize>>,
    /// Which tier produced it + whether lines were available.
    pub fidelity: DiffFidelity,            // Exact | ByteCoarse
}
```

**Tier 1 — structural pre-pass (same byte pool only).** When baseline
and buffer share history (the saved baseline), run the existing
`diff_piece_trees` identity walk first. Its output bounds the changed
region; hand *only that region* to tier 2. This keeps the O(1)
`!modified` / `Arc::ptr_eq` fast paths that make per-frame use free,
and it fixes the current semantic wart: today the native stripe is
content-accurate only under the 64 KiB verify cap and identity-based
above it, while everything git-derived is content-accurate. With tier 2
scoped to the (usually tiny) changed region, saved-diff becomes
content-accurate at every size for ~the same cost, and `MAX_VERIFY_BYTES`
disappears.

**Tier 2 — patience line diff over line sources.** Move
`fresh-plugin-runtime/src/diff.rs` down into `fresh-core` (it depends
only on `LineDiffHunk`) and generalize its input from `&str` to an
interning-friendly line source, so it can consume two buffers'
`iter_lines_from` (which already yields `(byte_offset, content,
line_number)` without per-line O(log n) lookups) instead of
materialized whole-file strings. The plugin-facing `computeLineDiff`
keeps its pure-string signature and becomes a thin wrapper — its
robustness properties (never refuses, bounded fallbacks) are the
engine's properties.

**Coordinates.** Line hunks are canonical; byte ranges are a derived
projection computed once (line-start descents are ~0.5 µs). In
large-file mode before the line-feed scan completes, tier 2 can't run —
`fidelity: ByteCoarse` carries tier 1's byte ranges alone, which is
exactly the degradation the render path already handles (`MarkerBasis::
Bytes`, `Option<line>` everywhere). No third coordinate system is
introduced, and `MarkerBasis` keeps reconciling at projection time.

## Baselines: registration, generations, invalidation

```rust
pub enum BaselineSource {
    Saved,                       // Persistence::saved_root — free
    Disk,                        // buffer.filesystem().read — file watcher invalidates
    GitRef { ref_spec: String }, // resolved to an OID at registration
    GitIndex,                    // `git show :0:<path>` — .git/index watch invalidates
}
```

A `BaselineStore` (per window, beside `composite_buffers`) owns
`BaselineId → { source, buffer: BufferId /* hidden */, generation }`.
Every invalidation is a watch the system already performs, made
authoritative in one place instead of per-plugin:

| Source | Invalidated by | Mechanism that already exists |
|---|---|---|
| Saved | save | `save_state_version` (`persistence.rs:54`) |
| Disk | file change | external-file watcher |
| GitRef | HEAD/ref movement | the `logs/HEAD` reflog watch live_diff.ts registers today — moved host-side, registered once per repo |
| GitIndex | staging | new watch on `.git/index` (only genuinely new piece) |

Resolving `ref_spec → OID` at registration makes GitRef baselines
immune to branch movement while cached; a generation bump re-resolves.
Baselines are dropped with their buffer (`buffer_closed`), the same
lifecycle composite source panes already follow — no new leak class.

## The cache — fixing today's 3×-per-frame recompute

One memo per (buffer, baseline):

```
key = (buffer.version(), buffer.save_state_version(), baseline.generation)
val = Arc<DiffResult>
```

This is the `ProjectionKey` idiom (`scrollbar_marker.rs:356`) promoted
from the scrollbar's private projection cache to the diff itself — the
version *pair* is load-bearing (a save changes diff meaning without a
content-version bump; `persistence.rs:47` documents the bug class).
Consumers must keep the scrollbar's probe-cache-before-computing-inputs
ordering (`scrollbar.rs:201`): on a steady-state frame nothing is
computed at all. The immediate win, before any new feature: gutter,
scrollbar and plugin snapshot stop invoking `diff_since_saved`
independently and read the one memo.

## Plugin API

Nothing large crosses the bridge in either direction; the existing
pure `computeLineDiff(oldText, newText)` stays for ad-hoc use.

```ts
registerDiffBaseline(bufferId, source: BaselineSource): Promise<BaselineId>
diffAgainstBaseline(bufferId, baselineId):
    Promise<{ revision: number; fidelity: "exact"|"byteCoarse";
              hunks: LineDiffHunk[]; byteRanges: [number, number][] }>
getBaselineLines(baselineId, startLine, count): Promise<string[]>
releaseDiffBaseline(baselineId): void   // also auto-released on buffer close
```

- `diffAgainstBaseline` returns the buffer revision the hunks were
  computed against; a plugin rendering decorations re-checks it — the
  coherence gap that made the pure-string API attractive is closed by a
  token instead of a copy.
- `getBaselineLines` is how live_diff renders virtual deletion lines:
  only *changed* old-side lines cross the bridge — bridge traffic
  proportional to the diff, not the file.
- Async because registration may spawn git; per the git_index pattern
  the spawn runs off-loop and lands via the event loop.

## What each consumer becomes

| Consumer | Today | With the service |
|---|---|---|
| Unsaved stripe (gutter+scrollbar+snapshot) | 3× uncached `diff_since_saved`, identity semantics above 64 KiB | one cached `DiffResult` vs `Saved`; content-accurate at any size |
| live_diff.ts | full texts over the bridge per recompute; JS-side ref cache + reflog watch | `registerDiffBaseline` + `diffAgainstBaseline` + line slices; keeps only policy (mode choice, colors, word-level refinement) |
| git_gutter.ts | spawns + parses `git diff` (disk vs HEAD) | `diffAgainstBaseline` between two baselines (Disk vs GitRef "HEAD") — the service diffs any two TextBuffers, a baseline pair included |
| audit_mode.ts / composite alignment | parses `git diff` text into `CompositeHunk`s | feeds `DiffResult.hunks` straight into `LineAlignment::from_hunks` (same shape; per-line `ops` derivable from hunk counts) |
| diff_nav.ts | three ad-hoc sources | one: enumerate registered baselines' hunks |
| `model/line_diff.rs` | dead | deleted |

## Sequencing

1. **Cache the existing saved-diff** behind the version-pair memo and
   point the three native consumers at it. Pure refactor, immediate
   frame-cost win, no API change.
2. **Move the patience engine to `fresh-core`**, generalize to line
   sources, add the tier-1→tier-2 composition for the saved baseline
   (removes `MAX_VERIFY_BYTES` semantics split). Delete
   `model/line_diff.rs`.
3. **BaselineStore + plugin API**; migrate live_diff.ts (drops
   full-text bridge crossings and its JS reflog watch).
4. **Migrate git_gutter and audit_mode**; composite alignment consumes
   service hunks; optional later: word-level refinement host-side, and
   an incremental `LineAlignment` update.

Each step is independently shippable and testable with the patterns
already in the tree: the reconstruction-invariant unit tests from
`diff.rs`, e2e tests that assert rendered output only, and the
`ProjectionKey`-style cache keyed tests.
