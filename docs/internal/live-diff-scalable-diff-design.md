# Live Diff: scalable diff design (kill the "file too large" refusal)

Status: implemented — with one deviation from the original proposal:
per review feedback, no new dependency was added. `computeLineDiff` is
backed by an in-house patience diff (`fresh-plugin-runtime/src/diff.rs`:
line interning → unique-line anchors chained by LIS → recursion between
anchors, with a small dense-LCS fallback for anchor-free chunks and a
coarse whole-chunk replacement past `FALLBACK_LCS_CELLS`) instead of the
`imara-diff` crate discussed below. The API shape, degraded render
levels, and test strategy are as proposed. The API also ended up
synchronous rather than promise-based: it executes on the plugin
runtime's own thread — the same thread that previously ran the JS DP —
so a native sub-millisecond call needs no async plumbing, and the editor
loop was never involved.
Motivated by a reproduced refusal: checking out `HEAD~1200`'s `main.rs`
→ 24.9M DP cells → `lineDiff` bails → plugin renders nothing.

## Why this happens at all

`live_diff.ts` implements the line diff as a dense LCS DP table:
O(m·n) time *and* memory over the post-prefix/suffix-strip middle,
with `MAX_DP_CELLS = 16M` as the safety valve. Two structural problems:

1. **The algorithm is the wrong complexity class.** Every serious diff
   implementation (git, jgit, gitoxide) is output-sensitive — Myers
   O((N+M)·D) — or anchor-based (patience/histogram, ~O(N log N) on
   real code). Dense LCS is quadratic regardless of how similar the
   inputs are. A 1200-commit drift is *large* but nowhere near
   pathological; git diffs this exact pair in ~1 ms.
2. **The failure mode is total.** When the cap trips, the plugin
   clears every decoration and refuses. (The comment at
   `live_diff.ts:75-79` promises a degraded gutter-only path; it was
   never implemented.)

A robust fix removes both: an algorithm whose worst case is acceptable,
and a pipeline whose degradation is *local* (less detail) instead of
*global* (nothing).

## Recommendation

### 1. Move the line diff into the Rust host, expose it as a plugin API

Add a host function and delete the plugin's dense-DP `lineDiff`
entirely:

```ts
// fresh.d.ts
interface LineDiffHunk {
  oldStart: number; oldCount: number;  // 0-indexed, old side
  newStart: number; newCount: number;  // 0-indexed, new side
}
/** Line-level diff of two texts. Histogram algorithm, native. */
computeLineDiff(oldText: string, newText: string): Promise<LineDiffHunk[]>;
```

- Implement with the **`imara-diff`** crate (gitoxide's engine:
  interned tokens, Histogram algorithm with Myers fallback for
  high-occurrence regions). Alternative: `similar` (Patience/Myers,
  broader API incl. word/char modes). Either turns the repro case
  into sub-millisecond work; `imara-diff` is the faster and smaller
  dependency, and Histogram gives the best hunk quality on code.
- Register via the existing `#[plugin_api(async_promise, ...)]`
  machinery in `crates/fresh-plugin-runtime/src/ts_export.rs` — the
  same pattern as `spawnProcess`/`delay`. Run the diff off the editor
  loop (worker thread, as `spawnProcess` does) so even worst-case
  inputs never block a frame.
- The hunk shape above is exactly what `opsToHunks` + `fillOldLines`
  produce today (`oldStart/oldCount` replaces the `_oldStart/_oldEnd`
  stash), so `live_diff.ts` integration is a small mechanical change:
  `refineHunks`, rendering, `diff_nav` view-state publishing all stay
  as they are.
- Why host-side instead of a better JS algorithm: the plugin runtime
  is QuickJS with **no typed arrays** — every DP/vector algorithm
  pays boxed-`Array` costs, and Myers' worst case (~10⁸ int ops for
  a 5k×6k full rewrite) is tens of milliseconds native but seconds
  in QuickJS. Native also makes the engine reusable: `git_log`,
  review-diff tooling, and future plugins get one battle-tested diff
  instead of per-plugin reimplementations.

Sizing: `getBufferText` already copies the buffer to JS per recompute,
so passing both texts over the bridge adds nothing new. (A later
optimization — `diffBufferAgainstText(bufferId, refText)` reading the
rope directly host-side — halves the copies, but is not needed for
correctness.)

### 2. Make degradation local, never total

With Histogram the caps almost never trip, but the pipeline should
still be a total function:

- **Delete `MAX_DP_CELLS`** (dead concept with the host diff).
- **Keep a generous `MAX_DIFF_LINES`** (say the current 100k) as a
  render-sanity guard, but change its behavior: instead of clearing
  everything, do the O(N) prefix/suffix scan and render *one* coarse
  "replaced block" hunk (gutter `~` on the changed span + one
  scrollbar streak), with status
  `"Live Diff: change too large — showing outline only"`. That is the
  degraded path the code comment already promises.
- **Cap rendering, not diffing.** A 10k-line drift produces thousands
  of hunks; the expensive part is virtual lines + word overlays, not
  the diff. Add a budget (e.g. > 2,000 virtual lines): above it,
  render gutter indicators, backgrounds and scrollbar markers but
  skip virtual old-content lines and word refinement, with a status
  noting simplified rendering. Below it, render exactly as today.
- **Budget the refinement pass.** `refineHunks` runs a per-pair char
  LCS (up to 2000×2000 = 4M boxed-array cells *per pair*). Fine for
  a handful of pairs; slow for a 1200-commit drift with hundreds.
  Add a global per-recompute budget (e.g. 10M cells, spent
  first-hunk-first or viewport-first); pairs past the budget keep
  plain `modified` rendering without word underlines. (If `similar`
  is chosen in step 1, its char-diff mode can replace this JS DP
  outright.)
- **Fix the message.** `status.too_large` says the *file* is too
  large; after this change the only remaining message concerns the
  *change* being summarized, and should say so (all 14 locales in
  `live_diff.i18n.json`).

### 3. Tests that pin the behavior

- **E2E regression for the repro**: build a fixture repo whose HEAD
  holds file A and whose working file is a ~5k-line variant sharing
  <1% of lines (synthesize it — don't depend on real history), enable
  live diff, assert gutter indicators exist and no "too large" status
  is shown. This is precisely the scenario that today renders nothing.
- **Parity test**: assert `computeLineDiff` hunks match
  `git diff --no-index -U0` on the same pair (precedent:
  `review_diff_hunk_parity.rs`).
- **Perf guard**: `computeLineDiff` on the synthetic worst pair
  completes within a bound (native Histogram: single-digit ms).
- **Degraded-path test**: force the render budget low, assert gutter +
  scrollbar still render while virtual lines are absent.

## Alternatives considered

- **Pure-plugin patience/histogram + bounded Myers in TS** (line
  interning to ints, unique-anchor recursion, block-replace fallback
  for anchor-free regions). Fixes the refusal with zero API changes
  and is the right fallback if a host API is off the table — but it
  keeps worst-case diff cost on the QuickJS thread and adds ~300
  lines of subtle algorithmic TS that the Rust ecosystem already
  provides hardened. Not preferred.
- **Shelling out to `git diff --no-index` per recompute**: needs two
  temp-file writes per 75 ms debounce tick, couples the hot path to
  process spawn latency (and trust-mode/PATH concerns), and produces
  output that must be reparsed. Reference *loading* already uses git;
  the per-keystroke hot path should not.
- **Incremental hunk maintenance on buffer edits** (adjusting hunks
  in place instead of re-diffing): significant complexity for no need
  once the full diff is sub-millisecond. Explicit non-goal.

## Suggested sequencing

1. `computeLineDiff` host API + `imara-diff` dependency + parity/perf
   tests.
2. `live_diff.ts`: swap `lineDiff` for the API, delete DP code and
   `MAX_DP_CELLS`, reword statuses.
3. Degraded render paths + budgets (virtual-line cap, refinement
   budget, coarse-block fallback) + degraded-path e2e.
4. Adopt the same API in other diff-consuming plugins as opportunity
   arises.

Steps 1–2 alone make the reported scenario a non-issue; step 3 makes
the plugin total under any input.
