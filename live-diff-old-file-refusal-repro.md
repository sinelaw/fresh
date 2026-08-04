# Live Diff refuses to diff an old checkout of a large file — reproduction

## Report

> live_diff refuses to work on diff if I check out very old version of main.rs
> and enable live diff mode on that buffer:
> `git checkout HEAD~1200 -- crates/fresh-editor/src/main.rs`

## Reproduced

Confirmed interactively (fresh built from `1505e2c`, driven in tmux):

1. In a worktree at HEAD, ran
   `git checkout HEAD~1200 -- crates/fresh-editor/src/main.rs`
   (old version: 5118 lines; HEAD version: 6094 lines).
2. Opened the file: `fresh crates/fresh-editor/src/main.rs`.
3. Command palette → **Live Diff: Toggle (Buffer)**.
4. Result: status bar shows **“Live Diff: file too large for live diff”**,
   and *no* diff decorations render — no gutter `+`/`~`/`-` glyphs, no
   virtual deletion lines, no scrollbar markers — despite the buffer
   differing from HEAD by thousands of lines.

Control in the same session: swapping the on-disk file to the `HEAD~50`
version (buffer auto-reloads) and running **Live Diff: Refresh** renders
`+` markers correctly from line 3353 (first differing line) onward.
Swapping back to the `HEAD~1200` version makes all markers vanish and the
“file too large” status return. So the failure is specific to the size of
the *diff*, not to anything about the buffer or mode plumbing.

## Root cause

`crates/fresh-editor/plugins/live_diff.ts` computes a line-level LCS with a
dense DP table over the diff's middle (common prefix/suffix stripped
first). `lineDiff()` bails out and returns `null` when
`(m + 1) * (n + 1) > MAX_DP_CELLS` (16,000,000 — `live_diff.ts:83`).

For this file pair the common prefix is only 4 lines and the common suffix
591, leaving a middle of 4523 × 5499 lines ≈ **24.9M cells > 16M**, so the
DP bails. The `null` handler in `onePass()` (`live_diff.ts:1123‑1130`)
then clears every decoration, publishes `live_diff_hunks = null`, and sets
`status.too_large`.

Notes:

- The `MAX_DIFF_LINES = 100_000` cap is *not* the trigger here (both sides
  are ~5–6k lines). It is the O(m·n) DP-cell cap that bites; for this file
  even `HEAD~200` (5162 × 5493 middle ≈ 28.4M cells) already exceeds it.
  Only ~50 commits of drift stay under the cap.
- The status message (“file too large for live diff”) is misleading: the
  file is well within the documented size limits; it is the *change* that
  is too large for the quadratic LCS.
- The comment at `live_diff.ts:75‑79` says “Gutter glyphs still render via
  a degraded path” when the caps are exceeded, but no such degraded path
  exists in the code — both the `MAX_DIFF_LINES` and the DP-cell bailouts
  clear everything and render nothing.

## Repro script (headless verification of the bail)

```sh
git show HEAD~1200:crates/fresh-editor/src/main.rs > /tmp/old.rs
git show HEAD:crates/fresh-editor/src/main.rs      > /tmp/new.rs
# strip common prefix/suffix like lineDiff(), then:
#   middle m=4523, n=5499  →  (m+1)*(n+1) = 24,882,000 > MAX_DP_CELLS (16,000,000)
```

## Possible directions for a fix (not implemented here)

- Replace the dense-DP LCS with Myers O(ND) / patience diff, or shell out
  to `git diff --no-index` for the line-level pass — an old-revision
  buffer produces a large but far-from-worst-case edit script.
- Or implement the degraded path the comment already promises: coarse
  gutter-only marking (e.g. per-block “modified” from a cheap heuristic)
  when the DP would be too big.
- Reword `status.too_large` to say the *diff* is too large.
