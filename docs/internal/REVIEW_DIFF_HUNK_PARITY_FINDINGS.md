# Review Diff (hunk-parity) — Interactive Testing Findings

Status: open issues found by driving the in-panel hunk-parity Review Diff
interactively in tmux (unified + side-by-side, single- and multi-file
changesets, with `capture-pane -e`). Companion to
`REVIEW_DIFF_HUNK_PARITY_UX_DESIGN.md`. None of these are fixed yet.

Read CONTRIBUTING.md before starting. The plugin lives in
`crates/fresh-editor/plugins/audit_mode.ts`; composite-buffer host code in
`crates/fresh-editor/src/app/composite_buffer_actions.rs`,
`crates/fresh-editor/src/app/input_helpers.rs`, and the composite renderer
in `crates/fresh-editor/src/view/ui/split_rendering/orchestration/mod.rs`.

---

## A. Broken behavior (functional bugs)

### A1. Side-by-side: keyboard cursor movement is completely frozen
`Up`/`Down`/`j`/`k`/`PageUp`/`PageDown` do nothing in split view — the
cursor stays at `Ln 1, Col 1` and the composite never scrolls, so anything
below the first viewport is unreachable. Unified mode moves fine.

Path is confirmed correct up to the host: `Down → review_nav_down →
editor.executeAction("move_down") → handle_action → catch-all →
apply_action_as_events`, which checks `is_composite_buffer(active_buffer())`
(true — status bar shows `*Review: <file>*`) and calls
`handle_composite_action(MoveDown)`. That function bails early at:

```rust
let split_id = …active_split();
let _view_state = self.active_window()
    .composite_view_states.get(&(split_id, buffer_id))?;   // <-- likely None
```

The render path lazily creates the view-state keyed by `(split_id,
buffer_id)` for the leaf the composite renders in
(`orchestration/mod.rs` ~L429). Strong suspicion: `active_split()` returns a
different leaf than the diff-panel leaf the composite is rendered under, so
the lookup misses and movement is dropped. **Next step:** add tracing in
`handle_composite_action` for `active_split` vs the keys present in
`composite_view_states`, confirm the mismatch, and resolve the correct leaf
(or create/sync the view-state under `active_split`).

### A2. Hunk navigation `n` / `p` does nothing (unified focus mode)
Cursor never moves between hunks regardless of position (pressed repeatedly
from `Ln 6`, stayed at `Ln 6`). Likely the hunk-row model is computed
against the full multi-file stream while the focus-only buffer renders only
the focused file's body, so the target rows don't line up with the rendered
buffer. Verify `review_next_hunk`/`review_prev_hunk` row math against the
focus-only `buildDiffLines` output.

### A3. Stash review hangs
`Review Stash` → `start_review_stash` → `bootstrapRangeReview` with
`git stash show -p` times out (180s). Reproduced locally on Linux and on all
three CI runners (macOS / Ubuntu / Windows) via
`e2e::plugins::review_diff_hunk_parity::test_review_stash_shows_stashed_diff`
— this single bug is what is currently reddening PR #2271's CI. Likely hangs
interactively too, not just under test.

### A4. `[⚠]` warning counter increments on composite Tab
In side-by-side, pressing `Tab` bumps the editor warning/error indicator
(`[⚠ 1]` → `[⚠ 2]` …), suggesting the fold handler throws in composite
context. Investigate `review_toggle_file_collapse` when the diff panel hosts
a composite buffer.

---

## B. Navigation / scroll-positioning gaps

### B1. Navigating to a file does not scroll that file to the top
`,` / `.` collapses the other files and expands the target, but the view
stays anchored at the `UNSTAGED` header — the focused file's diff appears
several rows down and the cursor stays at `Ln 1`. Expectation: moving to a
file should bring that file to the top (best-fit) of the diff viewport and
place the cursor there.

### B2. Sidebar does not auto-scroll to the selected file
With a changeset larger than the sidebar height (verified at 27 files),
navigating past the visible window leaves the selection off-screen with no
highlight — the FILES panel never scrolls to follow `filesCurrentKey`.

### B3. Hunk nav and side-by-side need the same scroll-to-position
The best-fit-to-top behavior from B1 should also apply when jumping hunks
(`n`/`p`) and in side-by-side. Currently unmet (compounded by A1/A2).

---

## C. Focus model

### C1. No `Tab` focus cycling between panels
Expectation: `Tab` cycles file list → OLD pane → NEW pane → back to file
list (and the diff pane in stacked view). Currently `Tab` = fold; in
side-by-side the review-mode `Tab` binding shadows the composite's native
`InsertTab → composite_focus_next`, so pane focus never switches.

### C2. Cursor keys should act on the focused panel
`Up`/`Down`/`PageUp`/`PageDown` should drive whichever panel has focus.
Today arrows only ever target the diff (and in side-by-side not even that —
see A1); the file list is reachable only via `,`/`.` and comments only via
backtick. There is no "focused panel receives cursor keys" concept.

### C3. No visible focus indicator
Nothing shows which pane currently holds keyboard focus, so even the
existing backtick→comments focus switch is invisible.

---

## D. Discoverability / polish

### D1. Top-bar legend omits the primary navigation keys
The two persistent toolbar rows show `n/p`, `s/u/d`, `Tab`, `Enter`, etc.,
but not `,`/`.` (file nav) or `1`/`2`/`0` (layout toggle) — exactly the keys
a newcomer needs. They appear only in the `?` help screen.

### D2. `?` help hijacks the view
Help opens as a new `*Review Keys*` tab and steals focus; review-mode keys
stop working there and there is no "press q/Esc to return" affordance.
Should be an overlay, or restore focus to the review on close.

### D3. Help text vs. behavior mismatch
Help says "Tab — fold the file under the cursor," but `Tab` actually folds
the **hunk** under the cursor.

---

## E. Validated as working (no action needed)

- File nav `,`/`.` updates the sticky header + sidebar highlight in both
  layouts; clamps at the top boundary (no wrap).
- Up/Down + cursor-line overlay in **unified** mode.
- Side-by-side composite rendering (OLD/NEW alignment is correct).
- **No `[No Name]` tab churn and no flicker** across repeated side-by-side
  file switches (the earlier regression is fixed).

---

## Suggested fix order

1. **A1** — split-view scrolling (core of side-by-side).
2. **A2 / B1 / B3** — scroll-to-position for file and hunk nav (shared
   mechanism: compute a cursor/scroll target and apply per panel).
3. **C1 / C2 / C3** — `Tab` focus model + focus indicator.
4. **A3** — stash hang (unblocks PR CI).
5. **B2, D1–D3** — sidebar follow + discoverability polish.

A1, A2, and B1 likely share one underlying fix in how the cursor/scroll
target is computed and applied per panel.
