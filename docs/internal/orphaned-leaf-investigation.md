# Orphaned split-leaf blank-pane bug (#1939 class)

## Symptom

In a multi-window / multi-session workflow, restoring a window shows a blank
editor pane with no prompt, even though the status bar reports a real buffer
(and, for a terminal pane, the underlying PTY is alive). The log floods every
frame with:

```
effective_active_pair: split manager's active leaf points at a BufferId missing
from window.buffers (issue #1939). Falling back to any live buffer; the split
tree is in an inconsistent state and should be repaired
  stale_buffer_id=BufferId(4) fallback_buffer_id=BufferId(5) active_split=LeafId(SplitId(0))
```

correlated with:

```
Applying workspace layout with 1 split states
Removing orphaned empty unnamed buffer BufferId(4)
```

## Root cause (hypothesis C confirmed)

The trigger is restoring a window whose **saved active tab is an empty
`[No Name]` buffer** that sits alongside at least one *resolvable, non-file*
tab (a terminal in the original orchestrator repro, or any content-bearing
unnamed buffer).

Walking the restore path in `apply_workspace_layout`
(`crates/fresh-editor/src/app/workspace.rs`):

1. A fresh window is seeded with an empty `[No Name]` buffer via
   `seed_initial_layout` (call it `BufferId(4)`).
2. The empty `[No Name]` active buffer was **never persisted** — empty unnamed
   buffers get no recovery id (`save_all_recovery` only assigns ids to
   recovery-pending buffers) and are excluded from `unnamed_buffers`
   (`total_bytes() == 0`). So at capture time it is *not* serialized as a tab,
   and the serialized `active_tab_index` is `None`.
3. A **non-file** resolvable tab is the key ingredient: unlike `open_file`
   (which replaces the empty seed *in place*, so it can never be orphaned),
   restoring a terminal / content-unnamed tab creates a brand-new buffer and
   leaves the seed untouched. So the seed survives as a separate live buffer.
4. `restore_split_node` resolves the saved active leaf's buffer to
   `unwrap_or(self.active_buffer())` — the seed `BufferId(4)` — and calls
   `set_pane_buffer(leaf, seed)`.
5. `restore_split_view_state` clears `open_buffers`, re-adds the resolvable
   tab(s) (the seed is *not* among them), then tries to resolve the saved
   active tab. Because `active_tab_index` is `None` (or points at the
   unrecoverable unnamed tab), `active_buffer_id` stays `None`, so the final
   `set_split_buffer` is skipped. **The leaf is left pointing at the seed,
   which is absent from the tab list.**
6. `clean_orphaned_buffers` builds its referenced set from
   `buffer_tab_ids()` only, sees the seed in no tab list, and removes it.
   The split-manager leaf now dangles at a dead `BufferId`.

The render path reads the leaf's buffer id directly and paints blank;
`effective_active_pair` independently falls back to another live buffer for
status-bar queries — hence "status bar shows a real buffer, pane is blank."
The #1939 `effective_active_pair` guard only papers over the inconsistency for
status queries; it doesn't repair the dangling leaf.

## Fix (right layer: repair the invariant at its source)

In `restore_split_view_state`, after attempting to resolve the saved active
tab, if `active_buffer_id` is still `None`, fall back to the first surviving
tab (`view_state.buffer_tab_ids().next()`). This routes through the existing
`switch_buffer` + `set_split_buffer` calls, so the split-manager leaf, the
`SplitViewState.active_buffer`, and the tab list all agree — and the seed is
then *correctly* orphan-removed because nothing references it.

This is preferred over:

- **(A) Teaching `clean_orphaned_buffers` to treat leaf buffers as referenced.**
  That would *keep* the seed `[No Name]` even in normal restores (leaking a
  stray scratch buffer) and would only mask the underlying leaf/tab-list
  disagreement rather than fix it.
- **Widening the `effective_active_pair` fallback into the render path.** That
  hides the inconsistency instead of preventing it, and the leaf would still
  point at a dead buffer.

The fix preserves the existing `#1278` behavior (when *all* tabs were
unresolvable, `open_buffers` is re-seeded with the leaf's own buffer above, so
the fallback keeps that buffer) and is independent of the orchestrator plugin.

## Regression test

`test_restore_orphaned_active_unnamed_tab_renders_surviving_tab` in
`crates/fresh-editor/tests/e2e/workspace.rs` reproduces the bug PTY-free using a
content-bearing unnamed buffer as the resolvable background tab. It drives a
real save/restore round-trip and asserts on rendered output (the surviving
tab's content appears, the pane is not blank) plus a model invariant (the raw
active-leaf buffer is present in `window.buffers`). It fails without the fix
(blank `[No Name]` pane) and passes with it.

## Out of scope / adjacent observations

- `capture_workspace` serialized an empty active unnamed buffer into
  `external_files: [""]` (an empty-string path). Harmless in restore (it's a
  no-op path) but is a latent oddity worth a separate cleanup.
- The `effective_active_pair` warning + fallback is now effectively defensive
  belt-and-suspenders; it remains valuable for surfacing any *future*
  invariant violation, so it is intentionally left in place.
