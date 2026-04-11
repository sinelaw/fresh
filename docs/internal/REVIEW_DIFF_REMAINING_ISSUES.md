# Review Diff Mode — Remaining Issues

Status: 8 of 9 bugs from REVIEW_DIFF_COMBINED_UX_REPORT.md are fixed with
passing e2e tests. This document describes the 3 remaining issues for a
follow-up session.

Read CONTRIBUTING.md before starting. Run existing tests with:
```
cargo test --package fresh-editor --test e2e_tests review_diff_ux_bugs
cargo test --package fresh-editor --test e2e_tests diff_cursor
cargo test --package fresh-editor --test e2e_tests audit_mode
cargo test --package fresh-editor --test e2e_tests file_explorer
```
All must pass (the one `#[ignore]`'d test is BUG-10 below).

---

## Issue 1: BUG-10 — Toolbar "Export" label truncated (COSMETIC)

### Problem

When the File Explorer sidebar is open and the Review Diff viewport is
narrow (~100 cols), the toolbar hint `e Export` is truncated to `e E` or
missing entirely. The other hints (`s Stage`, `u Unstage`, etc.) render
fine because they come first.

### Where to look

The toolbar is built by `buildToolbar(width)` in
`crates/fresh-editor/plugins/audit_mode.ts`. It takes `state.viewportWidth`
and lays out hint groups separated by `│`. The last group (`e Export`,
`r Refresh`, `q Close`) gets cut when the total width exceeds the panel.

### What to do

Either truncate labels gracefully (e.g. `e Exp` or just `e`) when space
is tight, or drop low-priority hints entirely. The toolbar is a single
`TextPropertyEntry` line in the "toolbar" fixed-height panel.

### Test

There is an `#[ignore]`'d e2e test at:
`crates/fresh-editor/tests/e2e/plugins/review_diff_ux_bugs.rs` —
`test_bug10_toolbar_export_label_truncated`. Un-ignore it, make it pass.

---

## Issue 2: BUG-2 resize — real-world timing edge case

### Problem

The e2e test for terminal resize passes (the harness processes async
plugin commands synchronously in its tick loop), but manual tmux
reproduction sometimes still shows corruption after a resize cycle.
The plugin's `onReviewDiffResize` handler fires asynchronously via the
plugin thread, so the first post-resize render may happen before the
handler has rebuilt the panel content.

### Where to look

- `Editor::resize()` in `crates/fresh-editor/src/app/mod.rs:3778` — fires
  the `resize` hook asynchronously via `plugin_manager.run_hook`.
- `onReviewDiffResize` in `crates/fresh-editor/plugins/audit_mode.ts:1252`
  — clears `state.diffCache` and calls `updateMagitDisplay()`.
- The plugin commands (`setPanelContent`) are sent back to the editor
  via an async channel and processed on the next `process_async_messages`
  call, which happens in the event loop's tick.

### What to do

Investigate whether `Editor::resize()` should synchronously wait for the
plugin resize hook to complete before returning (so the next render sees
the updated content). Alternatively, the rendering path could detect that
a buffer group's panel content is stale after a resize and skip rendering
until the plugin has refreshed it. Another option: have the Rust-side
buffer group layout code handle resize natively without depending on the
plugin to rebuild content — the panel content is text that doesn't change
on resize, only the layout dimensions change.

### How to reproduce manually

```bash
# Build the editor
cargo build --package fresh-editor

# Create a test repo (see /tmp/review-diff-test setup in the session)
# Open the editor, Ctrl+P -> "Review Diff"
# In another terminal:
tmux resize-window -t <session> -x 80 -y 24
sleep 1
tmux resize-window -t <session> -x 120 -y 40
# Observe: toolbar/header/content may be missing
```

---

## Issue 3: `setBufferCursor` doesn't work for buffer group panels

### Problem

`editor.setBufferCursor(bufferId, byteOffset)` does not move the cursor
in buffer group panel buffers. The Rust-side `handle_set_buffer_cursor`
finds the inner leaf's `SplitViewState` and calls
`view_state.cursors.primary_mut().move_to(position)`, but the status bar
and `cursor_moved` events reference the **effective active split** (the
outer split), not the inner leaf. So the cursor moves in the wrong view
state.

This forced the BUG-4 fix (`jumpDiffCursorToRow` in audit_mode.ts) to
use an O(n) workaround: call `editor.executeAction("move_down")` in a
loop, which correctly routes through the focused panel. For large diffs
(1000+ lines) this is slow.

### Where to look

- `handle_set_buffer_cursor` in
  `crates/fresh-editor/src/app/plugin_commands.rs:609` — walks grouped
  subtrees to find the inner leaf, but updates a cursor that isn't the
  one the rendering/status bar reads from.
- `effective_active_split()` in `crates/fresh-editor/src/app/mod.rs:1904`
  — returns the inner leaf when a group panel is focused, but
  `handle_set_buffer_cursor` compares against
  `split_manager.active_split()` (the outer split) for the `is_active`
  check.
- `jumpDiffCursorToRow` in
  `crates/fresh-editor/plugins/audit_mode.ts:1830` — the O(n) workaround.

### What to do

Fix `handle_set_buffer_cursor` to use `effective_active_split()` instead
of `split_manager.active_split()` when checking `is_active`. Or make the
status bar and cursor_moved events read from the inner leaf's view state
when a buffer group panel is focused. Once `setBufferCursor` works for
panel buffers, revert `jumpDiffCursorToRow` to the direct byte-offset
approach.

### Test

After fixing, update `test_bug4_hunk_navigation_n_does_not_move_cursor`
to verify cursor moves to the correct line (not just "different from
before"). Also add a targeted test that calls `setBufferCursor` on a
panel buffer and asserts the status bar line number updates.
