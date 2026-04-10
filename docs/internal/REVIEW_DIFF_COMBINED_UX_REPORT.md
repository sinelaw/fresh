# Review Diff Mode -- Combined UX Report

**Sources:**
- `claude/audit-review-diff-mode-nGTV4` -- UX Audit & Bug Report
- `claude/test-review-diff-mode-7xnTV` -- Manual Testing Bug Report

**Date:** 2026-04-10
**Editor version:** Fresh 0.2.22 (debug build, commit 9ab13b3)
**Test environments:** tmux, 120x35 and 160x45 terminals, Linux 4.4.0

---

## Executive Summary

Two independent UX testing sessions were conducted on Review Diff mode. After
cross-referencing and deduplicating, **9 distinct bugs** were identified, sorted
below from most dangerous to least.

---

## Bugs by Criticality

### Tier 1 -- Hangs / Unrecoverable State

#### BUG-5: Side-by-Side Drill-Down Hangs for Deleted Files

| Field | Value |
|-------|-------|
| **Criticality** | **HANG** |
| **Reported in** | Bug Report #3 |
| **E2E test** | `test_bug5_deleted_file_drill_down_hangs` |

Pressing `Enter` on a deleted file shows "Loading side-by-side diff..."
**indefinitely**. The user is stuck -- no timeout, no error, no way out except
closing the tab. This is the only bug that produces a UI hang.

**Root Cause:** `review_drill_down()` calls `editor.readFile(path)` for the
new version. For a deleted file the file doesn't exist, `readFile` returns
null, the function exits early but the "Loading..." status persists and the
view is never created nor cleaned up.

**Fix:** Detect deleted-file status before calling `readFile`. Show OLD content
on the left and an empty pane on the right. Clear the loading status on error.

---

#### BUG-2: Terminal Resize Destroys Review Diff Layout (Unrecoverable)

| Field | Value |
|-------|-------|
| **Criticality** | **UNRECOVERABLE CORRUPTION** |
| **Reported in** | Bug Report #2 |
| **E2E test** | `test_bug2_resize_destroys_review_diff_layout` |

Resizing the terminal while in Review Diff mode causes the toolbar, header,
separator, and content to disappear. The layout does **not recover** even after
resizing back. Neither `r` (refresh) nor navigation keys restore it. Pressing
`Home` causes diff content to render *below* the status bar.

**Fix:** The resize event handler must trigger a full re-layout of all Review
Diff panels. The design doc specifies listening to the `resize` event but this
path is broken.

---

### Tier 2 -- Entire Feature Surface Broken

#### BUG-1: CompositeInputRouter Is Dead Code -- Side-by-Side Keyboard Nav Broken

| Field | Value |
|-------|-------|
| **Criticality** | **FEATURE INOPERABLE** |
| **Reported in** | Audit #3, Audit #4, Bug Report #6 |
| **E2E tests** | `test_bug1_side_by_side_vim_keys_produce_editing_disabled`, `test_bug1_side_by_side_escape_does_not_close`, `test_bug1_side_by_side_tab_does_not_switch_pane` |

All vim-style keys (`j`/`k`/`Tab`/`n`/`p`/`Escape`) in the side-by-side diff
view produce "Editing disabled in this buffer". Only `q` and arrow keys
partially work.

**Root Cause:** `CompositeInputRouter` (`crates/fresh-editor/src/input/composite_router.rs`)
implements the full routing but is **never called** from `app/input.rs`. Key
events fall through to the standard text-editing path, which rejects them.

**Fix:** In `app/input.rs`, after mode binding resolution, check if the active
buffer is composite. If so, route through `CompositeInputRouter::route_key_event()`
and dispatch the `RoutedEvent`. The code is already written and unit-tested --
it just needs to be connected.

**Also resolves:** BUG-9 (viewport scroll).

---

### Tier 3 -- Silent Failures (Features Don't Work, No Error)

#### BUG-3: File Explorer Steals Focus from Review Diff on Launch

| Field | Value |
|-------|-------|
| **Criticality** | **SILENT FAILURE** |
| **Reported in** | Audit #1, Bug Report #1 |
| **E2E test** | `test_bug3_file_explorer_steals_review_diff_keys` |

When File Explorer is open (default state), opening Review Diff does not
transfer focus. All review-mode keybindings silently fail -- `j` triggers
the File Explorer's quick-search instead.

**Workaround:** `Ctrl+E` toggles focus, but this is not discoverable.

**Fix:** In `start_review_diff()`, explicitly move focus to the review diff
files panel after creating the buffer group.

---

#### BUG-4: Hunk Navigation (`n`/`p`) Non-Functional in Diff Panel

| Field | Value |
|-------|-------|
| **Criticality** | **SILENT FAILURE** |
| **Reported in** | Audit #5, Bug Report #4 |
| **E2E test** | `test_bug4_hunk_navigation_n_does_not_move_cursor` |

Pressing `n`/`p` in the diff panel does nothing. No error, no movement. Other
review-mode keys (`c`, `s`, `u`, `d`) work from the same panel, so the mode
bindings are resolving -- the handler logic itself is broken.

**Root Cause candidates:**
1. `state.hunkHeaderRows` empty (not populated for current file)
2. `state.diffCursorRow` not updated by programmatic cursor moves
3. Stale byte offsets causing `jumpDiffCursorToRow()` to silently bail

---

#### BUG-6: Comments from Files Panel Never Display Inline

| Field | Value |
|-------|-------|
| **Criticality** | **SILENT DATA LOSS (visual)** |
| **Reported in** | Bug Report #5 |
| **E2E test** | `test_bug6_comment_from_files_panel_not_visible_in_diff` |

Comments added from the files panel are stored but **never rendered** inline in
the diff view. The user thinks the comment was saved (status bar says "Comment
added") but it's invisible. Comments ARE stored in the session file, so no
actual data loss, but the user cannot see or interact with them in the UI.

**Root Cause:** `getCurrentLineInfo()` returns no line-level info when the
files panel is focused. `pushLineComments()` skips comments without
`line_type`/`old_line`/`new_line`.

---

#### BUG-9: Down Arrow Doesn't Scroll Viewport in Side-by-Side View

| Field | Value |
|-------|-------|
| **Criticality** | **SILENT FAILURE** |
| **Reported in** | Bug Report #6 |
| **E2E test** | `test_bug9_side_by_side_down_arrow_no_viewport_scroll` |

In side-by-side diff, pressing `Down` updates the status bar line number but
the viewport stays frozen at the top.

**Note:** Symptom of BUG-1. Likely resolved once CompositeInputRouter is wired.

---

### Tier 4 -- UX Annoyances / Missing Keybindings

#### BUG-7: Escape Does Not Exit File Explorer Focus

| Field | Value |
|-------|-------|
| **Criticality** | **UX GAP** |
| **Reported in** | Audit #2 |
| **E2E test** | `test_bug7_escape_does_not_exit_file_explorer_focus` |

When the File Explorer has focus, pressing Escape does nothing. Users must know
`Ctrl+E` to escape. Minor, but compounds with BUG-3 to make the first-use
experience confusing.

---

### Tier 5 -- Cosmetic

#### BUG-10: Toolbar "Export" Label Truncated with File Explorer Open

| Field | Value |
|-------|-------|
| **Criticality** | **COSMETIC** |
| **Reported in** | Bug Report #8 |
| **E2E test** | `test_bug10_toolbar_export_label_truncated` |

With the File Explorer sidebar open, the toolbar's `e Export` hint is truncated
to `e E` or missing entirely.

---

## Features Verified Working (Both Reports Agree)

| Feature | Status |
|---------|--------|
| File list navigation (`j`/`k`/Up/Down/Home/End/PageUp/PageDown) | Working (after focus is correct) |
| File list boundary clamping | Working |
| Tab focus toggle (files <-> diff) | Working, robust under rapid toggling |
| `s` (stage file/hunk) | Working |
| `u` (unstage file/hunk) | Working |
| `d` (discard) with confirmation dialog | Working |
| `r` (refresh) | Working |
| `c` (comment) prompt | Working (but inline display broken -- BUG-6) |
| `N` (note) | Working |
| `x` (delete comment) | Working |
| `e` (export to markdown) | Working |
| `Enter` (drill-down to side-by-side) | Working (except deleted files -- BUG-5) |
| Side-by-side diff layout and alignment | Working |
| Horizontal scrolling in side-by-side | Working |
| Long line handling | Working |
| `q` (close) | Working |
| Diff coloring (added/removed/context/word-level) | Working |
| Section headers (Staged/Changes/Untracked) | Working |

---

## Prioritized Fix Order

| Order | Bug | Criticality | Effort | Rationale |
|-------|-----|-------------|--------|-----------|
| 1 | BUG-5 | Hang | Low-Med | Only hang in the feature. Deleted file drill-down traps the user. |
| 2 | BUG-2 | Unrecoverable | Med-High | Terminal resize destroys UI with no recovery path. |
| 3 | BUG-1 | Feature inoperable | Medium | Highest ROI: one wiring change unlocks all side-by-side keyboard nav. Also fixes BUG-9. |
| 4 | BUG-3 | Silent failure | Low | Auto-focus on launch. One-line fix, eliminates first-use confusion. |
| 5 | BUG-4 | Silent failure | Medium | Hunk nav is a core workflow feature. |
| 6 | BUG-6 | Silent data loss | Medium | Comments are stored but invisible. |
| 7 | BUG-9 | Silent failure | Low | Likely fixed by BUG-1. Verify only. |
| 8 | BUG-7 | UX gap | Low | Escape to leave File Explorer. |
| 9 | BUG-10 | Cosmetic | Low | Toolbar label truncation. |
