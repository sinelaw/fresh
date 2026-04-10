# Review Diff Mode -- Combined UX Report

**Sources:**
- `claude/audit-review-diff-mode-nGTV4` — UX Audit & Bug Report (REVIEW_DIFF_AUDIT_REPORT.md)
- `claude/test-review-diff-mode-7xnTV` — Manual Testing Bug Report (REVIEW_DIFF_BUG_REPORT.md)

**Date:** 2026-04-10
**Editor version:** Fresh 0.2.22 (debug build, commit 9ab13b3)
**Test environments:** tmux, 120x35 and 160x45 terminals, Linux 4.4.0

---

## Executive Summary

Two independent UX testing sessions were conducted on Review Diff mode. After
cross-referencing and deduplicating, **10 distinct bugs** were identified.
Of these, **1 is an architectural blocker** (dead code preventing an entire
feature surface from working), **1 is a high-severity rendering regression**
(terminal resize), and the rest range from medium to cosmetic.

The single highest-impact fix is wiring `CompositeInputRouter` into the key
dispatch pipeline -- this one change resolves 3 of the 10 bugs simultaneously.

---

## Consolidated Bug List

### BUG-1: CompositeInputRouter Is Dead Code -- Side-by-Side Keyboard Navigation Completely Broken

| Field | Value |
|-------|-------|
| **Severity** | **CRITICAL (Architectural Blocker)** |
| **Reported in** | Audit #3, Audit #4, Bug Report #6 |
| **Impact** | All vim-style keys (`j`/`k`/`Tab`/`n`/`p`/`Escape`) in side-by-side diff view are non-functional. Down arrow moves cursor but viewport doesn't scroll. Only `q` and arrow keys partially work. |

**Root Cause:** `CompositeInputRouter` (`crates/fresh-editor/src/input/composite_router.rs`) implements full keyboard routing for composite buffers -- vim scrolling, pane switching, hunk navigation, visual selection, yank -- but is **never called** from the application's key dispatch pipeline (`app/input.rs`). The router and all its action methods (`composite_scroll`, `composite_focus_next`, `composite_next_hunk`, etc.) are only referenced in their own module and unit tests.

**Key events are instead routed through the standard text-editing path**, producing "Editing disabled in this buffer" errors for `j`/`k`/`Tab`/`n`/etc.

**Fix:** In `app/input.rs`, after mode binding resolution (~line 164), check if the active buffer is a composite buffer. If so, route key events through `CompositeInputRouter::route_key_event()` and dispatch the resulting `RoutedEvent` to the appropriate `composite_*` methods.

**Resolves:** BUG-1 (this bug), and contributes to fixing viewport scroll (Bug Report #6) and side-by-side hunk navigation.

---

### BUG-2: Terminal Resize Destroys Review Diff Layout

| Field | Value |
|-------|-------|
| **Severity** | **HIGH** |
| **Reported in** | Bug Report #2 (unique finding) |
| **Impact** | Resizing the terminal while in Review Diff mode causes catastrophic rendering corruption. Toolbar, header, separator, and content disappear. Layout does not recover even after resizing back. Neither `r` (refresh) nor navigation keys restore it. |

**Reproduction:**
1. Open Review Diff
2. Resize terminal (e.g., `tmux resize-window -x 80 -y 24`)
3. Resize back to original size
4. Observe: menu bar, toolbar, header, separator, and diff content are all missing

**Evidence:** After resize-back, only a partial file list remains visible with 22 empty lines. Pressing `Home` causes diff content to render *below* the status bar.

**Fix:** The resize event handler must trigger a full re-layout of the Review Diff panels (file list, separator, diff panel, toolbar). The design doc specifies listening to the `resize` event to update `viewportWidth`/`viewportHeight` and re-render, but this is not working correctly.

---

### BUG-3: File Explorer Steals Focus from Review Diff on Launch

| Field | Value |
|-------|-------|
| **Severity** | **MEDIUM** (downgraded from initial HIGH -- workaround exists) |
| **Reported in** | Audit #1, Bug Report #1 |
| **Impact** | When File Explorer is open (default state), opening Review Diff does not transfer focus. All review-mode keybindings silently fail. |

**Workaround:** Press `Ctrl+E` to toggle focus from File Explorer to the editor area. After this, all review-mode keys work correctly.

**UX Problem:** The workaround is not discoverable. The `(Ctrl+E)` hint only appears in the explorer header *after* focus has already been manually switched away.

**Fix:** In `start_review_diff()`, after creating the buffer group, explicitly move focus to the review diff files panel. Consider calling `editor.focusBufferGroupPanel(state.groupId, 'files')` and setting `key_context` to `Normal`.

---

### BUG-4: Hunk Navigation (`n`/`p`) Non-Functional in Review Diff's Diff Panel

| Field | Value |
|-------|-------|
| **Severity** | **MEDIUM** |
| **Reported in** | Audit #5, Bug Report #4 |
| **Impact** | Pressing `n`/`p` in the diff panel does not move the cursor to hunk headers. No error message, no visible change. |

**Evidence:** Cursor stays at its current position after pressing `n` or `p`, even in multi-hunk files. Other review-mode keys (`c`, `s`, `u`, `d`) work from the same panel, confirming mode bindings resolve correctly.

**Root Cause Candidates (from both reports):**
1. `state.hunkHeaderRows` may be empty (not populated for the current file)
2. `state.diffCursorRow` may not update via the `cursor_moved` event when cursor is moved programmatically by `editor.setBufferCursor()`
3. Byte offsets in `state.diffLineByteOffsets` may be stale, causing `jumpDiffCursorToRow()` to silently fail at bounds check (line 1811)

**Fix:** Add debug logging to `review_next_hunk()` to trace `state.hunkHeaderRows` and `state.diffCursorRow`. Most likely requires ensuring `hunkHeaderRows` is populated during `buildDiffPanelEntries()` and that `cursor_moved` fires for plugin-driven cursor movements.

---

### BUG-5: Side-by-Side Drill-Down Fails for Deleted Files

| Field | Value |
|-------|-------|
| **Severity** | **MEDIUM** |
| **Reported in** | Bug Report #3 (unique finding) |
| **Impact** | Pressing `Enter` on a deleted file shows "Loading side-by-side diff..." indefinitely. The view never opens. |

**Root Cause:** In `review_drill_down()` (~line 1666), the code calls `editor.readFile(absoluteFilePath)` for the new version. For a deleted file, the file doesn't exist on disk, so `readFile` returns `null`. The function exits early but the loading status message persists.

**Fix:** Handle deleted files specially -- show OLD content on the left pane and an empty pane on the right. Also ensure the loading status is cleared on error.

---

### BUG-6: Comments Added from Files Panel Never Display Inline

| Field | Value |
|-------|-------|
| **Severity** | **MEDIUM** |
| **Reported in** | Bug Report #5 (unique finding) |
| **Impact** | Comments added while the files panel is focused (or on hunk header lines) are stored with no line-level info. They never render inline in the diff view, making them invisible. |

**Root Cause:** `getCurrentLineInfo()` reads text properties from the diff buffer's native cursor position. When the files panel is focused, the diff cursor is not on a line with `hunkId`/`lineType`/`oldLine`/`newLine` properties. The `pushLineComments()` function only matches comments with specific `line_type` AND matching `old_line`/`new_line`, so hunk-level comments are skipped.

**Fix:** Either (a) render hunk-level comments in the diff panel at the hunk header position, or (b) when adding a comment from the files panel, prompt for a line reference or attach to the first hunk of the selected file.

---

### BUG-7: Escape Key Does Not Exit File Explorer Focus

| Field | Value |
|-------|-------|
| **Severity** | **LOW** |
| **Reported in** | Audit #2 |
| **Impact** | Users trapped in File Explorer focus cannot escape back to Review Diff using `Escape`. |

**Fix:** Add an `Escape` binding in the File Explorer context that transfers focus back to the active editor/buffer group.

---

### BUG-8: Escape Key Not Mapped to Close Review Diff

| Field | Value |
|-------|-------|
| **Severity** | **LOW** |
| **Reported in** | Bug Report #7 (unique finding) |
| **Impact** | `Escape` does nothing in review-mode. Design spec says both `q` and `Esc` should close. Only `q` is bound. |

**Design Reference:** `docs/internal/review-diff-feature-restoration-plan.md` line 95: `q/Esc | Close review diff`

**Fix:** Add `["Escape", "close"]` binding alongside the existing `["q", "close"]` in review-mode (audit_mode.ts ~line 2675).

---

### BUG-9: Side-by-Side View -- Down Arrow Doesn't Scroll Viewport

| Field | Value |
|-------|-------|
| **Severity** | **LOW** |
| **Reported in** | Bug Report #6 |
| **Impact** | In side-by-side diff, pressing `Down` updates the status bar line number but the viewport stays frozen at the top. |

**Note:** This is a *symptom* of BUG-1 (CompositeInputRouter dead code). Once the router is wired in, `j`/`k` will handle scrolling. Arrow key viewport following may need a separate scroll-into-view call if not handled by the router.

---

### BUG-10: Toolbar "Export" Label Truncated with File Explorer Open

| Field | Value |
|-------|-------|
| **Severity** | **COSMETIC** |
| **Reported in** | Bug Report #8 (unique finding) |
| **Impact** | With the File Explorer sidebar open, the toolbar's `e Export` hint is truncated to `e E` or missing. |

**Fix:** Adjust toolbar rendering to either truncate labels gracefully (e.g., show abbreviated hints) or wrap to a second line, or prioritize the most important hints.

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

## Prioritized Action Plan

### Phase 1: Architectural Blocker (Unlocks Entire Feature Surface)

| Priority | Bug | Effort | Rationale |
|----------|-----|--------|-----------|
| **P0** | BUG-1: Wire CompositeInputRouter into key dispatch | Medium | **Highest ROI fix.** One change enables all side-by-side keyboard navigation (vim keys, pane switching, hunk nav, visual selection, yank). Also partially resolves BUG-9. The code is already written and tested -- it just needs to be connected. |

**Suggested approach:**
1. In `app/input.rs`, after mode binding resolution (~line 164), add a check: if `is_composite_buffer(active_buffer_id)`, call `CompositeInputRouter::route_key_event(key_event)`.
2. Dispatch the returned `RoutedEvent` to the appropriate `composite_*` action method.
3. Verify unit tests still pass and manually test `j`/`k`/`Tab`/`n`/`p`/`Escape`/`q` in side-by-side view.

---

### Phase 2: High-Severity Rendering Bug

| Priority | Bug | Effort | Rationale |
|----------|-----|--------|-----------|
| **P1** | BUG-2: Terminal resize destroys layout | Medium-High | Users commonly resize terminals. Unrecoverable corruption forces closing and reopening Review Diff. This is the only bug that *destroys user state* rather than just blocking a feature. |

**Suggested approach:**
1. Audit the resize event handler for the review diff buffer group.
2. Ensure `viewportWidth`/`viewportHeight` are recalculated and a full re-layout is triggered (toolbar, header, separator, panels, status bar).
3. Add a resize integration test that validates panel dimensions post-resize.

---

### Phase 3: Focus & Discoverability (Quick Wins)

| Priority | Bug | Effort | Rationale |
|----------|-----|--------|-----------|
| **P2** | BUG-3: Auto-focus on Review Diff launch | Low | One-line fix in `start_review_diff()` to call focus. Eliminates the most common first-impression frustration. |
| **P2** | BUG-8: Add Escape binding to close Review Diff | Trivial | One-line config change. Matches design spec and user expectations. |
| **P2** | BUG-7: Escape exits File Explorer focus | Low | Small keybinding addition. Improves focus navigation. |

**These three fixes together take ~1 hour and dramatically improve first-use experience.**

---

### Phase 4: Functional Gaps

| Priority | Bug | Effort | Rationale |
|----------|-----|--------|-----------|
| **P3** | BUG-4: Hunk navigation (`n`/`p`) in diff panel | Medium | Core navigation feature. Requires debugging state synchronization between `hunkHeaderRows`, `diffCursorRow`, and `diffLineByteOffsets`. |
| **P3** | BUG-5: Deleted file drill-down | Low-Medium | Edge case but important for completeness. Needs special handling for deleted-file status in `review_drill_down()`. |
| **P3** | BUG-6: Inline comment display | Medium | Comments are stored but invisible. Fix `pushLineComments()` to render hunk-level comments, or improve `getCurrentLineInfo()` to resolve line context from the files panel. |

---

### Phase 5: Polish

| Priority | Bug | Effort | Rationale |
|----------|-----|--------|-----------|
| **P4** | BUG-9: Arrow key viewport scroll in side-by-side | Low | Likely resolved by BUG-1 fix. Verify and add scroll-into-view if needed. |
| **P4** | BUG-10: Toolbar label truncation | Low | Cosmetic. Improve width calculation or add responsive label shortening. |

---

## Recommended Execution Order

```
Week 1:  BUG-1 (P0) + BUG-3, BUG-7, BUG-8 (P2 quick wins)
         -> Ship: side-by-side keyboard nav works, focus issues resolved

Week 2:  BUG-2 (P1) + BUG-4 (P3)
         -> Ship: resize handling fixed, hunk navigation works

Week 3:  BUG-5, BUG-6 (P3) + BUG-9, BUG-10 (P4)
         -> Ship: edge cases and polish complete
```

**Total estimated bugs: 10 | Already-working features: 17+ | Overall assessment: Review Diff is functionally solid for the happy path. The critical gap is that side-by-side keyboard navigation is entirely broken due to dead code, and resize handling is destructive. Fixing these two issues brings the feature to a shippable state.**
