# Review Diff Rewrite Plan

## Goal

Rewrite `plugins/audit_mode.ts` to use a magit-style split-panel UI and
`git status --porcelain -z` as the single source of truth for changed files.

## Current Problems

1. **File detection** uses three separate git commands (`git diff --cached`,
   `git diff`, `git ls-files --others`) instead of a single authoritative
   source. Edge cases (renames, copies, files in both staged and unstaged)
   can fall through the cracks.

2. **Rendering** produces a variable-length buffer that can exceed the viewport,
   causing buffer-wide scrolling. The review panel should be a fixed-height
   viewport-clamped UI like theme_editor.

3. **Layout** is a single column of hunks. The target is a two-panel magit
   style: file list on left, diff on right, action hints at bottom.

## Design Reference

```
┌── GIT STATUS ──────────────┐┌── DIFF FOR file.rs ───────────────────────┐
│ @  (Staged)                 ││   @@ fn main() @@                         │
│  +  M  hello.c           M ││     fn main() {                           │
│                             ││   -     println!("Hello");                │
│ @  [Unstaged]               ││   +     println!("Hello, world!");        │
│ >M  flake.nix            M ││   +     let x = 42;                      │
│                             ││     }                                     │
│ @  (Untracked)              ││                                           │
│  o  A  shimen.md            ││                                           │
│                             ││                                           │
├─────────────────────────────┴┴───────────────────────────────────────────┤
│ [esc] Close  [s] Stage  [u] Unstage  [d] Discard  [Enter] Drill-Down    │
└──────────────────────────────────────────────────────────────────────────┘
```

Selected file is highlighted with a background color. Diff panel shows the
diff for the selected file.

## Architecture

### Rendering model (theme_editor pattern)

- **Single virtual buffer**, `editingDisabled: true`, `showCursors: false`.
- Output is **exactly `viewportHeight` lines** — never more, never less.
  This prevents buffer-wide scrolling entirely.
- Each visual row = 4 text entries: `leftPadded + "│" + rightPadded + "\n"`.
  Bottom rows (separator, hints) are full-width.
- **Inline overlays** for all styling — no global byte-offset `addOverlay`
  tracking needed.
- Re-render on every state change by calling `updateDisplay()` which rebuilds
  entries and calls `editor.setVirtualBufferContent()`.
- Listen to `resize` event to update `viewportWidth`/`viewportHeight` and
  re-render.

### Layout breakdown

Given viewport height `H` and width `W`:

```
Row 0           : Header row  (left: "GIT STATUS", right: "DIFF FOR <file>")
Rows 1..H-3     : Main content (left: file list, right: diff) — H-3 rows
Row H-2         : Separator    (full width "─...─")
Row H-1         : Hints bar    (full width keybinding hints)
```

- Left width: `max(28, floor(W * 0.3))`.
- Right width: `W - leftWidth - 1` (1 for divider).

### Scrolling

Each panel has its own scroll offset stored in state:

| Panel     | Scroll var          | Nav keys         |
|-----------|---------------------|------------------|
| File list | `fileScrollOffset`  | Up/Down, j/k     |
| Diff      | `diffScrollOffset`  | PageUp/PageDown   |

When `selectedIndex` changes, `fileScrollOffset` is auto-adjusted to keep the
selected file visible (same pattern as theme_editor's `treeScrollOffset`).

The total rendered output is always exactly `H` lines, so buffer-wide scroll
never engages.

### State model

```typescript
interface FileEntry {
    path: string;
    status: string;        // 'M', 'A', 'D', 'R', 'C', '?'
    category: 'staged' | 'unstaged' | 'untracked';
    origPath?: string;     // for renames/copies
}

interface ReviewState {
    files: FileEntry[];
    hunks: Hunk[];
    selectedIndex: number;      // index into files[]
    fileScrollOffset: number;
    diffScrollOffset: number;
    viewportWidth: number;
    viewportHeight: number;
    reviewBufferId: number | null;
    comments: ReviewComment[];
    hunkStatus: Record<string, HunkStatus>;
    overallFeedback?: string;
}
```

## Implementation Steps

### Step 1 — Replace git file detection with `git status --porcelain -z`

**File:** `plugins/audit_mode.ts` — replace `getGitDiff()`

New function `getGitStatus()`:
1. Run `git status --porcelain -z`.
2. Parse null-delimited output: each entry is `XY<space>path\0`
   (renames add `old_path\0` after).
3. Categorize: X != ' '/'?' → staged; Y != ' '/'?' → unstaged; '??' → untracked.
4. A file can appear in BOTH staged and unstaged if both X and Y are set.
5. Return `FileEntry[]`.

New function `fetchDiffsForFiles(files)`:
1. If any staged files: `git diff --cached --unified=3` → `parseDiffOutput(stdout, 'staged')`.
2. If any unstaged files: `git diff --unified=3` → `parseDiffOutput(stdout, 'unstaged')`.
3. For each untracked file: `git diff --no-index --unified=3 /dev/null <file>`.
4. Return `Hunk[]`.

**Key improvement:** single source of truth for file states; handles renames,
copies, partial staging (MM), deleted files correctly.

### Step 2 — Rewrite rendering to viewport-clamped magit layout

**Replace:** `renderReviewStream()`, `updateReviewUI()`, `HighlightTask`

New functions:

- `buildFileListLines(): ListLine[]` — section headers + file entries,
  with inline overlays for icons/colors. Section headers not selectable.

- `buildDiffLines(rightWidth): DiffLine[]` — diff content for
  `state.files[state.selectedIndex]`. Includes hunk headers (`@@`),
  context/add/remove lines with character-level diff highlighting
  (reuses existing `diffStrings()`).

- `buildDisplayEntries(): TextPropertyEntry[]` — composites the above into
  exactly `viewportHeight` lines using the theme_editor left+divider+right
  pattern. Handles scroll offsets, selection highlighting, header/footer.

- `updateDisplay()` — calls `buildDisplayEntries()`, sets buffer content,
  no separate overlay pass needed (all styling via inline overlays).

- `onResize(data)` — updates `viewportWidth`/`viewportHeight`, calls
  `updateDisplay()`. Registered on `resize` event.

### Step 3 — Rewrite navigation handlers

**Replace:** `review_next_hunk`, `review_prev_hunk`

New handlers:

| Handler                | Action                                         |
|------------------------|-------------------------------------------------|
| `review_nav_up`        | `selectedIndex--`, reset diffScrollOffset, updateDisplay |
| `review_nav_down`      | `selectedIndex++`, reset diffScrollOffset, updateDisplay |
| `review_page_up`       | `diffScrollOffset -= pageSize`, updateDisplay   |
| `review_page_down`     | `diffScrollOffset += pageSize`, updateDisplay   |
| `review_file_page_up`  | `selectedIndex -= pageSize`, updateDisplay      |
| `review_file_page_down`| `selectedIndex += pageSize`, updateDisplay      |

Navigation wraps/clamps at boundaries. When `selectedIndex` changes,
`diffScrollOffset` resets to 0.

### Step 4 — Add real git stage/unstage/discard actions

New handlers (run git commands and refresh):

| Handler                | Git command                                    |
|------------------------|------------------------------------------------|
| `review_stage_file`    | `git add -- <file>`                            |
| `review_unstage_file`  | `git reset HEAD -- <file>`                     |
| `review_discard_file`  | `git checkout -- <file>` (tracked) or `rm` (untracked) |

After each action: re-run `getGitStatus()` + `fetchDiffsForFiles()`,
clamp `selectedIndex`, call `updateDisplay()`.

### Step 5 — Update `start_review_diff()` entry point

1. Get viewport size from `editor.getViewport()`.
2. Run `getGitStatus()` + `fetchDiffsForFiles()`.
3. Create virtual buffer with `editingDisabled: true`, `showCursors: false`.
4. Register `resize` handler.
5. Call `updateDisplay()`.

### Step 6 — Adapt drill-down to new state

`review_drill_down()` currently reads hunk from cursor text properties.
Change to use `state.files[state.selectedIndex]` to determine which file
to drill into. Rest of composite buffer logic stays the same.

### Step 7 — Update mode keybindings

```typescript
editor.defineMode("review-mode", [
    ["Up", "review_nav_up"],
    ["Down", "review_nav_down"],
    ["k", "review_nav_up"],
    ["j", "review_nav_down"],
    ["PageUp", "review_page_up"],
    ["PageDown", "review_page_down"],
    ["s", "review_stage_file"],
    ["u", "review_unstage_file"],
    ["d", "review_discard_file"],
    ["Enter", "review_drill_down"],
    ["r", "review_refresh"],
    ["q", "close"],
    ["Escape", "close"],
    // Review actions (apply to all hunks of selected file)
    ["a", "review_approve_hunk"],
    ["x", "review_reject_hunk"],
    ["c", "review_add_comment"],
    ["E", "review_export_session"],
], true);
```

### Step 8 — Update i18n

Update `audit_mode.i18n.json` — remove old panel.help_* keys, add new
section header keys if needed. Keep status.* keys.

### Step 9 — Remove dead code

Remove functions no longer called:
- `computeFullFileAlignedDiff()` (~140 lines)
- `generateDiffPaneContent()` (~145 lines)
- `AlignedLine` interface
- `SideBySideDiffState`, `activeSideBySideState`
- `on_viewport_changed()`, `findLineForByte()`
- `HighlightTask` interface
- Old `renderReviewStream()`, `review_next_hunk`, `review_prev_hunk`

Estimated removal: ~400 lines of dead code.

### Step 10 — Add e2e test for multi-batch scrolling correctness

**File:** `tests/e2e/plugins/audit_mode.rs`

Test `test_review_diff_scrolling_many_files`:
1. Create repo with initial commit.
2. Add `.gitignore` for `plugins/`.
3. Create 8 staged modified files, 5 unstaged modified files, 5 untracked
   new files (total 18 files + 3 section headers = 21 file list lines).
4. Use viewport `80x15` — main area is ~12 rows, so file list overflows.
5. Open Review Diff.
6. Verify initial render shows first files and correct diff.
7. Navigate down to a file that was NOT in the initial viewport (e.g.,
   the 15th file).
8. Verify screen now shows that file name AND correct diff content.
9. Verify screen does NOT show files that should have scrolled off the top.

This validates that:
- Scroll logic works correctly across section boundaries.
- Diff panel updates correctly when selection changes.
- No content corruption when file list exceeds viewport.

## Files Changed

| File | Change |
|------|--------|
| `plugins/audit_mode.ts` | Major rewrite (~60% of file) |
| `plugins/audit_mode.i18n.json` | Update keys |
| `tests/e2e/plugins/audit_mode.rs` | Add scrolling test |

## Preserved Functionality

These features are **kept unchanged**:
- Side-by-side drill-down via composite buffers (`review_drill_down`)
- `side_by_side_diff_current_file` command
- Export to markdown/JSON
- Comment system (prompt-based)
- Review status actions (approve/reject/question/needs_changes)
- diff-view mode keybindings
- Buffer cleanup handlers

## Risk Assessment

- **Low risk:** git status porcelain format is stable and well-documented.
- **Medium risk:** viewport clamping logic — off-by-one errors in scroll
  offset math. Mitigated by the multi-batch e2e test.
- **Low risk:** real git stage/unstage operations change working tree state.
  This is intentional (matching magit behavior) but is a behavior change
  from the old virtual-staging model.
