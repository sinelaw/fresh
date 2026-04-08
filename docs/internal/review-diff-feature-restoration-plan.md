# Review Diff Feature Restoration Plan

**Status**: Planned
**Issue**: #1503 — Review diff comments degradation
**Scope**: Restore features lost in the v0.2.22 magit rewrite, adapted to the
new split-panel UI

---

## Problem

The v0.2.22 magit-style rewrite (`7c33606`) replaced the old single-column
hunk view with a split-panel UI. Several features were accidentally dropped:

1. **Hunk-level git operations** — stage/discard/unstage operated per-hunk;
   now they only work per-file.
2. **Review status keybindings** — `x` (reject), `!` (needs changes),
   `?` (question), `O` (overall feedback) lost their keybindings.
3. **Review status indicators** — hunk headers showed colored status icons
   (✓/✗/!/?) with labels like `← APPROVED`. Now invisible.
4. **Word-level diff highlighting** — adjacent -/+ lines showed character-level
   changes (bold+bg on changed words). Now whole-line bg only.
5. **`q` to close** — standard magit close key was removed.
6. **`u` reassigned** — was "clear review status", now "unstage file".

Line-specific comments (#1503) have already been fixed in a prior commit.

## Design Principles

All changes follow the existing Fresh UX conventions and NN/g usability
heuristics:

- **Visibility of system status**: review status must be visible in the UI,
  not hidden behind exports. Users need to see at a glance which hunks are
  approved/rejected.
- **Match between system and real world**: follow magit conventions since the
  UI is explicitly magit-style. `q` closes, `s`/`u` stage/unstage, actions
  apply to whatever is under the cursor.
- **User control and freedom**: hunk-level undo (clear status) must be
  accessible. Destructive actions (discard) require confirmation.
- **Consistency and standards**: keybindings should be predictable based on
  focus context. Same key (`s`) does the natural action at the current
  granularity level.
- **Recognition over recall**: the toolbar and status bar should surface
  available actions. Don't require users to memorize hidden keybindings.
- **Flexibility and efficiency**: power users get single-key shortcuts;
  all actions also available via command palette.

## UX Design

### Context-sensitive actions

The core UX insight: **actions apply to whatever the cursor is on.** When the
file list is focused, actions apply to the whole file. When the diff panel is
focused, actions apply to the hunk containing the selected line.

```
┌── GIT STATUS ──────────────┐┌── DIFF FOR main.rs ────────────────────────┐
│ ▸ Staged                    ││ @@ fn main() @@ ✓ APPROVED                │
│  >M  hello.c                ││   fn main() {                             │
│                              ││ -     println!("Hello");                  │
│ ▸ Changes                    ││ +     println!("Hello, world!");          │
│   M  main.rs                 ││ +     let x = 42;                        │
│                              ││   }                                       │
│ ▸ Untracked                  ││   » [+4] Consider splitting statements    │
│   A  notes.txt               ││                                           │
│                              ││ @@ fn helper() @@   (pending)             │
│                              ││   fn helper() {                           │
│                              ││ -     todo!()                             │
│                              ││ +     42                                  │
│                              ││   }                                       │
├──────────────────────────────┴┴───────────────────────────────────────────┤
│ [s]tage [u]nstage [d]iscard [c]omment [a]pprove [x]reject [Enter]drill   │
└───────────────────────────────────────────────────────────────────────────┘
```

### Keybinding map

Both panels share the same keys. The action scope depends on which panel
has focus:

| Key | File panel focused | Diff panel focused |
|-----|--------------------|--------------------|
| `s` | Stage entire file | Stage current hunk |
| `u` | Unstage entire file | Unstage current hunk |
| `d` | Discard entire file | Discard current hunk |
| `a` | Approve all hunks | Approve current hunk |
| `x` | Reject all hunks | Reject current hunk |
| `!` | — | Mark hunk needs changes |
| `?` | — | Mark hunk with question |
| `c` | Comment (file-level) | Comment on selected line |
| `O` | Set overall feedback | Set overall feedback |
| `n`/`p` | — | Jump to next/prev hunk header |
| `e` | Export to markdown | Export to markdown |
| `q`/`Esc` | Close review diff | Close review diff |

### Navigation

| Key | File panel | Diff panel |
|-----|------------|------------|
| `Up`/`k` | Previous file | Previous diff line |
| `Down`/`j` | Next file | Next diff line |
| `n` | — | Jump to next hunk header |
| `p` | — | Jump to prev hunk header |
| `PageUp` | Page up in file list | Page up in diff |
| `PageDown` | Page down in file list | Page down in diff |
| `Home` | First file | Top of diff |
| `End` | Last file | Bottom of diff |
| `Tab` | Switch to diff panel | Switch to file panel |
| `Left` | Focus file panel | Focus file panel |
| `Right` | Focus diff panel | Focus diff panel |
| `Enter` | Drill down to side-by-side | Drill down to side-by-side |

### Review status indicators in diff panel

Hunk headers show the current review status with a colored icon and label:

```
@@ fn main() @@ ✓ APPROVED        ← green
@@ helper() @@ ✗ REJECTED         ← red
@@ parse() @@ ! NEEDS CHANGES     ← yellow
@@ validate() @@ ? QUESTION       ← yellow
@@ render() @@   (pending)         ← dim, only when others have status
```

When all hunks are pending, no status suffix is shown (clean default).

### Word-level diff highlighting

Adjacent `-`/`+` line pairs in the diff panel get character-level highlighting
using the existing `diffStrings()` function:

```
-     println!("Hello");              ← "Hello" has bold red bg
+     println!("Hello, world!");      ← "Hello, world!" has bold green bg
```

The unchanged portions (`println!("` and `");`) use the normal diff line
foreground without bold/bg emphasis. This reuses the same approach as the
side-by-side drill-down view but applies it to the magit panel's right column.

Implementation: `buildDiffLines()` detects adjacent remove/add pairs and
attaches `inlineOverlays` to the `DiffLine` entries. The
`buildMagitDisplayEntries()` renderer already passes `inlineOverlays` through.

### Inline comment display

Comments appear inline below the line they reference (already implemented):

```
+     let x = 42;
  » [+5] Consider splitting into two statements
```

Format: `  » [±line] text` with `STYLE_COMMENT` (yellow/warning color).

### Toolbar

The toolbar adapts to show the most relevant actions for the current focus:

```
File panel:  [Tab] Switch  [s] Stage  [u] Unstage  [d] Discard  [Enter] Drill-Down  [r] Refresh
Diff panel:  [Tab] Switch  [s] Stage Hunk  [c] Comment  [a] Approve  [x] Reject  [n/p] Next/Prev Hunk
```

This follows NN/g's "recognition over recall" heuristic — users see exactly
which actions apply to their current context.

### Status bar messages

Follow existing patterns (past tense, interpolated with `%{var}`):

| Action | Message |
|--------|---------|
| Stage hunk | `"Hunk staged (1 of 3 in main.rs)"` |
| Unstage hunk | `"Hunk unstaged"` |
| Discard hunk | `"Hunk discarded"` |
| Approve | `"Hunk approved"` (existing) |
| Reject | `"Hunk rejected"` (existing) |
| Stage file | `"File staged: main.rs"` |
| Discard file | `"Changes discarded: main.rs"` |

### Confirmation prompts

Destructive actions require confirmation (existing pattern from
`review_discard_file`):

- **Discard hunk**: `Discard this hunk in "main.rs"? This cannot be undone.`
  with suggestions: `[Discard hunk]` / `[Cancel]`
- **Discard file**: existing behavior unchanged

Non-destructive actions (stage, unstage, review status) execute immediately
with no confirmation.

## Git Implementation

### Approach: `git apply --cached` with temp file

This is the canonical method used by git itself (`add-patch.c`), magit, and
lazygit. It is stable across git versions and handles all edge cases.

| Operation | Command |
|-----------|---------|
| Stage hunk | `git apply --cached <patchfile>` |
| Unstage hunk | `git apply --cached --reverse <patchfile>` |
| Discard hunk | `git apply --reverse <patchfile>` |
| Validate | `git apply --cached --check <patchfile>` |

### Why this approach

- **`git apply --cached`** is what `git add -p` uses internally in C code
  (`add-patch.c`). It constructs a patch from selected hunks and applies it.
- **magit** uses the same mechanism: `git apply --cached -` with patch piped
  via stdin.
- **lazygit** writes patch to a temp file and calls `git apply --cached`.

Alternatives considered and rejected:
- `git update-index` — blob-level, cannot target individual hunks without
  reimplementing patch application.
- `git add -p` with scripted y/n — fragile across git versions, and
  `spawnProcess` has no stdin support.

### API constraint: no stdin piping

Fresh's `spawnProcess` API (`process.rs`) only pipes stdout/stderr. Stdin is
not available. Therefore we use the lazygit approach:

1. Write the patch to a temp file via `editor.writeFile()`
2. Pass the file path to `git apply --cached <path>`
3. Use `editor.getTempDir()` for the temp directory

### Constructing a single-hunk patch

```typescript
function buildHunkPatch(filePath: string, hunk: Hunk): string {
    const oldCount = hunk.lines.filter(
        l => l[0] === '-' || l[0] === ' '
    ).length;
    const newCount = hunk.lines.filter(
        l => l[0] === '+' || l[0] === ' '
    ).length;
    const header = `@@ -${hunk.oldRange.start},${oldCount} `
                 + `+${hunk.range.start},${newCount} @@`;
    return [
        `diff --git a/${filePath} b/${filePath}`,
        `--- a/${filePath}`,
        `+++ b/${filePath}`,
        header,
        ...hunk.lines,
        ''  // trailing newline
    ].join('\n');
}
```

### Edge cases

| Case | Handling |
|------|----------|
| Untracked files | `git add` directly (no patch needed) |
| Binary files | Stage/unstage whole file only |
| New file (all additions) | Patch header: `--- /dev/null`, `new file mode 100644` |
| Deleted file (all removals) | Patch header: `+++ /dev/null`, `deleted file mode 100644` |
| CRLF | Use raw `git diff` output lines (already normalized by git) |
| No trailing newline | Preserve `\ No newline at end of file` marker |
| Partial staging | File appears in both staged and unstaged — already handled by porcelain parser |

### Validation before apply

Always dry-run before applying:

```typescript
const check = await editor.spawnProcess("git", [
    "apply", "--cached", "--check", patchPath
]);
if (check.exit_code !== 0) {
    editor.setStatus("Patch failed: " + check.stderr.trim());
    return;
}
// Safe to apply
await editor.spawnProcess("git", ["apply", "--cached", patchPath]);
```

### Hunk identification

When the diff panel is focused, `diffSelectedLine` (added in the #1503 fix)
identifies which diff line the cursor is on. Each `DiffLine` carries `hunkId`.
Look up the `Hunk` object from `state.hunks` to get the full line content
for patch construction.

When the user stages a hunk, it may cause the file to appear in both "Staged"
and "Changes" sections. After any git operation, call `refreshMagitData()` to
re-query `git status --porcelain -z` and rebuild the view.

## Implementation Steps

### Step 1 — Add hunk-level git operations

**File:** `plugins/audit_mode.ts`

New functions:

```typescript
function buildHunkPatch(filePath: string, hunk: Hunk): string
async function applyHunkPatch(patch: string, flags: string[]): Promise<boolean>
async function review_stage_hunk(): Promise<void>
async function review_unstage_hunk(): Promise<void>
async function review_discard_hunk(): Promise<void>
```

The stage/unstage/discard handlers check `state.focusPanel`:
- `'files'` → delegate to existing file-level handlers
- `'diff'` → build patch from current hunk, apply via temp file, refresh

### Step 2 — Restore review status keybindings

Add missing keybindings back to `defineMode("review-mode", ...)`:

```typescript
["x", "review_reject_hunk"],
["!", "review_needs_changes"],
["?", "review_question_hunk"],
["O", "review_set_overall_feedback"],
["q", "close"],
```

Make review status handlers context-aware: when diff panel is focused, apply
to the hunk at `diffSelectedLine`. When file panel is focused, apply to all
hunks of the selected file.

### Step 3 — Render review status in hunk headers

Update `buildDiffLines()` to append status to hunk-header lines:

```typescript
if (hunk.reviewStatus !== 'pending') {
    const icon = { approved: '✓', rejected: '✗',
                   needs_changes: '!', question: '?' }[hunk.reviewStatus];
    const label = hunk.reviewStatus.toUpperCase().replace('_', ' ');
    header += ` ${icon} ${label}`;
}
```

Add `inlineOverlays` to color the status portion using the existing
`STYLE_APPROVED`/`STYLE_REJECTED`/`STYLE_QUESTION` constants (currently
defined but unused).

### Step 4 — Add word-level diff highlighting to magit panel

Update `buildDiffLines()` to detect adjacent `-`/`+` line pairs and compute
`inlineOverlays` using `diffStrings()`:

```typescript
// When processing hunk.lines, detect pairs:
if (line[0] === '-' && nextLine && nextLine[0] === '+') {
    const parts = diffStrings(line.slice(1), nextLine.slice(1));
    // Build inlineOverlays for the removed line (highlight 'removed' parts)
    // Build inlineOverlays for the added line (highlight 'added' parts)
}
```

The `buildMagitDisplayEntries()` renderer already passes `inlineOverlays`
through to `TextPropertyEntry`, so no rendering changes needed.

### Step 5 — Add hunk navigation (`n`/`p`)

New handlers:

```typescript
function review_next_hunk() {
    const diffLines = buildDiffLines(...);
    for (let i = state.diffSelectedLine + 1; i < diffLines.length; i++) {
        if (diffLines[i].type === 'hunk-header') {
            state.diffSelectedLine = i;
            scrollDiffToSelected();
            updateMagitDisplay();
            return;
        }
    }
}
```

Same pattern for `review_prev_hunk()` scanning backward.

### Step 6 — Context-sensitive toolbar

Update `buildMagitDisplayEntries()` to show different toolbar text based on
`state.focusPanel`:

```typescript
const toolbar = state.focusPanel === 'files'
    ? " [Tab] Switch  [s] Stage  [u] Unstage  [d] Discard  [Enter] Drill-Down  [r] Refresh"
    : " [Tab] Switch  [s] Stage Hunk  [c] Comment  [a] Approve  [x] Reject  [n/p] Hunk Nav";
```

### Step 7 — Update i18n keys

Add to `audit_mode.i18n.json`:

```json
"status.hunk_staged": "Hunk staged",
"status.hunk_unstaged": "Hunk unstaged",
"status.hunk_discarded": "Hunk discarded",
"prompt.discard_hunk": "Discard this hunk in \"%{file}\"? This cannot be undone."
```

### Step 8 — Add e2e tests

**File:** `tests/e2e/plugins/audit_mode.rs`

New tests:
- `test_review_diff_hunk_stage` — stage a single hunk, verify file appears
  in both staged and unstaged sections
- `test_review_diff_hunk_discard` — discard a hunk, verify it disappears
  from diff
- `test_review_diff_review_status_display` — approve a hunk, verify status
  icon appears in hunk header
- `test_review_diff_hunk_navigation` — press `n`/`p`, verify cursor jumps
  between hunk headers
- `test_review_diff_context_sensitive_toolbar` — verify toolbar changes
  when switching panels

## Files Changed

| File | Change |
|------|--------|
| `plugins/audit_mode.ts` | Hunk operations, status rendering, word-level diff, keybindings |
| `plugins/audit_mode.i18n.json` | New status/prompt keys |
| `tests/e2e/plugins/audit_mode.rs` | New e2e tests |
| `docs/internal/review-diff-feature-restoration-plan.md` | This document |

## Risk Assessment

- **Low risk:** `git apply --cached` is the standard mechanism used by git
  itself, magit, and lazygit. Well-tested across git versions.
- **Low risk:** keybinding additions are additive — no existing bindings
  are changed or removed.
- **Medium risk:** hunk-level staging changes actual git index state. A bug
  in patch construction could stage wrong content. Mitigated by `--check`
  validation before every apply.
- **Low risk:** word-level highlighting reuses the existing `diffStrings()`
  function that already works in the side-by-side drill-down view.
- **Low risk:** context-sensitive toolbar is a pure display change with no
  behavioral impact.
