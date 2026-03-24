# Design: Diff Chunk Navigation

## Overview

Add **next/previous diff-chunk** commands to the command palette, enabling users
to jump between changed regions in the current buffer.

Two diff sources exist, and the commands **merge changes from both**:

1. **Git diff** — compares the buffer against the git index/HEAD.  Available
   for files tracked by git.
2. **Piece-tree saved-diff** — compares the live buffer against its last-saved
   snapshot.  Works for *every* buffer (including huge files not tracked by any
   VCS), and updates instantly without shelling out.

The user gets a single **"Next Change" / "Previous Change"** pair.  When both
sources are available (git-tracked file with unsaved edits), changes from both
are merged into a single sorted list with overlapping regions deduplicated.
When only one source is available, it uses that source alone.

## Commands

| Command name       | Handler              | Default keybinding   |
|--------------------|----------------------|----------------------|
| Next Change        | `diff_nav_next`      | `Alt+F5`             |
| Previous Change    | `diff_nav_prev`      | `Shift+Alt+F5`       |

`Alt+F5` / `Shift+Alt+F5` match VS Code's default keybindings for next/previous
change navigation.

## Source merging

When a command is invoked, the plugin collects jump targets from **all
available sources**, merges them into a single sorted list, and deduplicates
overlapping positions:

```
1. Collect git gutter hunks (if available) → convert to byte positions
2. Collect saved-diff byte_ranges (if buffer has unsaved changes)
3. Merge all targets, sort by byte position
4. Deduplicate targets on the same line or at near-identical byte offsets
```

Status message: `"Change 3/7"` or `"Change 3/7 [wrapped]"` — no source label
needed since changes from all sources are unified.

## Implementation

### New plugin: `diff_nav.ts`

A single new plugin that owns both commands.  It imports no shared library
beyond `fresh.d.ts`.

```
plugins/
  diff_nav.ts          ← new
  git_gutter.ts        ← exports hunks via setViewState
```

#### Data flow

```
async collectTargets(bid):
  targets = []

  // 1. Git hunks → convert line numbers to byte positions
  hunks = editor.getViewState(bid, "git_gutter_hunks")
  for each hunk:
    pos = await editor.getLineStartPosition(hunk.startLine - 1)
    targets.push({ bytePos: pos, line: hunk.startLine - 1 })

  // 2. Saved-diff → byte ranges already available
  diff = editor.getBufferSavedDiff(bid)
  for each [start, end] in diff.byte_ranges:
    targets.push({ bytePos: start, line: -1 })  // line resolved during dedup

  // 3. Sort by byte position, deduplicate overlapping targets
  sort targets by bytePos
  remove targets on same line or within 2 bytes of each other

  return targets
```

All targets are converted to byte positions for a unified sort.  Git hunks
get their line number resolved via `getLineStartPosition` (async).
Saved-diff targets already have byte positions.

**Deduplication**: After sorting, targets on the same line or within 2 bytes
of each other are merged.  This prevents double-jumping when a git hunk and
an unsaved change overlap.

**Navigation**: `setBufferCursor(bid, target.bytePos)` jumps to the target.
For targets with a known line, `scrollToLineCenter` centers the viewport.

**Wrapping**: Wrap around (first ↔ last) with a `[wrapped]` status message,
matching VS Code and gitsigns behavior.

**Complexity**: The piece-tree diff is O(edit-path) thanks to `Arc::ptr_eq`
structural sharing.  Git hunks come from cached data in git_gutter.
Navigation is O(N) where N = total targets (typically small).

#### Cross-plugin data sharing

git_gutter.ts exports its hunks via `setViewState`, which is shared across
all plugins (not scoped per-plugin):

```typescript
// In git_gutter.ts — after computing hunks:
editor.setViewState(bufferId, "git_gutter_hunks", hunks);
// Set to null for untracked files (so diff_nav knows git is unavailable):
editor.setViewState(bufferId, "git_gutter_hunks", null);

// In diff_nav.ts — read them:
const hunks = editor.getViewState(bid, "git_gutter_hunks") as DiffHunk[] | null;
```

### Rust-side changes

**No Rust changes are needed.**

The existing `BufferSavedDiff` struct already provides `byte_ranges`, which is
sufficient for navigation.  Line numbers are obtained after jumping via
`editor.getCursorLine()` on the TypeScript side — no precomputation or
line-feed scanning required.

```rust
// fresh-core/src/api.rs — unchanged
pub struct BufferSavedDiff {
    pub equal: bool,
    pub byte_ranges: Vec<Range<usize>>,
}
```

This avoids any risk of full-buffer scans (CONTRIBUTING.md guideline #2) and
keeps the API surface minimal.

### Keybindings

`Alt+F5` / `Shift+Alt+F5` are registered globally (not mode-specific).  Both
commands are also accessible via the command palette.

### Edge cases

| Case | Behavior |
|------|----------|
| No changes from either source | Status message: "No changes" |
| Cursor already on a change | "Next" skips to the next distinct change |
| Single change in buffer | "Next"/"prev" both jump to it; wrap message shown |
| File not tracked by git | Uses saved-diff only |
| No git repo at all | Uses saved-diff only |
| Git changes + unsaved edits | Both sources merged, overlapping targets deduplicated |
| Buffer not modified since save (no git) | Saved-diff returns `equal: true`; status: "No changes" |
| Huge file, line feeds not scanned | Jump by byte offset works |
| Deleted hunk (git, lineCount=0) | Jump to the deletion marker line (startLine - 1), matching gutter indicator |

## Testing

Per CONTRIBUTING.md:

1. **E2E test for git navigation**: Open a git-tracked file, make edits, save,
   invoke `diff_nav_next` / `diff_nav_prev`, verify cursor lands on expected
   lines.

2. **E2E test for saved-diff**: Open an untracked file, make edits (don't
   save), invoke `diff_nav_next` / `diff_nav_prev`, verify cursor lands within
   the edited byte ranges.

3. **E2E test for merged sources**: Open a git-tracked file with committed
   changes, make additional unsaved edits at a different location, invoke
   `diff_nav_next` repeatedly, verify it visits both git and unsaved changes.

4. **E2E test for wrapping**: Navigate past the last change, verify cursor
   wraps to the first change.

5. **E2E test for no-changes case**: Open an unmodified file, invoke commands,
   verify status message and no cursor movement.

All tests use semantic waiting (no timeouts), isolated temp dirs, and internal
clipboard mode.

## Non-goals (future work)

- **Inline diff peek / revert hunk**: Natural follow-up, but out of scope for
  this change.
- **Stage hunk**: Requires git index manipulation, out of scope.
- **Saved-diff gutter indicators**: Could show save-status in the gutter
  alongside git indicators. Out of scope.
- **Staged vs unstaged distinction**: Like gitsigns.nvim's
  `signs_staged_enable`. Out of scope.
- **Explicit source-specific commands**: If users want to force a specific
  source (e.g. always saved-diff even in a git repo), these could be added
  as separate palette commands later. Out of scope for now.

## Summary of changes

| File | Change |
|------|--------|
| `fresh-editor/plugins/git_gutter.ts` | Export hunk data via `setViewState` |
| `fresh-editor/plugins/diff_nav.ts` | New plugin: merged `nextChange` / `prevChange` from all sources |
| `tests/` | E2E tests for git, saved-diff, merged sources, wrapping |

No Rust-side changes are required — the existing `BufferSavedDiff` API already
provides everything needed.
