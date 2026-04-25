# Git

> **Palette:** `Review Diff`, `Review: Commit Range`, `Review: PR Branch`, `Git Log`, `Next Diff Chunk`, `Previous Diff Chunk`. Run **Keybinding Editor** to see or change the keys.

Fresh has built-in tooling for reviewing diffs, navigating git history, and jumping between changes. Everything here is driven from the command palette.

*   **Review Diff** — unified buffer of working-tree hunks, with stage / unstage / discard on the cursor row.
*   **Review: Commit Range / PR Branch** — same buffer against an arbitrary range or a branch's commits.
*   **Git Log** — magit-style log with a live-preview diff panel on the right.
*   **Diff Chunk Navigation** — jump between hunks from git *or* saved diff files with the same commands.

## Review Diff

**Review Diff** opens a unified buffer that lists files and their diffs in a single scrollable view. The file list sits at the top; each file's hunks follow and can be collapsed. The buffer is the same kind of buffer the editor uses everywhere else, so scrolling, search, and splits all work as normal.

Entry points (all in the command palette):

- **Review Diff** — everything staged, unstaged, and untracked in the working tree right now.
- **Review: Commit Range** — any range expression, e.g. `main..feature` or `HEAD~5...HEAD`.
- **Review: PR Branch** — walk a branch's commits, with a side-by-side `git show` for the currently selected commit.

Inside a review:

- **`n` / `p`** jump to the next and previous hunk.
- Stage, unstage, or discard the hunk, file, or a line-level visual selection on the cursor row.
- **Comments** — leave a line comment or a session-wide note. Comments persist per repository across sessions, so you can close the editor mid-review and pick up where you left off. A dedicated Comments panel lets you jump through them, edit, delete, or export to Markdown.

## Git Log

**Git Log** opens a live-preview commit history. Moving through the log updates the right panel with the diff for the selected commit — no need to open each one to see what it touched. Commit messages wrap, columns align, and the toolbar is clickable.

## Diff Chunk Navigation

The built-in **Diff Chunk Navigation** plugin merges two sources of hunks — the active git diff and any saved diff files — so you can jump between changes the same way in either context. It adds commands like **Next Diff Chunk** and **Previous Diff Chunk** to the palette.

See it in action: [Review Diff](/blog/fresh-0.3.0/#review-diff-rewrite) and [Git Log](/blog/fresh-0.3.0/#git-log) in the 0.3.0 blog post.
