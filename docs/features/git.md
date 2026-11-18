# Git

> **Palette:** `Review Diff`, `Review Diff: Range (Commit or Branch)`, `Review Diff: Stash`, `Git Log`, `Git Log: PR Branch`, `Next Change`, `Previous Change`. Run **Keybinding Editor** to see or change the keys.

Fresh has built-in tooling for reviewing diffs, navigating git history, and jumping between changes. Everything here is driven from the command palette.

*   **Review Diff** — unified buffer of working-tree hunks, with stage / unstage / discard on the cursor row.
*   **Review Diff: Range / Stash** — same buffer against an arbitrary range, a branch, or a stash entry.
*   **Git Log** — magit-style log with a live-preview diff panel on the right, over the repo, one file, or a PR branch.
*   **Git Blame** — magit-style blame for the current file.
*   **Change Navigation** — jump between changes from git, Live Diff, or your unsaved edits with the same commands.
*   **Compare Files** — any two files side by side, both editable; `fresh --diff A B` is a `git difftool`. See [Compare Files](./compare-files.md).

## Review Diff

**Review Diff** opens every changed file and its diff in a single scrollable view, with syntax highlighting. Each file's hunks can be collapsed. The diff takes the full width at first; two side panels are there when you want them:

- **Files** — a sidebar on the left listing the changed files, nested by directory. Toggle it with `F`.
- **Comments** — a rail on the right listing your review comments. Toggle it with `C`.

The diff can be shown **stacked** (one unified column) or **side by side** (old and new in two columns); press `1`, `2`, or `0` to pick stacked, side by side, or automatic by window width. Press `?` inside a review for the full list of keys.

![Review Diff showing a file side by side, old version on the left and working copy on the right, with the Files sidebar listing unstaged and untracked files](/images/git-review-diff.png)

*Review Diff, side by side, with the Files sidebar open.*

Entry points (all in the command palette):

- **Review Diff** — everything staged, unstaged, and untracked in the working tree right now.
- **Review Diff: Range (Commit or Branch)** — any range expression, e.g. `main..feature` or `HEAD~5...HEAD`, or a single commit SHA. This is how you review a whole branch: the commits arrive flattened into one diff.
- **Review Diff: Stash** — a stash entry, `stash@{0}` by default.

Everything that acts on an open review — **Review Diff: Stop**, **Refresh**, **Add Comment**, **Edit Note**, **Export to Markdown**, **Export to JSON** — shares the same `Review Diff: ` prefix and appears in the palette once a review is open.

Inside a review:

- **`n` / `p`** jump to the next and previous hunk; **`,` / `.`** jump between files; `Tab` moves focus between the panels.
- Stage, unstage, or discard the hunk, file, or a line-level visual selection on the cursor row.
- **Big changesets** — a range spanning a hundred commits is laid out in full, like any other review; scrolling and hunk navigation cover the whole diff.
- **Very large files** — a single file over 1 MiB is listed and marked `(binary or too large to diff)` rather than expanded. A patch that long is not something a review reads, and rendering one stalls the editor; open the file itself to see it. Binary files are marked the same way. The cap applies to every `git diff` the editor runs, so no feature stalls on a huge blob.
- **Comments** — leave a line comment or a review-wide note. Comments persist per repository across editor restarts, so you can close the editor mid-review and pick up where you left off. The Comments rail lets you jump through them, edit, delete, or export to Markdown.

## Git Log

**Git Log** opens a live-preview commit history. Moving through the log updates the right panel with the syntax-highlighted diff for the selected commit — no need to open each one to see what it touched. Commit messages wrap, columns align, and the toolbar is clickable. **Git Log: Current File** scopes the history to the active buffer's file, and **Git Log: PR Branch** scopes it to the commits on the current branch against a base ref, with a side-by-side `git show` for the selected commit (**Git Log: Close PR Branch** and **Git Log: Refresh PR Branch** act on that panel). To *review* those commits rather than browse them, use **Review Diff: Range**.

![Git Log with the commit list on the left and the selected commit's message and diff on the right](/images/git-log.png)

## Git Blame

**Git Blame** opens a magit-style blame for the current file. It lands on the line you were on when you ran it (multi-byte text included), keeping that line on the same screen row it already occupied so you do not have to re-find it. **Git Blame: Go Back** — `b` in the panel — re-blames at the parent commit of the current line so you can walk a line's history, again holding the line's screen row steady. `q` steps back out one hop at a time: it unwinds the last `b`, and closes the panel only once there are no hops left, so a blame walked two commits deep takes three presses to leave. `Escape`, and the **Git Blame: Close** command, dismiss the panel outright.

## Git Gutter

The gutter marks lines that differ from git: added, modified, and deleted lines each get their own colour. The same hunks, and your unsaved edits, are marked on the scrollbar, so changes elsewhere in the file are visible without scrolling to find them. The marks refresh when you save; run **Git Gutter: Refresh** to update them by hand.

## Live Diff

**Live Diff** marks changed lines in the gutter as you edit, comparing the buffer against a reference. Toggle it for the current buffer with **Live Diff: Toggle (Buffer)** or everywhere with **Live Diff: Toggle (Global)**, and choose the reference with **Live Diff: vs HEAD**, **vs Disk (unsaved changes)**, **vs Branch...**, or **vs Default Branch**. Removed lines appear above the edit, and word-level highlighting marks exactly which words changed.

## Monorepos and Nested Repositories

Git features work inside nested repositories even when the folder you opened is not itself a repository. Each sub-project gets file-explorer git decorations, its own branch in the status bar's optional **Git: branch** element (`{git_statusbar:branch}`), **Git Blame**, **Git Log**, **Git Grep** and **Git Find File**.

## Change Navigation

**Next Change** and **Previous Change** jump between the changed regions of the current buffer. They merge the git gutter's hunks, Live Diff's hunks, and your unsaved edits into one list, so the same two commands work whichever is showing; the status bar reports where you are (`Change 2/6`).

See it in action: [Review Diff](/blog/fresh-0.3.0/#review-diff-rewrite) and [Git Log](/blog/fresh-0.3.0/#git-log) in the 0.3.0 blog post.
