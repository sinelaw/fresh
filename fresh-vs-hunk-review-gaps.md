# Fresh "Review Diff" vs. `hunk` — factual comparison and gap list

**Date:** 2026-06-07
**Method:** Both tools driven interactively in `tmux` against the same working-tree
changeset in this repo (3 files: a modified `README.md`, a modified `path_utils.rs`,
a new untracked `workspace_guard.rs`). Panes captured with `tmux capture-pane -e`.
Everything below was observed by *using* the tools, not by reading source.

- Fresh review tool: the `audit_mode` plugin, launched via command palette → **Review Diff**
  (also **Review PR Branch**, **Review Range**). Fresh built from this repo (`target/debug/fresh`).
- `hunk`: `modem-dev/hunk` v0.14.1, `hunk diff`.

---

## PART 1 — Objective factual differences

### A. Form factor / integration

| | **Fresh — Review Diff** | **hunk** |
|---|---|---|
| What it is | A mode inside the editor; opens a read-only `*Review Diff*` **buffer/tab** | A standalone TUI in a separate process, launched from the shell |
| Chrome | Editor menu bar, tab bar, and status line remain; a 2-line hint bar at top | Own full-screen TUI: top menu bar (File/View/Navigate/Theme/Agent/Help) + sidebar |
| Launch | Command palette (`Ctrl+P` → "Review Diff") | `hunk diff` / `hunk show` / `hunk diff <ref>` from a shell |
| Leaving | `q` returns to editor | `q` quits the process |

### B. Diff layout

| | **Fresh** | **hunk** |
|---|---|---|
| Layout in the review session | **Unified only** (single column, old+new line-number gutters) | **Split, Stack, or Auto**, toggled live with `1`/`2`/`0` |
| Side-by-side | Exists, but as a **separate** per-file command ("Side-by-Side Diff"), OLD(HEAD)│NEW(Working); **no comments, not part of the review session**; errors on the review buffer ("No file open") | Built into the same view; `1` switches the whole multi-file review to side-by-side |
| Syntax highlighting in diff | **No** — lines colored only by add/remove (green/plain) | **Yes** — full per-token syntax highlighting on both sides |
| Intraline (word-level) highlight | Not observed | Yes — changed words highlighted within a line |
| Context folding | `Tab` fold hunk, `z a` fold all, `z r` unfold all | `z` toggles "N unchanged lines" folds; expandable |
| Color depth | **256-color** (`38;5;n`), follows the editor theme | **24-bit truecolor** (`38;2;r;g;b`) |
| Themes | Inherits the one active editor theme | 6 diff-specific themes (Graphite/Midnight/Paper/Ember/Catppuccin×2), switched live |

### C. Organization & navigation

| | **Fresh** | **hunk** |
|---|---|---|
| Grouping | By **git index state**: `STAGED` / `UNSTAGED` / `UNTRACKED` sections | By **file** only (flat sidebar file list) |
| File list / sidebar | No separate sidebar; files are headers inline in the scroll | Dedicated left sidebar with per-file `+/-` counts and a `*N` comment badge |
| Hunk nav | `n` / `p` | `[` / `]` |
| File nav | (scroll / jump) | `,` / `.` |
| Comment nav | `]` / `[` | `{` / `}` |
| Help discovery | 2-line hint bar always visible at top | `?` full keymap overlay; `F10` menus |

### D. Comments / review notes

| | **Fresh** | **hunk** |
|---|---|---|
| Add comment | `c` on a diff line → prompt "Comment on L20:" | `c`, or via the daemon CLI |
| Rendering | Inline single line `» [20] text` **and** a right-hand `COMMENTS` panel | Inline **bordered multi-line box** anchored to the line (summary + rationale) |
| Panel | Yes, fixed right column; long text truncated (`…`) | No panel; notes live inline; sidebar shows per-file counts |
| Persistence | Saved to disk under `.review/` and restored on reopen | Lives in the live session |
| Export | `e` → Markdown (`.review/session.md`); also a JSON export command | Not a built-in export; the session model is queryable as JSON via the CLI |

### E. Git actions (the big functional divergence)

| | **Fresh** | **hunk** |
|---|---|---|
| Stage / unstage hunk | `s` / `u` — **mutates the git index** (verified with `git diff --cached`; the file moved into a `STAGED` section live) | **None** — hunk is review-only |
| Discard hunk | `d` | None |
| File-level stage/unstage/discard | `S` / `U` / `D` | None |
| Partial selection | `v` (select lines) | None |
| Jump to source / edit | `Enter` jump, `Alt+o` open the real file to edit | `Enter`/`Alt+o` open file in `$EDITOR`; no in-app editing |

### F. Input sources / scope

| | **Fresh** | **hunk** |
|---|---|---|
| Working tree | Review Diff | `hunk diff` |
| Commit range / branch | Review Range (`A..B`, `A...B`, `<sha>`), Review PR Branch (vs base) | `hunk diff <ref>` / `hunk show <ref>`; live `reload` to swap inputs |
| Staged-only | Implicit via the STAGED section | `hunk diff --staged` |
| Stash / patch / stdin | Not observed | `hunk stash show`, `hunk patch -` |
| jj / Sapling | Not observed | Auto-detected, native revsets |
| Pager / difftool | Not observed | `hunk pager`, `hunk difftool` |

### G. Automation / agent interface

| | **Fresh** | **hunk** |
|---|---|---|
| Programmatic control | None observed; the tool is keyboard-driven for a human (the plugin host could drive it, but there is no external review CLI) | **Daemon + `hunk session` CLI**: `list`/`get`/`review --json`/`context`/`navigate`/`comment add|apply`/`reload`. An agent can inspect the human's live window, move the cursor, and post inline notes |
| Watch / auto-reload | Not observed (manual refresh command exists) | `--watch` auto-reloads on working-tree changes |
| Agent rationale sidecar | No | `--agent-context <json>` |

### H. Things Fresh's review tool has that hunk does not
- Real **git staging / unstaging / discarding** at hunk, line, and file granularity.
- **STAGED/UNSTAGED/UNTRACKED** grouping (index-aware).
- **Editor integration**: jump to and edit the actual file in the same app; shares tabs/theme/keymap.
- **Comment persistence to disk** + **Markdown/JSON export** of the review session.
- **PR-branch** and **flattened-range** review modes out of the box.

### I. Things hunk has that Fresh's review tool does not
- Multiple **live layouts** (split / stack / auto) inside the review.
- **Syntax highlighting** and word-level intraline highlighting in the diff.
- Dedicated **file sidebar** with counts + comment badges.
- **Diff-specific themes** and truecolor.
- **Agent/daemon CLI** for programmatic review and comment injection.
- **Watch mode**, **stash/patch/stdin** inputs, **jj/Sapling**, **pager/difftool**.
- `?` keymap overlay; **multi-line bordered** inline notes with summary + rationale.

---

## PART 2 — Opinion: actionable UX changes for Fresh

Ordered by impact-to-effort. The goal is to make Fresh's *review* experience match
hunk's *reading* quality while keeping Fresh's unique edge (staging + editor integration).

### Tier 1 — closes the most visible quality gap
1. **Syntax-highlight diff lines in the review buffer.** This is the single biggest
   visual gap. The review buffer currently renders add/remove coloring only; hunk shows
   full token coloring on both sides. Fresh already has syntax engines — reuse them in
   the `*Review Diff*` buffer.
2. **Add word-level intraline highlighting** for modified lines (emphasize the changed
   span, dim the unchanged remainder). Pairs naturally with #1.
3. **Bring side-by-side *into* the review session.** Today it's a disconnected per-file
   command with no comments and it errors on the review buffer. Make split/unified a
   live toggle of the review view itself (mirror hunk's `1`/`2`/`0`), carrying comments
   and staging across both modes.

### Tier 2 — navigation & legibility parity
4. **Add a persistent file sidebar / outline** for the review session with per-file
   `+/-` counts and a comment-count badge, and file-level jump. Scrolling a long flat
   list is the current weak point vs hunk's sidebar.
5. **Make the comments panel readable**: it truncates to one line with `…`. Allow
   multi-line notes, show author/rationale, and let the panel scroll/expand. Consider
   hunk-style anchored bordered notes as an option.
6. **Add a `?` keymap overlay.** The 2-line hint bar is good but incomplete; a full
   overlay (like hunk's) aids discovery of `S/U/D`, `v`, `z a`, range mode, etc.

### Tier 3 — workflow reach
7. **Watch / auto-refresh** the working-tree review on file changes (hunk `--watch`).
   Today refresh is manual.
8. **More input sources**: staged-only view, stash review, and review-from-stdin/patch,
   to match hunk's `--staged` / `stash show` / `patch -`.
9. **A scriptable review API** (the highest-ceiling item). hunk's daemon + `session`
   CLI is what makes it *agentic*. Fresh's differentiator could be even stronger: expose
   a plugin/IPC surface so an agent can open a review, post inline comments with
   rationale, and navigate — then a human reviews **and edits/stages** in the same app.
   This fuses hunk's agent-review model with Fresh's editing+staging, which neither tool
   does today.

### Explicitly *keep* (Fresh's edge — don't regress)
- Hunk/line/file **staging, unstaging, discarding** — hunk has nothing here.
- **Jump-to-file + edit in place**, shared tabs/theme/keymap.
- **Comment persistence + Markdown/JSON export**.
- **PR-branch / range** review modes.

### One-line summary
hunk is a better *reader* (layouts, syntax color, sidebar, agent CLL); Fresh is a better
*actor* (staging, editing, export). Close the reading gaps — **syntax highlighting,
in-session side-by-side, a file sidebar, and a readable comments panel** — and Fresh's
review tool becomes strictly more capable than hunk for a human reviewer, while a
**scriptable review API** would let it win the agentic use case too.
