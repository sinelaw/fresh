# UX Testing Report — PR #2369

**PR:** `fix: support monorepo/multi-repo workspaces with nested git sub-projects`
**Head:** `c78e11a` (can2049/fresh:fix/monorepo-git-support)
**Tested build:** `cargo build --release` of the PR head, driven interactively inside `tmux`
**Date:** 2026-07-03
**Method:** Black-box, end-behavior testing only. Every feature was exercised through the real UI (command palette, file explorer, keybindings) in a live `fresh` process. Git command execution was cross-checked against `--log-file` output to confirm the working directory each feature ran in.

---

## Verdict

The PR's **core promise works**: in a workspace whose root is *not* a git repo but which contains nested git sub-projects, the file explorer, blame, find-file, live-grep, merge-conflict resolution, review-diff, and git-gutter all correctly resolve to the **active file's own sub-repository**. Depth limiting, `node_modules` exclusion, sibling repos with shared prefixes, and **`.git`-as-a-symlink** all behave correctly. Single-repo workspaces are unaffected (no regression observed).

**However, two prominent git features that the PR summary claims to cover are still broken in a monorepo:**

| Feature | Monorepo result | Single-repo result |
|---|---|---|
| **Git Log** / **Git Log (Current File)** | ❌ "No commits found or not a git repository" | ✅ works |
| **Git Grep** (standalone command) | ❌ "No matches" (`git` exits 128, *not a git repository*) | ✅ works |

Both run git in the **workspace root** (not a repo) instead of the active file's sub-repo. The PR updated `live_grep.ts` but **not** the standalone `git_grep.ts`, and updated `git_history.ts`'s `git show` path but **not** its `git log` path (`fetchGitLog` still defaults to `editor.getCwd()`).

---

## Affected features / plugins / modes / UI elements

Derived from the 20 changed files; each was mapped to its user-facing surface.

### Core (Rust) — behavioral surfaces
- **`readDir` symlink following** (`quickjs_backend.rs`): `.git` symlinks now report as directories → sub-repo detection works when `.git` is a symlink.
- **Nested sub-repo discovery, BFS depth 3** (`file_operations.rs`, new `git_index.rs`): `workspace/group/project/` (depth ≤3) discovered; deeper excluded.
- **Multi-repo git-index watching** (`git_index.rs`): watches every discovered sub-repo's index.

### Plugins / UI features
| # | Feature (palette command / trigger) | Plugin |
|---|---|---|
| 1 | **File Explorer git decorations** — `M/A/D/U/!` badges, dir bubbling (`●`), `colorNames` | `git_explorer.ts` |
| 2 | **Status-bar branch** | `git_statusbar.ts` |
| 3 | **Git Blame** (mode `git-blame`: `b` back / `q` close / `y` yank hash) | `git_blame.ts` |
| 4 | **Git Find File** | `git_find_file.ts` |
| 5 | **Live Grep (Find in Files)** — providers incl. git-grep | `live_grep.ts` |
| 6 | **Merge Conflict** — 3-way resolution (OURS/RESULT/THEIRS) | `merge_conflict.ts` |
| 7 | **Git Log** / **Git Log (Current File)** | `git_log.ts` + `lib/git_history.ts` |
| 8 | **Review Diff / Audit Mode** (modes: review-diff, review-branch, review-range, review-stash) | `audit_mode.ts` |
| 9 | **Code Tour** | `code-tour.ts` |
| 10 | **Git Gutter** (change markers) — *not in PR, git-dependent* | `git_gutter.ts` |
| 11 | **Git Grep** (standalone command) — *not in PR* | `git_grep.ts` |

---

## Test fixtures (reproducible)

A workspace whose **root is not a git repo**, containing nested sub-repos at multiple depths, plus a single-repo control. Rebuildable via `scratchpad/make_fixtures.sh`:

```
fixtures/monorepo/                 <- NOT a git repo (workspace root)
├── project-a/                     depth 1  (M/A/D/U mix)
├── project-a-extra/               depth 1  (shared-prefix sibling of project-a; modified)
├── group/project-b/               depth 2  (modified)
├── a/b/project-c/                 depth 3  (modified)   <- at the limit → discovered
├── deep/x/y/project-d/            depth 4  (modified)   <- beyond limit → NOT discovered
├── node_modules/pkg/              has .git             <- must be skipped
├── project-sym/  (.git → symlink) depth 1  (modified)   <- symlink-.git case
└── project-conflict/              depth 1  (real merge conflict: UU conf.txt)
fixtures/singlerepo/               <- IS a git repo (regression control)
```

---

## Detailed results

### ✅ 1. File Explorer git decorations — PASS (the headline fix)
**Steps:** open `fresh` in `fixtures/monorepo`, `Ctrl+E`, expand nodes.
- Directory-level `●` shown on: `project-a`, `project-a-extra`, `group/project-b`, `a` (bubbled from depth-3 `project-c`), `project-sym`, `project-conflict`.
- **No** decoration on `node_modules/` → correctly skipped.
- **No** decoration on `deep/x/y/project-d/` → depth-4 repo correctly **not** discovered; its modified `file_d.txt` shows **no** `M`.
- **`project-sym`** (whose `.git` is a symlink) **is** decorated → symlink handling works.
- **`project-a-extra`** decorated independently of `project-a` → shared-prefix siblings not conflated.
- Per-file badges correct: `modified_a.txt → M`, `staged_a.txt → A`, `untracked_a.txt → U`, `clean_a.txt → (none)`.

### ✅ 2. Git Blame — PASS
**Steps:** open `group/project-b/main_b.txt` (depth 2), palette → *Git Blame*.
- Blame headers render per commit; edited line shows *"Not Committed Yet"*. Log: `git blame` ran in `cwd=.../group/project-b`.
- **`b` (go back):** on a multi-commit file, showed the parent commit's content. Log confirms `git show <hash>^:./clean_a.txt` in `cwd=.../project-a` — the changed `${commit}:./${name}` path, in the sub-repo. ✅
- **`y`:** "Copied: 04a263f (…full hash…)". **`q`:** closed, restored source buffer. ✅

### ✅ 3. Git Find File — PASS
**Steps:** with `project-a/clean_a.txt` active, palette → *Git Find File*.
- Listed only project-a's tracked files (`clean_a.txt`, `modified_a.txt`, `staged_a.txt`), repo-relative. Log: `git ls-files --full-name` in `cwd=.../project-a`.
- Selecting `modified_a.txt` opened the correct **absolute** path (relative→absolute mapping works). ✅

### ✅ 4. Live Grep — PASS (PR fix confirmed)
**Steps:** with `project-a` file active, palette → *Live Grep*, query `line` / `CHANGED`.
- Log shows the exact PR fix: availability probe `git rev-parse --is-inside-work-tree` at **workspace root fails**, then **falls back to the active buffer's dir** `project-a` (succeeds), then greps there.
- With `git-grep` provider: searches the active sub-repo. With `rg` provider (`Alt+P`): searches the whole tree — `CHANGED` matched `project-a`, `project-sym`, `project-a-extra` (incl. the symlink repo). ✅
- *Note:* git-grep provider is scoped to the **active** sub-repo only (see Observations).

### ✅ 5. Merge Conflict (3-way) — PASS
**Steps:** open `project-conflict/conf.txt`, palette → *Merge: Start Resolution*.
- Conflict auto-detected (status bar + markers). Three panes OURS / RESULT / THEIRS rendered.
- Log confirms `git show :1:conf.txt` (base), `:2:` (ours), `:3:` (theirs) all in `cwd=.../project-conflict` — the changed base-fetch path, in the sub-repo. ✅

### ✅ 6. Review Diff (audit_mode) — PASS
**Steps:** open `project-sym/mod_s.txt`, palette → *Review Diff*.
- `repoRoot` resolved via active buffer to `project-sym`; `git status`/`git diff` ran there; unstaged hunk `-s original / +s CHANGED` rendered correctly. Works on the symlink-`.git` repo. ✅
- (An earlier project-a run showed only the untracked file — this was because my own fixture-setup `git commit -am` had committed the M/A/D files; Review Diff faithfully reflected the real state.)

### ✅ 7. Git Gutter — PASS (unchanged by PR, still works)
- `project-a-extra/mod_e.txt` line 1 shows a change marker; `git diff HEAD -- <file>` ran in `cwd=.../project-a-extra`. Already used the file's dir, so no regression.

### ✅ 8. Single-repo regression — PASS
In `fixtures/singlerepo`: explorer decorations (`main.rs → M`, `new.rs → U`), **Review Diff** (both M+U with correct hunks), **Git Log** (lists commit + detail), gutter — all work as before.

### ❌ 9. Git Log — FAIL in monorepo
**Steps:** open `project-a/modified_a.txt`, palette → *Git Log* (also *Git Log (Current File)*).
- **Result:** status bar *"No commits found or not a git repository"*; empty panel — despite the file having real history.
- **Cause (from log):** `git log --format=… -- <abs path>` ran in `cwd=.../monorepo` (workspace root, not a repo). `fetchGitLog` in `lib/git_history.ts` defaults `cwd` to `editor.getCwd()`; the `git_log.ts` caller does not pass the sub-repo dir.
- **Works** in single-repo (same command, cwd = repo root). The PR body lists git log among fixed commands, but only the `git show` (commit-detail) cwd was updated, not `git log`.

### ❌ 10. Git Grep (standalone command) — FAIL in monorepo
**Steps:** with `project-a` file active, palette → *Git Grep*, query `line`.
- **Result:** *"No matches"* — `git grep` exited **128: fatal: not a git repository**.
- **Cause (from log):** `git grep -n --column -I -- line` ran in `cwd=.../monorepo` (workspace root).
- **Works** in single-repo. `git_grep.ts` was **not** among the PR's changed files; only `live_grep.ts` was fixed. Users who invoke the dedicated "Git Grep" command in a monorepo still hit the original failure.

---

## Observations (not defects introduced by this PR)

1. **Status-bar branch label not visibly rendered.** The `git_statusbar` plugin runs `git rev-parse --abbrev-ref HEAD` in the correct cwd (active buffer's sub-repo — confirmed in log), but no branch segment appeared in the status bar in **either** the monorepo or single-repo workspace. So this is a pre-existing display/layout detail, **not** a regression from this PR; I could not visually confirm the branch label renders at all in this build/terminal.

2. **Live Grep with the `git-grep` provider searches only the active sub-repo**, not all sub-repos in the monorepo. This is expected given git-grep's single-repo semantics; switching to the `rg` provider (`Alt+P`) searches the entire workspace tree. Worth documenting for monorepo users.

3. **Explorer decorations don't live-update on *external* git changes** (e.g., a commit made in another terminal) until a `focus_gained` / explorer interaction / file save occurs. `git_explorer.ts` refreshes on those editor events, not on a git-index change event. Manual interaction (collapse/expand) refreshed correctly. This matches single-repo behavior and could not be fully exercised because the headless tmux harness cannot deliver a terminal focus event.

---

## Not covered
- **Code Tour** (`code-tour.ts` `git rev-parse` cwd) — not exercised; low risk (single `rev-parse`).
- **Review PR Branch / Review Range / Review Stash** (audit_mode variants) — only *Review Diff* of the audit_mode family was driven end-to-end.

---

## Suggested follow-ups for the author
1. Make `git_log.ts` pass the active buffer's sub-repo dir as `cwd` (mirror `resolveGitRepo` from `lib/git_repo.ts`), or default `fetchGitLog`'s cwd to a resolved repo rather than `editor.getCwd()`.
2. Apply the same active-buffer-dir resolution to the standalone `git_grep.ts` command (the PR body lists "git grep" as covered, but only `live_grep.ts` was updated).
