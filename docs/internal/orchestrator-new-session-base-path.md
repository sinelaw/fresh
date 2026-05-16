# Orchestrator: New Session — Base Path + Worktree Toggle

> **Status**: Design Document
> **Date**: May 2026
> **Driving feature**: Let the user create Orchestrator sessions
> against an arbitrary base path (any directory, not necessarily
> the current cwd), and let them choose whether the session gets
> its own git worktree or runs directly inside the given path.

## Motivation

Today the New Session dialog has three inputs — Session Name,
Agent Command, Branch — and silently assumes:

1. The current working directory is a git repository.
2. The user wants the new session in a *fresh git worktree* of
   that repository, forked off origin's default branch.

That works for the common case (one editor instance per repo,
one agent per branch) but fails the long tail:

- **Non-git directories.** "I want to run an agent in
  `~/notes/` to refactor my markdown" is currently impossible:
  `git worktree add` aborts and the dialog reports
  `not a git repository`.
- **Multiple agents on the same worktree.** Two agents that
  share a checkout (e.g. one driving the editor, one running
  long-running builds) need the *same* path, not two parallel
  worktrees.
- **Working from a linked worktree.** The current dialog
  already corrects the slug back to the main worktree — but it
  doesn't let the user choose a *different* base repo when
  they have several checked out side-by-side.
- **Foreign repos.** "Spin up an agent against
  `~/repos/upstream-thirdparty/` to investigate a bug" needs
  the user to point the dialog at that repo without having to
  first `cd` the editor there.

The goal is for users to be able to create sessions
*regardless of current git state, or even using git at all*.

## Wireframe

### Default state — base path pre-filled from the canonical repo root

```
╭─ ORCHESTRATOR :: New Session Dialog :: Review Synthesized ───────────╮
│                                                                      │
│                      Project: noam/fresh                             │
│                                                                      │
│ ╭─ Base Repository Path ───────────────────────────────────────────╮ │
│ │ [/home/noam/repos/fresh                                         ]│ │
│ ╰──────────────────────────────────────────────────────────────────╯ │
│   ↳ canonical repo root (worktree-resolved). Leave blank to keep.    │
│                                                                      │
│ [x] Create a new git worktree for this session                       │
│      └─ unchecked = run the session directly inside the path above   │
│         (use this for non-git paths, or to share a worktree across   │
│         multiple sessions)                                           │
│                                                                      │
│ ╭─ Session Name ───────────────────────────────────────────────────╮ │
│ │ [                                                  ] (auto-gen) │ │
│ ╰──────────────────────────────────────────────────────────────────╯ │
│                                                                      │
│ ╭─ Agent Command ──────────────────────────────────────────────────╮ │
│ │ [                                                  ] (claude)   │ │
│ ╰──────────────────────────────────────────────────────────────────╯ │
│                                                                      │
│ ╭─ Branch ─────────────────────────────────────────────────────────╮ │
│ │ [                                                  ] (origin/m…)│ │
│ ╰──────────────────────────────────────────────────────────────────╯ │
│   ↳ ignored when "Create a new git worktree" is unchecked            │
│                                                                      │
│                              [ Cancel ]   [ Create Session ]         │
│                                                                      │
│  Tab next · S-Tab prev · Space toggle · Enter act · Esc cancel       │
╰──────────────────────────────────────────────────────────────────────╯
```

### Non-git path — checkbox auto-cleared, branch row dimmed

When the user types (or pastes) a path that isn't inside a git
working tree, the dialog detects it asynchronously (via
`git -C <path> rev-parse --is-inside-work-tree`) and:

- Forces "Create a new git worktree" to **off** (the option
  isn't meaningful — there's no repo to fork from).
- Renders the checkbox as `[·]` with a dim foreground and the
  hint text `non-git path — worktree creation unavailable`.
- Renders the Branch row dim with the placeholder
  `(no git — branch not applicable)` and skips it in the Tab
  cycle.

```
│ ╭─ Base Repository Path ───────────────────────────────────────────╮ │
│ │ [/home/noam/notes                                               ]│ │
│ ╰──────────────────────────────────────────────────────────────────╯ │
│   ↳ not a git working tree                                           │
│                                                                      │
│ [·] Create a new git worktree for this session   (non-git path)      │
│                                                                      │
│ ╭─ Branch ─────────────────────────────────────────────────────╮ dim │
│ │                                       (no git — N/A)        │     │
│ ╰──────────────────────────────────────────────────────────────╯     │
```

### Git path, worktree toggle off — "share-the-checkout" mode

When the user explicitly unchecks the worktree option on a git
path, the dialog stays interactive but warns about the
implications:

```
│ [ ] Create a new git worktree for this session                       │
│      ⚠ session will share its working tree with any other sessions  │
│         rooted at this path; concurrent writes may conflict.         │
│                                                                      │
│ ╭─ Branch ─────────────────────────────────────────────────────╮ dim │
│ │                              (shared worktree — N/A)        │     │
│ ╰──────────────────────────────────────────────────────────────╯     │
```

The Branch field becomes inert in this mode for the same reason
as the non-git case: there's no `git worktree add` to fork off
a ref.

## Field semantics

| Field                  | Default                                       | When empty submits as |
|------------------------|-----------------------------------------------|-----------------------|
| Base Repository Path   | canonical repo root resolved from editor cwd  | the placeholder default (canonical repo root) |
| Create worktree (cb)   | checked iff path resolves to a git work tree  | (boolean — no empty)  |
| Session Name           | empty                                         | auto-generated (`session-N`) |
| Agent Command          | empty (placeholder = `lastCmd` or `terminal`) | the placeholder       |
| Branch                 | empty (placeholder = detected default branch) | the placeholder (only valid when worktree=on) |

### "Canonical repo root" resolution

The pre-filled default for Base Repository Path is derived from
the editor's cwd in this order:

1. `git -C <cwd> rev-parse --path-format=absolute --git-common-dir`
   → `dirname(...)` of the result is the **main worktree's
   root**, regardless of whether the editor was launched from a
   linked worktree. This matches the existing logic in
   `submitForm` (the slug-resolution path) and protects against
   nested-orchestrator path blow-up.
2. If `git` rejects the cwd (not a working tree), fall back to
   the editor's cwd verbatim. The placeholder text changes to
   `(non-git — sessions run in-place)` so the user knows what
   they're committing to.

The probe runs at `openForm` time, asynchronously, the same way
the current default-branch probe does. While it's in flight the
input renders the cwd as the placeholder; the resolved value
replaces it on completion if the field is still empty.

### Worktree checkbox — interaction model

- **Checked + git path** → today's behaviour:
  `git worktree add <root> -b <branch> <base>` rooted at
  `<XDG>/orchestrator/<slug-of-base-path>/<session-name>/`.
- **Unchecked + git path** → session root is the **base path
  itself**. No `git worktree add`. The session inherits whatever
  branch the worktree is currently on. Branch field is inert.
- **Checked + non-git path** → impossible (checkbox is forced
  off and grayed).
- **Unchecked + non-git path** → session root is the base path.
  No git interaction at all. Branch field is inert.

When the worktree is shared (unchecked + git path) the session
record still goes into the normal persistence layer; it's just
that multiple sessions can legitimately resolve to the same
`root`. Reconciliation already keys on session id, not root, so
this works without changes to `orchestrator_persistence.rs`.

## Where the multi-window list lives

This is the open question the spec calls out: when a user
returns days later and wants to resume an orchestration, where
do we look for the session list?

### Current behaviour

`orchestrator_persistence.rs` writes
`<XDG data>/fresh/orchestrator/<encoded_working_dir>/windows.json`,
keyed by **the editor process's working directory** at launch
time (encoded via `workspace::encode_path_for_filename`). This
means:

- Two editor windows launched from the same cwd share their
  windows list (and would clobber each other).
- An editor launched from `/home/noam/repos/fresh` and one
  launched from `/home/noam/repos/fresh/sub-dir` see **different
  windows lists**, even though they're in the same repo.
- An editor launched from `/tmp/scratch` works for non-git use
  cases — the keying doesn't require git.

### Three candidate keyings

#### A. Per editor working directory (status quo)

- **Pro**: works for non-git paths trivially; matches how users
  think about "the editor I opened in this folder".
- **Con**: keying by cwd is brittle — `cd repos/fresh` vs.
  `cd ~/repos/fresh` writes to different encoded keys. Two
  editor instances in the same project but different
  subdirectories see disjoint session lists.
- **Migration**: zero.

#### B. Per canonical git repo root (with non-git fallback)

Resolve cwd to the canonical repo root (the same logic that
fills the Base Repository Path default) and key the persistence
directory on that. For non-git paths, fall back to encoding the
path verbatim — i.e. `<XDG>/orchestrator/<slug-of-repo-or-path>/`.

- **Pro**: matches the user's mental model — "open my fresh
  sessions for *this project*" produces the same list whether
  the editor was launched from the repo root, a sub-directory,
  or a linked worktree.
- **Con**: surprises users who launch the editor from a
  symlinked / sibling path expecting a fresh slate. Mitigation:
  a tiny status-bar message on first load
  (`Resumed N sessions from <repo>`) keeps the resolution
  visible.
- **Migration**: scan existing
  `<XDG>/orchestrator/<encoded_cwd>/windows.json` files and
  fold matches into the canonical-repo bucket on first launch.
  Conflicts (two cwd-keyed files mapping to the same repo)
  merge by union; ids stay stable because they're already
  globally unique within a `windows.json`.

#### C. Global per-user

One `<XDG>/orchestrator/windows.json` with a `base_path` /
`repo_root` field on each session entry.

- **Pro**: a single "list every orchestration I've ever run"
  view; cleanest answer for "where is the data".
- **Con**: every editor instance has to filter to the
  sessions relevant to it, and the file becomes a hot spot
  for concurrent writers (two editors saving on quit at the
  same time). The orchestrator dialog already exists per
  editor instance, and most users never want to see another
  project's sessions in *this* editor's picker.
- **Migration**: as in B, plus everything funnels into one
  file. The concurrent-writer risk argues for a per-session
  fragmented layout
  (`<XDG>/orchestrator/sessions/<id>.json`) rather than a
  single monolithic file — which is a bigger refactor.

### Recommendation

**Adopt B** (per canonical git repo root, with a verbatim-path
fallback for non-git launches), and surface the resolution in
the dialog's subtitle:

```
Project: noam/fresh   (sessions stored under .../orchestrator/home_noam_repos_fresh/)
```

This:

- Decouples the persistence key from incidental cwd choices.
- Works for non-git paths (the fallback keys on the path
  verbatim, slug-encoded — same scheme as the new-session
  worktree slug).
- Preserves the "what is this directory's worth of sessions"
  affordance that users already have a mental model for.
- Avoids the concurrent-write headache of option C.

The base-path field in the New Session dialog reuses the
*same* resolution: typing a path there both targets the new
session AND, on the next editor launch from that path, restores
the same windows list. The two features compose cleanly.

A future "cross-repo session browser" can layer on top of B by
listing the directories under `<XDG>/orchestrator/` — but it's
a separate feature, not a precondition for this change.

## Behavioural details

### Validation order on submit

1. Trim the Base Repository Path. Substitute the placeholder
   (canonical repo root or cwd) if empty.
2. `editor.pathExists` the result. If missing, render
   `path does not exist` in the in-dialog error row and bail.
3. Probe `git -C <path> rev-parse --is-inside-work-tree`.
   - If yes and worktree-toggle is checked → existing path
     (worktree-add) runs.
   - If yes and worktree-toggle is unchecked → use the path
     as-is for the session root; skip `git worktree add`;
     ignore Branch.
   - If no, force worktree-toggle off (UI was already showing
     this); use the path as-is.
4. Auto-generate session name if empty (existing logic, but
   the namespace it scans is now keyed on the resolved base
   path).
5. Create the session via `editor.createWindow({ root, ... })`
   exactly as today.

### Backwards compatibility

The form's existing behaviour is the **default** for a
git-cwd launch: the path field pre-fills to the canonical
repo root, the worktree checkbox starts checked, and pressing
Enter through the form lands on Create with all the same
behaviour as today. The new options are additive — users who
never touch them see the dialog they're used to (plus the new
top-of-form path row).

### Focus / tab order in the new dialog

```
Base Path → Worktree Checkbox → Session Name → Agent Command
         → Branch (skipped when inert) → Cancel → Create
```

- `Space` toggles the checkbox while it has focus.
- `Tab` skips the Branch field when it's inert (non-git path
  or worktree=off).
- Default focus is the Base Path field (matches the layout's
  top-to-bottom reading order; the user's first decision is
  *where* the session runs).

## Out of scope

- Browsing for the base path with a file picker. The plain
  text input is enough for the first cut; users paste paths
  from their shell or terminal. A `Browse…` button can come
  later as a small button next to the field.
- Reusing an existing branch on a non-base-path target
  (e.g. "create a session in `/tmp/scratch` but check out
  branch `feat/x`"). The current shape — checkbox on / off —
  doesn't have room for "yes worktree but at this custom
  root path". If it becomes a real ask, a dedicated
  `Worktree Root` row appears below the checkbox.
- Tracking shared-worktree sessions in the open dialog with
  a distinct badge. The list already shows the root path; two
  sessions on the same root render adjacent and look correct.
  A `SHARED` badge can come if the visual collision is a real
  problem in practice.

## Implementation phases

### Phase 1 — Base Path field

- Add the Base Path text input to `buildFormSpec` above the
  Session Name row.
- Wire the placeholder probe (canonical repo root via
  `git rev-parse --path-format=absolute --git-common-dir`,
  with cwd fallback) into `openForm` alongside the existing
  `defaultBranch` probe.
- `submitForm` substitutes the placeholder when the field is
  empty, then uses the resolved value as `repoRoot` for the
  rest of the existing flow.

### Phase 2 — Worktree checkbox

- Add `createWorktree: boolean` to `NewSessionForm`,
  defaulting to `true`.
- Render a `checkbox` widget (new widget kind or styled
  `button` with a `[x] / [ ]` glyph, depending on widget
  library state).
- On submit, branch the create path:
  - `createWorktree === true` → existing
    `git worktree add` flow.
  - `createWorktree === false` → `root = <base path>`,
    skip the worktree-add subprocesses and the branch
    handling.

### Phase 3 — Non-git path detection

- Async probe of `rev-parse --is-inside-work-tree` against
  the typed path; debounce on every change to the Base Path
  field (200ms).
- Force-clear `createWorktree` and dim the Branch row when
  the probe reports non-git.

### Phase 4 — Persistence keying by canonical repo root

- Change `orchestrator_persistence::orchestrator_dir` to
  resolve `working_dir` via the same git logic before
  encoding. Fall back to the verbatim path encoding when git
  reports no working tree (matches today's behaviour for
  non-git launches).
- Migration: on first load, look for the legacy
  `<XDG>/orchestrator/<encoded_cwd>/windows.json` and merge
  any sessions into the canonical-repo bucket.
- Subtitle of both dialogs gains a small `Sessions stored
  under …` annotation so the keying isn't invisible.

### Phase 5 — Shared-worktree session UX polish

- Surface a "shared with N other sessions" hint in the Open
  dialog's preview pane when more than one session resolves
  to the same root.
- Decide whether `Stop` / `Archive` / `Delete` on a shared-
  worktree session means "this row only" or "everything at
  this root". Leaning: row-only for Stop, but Archive /
  Delete refuse with a "remove the other sessions on this
  root first" error.

## Open questions

- **Where does a non-git session's data live on disk?** Two
  natural answers: (a) the path the user gave us (so all
  artefacts stay with their work); (b) under
  `<XDG>/orchestrator/<slug>/`, the same as the git case
  (clean separation, no surprise dotfiles in the user's
  folder). Leaning toward (a) — the user explicitly opted out
  of the worktree, so they probably want their files where
  they pointed us.
- **Inferring `createWorktree` from path content.** If the
  user pastes a path that's already a Fresh orchestrator
  session root (under `<XDG>/orchestrator/<slug>/<session>/`),
  the dialog could default the checkbox to off automatically.
  Worth doing in Phase 3 if the detection is cheap.
- **Path completion.** The text input doesn't currently have
  filesystem-aware completion. Worth a separate proposal —
  the host already has a fuzzy file picker we could embed,
  but the UX of "embed a picker in a form field" needs its
  own design.
