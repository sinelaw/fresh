# Orchestrator: New Session — Project Path + Worktree Toggle

> **Status**: Design Document
> **Date**: May 2026
> **Driving feature**: Let the user create Orchestrator sessions
> against an arbitrary project path (any directory, not
> necessarily the current cwd), and let them choose whether the
> session gets its own git worktree or runs directly inside the
> given path.

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

### Default state — project path pre-filled from the canonical repo root

The "Project: <label>" subtitle is gone: the Project Path field
itself is the project identifier now, so a static label above
it would just duplicate (or worse, drift from) the input.

All four text inputs (Project Path, Session Name, Agent
Command, Branch) carry **value history**: Up / Down on a
focused input scrolls through the values the user has
previously submitted in that field, MRU-ordered, much like a
shell prompt. An empty value at the bottom of the stack is the
"clear" entry. History is per-field, stored globally per user
(see [Where the multi-window list lives](#where-the-multi-window-list-lives))
so it follows the user across projects.

```
╭─ ORCHESTRATOR :: New Session Dialog :: Review Synthesized ───────────╮
│                                                                      │
│ ╭─ Project Path ───────────────────────────────────────────────────╮ │
│ │ [/home/noam/repos/fresh                                         ]│ │
│ ╰──────────────────────────────────────────────────────────────────╯ │
│   ↳ canonical repo root (worktree-resolved). ↑↓ for history.         │
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
│  Tab next · S-Tab prev · ↑↓ history · Space toggle · Enter act · Esc │
╰──────────────────────────────────────────────────────────────────────╯
```

Inputs stay stacked vertically full-width (not packed
side-by-side) so long paths and commands have room to breathe
without truncation or horizontal scrolling.

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
│ ╭─ Project Path ───────────────────────────────────────────────────╮ │
│ │ [/home/noam/notes                                               ]│ │
│ ╰──────────────────────────────────────────────────────────────────╯ │
│   ↳ not a git working tree. ↑↓ for history.                          │
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

| Field                | Default                                       | When empty submits as |
|----------------------|-----------------------------------------------|-----------------------|
| Project Path         | canonical repo root resolved from editor cwd  | the placeholder default (canonical repo root) |
| Create worktree (cb) | checked iff path resolves to a git work tree  | (boolean — no empty)  |
| Session Name         | empty                                         | auto-generated (`session-N`) |
| Agent Command        | empty (placeholder = `lastCmd` or `terminal`) | the placeholder       |
| Branch               | empty (placeholder = detected default branch) | the placeholder (only valid when worktree=on) |

### Input history (Up / Down)

Every text input in the form keeps a per-field history list.
The shape (stored under
`<XDG>/fresh/orchestrator/input_history.json`):

```json
{
  "version": 1,
  "project_path":   ["/home/noam/repos/fresh", "/home/noam/notes", …],
  "session_name":   ["bugfix-1991", "refactor-lsp", …],
  "agent_command":  ["claude", "python3 agent.py", …],
  "branch":         ["origin/main", "feat/diff-folding", …]
}
```

Behaviour:

- **↑ / Up** on a focused input: walk one entry older into
  history. The first press saves the current draft (whatever
  the user has typed but not submitted) at the top of the
  stack so ↓ can return to it.
- **↓ / Down** on a focused input: walk one entry newer; at
  the bottom of the stack, restore the saved draft (or
  empty).
- **Submit** appends the value to the front of that field's
  history (deduplicated — if the value already exists in the
  list it moves to the front rather than duplicating).
- **Empty submissions** (i.e. the user accepted the
  placeholder) record the placeholder's resolved value, not
  the empty string, so the next launch surfaces "fresh repo
  root" / "origin/main" / etc. by name.
- History is **global per user**, not per project — the
  windows.json store is global too (see [Where the multi-
  window list lives](#where-the-multi-window-list-lives)),
  and the user's mental model of "the commands I run" lives
  with them across projects.
- Capped at 100 entries per field, MRU-trimmed.
- The smart-key forwarder used by the Open dialog (where
  Up/Down on the filter input forward to the list) needs to
  be **disabled** for fields with history — Up/Down navigates
  history here, not a sibling list.

### "Canonical repo root" resolution

The pre-filled default for Project Path is derived from the
editor's cwd in this order:

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
  `git worktree add <root> -b <branch> <project-path>` rooted at
  `<XDG>/orchestrator/<slug-of-project-path>/<session-name>/`.
- **Unchecked + git path** → session root is the **project path
  itself**. No `git worktree add`. The session inherits whatever
  branch the worktree is currently on. Branch field is inert.
- **Checked + non-git path** → impossible (checkbox is forced
  off and grayed).
- **Unchecked + non-git path** → session root is the project
  path. No git interaction at all. Branch field is inert.

When the worktree is shared (unchecked + git path) the session
record still goes into the normal persistence layer; it's just
that multiple sessions can legitimately resolve to the same
`root`. Reconciliation already keys on session id, not root, so
this works without changes to `orchestrator_persistence.rs`.

## Where the multi-window list lives

**Decision: global per-user.** A single
`<XDG>/fresh/orchestrator/windows.json` holds every
orchestrator session the user has ever created, regardless of
which project they belong to. Sessions carry a `project_path`
field so the Open dialog can filter / group by project.

Rationale:

- The whole point of the project-path field is to **decouple**
  session creation from the editor's cwd. Persistence should
  follow the same principle — keying windows.json on cwd or on
  repo root would re-introduce the coupling the form is
  explicitly trying to break.
- A user running an agent in `~/notes/` (non-git) and another
  in `~/repos/fresh` (git) shouldn't have two disjoint stores
  with different schemas. One store, one schema, sessions
  filtered by project_path at read time.
- Users frequently want a cross-project "all my running
  agents" view — global is the natural home for it.
- Input history (project paths, agent commands, branch names)
  already lives globally for the same reason; co-locating
  windows.json with it keeps the storage model consistent.

### File layout

```
<XDG data>/fresh/orchestrator/
├── windows.json              ← single global store
├── input_history.json        ← per-field MRU history
└── <slug>/                   ← per-project worktrees / artefacts
    └── <session-name>/       ← session root (when worktree=on)
```

`windows.json` shape:

```json
{
  "version": 2,
  "active": 42,
  "next_id": 87,
  "windows": [
    {
      "id": 42,
      "label": "bugfix-1991",
      "root": "<XDG>/fresh/orchestrator/home_noam_repos_fresh/bugfix-1991",
      "project_path": "/home/noam/repos/fresh",
      "shared_worktree": false,
      "plugin_state": { … }
    },
    {
      "id": 43,
      "label": "notes-cleanup",
      "root": "/home/noam/notes",
      "project_path": "/home/noam/notes",
      "shared_worktree": true,
      "plugin_state": { … }
    }
  ]
}
```

### Filtering in the Open dialog

The picker bumps to two modes:

- **Project view** (default): shows only sessions whose
  `project_path` matches the editor's resolved project. The
  filter input ranks within that subset. Matches today's
  "sessions for this thing I'm working on" UX.
- **All-projects view** (toggle in the filter row, persists
  per editor instance): shows every session in `windows.json`,
  with the `project_path` rendered as a secondary column so
  cross-project rows are distinguishable.

The Open dialog's existing filter logic doesn't change — it
just operates on a subset.

### Concurrent writers

Two editors writing windows.json on quit is a real concern
(esp. when both instances watch the same `~/.local/share/`):

- **Read-modify-write with an atomic rename**: load the
  current file, splice in this editor's changes (touching
  only the ids this editor owns), write to `windows.json.tmp`,
  rename. Last writer wins for the `active` and `next_id`
  fields, but per-session entries are merged by id so neither
  editor clobbers the other's sessions.
- **`next_id` global**: kept monotonic by clamping to
  `max(local, on-disk) + 1` at write time. Two editors that
  both allocate id=87 will see the conflict at the next
  write boundary; the loser bumps to 88 and rewrites its
  in-memory state. (In practice id collisions are vanishingly
  rare because sessions are created interactively.)

This is enough for the common case — a single user across a
handful of editor instances. If contention ever becomes a
real problem the fragmented layout
(`<XDG>/orchestrator/sessions/<id>.json`) can drop in without
schema migration.

### Migration from per-cwd persistence

On first launch under v2:

1. Scan `<XDG>/fresh/orchestrator/*/windows.json` (the legacy
   per-cwd files).
2. For each entry, fill `project_path` by decoding the
   directory name (the slug → original path), and
   `shared_worktree = false` (the legacy flow always created
   a fresh worktree).
3. Merge everything into the new global `windows.json`; ids
   collide on the off chance two cwd-keyed files used the
   same id, in which case the most-recently-modified file
   wins and the loser gets re-numbered.
4. Leave the legacy files in place but rename them
   `windows.json.migrated.bak` so a downgrade isn't a one-way
   trip.

The migration runs once and is idempotent — re-running it is
a no-op once the v2 file exists.

## Behavioural details

### Validation order on submit

1. Trim the Project Path. Substitute the placeholder
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
   the namespace it scans is now keyed on the resolved
   project path).
5. Append the submitted (post-placeholder-substitution) values
   to each field's input history.
6. Create the session via `editor.createWindow({ root, ... })`
   exactly as today, and write the new entry into the global
   `windows.json` with the resolved `project_path` and
   `shared_worktree` flag.

### Backwards compatibility

The form's existing behaviour is the **default** for a
git-cwd launch: the Project Path field pre-fills to the
canonical repo root, the worktree checkbox starts checked,
and pressing Enter through the form lands on Create with all
the same behaviour as today. The new options are additive —
users who never touch them see the dialog they're used to
(plus the new top-of-form Project Path row).

### Focus / tab order in the new dialog

```
Project Path → Worktree Checkbox → Session Name → Agent Command
            → Branch (skipped when inert) → Cancel → Create
```

- `Space` toggles the checkbox while it has focus.
- `Tab` skips the Branch field when it's inert (non-git path
  or worktree=off).
- Default focus is the Project Path field (matches the
  layout's top-to-bottom reading order; the user's first
  decision is *where* the session runs).
- `↑` / `↓` walk history for the focused input — they no
  longer forward to anything else.

## Out of scope

- Browsing for the project path with a file picker. The plain
  text input is enough for the first cut; users paste paths
  from their shell or terminal, and history covers
  re-selection. A `Browse…` button can come later as a small
  button next to the field.
- Reusing an existing branch on a non-project-path target
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
- Per-project input history. History is global per user;
  scoping it to projects would force a more complex storage
  schema for marginal benefit (and most useful values — agent
  commands, common branch names — travel with the user).

## Implementation phases

### Phase 1 — Project Path field + drop subtitle

- Remove the `Project: <projectLabel>` subtitle row from
  `buildFormSpec`.
- Add the Project Path text input at the top of
  `buildFormSpec`, above the Session Name row.
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
  - `createWorktree === false` → `root = <project path>`,
    skip the worktree-add subprocesses and the branch
    handling.

### Phase 3 — Non-git path detection

- Async probe of `rev-parse --is-inside-work-tree` against
  the typed path; debounce on every change to the Project
  Path field (200ms).
- Force-clear `createWorktree` and dim the Branch row when
  the probe reports non-git.

### Phase 4 — Input history (Up / Down)

- Storage: `<XDG>/fresh/orchestrator/input_history.json`
  with the schema shown in
  [Input history (Up / Down)](#input-history-up--down).
- Plugin-side state: a `historyCursor` and `draftValue` per
  field on `NewSessionForm`. Up/Down adjust the cursor and
  rewrite `value` from the history list (saving the draft on
  the first ↑).
- The smart-key forwarder used in the Open dialog (filter →
  list) is opt-in via a `forwardArrows` flag on `text({…})`.
  Leave the flag off for the form's inputs so ↑/↓ don't
  forward.
- Submit: dedupe-merge the resolved value into the field's
  history, cap at 100, write the file (best-effort, fire-and-
  forget).

### Phase 5 — Global windows.json + migration

- Move persistence from
  `<XDG>/fresh/orchestrator/<encoded_cwd>/windows.json` to a
  single `<XDG>/fresh/orchestrator/windows.json`.
- Add `project_path` and `shared_worktree` to
  `PersistedWindow`. Bump the file version to 2.
- Migrate on first load: read all legacy per-cwd files,
  decode each filename → original cwd path, fold sessions
  into the new store with `project_path = decoded_cwd`,
  `shared_worktree = false`. Rename the legacy files to
  `windows.json.migrated.bak`.
- Add a `project_path` filter to the Open dialog's
  list-population step (default: only sessions whose
  `project_path` matches the editor's resolved project; the
  filter input bar gets a new toggle `[all projects]` to lift
  it).
- Concurrent-write safety via atomic-rename read-modify-write.

### Phase 6 — Shared-worktree session UX polish

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
