# Conductor: Open Dialog Redesign + Session Lifecycle

> **Status**: Design Document
> **Date**: May 2026
> **Driving feature**: A picker UX for browsing/operating on
> Conductor sessions, plus a richer session lifecycle (Stop /
> Archive / Delete) with cross-machine recovery.

## Motivation

The current `Conductor: Open` picker is built on the legacy
`startPrompt` infrastructure. It works for "list + filter +
pick one", but it can't host the action surface Conductor
needs:

- No per-session actions besides "dive" — killing a session
  requires a separate command.
- No custom keystroke handling — typing collides with filter
  text, and we can't bind `Alt+D` or similar to "kill the
  highlighted session" without leaking the chord to global
  bindings.
- Pseudo-rows (`+ New`, `× Kill highlighted`) get filtered out
  by the prompt's fuzzy matcher, so they vanish exactly when
  the user has typed something they want to act on.

Separately, the term "kill" is too coarse. Users want three
distinct lifecycle operations:

- **Stop** — abort the session's running processes but keep
  the worktree and editor session around.
- **Archive** — declare "I'm done with this for now" and move
  the worktree out of the way, while keeping it recoverable.
- **Delete** — permanently remove the worktree and all
  metadata.

Finally, when a session represents real work-in-progress, the
user wants to recover it on a different machine — pulling a
git branch should be enough to resume.

## Wireframe

The new dialog is rendered through the existing
`FloatingWidgetPanel` infrastructure (the same primitive the
new-session form already uses), composed from `text` (filter
input), `list` (sessions), and `labeledSection` (chrome) widgets.

### Normal state — session highlighted, action menu in preview pane

```
╭─ CONDUCTOR :: Sessions ────────────────────────────────────────────╮
│ ╭─ Filter ───────────────────────────────────────────────────────╮ │
│ │ [filter text                                                  ]│ │
│ ╰────────────────────────────────────────────────────────────────╯ │
│ ╭─ Sessions ─────────────╮ ╭─ [2] moshiko ──────────────────────╮  │
│ │   [1] ACT  fresh       │ │ Root:  /home/noam/repos/fresh/     │  │
│ │ ▸ [2] RUN  moshiko     │ │        .fresh/conductor/moshiko    │  │
│ │   [3] RUN  session-1   │ │ Age:   3m       State: RUN         │  │
│ │   [4] RUN  session-2   │ │ pgid:  12345    pids: 12345, 12387 │  │
│ │                        │ │ ────────────────────────────────── │  │
│ │                        │ │ Last terminal lines:               │  │
│ │                        │ │   $ make build                     │  │
│ │                        │ │   compiling...                     │  │
│ │                        │ │ ────────────────────────────────── │  │
│ │                        │ │ ▸ Dive into session                │  │
│ │                        │ │   Stop processes      (Alt+S)      │  │
│ │                        │ │   Archive             (Alt+A)      │  │
│ │                        │ │   Delete permanently  (Alt+D)      │  │
│ ╰────────────────────────╯ ╰────────────────────────────────────╯  │
│                                                                    │
│  ↑↓ nav · Tab focus · Enter activate · Alt+N new · Esc close       │
╰────────────────────────────────────────────────────────────────────╯
```

### Confirmation state — `Delete` chosen

Only irreversible actions confirm. `Stop` and `Archive` are
both recoverable (relaunch the agent; unarchive the session),
so they fire immediately.

```
╭─ CONDUCTOR :: Sessions ────────────────────────────────────────────╮
│ ╭─ Filter ───────────────────────────────────────────────────────╮ │
│ │ [filter text                                                  ]│ │
│ ╰────────────────────────────────────────────────────────────────╯ │
│ ╭─ Sessions ─────────────╮ ╭─ Confirm Delete ───────────────────╮  │
│ │   [1] ACT  fresh       │ │                                    │  │
│ │ ▸ [2] RUN  moshiko     │ │ Delete session [2] moshiko?        │  │
│ │   [3] RUN  session-1   │ │                                    │  │
│ │   [4] RUN  session-2   │ │ This will:                         │  │
│ │                        │ │   • stop all session processes     │  │
│ │                        │ │   • run `git worktree remove`      │  │
│ │                        │ │   • drop the session record        │  │
│ │                        │ │                                    │  │
│ │                        │ │ Uncommitted changes will be lost.  │  │
│ │                        │ │                                    │  │
│ │                        │ │ [ Confirm Delete ]   [ Cancel ]    │  │
│ ╰────────────────────────╯ ╰────────────────────────────────────╯  │
│                                                                    │
│  Tab focus · Enter activate · Esc cancel                           │
╰────────────────────────────────────────────────────────────────────╯
```

### Archived row state — action menu swaps

When the highlighted row is an archived session, `Archive`
becomes `Unarchive` and `Stop` is hidden (no live processes):

```
                            │ ▸ Dive into session                │
                            │   Unarchive           (Alt+A)      │
                            │   Delete permanently  (Alt+D)      │
```

Diving into an archived session implicitly unarchives it first
(can't activate a closed editor window).

## Action semantics

| Action     | Touches processes              | Touches worktree                                | Touches editor session | Recoverable                | Needs confirm |
|------------|--------------------------------|-------------------------------------------------|------------------------|----------------------------|---------------|
| Dive       | no                             | no                                              | sets active            | n/a                        | no            |
| Stop       | SIGTERM → SIGKILL the pgid     | no                                              | no                     | yes (relaunch the agent)   | no            |
| Archive    | stops first                    | `git worktree move` to `.archived/` graveyard   | `closeWindow`          | yes (Unarchive)            | no            |
| Unarchive  | no                             | `git worktree move` back to active path         | `createWindow`         | yes (Archive again)        | no            |
| Delete     | stops first                    | `git worktree remove` + rmdir                   | `closeWindow`          | **no**                     | yes           |
| New        | spawns                         | creates                                         | `createWindow`         | n/a                        | no            |

## Focus model and key surface

- Default focus: the filter input.
- `Tab` cycles `filter → list → Dive → Stop → Archive → Delete`
  (skipping `Stop` for archived rows, swapping `Archive` →
  `Unarchive`).
- `↑` / `↓` on the focused filter input forwards to the list
  (smart-key tweak so the user can both type and navigate
  without leaving the filter).
- `Enter` activates whichever element has focus. On a focused
  list row, `Enter` dives. On a focused button, `Enter` fires
  the button's action.
- `Esc` closes the dialog (and cancels confirmation when one
  is open).
- `Alt+S` / `Alt+A` / `Alt+D` / `Alt+N` are chord shortcuts.
  See [Keybinding integration](#keybinding-integration) for
  how they're registered and rendered cross-platform.

## Infrastructure work

### Process-group signal API

Stop and the stop-leg of Archive / Delete need to terminate
**every** process the session has spawned, including children
the agent forks itself. The terminal layer already runs each
session's command under a fresh pty, which gives us a
session-leader process with its own process-group id (pgid).

New host-side API surface:

```ts
// Plugin API (TypeScript binding via ts-rs).
//
// Sends `signal` to the *process group* led by the terminal's
// pty session leader. Defaults to a graceful escalation:
// SIGTERM, wait `gracePeriodMs`, then SIGKILL anything still
// alive.
editor.signalTerminal(
  terminalId: number,
  options?: {
    signal?: "SIGTERM" | "SIGKILL" | "SIGINT",
    gracePeriodMs?: number,
  },
): Promise<{ stopped: boolean }>;
```

Rust-side implementation lives in `services/terminal/manager.rs`
next to the existing `closeTerminal` path. It walks the pgid
via `kill(-pgid, signal)` on Unix; on Windows it walks the
job object that `portable_pty` already attaches the child to.

### Archive: worktree move + local manifest

`git worktree move` keeps git's internal bookkeeping
consistent with the on-disk move — the worktree still appears
in `git worktree list`, just under the new path, which is
fine because it's still a valid worktree the user could
inspect or fall back to manually.

Layout:

```
<XDG data dir>/conductor/<repo-slug>/
├── session-1/                ← active
├── session-2/                ← active
└── .archived/
    ├── session-3/            ← archived
    └── session-4/            ← archived
```

`<repo-slug>` is the slugified repository toplevel path
(`/home/noam/repos/fresh` → `home_noam_repos_fresh`), matching
what the new-session form already produces.

A local manifest at
`<XDG data dir>/conductor/<repo-slug>/archived.json` records
the archived sessions so the conductor plugin can show them
in the "Show archived" view without scanning the filesystem:

```json
{
  "version": 1,
  "sessions": [
    {
      "label": "session-3",
      "root": "<XDG>/conductor/<repo-slug>/.archived/session-3",
      "branch": "session-3",
      "archived_at": "2026-05-13T11:00:00Z",
      "last_state": "ready"
    }
  ]
}
```

"Show archived" is a toggle in the filter row (default off).
When on, archived rows are interleaved with active rows in
the list, rendered with a dim foreground and `ARCH` state
badge.

### Cross-machine recovery via a git branch

The local manifest is the source of truth on one machine. To
recover sessions on another machine the user pushes a special
git branch:

```
refs/heads/<user>/fresh-sessions
```

`<user>` is derived in this order:

1. `$FRESH_SESSIONS_USER` environment variable, if set.
2. The local-part of `git config user.email`
   (`noam@example.com` → `noam`).
3. The username from `gh auth status` when the `gh` CLI is
   configured.
4. `$USER` as a last resort.

The branch is an *orphan-ish* branch carrying only a single
file at its root, `sessions.json`:

```json
{
  "version": 1,
  "machine_id": "chunky.lan",
  "updated_at": "2026-05-13T11:00:00Z",
  "active": [
    {
      "label": "session-2",
      "branch": "session-2",
      "base_ref": "origin/master",
      "created_at": "2026-05-13T09:00:00Z"
    }
  ],
  "archived": [
    {
      "label": "session-3",
      "branch": "session-3",
      "base_ref": "origin/master",
      "archived_at": "2026-05-13T10:00:00Z"
    }
  ]
}
```

Sync behaviour:

- **Push** *(asynchronous, never blocks)*: any local lifecycle
  action (new session, archive, unarchive, delete) commits to
  the sessions branch and fires-and-forgets a
  `git push origin <user>/fresh-sessions`. The user-visible
  action returns immediately — the push runs in the
  background. Failures are non-fatal: the local manifest has
  already been updated, and the next successful push
  reconciles. A small *unobtrusive* indicator surfaces failure
  state — a `⤒` glyph (or similar) appended to the dialog's
  footer when there is unsynced state, plus a one-line
  hover/status-bar message naming the last error. The
  indicator clears as soon as a subsequent push succeeds.
  Users who care can run an explicit "Conductor: Sync Now"
  command from the palette to retry on demand.
- **Pull on open**: when `Conductor: Open` first loads in a
  fresh editor process, it tries
  `git fetch origin <user>/fresh-sessions` and merges any
  entries it doesn't already know about. Sessions whose
  `branch` is missing locally are shown as "remote" rows that
  resolve to "Dive" by first running `git fetch` for that
  branch and creating the worktree locally.
- **Merge strategy**: per-session `created_at` /
  `archived_at` timestamps decide which side wins on
  conflict. Two machines archiving the same session is
  idempotent.
- **Privacy**: the branch lives under
  `refs/heads/<user>/fresh-sessions` so it doesn't pollute
  the default `git branch` output (Git already hides
  namespaced refs in many UIs). Users who want full opt-out
  can set `fresh.conductor.sync = false` in their config.

This feature builds on top of the local manifest and ships
in a later phase (see [Implementation phases](#implementation-phases)).

### Keybinding integration

Shortcuts go through the existing keybinding pipeline rather
than being hardcoded in the plugin. The plugin registers
chord defaults under a `conductor-open` plugin mode:

```rust
keybindings.load_plugin_chord_default(
    KeyContext::Mode("conductor-open".into()),
    vec![(KeyCode::Char('s'), KeyModifiers::ALT)],
    Action::PluginCommand("conductor_stop".into()),
);
// …same for 'a' (archive), 'd' (delete), 'n' (new)
```

What this buys us:

- **User override**: any of these chords can be rebound in
  `~/.config/fresh/keybindings.json` — the resolution path
  in `KeyBindings::resolve_chord` checks user settings before
  the plugin defaults, identical to how built-in actions
  behave.
- **Cross-platform display**: footer hints and the
  Keybinding Editor render the chord through
  `format_keybinding`, which produces `Alt+D` on Linux /
  Windows and `⌥D` on macOS without the plugin caring.
- **Mode scoping**: the chord only fires while the
  `conductor-open` mode is active (i.e. the dialog is
  open), so `Alt+D` doesn't shadow anything global.

This requires one small host change: the floating-widget-panel
keystroke dispatcher (`dispatch_floating_widget_key` in
`app/input.rs`) currently swallows Ctrl/Alt chords. The new
behaviour is to *first* attempt a mode-chord resolution
against the active editor mode, then fall back to the
existing swallow-don't-leak rule.

## Implementation phases

### Phase 1 — Widget-based picker shell

- Build the layout: header, filter input, two-pane (list +
  preview), focus model, default keys (Up/Down/Tab/Enter/Esc).
- Plugin-side fuzzy filter over `conductorSessions` (small
  ranker, substring + prefix bonus). No external action
  surface yet — Dive is the only action.
- Replaces today's `startPrompt`-based picker.

### Phase 2 — Process-group signal API

- `editor.signalTerminal(terminalId, options)` host API in
  `services/terminal/manager.rs`.
- Unix: `kill(-pgid, signal)`. Windows: walk the
  `portable_pty` job object.
- TS-rs binding + plugin wrapper.

### Phase 3 — Stop action

- Wire `Stop` button + `Alt+S` shortcut to the new signal
  API.
- Preview pane renders pgid + pid list once the API is
  available.

### Phase 4 — Archive / Unarchive

- Local manifest at
  `<XDG>/conductor/<repo-slug>/archived.json`.
- `git worktree move` to / from the `.archived/` graveyard.
- "Show archived" toggle in the filter section; archived rows
  rendered with dim fg + `ARCH` badge.

### Phase 5 — Delete

- Confirmation panel in the preview pane.
- `git worktree remove` + rmdir for active sessions; manifest
  cleanup for archived sessions.

### Phase 6 — Cross-machine recovery

- Derive `<user>` from env / git config / gh / `$USER`.
- Commit + best-effort push of `<user>/fresh-sessions` branch
  on every lifecycle action.
- Fetch + merge on dialog open. Render remote-only sessions
  as a distinct row category, resolvable via Dive (which
  performs the worktree fetch / create lazily).
- `fresh.conductor.sync = false` config opt-out.

## Open questions

- **Stop with no live processes**: silent no-op or status-bar
  feedback? Leaning toward status-bar — surprise-no-op feels
  broken.
- **Diving into an archived session**: implicit unarchive
  (today's plan), or refuse and require the user to
  explicitly unarchive first? Implicit is more ergonomic but
  hides the worktree move under what reads as a navigation
  action.
- **Push frequency**: every lifecycle action vs. batched on
  dialog close vs. an explicit "Sync" button. Every-action
  is simplest; the push is async and non-fatal so latency is
  hidden, but it does add network traffic.
- **`<user>` collision**: two contributors with the same
  `git config user.email` local-part would collide on the
  branch namespace. Possibly require the gh username or a
  config when ambiguous.
