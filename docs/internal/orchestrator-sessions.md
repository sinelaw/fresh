# Orchestrator & Session Persistence

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

Purpose: explain how Fresh manages many concurrent editor/agent sessions (the
Orchestrator "dock"), how each session's state is persisted and restored across
restarts, and the Live/Dormant lifecycle — separating what is *shipped* from the
large body of forward-looking design docs.

This doc is the implementation-anchored counterpart to the design files listed
under "Superseded / aspirational docs" at the end. Where those describe phased
roadmaps, this records what the code actually does.

---

## 1. Concepts and naming

The Orchestrator lets one Fresh process hold several independent editor sessions
— typically one per git worktree, each often running a coding agent (Claude,
aider) in a terminal — and switch between them instantly via a left-column
**dock** or a modal **Open dialog**.

There are three names for the same thing at three layers, and the disambiguation
matters:

- **`Window` / `WindowId`** — the editor-internal type. Modelled on VS Code
  "windows" specifically to avoid colliding with Fresh's two pre-existing
  "session" concepts (workspace-recovery sessions and config-layer sessions).
- **"agent session"** — what the Orchestrator UX calls a Window. Parallel-agents
  is the user-facing domain language.
- **`PersistedWindow`** — the on-disk/in-memory shape produced by boot discovery.

Throughout this doc "session" = `Window`. Do not confuse it with the
client/server *session-persistence* feature (detach/reattach, design-decisions.md
#8) or `fresh session attach` CLI subcommands (#3) — those are unrelated
subsystems that happen to share the word.

The Orchestrator itself is a **TypeScript plugin**. It owns all dock/dialog UI
and the new-session/worktree/agent-resume logic; the Rust core provides
per-`Window` state ownership, persistence, and a small set of host APIs
(`createWindowWithTerminal`, `setWindowState`, authority attach). There is **no
`SessionLifecycle` enum** in Rust — lifecycle is expressed implicitly through the
data structures below.

> Note: the split-rendering "orchestration" plumbing in the rendering layer is
> **not** session UI. It carries shared selection/decoration context. The actual
> dock UI is in the plugin.

---

## 2. The session-as-Window model (the pivotal decision)

The defining architectural choice (orchestrator-sessions-design.md, "Step 0"):
**each `Window` owns its state outright** — buffers, splits, terminals, LSP,
file explorer, position history, and its `Authority` — rather than keeping
editor-global pools with per-session membership pointers.

Consequences that fall out by construction:

- Switching session is a **single field write** (`active_window = id`), not a
  "warm-swap" that stashes and restores N fields. The class of "forgot to swap
  field X back" bugs is eliminated.
- "Save all buffers" iterates only the active window's buffers; closing a session
  drops its struct (and SIGTERMs its agent terminals) — no orphan PTYs.
- The Orchestrator plugin's state is untouched by a session swap, so "the
  Orchestrator lives *above* sessions" is true structurally.

Alternatives considered and rejected (sessions-design.md trade-off section):

- **(A) Plugin-driven cwd mutation** (one editor, plugin asks core to rebuild
  cwd): smallest core change, but visible seams between subsystem rebuilds.
- **(C) Multi-process** (one Fresh server per worktree): best crash isolation
  but a substantial fixed memory overhead per server and the biggest lift; crash
  isolation was not a requirement.
- **(B) First-class `Window` in core** (chosen): larger core migration, atomic
  swap, inactive sessions stay warm in memory.

The per-subsystem move off `Editor` onto `Window` (buffers, LSP, terminal
subsystem, event logs, file explorer, position history, …) is **shipped**. The
earlier interim "manual switch between two sessions" steps in the design doc are
superseded by this model.

---

## 3. Session persistence model

### 3.1 The registry *is* the directory set

There is **no central session-list file**. A session *is* a directory (one
session per canonical dir), and the registry is the per-directory workspace
cache:

- A per-directory workspace file under the data dir's `workspaces/` — one file
  per directory ever opened, keyed on the encoded canonical root. Each carries
  that window's identity (`label`, `session_plugin_state`, `authority_spec`)
  *plus* its buffer/split layout.
- A per-plugin global-state file under the data dir's `orchestrator/state/` —
  editor-wide plugin global state, one file per plugin (not per-project).

Path encoding is reversible percent-encoding. State lives under the platform
data dir (e.g. `$XDG_DATA_HOME/fresh/`), **never the working tree** —
regression-tested.

This is the v2 design. It deliberately replaced an earlier global
`windows.json` (which itself replaced per-cwd `windows.json` files). The
motivation: a per-cwd store made "yesterday's directories" bleed into today; a
workspace-keyed registry gives one session per dir for free — each file is keyed
on the canonical root, so discovery yields at most one window per dir with **no
post-hoc dedup**.

### 3.2 The `Workspace` file shape

The `Workspace` record is the per-dir file. Beyond the editor-state fields
(split layout, per-split view state, cursors/scroll, bookmarks, file explorer,
histories, folds, terminals), three fields make it the session record:

- `label` — optional display name; defaults to the root basename. Lives here
  precisely because `windows.json` was dropped.
- `session_plugin_state` — the window's own per-session plugin state, carrying
  the Orchestrator's `project_path` / `shared_worktree` keys. Distinct from
  `plugin_global_state` (editor-wide, separate store).
- `authority_spec` (`SessionAuthoritySpec`) — how to rebuild/reconnect this
  session's backend on restore; the `Local` variant is skip-serialized so
  ordinary sessions carry no redundant marker. See §6.

Writes are **atomic** (temp file + fsync + rename) so a crash never leaves a
torn file. The schema is versioned, with a "version too new" guard on load.

### 3.3 Discovery and GC at boot

Boot discovery runs from the editor factory *before* an `Editor` exists. It:

1. Migrates legacy layouts (§3.4), then
2. Scans the workspace cache, parses each file's working directory, and returns
   one `PersistedWindow` per survivor.
3. IDs are assigned by **sorted canonical root** so they stay stable run-to-run
   for a stable dir set.

GC is careful: a local session's file is deleted only on a *definitive* "this
dir is gone" answer (a not-found result, or a confirmed "not a directory").
Any ambiguous error (permission, IO, unmounted FS) keeps the file rather than
irreversibly losing the session. **Remote sessions are never GC'd against the
local filesystem** — their `root` is a path on the remote host, so checking it
locally would silently delete every remote session's file on next boot. This is
regression-tested.

### 3.4 Migration chain

Idempotent, best-effort, and reversible (every retired file keeps a `.bak`):

- Per-cwd `windows.json` files are folded into one v2 envelope, decoding
  `project_path` from the slug and renumbering id collisions.
- The global `windows.json`'s `label` / `session_plugin_state` are backfilled
  into the matching per-dir workspace files, then it is retired. After this the
  workspace cache is the sole registry.
- Per-cwd plugin-state files are merged into the global state dir.

---

## 4. Session lifecycle (Live / Dormant / inert / warm)

The lifecycle is a set of *implicit* states realized by three mechanisms. There
is no single enum.

### 4.1 Lazy materialization (inert → warm)

At boot only the **foreground** window is restored eagerly. Which one that is, is
decided by the launch cwd, not by "last used globally":

1. If the globally-last-used session belongs to this cwd, reopen it.
2. Else the most-recently-created session whose `root == cwd` (highest id).
3. Else none → boot a clean base window at cwd.

Matching is on **`root`, not `project_path`** — a worktree session carries
`project_path == <parent repo>` but `root == <worktree>`, and matching on
`project_path` resurrected the wrong window. This is regression-tested.

Every *other* discovered session comes back as an **inert shell**: a `Window`
with only an empty seed layout, no splits/LSP, recorded in a pending-materialize
set. It is restored from disk lazily on first dive/preview, which:

- removes the id from the pending set up front (idempotent; a corrupt
  workspace doesn't retry every frame),
- runs the same restore path as a cold launch,
- **snapshots and restores `plugin_global_state`** around the per-window restore
  so a background window's stale copy can't clobber the live one.

The **"warm" in-memory layout is intentionally not persisted** across restarts —
re-warming on first dive (a fast lazy load versus a far slower log replay) is
fast enough.

The quit-time counterpart deliberately **skips windows still pending
materialization**: they hold only their empty seed, while the on-disk file is
authoritative — saving the seed would clobber the real workspace.

### 4.2 Live / Dormant (remote backends)

For remote sessions, persistence introduces an explicit Live/Dormant split
(PER_SESSION_BACKENDS_DESIGN.md, **shipped** for local/container/SSH/Kubernetes):

- **Live** — connection established (local always; remote after a successful
  connect). The `Authority` routes every filesystem/spawn primitive.
- **Dormant** — the `SessionAuthoritySpec` is known but not connected. The window
  runs on a **local placeholder authority** (instantly usable, never holds a dead
  handle) and is presented as the real backend, disconnected. At boot,
  unmaterialized remote sessions are carried in a separate dormant-remote
  collection and keep their `authority_spec` so a later save doesn't downgrade
  them to local.

Reconnect happens on activate (switch or explicit), not at boot — SSH/Kubernetes
via core connect functions, containers via a `session_reattach_requested` plugin
hook. Agent terminals re-run *after* reconnect, inside the real backend, never on
the placeholder.

### 4.3 Terminal Live / Scrollback

Orthogonal to session lifecycle: each restored terminal comes back in **Live**
mode so focusing its tab re-enters a live PTY rather than read-only scrollback;
Ctrl+Space flips to Scrollback. The PTY itself is ephemeral and re-spawned; only
the backing file (scrollback + screen snapshot) is persisted (design-decisions.md
#18).

### 4.4 Plugin-level agent state

The dock additionally shows a coarse agent state inferred from terminal output
(e.g. working/idle, plus richer running/awaiting/ready/errored glyphs derived in
the plugin). This is display-only and not part of the persistence model.

---

## 5. The dock and Open dialog UX

All UI is plugin-side (orchestrator.ts). Shipped surfaces:

- **Dock** — persistent left-column session list; rows are bordered two-line PR
  pill cards (see below). Toggled via `orchestrator_dock_toggle`. Live-switch
  with ↑/↓ rebuilds the active window in place.
- **Open dialog** (`OPEN_MODE = "orchestrator-open"`) — modal two-pane picker
  (session list + preview), with filter, project **scope toggle** (Alt+P),
  "show all worktrees" toggle (Alt+T), "hide trivial" toggle, and multi-select
  for bulk lifecycle actions.
- **New Session form** (`NEW_SESSION_MODE = "orchestrator-new-form"`) — see §7.
- **Preview pane** — branch, worktree path, working-tree diffstat, PR info, and
  per-session action buttons (Visit / Stop / Archive / Delete).

### 5.1 Project scoping (the "yesterday's directories" fix)

Globally-listed sessions confused users by combining unrelated projects. The
shipped rule (orchestrator-open-dialog-and-lifecycle.md): **scope to the current
project by default, go global on demand** (Alt+P). Current-project rows sort
first; a project column appears only for cross-project rows. Persisted sessions
are **never auto-activated** — any "Resume last?" must be explicit and
dismissible. `project_path` exists purely as this grouping metadata; it is *not*
used to choose the boot window (§4.1).

### 5.2 PR pills

Rich session display was the forcing function for a host change
(orchestrator-pr-pill-wireframes.md). Three options were weighed:

- **A** enriched single line (zero host change, but cramped),
- **C** paired list entries (fragile selection/height math),
- **B** true two-line pill — chosen and **shipped**. The `list` widget gained
  multi-row item specs so an item can span several rows; selection background
  spans all rows; the hit area covers them; visible-row counting is in pills, not
  lines.

PR data is fetched by running `gh pr view <branch> --json` per visible session,
throttled with a short TTL and cached on the session; missing `gh` or no PR
renders a dim fallback.

### 5.3 Discovered worktrees

Rows can be on-disk worktrees found via `git worktree list` that have **no live
window yet**. They carry a synthetic **negative id** and no terminal id; diving
*attaches* a new session to that `root`, and the row is dropped from the
in-memory session map the moment a real window opens there.

### 5.4 Lifecycle actions

Shipped via a process-group signal API (`editor.signalTerminal`, SIGTERM→SIGKILL
on the pgid):

- **Stop** — kill the agent's process group; recoverable (relaunch). No confirm.
- **Archive** — `git worktree move` to an `.archived/` graveyard; recoverable via
  Unarchive. No confirm.
- **Delete** — `git worktree remove` + rmdir; **not** recoverable, confirm
  required.

Cross-machine recovery (archived sessions synced via a
`refs/heads/<user>/fresh-sessions` git branch) is **designed but deferred**
(v1.1+).

### 5.5 Known UX gaps (shipped Phase 1)

The core loop (work → list → switch → work) is mechanically solid and
per-session state persists. Focus/input routing has open gaps:

- **F1** — the dock can hold keyboard focus while a terminal consumes keys. The
  cause is that dock keys are dispatched at the floating-panel layer, shadowed by
  terminal mode. A guard prevents the failure; the proper fix (dock as a
  first-class `KeyContext` chrome, like the file explorer) is deferred.
- **F2** — diving into a switched session lands focus in the file tree, not the
  buffer.
- **F3** — hiding the dock can leave a stale gutter until the next resize,
  because the stored last-frame size is the full size rather than the chrome
  area.

---

## 6. Per-session backends (cross-ref: PER_SESSION_BACKENDS_DESIGN.md, AUTHORITY_DESIGN.md)

Each `Window` owns its own `Authority` outright — it is **not `Clone`** and is
owned by exactly one window, so backend isolation is enforced by the type system.
The persisted recipe is `SessionAuthoritySpec`:

- `Local` — host-local; the default and back-compat (skip-serialized).
- `Plugin` — devcontainer/docker; reconnectable only by the installing plugin.
- `RemoteAgent` — born-attached SSH / Kubernetes; reconnectable by core. The
  transport spec covers SSH (user, host, port, identity file, remote path, …) and
  kubectl-exec (context, namespace, pod, container, workspace).

Terminal/agent argv is composed through the active authority's command wrapper
(direct / prefix for docker exec / SSH with a `cd` hop / kube), so an agent
always runs *inside* the session's backend, not on the host. Per-session
**trust** and **env** are likewise move-only session-scope handles consumed into
the authority — no shared copy, sharing rejected at compile time. All four phases
(SessionProfile/Dormant restore, reconnect-on-activate, per-session trust,
per-session env) are **shipped**; warm background remote sessions surviving
restart with keepalives are **deferred** (v1.1+).

---

## 7. New-session / base-path flow

The New Session form fields (Phases 1–5 **shipped**):

- **Project Path** — an *arbitrary* path, no longer implicit from cwd. A debounced
  async git probe on each keystroke detects whether the path is a git repo / a
  linked worktree, and enables/disables the worktree controls. The placeholder
  shows the canonical repo root (or cwd for non-git).
- **Create new git worktree** checkbox — enabled only for git paths. Checked
  (default) → `git worktree add` under the repo; the new session's `root` is the
  fresh worktree. Unchecked → `root` *is* the project path itself
  (`shared_worktree = true`), which also covers non-git directories and
  multi-session-sharing-one-tree.
- **Branch** — base ref for the worktree fork (git + worktree-on only).
- **Agent** dropdown — terminal / claude / aider / custom (Phase 3 of
  agent-resume).
- **Input history** — per-field MRU (Up/Down), global per user, capped, stored
  under the data dir's `orchestrator/`.

Creation is atomic via the host API:

```ts
editor.createWindowWithTerminal({ root, label, cwd: root,
  command: launchArgv, resume: resumeArgv });
editor.setWindowState("project_path", effectiveProjectPath);
editor.setWindowState("shared_worktree", sharedWorktree);
```

`createWindowWithTerminal` is dispatched into core, which enforces
**one-session-per-canonical-directory** (reuses an existing window if `root` is
already open) and persists the `command` / `resume` argv onto the terminal. The
two `setWindowState` keys land in the window's `session_plugin_state` and are
read back at boot.

Type-aware New Session forms for SSH/Kubernetes backends
(NEW_SESSION_DIALOG_WIREFRAMES.md, segmented-tab "Option A") are **designed, not
shipped** — today's form is the local worktree/folder flow.

---

## 8. Agent resume (cross-ref: agent-resume-design.md — Phases 0–3 shipped)

Goal: a restored session **rejoins** its agent conversation instead of coming
back as a bare shell or re-running the launch command. No agent-specific logic
lives in Rust; the core only substitutes argv and runs it through the authority.

The plugin holds a user-overridable agent registry with two strategies:

- **provision** (preferred; Claude): mint a session id at launch
  (`claude … --session-id <uuid>`) and resume with it
  (`claude --resume <uuid>`); `--continue` as the cwd-latest fallback. Trusted by
  construction — no output capture, no parsing.
- **continue** (broad default; aider): resume the most recent session in the cwd
  (`aider --restore-chat-history`), ambiguity broken by per-session config
  isolation.

Launch resolution returns a launch argv plus an optional resume argv: the launch
carries any minted `--session-id`, the resume is the resolved rejoin argv. The
resume spec is persisted **separately from the launch command**, as an
agent-resume argv on the terminal's serialized workspace. On restore the order is:
agent-resume → command → plain shell → backing-file, gated by a
`terminal.resume_agents` master switch (default on). Resume is **deferred to
first dive**, so reopening the editor doesn't spend tokens on sessions never
looked at. Broader registries, remote-backend resume, and per-resume confirmation
policy are **deferred**.

---

## 9. The daemon (not session multiplexing)

The server daemon module is unrelated to the Orchestrator. It is a thin
**background-server detach** helper: PID-file read/write, daemonize (double-fork
+ `setsid` on Unix; unsupported on Windows), detached server spawn, and a
process-liveness check. It backs the client/server detach/reattach feature
(design-decisions.md #8), where the `session_id` is a *socket/PID* identifier —
**not** an Orchestrator window. The daemon does not switch or multiplex
Orchestrator sessions; the editor process manages windows itself.

---

## 10. Quick reference — implemented vs planned

Implemented (shipped):

- Directory-keyed session registry + atomic per-dir workspace files; full
  legacy-migration chain with `.bak` safety.
- Lazy materialization (inert → warm on first dive), cwd-scoped boot selection,
  quit-time save that won't clobber unmaterialized seeds.
- Dock + Open dialog + New Session form + preview, PR pills, project scoping,
  multi-select Stop/Archive/Delete.
- Per-session backends (Local/Plugin/RemoteAgent), Live/Dormant restore,
  reconnect-on-activate, per-session trust + env, type-enforced isolation.
- Agent resume (provision/continue), resume-spec persistence, deferred-to-dive
  rejoin.
- One-session-per-canonical-directory enforcement.

Planned / aspirational (in design docs, not in code):

- Cross-machine session recovery via a `refs/heads/<user>/fresh-sessions` branch.
- Warm background remote sessions surviving restart (keepalives).
- Type-aware SSH/Kubernetes New Session form (segmented tabs).
- Dock as a first-class `KeyContext` chrome (resolves focus gaps F1/F2).
- Broader agent registry, per-resume confirm policy, path/branch completion.
- Collapsible project-group headers (currently a flat list with per-row tag).

---

## Superseded / aspirational source docs

These were mined to write this doc. Most are **phased design records**, not
status reports — read them for rationale, treat their later phases as aspirational
unless confirmed against code here.

- `orchestrator-sessions-design.md` — the session-as-Window model and
  Step-0 migration. *Mostly shipped; its early "manual switch" steps are
  superseded by the materialization model in §4.*
- `orchestrator-new-session-base-path.md` — arbitrary project paths, worktree
  toggle, input history, global persistence. *Phases 1–5 shipped; 6–7 deferred.*
- `orchestrator-open-dialog-and-lifecycle.md` — picker UX, Stop/Archive/Delete,
  project scoping. *Phases 1–5 shipped; cross-machine recovery deferred.*
- `orchestrator-pr-pill-wireframes.md` — two-line PR pill (Option B). *Shipped.*
- `orchestrator-dock-gaps.md` — open dock UX gaps + the "dock as chrome" (P1) /
  layer-compositor (P2) principles. *Phase 1 shipped; P1/P2 aspirational.*
- `orchestrator-bringup-dataflow-review.md` — boot dataflow fixes.
  *§2–5 landed (pick-on-root, non-colliding ids, explorer at window root); unify-
  restore-path §6 still TODO.*
- `ORCHESTRATOR_DOCK_NNG_FINDINGS.md` — usability findings F1–F8. *Test results;
  several fixes shipped, F1/F2/F3 open.*
- `ORCHESTRATOR_DOCK_NNG_USABILITY_GUIDE.md`, `dock-ux-test-plan.md` — test
  protocol / checklist. *Methodology, not design.*
- `agent-resume-design.md` — provision/continue strategies. *Phases 0–3 shipped.*
- `PER_SESSION_BACKENDS_DESIGN.md` — SessionProfile, Live/Dormant, per-session
  trust/env. *Phases 1–4 shipped; warm-background deferred.* (Also see
  `AUTHORITY_DESIGN.md`, `K8S_AUTHORITY_DESIGN.md`.)
- `NEW_SESSION_DIALOG_WIREFRAMES.md` — type-aware SSH/Kube form. *Designed, not
  shipped.*
- `design-decisions.md` #3 (CLI subcommands) and #8 (dual-socket client/server
  persistence) — *shipped, but a different "session" subsystem; see §1/§9.*
