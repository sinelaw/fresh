# Conductor & Sessions Design

> **Status**: Design Document
> **Date**: May 2026
> **Branch**: `claude/plan-conductor-architecture-6YsJt`
> **Driving feature**: "Conductor" multi-agent orchestration UI (PRD external).
> **Core change required**: first-class `Session` abstraction in the editor.

## Motivation

The "Conductor" feature lets a developer run multiple AI coding agents
(`aider`, `claude -p`, `opencode`, …) in parallel, each in its own git
worktree, and switch between them from a single Fresh process. The PRD
calls for two modes:

1. A full-screen **Control Room** that lists every active agent, its
   parsed state (running / awaiting input / ready / errored), live
   terminal preview, diff stats, and a file-collision radar across
   worktrees.
2. A standard **Session IDE** (file explorer, LSP, quick-open, splits,
   buffers) scoped to one worktree at a time, that the user "dives" into
   from the Control Room.

The user-facing requirement that drives this design is:

> Switching sessions from Conductor should feel like swapping the
> entire Fresh state. File explorer, LSP, quick-open scope, ignore
> rules, buffer set, splits — all of it retargets atomically. Conductor
> itself stays anchored above the swap, with its session list,
> collision matrix, and agent PTY handles untouched.

Today, Fresh's editor state is built around a single implicit project
root. The cwd is read in dozens of places (`getCwd()` on the plugin
API, file explorer init, LSP root URI, ignore-matcher construction,
quick-open scoping, plugin path resolution). There is no abstraction
that bundles "everything rooted at one project" so that several can
coexist and one can be made active. A Conductor plugin alone cannot
deliver the required UX, because the things that need to retarget
(file explorer, quick-open, LSP set) are core-owned and scoped
implicitly to whatever `getCwd()` returns.

This document specifies the smallest core abstraction that makes the
required UX possible — a first-class `Session` — and the plugin-API
surface a Conductor plugin needs on top of it. It deliberately does
not specify the Conductor plugin itself; that is a follow-up doc once
this design is settled.

## Architecture priority: complete the session-as-window model first

> **This is the top priority.** All further Conductor feature work
> (Control Room polish, hotkeys, diff/merge actions, collision
> radar) is gated on completing the session-as-window architecture
> below. The interim warm-swap implementation that currently sits
> on the branch (`§ Implementation status snapshot`) is a
> transitional bridge, not the destination.

### The model

Each `Session` is the editor-state equivalent of a **VS Code
window**: an isolated bundle of everything the user sees and acts
on within that session. Closing a session evicts everything it
owned; opening the same file in two sessions creates two
independent buffers; "save all" / "close all" / quick-open / find-
in-files act on **this session only**.

Concretely:

| Lives on `Session` (per-window state) | Lives on `Editor` (cross-session) |
|---|---|
| `buffers: HashMap<BufferId, EditorState>` | `next_buffer_id` (still globally unique) |
| `event_logs` (undo history per buffer) | Plugin runtime (single QuickJS) |
| `terminal_manager`, `terminal_buffers`, `terminal_backing_files` | Theme, config, keybindings (user-level) |
| `splits`, `split_view_states` | `plugin_global_state` (cross-session by definition) |
| `file_explorer`, `lsp`, `panel_ids`, `file_mod_times` | `sessions: HashMap<SessionId, Session>`, `active_session` |
| `position_history`, `bookmarks` | Workspace recovery framework |
| `cached_layout` (split / tab / file-explorer rects) | Chrome layout (status bar, menu, prompt overlay rects) |

`Editor` becomes the multiplexer: it holds the session map, the
active-session pointer, the cross-session shared infrastructure,
and editor chrome. **Almost no command logic reads editor-global
state directly** — commands are dispatched on the active session.

### Why this is the right target

1. **Render becomes trivially session-pluggable.** A
   `render_session(frame, area, &Session, &Editor /* chrome */)`
   call works for any session — active, previewed, or
   off-screen. The "preview the entire editor UI" requirement
   from `§ Rich Control Room rendering` falls out for free, with
   no swap gymnastics and no risk of side-effects bleeding across
   sessions.
2. **Commands are correct by default.** "Save all," "close all,"
   quick-open, find-in-files, list-buffers all enumerate
   `active_session.buffers` because that's where buffers live.
   No risk of acting on another session's content. Cross-session
   operations (e.g. compare alpha vs base for a diff) become
   explicit, opt-in APIs.
3. **`closeSession` is principled.** Drop the `Session` struct;
   its buffers, terminals, undo logs, watchers all go with it.
   No refcounting, no "is anyone else using this buffer?" logic.
4. **Crash isolation per session is feasible later** (each
   session in its own panic boundary / thread), though not free
   today.
5. **Eliminates the entire warm-swap pattern.** `setActiveSession`
   becomes a single field write; there are no stashes to keep
   in sync; the bug class "I forgot to swap field X back" goes
   away.

### Why the alternative (global storage + membership) was rejected

Earlier in this doc's history, buffer storage was deliberately
kept Editor-global with `Session.buffers: HashSet<BufferId>` as
a membership pointer (`§ Why each session owns its buffers`).
The rationale was "two sessions might want to share a buffer"
and "Conductor's terminal buffers need to be addressable from
the Control Room."

In practice both arguments fold:

- **Sharing.** The parallel-agents use case wants edits in
  alpha and beta to *diverge*, not propagate — that's the whole
  point of running independent worktrees. Forcing shared storage
  is the wrong default.
- **Cross-session addressability.** Conductor lives in
  editor-global plugin state and naturally has session ids; if
  it needs a buffer from another session it asks via
  `editor.sessions.get(sid).buffer(id)`, which is a one-line
  helper. The "global lookup" benefit was illusory.

The half-migrated state on the branch — buffers and terminals
global, view state per-session — is **architecturally
inconsistent**: rendering correctly scopes to the active
session, but commands operate on the global buffer pool. Every
command that doesn't go through the session is a latent bug
("save-all from alpha saves base's files too" is one such bug
already observed).

### Status of the interim implementation

The work landed on this branch is **not wasted**:

- The `Session` struct, the `sessions` map, `active_session`
  pointer, `createSession`/`setActiveSession`/`closeSession`
  plugin APIs, the lifecycle hooks, and cross-restart
  persistence all stay.
- The warm-swap stashes (`splits_stash`, `lsp_stash`, etc.)
  become live fields on `Session` rather than `Option<…>`
  stashes — most of the storage shape is right; only the
  ownership semantics flip.
- The Conductor plugin (Step 6) keeps working because its
  plugin-API surface doesn't change.

What gets discarded:

- The `swap_active_session_state` implementation
  (`session_actions.rs::set_active_session`) — replaced by a
  pointer write.
- The "transient swap for preview" approach
  (`render_session_preview_into_rect`) — replaced by
  `render_session(&Session, area, &Editor)`.
- The half-finished "global buffer pool with session
  membership" model — replaced by per-session ownership.

The migration sequence is laid out in `§ Migration sequence`,
**Step 0** below.

## Non-goals

- Multi-process isolation. Crash isolation between worktrees is not a
  requirement (`§ Trade-off discussion`). One Fresh process, one
  plugin runtime, one editor instance.
- Remote / SSH / devcontainer worktrees. The authority model
  (`AUTHORITY_DESIGN.md`) is orthogonal; sessions and authorities
  compose, but this doc only specifies sessions on the local
  authority. Remote sessions are a follow-up.
- Replacing the existing `panelId` / `utility_dock` machinery. This
  design composes on top of it (`§ Control Room placement`).
- Hot-reload of the Conductor plugin itself. Standard plugin reload
  semantics apply.

## MVP scope

The minimum viable Conductor delivers the load-bearing UX claim:

> spawn agents in parallel worktrees, switch between them with the
> entire editor retargeting (file tree, LSP, quick-open, ignore
> rules, buffer set, splits), and have Conductor's session list
> survive every switch unchanged.

Everything else in this document is wanted but deferrable. Items
throughout the doc are tagged `[MVP]` or `[v1.1+]`. This section is
the index.

> **Gating constraint (May 2026):** `Step 0 — Session-as-window
> migration` (`§ Migration sequence`) is the new top priority and
> blocks every MVP item below that hasn't already shipped.
>
> **Progress:** Step 0a (cached_layout split) and Step 0b (warm-
> swap stashes → live Window fields) shipped on
> `claude/window-state-migration-RjEwX`. `set_active_window` is
> now a pointer write — the warm-swap pattern is dead for every
> field except `Editor.buffers` (and the field-pairs that follow
> from it: terminals, event_logs, position_history). Step 0c
> (`Editor.buffers` → `Window.buffers`) is the gating piece for
> 0d–0i; first attempt reverted on borrow-checker friction (see
> `§ Step 0c`). UX features built on top of the warm-swap
> interim still accumulate technical debt and must wait for the
> rest of Step 0 to complete.

### `[MVP]` — load-bearing for the core UX promise

**Core abstraction**
- `Session` struct with: `id`, `label`, `root`, `buffers`,
  `file_tree`, `ignore_matcher`, `lsp_clients`, `split_layout`,
  `view_states`, `panel_ids`
- `Editor.sessions` + `active_session` pointer
- Editor-global plugin state (where Conductor lives, by default)
- Atomic dive (`setActiveSession`)
- Lazy LSP startup on first activation

**Plugin APIs**
- `listSessions`, `activeSession`, `createSession`,
  `setActiveSession`, `closeSession`
- `session_created`, `session_closed`, `active_session_changed` events
- `createTerminal({ sessionId, cwd, ... })` (existing API gains
  `sessionId` field)
- `terminal_output`, `terminal_exit` events (the smallest core
  change; `§ Background`)

**Screens**
- Empty Conductor (`Screen 1`) — full
- Control Room (`Screen 2`) — reduced column set: `#`, `LABEL`,
  `ROOT PATH`, `AGENT`, `STATE`, `DIFF`, `AGE`. The COMMITS
  column, memory header, and collision-radar pane are deferred;
  the radar pane area renders an empty placeholder.
- Session IDE (`Screen 3`) — full (falls out of the architecture
  for free)
- New-session prompt (`Screen 4`) — full

**States** (in the `STATE` column)
- `ACTIVE`, `RUNNING`, `AWAITING (Y/n)`, `READY`, `ERRORED`,
  `KILLED`. `KILLED` rows drop immediately in MVP (no tombstone).

**Controls**
- Up / Down / `Ctrl+n` / `Ctrl+p` (navigate)
- `Enter` (dive), `n` (new), `d` (diff), `m` (merge), `k`
  (kill+drop), `Esc` (close)
- Mouse: click row to select, double-click to dive

**Diff invocation (`d`)**
- Spawns the existing review-diff feature (see
  `docs/internal/REVIEW_DIFF_*.md`) on the selected session's
  worktree against the base. No new diff renderer is needed for
  MVP. The native side-by-side renderer in `§ Plugin API surface`
  is a v1.1+ refinement, not a prerequisite.

**Migration steps from `§ Migration sequence`**
- Step 1, Step 2, Step 3, Step 5 (global namespace only), Step 6.

This MVP set delivers PRD user stories 1 (orchestrate parallel
agents) and 2 (focused coding in a single worktree) and the
review-and-merge flow from story 3. It does *not* deliver passive
status-bar awareness or pre-merge collision warnings.

### `[v1.1+]` — wanted, deferred

**Plugin APIs**
- `watchPath` / `unwatchPath` / `path_changed` event (enables
  collision radar)
- `setSessionState` / `getSessionState` (other plugins' concern)
- `openDiffView` for a *native* side-by-side diff renderer
  invoked programmatically with arbitrary `oldText`/`newText`.
  MVP uses the existing review-diff feature instead, invoked from
  Conductor's `d` action.
- `openFile({ sessionId })` (MVP only opens in active session)

**Control Room enrichments**
- `COMMITS` column
- Header memory readout (`2.1GB / 32GB`)
- `SYNCING` state and Conductor-driven git operations
- `KILLED` tombstones with two-press semantics for `k`
- Multi-select (Shift+arrow), parallel-attempts compare via `d`
  on a multi-selection
- "Promote one, kill the rest" cluster action on `m`
- Rename (`r`)
- Tab to cycle preview/collision pane focus

**Lifecycle**
- Cross-restart persistence of session list and layout snapshots
- Auto-open Control Room on `AWAITING` / `ERRORED`
- `editor.prewarmSession(id)`

**Other**
- Collision warning popup (`Screen 5`) — depends on `watchPath`
- Native vertical diff renderer (Migration `Step 7`)
- Bottom-of-window agent-state indicator (separate, depends on
  `registerStatusBarElement` in PR #1843)

### What's deliberately *not* MVP, even though small

- **`r` rename.** The branch name is the default label and works.
  Adding rename without a UI for "what was the original branch
  again?" is mildly confusing; defer until needed.
- **Memory readout in header.** Visible-by-default UX nicety.
  Hide-by-default until users ask.
- **Auto-open on AWAITING.** Interrupts the user; needs careful
  default behavior. Ship with manual `<Leader>o` only and tune later.

## Background: the primitives we already have

### Project root is implicit and editor-wide

Fresh has no `project` or `workspace` struct. The cwd of the Fresh
process is the project root, surfaced to plugins via
`editor.getCwd()` and read directly in many places:

- File explorer (`crates/fresh-editor/src/app/file_explorer.rs`)
  walks from cwd.
- Quick-open / file finder
  (`crates/fresh-editor/src/input/quick_open/providers.rs`) is scoped
  to cwd.
- Ignore rules (`crates/fresh-editor/src/view/file_tree/ignore.rs`)
  load `.gitignore` from cwd upward.
- LSP root URIs derive from cwd or per-buffer file paths
  (`crates/fresh-editor/src/services/lsp/manager.rs`).
- Plugin runtime exposes cwd as a JS string read on demand.

There is no central registry; each subsystem reads cwd when it needs
it. Changing cwd at runtime today would race against any of these
readers and would not retroactively rebuild file-tree or LSP state.

### Buffers and splits live on the Editor struct

`crates/fresh-editor/src/app/mod.rs` (the `Editor` struct) owns:

- `buffers: HashMap<BufferId, Buffer>` — every open buffer.
- `split_manager: SplitManager` — the pane tree.
- `split_view_states: HashMap<SplitId, SplitViewState>` — per-split
  scroll/cursor state.
- `terminal_manager` — every PTY.
- `plugin_manager` — single plugin runtime, single QuickJS instance.
- `file_mod_times: HashMap<PathBuf, _>` — polling-based change
  detection.
- `panel_ids: HashMap<String, BufferId>` — utility-dock occupancy.

None of these are scoped by project root. There is one of each, for
the whole Fresh process.

### Plugins are editor-scoped, not session-scoped

The plugin runtime lives on the Editor (singleton). Plugin state in
JS is whatever the plugin module's top-level scope holds, which
persists for the lifetime of the editor (or until plugin reload). No
plugin state is currently scoped narrower than that. This is
fortunate: it is exactly the property that lets Conductor "live above"
sessions for free, once sessions exist.

### `utility_dock` and virtual buffers

`createVirtualBufferInSplit({ role: "utility_dock", … })` (handled at
`crates/fresh-editor/src/app/plugin_dispatch.rs:2167` onward)
implements a one-leaf-per-role dock for diagnostics, file explorer,
search/replace, finder. Conductor's Control Room will use this same
dock with its own role tag.

`defineMode(name, bindings, …)`
(`crates/fresh-plugin-runtime/src/backend/quickjs_backend.rs:3196`)
binds keys to commands within a named mode that virtual buffers can
opt into via the `mode` field. This is how Conductor binds its own
hotkeys.

### Terminal manager already emits the events we need

`AsyncMessage::TerminalOutput { terminal_id }` and
`AsyncMessage::TerminalExited { terminal_id }` are emitted from
`crates/fresh-editor/src/services/terminal/manager.rs:407,433` and
consumed internally at
`crates/fresh-editor/src/app/async_dispatch.rs:427,453`. They are not
exposed to plugins today. Surfacing them is one of the changes this
design requires (§ Plugin API surface).

### Daemon / IPC

Fresh's client/server (`crates/fresh-editor/src/server/`) is already
robust and used for persistence-across-disconnect. This design does
**not** introduce a second server or a new RPC channel. The daemon
hosts one Editor with N sessions; the client renders whichever
session is active plus the editor-level chrome.

## The `Session` abstraction

A `Session` owns the per-project-root state that today is implicit on
the Editor.

```rust
pub struct Session {
    pub id: SessionId,
    pub label: String,                   // user-visible
    pub root: PathBuf,                   // canonical absolute path

    // What used to be "the editor's"
    pub buffers: HashSet<BufferId>,      // ids; storage stays Editor-global
    pub split_layout: SplitTree,
    pub view_states: HashMap<SplitId, SplitViewState>,
    pub active_split: SplitId,
    pub panel_ids: HashMap<String, BufferId>,  // utility-dock occupancy
    pub file_tree: FileTreeState,
    pub ignore_matcher: IgnoreMatcher,
    pub lsp_clients: LspClientSet,       // keyed by language, rooted at `root`
    pub watch_handles: Vec<WatchHandle>,
    pub plugin_state: HashMap<PluginId, JsValue>,  // session-scoped, opt-in

    // Persistence
    pub layout_snapshot: Option<LayoutSnapshot>,   // for save/restore
    pub created_at: SystemTime,
}

pub struct Editor {
    sessions: HashMap<SessionId, Session>,
    active_session: SessionId,

    // Editor-global (one per process):
    buffers: HashMap<BufferId, Buffer>,            // owned here; sessions hold ids
    terminal_manager: TerminalManager,             // PTYs survive session swaps
    plugin_manager: PluginManager,                 // one runtime
    plugin_global_state: HashMap<PluginId, JsValue>,
    theme: Theme,
    config: Arc<Config>,
    keybindings: KeyBindings,
    // ...
}
```

### Editor-global vs session-scoped state — the key picture

```
                       +-----------------------------------+
                       |          Editor (global)          |
                       | -------------------------------   |
                       |   plugin runtime (one QuickJS)    |
                       |   plugin_global_state ............| <- Conductor's
                       |     conductor: {                  |    session list,
                       |       sessions: Map,              |    collision matrix,
                       |       collisions: Map,            |    agent PTY refs
                       |       watchers: Map,              |
                       |     }                             |
                       |   terminal_manager (all PTYs)     |
                       |   buffers (storage)               |
                       |   theme, config, keybindings      |
                       |   active_session ----------+      |
                       +----------------------------|------+
                                                    |
                  +---------------------------------+----+
                  |                |                     |
                  v                v                     v
         +------------------+ +------------------+ +------------------+
         |   Session 1      | |   Session 2 *    | |   Session 3      |
         | ---------------- | | ---------------- | | ---------------- |
         |  root: /repo     | |  root: /wt/auth  | |  root: /wt/redis |
         |  file tree       | |  file tree       | |  file tree       |
         |  ignore matcher  | |  ignore matcher  | |  ignore matcher  |
         |  LSP clients     | |  LSP clients     | |  LSP clients     |
         |  watch handles   | |  watch handles   | |  watch handles   |
         |  split layout    | |  split layout    | |  split layout    |
         |  buffers: {1}    | |  buffers: {2,3,4}| |  buffers: {5}    |
         |  panel_ids: {..} | |  panel_ids: {..} | |  panel_ids: {..} |
         |  plugin_state:{} | |  plugin_state:{} | |  plugin_state:{} |
         +------------------+ +------------------+ +------------------+
                                  * ACTIVE
                                  ^
                                  |
                            renderer reads this
                            once per frame
```

The renderer's only session-aware read is `editor.active_session()`.
Everything `Conductor` owns is in `plugin_global_state`, which the
swap pointer does not touch — that is the structural property that
makes "Conductor lives above sessions" true.

### Why each session owns its buffers (window model)

> **Revised.** The earlier "buffer storage stays Editor-global"
> framing has been rejected (`§ Architecture priority`). This
> section describes the target model.

Each `Session` owns its own `HashMap<BufferId, EditorState>`.
Opening the same file in two sessions creates two independent
buffers; edits diverge. `BufferId` allocation stays globally
unique (single `Editor.next_buffer_id` counter) so plugin APIs
that thread buffer ids around don't have to disambiguate by
session, but the *storage* lives in whichever session the buffer
was opened into.

Why this is the right ownership:

1. **Parallel-agents semantics.** Alpha and beta are
   independent worktrees, possibly on independent branches.
   Edits the user makes in alpha's view of `foo.rs` should
   *not* echo into beta's view. Independent buffers per
   session is the model that delivers this.
2. **`closeSession` becomes trivial.** Drop the `Session`,
   take its buffer map and terminal manager with it. No
   refcount, no "shared with another session" check.
3. **Commands scope correctly by construction.** "Save all"
   iterates `active_session.buffers` — there's nothing else to
   iterate. "Close all," buffer cycling (Ctrl+Tab),
   quick-open, find-in-files, list-buffers all naturally act
   on this session's buffers because that's where buffers
   live.
4. **Cross-session operations stay explicit.** A diff that
   compares alpha's `foo.rs` against base's `foo.rs` calls
   `editor.sessions[base].buffers[id]` — a one-line helper.
   Cross-session is opt-in and visible at every call site,
   which is what we want for a feature whose UX promise is
   "each session is its own world."

### Why terminals live on `Session` too

Terminal PTYs (the OS process), terminal grid state, and
backing-file paths all live on `Session` alongside its buffer
map. The `terminal_manager`'s read/wait threads are owned by
the session that created them. Closing a session sends SIGTERM
to its agents and joins the threads — no orphan PTYs.

This matches user expectation: closing alpha's worktree should
clean up alpha's agent. With editor-global terminal storage we'd
need a separate "which session does this PTY belong to" lookup
plus closure logic; with per-session storage it falls out.

### Cross-session shared state lives on `Editor`

Some state genuinely is cross-session and stays on `Editor`:

- `next_buffer_id` — single counter so buffer ids are globally
  unique.
- Plugin runtime — single QuickJS instance.
- `plugin_global_state` — explicitly cross-session by
  definition.
- Theme / config / keybindings — user-level, not project-level.
- `sessions: HashMap<SessionId, Session>` — the multiplexer's
  table.
- `active_session: SessionId` — the pointer.
- Workspace recovery framework — the *infrastructure* is
  cross-session; per-session recovery files live under
  `.fresh/sessions/<id>/`.

Editor chrome (status bar rects, menu rects, prompt overlay
rects) also lives on `Editor` via a separate `chrome_layout`
struct, so mouse hit-testing on chrome doesn't collide with
session-scoped hit-testing on splits and tabs.

### Active session is a single pointer

`active_session: SessionId` is the only piece of session state read
on every render. Switching is atomic from the renderer's perspective:
update the pointer, redraw. All cached state — file tree expansion,
LSP clients, watchers — already lives on the (now-active) session.

> **In the window model:** there is no swap. `setActiveSession`
> is a single field write — `self.active_session = id`. Each
> render reads from `self.sessions[self.active_session]`. The
> warm-swap pattern that the interim implementation uses
> (`§ Implementation status snapshot`) is replaced by direct
> per-session field ownership.

### Session-global vs session-scoped plugin state

Two storage namespaces exposed to plugins:

```ts
// Editor-global (default).
editor.setGlobalState("conductor.sessions", JSON.stringify(state));
editor.getGlobalState("conductor.sessions"): string | null;

// Session-scoped (opt-in).
editor.setSessionState("my-plugin.foo", value);
editor.getSessionState("my-plugin.foo"): unknown;  // current active session
```

Conductor uses **only** the global namespace. Plugins that genuinely
want per-project state (per-language helpers, per-repo lint configs)
opt in to session scope.

The default is global because that's the *current* behavior — plugin
top-level scope persists for the lifetime of the editor — and we do
not want to silently change the meaning of existing plugins' module
state.

## Dive: pointer write (window model) / atomic swap (interim)

> **Window model (target):** `setActiveSession(id)` is a single
> field write — `self.active_session = id`. Every render reads
> from `self.sessions[self.active_session]`; nothing is moved.
> The "swap" framing below describes the **interim warm-swap
> implementation** that the migration replaces.

What visibly changes during `setActiveSession(1 -> 2)`:

```
BEFORE                                      AFTER
+--------------------------------------+    +--------------------------------------+
| Session 1: main                      |    | Session 2: feat/auth                 |
+--------------------------------------+    +--------------------------------------+
|  /repo               | src/main.rs   |    |  /wt/feat-auth      | db/schema.sql  |
|  - Cargo.toml        |  fn main() {  |    |  - db/              |  CREATE TABLE  |
|  - src/              |    println!.. |    |    - schema.sql ●   |    users (    |
|    - lib.rs          |  }            |    |  - src/             |    id SERIAL.. |
|    - main.rs ●       |               |    |    - models/        |    uuid UUID   |
|  - tests/            | rust-analyzer |    |      - user.ts ●    |       NOT NULL |
|                      |   (warm)      |    |  - aider.terminal   |       DEFAULT  |
|                      |               |    |                     |       uuid_..  |
+--------------------------------------+    +--------------------------------------+
| NORMAL Ln 12 main.rs                 |    | NORMAL Ln 8 schema.sql               |
+--------------------------------------+    +--------------------------------------+

CHANGES:                            UNCHANGED:
  file tree root                       Editor.terminal_manager (every PTY)
  ignore matcher                       Editor.theme, .config, .keybindings
  buffer set + tabs                    plugin runtime + plugin_global_state
  active LSPs (now session 2's)        session 1's LSPs (kept warm)
  split layout                         session 1's watchers
  status bar buffer state              Conductor's session list/collisions
```

`editor.setActiveSession(id)` performs:

1. **Snapshot** the outgoing session's last-active split, scroll
   positions, file-tree expansion, prompt state. Persist to
   `Session.layout_snapshot`.
2. **Update** `Editor.active_session = id`.
3. **Restore** the incoming session's snapshot to the live view
   state.
4. **Emit** `active_session_changed` to plugins.

LSPs, watchers, and plugin global state are never touched. The
inactive session's LSPs continue running; if a tool finishes
indexing while the user is in another session, it is ready
immediately on the next dive.

The renderer reads `editor.active_session()` once at the top of each
frame. There is no per-subsystem "switch" call — the switch is the
pointer write, and every read from then on routes through the
session.

## Lifecycle

| Event | Effect |
|---|---|
| `createSession({ root, label })` | Construct a new `Session`, walk file tree, build ignore matcher, lazily start LSPs on first buffer open. Return `SessionId`. Does not switch active. |
| `setActiveSession(id)` | Atomic swap (above). |
| `closeSession(id)` | Shut down LSPs, drop watchers, free per-session caches. If `id == active_session`, refuse with error (caller must switch first). Buffers attached to this session and not to any other are closed. |
| Editor shutdown | Persist session list (root, label, layout snapshot) to `.fresh/sessions.json`. Terminal PTYs and agent processes are torn down per existing rules. |
| Editor startup | Rehydrate session list. **Inactive sessions are lazy** — LSPs and file watchers do not start until the session is first activated. Only the active session is fully spun up. |

A typical lifecycle from a user's perspective:

```
t=0   Editor starts
      Editor.sessions = { 1: "main" (active) }
      plugin_global_state.conductor = { sessions: {}, collisions: {} }

t=1   User: <Leader>o, n, "feat/auth", "aider --message ..."
      git worktree add ../wt-auth feat/auth
      createSession({ root: /wt-auth, label: "feat/auth" }) -> id=2
      createTerminal({ sessionId: 2, cwd: /wt-auth })
      Editor.sessions = { 1: main (active), 2: feat/auth (warm) }

t=2   User: <Leader>o, Enter on session 2
      setActiveSession(2)        <-- atomic pointer swap
      Editor.sessions = { 1: main (warm), 2: feat/auth (active) }
      Conductor's internal map: untouched

t=3   Agent finishes; transitions to READY (terminal_exit, code 0)
      Conductor updates its map; status updates in Control Room

t=4   User: <Leader>o, m on session 2 (review skipped)
      git -C /repo merge feat/auth
      closeSession(2)            <-- LSPs torn down, watchers dropped
      git worktree remove /wt-auth
      Editor.sessions = { 1: main (active) }
```

## Control Room placement

The Control Room is a virtual buffer that must render identically
regardless of which session is active. Two options:

- **(A) Editor-global virtual buffer.** A new buffer-attachment kind
  that is not in any `Session.buffers` set; the renderer treats it
  as part of editor chrome. Drawn over the active session's UI.
- **(B) Mirrored across all sessions.** Every session's `panel_ids`
  contains the Control Room buffer, so it stays addressable after
  switches.

(A) is cleaner: one buffer, one panel id, no per-session
bookkeeping. It requires a small new affordance in
`virtual_buffers.rs` — an "editor-global" flag — but the rendering
path already special-cases dock leaves, so this is local.

(B) reuses existing machinery but means every `closeSession` has to
remember not to evict the Control Room. Strictly more error-prone.

This design picks **(A)**.

## Rich Control Room rendering — the two complementary primitives

The Screen 2 mockup (header line + 8-column SESSIONS table + dual
preview/collision-radar panes + hotkey footer + summary line, all
full-screen) is more than the existing `CenteredOverlay` (Live
Grep–style centered prompt with input + suggestions + path-driven
preview pane) can render. The two primitives below are designed to
compose to deliver it, and to be independently useful for any
future plugin that needs embedded editor views or full-screen
chrome.

### Primitive #1 — `render_content` becomes session-pluggable

Refactor the existing renderer so its dependencies are explicit
parameters rather than implicit `&mut self.*` reads. Concretely:

- `cell_theme_map` and `pending_hardware_cursor` (the two pieces
  of per-frame scratch state currently shared with the active
  render) become **per-call scratch buffers** the caller owns.
  Calling the renderer twice in one frame for two different
  `(SplitManager, view_states)` pairs no longer clobbers
  hit-testing or cursor placement of the first pass.
- The per-frame render path then makes a second call, after
  drawing the active session, with a previewed session's
  stashed `(SplitManager, view_states)` into a sub-rect.
- New plugin API: `editor.previewSessionInRect(sessionId)` —
  one-shot "for the next frame, render this session in the
  designated preview pane." Cleared on overlay close.

This consumes the warm-swap state from Step 1f (split tree +
view_states stash on `Session`) directly: the previewed session's
splits are already structured exactly the same way as the active
session's, just parked in `Session.splits_stash`. Rendering them
needs no transformation, just temporary access.

### Primitive #2 — plugin-owned full-screen overlay

A virtual buffer can declare itself a **full-screen overlay**: the
renderer treats it as editor chrome (drawn over the active
session, doesn't mutate splits, doesn't take over the buffer set),
and the plugin owns layout via a `Vec<Region>` callback. Each
region is either:

- **plugin-rendered text** — styled tokens from the existing
  virtual-buffer entry mechanism (header, table rows, footer,
  status — anything the plugin builds), or
- **a delegate** — `{kind: "session_preview", session_id}` that
  core fulfils via primitive #1.

Input flows through the existing `defineMode` mechanism — the
overlay declares a mode, plugin registers bindings (`Up`, `Down`,
`Enter`, `n`, `d`, `m`, `k`, `r`, `Esc`, etc.), and key events
route through the same dispatcher used by every other buffer
mode. No new input model.

### How Conductor composes both

```
+-----------------------------------------------------------+
| HEADER          (plugin region: styled text)              |
+-----------------------------------------------------------+
| SESSIONS TABLE  (plugin region: 8-column rows w/ styling) |
|                                                           |
+---------------------+-------------------------------------+
| PREVIEW (delegate)  | COLLISION RADAR (plugin region)     |
|                     |                                     |
| { kind:             |                                     |
|   "session_preview",|                                     |
|   session_id: <sel> |                                     |
| }                   |                                     |
+---------------------+-------------------------------------+
| FOOTER          (plugin region: hotkey hints)             |
+-----------------------------------------------------------+
| SUMMARY         (plugin region: aggregate counts)         |
+-----------------------------------------------------------+
```

The preview pane shows the **highlighted session's full editor
UI** — splits, terminals, syntax-highlighted buffers, LSP
markers, inline decorations — rendered natively. Live PTY output
streams in for free because the renderer reads the terminal grid
state directly each frame; no plugin TS code in the per-frame
path.

### Why this is the destination, not a steppingstone

- **Zero plugin frame-loop cost on the live content.** The agent
  terminal, the heaviest live data, is rendered by the same Rust
  code that draws the active session. No QuickJS bridge in the
  hot path.
- **Composable.** Primitive #1 unblocks any future plugin that
  wants embedded session/buffer views: side-by-side diff, code
  review, multi-session dashboards, picture-in-picture.
  Primitive #2 unblocks any plugin chrome: which-key menus,
  status dashboards, custom REPLs.
- **Clean ownership.** Chrome is plugin-rendered; embedded
  editor content is core-rendered; one delegate kind bridges
  them. There's no third layout system to maintain.
- **Bug-for-bug parity with the active session.** When the
  renderer adds a new feature (decorations, ANSI cell flags,
  fold rendering), the previewed session inherits it
  automatically.
- **No new IPC, daemon, or process.** Both primitives are local
  extensions of existing structures: parameters on the renderer,
  attachment-kind flag on virtual buffers.

### Effort breakdown

- Primitive #1 — Medium. Borrow-checker work to lift
  `cell_theme_map` and `pending_hardware_cursor` out of `Editor`
  into per-call scratch; one new render-call site for the
  preview rect; one plugin API.
- Primitive #2 — Medium. New attachment kind for virtual
  buffers; layout-region callback in the rendering path; plugin
  API surface (similar in spirit to PR #1880's Global Panel,
  but full-screen rather than edge-anchored, and with
  delegate-region support).

Status:
- **Primitive #1 — implemented.** `editor.previewSessionInRect(id)`
  renders the previewed session's stashed split tree (with
  syntax highlighting, terminal grid, decorations) into the
  floating overlay's preview pane. The active session's
  rendering is unchanged; `cell_theme_map` and
  `pending_hardware_cursor` were lifted into per-call scratch
  so a second render-pass per frame doesn't clobber the active
  area's hit-testing.
- **Primitive #2 chrome — implemented in minimum form.**
  `editor.setPromptTitle(...)` (header) and
  `editor.setPromptFooter(...)` (footer hotkey row) supply the
  styled chrome around the prompt + preview pane. Combined
  with Primitive #1's preview delegate, this composes to the
  screenshot's header / table / preview / footer layout
  without introducing a brand-new buffer-attachment kind.
- **Primitive #2 full attachment kind — deferred.** The
  fully-general "plugin-owned full-screen overlay" with
  arbitrary `Vec<Region>` layout is not implemented. Adding a
  side-by-side *collision radar* pane, multi-region custom
  layouts, or non-prompt overlays still requires this work.
  Tracked as a follow-up; the floating-prompt path covers the
  Conductor MVP's needs.

## User-facing screens

This section catalogues every screen the user can see, in the order
they typically encounter them. Each entry: a sketch, the user
objective the screen exists to satisfy, the flows that lead in and
out, and the controls available.

### Screen 1: Empty Conductor (first run)  `[MVP]`

```
+------------------------------------------------------------------+
| TABS:  src/main.rs                                               |
+------------------------------------------------------------------+
|                                                                  |
|  +============== CONDUCTOR =================================+    |
|  |                                                          |    |
|  |   No active sessions.                                    |    |
|  |                                                          |    |
|  |   Conductor lets you run multiple coding agents in       |    |
|  |   parallel git worktrees and switch between them as if   |    |
|  |   each were its own Fresh session.                       |    |
|  |                                                          |    |
|  |   Press  n  to spawn the first one.                      |    |
|  |   Press Esc to close.                                    |    |
|  |                                                          |    |
|  +==========================================================+    |
|                                                                  |
+------------------------------------------------------------------+
| NORMAL  Ln 1 main.rs                                             |
+------------------------------------------------------------------+
```

**Objective.** Discoverability. A user who pressed `<Leader>o` on a
hunch needs to learn (a) what the feature is and (b) the single key
that gets them started, without reading docs.

**Entry.** `<Leader>o` from any session, when `sessions.size == 0`
(only the implicit base session exists).

**Exit.** `n` opens the new-session prompt (Screen 4); `Esc` closes
the dock and returns the user to whatever they were editing.

**Controls.**

| Key | Action |
|---|---|
| `n` | Open new-session prompt |
| `Esc` | Close Control Room |

### Screen 2: Control Room  `[MVP — reduced; see § MVP scope]`

```
+----------------------------------------------------------------------------------+
| Fresh v1.0.2                                          2026-05-18  2.1GB / 32GB   |
+- SESSIONS -----------------------------------------------------------------------+
| #   LABEL            ROOT PATH               AGENT      STATE          DIFF    C  AGE|
| 1   main             /repo (base)            -          ACTIVE         -       -   - |
| 2 > feat/auth-v2     /wt/feat-auth-v2        aider      AWAITING (Y/n) +118-5  1  14m|
| 3   fix/redis-cache  /wt/fix-redis-cache     claude -p  RUNNING        +45-12  1  25m|
| 4   UI-refactor      /wt/UI-refactor         opencode   READY          +104-4  3   7m|
| 5   feat/auth-v2     /wt/feat-auth-v2-alt    aider      AWAITING (Y/n) +118-5  1  14m|
| 6   feat/redis       /wt/fix-redis-cache-alt aider      AWAITING (Y/n) +118-5  1  25m|
| 7   ...              ...                                                              |
| 11  new-editor       /wt/new-editor          shell      ERRORED        -0      1  13m|
| 12  test-user        /wt/test-user           shell      KILLED         -0      0  19m|
| 13  cargo-bump       /wt/cargo-bump          shell      SYNCING        -       1  25m|
| 14  agent-experiment /wt/agent-experiment    shell      SYNCING        +23-3   1  25m|
| 15  feat-editor      /wt/feat-editor         shell      SYNCING        +112-5  1  ...|
+----------------------------------------------------------------------------------+
| PREVIEW (session 2, feat/auth-v2)            | COLLISION RADAR  3 detected      |
| Aider> Tests failed at src/auth.rs:42.       | src/auth.rs       conflict-likely |
| > Analysis complete.                         |   session 1                       |
| > 1 failed test in src/models/user.ts:42     |   session 2                       |
| > Marked tests in src/auth.rs                | src/models/user.ts conflict-likely|
| AIDER> Do you want me to attempt a fix?      |   session 2                       |
| (Y/n): _                                     |   session 3                       |
|                                              | Cargo.toml         conflict-likely|
|                                              |   session 1, 2, 4                 |
+----------------------------------------------------------------------------------+
| Enter:dive  n:new  d:diff  m:merge  k:kill  r:rename  Esc:close  Ctrl+n/p:cycle  |
+----------------------------------------------------------------------------------+
| NORMAL  Ln 12 Col 1            CONDUCTOR | 15 sessions, 2 awaiting, 3 collisions |
+----------------------------------------------------------------------------------+
```

**Objectives.** This screen has to satisfy three distinct user tasks
in one view, ranked by frequency:

1. **Triage.** "Does anything need me right now?" — answered by the
   header line, the bottom Conductor summary line, and the
   AWAITING/ERRORED rows. The user should be able to leave the
   screen in under two seconds if the answer is no.
2. **Decide.** "Which session should I dive into / merge / kill?" —
   answered by the table (state, diff size, commits, age) plus the
   preview pane for the selected row.
3. **See trouble coming.** "Are any of these agents about to fight
   each other?" — answered by the collision radar with
   per-path severity.

A quaternary objective is **monitoring agent health passively**, but
the design deliberately does not satisfy that here — passive
awareness lives in the status bar (deferred; see "deferred features"
in the design conversation), not in this screen, because this screen
is full-screen and disruptive. The Control Room's own bottom summary
line (`15 sessions, 2 awaiting, 3 collisions`) provides aggregate
awareness *inside* the Control Room.

#### Columns

| Column | Meaning | MVP |
|---|---|---|
| `#` | Stable session id (1-indexed; the base session is always 1). Survives across restart, monotonically grows. | MVP |
| `LABEL` | User-facing name. Defaults to the branch name; `r` lets the user rename. Does not have to match the branch. | MVP (no rename) |
| `ROOT PATH` | Absolute filesystem root of the worktree. The base session shows the repo root annotated `(base)`. | MVP |
| `AGENT` | The shell command form spawned in the session's terminal. `-` for the base session; `shell` for a plain shell. | MVP |
| `STATE` | Parsed lifecycle state (see below). | MVP (reduced state set) |
| `DIFF (+/-)` | Lines added/removed compared to the merge base. Includes uncommitted changes; refreshed on a debounce. | MVP |
| `COMMITS` | Number of commits the session has made on its branch since branching from the base. | v1.1+ |
| `AGE` | Wall-clock age since session creation. | MVP |

#### States

| State | Meaning | Set by | MVP |
|---|---|---|---|
| `ACTIVE` | This session is the one currently being rendered (`active_session`). | Editor pointer. | MVP |
| `RUNNING` | Agent process is alive and producing output. | Default. Re-entered when output advances after `AWAITING`. | MVP |
| `AWAITING (Y/n)` | Output ends in a recognised prompt pattern; agent has stopped. | Regex on terminal output. | MVP |
| `READY` | Agent process exited cleanly (code 0). | `terminal_exit` event. | MVP |
| `ERRORED` | Agent process exited non-zero. | `terminal_exit` event. | MVP |
| `KILLED` | User pressed `k`; conductor sent SIGTERM. The row remains as a tombstone until dismissed (see "KILLED retention" in open questions). | User action. | MVP — but no tombstone in MVP; row drops immediately |
| `SYNCING` | A git operation initiated by Conductor is in flight in this worktree (merge into base, pull from remote, push). The terminal may be unresponsive during this. | Conductor entered git operation. | v1.1+ |

`KILLED` and `READY`/`ERRORED` are terminal states — the agent
process is gone — but the worktree is *not* automatically removed.
The user must press `m` (merge then remove) or `k` again on a
killed/ready row (remove without merge) to drop the worktree and
the session row.

#### Agent types

The `AGENT` column accepts any command. There are three usage
patterns the design accommodates explicitly:

- **AI coding agents** (`aider`, `claude -p`, `opencode --task`,
  …) — the primary use case. Their interactive prompts drive the
  `AWAITING` state.
- **`shell`** — a plain interactive shell in the worktree. State
  inference falls back to running-while-output-moves and
  ready-on-exit; there is no AWAITING heuristic for shells. Useful
  for "I want a worktree to poke around in by hand."
- **One-shot scripts** — anything that runs to completion and
  exits. They flicker through RUNNING → READY/ERRORED.

The Conductor plugin does not need to know which is which; the
state machine is uniform.

#### Parallel attempts on the same branch  `[MVP for spawn; v1.1+ for compare/promote-cluster]`

Rows 2/5, 3/6, 4/10 in the sketch above are deliberately on the
same logical task (`feat/auth-v2`, `fix/redis-cache`,
`UI-refactor`). PRD user story 1 ("spawn 3 different agents to
explore 3 different architectural approaches in parallel") drives
this. Conductor must allow:

- Multiple sessions on the same branch name. Implementation:
  worktrees get unique paths (`-alt`, `-2`, …) so `git worktree
  add` doesn't conflict.
- Side-by-side preview / diff between two sessions on the same
  task — pressed via `d` while two rows are selected (multi-select
  with Shift).
- "Promote one, kill the rest" as a single action — `m` on a
  selected row in a parallel-attempt cluster offers to kill the
  siblings.

The `LABEL` column makes parallel attempts identifiable; renaming
(`r`) is how the user disambiguates them ("auth-with-uuid",
"auth-with-snowflake").

#### Memory display in the header  `[v1.1+]`

`2.1GB / 32GB` is total Fresh-process RSS over total system RAM.
This exists because the warm-LSP architecture (`§ Lifecycle`) makes
memory the dominant cost of running many sessions; surfacing it
in the Control Room lets the user see it climb in real time as
they spawn sessions, and gives them a basis for deciding when to
close idle ones. Not load-bearing for any feature; can be hidden
via config.

**Entry.**
- `<Leader>o` from any session.
- Auto-open option (configurable, off by default): when any session
  transitions to AWAITING or ERRORED.
- After a successful `conductor.new` or `conductor.merge`, returning
  here.

**Exit.**
- `Enter`: dive into the selected session (Screen 3).
- `Esc`: close, return to active session's IDE.

**Common sub-flows.**

- *Quick triage*: open with `<Leader>o`, scan, close with `Esc`. No
  selection change persisted.
- *Spawn*: `n` → new-session prompt (Screen 4) → returns here with
  the new session selected.
- *Dive*: arrow to row → `Enter` → Screen 3.
- *Cycle without diving*: `Ctrl+n` / `Ctrl+p` cycles selection
  through sessions in id order; preview pane updates live.
- *Review-and-merge*: arrow to a `READY` row → `d` for diff → `m`
  to merge if happy → row enters `SYNCING` while git merge runs →
  on success row is dropped and worktree torn down.
- *Compare parallel attempts*: Shift-arrow to multi-select two
  rows on the same branch → `d` shows a three-way diff (base / row
  A / row B).
- *Abort*: arrow to a stuck or runaway session → `k` → confirm →
  state moves to `KILLED`. Row stays as a tombstone; press `k`
  again on the killed row to drop the worktree and remove the row.
- *Resolve collision*: collision radar shows path → click or arrow
  to it → opens diff comparing the two worktrees' versions.
- *Rename*: `r` on selected row → inline edit in the LABEL cell.

**Controls.**

| Key | Action | When enabled | Phase |
|---|---|---|---|
| Up / Down | Move selection | always | MVP |
| Shift + Up/Down | Extend multi-select | always | v1.1+ |
| Ctrl + n / Ctrl + p | Cycle selection (with wrap) | always | MVP |
| Enter | Dive into selected | session is not the active one | MVP |
| n | New session | always | MVP |
| d | Show diff | selection has changes | MVP (invokes existing review-diff) |
| m | Merge selected into base | state == READY | MVP |
| k | Kill agent (first press) / drop worktree (second press on tombstone) | not the base session | MVP — single press kills+drops |
| r | Rename / re-label session | always | v1.1+ |
| Tab | Cycle preview pane focus (terminal / collisions) | always | v1.1+ |
| Esc | Close Control Room | always | MVP |
| Mouse: click row | Select | always | MVP |
| Mouse: double-click row | Dive | session is not the active one | MVP |

`m` and `k`-on-non-tombstone both prompt for confirmation via
`showActionPopup` because both are destructive (work that hasn't
been pushed lives only in the worktree).

### Screen 3: Session IDE (post-dive)  `[MVP — falls out of architecture]`

```
+------------------------------------------------------------------+
| TABS:  schema.sql ●  | user.ts ●  | aider.terminal               |
+------------------------------------------------------------------+
|  /wt/feat-auth          | db/schema.sql                          |
|  - db/                  |  CREATE TABLE users (                  |
|    - schema.sql ●       |     id SERIAL PRIMARY KEY,             |
|  - src/                 |     uuid UUID NOT NULL DEFAULT         |
|    - models/            |       uuid_generate_v4(),  << aider    |
|      - user.ts ●        |     email VARCHAR(255) UNIQUE NOT NULL,|
|  - aider.terminal       |     created_at TIMESTAMP DEFAULT NOW() |
|                         |  );                                    |
|                         |                                        |
+------------------------------------------------------------------+
| TERMINAL: aider                                                  |
|  > Tests failed on line 42.                                      |
|  > Do you want me to attempt to fix them? (Y/n): Y_              |
+------------------------------------------------------------------+
| NORMAL Ln 12 schema.sql  |  feat/auth  |  agent: AWAITING        |
+------------------------------------------------------------------+
```

**Objective.** Provide a *normal Fresh editing experience*, scoped
to one worktree, with the agent's terminal a keystroke away. The
user has to be able to forget Conductor exists for the duration of
their focused work — the IDE must not feel like a sub-mode of
Conductor.

This screen is "as if Fresh always lived in this worktree."
Everything that's normally in a Fresh session — file explorer,
splits, LSP, quick-open, command palette, mouse — works unchanged.
The only Conductor-specific affordances are:

- The status bar shows the session label (`feat/auth`) and the
  agent's parsed state (`AWAITING`).
- `<Leader>o` returns to Control Room.
- (Optional) `<Leader>n` / `<Leader>p` cycle to next/previous
  session without going through the Control Room.

**Entry.** `Enter` on a row in the Control Room.

**Exit.**
- `<Leader>o` → Control Room.
- `<Leader>n` / `<Leader>p` → directly to another session's IDE.
- Closing the agent's terminal does not close the session; the user
  can keep editing or spawn a follow-up agent.

**Common sub-flows.**

- *Respond to prompt*: agent terminal is a tab → switch to it →
  type `Y` or whatever → return to editing.
- *Edit the agent's output*: open the modified files normally; LSP
  is rooted at this worktree, so jump-to-definition works in-tree.
- *Push back to Control Room*: `<Leader>o`.

**Controls.** All standard Fresh keybindings, plus:

| Key | Action |
|---|---|
| `<Leader> o` | Open Control Room |
| `<Leader> n` | Next session (cycle) |
| `<Leader> p` | Previous session (cycle) |

### Screen 4: New-session prompt  `[MVP]`

```
+------------------------------------------------------------------+
|  TABS:  src/main.rs                                              |
+------------------------------------------------------------------+
|                                                                  |
|     +---- New session (1/2) ----+                                |
|     | Branch name:              |                                |
|     | feat/auth-schema_         |                                |
|     +---------------------------+                                |
|       fix/redis-cache                                            |
|       feat/login                                                 |
|       (existing worktree branches)                               |
|                                                                  |
+------------------------------------------------------------------+
| NORMAL                                                           |
+------------------------------------------------------------------+
```

```
+------------------------------------------------------------------+
|  TABS:  src/main.rs                                              |
+------------------------------------------------------------------+
|                                                                  |
|     +---- New session (2/2) ----+                                |
|     | Agent command:            |                                |
|     | aider --message "_        |                                |
|     +---------------------------+                                |
|       claude -p ""                                               |
|       opencode --task ""                                         |
|       aider                                                      |
|       (recent commands)                                          |
|                                                                  |
+------------------------------------------------------------------+
| NORMAL                                                           |
+------------------------------------------------------------------+
```

**Objective.** Spawn a new agent with as few keystrokes as possible
while still letting the user pick the branch and command precisely.
Two steps because the two questions are conceptually distinct:
*where* the work happens (branch / worktree) and *what* runs there
(agent command).

**Entry.** `n` from the Control Room.

**Exit.**
- `Esc` at any step: cancel, no worktree created, return to Control
  Room.
- `Enter` on step 2 with a non-empty command: Conductor runs `git
  worktree add`, calls `createSession`, calls `createTerminal`,
  sends the command, and returns to Control Room with the new
  session selected.

**Common sub-flows.**

- *Resume existing branch*: type a name that matches an existing
  branch, accept the suggestion, agent boots in a worktree on that
  branch.
- *Create new branch*: type a name that doesn't exist, Conductor
  creates the branch off `main` (configurable base) before the
  worktree.
- *Reuse last command*: arrow down on step 2 to pick a recent
  command verbatim.

**Controls.**

| Key | Action |
|---|---|
| Type | Edit current step's value |
| Tab / Down | Cycle to next suggestion |
| Shift-Tab / Up | Cycle to previous suggestion |
| Enter | Submit current step |
| Esc | Cancel |

**Failure modes.** If `git worktree add` fails (dirty worktree,
locked branch, path collision), Conductor surfaces the git error in
a `showActionPopup` and leaves the user in the Control Room with no
state change.

### Screen 5: Collision warning popup  `[v1.1+ — depends on watchPath]`

```
+------------------------------------------------------------------+
|  Session 1 IDE (file tree | editor)                              |
|                                                                  |
|     +--- Collision detected ---------------------------+         |
|     |                                                  |         |
|     | src/models/user.ts is being modified by:         |         |
|     |   - session 2 (feat/auth-v2)                     |         |
|     |   - session 3 (fix/redis-cache)                  |         |
|     |                                                  |         |
|     | Severity: conflict-likely                        |         |
|     |                                                  |         |
|     | [Open Control Room]  [Show diff]  [Dismiss]      |         |
|     +--------------------------------------------------+         |
|                                                                  |
+------------------------------------------------------------------+
```

**Objective.** Make the user aware of an impending merge conflict
*at the time the second agent first touches the path*, when
intervention is cheapest, rather than at merge time when the diffs
have grown.

This is the only Conductor screen that interrupts the user's work
unsolicited. It is therefore deliberately conservative: it fires
once per collision-pair-per-session-pair, not on every subsequent
edit.

**Entry.** Automatic, fired by the collision matrix when a path's
modifying-session set grows from 1 to 2 (or 2 to 3, etc.).

**Exit.**
- `Open Control Room`: closes popup, opens Control Room with the
  collision pane focused on this path.
- `Show diff`: closes popup, opens a diff buffer comparing the two
  worktrees' versions of the file.
- `Dismiss`: closes popup; this collision-pair-on-this-path is
  silenced for the rest of the editor session. New collision pairs
  on the same path still fire.

**Controls.** Standard `showActionPopup` controls (Tab to move
between buttons, Enter to activate, Esc = Dismiss).

### How the screens compose

```
                              +---------------+
                              |  Empty (1)    |
                              +-------+-------+
                                      | n
                                      v
                              +---------------+
                              |  Prompt (4)   |
                              +-------+-------+
                                      | Enter (×2)
                                      v
       +-----------+  <Leader>o  +---------------+   Enter   +-------------+
       | Session   |<------------|  Control Room |---------->| Session IDE |
       | IDE  (3)  |------------>|     (2)       |<----------|     (3)     |
       +-----------+             +---------------+  <Leader>o+-------------+
             ^                          ^
             |                          |
             | (any screen)             | (any session, autofire)
             |                          |
       +-----+--------------------------+
       |  Collision popup (5)            |
       |  [Open Control Room] / [diff]   |
       +---------------------------------+
```

The Control Room is the hub; every other screen either feeds into
it (Empty, Prompt, Collision) or is reached through it (Session
IDE).

## Plugin API surface

Additions only. Nothing existing is removed or changed shape.

### Sessions  `[MVP]`

```ts
type SessionId = number;
type SessionInfo = { id: SessionId; label: string; root: string; createdAt: number };

editor.listSessions(): SessionInfo[];
editor.activeSession(): SessionId;
editor.createSession(opts: { root: string; label: string }): Promise<SessionId>;
editor.setActiveSession(id: SessionId): void;
editor.closeSession(id: SessionId): Promise<void>;

// Events
editor.on("session_created",        handler: string): void;
editor.on("session_closed",         handler: string): void;
editor.on("active_session_changed", handler: string): void;
// payload: { previousId: SessionId | null; activeId: SessionId }
```

### Buffer/terminal scoping  `[MVP for createTerminal; v1.1+ for openFile]`

`createTerminal` gains an optional `sessionId` and is `[MVP]`.
`openFile`'s `sessionId` parameter is `[v1.1+]` — MVP plugins open
files in the active session only.

```ts
editor.createTerminal({ sessionId?: SessionId, cwd?: string, ... }): Promise<TerminalResult>;
editor.openFile(path: string, opts?: { sessionId?: SessionId }): Promise<BufferId>;
```

Existing call sites without `sessionId` get the active session, so
existing plugins keep working.

### Terminal output and exit events  `[MVP — the small core change]`

```ts
editor.on("terminal_output", handler: string): void;
// payload: { terminalId: number; recentBytes: string; lastLine: string }

editor.on("terminal_exit", handler: string): void;
// payload: { terminalId: number; code: number | null }
```

Wired by firing plugin events at
`crates/fresh-editor/src/app/async_dispatch.rs:427,453`.

### File watching  `[v1.1+]`

Required for the collision radar. MVP ships with a placeholder
empty pane in the Control Room where the radar will go.

```ts
editor.watchPath(path: string, opts?: {
  recursive?: boolean;
  sessionId?: SessionId;     // tag for collision matrix; not for scoping
}): Promise<WatchHandle>;

editor.unwatchPath(handle: WatchHandle): void;

editor.on("path_changed", handler: string): void;
// payload: { handle: WatchHandle; path: string; kind: "modify"|"create"|"delete" }
```

Backed by the `notify` crate. The `sessionId` field is informational
(passed back in the event payload) so Conductor can build a
`Map<path, Set<SessionId>>` collision matrix without juggling its
own handle-to-session map.

### Plugin state scopes  `[MVP for global namespace; v1.1+ for session namespace]`

The global namespace is `[MVP]` because Conductor itself uses it.
The session namespace is `[v1.1+]` and exists for other plugins
that genuinely want per-project state.

```ts
editor.setGlobalState(key: string, value: unknown): boolean;
editor.getGlobalState(key: string): unknown;          // undefined if missing

editor.setSessionState(key: string, value: unknown): void;  // v1.1+
editor.getSessionState(key: string): unknown;               // v1.1+
```

The global namespace **already exists in core** as of this
design's authoring — `setGlobalState`/`getGlobalState` accept any
JSON-compatible value, are namespaced by the calling plugin's
name automatically, and treat `null`/`undefined` as delete. Plugin
isolation is verified by existing tests (Plugin A's keys are
invisible to Plugin B).

Cross-restart persistence is `[v1.1+]` — values live in the state
snapshot only, so they survive plugin reloads (good enough for
Conductor reloading itself) but not editor restarts. Persisting
to `.fresh/state/<plugin>.json` is the v1.1+ extension.

### Diff rendering  `[v1.1+]`

MVP invokes the existing review-diff feature
(`docs/internal/REVIEW_DIFF_*.md`,
`docs/internal/SIDE_BY_SIDE_HUNK_NAV_REBINDABLE.md`) from
Conductor's `d` action — Fresh already has a side-by-side review
diff with hunk navigation, and Conductor reuses it pointed at the
selected session's worktree against the base. The
`openDiffView` API below is a programmatic entry point with
arbitrary `oldText`/`newText` for plugins that need to diff
non-git content; it is `[v1.1+]`.

```ts
editor.openDiffView(opts: {
  oldText: string; newText: string;
  title: string;
  mode?: string;
  sessionId?: SessionId;
}): Promise<{ bufferId: BufferId }>;
```

## Migration sequence

The work is large (`§ Risks`) but factorable. Each step is a
reviewable PR.

> **Re-prioritised May 2026.** The branch landed the warm-swap
> migration and a working Conductor MVP plugin, but identified
> the warm-swap pattern itself as a half-finished architecture
> (`§ Architecture priority`). **Step 0 below — the
> session-as-window migration — is the new top priority and
> blocks all other Conductor feature work** (single-key prompt
> hotkeys, `d`/`m` actions, AGENT/DIFF columns, full Primitive
> #2, collision radar). Steps 4 / 7 / v1.1+ items resume after
> Step 0 lands.

### Step 0 — Session-as-window migration  `[BLOCKING — top priority]`

Goal: each `Session` owns the storage for everything it needs
to render and operate on, exactly like a VS Code window. The
warm-swap pattern goes away. `setActiveSession` becomes a
pointer write. Render becomes window-pluggable as
`Window::render(frame, area, &EditorChrome)`.

**Architectural rule for 0c onward (revised after 0b):** state
moves to `Window`, *and so do the methods that mutate it*.
Action handlers, edit operations, save/revert, undo/redo,
render, terminal operations — anything whose body primarily
mutates window-scoped state — relocates from `impl Editor`
onto `impl Window`. `&mut self` inside those methods is the
window; there is no "active window" lookup at the call site.
Editor-global state that handlers need (config, theme,
filesystem) is `Arc<…>` cloned into Window or threaded as
parameters; plugin hooks that handlers want to fire become
return values dispatched by the Editor shim. `impl Editor`
keeps only window lifecycle, cross-window orchestration,
editor-global mutations, and the top-level dispatcher.

> **Strong preference: `impl Window` methods over inline
> borrows.** When you hit a borrow-checker conflict in a handler
> on `impl Editor`, the *first* thing to try is **moving the
> handler onto `impl Window`** — not adding a
> `let __win = self.windows.get_mut(&self.active_window).expect(...);`
> + sub-field-extraction block. The inline-borrow pattern works
> for one-off cases where two disjoint sub-fields of the active
> window need to be `&mut`-aliased in the same expression
> (the `apply_event_to_buffer` shape: buffers + splits +
> event_logs together), but it is **not the right tool when an
> impl method is possible**. Reasons, in order of importance:
>
> 1. *The "active" concept leaks into every handler body.*
>    `self.windows.get_mut(&self.active_window)` only makes
>    sense when the editor has an active concept — it spreads
>    that concept into code that should just operate on a
>    window. Moving the method onto `impl Window` makes
>    `&mut self` *be* the window; the body has no idea
>    whether it's active.
> 2. *It blocks "operate on a non-active window."* Conductor
>    diffs and cross-window orchestration want to call the
>    same handler against any window, not just the active
>    one. An `impl Window` method makes that free
>    (`alpha.handle_X(...)`, `base.handle_X(...)`); an
>    inline-borrow handler is permanently active-only.
> 3. *It's verbose.* Each conflict site bloats by 3–6 lines
>    of boilerplate. An `impl Window` method has zero
>    boilerplate at the call site (one method call) and
>    inside the body uses normal `self.X` field access.
> 4. *The borrow problem doesn't actually require it.* The
>    inline-borrow pattern is a workaround for the wrong
>    layer holding the method. Putting the method on
>    `impl Window` makes `self.X` and `self.Y` cleanly
>    splittable by the borrow checker — no workaround needed.
>
> The escape hatch — when `impl Window` is genuinely not
> possible — is when the handler body needs *Editor-global*
> state that can't be cheaply threaded as a parameter
> (`buffer_metadata`, `composite_buffers`, `plugin_manager`,
> mutating UI state like `prompt`/`status_message` deeply
> intermixed with window mutation, or several of these at
> once). In that case the inline-borrow pattern is acceptable
> as a holding measure — but the comment at the site should
> say "TODO: move to impl Window once <X> is threadable" so
> the debt is visible.
>
> When migrating an existing inline-borrow site to
> `impl Window`: pull the body into a `Window` method,
> change `&mut self.active_window_mut().X` to `&mut self.X`,
> have the method *return* anything Editor-global (events to
> log, status messages to set, plugin-hook payloads), and
> rewrite the Editor caller to one line plus the
> post-mutation dispatch.

This rule was learned the hard way during 0b: the
accessor-method strategy used there (`Editor::split_manager_mut`
etc.) returns references bound to `&mut self`, which makes the
borrow checker treat *every* such call as locking all of
`Editor`. Two such accessors can't compose; nor can one
accessor compose with a read of any other Editor field. The
0b code worked around this with inline `self.windows.get_mut(
&self.active_window)…` direct field access at conflict sites,
which is correct but verbose and leaks the "active" concept
into every handler. Putting the methods on `impl Window`
eliminates the workaround: `self.X` becomes a normal Rust
field access on the right type, and the borrow checker splits
it cleanly.

Sub-steps, in dependency order:

**0a — Move `cached_layout` (split / tab / file-explorer
parts) onto `Session`.** **Status: shipped.** Audit
`Editor::cached_layout` reads/writes; split into
`Window.layout_cache` (split-leaf rects, tab rects,
file-explorer rects, view-line mappings) and
`Editor.chrome_layout` (status bar, menu, prompt overlay,
popups, suggestions, settings modal, full-frame cell-theme
map). Mouse hit-testing routes through the right one. Reached
on the active window via `Editor::active_layout()` /
`active_layout_mut()`. (One commit on
`claude/window-state-migration-RjEwX`.)

**0b — Convert warm-swap stashes to live fields on `Session`.**
**Status: shipped.** The fields that today are `Option<…>`
stashes (`splits_stash`, `file_explorer_stash`, `lsp_stash`,
`panel_ids_stash`, `file_mod_times_stash`) become live
`Window` fields (`splits` is `Option<(SplitManager,
HashMap<LeafId, SplitViewState>)>` because layout allocation is
deferred to first activation; the rest are direct).
`set_active_window` is now a pointer write — the swap body is
gone, replaced by seed-buffer/layout allocation on first dive
into a never-activated window. Editor accessors
(`split_manager()` / `_mut()`, `split_view_states()` / `_mut()`,
`file_explorer()` / `_mut()`, `lsp()` / `_mut()`, `panel_ids()`
/ `_mut()`, `file_mod_times()` / `_mut()`) cover the common
case. (Five commits on `claude/window-state-migration-RjEwX`,
one per field.)

**Lessons from 0b — the accessor-method strategy was wrong.**
The shipped 0b uses `Editor::X()` / `X_mut()` accessor methods
that resolve to the active window's field
(`Editor::split_manager_mut(&mut self) -> &mut SplitManager`,
etc.). That works at sites that touch one window field at a
time, but breaks at sites that compose two: the method's
return reference is bound to `&mut self`, so it locks *all*
of `Editor` for its lifetime. Concretely: `self.X_mut()` and
`self.Y_mut()` (or `&self.Z`) cannot coexist even when X, Y, Z
are disjoint fields, because the borrow checker only sees two
overlapping `&mut self` borrows.

Direct field access splits cleanly — `self.windows.get_mut(...)`
locks `self.windows` only, leaving `self.config` / `self.theme`
/ `self.event_logs` free. So in 0b's hot-path code, we worked
around the accessor problem by inlining the field access at
conflict sites:

```rust
let active_id = self.active_window;
let window = self.windows.get_mut(&active_id).unwrap();
let state = window.buffers.get_mut(&id).unwrap();
let (mgr, vs) = window.splits.as_mut().unwrap();
// state, mgr, vs all live at once: disjoint sub-fields of Window.
```

This pattern works but it's verbose, repetitive, and at the
wrong layer — every action handler that operates on a window
shouldn't have to thread "active" through its body or rebuild
the same boilerplate.

**The right primitive: methods on `impl Window`.** Most action
handlers today live on `impl Editor` because that's where the
state lived in the legacy single-window codebase. After Step 0,
the state for those handlers (buffers, splits, file_explorer,
lsp, event_logs, terminals, …) lives on `Window`. The methods
should follow the data: handlers that mutate window-scoped
state move to `impl Window`, where `&mut self` *is* the window
and there is no "active" concept inside the method body. The
borrow problem disappears structurally — `self.buffers` is a
direct field access on Window, splits cleanly with
`self.splits`, and Editor isn't involved.

```rust
impl Window {
    pub fn handle_insert_char(&mut self, ch: char, cfg: &Config) {
        let buf_id = self.active_buffer();
        let state = self.buffers.get_mut(&buf_id).unwrap();
        let cursors = &mut self.splits.as_mut().unwrap().1
            .get_mut(&self.active_split()).unwrap().cursors;
        // ... mutate state + cursors freely ...
    }
}
```

What stays on `impl Editor`:

- Genuinely cross-window operations (Conductor's compare-alpha-
  vs-base, find-references-across-all-windows).
- Window lifecycle (`create_window`, `set_active_window`,
  `close_window`).
- Editor-global mutations (theme apply, config reload, plugin
  reload, quit).
- The thin top-level dispatcher that pulls the active window
  out, calls the right `Window` method, and fires deferred
  plugin hooks based on what changed.

Editor-global state that handlers genuinely need (config,
theme, filesystem) is shared via `Arc<…>` cloned into Window
on construction, or passed as `&Config` parameters. Plugin
hooks are *returned* from Window methods as event values so
the Editor shim can dispatch them after the window mutation
returns — keeps `plugin_manager` off Window.

This shape removes the macro / inline-direct-field-access
workarounds entirely. It also makes "operate on a non-active
window" first-class: Conductor's diff helper just calls
`alpha.X(...)` and `base.Y(...)` directly, no swap, no
"setActiveWindow before the operation" gymnastics.

**0c — Move `Editor.buffers` onto `Session`.** **Status: not
yet shipped — first attempt reverted, recommended approach
revised.**

`Window.buffers: HashMap<BufferId, EditorState>` replaces
today's `Window.buffers: HashSet<BufferId>` and
`Editor.buffers: HashMap<BufferId, EditorState>`.
`next_buffer_id` stays globally unique (allocated via
`Arc<AtomicUsize>` shared into windows, or a `&mut
IdAllocator` parameter to the few methods that allocate ids).

**First attempt:** moved the type, then sed-rewrote every
`self.buffers.X` call site to inline-windows-access. That
left ~50 borrow-checker conflict sites because the inline
expression locks `self.windows` while the body needs other
window or Editor fields. Reverted to keep the branch
compiling cleanly. The conflict pattern is the same one from
the 0b lessons above — the accessor-method strategy can't
support it.

**Recommended approach for the next attempt — three phases:**

1. **Move the field.** Change `Window.buffers` from
   `HashSet<BufferId>` to `HashMap<BufferId, EditorState>`,
   delete `Editor.buffers`, hand the seed buffers to the base
   window in `editor_init`. No call-site rewrites yet — this
   step intentionally breaks compilation.

2. **Move the methods.** For each `impl Editor` method whose
   body primarily mutates buffer state (action handlers, edit
   ops, save/revert, undo/redo), relocate it to `impl Window`.
   `&mut self` becomes `&mut Window` and `self.buffers` is now
   direct field access. Editor-global needs become Arc-shared
   fields on Window (`config`, `theme`, `filesystem`) or
   parameters (`&Config`, `&mut PluginManager` if it really
   has to fire a hook from inside; usually it shouldn't).
   Plugin-hook side effects become return values that the
   Editor dispatcher fires after the call.

3. **Editor shim layer.** The top-level `Editor::handle_action`
   / `Editor::handle_key` / `Editor::render` becomes a thin
   dispatcher that pulls `&mut self.windows[&self.active_window]`
   (direct field access — no accessor method), calls the
   appropriate Window method, then handles the returned
   plugin hooks / events.

Cross-window operations (Conductor diff, find-references-all)
stay on `impl Editor` because they really do touch multiple
windows. They access them by id with explicit
`self.windows.get(&id)` — no accessor wrappers needed.

**Why this is faster than the macro / per-site refactor.**
Both fix-the-symptoms approaches (macros, pre-extracted
`&mut Window` at every conflict site) leave the methods on
`impl Editor` and pay borrow-checker tax at every call.
Moving the methods to `impl Window` fixes the cause — the
methods belong there architecturally, and the borrow problem
goes away because `self` is the right type. Each method
relocation is mechanical (cut/paste + parameter changes for
Editor-global needs), and the result is shorter, clearer
code at every call site.

**0d — Move terminal manager + terminal-buffer indexes onto
`Session`.** `terminal_manager`, `terminal_buffers`,
`terminal_backing_files` all become per-window. PTY threads
are owned by the window that created them. `closeWindow`
joins those threads. `terminal_id` allocation stays global
(`Arc<AtomicUsize>` or similar) for plugin-API stability.
Terminal action handlers move to `impl Window` as part of 0d
— same recipe as 0c.

**0e — Move `event_logs` (undo per buffer) onto `Session`.**
*Shipped.* Undo logs followed the buffer storage onto `Window`
— each window owns its event-log map alongside its buffers, and
the existing undo/redo handlers route through
`active_window_mut().event_logs` (or split-borrow `__win.event_logs`
when paired with split-tree access in the same handler).

**0f — Move `position_history`, `bookmarks`, and similar
session-scoped per-buffer metadata onto `Session`.** *Shipped.*
`Window` now owns its back/forward navigation stack
(`position_history` plus the `in_navigation` and
`suppress_position_history_once` companion flags) and its
single-char bookmark register set (`bookmarks`). Switching
windows preserves each window's nav history and bookmarks
intact — the post-switch user sees their previous back-stack,
not the other window's. Workspace serialization captures the
active window's bookmarks; restore re-creates them on the
active window.

**0g — Audit commands.** *Shipped.* Audit pass over the
codebase after 0a–0f confirms:
* Zero direct `self.<moved-field>` references on `impl Editor`
  for any field that 0a–0f moved (`buffers`, `event_logs`,
  `terminal_*`, `file_mod_times`, `file_explorer`, `lsp`,
  `panel_ids`, `splits`, `position_history`,
  `in_navigation`, `suppress_position_history_once`,
  `bookmarks`). The few hits in `bookmarks.rs` are inside
  `BookmarkState::self.bookmarks` (its internal `HashMap`
  field, not the editor's).
* Cross-window `self.windows.get(&id)` / `get_mut(&id)` calls
  exist only where they should: the plugin API surface
  (`plugin_dispatch.rs`, `plugin_commands.rs` — `createWindow`,
  `setWindowState`, plugin-driven cross-window dispatch),
  window lifecycle (`window_actions.rs` —
  `set_active_window`, `close_window`, first-dive seeding),
  and a couple of split-borrow inline patterns in handlers
  that need disjoint `&mut __win.X` and `&mut __win.Y`
  sub-borrows in the same call.
* Active-window-routed handlers go through the
  `active_window()` / `active_window_mut()` accessors or
  through inline `self.windows.get_mut(&self.active_window)`
  borrows where the borrow checker needs sub-field splitting
  (the established pattern from 0c–0f). No method on
  `impl Editor` reaches around the routing.

**Outstanding debt (do not park indefinitely):** ~33 handler
sites still use the inline
`let __win = self.windows.get_mut(&self.active_window).expect(...)`
+ sub-field-extraction pattern instead of an `impl Window`
method. Each one mixes window-scoped state mutation with at
least one editor-global concern (`status_message`,
`plugin_manager`, `buffer_metadata`, `composite_buffers`,
`config` reads, mouse / drag UI state, etc.) which is why they
weren't moved during 0c–0f's bulk migration. **They should
still be moved.** The migration recipe — pull the body into a
`Window` method, return events / status payloads as values,
have the Editor caller dispatch them after the call — applies
to all of them; it just takes a per-site judgement on what
shape the return value should be.

This is genuinely the right cleanup, not a "could be nicer"
nice-to-have: every inline-borrow site is a permanent
"active-only" lock on a handler that should work against any
window for Conductor's cross-window orchestration to compose
cleanly. Leaving them inline means Conductor's diff /
find-references-all features will hit the same workarounds we
just paid down. **Strong preference: drain this list as part
of the work that introduces the first cross-window consumer.**

**0h — Refactor render to `Window::render`.** *Shipped, in
the form that turns out to matter.* The concrete pain point
0h was meant to solve was the preview path's `splits.take()`
+ restore dance in `render_session_preview_into_rect`: it
took the previewed window's split stash out, called the
shared `SplitRenderer::render_content` against it, then
swapped it back. After 0a–0g moved every per-window field
onto `Window`, the preview path can split-borrow the
previewed window's `buffers`, `event_logs`, and `splits`
sub-fields directly under one `&mut Window` borrow — the
take/restore is gone, and the foreign-window preview is now
literally the same `SplitRenderer::render_content` call
against a different `Window` with a sub-rect, matching the
design's Primitive #1.
* What we did *not* do, and why: a full move of
  `Editor::render` onto `impl Window` would require
  splitting the function's chrome (status bar, prompt,
  popups, menus, mouse hit-testing, animations) from its
  content (split tree, panels). The chrome is editor-global
  and lives on `Editor` legitimately; the content rendering
  already routes through the shared `SplitRenderer::render_content`
  helper that both paths call. Moving `Editor::render`
  itself would just relocate the chrome plumbing onto
  `Window`, which is the wrong direction. Park unless a
  consumer (e.g. a multi-window split-screen layout) needs
  to render two windows side-by-side in one frame.

**0i — Remove the warm-swap helpers and Conductor's reliance
on them.** *Shipped.* The swap body in `set_active_window`
went away in 0b (it's a pointer write plus first-dive seed
allocation). What remained for 0i, now done:
* Deleted the `attach_buffer_to_active_window` no-op shim and
  removed every call site (~14 locations across virtual
  buffers, terminal, file open, macros, composite buffers,
  buffer management). Buffer inserts go directly into
  `Window.buffers` via the canonical
  `insert_buffer_into_active_window` path.
* Updated test-helper comments in `editor_accessors.rs` that
  still framed the assertions as "warm-swap restored the
  stash" — the assertions remain correct, but they're now
  just "the active window owns this state directly."
* `detach_buffer_from_all_windows` is kept because it serves
  a real purpose (find-and-remove-by-id across windows when
  the caller doesn't know which window owns the buffer); its
  doc-comment is updated to note that with each buffer in at
  most one window, it succeeds at most once.

The `splits` field's `Option` wrapper remains: a
never-activated window still has no layout until first
dive, and seeding a layout at `Window::new` time would
require allocating a fresh `BufferId` from the editor-scoped
allocator before the window is wired into `Editor.windows`.
The `Option` accurately models "no layout allocated yet,"
not "stash currently swapped out," so it stays.

After Step 0 lands:

- "Save all," quick-open, find-in-files, list-buffers all
  scope to the active session by construction.
- Same file open in two sessions = two independent buffers,
  edits diverge. Matches the parallel-agents promise.
- Preview renders the entire session UI (file explorer,
  splits, terminals, status bar, tabs) for free via
  `render_session`.
- Closing a session is a single `Session::drop` — buffers,
  PTYs, undo logs, watchers all evicted.
- The "transient session swap" code path I built for the
  preview goes away.

Estimated effort: 5–10× the work that has gone into the
branch so far. Multiple commits per sub-step. The mechanical
churn is large but the per-commit risk is bounded; tests
catch regressions immediately because they exercise the
active-session path.

### Implementation status snapshot

The branch `claude/window-state-migration-RjEwX` shipped the
warm-swap interim (Steps 1a–1h, 2, 3, 5, 6) and then the first
half of the Step 0 (window-model) migration. Where we are
right now:

**Shipped on branch (compiles cleanly):**

- Steps 1–6 from the original interim (warm-swap rendering of
  every per-session subsystem; Conductor plugin MVP).
- **Step 0a** — `cached_layout` split into `Editor.chrome_layout`
  (full-frame chrome rects + cell-theme map) and
  `Window.layout_cache` (split / tab / file-explorer rects +
  view-line mappings). One commit.
- **Step 0b** — every warm-swap stash converted to a live
  `Window` field. `set_active_window` is a pointer write
  (plus first-dive seed allocation for windows that have never
  been activated). Five commits, one per field:
  `panel_ids`, `file_mod_times`, `file_explorer`, `lsp`,
  `splits` (the split tree + per-leaf view state pair).

The "switching windows feels like swapping the entire Fresh
state" promise from `§ Motivation` is now true by
*construction*, not by warm-swap: file tree, ignore matcher,
LSPs, watchers, split layout, dock occupancy, and mtime cache
all live on `Window` outright; switching is a single
`active_window = id` write.

The 16 e2e tests in `crates/fresh-editor/tests/e2e/sessions.rs`
still pass — their assertions about "diving and diving back
preserves state" are now satisfied by the per-window storage
rather than by the swap, but the surface behaviour is
unchanged.

**Not yet shipped (Step 0c–0i):**

`Editor.buffers` is still editor-global. Step 0c was attempted
on this branch and reverted because it ran into ~50 unique
borrow-checker conflict sites — the inline windows-field
access pattern that worked for 0b doesn't compose with sites
that need *two* concurrent mutable sub-borrows on a window
(typically `buffers` + `splits`). See `§ Step 0c` above for
the diagnosis: the accessor-method strategy used in 0b was
the wrong primitive. Methods that mutate window-scoped state
should live on `impl Window`, not `impl Editor` — that's what
makes the borrow problem go away by construction (`&mut self`
becomes the window, not the editor; sub-fields split the
normal way).

The recommended next attempt for 0c–0f is the recipe in
`§ Step 0c`:

1. Move the field (deletes the Editor field, breaks
   compilation intentionally).
2. Move the methods that primarily mutate that field onto
   `impl Window`. Editor-global needs become `Arc<Config>` /
   `Arc<Theme>` / `Arc<dyn FileSystem>` shared into Window,
   or `&Config` parameters. Plugin-hook side effects become
   return values dispatched by the Editor shim.
3. Editor's top-level dispatcher pulls `&mut self.windows[
   &self.active_window]` directly (field access, splits the
   borrow on `self.windows`) and calls the Window method.

The same recipe drives 0d (terminal manager), 0e (event logs),
0f (position history / bookmarks). 0g is the audit pass that
makes sure no "operate on the active window" logic is left on
`impl Editor`. 0h moves render to `impl Window`. 0i cleans up
the last remnants of the warm-swap shape (drop `splits`
`Option`, drop the `attach_buffer_to_active_window` /
`detach_buffer_from_all_windows` shims, simplify the e2e
tests' "stash" framing).

### Step 1 — `Session` struct, single forced session  `[interim — superseded by Step 0]`

> **Status:** landed on the branch as the warm-swap interim.
> Step 0 above replaces this with the window model.

- Introduce `Session` with the fields above.
- Construct exactly one session at startup, rooted at process cwd.
  Active forever.
- Move project-root reads to flow through
  `editor.active_session().root` *without changing behavior*.
- File tree, ignore matcher, LSP clients, watchers move to the
  session. Buffer storage stays on `Editor`; add the
  `Session.buffers: HashSet<BufferId>` membership field.
- Existing plugin APIs (`getCwd`, etc.) read from the active session.
- All existing tests must pass unchanged.

This was the bulk of the warm-swap refactor and the riskiest
step. It is purely a rearrangement: behavior is identical to
today's editor. **Step 0 (window model) inverts this step's
"buffer storage stays on Editor" choice and lifts the
warm-swap stashes into per-session live fields.**

### Step 2 — multiple sessions, manual switching  `[MVP]`

- Add `createSession`, `setActiveSession`, `closeSession`.
- Implement the atomic swap (`§ Dive`).
- Add `editor.listSessions()` / `activeSession()` plugin APIs and
  the `active_session_changed` event.
- A test plugin that calls `createSession` + `setActiveSession`
  exercises the swap end-to-end.

### Step 3 — terminal events to plugins  `[MVP]`

Smallest core change. Add `terminal_output` / `terminal_exit` events
at the two `async_dispatch.rs` arms.

### Step 4 — `watchPath` plugin API  `[v1.1+]`

Wrap `notify` crate. Surface `path_changed` event. Required for the
collision radar.

### Step 5 — plugin state scopes  `[MVP — already implemented in core; v1.1+ for session and persistence]`

`setGlobalState`/`getGlobalState` are already in core at the time
this design was written, with per-plugin namespacing and
roundtrip/isolation/delete tests. No further work needed for MVP.

`setSessionState`/`getSessionState` (per-session scope) and
cross-restart persistence to `.fresh/` are implemented.
Persistence flushes:
- `<wd>/.fresh/sessions.json` — `{ active, next_id, sessions[] }`
  with each session's id, label, root, and per-session
  plugin_state.
- `<wd>/.fresh/state/<plugin>.json` — one file per plugin
  with that plugin's `setGlobalState(...)` map.
Reload runs after authority install and before plugins load,
so plugin on-load handlers see the previous run's
`getGlobalState(...)` values. Persisted sessions reload as
inert shells — first dive re-warms exactly like a freshly
created session.

### Step 6 — Conductor plugin (separate doc)  `[MVP — minimum viable plugin]`

A first-party plugin shipping in `crates/fresh-editor/plugins/conductor/`.
Drives the whole feature. Uses only the APIs introduced above. The
MVP plugin implements Screens 1–4 with the reduced column set and
state set defined in `§ MVP scope`.

### Step 7 — programmatic diff API  `[v1.1+]`

A plugin-callable `openDiffView` with arbitrary content (not just
two refs in a git repo). MVP doesn't need this because the existing
review-diff feature covers the only Conductor diff use case
(worktree vs base).

### Step 8 — session persistence across restart  `[implemented]`

Implemented as cold rehydration: persisted sessions load as
inert shells (no LSP, no warm split tree). The first dive
into a previously persisted session re-warms it the same way
a freshly created session is warmed. Storage lives at
`<wd>/.fresh/sessions.json` and `<wd>/.fresh/state/<plugin>.json`
(see Step 5 above).

Hot/lazy rehydration of inactive sessions (warm LSPs at boot,
warm split layout, warm file watchers) is the v1.1+ extension
— useful when the user has many always-on agents and wants
zero-latency dive-back at startup.

## Risks

1. **Step 1 is invasive.** Every place that today reads cwd or
   project-root state must be re-routed through
   `editor.active_session()`. Compiler enforcement is the mitigation:
   move the field off `Editor` and onto `Session` early so the
   compiler errors point at every call site.

2. **LSP teardown on `closeSession`.** Today LSPs mostly key on
   project root, but the manager has assumed-singleton ergonomics in
   places. Audit `services/lsp/manager.rs` before Step 2.

3. **Buffer-to-session attribution edge cases.** A buffer opened
   from a path that lies under no session's root: which session
   owns it? Proposal: editor-global, attached to no session, opens
   in a "scratch" surface. Surfaced as a separate concept so it
   doesn't muddy session semantics.

4. **Plugin reload during a session swap.** If the plugin runtime
   reloads mid-swap, in-flight events are lost. Mitigation: drain
   the plugin event queue before the swap commits.

5. **Lazy LSP startup may surprise users.** First-time activation of
   an inactive session has the usual "rust-analyzer is indexing"
   pause. Document explicitly. A pre-warm hint
   (`editor.prewarmSession(id)`) could be added later if needed.

6. **Cross-session cursor jumps.** "Go to definition" landing in a
   file under a different session's root is undefined under this
   design. Proposal: open the target buffer in the *current* session
   (attaching the buffer id to its `buffers` set) rather than
   switching sessions — the alternative is a surprise dive.

7. **Memory growth with many warm sessions.** N rust-analyzers at
   500MB+ each adds up. This is intrinsic to "warm LSPs across
   sessions" and acceptable per `§ Trade-off discussion`. A future
   `editor.suspendSession(id)` (kill LSPs, keep buffer text) is a
   reasonable escape hatch but not part of v1.

## Trade-off discussion

(Carried over from the design conversation that produced this doc;
recorded here so the rationale is reviewable.)

Three architectures were considered:

- **(A) Plugin-driven workspace switching.** One Fresh process; a
  plugin asks core to mutate `cwd` and rebuild file-tree / LSP /
  ignore in place. Smallest core change but most fragile UX: every
  subsystem rebuild is a separate event the user can see seams in.
- **(B) First-class `Session` in core.** This document. Larger core
  change but the swap is atomic and inactive sessions are warm.
- **(C) Multi-process: one Fresh server per worktree, client
  multiplexes.** Best crash isolation, biggest architectural lift,
  new IPC, two plugin runtimes (or a coordinator). Roughly N×60MB
  fixed-cost-per-server overhead beyond the N×LSP cost that
  dominates either way.

(C) was rejected because crash isolation is not a requirement and
the per-process overhead, while not free, is small relative to LSP
cost. (A) was rejected because "Conductor lives above sessions" is a
load-bearing UX claim that (A) cannot honor — under (A), Conductor
*is* the editor reaching into its own root, and every glitch in the
in-place rebuild is a Conductor glitch. (B) is the architecture
that makes the UX claim true by construction.

## Open questions

1. **Should sessions persist across restarts by default?** Two
   schools: VS Code reopens last workspace; vim opens fresh. Default
   to "rehydrate session list, do not auto-dive into one of them"
   for now; user lands in a scratch session and picks. Configurable.

2. **Maximum sessions.** N=20 worktrees with N rust-analyzers will
   melt a laptop. A soft cap (configurable, default 8?) with a warning
   would be friendly. Out of scope for the core abstraction; can be
   enforced in the Conductor plugin.

3. **Session-aware command palette.** Should the palette show
   commands from all sessions, or just the active one? Default:
   active only, since commands tend to be buffer-scoped.

4. **Cross-session search.** Quick-open today scopes to cwd; under
   sessions, default is active session's root. A "search across all
   sessions" mode is desirable but post-v1.

5. **Authority composition.** A future remote session would carry an
   authority alongside its root. The fields nest cleanly
   (`Session.authority: AuthorityHandle`), but the spawning/teardown
   sequence interacts with `AUTHORITY_DESIGN.md` and is deferred.

6. **`KILLED` retention policy.** The Control Room mockup shows
   killed rows lingering as tombstones (e.g. a 19-minute-old row).
   Two design choices to settle:
   - *Within a session*: linger until the user presses `k` again,
     or auto-drop after N minutes? Tentative default: linger
     until acknowledged. Tombstones are evidence; auto-drop loses
     it.
   - *Across editor restart*: persist tombstones, or drop them?
     Tentative default: drop on restart — tombstones are a
     within-session debugging aid, not durable record.

7. **`SYNCING` semantics — what counts as a sync?** Three candidates:
   - Conductor-initiated git operations on the worktree (merge,
     pull, push). Definitely.
   - User-initiated git operations from inside the worktree's
     terminal (`git pull` typed by hand). Probably no — Conductor
     can't reliably detect these.
   - Filesystem syncs after a remote agent edit (e.g. waiting for
     `aider` to finish writing a batch of files before recomputing
     diff stats). Maybe — would smooth the diff readout but adds
     complexity. Defer until the diff readout is shown to flicker.

8. **`shell` agent state machine.** Plain shell sessions never hit
   `AWAITING` (no recognised prompt pattern). Should they show a
   distinct state, or just stay in `RUNNING` forever until exit?
   Tentative: stay in `RUNNING`. Users who want explicit "the shell
   is idle at a prompt" indication can use a wrapper script.

## Appendix: a Conductor plugin sketch (illustrative only)

This is *not* a spec — the Conductor plugin gets its own design doc.
Included here only to illustrate that the API surface above is
sufficient.

```ts
const sessions = new Map<SessionId, AgentSession>();
const collisions = new Map<string, Set<SessionId>>();

editor.registerCommand("conductor.new", async () => {
  const branch = await editor.startPrompt("Branch");
  const cmd    = await editor.startPrompt("Agent command");
  const wt     = await git.worktreeAdd(branch);
  const id     = await editor.createSession({ root: wt.path, label: branch });
  const term   = await editor.createTerminal({ sessionId: id, cwd: wt.path });
  editor.sendTerminalInput(term.terminalId, cmd + "\n");
  await editor.watchPath(wt.path, { recursive: true, sessionId: id });
  sessions.set(id, { id, branch, terminal: term, state: "running" });
  rerenderControlRoom();
});

editor.registerCommand("conductor.dive", () => {
  editor.setActiveSession(selectedSessionId);
  // file tree, LSP, quick-open, splits all retarget. Conductor state untouched.
});

editor.on("terminal_output", e => stateMachine.observe(e));
editor.on("terminal_exit",   e => stateMachine.observe(e));
editor.on("path_changed",    e => collisionMatrix.observe(e));
editor.on("active_session_changed", () => rerenderControlRoom());
```

The plugin's `sessions` map and `collisions` map live in the plugin
module's top-level scope, which under this design is editor-global
and is not affected by `setActiveSession`. That is the property the
PRD asks for.
