# Glossary & Naming Conventions

This is the source of truth for what we call things in user-facing docs, UI
strings, and (where non-breaking) code. It has two parts: the **naming scheme**
for daemon/workspace/backend (the word "session" was retired because it meant
nine different things), and a **core architecture vocabulary** that the
sibling docs in this directory assume.

---

## Part 1 — The daemon / workspace / backend scheme

| Word | Layer | Means |
|---|---|---|
| **daemon** | user-facing | The background process that outlives a client connection — you **attach** / **detach** / **reattach** to it (tmux-like). One daemon hosts many workspaces. |
| **workspace** | user-facing | One project/root the editor manages: its files, layout, terminals, buffers, trust level, and saved state all belong to it. The thing the Orchestrator lists and switches between. (Internal code type: `Window`/`WindowId`.) |
| **backend** | user-facing | *Where* a workspace runs: **local**, **SSH host**, **dev container**, or **k8s environment**. |
| **`Authority`** | internal only | The code implementation of a backend (routes filesystem, process spawning, terminal, trust, env). Not exposed to users. See `remote-authority-trust.md`. |

**One sentence:** *A daemon hosts workspaces; each workspace runs on a backend.*

### Backends
- **local** — the machine the daemon runs on.
- **SSH host** — a remote machine over SSH.
- **dev container** — a devcontainer.
- **k8s environment** — a durable Kubernetes dev environment (persistent
  storage that survives across pods). Say **"k8s environment"** for the
  durable identity you reconnect to; say **"pod"** only for the live compute
  instance, since the pod is recreated on resume while the environment
  persists.

### The config-layer "Session" (a separate concept)
There is a distinct use of "session" that is **not** the daemon and **not** a
workspace: the **config layer**. Settings merge across layers
System → User → Project → **Session**, where the Session layer is temporary
per-run overrides stored in `.fresh/session.json` (surfaced in the Settings UI
as the `[ Session ]` layer button). See `config-themes-settings.md`. A future
rename to **Runtime**/**Temporary** is deferred because it is coupled to the
on-disk filename and the UI label.

### Rename rules (kept for ongoing doc/UI work)
- **Do rename (non-breaking):** user docs, CLI help/about text (lead with
  **daemon**; keep a `session` alias), UI/status strings and the English
  locale values, k8s plugin labels ("workspace" → "environment"), and code
  comments explaining these concepts.
- **Do NOT rename (breaking — out of scope):** the plugin API surface
  (`createWindow`, `listWindows`, `attachRemoteAgent`, `window_*` events,
  `setSessionState`/`getSessionState`), plugin-facing context keys
  (`SESSION_MODE`), on-disk paths / serialized field names (`workspaces/`,
  `session-workspaces/`), external tool CLIs (`claude --session-id`), locale
  *keys* and non-English locale *values*, and the internal `Window`/`WindowId`
  type names.

---

## Part 2 — Core architecture vocabulary

| Term | Means | Doc |
|---|---|---|
| **`Editor`** | The "god object" — the central mutable struct holding all state. Most logic is `impl Editor` spread across ~90 `app/` modules. | `00-overview.md` |
| **`EditorState`** | Per-**buffer** shared state: the document, authoritative cursors, overlays/margins/virtual-text, highlight caches. Implements the `DocumentModel` trait. | `00-overview.md`, `text-model.md` |
| **`SplitViewState`** | Per-**split** view state: viewport/scroll, wrap mode, a render copy of cursors, plugin `view_transform`. The same buffer can show in many splits. | `buffers-splits-undo.md`, `rendering-and-layout.md` |
| **Action** | The "what the user wants" intent layer (`Save`, `MoveLeft`, `LspHover`, `PluginAction`). Produced by keybindings/menus/palette; rebindable and serializable. | `input-keybindings-actions.md` |
| **Event** | The event-sourced "what changed" layer (`Insert`, `Delete`, `BulkEdit`). Stored in a per-buffer `EventLog` for undo/redo and modified-tracking. | `text-model.md`, `input-keybindings-actions.md` |
| **`BulkEdit`** | A single `Event` that applies N edits in one tree traversal with an `Arc`-snapshot for O(1) undo — the multi-cursor O(n²)→O(n) fix. | `text-model.md` |
| **Piece tree** | The persistent, path-copying piece-table that stores document text; bytes live in append-only `StringBuffer` pools; `Arc` nodes give cheap copy-on-write snapshots. | `text-model.md` |
| **Marker** | A text-anchored interval (bookmark, diagnostic, selection, fold, highlight) held in an interval tree with insertion gravity so it survives edits. | `text-model.md` |
| **`CompositeBuffer`** | A synthesized read-only view over multiple source buffers (side-by-side / unified / stacked) — backs the diff/review viewer. | `text-model.md`, `search-and-diff.md` |
| **Scene** | The semantic projection of a frame (rows, spans, decorations) shared between the terminal renderer and the web/GUI frontends. | `rendering-and-layout.md` |
| **`Authority`** | The single backend slot per workspace deciding where filesystem/spawn/terminal operations execute (local/SSH/docker/k8s). | `remote-authority-trust.md` |
| **`PluginCommand`** | A message the plugin thread sends back to the `Editor`, applied on the main thread during `process_async_messages` (one-frame async lag). | `plugins.md` |
| **`AsyncBridge`** | The std-mpsc channel hub that delivers background results (LSP, file I/O, terminal, watch) into the per-frame `process_async_messages` drain. | `00-overview.md` |
| **Provider pattern** | Plugins/subsystems register as ordered *providers* for a capability (completion, finder rows, view transform) rather than the core hard-coding behavior. | `plugins.md`, `input-keybindings-actions.md` |
| **Orchestrator / Dock** | The UI and machinery for managing many concurrent workspaces/agent sessions. | `orchestrator-sessions.md` |
| **GPM (disambiguation)** | In the plugin marketplace, **G**it **P**ackage **M**anager (`pkg.ts`). In `services/gpm/`, the unrelated Linux-console **G**eneral **P**urpose **M**ouse FFI. | `plugins.md` |

---

## Concept history (why "session" was retired)

"session" previously meant all of: the daemon; the editor unit (`Window`); a
terminal/PTY; hot-exit/recovery state; a backend/authority; agent resume
(`--session-id`); the saved-state file; the `session_mode` daemon flag; and
plugin per-unit state. "workspace" separately meant: the saved-state file;
`WorkspaceTrust`; the k8s pod env; devcontainer `workspaceFolder`; the SSH
remote root. This scheme fixes one word per concept.
