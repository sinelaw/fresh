# Terminology

> Status: **active convention.** This is the source of truth for what we
> call things in user-facing docs, UI strings, and (where non-breaking)
> code. The word "session" is being retired because it meant nine
> different things; see the history at the bottom.

## The scheme: three user-facing words, one internal

| Word | Layer | Means |
|---|---|---|
| **daemon** | user-facing | The background process that outlives a client connection — you **attach** / **detach** / **reattach** to it (tmux-like). One daemon hosts many workspaces. |
| **workspace** | user-facing | One project/root the editor manages: its files, layout, terminals, buffers, trust level, and saved state all belong to it. The thing the Orchestrator lists and switches between. |
| **backend** | user-facing | *Where* a workspace runs: **local**, **SSH host**, **dev container**, or **k8s environment**. |
| **`Authority`** | internal only | The code implementation of a backend (routes filesystem, process spawning, terminal, trust, env). Not exposed to users. |

**One sentence:** *A daemon hosts workspaces; each workspace runs on a backend.*

## Backends

- **local** — the machine the daemon runs on.
- **SSH host** — a remote machine over SSH.
- **dev container** — a devcontainer.
- **k8s environment** — a durable Kubernetes dev environment (persistent
  storage that survives across pods). Say **"k8s environment"** for the
  durable identity you reconnect to; say **"pod"** only for the live
  compute instance, since the pod is recreated on resume while the
  environment persists.

## Rename rules

### Do rename (non-breaking)
- **User docs** (`docs/features/*.md`, `docs/getting-started/`, etc.):
  "session" → **daemon** (persistence) or **workspace** (the editor unit),
  per meaning. The persistent-process feature is "daemon mode."
- **CLI help / about / example text**: lead with **daemon**. Add a
  `daemon` **alias** for the `session` subcommand; keep `session` working.
- **UI / status-bar display strings** and the **English locale**
  (`locales/en.json`) values.
- **k8s plugin user-facing labels**: "workspace" → **"environment"** /
  **"k8s environment"** (display text and command titles).
- **Code comments** that explain these concepts.

### The config-layer "Session" (a separate concept)

There is a tenth use of "session" that is **not** the daemon and **not** a
workspace: the **config layer**. Settings merge across layers
System → User → Project → **Session**, where the Session layer is
temporary per-run overrides stored in `.fresh/session.json` (surfaced in
the Settings UI as the `[ Session ]` layer button and a `(session)`
indicator). A good future name is the **Runtime** (or **Temporary**) layer.
The rename is **deferred** because it is coupled to the Settings UI label
and the on-disk `.fresh/session.json` filename (breaking). Docs keep the
"Session" layer name for now but add a note distinguishing it from the
daemon/workspace.

### Do NOT rename (would break things — out of scope)
- **Plugin API** surface: `createWindow`, `listWindows`, `setWindowState`,
  `attachRemoteAgent`, `window_created`/`window_closed`/`active_window_changed`
  events, `setSessionState`/`getSessionState`. (Public contract.)
- **Plugin-facing context keys** (e.g. the `SESSION_MODE` key string).
- **On-disk paths / serialized field names** (`workspaces/`,
  `session-workspaces/`, workspace-JSON field names). Renaming orphans
  users' saved state without a migration.
- **External tool CLIs** — `claude --session-id` / `--resume` etc. are not
  ours; docs keep "session" when describing agent resume.
- **Locale keys** (rename values, not keys) and **non-English locale
  values** (need translators).
- The internal `Window`/`WindowId` type names stay for now (they are the
  code name for a workspace; renaming is a large breaking churn deferred
  to a later pass).

## Concept history (why "session" was retired)

"session" previously meant all of: the daemon; the editor unit (`Window`);
a terminal/PTY; hot-exit/recovery state; a backend/authority; agent resume
(`--session-id`); the saved-state file; the `session_mode` daemon flag; and
plugin per-unit state. "workspace" separately meant: the saved-state file;
`WorkspaceTrust`; the k8s pod env; devcontainer `workspaceFolder`; the SSH
remote root. This document fixes one word per concept.
