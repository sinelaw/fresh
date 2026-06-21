# Per-session backends, trust, and env — final design

> Status: **design target**, partially landed. Realizes the
> [`AUTHORITY_DESIGN.md`](AUTHORITY_DESIGN.md) §"Evolution: per-session
> authority" direction and closes the gaps tracked in issue #2280.
> Keep this short; deep mechanics belong in `AUTHORITY_DESIGN.md` /
> `K8S_AUTHORITY_DESIGN.md`.

## Problem

A session (project / window) should fully own *where it runs* (local /
container / SSH / Kubernetes), *whether it's trusted*, and *which dev
environment it has activated*. Today the live `Authority`, `WorkspaceTrust`,
and `EnvProvider` are effectively process-wide: one is fanned across every
window at boot/restart. Visible consequences (issue #2280): remote sessions
come back **local** after a restart/relaunch, and trusting/activating one
project bleeds into others.

Already landed: an installed backend no longer leaks onto *other* windows
when you switch (each window owns `resources.authority`; background windows
are built local instead of inheriting the active backend). The rest of this
doc is the remaining design.

## Core model: a session owns a *Session Profile*

Give every session a small, declarative **`SessionProfile`** — the data
needed to *rebuild* its world — alongside its live handles:

```
SessionProfile {
    backend: BackendSpec,   // Local | Plugin(AuthorityPayload) | RemoteAgent(RemoteAgentSpec)
    trust:   TrustDecision, // this session's level (+ key into a shared registry)
    env:     EnvSpec,       // activated venv/direnv/mise recipe, or none
}
```

`BackendSpec` reuses the existing `AuthorityPayload` / `RemoteAgentSpec`
verbatim, so there is no new backend vocabulary and core stays
backend-opaque (`AUTHORITY_DESIGN.md` principle 3). The profile is set
wherever a backend/trust/env is installed and is the *source of truth* for
restoration; the live `Authority` is derived from it.

### Restoring agent terminals (as-built: agent-resume)

Bringing a session's backend back is not enough for an **agent** session
(`claude`, `aider`, …): its seed terminal ran a process that is gone, and
re-opening a bare shell loses the agent. This is the *what to re-run* half of
restore, and it is **already shipped** — see
[`agent-resume-design.md`](agent-resume-design.md). In brief, each terminal
persists two argvs in its workspace entry:

- `command` — the **launch** argv (the agent / shell the PTY was spawned
  with), and
- `agent_resume.argv` — *how to rejoin* the conversation, distinct from
  launch and provided by the Orchestrator's agent registry (e.g. launch
  `claude --session-id <uuid>`, resume `claude --resume <uuid>`).

On restore, `restore_terminal_from_workspace` runs `agent_resume → command →
shell`, gated by `terminal.resume_agents`. Detection and the per-agent flags
live in TS data (the registry), not core — the same mechanism/policy split
this design uses for backends.

So a session's restore data has **two complementary halves**: the
`SessionProfile` (this doc — *where* the session runs) and the per-terminal
agent-resume (*what* to re-run in it). They are persisted independently in the
same workspace file and **compose** at restore. Two constraints make that
composition correct — and both are *new work this design owns*, because the
agent-resume feature landed assuming a local backend:

1. **The agent runs *inside* the session's backend, not beside it.** Today a
   command terminal builds a `TerminalWrapper` that **replaces** the
   authority's wrapper, so an agent argv runs on the **host** even under a
   container / SSH / k8s authority. The fix is to make the authority own
   *"run this interactive argv in my backend"* —
   `Authority::terminal_command(argv)` returning `docker exec … <argv>` /
   `kubectl exec … <argv>` / `ssh … -- <argv>` (bare `argv` for local). Launch
   **and** resume argvs go through it, so they compose with the backend
   instead of bypassing it. (This also fixes a born-attached remote agent's
   *seed* terminal, which has the same bypass today.)

   > **Status: shipped for local, containers, SSH, and Kubernetes.** Local
   > runs the argv directly; containers prepend `docker exec -it [-u][-w][-e
   > env] <id>`, the argv staying intact (no shell string, cwd pinned via
   > `-w`). `kubectl exec` and `ssh` have **no cwd flag**, so they pin cwd with
   > a shell hop (`… 'cd <dir>; exec ${SHELL:-/bin/sh} -lc <argv>'`): the argv
   > is POSIX-quoted and handed to a remote **login** shell so its `PATH`
   > resolves the agent binary, exactly as the bare remote terminal does. This
   > trades away argv-slot purity for ssh/kube (the quoting carries the argv
   > intact instead), the deliberate decision flagged here. Before this landed,
   > ssh/kube command terminals ran the agent on the **local** host, ignoring
   > the session's remote path — see `CommandWrap` in `services/authority`.

2. **Backend first, then agent.** A restored remote session is **Dormant**
   (local placeholder) until reconnect, so its agent must **not** re-run on
   the placeholder. Restore order is: local sessions re-run the agent
   immediately (unchanged); remote sessions defer the agent re-run to the
   reconnect-on-activate step and then run it via
   `authority.terminal_command(resume_argv)` in the now-live backend.

## Lifecycle: Live vs Dormant

Each session's authority is in one of two states:

- **Live** — connection established (local always; remote after a successful
  connect). Routes every primitive.
- **Dormant** — profile known, not connected. The window runs on a **local
  placeholder** authority (instantly usable, never holds a dead remote
  handle) but is *presented as its real backend, disconnected* — reusing the
  existing `RemoteIndicatorState::Disconnected`/`Connecting` facet. Reads /
  terminals that require the real backend are gated until it activates.

> Only one authority is the active router at a time (principle 2 intact);
> background **live** sessions keep their connection warm via their own
> `session_keepalive`, exactly as today.

## Persistence

The profile round-trips through the **per-dir workspace file** (the session
registry — there is no central `windows.json` for sessions anymore). Saved
on the same paths that already persist a session (`save_all_windows_workspaces`,
pre-restart, pre-quit); read back by session discovery at construction. A
missing profile reads as `Local` (back-compat).

## Restore

Construction (cold launch **and** the `install_authority` restart, which
both rebuild from disk) builds each session's authority **from its profile**:

- `Local` → local authority.
- Remote / container → **Dormant** (placeholder + retained profile).
- The **active** session, if remote, is queued to reconnect immediately
  (surface `Connecting → Connected / FailedAttach`); background sessions stay
  dormant until used.

This replaces today's "fan one authority onto every window."

## Reconnect (on switch or explicit)

Activating a dormant remote session reconnects **that session only** — the
per-window activation `AUTHORITY_DESIGN.md` calls for:

- SSH / Kubernetes → reuse `connect_ssh_authority` / `connect_kube_authority`
  (async, via the existing `RemoteAttachReady` bridge), then
  `set_session_authority(id, authority)` and park the keepalive in
  `session_keepalives[id]`.
- Container → core can't run `devcontainer up`; fire a
  `session_reattach_requested { window_id, profile }` hook so the
  devcontainer plugin re-attaches. Core stays opaque.

Once the backend is live again, the session's **agent terminals re-run in
it** — each terminal's `agent_resume → command` argv is run through
`authority.terminal_command(argv)` (the composition seam above), so the agent
rejoins inside its real backend rather than on the host. This is the
"backend first, then agent" order from *Restoring agent terminals*.

Reconnect is **trust-gated** (below). A dead container/pod surfaces
`FailedAttach`, not a crash.

## Per-session trust and env

`WorkspaceTrust` and `EnvProvider` move from one shared handle to one **per
session**, each window's authority carrying its own:

- **Trust — shipped.** Each session owns a `WorkspaceTrust` scoped to its root
  (`WorkspaceTrust::for_session`), so a trust decision in one project never
  changes the live level another open session's spawns are gated against. The
  "remember this folder" registry is the existing per-project on-disk
  `TrustStore` (each session adopts its project's recorded level on
  construction). Every per-session-authority construction point —
  `local_session_authority`, `create_window_at`, the `set_boot_authority`
  background fan-out, and the constructor's restored shells — builds a trust
  for *that window's* root; the active session's trust is what drives the pill
  and gating. (Auto-prompting on *switch* is deliberately not wired: the dock
  live-switches windows on ↑/↓, so a modal there would spam — the boot prompt
  + the per-session restricted pill cover it; per-session prompt-on-dive is a
  separate UX decision.) Auto-reconnect on restore should consult the
  session's trust (don't silently re-establish a remote backend for an
  untrusted folder).
  A new session for an *undecided benign* folder (no executable-content
  markers) defaults Trusted, matching the boot session's prompt default, so
  it doesn't silently block its own LSP/tooling.
- **Env — shipped.** Each session owns its own `EnvProvider` (fresh + inactive
  per session), so activating a venv/direnv/mise in one project never
  activates it for another open session.

### Isolation is enforced by construction, not a runtime check

`Authority` is **not `Clone`** and is **owned by exactly one `Window`** — there
is no editor-wide copy (`Editor::authority()` returns
`active_window().authority`). Trust + env are minted as a **move-only
`SessionScope`** (`SessionScope::for_root`) and consumed into the authority.
So a session's backend/trust/env *cannot* be shared into another window: the
type system rejects it at construction, rather than a runtime assertion
catching it after the fact. Authorities are **moved** between slots (restore,
`set_session_authority`, `set_boot_authority`), never copied.

Switching sessions therefore never changes another session's backend, trust,
or env.

## Invariants

1. One **active** authority routes everything; background sessions are live
   (warm) or dormant, routing nothing.
2. Core never names a backend — profiles carry opaque payloads; the
   Orchestrator renders the "remote facet" generically.
3. The live `Authority` is always derivable from the `SessionProfile`; the
   profile, not the live handle, is what persists.

## Trade-offs

- **Reuses existing payload + connect + indicator machinery** → additive,
  back-compat. Cost: `AuthorityPayload` / `RemoteAgentSpec` now double as a
  persistence format and must stay serde-stable.
- **Connect only the active session; reconnect background lazily on switch**
  → bounds startup cost, avoids N hanging connects (matches the warm/cold
  split in `K8S_WORKSPACE_UX_DESIGN.md`). Cost: a switch into a cold remote
  session has connect latency (shown via the spinner).
- **Container restore needs the plugin** (only it runs `devcontainer up`), so
  core hands off via a hook. Cost: a small plugin contract.
- **Per-session trust** needs a trusted-host/cluster registry to stay usable;
  without it, every restored remote session re-prompts.

## Phasing (each step independently testable)

1. `SessionProfile.backend` + per-window field + workspace-file persistence;
   spec-driven **Dormant** restore (no reconnect yet). Fixes "comes back
   local" → "comes back disconnected, profile retained."
2. Reconnect-on-activate for SSH / Kubernetes; container reattach hook.
3. Per-session `WorkspaceTrust` + trusted-host registry; trust-gate reconnect.
4. Per-session `EnvProvider`.
5. Warm background remote sessions (per-session keepalives surviving restart).
