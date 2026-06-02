# Cloud Workspaces — the feature, from the user's side

Status: design / product framing. This is the **top of the stack**. It
defines what the cloud-editing feature *is* for a user and the journeys
it must support. The two docs below it are implementation detail:

- [`EKS_S3_AUTHORITY_DESIGN.md`](EKS_S3_AUTHORITY_DESIGN.md) — *how* the
  editor attaches (SSH remote-agent stack over a `kubectl exec`
  transport). Mechanics.
- [`EKS_WORKSPACE_PLUGIN_DESIGN.md`](EKS_WORKSPACE_PLUGIN_DESIGN.md) —
  *how* pods come into being (the `Provider` contract; Terraform/manifest
  escape hatches; storage = EBS-live + S3-sync). Plumbing.

Storage, transport, and provisioning are settled enough. **This doc is
about the experience**, because that is what determines whether anyone
uses the feature.

## The one-sentence promise

> Open Fresh, pick a workspace, and you're editing in your own cloud — it
> feels local, it survives interruptions, and it never bills you by
> surprise.

Everything below serves that sentence.

## The mental model: a *Workspace*, not a pod

The single most important UX decision. The user must think in terms of a
durable, named **Workspace** they own — *not* a Kubernetes pod, not a
"connection." A Workspace is:

- **Durable & named** — `acme-api`, `my-scratch`. Its identity is its
  storage + its environment definition, *not* whatever pod is currently
  running it.
- **Stateful** — it is in one of: `not-provisioned` · `starting` ·
  `running` · `connected` · `stopped` (suspended, storage kept, compute
  released) · `error`.
- **Pod-agnostic** — pods come and go (Spot reclaim, rebuild, resize);
  "my workspace" persists across all of it. The pod is never named in
  the primary UI.

This abstraction is what lets reconnect-after-reschedule feel like
nothing happened, and what lets "stop to save money, resume tomorrow"
make sense. Everything the user does is a verb on a Workspace: *connect,
disconnect, stop, resume, rebuild, resize, destroy.*

```
not-provisioned ──connect──► starting ──► running ──attach──► connected
      ▲                          │            ▲  │                │
      │                       (fail)          │  │ idle/explicit  │ disconnect
   destroy                       ▼            │  ▼                ▼
      └──────────────────────  error      stopped ◄──────────  running
                                             │  (compute released, storage kept)
                                          resume│
                                             └──► starting …
```

## Orchestrator integration — workspace = session

**This is the structural keystone.** Workspace management is not a new
subsystem bolted onto Fresh; it *is* the Orchestrator
([`orchestrator-sessions-design.md`](orchestrator-sessions-design.md))
with one additive facet. The fit is near-exact:

- A **`Session`** already bundles "everything rooted at one project root"
  — its own file explorer, LSP, quick-open scope, buffers, splits,
  terminals — and switching sessions **retargets the entire editor state
  atomically** while the Orchestrator stays anchored above the swap.
- A **Cloud Workspace is just a `Session` whose `Authority` is the EKS
  remote-agent authority** instead of local. The durable "Workspace"
  identity (§mental model) is the durable "Session" identity. Connect =
  activate that session; its session-swap *is* the destructive authority
  transition (same machinery — drop & rebuild editor state around the new
  backend). The two concepts were the same thing all along.
- The **Orchestrator session list / Control Room is the Workspaces
  panel.** Multi-workspace switching (F7) and cost hygiene (F8) are the
  session list with two extra columns. No separate surface.

### How it stays seamless for pod users *and* invisible to everyone else

The hard requirement: great for cloud users, **zero cost for local-only
users.** The mechanism is that the Orchestrator stays *backend-opaque*
(Authority principle 3 — core never names "eks"):

- A session optionally exposes a **generic "remote facet"**: a state
  (`starting/running/stopped/error`), an optional cost/idle hint, and a
  set of lifecycle actions (`stop/resume/rebuild/resize`). The
  Orchestrator renders whatever a session provides and **nothing when a
  session provides nothing.**
- A **local session provides no facet** → the session row looks and
  behaves exactly as today. No new columns, no new verbs, no new
  concepts, no perf cost. Someone who never touches EKS sees the
  Orchestrator they already have.
- All cloud-specific logic (provisioning, stop/resume, state reconcile)
  lives in the **plugin + authority**, never in the Orchestrator. The
  Orchestrator gains a generic facet interface, not EKS knowledge.

So the blast radius on the non-cloud experience is: one optional,
empty-by-default field on a session row. That is the whole answer to
"without hurting people who don't use pods."

### Bonus: it composes with the Orchestrator's original purpose

The Orchestrator exists to run **parallel AI agents, each in its own
worktree**. Cloud Workspaces compose with that directly: run those agents
in cloud pods (ephemeral, scoped creds, scale-to-zero) and the Control
Room's existing live-preview / parsed-state / collision-radar machinery
covers them with no new UI. Cloud + agents is a multiplier, not a
parallel track.

## End goals (what "done" means)

1. **Local-grade editing, remotely.** LSP, multi-cursor, search,
   integrated terminal, git, and large-file handling all work against the
   remote workspace with latency hidden. The user does not feel exiled.
2. **Two-step connect.** "I want to work" → one command + one pick →
   editing. Cold starts show honest, watchable progress; warm/resumed
   workspaces are near-instant.
3. **Continuous identity.** A Workspace survives pod churn, network
   drops, Spot reclaims, and laptop sleep. Reconnect is transparent;
   unsaved-to-disk work is the only thing at risk, and even that is
   minimized.
4. **No surprise bills.** What's running, roughly what it costs, and the
   idle countdown are always visible. Auto-stop on idle is on by default;
   teardown is one click and trustworthy.
5. **Bring-your-own-everything; adapt to any flow.** The user's AWS,
   the user's cluster. Provisioning is pluggable: zero-config for
   beginners, a `command`/Terraform escape hatch for platform teams.
6. **Reproducible & shareable.** A Workspace's environment is codified
   (devcontainer / manifest / template) and travels in the repo, so a
   teammate opens the project and connects to the same thing.
7. **Secure & least-privilege by default**, without nagging: trust a
   cluster once, confirm spend once, scoped creds, session TTLs.
8. **Graceful failure.** Every failure has a specific message and a
   one-click next action. Never a frozen screen, never a silent
   half-attach, never a mystery.

## The full set of user flows we must support

### F1 — Onboarding (zero → first successful connect)
The make-or-break flow. Branch by starting point, easiest first:
- **Already have a running dev pod** → `attach-existing`: pick context →
  namespace → pod → connected. No provisioning, no config.
- **Have a cluster, no workspace** → pick/define a template → provision →
  connect (this is F2's cold path).
- **Have AWS, no cluster** → out of scope to *create* clusters; detect
  the gap and point to a starter (docs + a reference Terraform module).
  Be honest about the boundary rather than half-doing cluster creation.
- Preflight gates throughout (kubectl/aws present, context reachable,
  RBAC `create` on `pods/exec`, not-on-Fargate) with fix-it messages.

### F2 — Connect / resume (the daily driver)
Pick a Workspace → Fresh reconciles to `connected`:
- `running` → attach immediately.
- `stopped` → resume (start compute, re-mount storage), then attach.
- `not-provisioned` → confirm spend → provision → attach.
Progress streams to a log view; status bar shows the phase.

### F3 — Steady-state work (the illusion holds)
Editing, **integrated terminal in the pod**, **LSP in the pod**,
run/build/test, git, file explorer — all routed through the authority.
Plus the things that *break the illusion* if we ignore them:
- **Port-forwarding** — preview a dev server running in the pod from the
  local browser (`kubectl port-forward`), surfaced as a first-class
  "Forward a port" action, auto-detecting listening ports.
- **Clipboard** across the boundary (terminal copy/paste).
- **Secrets/env** — the workspace's env (from the pod) vs. local.

### F4 — Leave (disconnect / stop / destroy)
Three distinct, clearly-labeled exits — conflating them is a classic
footgun:
- **Disconnect (keep running)** — detach this window; pod keeps running.
- **Stop (suspend)** — release compute, keep storage. Cheap. The default
  "I'm done for the day" action.
- **Destroy** — delete everything. Confirm hard.
Plus implicit exits: **idle auto-stop** (countdown shown), closing the
window, laptop sleep.

### F5 — Reconnect after interruption (transparent)
- **Network blip** → heartbeat keeps the stream; if it drops, silent
  reconnect, brief "reconnecting…" banner.
- **Pod reschedule (Spot/eviction)** → re-resolve the new pod, recover
  from the persistent volume, banner "workspace moved, reconnected."
- **Laptop sleep/wake** → resume on wake.
- **Stopped while away** → on return, offer "Resume workspace?".

### F6 — Rebuild / reconfigure / resize
- **Rebuild** — image or devcontainer changed: replace the pod, keep the
  data volume, re-attach.
- **Resize** — need more RAM/CPU: reschedule onto a bigger node, keep the
  volume. A special-case rebuild.
Both are destructive to *compute*, not *data* — say so in the prompt.

### F7 — Many workspaces & switching
A **Workspaces panel** lists all of them across clusters with state +
rough cost + idle timer. Switch = disconnect current + connect chosen
(authority is modal: one window, one workspace). Open a second workspace
= new window.

### F8 — Hygiene / cost cleanup
"What do I have running?" view; bulk stop/destroy; **orphan detection**
("a pod from this workspace has been running 3 days — stop it?").

### F9 — Team distribution
Platform eng commits a template/provider config to the repo (or org
config). A teammate opens the repo → Fresh detects it (devcontainer-
style) → "This project defines a cloud workspace. Connect?" → remembered
per project. Zero ceremony for the consumer.

### F10 — Failure & recovery (each with a named action)
Creds expired → re-auth. Quota hit → message + which quota. Image pull
fail → show the pull error. Provision timeout → keep logs, offer retry.
Pod evicted-and-gone → "ended; Rebuild?". Unschedulable (AZ/Spot) → "no
capacity; try On-Demand / another AZ?".

### F11 — Agent / automation (adjacent, keep the door open)
An AI agent or CI spins up an ephemeral workspace, works, tears down. The
same verbs exposed programmatically (the plugin's `Provider` + the CLI
form). Not a v1 UI, but the primitives shouldn't preclude it.

## UX alternatives & trade-offs (the real forks)

Each row is a decision; the **bold** option is my recommendation.

| # | Decision | Options & trade-offs |
|---|---|---|
| 1 | **Unit of interaction** | **Workspace** (durable, hides pods; more to build but the only model that makes resume/reconnect/cost coherent) · Pod (k8s-native, leaky, confusing churn) · Session (transient, loses "my durable thing"). |
| 2 | **Lifecycle ownership** | Pure-attach (Fresh only connects; user runs Terraform themselves — minimal, but "DIY then attach" is a poor daily UX) · Hybrid (Fresh tracks state & drives verbs, the `Provider` executes) · **Full-manage [DECIDED] — Fresh owns an opinionated, zero-config provisioning engine end to end; the `command`/Terraform provider is the deliberate escape hatch, not the default.** |
| 3 | **Cold-start strategy** | Provision-on-connect (cheapest, slowest — minutes) · **Stop/resume as headline (keep volume, release compute — cheap *and* ~fast; the VDI-style model teams expect)** · Warm pool (instant, idle cost — offer via provider for teams who want it). |
| 4 | **Primary surface** | Command palette only (discoverable-ish, no overview) · **A "Remote/Workspaces" panel as home base + palette commands + a `fresh eks://…`-style CLI form mirroring `fresh user@host:path`** · Status-bar menu only (too small for management). |
| 5 | **How much k8s/AWS is shown** | Hide everything (magical until it breaks, then opaque) · Show the plumbing (powerful, intimidating) · **Progressive disclosure (workspace verbs up front; "Show details / logs / pod" one click away)**. |
| 6 | **Provisioning config** | Repo `.fresh/eks.json` only · User-global only · **Layered: zero-config attach → repo config (shareable) → user-global, and reuse `devcontainer.json` where present** (don't reinvent environment definition). |
| 7 | **Connections per window** | Multi-root in one session (breaks the modal Authority principle, huge complexity) · **One session = one workspace/authority; the Orchestrator holds many sessions and the active one is connected — switching sessions retargets the authority atomically (existing session-swap machinery)**. |
| 8 | **Idle / cost default** | Off (simplest, surprise bills) · Conservative long timeout · **On by default, sane timeout, visible countdown, one-click "keep awake"** (protective without being patronizing). |
| 9 | **Failure stance** | Always-ask (safe, naggy) · Auto-everything (smooth, scary for destructive ops) · **Auto-recover the transient (reconnect, re-resolve pod), always-ask the destructive (rebuild/destroy/resize)**. |
| 10 | **Trust & spend prompts** | Per-connect (naggy) · Off (unsafe) · **Trust a cluster once (remembered), confirm spend once per workspace** (matches WorkspaceTrust + devcontainer's remembered-decision pattern). |
| 11 | **Persistent vs. ephemeral workspaces** | Force one model · **Make it a per-workspace policy: persistent volume + stop/resume = "VDI-style" long-lived; destroy-on-disconnect = throwaway-per-branch** — same primitives, a config flag. |

## Surfaces (where the flows live)

- **Orchestrator session list / Control Room** (home base — *not* a new
  panel): the existing session list, with cloud sessions carrying the
  optional remote facet (state dot, rough cost, idle timer) and lifecycle
  verbs; "Show logs/pod/details" for disclosure. Local sessions render
  unchanged. See §"Orchestrator integration".
- **Command palette**: every verb (`EKS: Connect`, `Stop`, `Resume`,
  `Rebuild`, `Forward Port`, `Disconnect`, `Destroy`, `Show Workspaces`).
- **Status bar**: compact `● acme-api · running · ~$0.40/hr · idle 12m`;
  click → panel. Color = state. Mirrors today's SSH/devcontainer status.
- **CLI**: `fresh eks://context/namespace/workspace` (and bare `fresh`
  picking up a repo's `.fresh/eks.json`), paralleling `fresh user@host:path`.
- **Notifications/banners**: reconnecting, moved, resumed, idle-stopping
  in N min, provision failed.

## What this is not

- Not a Kubernetes dashboard or a cluster creator.
- Not multi-root / multi-workspace-per-window.
- Not a hosted control plane — all state lives in the user's cluster +
  Fresh's local per-workspace memory; Fresh stores no secrets.
- Not a sync product — storage durability is the pod volume's job
  (authority doc).

## Decisions taken (v1 sign-off)

Three forks are now settled. They define v1's scope and shape.

### D1 — Full-manage, opinionated by default (with an escape hatch)

Fresh owns the **whole lifecycle end to end** with a batteries-included
default: a solo developer with nothing but an AWS account + a cluster
gets a working workspace with **zero config**. Fresh's built-in
provisioning knows how to create the EBS-live/S3-sync pod, apply
Karpenter/Spot-friendly + Pod-Identity-scoped specs, and run the full
`stop / resume / rebuild / resize / destroy / idle-stop` state machine.

This is *not* a reversal of "bring-your-own-flow." The
`command`/Terraform `Provider` remains — it is now the **deliberate
override** for platform teams (point at your Terraform repo and Fresh
drives it), not the thing every user must configure first. Default path:
Fresh just does it. Power path: hand Fresh your flow.

**Consequence — Fresh ships an opinionated provisioning engine, not just
provider plumbing.** The plugin doc's "built-in providers" become
real, Fresh-owned default templates + lifecycle logic. See
[`EKS_WORKSPACE_PLUGIN_DESIGN.md`](EKS_WORKSPACE_PLUGIN_DESIGN.md).

**Consequence — state is authoritative but can still drift.** Because
Fresh provisions, it knows the intended state; but a user `kubectl
delete` or an out-of-band Spot reclaim can diverge it. v1 must
**reconcile on connect** (query real pod/volume state before acting) and
expose a "refresh state" action, so Fresh never bills against a phantom
`stopped` or attaches to a phantom `running`.

### D2 — Stop/resume is the headline lifecycle model

Keep the data volume, release compute on stop/idle, resume fast. This is
the "VDI-style" model and the cheap-but-quick default. Persistent vs.
ephemeral becomes a per-workspace policy flag (row 11): persistent =
stop/resume; ephemeral = destroy-on-disconnect. Build the suspend/resume
machinery in v1.

### D5 — Authority becomes per-session (not per-process)

A direct consequence of D4. Warm background sessions each hold a live
backend, so the `Authority` (its filesystem, spawners, keepalive) must be
**owned per `Session`/`Window`, with exactly one *active*** — not one per
process as today. The active session's authority is still the sole
router; background ones are dormant-but-connected. Switching sessions
*activates* an authority instead of restarting the process, and
`install_authority` retargets the active session, not the whole `Editor`.

This continues the Orchestrator migration (which already moved buffers,
LSP, terminals, explorer onto `Window`); authority is the remaining
per-project field. It also pulls `WorkspaceTrust`, `EnvProvider`, and the
daemon's single `session_keepalive`/`startup_authority` slots toward
per-session ownership. Full write-up: `AUTHORITY_DESIGN.md`
§"Evolution: per-session authority".

### D4 — Background cloud sessions stay warm

When a cloud session is not the active one in the Orchestrator, its
`kubectl exec` channel **stays connected** (the `RemoteKeepalive` bundle
— agent, reconnect task, runtime — is held per session). Switching back
is instant; no reconnect, no resume.

Consequences:
- **The agent heartbeat runs for *every* warm session, not just the
  active one** — otherwise a backgrounded session's idle `exec` stream
  gets dropped by ELB/NAT timeouts and "instant switch-back" becomes
  "switch back, then wait for a reconnect." The heartbeat is what makes
  warm actually warm.
- **Warm ≠ exempt from idle auto-stop.** "Stays warm" governs the
  *connection* while the workspace is `running`; the workspace-lifecycle
  idle timer (D2) still measures per-session inactivity and can `stop`
  the pod (closing its channel). A backgrounded session that goes idle
  long enough suspends; switching back to it then resumes. Warm keeps
  switching snappy; idle-stop keeps the bill honest. They don't fight —
  warm is about the live connection, idle-stop is about the pod.
- **Resource ceiling.** Each warm session holds a host-side `kubectl`
  subprocess + an in-pod agent + a tokio task. Fine for a handful; with
  many sessions it accumulates. Keep all warm by default (the decision),
  but cap the warm set at a configurable max (suspend the
  least-recently-active beyond it) as a safety valve.

### D3 — Management lives *in the Orchestrator*, not a separate panel (v1)

The original D3 ("ship a Workspaces panel") is **superseded**: a cloud
workspace is a [`Session`](orchestrator-sessions-design.md), and the
Orchestrator's session list / Control Room *is* the management surface.
There is no second panel. See §"Orchestrator integration" — this is the
load-bearing structural decision, so it gets its own section.

The home-base requirements still hold (list, state dots, rough cost, idle
countdown, verbs, progressive "show logs/pod/details") — they are met by
**extending the existing Orchestrator session row with an optional remote
facet**, not by building new chrome.

## Still open

- **Cost display fidelity (row 8 detail).** Rough `~$/hr` needs an
  instance→price source; can Fresh estimate it itself from the node type,
  or does it require a provider hook? Decide before promising a number.
- **Cluster-creation boundary (F1).** Confirmed out of scope to *create*
  clusters — but how guided is the "you have AWS, no cluster" dead-end?
  Reference Terraform + docs link, or something warmer?
- **Warm-set cap default (D4 detail).** "Stay warm" is decided; the only
  remaining knob is the max number of simultaneously warm sessions before
  the least-recently-active suspend. Pick a sane default (e.g. 5-8).
