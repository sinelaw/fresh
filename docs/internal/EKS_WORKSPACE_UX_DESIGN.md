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
| 2 | **Lifecycle ownership** | Pure-attach (Fresh only connects; user runs Terraform themselves — minimal, but "DIY then attach" is a poor daily UX) · **Hybrid (Fresh tracks state & drives verbs, the `Provider` executes — rich UX, must guard against state drift)** · Full-manage (Fresh owns provisioning opinions — easiest start, least flexible). |
| 3 | **Cold-start strategy** | Provision-on-connect (cheapest, slowest — minutes) · **Stop/resume as headline (keep volume, release compute — cheap *and* ~fast; the VDI-style model teams expect)** · Warm pool (instant, idle cost — offer via provider for teams who want it). |
| 4 | **Primary surface** | Command palette only (discoverable-ish, no overview) · **A "Remote/Workspaces" panel as home base + palette commands + a `fresh eks://…`-style CLI form mirroring `fresh user@host:path`** · Status-bar menu only (too small for management). |
| 5 | **How much k8s/AWS is shown** | Hide everything (magical until it breaks, then opaque) · Show the plumbing (powerful, intimidating) · **Progressive disclosure (workspace verbs up front; "Show details / logs / pod" one click away)**. |
| 6 | **Provisioning config** | Repo `.fresh/eks.json` only · User-global only · **Layered: zero-config attach → repo config (shareable) → user-global, and reuse `devcontainer.json` where present** (don't reinvent environment definition). |
| 7 | **Connections per window** | Multi-root (powerful, breaks the modal Authority principle, huge complexity) · **One window = one workspace (modal, matches Authority; the panel manages many; second workspace = second window)**. |
| 8 | **Idle / cost default** | Off (simplest, surprise bills) · Conservative long timeout · **On by default, sane timeout, visible countdown, one-click "keep awake"** (protective without being patronizing). |
| 9 | **Failure stance** | Always-ask (safe, naggy) · Auto-everything (smooth, scary for destructive ops) · **Auto-recover the transient (reconnect, re-resolve pod), always-ask the destructive (rebuild/destroy/resize)**. |
| 10 | **Trust & spend prompts** | Per-connect (naggy) · Off (unsafe) · **Trust a cluster once (remembered), confirm spend once per workspace** (matches WorkspaceTrust + devcontainer's remembered-decision pattern). |
| 11 | **Persistent vs. ephemeral workspaces** | Force one model · **Make it a per-workspace policy: persistent volume + stop/resume = "VDI-style" long-lived; destroy-on-disconnect = throwaway-per-branch** — same primitives, a config flag. |

## Surfaces (where the flows live)

- **Workspaces panel** (home base): list, state dots, rough cost, idle
  timer; verbs as buttons; "Show logs/pod/details" for disclosure.
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

## Biggest open forks for product sign-off

These three shape the most work and want an explicit decision:

1. **Workspace abstraction (row 1) + lifecycle ownership (row 2).** Going
   "Workspace + hybrid" is the ambitious, good UX but it means Fresh
   tracks state and risks drift (Fresh thinks `stopped`, reality is
   `running` and billing). The cheap alternative is "pure attach," which
   is a much weaker product. *How much lifecycle do we own?*
2. **Stop/resume as the headline model (row 3).** Worth building the
   suspend/resume machinery (and depending on providers to support it)
   vs. shipping provision-on-connect + destroy only for v1?
3. **The Workspaces panel (row 4).** A real management surface is
   significant UI work; v1 could be palette-only. Does the panel land in
   v1 or v2?
