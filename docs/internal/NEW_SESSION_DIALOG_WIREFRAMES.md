# Orchestrator "New Session" dialog — session-type selection wireframes

Status: design / product framing. Companion to
[`K8S_WORKSPACE_UX_DESIGN.md`](K8S_WORKSPACE_UX_DESIGN.md) (the
Workspace-=-Session model) and the Orchestrator plugin
(`crates/fresh-editor/plugins/orchestrator.ts`).

## Why

Today "Orchestrator: New Session" only creates a **local** session (a git
worktree or a folder, running an agent in a terminal). We now have two more
backends behind the `Authority` seam:

- **SSH** — edit on a remote host (`Authority::ssh`, `fresh user@host:path`).
- **Kubernetes** — edit inside a pod via `kubectl exec` (`Authority::kube`,
  any cluster: EKS/GKE/AKS/k3d/minikube/kind).

The New Session dialog should let the user **choose the session type** and
collect the right fields for it, while leaving the local flow exactly as it is
for people who never touch remote/cloud.

## The existing dialog (observed, debug build, tmux)

```
┌───────────────────────── ORCHESTRATOR :: New Session ──────────────────────────┐
│                                                                                 │
│ ╭─ Project Path ──────────────────────────────────────────────────────────────╮│
│ │ ~/code/acme-api                                                              ││
│ ╰───────────────────────────────────────────────────────────────────────────-─╯│
│ [v] Create a new git worktree for this session                                  │
│ ╭─ Session Name ──────────────────────────────────────────────────────────────╮│
│ │ session-3                                                                    ││
│ ╰─────────────────────────────────────────────────────────────────────────────╯│
│ ╭─ Agent Command ─────────────────────────────────────────────────────────────╮│
│ │ terminal                                                                     ││
│ ╰─────────────────────────────────────────────────────────────────────────────╯│
│ ╭─ Branch ────────────────────────────────────────────────────────────────────╮│
│ │ origin/main                                                                  ││
│ ╰─────────────────────────────────────────────────────────────────────────────╯│
│                                                       [ Cancel ]  [ Create Session ]│
│   Tab next/accept  S-Tab prev  ↑↓ suggest/history  Space toggle  Enter act  Esc  │
└─────────────────────────────────────────────────────────────────────────────────┘
```

Chrome to reuse: centered `ORCHESTRATOR :: New Session` title, `labeledSection`
bordered inputs, the worktree `toggle`, inline completion popups (Project Path,
Branch), the bottom-right `Cancel` / `Create Session` buttons, the footer hint
bar. All four backends share **Session Name** + **Agent Command**.

Field needs per type:

| Type       | Type-specific fields                                                   |
|------------|------------------------------------------------------------------------|
| Local      | Project Path · [worktree toggle] · Branch (when worktree on)           |
| SSH        | Host `user@host[:port]` · Remote Path · Identity file (optional)       |
| Kubernetes | Target (`.fresh/k8s.json`) **or** Context · Namespace · Pod · Workspace |

---

## Alternative A — Segmented type tabs at the top  *(recommended)*

A horizontal segmented control is the first thing in the form; ←/→ (or click)
switches it, and the field body below swaps to match. Common fields stay put.

```
┌───────────────────────── ORCHESTRATOR :: New Session ──────────────────────────┐
│                                                                                 │
│   Run in:   « Local »   SSH   Kubernetes                  (←/→ to switch)        │
│             ▔▔▔▔▔▔▔▔▔                                                            │
│ ╭─ Project Path ──────────────────────────────────────────────────────────────╮│
│ │ ~/code/acme-api                                                              ││
│ ╰─────────────────────────────────────────────────────────────────────────────╯│
│ [v] Create a new git worktree for this session                                  │
│ ╭─ Session Name ─╮  ╭─ Agent Command ─╮  ╭─ Branch ─╮                            │
│                                                       [ Cancel ]  [ Create Session ]│
└─────────────────────────────────────────────────────────────────────────────────┘
```

SSH selected:

```
│   Run in:    Local   « SSH »   Kubernetes                                        │
│                      ▔▔▔▔▔▔▔                                                     │
│ ╭─ Host  (user@host[:port]) ──────────────╮ ╭─ Remote Path ────────────────────╮│
│ │ deploy@build-01                          │ │ /srv/acme-api                    ││
│ ╰──────────────────────────────────────────╯ ╰──────────────────────────────────╯│
│ ╭─ Identity file (optional) ──────────────────────────────────────────────────╮ │
│ ╭─ Session Name ─╮  ╭─ Agent Command ─╮                                          │
```

Kubernetes selected:

```
│   Run in:    Local   SSH   « Kubernetes »                                        │
│                            ▔▔▔▔▔▔▔▔▔▔▔▔                                          │
│ ╭─ Target  (from .fresh/k8s.json — or define inline ↓) ───────────────────────╮ │
│ │ acme-api                                                  ▾ (2 targets)      │ │
│ ╰─────────────────────────────────────────────────────────────────────────────╯ │
│ ╭─ Context ─╮ ╭─ Namespace ─╮ ╭─ Pod ───────────╮ ╭─ Workspace ─╮                │
│ │ k3d-dev   │ │ default     │ │ acme-pod  ▾(run)│ │ /workspace  │                │
│ ╰───────────╯ ╰─────────────╯ ╰─────────────────╯ ╰─────────────╯                │
│ ╭─ Session Name ─╮  ╭─ Agent Command ─╮                                          │
│   ⓘ  preflight: kubectl ✓ · context reachable ✓ · create pods/exec ✓            │
```

- **+** Most discoverable; the type is always visible; matches the user's "nice
  way to choose the type." Self-documenting (you see all three).
- **+** Each type shows only its own fields → no clutter.
- **−** Needs a new segmented-control widget (or fake one with a focusable
  `raw` row + ←/→ handling); a little more build than reusing existing widgets.

## Alternative B — Single "Run in:" cycle field

One labeled field whose value cycles `Local ▸ SSH ▸ Kubernetes` with Space/←→
(like a `<select>`); the body below adapts exactly as in A.

```
│ ╭─ Run in ────────────────────────────────────────────────────────────────────╮│
│ │ ‹ Kubernetes ›                                            (Space / ←→ cycles) ││
│ ╰─────────────────────────────────────────────────────────────────────────────╯│
│  …type-specific fields…                                                          │
```

- **+** Smallest build — reuses the text/labeled-section pattern, fits the Tab
  cycle naturally.
- **−** The other choices are hidden until you cycle; less discoverable than
  tabs. Reads as "advanced".

## Alternative C — Two-step launcher (pick type, then a tailored form)

`Orchestrator: New Session` first opens a tiny picker; choosing an entry opens
the dedicated form for that type (the local form is exactly today's).

```
┌ New Session — choose a type ───────────────────────────────┐
│ ▸ Local        worktree or folder, runs on this machine    │
│   SSH          remote host  (user@host:/path)              │
│   Kubernetes   pod via kubectl exec  (EKS/GKE/AKS/k3d)     │
│   Devcontainer .devcontainer in a container                │
└────────────────────────────────────────────────────────────┘
```

- **+** Cleanest per-type forms; trivially extensible (Devcontainer, …); the
  picker doubles as documentation.
- **+** Zero change to the local form (lowest regression risk on the hot path).
- **−** One extra keystroke/step for every new session, including plain local.
  Could keep `Alt+N` → local directly and a separate "New Session (choose
  type)" for the picker.

## Alternative D — Unified smart "Target" field (URL-style)

Keep today's form almost verbatim; the top field becomes **Target** and parses
what you type, mirroring the existing `fresh user@host:path` / `ssh://` CLI. A
detected-type chip appears to the right.

```
│ ╭─ Target ──────────────────────────────────────────────────────[ Kubernetes ]╮│
│ │ k8s://k3d-dev/default/acme-pod/workspace                                     ││
│ ╰─────────────────────────────────────────────────────────────────────────────╯│
│   ⓘ  ./path  ·  user@host:/path  ·  ssh://host/path  ·  k8s://ctx/ns/pod[/ws]   │
│ [v] Create a new git worktree   (local & ssh-git only)                          │
│ ╭─ Session Name ─╮  ╭─ Agent Command ─╮                                          │
```

- **+** Smallest visual change; one mental model; great for power users; reuses
  the existing parser shapes.
- **−** Discoverability rests on the hint line; typos in the URL are the main
  failure mode; per-type validation/preflight is harder to surface inline.

---

## Recommendation

**A (segmented tabs) as the primary UX, borrowing D's parsing as an accelerator.**
Tabs make the choice obvious and keep each type's fields clean (the user asked
for "a nice way to choose the type"). Within the SSH/Kubernetes bodies, the
first field accepts a pasted `user@host:/path` or `k8s://ctx/ns/pod` and
auto-fills the rest — so power users get D's speed without losing A's clarity.
Local stays byte-for-byte today's flow (it's just the default tab), so the
no-remote experience is unchanged.

If build cost must be minimal for v1, **B** is the cheapest path to the same
field-swapping behavior and can be upgraded to A's tabs later without changing
the submit logic.
```
