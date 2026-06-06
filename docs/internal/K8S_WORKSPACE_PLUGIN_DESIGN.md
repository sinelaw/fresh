# `k8s-workspace` plugin — bring-your-own-cluster pod management

> **Scope:** bring-your-own *any* Kubernetes cluster (EKS/GKE/AKS/k3d/
> minikube/kind). The `command` provider is the escape hatch for
> Terraform/Helm/CDK. AWS examples below are illustrative, not required.

Status: design. Companion to
[`K8S_AUTHORITY_DESIGN.md`](K8S_AUTHORITY_DESIGN.md), which makes
the editor able to *attach* to a pod. This doc covers the plugin that
gives the user a clean way to **choose, bring up, attach to, and tear
down** the pods, against **their own EKS clusters and AWS account**.

Modeled on `crates/fresh-editor/plugins/devcontainer.ts`: core owns the
authority slot; the plugin owns the backend lifecycle. The plugin runs
in the sandboxed TS runtime and only ever does host-side work through
`editor.spawnHostProcess(...)`, plus the new
`editor.attachRemoteAgent(...)` op from the authority doc.

## Goals

- **BYO everything.** The user's existing kubeconfig + AWS credential
  chain. No Fresh-hosted control plane, no Fresh AWS account, no secrets
  stored by Fresh. If `kubectl get pods` works in their terminal, the
  plugin works.
- **One-command connect.** "EKS: Connect workspace" → pick → attached.
  The common path is two or three keystrokes.
- **Fully customizable bring-up.** How a pod comes into existence is a
  *provider* the user configures. Fresh ships sensible built-ins but the
  escape hatch — "just run my command/script/Terraform" — is a
  first-class provider, not an afterthought.
- **Adaptable to other people's flows.** A team that manages
  "VDI-style developer containers" via Terraform in a separate repo
  should be able to point the plugin at that and have Connect/Disconnect
  drive it, without the plugin knowing anything about Terraform.

## Non-goals

- Not a Kubernetes dashboard. We manage *dev workspace* pods, not
  arbitrary cluster objects.
- Not a cluster provisioner. We attach to clusters that already exist.
- Not a credential manager. Auth is whatever the user's
  kubeconfig/AWS chain already does (SSO, IRSA, `aws eks get-token`).
- Not multi-pod-per-window (the authority is modal — one pod).

## Default posture: full-manage, with an escape hatch

Per [`K8S_WORKSPACE_UX_DESIGN.md`](K8S_WORKSPACE_UX_DESIGN.md) D1, the
default is **full-manage**: Fresh ships an opinionated, zero-config
provisioning engine so a developer with only an AWS account + a cluster
gets a working workspace with no setup. The built-in providers below are
therefore *real Fresh-owned defaults* (default EBS-live/S3-sync pod spec,
Karpenter/Spot-friendly, Pod-Identity-scoped) that also drive the full
`stop / resume / rebuild / resize / destroy / idle-stop` lifecycle — not
mere "apply the user's YAML" shims.

The `command` provider remains the **deliberate escape hatch** for teams
who own their provisioning (Terraform/Helm/CDK). It is the override, not
the thing every user must configure. Default path: Fresh just does it.

## The core abstraction: a `Provider`

Everything customizable is funneled through one small contract. A
provider answers four questions about a *workspace target*:

```ts
interface Provider {
  /** Bring a pod into a ready, attachable state. Idempotent: if a
   *  matching pod already runs, return it instead of creating one. */
  up(ctx: WorkspaceCtx): Promise<PodCoords>;
  /** Reverse of up(). Stop or destroy per the provider's policy. */
  down(ctx: WorkspaceCtx, pod: PodCoords): Promise<void>;
  /** Cheap liveness check used by the status bar / reconnect. */
  status(ctx: WorkspaceCtx, pod: PodCoords): Promise<"ready"|"pending"|"gone">;
  /** Optional: enumerate pre-existing pods for the "attach existing" UX. */
  list?(ctx: WorkspaceCtx): Promise<PodCoords[]>;
}

type PodCoords = {
  context?: string;      // kubeconfig context (defaults to current)
  namespace: string;
  pod: string;
  container?: string;
  workspace: string;     // pod-side path of the mounted workspace
};
```

`PodCoords` is exactly what `editor.attachRemoteAgent({ kind:
"kubectl-exec", ... })` needs. The plugin's only job is to *produce*
`PodCoords` (via some provider) and hand them to core. Core does the
agent bootstrap and the attach.

### Built-in providers

| `kind` | `up` does | Use case |
|---|---|---|
| `attach-existing` | nothing; user picks from `list()` (`kubectl get pods`) | A pod a team already runs per developer. |
| `manifest` | `kubectl apply -f <rendered template>`, wait for `Ready` | Self-serve ephemeral pod from a checked-in Pod/Job spec. |
| `run` | `kubectl run <name> --image=… --overrides=…`, wait | Quick throwaway pod, no manifest file. |
| `command` | runs a **user command**, parses its stdout for `PodCoords` (JSON) | **The escape hatch.** Terraform, Helm, a bash script, an internal CLI — anything. |

The `command` provider is what makes the plugin adapt to *any* flow.
It is deliberately dumb: run what you're told, read JSON back.

### Example: the Terraform-in-another-repo flow

A team manages "VDI-style terminal containers for developers" with
Terraform in `~/work/dev-infra`. They configure:

```jsonc
// .fresh/k8s.json  (or user config)
{
  "defaultTarget": "vdi",
  "targets": {
    "vdi": {
      "provider": {
        "kind": "command",
        "up":     { "command": "make",  "args": ["dev-up",   "USER=${user}"], "cwd": "~/work/dev-infra" },
        "down":   { "command": "make",  "args": ["dev-down", "USER=${user}"], "cwd": "~/work/dev-infra" },
        "status": { "command": "make",  "args": ["dev-status","USER=${user}"], "cwd": "~/work/dev-infra" }
        // each command prints PodCoords as JSON on its last stdout line
      }
    }
  }
}
```

`make dev-up` runs `terraform apply` and ends with, say,
`terraform output -json | jq -c '{namespace,pod,workspace}'`. The plugin
reads that line, gets `PodCoords`, calls `attachRemoteAgent`. The plugin
never mentions Terraform. Swapping to Pulumi, CDK, or a Helm chart is a
config edit, not a code change.

### Example: self-serve ephemeral pod (manifest provider)

```jsonc
{
  "targets": {
    "scratch": {
      "provider": {
        "kind": "manifest",
        "template": ".fresh/workspace-pod.yaml",   // Go/${var} templated
        "namespace": "dev-${user}",
        "waitTimeoutSec": 180
      },
      "vars": { "image": "ghcr.io/acme/dev:latest", "cpu": "2", "mem": "4Gi" },
      "idleStopMinutes": 30
    }
  }
}
```

The template provisions an **EBS GP3 volume** at `workspace` and a sync
sidecar to S3 (see §"Storage" for why — *not* an S3 live mount). Those
lines are the user's; the plugin just renders `${var}` / `${user}` /
`${workspace}` and applies the manifest.

## Storage — what the pod's volume should be (research-driven)

This is where the deep-research review changed the recommendation. The
durability requirement ("bytes in S3 when the pod is down") does **not**
mean "mount S3 as the live workspace." A code editor needs POSIX
fidelity (atomic `rename` on save) and small-file speed; S3 mounts and
EFS both fail that hard. The recommended shape:

> **Live tier = Amazon EBS GP3** (dynamic PV via the EBS CSI driver).
> **Durable tier = S3**, reached by *syncing* the EBS workspace to a
> bucket on `preStop` + periodically (+ optionally debounced on save),
> and restoring it via an initContainer on a fresh pod.

This keeps Fresh's normal atomic-save path working unchanged (no
`direct_write` hack) and still puts the bytes in S3 when the pod is gone.
EBS is single-AZ, so the manifest must pin the pod to the volume's AZ
(topology-aware scheduling); Karpenter then provisions the node in that
AZ.

### Alternatives & trade-offs

| Volume strategy | Atomic save (`rename`) | Small-file speed | Durable when pod down | Cost | Verdict |
|---|---|---|---|---|---|
| **EBS GP3 + S3 sync** (recommended) | ✅ full POSIX | ✅ sub-ms, ~3000 IOPS | ✅ via sync (loss window = last sync) | EBS while running + S3 storage | **Default.** Fast, atomic, durable; cost is a sync component + AZ pinning. |
| Mountpoint for S3 (standard) — *live mount* | ❌ rename fails (I/O error) | ❌ per-op REST latency | ✅ always (it *is* S3) | cheapest | **Reject for source.** Saves fail; no dir-rename/random-write/symlink. OK for read-only artifact fetch. |
| Mountpoint for S3 Express One Zone `--allow-overwrite` | ⚠️ file rename only | ⚠️ better, still object latency | ✅ | higher (Express) | Marginal; still no dir-rename/random-write, no Local Zones. |
| Amazon EFS | ✅ POSIX | ❌ ~100× slower file-create; Maven build 16 min vs 1:45 on EBS | ✅ multi-AZ | medium-high | **Reject for source.** RWX/multi-AZ is wasted on a single-editor workspace and the latency cripples `npm i`/`git clone`. |
| `s3fs` / `goofys` live mount | ⚠️ emulated, fragile | ❌ slow, cache quirks | ✅ | cheap | Weak; only if S3-as-source is a hard mandate. |
| Local NVMe instance store + S3 sync | ✅ | ✅ fastest | ⚠️ only what was synced (lost on stop) | included w/ instance | Fastest, but sync is *critical-path* and instance-type-bound. |

The plugin doesn't enforce a choice — storage is the manifest/provider's
business — but ships the EBS+sync manifest as the `manifest`-provider
default and documents this table so users don't reach for the S3 mount
and hit failing saves.

## Compute & identity defaults (research-driven)

These belong in the user's manifest/Terraform, but the plugin's
defaults, docs, and preflight should steer toward them:

- **Karpenter + EC2 Spot for workspace pods**, with On-Demand fallback;
  cluster add-ons on a static managed node group. Enforce a NodePool
  instance-type allow-list so a workspace can't request a $30/hr box.
  Scale-to-zero off-hours via CronJob keeps idle cost near zero.
- **Avoid AWS Fargate for workspaces.** Fargate forbids DaemonSets, so
  the **EKS Pod Identity Agent can't run** — the pod loses secure AWS
  access (e.g. the S3 sync's credentials) — and there's no eBPF for
  sandboxing. The plugin's preflight should warn if the target pod is
  scheduled on Fargate.
- **Identity: EKS Pod Identity (not IRSA) + Session Policies; EKS Access
  Entries (not `aws-auth`).** A single broad "developer workspace" role,
  scoped down at provisioning time by an inline session policy to *this*
  developer's bucket prefix — least privilege without exhausting the
  5,000-IAM-role account quota. All of this is the user's IaC; the
  plugin only needs the resulting kubeconfig + the pod's own creds.

## Configuration model

Layered, lowest-effort-first:

1. **Zero config**: command palette → "EKS: Connect workspace" → if no
   targets configured, fall straight to `attach-existing` against the
   current kubeconfig context (pick namespace → pick pod). Works for
   anyone who already has a dev pod running.
2. **`.fresh/k8s.json` in the repo**: shareable team targets (the
   examples above). Discovered like devcontainer's `findConfig()`.
3. **User-global config**: personal targets/overrides in the user config
   dir, for clusters not tied to one repo.

Schema sketch (full JSON-schema ships alongside, like
`plugins/config-schema.json`):

```ts
type K8sConfig = {
  defaultTarget?: string;
  targets: Record<string, {
    provider: ProviderSpec;          // attach-existing | manifest | run | command
    vars?: Record<string, string>;   // template vars
    idleStopMinutes?: number;        // auto-stop guardrail (0 = never)
    confirmCreate?: boolean;         // default true: ask before making a pod
    preflight?: CommandSpec[];       // e.g. check `python3` in image, check quota
  }>;
};
```

## User experience

Commands (registered via `editor.registerCommand`):

- **EKS: Connect workspace** — the front door. Quick-pick of configured
  targets (or the zero-config path). Drives the lifecycle state machine
  below. Status bar shows progress (`setStatus`).
- **EKS: Disconnect** — `clearAuthority()` + the provider's `down()` per
  the target's teardown policy (stop vs. destroy vs. leave-running).
- **EKS: Switch pod / target** — disconnect + connect to another.
- **EKS: Rebuild pod** — `down()` then `up()` (the Terraform/manifest
  re-apply path), then re-attach.
- **EKS: Show workspace info** — a panel (devcontainer-style) with
  cluster/ns/pod, image, mount, idle timer, last error.

Pickers use the existing `startPrompt` + `setPromptSuggestions` quick-
pick machinery (the same one Quick Open uses). Decisions are remembered
per workspace via `setGlobalState`/`getGlobalState` keyed on `getCwd()`
+ target — reopening a project doesn't re-prompt, exactly like
devcontainer's remembered attach decision.

Status bar: a compact indicator (`● eks:dev/pod`) with color for
ready/pending/disconnected, mirroring how the SSH/devcontainer status
surfaces today.

## Lifecycle state machine

```
        ┌─ Connect ─┐
detached ──────────► resolving-target
                         │ (pick target / read config)
                         ▼
                     preflight ──fail──► error (clear message, stay detached)
                         │ ok
                         ▼
                     provider.up()  ◄── confirmCreate? ask first
                         │ (stream logs to a buffer via stdoutTo)
                         ▼
                     wait-ready (poll provider.status)
                         │ ready
                         ▼
                     env-probe (kubectl exec … env)  [optional]
                         │
                         ▼
                     attachRemoteAgent(PodCoords)  ──► core restarts, attached
                         │
   ┌──── idle timer ─────┤
   │  (idleStopMinutes)  ▼
   │                  ATTACHED ──Disconnect──► provider.down() ──► detached
   └─ auto-stop ────────┘
```

Every host-side step is `spawnHostProcess`; long ones (`up`, image pull,
`terraform apply`) stream stdout into a scratch buffer with `stdoutTo`
so the user watches progress instead of staring at a spinner.

## Customization & adaptation hooks

The whole point is "easy to adapt." The seams:

- **`command` provider** — arbitrary up/down/status/list commands. The
  universal adapter; covers Terraform, Helm, Pulumi, CDK, internal CLIs.
- **Template vars** — `${user}`, `${workspace}`, `${cwd}`, plus any
  `vars` from config, substituted into manifests and command args.
- **`preflight` commands** — per-target gates with the failing command's
  stderr surfaced. The research makes three checks worth shipping as
  built-in defaults (over and above user-supplied ones):
  - **`python3` present** in the pod (agent prerequisite) —
    `kubectl exec … -- sh -lc 'command -v python3'`.
  - **`create` on `pods/exec`** for the developer identity —
    `kubectl auth can-i create pods/exec -n <ns>`. K8s ≥1.30 runs `exec`
    over WebSockets and ≥1.35 demands the `create` verb; a read-only
    `get` grant that "worked before" silently breaks attach after a
    cluster upgrade. Catch it here with a clear message.
  - **not on Fargate** — warn if the target pod's node is Fargate, since
    the EKS Pod Identity Agent (a DaemonSet) can't run there and the
    pod's S3-sync credentials will fail.
- **Teardown policy** — `down()` can stop, destroy, or no-op; teams that
  keep warm per-developer pods set "leave running" + `attach-existing`.
- **No lock-in on bring-up** — Fresh's built-ins (`manifest`/`run`) are
  conveniences; a team can ignore them entirely and route everything
  through `command`.

## Security & cost

- **Credentials never touch Fresh.** Everything shells out to the user's
  `kubectl`/`aws`, which resolve auth the way they already do (SSO,
  `aws eks get-token`, **EKS Access Entries**). In-pod AWS access (the
  S3 sync) uses **EKS Pod Identity + a scoped session policy**, not
  baked-in keys. No payload carries a secret (authority doc principle:
  payloads name resources, not secrets).
- **WorkspaceTrust unchanged.** Attaching to a cluster doesn't bypass
  command gating; the remote authority gates spawns like any other.
- **Confirm before create** (`confirmCreate`, default on) — making a pod
  costs money; the user okays it the first time per workspace.
- **Idle auto-stop** (`idleStopMinutes`) — the plugin tracks editor
  activity and runs `provider.down()` after idle, so a forgotten pod
  doesn't bill overnight. Pairs with cluster-side scale-to-zero CronJobs
  (off-hours) and Karpenter draining empty nodes.
- **Session TTL** — independent of idle, cap a session's lifetime
  (e.g. 12 h) and force teardown, so a compromised kubeconfig token
  can't be leveraged indefinitely even while keep-alives hold the stream
  open. A security control, not just a cost one.
- **Clear teardown** — Disconnect always offers to stop/destroy; the
  panel shows what's still running and roughly what it's costing if the
  provider reports it.

## Failure handling

- **`up` fails / times out** → stay detached, show the captured stderr
  in the log buffer + a one-line status. Never half-attach.
- **Pod evicted/rescheduled mid-session** → the authority's reconnect
  task asks the provider for the *current* pod (the
  `resolve current pod` callback in the authority doc's open question 3)
  rather than re-running the stale `kubectl exec`. If the pod is `gone`,
  surface "workspace pod ended" with a one-click Rebuild.
- **`python3` missing in image** → caught by preflight with an
  actionable message, before the confusing agent-handshake failure.
- **Idle freeze** → the agent heartbeat (authority doc) keeps the
  `exec` stream past ELB/NAT idle timeouts; if the channel drops anyway,
  reconnect re-resolves the pod rather than presenting a frozen UI.

## Relationship to core (what the plugin needs from Fresh)

Only two ops, both small:

- `editor.spawnHostProcess(cmd, args, cwd?)` — exists today. Runs all
  provider commands on the host.
- `editor.attachRemoteAgent(spec)` — **new**, from the authority doc.
  Hands core the `PodCoords` as a `kubectl-exec` transport spec; core
  does the agent bootstrap + destructive restart. Until it lands, the
  plugin is unbuildable — this is the one core dependency.

`clearAuthority()` (exists) covers detach. No other core changes.

## Testing

- **Provider unit tests** (TS) — `command`-provider stdout parsing,
  template var substitution, config layering, remembered-decision keys.
  No cluster.
- **Lifecycle tests** — drive the state machine with a fake provider
  (mock `spawnHostProcess`), assert attach/detach/rebuild/idle-stop
  transitions and that failures never half-attach. Mirrors the
  devcontainer plugin's e2e style.
- **Integration** (separate CI lane) — a `kind` cluster, the `manifest`
  provider, a real attach round-trip. Not in the default test run.

## Open questions

1. **Idle detection granularity** — what counts as "active"? Keystrokes,
   saves, LSP traffic, terminal use? Start coarse (any editor event),
   refine if it stops pods mid-think.
2. **Multiple windows, one pod** — if two Fresh windows target the same
   pod, who owns `down()`? Likely ref-count via `globalState`, or simply
   "Disconnect never destroys a pod it didn't create."
3. **`command`-provider contract** — JSON on last stdout line is simple
   but brittle. A `--fresh-json` sentinel-delimited block would be more
   robust; decide before teams build against it.
4. **Cost reporting** — can we get a meaningful "$ so far" without
   provider-specific hooks? Probably provider-optional; don't promise it
   generically.
