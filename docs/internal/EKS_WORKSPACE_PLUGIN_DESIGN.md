# `eks-workspace` plugin — bring-your-own-cluster pod management

Status: design. Companion to
[`EKS_S3_AUTHORITY_DESIGN.md`](EKS_S3_AUTHORITY_DESIGN.md), which makes
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
// .fresh/eks.json  (or user config)
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

The template mounts the S3-backed volume (Mountpoint-for-S3 CSI) at
`workspace` — that line is the user's, per the authority doc's "S3 is the
pod's problem." The plugin renders `${var}` / `${user}` / `${workspace}`
and applies it.

## Configuration model

Layered, lowest-effort-first:

1. **Zero config**: command palette → "EKS: Connect workspace" → if no
   targets configured, fall straight to `attach-existing` against the
   current kubeconfig context (pick namespace → pick pod). Works for
   anyone who already has a dev pod running.
2. **`.fresh/eks.json` in the repo**: shareable team targets (the
   examples above). Discovered like devcontainer's `findConfig()`.
3. **User-global config**: personal targets/overrides in the user config
   dir, for clusters not tied to one repo.

Schema sketch (full JSON-schema ships alongside, like
`plugins/config-schema.json`):

```ts
type EksConfig = {
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
- **`preflight` commands** — per-target gates (image has `python3`?
  quota available? VPN up?), with the failing command's stderr surfaced.
- **Teardown policy** — `down()` can stop, destroy, or no-op; teams that
  keep warm per-developer pods set "leave running" + `attach-existing`.
- **No lock-in on bring-up** — Fresh's built-ins (`manifest`/`run`) are
  conveniences; a team can ignore them entirely and route everything
  through `command`.

## Security & cost

- **Credentials never touch Fresh.** Everything shells out to the user's
  `kubectl`/`aws`, which resolve auth the way they already do (SSO,
  IRSA, `aws eks get-token`, EKS access entries). No payload carries a
  secret (authority doc, principle: payloads name resources, not
  secrets).
- **WorkspaceTrust unchanged.** Attaching to a cluster doesn't bypass
  command gating; the remote authority gates spawns like any other.
- **Confirm before create** (`confirmCreate`, default on) — making a pod
  costs money; the user okays it the first time per workspace.
- **Idle auto-stop** (`idleStopMinutes`) — the plugin tracks editor
  activity and runs `provider.down()` after idle, so a forgotten pod
  doesn't bill overnight. Scale-to-zero friendly.
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
