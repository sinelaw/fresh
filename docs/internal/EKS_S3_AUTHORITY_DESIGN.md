# EKS authority — remote editing into a Kubernetes pod

> **Implementation status (foundational slice landed).** The
> `RemoteTransport` seam (`services/remote/transport.rs`) ships:
> `KubectlExecTransport`, the shared `kubectl_exec_argv` builder,
> `agent_bootstrap_pycode`, the generic `bootstrap_agent`, and
> `EksConnection` — the EKS analogue of `SshConnection`, reusing the
> transport-agnostic `AgentChannel`. Plus `build_eks_terminal_args`,
> `TerminalWrapper::eks`, and the `Authority::eks` constructor. SSH's
> `connection.rs` is untouched (SSH provably unchanged). Unit-tested
> (argv ordering, bootstrap framing, terminal re-parenting). **Not yet
> wired:** the kubectl long-running (LSP) spawner with env/cwd, the async
> `attachRemoteAgent` op + reconnect-respawn, the agent heartbeat,
> per-session authority, and the `eks-workspace` plugin.

Status: design. Nothing else here ships yet. Supersedes the earlier
"EKS + S3 full cloud authority" draft, which proposed a bespoke
`S3FileSystem`. That half is **deleted**: per the refined requirement,
Fresh reaches workspace data *only through the pod*, so S3 never appears
in Fresh's code at all. It is demoted to a pod-provisioning detail (see
§"S3 is the pod's problem").

Read [`AUTHORITY_DESIGN.md`](AUTHORITY_DESIGN.md) first, then the SSH
remote design ([`ssh-remote-editing-design.md`](ssh-remote-editing-design.md)).
This document is small on purpose: **the EKS authority is the SSH
authority with the transport swapped from `ssh` to `kubectl exec`.**

## The requirement, stated as an invariant

> The durable home for workspace bytes is an S3 bucket (a cheap storage
> tier). Fresh accesses those bytes **only through a running pod** that
> mounts the bucket. When the pod is down, the data still lives in S3 —
> Fresh simply can't open it until a pod is back. Bringing up a fresh
> pod against the same bucket restores access with no warm-up step.

Two consequences fall straight out:

1. **Fresh does not speak S3.** No AWS SDK dependency, no object-store
   impedance matching, no `S3FileSystem`. Fresh talks to a *pod*, which
   presents an ordinary POSIX view of the workspace.
2. **Durability is a property of the pod's volume, not of Fresh.** The
   pod mounts an S3-backed volume; that mount is what keeps the bytes in
   S3 when the pod dies. Fresh is oblivious.

## Why this is (almost) free

The remote-agent stack Fresh already ships for SSH is transport-agnostic
end to end:

- `AgentChannel::from_transport<R, W>` takes **any**
  `AsyncBufRead`/`AsyncWrite` pair
  (`services/remote/channel.rs:119`). It is not SSH-aware.
- The SSH connection is nothing but: spawn
  `ssh … python3 -u -c "exec(sys.stdin.read(N))"`, stream the agent
  source into its stdin, wait for the `ready` line, then hand the
  child's stdout/stdin to `AgentChannel::new`
  (`services/remote/connection.rs:117-186`).
- Everything above the channel —
  [`RemoteFileSystem`](../../crates/fresh-editor/src/services/remote/filesystem.rs),
  `RemoteProcessSpawner`, `RemoteLongRunningSpawner`, and the Python
  agent itself — only ever talks to the `AgentChannel`. None of them
  knows what carries the bytes.
- Reconnect is already generic: `spawn_reconnect_task_with` calls a
  caller-supplied closure to produce a fresh `(reader, writer)` and
  hot-swaps it via `channel.replace_transport(...)`
  (`connection.rs:252-323`).

So an EKS authority needs exactly one genuinely new thing: a transport
that bootstraps the agent over `kubectl exec` instead of `ssh`.
Everything else — file I/O, process spawn, LSP spawn, find-in-files,
save, auto-recovery, reconnect — is the SSH implementation, unchanged.

### The new code, in full

1. **A `RemoteTransport` seam.** Factor the "spawn the agent process and
   give me `(reader, writer)` plus a respawn closure" step out of
   `connection.rs` into a small trait with two impls:

   ```rust
   /// Bootstraps the Python agent over some carrier and yields the
   /// stdio pair the AgentChannel rides on. The respawn closure is what
   /// the reconnect task calls to rebuild the carrier after a drop.
   pub trait RemoteTransport: Send + Sync {
       async fn connect(&self) -> Result<AgentStdio, TransportError>;
       fn display(&self) -> String; // "user@host" / "eks:ctx/ns/pod"
   }

   pub struct SshTransport   { params: ConnectionParams, /* … */ }
   pub struct KubectlExecTransport {
       context: Option<String>,
       namespace: String,
       pod: String,
       container: Option<String>,
   }
   ```

   `KubectlExecTransport::connect` spawns

   ```
   kubectl [--context CTX] exec -i -n NS [-c C] POD -- \
       python3 -u -c "import sys;exec(sys.stdin.read(N))"
   ```

   then performs the *identical* agent-source send + `ready` handshake
   the SSH path already does. The bytes after handshake are the same
   agent protocol over the same channel.

2. **`Authority::eks(...)`** — a near-clone of `Authority::ssh(...)`. It
   takes the already-built `RemoteFileSystem` / remote spawners (over the
   kubectl-exec channel) and sets `TerminalWrapper::eks(...)`. Like SSH,
   `path_translation: None` — the editor operates directly in the pod's
   path space (the mount looks like a normal directory in the pod;
   there's nothing to translate).

3. **`TerminalWrapper::eks(target, workspace)`** — the only spawn that
   does *not* ride the agent channel, exactly as SSH's terminal uses a
   separate `ssh -t` PTY:

   ```
   kubectl exec -it -n NS [-c C] POD -- sh -lc 'cd WS; exec "$SHELL" -l'
   ```

   Pins cwd through its own args ⇒ `manages_cwd = true`, same rule as the
   SSH and docker wrappers.

That's the entire Fresh-side surface. Process spawning, including LSP,
comes for free: `RemoteProcessSpawner`/`RemoteLongRunningSpawner` send
spawn RPCs to the agent, which launches them *inside the pod*. There is
no separate `EksExecSpawner` and no `docker_spawner`-style argv builder —
the agent is already the in-pod executor.

## How attach is triggered (plugin → core)

SSH connects at startup (`fresh user@host:path`). EKS attaches
post-boot, driven by the pod-management plugin (see
[`EKS_WORKSPACE_PLUGIN_DESIGN.md`](EKS_WORKSPACE_PLUGIN_DESIGN.md)). The
wrinkle: building the transport is **async** (spawn kubectl, bootstrap
the agent, await `ready`) and produces **keepalive resources** (the
child process, the Tokio runtime, the reconnect task). The synchronous
`from_plugin_payload` path can't express that — and shouldn't, because a
live stdio channel can't travel through a JSON payload.

So EKS attach reuses the SSH *connect* machinery, not the docker
*payload* machinery:

- A new plugin op `editor.attachRemoteAgent(spec)` where `spec` names a
  transport (`{ kind: "kubectl-exec", context, namespace, pod, container,
  workspace, displayLabel }`). It is fire-and-forget with restart
  semantics, exactly like `setAuthority`.
- Core stashes the spec as a `PendingAuthoritySpec` and triggers the
  same destructive restart `install_authority` uses. During rebuild
  (the existing `connect_remote` / `create_startup_authority` seam, and
  its `EditorServer::rebuild_editor` mirror), core runs
  `connect_remote_agent(transport)`:

  ```rust
  async fn connect_remote_agent(t: Arc<dyn RemoteTransport>)
      -> Result<(Arc<RemoteFileSystem>,
                 Arc<dyn ProcessSpawner>,
                 Arc<dyn LongRunningSpawner>,
                 RemoteKeepalive), ConnectError>;
  ```

  SSH startup and EKS attach both call this; only the transport differs.
- The resulting `RemoteKeepalive` (runtime + child + reconnect task)
  rides in the `session_keepalive` slot so the daemon path keeps the
  channel alive across the rebuild — the same slot SSH already uses for
  `SshConnection`. (Under per-session authority — `AUTHORITY_DESIGN.md`
  §"Evolution" — this slot becomes per-session, so each warm Cloud
  Workspace owns its own keepalive rather than the process owning one.)

`setAuthority` (docker, local) and `attachRemoteAgent` (ssh-style remote
over a transport) are the two attach families. Keeping them separate is
honest: one swaps synchronously-constructible backends, the other
establishes a live connection core must own.

## S3 is the pod's problem — but *not* as the live mount

Fresh never sees S3. How the pod's workspace volume is provisioned is
owned entirely by the plugin / cluster manifest, not core. The earlier
draft of this doc proposed mounting the bucket *live* (Mountpoint for S3
CSI) and giving the agent an in-place `direct_write` save path. **The
deep-research review (`eks-workspace-research-prompt.md` findings)
killed that recommendation, and it's worth being explicit about why.**

### Decision 1 (load-bearing, REVISED): EBS GP3 as the live tier; S3 as the durable tier

A code editor lives or dies on POSIX fidelity and small-file latency,
and the benchmarks are damning for an S3 live mount:

- **Mountpoint for S3 forbids non-replacing `rename`** on standard
  buckets — it fails early with an I/O error rather than emulate
  copy+delete. Fresh's save is temp-write-then-`rename` (`save.rs`,
  `temp_path_for`), so **every save would fail**. It also blocks
  directory renames, random mid-file writes, `chmod`/`chown`, and
  sym/hard links. S3 Express One Zone + `--allow-overwrite` restores
  *file* rename only, at higher cost and with no Local Zones — still no
  dir-rename or random writes. A non-starter for a live workspace.
- **EFS is ~100× slower on small files** — file-create latency of
  ~22 ms vs. ~0.2 ms on EBS; a Maven build measured at **16 min on EFS
  vs. 1:45 on EBS**. `git clone` / `npm install` / venvs generate
  exactly the synchronous small-file metadata storm EFS is worst at.

So the live working volume is **Amazon EBS GP3** (dynamic PV via the EBS
CSI driver): full POSIX, sub-ms metadata, ~3000 IOPS baseline. The
durable, cheap S3 tier is reached by **syncing** the EBS workspace to a
bucket — on graceful teardown (a `preStop` hook), periodically, and/or
debounced on save — and restoring it on a fresh pod's startup
(initContainer). This *still satisfies the stated invariant* ("the data
is in S3 when the pod is down") while keeping editing fast and saves
atomic.

**The win for Fresh: zero core change.** Against an EBS-backed POSIX
filesystem, the existing remote save path (temp + atomic `rename`) just
works — the `direct_write` flag the prior draft proposed is **deleted**
from this design. Storage policy lives entirely in the pod manifest /
plugin; Fresh is oblivious, exactly as intended.

See [`EKS_WORKSPACE_PLUGIN_DESIGN.md`](EKS_WORKSPACE_PLUGIN_DESIGN.md)
§"Storage" for the full alternatives table and the recommended
EBS-live + S3-sync manifest pattern.

### Decision 2: durability granularity / loss window

With the sync model, durability is **"durable as of the last sync,"** not
"durable on keystroke." Tighten the window by syncing on save (debounced)
and on `preStop`, accepting more S3 PUT churn; or accept a coarser
periodic sync. Either way it is a *pod-side sync policy*, not a Fresh
concern. Pod scratch (build outputs, caches) is intentionally excluded
from the durable set and lost on pod death — that's the point.

### Decision 3: agent prerequisites in the pod image

The remote agent is Python over stdin. The workspace image must ship
`python3` (same constraint SSH already imposes), or we `kubectl cp` a
static agent binary in before exec. v1: require `python3`, checked by the
plugin's preflight with a clear error.

## Connection liveness — what the research forces on the transport

The research surfaced three exec-layer realities the kubectl-exec
transport must handle. None require an `S3FileSystem`-style rethink, but
they shape the transport and reconnect logic:

- **Idle timeouts silently freeze sessions.** ELB/NAT idle timers
  (5-15 min) drop a long-lived `exec` stream that sees no traffic, with
  no TCP FIN — the UI just freezes. The agent channel already has
  reconnect (`replace_transport`), but it needs an **application-level
  heartbeat**: a periodic no-op ping RPC (well under the timeout) to keep
  the stream warm, plus prompt detection of a dropped channel to trigger
  reconnect. This is a small addition to the agent protocol, shared by
  SSH and EKS.
- **Pod reschedule changes the pod name.** A Spot interruption / eviction
  reschedules onto a new node with a new pod name and IP; volatile state
  is gone, only the volume survives. So the transport's respawn closure
  **cannot re-run a cached `kubectl exec <oldpod>`** — it must call back
  to the plugin to *re-resolve the current pod* before reconnecting
  (open question 3, now load-bearing). On `gone`, surface "workspace pod
  ended → Rebuild," never a frozen screen.
- **TTY resize and SPDY→WebSocket: we get these free.** Because the
  integrated terminal shells out to the real `kubectl exec -it` binary,
  `kubectl` implements the `TerminalSizeQueue` (SIGWINCH) protocol and
  the K8s ≥1.30 WebSocket negotiation for us — we don't hand-roll a
  streaming API client. One inherited gotcha: K8s ≥1.30 routes `exec`
  over WebSockets, and ≥1.35 requires the **`create` verb on
  `pods/exec`**; the plugin's preflight must check the developer identity
  holds it, or attach fails confusingly after a cluster upgrade.

## What this is

- The SSH remote authority with a `kubectl exec` transport. New code is
  one transport impl, one constructor, one terminal wrapper, one attach
  op. Filesystem, spawners, agent, reconnect: reused verbatim.
- Durable-on-S3-when-the-pod-is-down, via an **EBS live volume synced to
  S3** (not an S3 live mount) that Fresh knows nothing about.

## What this is not

- **Not an `S3FileSystem`.** Fresh imports no AWS crate.
- **Not pod-independent access.** Pod down ⇒ no editing (by design — the
  user accepted this). The bytes are safe in S3; they're just not
  reachable through Fresh until a pod returns.
- **Not an S3 live mount.** Per the research, the live workspace is EBS;
  S3 is a sync target. Saves stay atomic and fast; no `direct_write`.
- **Not a pod provisioner.** Bringing pods up/down, the EBS volume, the
  S3 sync, autoscaling, cost controls — all the plugin's job (next doc).
- **Not multi-pod / multi-root.** One authority, one pod (principle 5).

## Open questions

1. The `RemoteTransport` refactor touches `connection.rs`, which SSH
   depends on — must land behind tests proving SSH is byte-for-byte
   unchanged before EKS rides on it.
2. `kubectl` as a host dependency for v1 (vs. `kube-rs` WebSocket exec
   later). Acceptable to start; `RemoteTransport` makes the swap local.
   Note: using the `kubectl` binary is also how we inherit TTY-resize
   and the SPDY→WebSocket transition for free.
3. **Reconnect after pod eviction/reschedule (now load-bearing).** Spot
   interruptions make this the common case, not an edge. The pod name
   changes, so the respawn closure must re-resolve the target via a
   "resolve current pod" callback into the plugin, then reconnect —
   recovering workspace state from the synced EBS volume. Needs design
   alongside the plugin's provider `status()`/`up()`.
4. **Agent heartbeat.** Add a periodic no-op ping RPC to the agent
   protocol so idle `exec` streams survive ELB/NAT idle timeouts
   (5-15 min) instead of silently freezing. Shared by SSH and EKS;
   pick an interval well under the smallest common timeout (~60 s).
