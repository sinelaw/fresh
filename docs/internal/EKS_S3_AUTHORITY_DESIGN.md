# EKS authority — remote editing into a Kubernetes pod

Status: design. Nothing here ships yet. Supersedes the earlier
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
  rides in the existing `session_keepalive` slot so the daemon path
  keeps the channel alive across the rebuild — the same slot SSH already
  uses for `SshConnection`.

`setAuthority` (docker, local) and `attachRemoteAgent` (ssh-style remote
over a transport) are the two attach families. Keeping them separate is
honest: one swaps synchronously-constructible backends, the other
establishes a live connection core must own.

## S3 is the pod's problem

Fresh never sees S3. The durability guarantee comes entirely from how
the pod's workspace volume is provisioned — owned by the plugin /
cluster manifest, not core. Practically that means a **Mountpoint for
Amazon S3 CSI** volume (or `s3fs`, or an S3-synced PVC) mounted at the
workspace path. When the pod terminates, the objects persist in S3; a
new pod mounting the same bucket sees them immediately.

This pushes object-store semantics out of Fresh, but it does **not**
make them vanish — they reappear as one constraint on the *agent's*
behaviour:

### Decision 1 (load-bearing): mount semantics vs. the agent's save path

Fresh's save path writes a temp file and atomically `rename`s it into
place (see `model/buffer/save.rs`, and `RemoteFileSystem::write_patched`
/ `temp_path_for`). **Mountpoint for S3 does not support `rename` or
random/in-place writes** — it only does sequential writes to new
objects. So a temp-then-rename save *against a Mountpoint mount will
fail*. The choices:

- **(a) Pick a mount that supports rename** — `s3fs` (more POSIX, slower,
  caches locally) or a block PVC (EBS/EFS) that is *separately* synced
  to S3. Costs latency or adds a sync component.
- **(b) Give the EKS authority an in-place save path** — write directly
  to the destination object, skipping temp+rename, accepting a non-
  atomic save (a crash mid-write can truncate the file). This is a
  per-authority save-strategy flag the agent honours.

Recommendation: **(b)** for v1 (a config flag, `direct_write`, on the
remote authority), with **(a)** via `s3fs` documented for users who need
atomic saves. This keeps the cheap, native-object property the user
asked for and confines the cost to "saves are non-atomic on this
backend," which is acceptable for a single-editor workspace.

### Decision 2: durability granularity / loss window

A completed save → agent writes the object → the mount flushes to S3 on
close. So **a saved-and-closed file is durable.** The loss window is
anything written-but-not-flushed at the instant the pod dies, plus pod
scratch (build outputs, caches) that was never meant to be durable. The
contract to document: *"durable on save"*, not *"durable on keystroke."*
Good enough for the stated requirement; flagged so it's not a surprise.

### Decision 3: agent prerequisites in the pod image

The remote agent is Python over stdin. The workspace image must ship
`python3` (same constraint SSH already imposes on the remote host), or
we ship a static agent binary and `kubectl cp` it in before exec. v1:
require `python3`; document it; the plugin's preflight checks for it and
gives a clear error.

## What this is

- The SSH remote authority with a `kubectl exec` transport. New code is
  one transport impl, one constructor, one terminal wrapper, one attach
  op. Filesystem, spawners, agent, reconnect: reused verbatim.
- Durable-on-S3-when-the-pod-is-down, via an S3-backed pod mount that
  Fresh knows nothing about.

## What this is not

- **Not an `S3FileSystem`.** Fresh imports no AWS crate.
- **Not pod-independent access.** Pod down ⇒ no editing (by design — the
  user accepted this). The bytes are safe in S3; they're just not
  reachable through Fresh until a pod returns.
- **Not a pod provisioner.** Bringing pods up/down, the S3-backed PVC,
  autoscaling, cost controls — all the plugin's job (next doc).
- **Not multi-pod / multi-root.** One authority, one pod (principle 5).
- **Not atomic-save guaranteed.** Depends on the mount; see Decision 1.

## Open questions

1. The `RemoteTransport` refactor touches `connection.rs`, which SSH
   depends on — must land behind tests proving SSH is byte-for-byte
   unchanged before EKS rides on it.
2. `kubectl` as a host dependency for v1 (vs. `kube-rs` SPDY exec
   later). Acceptable to start; `RemoteTransport` makes the swap local.
3. Reconnect after pod *eviction/reschedule*: the pod name changes, so
   the respawn closure must re-resolve the target (ask the plugin for
   the current pod), not just re-run the old `kubectl exec`. Needs a
   "resolve current pod" callback in the transport.
