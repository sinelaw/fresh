# EKS + S3 Authority — editing in the cloud

Status: design. Nothing in this document ships yet. It describes a new
*full* `Authority` whose filesystem is an S3 bucket and whose process
spawner is `kubectl exec` against a pod running in an EKS cluster.

Read [`AUTHORITY_DESIGN.md`](AUTHORITY_DESIGN.md) first. This is an
instance of that pattern, not a change to it. The contract is unchanged:
one `Authority` per `Editor`, opaque to core, constructed in one place,
transitions are destructive. Everything below lives behind the existing
`Arc<dyn FileSystem>` / `Arc<dyn ProcessSpawner>` / `Arc<dyn
LongRunningSpawner>` slots.

## Why this authority exists

SSH gives you "someone else's POSIX box". Docker-exec gives you "a
container on this box". Neither covers the shape a lot of cloud work
actually has:

- The *source of truth for files* is an object store (an S3 bucket /
  prefix), durable and independent of any compute.
- The *compute* is ephemeral and elastic — a pod scheduled onto EKS,
  which may be recycled, rescheduled, or scaled to zero between edit
  sessions.

The editor should be able to point at `s3://bucket/prefix` for content
and at a pod in a cluster for "run the build / the LSP / a shell", with
neither half assuming the other is a normal Unix machine you SSH'd into.

That split — durable object store for bytes, disposable pod for
execution — is the thing this authority models. It is one authority
(principle 5: modal, no composition), but unlike local/SSH/docker its
two halves genuinely live in different places, which is the entire
design problem.

## The central tension: object store ≠ filesystem, and two namespaces

Two hard facts drive every decision here.

**1. S3 is not a POSIX filesystem.** It has no directories (only key
prefixes), no atomic rename, no append, no `mtime` you set, no
permissions model, no symlinks, no partial in-place writes, and
eventually-but-now-strongly-consistent reads. The `FileSystem` trait
(`crates/fresh-editor/src/model/filesystem.rs`) is ~40 methods of
unapologetic POSIX. Most of the work is the impedance match.

**2. The bytes and the compute are in different namespaces.** A file is
`s3://bucket/proj/src/main.rs` to the editor, but the LSP running in the
pod wants to open `/workspace/src/main.rs`. Something has to reconcile
those. We have exactly one existing mechanism for that —
[`PathTranslation`](AUTHORITY_DESIGN.md) — and one architectural choice
to make about whether the pod even sees the same bytes.

### Resolving namespace #2: the pod mounts the bucket

The design decision is: **the pod mounts the same bucket the editor's
filesystem reads**, via the [Mountpoint for Amazon S3 CSI
driver](https://github.com/awslabs/mountpoint-s3-csi-driver) (or
`s3fs`), at a known path (default `/workspace`). The editor's S3
filesystem and the pod's mounted view are then two windows onto one
bucket/prefix.

This buys us the same property devcontainers rely on: the workspace is
*mounted*, not copied, so we translate paths rather than sync bytes.
`PathTranslation { host_root: s3-key-prefix, remote_root: /workspace }`
maps the editor's S3 key space to the pod's mount path for LSP URIs and
spawn cwds, exactly as the dev-container authority maps host paths to
in-container paths today.

What we explicitly do **not** build (at least v1):

- A bidirectional file *sync* engine. The pod reads/writes the bucket
  directly through its mount; the editor reads/writes the bucket
  directly through the SDK. There is no third copy to reconcile.
- A guarantee of write-visibility *latency*. Mountpoint-for-S3 is
  read-optimized; a file the editor `PutObject`s is visible to the pod
  on its next open, but a process holding the old object open won't see
  it. This is a documented limitation, not a bug to engineer around in
  v1 (see *Consistency & caching*).

The alternative — pod has its own scratch disk, editor syncs to/from S3
— is a real product (it's how some remote-dev tools work) but it is a
*different, larger* project (a sync daemon, conflict resolution, a
watch protocol). Out of scope. Noted in *What this is not*.

## Shape of the authority

A new constructor alongside `local` / `ssh` / `from_plugin_payload`:

```rust
impl Authority {
    /// Editing against an S3-backed workspace with execution on an EKS pod.
    ///
    /// `fs` is an `S3FileSystem` already wired to a bucket/prefix and an
    /// async handle; `process_spawner` / `long_running_spawner` are
    /// `EksExecSpawner` variants targeting a ready pod. The caller owns
    /// the keepalive bundle (Tokio runtime, AWS config, the pod's
    /// liveness lease) — same shape as SSH's `session_keepalive`.
    pub fn eks_s3(
        filesystem: Arc<dyn FileSystem + Send + Sync>,
        process_spawner: Arc<dyn ProcessSpawner>,
        long_running_spawner: Arc<dyn LongRunningSpawner>,
        target: &EksTarget,
        mount_path: &str,
        bucket_prefix: &str,
        display_label: String,
        trust: Arc<WorkspaceTrust>,
        env: Arc<EnvProvider>,
    ) -> Self {
        Self {
            filesystem,
            process_spawner,
            long_running_spawner,
            terminal_wrapper: TerminalWrapper::eks(target, mount_path),
            display_label, // e.g. "eks:prod/ns/pod · s3://bucket/prefix"
            path_translation: Some(PathTranslation {
                host_root: PathBuf::from(bucket_prefix),
                remote_root: PathBuf::from(mount_path),
            }),
            workspace_trust: trust,
            env_provider: env,
        }
    }
}
```

Note what does **not** change: the `Authority` struct keeps its exact
field set. `display_label` is *not* left empty here (unlike SSH) because
there is no `remote_connection_info()` fallback that knows about an S3
bucket; identity lives in the label (principle 9).

## Half one: `S3FileSystem`

New module `crates/fresh-editor/src/services/cloud/s3/filesystem.rs`,
implementing `FileSystem` (and inheriting the default-impl
`FileSystemExt` async wrappers). It owns:

```rust
pub struct S3FileSystem {
    client: aws_sdk_s3::Client,
    bucket: String,
    /// Key prefix the workspace root maps to (no leading slash;
    /// trailing slash normalized). The editor sees paths *under* this;
    /// we strip/join it on every call.
    prefix: String,
    /// Block-on handle so the synchronous `FileSystem` trait can drive
    /// the async AWS SDK — identical strategy to `AgentChannel`'s
    /// `runtime_handle` / `request_blocking` (services/remote/channel.rs).
    runtime: tokio::runtime::Handle,
    /// Read-through metadata + small-object cache (see Consistency).
    cache: Arc<S3Cache>,
    connection_string: String, // for remote_connection_info()
}
```

### Sync-over-async: the load-bearing bridge

The `FileSystem` trait is synchronous (`io::Result<T>`), the AWS SDK is
async. This is the *exact* problem `RemoteFileSystem` already solved:
`AgentChannel` stores a `tokio::runtime::Handle` and exposes
`request_blocking(...)` = `handle.block_on(self.request(...))`
(`services/remote/channel.rs:471`). We copy that move verbatim:

```rust
impl S3FileSystem {
    fn block<F: Future>(&self, fut: F) -> F::Output {
        self.runtime.block_on(fut)
    }
}
```

Every trait method is `self.block(async { ...sdk calls... })`. The
editor already calls filesystem methods off the UI thread for anything
that can be slow (load/save/walk run on worker threads); the same
threads that today block on an SSH round-trip will block on an S3
round-trip. No new concurrency model.

> Caveat to verify in implementation: `block_on` must not run on a
> thread that's already inside the runtime's worker pool, or it panics.
> `RemoteFileSystem` is called from blocking worker threads / dedicated
> task contexts; `S3FileSystem` inherits the same call sites. Where a
> call originates on a runtime worker, route through `spawn_blocking`
> (the `FileSystemExt` async methods give us the seam to do this cleanly
> for the async-native call sites).

### Trait method → S3 mapping

The interesting third of the surface. The rest (`exists`, `is_dir`,
`is_file`, `metadata_if_exists`) compose from `HeadObject` / `ListObjectsV2`.

| `FileSystem` method | S3 realization | Notes |
|---|---|---|
| `read_file` | `GetObject` | Whole object into `Vec<u8>`. |
| `read_range(off, len)` | `GetObject` with `Range: bytes=off-off+len-1` | **The keystone.** Fresh's multi-GB lazy loading is built on `read_range`; S3 ranged GET is a first-class match. This is the method that makes "open a 4 GB log out of S3" actually work without pulling the whole object. |
| `count_line_feeds_in_range` | ranged `GetObject` + count locally | Same as remote: stream the range, count `\n`. |
| `write_file` | `PutObject` (or multipart for large) | Whole-object replace. Atomic at the object level — a reader sees old or new, never torn. |
| `create_file` / `open_file_for_write` | buffered `S3FileWriter` → `PutObject`/multipart on `sync_all` | S3 has no streaming-append-to-key; the writer buffers (spilling to a temp file past a threshold) and uploads on flush. |
| `open_file_for_append` | read-modify-write, or **error** | S3 cannot append. v1: emulate via download-append-reupload for small objects; hard-error past a size cap. Documented sharp edge. |
| `open_file` (`FileReader: Read+Seek`) | lazy ranged reader | `Seek` becomes the next `Range` start; `Read` issues a ranged GET (with a read-ahead window). |
| `set_file_length` (truncate) | re-`PutObject` truncated content | No native truncate; read-modify-write. |
| `write_patched(src, dst, ops)` | `UploadPartCopy` + `UploadPart` | **Big win.** `WriteOp::Copy{offset,len}` → `UploadPartCopy` from the existing object (server-side, no download); `WriteOp::Insert{data}` → `UploadPart`. This is *the* method that makes saving a small edit to a huge S3 file cheap — exactly why the trait has `write_patched` for remote. 5 MB min-part-size rules apply; tiny ops fall back to whole-object PUT. |
| `rename(from,to)` | `CopyObject` + `DeleteObject` | Not atomic. A crash between leaves both. Acceptable for v1 (editor renames are user-driven, rare); flagged as a known non-atomicity. |
| `copy` | `CopyObject` | Server-side, returns size. |
| `remove_file` | `DeleteObject` | |
| `remove_dir` / `remove_dir_all` | `ListObjectsV2` + `DeleteObjects` (batched 1000) | "Directories" are prefixes; removal is bulk-delete of keyed objects under the prefix. `remove_dir` (non-recursive) errors if the prefix has nested keys, matching POSIX `ENOTEMPTY`. |
| `create_dir` / `create_dir_all` | **no-op** (or zero-byte `dir/` marker) | Prefixes spring into existence when a key is written. We optionally write a `prefix/` marker so empty dirs survive (S3 has no empty dirs otherwise), matching what the explorer expects to see. |
| `read_dir` | `ListObjectsV2` with `Delimiter: "/"` | `CommonPrefixes` → subdirectories, `Contents` → files. One page = one level. |
| `walk_files` | `ListObjectsV2` paginated, no delimiter | Flat recursive listing; apply `skip_dirs` against key prefixes; honor the `AtomicBool` cancel between pages. Fast: S3 list is the natural recursive walk. |
| `metadata` / `symlink_metadata` | `HeadObject` | `size` from `ContentLength`, `modified` from `LastModified`. No symlinks → `symlink_metadata == metadata`. |
| `set_permissions` | **no-op** + debug log | No POSIX mode in S3. Like docker ignoring `process_limits`, we log-and-ignore rather than error, so callers in the save path don't blow up. |
| `canonicalize` | normalize `.`/`..` in the key lexically | No real path resolution; pure string normalization against `prefix`. |
| `current_uid` / `is_owner` / `is_writable` | `0` / `true` / bucket-writability probe | No ownership; "writable" = we hold `s3:PutObject` (best-effort, can cache a single probe). |
| `search_file` | ranged streaming scan | Reuse the generic `search_file` scanner over a ranged reader; no server-side grep. |
| `sudo_write` | **error** | No privilege escalation against an object store. |
| `home_dir` | `prefix` root | There is no `$HOME`; the workspace root is the only meaningful anchor. |
| `temp_path_for` / `unique_temp_path` | sibling key `…/.fresh-tmp-<rand>` | Save path writes a temp object then `rename` (copy+delete) into place — same save dance the trait expects, just non-atomic at the final step. |
| `remote_connection_info` | `Some("s3://bucket/prefix")` | Drives the status bar, like SSH's `user@host`. |
| `is_remote_connected` | client/credential liveness | `true` unless creds expired / a probe failed. |

`FileReader`/`FileWriter` get S3-backed impls:

```rust
/// Lazy, seekable reader. `Read` issues ranged GETs with a read-ahead
/// window; `Seek` just moves the next-range cursor. No whole-object
/// buffering, so a `BufReader` over this streams a 10 GB object in
/// bounded memory — the property the whole editor depends on.
struct S3FileReader { fs: Arc<S3FileSystem>, key: String, pos: u64, len: u64, readahead: Bytes }

/// Buffered writer. Accumulates in memory, spills to an on-disk temp
/// past `SPILL_THRESHOLD`, and on `sync_all()` does a single `PutObject`
/// (small) or multipart upload (large). Drop-without-sync aborts any
/// in-flight multipart so we don't leak parts (billable).
struct S3FileWriter { fs: Arc<S3FileSystem>, key: String, buf: SpillBuffer, upload_id: Option<String> }
```

### Consistency & caching

S3 is now strongly read-after-write consistent for new objects and
overwrites, which removes the historical "I just wrote it and a read
404s" class of bug. What remains:

- **Latency, not correctness.** Every metadata call is a network round
  trip. The explorer stats hundreds of entries; the file watcher polls.
  An `S3Cache` keyed by `key → (ETag, ContentLength, LastModified)` with
  a short TTL (and ETag revalidation on read) keeps the UI responsive.
  `read_dir` results cache per-prefix per-page.
- **No file watcher.** S3 has no inotify. We do not get push
  notifications when the *pod* changes a file. v1 ships a polling
  watcher (re-`HeadObject` on a timer for open buffers; re-list on
  explorer focus) and documents that external changes appear on poll,
  not instantly. (S3 Event Notifications → SQS is a possible v2 push
  path; out of scope here.)
- **Mount visibility.** The pod's Mountpoint-for-S3 view caches too;
  a byte the editor writes is visible to a *new* open in the pod, not
  to a process already holding the file. This is the one place the
  two-namespace model leaks, and it's a doc note, not code.

## Half two: `EksExecSpawner`

New module `crates/fresh-editor/src/services/cloud/eks/spawner.rs`. It is
the direct analogue of `docker_spawner.rs` — same two traits, same
"core never names the backend" rule, same build-the-argv-once helper
shared between one-shot and long-running paths. Where docker composes
`docker exec …`, this composes a Kubernetes streaming-exec request.

```rust
pub struct EksTarget {
    pub cluster: String,       // for kubeconfig context / display
    pub namespace: String,
    pub pod: String,
    pub container: Option<String>, // -c <container> when the pod is multi-container
    pub workspace: Option<String>, // default cwd (the mount path)
}

pub(crate) struct EksExecSpawner {
    target: EksTarget,
    base_env: Vec<(String, String)>, // captured env probe, like docker's userEnvProbe
    transport: EksExecTransport,      // see "Transport choice" below
    trust: Arc<WorkspaceTrust>,
}
```

Both `ProcessSpawner` and `LongRunningSpawner` are implemented exactly as
docker does:

- `ProcessSpawner::spawn` → `gate(&trust, &command, cwd)?`, then a
  one-shot exec, collecting stdout/stderr/exit-code into `SpawnResult`.
- `ProcessSpawner::spawn_to_file` → error (`"stdoutTo is not supported
  for eks-exec processes"`), matching docker's stance.
- `LongRunningSpawner::spawn_stdio` → an exec with stdin/stdout/stderr
  streamed, wrapped in a `StdioChild` built with `spawned_locally =
  false` so host-side cgroup/rlimit application is skipped (the wrapper
  PID isn't the in-pod process). `ProcessLimits` are *log-and-ignore*
  with the same reasoning docker uses — host rlimits don't reach into a
  pod (and real limits belong on the pod spec, set at schedule time, not
  per-exec).
- `LongRunningSpawner::command_exists` → exec `sh -c 'command -v <cmd>'`
  in the pod, success = present. Same POSIX `command -v` choice and same
  `shell_quote` helper docker uses, and for the same reason: the probe
  must see the pod's `$PATH`, not the host's, or the LSP popup lies.

### Transport choice: `kubectl exec` vs. the k8s API

Two ways to drive an exec; the design supports both behind
`EksExecTransport`, defaulting to the first:

1. **`kubectl exec` subprocess** (default, v1). Compose
   `kubectl --context <ctx> exec [-i] -n <ns> [-c <c>] <pod> -- <cmd> args…`
   and run it with `tokio::process::Command`, *exactly* the
   `Command::new("docker")` shape in `docker_spawner.rs`. Pros: trivial,
   reuses the host's kubeconfig/IRSA/aws-iam-authenticator credential
   chain, no new heavy dependency, identical `StdioChild::from_tokio_child`
   wiring. Cons: requires `kubectl` on the host; cwd via
   `-- sh -c 'cd <ws> && exec "$@"'` since `kubectl exec` has no `-w`.
2. **`kube`/`kube-rs` API streaming exec** (v2 option). Direct SPDY/WS
   exec against the API server. Pros: no `kubectl` dependency, finer
   control over streams/TTY/resize. Cons: a real dependency, its own
   auth plumbing, more surface. Deferred.

Defaulting to `kubectl exec` keeps v1 a near-clone of the proven docker
path. The `build_exec_args` helper is the only genuinely new code, and
it mirrors `DockerExecSpawner::build_exec_args` one-to-one (flags before
the pod name, command and args after the `--`).

### Terminal wrapper

```rust
impl TerminalWrapper {
    /// Open the integrated terminal as an interactive shell inside the
    /// pod: `kubectl exec -it -n <ns> [-c <c>] <pod> -- sh -c
    /// 'cd <ws>; exec "$SHELL" -l'`. Pins cwd through its own args, so
    /// `manages_cwd = true` (the terminal manager must not hand a local
    /// PTY a pod-side cwd it can't honour) — same rule as the SSH and
    /// docker wrappers.
    pub fn eks(target: &EksTarget, mount_path: &str) -> Self { /* … */ }
}
```

## Credentials, auth, and trust

Three credential surfaces, kept out of core:

- **S3 access**: standard AWS credential chain via `aws-config`
  (env, profile, SSO, IRSA/web-identity if the editor itself runs in a
  pod). The `S3FileSystem` holds a resolved `aws_sdk_s3::Client`. Token
  refresh is the SDK's job; on a hard auth failure `is_remote_connected`
  flips false and the status bar annotates it (mirroring SSH
  disconnect).
- **EKS access**: the host kubeconfig context (which for EKS typically
  shells out to `aws eks get-token` / `aws-iam-authenticator`). With the
  `kubectl exec` transport we inherit this for free — `kubectl` resolves
  it the same way a human operator's would.
- **WorkspaceTrust** is unchanged and mandatory. Every `spawn` /
  `spawn_stdio` / `command_exists` goes through `gate(&trust, …)?` just
  like local and docker. Pointing the editor at a cloud pod does *not*
  bypass command gating; if anything the bar is higher and trust starts
  untrusted until the user accepts.

No credentials live in the `AuthorityPayload`. The payload names a
bucket, a prefix, a cluster/namespace/pod, and a mount path — it
references *what*, never *secrets*. The credential chain is resolved
host-side at construction. This matters because payloads are plugin-
authored JSON and may be logged.

## Plugin API: payload additions

Two new tagged variants, purely additive (serde tagged-enum, old
payloads keep parsing — principle from `AUTHORITY_DESIGN.md`):

```rust
pub enum FilesystemSpec {
    Local,
    /// S3-backed workspace. `region` optional (SDK resolves a default).
    /// `prefix` is the key the workspace root maps to.
    S3 {
        bucket: String,
        #[serde(default)] prefix: String,
        #[serde(default)] region: Option<String>,
        #[serde(default)] endpoint: Option<String>, // for S3-compatible / LocalStack
    },
}

pub enum SpawnerSpec {
    Local,
    DockerExec { /* … unchanged … */ },
    /// Exec into a pod on an EKS (or any kube) cluster.
    EksExec {
        context: Option<String>,   // kubeconfig context; None = current
        namespace: String,
        pod: String,
        #[serde(default)] container: Option<String>,
        #[serde(default)] workspace: Option<String>, // default cwd = mount path
        /// Captured in-pod env probe (PATH/HOME/LANG/…), same role as
        /// docker-exec's `env`. Empty when no probe was run.
        #[serde(default)] env: Vec<(String, String)>,
    },
}
```

`AuthorityPayload` itself is untouched — it already carries
`filesystem`, `spawner`, `terminal_wrapper`, `display_label`, and the
optional `path_translation` we need for the bucket-prefix↔mount-path
mapping. The plugin fills `path_translation` with
`{ host_root: prefix, remote_root: mount_path }`.

`Authority::from_plugin_payload` grows two match arms (S3 filesystem,
EksExec spawner) and nothing else changes. All translation from "kind +
params" to `Arc<dyn …>` stays in that one function, per the contract.

Example payload:

```jsonc
{
  "filesystem": { "kind": "s3", "bucket": "acme-src", "prefix": "proj/", "region": "us-east-1" },
  "spawner": {
    "kind": "eks-exec",
    "context": "arn:aws:eks:us-east-1:…:cluster/dev",
    "namespace": "dev",
    "pod": "fresh-workspace-7c9f",
    "workspace": "/workspace",
    "env": [["PATH", "/home/dev/.local/bin:/usr/bin"], ["LANG", "C.UTF-8"]]
  },
  "terminal_wrapper": {
    "kind": "explicit",
    "command": "kubectl",
    "args": ["exec","-it","-n","dev","fresh-workspace-7c9f","--","sh","-lc","cd /workspace; exec \"$SHELL\" -l"],
    "manages_cwd": true
  },
  "display_label": "eks:dev/fresh-workspace-7c9f · s3://acme-src/proj",
  "path_translation": { "host_root": "proj/", "remote_root": "/workspace" }
}
```

## End-to-end, the plugin-owned lifecycle

Mirrors the devcontainer flow (`AUTHORITY_DESIGN.md` §"Devcontainer,
end-to-end"). A `plugins/eks-workspace.ts` plugin owns the cloud
lifecycle; core only holds the slot.

1. Editor boots `Authority::local()`.
2. Plugin sees an `.fresh/eks-workspace.json` (or a command palette
   "Open S3 workspace…"), shows an "Attach?" action, remembers the
   answer keyed by `getCwd()`.
3. On attach, the plugin does host-side work via
   `editor.spawnHostProcess(...)`: ensure the pod exists (apply a Job/
   Pod manifest that mounts the bucket via the S3 CSI driver at
   `/workspace`), wait for `Ready`, and run an env probe
   (`kubectl exec … -- sh -lc env`) to capture the in-pod `PATH`/etc.
4. Plugin builds the `AuthorityPayload` above and calls
   `editor.setAuthority(payload)`.
5. Core stashes it, triggers the destructive restart, rebuilds the
   `Editor` around the new authority. The keepalive bundle (AWS config,
   the pod's liveness lease / a renewing annotation, the runtime) rides
   in `session_keepalive` so the daemon path (session mode) keeps it
   alive across the rebuild, exactly like SSH.
6. Plugin reloads with the cloud authority active; status bar shows the
   label.

Detach = `clearAuthority()` (+ optionally tear the pod down host-side).
Pod recycled out from under us = `is_remote_connected` goes false, the
plugin can re-attach (new pod) by issuing a fresh `setAuthority`.

## Module / crate layout

Keep it in `fresh-editor` under a new `services/cloud/` tree, parallel to
`services/remote/` and `services/authority/`:

```
crates/fresh-editor/src/services/cloud/
├── mod.rs                  // re-exports; EksTarget; shared cache types
├── s3/
│   ├── filesystem.rs       // S3FileSystem : FileSystem
│   ├── reader.rs           // S3FileReader : FileReader (lazy ranged)
│   ├── writer.rs           // S3FileWriter : FileWriter (buffered/multipart)
│   └── cache.rs            // S3Cache (ETag/metadata/listing)
└── eks/
    └── spawner.rs          // EksExecSpawner : ProcessSpawner + LongRunningSpawner
```

`authority/mod.rs` gains `Authority::eks_s3(...)` and the two
`from_plugin_payload` arms; `authority/docker_spawner.rs` stays the
template the EKS spawner is modeled on. New deps (feature-gated behind a
`cloud` cargo feature so the default build stays lean): `aws-config`,
`aws-sdk-s3`, `bytes`. The EKS half adds no Rust dep in v1 (it shells out
to `kubectl`); the `kube-rs` transport, if built, goes behind a further
`eks-api` feature.

## Testing strategy

- **S3 filesystem**: a `trait`-level conformance suite already implied by
  `FileSystem` — run the same battery against `StdFileSystem`,
  `RemoteFileSystem`, and `S3FileSystem` pointed at **LocalStack** (or
  MinIO via the `endpoint` override) in CI. Pure-unit tests for key
  mapping (`prefix` strip/join, `canonicalize` normalization,
  `read_dir` delimiter parsing) and for `write_patched` → multipart op
  planning need no network.
- **EksExec spawner**: unit-test `build_exec_args` the way docker does
  (assert flag/`--`/cmd ordering, `-c` container, env probe placement,
  cwd-via-`sh -c`) with zero cluster. Integration against a **kind**
  cluster behind a `cloud-it` feature/CI lane, not the default test run.
- **Authority**: extend the serde round-trip tests in `authority/mod.rs`
  with the two new payload variants (parse → construct → assert label,
  path_translation, terminal wrapper), matching the existing
  `payload_roundtrips_through_serde_json` style.

## What this is

- A full `Authority`: durable S3 object store for bytes + ephemeral EKS
  pod for execution, behind the unchanged three-slot contract.
- A faithful reuse of two proven patterns: `docker_spawner.rs` (the
  spawner half) and `AgentChannel`'s block-on bridge (the filesystem
  half).
- Path-translated, mount-based: the pod and the editor see one bucket;
  we translate paths, we don't sync bytes.

## What this is not

- **Not a file sync engine.** No pod-local scratch disk reconciled
  against S3. The pod mounts the bucket; that's the whole story. A
  sync-based model is a separate, larger project.
- **Not a pod scheduler / cluster manager.** Provisioning the pod, the
  CSI mount, autoscaling, and teardown are the plugin's job (host-side
  `kubectl apply` / `spawnHostProcess`), never core's.
- **Not multi-bucket / multi-root.** One bucket+prefix, one pod, one
  authority (principle 5).
- **Not a credential store.** Payloads name resources, never secrets;
  AWS/EKS creds resolve through the host's standard chains.
- **Not atomic for rename/append/truncate.** S3's object model forbids
  it; these are read-modify-write or copy+delete with documented sharp
  edges, not engineered-around guarantees.
- **Not a live external-change feed.** No inotify over S3; v1 polls and
  says so.

## Open questions / risks

1. **`block_on` re-entrancy.** Must confirm every `S3FileSystem` call
   site is off a runtime worker, or thread the `FileSystemExt` async
   methods through `spawn_blocking`. Same risk class `RemoteFileSystem`
   already lives with — verify, don't assume.
2. **`open_file_for_append` cap.** Pick the size threshold past which
   append hard-errors vs. read-modify-write. Log-and-recover where the
   save path can tolerate it.
3. **Multipart minimums vs. `write_patched`.** S3's 5 MB minimum part
   size means small `Copy`/`Insert` ops can't each be a part; the op
   planner must coalesce or fall back to whole-object PUT below a
   threshold. Needs a real heuristic + tests.
4. **Pod liveness across a restart.** The destructive authority
   transition rebuilds the `Editor`; the pod and its lease must outlive
   that. Confirm the `session_keepalive` bundle is sufficient (it is for
   SSH) and define what "pod died mid-session" surfaces as.
5. **Mount visibility latency** between editor writes and pod reads —
   acceptable for v1? Decide the documented contract before shipping.
6. **`kubectl` dependency** on the host for v1. Acceptable, or do we
   need the `kube-rs` transport sooner than v2?
```
