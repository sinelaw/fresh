# Remote Authority & Workspace Trust

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

Purpose: the one map of *where the editor acts* (the `Authority` backend slot — local / SSH / docker-exec / kubectl-exec), how a remote backend is built, kept alive, and reconnected, and *whether code may run there* (Workspace Trust + the live env provider). Covers what is implemented today versus what is forward-looking design.

This doc is the as-built companion to the `Authority` abstraction design, the per-session-backends design, the K8s authority design, and the trust/env design set. Where those describe direction, this records what the tree actually does.

---

## 1. The Authority pattern

### 1.1 What it is

`Authority` is the single backend slot answering "where does this primitive run?" — file I/O, the integrated terminal, plugin `spawnProcess`, LSP server spawn, formatters, find-in-files, save, recovery, the file watcher. Everything routes through the active authority; **nothing in core branches on "is this SSH / a container."**

The struct carries:

- `filesystem: Arc<dyn FileSystem + Send + Sync>` — local `StdFileSystem` or `RemoteFileSystem`.
- `process_spawner: Arc<dyn ProcessSpawner>` — one-shot "run and collect" (plugin spawn, git, find-in-files).
- `long_running_spawner: Arc<dyn LongRunningSpawner>` — long-lived stdio children (LSP servers; future tool agents). Routing LSP through here is what gives a container authority in-container LSP with no special-case in `LspHandle`.
- `terminal_wrapper: TerminalWrapper` — how the integrated terminal is launched.
- `display_label: String` — status-bar/explorer label (empty = render nothing; SSH leaves it empty and lets the filesystem's `remote_connection_info()` annotate disconnect).
- `path_translation: Option<PathTranslation>` — host↔remote workspace path mapping (container only).
- `workspace_trust: Arc<WorkspaceTrust>` — mandatory execution gate, shared with every spawner.
- `env_provider: Arc<EnvProvider>` — live activated env, shared with every spawner.
- `command_wrap: CommandWrap` — how an interactive agent argv is composed to run *inside* the backend.

Constructors (all unit-testable in isolation): `Authority::local`, `Authority::ssh`, `Authority::kube` / `kube_from_connection`, `Authority::from_plugin_payload`.

### 1.2 Opaque-to-core, sole-router (load-bearing principles)

The four routing fields are the entire contract. The only code that names a backend by string ("docker", "ssh", "kubectl") is that backend's constructor and its spawner module; consumers never inspect the kind. This is enforced socially (the opaque-to-core principles in the `Authority` design) and partly by the type system — see §1.5.

### 1.3 `Authority` is **not `Clone`**, owned per `Window`

`Authority` deliberately does not implement `Clone`. It is *moved* between slots, never copied. A session's backend/trust/env therefore cannot leak into another window — the isolation is a compile-time fact, not a runtime check. The per-window field already exists: `WindowResources` holds an `authority`, exposed via `Window::authority()`.

### 1.4 Transitions are atomic and destructive (production path)

Installing a new authority does **not** swap fields in place. `Editor::install_authority(new)` stashes the replacement in `pending_authority` and calls `request_restart`; the standalone entry point or `EditorServer::rebuild_editor` (daemon) drops the entire `Editor`, rebuilds it, and reinstalls the new authority via `set_boot_authority` before plugins load. Every cached `Arc<dyn FileSystem>`, LSP handle, terminal PTY, and in-flight task dies with the old editor.

Rationale: an in-place swap would require enumerating every cache that closed over the old filesystem/spawner (open buffers, `FsManager`, Quick Open's `FileProvider`, `LspManager`, `TerminalManager`, the watcher, recovery, background tokio tasks). A miss manifests as "files save to the wrong place" — a trust-destroying bug class. The restart path already drops and rebuilds everything correctly, at a cost paid once per attach/detach (never per keystroke). The escape hatch to a no-restart swap lives at `install_authority`.

`change_working_dir` uses the same machinery to switch project roots — authority swap and project-root change are the same main-loop primitive (drop + rebuild) with different "what changed" semantics.

**Daemon (session mode):** `fresh --session` / `fresh server` runs a long-lived `EditorServer`. It must not exit on a transition or clients disconnect, so `rebuild_editor` mirrors the standalone restart: save workspace, drop the editor, swap `current_authority` and/or `working_dir`, rebuild, restore buffers, repaint clients. `EditorServerConfig` has a `startup_authority` slot and an opaque session-keepalive slot (SSH backs this with the runtime + `SshConnection` + reconnect task; dropping any one tears the remote session down).

### 1.5 Per-session activation primitive (partly landed)

A *no-restart, single-window* swap primitive has landed: `Editor::set_session_authority(window_id, authority)` swaps one window's authority and re-points that window's LSP, mirroring into the editor-wide cache only when it is the active window. Covered by e2e tests.

**Still gated (so production attach still uses the destructive restart):** live multi-session (the active session is pinned to the first window), per-window keepalives so a background window keeps its live backend, and cache-invalidation of buffers/terminals opened under the old authority. The forward-looking target ("one authority per `Session`, exactly one *active*; background sessions hold dormant authorities") is **direction, not fully shipped.**

### 1.6 `SessionScope` — minting trust+env together

`SessionScope { trust, env }` is the one blessed way to obtain per-session trust + env. `SessionScope::for_root(root, project_state_dir)` mints **fresh** handles owned by one session: a per-root `WorkspaceTrust` adopting that project's recorded level, and a per-session `EnvProvider` that restores a previously-activated recipe *only when trusted*. Move-only, consumed by `Authority::local_scoped`. This is what prevents "trust project A" from raising the live level project B's spawns are gated against.

---

## 2. `CommandWrap` and `TerminalWrapper` — composing into the backend

Two related seams decide how a command runs inside the active backend.

`TerminalWrapper` launches the integrated *shell*:

- `host_shell()` — detected shell, no args, `manages_cwd = false` (the terminal manager sets cwd itself).
- `ssh(params, remote_dir)` — `ssh -t … 'cd <dir>; exec $SHELL -l'`, `manages_cwd = true`.
- `kube(target, base_env)` — `kubectl exec -it … -- sh -lc 'export …; cd <ws>; exec $SHELL -l'`, `manages_cwd = true`.

A re-parenting wrapper pins cwd through its own args, so `manages_cwd = true`; `with_user_shell_override` is then a no-op for it (replacing the command would drop the re-parenting and spawn the user's shell on the *host*). Without the SSH/kube wrappers the embedded terminal silently ran on the **local** machine — that was the bug they fix.

`CommandWrap` generalizes this to an arbitrary interactive argv (e.g. `claude --resume <id>` from the Orchestrator agent-resume path). `Authority::terminal_command(argv)` dispatches:

- `Direct` (local) — argv is the PTY child; terminal sets cwd.
- `Prefix(vec)` (container) — `docker exec -it [-u][-w][-e] <id> <argv>`, argv appended verbatim (never a shell string), cwd pinned by `-w`.
- `Ssh { params, remote_dir }` — `ssh` has no cwd flag, so a `cd <dir>; exec <argv>` shell hop, argv POSIX-quoted.
- `Kube { target, base_env }` — same shell-hop, env exported first.

This composition seam is **shipped for local, container, SSH, and Kubernetes**; dedicated SSH/kube agent argv builders compose the interactive terminal command for each carrier.

---

## 3. The remote agent stack (SSH and Kubernetes)

SSH and Kubernetes share almost the entire remote stack; only the *carrier* differs. This is the load-bearing thesis of the K8s authority design: **the K8s authority is the SSH authority with the transport swapped from `ssh` to `kubectl exec`.** Everything above the channel is reused verbatim. Both are **shipped** (the K8s/kubectl-exec slice landed first as EKS, then generalized to any cluster).

### 3.1 The Python agent

A single embedded Python agent file, included at compile time as `AGENT_SOURCE`. The carrier streams it to a remote `python3` and the agent then reads JSON-lines protocol messages off stdin. **Zero install** — only `python3` is required on the far side (a missing-interpreter exit code from the bootstrap is detected and surfaced as "Python 3 was not found on the remote host…").

Bootstrap framing is byte-count based — `import sys;exec(sys.stdin.read(<agent byte length>))` — so it needs no remote shell, here-doc, or `bash`.

### 3.2 Protocol & channel

JSON-lines, short keys to save bandwidth: request `{id, m, p}`, response `{id, d?, r?, e?}`. A protocol version is checked in the ready handshake; mismatch is a hard error. Binary is base64.

`AgentChannel` multiplexes request/response over the carrier's stdin/stdout:

- Long-lived read and write tasks survive transport drops (`from_transport`).
- `request`/`request_streaming` register a pending entry keyed by id; streaming uses `send().await` for **backpressure** (a prior `try_send` silently dropped data).
- A bounded default request timeout; a timed-out request marks the channel disconnected so the reconnect path engages (a remote drop never freezes the UI forever).
- A stable per-channel `id` survives hot-swaps — a durable key mapping a reconnect event back to its window.
- `replace_transport` hot-swaps the reader/writer in place (write task first, then read task drains stale pending requests before flipping `connected` true) and fires `reconnect_notify`.

### 3.3 Carriers and connections

- **SSH**: `SshConnection::connect` spawns `ssh -o StrictHostKeyChecking=accept-new [-p][-i] <extra_args> user@host <python bootstrap>`. stderr is **piped, not inherited** — an inherited stderr would let `ssh` scribble "Could not resolve hostname" straight over the ratatui alternate screen; instead the most-specific stderr line is folded into the connection error (`ssh_eof_error`). `kill_on_drop(true)` covers a connect future cancelled mid-handshake. `ConnectionParams::parse` accepts `host`, `user@host`, `user@host:port`, and a leading `ssh://` (user optional).
- **Kubernetes**: the `RemoteTransport` trait supplies a configured carrier `Command`; `KubectlExecTransport` builds `kubectl [--context] exec -i -n NS [-c C] POD -- python3 -u -c <bootstrap>`. `KubeConnection::connect` bootstraps the same agent, discards carrier stderr (same alternate-screen reason), and starts the heartbeat. `KubeTarget` is `{context?, namespace, pod, container?, workspace?}`; `display()` yields `k8s:ctx/ns/pod[/container]`.

`bootstrap_agent` is the shared "spawn carrier, stream agent, await `ready`, check version" function for the transport seam; SSH keeps its own near-identical `establish_ssh_transport` (the transport seam was added additively so SSH stayed byte-for-byte unchanged — SSH may migrate onto it later).

### 3.4 `RemoteFileSystem`

`RemoteFileSystem` implements the synchronous `FileSystem` trait over the channel via `request_blocking` (the channel owns a runtime handle). Notable choices:

- `read_file`/`read_range` stream base64 chunks; `read_range` validates the exact byte count (matches local `read_exact`; a short read signals truncation/race).
- `walk_files` is a **server-side walk**: the agent walks the tree and streams batches of relative paths, processed as they arrive so memory stays bounded; dropping the receivers cancels the server.
- `write_patched` sends `Copy{offset,len}` / `Insert{data}` ops so a save is O(edits), not O(file) — the agent reassembles. Append and truncate are server-side atomic ops.
- Errors map remote strings back to `io::ErrorKind` (NotFound, PermissionDenied, …).
- The remote-specific `FileSystem` hooks — `remote_connection_info`, `is_remote_connected`, `remote_channel_id`, `remote_reconnect_notify` — are how core observes connection state without knowing the backend kind.

`SlowFileSystem` is a test-only decorator that injects per-op latency and counts calls (presets for slow-network / slow-disk), used to validate the editor never blocks the UI on slow I/O.

### 3.5 The spawners

Three spawner families implement `ProcessSpawner` (one-shot) and `LongRunningSpawner` (stdio), all gated by trust and carrying the env provider:

- **Local**: `LocalProcessSpawner` / `LocalLongRunningSpawner` run children directly via tokio, applying the captured env, honoring `ProcessLimits` (cgroups/rlimits) on local children. `spawn_cancellable` / `spawn_to_file` stream stdout to disk for huge outputs (e.g. a multi-megabyte `git show`).
- **SSH**: `RemoteProcessSpawner` routes one-shots through the agent channel's `exec`; `RemoteLongRunningSpawner` opens a **separate `ssh … <remote-cmd>` per LSP server** whose piped stdio *is* the remote process's stdio. A separate connection per server (rather than multiplexing through the agent) is a deliberate trade-off: the agent's one-shot `exec` can't keep a process alive with writable stdin, and abstracting the whole LSP I/O layer over the channel would be a far larger change. `StdioChild` carries `spawned_locally = false` so host-side resource limits skip themselves (the local PID is the `ssh` client, not the server).
- **docker-exec**: `DockerExecSpawner` / `DockerLongRunningSpawner` run `docker exec [-i] [-u][-w][-e KEY=VAL] <id> <cmd>`. `base_env` carries the container's captured `userEnvProbe` (notably `PATH`) injected on **every** exec, so a `pylsp` installed by `postCreateCommand` into `~/.local/bin` is actually discoverable. `command_exists` runs `command -v` *inside* the container so the LSP popup reflects the container's `PATH`, not the host's.
- **kubectl-exec**: `KubectlLongRunningSpawner` runs `kubectl exec -i … -- sh -c 'cd <dir>; exec env K=V… "$0" "$@"' <cmd> <args>`. `kubectl exec` has no `-w`/`-e` flags, so cwd and env are injected via the `sh -c` wrapper. Uses `-c` (not `-lc`) since env is replayed explicitly — avoids profile-sourcing latency/noise on the server's stdout. One-shot commands and the filesystem ride the agent channel (`RemoteProcessSpawner`/`RemoteFileSystem`); only the long-running path needs this separate carrier.

Env injection on backends that pass an argv array (SSH/docker/kube) uses `env K=V … cmd args` rather than a shell string (`env_wrap`); the SSH login-shell path uses `exec env … cmd` with POSIX shell-quoting (`build_remote_exec`).

### 3.6 Keepalive, heartbeat, and reconnection (all shipped)

A remote authority needs three live resources kept alive across the attach-time editor rebuild, owned in a keepalive bundle (`SshKeepalive`, `KubeKeepalive`): the carrier connection, the reconnect task, and a **dedicated tokio runtime**. The runtime is load-bearing — the agent channel's read/write tasks must *not* ride the editor's per-instance runtime, which is dropped during the attach restart; if they did, every file op would fail with "Channel closed" the instant the attach completed. `connect_ssh_authority` / `connect_kube_authority` bootstrap on a short-lived helper thread (because `block_on` can't run inside the caller's async context), hand back the live runtime, and park it in the keepalive. Both race the connect against an optional cancel signal so a hung handshake leaves no orphan child.

**Heartbeat:** a periodic `info` ping keeps an idle agent stream warm against ELB/NAT idle timeouts (on the order of minutes) that would otherwise silently drop the connection — the client never sees a FIN, so the *next* request just hangs. Holds only a `Weak` ref, so it self-terminates when the channel is dropped. `info` is handled by every agent version → no protocol bump. Shipped for both carriers.

**Reconnect:** a background task watches `channel.is_connected()`; on a drop it retries on a fixed interval, re-establishing the carrier and calling `replace_transport` to hot-swap. SSH re-runs `establish_ssh_transport`; K8s re-runs `kubectl exec` against the **same** target. Caveat: a pod reschedule/eviction changes the pod name, which the same-target reconnect does **not** re-resolve — the "resolve current pod" plugin callback is an open item (planned). Same-name reconnect still covers the common transient-drop / idle-blip case.

**Respawn embedded terminals on silent reconnect** (shipped): a hot-swap revives the *agent channel*, but an embedded terminal opened its **own** carrier (a separate `ssh -t` / `kubectl exec` PTY) which died with the old connection. `replace_transport` fires `reconnect_notify`; the editor forwards each notification to `AsyncMessage::RemoteReconnected` (event-driven, not polled). `Editor::detect_remote_terminal_reconnects` (a per-frame edge detector) is gated on a *live remote authority* (`remote_connection_info().is_some() && !is_remote_connected()`) rather than on the persisted spec kind — so plain `fresh ssh://…` windows revive too, not only Orchestrator-attached ones — and `respawn_terminals_through_authority` returns a revive count to avoid double-respawn. Covered by e2e tests.

---

## 4. Plugin-built backends, persistence, and reconnection specs

### 4.1 Plugin payloads

Three small plugin ops: `editor.setAuthority(payload)`, `editor.clearAuthority()`, `editor.spawnHostProcess(...)` (runs on the host regardless of the active authority — needed by a plugin to run `devcontainer up` *before* the authority it wants exists). `setAuthority` is fire-and-forget: the editor restarts before any follow-up code on its return could run.

`AuthorityPayload` is a tagged, additive shape: `filesystem` (currently only `Local` — containers bind-mount, so paths coincide), `spawner` (`Local` or `DockerExec { container_id, user?, workspace?, env }`), `terminal_wrapper` (`HostShell` or `Explicit`), `display_label`, and optional `path_translation`. `Authority::from_plugin_payload` is the *only* place "kind + params" becomes concrete `Arc<dyn …>`. serde's tagged-enum representation means old payloads keep parsing as new kinds are added.

Env is deliberately **not** expressed in `SpawnerSpec` — it is a live provider set via `setEnv`/`clearEnv`, because a serializable `setAuthority` payload cannot carry the live SSH `AgentChannel`. `SpawnerSpec` is for *backend selection* only.

### 4.2 Path translation (container only)

`PathTranslation { host_root, remote_root }` maps host-side buffer paths to their in-container mount path and back (`host_to_remote` / `remote_to_host`; `None` for paths outside the workspace, e.g. system headers). The primary consumer is **LSP URIs**: editor buffers are host-side, but the LSP server is across the mount and only knows the remote-side path. Translation happens at the URI boundary, so the editor keeps host paths internally and the server sees the paths it expects. Local and SSH leave this unset — they operate directly in one path space.

### 4.3 `SessionAuthoritySpec` — the persisted, rebuildable descriptor

`SessionAuthoritySpec` is the declarative, source-of-truth counterpart to the live (non-serializable) `Authority`, persisted in the per-dir workspace file so a backend survives an editor restart instead of degrading to local. Variants: `Local`, `Plugin(AuthorityPayload)` (devcontainer — only the owning plugin can re-run `devcontainer up`), `RemoteAgent(RemoteAgentSpec)` (SSH/Kubernetes — reconnectable from core). `Authority::session_spec()` derives the spec from the live authority's `command_wrap`, so a plain `fresh ssh://…` launch carries a real `RemoteAgent` spec (making persistence, the dormancy model, and manual reconnect all work) rather than the historical inert `Local` default. `RemoteTransportSpec` is `Ssh{user?,host,port?,identity_file?,remote_path?,extra_args}` or `KubectlExec{context?,namespace,pod,container?,workspace?}`; `RemoteAgentSpec` also carries `base_env`, plus `window`/`label`/`command` for born-attached Orchestrator windows.

The forward-looking Live/Dormant restore model (a dormant session runs a local placeholder authority "presented as its real backend, disconnected", reconnecting only on activation) is **partly landed** (terminal_command, per-session trust, per-session env shipped; reconnect-on-activate and warm-background-survives-restart still gated on live multi-session).

---

## 5. Workspace Trust — gating execution

### 5.1 The model (implemented)

A freshly opened project may contain attacker-controlled content that only matters when *executed* (`./.venv/bin/python`, a `.envrc`, build scripts, proc-macros). Trust is the single per-project gate. Three levels (`TrustLevel`):

- **Restricted** (the safe default): no repo-controlled code runs. A spawn whose **explicit executable path** resolves inside the workspace is refused; an ordinary bare-name spawn resolved via `$PATH` proceeds (git/rg work). Safe because no env manager activates under Restricted, so the repo's `bin/` is never on `$PATH`.
- **Trusted**: every spawn allowed.
- **Blocked**: every spawn fails.

The containment check (`decide_restricted`) lexically normalizes the candidate path (no filesystem touch, so it never blocks or fails on nonexistent paths) and tests it against both the given root and its canonical form (they differ across symlinks, e.g. `/tmp`→`/private/tmp`). A poisoned lock fails *open* (Restricted's job is to stop repo execution; a poisoned lock is an internal bug, not a hostile project).

### 5.2 The single choke-point

Every editor primitive that runs a child routes through the active authority's `ProcessSpawner` / `LongRunningSpawner`, and **every** spawner calls `gate(&trust, command, cwd)` at the top of each spawn method — local, SSH, docker, kube alike. That is the one place covering all callers with no per-caller cooperation. `spawnHostProcess` bypasses the authority spawner (it must run on the host), so it consults `WorkspaceTrust::decide` directly at its call site.

Trust is a mandatory `Arc<WorkspaceTrust>` on every spawner — no `Option`, no post-hoc decorator wrapping; a spawner literally cannot be built without it. It is the same `Arc` the server owns, so the command palette / status-bar pill mutate the level through it and every spawner sees it live with no rebuild (`set_level` takes effect on the next spawn).

### 5.3 Persistence and per-session trust

`TrustStore` is one small `trust.json` per project, inside the user's data dir at `<data_dir>/workspaces/<encoded-path>/` — **never inside the repo** (a repo must not vouch for itself), one file per project so concurrent `fresh` processes never contend. Writes are atomic (pid-tagged temp + rename). A corrupt file reads as "undecided" rather than crashing. `is_decided()` (recorded?) is distinct from `level()` (always concrete; undecided ⇒ Restricted) and drives whether to prompt.

`WorkspaceTrust::for_session` mints a per-session handle adopting the project's recorded level — so trusting one project never raises the live level another open session is gated against (the shared "remember this folder" registry is the per-project store itself). For an *undecided* folder with no executable-content markers it starts **Trusted** (a plain text/docs folder shouldn't block its own tooling); folders *with* markers stay Restricted until the user decides.

### 5.4 The open-time prompt (implemented; supersedes a stale doc banner)

`Editor::maybe_prompt_workspace_trust` is the single open-time trust prompt for every marker kind (env-shell, project manifests, devcontainer, .NET). It fires only for *undecided* projects, and only when `executable_content_markers` finds markers; an unmarked folder is silently trusted and persisted (no restricted chip, no question with no downside). The prompt is rendered by a bespoke security modal (radio group + descriptions, scrollable, "Quit" as the mandatory secondary at startup vs "Cancel" from the palette; no undecided outcome).

> Note: the sandbox design doc carries a "WIP: prompt disabled, defaults to Trusted" banner. That banner is **stale** — the code above shows the Phase-1 prompt is active and marker-gated. `executable_content_markers` lists env markers (from the single `default_env_detectors` list so trust and env activation can't disagree), project manifests whose LSP runs project code at load, devcontainer config, and `.sln`/`.csproj`/`.fsproj`. Detection is passive (a shallow scan; it never runs anything).

### 5.5 What trust does *not* yet do

The "prompt each time" sub-mode of Blocked (ask per spawn rather than fail) is unimplemented — Blocked currently always fails. It needs an async UI round-trip from the spawn site. The broader **queue-and-drain** elaboration (gate stays sync `Allow`/`Deny`; a `broadcast::Sender<TrustLevel>` re-triggers denied LSP/plugin/env work on trust change, rather than blocking the spawn site) is **planned** (Phase 2). The **Sandboxed** level (replace Restricted's syntactic gate with an ad-hoc container mounting only the project) is a **design proposal**: the enforcement core and per-project persistence exist; the containment model does not.

---

## 6. The live environment provider

The active environment is a **recipe** (`{snippet, dir}`), not a stored snapshot — re-evaluated on demand by running the snippet on the active backend's host and capturing the result, so it can never go stale. Shared and interior-mutable exactly like `WorkspaceTrust`: every spawner holds the same `Arc<EnvProvider>`; the env-manager plugin sets it via `editor.setEnv`/`clearEnv`; no authority rebuild.

Backend-agnostic by design: `EnvProvider::current(run)` builds the capture *script* (`cd <dir>; <snippet>; command env`) and hands it to a caller-supplied `run` closure that executes it on the right host — local `$SHELL -lc`, SSH agent `exec`, the per-server ssh, docker/kube exec. That closure **must** be a raw spawn that does not itself apply this provider's env, or capture would recurse. A content-hash cache over the env-input files (`.envrc`, `mise.toml`, …) keeps the common path free; correctness never depends on the cache.

For the integrated terminal (a synchronous, non-tokio portable-pty path) there is a blocking **delta** capture (`current_local_delta_blocking`): dump env baseline, print a sentinel, run the recipe, dump again, diff. The `EnvDelta` carries only what the recipe changed over a clean login shell — volatile bookkeeping (`PWD`, `SHLVL`) diffs out — so applying it to a child's env is shell-agnostic. SSH's terminal uses the same idea remotely via a base64-encoded python3 launcher (`ssh_remote_env_launcher`) so the env applies as *data* and survives any login shell's quoting (fish-safe), reusing the identical delta sentinel. This uniform `spawn(argv, cwd, env_delta, …)` direction is **core shipped; Local & SSH e2e-validated, Docker & Kube unit-tested but not e2e-validated.**

`EnvStore` persists the activated recipe (`env.json`, alongside `trust.json` in the project state dir, never in the repo). `for_session(dir, trusted)` restores a persisted recipe **only when trusted** — the env gate mirrors the spawn gate — so a trusted session boots already in its env with no auto-activation restart flicker; an untrusted session restores nothing.

---

## 7. Devcontainer & Kubernetes — implemented vs planned

### 7.1 Devcontainer (shipped as a plugin)

The devcontainer plugin owns the backend lifecycle; core owns the slot. Flow: boot local → plugin finds `.devcontainer/devcontainer.json` → one-shot "Attach?" (decision stored in plugin global state keyed by `getCwd()`, no re-prompt) → `spawnHostProcess("devcontainer", ["up", "--workspace-folder", cwd])` → parse the JSON result → build a `docker-exec` `AuthorityPayload` (filesystem `local`, since the workspace is bind-mounted so host/container paths coincide) → `setAuthority` → core restarts into the container authority. Detach is `clearAuthority`; rebuild is `up --remove-existing-container`. The `userEnvProbe` capture rides in `SpawnerSpec::DockerExec.env`.

The shipped-code gap analysis confirms the architectural divergences are **intentional**: not a remote extension host (the UI stays on the host; only spawned processes cross into the container, one-shot `docker exec`), and **paths are not translated for the filesystem** (bind-mount means they coincide; `remoteWorkspaceFolder` is passed as `-w`). Missing/planned (UX around the build lifecycle): image-pull/build/start state machine, live `devcontainer up` log streaming, cancel-in-flight, port-forwarding detection, auto-install of `customizations.*.extensions`; one flagged spec violation — `initializeCommand` is never invoked.

**Fake-devcontainer-CLI test harness** (shipped): pure-shell `fake-devcontainer` + `fake-docker` scripts prepended to `$PATH` so the plugin's `devcontainer --version` probe resolves the fake. `devcontainer up` streams fake build progress to stderr and emits the real `{outcome,containerId,remoteUser,remoteWorkspaceFolder}` JSON line on stdout; `docker exec` parses `-i/-t/-u/-w` then **runs the command on the host** (no real isolation). Failure-injection env knobs (`FAKE_DC_UP_FAIL`, `FAKE_DC_UP_HANG`, `FAKE_DC_UP_BAD_JSON`, `FAKE_DC_UP_NO_CONTAINER_ID`, …) exercise the editor's attach/lifecycle/log/cancel/parse-failure paths with no Docker daemon. Explicit boundary: it does **not** test in-container PATH/LSP/file-access (since `docker exec` runs on the host) — "all green under the fake" is not "ships."

### 7.2 Kubernetes (transport + plugin shipped; storage & warm sessions planned)

**Shipped:** the `kubectl exec` transport, `KubeConnection`, `connect_kube_authority` + `KubeKeepalive`, `KubectlLongRunningSpawner`, the heartbeat + reconnect tasks, `attachRemoteAgent` wired end-to-end, the K8s-workspace plugin's provider model, and a fake-kubectl e2e harness. v1 deliberately depends on the **`kubectl` binary** (vs `kube-rs` WebSocket exec) — it buys TTY-resize and the SPDY→WebSocket transition for free. The plugin owns pod provisioning via a `Provider` contract (`attach-existing` / `manifest` / `run` / `command`) and three preflight checks (python3 present, `create` on `pods/exec`, not on Fargate); core does only the agent bootstrap + attach.

**Planned / design-doc only:**

- **Storage:** live tier **EBS GP3**, durable tier **S3 reached by *syncing*** (preStop hook + periodic), restored via initContainer. S3 is *never* a live mount: Mountpoint-for-S3 forbids non-replacing `rename`, so Fresh's temp-write-then-rename save would fail on every save; EFS is much slower on small files. Fresh imports no AWS crate — zero core change. **No shipping code exists** for this; it is purely design.
- **Reconnect after pod reschedule** (the pod-name re-resolution callback) — load-bearing for Spot interruptions, not yet wired.
- **Warm background cloud sessions:** a Cloud Workspace *is* an Orchestrator `Session` whose `Authority` is the kube remote-agent authority, surfaced via a generic optional "remote facet" on the session row (state glyph, rough `$/hr`, idle timer) — no separate panel, zero cost for local-only users. Keeping background sessions warm (heartbeat for *every* warm session, instant switch-back, a configurable warm-set cap) is what forces the per-session-authority refactor (§1.5) — **the main thing still gated.**

---

## 8. Quick implemented-vs-planned table

| Capability | Status |
| --- | --- |
| `Authority` single slot, opaque-to-core, non-`Clone`, per-`Window` field | Implemented |
| Destructive restart transition (standalone + daemon) | Implemented |
| `CommandWrap` / `terminal_command` (local/docker/ssh/kube) | Implemented |
| SSH authority: agent, `RemoteFileSystem`, spawners, terminal | Implemented |
| kubectl-exec authority (transport, connection, spawner, keepalive) | Implemented |
| docker-exec authority (plugin-built, `userEnvProbe` env) | Implemented |
| Heartbeat + reconnect task (SSH + kube) | Implemented |
| Respawn embedded terminals on silent reconnect | Implemented |
| `SessionAuthoritySpec` persistence + reconnect rebuild | Implemented |
| Path translation for container LSP URIs | Implemented |
| Workspace Trust (3 levels, choke-point gate, per-project store) | Implemented |
| Per-session trust + env (`SessionScope`, `for_session`) | Implemented |
| Open-time trust prompt (marker-gated, single prompt) | Implemented |
| Live env provider + delta capture (local/SSH) | Implemented |
| `set_session_authority` no-restart single-window swap | Partial (gated on live multi-session) |
| Live multi-session / warm background sessions / per-window keepalive | Planned |
| Reconnect after pod reschedule (pod-name re-resolution) | Planned |
| K8s EBS-live + S3-sync storage | Planned (design only) |
| Sandboxed trust level (ad-hoc container) | Planned (design only) |
| Trust queue-and-drain (`broadcast` re-trigger) | Planned (Phase 2) |
| Blocked "prompt each time" audit sub-mode | Not implemented |
| Devcontainer build-lifecycle UX (logs/cancel/state machine/ports) | Planned |

---

## 9. Superseded / consolidated docs

This doc consolidates the as-built picture from the following design notes. Several are aspirational or partly stale; flagged accordingly.

- The `Authority` abstraction design — as-built for the abstraction itself; its "Evolution: per-session authority" section is **design direction, not fully shipped** (this doc records what landed).
- The per-session-backends design — "design target, partially landed"; terminal_command / per-session trust / per-session env shipped, reconnect-on-activate + warm background **planned**.
- The K8s authority design — transport/bootstrap/heartbeat/reconnect-task **shipped**; storage (EBS+S3) and warm-session story **design only**.
- The K8s-workspace UX and plugin designs — **forward-looking** product/plugin framing (provider model slice shipped; warm sessions / cost facet planned).
- The devcontainer plugin design — predates the implementation; the plugin has since **shipped** (see the gap-analysis).
- The devcontainer spec gap-analysis / remediation plan — audit of **shipped** code + a remediation tracker (most fixes landed; lifecycle UX still planned).
- The fake-devcontainer-CLI note — the test harness, **shipped**.
- The SSH remote-editing design — **pre-implementation blueprint**; the SSH stack it describes has since shipped.
- The remote-env-manager design — Done/Remaining split; trust + SSH-LSP + `EnvProvider` + `setEnv` **shipped (local & SSH)**; its three-level "Restricted" middle is **partly superseded** by the trust-sandbox design.
- The remote-filesystem-optimization note — proposal; the delta-patch / server-side-walk / streaming pieces it weighs are **implemented** in `RemoteFileSystem`, though the doc itself reads aspirational.
- The async-remote-file-explorer design — the concrete async/timeout fix design; the bounded channel timeout + non-blocking explorer are **implemented**.
- The uniform-env-activation design — "design + in-progress" but core **shipped & validated** for Local/SSH; Docker/Kube unit-tested, **not e2e-validated**.
- The workspace-trust-sandbox design — **design proposal** for the Sandboxed level; its "prompt disabled / defaults to Trusted" status banner is **stale** (the marker-gated open-time prompt is active).
- The trust/env/devcontainer UX plan — Phase-1 plugin/prompt UX **done**; queue-and-drain, per-marker hashing, clickable chips, action-log panel **Phase 2 / planned**.
