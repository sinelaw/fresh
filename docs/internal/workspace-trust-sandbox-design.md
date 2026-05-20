# Workspace Trust: Sandboxed Execution Model

Status: design proposal. Supersedes the fuzzy "Restricted" middle level
sketched in `remote-env-manager-design.md` §Threat model with a containment
model. The enforcement core (`crates/fresh-editor/src/services/workspace_trust.rs`)
and per-project persistence already exist; this doc specifies the level *semantics*
we want to grow into and the cross-platform constraints that shape them.

## The reframe: from classification to containment

Workspace Trust gates one thing: a freshly-opened project may contain
attacker-controlled content that only becomes dangerous when **executed**
(a `.envrc`, a repo-placed `./.venv/bin/python`, `dotnet restore` evaluating a
malicious `.csproj`, an npm `preinstall`, a Cargo `build.rs`, `git` reading a
repo-set `core.pager`/hook).

The current "Restricted" level tries to answer *"which spawns are dangerous?"*
syntactically — allow bare `$PATH` names, refuse executables resolved inside the
workspace. That question is **not precisely answerable**:

- Bare-name build drivers (`dotnet`, `make`, `npm`, `cargo`, `gradle`, `mvn`)
  resolve via `$PATH` yet read repo-controlled config and run arbitrary code —
  they slip through the bare-name allowance. This is the reported
  "opening a `.cs` file runs project commands" bug.
- Even an unambiguously "system" tool can execute repo content via config it
  auto-reads (`git` + repo `.git/config` `core.fsmonitor`/`core.pager`/hooks —
  the CVE-2022-24765 class). So no allow-list of "safe" tools is actually safe.
- A denylist of known drivers fails **open** for the next driver we forget;
  an allowlist of safe tools is wrong wherever a "safe" tool reads repo config.

The escape is to stop classifying spawns and instead **contain** them. We don't
need to know whether `dotnet restore` is dangerous if it runs in a throwaway
sandbox with no access to the host filesystem, the user's credentials, or
(optionally) the network. This converts an unsolvable *classification* problem
into a tractable *containment* one, and it matches how CI systems and
sandbox-first editors (Zed's WASM extension host) treat untrusted code.

## Three trust levels

The axis becomes **where** code runs, not **which** spawns are permitted:

1. **Blocked** — nothing runs. No spawn of any kind succeeds (optionally:
   prompt-per-spawn as an audit mode). For reading genuinely hostile code with
   zero process execution. Unchanged from today; always available.

2. **Sandboxed** (the new default, replacing today's Restricted) — everything
   runs, but inside an **ad-hoc container** that mounts the project and nothing
   else sensitive. Build drivers, env activation, LSP servers, analyzers all
   execute; the blast radius is the container, not the host. Repo-controlled
   code cannot read `~/.ssh`, `~/.aws`, host env secrets, or the host
   filesystem outside the mounted project.

3. **Trusted** — everything runs **on the host**, with the user's real
   environment, credentials, and tools. The user has vouched for the project.

The trust prompt on first open of an undecided project becomes a clean
trichotomy: *Open sandboxed (default) / Trust on host / Block*. "Sandboxed"
being the default means the safe choice is also the *usable* one — unlike a
gate that silently suppresses tooling and leaves the user wondering why their
LSP is dead.

> Note: containment protects the **host**, not the repo. The project tree is
> mounted read-write (you are editing and building it), so repo code can still
> mutate the repo and reach whatever we mount or leave reachable. That is
> acceptable — the repo is already attacker-controlled — but it is why network
> policy and mount scope (below) matter, and why **Blocked** must remain for the
> "zero execution" case.

## The Sandboxed level in detail

### Ad-hoc container, config stored outside the project tree

When the user opens an undecided project and chooses (or defaults to) Sandboxed:

- If the repo **ships a `devcontainer.json`**, we do *not* trust it blindly — a
  committed devcontainer is itself repo-controlled content (its `build`,
  `postCreateCommand`, `features` run code). Under Sandboxed we may *use its
  image/toolchain hints* but run with our own hardened runArgs (no host mounts
  beyond the project, our network policy), ignoring lifecycle hooks unless the
  user escalates to Trusted. (Open question: how much of a shipped devcontainer
  to honor — see below.)
- If the repo ships **no** devcontainer, we synthesize one ad-hoc.

**The synthesized config never lands in the project tree.** Littering a
Fresh-specific `.devcontainer/` into every untrusted repo is exactly the
artifact-in-the-tree problem the env-manager design rejected for committed
config. Instead the generated Dockerfile/compose/runArgs live in the user's
data dir, keyed by canonical workspace path — the same place trust decisions and
per-workspace overrides already live (`<data_dir>/workspaces/<encoded-path>/`,
alongside `trust.json`). The repo stays clean; teammates on other editors see
nothing.

### Toolchain: derive per-project, do not ship one fat image

A single "comes with everything" base image (dotnet + node + python + go + rust
+ java + …) is the tempting version of "works for most projects," but it is the
wrong design:

- It is many GB, slow to pull on first open, and a maintenance burden (which
  versions? who updates it?).
- It **still won't match** the project's required versions — which is the entire
  reason `.tool-versions`/`mise`/`asdf`/`pyenv` exist. A repo pinned to
  Node 18 or .NET 6 gets the wrong runtime from a fat "latest" image.

Instead, **derive the toolchain from the project's own declared files**, in
priority order:

1. A shipped `devcontainer.json` image/features (used as a hint, hardened — see
   above).
2. Version pins the project already commits: `.tool-versions`, `mise.toml`,
   `.nvmrc`, `.python-version`, `global.json` (.NET SDK), `rust-toolchain.toml`,
   `.ruby-version`.
3. The manifest's ecosystem as a fallback (`package.json` ⇒ Node LTS,
   `*.csproj`/`*.sln` ⇒ matching .NET SDK, `Cargo.toml` ⇒ stable Rust,
   `pyproject.toml`/`requirements.txt` ⇒ Python, `go.mod` ⇒ Go).

Build a **minimal per-project image** from a small common base plus the detected
toolchain(s), cached by a content-hash of the inputs so the second open is
instant. This reuses the existing devcontainer build/cache machinery rather than
introducing a parallel one. A small curated base (shell, git, common build
essentials, the language runtime) covers the long tail; exotic native deps fall
to the user escalating to a shipped devcontainer or to Trusted.

### Mount scope

- **Project tree:** mounted read-write at a stable path (it is the thing being
  edited/built).
- **Nothing else by default.** No `~`, no `~/.ssh`/`~/.aws`/`~/.config`, no host
  Docker socket (a mounted socket is host root — never mount it under
  Sandboxed), no host env passthrough beyond a minimal safe set
  (`TERM`, `LANG`, …). Secrets and credentials stay on the host.
- Caches that speed builds without leaking host data (a per-project,
  container-local package cache volume) are fine; sharing the host's real
  `~/.cargo`/`~/.npm`/`~/.nuget` is not (a poisoned build could write into them
  and affect host/Trusted runs).

### Network policy (must be explicit)

Default-deny network is the safest, but it breaks `npm install`/`dotnet
restore`/`go mod download`/`cargo fetch` — which is most of why anyone runs a
build at all. Options, to be decided per the security bar we want:

- **Default-deny, explicit allow.** Strictest; the user opts a Sandboxed project
  into network when a build needs to fetch deps. Most secure, most friction.
- **Egress-only to package registries** (an allowlist of known registry hosts).
  A middle ground; meaningful but a maintenance/accuracy burden.
- **Default-allow egress, no inbound.** Lowest friction; accepts that a
  malicious build can exfiltrate over the network. Weakest of the three.

Recommendation: **default-deny with a one-click "allow network for this
project's builds"** surfaced exactly when a sandboxed spawn looks like a
dependency fetch — so the common case (read + light build of trusted-enough
code) stays offline, and enabling network is an explicit, per-project, informed
choice.

## The no-runtime fallback (required)

Sandboxing needs a container runtime, and one is **not always present**: SSH
sessions to bare hosts, machines without Docker/Podman, locked-down corporate
environments, native Windows without WSL. Sandboxed **cannot** silently
degrade to "run on host" when there's no runtime — that reopens the exact hole
we are closing.

So when no runtime is available, "Sandboxed" degrades to one of:

- **A syntactic gate on the host** (the fallback): deny-by-default for any spawn
  whose cwd resolves inside the workspace *or* whose path arguments point into
  it, with a deliberately tiny allowlist of genuinely inert tools. This is the
  "Design B" fail-safe posture — weaker than a container (a host spawn can still
  reach host resources) but better than nothing, and fail-safe for unknown
  tools. It needs the async "prompt/allow-once" plumbing noted as not-yet-built
  in `workspace_trust.rs`.
- **Or fall back to Blocked** and tell the user plainly that sandboxing is
  unavailable here, so they can choose Trusted consciously.

Either way the rule holds: **no environment silently runs untrusted code on the
host.** The UI must state which mode is actually in effect ("Sandboxed via
Docker" vs "Restricted (no container runtime — host gate)" vs "Blocked").

## Cross-platform

The runtime must work on **macOS, Linux, and Windows/WSL2** at minimum.

### Linux
Native containers (Docker or Podman, rootless preferred). Fast, cheap, good
bind-mount performance. The reference platform. Rootless Podman is attractive
because it avoids a root daemon and reduces the "container escape ⇒ host root"
surface.

### macOS
Containers run inside a Linux VM (Docker Desktop / Podman Machine / Colima /
Lima). Works, but two costs:

- **Bind-mount performance** is the known pain point (the VM boundary;
  gRPC-FUSE vs virtiofs). Mounting a large repo can be slow. Mitigations:
  prefer virtiofs where available; consider a build-cache volume inside the VM
  rather than crossing the bind mount for hot paths.
- **First-run weight:** the VM must be up. Spin-up latency argues for lazy
  start (don't boot a VM just because a folder was opened to read — start on
  first *execution*).

We should detect any of the common providers rather than hard-requiring Docker
Desktop (license considerations too): probe for `docker`, then `podman`, then a
Colima/Lima-managed socket.

### Windows + WSL2
WSL2 is a real Linux kernel, so Linux containers run well via Docker Desktop's
WSL2 backend or `docker`/`podman` installed inside a WSL2 distro. Treat
"Windows + WSL2" as effectively the Linux path: the project should be opened
from within the WSL2 filesystem for good mount performance (mounting across the
`/mnt/c` Windows↔WSL boundary is slow, the macOS-bind-mount problem again). When
Fresh runs inside WSL2 it is just Linux; when Fresh runs as a native Windows
process talking to a WSL2 docker, the path translation (`C:\…` ↔ `/mnt/c/…`)
must be handled.

### Native Windows (no WSL)
This is the uncertain one, and we should be honest about it:

- **Linux containers without WSL2 are not really available** — Docker Desktop's
  Hyper-V backend still runs a Linux VM, but increasingly the supported path *is*
  WSL2. Without virtualization (corp policy disabling Hyper-V/WSL), there is no
  Linux-container option at all.
- **Windows containers** exist but are a different world: they only run Windows
  base images (can't run a Linux toolchain image), come in process-isolation vs
  Hyper-V-isolation flavors, and are not a realistic target for sandboxing a
  Node/.NET-on-Linux project.

Pragmatic stance for native Windows: **require WSL2 for the Sandboxed level**,
and where WSL2 is genuinely unavailable, fall back to the host syntactic gate or
Blocked (the no-runtime path above), with the UI saying so. We are not going to
build a bespoke Windows sandbox; the honest options on locked-down Windows are
"gate on host" or "Block." (A future, heavier option — a microVM such as
Firecracker, or gVisor — is out of scope and also Linux/KVM-bound.)

## Remote (SSH) + Sandboxed: the composition problem

Running an editor against an SSH host *and* sandboxing untrusted code means the
container must come up **on the remote host** (where the repo lives), not on the
user's local machine. The current `Authority` model does not support this, and
it's worth being precise about why.

### What the code does today

- An `Authority` holds **one** `process_spawner` and **one**
  `long_running_spawner` (`authority/mod.rs:245`). They are chosen exclusively:
  an SSH authority wires the SSH-routed spawners; a container authority wires
  the docker-exec spawners. There is no nesting and no decorator — the spawner
  is a single `Arc<dyn ProcessSpawner>`.
- `DockerExecSpawner` runs `Command::new("docker")` directly
  (`docker_spawner.rs:141`, `:256`, `:285`). It builds `docker exec …` argv and
  executes it **on whatever host the editor process runs on** — i.e. always the
  *local* docker daemon. There is no transport seam.
- `spawnHostProcess` — the op the devcontainer plugin uses to run
  `devcontainer up` (bring the container up) — hardcodes a local
  `TokioCommand::new(&command)` and deliberately *bypasses the active authority*
  (`plugin_dispatch.rs:2076`, `:2127`). So container **lifecycle** is local-only
  as well.

So an SSH authority + DockerExec spawner can't be expressed: the docker spawner
would talk to the *local* daemon, whose containers can't see the remote
filesystem, while the SSH filesystem points at the remote host. The two
backends are mutually exclusive, not stackable.

### Three ways to enable it

**Option A — transport-agnostic docker spawner (compose over the active backend; recommended).**
Refactor `DockerExecSpawner`/`DockerLongRunningSpawner` to delegate the *actual
run* to an `inner: Arc<dyn ProcessSpawner>` (and inner long-running) instead of
calling `Command::new("docker")`. The docker spawner's job becomes pure argv
rewriting — `(cmd, args, cwd)` → `("docker", ["exec", …], None)` — and it hands
that to `inner`. Then:
- `inner = LocalProcessSpawner` → container on the local daemon (today's behavior).
- `inner = RemoteProcessSpawner` (the SSH agent) → the agent runs `docker exec`
  **on the remote host**, so the container lives remote-side. This reuses the
  existing remote `exec` path and the `StdioChild`-over-SSH plumbing the LSP
  already relies on (`remote/spawner.rs`), so the long-running/`docker exec -i`
  case works the same way.

This keeps the single-spawner `Authority` contract intact — the composition is
internal to *building* the spawner, not a new field. It is the most
Fresh-idiomatic shape (a spawner decorator over the existing trait objects).

Two concrete blockers to fix alongside it:
1. **Container lifecycle must run remote-side.** `devcontainer up` / `docker
   run` currently go through `spawnHostProcess`, which is hardwired local.
   Either route lifecycle through the active authority's spawner, or redefine
   "host process" to mean *the backend's host* (the remote under SSH) rather
   than the local machine. The latter is the cleaner mental model and fixes a
   latent inconsistency regardless of sandboxing.
2. **Path translation gains a layer.** Today `PathTranslation` is a single
   host↔remote pair (`authority/mod.rs:67`). With SSH the editor's "host" paths
   are *already* remote-host paths (the FS is the SSH FS), so the container case
   is remote-host-path ↔ container-mount-path — likely still a single
   translation, but the derivation changes (the mount source is a remote path,
   resolved by the remote daemon). This needs to be worked through so LSP URIs
   still round-trip.

**Option B — `DOCKER_HOST=ssh://user@host` / docker context (local CLI, remote daemon).**
Keep the docker spawner running the *local* `docker` CLI but point it at the
remote daemon over SSH via `DOCKER_HOST`/a docker context. Smallest change to
the spawner itself, but: it requires a local docker CLI *and* local→remote
daemon SSH access (a different, sometimes awkward setup than the agent), the
workspace must still live remote-side (the daemon resolves bind mounts there),
and it introduces a *second* remoting mechanism parallel to the agent. Weaker
fit with the existing architecture; reasonable only if Option A's refactor is
too large to take on now.

**Option C — run the editor/agent itself on the remote (no nesting at all).**
If `fresh` runs *on* the remote host (the user runs it there, or a remote agent
process hosts the editor), then "local sandbox" on that machine *is* remote
sandbox from the user's point of view — docker is local to the agent and Option
A's local path just works. This sidesteps composition entirely but is a
different deployment model (editor-on-server vs editor-local-acting-on-remote).
Worth keeping in mind: for some remote workflows it is strictly simpler than
nesting, and it may already be a supported mode.

### Recommendation

Pursue **Option A**: make the docker spawner transport-agnostic by composing it
over the active authority's spawner, and redefine container *lifecycle* to
target the backend's host. It reuses the SSH agent exec + `StdioChild`-over-SSH
machinery, preserves the single-spawner `Authority` contract, and the two
blockers (remote lifecycle, path-translation layering) are tractable and worth
fixing on their own merits. Treat **Option C** as the pragmatic fallback for
deployments where the editor already runs remote-side, and **Option B** only if
A can't be scheduled.

## Security caveats (do not oversell)

- **A container is not a hard security boundary.** Kernel exploits, daemon
  misconfig, and especially a mounted Docker socket can mean host compromise.
  Sandboxed is *vastly* better than running on the host, not airtight. Rootless
  Podman and never mounting the socket reduce the surface; a microVM/gVisor
  would be stronger at much higher weight and is not in scope.
- **The repo is mounted**, so repo code can mutate the repo and use any network
  we allow. Containment is about protecting the host and the user's credentials,
  not about making the repo's own contents inert.
- **Blocked remains the only zero-execution guarantee.** Keep it.
- **Trust state never comes from the repo.** Unchanged and load-bearing: the
  level, the synthesized container config, and any override live in user data
  dir keyed by canonical path, never in the tree — a repo cannot vouch for
  itself (`workspace_trust.rs` `TrustStore`, design rule #2).

## Relationship to existing machinery

This is deliberately not a new subsystem. It reuses what the devcontainer work
already proved out (`crates/fresh-editor/plugins/devcontainer.ts`,
`docs/internal/DEVCONTAINER_PLUGIN_DESIGN.md`, `AUTHORITY_DESIGN.md`):

- **`Authority` + `SpawnerSpec::DockerExec`** already route *every* spawn
  (one-shot, LSP, terminal) through a container with an injected env. A
  Sandboxed authority is a DockerExec authority pointed at the ad-hoc image with
  hardened runArgs.
- **The choke-point** (`Authority::with_trust`, `workspace_trust::gate`) stays
  the enforcement point: under Sandboxed the spawner *is* the container; under
  the no-runtime fallback it applies the host syntactic gate; under Blocked it
  refuses.
- **`EnvProvider`** capture works through the active backend, so env activation
  inside the sandbox is the existing `docker exec sh -lc` capture path (the
  "containers" follow-up already noted in `remote-env-manager-design.md`).
- **Per-project persistence** (`TrustStore`) extends to hold the synthesized
  container config + the chosen network policy, same file location.

## Open questions

- **Honoring a shipped `devcontainer.json` under Sandboxed.** How much to use
  (image/features) vs ignore (lifecycle hooks, host mounts)? A shipped
  devcontainer is repo-controlled; running its `postCreateCommand` is execution.
  Likely: use image/toolchain, run our own hardened args, skip lifecycle unless
  Trusted.
- **Network policy default** (deny / registry-allowlist / egress-allow) — needs
  a decision; recommendation above is default-deny with one-click per-project
  enable.
- **Lazy vs eager container start.** Strongly lean lazy (start on first
  execution, not on folder open) so browsing stays instant and macOS/WSL VM
  latency isn't paid for read-only sessions.
- **Build-cache sharing.** A per-project container-local cache is safe; sharing
  host caches is not. Is per-project cache duplication an acceptable disk cost,
  or do we want a Sandboxed-only shared cache volume isolated from host/Trusted?
- **Remote (SSH) + Sandboxed.** Analyzed in its own section above — the
  recommendation is Option A (transport-agnostic docker spawner composed over
  the active backend). Remaining sub-question: when the remote host has *no*
  container runtime, fall back to the remote host-gate or Blocked, mirroring the
  local no-runtime logic on the far side.
- **Async prompt/allow-once plumbing** for the no-runtime host-gate fallback is
  the prerequisite the enforcement module flags as unbuilt.
