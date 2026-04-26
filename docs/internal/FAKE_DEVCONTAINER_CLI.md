# Fake Devcontainer CLI — Design & Plan

## Why

Fresh's devcontainer plugin (`crates/fresh-editor/plugins/devcontainer.ts`)
shells out to two host binaries:

- **`devcontainer up --workspace-folder <cwd> [--remove-existing-container]`**
  to bring a container up and parse a JSON outcome line from stdout.
- **`docker exec [-i] [-u user] [-w workspace] <id> <cmd>...`** for every
  process the container authority spawns (LSP servers, lifecycle commands
  after attach, terminal `bash -l`, etc.), plus `docker port <id>` and
  `docker logs --tail N <id>` for the panels.

Real attach requires Node + `@devcontainers/cli`, a working Docker daemon,
and a real container image — none of which are present in this sandbox or
in the CI sandbox. That makes interactive debugging of the attach /
lifecycle / log surfaces effectively impossible without a stand-in.

This doc proposes a **pure-shell fake** — `fake-devcontainer` and
`fake-docker` — that stands in for the two CLIs over the wire-shape Fresh
actually relies on. No daemon, no images, no containers. It runs anywhere
Bash and `coreutils` run.

## Non-goals

- Not a faithful re-implementation of the dev-container spec. We mock only
  the surfaces Fresh's plugin and authority code touch.
- Not a replacement for end-to-end CI tests of real container behaviour.
  The fake exercises the editor side of the boundary; the real CLIs are
  still authoritative for spec conformance.
- Not a Docker-in-Docker shim. Container "exec" runs the command on the
  host, scoped by env; the fake never tries to enter an isolation
  namespace.

## Wire surface in scope

Captured by reading every `editor.spawnHostProcess(...)` site in
`devcontainer.ts` plus `docker_spawner.rs`:

| Caller (file:line) | Invocation | What Fresh expects back |
|---|---|---|
| `devcontainer.ts:1378` | `which devcontainer` | exit 0 with path on stdout |
| `devcontainer.ts:1440` | `sh -c 'exec devcontainer "$@" 2> "$LOG"' sh <log> up --workspace-folder <cwd> [extra]` | stdout: a line that is a JSON object `{ outcome, containerId, remoteUser, remoteWorkspaceFolder }` somewhere near the end; stderr → log file (any human progress text) |
| `devcontainer.ts:1694` | `which docker` | exit 0 with path on stdout |
| `devcontainer.ts:1701` | `docker logs --tail 1000 <id>` | stdout/stderr of the "container" |
| `devcontainer.ts:791,880` | `docker port <id>` | lines like `8080/tcp -> 0.0.0.0:32769` |
| `docker_spawner.rs:46` | `docker exec [-i] [-u U] [-w W] <id> <cmd> [args]` | runs the cmd, exit code mirrors child |

Two flags matter for `up`:

- `--workspace-folder <path>` — always present.
- `--remove-existing-container` — added by Rebuild.

## Design

### Layout

```
scripts/fake-devcontainer/
├── README.md                # how to use, env knobs, examples
├── activate.sh              # source-able: prepends bin/ to $PATH and
│                            # sets FAKE_DEVCONTAINER_STATE if unset
├── bin/
│   ├── devcontainer         # bash, dispatches subcommands
│   └── docker               # bash, dispatches subcommands
└── lib/
    └── fake-state.sh        # shared helpers for state dir / id alloc
```

`activate.sh` is the entry point most callers (humans, smoke scripts) use:

```bash
source scripts/fake-devcontainer/activate.sh
which devcontainer   # → .../scripts/fake-devcontainer/bin/devcontainer
which docker         # → .../scripts/fake-devcontainer/bin/docker
```

For CI it's also runnable as `eval "$(scripts/fake-devcontainer/activate.sh --print-env)"` or by prepending the bin dir manually.

### State directory

`${FAKE_DEVCONTAINER_STATE:-${XDG_CACHE_HOME:-$HOME/.cache}/fake-devcontainer}`.

Layout:

```
<state>/
├── containers/
│   └── <id>/
│       ├── workspace        # path the container was "built" against
│       ├── created_at       # epoch seconds
│       ├── status           # running | stopped
│       ├── remote_user      # whatever we reported back
│       ├── remote_workspace # likewise
│       ├── ports            # one line per fake port mapping
│       └── logs             # appended by `up`; tailed by `docker logs`
└── last_id                  # 12-hex of the most recently created container
```

A container "id" is a deterministic-ish 12-hex derived from
`sha256(workspace + epoch_ms)` truncated; collisions are vanishingly
unlikely and easy to spot if they do happen.

### `bin/devcontainer`

Dispatches on `$1`:

#### `up`

1. Parse args: `--workspace-folder <p>`, `--remove-existing-container`.
   Anything unknown is ignored (real CLI accepts many flags; we don't
   need to be strict).
2. If a previous container exists for this workspace and the
   `--remove-existing-container` flag was given, mark it `stopped` and
   allocate a new id. Otherwise reuse the latest id for this workspace.
3. Stream "build" progress to **stderr** with sleeps, configurable via
   `FAKE_DC_UP_DELAY_MS` (default 150ms). Lines look like:
   ```
   [+] Building 0.0s ...
   [+] Building 0.5s ...   resolving image ubuntu:22.04
   [+] Building 1.0s ...   pulling layers (4/12)
   ...
   [+] Starting container <id>
   [+] Running postCreateCommand
   ```
   This matches the *shape* of `@devcontainers/cli` stderr output — line
   prefixes / phases — which is what the user sees scrolling in the
   build log split.
4. Emit one final JSON object on **stdout**:
   ```json
   {"outcome":"success","containerId":"<id>","remoteUser":"vscode","remoteWorkspaceFolder":"/workspaces/<basename>"}
   ```
   The plugin parses backwards from stdout's last line until a JSON
   object parses, so order vs. any other stdout chatter is forgiving.
5. Exit 0.

Failure injection:

| Env var | Effect |
|---|---|
| `FAKE_DC_UP_FAIL=1` | After streaming a partial build, emit `error: <reason>` on stderr and exit 1. |
| `FAKE_DC_UP_FAIL_REASON=<text>` | Override the failure reason text. |
| `FAKE_DC_UP_HANG=1` | `sleep` indefinitely after the first progress line so the cancel-attach flow can be exercised. |
| `FAKE_DC_UP_DELAY_MS=<n>` | Per-line stderr delay (default 150). 0 = no delay. |
| `FAKE_DC_UP_LINES=<n>` | How many "[+] Building …" lines before the container-start line (default 6). |
| `FAKE_DC_UP_BAD_JSON=1` | Skip the success JSON entirely. Plugin should hit `rebuild_parse_failed`. |
| `FAKE_DC_UP_NO_CONTAINER_ID=1` | Emit JSON with `outcome:success` but no `containerId`. Plugin should hit `rebuild_missing_container_id`. |

Any other subcommand prints `unsupported subcommand: <x>` and exits 64,
which is enough to get a clear failure if the plugin grows new calls.

#### `read-configuration` (stub)

Not currently called by Fresh, but cheap to add: print `{}` and exit 0.
Documented as "no-op" so future plugin code can probe it.

### `bin/docker`

Dispatches on `$1`:

#### `exec`

Parses leading flags: `-i`, `-t`, `-it`, `-u <user>`, `-w <dir>`, then a
container id, then `cmd args...`. Behaviour:

- If id has no record under `containers/<id>` we still proceed — the
  shape of the call is what matters; mismatches just mean the user
  didn't go through `up` in this state dir.
- `cd` into the `-w` dir if given (treat as host path; the workspace is
  mounted 1:1 in real devcontainers, and the fake mirrors that), else
  the container's recorded workspace, else `$PWD`.
- If `-u <user>` is given, set `FAKE_DC_USER=<user>` in the child env so
  scripts that want to assert it can. Do **not** actually `su` — we
  don't need root and we don't want sudo prompts.
- `exec` the command. Exit code propagates.

The interactive shell case (`docker exec -it <id> bash -l`) thus becomes
"start a login bash here." That's exactly what the user wants when
debugging the attach flow in tmux: prompt comes up, they type, things
happen.

#### `logs`

`docker logs [--tail N] <id>` → if `<state>/containers/<id>/logs`
exists, `tail -n N` it; else emit a stub line. Exit 0.

#### `port`

`docker port <id>` → cat `<state>/containers/<id>/ports` (`up` writes a
deterministic mapping per declared `forwardPorts`). Empty file is fine.

#### Other subcommands

`info`, `version`, `ps`, etc. — print a minimal stub that's enough for
`docker info` to return a "daemon present" exit-0. Anything truly
unimplemented prints `fake-docker: unsupported subcommand <x>` and
exits 64.

### Determinism & cleanup

- `up` writes `last_id` so test scripts can `cat $FAKE_DEVCONTAINER_STATE/last_id` to discover the id.
- `scripts/fake-devcontainer/bin/devcontainer reset` (custom subcommand,
  not real CLI) wipes `<state>/containers/`. Documented as
  "fake-only".
- The state dir is per-user, not per-workspace — matching how
  `~/.docker` is shared.

### Why bash, not Rust

- Zero build step → trivially usable in CI before the editor is built.
- Reads small enough to grok in one screen.
- Interpolation of stderr/stdout streams uses primitives we already
  trust (`printf`, `sleep`, `tail`).
- The fake never needs to be *fast* — `up` deliberately runs slow so the
  build-log split has something to stream.

### Integration with the editor

The plugin doesn't know it's talking to a fake — `which devcontainer`
just resolves earlier in `$PATH`. That means the test recipe is just:

```bash
source scripts/fake-devcontainer/activate.sh
./target/debug/fresh /path/to/workspace
```

For CI we can extend the existing nextest harness with a helper that
prepends the fake bin dir before launching the editor under test.

## What this does *not* test

- The real `setAuthority` restart path actually restarts the editor
  process. With `docker exec` faked to "run on the host," the spawner
  routes commands through `fake-docker exec ... bash -l`, which works
  for terminals and LSP smoke-tests but does **not** validate
  in-container PATH, in-container LSP availability, or in-container file
  access. Those still require a real container image.
- Filesystem authority is `kind: "local"` in
  `buildContainerAuthorityPayload`, so file ops were never going through
  Docker anyway. The fake is faithful here.
- Network port forwarding: the `ports` file is just text. Nothing
  actually listens.

These boundaries are documented in
[`scripts/fake-devcontainer/README.md`](../../scripts/fake-devcontainer/README.md)
so a future contributor doesn't mistake "all green under the fake" for
"ships."
