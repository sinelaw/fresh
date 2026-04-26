# Fake devcontainer / docker CLI

Stand-in for `devcontainer` and `docker` so Fresh's devcontainer plugin
flows can be exercised without Node, `@devcontainers/cli`, a Docker
daemon, or any container images. Pure Bash; runs in CI sandboxes and
this dev sandbox unchanged.

Design + rationale: [`docs/internal/FAKE_DEVCONTAINER_CLI.md`](../../docs/internal/FAKE_DEVCONTAINER_CLI.md).
Interactive test plan: [`docs/internal/FAKE_DEVCONTAINER_TEST_PLAN.md`](../../docs/internal/FAKE_DEVCONTAINER_TEST_PLAN.md).

## Quick start

```bash
source scripts/fake-devcontainer/activate.sh
which devcontainer
which docker
mkdir -p /tmp/wkspc/.devcontainer
cat > /tmp/wkspc/.devcontainer/devcontainer.json <<'JSON'
{
  "name": "fake",
  "image": "mcr.microsoft.com/devcontainers/base:ubuntu",
  "forwardPorts": [8080, 5432],
  "initializeCommand": "echo init >&2",
  "postCreateCommand": "echo postcreate >&2"
}
JSON
./target/debug/fresh /tmp/wkspc
```

`activate.sh` prepends `scripts/fake-devcontainer/bin` to `$PATH` and
sets `FAKE_DEVCONTAINER_STATE` if unset (default
`$XDG_CACHE_HOME/fake-devcontainer`).

## Failure / timing knobs (env vars on `devcontainer up`)

| Var | Default | Effect |
|---|---|---|
| `FAKE_DC_UP_DELAY_MS` | `150` | Per stderr-line sleep. `0` for instant. |
| `FAKE_DC_UP_LINES` | `6` | Number of "[+] Building …" lines emitted. |
| `FAKE_DC_UP_FAIL` | unset | If `1`, exit 1 with an `error: …` line on stderr. |
| `FAKE_DC_UP_FAIL_REASON` | `image not found: ubuntu:bogus` | Override the failure message. |
| `FAKE_DC_UP_HANG` | unset | Sleep forever after the first build line. Use to test cancel-attach. |
| `FAKE_DC_UP_BAD_JSON` | unset | Skip the success JSON. Plugin should hit `rebuild_parse_failed`. |
| `FAKE_DC_UP_NO_CONTAINER_ID` | unset | Emit `outcome:success` without `containerId`. |
| `FAKE_DC_REMOTE_USER` | `vscode` | Override `remoteUser` in the success JSON. |
| `FAKE_DC_REMOTE_WORKSPACE` | `/workspaces/<basename>` | Override `remoteWorkspaceFolder`. |
| `FAKE_DC_PORTS` | unset | Comma-separated ports (e.g. `8080,5432`); written to the container's `ports` file so `docker port` returns mappings. |
| `FAKE_DEVCONTAINER_STATE` | `$XDG_CACHE_HOME/fake-devcontainer` | State directory. |

## Subcommand coverage

`devcontainer`:

- `up --workspace-folder <path> [--remove-existing-container]` — full fake.
- `read-configuration` — stub returning `{}`.
- `reset` — fake-only: wipe state. Not part of the real CLI.

`docker`:

- `exec [-i|-t|-it] [-u <user>] [-w <dir>] <id> <cmd>...` — runs the
  command on the host, in the recorded workspace dir if `-w` not given.
  Sets `FAKE_DC_USER` / `FAKE_DC_CONTAINER_ID` in the child env.
- `logs [--tail N] <id>` — tails the container's `logs` file (built up
  by `up`).
- `port <id>` — cats the container's `ports` file.
- `info`, `version`, `ps` — minimal stubs.

Anything else exits 64 with `unsupported subcommand: <x>`.

## Limitations

- `docker exec` runs the command on the **host**. There is no isolation,
  no in-container PATH, no in-container filesystem. Good enough for the
  Fresh-side flows (terminal, lifecycle commands, log surfaces); not
  good enough to validate that the LSP available *inside* a real image
  works.
- The plugin's filesystem authority is `kind: "local"` so file I/O never
  goes through Docker — that's faithfully captured by the fake.
- Port mappings are text only; nothing actually listens.
- The fake is intentionally forgiving about unknown flags (real
  `@devcontainers/cli` accepts dozens). If the plugin grows a flag we
  must act on, add it explicitly.
