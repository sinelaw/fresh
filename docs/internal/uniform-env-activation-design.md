# Uniform Environment Activation — Design

Status: design + in-progress implementation. Specifies how the *activated*
environment (venv / direnv / mise) is captured and applied so that **every**
process the editor launches — LSP servers, formatters, find-in-files, one-shot
`spawnProcess`, **and the integrated terminal** — sees the same environment,
identically on local, SSH, Docker, and Kubernetes backends.

Motivation: issue #2355, Problem 2. The status bar said "Environment active"
while the integrated terminal ran the *system* toolchain with none of the
project's env vars, because the terminal spawned through portable-pty directly
and bypassed the env-application path the spawners use. Fixing only the local
terminal (see the first commit on this branch) leaves the same gap on SSH and
Docker, and leaves two different notions of "the env" in the tree. This design
removes the divergence everywhere.

Related: `AUTHORITY_DESIGN.md` (the spawner choke-point), the env provider
(`crates/fresh-editor/src/services/env_provider.rs`), and the trust model in
`workspace-trust-sandbox-design.md` (capture/apply are gated by trust exactly
like a spawn).

## The two invariants (this *is* the uniform solution)

1. **Capture the activation as a delta, on the host where processes run.**
   On that host, run a shell *we* pick (`bash -lc` / `sh -lc`) twice: once
   clean (login shell, no recipe) and once with the project's activation
   recipe. The **difference** — keys added/changed/removed — is the captured
   environment. It is pure `KEY=VALUE` data, so the recipe is only ever parsed
   by our capture shell, never by the user's interactive shell.

2. **Apply the delta as process-environment data, before any shell starts** —
   never by interpolating values into a command the user's interactive shell
   re-parses. Every child goes through one abstraction,
   `spawn(argv, cwd, env_delta, pty, size)`, and each backend realizes it with
   the env mechanism it *already* ships.

Because env is always applied as data to the process, the user's interactive
shell (bash / zsh / fish / nu) is irrelevant — the result is byte-identical on
every backend. The delta carries only the activation's contribution, so
volatile, shell-managed keys (`PWD`, `OLDPWD`, `SHLVL`, `_`) are never in it
and need no special-casing.

This single definition **subsumes both** of today's env notions: the local/SSH
`EnvProvider` snapshot *and* the Docker/Kube `base_env` (`userEnvProbe`) become
the same delta captured on the relevant host.

## Capture

`EnvProvider` keeps its role as the single source of truth (recipe = snippet +
dir) and its content-hash cache over env-input files (`.envrc`, `mise.toml`,
…). The capture runs one script on the target host and returns its stdout:

```
cd '<dir>'; command env; printf '<SENTINEL>\n'; <recipe>; command env
```

- The text before `<SENTINEL>` is the **baseline** (login env in the project
  dir, recipe not yet run). The text after is the **activated** env. The delta
  is computed by diffing them: a key whose value is new or changed is a
  `set`; a key present in baseline and gone from activated is an `unset`.
- One subprocess per capture, cached by the env-inputs hash; conceptually every
  spawn recaptures, but unchanged inputs serve from cache for free. No
  staleness, because both capture and apply are data on the host.
- The recipe is interpreted only by the capture shell we choose. Its output is
  data. (venv / direnv / mise all export to `bash`; we always capture via
  `bash`/`sh`.)
- Capture is gated by Workspace Trust: restricted ⇒ empty delta ⇒ no
  activation anywhere.

The capture is parameterized by a backend "runner" that executes the script on
the host and returns stdout — the abstraction `EnvProvider::current(run)`
already has. Each backend supplies its runner:

| Backend | Capture runner |
|---|---|
| Local | `bash -lc <script>` as a subprocess |
| SSH   | the agent runs `sh -lc <script>` on the remote host |
| Docker | `docker exec <id> bash -lc <script>` |
| Kube  | `kubectl exec … -- sh -lc <script>` |

## Apply

Every authority launches children through one `spawn(argv, cwd, env_delta,
pty, size)`. Each backend applies `env_delta` as **process-environment data**
using a mechanism it already has — no new runtime dependency:

| Backend | Apply the delta (always data, never the interactive shell) |
|---|---|
| **Local** | native process env (`Command` / `CommandBuilder.env`) |
| **Docker** | `docker exec -e KEY=VAL …` (the runtime sets it on the process) |
| **Kube** | `kubectl exec -- env KEY=VAL … argv` (`env(1)` is a POSIX exec wrapper, not shell parsing) |
| **SSH** | the existing Python agent writes a tiny launcher (below); `ssh -t host python3 <path>` execs it |

Python appears **only** for SSH, where it is already mandatory (the agent is
bootstrapped via `python3` over the connection). Local, Docker, and Kube add
nothing.

### The terminal is not special

The integrated terminal becomes:

```
spawn(argv=[user_interactive_shell], cwd, env_delta=captured, pty=true, size)
```

— same delta, same per-backend apply as LSP; only `pty=true` and the argv
differ. This removes the three divergent terminal shapes
(`ssh -t '… exec $SHELL -l'`, `docker exec … bash -l`, local `CommandBuilder`).
Editor control vars (`TERM=xterm-256color`, `FRESH_SESSION`) are appended after
the delta so they always win.

### SSH apply without agent-PTY: a python3 launcher

The SSH terminal keeps its current `ssh -t` PTY (no `pty.openpty()` in the
agent). The only change is *what* it execs. The problem with `ssh -t` is that
any command string is parsed by the user's **remote login shell** (possibly
fish), so inline `env K=V …` hits shell-quoting hazards. Avoid it entirely:

- Through the agent's existing file API, write a launcher to a no-space path on
  the remote:

  ```python
  #!/usr/bin/env python3
  import os, json, base64
  data = json.loads(base64.b64decode("<BASE64>").decode())   # {env, unset, cwd, shell}
  os.environ.update(data["env"])
  for k in data["unset"]:
      os.environ.pop(k, None)
  os.chdir(data["cwd"])
  os.execvp(data["shell"], [data["shell"], "-l"])
  ```

- The terminal runs `ssh -t host python3 <path>`. The user's login shell sees
  only two bare words (`python3` + path) → safe in bash/zsh/fish. The launcher
  runs under python3, so env is set from **data** (base64'd JSON) — robust for
  any byte content (spaces, quotes, newlines, unicode); no shell quoting
  anywhere. It then `exec`s the user's real shell with the env already applied.
- python3 is the dependency SSH already requires; this adds no new one and no
  agent-PTY work.

One thing to name (not new to this approach): the launcher execs `$SHELL -l`, a
login shell, which re-sources the user's rc. If that rc *also* activates (a
direnv/mise hook), it re-runs on top of our delta — harmless, it sets the same
vars. Execing a non-login `$SHELL` would avoid any double-run but costs the
user their normal prompt/aliases, so `-l` is the right default.

## Errors — missing python3 on an SSH remote

python3 is required **only for SSH**. Local/Docker/Kube never touch it (capture
is `bash`/`sh` in the container or a local subprocess; apply is native / `-e` /
`env`-prefix), so this error can only appear on an SSH connect, and only there
do we check.

The agent bootstrap is already the gate: if python3 can't start, the
connection fails before any feature is attempted, and ssh's stderr is piped
(not painted over the UI), so it is a clean status line. The contract:

- Treat **exit 127 / "command not found"** on the bootstrap as the definitive
  "no python3" signal (already implemented in `connection.rs::ssh_eof_error`).
- Harden with a **version assertion**: the agent's first reply carries
  `sys.version_info`; below the minimum the agent needs, fail with the same
  shape of message rather than a confusing mid-session protocol error later.
- Emit **one clear, actionable line** naming the host and the fix, e.g.
  *"Python 3 was not found on `<host>`. Install Python 3 there, or connect
  without remote features."*

The no-python backends are unaffected; a missing `docker`/`kubectl` surfaces
through that tool's own clear failure.

## What this deletes

- `local_captured_env` + `apply_env` (local-only application) and `env_wrap`
  (SSH argv-prefix) collapse into "set `env_delta` on the child."
- The Docker/Kube `base_env` / `userEnvProbe` notion — folded into the same
  delta captured in the container/pod.
- `TerminalWrapper`'s divergent shapes (`host_shell`, `ssh`, docker/kube
  explicit) reduce to one `spawn(pty=true)`.
- The first commit's `current_local_blocking` full-snapshot + the
  `is_volatile_terminal_env_key` skip-list — unnecessary once capture is a
  delta (volatiles are never in it).

## Consequences

- `Env: Show Status` reporting "active" becomes **true by construction** in
  every surface on every backend — the status can no longer lie.
- venv (no shell hook), direnv/mise (hook or not), and the container env probe
  all behave identically, locally and remotely.
- Shell choice is irrelevant: fish/zsh/nu users get the same activated env as
  bash users.

## Implementation status

- [x] Local: `EnvProvider` delta capture; integrated terminal applies the
      delta (replaces the full-snapshot + volatile skip-list). **Validated
      end-to-end** (tmux + active `.venv`).
- [x] SSH: terminal runs `ssh -t host … exec python3 -c '<literal>'`, a single
      shell-literal token (fish-safe — verified against the fish docs) that
      decodes + execs a launcher capturing the delta on the remote. No agent
      PTY, no new dependency. **Validated end-to-end** against a real local
      `sshd` on a userspace port (active `.venv` → terminal sees the remote
      env; inactive → unchanged login shell).
- [x] Docker: the devcontainer plugin's terminal wrapper now passes the
      `userEnvProbe` env as `-e KEY=VAL` (mirrors the spawner's
      `build_docker_exec_prefix`). `docker exec -e` semantics confirmed both by
      the existing production LSP path and empirically. *Not* e2e-validated here
      (no pullable image / no live devcontainer probe).
- [x] Kube: `build_kube_terminal_args` exports the in-pod probe env inside the
      `sh -lc` wrapper it controls (kubectl has no `-e`; the `env`/`export`
      approach is the documented one). Unit-tested; *not* e2e-validated here (no
      cluster / kubectl).
- [x] SSH preflight: the existing exit-127 detection in
      `connection.rs::ssh_eof_error` surfaces a clear, host-named "Python 3 not
      found … install it, then reconnect" message; wording sharpened.

Deliberately *not* changed: the local/SSH **LSP / one-shot spawners** keep
applying the full captured snapshot. That snapshot is `baseline + activation`,
so the *effective* child env is identical to `baseline + delta` — migrating
them to the delta form is a cosmetic internal-consistency cleanup, not a
behavior fix, and is left out to avoid churning working, e2e-tested remote LSP
paths. The user-facing goal (terminal sees the same env as LSP on every
backend) is met.
