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
- **Per-workspace user state** — `getGlobalState`/`setGlobalState`, keyed by the
  canonicalized workspace path. Stored in the user's config dir, never in the
  repo. This is where the trust decision and any per-user override live (the
  devcontainer plugin uses the same store for its per-workspace attach
  decisions). Fresh *also* has a committed project-config layer
  (`$PROJECT_ROOT/.fresh/config.json`, `config_io.rs:283`), but this feature
  deliberately does **not** use it — see §Tier 3.
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

## Core model: one universal capture, a live provider, never a stored snapshot

Do **not** model `venv`, `direnv`, and `mise` as three independent features, and
do **not** bake a captured env into a stored snapshot. Two converged decisions:

**1. One universal capture mechanism.** The active environment is produced by a
single function — run a shell snippet in the project directory *on the active
backend's host*, and capture the resulting environment delta:

```
capture(host, snippet, dir) -> Vec<(String, String)>
    // runs `$SHELL -lc 'cd <dir>; <snippet>; env -0'` through the active
    // backend (local tokio / SSH agent / docker exec), diffs against a
    // baseline, and returns the delta. The snippet's own stdout is parsed
    // and discarded, so the captured value is clean.
```

There are no bespoke per-manager code paths. The only manager-specific
knowledge is a tiny, **overridable default-snippet table** chosen by passive
detection:

| Detected in project root | Default snippet |
|--------------------------|-----------------|
| `.envrc` (direnv) | `eval "$(direnv export bash)"` |
| `mise.toml` / `.mise.toml` / `.tool-versions` | `eval "$(mise env -s bash)"` |
| `.venv/` / `venv/` | `source <dir>/bin/activate` |
| nothing, or user override | `` (empty ⇒ pure login shell) or a user snippet |

Why the table can't be dropped entirely: direnv and mise hook the shell
**prompt** (`PROMPT_COMMAND` / `precmd`), which never fires in a non-interactive
capture — so a blank login-shell capture silently misses them. That's why they
ship dedicated exporters (`direnv export`, `mise env`); we just use those as the
default snippets. Everything else — `pyenv`, `conda`, `asdf`, custom rc edits —
*is* caught by the empty (login-shell) snippet, and anything exotic is a
user-supplied snippet. So `LoginShell` (empty) and `Command(snippet)` are
first-class universal catch-alls; the named managers are merely auto-filled
defaults, not special cases.

**One active provider, no N-way merging.** Setups that compose (a `.envrc` that
does `use flake` / `eval "$(mise activate)"`) are handled by the *outermost*
manager: `direnv export` already returns the fully-composed result. We run one
provider; we never merge several.

**2. The provider is live, not a stored snapshot — and never goes stale.** The
active environment is a *recipe* (`{ snippet, project_dir }`) held as a shared,
interior-mutable handle on the `Authority` — exactly parallel to
`Arc<WorkspaceTrust>` — and re-evaluated fresh against the active backend:

- **One-shot spawns** (`spawnProcess`, formatters, find-in-files) re-evaluate
  the recipe at spawn time. A cache keyed on a content-hash of the env inputs
  (`.envrc`/`mise.toml`/`pyvenv.cfg`) skips re-running when nothing changed, but
  correctness never depends on it — there is no stored vars blob to drift.
- **Long-running servers (LSP)** capture the env at spawn. A running process's
  environment *cannot* be refreshed live (OS-level fact), so "no staleness" for
  the LSP means: watch the env inputs and **restart the server** with a fresh
  capture when they change. The capture is always fresh; applying it to a live
  server is inherently a restart.
- **The integrated terminal** is a real login/interactive shell (its own
  `TerminalWrapper`), so direnv/mise hooks fire natively on `cd` — fresh by
  construction, and stdout noise is expected there.

**Mechanism: a plugin op, not `setAuthority`.** Activation sets the live
provider in place via `editor.setEnv(...)` / `clearEnv()` — *not* by rebuilding
the authority. This is the only thing that works over SSH: a `setAuthority`
payload is serializable JSON and **cannot carry the live SSH `AgentChannel`**,
so it could never reconstruct a remote backend with env. `setAuthority` /
`SpawnerSpec` remain for *backend selection* (devcontainer attach, SSH startup);
env stops riding on them. Because every spawner already holds the active
backend's handle, and the capture runs through that backend, the whole thing is
**remote-correct by construction** — evaluation and execution always happen on
the same host.

## The UX: three tiers of control (auto → glance → explicit)

The entire UX is a ladder from zero-effort to total control. A user never *has*
to configure anything, but can always override the behavior — and every override
lives in the user's own config, never in the repository.

### Tier 1 — Auto-detect from the ecosystem's own files (zero config, the default)

Fresh is a pure *consumer* of the standard, editor-agnostic files a project
already commits — it adds none of its own. On opening a workspace, scan the root
for, in priority order:

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

Selecting an entry sets the live env provider via `editor.setEnv(...)` — no
authority rebuild, no restart for one-shot tools; the LSP is restarted so it
re-spawns inside the environment. This is the TUI equivalent of VS Code's
"click the interpreter in the status bar" quick-pick.

### Tier 3 — Per-user overrides, stored outside the repo (full control)

A user can override, enable, or disable the auto-detect behavior — but the
override lives in **user-global state, never in the repository.** Fresh is not a
widely used editor; committing a Fresh-specific config file (`.vscode`-style)
would land an obscure artifact in the tree that teammates on other editors read
as garbage. More importantly, the *what environment to use* answer is already
committed in editor-agnostic form (§Tier 1) — Fresh has nothing project-specific
worth adding to git.

So overrides are stored via `setGlobalState`, keyed by the **canonicalized
workspace path** (the same store the devcontainer plugin uses for its
per-workspace decisions, and the same store that holds the trust decision):

```jsonc
// conceptual shape of the per-workspace record in user-global state
{
  "enabled": true,                 // master switch for this workspace
  "enabled": true,                 // master switch for this workspace
  "snippet": "source .venv/bin/activate", // the capture recipe; "" = pure login
                                   // shell; overrides the auto-detected default
  "applyToTerminal": true
}
```

The override is just the capture **snippet** (plus on/off) — the same recipe the
universal capture runs. Leaving it unset uses the auto-detected default;
setting it to `""` forces a pure login-shell capture; setting it to any shell
snippet is the universal escape hatch for exotic/custom setups.

Plus a **global default** (in the user's config, not per-workspace) to turn the
whole feature on or off, or set it to "ask, never auto." Reached through the
picker's "Edit activation command…" / "Use system / none" entries and the
`Env: …` command-palette commands — the user never hand-edits a file unless they
want to.

What this trades away: a team **cannot** share a *Fresh-specific* override
through git. In practice that override is almost always expressible in the
standard files anyway — pin the version in `.tool-versions`, point `.envrc` at
the venv, set `[tool.poetry]` in `pyproject.toml` — so the loss is small and the
repo stays clean. If a real demand for committed Fresh overrides ever appears,
it can be added later as an explicit, opt-in "save to project" action; it is
deliberately **not** a default, and the threat-model rules below would then
apply to it.

## Remote: how the universal capture reaches the host

For `direnv`/`mise`/`pyenv` to work over SSH, the capture and the spawn both
have to happen *on the remote host*. The universal capture handles this by
construction — it runs the snippet through the **active backend**, so on an SSH
authority `capture()` issues `$SHELL -lc 'cd <dir>; <snippet>; env -0'` over the
agent, and the captured env is injected into the (remote) spawns. No special
"login shell mode" toggle on the agent is needed: the snippet *is* the login
shell when that's what we want, and the snippet's stdout is parsed and
discarded so the agent's `exec` stays clean.

Two SSH gaps had to close for this, and both are now done:

1. **LSP must run on the remote host.** Previously `Authority::ssh` routed the
   long-running spawner to the host-local `LocalLongRunningSpawner` (the old
   "Phase L" gap). **Fixed:** `RemoteLongRunningSpawner` runs each server as its
   own `ssh user@host <remote-cmd>` subprocess — a real local
   `tokio::process::Child` whose piped stdio *is* the remote process's stdio
   (the same trick Docker uses with the local `docker` CLI), so the LSP I/O
   layer is untouched. It honours an injected `PATH` in `command_exists`, so a
   remote venv's `pyright`/`ruff` is discoverable.

2. **Remote spawns must carry the env.** **Fixed:** every spawner (local / SSH /
   docker, one-shot + long-running) applies the active env via its transport's
   native mechanism (`Command::envs` / `env K=V …` over ssh / `docker exec -e`).
   The env itself comes from the live provider (above), captured on the remote
   host — not from a `setAuthority` payload, which can't carry the live SSH
   connection.

The **integrated terminal** is the easy case: its `TerminalWrapper`
(`authority/mod.rs:99`) opens a real login/interactive shell, so `cd`-triggered
direnv/mise hooks fire natively, matching the manual experience — fresh by
construction, no capture needed there.

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
- **`activate`** wrapper (the hand-typed escape hatch in user state) — arbitrary
  shell. Note this one is *user-authored*, not repo-supplied, so the risk is the
  user's own typo rather than a hostile project — but it still executes.
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

### Workspace Trust: three levels, per project

Trust is a single **per-workspace** setting, not a per-feature one, persisted via
`getGlobalState`/`setGlobalState` keyed by the **canonicalized** workspace path
(resolve symlinks; a path-spoof must not inherit another path's trust). Env
managers, the C# project loader, build/restore commands, tasks, and any LSP
launched with project-specified arguments all consult the *same* setting. It is
a small **core service** — e.g. `editor.workspaceTrustLevel()` and
`editor.requestWorkspaceTrust()` in the plugin API — that any feature which
would spawn or run repo-controlled content checks first. The env-manager plugin
is just one consumer; it must not invent a parallel notion.

There are three levels:

1. **Restricted — the default.** No repo-controlled code runs. Env managers do
   **not** activate; C#-style project execution (restore/build, MSBuild targets,
   analyzers/source generators, project-configured LSP, tasks) is suppressed;
   spawns whose resolved executable lives inside the workspace are refused. But
   **ordinary spawns still proceed** — with the plain, non-activated
   environment. `git`, `rg`, and normal plugins keep working; only repo-controlled
   execution is held back. This is the safe-but-usable default a freshly opened,
   never-seen project gets (including if the user just dismisses the prompt).

2. **Trusted — full execution.** Env managers activate and inject; the C# loader
   and project tooling run; repo-relative executables are allowed. The user has
   vouched for the project.

3. **Blocked — nothing runs without per-spawn say-so.** A hard lockdown over
   **every** spawn — `spawnProcess`, `spawnHostProcess`, and every core spawn
   path (LSP, formatter, exec'd file watchers) alike. It has two behaviors,
   chosen by the user:
   - **Fail silently** (default for Blocked): every spawn errors immediately.
     For reading genuinely hostile code with zero process execution.
   - **Prompt each time:** every spawn pauses and asks before running, showing
     the resolved executable, args, and cwd — `Allow once` / `Deny`
     (deny behaves like fail). The strictest *interactive* mode: an audit lens
     on exactly what a project tries to run, nothing executing without explicit
     per-execution consent.

   Both are strictly more restrictive than Restricted, which lets system tools
   run unprompted.

The level (and the Blocked sub-behavior) is settable any time from the
status-bar picker / a `Workspace: Set Trust Level` command and remembered per
canonical path.

### The prompt

When a never-decided workspace is opened **and it actually contains something
that would execute** (a detected env file, a `.sln`/`.csproj`, repo-defined
tasks, analyzers), a single one-shot `showActionPopup` asks once. Don't prompt
for a plain folder with nothing to gate — a popup on *every* open trains users
to dismiss it. The prompt names what would run, so consent is *informed*, and
the safe choice is the default:

```
This project can run code on your machine through its tooling
(found: .envrc, MyApp.csproj). How should Fresh treat it?

  [ Restricted (default) ]   [ Trust this folder ]   [ Block all execution ]
       remember? (·)                remember? (·)          remember? (·)
```

- **Restricted** (default; also what Escape / dismiss does) → level 1.
- **Trust this folder** → level 2.
- **Block all execution** → level 3.
- **remember** persists the chosen level for this canonical path; otherwise it
  is session-only and the project is re-evaluated on next open.

The decision is global to the project, so choosing Trusted once lets env
activation, the C# loader, etc. all proceed without their own separate popups.

> **Why not always prompt / never prompt?** Prompting on every open is friction
> and breeds reflexive "yes"; never prompting is the C# bug. Prompting only when
> there is genuinely executable content present, once, and remembering the
> answer, keeps the gate meaningful and rare.

### What happens at a process spawn

Because every primitive routes through the one `Authority` spawner, the trust
level is enforced at that single choke-point — so it covers plugin
`spawnProcess` *and* core Rust callers (LSP, formatter, watchers) identically,
with no per-caller cooperation required:

| Level | Repo-controlled exec (`.envrc`, analyzers, repo-relative binary) | Ordinary spawn (`git`, `rg`, system tool) | Env injection |
|---|---|---|---|
| **Restricted** (default) | Refused / suppressed | **Proceeds** | None (env never activated) |
| **Trusted** | Runs | Proceeds | Active env injected |
| **Blocked / fail** | Fails with error | **Fails with error** | N/A — nothing spawns |
| **Blocked / prompt** | `Allow once` / `Deny` per spawn | `Allow once` / `Deny` per spawn | None unless an allowed env step ran |

Under **Blocked / prompt**, the call (plugin or core) blocks awaiting the
decision; `Deny` returns the same error as Blocked / fail. Because LSP,
formatters, and watchers spawn frequently, per-spawn prompting can flood — so
this mode coalesces identical `(executable, args)` requests and offers
`Allow for this session` on the prompt to suppress repeats of the same command.
It is an audit/power-user mode, not a daily default.

Two layers implement the Restricted row, since the spawner can't always tell a
repo-controlled spawn from a benign one:

- **Choke-point backstop (automatic):** the spawner refuses any spawn whose
  resolved executable path is inside the canonical workspace root
  (`./.venv/bin/python`, `./node_modules/.bin/eslint`, `./scripts/build`). This
  catches repo-placed binaries regardless of caller. Note that *not activating*
  already keeps repo `bin/` off `PATH`, so a bare `python` can't silently
  resolve into the repo — the backstop only has to catch explicit in-workspace
  paths.
- **Feature-level gate (explicit):** features that launch a *system* tool whose
  identity or args are chosen by repo content (the C# loader picking `dotnet`
  for a `.csproj`, a task runner, a project-configured LSP command) check
  `workspaceTrustLevel()` themselves and degrade. The spawner can't infer this;
  the feature declares it.

> **Honest caveat.** The executable-path backstop is not a proof: even a system
> tool can run repo-controlled content via config it auto-reads (`git` with a
> repo-set `core.fsmonitor`/`core.pager`/hooks is the classic case). Restricted
> reduces accidental exposure; the real protection is that repo-configurable
> features degrade when not Trusted. **Blocked** is the only level that
> guarantees zero spawning, for when that residual risk is unacceptable.

### Hard rules

1. **No autorun from open-folder.** Repo-controlled execution — env activation,
   C# restore/analyzers, task execution, anything in §What counts as code
   execution — never happens as a side effect of opening a file or directory,
   only at the **Trusted** level for that exact canonical path (set explicitly or
   remembered).
2. **Trust and overrides never come from the repository.** All persisted state —
   the trust level, the provider/interpreter override, the `enabled` switch —
   lives in user-global state keyed by canonical path (§Tier 3), so the repo
   cannot vouch for itself or pre-set an override. Dropping the committed
   project-config layer for this feature removes the self-authorization vector
   entirely (a malicious repo has no Fresh file to plant a trust level in). The
   only thing read from the repo is the *standard* ecosystem files, and reading
   them is passive; running them needs Trusted.
3. **Surface the `PATH` risk.** When activation prepends a repo-local `bin/`,
   the info panel states plainly that subsequent tools resolve from inside the
   project, so a user auditing an untrusted repo understands the consequence.
4. **Re-prompt on change of trusted content.** If a Trusted project's `.envrc` /
   `mise.toml` / `activate` value changes on disk, drop back to Restricted and
   re-ask rather than silently running the new code (content-hash the trusted
   value, not just the path).
5. **No execution to render UI.** The pill and picker must be fully populated
   from read-only sources; never run a binary just to label it.

### Beyond env managers: the same levels for C# and the rest

The reported C# issue — *opening a `.cs` file runs commands from the project* —
is the same bug with a different trigger, and it gets the same fix. Loading a
C# project is **not** passive: the language server (OmniSharp/Roslyn) runs
`dotnet restore`/build, evaluates project-specified MSBuild targets, and — most
dangerously — loads the project's **analyzers and source generators**, which are
arbitrary code executed at design time. So:

- **Restricted (default):** the C# LSP **does not start** (or starts in a
  restricted, analyzer-disabled mode if Roslyn supports it); no restore/build
  runs; project tasks don't execute. The rest of the editor works.
- **Trusted:** the server starts normally.
- **Blocked:** nothing C#-related spawns at all, same as everything else.

The general rule for *every* feature that would run repo-controlled content:
check `workspaceTrustLevel()`, degrade gracefully when not Trusted, and surface
why. A dedicated core design doc for the Workspace Trust service is the right
home for the full enumeration of gated behaviors; this section establishes the
contract env managers rely on.

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

## Where it lives: a thin plugin over a live core handle

The split is: **core owns the live env handle and applies it**; the
**`env-manager.ts` plugin owns detection, the snippet defaults, and the UX**.

Core (`EnvProvider`, parallel to `WorkspaceTrust`):
- a shared, interior-mutable provider on the `Authority`, set via the plugin op
  `editor.setEnv(snippet)` / `clearEnv()` — never a `setAuthority` rebuild
- `capture(host, snippet, dir)` runs through the active backend (local / SSH
  agent / docker), with the inputs-hashed cache and clean-stdio parsing
- every spawner reads the provider and applies the env via its native mechanism

Plugin (`env-manager.ts`, composing primitives the devcontainer plugin proves
out):
- passive detection (`fileExists`/`readFile`) → the default snippet table
- the status-bar pill (`registerStatusBarElement`/`setStatusBarValue`),
  pickers (`startPrompt`), trust prompt (`showActionPopup`)
- gates activation on `editor.workspaceTrustLevel() === "trusted"`, then calls
  `editor.setEnv(snippet)`
- persists the per-user override (enabled / snippet) via `setGlobalState`

## Implementation status & phases

Done:
- **Workspace Trust** end to end — enforcement at the spawner choke-point as a
  *mandatory* `Arc<WorkspaceTrust>` field (no decorator, no `Option`), per-project
  persistence, default-deny + prompt-on-open, three levels, host-spawn gating,
  and `editor.workspaceTrustLevel()`.
- **SSH LSP on the remote host** via `RemoteLongRunningSpawner` (closes the old
  Phase-L fallback), with PATH-aware `command_exists`.
- **Native env application** in every spawner (local/ssh/docker), trust gated.
- **`env-manager.ts`** Tier-1 plugin (detect + status pill + trust-gated
  activate), currently still using the `setAuthority`/`local-with-env` path.

Remaining:
1. **`EnvProvider` + `setEnv`/`clearEnv`** — replace the static per-spawner env
   field with the shared live provider and the universal `capture()`; expose the
   plugin op. This is the change that makes env work over SSH and never go stale.
   Retire `SpawnerSpec::LocalWithEnv`.
2. **Migrate `env-manager.ts`** from `setAuthority(local-with-env)` to
   `setEnv(snippet)`, with the default-snippet table + user-snippet override.
3. **Freshness wiring** — watch `.envrc`/`mise.toml`/`pyvenv.cfg`, invalidate the
   capture cache, and restart the LSP on change.
4. **Polish** — info panel, the `env: ⚠` diagnostics path, multi-root.

## Open questions

- **LSP restart on env change.** A running server's env can't change live, so a
  changed `.envrc`/`mise.toml` means restarting the server. Debounce the watch
  so a burst of edits doesn't thrash; consider a "reload" affordance instead of
  auto-restarting under the user mid-task.
- **Per-language vs whole-environment.** A project may want different toolchains
  per language (mise can express this). v1 treats the environment as one
  provider; per-language overrides via `registerLspServer` env are a later
  extension once `LspServerPackConfig` grows an `env` field.
- **Capture latency.** One-shot spawns re-evaluate; the inputs-hashed cache keeps
  the common path free. If a pathological env source isn't a pure function of the
  watched files, fall back to always-evaluate for correctness over speed.
