# Dev Container Spec — Implementation Plan

Companion to `DEVCONTAINER_SPEC_GAP_ANALYSIS.md`. That document
catalogs the gaps; this one lays out how to close them.

## How to read this plan

The plan is organized into **pre-work** (bugs uncovered during the gap
analysis) plus **five phases** (A–E). Each phase is independently
mergeable — a reviewer can ship A without committing to B, and so on.
Within a phase, work is broken into individual commits that each pass
`cargo check --all-targets` and `cargo fmt` on their own, per
`CONTRIBUTING.md`.

For every work item we record:

- **Why** — the gap or bug from the analysis.
- **Files** — concrete paths touched.
- **Tests** — the e2e and unit coverage needed. Per `CONTRIBUTING.md`,
  every new user-facing flow gets an e2e test that drives
  keyboard/mouse events and asserts on rendered output — never on
  internal state. Bugs get a failing test first, then the fix.
- **Regen** — any `cargo test … write_fresh_dts_file` or
  `./scripts/gen_schema.sh` runs required when touching the plugin API
  or config types.
- **Commit split** — how the work divides into bugfix vs. feature
  commits, so `git log` stays readable.

## Guardrails from `CONTRIBUTING.md`

These shape the plan end-to-end; calling them out once so later
sections can assume them:

1. **`FileSystem` trait for all filesystem access.** Anything that
   reaches for `.devcontainer/devcontainer.json`, a log file, or a
   workspace path must go through `authority.filesystem`, not
   `std::fs` / `std::path::Path::exists`. The container's workspace
   is bind-mounted so paths coincide on local authorities, but remote
   SSH users would silently break without this discipline.
2. **`ProcessSpawner` for external commands.** Authority-scoped
   commands (LSPs, `:term`, plugin `spawnProcess`) must route through
   the active spawner. Host-side plugin work (`devcontainer up`,
   `docker logs`) is the one documented exception — it goes through
   `LocalProcessSpawner` via `spawnHostProcess` even when the active
   authority is a container, because the container may not exist yet
   or may be about to be torn down (see `AUTHORITY_DESIGN.md`).
3. **Tests run in parallel, in isolated per-test workdirs.** No shared
   files, no ambient clipboard state, no fixed timeouts — use
   semantic waits on rendered output.
4. **Regenerate types + schemas** whenever the plugin API or any
   `#[derive(JsonSchema)]` / `#[derive(TS)]` type changes. Each such
   commit bundles the regenerated artifact.
5. **Separate bug fixes from features.** Pre-work commits are
   `fix:`-prefixed; phase commits introducing new surface are `feat:`.

## Scope boundary

Out of scope (reiterated from the gap analysis):

- **Spec §5 "Remote Server Injection"** — injecting a headless editor
  into the container contradicts `AUTHORITY_DESIGN.md` principles 2–4
  and the "shrink the core" stance. Not recommended to close.
- **Spec §7 auto-detection of container-side listening ports** — too
  invasive for a terminal editor; we stop at showing configured
  `forwardPorts` + `docker port` output.

Everything else from the gap analysis is in scope and covered below.

---

## Pre-work — bugs uncovered by the analysis

Three items surfaced while walking the existing implementation. They
are small, independent, and should land before Phase A so the baseline
is clean.

### P-1 · `find_devcontainer_config` bypasses the `FileSystem` trait

**Why.** The helper added in the Remote Indicator popup branch
(`app/popup_dialogs.rs::find_devcontainer_config`) uses
`std::path::Path::exists()` directly. That call reaches for
`std::fs::metadata` under the hood, bypassing
`authority.filesystem`. On SSH authorities it would probe the host
filesystem instead of the remote — silently wrong, exactly the failure
mode `CONTRIBUTING.md` guideline 4 exists to prevent.

**Files.**

- `crates/fresh-editor/src/app/popup_dialogs.rs` — rewrite the helper
  to call `self.authority.filesystem.exists(&primary)`.

**Tests.** Add a regression unit test in `popup_dialogs.rs` (or the
closest existing test module) that installs a mock filesystem
returning `true` for `.devcontainer/devcontainer.json` and asserts the
helper returns `Some(path)`. Failing-first per the bug-fix rule.

**Commit split.** One commit, `fix:`-prefixed.

### P-2 · Verify `plugins/config-schema.json` matches the generator

**Why.** The Remote Indicator branch hand-edited
`plugins/config-schema.json` alongside the `JsonSchema` derive impl in
`config.rs`. Per `CONTRIBUTING.md` guideline 6, the JSON file is an
auto-generated artifact and must come from `./scripts/gen_schema.sh`.
If the two diverge by so much as a whitespace diff, future contributors
will overwrite the hand edit on their next schema regen.

**Files.**

- Run `./scripts/gen_schema.sh`.
- Review `plugins/config-schema.json` diff and commit the regenerated
  file.
- Review `plugins/schemas/theme.schema.json` and
  `plugins/schemas/package.schema.json` too — the script regenerates
  all three and we don't want to leave unrelated drift behind.

**Tests.** None — regeneration is mechanical. A CI check that diffs
the artifact against a fresh regen would catch future drift; adding
that check is out of scope for this pre-work but worth a follow-up
issue.

**Commit split.** One commit, `chore:` or `fix:` depending on whether
the diff is semantic. Mark the generated files as such in the
message.

### P-3 · Regenerate TypeScript plugin definitions (`fresh.d.ts`)

**Why.** The Remote Indicator branch didn't touch the plugin API
surface — it added a core action and a status-bar element, neither of
which is plugin-facing. But the `show_remote_indicator_menu` action
will appear in `Action::all_names()` if we later wire it into the
keybinding editor list, and `fresh.d.ts` enumerates action names
through a `#[derive(TS)]` boundary. Running the regeneration command
now catches any accidental surface creep and keeps the artifact
honest before Phase B adds a real new op.

**Files.**

- Run
  `cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored`.
- Commit `plugins/lib/fresh.d.ts` only if the regen produced a real
  diff; otherwise close out with a note in the PR description.

**Tests.** The regen command *is* the test — it runs through the
generator and diffs against the checked-in file.

**Commit split.** One commit, `chore:` prefix if any diff lands.

### Pre-work acceptance

All three items land before starting Phase A. Collectively they
establish: every devcontainer-adjacent filesystem probe is
authority-routed (P-1), every generated artifact is current (P-2,
P-3). Phases L and A–E can then add new files and types without
inheriting drift.

---

## Phase L · LSP (and other long-running stdio) in the container

While verifying the gap analysis, we found that `LspHandle::spawn`
(`services/lsp/async_handler.rs:2603`) calls
`tokio::process::Command::new(command)` directly and
`services/lsp/mod.rs::command_exists` calls `which::which(...)` —
both bypass the authority. That means when attached to a container,
LSP servers still run on the **host**, looking up binaries in the
**host's** `$PATH`. `rust-analyzer` can't see the project's sysroot
from outside the container, `clangd` misses the container's include
dirs, and any language that's only installed inside the container
simply fails with "binary not found." This is a correctness bug
relative to `AUTHORITY_DESIGN.md` principle 2 ("authority is the sole
router for 'where'"), not a UX gap.

Closing it without adopting the VS Code remote-host model (spec §5)
requires a new trait abstraction: `ProcessSpawner` is one-shot
(`command/args/cwd → SpawnResult`), which doesn't fit a stdin/stdout
LSP server. Phase L introduces the long-running equivalent and routes
LSP through it.

### L-1 · Define `LongRunningSpawner` on `Authority`

**Why.** One trait for every long-lived stdio process (LSP today;
PTY-less tool agents tomorrow) so the container case stays a single
implementation.

**Files.**

- `crates/fresh-editor/src/services/remote/spawner.rs` — new trait:
  ```rust
  #[async_trait]
  pub trait LongRunningSpawner: Send + Sync {
      async fn spawn_stdio(
          &self,
          command: &str,
          args: &[String],
          env: Vec<(String, String)>,
          cwd: Option<&Path>,
      ) -> Result<StdioChild, SpawnError>;

      async fn command_exists(&self, command: &str) -> bool;
  }
  ```
  where `StdioChild` wraps `tokio::process::Child` but exposes only
  `stdin`, `stdout`, `stderr`, `id()`, `kill()`, `wait()`. Wrapping
  is important because the container variant spawns `docker exec`,
  not the binary directly — the rest of the LSP code shouldn't care.
- `crates/fresh-editor/src/services/authority/mod.rs` — add
  `pub long_running_spawner: Arc<dyn LongRunningSpawner>` to
  `Authority`. Constructors (`Authority::local`, `Authority::ssh`,
  `Authority::from_plugin_payload`) wire the right impl.

**Tests.** Trait-object round-trip: spawn `sh -c 'echo hello'` via
each impl, assert the stdout line matches.

**Regen.** None — internal trait, not exposed via plugin API or
JSON schema.

**Commit split.** One commit adding the trait + `StdioChild` +
Authority wiring. No LSP call-site changes yet.

### L-2 · Implement `LongRunningSpawner` for Local and Docker

**Why.** L-1 adds the trait; L-2 provides the two concrete impls.

**Files.**

- `crates/fresh-editor/src/services/remote/spawner.rs` — new
  `LocalLongRunningSpawner` wrapping `tokio::process::Command`. Host
  `command_exists` delegates to `which::which` as today.
- `crates/fresh-editor/src/services/authority/docker_spawner.rs` —
  sibling `DockerLongRunningSpawner` that runs
  `docker exec -i -u <user> -w <workspace> <id> <command> <args>`.
  `-i` keeps stdin open for the LSP's JSON-RPC stream; stdout/stderr
  pipe back through the `docker` CLI like any other child. For
  `command_exists`, run `docker exec <id> sh -c 'command -v <cmd>'`
  and read the exit code.
- SSH authority: for now, delegate `command_exists` to `ssh … 'command -v'`
  and `spawn_stdio` to `ssh … <cmd>`. Ship container first; land SSH
  in the same phase if low-risk, otherwise a follow-up.

**Tests.** Unit test (with a fake `docker` shim on `PATH`) that
asserts the composed command line. `#[ignore]`-gated integration
test that requires a real Docker daemon; contributors with docker
installed run it locally.

**Commit split.** Two commits: local impl first (zero behavioral
change because LSP still uses the direct path), then docker impl.

### L-3 · Route `LspHandle::spawn` through `Authority`

**Why.** L-1/L-2 are dead surface until `LspHandle` adopts the
trait. This is the correctness fix.

**Files.**

- `crates/fresh-editor/src/services/lsp/async_handler.rs`:
  - `LspHandle::spawn` takes `authority: &Authority` as a parameter.
    Replace `Command::new(command)` with
    `authority.long_running_spawner.spawn_stdio(command, args, env, cwd).await`.
  - The existing `stderr_file` fd-redirect doesn't compose with
    `docker exec` — switch to reading from `StdioChild::stderr` in a
    tokio task that writes lines to the log file via
    `authority.filesystem.write` (guideline 4; the log file itself
    lives host-side in both the local and container case because
    the log dir is host-configured).
  - `process_limits` / cgroup application only makes sense for
    host-spawned children. Add a `spawned_locally: bool` on
    `StdioChild` and skip `post_spawn.apply_to_child(...)` when the
    child is `docker`/`ssh` itself rather than the real LSP.
- `crates/fresh-editor/src/services/lsp/mod.rs::command_exists` —
  take `&Authority`, forward to
  `authority.long_running_spawner.command_exists(...)`. Callers in
  `popup_dialogs.rs` and `lsp_status.rs` pass `self.authority()`.
- `LspManager::force_spawn` — plumb the authority down from `Editor`.

**Tests.**

- Unit: mock `LongRunningSpawner` that records the spawn call;
  assert `LspHandle::spawn` passed the right command + args.
- E2E: fake `docker` shim that forks a stub LSP server emitting a
  canned `initialize` response. Boot Fresh with a container
  authority (constructed via test-only helper), open a `.rs` file,
  semantic-wait on the LSP indicator transitioning to "ready";
  assert the shim received `docker exec -i <id> rust-analyzer`-style
  arguments.

**Regen.** None.

**Commit split.** Three commits — each passes `cargo check
--all-targets` on its own:
1. Thread `authority` through `force_spawn` and `LspHandle::spawn`,
   still using the local spawn impl (no behavior change).
2. Switch the spawn call site to `authority.long_running_spawner`.
3. Switch `command_exists` to the authority variant.

### L-4 · User-visible diagnostics for container LSP failures

**Why.** When `command_exists` returns false inside the container,
the current "binary not in PATH" advisory is misleading — it says to
install on the host. Update the copy.

**Files.**

- `crates/fresh-editor/src/app/popup_dialogs.rs` — check the active
  authority when rendering the `(binary not in PATH)` row; if
  container, reword to `(not installed in container)` and swap the
  install hint to point at `postCreateCommand` in
  `devcontainer.json`.
- `locales/en.json` — new strings.

**Tests.** E2E: attach to the fake container shim, open a file whose
LSP binary is absent, trigger the LSP popup, assert the rendered
row says "not installed in container."

**Commit split.** One commit.

### Phase L acceptance

With L-1..L-4 merged: opening a Rust file while attached to a
container spawns `rust-analyzer` inside the container, sees the
project's real sysroot, and responds to `textDocument/hover`
requests as if the editor were running natively inside. The LSP log
panel still works because stderr is forwarded. The authority
contract is preserved — no core code branches on "is this a
container?"; everything goes through `authority.long_running_spawner`.

Phase L does **not** adopt the VS Code remote-host model. We run the
LSP server process inside the container and the editor UI on the
host, with JSON-RPC flowing through `docker exec -i`'s stdio pipe.
Spec §5 stays out of scope for the same reason — we don't need a
second Fresh instance inside the container to get the correctness
property the user actually cares about (servers see the container's
filesystem).

### Phase L consequences for later phases

- **A-4 ("Show Container Logs")** — already uses `spawnHostProcess`
  for `docker logs`; no change.
- **Phase C streaming API** — unrelated; Phase L is long-lived
  stdio routed through authority, Phase C is host-side one-shot
  streaming that deliberately bypasses authority per
  `AUTHORITY_DESIGN.md`.
- **Phase D build-log buffer** — unrelated; build output comes from
  `devcontainer up` on the host.

---

## Phase A · Small spec alignments (plugin-only)

Five low-risk items that don't need new Rust surface. All changes live
in `crates/fresh-editor/plugins/devcontainer.ts` and
`crates/fresh-editor/plugins/devcontainer.i18n.json`. Each ships as
its own commit so the `git log` reads as a checklist of spec-aligning
fixes.

### A-1 · Run `initializeCommand` on the host before `devcontainer up`

**Why.** Gap analysis §6. The spec defines `initializeCommand` as
running on the host before container creation; the plugin currently
lists it in the info panel but never invokes it. This is a correctness
bug, not a UX one.

**Files.**

- `crates/fresh-editor/plugins/devcontainer.ts` — inside
  `runDevcontainerUp`, add a step before the `devcontainer` CLI call
  that reads `config.initializeCommand`, formats it per
  `formatLifecycleCommand`, and runs it via `editor.spawnHostProcess`.
  Abort the attach on non-zero exit with the existing
  `status.rebuild_failed` branch.
- Extend the lifecycle array in `devcontainer_run_lifecycle` to
  include `initializeCommand` so the palette picker offers it too.

**Tests.** E2E: create a fixture workspace with
`.devcontainer/devcontainer.json` whose `initializeCommand` writes a
sentinel file to the fixture's temp dir. Trigger attach, assert the
sentinel exists before the (mocked) `devcontainer up` invocation
completes. Mocking is via `PATH`-prepending a fake `devcontainer`
script written into the fixture — same pattern e2e tests use today for
`git` and LSPs.

**Commit split.** Two commits. First commit: add the lifecycle entry
to the runner picker (pure additive, no behavior change to attach).
Second commit: wire `initializeCommand` into the attach flow —
`fix:`-prefixed because it closes a spec-violation bug.

### A-2 · Rename attach prompt actions to spec wording

**Why.** Gap analysis §2. Plugin labels "Attach" / "Not now" don't
match the spec's "Reopen in Container" / "Ignore". Low-risk copy
change.

**Files.**

- `crates/fresh-editor/plugins/devcontainer.i18n.json` — rename the
  `popup.attach_action_attach` / `popup.attach_action_dismiss` strings
  across every locale. Keep the keys; change the English values and
  re-translate the others or fall back (rust-i18n falls back to `en`
  when a key is missing).
- Consider also retitling the popup itself from "Dev Container
  Detected" to match the spec's "Folder contains a Dev Container
  configuration" phrasing.

**Tests.** E2E: assert the rendered action popup contains "Reopen in
Container". The existing attach-prompt e2e test (if absent, add one)
already renders the popup; the assertion becomes a one-line change.

**Commit split.** One commit, `feat:` or `refactor:` — pure surface
rename.

### A-3 · Scaffold command: "Create Dev Container Config"

**Why.** Gap analysis §1. Remote Indicator menu shows a disabled "No
dev container config detected" row when local and no config exists.
The spec's "Configure Dev Container" option implies a create-flow.

**Files.**

- `crates/fresh-editor/plugins/devcontainer.ts` — new
  `devcontainer_scaffold_config` handler that writes a minimal
  template to `.devcontainer/devcontainer.json` via
  `editor.writeFile`, then opens it. Template content is
  `{ "name": "<workspace>", "image": "mcr.microsoft.com/devcontainers/base:ubuntu" }`
  — deliberately conservative so it's obviously a starting point.
- Register a palette command `Dev Container: Create Config`.
- Optional: have the Remote Indicator popup in core swap the disabled
  hint row for an actionable row that dispatches
  `Action::PluginAction("devcontainer_scaffold_config")`. This is the
  only core change in Phase A; make it a separate commit.

**Tests.** E2E: open a temp workspace without `.devcontainer`,
trigger the scaffold command, assert the file exists and is opened in
a buffer. Second e2e: click the Remote Indicator, assert the
scaffold row is present and actionable.

**Commit split.** Two commits. First: plugin-only scaffold handler +
palette command. Second: wire the row into the Remote Indicator popup
(touches `app/popup_dialogs.rs`).

### A-4 · "Show Container Logs" (one-shot, non-streaming)

**Why.** Gap analysis §1. Remote Indicator popup advertises "Show
Container Info" but the spec calls out "Show Container Logs"
separately — today there is no way to see the container's stdout.

**Files.**

- `crates/fresh-editor/plugins/devcontainer.ts` — new
  `devcontainer_show_logs` handler. Reads the active authority's
  container id (via a new `editor.getAuthority()` op or by parsing
  `display_label` — the latter avoids plugin API churn for now),
  runs `editor.spawnHostProcess("docker", ["logs", "--tail", "1000",
  id])`, and writes the output into a virtual buffer
  `*Dev Container Logs*`.
- Register a palette command `Dev Container: Show Logs`.
- Wire a popup row `Show Container Logs` in
  `app/popup_dialogs.rs::show_remote_indicator_popup` that dispatches
  the plugin action (when attached to a container authority).

**Tests.** E2E: with a fake `docker` shim in `PATH` that emits
scripted log content, trigger the command and assert the virtual
buffer contains the scripted lines.

**Commit split.** Two commits. First: plugin handler + palette
command. Second: core popup row. (Streaming comes later in Phase C —
this cut uses the existing buffered `spawnHostProcess`.)

### A-5 · "Show Forwarded Ports"

**Why.** Gap analysis §7. `forwardPorts` is shown in the info panel
but there's no way to see what the running container actually exposes.

**Files.**

- `crates/fresh-editor/plugins/devcontainer.ts` — extend the existing
  `devcontainer_show_ports` handler to, when a container authority is
  active, run `docker port <id>` via `spawnHostProcess` and merge the
  output with the configured `forwardPorts` list in the prompt
  suggestions.
- Each row's description becomes
  `configured: tcp · runtime: <host-port> → <container-port>` (or
  `configured only` when not bound, or `runtime only` when Docker
  exposes a port not in config).

**Tests.** E2E with a fake `docker` shim: trigger the command, assert
the rendered prompt suggestions match the scripted merge.

**Commit split.** One commit. Scoped to
`devcontainer_show_ports`; doesn't touch other commands.

### Phase A acceptance

With A-1..A-5 merged: `initializeCommand` is honored, the attach
prompt reads per spec, the "Configure Dev Container" path works end
to end, container logs are one command away, and users can see which
configured ports are actually bound. Everything still uses the
buffered `spawnHostProcess`; no new plugin API surface, no state
machine, no indicator sub-states.

---

## Phase B · Remote Indicator state machine

Phase A leaves the Remote Indicator with three states (Local,
Connected, Disconnected). The spec also asks for Connecting/Building
(§3, §4) and a visible failure state that surfaces Retry (§8).
Phase B adds those, plus the plugin op that drives them.

### B-1 · `RemoteIndicatorState::Connecting` + `FailedAttach` variants

**Why.** Gaps §3, §4, §8. The status bar currently has no way to say
"an attach is in progress" or "the last attach failed"; both are
reachable but indistinguishable from Local.

**Files.**

- `crates/fresh-editor/src/view/ui/status_bar.rs` — add two variants
  to `RemoteIndicatorState`:
  - `Connecting { phase: ConnectingPhase, since: Instant }`
  - `FailedAttach { last_error: String }`
  plus a new `ConnectingPhase` enum (`Initialize`, `Build`, `Start`,
  `PostInit`) mapping to the spec's state machine.
- Rendering: `Connecting` uses a Unicode spinner glyph that rotates
  per-frame (`⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏`) plus the authority label.
  `FailedAttach` uses the error palette and renders as
  `[Attach failed — click for options]`.
- `ElementKind::RemoteIndicator(RemoteIndicatorState)` already carries
  the state through; expand the palette selector in `element_style`
  to map the two new variants.

**Tests.** Unit test: assert `element_style` returns a non-default
style for each new variant. E2E test: construct an editor with a
test-only API for setting the state directly (gated behind
`#[cfg(test)]` so it doesn't leak into the plugin surface), assert
the rendered status bar contains the spinner glyph or the "Attach
failed" text.

**Regen.** None — these variants live inside the view crate; no
JsonSchema or TS types.

**Commit split.** One commit. New rendering branches are purely
additive and the default never triggers them, so
`cargo check --all-targets` passes trivially.

### B-2 · Plugin op: `editor.setRemoteIndicatorState(payload)`

**Why.** B-1 adds the states to the view; B-2 gives the plugin a way
to drive them. Without this op the spinner would never appear.

**Files.**

- `crates/fresh-core/src/api.rs` — add a new `PluginCommand` variant:
  ```rust
  SetRemoteIndicatorState {
      state: RemoteIndicatorStatePayload,
  }
  ```
  where `RemoteIndicatorStatePayload` is a tagged enum mirroring the
  view variants but with serializable error strings. Derives:
  `Debug, Clone, Serialize, Deserialize, TS, JsonSchema`.
- `crates/fresh-editor/src/app/plugin_dispatch.rs` — match the new
  variant. Translate the payload into a `RemoteIndicatorState` and
  store it on a new `pending_remote_state: Option<...>` field on
  `Editor`.
- `crates/fresh-editor/src/app/render.rs` — read
  `editor.remote_state()` (new accessor) alongside
  `connection_display_string()`; if `remote_state` is `Some`, it
  overrides the derived Local/Connected/Disconnected state for the
  rendered `{remote}` element.
- `crates/fresh-editor/plugins/lib/fresh.d.ts` — regenerated (see
  Regen below).

**Tests.** Plugin-runtime unit test that sends a
`SetRemoteIndicatorState` command and asserts it round-trips through
`fresh_core::api`. E2E that loads a test plugin calling
`editor.setRemoteIndicatorState({kind: "connecting", phase:
"build"})` on a hook, waits for the next render (semantic wait on the
rendered spinner, not a timer), and asserts the status bar shows it.

**Regen.**

- `cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored`
  for `fresh.d.ts`.
- `./scripts/gen_schema.sh` because the new payload derives
  `JsonSchema` and surfaces in the config schema's `$defs`.

**Commit split.** Two commits. First: Rust-side variant + dispatch +
render integration, with the regenerated `fresh.d.ts` and
`config-schema.json` bundled in (per `CONTRIBUTING.md` artifact
rules). Second: `chore:` commit that only re-runs the generators in
case the first commit's diff isn't byte-identical to a clean regen.

### B-3 · Plugin wiring in `devcontainer.ts`

**Why.** B-1 and B-2 are dead surface until the devcontainer plugin
actually transitions through the states.

**Files.**

- `crates/fresh-editor/plugins/devcontainer.ts` — modify
  `runDevcontainerUp`:
  1. Set `connecting { phase: initialize }` before
     `initializeCommand` (wired in A-1).
  2. Set `connecting { phase: build }` before calling
     `devcontainer up`.
  3. Parse `devcontainer up` JSON; on success call `setAuthority`
     (which restarts the editor — state is reset naturally).
  4. On non-zero exit, set `failed_attach { last_error: stderr }`.
- Add a new handler `devcontainer_retry_attach` that re-runs
  `runDevcontainerUp`. The Remote Indicator popup's FailedAttach
  branch (below) points to this handler.
- On plugin load, check plugin global state for a pending
  `Connecting` marker (set by a previous instance before `setAuthority`
  restarted the editor). If found and an authority is now active,
  clear it. If found and no authority is active, the previous attach
  presumably failed or was cancelled; transition to `FailedAttach`.

**Tests.** E2E with a fake `devcontainer` CLI shim that exits with
status 1 and a scripted stderr: trigger attach, semantic-wait on the
status bar reaching "Attach failed". Second e2e: fake CLI with a
long sleep and success JSON, semantic-wait on the spinner glyph
appearing and the indicator then transitioning to `Connected` once
the shim completes.

**Commit split.** Two commits. First: forward-path state transitions
(happy-path Connecting → restart → Connected). Second: failure path
(`FailedAttach` + retry handler).

### B-4 · Remote Indicator popup updates

**Why.** The popup's context-aware rows must reflect the new states.
Connecting should offer "Show Logs" + "Cancel Startup" (the latter
hooks into Phase C); FailedAttach should offer "Retry" + "Reopen
Locally" + "Show Build Logs".

**Files.**

- `crates/fresh-editor/src/app/popup_dialogs.rs` — extend
  `show_remote_indicator_popup` with branches for the two new
  variants:
  - `Connecting` rows: "Show Logs" (→
    `plugin:devcontainer_show_build_logs`, wired in Phase D) and
    "Cancel Startup" (→ `plugin:devcontainer_cancel_attach`, wired
    in Phase C). Until those plugin handlers exist the rows are
    `disabled()` with a `(coming soon)` suffix — never broken.
  - `FailedAttach` rows: "Retry" (→
    `plugin:devcontainer_retry_attach`), "Reopen Locally" (→
    `detach`, already handled), "Show Build Logs" (→ same Phase D
    handler).

**Tests.** E2E driving the editor into each state and asserting the
popup contents.

**Commit split.** One commit, `feat:`-prefixed.

### Phase B acceptance

With B-1..B-4 merged: the Remote Indicator visibly spins during
attach, the status bar flips to an error palette on failure, and the
popup's rows match the state. Phase C fills in "Cancel Startup" and
Phase D fills in "Show Build Logs" — both currently render as
disabled rows that clearly communicate the feature is coming.

---

## Phase C · Streaming process API + cancellation

Phases A/B visualize attach lifecycle but still rely on `devcontainer
up` running to completion with output buffered in memory. Phase C
introduces line-streamed host-process execution and the kill-handle
plumbing that "Cancel Startup" needs. This is the largest plugin-API
change in the plan; it's gated so every piece is independently
testable.

### C-1 · New plugin command: `SpawnHostProcessStreaming`

**Why.** Gap analysis §4. The current `SpawnHostProcess` returns a
completed `{stdout, stderr, exit_code}`; there is no way to see
output as it arrives, and no handle to cancel.

**Files.**

- `crates/fresh-core/src/api.rs`:
  - New `PluginCommand::SpawnHostProcessStreaming { command, args,
    cwd, process_id, callback_id }`. `process_id` is caller-chosen
    (TS side allocates it) so the TS Promise wrapping the handle can
    correlate kill requests without waiting for a round trip.
  - New `AsyncMessage::PluginProcessStreamLine { process_id, line,
    stream: StdStream }` where `StdStream` is `Stdout | Stderr`.
  - Keep `PluginProcessOutput { process_id, stdout, stderr, exit_code
    }` as the terminal event — `stdout`/`stderr` left empty when
    streaming.
- `crates/fresh-editor/src/app/plugin_dispatch.rs`:
  - Handle the new variant. Spawn via `LocalProcessSpawner`
    (host-side, per `AUTHORITY_DESIGN.md`), drive stdout/stderr with
    `tokio::io::BufReader::lines` and forward each line to the
    async-bridge sender.
  - Store the `tokio::process::Child` handle in a new
    `host_process_handles: HashMap<u64, tokio::process::Child>` on
    `Editor` so a subsequent kill command can find it.
- `crates/fresh-editor/plugins/lib/fresh.d.ts` — regenerated.
- `crates/fresh-editor/plugins/lib/fresh.ts` (or wherever the TS
  surface shim lives) — implement `spawnHostProcessStreaming(command,
  args, cwd?)` returning
  `{ processId, onStdout, onStderr, wait, kill }`. Under the hood
  the function registers `onStdout` / `onStderr` callbacks keyed by
  `processId`, then issues the plugin command.

**Tests.** Unit test that serializes/deserializes every new
variant. Integration test that spawns `sh -c 'for i in 1 2 3; do echo
$i; sleep 0.05; done'` through the new API and asserts the three
lines arrive before the exit event. Use semantic-wait on the exit
event, not a timer.

**Regen.** `cargo test -p fresh-plugin-runtime write_fresh_dts_file
-- --ignored` and `./scripts/gen_schema.sh`.

**Commit split.** Two commits. First: Rust-side variants + dispatch +
child-handle storage. Second: TS surface (`fresh.d.ts` regen +
`fresh.ts` shim).

### C-2 · New plugin command: `KillHostProcess`

**Why.** Pairs with C-1. `Cancel Startup` in the Remote Indicator
popup needs a way to actually stop `devcontainer up`.

**Files.**

- `crates/fresh-core/src/api.rs` — new
  `PluginCommand::KillHostProcess { process_id }`.
- `crates/fresh-editor/src/app/plugin_dispatch.rs` — look up the
  handle from `host_process_handles`, call `Child::kill()`, remove
  from the map. Gracefully no-op (with `tracing::debug!`) when the
  id is unknown — the handle may have already exited.
- `crates/fresh-editor/plugins/lib/fresh.ts` — the `kill()` method on
  the returned handle object calls this command with the stored
  `processId`.

**Tests.** Integration test: spawn `sh -c 'sleep 10'` streaming, call
`kill()`, semantic-wait on exit event. Assert the exit event arrived
with a non-zero `exit_code` (signal termination convention is `-1` in
`SpawnResult`).

**Regen.** As above.

**Commit split.** One commit. Kill is the minimum viable surface;
timeouts and signal-choice knobs (SIGTERM vs SIGKILL, graceful kill
with timeout) can come later if asked.

### C-3 · Rewrite `runDevcontainerUp` to stream

**Why.** C-1 and C-2 are dead surface until the devcontainer plugin
adopts them.

**Files.**

- `crates/fresh-editor/plugins/devcontainer.ts`:
  - Replace the single `await editor.spawnHostProcess("devcontainer",
    args)` with `spawnHostProcessStreaming`. Collect stdout into a
    buffer for the final JSON parse (the JSON line is emitted on
    stdout at the end; streaming doesn't change that).
  - Forward each stdout/stderr line via a new `onBuildLine(line,
    stream)` callback — currently dumps to `editor.debug`; Phase D
    replaces this with a write to the build-log virtual buffer.
  - Store the returned `processId` in a module-level
    `attachInFlight: ProcessHandle | null` so
    `devcontainer_cancel_attach` can call `attachInFlight?.kill()`.
  - New handler `devcontainer_cancel_attach` that calls `.kill()` on
    the in-flight handle, then sets
    `RemoteIndicatorState::Local` (cancelled is not a failure, it's
    a user-initiated revert).

**Tests.** E2E with the existing fake-CLI shim extended to sleep
before emitting JSON. Trigger attach, semantic-wait on
`Connecting`, open the popup, confirm the "Cancel Startup" row,
semantic-wait on the indicator returning to Local. Assert the fake
shim actually received a termination (the shim can write a sentinel
file in its signal handler).

**Commit split.** Two commits. First: adopt streaming for the
happy-path (no cancellation yet). Second: cancellation handler +
`attachInFlight` tracking.

### C-4 · Finalize the "Cancel Startup" popup row

**Why.** Phase B's B-4 stubbed the row as disabled; now we can enable
it.

**Files.**

- `crates/fresh-editor/src/app/popup_dialogs.rs` — drop the
  `(coming soon)` suffix and the `disabled()` call for the row.

**Tests.** Reuse the C-3 e2e but assert the popup row is actionable
(no `[dim]` overlay in the rendered output — the closest proxy for
`disabled` in terminal tests).

**Commit split.** One commit.

### Phase C acceptance

With C-1..C-4 merged: `devcontainer up` output streams to `debug`
(Phase D makes it user-visible), the user can cancel an in-flight
attach from the Remote Indicator menu, and the plugin API has a
reusable streaming-spawn/kill primitive that future plugins can use.
No other core surface has changed; `DockerExecSpawner` and the
authority contract are untouched.

---

## Phase D · Build-log panel + retry UX

Phase C streams lines but sends them to `editor.debug`. Phase D makes
those lines user-visible in a dedicated buffer and closes the loop on
the "Show Build Logs" / "Retry" popup rows that Phase B stubbed.

### D-1 · `*Dev Container Output*` virtual buffer

**Why.** Gap analysis §4. The spec wants a "dedicated 'Dev Container
Output' terminal" that streams stdout/stderr live. Fresh already
supports virtual buffers via `editor.createVirtualBufferInSplit` —
reusing that avoids introducing a new buffer flavor.

**Files.**

- `crates/fresh-editor/plugins/devcontainer.ts`:
  - New module-level `buildLogBufferId: number | null`. Lazily create
    the virtual buffer the first time a build line arrives after an
    attach starts. Close it on successful attach (it will be recreated
    next time) but **keep** it open on failure so "Show Build Logs"
    is not an empty tab.
  - Replace the C-3 `onBuildLine` stub with `appendToBuildLog(line,
    stream)`: read current content, append
    `line + "\n"` (prefixed with `stderr: ` for the stderr stream to
    keep interleaving readable), write back. Virtual-buffer
    `setVirtualBufferContent` is the existing API; if it doesn't
    support efficient append, add a new `appendVirtualBuffer(id,
    text)` plugin command alongside (see D-2).
  - New handler `devcontainer_show_build_logs` — opens the buffer in a
    split (focusing it if already visible). Uses the same
    `createVirtualBufferInSplit` pattern as the info panel.

**Tests.** E2E with the streaming fake CLI from Phase C: trigger
attach, semantic-wait on the spinner, trigger "Show Build Logs" from
the popup, assert the rendered buffer contains the scripted lines.

**Commit split.** One commit, `feat:`-prefixed.

### D-2 · (Conditional) `AppendVirtualBuffer` plugin command

**Why.** If profiling D-1 shows `setVirtualBufferContent` rewrites the
whole buffer per line — which it likely does given the Rope storage —
high-volume builds (`cargo build` emitting thousands of lines) will
cause an O(n²) slowdown.

**Decision gate.** Run D-1 with a build that emits ~1000 lines and
measure; only add D-2 if the profile shows quadratic behavior. Log
the measurement in the PR description either way.

**Files (if needed).**

- `crates/fresh-core/src/api.rs` — new
  `PluginCommand::AppendVirtualBuffer { buffer_id, text }`.
- `crates/fresh-editor/src/app/plugin_commands.rs` — handle via an
  insert at the buffer's end rather than a full-content replacement.
- `crates/fresh-editor/plugins/lib/fresh.ts` —
  `editor.appendVirtualBuffer(id, text)`.

**Tests.** Microbenchmark in the unit-test layer: append 10k short
lines and assert total time is under some budget. Standard
CONTRIBUTING testing rule: no fixed timeout — the test uses a
`Duration` comparison to a generous budget that's still strictly
sub-quadratic (e.g. 2s for 10k lines).

**Regen.** `fresh.d.ts` + `config-schema.json`.

**Commit split.** One commit. Preceded by the profile data in the
PR description.

### D-3 · Wire the Phase B disabled rows to the new buffer

**Why.** Phase B stubbed "Show Logs" (Connecting) and "Show Build
Logs" (FailedAttach) as disabled rows; both should now dispatch
`plugin:devcontainer_show_build_logs`.

**Files.**

- `crates/fresh-editor/src/app/popup_dialogs.rs` — drop the
  `disabled()` and `(coming soon)` from those rows; they're now
  actionable.

**Tests.** Extend the Phase B e2e tests that assert the popup
contents for each state.

**Commit split.** One commit.

### D-4 · Failed-attach action popup with "Retry" / "Reopen Locally"

**Why.** Gap analysis §8. Spec calls for a notification on build
failure with Retry / Reopen Locally; we want both the Remote Indicator
popup path (already covered by Phase B's FailedAttach branch) *and* a
proactive action popup so the user doesn't have to go hunting for the
indicator.

**Files.**

- `crates/fresh-editor/plugins/devcontainer.ts` — after setting
  `FailedAttach`, show an action popup via
  `editor.showActionPopup({...})` with:
  - `Retry` → `devcontainer_retry_attach`
  - `Show Build Logs` → `devcontainer_show_build_logs`
  - `Reopen Locally` → already-`Local` no-op but shown for symmetry
  - `Dismiss`
  - The first three map to existing handlers; Dismiss leaves the
    `FailedAttach` indicator in place (the user may want to retry
    later from the popup menu).

**Tests.** E2E with the failing fake CLI: trigger attach, semantic-
wait on the "Attach failed" action popup, select "Show Build Logs",
assert the build-log buffer is focused.

**Commit split.** One commit.

### Phase D acceptance

With D-1..D-4 merged: build output is live-visible in a dedicated
buffer, failure surfaces a user-prompted Retry/Show Logs popup, and
the Remote Indicator popup's previously-stubbed rows all dispatch to
real handlers. The §4 and §8 gaps are fully closed. The only
remaining spec items are §7 (customizations + ports), which Phase E
picks up.

---

## Phase E · Customizations and port forwarding

The final phase covers what §7 ("Ready State") asks of a container-
active editor beyond launching shells at the right cwd. Two separate
concerns land here; they are sequenced but do not depend on each
other.

### E-1 · `customizations.fresh.plugins` namespace

**Why.** Gap analysis §7. The spec calls out
`customizations.vscode.extensions`; VS Code extensions don't apply to
Fresh (different plugin model), but the *shape* of the feature is
worth mirroring under a Fresh-specific namespace.

**Design.** The plugin reads
`config.customizations?.fresh?.plugins` as `string[]`. Each entry is
a plugin file path relative to the workspace root, or a built-in
plugin name. After attach, the devcontainer plugin iterates and
invokes `editor.loadPlugin(path)` for each. Paths go through
`authority.filesystem` so they resolve inside the container on the
container authority (where the plugin files presumably live).

**Files.**

- `crates/fresh-editor/plugins/devcontainer.ts`:
  - On `plugins_loaded` when the active authority carries a
    `Container:` label, read `config.customizations?.fresh?.plugins`
    and call `editor.loadPlugin(path)` per entry.
  - Entries already loaded (by path) are skipped with an `editor.debug`
    message — this avoids double-load after a reconnect.
- No Rust code change required. `editor.loadPlugin` already exists
  per the existing pattern used by the init-script loader.

**Tests.** E2E: fixture workspace with
`.devcontainer/devcontainer.json` containing
`customizations.fresh.plugins = ["./my-test-plugin.ts"]`, and a
sibling `my-test-plugin.ts` that registers a command with a
distinctive name. Trigger attach (with the fake CLI that succeeds),
semantic-wait on the new command appearing in the palette.

**Commit split.** One commit.

### E-2 · Document the namespace

**Why.** `customizations.fresh.*` is now a supported extension point;
plugin authors need to discover it.

**Files.**

- `docs/internal/DEVCONTAINER_PLUGIN_DESIGN.md` — add a
  "Customizations" section describing the `fresh.plugins` array,
  noting that paths resolve through the container's filesystem and
  listing what `vscode.extensions` does **not** do (install VS Code
  extensions).
- `README.md` for the devcontainer plugin (if one exists; otherwise
  skip).

**Commit split.** One commit, `docs:`-prefixed.

### E-3 · Port forwarding visibility

**Why.** Gap analysis §7. Phase A's A-5 merges configured
`forwardPorts` with `docker port <id>` output in the palette picker;
E-3 extends that to a standalone status view.

**Files.**

- `crates/fresh-editor/plugins/devcontainer.ts`:
  - New handler `devcontainer_show_forwarded_ports_panel` that opens
    a virtual buffer `*Dev Container Ports*` tabulating:
    - `forwardPorts` entries (configured)
    - `portsAttributes` labels / protocols / `onAutoForward` policy
    - runtime `docker port <id>` binding output
  - Columns: `Configured | Protocol | Label | Runtime binding`.
  - Refresh button (`r` keybinding within the panel, following the
    info-panel button-row pattern already in `devcontainer.ts`).
- Register a palette command `Dev Container: Show Forwarded Ports`
  (distinct from the picker-style `Show Ports` already registered —
  this one opens the full panel).

**Tests.** E2E: attach via fake CLI, trigger the panel, assert rows
for each configured port with the expected runtime binding string
(the fake `docker port` shim scripts the output).

**Commit split.** One commit.

### E-4 · (Deferred by design) auto-forward detection

The spec asks to "Detect any ports opened by the containerized
application and offer to forward them to localhost." Per the gap
analysis scope boundary, Fresh does not watch container-side
listeners. Users see what's configured and what Docker actually bound
via E-3; anything further would require a `docker exec <id> ss -tln`
loop that the terminal editor shouldn't run unbidden.

If demand for this appears post-launch, the cleanest add-on would be
a one-off palette command `Dev Container: Scan for Listening Ports`
that runs `ss -tln` inside the container and offers to add each new
entry to the panel — triggered, not continuous.

### Phase E acceptance

With E-1..E-3 merged: `customizations.fresh.plugins` is a documented
extension point, port forwarding has a dedicated live panel, and
Phase A's picker still works for quick lookups. All §7 spec items
within the declared scope are implemented.

---

## Open questions — decide before starting each phase

These are not blockers but decisions that should land before the
first commit of the phase they gate. Recorded here so the reviewer
and implementer can align up front.

### Q-A1 · `initializeCommand` run direction for SSH authorities

The spec says `initializeCommand` runs on the host. Fresh's
`spawnHostProcess` currently means "local machine" regardless of
authority — correct for plain local attach, wrong for an SSH
authority that discovered a `devcontainer.json` on the remote and
wants to attach to a container on that same remote. The first
version (Phase A) should just run it via `spawnHostProcess`
(host-local) and accept the SSH-nested-container edge case as a
known limitation; revisit if we ever grow that workflow.

### Q-B2 · Should `setRemoteIndicatorState` be idempotent-within-a-tick?

If the plugin calls `setRemoteIndicatorState` three times in one
event loop tick, the first two are overwritten before any render.
This is fine functionally but means test harness semantic waits on
intermediate states can miss them. Options:

1. Keep the naive "last writer wins" — simplest, matches every other
   plugin op.
2. Queue state transitions so each renders at least once.

Recommendation: option 1. Tests that need to observe intermediate
states should drive timing with `editor.awaitRenderTick()` (a plugin
API we don't have yet — if this recommendation doesn't hold up, add
that op rather than complicate the state machine).

### Q-B3 · Recovery heuristic after unclean shutdown

Phase B-3 says "on plugin load, check for a pending Connecting
marker; if found and no authority is active, transition to
FailedAttach." But the marker could also survive a deliberate crash,
an OOM kill, a `SIGKILL` from the user. How long should a stale
marker linger before we assume it's unreachable?

Proposal: timestamp the marker; if older than 30 minutes on load,
quietly clear without surfacing FailedAttach. 30 minutes is a guess
— adjust based on feedback.

### Q-C1 · Line-buffering vs. raw-byte streaming

Phase C-1 assumes line-delimited events. `devcontainer up` emits
JSON, which is one line at the end but may include progress updates
on earlier lines that are reasonable to surface. If a plugin wants
raw bytes (e.g. a future LSP log streamer), this API won't serve it.

Decision: line-delimited is fine for every current use case and is
the shape every existing log-streaming API in terminal editors uses.
If raw-byte arrives as a need, add a second variant
`SpawnHostProcessStreamingRaw` alongside — don't change C-1's
shape.

### Q-C2 · Kill signal semantics

`Child::kill()` sends SIGKILL on Unix. `devcontainer up` may own
child `docker` processes that SIGKILL leaks. We can address this
later with process-group handling; the initial Phase C cut accepts
possibly-leaked children, with a log line noting the limitation.

### Q-D1 · Buffer vs. terminal for build output

Fresh has virtual buffers and integrated terminals. Terminals can
render ANSI color codes; buffers cannot. `devcontainer up` emits
color when its stdout is a TTY. We have three options:

1. **Virtual buffer, strip ANSI** (simplest; loses color).
2. **Virtual buffer, translate ANSI to overlays** (most work; color
   preserved).
3. **Embedded terminal with the streaming output piped in** (uses
   existing TTY infra; more plumbing at the plugin boundary).

Recommendation: option 1 for Phase D. Revisit if users complain
about lost color context. This matches the existing info-panel
pattern which is also a colored-via-overlay virtual buffer.

### Q-E1 · Plugin load scope for `customizations.fresh.plugins`

If a plugin listed in `customizations` depends on an npm package
installed by `onCreateCommand`, the plugin file may reference symbols
that don't exist until the lifecycle hook has run. Phase E-1 runs
plugin loads after `plugins_loaded`, which fires after authority
install — but the first run through the state machine may load them
before `onCreateCommand` output is observable. Any load failure
becomes a silent log line today.

Proposal: surface load failures via the same action-popup mechanism
as attach failures. Deferred to a follow-up PR post-E-3.

---

## Summary table

| Phase | Scope | Gap items closed | New plugin API | Lines of Rust added (est.) |
|-------|-------|------------------|----------------|-----------------------------|
| Pre | 3 cleanups | — | none | ~20 |
| A | Plugin-only spec alignment | §1 (partial), §2, §6, §7 (partial) | none | ~30 (scaffold-row wiring) |
| B | Indicator state machine | §3, §4 (visibility), §8 (display) | `setRemoteIndicatorState` | ~200 |
| C | Streaming host spawn + cancel | §4 (logs/cancel) | `spawnHostProcessStreaming`, `killHostProcess` | ~250 |
| D | Build-log buffer + retry popup | §4 (logs shown), §8 (retry) | `appendVirtualBuffer` (conditional) | ~50 |
| E | Customizations + ports panel | §7 | none | ~0 (pure plugin) |

Each row's estimate is a rough ballpark for review-planning purposes;
actual numbers will emerge from the PRs.

---

## Closing

Pre-work + five phases close every in-scope spec gap identified in
`DEVCONTAINER_SPEC_GAP_ANALYSIS.md`. The plan respects Fresh's
architectural principles (authority opacity, one-slot transition,
plugin-owned backend lifecycle) and lines up with `CONTRIBUTING.md`
commit, test, and regeneration discipline throughout. Each phase can
stand on its own: if Phase C is cancelled after Phase B, the Remote
Indicator still works as a visible-but-non-cancellable state
machine, which is strictly more useful than today's always-Local
display.
