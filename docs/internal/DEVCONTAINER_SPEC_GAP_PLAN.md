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
P-3). Phases A–E can then add new files and types without inheriting
drift.

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
