# Devcontainer Spec — Additional Tests & Fake-CLI Improvements

A spec-driven walk-through of dev-container behaviors we don't currently
exercise, ordered by severity. Sources: [containers.dev/spec](https://containers.dev/implementors/spec/),
[json_reference](https://containers.dev/implementors/json_reference/),
[json_schema](https://containers.dev/implementors/json_schema/),
[features](https://containers.dev/implementors/features/).

The "interactive walk found this" notes describe what fell out of the
tmux probe in this branch (see `FAKE_DEVCONTAINER_TEST_PLAN.md`); they
turn each spec gap into a concrete reproducer.

## What we cover today

`crates/fresh-editor/tests/e2e/plugins/`:

- `devcontainer_attach_e2e.rs` — happy-path attach, three failure
  modes, F1 stale-log regression, F2 attach-decision persistence.
- `devcontainer_run_lifecycle.rs` — `initializeCommand` (string form)
  via the picker.
- `devcontainer_ports_panel.rs` — basic numeric `forwardPorts` +
  `portsAttributes`.
- `devcontainer_failed_attach_popup.rs` — popup shape + action
  routing.
- `devcontainer_spec_repros.rs` — spec-bug reproducers (all green
  now that the bugs are fixed; locked in as regression guards):
  - **S1** (passing) lifecycle command cwd is `remoteWorkspaceFolder`
  - **S2** (passing) `remoteEnv` propagated via `env` wrapper
  - **S3** (passing) `containerEnv` replay regression guard
- `devcontainer_spec_conformance.rs` — broader spec coverage
  (all 14 passing now that the bugs are fixed):
  - **R1** (passing) object-form lifecycle runs entries in parallel
  - **R2** (passing) object-form runs all entries even on failure
  - **R3** (passing) lifecycle hooks fire in spec order during `up`
  - **G1** (passing) lifecycle array form executes verbatim
  - **G2** (passing) no `remoteUser`/`containerUser` → no `-u` flag
  - **G3** (passing) `remoteUser` falls back to `containerUser`
  - **G4** (passing) JSONC config (`//`, `/* */`, trailing commas)
  - **G5** (passing) subfolder `.devcontainer/<sub>/devcontainer.json`
  - **G6** (passing) `forwardPorts` host:port string renders
  - **G7** (passing) `portsAttributes.onAutoForward` renders
  - **B1a** (passing) default `waitFor=updateContentCommand` blocks `up` at the right point; post-waitFor hooks run in background
  - **B1b** (passing) explicit `waitFor` changes the cutoff
  - **B2** (passing) `shutdownAction: stopContainer` stops on Detach
  - **B3** (passing) `userEnvProbe` applies captured env to lifecycle commands
- `remote_indicator_popup.rs` — Local-with-config and
  Container-state branches of the F6 menu.

Status summary: **All 17 spec-related tests pass**, locked in as
regression guards. The six bugs that originally surfaced as failing
reproducers (S1, S2, R1, R2, B2, B3) have all been fixed in the plugin
— the formerly-red tests now keep them fixed.

---

## High severity — real bugs the fake walk surfaced

### S1. Lifecycle commands run in the wrong cwd vs. the terminal wrapper

**Spec:** lifecycle scripts (other than `initializeCommand`) run inside
the container. Their cwd must be the container-side path
(`workspaceFolder`, default `remoteWorkspaceFolder`).

**What the plugin actually does:**

- Lifecycle picker (`devcontainer.ts:694, 703, 721`) calls
  `editor.spawnProcess(bin, args, editor.getCwd())`. `getCwd()` is the
  **host** workspace path. The `DockerExecSpawner` then uses that as
  `-w` to `docker exec`.
- Terminal wrapper (`devcontainer.ts:1278-1281`) pushes
  `-w <remoteWorkspaceFolder>` (the in-container path). Different code
  path, different value.

**Real-world consequence:** when `workspaceFolder` is overridden (or
the host workspace path doesn't exist inside the container), lifecycle
commands fail with "no such directory" but terminal splits succeed.
The two code paths disagree.

**Test to add:**

```rust
// tests/e2e/plugins/devcontainer_lifecycle_cwd.rs
fn lifecycle_command_cwd_matches_remote_workspace_folder() {
    // remoteWorkspaceFolder = "/workspaces/probe", host =
    // /tmp/<random>. With the fake's "cd <path-or-fail>" semantics
    // (proposed below in F-1), an in-container-only path makes the
    // lifecycle command fail loudly. Today the plugin passes the
    // host path so the fake's lenient cd masks the bug.
}
```

**Fake-CLI improvement (F-1):** make `docker exec -w <path>` exit
non-zero when `<path>` doesn't exist on the host (current code
silently skips the cd). That alone surfaces this bug.

### S2. `remoteEnv` is never applied

**Spec:** "Remote environment variables and user configuration should
be applied to all created processes." `remoteEnv` is the attaching
tool's job, not the container runtime's.

**What the plugin does:** nothing reads `config.remoteEnv`. The probe
in this branch's tmux walk confirmed `DC_TEST_REMOTE=unset` after the
lifecycle command ran.

**Test to add:**

```rust
fn lifecycle_command_sees_remote_env() {
    // devcontainer.json: { "remoteEnv": { "FOO": "bar" } }
    // postCreateCommand prints $FOO to a sentinel file.
    // Asserts file content == "bar".
}
```

**Plugin fix:** before each lifecycle exec, prepend
`["env", "FOO=bar", ...]` to the command, or pass `remoteEnv` through
the spawner (extending `DockerExecSpawner` to accept
`HashMap<String, String>` env).

### S3. `containerEnv` not propagated through the fake

**Spec:** `containerEnv` is set at container creation, so real
`docker exec` sees it as the container's runtime env. Our fake doesn't
simulate that, and the plugin doesn't supplement it either. So even
correctly-implemented containerEnv would be invisible in tests.

**Fake-CLI improvement (F-2):** add `--container-env` reading. When
the fake's `up` records the `<state>/containers/<id>/env` file from
the JSON's `containerEnv`, fake `docker exec` exports those env vars
before running the child. Then a `containerEnv: {FOO: bar}` test sees
`FOO=bar` in the spawned shell.

---

## Medium severity — spec features we're silent about

### M1. Lifecycle order + `waitFor` semantics

**Spec lifecycle order:** `initializeCommand` (host) →
`onCreateCommand` → `updateContentCommand` → `postCreateCommand` (in
the background by default) → `postStartCommand` → `postAttachCommand`.

**`waitFor` enum:** `initializeCommand`, `onCreateCommand`,
`updateContentCommand` (default), `postCreateCommand`,
`postStartCommand`. Tools should block until the named command
finishes before declaring "ready."

**What we test:** only `initializeCommand` (via picker). Nothing
asserts the ordering or that all hooks fire on attach.

**Tests to add:**

```rust
fn attach_runs_all_lifecycle_commands_in_spec_order() {
    // Each hook appends its name + epoch_ns to /tmp/order.log.
    // After attach, file lines are: init, onCreate, updateContent,
    // postCreate, postStart, postAttach (in that order).
}

fn wait_for_blocks_until_named_command_completes() {
    // waitFor: "postCreateCommand", with onCreate / updateContent /
    // postCreate as slow sleeps. Authority must NOT be considered
    // "ready" (no Container indicator) until postCreateCommand
    // returns.
}

fn wait_for_default_is_update_content_command() {
    // Omit waitFor; assert ready fires when updateContentCommand
    // returns even if postCreateCommand is still running.
}
```

**Plugin gap to flag if the test fails:** today `runDevcontainerUp`
relies on the fake CLI returning a single success JSON. There's no
"per-hook" tracking on the plugin side — the spec's auto-run-all-hooks
contract isn't honored; the user has to invoke them one at a time
from the picker.

### M2. Lifecycle "object form" — parallel commands

**Spec:** any lifecycle field accepts an object whose keys are
arbitrary names and whose values are individual commands; the tool
runs them in parallel and the stage succeeds iff every entry exits 0.

**What the plugin does:** `devcontainer_on_lifecycle_confirmed`
(lines 709-728) iterates entries **sequentially** and bails on the
first non-zero exit. Spec violation.

**Test to add:**

```rust
fn lifecycle_object_form_runs_in_parallel_and_waits_for_all() {
    // postCreateCommand: {a: "sleep 0.3 && touch a", b: "touch b"}
    // Wall time must be < 0.5s (parallel), and both files must
    // exist when the picker reports success.
}

fn lifecycle_object_form_fails_when_any_entry_fails() {
    // One entry exits 1; assert "Failed: a (code: 1)" status; both
    // entries still ran.
}
```

### M3. Lifecycle array form

**Spec:** every lifecycle command can be `string` (run via shell) or
`string[]` (exec with no shell parsing). The plugin does support both
(`devcontainer.ts:700-707`) but no test exercises the array path.

**Test:**

```rust
fn lifecycle_array_form_runs_without_shell_interpolation() {
    // postCreateCommand: ["sh", "-c", "echo $$"]
    // (or whatever proves the args were exec'd verbatim, not shell-split)
}
```

### M4. `remoteUser` falls back to `containerUser`

**Spec:** scripts run as `remoteUser`; `remoteUser` defaults to
`containerUser`. If neither is set, defaults to the image's user.

**What we test:** `remoteUser: "vscode"` reaches the spawner.

**Tests:**

```rust
fn remote_user_defaults_to_container_user_when_unset() {
    // devcontainer.json: { "containerUser": "node" } (no remoteUser)
    // assert docker exec gets `-u node`
}

fn no_user_means_no_dash_u_flag() {
    // neither remoteUser nor containerUser set; docker exec must
    // NOT include a `-u` flag.
}
```

### M5. `userEnvProbe`

**Spec:** enum `none | loginShell | loginInteractiveShell |
interactiveShell`. The tool must probe the user's shell env (e.g.
`bash -lic env`) and apply those vars to remote processes.

**What the plugin does:** nothing — `userEnvProbe` isn't read.

**Test:** assert the probe runs at attach (e.g. by setting
`userEnvProbe: "loginShell"` and writing a sentinel from
`/etc/profile`-style path), then assert the env appears in lifecycle
commands. Currently this test would fail; that's the regression
guard for when we implement the feature.

### M6. JSONC support (comments + trailing commas)

**Spec:** devcontainer.json is JSONC (lines starting with `//`,
block `/* */`, trailing commas allowed).

**What the plugin does:** `editor.parseJsonc` (line 147). Should
handle JSONC. No test exercises it.

**Test:**

```rust
fn detect_devcontainer_with_comments_and_trailing_commas() {
    let dc = r#"{
        // This is a comment.
        "name": "with-comments", /* and a block comment */
        "image": "ubuntu:22.04",
        "forwardPorts": [8080,], // trailing comma
    }"#;
    // assert popup appears, ports panel shows 8080.
}
```

### M7. Subfolder discovery — `.devcontainer/<sub>/devcontainer.json`

**Plugin code:** `findConfig()` (line 168-186) walks the
`.devcontainer/` directory looking for subfolder configs after the
two primary paths fail. No test.

**Test:** put config under `.devcontainer/foo/devcontainer.json` only,
assert the plugin still detects + popup fires.

### M8. `forwardPorts` as `host:port` strings

**Spec:** entries are integer or `^([a-z0-9-]+):(\d{1,5})$`. Used to
target a non-`localhost` host (e.g. `"db:5432"` to forward an
inner-network DB port).

**What we test:** numeric only.

**Tests:**

```rust
fn forward_ports_accepts_host_port_string() {
    // "forwardPorts": ["db:5432", 8080]
    // assert ports panel renders both rows correctly.
}
```

### M9. `portsAttributes` ranges + regex keys

**Spec:** keys can be exact ports, ranges (`"40000-55000"`), or
regexes. `onAutoForward` enum:
`notify|openBrowser|openBrowserOnce|openPreview|silent|ignore`.

**What we test:** exact-port keys with `notify` / `silent`.

**Tests:**

```rust
fn ports_attributes_range_matches_port_in_range() {
    // "portsAttributes": { "8000-9000": { "label": "Range" } }
    // forwardPorts: [8080]; assert panel row label = "Range".
}

fn ports_attributes_regex_matches() {
    // /^7\d+$/ matches 7000 etc.
}

fn other_ports_attributes_default_for_unmatched() {
    // otherPortsAttributes.onAutoForward = "ignore"
    // assert a non-listed port renders as ignored.
}
```

### M10. `shutdownAction`

**Spec values:** `none | stopContainer | stopCompose`. Defaults differ
by container source. The attaching tool is responsible for honoring
this on quit.

**What the plugin does:** nothing — `Detach` clears the authority but
doesn't ask the fake/real CLI to stop the container. Quit drops the
authority too without stopping.

**Tests:**

```rust
fn shutdown_action_stop_container_stops_on_detach() {
    // assert fake state's container `status` flips to "stopped"
    // after Detach.
}

fn shutdown_action_none_keeps_running_after_detach() {
    // shutdownAction: "none"; container stays "running".
}
```

---

## Lower severity — discoverability + UX

### L1. Build directives surface (image vs build vs compose)

The plugin's `DevContainerConfig` defines `image`, `build`,
`dockerComposeFile`, `service`. `Show Info` should distinguish the
three sources. Test that the panel renders the right summary line
per source.

### L2. Features panel content

Existing test confirms `forwardPorts` panel renders. Add an analogous
one for the Features panel: features with options (string + boolean)
must show option names alongside values.

### L3. `customizations.<tool>` namespacing

Spec: customizations are tool-namespaced (`customizations.vscode`,
`customizations.fresh`, …). If we ever wire `customizations.fresh.*`
(plugins, themes, settings), each gets a regression test.

### L4. `init`, `privileged`, `capAdd`, `securityOpt`, `runArgs`

These don't apply to attaching tools (they're docker-run flags
consumed by `devcontainer up`), but the plugin's `Show Info` should
list them. Tests assert they render in the info panel without errors.

### L5. `hostRequirements`

Spec: `cpus|memory|storage|gpu`. The plugin parses these into the
config struct. Test: `Show Info` lists them; (optionally) attach
fails fast with a friendly error if `os.cpus() < hostRequirements.cpus`.

### L6. Multi-`config` discovery + active-config switching

When `.devcontainer/<sub-a>/` and `.devcontainer/<sub-b>/` both
exist, the plugin picks the first match. The spec doesn't mandate
which, but VS Code prompts the user. Test that the picker (or a
"choose config" command) works.

---

## Features-specific tests (separate test file recommended)

### Feat-1. `installsAfter` ordering

Round-based sort algorithm. Test: install order respects
`installsAfter` regardless of declaration order in `features` map.

### Feat-2. `dependsOn` recursive resolution

Test: B depends on A, C depends on B; install order is A, B, C.

### Feat-3. `overrideFeatureInstallOrder`

Test: user-specified order takes priority over implicit order, but
cannot violate `dependsOn`.

### Feat-4. Features-contributed lifecycle commands run BEFORE user-defined

Spec quote: "Commands provided by Features are always executed
*before* any user-provided lifecycle commands." Test: a feature's
`postCreateCommand` writes timestamp T1, user's writes T2; assert
T1 < T2.

### Feat-5. `devcontainer-features.env` options pass-through

Test: feature with option `version: "1.0"` sees
`VERSION=1.0` in its env via the spec's `devcontainer-features.env`
mechanism.

These five are gated on whether Fresh actually drives the build /
features step. Today the plugin assumes someone else
(`@devcontainers/cli` or VS Code) already built; if Fresh stays in
that lane these tests live with the upstream CLI, not us.

---

## Fake-CLI improvements that unlock the tests above

| ID | Change | Tests it unlocks | Status |
|---|---|---|---|
| F-1 | `docker exec -w <path>` errors when path doesn't exist on host (today: silent skip; `FAKE_DC_STRICT_CWD=1` opts in) | S1 | **landed** |
| F-2 | Read `containerEnv` from `<state>/containers/<id>/container_env` and export it before exec | S3 | **landed** |
| F-3 | Honor `remoteEnv` similarly via a separate file written by `up` (the plugin would write it) | S2 | open — needs plugin-side `remoteEnv` plumbing too |
| F-4 | Fake `up` parses + runs `onCreate` → `postAttach` lifecycle hooks in spec order; `waitFor` cuts the timeline so post-waitFor hooks run in the background | R3, B1a, B1b | **landed** |
| F-5 | `docker stop <id>` subcommand (records status="stopped") | B2 | **landed** |
| F-6 | Add `docker rm <id>` for `shutdownAction: none` cleanup | M10 (variant) | open |
| F-7 | `up --config <path>` accepting a custom devcontainer.json location | M7 (variant) | open — `M7`'s subfolder branch is already covered by G5 |
| F-8 | Fake `up` resolves `remoteUser` per spec fallback (`remoteUser` > `containerUser` > unset); reports the resolved value (or omits the field) in success JSON | G2, G3 | **landed** |
| F-9 | Fake docker exec exports `FAKE_DC_REQUESTED_CWD` so tests can assert what `-w` the editor actually passed regardless of host fs | S1 | **landed** |

Each F-X is an additive change to
`scripts/fake-devcontainer/bin/{devcontainer,docker}` and
`lib/fake-state.sh`; "landed" entries already ship on this branch.

---

## Status / open work

What's landed on this branch (test layer):

- ✓ **All five S/G groups + R1-R3 + B2-B3** as described in
  "What we cover today." Six failing reproducers (S1, S2, R1, R2,
  B2, B3) + thirteen passing guards.
- ✓ Fake-CLI changes F-1, F-2, F-4, F-5, F-8, F-9.

All six original bugs fixed:

| Spec gap | Fix landed |
|---|---|
| **S1 cwd** | Plugin captures `remoteWorkspaceFolder` at attach into plugin global state; lifecycle commands pass it as `cwd` to `spawnProcess` so `docker exec -w` lands in the in-container path. |
| **S2 remoteEnv** | Plugin reads `config.remoteEnv`; lifecycle commands wrap with `env K1=V1 K2=V2 bin args...`. (GNU `env` doesn't accept `--`, learned the hard way.) |
| **R1 parallelism** | Plugin's object-form branch now uses `Promise.all` and collects per-entry exit codes. |
| **R2 fail-fast** | Same rewrite — every entry runs to completion; the stage fails iff any entry exited non-zero. First failure surfaces in the status message; others are debug-logged. |
| **B2 shutdownAction** | `devcontainer_detach` now resolves `config.shutdownAction` (default `stopContainer`) and spawns `docker stop <id>` via `spawnHostProcess` *before* `clearAuthority`. Failures are debug-logged, never block the detach. |
| **B3 userEnvProbe** | Plugin reads `config.userEnvProbe`; `getOrComputeProbedEnv` spawns the probe shell with appropriate flags (`-l`, `-i`), captures `env` output, caches via plugin global state. `effectiveLifecycleEnv` merges probe ∪ remoteEnv into the env wrapper. |

Spec gaps explicitly NOT covered (need product decisions or
upstream-CLI work):

| Gap | Why deferred |
|---|---|
| **M5 features ordering** (`installsAfter`, `dependsOn`, `overrideFeatureInstallOrder`) | Features installation is `@devcontainers/cli`'s build-time job, not the attaching tool's. Fresh is the attaching tool. Tests live with the upstream CLI. |
| **L1-L5** | "Show Info" panel rendering for various config sections (build directives, features, customizations, hostRequirements, runArgs/init/privileged). Pure UI verification — useful but lower-priority than spec-correctness work. |

---

## Out of scope

- Anything that would require an actual Docker daemon (image build
  caching, real cgroup verification, real network port binding).
  Those tests live with the upstream `@devcontainers/cli` CI, not
  ours.
- Deep `docker-compose` integration. The plugin's `dockerComposeFile`
  + `service` fields are parsed but not driven. If we ever drive
  compose, the test surface there is its own doc.
