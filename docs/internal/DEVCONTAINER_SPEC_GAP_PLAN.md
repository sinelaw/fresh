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
