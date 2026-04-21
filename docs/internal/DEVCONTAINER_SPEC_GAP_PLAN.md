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
