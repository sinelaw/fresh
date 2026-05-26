# Orchestrator bring-up characterization fixtures

These JSON files reproduce the on-disk state a user accumulates by
running the orchestrator and quitting, across the three historical
persistence layouts (issue #2056).

Path values are **tokens** that the test substitutes with real
canonicalized temp dirs at runtime:

- `__PROJECT__`  — the launch cwd (`fresh .` is run here)
- `__WORKTREE__` — a separate git-worktree dir an orchestrator session runs in
- `__OTHER__`    — an unrelated project's dir

Layout each fixture is written to (see the test harness):

- v2 global  → `<data>/orchestrator/windows.json`
- v1 per-cwd → `<data>/orchestrator/<encoded-cwd>/windows.json` (migrated on first read)
- v0.3.6     → `<project>/.fresh/windows.json` (in the working tree)

The fixtures are validated by the real reader: each bring-up test
constructs an `Editor` which calls `read_persisted_windows_env` /
`Workspace::load`, so a schema mistake surfaces as a failed parse
(no sessions) rather than passing silently.

## Provenance

- `v2_*.json` — **captured from the real writer** (`save_orchestrator_state`)
  of this build, via the `orchestrator_fixture_gen` generator
  (`REGEN_ORCH_FIXTURES=1 cargo test ... --test orchestrator_fixture_gen`).
  Note the base window has no `project_path` and an empty
  `plugin_state` — that is exactly what the writer emits for a window
  the orchestrator never tagged.
- `v036_dotfresh.json` — captured from the real **v0.3.6** writer
  (which emitted `<project>/.fresh/windows.json`), via a generator run
  in a `git worktree` checked out at the `v0.3.6` tag. The current
  reader ignores this layout entirely, so its exact contents do not
  affect the outcome — the capture documents the genuine shape.
- `v1_legacy_percwd.json` — the per-cwd-under-data layout the v2
  migration reads. No released tag wrote this layout with a generator
  available here, so it is authored to the schema the migration
  documents; the migration test exercises the real migrator, so a
  wrong shape would fail rather than pass.
