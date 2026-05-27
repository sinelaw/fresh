# Orchestrator bring-up fixtures

`v2_worktree_session.json` is a **legacy `windows.json`** snapshot (the
central session registry that was dropped). It's kept as frozen test
data: the bring-up render tests plant it at
`<data>/orchestrator/windows.json` to exercise the one-time migration
path — on first read the editor folds any legacy `windows.json` into the
per-dir workspace files and retires it to `windows.json.retired.bak`,
then discovers sessions from the workspace cache.

Path values are **tokens** substituted with real canonicalized temp
dirs at runtime:

- `__PROJECT__`  — the launch cwd (`fresh <project>` is run here)
- `__WORKTREE__` — a separate worktree dir a session runs in
- `__OTHER__`    — an unrelated project's dir

The fixture is validated by the real reader: each bring-up test
constructs an `Editor`, so a schema mistake surfaces as a failed
parse/migration rather than passing silently.

The session model itself (one session per directory, discovered from the
workspace cache) is characterized in
`tests/orchestrator_bringup_characterization.rs`, which seeds per-dir
workspace files directly rather than fixtures.
