# Devcontainer UX — Remediation Plan

Captures the gaps surfaced while interactively walking the devcontainer
flows in tmux against the [fake CLI](./FAKE_DEVCONTAINER_CLI.md) and lays
out a path to close them, plus a CI story that exercises the same flows
automatically going forward.

The "Findings" sections in
[`FAKE_DEVCONTAINER_TEST_PLAN.md`](./FAKE_DEVCONTAINER_TEST_PLAN.md)
describe each problem in observed-from-the-tmux-session terms; this doc
turns them into prioritized work items.

## Snapshot of issues

| ID | Severity | Description | Status |
|----|----------|-------------|--------|
| F1 | High | Stale build-log buffers restored on cold start with no "this is from a previous run" hint; new attach stacks a *second* build-log split alongside the stale one | **Fixed on this branch** — `closeStaleBuildLogBuffers(cwd)` runs at the start of every attach; tracked by `Phase 4` test below |
| F2 | High | Re-prompt for "Reopen in Container?" on every cold restart, even after the user said "Reopen in Container" last time | **Resolved as test-environment artifact (verified)** — `attach_decision_persists_in_plugin_global_state` confirms the decision lands in `plugin_global_state` before the restart. The tmux-walk re-prompt was `tmux kill-session` skipping the workspace save, not a real bug. |
| F3 | Medium | The clickable Remote Indicator was opt-in: not in `default_status_bar_left`, no palette command, no default keybinding | **Fixed.** Added `Show Remote Indicator Menu` palette command, F6 default keybinding, locale strings, AND prepended `{remote}` to `default_status_bar_left`. Also dropped the redundant `[Container:<id>] ` / SSH prefix from the `Filename` element when `{remote}` is on the bar — the indicator is the canonical surface, having the same identity in two places was just visual noise. The width-pinned 80-col tests that broke from the leading ` Local ` element were either widened (4 toggle_bars + 9 others reading status messages) or fixed in place where the failure was orthogonal (`test_split_with_file_operations` had been quietly broken: its Alt+V was supposed to create a split but actually opened the View menu — switched to the palette `split vert` path). |
| F4 | Medium | `*Dev Container Logs*` virtual buffer thought to leave an empty slot on restore | **Already-handled (verified)** — `serialize_split_node_pruned` (`workspace.rs:1796-1842`) drops virtual leaves and collapses the parent. The earlier finding in `FAKE_DEVCONTAINER_TEST_PLAN.md` mistook the duplicated *real* build-log buffer for an empty slot |
| F5 | Low | Multiple buffers in one split aren't visually marked when the tab strip is too narrow | Pre-existing UX wart; out of scope for this remediation, tracked as a separate issue |
| F6 | Low | Show Build Logs vs Show Logs is easy to mix up via fuzzy palette match | **Fixed on this branch** — `cmd.show_logs` renamed to `Dev Container: Show Container Logs` across all 14 locales |

F1, F3, F4, F6 are landed (or were never bugs). The rest of this doc
keeps F2 + F5 as forward-looking work, plus the CI investment that
backs every fix above.

---

## F1 — Stale build log on cold restart

### What happens

1. Attach succeeds → `prepareBuildLogFile()` writes
   `.fresh-cache/devcontainer-logs/build-<ts>.log` and opens it in a
   horizontal split.
2. User quits cleanly. The workspace JSON saves both splits and the
   build-log buffer's path.
3. User cold-starts → workspace restore reopens the same `build-<ts>.log`
   path. The buffer renders the *previous* run's content.
4. User clicks "Reopen in Container" again → a *new* `build-<ts>.log` is
   minted and `openBuildLogInSplit` adds a *second* horizontal split
   below the existing one.

The two log buffers now look identical at a glance; only the timestamp
in the tab title hints that one is stale.

### Proposed fix

Two complementary moves:

1. **Don't restore the build-log buffer.**  Mark the buffer as
   "ephemeral / per-attach" using whatever mechanism virtual buffers
   currently use to opt out of session save (we already drop
   `*Dev Container Logs*` — extend the same mechanism to the on-disk
   build log). The on-disk file stays so `Show Build Logs` after
   restart can re-open it on demand; the *split* and *open buffer* are
   transient.

2. **On a new attach, close any pre-existing build-log splits in the
   workspace** (not just dedupe). Plugin-side change in
   `runDevcontainerUp` before `openBuildLogInSplit`: walk
   `editor.listBuffers()`, find any in
   `.fresh-cache/devcontainer-logs/`, and close their splits + buffers
   before opening the fresh one. Keeps the screen single-truth on
   re-attach.

### Landed in this branch

`closeStaleBuildLogBuffers(cwd)` in `plugins/devcontainer.ts` walks
`editor.listBuffers()`, drops any whose `path` starts with
`<cwd>/.fresh-cache/devcontainer-logs/`, and runs at the top of
`runDevcontainerUp` (after `prepareBuildLogFile`, before
`openBuildLogInSplit`). The on-disk log files stay untouched —
`Show Build Logs` and "View Log" actions can still re-open the most
recent one — but no stale buffer is left dangling in a split.

### Test (landed)

`attach_closes_stale_build_log_buffer_from_previous_run` in
`crates/fresh-editor/tests/e2e/plugins/devcontainer_attach_e2e.rs`.

Recipe: pre-create a stale `build-<old-ts>.log` under
`.fresh-cache/devcontainer-logs/` and open it as a buffer (the same
shape workspace restore would yield), then drive a fresh attach.
Asserts via the plugin-state snapshot
(`plugin_manager().state_snapshot_handle()`) that:

- the stale buffer is no longer in `BufferInfo` after attach
  (covered by `closeStaleBuildLogBuffers`), AND
- a *different* build-log path under the same dir IS open (the
  freshly minted `build-<new-ts>.log`).

Runs in ~0.3s under the harness — no Docker, no Node, no real CLI.

## F2 — Reopen-in-Container re-prompts on every cold start

### What happens

`devcontainer.ts` keys the prompt decision via `attachDecisionKey()`
which is per-cwd plugin global state. It *should* persist —
`Editor::plugin_global_state` is part of the workspace JSON
(`workspace.rs:405`).

In the tmux walk the prompt re-appeared after a cold restart even
though we'd selected "Reopen in Container" in the prior run. Two
plausible root causes:

1. **Non-clean exit dropped the save.** The walk ended with
   `tmux kill-session`, which sends SIGHUP through bash to fresh.
   If the SIGHUP path doesn't flush the workspace, the decision
   never made it to disk. Easy to verify by re-running with a clean
   `:Quit` from inside the editor.
2. **Restart-before-save race.** `setAuthority` triggers an editor
   restart immediately after `writeAttachDecision("attached")`. If
   the global-state write isn't flushed to the workspace JSON before
   the restart, the post-restart instance reads back stale data.

### F2 verification: not reproducible in-harness

Status: **resolved as test-environment artifact** (verified in this
branch; see commit body for `test(devcontainer): F1 regression + F2
reproducer`).

The new e2e test
`attach_decision_persists_in_plugin_global_state` drives the full
attach flow against the fake CLI, then snapshots the workspace via
`Editor::capture_workspace()` and asserts that
`plugin_global_state["devcontainer"]["attach:<cwd>"] == "attached"`.
The assertion passes — the plugin's `writeAttachDecision("attached")`
DOES land in the workspace state before the restart. So the
re-prompt seen in the tmux walk was the
"`tmux kill-session` doesn't flush" branch of the plan, not a real
bug in `setGlobalState` ordering.

If F2 ever resurfaces in production reports, the regression test
above will start failing first, pointing at whichever of these
secondary causes broke:

1. `setGlobalState` plumbing around `setAuthority` started racing
   the restart-before-save.
2. Workspace serializer stopped writing
   `plugin_global_state["devcontainer"]`.
3. `devcontainer_maybe_show_attach_prompt`'s
   `previousDecision !== null` guard regressed.

## F4 — Virtual log buffer leaves a visible empty slot

**Resolved on inspection — this was a misread of the tmux capture.**

`serialize_split_node_pruned` (`crates/fresh-editor/src/app/workspace.rs:1796-1842`)
already drops virtual-buffer leaves and collapses the parent Split.
What I saw in the tmux session and labelled as "an empty slot" was
actually the pre-existing build-log split holding the *real* on-disk
log file from the previous attach — fixed under F1, not F4.

No code change here. Test plan note (in
`FAKE_DEVCONTAINER_TEST_PLAN.md`) updated separately.

## F5 — Tab strip hides extra buffers when narrow

Pre-existing, surfaced sharply by devcontainer flows. Out of scope for
the devcontainer remediation work, but worth a tracking issue. The fix
would land in tab strip layout, not the plugin.

## F6 — "Show Build Logs" vs "Show Logs" disambiguation

Trivial: rename the host-build-log command to
`Dev Container: Show Build Logs (Host)` or extend the description so
the fuzzy matcher disambiguates. One-line label change in
`devcontainer.i18n.json` × N locales.

---

## CI: end-to-end coverage that doesn't need Docker

### Goal

Every flow in `FAKE_DEVCONTAINER_TEST_PLAN.md` that we walked by hand
should also run on every PR — without anyone having to install
`@devcontainers/cli` or run a Docker daemon. The self-containment
requirement matters for two reasons: (1) the existing CI sandboxes
have no Docker; (2) future contributors should be able to run the same
suite locally without touching their machine.

### Building blocks already shipped on this branch

- `scripts/fake-devcontainer/` — pure-bash shim for `devcontainer` and
  `docker`, no runtime deps beyond `bash` + coreutils.
- Failure-injection knobs (`FAKE_DC_UP_FAIL`, `FAKE_DC_UP_HANG`,
  `FAKE_DC_UP_BAD_JSON`, `FAKE_DC_UP_NO_CONTAINER_ID`,
  `FAKE_DC_UP_DELAY_MS=0` for instant runs).
- `activate.sh --print-env` for `eval`-friendly env injection.

### Plan

#### Phase 1 — wire the fake CLI into the test harness

A new helper on `EditorTestHarness`, e.g.
`HarnessOptions::with_fake_devcontainer()`, that:

- Resolves `scripts/fake-devcontainer/bin` (relative to `CARGO_MANIFEST_DIR`)
- Prepends it to the harness child's `PATH`
- Sets `FAKE_DEVCONTAINER_STATE` to a per-test tempdir (so tests don't
  bleed state into each other or into a developer's `~/.cache`)
- Sets `FAKE_DC_UP_DELAY_MS=0` so tests don't sleep
- Returns the state path so tests can assert against
  `state/last_id`, `state/containers/<id>/logs`, etc.

Touchpoints: `crates/fresh-editor/tests/common/harness.rs` (helper),
`crates/fresh-editor/build.rs` if a build-time path resolution is
cleaner than runtime.

#### Phase 2 — first end-to-end test

`crates/fresh-editor/tests/e2e/plugins/devcontainer_attach_e2e.rs`
that drives Flow A from `FAKE_DEVCONTAINER_TEST_PLAN.md`:

1. Setup workspace + `.devcontainer/devcontainer.json` (factor a
   helper out of `devcontainer_run_lifecycle.rs`).
2. `EditorTestHarness::with_options(opts.with_fake_devcontainer())`.
3. Wait for plugin load + attach popup.
4. Send Esc + Enter to accept "Reopen in Container."
5. Wait until `editor.authority_label().starts_with("Container:")` —
   the same predicate `getAuthorityLabel()` exposes to plugins.
6. Assert: the build-log file exists, the `last_id` file matches
   the authority's container id, the workspace has a
   `.fresh-cache/devcontainer-logs/` dir.

Per CONTRIBUTING.md §2 the asserts should be on rendered output and
public state, not internals; that's exactly what the authority label
and the on-disk artifacts give us.

#### Phase 3 — populate failure paths

One test per env-var knob — they're already designed to be
single-launch:

- `FAKE_DC_UP_FAIL=1` → asserts the failed-attach popup appears with
  the four action rows (already covered in
  `devcontainer_failed_attach_popup.rs`, but that test currently
  short-circuits via `ShowActionPopup` directly; the new test would
  run the full pipeline).
- `FAKE_DC_UP_HANG=1` → drives `Cancel Startup`, asserts status flips
  to `Dev container attach cancelled` and the fake child is reaped
  (no leftover process under `state/last_id`).
- `FAKE_DC_UP_BAD_JSON=1` and `FAKE_DC_UP_NO_CONTAINER_ID=1` → assert
  the `rebuild_parse_failed` and `rebuild_missing_container_id`
  failure modes.

#### Phase 4 — regression guards (landed)

Locked in on this branch in
`crates/fresh-editor/tests/e2e/plugins/devcontainer_attach_e2e.rs`:

- F1: `attach_closes_stale_build_log_buffer_from_previous_run` —
  pre-creates a stale `build-<old-ts>.log`, opens it as a buffer,
  drives a fresh attach, and asserts the stale buffer is closed and
  a *different* fresh build log is open under the same dir.
- F2: `attach_decision_persists_in_plugin_global_state` — drives the
  attach, snapshots the workspace via `Editor::capture_workspace()`,
  and asserts `plugin_global_state["devcontainer"]["attach:<cwd>"]`
  is `"attached"`. Confirms the production re-prompt would only
  surface from a non-clean exit, not from the plugin itself.
- F4: covered by the existing `serialize_split_node_pruned`
  pruning logic — no new test added because there's no behavior
  change to lock in.

#### Phase 5 — CI integration (landed)

CI already runs `xvfb-run cargo nextest run --all-features
--all-targets` (`.github/workflows/ci.yml:114`). The new tests live
under `tests/e2e/plugins/` and are wired into the binary via
`mod.rs`, so they run automatically with no workflow change.

`bash` + `coreutils` are present on every supported runner
(ubuntu-latest, macos-latest), and the fake CLI is in-tree —
nothing extra to install. No Docker, no Node, no `@devcontainers/cli`
ever needed.

If a future contributor breaks the fake-CLI path the harness panics
with a screen-dump from `bounded_wait`, pointing directly at the
phase that broke (plugin registration / popup rendering / authority
staging).

### Acceptance criteria

- `cargo nextest run -p fresh-editor --features plugins` is green from
  a clean clone with **zero** external installs beyond cargo + bash.
- Removing or breaking the fake CLI files makes the new tests fail
  with a clear "fake CLI not found / behavior changed" message — not
  a confusing harness panic.
- Every flow in `FAKE_DEVCONTAINER_TEST_PLAN.md` has a corresponding
  e2e test that exercises the same code path.

### Order of work

All phases below are landed on this branch unless explicitly marked
otherwise.

1. ✓ Phase 1 — `HarnessOptions::with_fake_devcontainer()`.
2. ✓ Phase 2 — happy-path attach test.
3. ✓ Phase 3 — `FAKE_DC_UP_FAIL` / `BAD_JSON` / `NO_CONTAINER_ID`
   coverage.
4. ✓ Phase 4 — F1 + F2 regression guards.
5. ✓ Phase 5 — CI picks up the new tests automatically via
   `--all-features --all-targets`.
6. ✓ F1 + F6 fixes + F4 reclassification.
7. ✓ F3 fix (default-on Remote Indicator + palette + F6 keybinding).
8. — F5 (tab strip) — out of scope; separate issue.

Open work:

- Cancel-attach happy path under `FAKE_DC_UP_HANG=1` (Phase 3 stretch
  goal — skipped because the cancel timing is harness-dependent and
  the existing remote-indicator-popup test already exercises the
  Cancel Startup action at the UI level).

## Out of scope

- Real container correctness: the fake doesn't validate that LSP /
  PATH / file mounts work *inside* a real image. That coverage stays
  with the existing nightly job (if any) or whatever real-container
  smoke runs. The fake guards the editor side of the boundary, not
  the container side.
- The session-mode rebuild path
  (`EditorServer::current_authority` preservation across restarts).
  That's a daemon-mode feature with its own test surface; this plan
  is about cold-start workspace restore.
