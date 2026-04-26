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
| F2 | High | Re-prompt for "Reopen in Container?" on every cold restart, even after the user said "Reopen in Container" last time | **Open** — investigation parked. The plugin writes the decision via `writeAttachDecision("attached")` which lands in `plugin_global_state` (workspace.rs:405). Likely root cause is non-clean process exit (e.g. SIGHUP from a parent shell) skipping the workspace save; verifying needs an integration test that drives a clean Quit. See "F2 next steps" below. |
| F3 | Medium | The clickable Remote Indicator was opt-in: not in `default_status_bar_left`, no palette command, no default keybinding | **Fixed on this branch** — `feat(remote-indicator): default-on, palette + F6 keybinding` |
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

### Test (CI-able)

Drive Flow A → quit cleanly → relaunch → assert workspace doesn't
contain a build-log buffer in the restored layout. With the fake CLI,
`prepareBuildLogFile` runs as normal, so the test exercises the real
code path. Lives next to the new tests in
`crates/fresh-editor/tests/e2e/plugins/devcontainer_attach_e2e.rs`
(test name TBD when F1 regression test is added — see Phase 4 below).

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

### F2 next steps

Pick up after Phase 4's harness work has the F1 regression test in
place — that test's "quit cleanly → relaunch" scaffold is exactly
what F2 needs:

1. Extend the F1 test to also assert `readAttachDecision()` returns
   "attached" on the post-restart plugin instance. If it does, the
   re-prompt was a tmux-kill artifact and we close F2 as
   non-reproducible.
2. If it doesn't, the bug is real. Audit setGlobalState plumbing
   around `setAuthority` — likely fix is to flush before the
   authority change, or move `writeAttachDecision` earlier so it
   lands in the workspace save that fires *before* restart.
3. If the key lands but the popup still shows, the bug is in
   `devcontainer_maybe_show_attach_prompt`'s
   `previousDecision !== null` guard.

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

#### Phase 4 — tests for F1 / F2 / F4 (regression guards for the fixes)

Once the fixes from those sections land, lock them in:

- F1: attach + quit + relaunch → assert workspace JSON contains no
  build-log buffer in its layout.
- F2: attach + quit + relaunch → assert no popup id
  `devcontainer-attach` is shown on the second start.
- F4: open Dev Container Logs → quit + relaunch → assert restored
  split count.

#### Phase 5 — CI integration

Two small changes to the CI workflow:

1. Make sure the runner has `bash` + `coreutils` (already a given on
   ubuntu-latest / macos-latest).
2. Run the new tests as part of the existing nextest invocation. They
   don't need extra setup because the harness helper takes care of
   PATH + state, and the fake CLI is in-tree.

No Docker, no Node, no `@devcontainers/cli` ever installed in CI.

### Acceptance criteria

- `cargo nextest run -p fresh-editor --features plugins` is green from
  a clean clone with **zero** external installs beyond cargo + bash.
- Removing or breaking the fake CLI files makes the new tests fail
  with a clear "fake CLI not found / behavior changed" message — not
  a confusing harness panic.
- Every flow in `FAKE_DEVCONTAINER_TEST_PLAN.md` has a corresponding
  e2e test that exercises the same code path.

### Order of work

1. Phase 1 (harness helper) — small, unblocks everything.
2. Phase 2 (happy-path attach test) — proves the helper works.
3. F3 — already done on this branch.
4. F1 + F4 fixes + Phase 4 regression tests — the most user-visible
   warts; they share the "what does workspace restore actually save"
   investigation.
5. F2 fix — needs the timing investigation above; landing it after
   F1/F4 keeps each PR small.
6. Phase 3 (failure-path tests) — pure additions, can land in
   parallel.
7. F6 (label clarification) — drive-by.
8. F5 (tab strip) — separate issue, separate PR.

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
