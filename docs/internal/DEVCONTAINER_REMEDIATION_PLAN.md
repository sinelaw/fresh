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

| ID | Severity | Description | Where it bites |
|----|----------|-------------|----------------|
| F1 | High | Stale build-log buffers restored on cold start with no "this is from a previous run" hint | Confuses the user the first time they re-open a workspace; new attach also stacks a *second* build-log split |
| F2 | High | Re-prompt for "Reopen in Container?" on every cold restart, even after the user said "Reopen in Container" last time | Annoying churn; defeats the per-workspace "remember decision" intent of `attachDecisionKey` |
| F3 | Medium | The clickable Remote Indicator was opt-in: not in `default_status_bar_left`, no palette command, no default keybinding | Users couldn't reach the menu without editing config (***fixed in this branch — see commit "feat(remote-indicator): default-on, palette + F6 keybinding"***) |
| F4 | Medium | `*Dev Container Logs*` virtual buffer disappears on restart, leaving an empty slot in the saved layout | Visual hiccup; layout looks "wrong" until the user manually closes splits |
| F5 | Low | Multiple buffers in one split aren't visually marked when the tab strip is too narrow | Pre-existing UX wart; surfaces sharply during devcontainer flows because attach opens many transient buffers |
| F6 | Low | Show Build Logs vs Show Logs is easy to mix up via fuzzy palette match | Cosmetic; just a label clarification |

The rest of this doc walks F1, F2, F4, F5, F6 plus the CI investment.
F3 is already handled in this branch.

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

Touchpoints: `crates/fresh-editor/plugins/devcontainer.ts` (logic),
plus whatever workspace-save filter governs which buffers persist —
likely in `crates/fresh-editor/src/server/editor_server.rs` or the
workspace serializer.

### Test (CI-able)

Drive Flow A → quit cleanly → relaunch → assert workspace doesn't
contain a build-log buffer in the restored layout. With the fake CLI,
`prepareBuildLogFile` runs as normal, so the test exercises the real
code path. Live in `crates/fresh-editor/tests/e2e/plugins/`.

## F2 — Reopen-in-Container re-prompts on every cold start

### What happens

`devcontainer.ts` keys the prompt decision via `attachDecisionKey()`
which is per-cwd plugin global state. It *should* persist.

In the tmux walk, the prompt re-appeared after a cold restart even
though we'd selected "Reopen in Container" in the prior run. The
likely cause: `setAuthority` triggers an editor restart immediately
after `writeAttachDecision("attached")` — the post-restart instance
reads back the decision but it never fired the Save path, OR the
write race-loses to a workspace save that captured the pre-write
state.

### Proposed investigation

1. Add a single-line debug on plugin load: log the value
   `readAttachDecision()` returns, compared with what
   `editor.getCwd()` resolves to. Run the tmux walk with the fake CLI;
   capture the log to see whether the key landed at all.
2. If the key is missing post-restart, audit
   `setGlobalState`/`getGlobalState` plumbing for ordering with
   `setAuthority`. The fix is likely a flush before authority change
   (or moving the write earlier in `runDevcontainerUp`).
3. If the key lands but the popup still shows, the bug is in
   `devcontainer_maybe_show_attach_prompt` — re-read its `previousDecision !== null`
   guard against the actual return value.

### Test

Same recipe as F1's test: attach via fake CLI → quit → relaunch →
assert no popup with id `devcontainer-attach`.

## F4 — Virtual log buffer leaves a visible empty slot

`*Dev Container Logs*` is created via `createVirtualBufferInSplit` and
correctly isn't persisted, but the *split* placeholder it occupied
gets restored as an empty area in the saved layout.

### Proposed fix

Two options:

1. **At save time:** drop empty splits from the saved layout (cheap, in
   the workspace serializer).
2. **At restore time:** collapse splits whose buffer is missing, with a
   single-frame relayout pass.

Option 1 is simpler and matches user expectation — if a split's
content didn't survive, the split shouldn't either.

### Test

Open Dev Container Logs in a split → quit → restart → assert the
restored split count matches what was actually persisted (i.e. the
old "Dev Container Logs" slot is gone, not present-but-empty).

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
