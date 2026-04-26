# Devcontainer Interactive Test Plan (with the Fake CLI)

Companion to [`FAKE_DEVCONTAINER_CLI.md`](./FAKE_DEVCONTAINER_CLI.md). Walks
through the devcontainer-related flows that require a human at the
keyboard, using the fake `devcontainer` / `docker` shims so no Docker
daemon, image, or `@devcontainers/cli` install is needed.

Every step lists the expected screen state. Where a step depends on
previous state (a popup that only appears once per session, a build log
that survives across restarts), that's called out.

The plan was authored by walking each flow in tmux against the just-built
`./target/debug/fresh`; observed gaps are noted under "Findings."

## Setup (do once per session)

```bash
# Build the editor under test.
cargo build --bin fresh

# Shim the CLIs.
source scripts/fake-devcontainer/activate.sh

# Fresh state every run, so prior breadcrumbs don't bleed in.
rm -rf /tmp/dc-tmux-workspace ~/.local/share/fresh/workspaces \
       ~/.cache/fake-devcontainer
mkdir -p /tmp/dc-tmux-workspace/.devcontainer
cat > /tmp/dc-tmux-workspace/.devcontainer/devcontainer.json <<'JSON'
{
  "name": "fake-tmux-test",
  "image": "mcr.microsoft.com/devcontainers/base:ubuntu",
  "forwardPorts": [8080, 5432],
  "portsAttributes": {
    "8080": { "label": "Web", "onAutoForward": "notify" },
    "5432": { "label": "Postgres", "onAutoForward": "silent" }
  },
  "features": {
    "ghcr.io/devcontainers/features/rust:1": "1.91.0",
    "ghcr.io/devcontainers/features/node:1": "lts"
  },
  "remoteUser": "vscode",
  "initializeCommand": "echo init-host >&2",
  "postCreateCommand": "echo postcreate >&2",
  "postStartCommand": "echo poststart >&2"
}
JSON
echo hello > /tmp/dc-tmux-workspace/hello.txt
```

Launch in tmux for an isolated, capture-able session:

```bash
tmux new-session -d -s dc -x 200 -y 60 \
  "bash -lc 'source scripts/fake-devcontainer/activate.sh && \
             cd /tmp/dc-tmux-workspace && \
             exec ./target/debug/fresh /tmp/dc-tmux-workspace'"
tmux attach -t dc
```

Tip: use `tmux capture-pane -t dc -p > /tmp/screen.txt` to snapshot the
screen for diffing.

---

## Flow A — Cold start, attach prompt, happy path

**Pre-state:** No prior session for this workspace; no fake-state dir.

1. Launch fresh as above. Within ~2s the screen shows file explorer on
   the left, dashboard center, and a popup in the bottom-right:

   ```
   ┌Dev Container Detected─────────────────────────────[×]┐
   │Folder contains a Dev Container configuration         │
   │('fake-tmux-test'). Reopen in container?              │
   │                                                      │
   │Reopen in Container                                   │
   │Ignore                                                │
   └──────────────────────────────────────────────────────┘
   ```

   **Trap:** focus starts on file explorer. Pressing Enter there
   collapses the directory; the popup is shadowed. Press **Esc** first
   to release explorer focus, then **Enter** selects "Reopen in
   Container."

2. After Enter the build log appears in the right split, streaming
   `[+] Building 0.0s …` lines. Status bar shows
   `Container:<id> | .fresh-cache/devcontainer-logs/build-<ts>.log`.
   The build-log buffer auto-reverts on a 2s poll so each new fake
   line shows up within a couple of seconds of being emitted.

3. When the fake emits the success JSON, the editor restarts (one
   visible repaint), the status bar gains the `[Container:<shortid>]`
   prefix, and the workspace re-renders.

**Pass criteria:** status bar prefix changes to `[Container:…]`; build
log split is still readable; no error popups.

## Flow B — Run lifecycle command (the bug fixed in da4969a3)

Continues from Flow A.

1. Open the palette: **Ctrl+P** (release file-explorer focus first if
   needed).
2. Type `Dev Container: Run Lifecycle`. The first match is "Dev Container:
   Run Lifecycle Command." Press **Enter**.
3. A second prompt opens listing every defined lifecycle command from
   `devcontainer.json` (with the fixture above: `initializeCommand`,
   `postCreateCommand`, `postStartCommand`, plus `updateContentCommand`
   if you add it). Use **Down/Up** to pick `postCreateCommand`. Press
   **Enter**.
4. Status bar should briefly show `Running postCreateCommand…` then
   `postCreateCommand completed successfully`.

**Pass criteria:** the second status message appears. Without the
da4969a3 fix the handler returns early on `data.value` (undefined) and
the user sees no second message — the run silently no-ops. The fake's
`echo postcreate >&2` lets you double-check by re-running with
`FAKE_DC_UP_DELAY_MS=0` and watching `tail -F .fresh-cache/devcontainer-logs/build-*.log`
*before* triggering the command — the line will appear in the build
log because the fake's stderr is captured there during `up`, but for
out-of-band lifecycle runs you'd add a sentinel like `touch /tmp/marker`.

## Flow C — Show Build Logs / Show Logs (container)

1. Build Logs: palette → `Dev Container: Show Build Logs`. The most
   recent `build-<ts>.log` opens in a horizontal split below.
   Subsequent invocations focus the existing split rather than
   stacking; verify by triggering twice.
2. Container Logs: palette → `Dev Container: Show Logs` (be careful —
   the fuzzy match also surfaces `Show Build Logs`; type a few more
   chars or use Down to disambiguate). A new split opens with the title
   `*Dev Container Logs*` and the contents of fake `docker logs`
   (which is the same stderr text the fake recorded during `up`, since
   we have no separate "container stdout" to record).

**Pass criteria:** both splits show the captured fake-build text,
read-only, line-numbers off for *Dev Container Logs*.

## Flow D — Forwarded Ports panel

1. Palette → `Dev Container: Show Forwarded Ports`. A panel opens
   listing the configured 8080 + 5432 with their labels and runtime
   bindings.
2. Bindings come from `docker port <id>`. With the fake, those are
   populated only if you set `FAKE_DC_PORTS=8080,5432` *before* `up`
   ran. Otherwise the panel shows the configured ports with no runtime
   binding — also a valid state to verify (it's the "container started
   but didn't bind anything" case).

## Flow E — Cancel an in-flight attach

**Pre-state:** clean fake state.

1. Launch with `FAKE_DC_UP_HANG=1` so `up` sleeps forever:

   ```bash
   FAKE_DC_UP_HANG=1 ./target/debug/fresh /tmp/dc-tmux-workspace
   ```

2. Esc → Enter the attach popup. Build log split appears with
   `[+] Hanging on purpose for cancel test`. Status bar shows
   `Connecting · Building`.
3. Palette → `Dev Container: Cancel Startup`. Status bar should flip
   to `Dev container attach cancelled`. The hung fake process is
   killed (verify with `ps -ef | grep fake` — it's gone). No error
   popup.

**Pass criteria:** status flips, no `FailedAttach` popup (the cancel
short-circuits `attachCancelled`).

## Flow F — Failed attach

**Pre-state:** clean fake state.

1. Launch with:
   ```bash
   FAKE_DC_UP_FAIL=1 \
   FAKE_DC_UP_FAIL_REASON='image not found: bogus:latest' \
   FAKE_DC_UP_DELAY_MS=80 \
   ./target/debug/fresh /tmp/dc-tmux-workspace
   ```
2. Esc → Enter the attach popup. Build log streams; after a few
   seconds the fake exits 1.
3. A popup opens:
   ```
   ┌Dev Container Attach Failed────────────────────────[×]┐
   │Dev container attach failed: error: image not found: bogus:latest │
   │                                                      │
   │Retry                                                 │
   │Show Build Logs                                       │
   │Reopen Locally                                        │
   │Dismiss (ESC)                                         │
   └──────────────────────────────────────────────────────┘
   ```
4. Status bar: `Rebuild failed: error: image not found: bogus:latest`.

Variations to spot-check (one launch each, fresh state in between):

- `FAKE_DC_UP_BAD_JSON=1` → `enterFailedAttach("Failed to parse devcontainer up output")` (or i18n equivalent).
- `FAKE_DC_UP_NO_CONTAINER_ID=1` → "missing containerId" failure.

## Flow G — Rebuild after success

Run Flow A first. Then:

1. Palette → `Dev Container: Rebuild`. The plugin re-invokes
   `devcontainer up --workspace-folder <cwd> --remove-existing-container`.
2. Build log split opens with a *new* `build-<ts>.log` (verify the
   filename in the title bar changes). The fake stops the previous
   container (per `--remove-existing-container`) and allocates a new
   id; the editor restarts again with the new authority.

## Flow H — Multi-tab session restore

This is the case the user explicitly asked about. Walks through a real
multi-buffer state being restored on cold start, with attention to the
build-log surfaces.

1. Run Flow A end-to-end so a container is attached.
2. Open a few buffers without closing the build-log split:
   - palette → type `hello.txt` → Enter (opens in the split that has
     focus — likely the bottom split next to the build log)
   - palette → type `devcontainer.json` → Enter
   - palette → `Dev Container: Show Logs` → Enter (adds *Dev Container
     Logs*)
3. Note: each newly opened buffer becomes a tab in the *focused* split
   — they don't all aggregate in one place. The screen ends up with two
   visible splits, one of which has the active tab, plus several tabs
   that are off-screen if the strip is too narrow to render them all.
4. Quit cleanly: **File → Quit** from the menu (or palette → `Quit`).
   `tmux kill-session` is *not* a clean exit and skips the workspace
   write — verify by re-running and seeing the previous tabs gone.
5. Relaunch:
   ```bash
   ./target/debug/fresh /tmp/dc-tmux-workspace
   ```
6. Observe:
   - Splits and tabs are restored.
   - `[Container:<id>]` is **gone** from the status bar — the container
     authority is ephemeral by design, so a cold restart drops back to
     local.
   - The `Reopen in Container?` popup re-appears (per-process attach
     decision was wiped with the process).
   - Both build-log splits still hold the *previous run's* log — that
     file is on disk and openFile reused it. The user has no in-buffer
     hint that this is stale until they trigger a new attach (which
     opens a *new* `build-<ts>.log` file in a new split).

### Findings — log display on session restore

- **Stale build log on cold restart.** When workspace restore brings
  back a `build-<ts>.log` buffer, there's no banner / tab badge marking
  it as historical. A user who attaches again gets a *second* build-log
  split below the first (because `prepareBuildLogFile` mints a new
  timestamp); the older split sits there full of detached lines from
  the prior process. Suggest: tag the older buffer's tab title (e.g.
  `…log (prev)`) on restore, or close it automatically when a new
  attach starts.

- **Re-prompt on every cold start.** `readAttachDecision` is keyed by
  cwd but stored in plugin global state, which is per-workspace JSON,
  so it *should* survive — but the fake-CLI runs above showed the
  prompt re-appearing. Worth confirming whether this is "we never
  attached because the previous run died unclean" vs "the per-workspace
  decision isn't actually being persisted." (See `attachDecisionKey`
  /`writeAttachDecision` in `devcontainer.ts`.)

- **`*Dev Container Logs*` is virtual and not restorable.** That split
  silently disappears on restart, leaving an empty bottom slot in the
  saved layout. Not a regression — virtual buffers aren't persisted —
  but worth knowing when comparing pre/post-restart screenshots.

- **Multiple tabs in the same split aren't visually marked when they
  collide with the buffer-group ribbon.** The active tab title shows
  in the strip; siblings are only reachable by Ctrl+Tab. With three or
  four buffers in the same split, the strip just shows one title and
  it's easy to think the rest were lost.

## Flow I — Detach

After a successful attach:

1. Palette → `Dev Container: Detach`. Status bar loses the
   `[Container:…]` prefix; the file-explorer header drops the
   `[Container]` tag and reverts to `File Explorer`.
2. Re-run a lifecycle command — should now run on the host (visible
   side-effect: any `FAKE_DC_USER` env var is unset in the spawned
   shell). Re-attaching with `Dev Container: Attach` brings everything
   back.

## Flow J — Scaffold a new config

In a directory **without** `.devcontainer/`:

1. Launch fresh in `/tmp/empty-workspace`. No popup, no devcontainer
   commands registered (only `Create Config` is unconditional).
2. Palette → `Dev Container: Create Config`. A new
   `.devcontainer/devcontainer.json` is written with a minimal
   template, opened in a buffer, and the rest of the devcontainer
   commands appear in the palette without needing a restart.

---

## CI integration sketch

The fake CLI is deliberately scriptable. A CI smoke job can do:

```bash
source scripts/fake-devcontainer/activate.sh
FAKE_DC_UP_DELAY_MS=0 \
FAKE_DC_PORTS=8080,5432 \
cargo nextest run --features plugins -p fresh-editor \
  -E 'test(devcontainer)' --no-fail-fast
```

The existing e2e tests don't currently shell out to a real
`devcontainer`, so the fake doesn't regress them — but new tests that
*do* want to drive the attach flow can opt into the fake by setting up
the harness with the bin dir prepended to `PATH`. Adding a helper
`harness.with_fake_devcontainer()` is a small follow-up.

## Things this plan deliberately *doesn't* cover

- LSP behaviour inside the "container" — the fake's `docker exec`
  doesn't change PATH, so any LSP probe sees the host's binaries.
- Terminal split that lives off `terminal_wrapper`. It works (it's just
  `bash -l` via `fake-docker exec`) but it's testing the host shell, not
  a container shell.
- `docker logs --follow` semantics — fake reads to EOF and exits.
- Network, port forwarding, file mounts — none of these are real.

## Re-running the captured fake outputs

If you want exact diffs against this doc:

```bash
# Reset
rm -rf ~/.cache/fake-devcontainer ~/.local/share/fresh/workspaces \
       /tmp/dc-tmux-workspace/.fresh-cache

# Sanity: shim is on PATH
which devcontainer  # → .../scripts/fake-devcontainer/bin/devcontainer
which docker        # → .../scripts/fake-devcontainer/bin/docker

# Quick e2e of `up` from the shell
FAKE_DC_UP_DELAY_MS=0 FAKE_DC_PORTS=8080,5432 \
  devcontainer up --workspace-folder /tmp/dc-tmux-workspace 2>&1 | tail
# stdout's last line should be the success JSON.
```
