# Usability Test Report — Devcontainer in Fresh TUI

**Date:** 2026-04-26
**Method:** Moderated think-aloud, single participant (TUI-savvy developer profile), executed via tmux against `target/debug/fresh`.
**Project under test:** `vscode-remote-try-python` (Flask app + `mcr.microsoft.com/devcontainers/python:1-3.12`)
**Container runtime:** Docker, healthy.

## Task-by-Task Findings & SEQ

### Task 1 — Environment Bootstrapping  ★ SEQ 7/7
- **Discoverability: excellent.** A `Dev Container Detected` modal appeared on launch with `Reopen in Container` / `Ignore`. Zero hunting required.
- **Affordance: strong.** After accepting, the file-explorer header flipped from `File Explorer` to `[Container]`, and the status bar replaced `Local` with `Container:1110a8fa510a`. Two persistent signals.
- **Build feedback: weak when reusing.** The container was actually reused (`Up 21 hours`), but the auto-opened build log only contained the CLI version line. As a first-time user I'd be unsure whether it just attached, just built, or skipped.

### Task 2 — Add `curl`, Rebuild  ★ SEQ 5/7
- **Rebuild discovery: medium.** The palette contains `Dev Container: Rebuild`, but in alphabetical order it sorts after every `Show *` command. Searching by "rebuild" finds it instantly; searching by "container" makes you scroll.
- **Buffer staleness:** the on-disk `devcontainer.json` was edited but the open buffer didn't refresh until a rebuild ran. A user editing the file from a sibling buffer wouldn't see their own change reflected.
- **Build progress feedback: good.** Status bar shows `⠿ Building` spinner; a fresh `build-<timestamp>.log` is auto-opened in a split.
- **Build log readability: poor for `apt`.** The log uses `\r` (CR-only) line endings, so apt's progress bars compress into one ~2 KB-long line. The buffer looks empty even while the file grows.
- **Failure UX: mixed.** The post-create script failed (`exit code 100`, yarn `NO_PUBKEY`). Modal: `Dev Container Attach Failed` with **good** affordances (`Retry`, `Show Build Logs`, `Reopen Locally`, `Dismiss`) — but the modal body is a JS stack trace from `devContainersSpecCLI.js`, not the underlying apt failure. Status bar truncates to `Attach failed: at async uG (...`. Have to dig into the log file to find the real cause.
- **Side effect:** rebuild kills the existing terminal tab (`*Terminal 0*` disappeared). Friction for users with long-running shells.

### Task 3 — Port Mapping & Verification  ★ SEQ 2/7
- **Auto-forwarding: doesn't appear to work.** Flask bound to `0.0.0.0:9000` inside the container, but `docker ps` showed empty `PORTS`. No host mapping, no notification fired despite `portsAttributes.9000.onAutoForward: notify`.
- **Discoverability of port commands: broken.** Searching the palette for `port`, `Ports`, `Forward`, `Forwarded` returned either zero matches or unrelated commands (Suspend Process, Mouse Support, etc.). The commands `Dev Container: Show Ports` and `Show Forwarded Ports` exist (visible when filtering by `container`) but cannot be located by their natural keywords.
- **Workaround: docker bridge IP works.** `curl http://172.17.0.2:9000/` from the host returned the page. Only an option for docker-savvy users.

### Task 4 — Error Recovery  ★ SEQ 1/7
- **Failure visibility: nearly invisible.** Injected a missing-comma + invalid key/value pair into `devcontainer.json`. No toast, no file-explorer badge, no obvious indicator on the buffer. Existing container kept running so status bar still showed `Container:e794813713ef`.
- **Severe recovery bug:** **all `Dev Container:` palette commands disappeared.** Verified by scrolling alphabetically through the D section — only `Debug…`, `Decrease Split Size`, `Dedent…`, `Delete…`, `Dump Config`, `Duplicate Line`. Rebuild, Detach, Open Config, Show Build Logs — all gone.
- **Fix doesn't restore commands.** Corrected the JSON; the `Dev Container:` commands did not return to the palette. The participant has no in-editor path back; an editor restart is the only recovery.
- **Could not trace error → line:** the JSONC LSP popup earlier offered an in-container install, but no inline error markers ever appeared on the broken lines.

## Cross-cutting Issues

1. **Palette filter is unreliable.** Same query produces different results across invocations; whole-word vs substring vs fuzzy ranking is inconsistent. This dominated frustration in Tasks 3 and 4.
2. **Buffer ↔ disk sync is one-way at best.** External edits don't reflow until a side-effect (rebuild) reopens the buffer.
3. **Terminal mode capture confusion.** `Ctrl+P` sometimes goes to terminal, sometimes opens palette, depending on focus. The status hint `Ctrl+Space to exit terminal mode` exists but is easy to miss.
4. **Failure modes show implementation guts.** Stack traces and CLI internals leak into modal bodies and the status bar.

## Logs, Pane Arrangement & Navigation

### Log display

- **One file per rebuild.** Each rebuild creates a new `.fresh-cache/devcontainer-logs/build-<UTC-timestamp>.log` (e.g. `build-2026-04-26_19-12-05.log`, then `_19-16-40`, then `_19-18-36`). 6 files had stacked up from prior sessions during this run. No rotation/cleanup observed.
- **Auto-open is the right default.** Triggering Rebuild opened the new log file in a fresh buffer in a horizontal split below the editor — without being asked. Status bar showed `⠿ Building` while it ran.
- **No live tail.** The log file on disk grew steadily (kilobytes per second during pip install) but the buffer text stayed at line 1 for tens of seconds. The buffer flushes content periodically, not as lines arrive. `tail` from the shell showed real progress while the in-editor buffer didn't.
- **`\r` ruins apt output.** Status bar reported `CR` line endings and the cursor sat at `Ln 1, Col 2088`. apt's progress bars write `\r` between updates, so the entire 2 KB+ apt section is one logical line that renders nearly blank. A user watching the build screen would think nothing was happening.
- **Logs as regular buffers — actually nice.** Once stable, the build log behaves like any text file: searchable with `/`, scrollable, copyable. For a TUI/vim user this is the right model. `grep`'ing the file from outside found `exit code 100` and the GPG error in seconds.
- **Stale log buffers don't auto-close.** After the second rebuild, the previous build log buffer (`_19-12-05.log`) was still in a tab; rebuild #2's log opened as a separate tab. Tabs accumulate.

### Pane arrangement

- **Initial layout is clean.** File explorer fixed-width on the left; one editor pane on the right with tabs across the top.
- **Rebuilds add horizontal splits without removing old ones.** Each rebuild appended a new split below — the right side grew to 3 stacked splits showing: top = `app.py`, middle = `devcontainer.json`, bottom = build-log + terminal. By the end of Task 2 the bottom panes were ~5 lines tall each.
- **Duplicate buffers across splits.** At one point `devcontainer.json` was open in two adjacent splits simultaneously (lines 33–36 visible in both). Looked like the rebuild auto-opened the config in a new split rather than focusing the existing one.
- **No automatic compacting.** The middle/bottom splits got increasingly cramped. Comment text in `devcontainer.json` wrapped aggressively (`# License…\ninformation.` across two visual lines) just because the split was narrow.
- **Sidebar doesn't yield space.** The file explorer column kept its width while the right side was divided 3 ways. Manual `Decrease Split Size` exists in the palette but I didn't trigger it.
- **Some commands seemed to no-op visually.** `Dev Container: Show Forwarded Ports` and `Show Ports` produced no visible pane — possibly the panel did open but couldn't be rendered in the crowded layout, or it opened off-screen. No status confirmation either way.

### Navigating between panes

- **Tab bars are clear.** Each split has its own row of `name × name × name ×` tabs at the top with `□ ×` controls, making it obvious which buffers belong where.
- **Status bar identifies the focused buffer.** Always shows the current buffer + line/col + container/local prefix. This was the main way I tracked focus during the session.
- **Terminal focus traps `Ctrl+P`.** When focus was inside the terminal pane, `Ctrl+P` keystrokes were eaten by the shell instead of opening the palette. Required `Ctrl+Space` first ("Exit Terminal Mode") to free the editor's bindings. The status hint `Terminal 0 opened (Ctrl+Space to ...` shows once when the terminal opens, then disappears — easy to miss.
- **The `#buffer` palette mode is advertised but unreliable in this session.** Bottom of the palette shows `file | >command | :line | #buffer` mode hints. Trying `#devcontainer` to switch buffers landed in terminal-mode (because focus was on the terminal pane) instead of routing through the palette. Discoverability of "you must be focused outside the terminal first" is poor.
- **Bindings exist but no in-pane indicator.** `Alt+]` Next Split / `Alt+W` Close Tab / `Ctrl+E` Focus File Explorer all show as bound shortcuts in the palette. Reasonable, but I never naturally discovered the cycle order between the 3 horizontal splits — there's no visible "split N of 3" indicator on each pane.
- **Modal dialogs land in their own corner.** The "Dev Container Detected" prompt appeared bottom-right; the "Attach Failed" modal appeared in roughly the same area. Both stacked behind/beside other notifications (the LSP install prompt overlapped the dev-container prompt at one point), making it slightly ambiguous which was foreground.

## Pane Splitting: The Highest-Impact UX Decision

The single design choice that turns the rest of the devcontainer UX
from "good" to "actively unusable by the third rebuild" is the
**always-split, never-close** pane strategy. Calling it out as its
own section because most of the Medium-severity items in the bug
table are downstream of it.

### What the feature does today

Every meaningful devcontainer event opens a *new horizontal split
below the existing splits* and shows the relevant buffer there:

- Accepting "Reopen in Container" → opens the build log in a new split
- Triggering Rebuild → opens the *new* build log in a new split (the
  previous one stays open in its own split)
- Opening a terminal → adds it as a tab in the bottom-most split
- `Show Forwarded Ports` / `Show Container Logs` → also wants its own
  pane

After 3–4 lifecycle events the right column is 5+ splits stacked
vertically, each ~5–7 rows tall. Buffer text wraps aggressively,
status-bar messages get truncated, and **the command palette popup
itself stops rendering** because there's nowhere to put it (this is
the popup-invisibility bug above).

### Why "always split" is the wrong default

1. **Splits are forever.** Nothing in the lifecycle ever closes one.
   The user pays the layout cost permanently for an event that
   mattered for 30 seconds (a build log they read once).
2. **Build logs from dead builds shouldn't compete with live ones.**
   After 3 rebuilds you have 3 build-log buffers fighting for screen
   space; only the latest is interesting.
3. **The bottom split is uninhabitable.** A 5-row pane can't show
   source code, can't show a real terminal session, can't show a
   build log in a useful way.
4. **It cascades into other bugs that would otherwise be minor.**
   The "popup invisible", "Show Ports produces no visible pane",
   "duplicate `devcontainer.json` across two splits", "splits don't
   compact", and "stale build-log tabs accumulate" rows in the bug
   table all collapse into "the layout strategy is wrong" once you
   look at them together.

### What good UX would look like

- **One persistent "devcontainer" panel** (status-panel pattern),
  reused across events. Keystroke to expand, collapse, or cycle
  which buffer it's showing (latest log / older log / forwarded
  ports / etc.).
- **Or: a one-row-tall bottom strip by default**, expandable on
  demand. Same pattern as VS Code's Problems / Output / Terminal
  panel — one slot, multiple tabs, user controls when it grows.
- **Auto-close the previous build log when a new one opens.** The
  plugin already has the primitive — `closeStaleBuildLogBuffers` is
  the source of the existing
  `attach_closes_stale_build_log_buffer_from_previous_run`
  regression test — but it only fires on cold-start, not on
  in-session rebuilds. Extending that to in-session would close one
  of the loops driving the layout pressure.
- **Don't open a new split for transient "info" panels.** Show Ports,
  Show Features, Show Info should be ephemeral popups or panel tabs,
  not splits.

### Net assessment

Of all the things the devcontainer feature does well (auto-detect
prompt, status-bar attach indicator, build logs as inspectable
buffers, JSONC LSP install offer), the pane proliferation is the
single decision that turns those wins into liabilities. **Worth
fixing before any of the smaller items in the bug table** — most of
those will become non-issues once the panel strategy changes.

## Magic Wand Asks (synthesized)

- **Replace always-split with a persistent panel slot** for build
  logs / ports / container logs (see "Pane Splitting" above) — the
  single-highest-impact change in this report.
- A toast / sticky banner when `devcontainer.json` parsing fails — never silently disable commands.
- Live tail of build logs that handles `\r` properly (or render in a dedicated panel with progress bars).
- Auto-publish ports declared in `forwardPorts` / detected at runtime, plus a port-forward toast on bind.
- Make the palette filter behave as a predictable substring/fuzzy search, with consistent ranking under load.

## Test Artifacts / End State

- All 4 protocol tasks executed; container left in a working state on `e794813713ef`.
- `devcontainer.json` restored to valid JSON; the `postCreateCommand` was hardened to be GPG-failure-tolerant (skips broken yarn apt source if `curl` is missing) — kept for future test runs.
- Build logs collected under `vscode-remote-try-python/.fresh-cache/devcontainer-logs/build-2026-04-26_19-*.log`.

## Bug / Gap Summary by Severity

Status legend (added after the 2026-04-26 retest below):
- **Confirmed** — reproduced in both interactive tmux and the harness, or in tmux alone with a clear repro recipe
- **Disconfirmed** — could not reproduce in retest; original observation appears to have been a transient artifact (focus capture, stale tmux capture, palette in flight)
- **Conditional** — real, but only triggers under specific environmental conditions (many commands loaded, crowded layout, restart cycle)
- **Untested** — not re-driven in the retest

| Severity | Bug / Gap | Reference | Status (2026-04-26) |
| --- | --- | --- | --- |
| **Critical** | Pane-splitting strategy is "always-split, never-close": every lifecycle event (attach, rebuild, Show Ports, terminal open) adds a new horizontal split, nothing ever closes one. By the third rebuild the right column is 5+ splits ~5 rows tall, and the layout becomes the root cause of several Medium rows below (popup invisible, Show Ports no visible pane, splits don't compact, duplicate `devcontainer.json` across splits, stale build-log tabs). | "Pane Splitting" section | Confirmed — root cause; fixing this collapses ~5 downstream rows |
| **Critical** | After a `devcontainer.json` syntax error and a Rebuild, all `Dev Container:` palette commands disappear and **do not return after the JSON is fixed** — only an editor restart recovers. | Task 4 | Confirmed (post-rebuild restart cycle re-runs plugin load against broken JSON; harness can't reproduce because it shortcuts the restart) |
| **High** | Auto port-forwarding doesn't publish ports declared in `portsAttributes` / `forwardPorts`; no host mapping, no `onAutoForward: notify` toast. | Task 3 | Confirmed |
| **High** | `devcontainer.json` syntax errors fail silently — no toast, no file-explorer badge, no inline marker. | Task 4 | Confirmed |
| **High** | Palette filter ranking is unpredictable: same query yields different results across invocations, and natural keywords don't surface obvious commands. | Task 3, Task 4, Cross-cutting #1 | Conditional — fuzzy ranking degenerates only with many commands loaded; harness has too few to reproduce |
| ~~**High**~~ | ~~Port-related commands (`Show Ports`, `Show Forwarded Ports`) cannot be located by typing `port`/`forward`/`forwarded`.~~ | Task 3 | Disconfirmed — retest in fresh tmux session found these instantly via `port` and via `Show Forwarded`; original symptom was the cramped-layout / popup-invisible bug below |
| Medium | Palette popup renders nothing when the layout has many horizontal splits — the prompt shows the filter but no result list appears, and `Enter` reports `No selection`. | Panes (added) | Confirmed |
| Medium | Build-log buffer doesn't tail live; lags the on-disk file by tens of seconds. | Logs | Untested |
| Medium | `\r`-only output (apt progress) collapses to one ~2 KB line and renders nearly blank in the buffer. | Task 2, Logs | Disconfirmed — real logs are mixed `\r` + `\n` (each progress line ends in `\n`), and the editor renders them fine. The original status-bar reading of `Col 2088` was likely a misread |
| ~~Medium~~ | ~~`Attach Failed` modal body shows a JS stack trace from `devContainersSpecCLI.js` instead of the root cause.~~ | Task 2 | Disconfirmed — harness `failed_attach_popup_includes_actual_failure_reason` shows the modal does include `FAKE_DC_UP_FAIL_REASON` text. Stack trace seen interactively was likely from an earlier failed-state buffer in the same session |
| ~~Medium~~ | ~~Externally edited buffers (`devcontainer.json` modified on disk) don't reload until a side effect (rebuild) reopens them.~~ | Cross-cutting #2 | Disconfirmed — harness `externally_modified_buffer_reloads_on_disk_change` shows the buffer reloads from disk |
| ~~Medium~~ | ~~`Dev Container: Show Ports` / `Show Forwarded Ports` produce no visible pane, and no status confirmation.~~ | Panes | Disconfirmed — same root cause as palette popup invisibility (above); the panel does open, the cramped layout hid it |
| Medium | Rebuild silently terminates open terminal tabs (`*Terminal 0*` disappears). | Task 2 | Untested |
| Low | Build-log files accumulate in `.fresh-cache/devcontainer-logs/` with no rotation/cleanup. | Logs | Confirmed |
| Low | Stale build-log tabs stay open after subsequent rebuilds. | Logs | Confirmed |
| Low | Same buffer (`devcontainer.json`) gets duplicated across two splits after rebuild. | Panes | Confirmed |
| Low | New splits don't compact existing ones or shrink the sidebar; bottom panes shrink to ~5 lines. | Panes | Confirmed |
| Low | No per-pane "split N of M" indicator; cycle order isn't discoverable. | Navigation | Confirmed (UX gap, not a bug) |
| Low | Terminal pane traps `Ctrl+P`; the `Ctrl+Space` exit hint shows once on terminal open then disappears. | Cross-cutting #3, Navigation | Confirmed |
| Low | LSP install prompt and `Dev Container Detected` prompt overlap with ambiguous z-order. | Navigation | Confirmed |
| Low | Reuse-existing-container path produces a build log containing only the CLI version line — user can't tell whether it built, attached, or skipped. | Task 1 | Confirmed |
| Low | `Dev Container: Rebuild` sorts alphabetically after every `Show *` command in the palette. | Task 2 | Confirmed (consequence of alphabetical sort + naming) |
| Low | Palette doesn't gate commands by state: `Attach` and `Cancel Startup` remain offered while already attached, alongside `Detach`. | Task 2 | **Confirmed** — covered by failing test `palette_attach_command_hidden_when_already_attached` |

## 2026-04-26 Retest Notes

A follow-up retest revisited each finding via the in-tree fake
`devcontainer` CLI harness (E2E tests under
`crates/fresh-editor/tests/e2e/plugins/devcontainer_usability_repros.rs`)
and a fresh interactive tmux run. Three substantive corrections to
the original report:

1. **Palette filter problems are conditional, not universal.** In a
   fresh tmux session the filter found `Dev Container: Show Forwarded
   Ports` instantly via `port` and `Show Forwarded`. The unreliable
   ranking *did* reappear later in the same session once many splits
   accumulated and a broken-JSON rebuild had been triggered, so the
   filter degeneracy is a real bug — but it manifests only under load
   (many commands registered) and/or in pathological session state,
   not on every keystroke. The harness can't reproduce it because it
   loads ~10× fewer commands than a real install.

2. **The "popup invisible" symptom is its own bug, distinct from the
   filter.** With 5+ horizontal splits stacked in the right column,
   the palette accepts the filter text (`>Dev` shows in the prompt)
   but no result list is drawn anywhere on screen, and `Enter`
   reports `No selection`. This explains the original "Show
   Forwarded Ports / Show Ports produce no visible pane" symptom in
   Task 3 and several of the "filter returns nothing" symptoms in
   Tasks 3 and 4.

3. **Several medium-severity items were observation artifacts, not
   real bugs.** The `\r`-rendering, external-buffer-reload, and
   failed-attach-modal-text claims all fail to reproduce when driven
   programmatically through the fake-CLI harness, and could not be
   re-triggered interactively. Most likely cause for the original
   reports: stale tmux captures, popup focus capture by the terminal
   pane, or a prior failed-attach buffer still being visible from
   an earlier rebuild attempt. Those rows have been struck through
   above.

The repro tests live in `devcontainer_usability_repros.rs`. One is
a `#[test]` that fails on master (`palette_attach_command_hidden_when_already_attached`).
The other two — for the post-rebuild command-disappearance bug and
the popup-invisible-with-many-splits bug — are `#[ignore]`'d with
notes explaining the harness limitations that prevent CI repro
(no editor-restart hook; PTY too tall to crowd the popup off-screen).
