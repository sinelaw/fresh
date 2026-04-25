# Dev Container Spec — Gap Analysis

Comparison between the user-provided "Dev Container Lifecycle and UX Flow"
specification (VS Code–style) and Fresh's current implementation as of
branch `claude/add-remote-indicator-WFOVB`.

Companion docs:
- `AUTHORITY_DESIGN.md` — the authority abstraction that underpins
  Fresh's container support.
- `DEVCONTAINER_PLUGIN_DESIGN.md` — the plugin that owns dev-container
  detection and lifecycle.

---

## Executive summary

Fresh already ships a working container workflow: a TypeScript plugin
(`plugins/devcontainer.ts`, ~1100 lines) parses `.devcontainer/devcontainer.json`,
drives `devcontainer up` on the host via `editor.spawnHostProcess`, and
installs a container-scoped authority via `editor.setAuthority({...})`.
The core contributes an `Authority` slot, a `DockerExecSpawner`, a
destructive-restart transition flow, and the status-bar pieces (most
recently a clickable `{remote}` indicator with a context-aware popup).

What's missing, relative to the spec, is primarily **UX surface around
the build lifecycle**:
- no visible Image-Pull/Build/Start/Post-Init state machine,
- no live log streaming for `devcontainer up`,
- no cancel-in-flight,
- no retry-on-failure action,
- no port-forwarding detection,
- no automatic install of `customizations.*.extensions`.

A handful of items are **intentional architectural divergences** rather
than gaps:
- Fresh does not inject a headless editor server into the container
  (spec §5); it uses `docker exec` to run tools inside the container
  while the editor UI stays on the host.
- Paths are not translated; the workspace is bind-mounted so host and
  container paths coincide.
- Core stays free of per-backend knowledge (per `AUTHORITY_DESIGN.md`
  principles 3 & 4); orchestration lives in the plugin and the third-party
  `devcontainer` CLI.

The rest of this document walks every spec bullet and records status,
evidence, and (where applicable) a suggested change.

---

## Legend

- **✅ Implemented** — matches the spec's intent, even if wording differs.
- **⚠️ Partial** — some of the behavior is present; gaps listed inline.
- **❌ Missing** — not implemented.
- **↔ Divergent by design** — not implemented, and the design doc says it
  shouldn't be. Listed for completeness.

---

## §1. Remote Authority & UI Entry Point

| Spec bullet | Status | Notes |
|---|---|---|
| Persistent UI element in bottom-left status bar | ✅ | `StatusBarElement::RemoteIndicator` (added on this branch). Element is opt-in via config; placement at far-left is achieved by putting `{remote}` first in `status_bar.left`. |
| Click opens Quick Pick / Command Palette menu | ✅ | Click on `{remote}` → `Action::ShowRemoteIndicatorMenu` → `show_remote_indicator_popup`. Popup is anchored to the indicator's x-column via `PopupPosition::AboveStatusBarAt`. |
| Menu options (Local): "Reopen in Container", "Configure Dev Container" | ⚠️ | "Reopen in Container" offered when a `.devcontainer/devcontainer.json` is detected; it dispatches `Action::PluginAction("devcontainer_attach")`. "Configure Dev Container" is approximated by "Open Dev Container Config" which calls the plugin's `devcontainer_open_config`. **Gap:** when no config exists, spec implies a scaffold flow; Fresh shows a single disabled "No dev container config detected" row. |
| Menu options (Connecting/Building): "Show Logs", "Cancel Startup" | ❌ | Fresh's core has no Connecting/Building state — `setAuthority` is fire-and-forget from the plugin, and `devcontainer up` output is buffered, not streamed. Both actions would need log-streaming and process-tracking infrastructure that doesn't exist yet. |
| Menu options (In-Container): "Reopen Locally", "Rebuild Container", "Show Container Logs" | ⚠️ | "Reopen Locally" → `clear_authority()`. "Rebuild Container" → `Action::PluginAction("devcontainer_rebuild")`. **Gap:** "Show Container Logs" is not implemented; the popup offers "Show Container Info" (the existing `devcontainer_show_info` panel) instead. |

**Recommended follow-ups for §1**
- Add "Create .devcontainer/devcontainer.json" scaffolding command that
  writes a minimal template and opens it for editing.
- Add "Show Container Logs" action that tails `docker logs <id>` through
  a streaming variant of `spawnHostProcess` (see §4 follow-up).

---

## §2. Trigger & Initial Detection

| Spec bullet | Status | Notes |
|---|---|---|
| Detect `.devcontainer/devcontainer.json` or `.devcontainer.json` on workspace open | ✅ | `plugins/devcontainer.ts::findConfig()` checks `.devcontainer/devcontainer.json`, then `.devcontainer.json`, then `.devcontainer/<subfolder>/devcontainer.json` in priority order. |
| Non-blocking toast prompt when Local | ⚠️ | Implemented as `editor.showActionPopup({...})`, not a toast. The popup is modal-ish (keyboard capture) but non-blocking wrt. editing; functionally similar. **Wording gap:** spec says "Folder contains a Dev Container configuration. Reopen in container?"; plugin says "Attach to dev container `'<name>'`?" with [Attach] / [Not now]. |
| Actions: "[Reopen in Container]" / "[Ignore]" | ⚠️ | Labels differ ("Attach" / "Not now"). Decision is persisted per-workspace in plugin global state so it isn't re-prompted on reopen — a desirable behavior the spec doesn't mention but is worth preserving. |

**Recommended follow-ups for §2**
- Align copy: rename plugin labels to "Reopen in Container" / "Ignore"
  to match spec and VS Code wording. (Pure i18n change.)
- Keep the per-workspace one-shot dismissal behavior.

---

## §3. Transition & Initialization

| Spec bullet | Status | Notes |
|---|---|---|
| Window reload / disposal of local watchers, LSPs | ✅ | Per `AUTHORITY_DESIGN.md` principle 7, every authority transition drops and rebuilds the entire `Editor`. Terminals, LSPs, watchers, buffers, plugin state are all recreated against the new authority. |
| UI transitions to "Connecting" overlay / splash | ❌ | Only a single `editor.setStatus("Rebuilding dev container...")` status-bar message; the editor remains fully interactive during `devcontainer up`. No overlay, no splash, no state change visible in the remote indicator itself. |
| Connection Agent background process to Docker | ⚠️ | Not a long-lived "agent" — the plugin invokes `devcontainer up` via `editor.spawnHostProcess`, awaits the result, parses JSON on stdout, then calls `setAuthority`. No streaming, no sub-states. Once attached, `DockerExecSpawner` is the "agent" that routes subsequent process spawns through `docker exec`. |

**Recommended follow-ups for §3**
- Add a `RemoteIndicatorState::Connecting` variant and a way for the
  plugin to set it (e.g. `editor.setRemoteState("connecting" \| "failed")`).
  Wire the indicator to render a spinner/underline while connecting.
- Optionally render a translucent modal overlay during the restart
  itself (between the plugin's `setAuthority` call and the first frame
  of the new editor instance).

---

## §4. Environment Provisioning (The Build)

| Spec bullet | Status | Notes |
|---|---|---|
| Three-phase state machine: Image Pull/Build → Container Start → Post-Init | ❌ | No state machine exists. The plugin delegates to `devcontainer up`, which emits final JSON after the whole pipeline completes; intermediate phases are not observed. |
| Status bar: "Spinning/Building" state | ❌ | `RemoteIndicator` has Local / Connected / Disconnected. No Building variant. |
| Progress toast: "Building Dev Container..." | ⚠️ | Only a single `setStatus` call (`status.rebuilding`). It is persistent until overwritten, but there is no spinner / progress animation. |
| [Show Logs] button → dedicated "Dev Container Output" terminal streaming stdout/stderr | ❌ | `editor.spawnHostProcess` returns `{stdout, stderr, exit_code}` only after the child process exits — there is no streaming API at the plugin surface. Adding one would require (a) a streaming variant of the plugin command, (b) a virtual-buffer terminal to render into, and (c) log-multiplexing glue. |
| Cancel button → graceful Docker shutdown → return to Local | ❌ | No cancellation. The underlying `tokio::process::Command::output()` call is awaited to completion; there's no handle to kill the child, and the plugin API exposes no kill primitive. |

**Recommended follow-ups for §4**
This is the biggest implementation gap. Minimum viable additions:
1. **Streaming spawn API**: new plugin command
   `editor.spawnHostProcessStreaming(command, args, cwd?, callbacks?)`
   that emits line-delta events; reuse `PluginProcessOutput` async
   message shape with a `delta` flag.
2. **Process cancellation**: expose `editor.killPluginProcess(id)` that
   stores the `tokio::process::Child` handle at spawn time and kills
   on request. Core already owns the async runtime.
3. **Build-log virtual buffer**: the plugin opens a virtual buffer
   (`editor.createVirtualBufferInSplit`, used elsewhere) and appends
   streamed log lines. Tailing this buffer is what "Show Logs"
   reveals.
4. **New RemoteIndicator sub-states**: extend `RemoteIndicatorState`
   with `Connecting { phase: BuildPhase }` and render a Unicode spinner
   glyph. The plugin would transition it explicitly via a new op
   (`editor.setRemoteState(...)` or `setAuthority` with a `phase`
   field on the payload).

---

## §5. Remote Server Injection

| Spec bullet | Status | Notes |
|---|---|---|
| Inject "Server Agent" (headless editor core) via `docker exec` | ↔ | **Intentional divergence.** `AUTHORITY_DESIGN.md` explicitly says "Not a remote extension host." Fresh's UI stays on the host; only spawned processes (LSP servers, terminals, formatters, `spawnProcess` calls) cross into the container via `DockerExecSpawner`. Each process is one-shot `docker exec`, not a long-lived agent. |
| Client/Server socket or SSH tunnel | ↔ | **Not applicable.** Process outputs are returned over ordinary pipes from each `docker exec` invocation. |
| Workspace bind-mount to `/workspaces/<folder-name>` | ✅ | Handled externally by the `devcontainer` CLI (honoring `workspaceFolder`/`workspaceMount` from `devcontainer.json`). Fresh reads `remoteWorkspaceFolder` from the `devcontainer up` JSON output and passes it as `-w` to every `docker exec`. |

**No follow-ups recommended.** The VS Code architecture requires a
remote editor; Fresh's architecture doesn't. Porting that design would
be a principles-level change rather than a feature addition.

---

## §6. Lifecycle Hook Execution

| Spec bullet | Status | Notes |
|---|---|---|
| Execute hooks in order: `initializeCommand` → `onCreateCommand` → `updateContentCommand` → `postCreateCommand` | ⚠️ | The `devcontainer` CLI (third-party, invoked via `devcontainer up`) runs `onCreateCommand`, `updateContentCommand`, `postCreateCommand`, `postStartCommand`, and `postAttachCommand` itself. `initializeCommand` is defined to run *on the host before creation*, and the plugin **never invokes it** — it's shown in the info panel but omitted from the lifecycle runner (`devcontainer.ts:638-644`). This is a spec violation, regardless of whether we implement the full state machine. |
| Auto-focus the output terminal during `postCreateCommand` | ❌ | There is no output terminal to focus (see §4 — no log streaming). |
| Manual "Run Lifecycle Command" picker | ✅ | `Dev Container: Run Lifecycle Command` palette command lets the user run any lifecycle hook on demand. Covers the same set minus `initializeCommand`. |

**Recommended follow-ups for §6**
- **Fix `initializeCommand` omission**: add it to the lifecycle array in
  `devcontainer.ts:638-644` and run it via `editor.spawnHostProcess`
  (so it runs on the host, per spec) *before* calling `devcontainer up`
  during `devcontainer_attach`.
- Include `initializeCommand` in the `Run Lifecycle Command` picker for
  parity with the info panel.

---

## §7. Ready State

| Spec bullet | Status | Notes |
|---|---|---|
| Remote Indicator flips to "Connected" state with container name | ✅ | `Authority::display_label` is set to `Container:<short-id>` by the plugin when building the payload (`devcontainer.ts::buildContainerAuthorityPayload`). The status bar renders it via `connection_display_string()`. After this branch, the `{remote}` element shows it persistently with the Connected color palette. |
| New terminals default to shell inside container | ✅ | `Authority::terminal_wrapper` is set to `{ command: "docker", args: ["exec", "-it", "-u", user, "-w", workspace, id, "bash", "-l"], manages_cwd: true }`. Terminal manager honors this unconditionally. |
| Install/enable plugins from `customizations.vscode.extensions` | ❌ | The plugin reads `customizations` into its config type but does nothing with it. VS Code extensions aren't applicable to Fresh (different plugin model); a Fresh-specific `customizations.fresh.plugins` namespace would need to be defined first. |
| Port forwarding: detect opened ports, offer to forward to localhost | ❌ | `forwardPorts` is displayed in the info panel (Section_ports), but nothing enforces, detects, or routes it. Docker container ports are reachable on the host only if the `devcontainer` CLI configured the container's own port mappings (e.g. via `appPort`). There is no watch-for-new-listeners logic. |

**Recommended follow-ups for §7**
- **Define a Fresh customizations namespace**: spec-out
  `customizations.fresh.plugins: string[]` (plugin file paths or names),
  then have the plugin iterate and call `editor.loadPlugin(path)` after
  attach.
- **Port forwarding**: lower priority since `devcontainer up` already
  honors `forwardPorts` for non-publish ports. A "Show Forwarded Ports"
  action that tabulates `docker port <id>` output would cover the
  observability half of the spec.

---

## §8. Error Handling

| Spec bullet | Status | Notes |
|---|---|---|
| On build failure: stop reload, keep log terminal open, offer [Retry] or [Reopen Locally] | ⚠️ | Partial. If `devcontainer up` exits non-zero, the plugin shows `status.rebuild_failed` in the status bar and does NOT call `setAuthority`, so the authority transition is never queued — equivalent to "stop reload" and "stay local". But: no log terminal exists to be "kept open", and there is no action popup with [Retry] / [Reopen Locally]. |
| Remote Indicator menu surfaces retry / revert after failure | ❌ | Not implemented. The `{remote}` popup's state machine (Local / Connected / Disconnected) has no "FailedBuild" branch — after a failed attach the indicator reads as Local, offering only "Reopen in Container" again, which is effectively a retry but without a dedicated [Retry] label or preservation of logs. |

**Recommended follow-ups for §8**
- When the build-log virtual buffer exists (§4), have the plugin keep
  it open on failure instead of closing it.
- Add a `RemoteIndicatorState::FailedAttach { last_error: String }`
  variant so the indicator renders with error styling and the popup
  offers "Retry" / "Reopen Locally" / "Show Build Logs".
- Persist the last-failure log path on the plugin side so "Show Build
  Logs" remains useful across restarts.

---

## Summary of work items (prioritized)

### Small (align with spec without architectural changes)

1. Fix the `initializeCommand` omission in `devcontainer.ts` (§6).
2. Rename the attach-prompt action labels to "Reopen in Container" /
   "Ignore" (§2).
3. Add a "Show Forwarded Ports" palette command that runs
   `docker port <id>` and renders the output (§7).

### Medium (requires new plugin-API surface)

4. Streaming spawn API + process cancellation (§4, §8).
5. `RemoteIndicatorState::Connecting` / `FailedAttach` variants plus a
   plugin op to transition them (§3, §4, §8).
6. Build-log virtual buffer + "Show Logs" action (§4).
7. "Show Container Logs" action via tailing `docker logs <id>` (§1).
8. Scaffold-new-config command for Local when no `devcontainer.json`
   is present (§1).

### Large / out of scope (principles-level divergence)

9. Remote editor host in the container (§5) — **not recommended**;
   violates `AUTHORITY_DESIGN.md` principles 2–4 and the "shrink the
   core" stance.

### Non-goals already documented

- No cross-authority composition, multi-root workspaces, or
  credential syncing — called out in `AUTHORITY_DESIGN.md` "What this
  refactor is not."

---

## Closing note

The implementation gap is dominated by UX surface (progress visibility,
logs, cancel, retry, port forwarding visibility) rather than by
architectural misalignment. The authority layer, the plugin boundary,
and the restart-based transition model together cover what the spec
describes as "Connection Agent" and "Remote Server Injection" in an
idiomatic way for a terminal editor. Most remaining items can land as
additive plugin-API surface (streaming, cancellation, indicator states)
plus plugin-side UI work, without touching the authority contract.
