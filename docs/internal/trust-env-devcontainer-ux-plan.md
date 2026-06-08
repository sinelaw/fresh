# Trust + Env + Devcontainer Unified UX — Implementation Plan

Status: design plan. Specifies the user-facing flow that re-enables the
workspace-trust prompt (currently a no-op, see
`crates/fresh-editor/src/app/popup_dialogs.rs:977`) and brings env activation
to parity with the devcontainer "reopen?" prompt.

Threat model and the trust levels themselves are out of scope here — they
live in `workspace-trust-sandbox-design.md`. This doc only specifies
**when** prompts surface, **what** they say, and **how** the three features
(trust / env / devcontainer) interact so the common case is 0–1 popup.

## Phase 1 (this PR) vs. Phase 2 (follow-ups)

Implementing all eight rules end-to-end touches the trust gate, persistence
schema, status bar, plugin API, and at least three plugins. The first PR
delivers the visible UX shift on the plugin side; the remaining Rust-core
changes are tracked as Phase 2.

| Rule | Phase 1 (this PR) | Phase 2 |
|---|---|---|
| 1. `.venv` auto-activate silently | done — env-manager fires `maybeAutoActivate` on `plugins_loaded` and activates path-only envs without a popup | — |
| 2. `.envrc`/`mise.toml` combined trust+activate popup | done — env-manager surfaces the combined popup when trust is Restricted; `Trust & activate` dispatches `workspace_trust_trust` and applies the env in one step | — |
| 3. Devcontainer takes precedence, env defers | done — env-manager skips its popup when a `devcontainer.json` is present and authority is local; the post-attach `plugins_loaded` re-runs inside the container | — |
| 4. Deferred trust on first denied spawn | concrete trust-elevation popup *from the env flow* is wired (the user who runs `Env: Activate` against a restricted folder gets a concrete prompt instead of a dead-end status message) | replaced with a **queue-and-drain** model (see Rule 4 spec below): subscribers re-trigger their work on a `TrustLevel` broadcast — *no* new denial variant, no parked spawners, no synchronous block-and-wait inside `gate` |
| 5. Content-hash persistence | done in part — env decision is persisted per-cwd via plugin global state (`env-decision:<cwd>` → `"activated"` / `"dismissed"`) so the popup doesn't re-fire after a decision | extend `TrustStore` JSON schema with per-marker SHA-256 so re-prompts fire only when `.envrc` / `mise.toml` content actually changes |
| 6. Restricted-mode chip in status bar | done — env-manager registers a `trust` status-bar element that shows nothing when Trusted, `restricted` / `blocked` otherwise | wire chip clicks to open the trust popup directly (today plugins can't register click handlers on status-bar elements) |
| 7. Never stack popups | done in part — env-manager defers entirely to the devcontainer plugin when both apply | core arbitration so any future plugin popup competing with the trust modal queues instead of stacks |
| 8. Trust parent folder setting | — | new setting `workspace.trust.inheritFromParent` + parent-traversal in `workspace_trust.rs`; off by default |

The rest of this document describes the full design. Items not yet wired in
Phase 1 are called out inline.

## Goal

> Trust once, activate silently where safe, ask only when running shell —
> and make the non-trusted state visible.

| Folder contents | Popups today | Popups after this plan |
|---|---|---|
| Plain | 0 | 0 |
| `.venv` / `venv` | 1 (trust) | 0 |
| `.envrc` / `mise.toml` / `.tool-versions` | 1 (trust), then user must run command | 1 (trust + activate, combined) |
| `.devcontainer.json` only | 2 (trust, then reopen) | 1 (reopen — trust folded in) |
| Both env + devcontainer | 2 (trust, then reopen) | 1 (reopen); env asks post-restart inside container |
| `.csproj` / `Cargo.toml` only | 1 (abstract trust on open) | 1 (concrete, deferred to first spawn) |

## The rules (normative)

1. **`.venv` / `venv` auto-activates.** No popup. Activation is a `PATH`
   prepend; not arbitrary code execution. Status pill is the undo affordance.
2. **`.envrc` / `mise.toml` / `.tool-versions` get a single combined popup.**
   "Trust this folder and activate direnv?" with `[Trust & activate] /
   [Restricted] / [Block]`. Trust + activate are one decision.
3. **Devcontainer stays a separate, explicit decision.** Authority changes
   are heavyweight enough to deserve their own consent. When both apply,
   devcontainer goes first; env-activate runs inside the container after the
   authority restart, not on the host.
4. **Generic trust prompt fires on the first concrete need; trust changes drain a queue, not block.** A folder with only project manifests (`.csproj`, `Cargo.toml`, …) and no env or devcontainer config opens silently in restricted mode. When a plugin or LSP first tries to spawn, the gate denies normally — the *caller* surfaces a popup naming the actual command ("rust-analyzer wants to run `cargo`"). Picking Trust broadcasts a `TrustLevelChanged` event; every subscriber re-triggers the work that was denied. The gate stays sync, the spawn stays a normal `Allow`/`Deny`, and no thread is parked waiting on a UI decision. See "Rule 4 spec" below.
5. **Re-prompt only when the file's content hash changes.** First-time
   decisions for `.envrc` / `mise.toml` / `devcontainer.json` are persisted
   keyed by content hash. Unchanged file → silent re-activate next open.
   Edited file → re-prompt with "this file changed since you trusted it".
6. **Restricted mode is always visible.** Persistent status-bar chip
   (`restricted: LSPs off`) clickable to elevate. Env pill `env: .venv
   (locked)` clickable to trust-and-activate.
7. **Never stack popups.** Only one trust/env/devcontainer popup on screen
   at a time. Env-manager defers if devcontainer is going to prompt; deferred
   trust prompts queue behind any modal already up.
8. **"Trust parent folder" is opt-in, not default.** A setting, not a
   checkbox on the prompt. Default behavior is per-folder.

## Affected files

| File | Change |
|---|---|
| `crates/fresh-editor/src/app/popup_dialogs.rs:977` | Replace the WIP no-op `maybe_prompt_workspace_trust` with the deferred-trust scheduler (rule 4) and the combined env popup (rule 2). |
| `crates/fresh-editor/src/services/workspace_trust.rs` (around `gate` and `set_level`) | `gate` stays a sync `Allow`/`Deny` — *no* new denial variant. Add a `tokio::sync::broadcast::Sender<TrustLevel>` on `WorkspaceTrust`; `set_level` publishes the new level so subscribers (LSP manager, env-manager, devcontainer plugin) can re-trigger their denied work. See "Rule 4 spec" below. |
| `crates/fresh-editor/src/services/workspace_trust.rs:389-461` | Add content-hash recording per marker file alongside the path-keyed decision (rule 5). Split markers into "env-shell" (`.envrc`, `mise.toml`, `.tool-versions`, `Pipfile`, `poetry.lock`), "env-path-only" (`.venv`, `venv`), "devcontainer", and "project-manifest" — the four rules treat them differently. |
| `crates/fresh-editor/plugins/env-manager.ts:48-74` | Split `detect()` by category. `.venv`/`venv` → return a `kind: "path-only"` result that the plugin auto-activates without checking trust (rule 1, since no shell runs). `.envrc`/`mise.toml` → `kind: "shell"`, gated on trust, surfaces the combined popup if undecided. |
| `crates/fresh-editor/plugins/env-manager.ts:84-87` | Replace the dead-end "not trusted" status message with the trust-elevation flow: untrusted user clicks Activate → combined `[Trust & activate]` popup. |
| `crates/fresh-editor/plugins/env-manager.ts:130-158` (status pill) | Pill `(locked)` becomes a clickable affordance — click fires the combined popup. |
| `crates/fresh-editor/plugins/devcontainer.ts:2376-2410` | Add a guard: if env-shell markers also exist and authority is local, the env-manager defers; nothing to change here, but document the contract. After successful attach + restart, env-manager re-runs inside the container — no change, this already works via `plugins_loaded`. |
| `crates/fresh-editor/plugins/csharp_support.ts:140-163` | On spawn `Deny`, surface a `showActionPopup` naming the command and offering `[Trust & retry]` / `[Keep restricted]`. Pick "Trust & retry" → `executeActions(workspace_trust_trust)`. The `trust_changed` hook (fired by the broadcast subscriber on the JS side) re-invokes the spawn — the plugin doesn't have to remember to retry. Same shape as the env-manager's existing trust-elevation popup, just from a different trigger. |
| `crates/fresh-editor/plugins/lib/fresh.d.ts` | Add a new `trust_changed` event (`HookEventMap.trust_changed: { level: "trusted" \| "restricted" \| "blocked" }`) bridged from the core broadcast channel. Plugins subscribe with `editor.on("trust_changed", …)` to re-trigger denied work after elevation. *Not* needed: a `requestTrustElevation` API — popups are normal `showActionPopup`s wired to `executeActions(workspace_trust_trust)`. |
| `crates/fresh-editor/src/services/lsp/manager.rs` (LSP server retry) | Subscribe to the trust broadcast at LspManager construction. On `Trusted`, re-issue server starts that failed under Restricted (track per-language denial state). This is what makes LSP "come back online" after the user trusts, with no human-visible "retry" button anywhere. |
| (new) status-bar chip for restricted mode | Persistent indicator when `workspaceTrustLevel() === "restricted"`, clickable to open the trust popup. Lives alongside the env pill. |

## Decision flow on open

```
on_workspace_open(cwd):
  markers = classify(executable_content_markers(cwd))
  prior   = load_decisions(cwd)            # path + content-hash keyed

  # rule 1 — silent
  if "path-only" in markers and (no prior dismissal):
    env_manager.activate_silently(".venv")  # no popup, sets PATH

  # rule 3 — devcontainer wins if present
  if "devcontainer" in markers and prior.devcontainer is undecided_or_stale_hash:
    show_devcontainer_popup()        # existing flow
    return                            # env defers to post-restart re-run

  # rule 2 — combined env+trust popup
  if "env-shell" in markers and prior.env is undecided_or_stale_hash:
    show_combined_env_trust_popup(detected_name, marker_file)
    return

  # rule 4 — silent open, concrete prompt at first spawn
  # no proactive popup. Restricted-mode chip is visible. The next spawn
  # that hits Deny(Restricted) → its caller surfaces a popup naming the
  # actual command. "Trust & retry" elevates the level; subscribers to
  # the TrustLevelChanged broadcast re-trigger their work.
```

## Combined popup spec (rule 2)

```
┌─────────────────────────────────────────────────┐
│  Environment detected                           │
│                                                 │
│  This folder has a direnv environment (.envrc). │
│  Activating it runs shell from the folder.      │
│                                                 │
│  [ Trust & activate ]                           │
│  [ Restricted (no env, no LSPs run repo code) ] │
│  [ Block all execution ]                        │
└─────────────────────────────────────────────────┘
```

- `Trust & activate` → trust level set to Trusted, env activates, hash recorded.
- `Restricted` → trust level set to Restricted, hash recorded, chip visible.
- `Block` → trust level set to Blocked, hash recorded, chip visible.
- Escape → undecided; re-prompt next open. Same as today's trust popup.

## Rule 4 spec — concrete prompt + queue-and-drain on trust change

This is the part of the plan that **replaces** the earlier "deferred denial /
third-state" idea after a research pass (see "Why not block-and-wait" below).

### What the user sees

A folder with only project manifests opens silently in Restricted. The
restricted-mode chip from rule 6 is visible — that's the "something is gated
here" signal. The user does *not* see a trust prompt on open.

The moment a piece of tooling actually tries to run, its caller shows a
contextual popup naming the actual command:

```
┌─────────────────────────────────────────────────┐
│  Trust this folder?                             │
│                                                 │
│  rust-analyzer wants to run `cargo` to load     │
│  this project. Trust this folder?               │
│                                                 │
│  [ Trust & retry ]                              │
│  [ Keep restricted ]                            │
│  [ Block ]                                      │
└─────────────────────────────────────────────────┘
```

The concrete command is the entire UX win — it answers "why is this prompt
on screen?" in the prompt itself, instead of the abstract "this project can
run code on your machine" that VS Code is criticized for. Picking
`Trust & retry` elevates and the tool starts; nothing the user has to
re-click.

### What runs underneath

1. **`gate` stays sync `Allow`/`Deny`.** No new denial variant, no
   `Undecided` third state. A spawn that hits Restricted denies normally.
2. **Caller surfaces the popup.** Whichever subsystem initiated the spawn
   (LSP manager, env-manager, plugin) catches the `SpawnError::Process(...)`
   from `gate`, sees the workspace is Restricted, and calls `showActionPopup`
   with the command name baked into the message. For plugins this is
   `editor.showActionPopup({...})` with two actions wired to
   `executeActions("workspace_trust_trust" | "workspace_trust_restrict")`.
3. **`set_level` broadcasts.** `WorkspaceTrust` gains a
   `tokio::sync::broadcast::Sender<TrustLevel>`. `set_level` publishes the
   new level on every transition (including Restricted → Trusted, which is
   the case rule 4 cares about).
4. **Subscribers drain on `Trusted`.** Each subsystem that holds
   denied-spawn state subscribes:
   - **LSP manager** tracks per-language "denied at startup" and re-issues
     the start on the broadcast.
   - **Plugin runtime** bridges the broadcast to a JS-visible
     `trust_changed` hook so plugin-side `spawnProcess` callers can retry.
   - **env-manager** already activates on `plugins_loaded` *and* on user
     command; the `trust_changed` subscription re-runs `maybeAutoActivate`
     so a shell-env folder activates as soon as the user trusts (today the
     user has to re-open or run `Env: Activate` manually).
5. **No coalescing required.** Multiple denied subsystems each surface
   their own popup independently *only if they hit the gate first*; in
   practice they don't, because the first one's popup blocks the user's
   attention and the others queue silently behind it (the existing
   "popup-on-popup-suppression" already handles this — see
   `popup_dialogs.rs:1014-1025`). Trust elevation drains the lot.

### Why not block-and-wait inside `gate`

The block-and-wait shape (gate parks the spawner on a oneshot, popup
unblocks it) is technically possible in fresh — every spawner is `async`
and runs on the Tokio runtime (`editor_init.rs:597`), and no spawn site is
on the UI event loop (`main.rs:4008`), so blocking inside a spawner
wouldn't freeze the UI. But:

- **The industry is moving away from blocking modals for permission
  prompts.** Chrome and Firefox explicitly retired blocking permission
  modals in favor of one-time-allow chips (see web.dev "Permissions best
  practices", Chrome "permissions chip"). Android never shipped a true
  blocking permission API — `ActivityResultLauncher` is callback / suspend.
  Modeling our trust prompt on a pattern browsers spent five years
  retreating from imports their UX baggage.
- **Zed and JetBrains both ship queue-and-drain.** Zed worktree trust
  refuses to start LSP/MCP server spawns under Restricted and re-runs them
  on trust grant (`zed.dev/docs/worktree-trust`). JetBrains Safe Mode
  disables Gradle/Maven/sbt import and replays the deferred startup
  activities when the user trusts. Both ship as the de facto convention for
  IDE workspace trust today.
- **VS Code offers both** (`requestWorkspaceTrust({ modal: true })` blocks
  via `await`; `onDidGrantWorkspaceTrust` is the deferred event), but the
  ecosystem treats the modal API as a niche escape hatch for explicit user
  actions; the recommended `capabilities.untrustedWorkspaces` extension
  manifest is exactly queue-and-drain.
- **Block-and-wait needs new plumbing fresh doesn't have today**: a
  per-workspace pending queue on `WorkspaceTrust`, a Notify or oneshot from
  the UI side back to the parked spawner, popup coalescing logic ("first
  spawn wins the popup, subsequent ones wait silently"), cancellation
  paths so killing the originating command also drops the waiter. Each is
  a real failure mode (leaked queues on shutdown, popup deduplication
  bugs, parked waiters surviving workspace switch). Queue-and-drain needs
  one broadcast channel and a per-caller subscription — substantially
  less surface area.
- **The "parked thread blocks UI" failure isn't just hypothetical.** If
  the gate ever moves to a sync API or a future caller forgets the async
  contract, blocking inside `gate` deadlocks. The sync-Allow/Deny
  contract is harder to misuse.

### Trade-offs of the chosen shape

- **Spawn may briefly fire-then-retry** when trust changes during startup
  (LSP starts under Restricted, denies, then trust is granted, LSP
  restarts). User-visible as a one-time "LSP starting…" flicker. The
  alternative — never starting until trust lands — is also fine but
  changes the perceived UX (no "I see the project loading" until trusted).
- **Subscribers must remember to re-trigger.** The cost of the
  responsibility moving from `gate` to the caller. Mitigated by routing
  through `LspManager` / plugin runtime, which already own retry logic for
  unrelated reasons (server crashes, plugin reloads).
- **A spawn that legitimately fails for a non-trust reason during a trust
  change** could race-retry on the broadcast and re-fail. Subscribers
  gate their retry on "the previous denial was a trust denial", not "any
  denial".

## Content-hash persistence (rule 5)

Existing trust decisions persist at `<data_dir>/workspaces/<encoded-path>/trust.json`
(see `workspace_trust.rs:322-376`). Extend the schema:

```jsonc
{
  "level": "trusted",
  "markers": {
    ".envrc":           { "sha256": "abc…", "decided_at": "..." },
    "devcontainer.json":{ "sha256": "def…", "decided_at": "..." }
  }
}
```

On re-open, if the file is still present and hash matches, skip the popup
and re-activate silently. If the hash differs, re-prompt with "this file
changed since you trusted it" in the message — same buttons.

## "Trust parent folder" setting (rule 8)

Off by default. A user setting `workspace.trust.inheritFromParent: bool` or
similar. When true, on open, walk the parent chain looking for a recorded
trust decision — if any ancestor is trusted, inherit. Power users who keep
all their code under `~/code` flip this on and never re-prompt for fresh
clones. The setting must be off by default because the entire point of
trust is to gate cloning hostile content into trusted-ancestor directories
(the documented VS Code attack pattern).

## Non-goals

- Changing the trust threat model or the sandboxed-execution semantics —
  see `workspace-trust-sandbox-design.md`.
- Automating "reopen in container" — the user must explicitly consent;
  authority changes are heavyweight.
- File-watching `.envrc` for live reload during a session. Reload remains
  a manual `Env: Reload` command, as today.

## Test plan

E2E coverage to add under `crates/fresh-editor/tests/e2e/`:

1. `.venv`-only folder → no popup, env pill shows `.venv`, terminal has the
   activated `PATH`.
2. `.envrc`-only folder, first open → combined popup; pick `Trust & activate`
   → env activates, hash recorded.
3. `.envrc`-only folder, second open, file unchanged → no popup, silent
   activation.
4. `.envrc`-only folder, second open, file edited → re-prompt with "changed"
   message.
5. `devcontainer.json` + `.envrc` → devcontainer popup only; dismiss
   "Reopen" → env popup appears.
6. `devcontainer.json` + `.envrc` → devcontainer popup; accept "Reopen" →
   no env popup on host; after restart inside container, env popup appears.
7. `.csproj`-only folder → no popup on open; open a `.cs` file → C# plugin
   tries `dotnet restore`, gate denies, plugin surfaces a popup naming
   the command; pick `Trust & retry` → `dotnet restore` runs and the LSP
   starts (driven by the LSP manager's broadcast subscriber, not a manual
   re-invoke from the plugin).
8. Restricted state shows a persistent chip; clicking the chip opens the
   trust popup.
9. Setting `workspace.trust.inheritFromParent = true` — fresh clone under a
   trusted parent opens silently.

## Open questions

- **Hash scope for `mise.toml`:** include `.tool-versions` siblings, or
  per-file? Decision: per-file. Editing `.tool-versions` should re-prompt
  independently of `mise.toml`.
- **Restricted vs Undecided messaging in the chip:** different copy?
  Probably yes — Undecided says "this folder hasn't been trusted yet";
  Restricted says "you chose restricted, click to change".
- **Surfacing of "Restricted" deferred denies:** if user explicitly chose
  Restricted and a plugin gets denied, do we show a (dismissible) toast or
  stay silent? Lean silent — the user said no. Status chip is enough.
- **Suppress the rule-4 popup when the user chose Restricted explicitly?**
  Yes — if `TrustStore::is_decided() && level == Restricted`, the spawn
  caller does *not* surface the "Trust & retry" popup. The user made a
  deliberate choice; re-asking on every denied spawn is the nag-screen
  failure mode. They can still flip via the status chip / palette
  command. The popup is reserved for the Undecided-default case.
- **Per-subsystem subscriber gating to avoid retry storms:** an LSP that
  crashed for unrelated reasons during a trust elevation should not
  silently re-spawn just because the broadcast fired. Subscribers
  remember "the last spawn was denied for trust", and the broadcast only
  fires that subset.
- **Devcontainer trust:** committed `devcontainer.json` is itself
  repo-controlled content; today we treat it as such (it's in the trust
  markers list). The combined popup for env does *not* extend to
  devcontainer because the reopen flow has its own explicit prompt. Keep
  separate.
