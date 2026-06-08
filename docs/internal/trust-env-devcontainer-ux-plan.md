# Trust + Env + Devcontainer Unified UX — Implementation Plan

Status: design plan. Specifies the user-facing flow that re-enables the
workspace-trust prompt (currently a no-op, see
`crates/fresh-editor/src/app/popup_dialogs.rs:977`) and brings env activation
to parity with the devcontainer "reopen?" prompt.

Threat model and the trust levels themselves are out of scope here — they
live in `workspace-trust-sandbox-design.md`. This doc only specifies
**when** prompts surface, **what** they say, and **how** the three features
(trust / env / devcontainer) interact so the common case is 0–1 popup.

## North Star — the ideal UX

> **Open a folder. Things work. You start coding.**

For the overwhelming majority of opens, that is the entire user-facing
experience: no popup, no chip nagging, no "trust this folder?" The editor
figures out the right thing from context, does it, and reflects what it
did via the status bar. Modals are not the cost of admission for
opening a project.

The rules below are stepping stones toward this state. They're chosen
because each is implementable on the architecture we have today — but the
direction we want every change to push is *fewer interactions, more
implicit defaults, clearer state at a glance*. If a proposed change would
*add* a prompt to a happy path that today has none, reject it.

### Principles, in priority order

1. **Memory beats prompting.** If the user has opened this folder before,
   the editor remembers what they decided. No re-asking.
2. **Provenance beats asking.** A folder cloned into the user's usual
   workspace from a host they've cloned from before is implicitly
   trusted. A folder dropped into `/tmp/from-email/` is not. The editor
   infers from context instead of asking.
3. **Status beats prompts.** A status bar that tells you "what is
   happening" is worth ten prompts that ask "should this happen."
4. **Inline beats modal.** When the editor does need an answer, it asks
   in a banner that yields to whatever the user is doing — never a modal
   that blocks the editor.
5. **Do the right thing, make undo trivial.** A reversible default
   action beats an irreversible question. If venv activation can be
   undone with one keystroke, just activate.
6. **Heavy actions confirm once, never again.** A devcontainer rebuild
   costs minutes — that deserves one confirmation per folder, ever, not
   per session or per `git pull`.
7. **Errors are inline, not modal.** A build failure is a banner with a
   "show logs" button, not a popup demanding attention.
8. **Settings are the canonical way to change recurring behavior.** "I
   never want venv to auto-activate" is a config option, not a habit of
   dismissing prompts.

### What the user sees in the ideal world

**Familiar project, second open onward:**

```
~/code/my-project · direnv ✓ · rust-analyzer ✓ · trusted
```

Status bar only. The editor recognized the folder, restored every
decision, started every server. Zero clicks. The user starts coding.

**New project from a familiar place (`git clone` into `~/code/`):**

```
~/code/new-repo · .envrc detected · ts-server starting
[Banner] This folder has a `.envrc`. Activate direnv?
         [Activate] [Always here] [Not now] [Never here]
```

Trust is implicit from provenance. The TS server (unambiguous from
`package.json`) starts. The `.envrc` is a genuinely new decision and
gets a non-modal banner — the editor is fully usable, the banner waits.

**Suspicious folder (`/tmp/from-zip`):**

```
/tmp/x · restricted · 3 things would run here · LSPs off
[Banner] This folder is outside your usual workspaces. It contains
         `.envrc`, `Cargo.toml`, `build.rs` — code that runs at
         project load. Review or trust?
         [Review what runs] [Trust this folder] [Read-only mode]
```

Framing is "outside your usual workspaces", not "MALWARE WARNING."
"Read-only mode" is first-class — many users open random folders just
to read code, and that path should require zero decisions.

**Devcontainer project:**

```
[Banner] This project has a dev container (`api-service`).
         Use it? Building takes ~2 min.
         [Use container] [Stay local for now]
```

One question, one time per folder, ever. The decision persists. To
revisit: click the authority indicator in the status bar.

**Trust required for an LSP, on a restricted folder:**

```
[Banner] rust-analyzer can't run here (trust required).
         [Trust this folder] [Show what it would run] [Dismiss]
```

Contextual — names the concrete tool. Non-modal. If dismissed, the
status bar shows `LSP: held` so the user has a visible way back.

**"What is the editor doing?" panel (one keystroke, e.g. `Alt+?`):**

```
Project actions
• Activated direnv (matched .envrc hash, allowed once)
• Started rust-analyzer (Cargo.toml found)
• Skipped: .devcontainer (you chose "stay local")
[Revisit decisions] [Restrict this folder]
```

Every decision is visible and revisitable. Discoverability without
nagging.

### What disappears from the user's life

- The phrase "Trust this folder?" Replaced by "this is outside your
  usual workspaces" when novel, silent when familiar.
- The "would you like to reopen in container?" popup, repeatedly.
  Replaced by one question per folder, ever.
- The `(locked)` pill. Replaced by a status line that says *what* is
  gated and *what to do*.
- A modal workspace-trust dialog blocking the editor on open. Ever.
- Stacked popups. Inline banners replace popups; the status bar replaces
  ambient indicators.
- The need to know what "Restricted mode" means. Either the editor is
  doing what you expect (trusted) or it's not, and a banner explains
  what's gated and offers the obvious next step.

### Capabilities the editor needs to deliver this

Not implementation detail, but capabilities that have to exist:

- **Persistent memory of every decision per folder**, surviving across
  sessions and machines (sync via dotfiles or a config service).
- **Provenance awareness** — heuristics about whether a folder was
  `git clone`d into a usual workspace, downloaded, or unzipped.
- **Content fingerprinting** — `.envrc` allowed by hash, so editing it
  re-asks; the same `.envrc` across multiple folders only asks once.
- **A real status bar with click affordances** — the indicator *is* the
  affordance, not a separate "click here to elevate" button.
- **A banner system distinct from popups** — non-modal, dismissible,
  persistent until acted on, at most one on screen at a time.
- **An action log** — what the editor decided, when, and why. Visible.
- **Settings that mean what they say** — "auto-activate direnv" actually
  skips the banner.

### Failure modes we have to design around

- **Over-eager auto-activate** wipes the user's `PATH`. Mitigation:
  status bar visibly changes; action log shows what happened; one
  keystroke undo.
- **Under-eager refuse-to-help** leaves the user in a restricted folder
  wondering why nothing works. Mitigation: banner is *always* present
  when there's a gated decision pending — no silent restrictions.
- **Persistent-decision regret**: user clicks "Trust always" and later
  wishes they hadn't. Mitigation: action log makes every decision
  visible and revisitable.
- **Novel-folder false positive**: editor thinks a familiar folder is
  novel because the user reorganized their workspace. Mitigation:
  provenance heuristics are advisory; worst case is one extra banner.
- **Banner fatigue**: banners accumulate, become as bad as popups.
  Mitigation: at most one banner on screen; secondary banners queue
  silently and reveal as the user resolves the first.

### Why the rest of this doc exists

The North Star is the target. The rules below are what we can ship on
the current architecture in this PR + the next couple. They consciously
trade some of the ideal (real provenance heuristics, content-hash
fingerprinting, sync-across-machines memory, a banner system distinct
from popups, an action log) for things that are tractable today
(one-popup-at-a-time within a single open, per-folder memory in
`trust.json`, the existing action-popup mechanism). When a future PR can
move us closer to the ideal — e.g., replacing the activate popup with a
non-modal banner — that PR should reference this section and explain
which principle it advances.

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

## Goal of the stepping-stone work

> Trust once, activate silently where safe, ask only when running shell —
> and make the non-trusted state visible.

This is the **near-term** goal — what the rules below collectively achieve
on the current architecture. It is *not* the North Star; it is the
shortest path toward the North Star that fits the existing popup,
status-bar, and persistence primitives. The differences are deliberate:

- Today we still use modal action popups, not banners, because the banner
  primitive doesn't exist. Once it does, every popup the rules below
  describe should migrate to a banner.
- Today we still ask about `.envrc` on first open instead of inferring
  from provenance, because provenance heuristics don't exist. Once they
  do, the combined trust+activate popup should be silent for clones into
  trusted parents.
- Today we still surface "(locked)" in the env pill, because clickable
  status-bar elements with custom handlers don't exist. Once they do,
  `(locked)` becomes a status-line affordance instead of a passive label.

Every "1" in the right column below is shorthand for "until banners exist
and provenance heuristics exist." When they do, the column should read
"0."

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

## Non-goals (of this stepping-stone plan)

- Changing the trust threat model or the sandboxed-execution semantics —
  see `workspace-trust-sandbox-design.md`.
- Automating "reopen in container" — the user must explicitly consent;
  authority changes are heavyweight.
- File-watching `.envrc` for live reload during a session. Reload remains
  a manual `Env: Reload` command, as today.

These are non-goals for *this* plan, not for the North Star. The ideal
UX described at the top eventually subsumes some of these (file-watching
for hash changes folds naturally into the content-fingerprint capability
in §"Capabilities the editor needs to deliver this"). But the stepping
stones don't need to ship them.

## Path from here to the North Star

The rules below land roughly 60–70% of the user-visible North Star UX on
the existing primitives. To close the remaining gap, each of the
following capability investments would compound — they unlock the
*invisibility* that the North Star describes:

1. **Non-modal banner system** (highest-leverage). Today every popup
   blocks something — focus, input, the visual center of the screen.
   The North Star asks for banners: an at-most-one persistent strip that
   doesn't steal focus, has dismiss/act buttons, and queues. Once this
   primitive exists, the env / devcontainer / trust-elevation prompts in
   the rules below should migrate to it. The popup is reserved for the
   handful of cases that genuinely need to interrupt (e.g., destructive
   confirmations).
2. **Provenance heuristics.** Today we treat every new folder identically
   regardless of how it arrived on disk. The North Star wants
   `git clone` into `~/code/` to be implicitly trusted, while
   `/tmp/unzipped` is implicitly suspect. Heuristics worth investing in:
   parent-folder trust (Rule 8 here, but framed as a heuristic not a
   setting), origin-of-folder via git remote URL hostname, path prefix
   patterns the user keeps clean code under, OS-level "downloaded from
   the internet" marks. Each is fallible; combined with the action log
   they're forgivable.
3. **Per-marker content fingerprinting in core.** Rule 5 in this plan.
   Beyond what Rule 5 covers: a global "I trust this `.envrc` hash"
   ledger that spans folders (the same env script committed to ten
   projects only asks once), and a "this file changed since I trusted
   it" prompt that compares the new content side-by-side with the
   trusted version.
4. **Clickable status-bar elements with custom handlers.** Today the
   env pill is informational; the chip in Rule 6 is informational; the
   authority indicator is informational. The North Star wants every
   status-bar indicator to *be* its own affordance. This needs a plugin
   API for click handlers on registered status-bar elements (or a
   core-side renderer that dispatches to a registered hook).
5. **An action log / "what is the editor doing?" panel.** Distinct from
   the existing status-message scrollback — a structured record of every
   trust, env, and authority decision the editor has made for this
   workspace, with timestamps and "revisit" buttons. This is what makes
   silent auto-activation safe: the user can always audit. Without it,
   silent activation feels like the editor lying.
6. **Cross-machine sync for decisions.** The North Star assumes that
   trusting a folder on machine A means it's trusted on machine B. Today
   `trust.json` lives in the user's data dir, so this is an upstream
   sync problem (dotfiles or a config service), not an editor problem —
   but the editor should make the file easy to sync (stable schema,
   merge-friendly format, documented location).
7. **Read-only mode as a first-class trust level.** The North Star's
   "Read-only mode" option for suspicious folders isn't in our trust
   model today — we have Restricted (gates code execution) but not "no
   writes, no edits even." Worth designing in: a fourth trust level
   below Restricted, scoped to "I'm just reading."

A reasonable sequencing: (1) banner system, (4) clickable status bar
elements, (5) action log — these three together cover the majority of
the user-visible North Star. (2) and (3) are background-only changes that
make the experience feel magical without changing what's on screen. (6)
and (7) are stretch goals.

The rules below ship without any of these; each subsequent PR that
delivers one should reference this section and identify which North Star
capability it unlocks.

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
