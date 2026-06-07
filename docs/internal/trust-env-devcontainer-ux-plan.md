# Trust + Env + Devcontainer Unified UX — Implementation Plan

Status: design plan. Specifies the user-facing flow that re-enables the
workspace-trust prompt (currently a no-op, see
`crates/fresh-editor/src/app/popup_dialogs.rs:977`) and brings env activation
to parity with the devcontainer "reopen?" prompt.

Threat model and the trust levels themselves are out of scope here — they
live in `workspace-trust-sandbox-design.md`. This doc only specifies
**when** prompts surface, **what** they say, and **how** the three features
(trust / env / devcontainer) interact so the common case is 0–1 popup.

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
4. **Generic trust prompt is deferred to the first denied spawn.** A folder
   with only project manifests (`.csproj`, `Cargo.toml`, …) and no env or
   devcontainer config opens silently in restricted mode. The trust prompt
   surfaces the moment a plugin tries to spawn (e.g. `dotnet restore`,
   `cargo`) and the gate denies — with the actual command and reason shown,
   not the abstract "this project can run code".
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
| `crates/fresh-editor/src/services/workspace_trust.rs:468` (`gate`) | When trust is `Undecided` and a spawn is denied, return a sentinel error that the spawn caller surfaces as a deferred prompt instead of a silent fail. Today it just denies. |
| `crates/fresh-editor/src/services/workspace_trust.rs:389-461` | Add content-hash recording per marker file alongside the path-keyed decision (rule 5). Split markers into "env-shell" (`.envrc`, `mise.toml`, `.tool-versions`, `Pipfile`, `poetry.lock`), "env-path-only" (`.venv`, `venv`), "devcontainer", and "project-manifest" — the four rules treat them differently. |
| `crates/fresh-editor/plugins/env-manager.ts:48-74` | Split `detect()` by category. `.venv`/`venv` → return a `kind: "path-only"` result that the plugin auto-activates without checking trust (rule 1, since no shell runs). `.envrc`/`mise.toml` → `kind: "shell"`, gated on trust, surfaces the combined popup if undecided. |
| `crates/fresh-editor/plugins/env-manager.ts:84-87` | Replace the dead-end "not trusted" status message with the trust-elevation flow: untrusted user clicks Activate → combined `[Trust & activate]` popup. |
| `crates/fresh-editor/plugins/env-manager.ts:130-158` (status pill) | Pill `(locked)` becomes a clickable affordance — click fires the combined popup. |
| `crates/fresh-editor/plugins/devcontainer.ts:2376-2410` | Add a guard: if env-shell markers also exist and authority is local, the env-manager defers; nothing to change here, but document the contract. After successful attach + restart, env-manager re-runs inside the container — no change, this already works via `plugins_loaded`. |
| `crates/fresh-editor/plugins/csharp_support.ts:140-163` | On spawn `Deny` with reason `Undecided`, do not show a generic status message — let the core surface the deferred prompt (rule 4). On `Deny` with reason `Restricted`, keep the existing status message. |
| `crates/fresh-editor/plugins/lib/fresh.d.ts` | Add `editor.showActionPopup` `kind: "trust-and-activate"` (or just reuse generic action popup with appropriate `actions`); add `editor.requestTrustElevation(reason: string, command?: string)` so plugins can ask core to surface the deferred prompt without re-implementing it. |
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

  # rule 4 — defer everything else
  # no proactive popup. Restricted-mode chip is visible; deferred prompts
  # fire on the first spawn that hits Deny(Undecided).
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

## Deferred trust prompt spec (rule 4)

Triggered when `workspace_trust::gate` would deny a spawn and trust is
`Undecided` (not explicitly `Restricted`). Plugin-facing API:
`editor.requestTrustElevation(reason, command)` — core schedules the popup
and returns; plugin treats the spawn as failed for this round.

```
┌─────────────────────────────────────────────────┐
│  Trust this folder?                             │
│                                                 │
│  C# support wants to run `dotnet restore` in    │
│  this folder. Trust it?                         │
│                                                 │
│  [ Trust ]                                      │
│  [ Keep restricted ]                            │
│  [ Block ]                                      │
└─────────────────────────────────────────────────┘
```

Concrete command in the prompt is the entire UX win. The user knows what
they're authorizing.

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
   tries `dotnet restore` → deferred trust prompt with concrete reason.
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
- **Devcontainer trust:** committed `devcontainer.json` is itself
  repo-controlled content; today we treat it as such (it's in the trust
  markers list). The combined popup for env does *not* extend to
  devcontainer because the reopen flow has its own explicit prompt. Keep
  separate.
