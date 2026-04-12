# LSP Heuristic Eval — Verification & Remediation Plan

Follow-up to [`LSP_HEURISTIC_EVAL_CLANGD.md`](./LSP_HEURISTIC_EVAL_CLANGD.md). Every finding in that evaluation was cross-checked against the source at commit `ba29f55` (the eval's own tip). This document records, per finding:

1. whether the code corroborates the finding,
2. any corrections needed, and
3. for the headline concern (LSP lifecycle visibility) a concrete remediation plan.

A new e2e test — `crates/fresh-editor/tests/e2e/lsp_lifecycle_visibility.rs` — pins down the current user-visible behavior of the headline concern so regressions (in either direction) are detected.

## 1. What Happened: Verification Summary

| # | Finding (short) | Code status | Notes |
|---|---|---|---|
| H-1 | Dormant LSP has no UI indicator | **Corroborated** | Confirmed by code (`file_operations.rs:849` log-only) and by new e2e test. Status bar is byte-identical to "no LSP at all." |
| H-2 | No `$/progress` relay during indexing | **Contradicted (partially)** | Progress pipeline exists: `handle_lsp_progress` (`async_messages.rs:783-836`) → `update_lsp_status_from_progress` (`mod.rs:5290-5308`). The eval's observation means the pipeline did not fire during the fmt run — a real issue, but it is a bug in an existing feature, not a missing feature. |
| H-3 | Dual diagnostic counters (`E:/W:` + `[⚠]`) | **Corroborated** | `status_bar.rs:611-618` (LSP counts) and `:703-707` (general warning badge). "Clear Warnings" only clears the latter. |
| H-4 | Panel counter/title mismatch | **Unverified** | Could not locate a "Diagnostics Panel" matching the description. `show_warnings` opens a warning log file, not a panel. Needs a source-file citation or removal. |
| H-5 | Hover does not surface diagnostic text | **Corroborated** | `handle_hover_response` (`lsp_requests.rs:842-975`) renders only `textDocument/hover` contents. |
| H-6 | Hover under-informative on templates | **Corroborated** (as a direct consequence of H-5 + clangd hover payload) | Same code path. |
| H-7 | Silent failure when `compile_commands.json` is absent | **Corroborated** | No code path surfaces the "no compile command" clangd guidance. |
| H-8 | `.h` defaults to C, not C++ | **Corroborated** | `config.rs:3010` (C extensions include `.h`) vs `:3040-3047` (C++ uses `.hpp/.hh/.hxx` only). No project-aware fallback. |
| H-9 | Hover popup has no interior fill | **Contradicted** | Default `popup_bg = RGB(30,30,30)`, and `popup.rs:860-876` paints the block with `background_style`. The cyan border (color 51) and transparent interior the eval reports come from the theme used during the eval, not the popup code. Reframe as a theme finding. |
| H-10 | Low-contrast warning text in diagnostics panel | **Corroborated (theme-dependent)** | Same caveat as H-9 — color values are theme-driven. |
| H-11 | Transient + persistent UI share the status strip | **Corroborated** | Confirmed; see `StatusBarElement::Messages` in `status_bar.rs:633-652`. |
| H-12 | `--no-restore` still leaks session content | **Corroborated (with caveat)** | `main.rs:132, 760-762, 804`: workspace restore is gated on `!no_restore`, but hot-exit recovery still applies to files passed on the CLI. |
| H-13 | SIGTSTP ghost bar; orphan clangd on abnormal exit | **Corroborated** | `services/signal_handler.rs:121-126` handles only SIGINT/SIGTERM. No suspend handling, no reap on abnormal termination. |
| H-14 | Popups dismissed silently | **Corroborated** | No dismissal feedback wired to hover/popup lifecycle. Cosmetic. |
| H-15 | Latency under load is good (strength) | **Corroborated** | Matches behavior. |

**Net:** 10 findings corroborated, 1 corroborated with caveats (H-12), 1 theme-scoped rather than code-scoped (H-9), 1 partially contradicted (H-2), 1 unverified (H-4). The headline message of the eval — *LSP lifecycle state is invisible* — holds.

## 2. Headline Concern: LSP Lifecycle Visibility (H-1, H-2)

### 2.1 What the e2e test shows

`tests/e2e/lsp_lifecycle_visibility.rs::test_dormant_lsp_has_no_visible_indicator` configures a fake LSP with `auto_start=false`, opens a `.rs` file, runs 40 async ticks, and asserts:

- the server process is not spawned (spawn marker never appears), and
- the status bar is **byte-identical** to an editor with no LSP configured at all, and
- no cue on screen mentions the server, its state, or a way to start it.

Observed status bar (both scenarios):

```
hello.rs | Ln 1, Col 1 | Opened hello.rs    LF  ASCII  rust  Palette: Ctrl+P
```

### 2.2 Root-cause breakdown

There are really **two** lifecycle-visibility failures, currently conflated in the eval:

| Failure | Mechanism |
|---|---|
| **Dormant state is invisible** (H-1) | `StatusBarElement::Lsp` (`status_bar.rs:693-701`) renders `lsp_status`. When no server has spawned, `lsp_server_statuses` is empty, so `update_lsp_status_from_server_statuses` sets `lsp_status = ""` (`mod.rs:5311-5324`). The `Lsp` element returns `None` and the status bar draws nothing. There is no separate "configured-but-dormant" code path. |
| **Indexing progress does not surface** (H-2) | The progress relay exists (`handle_lsp_progress` → `update_lsp_status_from_progress`), but clangd's `WorkDoneProgressBegin/Report/End` messages didn't materialize on screen during the fmt run. Possible causes: progress token registration, render debouncing, or clangd sending progress faster than the tick interval. Needs instrumentation, not new architecture. |

### 2.3 Remediation Plan

Work is ordered by dependency and impact. Each item cites the specific code site that will be touched.

**Step 1 — Compute a `DormantLspCount` for the active buffer.**
In `update_lsp_status_from_server_statuses` (`mod.rs:5311`), read the loaded config and count per-language servers whose language matches the active buffer and which have `enabled=true, auto_start=false`. Store on `App` as `dormant_lsp_count: usize`. This is the data the status bar needs; it should be recomputed on the same events that drive `update_lsp_status_from_server_statuses`.

**Step 2 — Add a new `StatusBarElement::LspDormant` element.**
Render in `status_bar.rs` next to `StatusBarElement::Lsp`. When `dormant_lsp_count > 0` and `lsp_status.is_empty()`, render a muted badge like `LSP: off (N)`. When both are present (e.g. one server running + one dormant), render both: `LSP [rust: ready] · off (1)`. Keep it muted — not red, not yellow — so it doesn't fight the warning badge.

**Step 3 — Wire a click handler / keybinding.**
Existing command `start_restart_lsp` (`input/commands.rs:948`) opens a picker. Make the new badge clickable (hit-test the cell range) and bind to that command. Fall back to showing the command's keybinding in the badge's tooltip/help text.

**Step 4 — Add an e2e test that flips the characterization test.**
Update `lsp_lifecycle_visibility.rs`: change the equality assertion to a *difference* assertion, and positively assert that the status bar contains the dormant badge. The existing negative-cue list becomes the anti-regression check.

**Step 5 — Diagnose the progress-not-rendered bug (H-2).**
This is a bug in an existing feature, not net-new work. Add a fake-LSP test that:
- delays the `initialize` response 200ms,
- emits `$/progress` Begin/Report/End with a percentage on `initialized`, and
- asserts the status bar contains `indexing` and the percentage at some point.

If the assertion fails, the fix lives in `handle_lsp_progress` or the render-trigger path. If it passes, the fmt-specific case needs clangd-log instrumentation.

**Step 6 — Documentation.**
Update `docs/internal/LSP_HEURISTIC_EVAL_CLANGD.md` H-2 to describe the observation ("progress messages did not render during fmt indexing") rather than the (incorrect) diagnosis ("no progress relay"). Cross-reference the test added in Step 5.

### 2.4 Acceptance criteria

- Opening a buffer whose language has a configured-but-`auto_start=false` server renders a visible, non-alarming badge on the status bar.
- Clicking the badge (or invoking the bound keybinding) opens the LSP start picker.
- `lsp_lifecycle_visibility.rs` positively asserts the badge's presence and format.
- A new test asserts `$/progress` messages reach the status bar within one tick of arrival.
- The eval doc is updated so H-2 describes the real problem.

## 3. Status of Other Concerns

### 3.1 Tractable as small follow-ups

| Finding | Suggested fix | Scope |
|---|---|---|
| **H-3** (dual counters) | Relabel the general badge as `[plugin: N]` or merge its contribution into `W:`. One-line change in `status_bar.rs:702-710`. | S |
| **H-8** (`.h` → C) | When a `.h` is opened inside a tree with a sibling `.cc/.cpp/.cxx` or a `compile_commands.json`, promote the detected language to `cpp`. Edit `config.rs` detection plus a fallback check. | S-M |
| **H-11** (transient in status strip) | Introduce a short-lived toast line above the status bar with an auto-dismiss timer. Reuse `status_message` as the data source; route only persistent metadata through `StatusBarElement::Messages`. | M |
| **H-12** (`--no-restore` leak) | Gate hot-exit content re-application on the same `!no_restore` predicate used for workspace restore. `main.rs:804`. | S |
| **H-13** (SIGTSTP / orphan clangd) | Register a SIGTSTP handler that runs the terminal-teardown routine before `raise(SIGSTOP)`; register a `SIGTERM` → clangd shutdown path in `services/signal_handler.rs`. | M |
| **H-14** (silent popup dismissal) | One-line flash toast when hover is dismissed by `Ctrl+P`/save. | XS |

### 3.2 Theme, not code

| Finding | Action |
|---|---|
| **H-9** (hover popup interior) | Adjust `popup_bg` in the theme used during the eval; verify default theme already paints the interior. |
| **H-10** (warning row contrast) | Raise the warning-row luminance in affected themes to ≥ 4.5 : 1. Keep the `[W]` tag so severity remains redundantly encoded. |

### 3.3 Needs more investigation before planning

| Finding | Why |
|---|---|
| **H-4** (panel counter/title mismatch) | No source file matching "Diagnostics Panel" was found in this pass. Either cite the panel implementation explicitly (with a source link) or withdraw the finding. |
| **H-5 / H-6 / H-7** (hover informativeness) | Real, but the fix is a product decision: fuse hover + diagnostic in the editor (`lsp_requests.rs:842-975`), or rely on clangd's configuration. Both have trade-offs — latency, vertical space, double-render of the same error. Decide before coding. |

### 3.4 Already fine

**H-15** — latency under load. No action.

## 4. Single Highest-Leverage Next Step

Do steps 1–4 of section 2.3 — add the dormant-LSP badge and flip the characterization test. It is:

- small (one new status-bar element, one new `App` field, one test update),
- directly user-visible, and
- the fix the heuristic eval itself identifies as the top priority.

Everything else in this document is easier to prioritize once the "is my LSP even configured for this file?" question has a visible answer.
