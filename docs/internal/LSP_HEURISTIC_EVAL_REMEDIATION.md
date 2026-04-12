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

Design choice, up front: **no new status-bar element.** `StatusBarElement::Lsp` is already a pure function of a single string. The fix is to widen what that string describes — from "statuses of servers that have spawned" to "the full LSP situation for the active buffer, including configured-but-dormant servers." One element, one string.

**Implementation note:** on inspection of the code, the rendered LSP string is *not* read from `App::lsp_status` — it is recomputed at render time in `app/render.rs` from `lsp_server_statuses` + `config.lsp` scoped to the active buffer's language (the field `App::lsp_status` is used only by `get_lsp_status()` and some ad-hoc transient messages in `app/lsp_requests.rs`). So the fix ultimately lands in `render.rs`, and widens its three-case branch (running → `"LSP"`, dormant-with-auto-start → `"LSP off"`, otherwise → `""`) to four cases, adding the `enabled && !auto_start` case as `"LSP: off (N)"`. The two LSP-status composition paths are an existing duplication worth unifying as a follow-up — see `docs/internal/multi-lsp-design.md:671` for the already-planned refactor.

Work is ordered by dependency and impact. Each item cites the specific code site that will be touched.

**Step 1 — Widen the render-time LSP-status composition in `render.rs`.** ✅ Done.
`app/render.rs:594-640` used to branch on two cases: a running server for the active buffer's language → `"LSP"`, else (if an `auto_start=true` config existed) → `"LSP off"`, else → `""`. The third, H-1 case — an `enabled && !auto_start` config — was intentionally suppressed ("don't show an indicator to avoid cluttering the status bar for every language"). Replaced that branch with a count of dormant servers and appended a fourth case: `"LSP: off (N)"`.

| Running servers | `enabled && auto_start` count | `enabled && !auto_start` count | Rendered segment |
|---|---|---|---|
| ≥ 1 | —               | —               | `LSP`            |
| 0   | ≥ 1             | —               | `LSP off`        |
| 0   | 0               | N > 0           | `LSP: off (N)`   |
| 0   | 0               | 0               | *(empty)*        |

**Step 2 — Muted styling.**
Not done. The current fix reuses `ElementKind::Lsp`'s existing color. A follow-up should desaturate the dormant variant so it does not read as a state/error indicator.

**Step 3 — Click / keybinding on the indicator.**
Not done. The existing `start_restart_lsp` command is already reachable via the command palette; wiring a click on the indicator is nice-to-have but not urgent.

**Step 4 — Flip the characterization test.** ✅ Done.
`tests/e2e/lsp_lifecycle_visibility.rs` now positively asserts that configured-but-dormant servers render as `LSP: off (N)`, that the count reflects multiple dormant servers, that the control case (no LSP configured for the language) renders no indicator, and that the indicator updates correctly on buffer switch.

**Step 5 — Diagnose the progress-not-rendered bug (H-2).**
This is a bug in an existing feature, not net-new work. Add a fake-LSP test that:
- delays the `initialize` response 200ms,
- emits `$/progress` Begin/Report/End with a percentage on `initialized`, and
- asserts the status bar contains `indexing` and the percentage at some point.

If the assertion fails, the fix lives in `handle_lsp_progress` or the render-trigger path. If it passes, the fmt-specific case needs clangd-log instrumentation.

**Step 6 — Documentation.**
Update `docs/internal/LSP_HEURISTIC_EVAL_CLANGD.md` H-2 to describe the observation ("progress messages did not render during fmt indexing") rather than the (incorrect) diagnosis ("no progress relay"). Cross-reference the test added in Step 5.

### 2.4 Acceptance criteria

- Opening a buffer whose language has a configured-but-`auto_start=false` server renders a visible, non-alarming segment in the existing `StatusBarElement::Lsp` cell on the status bar.
- Clicking the cell (or invoking the bound keybinding) opens the LSP start picker.
- `lsp_lifecycle_visibility.rs` positively asserts the `LSP: off` string appears in the status bar.
- A new test asserts `$/progress` messages reach the status bar within one tick of arrival.
- The eval doc is updated so H-2 describes the real problem.
- No new `StatusBarElement` variant is introduced — the change is in the string composed by `update_lsp_status_from_server_statuses`.

## 3. Status of Other Concerns

### 3.1 Tractable as small follow-ups

| Finding | Suggested fix | Scope |
|---|---|---|
| **H-3** (dual counters) | Relabel the general badge as `[plugin: N]` or merge its contribution into `W:`. One-line change in `status_bar.rs:702-710`. | S |
| **H-8** (`.h` → C) | ✅ Done. `detect_language` now promotes `.h` to `cpp` whenever the header sits in a directory with C++ siblings (`.cc/.cpp/.cxx/.C/.hpp/.hh/.hxx`) or under an ancestor containing `compile_commands.json`. Tests in `services::lsp::manager::tests::test_detect_language_h_*` cover sibling-source, sibling-hpp, ancestor compile_commands, pure-C tree (no promotion), `.c` source (never promoted), and `cpp`-absent config (no promotion). |
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
| **H-5** (hover does not surface diagnostic text) | ✅ Done. `handle_hover_response` in `app/lsp_requests.rs` now calls `compose_hover_diagnostic_prefix`, which inspects `stored_diagnostics` for the active buffer's URI, selects any diagnostic whose LSP range overlaps the hover position (tracked as `pending_hover_position`), and renders a markdown prefix (severity emoji + label + source + message) above a horizontal rule before the hover body. The popup is forced into markdown mode when a diagnostic is injected so plain-text hovers wrap into a fenced code block and the severity heading renders correctly. An empty hover is no longer suppressed when a diagnostic is present — the popup still opens with the diagnostic alone. E2E coverage: `test_hover_popup_fuses_overlapping_diagnostic` and `test_hover_shows_diagnostic_even_when_hover_is_empty` in `lsp_diagnostic_flow.rs`; unit coverage for `lsp_range_contains` (inclusive start / exclusive end, multiline, zero-length anchor). |
| **H-6 / H-7** (hover informativeness, missing compile DB) | H-6 partially resolved as a consequence of H-5 — when clangd emits a `not in compile DB` diagnostic it now appears in the hover. H-7 (explicit one-time banner) remains open. |

### 3.4 Already fine

**H-15** — latency under load. No action.

## 4. Single Highest-Leverage Next Step

Do steps 1–4 of section 2.3 — widen `lsp_status` to cover dormant servers and flip the characterization test. It is:

- small (one function-body change in `update_lsp_status_from_server_statuses`, muted styling on `ElementKind::Lsp`, one test update; no new element, no new `App` field),
- directly user-visible, and
- the fix the heuristic eval itself identifies as the top priority.

Everything else in this document is easier to prioritize once the "is my LSP even configured for this file?" question has a visible answer.
