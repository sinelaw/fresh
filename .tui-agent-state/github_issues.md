# GitHub Issues Index

This is the canonical reference for every GitHub issue this agent has filed.
**Check this file BEFORE searching GitHub or filing any new issue.**
If a topic appears here — open or closed — do not file a duplicate.

Last updated: Run #37, 2026-06-22 (#2307 CONFIRMED FIXED in v0.4.1 via UI — closed by maintainer; commented. No new issues filed.)

---

## Open Issues (agent-filed)

| [#2317](https://github.com/sinelaw/fresh/issues/2317) | Review Diff: line-level visual staging/unstaging/discard (`v` then `s`/`u`/`d`) never works | Run #36 | **Open** | With cursor ANSI-verified on a real `+`/`-` line: `v`+`s` and `v`+`d` → `Selection has no add/remove lines or crosses hunk boundary` (no-op); `v`+`u` → `Patch failed: … patch does not apply`. Tried single `+`, full `-`/`+` (v+j), pure single-add — all fail. CONTROL: plain hunk `s`/`u`/`d` (no `v`) all work same session → only the `v` line-level path is dead. Help bar advertises `Visual: j/k extend, s/u/d apply`; docs/features/git.md promises "line-level visual selection". Distinct from closed hunk-level #2117. Workaround: hunk/file ops or `git add -p`. Do NOT re-file. |
| [#2318](https://github.com/sinelaw/fresh/issues/2318) | Review Diff: file-level Discard (`D`) reports "Discarded" but leaves staged changes intact for a fully-staged file | Run #36 | **Open** | `D` "Discard changes in file" only reverts the WORKING tree. On a fully-staged file (`M `, no working diff) it reports `Discarded: <file>` after a "this cannot be undone" dialog but git status / `git diff --cached` / disk are unchanged — nothing discarded. Contrast: on an UNSTAGED file `D` correctly reverts to HEAD. `D` only ever touches the index-free working tree; UI claims a full file discard regardless. (Also: `D`/`d`/`x` only fire when cursor is on a hunk CONTENT row.) Workaround: unstage first, or `git restore --staged --worktree`. Do NOT re-file. |
| [#2315](https://github.com/sinelaw/fresh/issues/2315) | Review Diff: untracked directories are not expanded — files in a new folder show as a blank, contentless entry and can't be reviewed | Run #35 | **Open** | Flagship 0.4.0 Review Diff. An untracked DIRECTORY renders as `▾ dir/  +0 -0` + a file row with a **completely blank name** (`   ?   +0 -0`, verified byte-for-byte); center shows `(untracked directory)` placeholder, no hunks, `+0/-0` — contained files never listed/reviewable. Contrast: untracked FILE in a tracked dir (`new_feature.py`) shows `+2/-0` + content correctly. Mirrors `git status --short` (collapses `?? dir/`) instead of expanding like VS Code (`-uall`). Contradicts docs ("everything ... untracked in the working tree"). Workaround: `git add` the dir first. Do NOT re-file. |
| [#2314](https://github.com/sinelaw/fresh/issues/2314) | Configurable indentation rules (`[languages.<id>.indent]`) have no effect — all custom patterns ignored | Run #34 | **Open** | 0.4.0 headline feature broken. Custom-language `indent` block parses (language name shows in status bar) but NONE of the 5 patterns fire: `increase_indent_pattern` (end- & start-anchored), `decrease_indent_pattern`, `indent_next_line_pattern`, `dedent_next_line_pattern` all no-op when tested with non-bracket/non-colon tokens (OPEN/CLOSE/HDR/RET) that don't overlap built-in heuristics. Built-in colon/brace indent STILL fires regardless → custom block not consulted. Doc examples (`{`-end / `:`-end) coincidentally match built-in, masking it. Confirmed at both project `.fresh/config.json` and user `~/.config/fresh/config.json`. `self_close_pattern` unobservable (only modifies increase, which never fires). Do NOT re-file. |

| # | Title | Filed | Status | Notes for next run |
|---|-------|-------|--------|-------------------|
| [#2109](https://github.com/sinelaw/fresh/issues/2109) | Ctrl+H doesn't open Find & Replace in terminals (Ctrl+H = Backspace) | Run #1 | **Open** | Terminal sends `0x08`. Verify whether Calibrate Keyboard wizard detects it. Do NOT re-file. |
| [#2111](https://github.com/sinelaw/fresh/issues/2111) | Search: F3 does not navigate to next match while search bar is open | Run #1 | **Open** | Confirmed usability bug: F3 silently ignored while search bar is open. Contradicts VS Code/Sublime/browser behavior. Issue updated with clear expected vs actual. Do NOT re-file. |
| [#2112](https://github.com/sinelaw/fresh/issues/2112) | Search/Replace panel: "No matches found" for files opened outside project workspace | Run #2 | **FIXED** (Run #14) | Fixed by commit b7e7e64. Confirmed fixed in Run #14: /tmp files now appear in Search/Replace panel results. Comment added. |
| [#2113](https://github.com/sinelaw/fresh/issues/2113) | Command palette: keystrokes typed in fuzzy file mode can leak into editor buffer | Run #2 | **CLOSED not_planned** (Run #22) | Closed by maintainer 2026-06-03. Monitoring retired. Do NOT re-file. |
| [#2117](https://github.com/sinelaw/fresh/issues/2117) | Review Diff: "Discard hunk" fails with "patch does not apply" even when patch is valid | Run #5 | **FIXED** (Run #16) | Closed by maintainer. Confirmed fixed in 0.3.10 (Run #16): review_diff_test16.txt +4 lines → discard → "Review Diff: 0 hunks". File reverted to original. Do NOT re-file. |
| [#2125](https://github.com/sinelaw/fresh/issues/2125) | Diagnostics panel keyboard shortcuts (q: close, a: toggle filter, RET: goto) do not work | Run #9 | **CLOSED** (Run #16) | Closed by maintainer. Diagnostics panel 'q' confirmed still fixed in 0.3.10. *Keyboard Shortcuts* 'q' still broken → filed new #2165. Do NOT re-file. |
| [#2135](https://github.com/sinelaw/fresh/issues/2135) | Edit menu "Replace..." label maps to Ctrl+Alt+R (Query Replace), not basic Replace (Ctrl+R) | Run #13 | **Open** | Filed Run #13. Do NOT re-file. |
| [#2165](https://github.com/sinelaw/fresh/issues/2165) | *Keyboard Shortcuts* buffer: pressing 'q' shows 'Editing disabled' | Run #16 | **FIXED** (Run #22) | Closed by maintainer 2026-06-07. CONFIRMED FIXED in v0.3.12 via UI ("Tab closed"). Comment added. Do NOT re-file. |
| [#2197](https://github.com/sinelaw/fresh/issues/2197) | Pyright LSP: all request-based features (hover, definition, completions) timeout after 30s | Run #17 | **Open** | Real pyright on small Python project. Initialize succeeds, all requests timeout. Position encoding mismatch suspected. Do NOT re-file. |
| [#2212](https://github.com/sinelaw/fresh/issues/2212) | Alt+. shows "No code actions available" for diagnostic-based fixes even when clangd reports "(fix available)" | Run #19 | **FIXED** (Run #22) | Closed by maintainer 2026-06-08. CONFIRMED FIXED in v0.3.12 via UI: fix popup appears and applies. Comment added. Do NOT re-file. |
| [#2221](https://github.com/sinelaw/fresh/issues/2221) | SSH URL-style URI (`ssh://host/path`) treated as local file path instead of triggering SSH connection | Run #21 | **Open** | STILL BROKEN in v0.3.12 even with working sshd (Run #22 comment). scp-style works end-to-end. Do NOT re-file. |
| [#2291](https://github.com/sinelaw/fresh/issues/2291) | Workspace Trust: "Trust folder & Allow Tooling" restarts the editor and silently discards opened file + unsaved edits (with --no-restore) | Run #22 | **FIXED** (Run #23) | CONFIRMED FIXED in master @ f4ee3630 (v0.3.12): restart path now restores unsaved buffers from hot-exit recovery (`preserved N unnamed buffer(s)` / `Restored unsaved changes ... from hot exit recovery`). Verified via UI — file survives both Trust and Block-All restarts. Prior Run #23 (08:25Z) already commented; do NOT re-comment. Awaiting maintainer close. Do NOT re-file. |
| [#2307](https://github.com/sinelaw/fresh/issues/2307) | Keybinding Editor: switching keybinding map and back hides all plugin bindings (count drops 866 → 547) until restart | Run #28 | **FIXED** (Run #37) | Closed by maintainer 2026-06-21 (fix commit `4b6e1d2f2` "preserve plugin bindings when rebuilding the resolver"). CONFIRMED FIXED in v0.4.1 via UI: clean default load = 875 / Plugin 392 / Keymap 261; after `default→emacs→default` round-trip editor still shows **875 / Plugin 392** (was 547 / 0). Robust across a 2nd round-trip (`default→macos→default`). Comment added. Do NOT re-file. |
| [#2309](https://github.com/sinelaw/fresh/issues/2309) | Read-only buffers show no `[RO]` status-bar indicator (documented but never rendered) | Run #29 | **Open** | Found while testing the new `editor.auto_read_only` option (Run #29). Docs (editing.md:42 + 0.2.18 blog) promise `[RO]` in status bar; actual binary renders NO persistent RO indicator — only transient `Editing disabled`/`Read-only mode enabled` messages + `[BIN]` tab tag. RO *behavior* is correct; only the indicator is missing. Do NOT re-file. |
| [#2312](https://github.com/sinelaw/fresh/issues/2312) | Occurrence highlight uses a fixed near-black background that ignores the theme (invisible in high-contrast, inverted black box in light) | Run #32 | **Open** | Occurrence highlighting (#2154) is ON by default but its highlight bg is a fixed color 16 that doesn't adapt to the theme. Proven via ON/OFF differential ANSI diff in high-contrast (color 16 == editor bg 16 → invisible on all non-current lines; current-line word drawn recessed). Light theme → inverted black boxes. Works only on dark themes. Do NOT re-file. |
| [#2301](https://github.com/sinelaw/fresh/issues/2301) | Go to LSP Symbol: status bar line number stays stale after jump (only column updates) until next keypress | Run #25 | **Open** | Low-sev display bug in 0.3.12. **Run #27: confirmed NOT LSP-specific** — same staleness affects "Open file from a diff" (OLD-pane Enter → HEAD version: status shows `Ln 1,Col 1` while cursor is on the real line, self-corrects on keypress, 2/2). Comment added to #2301 broadening scope to a shared status-bar-refresh path. Cursor jumps correctly in all cases; only the status readout lags one keypress. Do NOT re-file. |

---

## Closed Issues (agent-filed — do NOT re-open or re-file)

| # | Title | Filed | Why Closed |
|---|-------|-------|-----------|
| [#2108](https://github.com/sinelaw/fresh/issues/2108) | Revert command fails when buffer has unsaved modifications | Run #1 | **False positive.** We triggered "Reload with Encoding..." not "Revert". `File > Revert` works correctly — shows `(r)evert/(c)ancel` prompt. |
| [#2110](https://github.com/sinelaw/fresh/issues/2110) | File opens as modified after session restore | Run #1 | **By design.** This is hot exit (`hot_exit` config, default on). Documented in `docs/features/session-persistence.md`. |

---

## Topics Already Investigated — Do Not Re-file

Even if the symptom looks fresh, these have already been fully investigated:

| Symptom | Conclusion | Issue |
|---------|------------|-------|
| `File > Revert` shows "Cannot reload" error | Wrong menu — that's "Reload with Encoding..." | #2108 closed |
| File opens with `[+]` / `*` on fresh launch | Hot exit restoring previous session | #2110 closed |
| `Ctrl+H` deletes a word | Terminal compat: `0x08` = Backspace | #2109 open |
| F3 does nothing during active search | F3 silently ignored while search bar is open; Enter closes bar first, then F3 works | #2111 open |
| Search/Replace panel returns "No matches found" for /tmp file | FIXED in b7e7e64. /tmp files now found via explicit queuing of out-of-root buffers. | #2112 fixed |
| Characters from command palette file search appear in editor buffer | Focus race condition during `>` → file mode transition | #2113 open |
| Review Diff `d` discard shows "Patch failed: error: patch failed" | Fresh's internal patch application is broken; manual `git apply --reverse` works | #2117 open |
| Pressing `q` in `*Keyboard Shortcuts*` buffer shows "Editing disabled" | Same root cause as #2125 — `[RO]` special buffers don't handle single-key shortcuts | #2125 open (comment added) |
| Edit menu "Replace..." shortcut is Ctrl+Alt+R (Query Replace), not Ctrl+R | Label mismatch — filed as new issue | #2135 open |

---

## How to Use This File Before Filing

1. Describe the symptom you observed in one sentence.
2. Scan the "Topics Already Investigated" table above for a match.
3. Scan the open issues table — if your topic is there, add a comment to the existing issue rather than opening a new one.
4. Search GitHub with at least 3 different query variations.
5. Only then open a new issue and add a row to this file.

## Issue #2312 — Occurrence highlight uses a fixed near-black background that ignores the theme
- **Filed:** Run #32, 2026-06-10
- **URL:** https://github.com/sinelaw/fresh/issues/2312
- **Label:** bug, tui-agent-auto-bug
- **Status:** Open
- **Summary:** Occurrence highlighting (PR #2154, ON by default) draws its highlight using a fixed 256-color index **16** (near-black) that does NOT derive from the active theme. **high-contrast theme:** editor bg is also color 16, so the highlight is invisible — proven by an ON/OFF differential ANSI capture where toggling Occurrence Highlight changes nothing on any non-current line; the only visible effect is the current-line word drawn bg 16 (DARKER than the current-line bg 233 → recessed, not highlighted). **light theme:** occurrences render as inverted solid black boxes (bg 16 on a ~231/254 background). **dark/dracula/nostalgia:** bg 16 on ~234/235 is a subtle box and looks fine (why it went unnoticed). Reference: VS Code uses theme-defined `editor.wordHighlightBackground`/`wordHighlightStrongBackground` so the cue is always visible and theme-appropriate. The *whole-word* matching and toggle behavior are correct — only the color is hard-wired. Reproduces with default config (just switch theme via palette); occurrence highlighting needs no config.
- **Search queries used (no dup):** `occurrence highlight theme color`, `occurrence highlight high-contrast invisible`, `word highlight background color not theme aware`, `highlight_occurrences`

## Issue #2309 — Read-only buffers show no `[RO]` status-bar indicator
- **Filed:** Run #29, 2026-06-10
- **URL:** https://github.com/sinelaw/fresh/issues/2309
- **Label:** bug, tui-agent-auto-bug
- **Status:** Open
- **Summary:** Discovered while testing the brand-new `editor.auto_read_only` config option (commit 9738ac661). Fresh's docs (`docs/features/editing.md:42` and `docs/blog/fresh-0.2.18`) both state read-only buffers show `[RO]` in the status bar. In v0.3.12 (built from origin/master @ 2dee83697) NO persistent `[RO]` indicator is rendered for ANY read-only buffer — library-path auto-RO (`/usr/include/stdio.h`), binary files, or manual Toggle Read-Only Mode. `grep RO` over the full captured screen = 0 matches while a buffer is read-only. The only feedback is transient one-shot status messages (`Editing disabled in this buffer`, `Read-only mode enabled/disabled`) and the `[BIN]` tab tag (which means "binary", not "read-only"). The read-only *behavior* itself is correct (auto-RO triggers, `auto_read_only:false` disables it, binaries stay RO regardless, Toggle works) — only the documented indicator is absent. Usability gap: a user has no standing cue a buffer is read-only until an edit silently fails.
- **Search queries used:** `read-only indicator status bar RO`, `[RO] read only buffer indicator`, `read-only mode no visual indication editing disabled` (all 0 results)

## Issue #2307 — Keybinding Editor hides all plugin bindings after keymap round-trip
- **Filed:** Run #28, 2026-06-10
- **URL:** https://github.com/sinelaw/fresh/issues/2307
- **Label:** bug, tui-agent-auto-bug
- **Status:** Open
- **Summary:** Resolves the long-standing "866 vs 548" Keybinding Editor count anomaly first noted in Run #22 (test_plan priority #8). On a clean `default` keymap the editor lists 866 bindings (Source[Plugin]=391/866, Source[Keymap]=260/866). A SINGLE "Select Keybinding Map" round-trip — `default → emacs → default` — drops the editor to 547 bindings with Source[Plugin]=0/547 (all 391 plugin bindings vanish); Keymap stays 260. 100% reproducible, persists across editor reopens + multi-second wait. Per-map first-load totals are each stable/correct (default 866, emacs 519, macos 600) — bug is only on *return* to an already-loaded map. **Plugin bindings still FUNCTION** (Alt+O = Toggle Orchestrator Dock Focus still opens dock after round-trip), so it is a listing/reporting defect, not loss of functionality. Workaround: restart Fresh.
- **Search queries used:** `keybinding editor binding count`, `Select Keybinding Map plugin bindings disappear`, `keybinding editor 866 547 count drops`, `active_keybinding_map switch plugin` (no duplicate)

## Issue #2291 — Workspace Trust restart discards opened file + unsaved edits
- **Filed:** Run #22, 2026-06-09
- **URL:** https://github.com/sinelaw/fresh/issues/2291
- **Label:** bug, tui-agent-auto-bug
- **Status:** Open
- **Summary:** Confirming "Trust folder & Allow Tooling" in the Workspace Trust dialog performs a full editor restart (`Restart requested with new working directory` in log, even when cwd is unchanged). With `--no-restore`, the CLI-opened file and any unsaved edits are silently discarded — no save prompt, no recovery offer (a recovery chunk IS written under `~/.local/share/fresh/recovery/` but never surfaced). In default mode session restore rebuilds the buffers (incl. unsaved edits), so only the File Explorer auto-open quirk is visible. "Keep Restricted" does not restart. Reference: VS Code keeps open editors and unsaved content when trusting a workspace.
- **Search queries used:** `workspace trust restart` (only #2280, different), `trust folder file closed unsaved`, `security warning dialog buffer lost`, `trust prompt no-restore data loss`, `"Trust folder" OR "Allow Tooling" editor restart`

## Issue #2221 — SSH URL-style URI (`ssh://host/path`) treated as local file path
- **Filed:** Run #21, 2026-06-03
- **URL:** https://github.com/sinelaw/fresh/issues/2221
- **Label:** bug, tui-agent-auto-bug
- **Status:** Open
- **Summary:** When Fresh is launched with `ssh://host/path`, the URI is resolved as a local relative path (CWD + URI string), resulting in an empty local file being opened silently. No SSH connection is attempted, no error is shown. The scp-style form (`user@host:/path`) correctly triggers the SSH code path. Log evidence: `path="/home/user/fresh/ssh://localhost/etc/hosts"`. The `ssh://` form is documented in `docs/features/ssh.md` as the preferred URL-style form.
- **Search queries used:** `repo:sinelaw/fresh ssh:// URI not recognized`, `repo:sinelaw/fresh ssh URL local path remote`, `repo:sinelaw/fresh ssh:// treated as local file path`, `repo:sinelaw/fresh ssh remote editing URL form`, `repo:sinelaw/fresh "ssh://" command line argument`

## Issue #2212 — Alt+. shows "No code actions available" for diagnostic-based fixes
- **Filed:** Run #19, 2026-06-03
- **URL:** https://github.com/sinelaw/fresh/issues/2212
- **Label:** bug, tui-agent-auto-bug
- **Status:** Open
- **Summary:** Fresh always sends `"context":{"diagnostics":[]}` (empty) in every `textDocument/codeAction` request. When clangd publishes diagnostics with "(fix available)" markers, pressing Alt+. shows "No code actions available" instead of the fix popup. Root cause confirmed via LSP log: clangd's `publishDiagnostics` was received correctly, but codeAction request omits them. This is the "TODO: Implement diagnostic retrieval when needed" left from closed issue #1915 — the capability fix in v0.3.6 resolved rust-analyzer WorkspaceEdit actions but did not implement `context.diagnostics` population.
- **Search queries used:** `code actions diagnostics empty no code actions available`, `LSP code action context diagnostics clangd`, `"no code actions available" clangd`, `code action fix available diagnostic context`

## Issue #2165 — *Keyboard Shortcuts* buffer 'q' does not close (persists in 0.3.10)
- **Filed:** Run #16, 2026-05-31
- **URL:** https://github.com/sinelaw/fresh/issues/2165
- **Label:** bug, tui-agent-auto-bug
- **Status:** Open
- **Summary:** The `*Keyboard Shortcuts*` buffer opened via `Shift+F1` documents "Press 'q' to close this buffer." on line 4, but pressing 'q' shows "Editing disabled in this buffer" in 0.3.10. All other special buffers (Diagnostics, Git Blame, Dev Container) correctly handle 'q'. The Diagnostics panel fix in commit 89caf72 used `panelKeys` for that plugin — this buffer uses a different mechanism that was not updated. Parent issue #2125 closed without fixing this.
- **Search queries used:** `keyboard shortcuts q editing disabled`, `keyboard shortcuts buffer close`, `Shift+F1 q close`

## Issue #2122 — move_to_paragraph_down/up Has No Default Keybinding (0.3.9 oversight)
- **Filed:** Run #7, 2026-05-26
- **URL:** https://github.com/sinelaw/fresh/issues/2122
- **Label:** bug, tui-agent-auto-bug
- **Status:** Open
- **Summary:** The `move_to_paragraph_down` and `move_to_paragraph_up` actions added in 0.3.9 (PR #2084) have no default keybinding and are not accessible from the command palette. The sibling `select_to_paragraph_*` actions have `Ctrl+Shift+↓/↑` bindings. Users cannot use the new "jump to paragraph" feature without manually binding it.
- **Search queries used:** `move_to_paragraph keybinding`, `paragraph navigation keybinding`, `paragraph action command palette missing`
