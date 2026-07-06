# Confirmed Bugs Registry

## Format
Each bug entry:
- **ID:** BUG-NNN
- **Title:** Short description
- **Severity:** Critical / High / Medium / Low
- **Status:** Open / Fixed / Closed
- **GitHub Issue:** #NNN (if filed)
- **Reproduction Steps:** (tmux send-keys sequence)
- **Expected:** What should happen
- **Actual:** What happened (from tmux capture-pane)
- **First Seen:** Date of first occurrence

---

## BUG-017: vi mode — indent operators `>>`/`<<` and visual-mode `>`/`<` are no-ops
- **ID:** BUG-017
- **Title:** In vi mode, the indent/dedent operators `>>` and `<<` (NORMAL) and `>`/`<` (Visual) do nothing — no indentation change, no status feedback.
- **Severity:** Medium (core Vim editing command silently unhandled; directly in scope of the "align vi compatibility motions with Vim" work).
- **Status:** Open — GitHub #2438 filed (Run #43).
- **GitHub Issue:** [#2438](https://github.com/sinelaw/fresh/issues/2438)
- **Reproduction:** File with `hello world foo bar` and an indented line `    indented line here`. `Ctrl+P`→"Toggle Vi mode"→Enter. NORMAL: `gg` `0`, then `>` then `>` (separate send-keys ~0.4s apart) — line unchanged. On the indented line: `<` `<` — 4 leading spaces remain. Visual: `V` (status `-- VISUAL LINE --`) then `>` — line unchanged AND mode stays `-- VISUAL LINE --`.
- **Expected (Vim):** `>>` indents the line one shiftwidth; `<<` dedents; Visual `>`/`<` indent/dedent selection and return to NORMAL.
- **Actual:** All silent no-ops. `>` key transmits fine (typing `>` in INSERT inserts a literal `>`), so it's the operator handling, not input. No error/status message.
- **First Seen:** Run #43, 2026-06-22 (v0.4.1, master @ 8ee2baf31).

## BUG-018: vi mode — quote text-objects `i"`/`a"` don't search forward on the line
- **ID:** BUG-018
- **Title:** `di"`/`ci"` (and `a"`) only operate when the cursor is already INSIDE the quotes; from before the quote on the same line they are no-ops, unlike Vim which searches forward on the current line.
- **Severity:** Medium (the common case — `ci"` from a line start — silently fails; in scope of the vi-compat motion work).
- **Status:** Open — GitHub #2439 filed (Run #43).
- **GitHub Issue:** [#2439](https://github.com/sinelaw/fresh/issues/2439)
- **Reproduction:** File with `the "quick" brown fox`. Enable vi mode. `gg` `j` `0` (col 1, before the quote). Send `d` `i` `"` separately (~0.4s apart). Capture the line.
- **Expected (Vim):** `the "" brown fox` — `i"` searches forward on the line; `ci"` enters INSERT.
- **Actual:** Line unchanged (no-op, 2/2). WORKS only with cursor inside: on `q`, `di"` → `the "" brown fox`; `ci"` → INSERT `the "WXYZ" brown fox`. `"` key transmits fine (verified in INSERT).
- **First Seen:** Run #43, 2026-06-22 (v0.4.1, master @ 8ee2baf31).
- **tmux note:** Quote text-objects need keys sent separately; `"` transmits literally in INSERT (`one` → `one"`). diw/daw/3dw/2dd all PASS (Vim-correct) and are NOT bugs — only the forward-search rule for `i"`/`a"` is missing.

## BUG-016: vi mode — `cw` deletes the trailing whitespace instead of acting like `ce` (Vim special case)
- **ID:** BUG-016
- **Title:** In vi mode, `cw` behaves like `dw`+insert (eats the trailing space) instead of like `ce` (change to end of word, keep the space) as Vim does on a non-blank.
- **Severity:** Medium (behavioral Vim-compat deviation a Vim user hits constantly; directly in scope of the new "align vi compatibility motions with Vim" commit).
- **Status:** Open — GitHub #2437 filed (Run #42).
- **GitHub Issue:** [#2437](https://github.com/sinelaw/fresh/issues/2437)
- **Reproduction:** File with line `hello world foo bar`. `Ctrl+P`→"Toggle Vi mode"→Enter. `gg` `0` (cursor on col 1 `h`). Send `c` then `w` (separate send-keys, ~0.4s apart), then `X`, then `Escape`. Capture line 3 via `capture-pane -p | sed 's/.*│ //'`.
- **Expected (Vim):** `X world foo bar` — `cw`==`ce` on a non-blank; trailing space preserved (`:help cw`).
- **Actual:** `Xworld foo bar` — trailing space consumed. Contrast: `ce`+`X` → `X world foo bar` (correct). Reproduced 2/2 at word start; mid-word (`0` `l` `l`, col 3) `cw`+`X` → `heXworld foo bar` (Vim: `heX world foo bar`). Control: `dw` from col 1 → `world foo bar` (correctly eats space — only `cw` is wrong).
- **First Seen:** Run #42, 2026-06-22 (v0.4.1, master @ eb3a349e6).
- **tmux note:** Operator+motion is timing-sensitive over tmux — sending `de` as one combined arg was a NO-OP; sending `d` then `e` separately over-deleted. Verify motions via BUFFER-TEXT effect (operator + insert marker), NOT cursor-position polling (`display-message #{cursor_x}` lagged/duplicated readings). `cw`/`ce` results were the only fully stable ones (2/2) and are what the issue rests on.

## BUG-015: Review Diff — line-level discard shows raw i18n key `status.lines_discardd` (misspelled) instead of a localized message
- **ID:** BUG-015
- **Title:** After the #2317 fix, line-level **discard** works but its status-bar success message prints the literal lookup key `status.lines_discardd` (typo: "discardd", double-d). Sibling op **stage** shows proper `Lines staged`.
- **Severity:** Low (cosmetic — discard itself git-verified correct; only the feedback string leaks an internal key).
- **Status:** Open — GitHub #2420 filed (Run #38).
- **GitHub Issue:** [#2420](https://github.com/sinelaw/fresh/issues/2420)
- **Reproduction:** Real git repo, single-line modification. Review Diff → cursor on `-` line (ANSI-verify) → `v` → `j` (extend over the paired `+`) → `d`.
- **Expected:** Localized confirmation consistent with stage (`Lines staged`), e.g. `Lines discarded`.
- **Actual:** Status bar shows literal `status.lines_discardd`. Deterministic across 2+ discards.
- **First Seen:** Run #38, 2026-06-22 (v0.4.1, master @ 205b9640e).

## BUG-013: Review Diff — line-level visual stage/unstage/discard (`v` then `s`/`u`/`d`) never works
- **ID:** BUG-013
- **Title:** The advertised line-level visual-selection staging (`v` then `s`/`u`/`d`) in Review Diff does nothing for any of the three ops, even with the cursor on a real +/- line.
- **Severity:** Medium (a documented, help-bar-promoted feature is fully non-functional; hunk/file ops still work as a workaround).
- **Status:** **FIXED** (Run #38) — maintainer closed 2026-06-22, fix commit `a1d3e4352`. CONFIRMED via UI in v0.4.1 @ 205b9640e: `v`+`s` stages a single line (git diff --cached shows only it), `v`+`u` unstages a single staged line, `v`+`d` discards a pure addition and (full `-`/`+` pair selected) a modification. All git-verified. Commented on #2317.
- **GitHub Issue:** [#2317](https://github.com/sinelaw/fresh/issues/2317)
- **Reproduction:** Real git repo w/ a one-line change. Review Diff → cursor on the `+`/`-` line (ANSI-verify highlight) → `v` (status `Visual: j/k extend, s/u/d apply`) → `s`.
- **Expected:** The selected line is staged (git_add_-p line granularity / VS Code "Stage Selected Ranges").
- **Actual (at filing, v0.4.0):** `v`+`s` and `v`+`d` → `Selection has no add/remove lines or crosses hunk boundary` (no-op); `v`+`u` → `Patch failed: … patch does not apply`. Now fixed in v0.4.1.
- **First Seen:** Run #36, 2026-06-11 (v0.4.0, master @ 1b5d7f8c8). **Fixed:** v0.4.1 (Run #38).

## BUG-014: Review Diff — file-level Discard (`D`) reports "Discarded" but leaves staged changes intact
- **ID:** BUG-014
- **Title:** `D` "Discard changes in file" only reverts the working tree; on a fully-staged file it is a no-op yet reports success after a "this cannot be undone" dialog.
- **Severity:** Medium (misleading destructive-action feedback; user believes staged changes are gone when they persist — opposite of data loss, but confusing/incorrect).
- **Status:** Open — GitHub #2318 filed (Run #36).
- **GitHub Issue:** [#2318](https://github.com/sinelaw/fresh/issues/2318)
- **Reproduction:** Stage a one-line change (`git add file` → `M  file`, working tree clean). Review Diff → STAGED group → cursor on the hunk content row → `D` → dialog "Discard changes in file / Permanently lose changes / This cannot be undone" → select Discard → Enter.
- **Expected:** File reverts to HEAD (losing the staged change), OR the UI states it won't touch staged changes and does NOT report "Discarded".
- **Actual:** Status `Discarded: <file>` but `git status` still `M  file`, `git diff --cached` unchanged, file on disk unchanged. Contrast: on an UNSTAGED file (` M file`) `D` correctly reverts to HEAD. So `D` only ever touches the working tree, never the index. (`D` also only fires when cursor is on a hunk content row.)
- **First Seen:** Run #36, 2026-06-11 (v0.4.0, master @ 1b5d7f8c8).

## BUG-010: Read-only buffers show no `[RO]` status-bar indicator (documented but never rendered)
- **ID:** BUG-010
- **Title:** Read-only buffers (auto library-path, binary, or manual toggle) display no persistent `[RO]` indicator anywhere, contradicting Fresh's own docs.
- **Severity:** Low–Medium (read-only behavior is correct; the documented visual cue is missing, so users only learn a buffer is read-only by failing an edit)
- **Status:** Open — GitHub #2309 filed (Run #29). Found while testing the new `editor.auto_read_only` option.
- **GitHub Issue:** [#2309](https://github.com/sinelaw/fresh/issues/2309)
- **Reproduction:**
  1. `fresh /usr/include/stdio.h` (library path → auto read-only).
  2. Move cursor (`Down Down Right`) to clear the transient message; resting status bar = ` Trusted  Local  Ln 3, Col 2 … LF ASCII C LSP (off) Palette: Ctrl+P` — no `[RO]`.
  3. Type any key → flashes `Editing disabled in this buffer` (only RO feedback).
  4. Also: binary file → `[BIN]` tab tag + editing blocked but no `[RO]`; manual palette "Toggle Read-Only Mode" ON → `Read-only mode enabled` flash, then resting bar still has no `[RO]`.
- **Expected:** Persistent `[RO]` status-bar segment per `docs/features/editing.md:42` ("The status bar shows `[RO]`") and 0.2.18 blog; matches VS Code/Sublime read-only affordance.
- **Actual:** No persistent RO indicator on screen (`grep RO` full screen = 0 while RO). Only transient messages + `[BIN]` tag.
- **First Seen:** Run #29, 2026-06-10 (v0.3.12, origin/master @ 2dee83697).

## BUG-009: Keybinding Editor — switching keymap and back hides ALL plugin bindings (count 866 → 547)
- **ID:** BUG-009
- **Title:** After "Select Keybinding Map" round-trips back to the same map, the Keybinding Editor drops every plugin-contributed binding (count falls from 866 to 547); persists until app restart.
- **Severity:** Medium (the editor — whose whole job is to list bindings — under-reports by ~319; plugin shortcuts still FUNCTION, but they vanish from the list so a user thinks they're gone)
- **Status:** Open — GitHub #2307 filed (Run #28). **Resolves the long-standing "866 vs 548" anomaly from Run #22 (priority #8).**
- **GitHub Issue:** [#2307](https://github.com/sinelaw/fresh/issues/2307)
- **Reproduction:**
  1. `rm -f ~/.config/fresh/config.json` then `fresh --no-restore` (clean → `default` map active)
  2. Open Keybinding Editor (Edit menu → "Keybinding Editor..." or palette "Open Keybinding Editor"): header `Source: [All]  866 bindings`; press `s` to cycle Source → `[Plugin]` = `391/866 shown`, `[Keymap]` = `260/866`. Esc.
  3. Palette → "Select Keybinding Map" → `emacs` ("Switched to 'emacs' keybindings")
  4. Palette (emacs palette = `M-x`) → "Select Keybinding Map" → `default` ("Switched to 'default' keybindings")
  5. Reopen Keybinding Editor.
- **Expected:** Returning to `default` shows the full 866 again (incl. all 391 plugin bindings) — switching maps is reversible/non-destructive (VS Code Keyboard Shortcuts editor always reflects the full current set).
- **Actual:** Editor now shows `547 bindings`; Source `[Plugin]` = `0/547 shown` (all 391 plugin bindings gone); `[Keymap]` still 260. 100% reproducible with a SINGLE round-trip; persists across reopens + multi-second wait. Per-map first-load totals are each stable/correct (default 866, emacs 519, macos 600) — bug is only on *return* to an already-loaded map.
- **Functional check:** Plugin bindings still WORK after the round-trip — Alt+O (Toggle Orchestrator Dock Focus, a plugin binding) still opens the dock. So this is a Keybinding-Editor listing/reporting defect, not loss of functionality.
- **Workaround:** Restart Fresh (fresh launch on `default` lists 866 again). Don't switch keymaps mid-session if you need the editor to show plugin bindings.
- **First Seen:** Run #28, 2026-06-10 (v0.3.12 @ 67d0c6e6c from master); minimal repro 100%.

---

## BUG-008: Go to LSP Symbol — Status Bar Line Number Stale After Jump
- **ID:** BUG-008
- **Title:** After "Go to LSP Symbol" Enter-jump, status bar `Ln` keeps the pre-jump line; only `Col` updates. Self-corrects on next cursor move.
- **Severity:** Low (cursor/editing correct; status-bar display glitch that clears on any keystroke)
- **Status:** Open — GitHub #2301 filed (Run #25)
- **GitHub Issue:** [#2301](https://github.com/sinelaw/fresh/issues/2301)
- **Reproduction:**
  1. C file + clangd running (Trusted, `LSP (on)`), e.g. `main` on line 44
  2. `Ctrl+G` → `30` → Enter (status `Ln 30, Col 1`)
  3. `Ctrl+P` → "Go to LSP Symbol" → Enter → type `main` → Enter
  4. Read status bar immediately vs actual cursor (`tmux display-message -p '#{cursor_y}'`)
- **Expected:** `Ln 44, Col 5` (VS Code/Sublime update status immediately; Fresh's own F12 & Ctrl+G do too)
- **Actual:** `Ln 30, Col 5` — line stale (pre-jump value), column correct; corrects to `Ln 44` on next `→`/`End`
- **Scope:** Feature-specific — F12 Go to Definition and Ctrl+G Go to Line both refresh `Ln` immediately
- **First Seen:** Run #25, 2026-06-10 (v0.3.12 @ f4ee3630); 3/3 reproducible

---

## BUG-007: Workspace Trust Confirm Restarts Editor, Discarding Open File + Unsaved Edits (--no-restore)
- **ID:** BUG-007
- **Title:** "Trust folder & Allow Tooling" → full editor restart → CLI file and unsaved edits silently lost when launched with `--no-restore`
- **Severity:** High (silent data loss; no prompt, no recovery offer)
- **Status:** FIXED (Run #23, confirmed in master @ f4ee3630 / v0.3.12) — restart path now restores unsaved buffers from hot-exit recovery; file survives Trust and Block-All restarts. GitHub #2291 awaiting maintainer close (prior Run #23 comment posted).
- **GitHub Issue:** [#2291](https://github.com/sinelaw/fresh/issues/2291)
- **Reproduction:**
  1. Folder with `compile_commands.json` (trust trigger); ensure no trust.json recorded
  2. `fresh --no-restore main.cpp` → SECURITY WARNING dialog
  3. (Variant) Keep Restricted → type into buffer (modified) → palette "Workspace Trust…"
  4. Select "Trust folder & Allow Tooling (T)", press Enter
- **Expected:** Open editors and unsaved content preserved (VS Code behavior)
- **Actual:** Editor restarts; main.cpp tab replaced by empty [No Name] + File Explorer; unsaved edits destroyed with no prompt. Recovery chunk written but never offered on reopen.
- **Log:** `INFO fresh::app::lifecycle: Restart requested with new working directory: <same cwd>`
- **Notes:** Default mode (session restore) rebuilds buffers incl. unsaved edits — bug is --no-restore specific. "Keep Restricted" does NOT restart.
- **First Seen:** Run #22, 2026-06-09 (v0.3.12); 3/3 reproducible

---

## BUG-006: SSH URL-style URI (`ssh://host/path`) Treated as Local File Path
- **ID:** BUG-006
- **Title:** `ssh://host/path` CLI argument silently opens empty local file instead of SSH connection
- **Severity:** High (documented feature not working; no error shown to user)
- **Status:** Open — GitHub issue #2221 filed (Run #21)
- **GitHub Issue:** [#2221](https://github.com/sinelaw/fresh/issues/2221) — filed Run #21 (2026-06-03)
- **Reproduction:**
  1. Launch Fresh with URL-style SSH URI: `fresh --no-restore "ssh://localhost/etc/hosts"`
  2. Observe: Tab opens titled "hosts", status bar shows "Local | ssh://localhost/etc/hosts", buffer is empty
  3. Check logs: `path="/home/user/fresh/ssh://localhost/etc/hosts"` — treated as relative local path
- **Expected:** Fresh connects via SSH per docs/features/ssh.md; status bar shows `[SSH:localhost]`
- **Actual:** Fresh treats URI as local relative path (CWD + URI). No connection, no error, empty file opened.
- **Contrast:** scp-style form (`user@host:/path`) correctly detects SSH and shows "Connecting via SSH to..."
- **First Seen:** Run #21, 2026-06-03

---

## BUG-001 (FIXED): *Keyboard Shortcuts* Buffer 'q' Does Not Close
- **ID:** BUG-001
- **Title:** `*Keyboard Shortcuts*` buffer 'q' does not close despite in-buffer documentation
- **Severity:** Low (Documentation/UX)
- **Status:** **FIXED** in v0.3.12 — confirmed via UI Run #22 ("Tab closed"); #2165 closed by maintainer 2026-06-07
- **GitHub Issue:** [#2165](https://github.com/sinelaw/fresh/issues/2165) — filed Run #16 (2026-05-31)
- **Reproduction:**
  1. Launch Fresh with `--no-restore`
  2. Press `Shift+F1` — `*Keyboard Shortcuts*` buffer opens
  3. Line 4 reads: "Press 'q' to close this buffer."
  4. Press `q`
  5. `tmux capture-pane -t SESSION -p | tail -3`
- **Expected:** Buffer closes
- **Actual:** Status bar shows "Editing disabled in this buffer"; buffer stays open
- **Workaround:** Use `Alt+W` to close
- **First Seen:** Run #12, 2026-05-27
- **Confirmed:** Run #14 (0.3.9), Run #15 (0.3.9), Run #16 (0.3.10)

## BUG-003 (FIXED): Review Diff "Discard hunk" Fails with "patch does not apply"
- **ID:** BUG-003
- **Title:** Review Diff "Discard hunk" fails with "Patch failed: error: patch does not apply"
- **Severity:** High (feature broken)
- **Status:** **FIXED** in 0.3.10 (Run #16, 2026-05-31)
- **GitHub Issue:** [#2117](https://github.com/sinelaw/fresh/issues/2117) — closed by maintainer
- **First Seen:** Run #5
- **Confirmed Fixed:** Run #16 — review_diff_test16.txt +4 lines, discard → "Review Diff: 0 hunks", file reverted to original state

---

## BUG-002: Edit Menu "Replace..." Label Maps to Query Replace (Ctrl+Alt+R), Not Basic Replace (Ctrl+R)
- **ID:** BUG-002
- **Title:** Edit menu mislabels "Query Replace" as "Replace..."
- **Severity:** Low (Documentation/UX)
- **Status:** Open
- **GitHub Issue:** [#2135](https://github.com/sinelaw/fresh/issues/2135) — filed in Run #13
- **Reproduction:**
  1. Launch Fresh: `fresh /tmp/any-file.txt`
  2. Press `F10` → navigate Right to Edit menu
  3. Find "Replace..." item — note shortcut: `Ctrl+Alt+R`
  4. Press Escape, open Command Palette (`Ctrl+P`), search "replace"
  5. Observe: "Replace" = `Ctrl+R` (basic); "Query Replace" = `Ctrl+Alt+R` (interactive)
- **Expected:** Edit menu "Replace..." should use `Ctrl+R` OR be labeled "Query Replace..."
- **Actual:** "Replace..." in Edit menu maps to `Ctrl+Alt+R` which is Query Replace (interactive). Basic Replace (`Ctrl+R`) has no Edit menu entry.
- **First Seen:** Run #12, 2026-05-27
- **Confirmed:** Run #13, 2026-05-27

---

## BUG-005 (FIXED): LSP Code Actions (Alt+.) Always Report "No Code Actions Available" for Diagnostic-Based Fixes
- **ID:** BUG-005
- **Title:** Alt+. code actions silently fail for clangd-reported "fix available" diagnostics due to empty `context.diagnostics`
- **Severity:** High (feature non-functional for all diagnostic-based fixes)
- **Status:** **FIXED** in v0.3.12 — confirmed via UI Run #22 (fix popup appears and applies); #2212 closed by maintainer 2026-06-08
- **GitHub Issue:** [#2212](https://github.com/sinelaw/fresh/issues/2212) — filed Run #19 (2026-06-03)
- **Reproduction:**
  1. Install clangd; configure `{"lsp": {"cpp": {"command": "clangd", "enabled": true}}}`
  2. Create `main.cpp` with `#include <string>` (unused) and `int z; return z;` (uninit)
  3. Launch Fresh, start clangd via LSP Status menu
  4. Wait for "LSP (cpp) ready"; open Diagnostics panel
  5. Observe `[W] 2:1 Included header string is not used directly **(fixes available)**`
  6. Navigate cursor to line 2, col 1; press `Alt+.`
  7. Status bar shows: **"No code actions available"**
- **Expected:** Code action popup with "Remove unused include" fix
- **Actual:** "No code actions available" — clangd returns empty `[]` because Fresh sends `"context":{"diagnostics":[]}` (empty) in every codeAction request
- **Evidence from LSP log:**
  - Fresh RECEIVED: `publishDiagnostics` with 7 diagnostics including "(fix available)" markers
  - Fresh SENT: `codeAction` with `"context":{"diagnostics":[]}` (always empty)
  - clangd replied: `"result":[]`
- **Root cause:** `context.diagnostics` in `textDocument/codeAction` is always empty — the "TODO: Implement diagnostic retrieval when needed" from source comment is not yet implemented
- **Workaround:** None — Alt+. does not provide diagnostic-based fixes
- **First Seen:** Run #18 (inconclusive), Run #19 (confirmed)
- **Confirmed:** Run #19, 2026-06-03

## BUG-004: Pyright LSP — All Request-Based Features Timeout After 30s
- **ID:** BUG-004
- **Title:** Pyright LSP: hover, definition, completions, signatureHelp all timeout; diagnostics not published
- **Severity:** High (major feature non-functional with real LSP)
- **Status:** Open
- **GitHub Issue:** [#2197](https://github.com/sinelaw/fresh/issues/2197) — filed in Run #17
- **Reproduction:**
  1. Install pyright: `pip install pyright`
  2. Config: `{"lsp": {"python": {"command": "pyright-langserver", "args": ["--stdio"], "enabled": true}}}`
  3. Create small Python project in /tmp with main.py
  4. Launch Fresh from that directory: `fresh --no-restore main.py`
  5. Wait for "LSP (python) ready" in status bar
  6. Try F12 (definition), Alt+K (hover), Ctrl+Space (completion) — all timeout after 30s
- **Expected:** Standard LSP features work (definition, hover, completion, diagnostics)
- **Actual:** Initialize succeeds ("Async LSP server initialized successfully") but ALL subsequent requests timeout. Diagnostics panel shows 0 items despite `[⚠ N]` counter (which counts timeout warnings, not code diagnostics).
- **Hint:** Log shows `LSP initialize result: position_encoding=None` — possible UTF-16 encoding mismatch causing pyright to discard all requests silently.
- **First Seen:** Run #17, 2026-06-02
- **Confirmed:** Run #17, 2026-06-02 (10/10 requests timed out across hover, definition, completion, signatureHelp)

---

## BUG-011: Configurable indentation rules `[languages.<id>.indent]` have no effect
- **ID:** BUG-011
- **Title:** Custom per-language auto-indent rules (0.4.0 headline) are completely ignored; all 5 patterns no-op while built-in heuristics run regardless.
- **Severity:** Medium (documented headline feature non-functional; affects anyone configuring indent for a custom/unrecognized language)
- **Status:** Open — GitHub #2314 filed (Run #34).
- **GitHub Issue:** [#2314](https://github.com/sinelaw/fresh/issues/2314)
- **Reproduction:**
  1. Project `.fresh/config.json`: `{"languages":{"incend":{"extensions":["t1"],"tab_size":4,"use_tabs":false,"indent":{"increase_indent_pattern":"OPEN\\s*$"}}}}`
  2. In the tmux shell: `cd /tmp/indent-test`, then `fresh --no-restore test.t1` (status bar confirms filetype `incend`).
  3. `send-keys -l 'foo OPEN'`, `Enter`, `send-keys -l 'child'`.
- **Expected:** new line indented one level (`    child`) — per docs (VS Code `increaseIndentPattern` parity).
- **Actual:** `│ 2 │ child` at column 0 — no indent. Same negative result for `decrease_indent_pattern`, `indent_next_line_pattern`, `dedent_next_line_pattern` (and `increase` both end- and start-anchored), tested with non-bracket/non-colon tokens (OPEN/CLOSE/HDR/RET). Built-in colon/brace indent STILL fires for the same custom languages → custom block not consulted. Confirmed at project AND user config layers.
- **First Seen:** 2026-06-11 (Run #34), fresh 0.4.0 @ 1b5d7f8c8.

---

## BUG-012: Review Diff does not expand untracked directories — files inside a new folder are unreviewable
- **ID:** BUG-012
- **Title:** Untracked directories appear in Review Diff as a single blank/nameless `?` entry with `+0/-0` and no content; the new files inside cannot be listed or reviewed.
- **Severity:** Medium (functional gap in flagship 0.4.0 Review Diff; new files in a new folder silently omitted from review, contradicting docs "everything ... untracked in the working tree")
- **Status:** Open — GitHub #2315 filed (Run #35).
- **GitHub Issue:** [#2315](https://github.com/sinelaw/fresh/issues/2315)
- **Reproduction:**
  1. In a git repo with some tracked changes, create a brand-new directory of files: `mkdir assets && echo data > assets/logo.txt && echo icon > assets/icon.txt` (`git status --short` → `?? assets/`).
  2. `fresh --no-restore`; Command Palette → **Review Diff**; press `r` to refresh.
  3. Look at the UNTRACKED group for `assets/`.
- **Expected:** Each untracked file inside the new directory is listed and reviewable (additions shown), like VS Code SCM (untracked=all) and like a top-level untracked FILE in Fresh (`src/core/new_feature.py` correctly shows `+2/-0` with its added lines). Docs/features/git.md: Review Diff shows "everything staged, unstaged, and untracked in the working tree right now."
- **Actual:** Sidebar shows `▾ assets/  +0 -0` group header then a file row with a **completely blank name**: `   ?   +0 -0`. Center pane shows `▾ assets/  +0 / -0` with no hunks and a `(untracked directory)` placeholder (revealed by `zr` unfold-all). The contained files (`assets/logo.txt`, `assets/icon.txt`) are never listed and have no diff/additions. `s`/Enter/Alt+o on the entry do nothing useful. Contrast: an untracked file in a TRACKED dir (`src/core/new_feature.py`) renders correctly with name + `+2/-0` + content.
- **First Seen:** 2026-06-11 (Run #35), fresh 0.4.0 @ 1b5d7f8c8.
- **Confirmed:** Run #35 — reproduced with two independent untracked dirs (`.review/` export artifact and a clean `assets/`); blank name verified byte-for-byte via `capture-pane | cat -A`.

---

## BUG-019: vi mode — find-char motions broken with operators; `;`/`,` repeat no-op
- **ID:** BUG-019
- **Title:** Operator + `f`/`t` (`df`/`dt`/`cf`/`ct`) hangs in operator-pending or silently mis-deletes; `;`/`,` repeat-find does nothing.
- **Severity:** Medium (very common Vim ops; one path HANGS the editor requiring Esc, another silently deletes the wrong span).
- **Status:** Open — GitHub #2441 filed (Run #44).
- **GitHub Issue:** [#2441](https://github.com/sinelaw/fresh/issues/2441)
- **Reproduction:** vi mode on `hello world foo bar baz`, cursor col 1.
  - (A) `d` `f` `r` → stuck `-- OPERATOR (d) --` forever (target + even `Z` swallowed; Esc to recover). Same for `dt,`, `cfr`. Held at 0.4s AND 1.2s key gaps → not a timing race.
  - (B) `d` `f` `w` → `world foo bar baz` (= plain `dw`; `f` dropped, `w` ran as word-motion). Vim expects `orld foo bar baz`.
  - (C) `f` `o` (→col5) then `;` → no move; `x` deletes col5 `o` (`hell world...`). `,` also no-op.
- **Expected (Vim):** `dfr`→`ld foo bar baz`; `dfw`→`orld foo bar baz`; `;` advances to next `o` (col 8).
- **Control (works):** pure `f`/`t` cursor motions land correctly (`fr`+`x` deletes `r` of world; `t,`+`x` deletes char before comma).
- **First Seen:** 2026-06-22 (Run #44), fresh 0.4.1 @ 3b8c2eca1.

---

## BUG-020: vi mode — `j`/`k` onto a shorter line parks cursor past EOL; `x` joins lines
- **ID:** BUG-020
- **Title:** Vertical motion onto a shorter line lands the cursor one column past the last char (col len+1); `x` then deletes the newline and joins the next line.
- **Severity:** Medium (silent corruption from ordinary navigate-then-delete; off-by-one vs correct horizontal clamp).
- **Status:** Open — GitHub #2442 filed (Run #44).
- **GitHub Issue:** [#2442](https://github.com/sinelaw/fresh/issues/2442)
- **Reproduction:** vi mode, lines `hello world foo bar baz` / `short` / `a longer line with many words here`.
  1. Line 1 `$` (col 23, correct) → `j` onto `short` (5 chars) → status `Ln 2, Col 6` (past EOL).
  2. `x` → deletes the newline → `shorta longer line with many words here` (lines joined).
  - Also via `l`-to-col10 + `j`, and `k` from line 3 (3/3).
- **Expected (Vim):** cursor clamps to last char (`t`, col 5); `x` → `shor`; `x` never joins lines in NORMAL.
- **Control (works):** `$` direct on `short` → col 5 → `x` = `shor`; `l` stops at last char. Only vertical clamp is off-by-one.
- **First Seen:** 2026-06-22 (Run #44), fresh 0.4.1 @ 3b8c2eca1.

## BUG-021: vi mode — dot-repeat (`.`) of `o`/`O`/`a`/`A` corrupts the buffer
- **ID:** BUG-021
- **Title:** `.` (repeat last change) of cursor-repositioning insert commands (`o`/`O`/`a`/`A`) injects unrelated line content instead of replaying the typed text — silent buffer corruption. `i`/`x` dot-repeat are correct.
- **Severity:** Medium (data corruption from a very common `o`/`a` + `.` Vim workflow).
- **Status:** Open — GitHub #2443 filed (Run #45).
- **GitHub Issue:** [#2443](https://github.com/sinelaw/fresh/issues/2443)
- **Reproduction:** vi mode, file `hello world foo bar baz` / `short`.
  1. Cursor on line 1, press `o` then type `abc` then `Esc` → line 2 = `abc` (correct).
  2. Press `.` → buffer corrupts: line 2 = `abhello world foo bar baz`, line 3 = `abcc` (expected line 3 = `abc`). Line 1's content was injected.
  - Also: `A`+`X`+`Esc` on `short` = `shortX`; `.` → `shortshortXX` (expected `shortXX`). `A`+`QZQ`+`Esc` → `shortQZQ`; `.` → `shortQZshortQZQQ` (expected `shortQZQQZQ`). `a`+`Y`+`Esc` at col1 = `sYhort`; `.` → `ssYYhort` (expected `sYYhort`).
- **Control (works):** `i`+`AB`+`Esc` → `ABalpha...`; `.` → `AABBalpha...` (correct). `x`,`.`,`.` deletes successive chars (correct). So defect is specific to the cursor-repositioning insert commands.
- **Expected (Vim, `:help .`):** `.` replays exactly the keystrokes of the last change.
- **First Seen:** 2026-06-22 (Run #45), fresh 0.4.1 @ 1c6bd8ce9.

---

## BUG-022: Virtual space — status-bar Ln/Col indicator freezes at the line-end position while cursor is in virtual space
- **ID:** BUG-022
- **Title:** With `editor.virtual_space` on, the status-bar `Ln, Col` readout freezes at the last real-text position while the cursor moves through virtual space (horizontal past EOL → `Col` frozen; vertical below last line → both `Ln` and `Col` frozen). The cursor really moves; only the indicator is stale until the cursor returns to real text.
- **Severity:** Low–Medium (display/usability; the position indicator is wrong for the whole time in the mode whose purpose is free positioning).
- **Status:** Open — GitHub #2577 filed (Run #46).
- **GitHub Issue:** [#2577](https://github.com/sinelaw/fresh/issues/2577)
- **Reproduction:** fixture with a short line 2 (`short`), `fresh --no-restore vspace.txt`.
  1. `Ctrl+P` → "Toggle Virtual Space (Current Buffer)" → `Virtual space: on (this buffer)`.
  2. On `short`, press End → `Col 6`. Press Right ×3.
  3. Status stays `Col 6` (frozen); real cursor is at col 9 (verified `tmux display-message -p '#{cursor_x}'` = screen col 14 = text col 9; and typing `X` there lands at col 9 with the gap space-filled).
  - Vertical: ArrowDown below the last line parks the cursor on a virtual line but the readout stays `Ln 5, Col 10`; typing materializes the newlines + leading spaces, confirming the caret moved.
- **Expected (Visual Studio — the feature's stated model):** the Ln/Col indicator reports the virtual position (e.g. `Col 9`).
- **Distinct from #2301** (post-jump `Ln` staleness): different trigger (continuous per-keystroke virtual movement) and symptom (both Ln+Col frozen for the whole duration); possibly a shared status-bar-refresh path.
- **First Seen:** 2026-07-06 (Run #46), fresh 0.4.3 @ 9f6135001.

---

## PENDING (not filed) — vi mode missing standard commands (→ IMP-023)
- `R` (Replace/overtype mode): unrecognized — stays `-- NORMAL --`, next key runs as a normal command (`R`+`A` fired append-at-EOL).
- `gU`/`gu`/`g~` case OPERATORS: no-ops (`gUw` left text unchanged, `w` only moved cursor). NB single-char `~` DOES work.
- Candidate for one consolidated "vi missing standard commands" issue once more are characterized (count them with `;`/`,` from #2441). Logged here + potential_improvements; NOT individually filed (missing-feature, lower sev than the broken-behavior bugs above).
- **Run #45 update:** swept the rest of the common command set — the gap list is SMALL. CONFIRMED WORKING (Vim-correct, do NOT re-test): `o`/`O` (open line), `s`/`S` (substitute char/line), `D`/`C`/`Y` (operate to EOL; `Y` is linewise like Vim), `3G`/count+`G`, `*`/`#` (search word under cursor), `n`/`N` (repeat search), `i`/`x` dot-repeat. So the ONLY still-missing commands are `R` and `gU`/`gu`/`g~` (above) — too few to warrant a consolidated issue right now; keep in IMP-023.
