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

## BUG-039: Terminal tab name not preserved across editor restart (restored exited terminal shows generic `*Terminal N*`)
- **ID:** BUG-039
- **Severity:** Medium (usability; defeats telling restored dead terminals apart; contradicts documented behavior).
- **Status:** Open — GitHub [#3074](https://github.com/sinelaw/fresh/issues/3074) filed (Run #60).
- **Version:** fresh 0.4.10 @ 992e61d02.
- **Reproduction Steps:** `cd /tmp/wr60 && fresh main.rs` (persistent, NOT --no-restore) → Open Terminal to the Right, `cd subdir` (tab `bash — root@vm: /tmp/wr60/subdir`) → Open Terminal (tab `bash — root@vm: /tmp/wr60`) → `exit` both → Ctrl+Q → `cd /tmp/wr60 && fresh` (pure restore).
- **Expected:** Each restored terminal keeps its tab name (`bash — .../subdir (exited)` / `bash — ... (exited)`), per terminal.md §Persistence + 0.4.6 changelog.
- **Actual:** Both restore as the SAME generic base name `*Terminal 0* (exited) 1` / `*Terminal 0* (exited) 2`; descriptive `bash — <cwd>` names lost. `(exited)` marker + `⟳ Restart` indicator + scrollback restore correctly.
- **Cause (black-box):** name re-derived from the live process each time — restarting the terminal regains `bash — ...`; a dead restored terminal has no live process, so it falls back to the internal buffer name. A live-at-quit terminal keeps its name (process present after restore).
- **Search queries (0 dupes):** `terminal tab name restart restore`, `terminal tab generic name exited restore session`, `terminal name survive editor restart`. Related-but-distinct: #2828 (closed), #2485 (closed), #2282 (rename feature request).

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
- **Status:** FIXED (Run #56) — closed by `11dccfad5`, CONFIRMED via UI (di"/ci"/da" forward-search from col 1 all work); commented. Remaining `a"` trailing-whitespace nuance → BUG-036 / #2604.
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
- **Status:** FIXED (Run #56) — closed by `463c435e3`, CONFIRMED via UI (`cw`+X → `X world foo bar` space kept, mid-word ✓, `dw` control ✓); commented.
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
- **Status:** FIXED — confirmed Run #50 in v0.4.3 @ 4e945b494 (fix commit `4e945b494`). All 3 original paths pass via UI: `$`+`j` → Col 5 + `x` → `shor` (no join); col-10 `j` clamps AND goal column restores on the next long line (Col 10); `k` from below → Col 5 + `x` → `shor`. Commented on the issue.
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

## BUG-023: Typing a dedent trigger does not re-indent the line (Python `else:`, custom `decrease_indent_pattern` tokens) — only `}` dedents live
- **ID:** BUG-023
- **Title:** Dedent rules apply ONLY at Enter-time (moved-down text). Typing a line matching a dedent trigger never re-indents it: built-in Python `else:` typed on an indent-4 line stays at 4 (VS Code/Sublime/Vim dedent live), and a custom `decrease_indent_pattern` token (`CLOSE`) behaves the same. Contrast: built-in `}` in C DOES live-dedent (electric bracket). The mis-indent then compounds (Enter after the wrong `else:` indents the next line to 8).
- **Severity:** Medium (daily Python if/else workflow; also makes the doc's own `begin`/`end` example useless in practice — you type `end`, never Enter-split before it).
- **Status:** Open — GitHub #2582 filed (Run #47).
- **GitHub Issue:** [#2582](https://github.com/sinelaw/fresh/issues/2582)
- **Reproduction (built-in Python, no config):** `fresh --no-restore test.py`; type `if a:` Enter `x = 1` Enter → line 3 inherits indent 4; type `else:` → stays `    else:` (expected: dedents to col 1). Custom variant: `.fresh/config.json` language w/ `"decrease_indent_pattern":"^\\s*CLOSE\\b"`, type `CLOSE` on an indented line → stays indented (this exact case was a row in #2314's expected table; #2314 was closed as completed).
- **What works (do not re-report):** Enter-split (cursor before `CLOSE tail`, Enter → moved-down line dedents one level, per docs); `increase_indent_pattern`; `dedent_next_line_pattern` (verified Run #47: `RET x` line at 4 → next line 0); C `}` live dedent.
- **First Seen:** 2026-07-07 (Run #47), fresh 0.4.3 @ 9f6135001.

---

## BUG-024: Search & Replace — match stepping (Ctrl+Alt+→/←) and Enter-open land on stale pre-edit positions after the buffer is edited
- **ID:** BUG-024
- **Title:** The S&R panel records match positions once at search time and never adjusts them for buffer edits. After a line is inserted/deleted above a match (or chars inserted before it on its line), every subsequent `Ctrl+Alt+→/←` step and Enter-open in that buffer moves the cursor to the OLD line/col, landing on unrelated text. The 0.4.2 stepping feature (#2434) exists explicitly to "review or edit" each match, so its core loop breaks on the first line-count-changing edit.
- **Severity:** Medium (behavioral; hits the feature's primary workflow; per-buffer — matches in unedited files keep landing correctly).
- **Status:** Open — GitHub #2583 filed (Run #48).
- **GitHub Issue:** [#2583](https://github.com/sinelaw/fresh/issues/2583)
- **Reproduction:** git project w/ `NEEDLE` matches; `Alt+A` → `NEEDLE`; `Ctrl+Alt+→` (Match 1 lands correctly); insert a line above (`Up`,`Home`, type, Enter); `Ctrl+Alt+→` → status `Match 2/5, Ln 4` and the REAL cursor cell (tmux `#{cursor_x}/#{cursor_y}`-verified) is on the pre-edit line 4 ("middle text") while the match is on line 5. Column variant: `Home`+`xx` on a match line, step away+back → lands col 1 on the `x` (match now col 3). Enter-open on a stale panel row: same. Panel previews also stay stale.
- **What works (do not re-report):** everything else about stepping — forward/backward, wrap both directions, cross-file opening in the source split, focus lands in editor for immediate edit, panel highlight follows current match, palette "Next/Previous Search Match", graceful "No Search & Replace results — run a search first" when no search/panel. **Workaround:** re-run the search (`Alt+A` then Enter) — refreshed results land correctly.
- **First Seen:** 2026-07-07 (Run #48), fresh 0.4.3 @ 9f6135001.

---

## BUG-025: File Explorer context menu does not grab the keyboard — keys fall through to type-ahead and can retarget menu actions
- **ID:** BUG-025
- **Title:** With the explorer right-click context menu open, printable keys and Backspace are NOT consumed by the menu: they reach the explorer's type-ahead find underneath (sidebar header becomes `/x`), which moves the tree selection behind the open menu. Menu actions act on the live selection (by design, "honoring the active multi-selection" per 0.4.1 changelog), so a stray keypress silently changes the target of Cut/Copy/Delete/Duplicate/Rename/Copy-path. The leaked filter also persists after the menu closes.
- **Severity:** Medium (silent retargeting of file operations; Delete partially mitigated by its confirm prompt naming the file).
- **Status:** Open — GitHub #2587 filed (Run #50).
- **GitHub Issue:** [#2587](https://github.com/sinelaw/fresh/issues/2587)
- **Reproduction:** `fresh --no-restore alpha.txt` in a dir with `alpha.txt`/`beta.txt`/`subdir`; `Ctrl+B`; right-click `alpha.txt` (selection marker `48;5;17` lands on it); type `sub` (header → `/sub`, marker moves to `subdir` behind the menu); Down×9 to Copy Relative Path, Enter → status `Copied path: subdir` (expected `alpha.txt`). Leak reproduced 3/3 with `q`/`z`/`sub`; Backspace edits the leaked filter; arrows/Enter/Esc correctly go to the menu.
- **Expected:** the app's own sibling menus grab the keyboard (0.4.2 changelog: "'+' popup and tab context menu grab the keyboard while open"; re-verified Run #49) and VS Code explorer context menus consume printable keys.
- **What works (do not re-report):** the menu itself is complete and functional — all 10 items; Duplicate (`alpha copy.txt`, disk-verified); Delete = y+Enter prompt → "Moved to trash"; New Directory (prefilled timestamped default name, `C-u` clears); Rename (prefilled current name); Copy Full/Relative Path (clipboard-verified); New File; same menu on dirs and empty space (acts on current selection); keyboard-grab DOES hold for arrows/Enter/Esc.
- **First Seen:** 2026-07-07 (Run #50), fresh 0.4.3 @ 4e945b494.

---

## PENDING (not filed) — vi mode missing standard commands (→ IMP-023)
- `R` (Replace/overtype mode): unrecognized — stays `-- NORMAL --`, next key runs as a normal command (`R`+`A` fired append-at-EOL).
- `gU`/`gu`/`g~` case OPERATORS: no-ops (`gUw` left text unchanged, `w` only moved cursor). NB single-char `~` DOES work.
- Candidate for one consolidated "vi missing standard commands" issue once more are characterized (count them with `;`/`,` from #2441). Logged here + potential_improvements; NOT individually filed (missing-feature, lower sev than the broken-behavior bugs above).
- **Run #45 update:** swept the rest of the common command set — the gap list is SMALL. CONFIRMED WORKING (Vim-correct, do NOT re-test): `o`/`O` (open line), `s`/`S` (substitute char/line), `D`/`C`/`Y` (operate to EOL; `Y` is linewise like Vim), `3G`/count+`G`, `*`/`#` (search word under cursor), `n`/`N` (repeat search), `i`/`x` dot-repeat. So the ONLY still-missing commands are `R` and `gU`/`gu`/`g~` (above) — too few to warrant a consolidated issue right now; keep in IMP-023.

## BUG-026: Git Grep always runs in the workspace root (broken in multi-repo workspaces, wrong repo from nested sub-repo buffers)
- **ID:** BUG-026
- **Title:** Standalone "Git Grep" palette command greps the workspace root instead of resolving the active buffer's repo.
- **Severity:** Medium (feature totally unusable in a multi-repo workspace — every search reports "No matches" and raises a `[⚠]` plugin ERROR; in a nested monorepo it silently searches the wrong repo).
- **Status:** Open — GitHub #2591 filed (Run #51).
- **GitHub Issue:** [#2591](https://github.com/sinelaw/fresh/issues/2591)
- **Reproduction:** Workspace root not a repo, sub-repo `app/` with committed `main.py`. Open `app/main.py`, palette → Git Grep, type any committed term. Status: "No matches", `[⚠]` increments; Show Warnings → `[git_grep] process exited with code 128: fatal: not a git repository`. Nested case: root repo + nested `vendored/` repo, from `vendored/inner.py` grep inner-only content → "No matches" (searched outer). Control: outer-content grep from outer buffer works.
- **Expected:** resolve the buffer's repo like git_find_file / live_grep git-grep provider / git_blame / audit_mode (all verified doing so in the same build).
- **Actual:** always workspace root; real error masked by "No matches"; exit 1 (normal no-match) also logged as ERROR.
- **First Seen:** Run #51, 2026-07-07 (v0.4.3, master @ 4e945b494).

## BUG-027: File Explorer git decorations ignore a nested sub-repo's status when the workspace root is itself a git repo
- **ID:** BUG-027
- **Title:** In a root-is-a-repo workspace, files inside a nested git sub-repo get no explorer decoration; the dir shows only the outer repo's `U`.
- **Severity:** Medium (contradictory state on one screen: gutter/blame/Review Diff say modified, tree says clean; decorations DO work for sub-repos when the root is not a repo).
- **Status:** Open — GitHub #2592 filed (Run #51).
- **GitHub Issue:** [#2592](https://github.com/sinelaw/fresh/issues/2592)
- **Reproduction:** Outer repo w/ committed+modified `outer.py`; nested `vendored/` repo w/ committed `inner.py` + uncommitted line. Launch at outer root, Ctrl+B, expand `vendored`. `inner.py` = no decoration (waited minutes); `outer.py` = `M`; `vendored` = `U`. Same file: gutter marks the added line, Git Blame shows inner commits, Review Diff scopes to inner (`M inner.py +1/-0`).
- **Expected (VS Code + Fresh's own multi-repo behavior):** `inner.py` shows `M` from its own repo.
- **Actual:** no decoration; only outer's perspective rendered.
- **First Seen:** Run #51, 2026-07-07 (v0.4.3, master @ 4e945b494).

## BUG-028: Theme color-transition animation never plays (documented but absent on every switch path)
- **ID:** BUG-028
- **Title:** The documented "brief color-transition animation" on theme switch never renders — every switch path swaps colors instantly, though the tab-switch slide from the same animations framework does animate.
- **Severity:** Medium (documented, toggleable feature missing / doc mismatch; purely cosmetic — theme still changes correctly).
- **Status:** Open — GitHub #2594 filed (Run #52).
- **GitHub Issue:** [#2594](https://github.com/sinelaw/fresh/issues/2594)
- **Reproduction:** `fresh --no-restore <colorful.py>`; confirm Settings → Editor → Display → Animations `[v]` (default). Frame-burst harness (`scratchpad/burst.py`: send key then loop `tmux capture-pane -e -p` ~4ms/frame ×200–300, diff color-code signature on a fixed line). Switch themes 5 ways: Select Theme picker preview (arrows) / apply (Enter) / cancel (Esc); Settings Theme dropdown + Ctrl+S; init.ts-registered `editor.applyTheme()`. All show only 2 signatures (before/after) 4ms apart, no intermediates over 1s+.
- **Expected (Fresh docs `configuration/index.md` §Screensaver + 0.4.0 blog):** a brief interpolated color-transition between old and new theme palettes.
- **Actual:** instant swap, zero interpolated frames. CONTROL: tab-switch slide (Ctrl+PgDn) DOES render transient offset frames (~16 cols, ~170ms) in the same session → framework + terminal are capable; the transition just never triggers.
- **First Seen:** Run #52, 2026-07-07 (v0.4.3, master @ 4e945b494).

## BUG-029: Large files — first line-index scan undercounts the total line count
- **ID:** BUG-029
- **Title:** The first on-demand line-index scan of a >10MB file computes a too-small total; Go to Line clamps below the real last line and EOF is labeled with a wrong line number.
- **Severity:** Medium (last ~200–1800 lines of a 12MB fixture unreachable by line jump; non-monotonic gutter at EOF; status vs gutter disagree on the same position right after an at-EOF scan: `Ln 150001` vs `298189`).
- **Status:** Open — GitHub #2596 filed (Run #54).
- **GitHub Issue:** [#2596](https://github.com/sinelaw/fresh/issues/2596)
- **Reproduction:** 12,000,000-byte file of 300,000 40-byte lines (true last line 300001). Open, `Ctrl+G` → `y`+Enter to scan → `Ctrl+G` `999999` Enter → lands "line 299828" (viewport-at-top scan, 3/3 at 1s/3s/15s post-launch) or "298189" (cursor-at-EOF scan, 2/2). `299500` also clamps. Below-max mappings exact (150000/298188 verified by content).
- **Expected (docs/features/navigation.md "exact line numbers"; VS Code last line):** max = 300001; every line reachable.
- **Actual:** wrong, position-dependent max; re-running "Scan Line Index" corrects to 300001 (2/2) = workaround. 150MB fixture scans correctly first time.
- **First Seen:** Run #54, 2026-07-07 (v0.4.3, master @ 4e945b494).

## BUG-030: Large files — palette `:N` line jump before a scan goes to EOF and claims "Jumped to line 1"
- **ID:** BUG-030
- **Title:** In an unscanned large-file buffer, the palette's `:line` mode jumps to the end of the file and reports "Jumped to line 1" instead of offering the line-index scan.
- **Severity:** Medium (silent wrong navigation with a wrong message; the advertised `:line` palette mode is a trap in exactly the file class where line jumps need help).
- **Status:** Open — GitHub #2597 filed (Run #54).
- **GitHub Issue:** [#2597](https://github.com/sinelaw/fresh/issues/2597)
- **Reproduction:** Open a >10MB file (byte-offset gutter). `Ctrl+P`, `C-u`, type `:1000`, Enter → cursor at `Byte 12000000` (file end), status "Jumped to line 1". 2/2 (also with `:150000`).
- **Expected (Ctrl+G parity + VS Code goto):** offer "Scan file for exact line numbers?" or a clear "no line index" message with the cursor unmoved.
- **Actual:** bogus jump to EOF + wrong message. Controls: `:N` exact in small files; `Ctrl+G` same buffer offers the scan; post-scan `:N` works.
- **First Seen:** Run #54, 2026-07-07 (v0.4.3, master @ 4e945b494).

## BUG-031: Rust LSP Reduced Memory mode — hover/Go-to-Def dead on macro names and inside macro invocations
- **ID:** BUG-031
- **Title:** After switching rust-analyzer to Reduced Memory mode ("Rust LSP: Configure Mode"), hover (Alt+K) and Go to Definition (F12) permanently fail on macro names (`println!`) and on any symbol inside macro invocation args (`println!`/`format!`); non-macro positions keep working.
- **Severity:** Medium (core navigation silently broken in ubiquitous cursor positions; mode's own status message names only checkOnSave/procMacro/cachePriming — println! is a builtin macro, unexplained by any of them; no warning surfaced).
- **Status:** Open — GitHub #2598 filed (Run #55).
- **GitHub Issue:** [#2598](https://github.com/sinelaw/fresh/issues/2598)
- **Reproduction:** Tiny trait crate (/tmp/ralsp: Shape trait, Circle/Square impls, describe()/area_sum() in main.rs). Start rust LSP (Full mode default): F12/hover on `describe` inside `println!("{}", describe(&c))` works. Palette → "Rust LSP: Configure Mode" → Reduced Memory → wait 60s+ → same position: F12 "No definition found" (retried to 5min, permanent), Alt+K "No hover information available"; hover on `println` itself dead; F12 on `name` in `format!` args dead. Controls seconds apart in same mode: bare `area_sum` call F12 ✓, `Circle` struct-literal F12 cross-file ✓, fn-def hover ✓, `self` F12 ✓. Switch back to Full → failing position recovers in seconds.
- **Expected (VS Code + rust-analyzer with those three settings off):** navigation inside builtin-macro args keeps working; if a mode degrades features, its description says so.
- **Actual:** silent per-position failure that looks random to the user. Secondary: every reduced-mode (re)start has a 30–60s window where ALL requests return empty while status shows "LSP (rust) ready" (Full mode answers within seconds).
- **First Seen:** Run #55, 2026-07-07 (v0.4.3, master @ 4e945b494; rust-analyzer 1.94.1).

## BUG-032: Rename Symbol (F2) — cross-file rename steals focus to the definition's file at a stale position
- **ID:** BUG-032
- **Title:** F2 rename invoked from a use site whose definition lives in another file applies correctly but switches the active tab to the definition's file, cursor at that buffer's stale previous position (not even the renamed symbol).
- **Severity:** Medium-low usability (user loses their place after every cross-file rename; VS Code/IntelliJ keep focus in the invoking file).
- **Status:** Open — GitHub #2599 filed (Run #55).
- **GitHub Issue:** [#2599](https://github.com/sinelaw/fresh/issues/2599)
- **Reproduction:** Same crate. In main.rs cursor on `radius` in `Circle { radius: 2.0 }` (use site; def in shapes.rs). F2 → C-u → `rad` → Enter → "Renamed successfully (4 changes)" but active tab is now shapes.rs at its old cursor line (7, while the def is line 11 in the `side` trial). 2/2 (radius, side). Controls: def-site invocation stays put; single-file rename (describe → render, 3 changes) stays put.
- **Expected (VS Code):** focus and cursor remain in the invoking file at the renamed symbol.
- **Actual:** active tab switches to the other edited file at a stale position. main.rs keeps its own cursor (verified on switching back). Also verified fine: F2 on whitespace → graceful "Cannot rename: ... No references found at position" without opening the prompt; undo reverts the rename's edits.
- **First Seen:** Run #55, 2026-07-07 (v0.4.3, master @ 4e945b494; rust-analyzer 1.94.1).

## BUG-033: Dismissing a hover popup removes error diagnostics elsewhere from gutter/status/F8 until next save
- **ID:** BUG-033
- **Title:** Dismissing a hover popup (Escape or cursor-move) whose position does not overlap an error diagnostic drops that error from the status severity counter, the gutter marker, and F8 navigation — while the Diagnostics panel still lists it and the code is still broken.
- **Severity:** Medium (hover is constant-use; every hover away from an error silently hides it — user believes the error is gone).
- **Status:** Open — GitHub #2601 filed (Run #56).
- **GitHub Issue:** [#2601](https://github.com/sinelaw/fresh/issues/2601)
- **Reproduction:** Crate with `let bad = p.zzz;` error + unused-var warnings; save so checkOnSave publishes (`E:1 W:2 I:1`, red ● on the error line). Alt+K at the warning line or any clean call site → popup → Escape (or move cursor). Status → `W:2 I:1`, ● gone, F8 cycles only warnings. 5/5 (Esc ×4, cursor-move ×1).
- **Expected (VS Code):** closing a hover tooltip never changes the problem set.
- **Actual:** error dropped from working set until re-save. Controls: hover ON the error position preserves (2/2); bare Esc preserves; panel open/close preserves; warnings/hints never drop.
- **First Seen:** Run #56, 2026-07-07 (v0.4.3, master @ 11dccfad5; rust-analyzer 1.94.1 Full mode).

## BUG-034: Diagnostics don't track buffer edits — markers/F8/panel point at stale lines until re-save
- **ID:** BUG-034
- **Title:** Diagnostic positions are frozen at publish time: inserting lines above an error (including via an editor-applied code action WorkspaceEdit like "Generate `new`") leaves the gutter ●, F8, and the panel pointing at the old line — now the middle of valid/generated code — while the real error line is unmarked.
- **Severity:** Medium usability (F8 confidently jumps into freshly generated code; markers on wrong lines).
- **Status:** Open — GitHub #2602 filed (Run #56).
- **GitHub Issue:** [#2602](https://github.com/sinelaw/fresh/issues/2602)
- **Reproduction:** Error at line 8 published (save). Alt+. on the struct name → accept "Generate `new`" (6 lines inserted above the error). ● stays on line 8 (inside generated `fn new`); F8 → 8:17 with the zzz message; real `p.zzz` (line 14) unmarked; panel `[E] 8:17`. Save corrects everything.
- **Expected (VS Code):** diagnostic ranges shift with buffer edits, especially editor-applied ones with known deltas.
- **Actual:** frozen until the next check-on-save re-publish. Same family as #2583 (S&R stepping), different subsystem. Note: inline-diagnostics advertises "version-aware staleness dimming", but gutter/F8/panel neither shift nor indicate staleness.
- **First Seen:** Run #56, 2026-07-07 (v0.4.3, master @ 11dccfad5; rust-analyzer 1.94.1 Full mode).

## BUG-035: Completions never include unimported symbols — documented auto-import-on-accept unreachable
- **ID:** BUG-035
- **Title:** LSP completions omit every candidate that would require an auto-import (rust-analyzer flyimport): bare `HashMa`/`HashMap` and import-needing trait methods (`p.type_i`) produce NO popup at all, so lsp.md's "Auto-imports are applied when you accept a completion" can never trigger.
- **Severity:** Medium (documented feature unreachable; a core completion workflow vs VS Code).
- **Status:** Open — GitHub #2603 filed (Run #56).
- **GitHub Issue:** [#2603](https://github.com/sinelaw/fresh/issues/2603)
- **Reproduction:** Crate with no `use` stmts, LSP ready. `let m = HashMa` + Ctrl+Space (and palette Show Completions) → nothing, ever; exact `HashMap` → nothing; `p.type_i` → nothing. Controls same session: `p.` members ✓; `Strin` → String/stringify! (prelude) ✓; `std::collections::HashMa` → HashMap (qualified) ✓.
- **Expected (VS Code + same RA):** `HashMap (use std::collections::HashMap)` offered; accept inserts ident + use line.
- **Actual:** no import-needing candidates exist in Fresh's results at all.
- **First Seen:** Run #56, 2026-07-07 (v0.4.3, master @ 11dccfad5; rust-analyzer 1.94.1 Full mode).

## BUG-036: vi `a"` text object excludes trailing whitespace (Vim includes it) — `da"` leaves a double space
- **ID:** BUG-036
- **Title:** `a"` selects only the quoted string incl. quotes but not the trailing whitespace, so `da"` on `the "quick" brown fox` yields `the  brown fox` (double space); Vim (`:help aquote`) includes trailing (else leading) whitespace → `the brown fox`.
- **Severity:** Medium-low (leaves stray whitespace on every `da"`/`ca"`; forward-search itself fixed by 11dccfad5).
- **Status:** Open — GitHub #2604 filed (Run #56).
- **GitHub Issue:** [#2604](https://github.com/sinelaw/fresh/issues/2604)
- **Reproduction:** vi mode on, line `the "quick" brown fox`, `0` then `d` `a` `"` → `the  brown fox` (cat -A: `the  brown fox$`). Same from inside the quotes (2/2). `di"` unaffected.
- **First Seen:** Run #56, 2026-07-07 (v0.4.3, master @ 11dccfad5), during the #2439 fix verification.

## BUG-037: vi visual-block indent operators `>`/`<` are no-ops and don't exit VISUAL BLOCK mode
- **ID:** BUG-037
- **Title:** In VISUAL BLOCK mode (Ctrl+V), `>` and `<` do nothing — buffer unchanged, no status feedback, and the editor remains in `-- VISUAL BLOCK --` (Vim `:help v_b_>`: shift the highlighted lines by shiftwidth and return to NORMAL).
- **Severity:** Medium (core Vim editing command silently dead in one mode; same symptom #2438 had before its fix).
- **Status:** Open — GitHub #2606 filed (Run #58).
- **GitHub Issue:** [#2606](https://github.com/sinelaw/fresh/issues/2606)
- **Reproduction:** vi mode on, cursor col 1 line 2, Ctrl+V, `j`, `>` → nothing, mode stays VISUAL BLOCK (2/2; `<` on an indented line 1/1). Keys sent individually with ≥0.4s gaps, buffer verified between steps, re-polled at 1.2s+ (render-lag guard).
- **Controls:** `V`+`>` indents 4sp + returns NORMAL (#2438 fix works for visual-line); `x` in VISUAL BLOCK deletes the selected rect + returns NORMAL (block mode + other operators fine).
- **First Seen:** Run #58, 2026-07-07 (v0.4.3, master @ 6ab255709).

---

## BUG-038: Extract Tab to New Workspace — the extracted co-tenant workspace is not persisted (lost on restart)
- **ID:** BUG-038
- **Severity:** Medium-high (silent loss of the co-tenant workspace arrangement the feature exists to create; an unsaved extracted buffer would be data loss).
- **Status:** **FIXED** (Run #60, confirmed via UI in v0.4.10; fix landed 0.4.9, maintainer-closed). Recheck: `fresh main.rs lib.rs` → Extract Tab on lib.rs → 2 co-tenant windows → Quit → TWO `.ws-*.json` on disk (was ONE) → relaunch `fresh` → BOTH windows restore with their files. `--no-restore` half also fixed: writes no session layout (only trust.json).
- **GitHub Issue:** [#2735](https://github.com/sinelaw/fresh/issues/2735)
- **Version:** v0.4.4 @ f545a75ad.
- **Feature:** 0.4.4 co-tenant workspaces — palette "Extract Tab to New Workspace" (builtin) moves the current tab into a NEW workspace over the same project root (a co-tenant window). Backed by the commit "Restore each co-tenant window to its own file on reboot."
- **Reproduction (2/2 clean):** clear state (`rm -rf ~/.local/share/fresh/workspaces/* orchestrator/*`); `cd proj && fresh --no-restore main.rs lib.rs` (2 tabs); Ctrl+P → Extract Tab to New Workspace (on lib.rs) → 2 co-tenant windows `proj` + `proj (2)` (verify via Orchestrator: Open + **Alt+I** "show empty/1-file workspaces" — both hold 1 file). Quit. → only ONE `workspaces/<root>.ws-<stable_id>-N.json` on disk, containing main.rs (the SOURCE). Relaunch `fresh` → only main.rs window; Alt+I shows no 2nd.
- **Expected:** both co-tenant windows restore, each with its own tab (Fresh's own "restore each co-tenant window" commit; VS Code restores both windows after moving an editor to a new window + reload).
- **Actual:** only the source workspace is written on quit; the newly-extracted co-tenant (and its tab) is dropped. Both windows are 1-file, so it is NOT the Alt+I filter (source survives).
- **Workaround:** none across a restart; reopen + re-extract each launch (underlying files untouched on disk).
- **First Seen:** Run #59, 2026-07-20.

---

## PENDING CANDIDATES (Run #59) — NOT filed; escalate only with clearer intent/repro

### PC-59a: Ctrl+] does not exit terminal mode (documented alias for Ctrl+Space)
- docs/features/terminal.md lists `Ctrl+]` under "Switching Between Modes: Exit terminal mode (same as Ctrl+Space)". In plain tmux (220×50, no Kitty/CSI-u), a single Ctrl+] (raw 0x1D via `send-keys -H 1d`) does NOT change the mode — status stays "Terminal mode enabled"; the byte is forwarded to the shell (non-printing). Ctrl+Space toggles correctly.
- Likely a terminal-encoding limitation (Ctrl+]=0x1D, analogous to Ctrl+H=0x08 = #2109) rather than a Fresh logic bug; Ctrl+Space is the working primary. Low impact. Retest in a KKP terminal before filing. → also logged in potential_improvements.

### PC-59b: Terminal scrollback reloads under `--no-restore`
- Terminal scrollback persists per-workspace at `~/.local/share/fresh/terminals/<enc-root>/fresh-terminal-N.txt`. Launching `fresh --no-restore` and opening a brand-new terminal reloads the PREVIOUS session's scrollback (deterministic). `--no-restore` is documented only as "Don't restore the previous **workspace**"; scrollback persistence is a separate documented feature (like shell history), so surprising-but-plausibly-intended. Escalate only if a user-facing contradiction is confirmed.

## Bug #3136 — Terminal input replay via Run Agent dialog + workspace switch (Run #61, FILED)
- **Status:** FILED https://github.com/sinelaw/fresh/issues/3136 (tui-agent-auto-bug), v0.4.10 @ 7bcc8ff6c.
- **Repro (3/3):** command in workspace terminal (`echo BAIT2`+Enter, executes) → Ctrl+P → Run Agent… → Enter (dialog) → Esc → dock (Alt+O) switch to another workspace and back → the command line sits at the prompt as PENDING input; **Enter re-executes it** (real PTY input; C-u clears it).
- **Controls:** switch-away/back without the dialog → clean (2/2); palette query + Esc without opening the dialog → clean; second bounce without re-opening the dialog → clean (one-shot per dialog interaction).
- **Family evidence (1×, unreproduced standalone):** palette query text (`Spawn a new editor`) appended into the dialog's Agent Command field at open.

## Bug #3137 — Move-to-Folder popup keyboard fall-through (Run #61, FILED)
- **Status:** FILED https://github.com/sinelaw/fresh/issues/3137 (tui-agent-auto-bug), v0.4.10 @ 7bcc8ff6c.
- **Repro (3/3):** visit workspace → palette "Orchestrator: Move to Folder…" → popup `● Top level`/`New Folder…` → press ↓ → dock selection moves (LIVE workspace switch) + popup dismissed; popup highlight never moves (ANSI-verified). Mouse click on popup rows works. Regression vs closed #2694.

## PENDING CANDIDATES (Run #61) — NOT filed; escalate only with a repro
### PC-61a: Enter on collapsed `[ ▶ Advanced… ]` closed the whole Run Agent dialog (1×)
- Once: Tab-burst to the collapsed Advanced button, Enter → dialog gone (no workspace created), Advanced state flipped to ▼ for the next open. Two deliberate retries: Enter toggles ▶/▼ correctly in place, dialog stays. Likely an input race in the retained-mode dialog; watch, don't file.
### PC-61b: first char after a focus transition swallowed (4× organic, 0/9 controlled)
- First typed char eaten right after click-into-terminal / workspace switch (`at -v`, `cho LEAKED`, `cho EXTRACTED-$PWD`, `cho R3-MARK`). Controlled trials (grid click / tab click / dock switch × 0.6s + 1.5s gaps) all clean. Trigger involves richer preceding palette/dialog state — plausibly #3136's input-routing family. If #3136 gets a fix, re-test this alongside. HARNESS: verify typed line before Enter after any focus transition.
