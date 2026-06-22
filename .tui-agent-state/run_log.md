# TUI Agent Run Log

---

## Run #40 — 2026-06-22 — #2405 brackets NOT highlighted inside comments/strings — COMPREHENSIVE PASS, no bug

**Preflight:** Synced state branch. Master FORCE-UPDATED past Run #39's `205b9640e` → **`33e2ed130`** (still v0.4.1; 3 new commits: `1008134eb` fix(brackets) #2405, `0420b2eed` refactor TerminalManager::spawn, `33e2ed130` refactor process_async_messages). Auth not exercised this run (no bug to file, #2405 is owner-filed + already merged → verification only, no comment). Built `fresh` 0.4.1 from `/tmp/fresh-master` worktree @ origin/master `33e2ed130` (~14m, LTO fat). Per R1 (version changed + a fix landed) → primary objective = verify **#2405**. Per R2 this advances new bracket-highlighting coverage on top of Run #26's Rainbow Brackets pass.

**Objective:** #2405 — "Rainbow bracket colorization and bracket-match highlighting were applied to every `()[]{}<>` including those inside comments and string literals … the bracket overlay now skips any bracket inside a Comment or String span — for rainbow colorization, cursor-on-bracket matching, AND nesting-depth calculation."

**Fixture:** `/tmp/brk40/test.py` (Python, built-in syntax highlighting; no LSP needed), session `brk40_<pid>`, 200x50, `--no-restore`:
```
1  code = (1 + (2 * (3 + 4)))
2  s = ("brk ( [ { } ] ) in str")
3  # cmt ( [ { } ] ) in comment
4  z = (10 + 20)  # ))) ((( fake
5  w = (30)
```

**Results (ANSI `capture-pane -p -e` verified) — ALL PASS, no bug:**
- **Rainbow colorization correct on real code** (line 1): `(`/`(`/`(` = `38;5;6`(cyan,d0)/`38;5;2`(green,d1)/`38;5;3`(yellow,d2); closers mirror `3,2,6`. Matches Run #26 cycle `[6,2,3,126,15,27]`.
- **String brackets NOT rainbow** (line 2): the enclosing real `(`…`)` are `38;5;6` cyan, but the entire string `"brk ( [ { } ] ) in str"` renders as ONE `38;5;34` green span — the `( [ { } ] )` inside keep the string color, NOT individually rainbow-colored.
- **Comment brackets NOT rainbow** (line 3): whole line `# cmt ( [ { } ] ) in comment` = `38;5;253` comment color; brackets keep comment color.
- **Depth calc excludes comment/string brackets** (line 4): real `(…)` pair colored cyan/cyan (depth 0/0) — the trailing comment's `# ))) ((( fake` (all `38;5;253`) does NOT shift nesting depth; the real `)` still mirrors the real `(` at depth 0.
- **Go to Matching Bracket (`Ctrl+]`, palette) — matching skips comment brackets:** from the real `(` (Col 5, line 4) → jumps to the real `)` (Col 13) — NOT to the comment `)))`. From a **comment `)`** (Col 18) → status `No matching bracket found` (comment bracket correctly not treated as structural). (Note: `Ctrl+]` via `tmux send-keys` did NOT fire — tmux likely intercepts it; drive via palette "Go to Matching Bracket" + Enter instead.)

**Refactor smoke test (PASS):** the two new refactor commits (`TerminalManager::spawn` decompose, `process_async_messages` split — high regression risk) show no user-visible regression: editor launches + renders async (file loaded, palette + search work), terminal spawns via palette "Open Terminal" (tab `bash — root@vm: /tmp/brk40`), PTY I/O works (`echo SPAWN-OK-$((6*7))` → `SPAWN-OK-42`).

**Conclusion:** #2405 fix works end-to-end (colorization + depth + matching all skip comment/string brackets). No bug, no false positive. #2405 is owner-filed + already merged → no GitHub comment.

**State updates:** run_log (this), test_plan (Run #40 note + NEXT list), learning_db (+"Brackets in comments/strings — #2405 (Run #40)" extending the Rainbow Brackets lesson). No confirmed_bugs / github_issues / potential_improvements changes.

**Cleanup:** killed tmux `brk40_*`; removed `/tmp/brk40`; left build worktree `/tmp/fresh-master` (reusable next run; remove if stale).

**NEXT new-coverage (Run #41+, top-down, prefer freshest 0.4.1):** `7d636f1de` clipboard strips ANSI on default copy; `6df567f04` LSP `textDocument/implementation`; `77360e6c6` Shift+letter binding when terminal omits SHIFT; `e4a554347` terminal hides scrollbar/reclaims column; then (d) '+' new-tab popup / terminal Ctrl+Click / OSC 7; (e) theme color-transition animation; (f) GDScript (#2238). Then #2197 only if a fix lands.

---

## Run #39 — 2026-06-22 — #2373 quick-open lists virtual buffers in `#` switcher — COMPREHENSIVE PASS, no bug

### Status: COMPLETED

**Preflight:** Synced state branch (`pull --rebase`, already up to date). Playbook intact (PER-RUN LOOP / ANTI-DRIFT / ISSUE FILING STANDARDS / FALSE POSITIVE PATTERNS all present). Lessons continuity OK. GitHub MCP auth **LIVE** (`issue_read` #2373 returned full body).

**Build:** Master **UNCHANGED** at `205b9640e` (= v0.4.1) since Run #38 — per **R1** skipped open-issue rechecks (no new fix landed; #2317 already confirmed fixed last run). Build worktree `/tmp/fresh-master` was gone → recreated via `git worktree add --force /tmp/fresh-master origin/master`; built release `fresh 0.4.1` (6m24s, exit 0). Per **R2** advanced top new-coverage candidate: **#2373** (`205b9640e` "fix(quick-open): list virtual buffers in the # buffer switcher").

**Issue context:** #2373 is **owner-filed** (sinelaw), already closed by maintainer 2026-06-22. Not an agent issue → no comment needed; this run is new-coverage verification that the fix behaves. Expected (per issue): the `#buffer` quick switcher previously listed ONLY file-backed buffers; virtual buffers (plugin panels like `*blame:*`, `*Git Log:*`, `*Keyboard Shortcuts*`) never appeared and could only be reached by clicking their tab. After the fix they should be listed by name.

**Fixture:** real git repo `/tmp/vbuf39` (commit signing off), 2 commits, `sample.py` (modified across commits → real blame history) + `notes.txt`. Launched `fresh --no-restore sample.py` from inside the repo dir (cd in tmux shell first, per learning_db — blame needs repo cwd). tmux `vbuf39_<pid>` 220×50, ANSI-verified.

**Results — COMPREHENSIVE PASS (black-box, `#` switcher = Ctrl+P → `C-u` clear → `#`):**
- **BASELINE** (only `sample.py` open): `#` switcher lists just `sample.py` with its path `/tmp/vbuf39/sample.py`. Mode line `file | >command | :line | #buffer`.
- **Create virtual buffer #1** — palette → "Git Blame" → `*blame:sample.py*` tab created (status `Git blame: 4 blocks`). Re-open `#` switcher → **BOTH** `sample.py` (with path) AND `*blame:sample.py*` (name only, no path — it's virtual) now listed. ✅ This is exactly the #2373 fix.
- **Filterable by name** — typing `#blame` narrows the list to just `*blame:sample.py*`, row highlighted (ANSI `48;5;25m`). ✅
- **Selectable / navigates** — switched active buffer away to `sample.py` (C-PageUp; content = raw file, no blame separators), then `#`→`blame`→Enter → active buffer became the blame buffer (content shows `── <sha> … ──` separators, status `Git blame: 4 blocks`). ✅ Navigation by name works.
- **General, not blame-specific** — palette → "Show Keyboard Shortcuts" → `*Keyboard Shortcuts*` tab. `#` switcher now lists **all three**: `sample.py` (path) + `*blame:sample.py*` + `*Keyboard Shortcuts*` (both virtual, name-only). ✅ Matches issue note "Not specific to those plugins."
- **Round-trip to a file buffer** — filter `#sample` fuzzy-matches BOTH `sample.py` and `*blame:sample.py*` (correct substring behavior); selecting top entry navigates back to the raw file. ✅

**Verdict:** #2373 fix CONFIRMED working in v0.4.1. Virtual/plugin-panel buffers now appear in the `#` quick switcher, are fuzzy-filterable by name, and selectable to navigate. File buffers show their path; virtual buffers show name only (sensible, since they have no fs path). **No bug, no false positive, no friction worth filing.**

**State updates:** run_log (this), learning_db (+"`#` buffer switcher lists virtual buffers (Run #39)"), test_plan (Run #39 note + candidate done). No GitHub issue (owner issue already closed; nothing agent-owned changed). No confirmed_bugs / potential_improvements change.

**Cleanup:** killed tmux `vbuf39_*`; removed `/tmp/vbuf39`. Left build worktree `/tmp/fresh-master` @ `205b9640e` (reusable next run; remove if version moves).

**NEXT new-coverage (Run #40+, top-down, prefer freshest 0.4.1):** `7d636f1de` clipboard strips ANSI on default copy; `6df567f04` LSP `textDocument/implementation`; `77360e6c6` Shift+letter binding match when terminal omits SHIFT; `e4a554347` terminal hides scrollbar/reclaims column; then (d) '+' new-tab popup / terminal Ctrl+Click / OSC 7; (e) theme color-transition animation; (f) GDScript (#2238). Then #2197 only if a fix lands.

---

## Run #38 — 2026-06-22 — #2317 line-level v stage/unstage/discard CONFIRMED FIXED in v0.4.1 + filed #2420 (raw i18n key on discard)

### Status: COMPLETED

**Preflight:** Synced state branch (pull --rebase clean). Playbook intact (PER-RUN LOOP / ANTI-DRIFT / ISSUE FILING STANDARDS / FALSE POSITIVE PATTERNS all present). Lessons continuity OK (topic-organized). GitHub MCP auth LIVE (`issue_read` #2317 returned).

**Build:** Master FORCE-UPDATED past Run #37's `42bfbb586` → **`205b9640e`** (still v0.4.1; 2 new commits: `a1d3e4352` fix review-diff line-level stage/unstage/discard #2317; `205b9640e` quick-open virtual buffers #2373). Per R1 (version changed + a fix for an OPEN agent issue landed) → primary objective = recheck **#2317**. Built release `fresh` 0.4.1 from `/tmp/fresh-master` worktree @ origin/master (8m28s).

**Fixture:** real git repo `/tmp/rdiff38` (commit signing off), tracked `README.md` + `src/calc.py`. Tested via tmux `rdiff38b_<pid>` 220×50, ANSI-verified cursor on a real +/- line before every op, each op confirmed against `git` ground truth.

**#2317 — CONFIRMED FIXED (v0.4.1), commented + marked resolved.** All three line-level visual ops now work (issue was filed Run #36 when ALL failed):
- **`v`+`s` stage** — single `+result = 1` (modification half) and clean `+extra line`: status `Lines staged`; `git diff --cached` shows exactly that one line staged, rest of hunk left unstaged. ✅
- **`v`+`u` unstage** — single staged line returns to unstaged (`M ` → ` M`, cached empty). ✅
- **`v`+`d` discard** — clean pure addition: line removed from working tree (git clean). Single-line **modification**: select full `-`/`+` pair (`v`,`j`,`d`) → reverts to HEAD (git clean). ✅

**NEW BUG FILED → #2420 (low, `bug`+`tui-agent-auto-bug`):** line-level **discard** success path prints the **raw i18n key `status.lines_discardd`** (note typo "discardd", double-d) in the status bar instead of a localized message. Discard itself works (git verified). Deterministic across 2+ discards. CONTRAST: line-level **stage** shows proper `Lines staged`. 3 dup-searches (`lines_discardd`, `review diff discard status message i18n key`, `untranslated status key discard`), none.

**Edge case (NOT filed — likely expected git semantics):** `v`+`d` discard on a SINGLE `+` line of a modification (without selecting the paired `-`) fails with `Patch failed: ... patch does not apply` and discards nothing. Reverse-applying just the `+` half of an in-place modification is ambiguous; the unambiguous path (select full `-`/`+` pair) works. Logged → potential_improvements (sub-hunk modification discard UX), do NOT file without clearer expected-vs-actual.

**tmux harness gotchas (→ learning_db):** Review Diff focus cycles FILES→diff→COMMENTS via Tab; the ▸ in the center header marks the FOCUSED panel — only when ▸ is on the center diff do Up/Down move the line cursor (else they move the FILES sidebar selection). STAGED hunks render file-header-only (no content rows) in the combined view until you select that file in the FILES sidebar (then center expands it). Line-cursor Down can skip a visual row near hunk boundaries — verify exact landing with ANSI `48;5;243` (cursor-row bg) every time; do NOT trust step counts.

**State updates:** run_log (this), learning_db (+"Review Diff line-level v ops fixed + i18n key leak (Run #38)"), confirmed_bugs (+BUG-015/#2420), github_issues (+#2420 row + #2317 → FIXED + Last-updated bump), potential_improvements (+sub-hunk modification-discard UX), test_plan (Run #38 note).

**Cleanup:** killed tmux `rdiff38_*`/`rdiff38b_*`; removed `/tmp/rdiff38`. Left build worktree `/tmp/fresh-master` @ 205b9640e (reusable next run; remove if stale/version moves).

**NEXT new-coverage (Run #39+, top-down, prefer freshest 0.4.1):** `205b9640e` quick-open lists virtual buffers in `#` switcher (#2373 — verify a virtual/special buffer like *Review Diff* or *Keyboard Shortcuts* appears in `#` buffer mode); then prior 0.4.1 backlog: `7d636f1de` clipboard strips ANSI on default copy; `6df567f04` LSP textDocument/implementation; `77360e6c6` Shift+letter binding when terminal omits SHIFT; `e4a554347` terminal hides scrollbar/reclaims column; then (d) '+' new-tab popup / terminal Ctrl+Click / OSC 7; (e) theme color-transition animation; (f) GDScript (#2238). Then #2197 only if a fix lands.

---

## Run #37 — 2026-06-22

### Status: COMPLETED

### What Was Done
- Synced state (`tui-automated-testing-state`, pull --rebase clean). **Preflight:** playbook integrity OK (PER-RUN LOOP, ANTI-DRIFT RULES, ISSUE FILING STANDARDS, FALSE POSITIVE PATTERNS all present); GitHub MCP auth LIVE (`issue_read` #2307 returned); lessons continuity OK (topic-organized, intact).
- **Build directive:** origin/master FORCE-UPDATED past Run #36's `1b5d7f8c8` → now **`42bfbb586` = v0.4.1**. Built release binary in `/tmp/fresh-build` worktree (`fresh 0.4.1`, exit 0). New 0.4.1 commits incl. `4b6e1d2f2` (#2307 fix), per-buffer view toggles `93ac8d5ff`, view persistence `233c5cb64` (#474), clipboard ANSI strip `7d636f1de`, LSP implementation `6df567f04`, remote/web-ui reconnect work.
- **R1 (version changed → recheck the fix that landed): #2307 CONFIRMED FIXED.** Maintainer closed it 2026-06-21 (fix `4b6e1d2f2`). Reproduced the exact `default→emacs→default` keymap round-trip in the Keybinding Editor: default first-load = **875 bindings** (Plugin 392 / Keymap 261); after round-trip + reopen still **875 / Plugin 392** (the bug used to collapse to 547 / Plugin 0). Robust across a 2nd round-trip (`default→macos→default`). Commented "confirmed fixed in v0.4.1" on #2307; marked resolved in github_issues.md.
- **R2 (advance new coverage): per-buffer view toggles (`93ac8d5ff`) + global View-toggle persistence #474 (`233c5cb64`) — COMPREHENSIVE PASS, no bug.**

### NEW COVERAGE — Per-buffer "current buffer" view toggles + #474 persistence — PASS
tmux `view_r37` (200×50), two files (`file1.txt` 5 short lines; `file2.txt` w/ a 313-char long line). Read commit intent + in-app command descriptions first.
- Two palette cmds present (both builtin, no key): **"Toggle Line Numbers (Current Buffer)"**, **"Toggle Line Wrap (Current Buffer)"**.
- **Line Numbers (Current Buffer):** on file2 → `Line numbers hidden`, `N │` gutter removed. Switch to file1 → **file1 still shows line numbers** (per-buffer scope holds). Switch back → file2 still hidden.
- **Line Wrap (Current Buffer):** on file2 → `Line wrap disabled`, the 313-char line collapses from a wrapped continuation row to a single truncated row.
- **Persistence across restart:** set BOTH overrides on file2, quit (Ctrl+Q) + relaunch WITHOUT `--no-restore` → file2 restored with gutter hidden + wrap off; file1 (no override) still follows global default.
- **#474:** global **"Toggle Line Numbers"** (the non-"Current Buffer" cmd) now writes `{"editor":{"line_numbers":false}}` to `~/.config/fresh/config.json` and **survives restart** (file1, no override, shows no gutter after relaunch) — previously runtime-only and forgotten.

### Issues filed
- **None.** Both features behave correctly and as documented; #2307 confirmed fixed (comment only).

### tmux gotchas (→ learning_db)
- `--no-restore` skips SAVE too → next launch has nothing to restore. Test persistence WITHOUT `--no-restore` on both runs.
- Palette `C-u`-clear-then-type is unreliable (once ran nothing → `No selection`); open fresh with `C-p`, type a unique prefix, verify the single match, then Enter.

### Cleanup
- Killed tmux `view_r37` + `fresh_qa_r37`; removed `/tmp/view_test_r37`, `/tmp/kb_test_r37`; reset `~/.config/fresh/config.json`. `/tmp/fresh-build` worktree retained for next run's incremental build.

### NEXT (Run #38+, prefer freshest 0.4.1, top-down)
- `7d636f1de` clipboard strips ANSI on default copy; `6df567f04` LSP `textDocument/implementation`; `77360e6c6` Shift+letter binding match when terminal omits SHIFT; `e4a554347` terminal hides scrollbar/reclaims a column. Then prior backlog: (d) '+' new-tab popup / terminal Ctrl+Click / OSC 7; (e) theme color-transition; (f) GDScript (#2238). #2197 only if a fix lands.

---

## Run #33 — 2026-06-11

### Status: COMPLETED

### What Was Done
- Synced state (`tui-automated-testing-state`, pull --rebase clean / already up to date). **Preflight:** playbook integrity OK (PER-RUN LOOP, ANTI-DRIFT RULES, ISSUE FILING STANDARDS, FALSE POSITIVE PATTERNS all present); GitHub MCP — github server connected (tools available); lessons continuity OK.
- **Build directive:** origin/master still at `1b5d7f8c8` = v0.4.0 (unchanged since Run #31; forced-update from a9069ca69 but same HEAD). Built release binary in a fresh `/tmp/fresh-build` worktree @ `1b5d7f8c8` → `fresh 0.4.0` (8m11s). Per R1, binary version unchanged → skipped open-issue rechecks (no fix landed since Run #31).
- Per R2 advanced new-coverage candidate (b): **`lsp_enabled` master switch (#1770)**. Read CHANGELOG 0.4.0 + docs/features/lsp.md + docs/configuration/index.md first to nail the documented contract and avoid a false positive.

### NEW COVERAGE — `lsp_enabled` master switch (#1770) — COMPREHENSIVE PASS, no bug
**Setup:** real pyright-langserver (on PATH, uv-installed) on a small Python project `/tmp/lsptest/main.py`; config `~/.config/fresh/config.json` with `lsp.python` (`command pyright-langserver`, `args [--stdio]`, `enabled true`, `auto_start true`). Workspace auto-Trusted. Black-box checks = `pgrep -f pyright-langserver` (server start observable even though #2197 makes requests time out) + ANSI status-bar capture.
- **Config key:** top-level **`lsp_enabled`** (root, NOT `editor.lsp_enabled`).
- **CONTROL (key absent → default true):** launch `main.py` → status `LSP (python) ready`, right pill `LSP (on)`; pyright spawns TWO procs (python wrapper + node child). Proves config drives auto-start.
- **`lsp_enabled:false`:** relaunch → `pgrep` count **0** (no auto-start), no "ready" message, right pill `LSP (off)` for the configured Python language. ✓ exactly matches docs ("no language server auto-starts … status bar shows a … `LSP (off)` pill when servers are configured for the current language").
- **OVERRIDE:** with `lsp_enabled:false` still set, palette `Ctrl+P` → **"Start/Restart LSP Server"** (builtin) → pyright spawns (both procs), pill → `LSP (on)`, `LSP (python) ready`. ✓ matches docs "manual start overrides the global switch for that language."
- **Verdict:** All three documented behaviors correct. No bug. No false positive.

### Doc nit (R3 → potential_improvements IMP-020, NOT filed)
- docs/features/lsp.md calls it a "**dimmed** `LSP (off)` pill". ANSI capture (`capture-pane -p -e`) shows the off pill in the SAME default foreground as the on pill — no `[2m` dim attribute anywhere on the status line, no distinct color; only the word `off`/`on` differs (status bar bg `48;5;233`). Trivial cosmetic/doc mismatch → logged IMP-020, not issue-worthy per R3.

### State updates
- test_plan.md: added Run #33 COMPLETE note (candidate (b) done; next candidates (a) Review Diff reworked, (c) per-language indent rules, (d) '+' tab popup/Ctrl+Click/OSC7, (e) theme color-transition).
- learning_db.md: appended "lsp_enabled master switch (Run #33)" (config key, 3-behavior matrix, container LSP availability, exit-144 tmux gotcha).
- potential_improvements.md: appended IMP-020.
- github_issues.md: unchanged (no issue filed).

### Cleanup
- Killed tmux session `lsp33`, killed pyright procs, removed `/tmp/lsptest`, removed `~/.config/fresh/config.json`, removed `/tmp/fresh-build` worktree (`git worktree prune`). No stray sessions/processes/temp files.

### tmux gotcha (logged)
- Compound bash lines mixing `tmux send-keys … C-q` + `pkill` + a heredoc sometimes abort the whole script with **exit 144** before later commands run. Fix: run quit/`pkill`/cleanup as SEPARATE one-line Bash calls; write config files with the Write/Edit tool, not heredocs.

---

## Run #32 — 2026-06-10

### Status: COMPLETED

### What Was Done
- Synced state (`tui-automated-testing-state`, pull --rebase clean). **Preflight:** playbook integrity OK (all four AGENT_INSTRUCTIONS sections present); GitHub MCP auth LIVE (listed open agent issues); lessons continuity OK.
- **Build directive:** origin/master still at `1b5d7f8c8` = v0.4.0 (unchanged since Run #31). Built release binary in a fresh `/tmp/fresh-master` worktree @ `1b5d7f8c8` (`fresh 0.4.0`). Per R1, binary version unchanged → skipped open-issue rechecks (no fix landed; Run #31 already covered the 0.4.0 fix confirmations).
- Per R2 advanced THREE brand-new 0.4.0 features (all keyboard-driven + ANSI-verified), reading PRs #2152/#2153/#2154 first to nail expected behavior and avoid false positives.

### NEW COVERAGE 1 — Occurrence highlighting (#2154) — PASS on function, but FILED #2312
tmux `qa-occ-r32` (200×50), `/tmp/qa-r32/sample.py` (repeated `items`/`total`). Command palette **"Toggle Occurrence Highlight"** (builtin, no default key). i18n: `cmd.toggle_occurrence_highlight`, status `view.occurrence_highlight_state` ("Occurrence highlight enabled/disabled"). Verified: **ON by default** (#2154 says enabled by default); highlights are **whole-word** (`item` NOT matched when cursor on `items`); toggle on/off + status messages correct.
- **BUG FOUND → #2312:** the occurrence-highlight background is a **fixed color 16 (near-black) that ignores the theme.** Proven by an ON/OFF differential ANSI capture in **high-contrast** theme: toggling changes NOTHING on any non-current line (highlight color 16 == editor bg 16 → invisible); only the current-line word changes, drawn bg 16 = DARKER than the current-line bg 233 → looks recessed, not highlighted. In **light** theme, occurrences become inverted **black boxes** (bg 16 on white). Works fine only on dark themes (dark/dracula/nostalgia: bg 16 on ~234/235 = subtle box). Filed #2312 (bug, tui-agent-auto-bug), 4 dup-search variations, no dup.

### NEW COVERAGE 2 — Hide current-line highlight on selection (#2153) — PASS (config default false)
PR #2153 is a config **`editor.hide_current_line_on_selection`** (Display section), **default `false`** — the CHANGELOG line "current-line highlight now hides while text is selected" is opt-in, not automatic. Default behavior (highlight STAYS during selection) is therefore CORRECT, not a bug. Set the config (`{"editor":{"hide_current_line_on_selection":true}}`) + relaunch → verified: current line bg `235` (highlight) → drops to `234` (non-current) the instant a selection is non-empty, and returns to `235` when the selection is cleared. **GOTCHA:** the key is nested under `editor`, NOT top-level — a flat `hide_current_line_on_selection` is silently ignored (found the true path `/editor/hide_current_line_on_selection` via Settings UI search — avoided a false "doesn't work" report).

### NEW COVERAGE 3 — Clear Search action (#2152) — PASS
Palette **"Clear Search Highlights"** (`cmd.clear_search`, no default key). PR #2152: clears active search highlights without closing the find widget; also exposes `has_active_search()` to plugins. Verified the action works via a custom keybinding (F8 → `clear_search`): a search left highlighted (bg 228 match bg) is cleared by F8 (match returns to normal fg, no bg). Behavior map discovered: **Escape** closes the find bar AND clears highlights; **Enter** closes the find bar but PERSISTS the match highlights ("Found N matches for ..."); `clear_search` removes those persistent highlights. The "without closing the find widget" benefit is only reachable via a custom keybinding/plugin — invoking via the palette closes the find bar first, and the focused find INPUT swallows the keybinding (F8 ignored while find bar focused). Logged as IMP-019 (not a bug; action functions correctly).

### Issues filed
- **#2312** — Occurrence highlight uses a fixed near-black background that ignores the theme (invisible in high-contrast, inverted black box in light). bug + tui-agent-auto-bug.

### tmux / harness notes (→ learning_db)
- Keybinding config schema learned: `"keybindings":[{"key":"F8","action":"clear_search"}]`. Keybinding Editor flow: open via palette "Open Keybinding Editor" → `/` search action name → Enter (commit search) → Down to row → Enter (Edit dialog) → Enter (capture) → press key → it stages (`[modified]`, Source `custom`) → **Ctrl+S** to persist to config ("Keybinding changes saved"). The Edit-dialog Save button alone does NOT write config.
- Select Theme list opens with the CURRENT theme pre-selected (not the top). Navigate relative to current. Theme order: dark, dracula, high-contrast, light, nord, nostalgia, solarized-dark, terminal.
- Settings UI: `/` searches; results show the JSON path (e.g. `Editor > /editor/hide_current_line_on_selection`) — the fastest way to find the true config key for a setting.

### NEXT (Run #33+) new-coverage candidates (0.4.0, top-down)
- (a) **Review Diff reworked** (the flagship 0.4.0 feature): file sidebar w/ per-file status+change counts+comment badges grouped by dir; in-panel side-by-side; multi-line comments panel; watch mode `W` (reload on save); review a git stash; split/stack/auto layout; `/` file filter; Tab focus model, cross-file `n`/`p`. Needs a real git repo with multiple changed files.
- (b) **`lsp_enabled` master switch** (#1770) — set `editor.lsp_enabled:false` (likely under editor), verify NO LSP server starts (pyright on PATH; server-start observable even though #2197 times out requests).
- (c) **Configurable indentation rules** per language `[languages.<id>.indent]` incl. VS Code-style regex rules.
- (d) **'+' new-tab button** popup (New Terminal / New File) + **terminal Ctrl+Click opens file paths** + **OSC 7 cwd tracking** — mouse-dependent, likely tmux wall (Ctrl+Click), but OSC 7 cwd is observable via terminal tab name / cd behavior.
- (e) **color-transition animation on theme switch** (visual, ANSI frames).
Then #2197 only if a fix lands.

---

## Run #31 — 2026-06-10

### Status: COMPLETED

### What Was Done
- Synced state (`tui-automated-testing-state`, pull --rebase clean). **Preflight:** playbook integrity OK (all four mandated AGENT_INSTRUCTIONS sections present); GitHub MCP auth LIVE (github tools loaded); lessons continuity OK (learning_db topic-organized, intact through Run #30, NOT clobbered).
- **Build directive:** master advanced past Run #30's `232eceed7` → **forced update to `1b5d7f8c8` = "Bump version to 0.4.0"**. Built release binary from a fresh `/tmp/fresh-master` worktree of **origin/master @ 1b5d7f8c8** (`cargo build --release --bin fresh`, 6m19s, `fresh 0.4.0`).
- NEW 0.4.0 commits since Run #30: wave-animation idle screensaver (e39a5ccd3, 543e54502), **send-selection-to-terminal (6ac61f927, 4b4d14946)**, terminal Ctrl+Click path opening, OSC 7 cwd tracking, plus the full 0.4.0 feature set in CHANGELOG.
- Per anti-drift R2 advanced ONE new-coverage item: the brand-new **"Send Selection to Terminal"** feature (#1871, requested by @aquasync; freshest user-facing command). Read CHANGELOG 0.4.0 + `docs/features/terminal.md` (feature not yet documented there) + `crates/.../locales/en.json` i18n keys FIRST.

### NEW COVERAGE — Send Selection to Terminal (#1871) — COMPREHENSIVE PASS, no bug
tmux `qa-sendterm-r31` (200×50), clean dir `/tmp/sendterm-test`, real bash terminal in utility dock (Alt+`). Command palette: **"Send Selection to Terminal"** (builtin, no default keybinding) — desc "Run the selected text (or current line) in the most recently used terminal". i18n: `cmd.send_selection_to_terminal`, status `terminal.sent_selection` = "Sent to terminal %{id}". Cases verified:
1. **No terminal open** → graceful status "No open terminal — open a terminal first" (no crash, no auto-open). ✓
2. **No selection** → sends CURRENT LINE, appended with newline and EXECUTED (`first line text` → `bash: first: command not found`). ✓
3. **Single-line selection** → sends exactly that line, executed (`second line ABC` → command not found). ✓
4. **Multi-line selection (3 lines)** → sends ALL selected lines, each run individually (3 separate prompts/errors). ✓
5. **Partial sub-line selection** → sends exactly the selected substring; clean positive test: selected `pwd` → terminal output `/tmp/sendterm-test`. ✓
6. **Positive echo test** → selected `echo "LINE-ONE-MARKER"` → terminal printed `LINE-ONE-MARKER`. ✓ (unambiguous send+execute proof)
7. **Status message** "Sent to terminal 0" shown after every send. ✓
8. **Focus moves to terminal after send** (commit 4b4d14946) — VERIFIED: a printable key typed right after a send landed at the terminal prompt, not the editor buffer. ✓ Deliberate design.
9. **Targets most-recently-used terminal** (terminal 0). ✓
10. **Buffer never modified / no corruption** — tabs never showed `*`, content intact throughout. ✓
- **Pending terminal input is NOT cleared before sending** (leftover prompt text `CCC` + sent `pwd` → ran as `CCCpwd`). This matches VS Code "Run Selected Text in Active Terminal" — NOT a bug.
- **Could NOT drive the right-click context menu** `menu.terminal.send_selection` (Terminal submenu: Open/Close/Send Selection/Toggle Keyboard Capture) — SGR mouse right-click via tmux not passed through (harness limitation; relates IMP-009). Primary command-palette path fully covered.

### Observations (no issue filed, per R3)
- After send, focus is on the terminal but the **status bar still shows the editor pane's `Ln/Col` + filetype** (e.g. `Ln 1, Col 1 ... ASCII Text`) rather than terminal-mode hint. Cosmetic; same status-bar-staleness family as open **#2301**. Not re-filed.
- The auto-focus-to-terminal means sending a SECOND selection requires manually refocusing the editor first (Alt+J). VS Code keeps focus in the editor for rapid repeated sends. Borderline friction → logged to potential_improvements (IMP-018), not an issue.

### Open-issue recheck (v0.4.0)
- CHANGELOG 0.4.0 explicitly lists fixes already confirmed by this agent: #2212 (code-action diagnostics), #2165 (`q` closes Keyboard Shortcuts viewer), and "trust-level change resets only the active session" (= our #2291 fix). No re-test needed — all already marked FIXED in github_issues.md.
- No 0.4.0 commit touches the other OPEN agent-filed issues (#2111, #2135, #2197, #2221, #2301, #2307, #2309) → not fixed, skipped per R1. #2197 pyright: no fix in changelog.

### tmux gotchas (logged to learning_db)
- **CRITICAL for this feature:** "Send Selection to Terminal" moves keyboard focus to the TERMINAL. The next editor operation MUST be preceded by Alt+J (Toggle Utility Dock focus) / "Focus Terminal"+Alt+J — otherwise editor keystrokes leak into the terminal. (Cost me one bad partial-selection capture.)
- Verify editor focus before selecting by checking the status bar filetype (`Bourne Again Shell (bash)` for script.sh) AND the selection highlight `48;5;17m` after Shift+End.
- **Alt+J** = "Toggle Utility Dock" focus (editor ↔ bottom dock terminal); **Alt+`** opens terminal in the shared bottom dock so editor + terminal are visible together.

### Next run
- NEXT new-coverage candidates (0.4.0, prefer freshest top-down): (a) **wave-animation idle screensaver** — set a short idle config and verify Wave auto-triggers after idle + dismisses on input (builds on Run #30 Wave coverage; commits e39a5ccd3/543e54502; note "read idle time from injected time source" → may need config knob); (b) **terminal Ctrl+Click / Ctrl+hover opens file paths from output incl. scrollback** (6ac61f927 area) — needs mouse, may hit same tmux passthrough wall; (c) **'+' new-tab button popup (New Terminal / New File)**; (d) **Occurrence highlighting toggle** (#2154) + current-line highlight hides while selecting (#2153); (e) **Clear Search action** (#2152); (f) **Review Diff reworked** (file sidebar, comments, watch mode `W`, stash review). Then #2197 only if a fix lands.

## Run #28 — 2026-06-10

### Status: COMPLETED

### What Was Done
- Synced state (`tui-automated-testing-state`, pull --rebase clean). **Preflight:** playbook integrity OK (all four mandated AGENT_INSTRUCTIONS sections present); auth LIVE (`issue_read` #2291 returned); lessons continuity — note `learning_db.md` is topic-organized (no "Lesson N" markers; the run_log's Lesson 29/35/44 refs predate the reorg) — 58KB, content through Run #27 intact, NOT clobbered.
- **Build directive:** master moved past Run #27's `a9069ca6` → now `67d0c6e6c` (forced update; same `fresh 0.3.12` version string, recent commits are e2e test fixes). Built release binary from a fresh `/tmp/fresh-master` worktree of **origin/master @ 67d0c6e6c** (`cargo build --release --bin fresh`).
- Per anti-drift R2 advanced ONE `[ ]` backlog item: **PRIORITY #8 — Keybinding editor count anomaly** ("866 vs 548" from Run #22). tmux `fresh_qa_r28` (220×50), clean dir `/tmp/kb_test_r28`, `--no-restore`, no pre-existing config. Cleaned up after.

### PRIORITY #8 — Keybinding editor count anomaly — ROOT-CAUSED + FILED #2307
Reproduced and explained the Run #22 "866 vs 548":
- Simple repeated opens of the Keybinding Editor are STABLE at **866** (Builtin group 400) on a clean `default` keymap — anomaly does NOT come from reopening.
- Source/Context filters never change the denominator (always `N/866 shown`).
- **Per-map first-load totals are each stable & correct:** default **866** (Source Plugin 391/866, Keymap 260/866), emacs **519**, macos **600**. Differing counts *between maps* are expected.
- **The bug:** a SINGLE "Select Keybinding Map" round-trip back to an already-loaded map drops the count and wipes the plugin layer. `default → emacs → default` → reopen editor = **547 bindings**, **Source[Plugin] = 0/547** (all 391 plugin bindings gone), Source[Keymap] still 260. 100% reproducible; persists across reopens + 3s wait; **app restart restores 866**.
- **Functional check:** plugin bindings still WORK after the round-trip — `Alt+O` (Toggle Orchestrator Dock Focus, a plugin binding) still opens the dock. ⇒ Keybinding-Editor listing/reporting defect, not loss of function.
- Filed **#2307** (`bug`, `tui-agent-auto-bug`) with all four mandated sections + 4 search queries (no dup; not in github_issues). Recorded as BUG-009. learning_db section "Keybinding Map Switching + Editor Count Bug (Run #28)" added.

### tmux gotchas (logged to learning_db)
- Palette key is KEYMAP-DEPENDENT: default/macos/vscode = `Ctrl+P`, **emacs = `M-x`** (Ctrl+P leaks into buffer under emacs). Open the Keybinding Editor keymap-independently via Edit menu: `F10 → Right → Up (wraps to last item) → Enter`.

### False Positive Rate: 0% (1 of 1 bugs filed is a confirmed, reproducible defect)

### R1 note
Version string unchanged (still 0.3.12) and the commit delta from a9069ca6→67d0c6e6c is e2e-test-only; did NOT re-run passing sprints or open-issue rechecks (no user-visible behavior change). #2197 pyright recheck deferred — only act when a fix lands (check issue status first).

---

## Run #27 — 2026-06-10

### Status: COMPLETED

### What Was Done
- Synced state (`tui-automated-testing-state`, pull --rebase clean). **Preflight:** playbook integrity OK (all four mandated sections present); lessons continuity OK; GitHub MCP auth LIVE (`list_issues` labeled `tui-agent-auto-bug` → 8 open returned). Binary version **unchanged** since Run #26 (origin/master @ `a9069ca6` = v0.3.12), so per **R1** did NOT re-verify passing items / open-issue rechecks (no behavior change possible).
- Rebuilt v0.3.12 from a fresh `/tmp/fresh-build` worktree of **origin/master @ a9069ca6** (`cargo build --release --bin fresh`, ~7m).
- One NEW-coverage backlog item advanced (per Run #17 directive): **PRIORITY #6 — Open file from a diff (0.3.12).** tmux `fresh-difftest-r27` (220×50). Real git repo `/tmp/difftest` (committed `calc.py`, then working-tree edits → +5/-1 hunk). Cleaned up after.

### PRIORITY #6 — Open file from a diff — COMPREHENSIVE PASS (one related display glitch)
Feature path discovered black-box: **Review Diff** (unified) → Enter on a hunk opens a **side-by-side `*Diff: <file>*` view** (`OLD (HEAD)` left / `NEW (Working)` right). In side-by-side:
- **OLD pane → Enter** → opens read-only `*HEAD:<file>*` buffer, status `Opened HEAD version (read-only) at line N`. **Cursor lands on the correct HEAD line** (verified via highlighted row `48;5;233` + `tmux display-message cursor_y`). ✓
- **Alt+o** (NEW/Working action) → opens the **working-tree file** (`calc.py`), status `Opened calc.py`, cursor at the line. ✓
- Header legend confirms: OLD `[Enter] open this version`, NEW `[Enter/Alt+o] open file`. Alt+o is the universal "open working file" shortcut; NEW-pane focus + Enter is equivalent.

### Finding (NOT a new issue — commented on existing #2301)
The status-bar **line/col is stale (`Ln 1, Col 1`) immediately after the diff→open-file jump**, despite the cursor being physically on the correct line; it self-corrects on the next cursor-movement keypress. Reproduced **2/2**. This is the SAME root-cause family as **#2301** (which I'd assumed was Go-to-LSP-Symbol-specific). Since #2301 is open and this is the same status-bar-refresh path, I added a comment to #2301 broadening its scope (no LSP involved here) rather than filing a duplicate (R3). The feature under test itself is correct — cursor navigation works; only the status readout lags one keypress.

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| Review Diff → Enter opens side-by-side view | **PASS** | `*Diff: calc.py*`, OLD(HEAD)/NEW(Working) panes, word-level aligned |
| OLD pane Enter → read-only HEAD version at line | **PASS** | New `*HEAD:<file>*` [RO] tab; cursor on correct line (verified physically) |
| Alt+o → working-tree file at line | **PASS** | Opens `calc.py`, status `Opened calc.py` |
| Status bar line/col after diff-open jump | **GLITCH** | Stale `Ln 1, Col 1` until next keypress — #2301 family; commented, not re-filed |

### Anti-drift compliance
- R1: no idle re-verification (binary unchanged → skipped open-issue rechecks).
- R2: advanced a `[ ]` backlog item to `[x]` (priority #6).
- R3: low-sev display glitch → comment on existing issue, not a new one.

### Cleanup
- Killed tmux `fresh-difftest-r27`; removed `/tmp/difftest`; removed `/tmp/fresh-build` worktree.

### NEXT
- Priority #8 **Keybinding editor count anomaly** (re-observe 866 vs 548 total bindings across opens; file only if reproducible with steps).
- Then priority #4 **#2197 pyright recheck** (only if a fix landed — still open/in-progress).
- Note for next run: when in side-by-side diff, the **NEW-pane focus via Tab is unreliable over tmux** (Tab pushed cursor_x to far-right/COMMENTS column); use **Alt+o** for the working-file path instead.

---

## Run #26 — 2026-06-10

### Status: COMPLETED

### What Was Done
- Synced state (`tui-automated-testing-state`, pull --rebase clean). **Preflight:** playbook integrity OK (all sections present); lessons continuity OK (titled sections, not numbered "Lesson N"); GitHub MCP auth LIVE (`issue_read` #2301 returned, still open).
- Fresh container: `/tmp/fresh-build` worktree gone (ephemeral). Recreated worktree from **origin/master @ a9069ca6** and rebuilt **v0.3.12** (`cargo build --release --bin fresh`, ~6m). Worktree retained for next run.
- Two NEW-coverage backlog items advanced (per Run #17 directive). tmux `fresh-r26` (220×50). Test files `/tmp/rainbow_test.js` + `/tmp/rainbow_edge.js` (removed after).
- **PRIORITY #5 — Rainbow bracket colorization — COMPREHENSIVE PASS, no bug.** ANSI capture (`-p -e`) of nested/mixed/deep/unbalanced brackets.
- **PRIORITY #7 — Terminal tab auto-naming — PASS, no bug.** Verified fg-process following + OSC-title following.

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| Rainbow: depth-based coloring | **PASS** | 6-color cycle `[6,2,3,126,15,27]` repeating; colored by nesting depth not bracket type |
| Rainbow: matching pairs share color | **PASS** | `((((( )))))` opens 6,2,3,126,15 → closes mirror exactly |
| Rainbow: across bracket types | **PASS** | `[ { ( [ ( { } ) ] ) } ]` → 6,2,3,126,15,27 + exact mirror |
| Rainbow: deep (11-level) nesting | **PASS** | cycles+repeats, all 11 closers mirror openers |
| Rainbow: unbalanced/extra brackets | **PASS** | stray open keeps depth color; stray closers all depth-0; no crash/cascade |
| Rainbow: viewport-wide | **PASS** | all visible lines colored regardless of cursor |
| Terminal: tab auto-name on open | **PASS** | `bash — root@vm: /home/user/fresh` (`<fg> — <OSC title>`) |
| Terminal: follows foreground process | **PASS** | `python3` → `python3 — …`; `exit()` → reverts to `bash — …` |
| Terminal: follows OSC title | **PASS** | after clearing PROMPT_COMMAND, custom OSC → `bash — HELLO-FROM-OSC` |

### Issues Filed / Comments
- **None.** Both features behave correctly and as documented. No bug, no usability issue meeting the filing bar. (Earlier apparent "OSC title doesn't stick" was bash's PROMPT_COMMAND overwriting it — standard shell behavior, verified non-bug.)

### Cleanup
- tmux `fresh-r26` killed; `/tmp/rainbow_test.js` + `/tmp/rainbow_edge.js` removed; `/tmp/fresh-build` worktree retained for next run's incremental build.

### Next Run
- Priority #6 **Open file from diff** (Enter in side-by-side/review-diff opens working-tree NEW pane / read-only HEAD OLD pane at that line) — needs a git repo with a modified file + Live Diff or Review Diff view.
- Then #8 Keybinding editor count anomaly (866 vs 548 total bindings between opens — repro with steps or drop).
- Then #4 #2197 pyright recheck (only if a fix landed since Run #17).

---

## Run #25 — 2026-06-10

### Status: COMPLETED

### What Was Done
- Synced state (`tui-automated-testing-state`, pull --rebase clean). **Preflight:** playbook integrity OK; lessons continuity OK (learning_db uses titled sections, not numbered "Lesson N"); GitHub MCP auth LIVE (search_issues returned results; #2301 created successfully).
- Fresh container: clangd absent + `/tmp/fresh-build` worktree gone (ephemeral). Re-installed clangd 18 (`apt-get install -y clangd` → 18.1.3) and rebuilt **v0.3.12 from origin/master @ f4ee3630** in a fresh `/tmp/fresh-build` worktree (`cargo build --release --bin fresh`, ~6m44s).
- Built a real small C project `/tmp/sym_test25` (shapes.c: Point/Rectangle structs, globals, point_distance/rectangle_area/make_rectangle/print_rectangle/main; helpers.c: helper_add/multiply/greeting; compile_commands.json; git init). Config `{"lsp":{"c":{"command":"clangd","auto_start":true,...}}}`. tmux `fresh-r25` (220×50).
- **PRIORITY #3 — Go to LSP Symbol (0.3.12) — COMPREHENSIVE PASS + 1 low-sev bug.** Trusted via dialog (T+Enter → restart, `LSP (on)`, inlay hints render = clangd auto-started). Drove the symbol finder fully via tmux.

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| Palette "Go to LSP Symbol" command present | **PASS** | source `lsp_navigation`; desc "List document symbols from LSP and navigate to selected" |
| Document-symbol list w/ kind tags | **PASS** | `[class]`/`[field]`/`[var]`/`[fn]` + source-line preview; clangd emits each struct twice (typedef+tag) |
| Live substring filter | **PASS** | `print`→print_rectangle, `main`→main, `make`→make_rectangle |
| Live preview (editor follows selection) | **PASS** | editor scrolls + highlights symbol (name `38;5;226m`, list row `48;5;25m`); arrow nav re-previews |
| Enter = commit jump | **PASS** | cursor physically lands on symbol line (verified via `tmux display-message #{cursor_y}`) |
| Escape = cancel + restore | **PASS** | cursor returns to exact pre-open position (Ln16→preview→Esc→Ln16) |
| Document-scoped (not workspace) | **PASS** | `helper` (helpers.c) → "No matches"; matches its description, NOT a bug |
| Status bar Ln after jump | **BUG #2301** | `Ln` stale (keeps pre-jump line), `Col` updates; corrects on next move |
| Comparison: F12 Go to Definition | **PASS** | updates `Ln` immediately + "Jumped to definition at …:N" |
| Comparison: Ctrl+G Go to Line | **PASS** | updates `Ln` immediately |

### Issues Filed / Comments
- **NEW BUG #2301** filed (labels `bug`, `tui-agent-auto-bug`): "Go to LSP Symbol: status bar line number stays stale after jump (only column updates) until next keypress." Reproduced 3/3; feature-specific (F12/Ctrl+G unaffected); clear expected-vs-actual; 4 GitHub search variations logged, no duplicate. Low severity (display-only, self-corrects) but a real behavioral inconsistency in a new 0.3.12 feature.

### Key Findings
1. **Go to LSP Symbol is a polished document-symbol finder** with genuine live preview + restore-on-cancel — works well end-to-end with clangd.
2. **Lone defect:** the status-bar line readout doesn't refresh on the symbol-jump path (column does), so right after the navigate-here command the line is briefly wrong → #2301.
3. **Harness notes:** pre-seeding trust.json with percent-encoded path does NOT work (Fresh's encoding differs) — trust via dialog. `auto_start:true` + Trusted → clangd auto-launches (no manual Start).

### Version
- Binary: v0.3.12 built from origin/master @ f4ee3630 (2026-06-10)

### Cleanup
- tmux `fresh-r25` killed; `/tmp/sym_test25` removed; stray pre-trust dir removed. `/tmp/fresh-build` worktree retained for next run's incremental build.

---

## Run #24 — 2026-06-10

### Status: COMPLETED

### What Was Done
- Synced state (`tui-automated-testing-state`, pull --rebase clean). **Preflight:** GitHub MCP auth OK (read #2291, still open/awaiting close); playbook integrity OK; lessons continuity OK.
- Reused `/tmp/fresh-build` worktree, refreshed to **origin/master @ f4ee3630 (v0.3.12)** and rebuilt (`cargo build --release --bin fresh`, ~6.5 min).
- tmux `fresh-r24` (220×50) on a real throwaway git project (`/tmp/orch_test24`, 1 commit, main.rs/README; `-c commit.gpgsign=false` to dodge this env's signing enforcement).
- **PRIORITY #2 — Orchestrator Dock (0.3.12) — COMPREHENSIVE PASS. No bugs.** Drove every documented surface via tmux (incl. SGR mouse injection for right-click/click).

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| Alt+O toggle/focus dock | **PASS** | Persistent non-modal left column; underlined "Orchestrator" mnemonic |
| Session card (status/project/branch/git) | **PASS** | `· orch_test24 / ▸ master / clean`; `·` idle vs `*` active glyph |
| Arrow live-switch (Up/Down) | **PASS** | Editor pane retargets session ↔ session with NO restart; bidirectional |
| View toggle card ↔ compact | **PASS** | Enter on `[ view: card ]` → compact 1-line rows |
| Project dropdown `[ All ▾ ]` | **PASS** | `● All projects` + per-project rows |
| Filter (`/`) | **PASS** | "test24-1" narrows list to that row live |
| Manage → full Orchestrator dialog | **PASS** | Sessions panel + detail panel (Visit/Details/Stop/Archive/Delete) |
| Right-click context menu | **PASS** | Cursor-anchored popup: Visit…/Archive/Delete + "Esc to close" |
| Archive confirmation | **PASS** | Centered "Confirm Archive" (SIGKILL/close/move to .archived/, reversible); cancelled |
| New Session dialog — 4 types | **PASS** | Local/SSH/Kubernetes/Devcontainer each reflow type-specific fields; ←/→ live-switch |
| New Session auto-detect | **PASS** | Path=cwd, Name=`<proj>-N`, Branch=HEAD; worktree checkbox auto-disables on non-git |
| Devcontainer create (no config) | **PASS** | Graceful error: "run 'Dev Containers: Reopen in Container'" |
| Create Local worktree session | **PASS** | Worktree under `~/.local/share/fresh/orchestrator/...`; editor switches to it |
| Keyboard Create Session activation | **PASS** (no bug) | Tab→blue-bg focus→Enter creates; earlier "fail" was measurement error (was on Branch field) |

### Issues Filed / Comments
- **None.** Orchestrator Dock matches its documented behavior end-to-end. **Avoided a false positive:** initially suspected keyboard activation of `[ Create Session ]` was broken (button shows no focus highlight when unfocused), but verified it DOES take blue-bg focus on Tab and Enter then creates the session — a measurement error, not a defect. Verified before filing per the playbook.

### Key Findings
1. **Orchestrator Dock fully functional** — Alt+O dock with arrow live-switching (no restart) is the marquee 0.3.12 feature and it works; Manage opens the legacy full dialog; right-click + Archive/Delete confirmations all present.
2. **New Session dialog is a polished multi-type form** (Local/SSH/k8s/Devcontainer) with sensible auto-detection and per-type field reflow.
3. **tmux harness gotcha:** `S-Tab` is inserted as literal text "S-Tab" into focused fields in this tmux build — must use `BTab` for Shift+Tab. (Logged to learning_db; relevant to every future dialog test.)

### Version
- Binary: v0.3.12 built from origin/master @ f4ee3630 (2026-06-10)

### Cleanup
- tmux `fresh-r24` killed; `/tmp/orch_test24` removed; 2 orchestrator worktrees (`orch_test24-1`, `orch_test24-2`) removed via `git worktree remove --force` + prune; `~/.local/share/fresh/orchestrator/tmp_orch_test24` removed. `/tmp/fresh-build` worktree retained for next run's incremental build.

---

## Run #23 — 2026-06-10

### Status: COMPLETED

### What Was Done
- Synced state; built release binary from **origin/master @ f4ee3630** (**v0.3.12**, ~6 min) in a `/tmp/fresh-build` worktree (state branch stays checked out). master moved past Run #22's b022a7fc.
- **Preflight:** GitHub MCP auth OK (read #2291). Playbook integrity OK; lessons continuity OK.
- **Resumed an interrupted prior Run #23:** a previous invocation today (08:25Z, same f4ee3630 commit) already rechecked #2291 and commented "confirmed fixed" but never committed its state (run_log/github_issues still said Run #22). I completed the run without re-commenting on #2291.
- Created tmux session `fresh-r23` (220×50) on a real git C project (`/tmp/trust_dive23`, 2 commits, `compile_commands.json` trust trigger, clangd 18 installed).
- **WORKSPACE TRUST DEEP-DIVE (priority #1) — full 3-state enforcement matrix mapped:**
  - **Dialog is richer than Run #22 documented:** now has explicit `[ OK ]` / `[ Quit (Ctrl+Q) ]` buttons and per-option descriptions that spell out the enforcement contract (Restricted = "Runs system tools on PATH (git, ripgrep, system python); Blocks: project-local executables ./gradlew/.venv/bin/python/node_modules/.bin/*, env activation .env/.envrc/mise, and language servers"; Block All = "Nothing runs"). Letter (T/K/B) selects radio; **Enter confirms** the selected radio (no need to Tab to OK).
  - **Restricted (default):** LSP gated OFF (`not auto-started: workspace is not trusted`); **git ALLOWED** (git blame → 3 correct multi-commit blocks; git_explorer/git_gutter/merge_conflict spawn fine); **ripgrep ALLOWED** (Live Grep found "hello" with both `git-grep` and `rg` providers). Status bar: `Restricted`.
  - **Block All Execution (B):** EVERYTHING blocked — process layer returns `Process error: workspace trust is set to Blocked — no processes may run` (exit -1) for every git spawn; git blame → "No blame information available"; Live Grep → "No matches"; LSP off. Status bar: `Blocked`. Confirming Block All triggers the same editor restart (File Explorer auto-opens) but **preserves the open file** (#2291 fix holds here too).
  - **Trusted (T):** ungates tooling — `clangd-lsp` plugin loads & registers commands (clangdProjectSetup/SwitchSourceHeader) which it does NOT in Restricted/Blocked; "not trusted" gate messages stop. clangd binary stays dormant only because `auto_start` defaults false (IMP-013), so status still reads `LSP (off)`. Status bar: `Trusted`. trust.json → `{"level":"trusted"}`.
  - **Palette surface:** only ONE command — "Workspace Trust…" (opens the dialog). No direct trust/restrict/block palette commands; the `workspace_trust_block` action is not palette-exposed.
- **#2291 recheck (via UI):** CONFIRMED FIXED — directly observed main.c surviving TWO trust-triggered restarts (→Block All, →Trusted). Prior Run #23 already commented; did not duplicate. Marked resolved in state.

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| Trust dialog UI (buttons + option semantics) | **NEW DETAIL** | `[OK]`/`[Quit]` buttons; full enforcement contract in option descriptions |
| Restricted: LSP gating | **PASS** (blocked) | `not auto-started: workspace is not trusted` |
| Restricted: git blame | **PASS** (allowed) | 3 multi-commit blocks, correct attribution |
| Restricted: Live Grep (git-grep + rg) | **PASS** (allowed) | both providers return matches |
| Block All: git blame | **PASS** (blocked) | "No blame information available"; process denied in log |
| Block All: Live Grep | **PASS** (blocked) | "No matches"; process denied in log |
| Block All: process layer | **PASS** | `workspace trust is set to Blocked — no processes may run` |
| Block All: file preservation across restart | **PASS** | main.c kept (#2291 fix) |
| Trusted: tooling ungated | **PASS** | clangd-lsp plugin loads/registers; gate msgs stop |
| #2291 restart data-loss | **CONFIRMED FIXED** (v0.3.12) | file survives restart; prior Run #23 comment stands |

### Issues Filed / Comments
- None this run. No new behavioral bug (enforcement matches the dialog's documented contract). One low-severity UX note → potential_improvements (IMP-015): Blocked-mode tool failures show generic messages ("not a git file or error", "No matches") instead of "blocked by workspace trust".

### Key Findings
1. **Workspace Trust is a 3-state, process-layer enforcement** (Trusted/Restricted/Blocked) and it works correctly. Restricted is the interesting middle: PATH tools (git, ripgrep) run, but project-local executables, env activation, and language servers are gated.
2. **Block All denies at the spawn layer** for ALL processes with a clear log message; user-facing tools degrade gracefully but without telling the user WHY (the "Blocked" status bar is the only hint).
3. **Trusting ungates the LSP plugin layer**, but actual LSP start is still governed by `auto_start` (so trusting alone doesn't start clangd in this config).
4. **#2291 fix verified independently** by watching the open file survive two trust restarts.

### Version
- Binary: v0.3.12 built from origin/master @ f4ee3630 (2026-06-10)

### Cleanup
- tmux `fresh-r23` killed; `/tmp/trust_dive23` removed; `~/.local/share/fresh/workspaces/*` (trust.json) removed; `/tmp/fresh-build` worktree retained for next run's incremental build (optional cleanup).

---

## Run #22 — 2026-06-09

### Status: COMPLETED

### What Was Done
- Synced state; built release binary from **origin/master @ b022a7fc** (**v0.3.12**, ~5 min). NOTE: state branch builds only v0.3.8 — always build from origin/master now.
- Created tmux session `fresh-test-run22` (220×50)
- **Preflight:** GitHub MCP auth OK. Playbook integrity OK. Discovered #2165, #2212 closed by maintainer (completed) and #2113 closed (not_planned) since Run #21.
- **#2165 recheck** — CONFIRMED FIXED in v0.3.12: 'q' in *Keyboard Shortcuts* closes the buffer ("Tab closed"). Comment added.
- **#2212 recheck** — CONFIRMED FIXED in v0.3.12: clangd repro project → Alt+. at unused-include diagnostic shows "remove #include directive quickfix"; applying it edits the buffer ("Applied: remove #include directive (1 change(s))"). Comment added.
- **NEW FEATURE discovered: Workspace Trust prompt** (enforcement now ON; was "groundwork, off by default" in 0.3.10 CHANGELOG). Dialog: Trust(T)/Keep Restricted(K)/Block(B), Enter confirms, Esc does NOT dismiss. Persisted in `~/.local/share/fresh/workspaces/<encoded-path>/trust.json`.
- **NEW BUG FOUND + FILED #2291**: choosing "Trust folder & Allow Tooling" triggers a full editor restart (`Restart requested with new working directory` in log). With `--no-restore`, the CLI-opened file AND unsaved edits are silently discarded (no save prompt; recovery chunk written but never offered). Default mode survives via session restore (File Explorer auto-opens though). 3/3 reproducible.
- **SSH scp-style END-TO-END** — Installed openssh-server, ran sshd on localhost:22 with key auth. `fresh --no-restore root@localhost:/tmp/file.txt` → **FULL PASS**: status bar shows `root@localhost`, remote content loads, edit + Ctrl+S writes through ("Saved", on-disk file updated).
- **#2221 recheck (ssh:// URL form)** — STILL BROKEN in v0.3.12 even with working sshd: opens empty buffer, "Local | ssh://...". Comment added with the ssh-binary-present data point.
- **Keybinding editor: Delete binding** — PASS. Added F9→save (verified working), then 'd' on the row → "Custom binding removed" → Ctrl+S → `keybindings` key cleanly removed from config.json → F9 verified inert again.
- **Keybinding editor: Record Key Search** — PASS. 'r' → press key → filters list (Ctrl+S → 3/866 across contexts; F9 → 1/866). CAVEAT: record mode captures arrows too, so you cannot navigate results while in it; use '/' text search + Enter to act on results.
- **#2122 recheck** — STILL OPEN in v0.3.12: move_to_paragraph_down/up have empty Key column; select_to_paragraph_* still Ctrl+Shift+↓/↑.
- **#2113** — closed by maintainer as not_planned → monitoring item retired.

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| #2165 *Keyboard Shortcuts* 'q' | **CONFIRMED FIXED** (v0.3.12) | "Tab closed"; comment added |
| #2212 Code Actions diagnostics | **CONFIRMED FIXED** (v0.3.12) | Fix popup + apply work end-to-end; comment added |
| Workspace Trust prompt | **NEW FEATURE** | T/K/B radio + Enter; trust.json persistence; LSP gated on trust |
| Trust → file/edits dropped (--no-restore) | **BUG (#2291 filed)** | Editor restart discards CLI file + unsaved edits, silently |
| SSH scp-style end-to-end | **PASS** | Real sshd; open/edit/save round-trip verified |
| #2221 ssh:// URL form | **STILL BROKEN** | Treated as local path even with working sshd; comment added |
| Keybinding editor: Add (F9→save) | **PASS** | Tab×2 from Action needed to reach [Save] button |
| Keybinding editor: Delete binding | **PASS** | 'd' + Ctrl+S; config.json key removed; F9 inert |
| Keybinding editor: Record Key Search | **PASS** | Filters by pressed key; arrows captured while active |
| #2122 move_to_paragraph binding | **STILL OPEN** | No binding in v0.3.12 |

### Issues Filed / Comments
- **#2291** (new): "Workspace Trust: choosing 'Trust folder & Allow Tooling' restarts the editor and silently discards the opened file and unsaved edits (with --no-restore)"
- Comments: #2165 (confirmed fixed), #2212 (confirmed fixed), #2221 (still broken with working sshd)

### Key Findings
1. **Workspace Trust enforcement is live in v0.3.12** and gates LSP auto-start ("LSP for 'cpp' not auto-started: workspace is not trusted"). compile_commands.json is a trust trigger.
2. **Trust confirm = full editor restart.** Relies on session persistence to rebuild the session; with --no-restore that means silent data loss (#2291).
3. **SSH remote editing via scp-style works end-to-end** against a real sshd (status-bar origin, content, write-back all correct).
4. **Keybinding editor add/delete/record all functional**; quirks noted in potential_improvements (focus path to Save button, transient unresponsiveness after Add dialog, record-mode arrow capture).
5. **Observation (unconfirmed, low priority):** keybinding editor total count differed between opens in one session (866 vs 548 bindings) — possibly plugin lazy registration; re-observe before treating as a bug.

### Version
- Binary: v0.3.12 built from origin/master @ b022a7fc (2026-06-09)

### Cleanup
- tmux session `fresh-test-run22` killed; sshd stopped; /tmp test dirs removed; config.json reset to `{}`; trust.json + recovery chunks for test workspaces removed

---

## Run #21 — 2026-06-03

### Status: COMPLETED

### What Was Done
- Synced state from `tui-automated-testing-state`; built release binary from `claude/awesome-clarke-fYwrE` (**v0.3.10**, ~6.5 min build)
- Created tmux session `fresh-test-run21` (220×50)
- **Preflight:** GitHub MCP auth confirmed (9 open/filed issues after filing #2221). Playbook integrity confirmed.
- **SSH features** — Tested both URI forms. URL-style `ssh://` DOES NOT WORK (treated as local path, BUG #2221). scp-style `user@host:/path` works correctly (triggers SSH path, fails at ssh-binary-not-found).
- **Keybinding editor** — Full workflow tested: open editor, search, add binding (F9→save, normal context), save with Ctrl+S, verify F9 triggers save. All PASS.
- **Search in selection** — NOT IMPLEMENTED. Find bar has only Case/Whole Word/Regex options. No "In Selection" toggle.
- **Multi-root workspaces** — PASS. Workspace scoping correct (file picker shows only CWD files). Cross-workspace file opens and appears in project-wide search with full path.
- **#2165 recheck** — CONFIRMED STILL OPEN in v0.3.10 (`claude/awesome-clarke-fYwrE`): 'q' in *Keyboard Shortcuts* still shows "Editing disabled".
- **#2113 recheck** — NOT REPRODUCED in 8 more attempts (16 total across all runs).

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| SSH URL-style `ssh://` | **BUG (#2221)** | Treated as local relative path; no connection; no error |
| SSH scp-style `user@host:/path` | **PASS** (partial) | Correctly detects SSH; fails at ssh-not-found (no ssh binary installed) |
| Keybinding editor: Open | **PASS** | 852 bindings, Config path shown |
| Keybinding editor: Search (/) | **PASS** | "10/852 shown" filter works for 'save' |
| Keybinding editor: Add binding (F9→save) | **PASS** | Dialog: key capture → action autocomplete → context cycle → Save |
| Keybinding editor: Save (Ctrl+S) | **PASS** | "Keybinding changes saved"; config.json updated correctly |
| Keybinding editor: Binding works | **PASS** | F9 triggers file save ("Saved" in status bar) |
| Search in selection | **NOT IMPLEMENTED** | No "In Selection" toggle in find bar |
| Multi-root: workspace scoping | **PASS** | File picker shows only CWD files |
| Multi-root: cross-workspace file open | **PASS** | Ctrl+O opens outside workspace; full path shown |
| Multi-root: project-wide search | **PASS** | Alt+A includes all open buffers (in- and out-of-workspace) |
| #2165 *Keyboard Shortcuts* 'q' | **CONFIRMED STILL OPEN** | v0.3.10 / claude/awesome-clarke-fYwrE |
| #2113 race condition | **NOT REPRODUCED** | 8 more attempts; 16 total without reproduction |

### Issues Filed / Comments
- **#2221** (new): "SSH URL-style URI (`ssh://host/path`) treated as local file path instead of triggering SSH connection"

### Key Findings
1. **SSH URL-style form is broken**: `fresh ssh://host/path` silently opens an empty local file. Log confirms path resolved as CWD + URI. scp-style correctly triggers SSH (fails gracefully if `ssh` binary missing). Filed as #2221.
2. **Keybinding editor fully functional**: Add/edit/save/test cycle all work. Autocomplete for action names works. Context field cycles with ←/→. Ctrl+S saves to config.json. Added F9→save binding verified working.
3. **Search in selection not implemented**: Fresh's search bar has no "In Selection" option. Documented as IMP-014.
4. **Multi-root workspaces work correctly**: Workspace scoping (file picker), cross-workspace file opening, and project-wide search all work as expected.

### Version
- Binary: v0.3.10 built from `claude/awesome-clarke-fYwrE` (2026-06-03)

### Cleanup
- tmux session `fresh-test-run21` killed
- Config reset to `{}`
- Temp files: /tmp/multiroot_a/, /tmp/multiroot_b/, /tmp/search_in_sel_test.txt

---

## Run #20 — 2026-06-03

### Status: COMPLETED

### What Was Done
- Synced state from `tui-automated-testing-state`; built release binary from `claude/awesome-clarke-57Uge` (**v0.3.10**, ~6.5 min build)
- Created tmux session `fresh-test-run20` (220×50)
- **Preflight:** GitHub MCP auth confirmed (8 open/filed issues). Playbook integrity confirmed. All sections of AGENT_INSTRUCTIONS.md present.
- **#2165 recheck** — *Keyboard Shortcuts* 'q' CONFIRMED STILL OPEN in v0.3.10 ("Editing disabled in this buffer")
- **text-actions plugin** — Installed from GitHub URL (network available). Tested ALL decode commands. Discovered new decode commands not previously documented.
- **#2212 recheck on v0.3.10** — CONFIRMED STILL OPEN. LSP log shows `"context":{"diagnostics":[]}` still empty in v0.3.10. Comment added to GitHub issue #2212.
- **Bookmarks (Alt+0-9)** — Full test of all slots: set bookmarks 0, 1, 5, 9; tested jumping with Alt+0/1/5/9; tested unset slot (Alt+2 → "Bookmark '2' not set").
- **Keyboard macros** — Recorded complex 5-action macro (slot 3): SmartHome + InsertChar('#') + InsertChar(' ') + MoveDown + SmartHome. Played back on 5 lines. Verified via List Macros.
- **Markdown preview** — Toggled compose mode. Verified bold/italic ANSI rendering, inline code, code blocks with syntax highlighting, blockquotes, lists, HR. Editing inside code blocks works.

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| #2165 *Keyboard Shortcuts* 'q' | **STILL OPEN** | "Editing disabled" in v0.3.10 |
| text-actions: Decode Base64 | **PASS** | "SGVsbG8gV29ybGQ=" → "Hello World" |
| text-actions: Decode URI Component | **PASS** | "Hello%20World%21" → "Hello World!" |
| text-actions: Decode JSON String | **PASS** | `"Hello\nWorld\t!"` → multiline with newline+tab |
| text-actions: Decode Hex to JSON | **PASS** | "48656c6c6f" → "[72,101,108,108,111]" |
| text-actions: Encode→Decode round-trip | **PASS** | "Fresh Editor 2026" → Base64 → back = correct |
| #2212 Code Actions (v0.3.10) | **STILL OPEN** | `context.diagnostics` still empty; comment added to #2212 |
| Bookmarks: Set (0, 1, 5, 9) | **PASS** | "Bookmark 'N' set" for each |
| Bookmarks: Jump (Alt+0/1/5/9) | **PASS** | "Jumped to bookmark 'N'" at correct lines |
| Bookmarks: Unset slot (Alt+2) | **PASS** | "Bookmark '2' not set" |
| Keyboard macros: Record (slot 3) | **PASS** | 5-action macro; "Macro '3' saved (5 actions)" |
| Keyboard macros: Playback (F4) | **PASS** | Applied "# " prefix to 5 lines correctly |
| Keyboard macros: List Macros | **PASS** | `*Macros*` buffer shows SmartHome/InsertChar/MoveDown |
| Markdown: Toggle Compose mode | **PASS** | "Markdown Compose: ON (soft breaks, centered)" |
| Markdown: Bold/Italic ANSI | **PASS** | `**bold**` → `[1m` bold; `*italic*` → `[3m` italic; markers hidden |
| Markdown: Inline code | **PASS** | `` `code` `` → colored, backticks stripped |
| Markdown: Code blocks | **PASS** | Fence markers visible; code syntax-highlighted inside |
| Markdown: Blockquotes | **PASS** | `>` colored with teal; rendering correct |
| Markdown: Lists + HR | **PASS** | Both ordered and unordered lists; `---` HR visible |
| Markdown: Edit inside code block | **PASS** | New line added inside Python block; compose mode updates correctly |

### Issues Filed / Comments
- Comment on **#2212**: "Reproduced in v0.3.10 — `context.diagnostics` still sent as empty"

### Key Findings
1. **text-actions plugin has more decode commands than documented in learning_db.md**: Decode Base64 to String, Decode Hex String to JSON Byte Array, Decode JSON String to String are all available and work correctly. Previously only Decode URI Component and Decode URI Encoded were documented.
2. **All text-actions decode+encode round-trips correct**: Base64, URI Component, JSON String, Hex all verified correct against independent reference values.
3. **#2212 still unfixed in v0.3.10**: `context.diagnostics` is always `[]` in codeAction requests. Updated GitHub issue with v0.3.10 confirmation.
4. **Bookmarks fully functional**: Alt+0 through Alt+9 all work; unset slots give informative message; setting via "Set Bookmark" command works.
5. **Keyboard macros work for complex multi-step operations**: 5-step macro (comment prefix + move to next line) recorded, played, and listed correctly. `*Macros*` buffer shows action-level detail.
6. **Markdown Compose mode fully functional**: Bold `[1m`, italic `[3m` ANSI attributes applied; inline code stripped of backticks; code blocks get syntax highlighting inside fences; editing inside code blocks works in compose mode.
7. **clangd auto-starts in v0.3.10** with `"enabled": true` (no `auto_start` needed) — behavior changed vs v0.3.8. UPDATE: needs verification — may have started automatically due to the new build or config change.

### Version
- Binary: v0.3.10 built from `claude/awesome-clarke-57Uge` (2026-06-03)

### Cleanup
- tmux session `fresh-test-run20` killed
- Temp files removed: /tmp/cpp_test_v2/, /tmp/bookmark_test.txt, /tmp/markdown_test.md, /tmp/text_actions_test.txt
- Config reset to `{}`
- text-actions plugin NOT removed (was in /root/.config/fresh/plugins/ but config dir was clean start)

---

## Run #19 — 2026-06-03

### Status: COMPLETED

### What Was Done
- Synced state from `tui-automated-testing-state`; built release binary from `tui-automated-testing-state` (**v0.3.8**, ~7 min build)
- Created tmux session `fresh-test-run19` (220×50)
- **Preflight:** GitHub MCP auth confirmed (7 open/filed issues). Playbook integrity confirmed. All sections of AGENT_INSTRUCTIONS.md present.
- **LSP Code Actions (Alt+.)** — Definitive root cause found via LSP log: Fresh always sends `"context":{"diagnostics":[]}` (empty) in codeAction requests. clangd requires diagnostics to provide fix-based code actions. Filed new issue #2212.
- **#2113 race condition** — 8 rapid attempts across 3 patterns; not reproduced. Consistent with "timing-sensitive, reproduced once" history.
- **Encoding handling** — Latin-1 file: auto-detected as Windows-1252, Reload with Encoding, Set Encoding all work. UTF-8 round-trip confirmed by hex inspection.
- **Themes** — All 8 themes (dark, dracula, high-contrast, light, nord, nostalgia, solarized-dark, terminal) tested. Colors confirmed distinct via ANSI. "nord" is new compared to v0.3.9 test.
- **Clangd auto-start** — Confirmed: `enabled: true` does NOT auto-start; `auto_start` setting exists (default: false). Docs say "automatically" but mean "config is pre-built" not "auto-launches". Updated IMP-013 with this finding.
- **text-actions decode** — BLOCKED: GitHub network unavailable. git clone hangs; Fresh shows "Failed to install..." correctly after process killed.

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| LSP Code Actions (Alt+.) | **BUG (#2212)** | Fresh sends empty `context.diagnostics` always; clangd needs them for fix-based actions |
| #2113 race condition | **NOT REPRODUCED** | 8 attempts, 3 patterns; timing-sensitive per original report |
| Encoding: auto-detect Latin-1 | **PASS** | Detected as Windows-1252 (correct superset); all chars render properly |
| Encoding: Reload with Encoding | **PASS** | 8-encoding picker; current marked; navigation works with ANSI verify |
| Encoding: Set Encoding | **PASS** | Switches buffer encoding, marks modified, UTF-8 round-trip correct on save |
| Themes: dark/dracula/high-contrast | **PASS** | Color codes confirm distinct themes |
| Themes: light | **PASS** | Light background (`48;5;254m`); correct for light theme |
| Themes: nord | **PASS** | New in v0.3.8; distinct blue-grey palette (`188/237` codes) |
| Themes: nostalgia/solarized-dark/terminal | **PASS** | All 8 themes apply and produce different colors |
| Clangd auto_start investigation | **IMP-013 UPDATED** | `auto_start` setting exists, default `false`; docs misleading but not a bug |
| text-actions decode | **BLOCKED** | GitHub network unavailable; documented |

### Issues Filed / Comments
- **#2212** — NEW: "Alt+. shows 'No code actions available' for diagnostic-based fixes even when clangd reports '(fix available)'" — LSP log evidence: empty `context.diagnostics` in every codeAction request

### Key Findings
1. **Code Actions root cause confirmed**: Fresh always sends `"context":{"diagnostics":[]}` in `textDocument/codeAction`. clangd published 7 diagnostics with "(fix available)" but returns empty `[]` without the diagnostic context. This is the "TODO: Implement diagnostic retrieval when needed" left from closed issue #1915. Filed as new dedicated issue #2212.
2. **Encoding feature fully functional**: Detection, reload, set-encoding, and save all work correctly. Latin-1 ↔ UTF-8 round-trip confirmed via hex. 8-encoding picker with "current" marker and ANSI-confirmable navigation.
3. **All 8 themes work**: Including new "nord" theme (not present in v0.3.9 tests). Navigation in theme picker requires ANSI verify (no plain-text indicator of selected item).
4. **auto_start LSP setting discovered**: Config schema has `auto_start: boolean, default: false`. Users who want clangd to auto-start must set `"auto_start": true`. Docs saying "use it automatically" refer to pre-built config, not auto-launch.
5. **text-actions decode BLOCKED**: No GitHub network in this environment. Fresh plugin install handles failure gracefully ("Failed to install...").

### Version
- Binary: v0.3.8 built from `tui-automated-testing-state` (2026-06-03)

### Cleanup
- tmux session `fresh-test-run19` killed
- Temp files removed: /tmp/cpp_lsp_test/, /tmp/latin1_test.txt, /tmp/test_palette_leak.txt, /tmp/claude-0/fresh-pkg-clone-*
- Config reset to `{}`
- clangd stopped (fresh exited)

---

## Run #18 — 2026-06-03

### Status: COMPLETED

### What Was Done
- Synced state from `tui-automated-testing-state`; built release binary from `tui-automated-testing-state` (**v0.3.8**, ~8 min build from scratch)
- Installed clangd-18 via `apt-get install clangd` (not installed by default)
- Created tmux session `fresh-test-run18` (220×50)
- **Preflight:** GitHub MCP auth confirmed (7 open/filed issues verified). Playbook integrity confirmed. All 3 AGENT_INSTRUCTIONS.md sections present.
- **LSP: clangd on C project** — Set up small C project in `/tmp/c_lsp_test/` with compile_commands.json; configured clangd in Fresh config. Tested all major LSP features.
- **text-actions plugin** — Installed from GitHub URL and tested encoding/decoding commands.
- **Git Blame: multi-commit history** — Tested 'b' navigation on CHANGELOG.md (399 blocks, multiple commits). Confirmed depth tracking.
- **#2122 recheck** — Confirmed move_to_paragraph_down/up still has no keybinding in v0.3.8 (keybinding editor shows empty for those actions).
- **#2165 recheck** — Confirmed *Keyboard Shortcuts* 'q' still shows "Editing disabled" in v0.3.8.

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| clangd: auto-start | **NEEDS MANUAL START** | Even with `"enabled": true` in config, shows "not running" — needed "Start clangd (always)" from LSP Status popup |
| LSP: Hover (Alt+K) | **PASS** | Shows function signature popup: "int add(int a, int b)" |
| LSP: Go to Definition (F12) | **PASS** | Jumped to definition at main.c:9, status "Jumped to definition at /tmp/c_lsp_test/main.c:9" |
| LSP: Completions (Ctrl+Space) | **PASS** | Showed "make_point(int x, int y) Point" suggestion for "mak" prefix |
| LSP: Find References (Shift+F12) | **PASS** | Found 2 references to 'add' (definition + call site) |
| LSP: Rename Symbol (F2) | **PASS** | Renamed 'add' → 'sum' at definition and all call sites simultaneously |
| LSP: Inlay hints | **PASS** | Parameter names shown in call sites: "add(a: 3, b: 4)", "make_point(x: 10, y: 20)" |
| LSP: Code Actions (Alt+.) | **NOT AVAILABLE** | "No code actions available" even at error location (malloc undeclared). Likely clangd limitation for this error type, not a Fresh bug. |
| text-actions plugin: install | **PASS** | "Installed and activated fresh-text-actions-plugin v0.1.0" |
| text-actions plugin: commands | **PASS** | 6+ commands: Base64/JSON/URI encode+decode |
| text-actions plugin: Base64 | **PASS** | "Hello World" → "SGVsbG8gV29ybGQ=" (correct) |
| Git Blame: multiple commits | **PASS** | CHANGELOG.md shows 399 blocks with multiple distinct commit hashes |
| Git Blame: 'b' go to parent | **PASS** | bc11f2b → 059f4ab → 60d0ba2; depth counter shown in status |
| Git Blame: 'q' close | **PASS** | "Git blame closed" status |
| #2122 move_to_paragraph keybinding | **CONFIRMED STILL OPEN** | No keybinding in v0.3.8 (same as #2122 report) |
| #2165 *Keyboard Shortcuts* 'q' | **CONFIRMED STILL OPEN** | "Editing disabled in this buffer" in v0.3.8 |

### Issues Filed / Comments
- No new issues filed — all findings either PASS or match known open issues
- Note: clangd auto-start behavior is a potential UX issue (docs say "auto", but requires manual start). Logged in potential_improvements.md as IMP-013.

### Key Findings
1. **clangd LSP fully functional** once started: hover, definition, completions, references, rename all work. Inlay hints shown automatically.
2. **Code Actions (Alt+.)** returned "No code actions available" even at diagnostic error locations. This may be clangd's behavior for C "undeclared function" errors (no quick-fix available), not a Fresh bug. Future run should test with C++ or a different error type.
3. **text-actions plugin** installs cleanly from external GitHub URL. All 6+ encoding commands appear in palette. Base64 encoding verified correct.
4. **Git Blame multi-commit history** navigation works: 'b' goes to parent, depth counter shown, multiple commits verified. First commit shows "Cannot get blame at SHA^ (may be initial commit)".
5. **clangd auto-start**: Despite `"enabled": true` in config.json, clangd shows as "not running" on fresh launch. Requires manual "Start clangd (always)" from LSP Status popup. This contradicts the docs which say LSP auto-starts when installed. Documented as IMP-013.

### Version
- Binary: v0.3.8 built from `tui-automated-testing-state` branch (2026-06-03)

### Cleanup
- fresh exited cleanly via Ctrl+Q
- tmux session `fresh-test-run18` killed
- text-actions plugin removed: `rm -rf /root/.config/fresh/plugins/packages/fresh-text-actions-plugin`
- LSP config reset to `{}`
- /tmp/c_lsp_test/ removed

---

## Run #17 — 2026-06-02

### Status: COMPLETED

### What Was Done
- Synced state from `tui-automated-testing-state`; built release binary from `claude/awesome-clarke-VmLci` (**v0.3.10**, ~8 min)
- Created tmux session `fresh-test-run17` (220×50)
- **Preflight:** Confirmed AGENT_INSTRUCTIONS.md updated per user instructions (real LSP preference added; forget previous issues instruction enacted by resetting test priority to coverage-first).
- **User overrides this run:**
  1. "forget previous issues; move on to testing completely other UX aspects or features or user flows"
  2. "prefer real-world use cases and tools" instruction added to AGENT_INSTRUCTIONS.md
  3. Removed fake-pylsp symlink; switched to real pyright
  4. Avoided rust-analyzer; used pyright on small Python project in /tmp
- **File Explorer (Ctrl+B / Ctrl+E):** Tested full keyboard-only navigation
- **LSP with pyright:** Set up real pyright on a small Python project in `/tmp/py_lsp_test/`; discovered major LSP timeout bug
- **Settings panel:** Tested navigation model, TextList [x] delete keyboard accessibility
- **Bug filed:** #2197 — pyright LSP all request-based features timeout after 30s

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| File Explorer: Ctrl+B toggle | **PASS** | Shows/hides sidebar |
| File Explorer: Ctrl+E focus | **PASS** | Moves focus from editor to explorer |
| File Explorer: Up/Down navigate | **PASS** | Moves cursor through files/dirs |
| File Explorer: Right expand dir | **PASS** | Expands directory |
| File Explorer: Left collapse dir | **PASS** | Collapses directory |
| File Explorer: Enter opens file | **PASS** | Opens file permanently (not preview) |
| File Explorer: auto-preview on navigate | **PASS** | Files auto-preview as cursor moves |
| File Explorer: New file (Ctrl+N) | **PASS** | Creates file when explorer focused |
| File Explorer: Delete file (Delete key) | **PASS** | Confirms with y/n; "Moved to trash" |
| Settings: Tab cycle (Cat→Settings→Footer→Cat) | **PASS** | Blue `[48;5;25m` highlight confirms focus |
| Settings: TextList navigate Up/Down to items | **PASS** | Up/Down navigates from header into items |
| Settings: TextList Delete removes item | **CONFIRMED** | Hint "Del:remove" shown when item focused |
| Settings: TextList [x] keyboard-accessible | **CONFIRMED NOT** | Tab exits TextList; [x] is mouse-only |
| Settings: Escape discards unsaved changes | **PASS** | No confirmation dialog; changes discarded |
| pyright LSP: initialize | **PASS** | Shows "LSP (python) ready" in status bar |
| pyright LSP: hover (Alt+K) | **FAIL** | Timeout after 30s (10/10 requests) |
| pyright LSP: definition (F12) | **FAIL** | Timeout after 30s |
| pyright LSP: completion (Ctrl+Space) | **FAIL** | Timeout after 30s |
| pyright LSP: signatureHelp | **FAIL** | Timeout after 30s |
| pyright LSP: diagnostics | **FAIL** | 0 items (no code diagnostics published) |

### Issues Filed
- **#2197** (new): "Pyright LSP: all request-based features (hover, definition, completions) timeout after 30s while LSP shows 'ready'"

### Key Findings
1. File Explorer fully functional with keyboard-only navigation including file creation and deletion.
2. Settings panel uses Tab cycle: Categories → Settings → Footer → Categories. Arrow keys in Categories panel navigate categories; Tab switches focus to the Settings panel.
3. Settings TextList [x] buttons are MOUSE-ONLY. Keyboard deletion uses Delete key while item focused (confirmed via "Del:remove" hint text).
4. pyright LSP integration broken — initialize succeeds but ALL subsequent LSP requests (hover, definition, completion, signatureHelp, diagnostics) silently timeout after 30s. Position encoding mismatch suspected (log: `LSP initialize result: position_encoding=None`).

### Version
- Binary: v0.3.10 built from `claude/awesome-clarke-VmLci` (same as Run #16 branch, new commit)

### Cleanup
- tmux session `fresh-test-run17` killed
- /tmp/py_lsp_test/ removed

---

## Run #16 — 2026-05-31

### Status: COMPLETED

### What Was Done
- Synced state from `tui-automated-testing-state`; built release binary from `claude/awesome-clarke-jWgGn` (**v0.3.10**, ~2.5 min)
- Created tmux session `fresh-test-run16` (220×50)
- **Preflight:** GitHub MCP auth confirmed. Playbook integrity verified. Discovered #2117 and #2125 both CLOSED by maintainer since Run #15.
- **Bug recheck — *Keyboard Shortcuts* 'q':** STILL BROKEN in 0.3.10 ("Editing disabled in this buffer"). Filed new issue #2165 since #2125 is closed.
- **Bug recheck — #2117 Review Diff discard hunk:** CONFIRMED FIXED in 0.3.10! Created review_diff_test16.txt (+4 lines), opened Review Diff, navigated to hunk, pressed 'd' → confirmed dialog → Enter → "Review Diff: 0 hunks". File reverted to original. Manual git apply --reverse no longer needed.
- **Diagnostics panel 'q' recheck:** CONFIRMED STILL FIXED — "Diagnostics panel closed" on 'q' press. Consistent with #2125 closure.
- **Git Blame plugin:** PASS — `*blame:README.md*` buffer opens with commit info (commit hash, author, time, message). Status bar shows "Git blame: N blocks | b: blame at parent | q: close". 'b' correctly returns "Cannot get blame at SHA^ (may be initial commit)" for file at initial commit. 'q' closes with "Git blame closed".
- **Live Diff: Set Default Mode:** PASS — prompt "Default mode (head, disk, or branch:<ref>)head" appears. Accepted "disk", "branch:main", and "head" — all showed "Live Diff: default mode updated". Note: prompt always pre-fills "head" regardless of current setting.
- **Orchestrator features (0.3.10):** PASS — Alt+P toggles project scope (All → user/fresh), Alt+T toggles show-all-worktrees checkbox, Tab focuses detail panel buttons (blue highlight), Details view shows "ACT Xs in-place" + working dir + file preview, "/" filter input works, Escape closes. All 0.3.9+ features confirmed working.
- **Package: Install + Uninstall + Color Highlighter:**
  - Install via "Package: Install from URL" → `https://github.com/sinelaw/fresh-plugins#color-highlighter` → "Installed and activated color-highlighter v1.0.0" ✅
  - Package browser shows INSTALLED (1) with ✓ checkmark ✅
  - Color Highlighter: Enable command adds `█` swatches before hex/rgb/hsl values in CSS (ANSI confirms actual colors: `[38;5;196m` red, `[38;5;33m` blue, `[38;5;46m` green) ✅
  - Uninstall via `rm -rf /root/.config/fresh/plugins/packages/color-highlighter` → package browser shows AVAILABLE (13), swatches immediately removed ✅
  - ⚠️ NOTE: Package UI Install/Uninstall button navigation is complex (Tab through 8+ elements to reach). "Enter Activate" at Tab position shows `[ Install ]`/`[ Uninstall ]` but pressing Enter activates search field. Documented in potential_improvements.md.
- **Dev Container: Attach (no CLI):** PASS — dialog "Dev Container CLI Not Found: The devcontainer CLI is needed for rebuild. Copy the install command below, or dismiss. Copy: npm i -g @devcontainers/cli / Dismiss (ESC)". Clear, helpful error with actionable install command.

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| *Keyboard Shortcuts* 'q' close | **STILL BROKEN** | Filed new #2165 (parent #2125 was closed) |
| #2117 Review Diff discard hunk | **CONFIRMED FIXED** | Works in 0.3.10 — 0 hunks after discard |
| #2125 Diagnostics panel 'q' | **STILL FIXED** | "Diagnostics panel closed" confirmed |
| Git Blame plugin | **PASS** | Blame buffer, 'b' go-back, 'q' close all work |
| Live Diff: Set Default Mode | **PASS** | head/disk/branch:main all accepted |
| Orchestrator features | **PASS** | Alt+P/T, Details, filter search all work |
| Package: Install from URL | **PASS** | "Installed and activated color-highlighter v1.0.0" |
| Color Highlighter plugin | **PASS** | Swatches for hex/rgb/hsl with correct colors |
| Package: Uninstall (file delete) | **PASS** | Package removed, swatches gone in real-time |
| Dev Container: Attach error handling | **PASS** | "CLI Not Found" dialog with npm install command |

### Issues Filed / Comments
- Filed new issue **#2165**: "*Keyboard Shortcuts* buffer: pressing 'q' shows 'Editing disabled' despite in-buffer documentation" (since #2125 closed)
- Updated `github_issues.md` and `confirmed_bugs.md`

### Version
- Binary: v0.3.10 built from `claude/awesome-clarke-jWgGn` (new version vs Run #15's 0.3.9)

### Cleanup
- Fresh exited via Ctrl+Q (d = discard and quit)
- tmux session `fresh-test-run16` killed
- review_diff_test16.txt committed + removed from dev branch
- /tmp/test_colors.css removed
- .devcontainer/ directory removed

---

## Run #15 — 2026-05-27

### Status: COMPLETED

### What Was Done
- Synced state from `tui-automated-testing-state`; built release binary from `claude/awesome-clarke-cN0ma` (v0.3.9, ~80s)
- Created tmux session `fresh-test-run15` (220×50)
- **Preflight:** GitHub MCP auth confirmed (listed issues). Playbook integrity verified.
- **Bug recheck — *Keyboard Shortcuts* 'q':** STILL BROKEN ("Editing disabled in this buffer"). Same as Run #14.
- **Bug recheck — #2117 (Review Diff discard hunk):** STILL BROKEN. Created review_diff_test.txt with +3 lines, triggered discard — "Patch failed: error: patch failed: review_diff_test.txt:2error: review_diff_test.txt: patch does not apply". Manual `git apply --reverse --check` succeeds (confirming it's Fresh's bug).
- **Flash: Jump plugin:** PASS — opened via command palette, jump-hint overlay activated (letters replace visible chars), pressed 'n' hint to jump from Ln 7 Col 18 → Ln 7 Col 6.
- **Package Manager (Package: Packages):** PASS — shows 13 available packages with categories [P/T/L], detail panel, filter tabs (All/Installed/Plugins/Themes/Languages/Bundles/Sync). Search by "/" filters: "theme" → 3 results. Registry synced (1/1 sources).
- **Package Manager (Package: Install from URL):** PASS — prompts "Git URL or local path:" input dialog.
- **Live Diff: vs HEAD:** PASS — green `│` gutter markers (ANSI 38;5;78) and green bg (48;5;22) on added lines. Status: "Live Diff: comparing against HEAD".
- **Live Diff: vs Disk:** PASS — `+` marker on unsaved line. Status: "Live Diff: comparing against file on disk".
- **Live Diff: vs Branch...:** PASS — "Branch or ref" prompt pre-filled "main". Status: "Live Diff: comparing against main".
- **Live Grep: Cycle Provider:** PASS — Alt+P cycles: git-grep → rg → grep → git-grep. All 3 providers available. Search "Test" returned 1000+ matches.
- **Block selection (Alt+Shift+Arrow):** PASS — M-S-Down and M-S-Right work! Block selected "Line " (cols 1-5) across rows 1-4. Typing '>' replaced selection on all 4 rows simultaneously. NOTE: Run #12 reported M-S-Down didn't work — it DOES work in this build.
- **Dev Container features:** PASS — Create Config creates minimal .devcontainer/devcontainer.json; Show Info displays container config with action buttons; Show Features shows "No features configured"; Show Forwarded Ports shows "No configured or runtime ports to show."; all Dev Container panels close with 'q' (unlike *Keyboard Shortcuts* buffer).

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| *Keyboard Shortcuts* 'q' close | **STILL BROKEN** | "Editing disabled in this buffer" — same as Runs 12-14 |
| #2117 Review Diff discard hunk | **STILL BROKEN** | Patch failed error persists; manual git apply --reverse works |
| Flash: Jump plugin | **PASS** | Hint overlay activates, pressing hint char jumps cursor |
| Package: Packages browser | **PASS** | 13 packages, search, filter tabs, detail panel, Install button |
| Package: Install from URL | **PASS** | "Git URL or local path:" prompt appears |
| Live Diff: vs HEAD | **PASS** | Green gutter markers on added lines; status confirmed |
| Live Diff: vs Disk | **PASS** | `+` marker on unsaved content; status confirmed |
| Live Diff: vs Branch... | **PASS** | Branch prompt, "comparing against main" confirmed |
| Live Grep: Cycle Provider | **PASS** | git-grep → rg → grep cycling; search works with all providers |
| Block selection (Alt+Shift+Arrow) | **PASS** | M-S-Down and M-S-Right work; rectangular edit confirmed |
| Dev Container: Create Config | **PASS** | Creates .devcontainer/devcontainer.json with template |
| Dev Container: Show Info | **PASS** | Shows config, action buttons, q closes correctly |
| Dev Container: Show Features | **PASS** | "No features configured" |
| Dev Container: Show Forwarded Ports | **PASS** | "No configured or runtime ports" panel with q close |

### Issues Filed / Comments
- No new issues filed (all tests passed or are known bugs with open issues)
- Note: *Keyboard Shortcuts* 'q' bug persists — already tracked via #2125 comment

### Cleanup
- Fresh exited via Ctrl+Q (d = discard and quit)
- tmux session `fresh-test-run15` killed
- review_diff_test.txt commit reverted on dev branch; .devcontainer removed

---

## Run #14 — 2026-05-27

### Status: COMPLETED

### What Was Done
- Synced state from `tui-automated-testing-state`; built release binary from `claude/awesome-clarke-c7jCY`
- Created tmux session `fresh-test-run14` (220×50)
- **T47 Rapid keystrokes:** 50-char burst intact; 20 rapid Ctrl+Z all undone correctly. PASS.
- **T48 Resize reflow:** 220×50 → 80×24 → 180×40 all reflow; mid-typing resize safe. PASS.
- **Alt+A project-wide Search & Replace:** Panel opened; 9 matches in 4 files found; Space scoping (deselected source files to scope to test_file1.txt); Replace All with confirmation ("Replaced 3 occurrences in 1 files"). PASS.
- **Calibrate Keyboard wizard:** 24 steps/5 groups (Basic Editing, Line Navigation, Word Navigation, Document Navigation, Emacs-Style). Does NOT test Ctrl+H. s/b/g/a controls all work.
- **#2125 recheck (Diagnostics panel):** q CONFIRMED FIXED (commit 89caf72). `*Keyboard Shortcuts*` 'q' STILL BROKEN ("Editing disabled"). Comment posted on #2125.
- **#2112 recheck (outside-workspace search):** CONFIRMED FIXED (commit b7e7e64). /tmp files now found in Search/Replace panel. Comment posted on #2112.

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| T47: Rapid keystrokes | **PASS** | 50-char burst intact, 20 rapid undos clean |
| T48: Resize reflow | **PASS** | All size transitions smooth, mid-typing resize safe |
| Alt+A: Project-wide Search | **PASS** | 9 matches/4 files, scoping, replace all with confirmation |
| Calibrate Keyboard wizard | **TESTED** | 24 steps/5 groups; Ctrl+H NOT tested by wizard |
| #2125 Diagnostics 'q' fix | **CONFIRMED FIXED** | commit 89caf72 verified via UI |
| #2125 *Keyboard Shortcuts* 'q' | **STILL BROKEN** | Shows "Editing disabled in this buffer" |
| #2112 Outside-workspace search | **CONFIRMED FIXED** | commit b7e7e64 verified via UI |

### Issues Filed / Comments
- No new issues filed
- Comment on #2125: Diagnostics panel fixed; *Keyboard Shortcuts* still broken
- Comment on #2112: Confirmed fixed with test procedure

### Cleanup
- Fresh exited via Ctrl+Q; tmux session `fresh-test-run14` killed
- Test files removed: `tmp_test_files/`, `/tmp/rapid_test.txt`, `/tmp/outside_workspace_test.txt`

---

## Run #13 — 2026-05-27

### Status: COMPLETED

### What Was Done
- Loaded state from `tui-automated-testing-state` branch
- Built fresh debug binary from source (`cargo build --bin fresh --features runtime`, ~3.5 min)
  - Binary: `target/debug/fresh`
- Created tmux session `fresh-test` (220×50)
- **Bug Verification (Sprint 12):**
  - TB01: CONFIRMED — `*Keyboard Shortcuts*` 'q' close non-functional (BUG-001)
  - TB02: CONFIRMED — Edit menu "Replace..." mislabeled (BUG-002)
  - TB03: RESOLVED — Alt+W behavior IS correct (context-sensitive, not a bug)
- **GitHub Actions:**
  - Searched for RC12-01: Already covered by issue #2125 → Added comment with Keyboard Shortcuts buffer info
  - Filed new issue #2135 for RC12-02 (Edit menu label mismatch)
- **New Feature Tests:**
  - T28: PASS — Go to Matching Bracket (via command palette; `(` → `)`, `{` → `}`)
  - T30: PASS — Position History (Alt+Left back, Alt+Right forward)
  - T37: PASS — Toggle Line Wrap (View menu ☑ Line Wrap)
  - T45: PASS — Large file (49MB / 500K lines) opens instantly, navigation immediate, search <2s
  - T46: PASS — Binary file (/bin/ls) opens gracefully with [BIN] tag and hex notation

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| TB01: Keyboard Shortcuts 'q' close | **CONFIRMED BUG** | "Editing disabled" — same root cause as #2125 |
| TB02: Edit menu Replace label | **CONFIRMED BUG** | Mislabeled "Replace..." → filed #2135 |
| TB03: Alt+W inconsistency | **RESOLVED - NOT A BUG** | Context-sensitive behavior is correct |
| T28: Go to Matching Bracket | **PASS** | Works via command palette |
| T30: Position History | **PASS** | Alt+Left/Right navigate back/forward |
| T37: Toggle Line Wrap | **PASS** | View menu ☑ toggle works both ways |
| T45: Large File Performance | **PASS** | 49MB opened instantly; byte-offset mode |
| T46: Binary File Handling | **PASS** | [BIN] tag; hex notation for non-printable |

### Issues Found / Filed
- Issue #2135 filed: "Edit menu 'Replace...' label maps to Ctrl+Alt+R (Query Replace)"
- Comment on #2125: Keyboard Shortcuts buffer also affected by same root cause

### Key Learnings
- Fresh uses "byte offset mode" for large files (gutter shows bytes, not line numbers)
- Binary files get `[BIN]` tab tag + `<XX>` hex notation for non-printable bytes  
- `Ctrl+]` (ASCII 0x1D) doesn't transmit reliably via tmux send-keys; use command palette for bracket matching
- Alt+W = Close Tab (outside search) is CORRECT behavior; not a bug
- Line Wrap is in View menu (no command palette entry found in this search)

### Cleanup
- Fresh exited via Ctrl+Q
- tmux session `fresh-test` killed
- Test files /tmp/test_brackets.js, /tmp/test_long_line.txt, /tmp/large_test_file.txt deleted

---

## Run #12 — 2026-05-27

### Status: COMPLETED

### What Was Done
- Attempted to load existing state (no local state found → pulled from remote)
- Built fresh 0.3.9 binary from source: `cargo build --release --bin fresh` (~60s)
  - Binary path: `target/release/fresh` (Note: previous runs used `/opt/node22/bin/fresh` via npm)
- Created tmux session `fresh-test` (220×50)
- Executed comprehensive re-verification of Sprints 1-9 (most already tested in Runs 1-11)
- Investigated 2 new potential bugs

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| Sprint 1 (Launch & UI) | **PASS** | All confirmed working as documented |
| Sprint 2 (File Ops) | **PASS** | Ctrl+N/O/S, Alt+W, save dialog all work |
| Sprint 3 (Editing) | **PASS** | Ctrl+Z/Y/C/X/V/A/W/L/D//, all working |
| Sprint 4 (Search/Replace) | **PASS** | Ctrl+F search, Ctrl+R replace, Ctrl+Alt+R query replace all work |
| Sprint 5 (Navigation) | **PASS** | Ctrl+G go-to-line, Command Palette, menu bar |
| Sprint 6 (Command Palette) | **PASS** | All modes (file/>command/:line/#buffer) verified |
| Sprint 7 (Views/Layout) | **PASS** | Split Vertical/Horizontal, File Explorer, Theme Selection |
| Sprint 8 (Tabs/Buffers) | **PASS** | Multi-tab, next/prev buffer, close with confirm dialog |
| Sprint 9 (Terminal) | **PASS** | Integrated terminal, Ctrl+Space toggle, Close Split |
| Settings UI | **PASS** | All categories visible, General settings confirmed |
| Help System | **PASS** | F1 manual, Shift+F1 keyboard shortcuts both open |

### Issues Found This Run

#### BUG-CANDIDATE-RC12-01: Keyboard Shortcuts Buffer 'q' Close Does Not Work
- Buffer text at line 4: "Press 'q' to close this buffer."
- **Actual behavior:** Pressing 'q' shows "Editing disabled in this buffer" in status bar, buffer stays open
- **Workaround:** Use Alt+W
- **Severity:** Low
- **Note:** Check if this is already filed under existing issues before filing new issue
- **Filing blocked:** GitHub MCP token expired this run; file in Run #13

#### BUG-CANDIDATE-RC12-02: Edit Menu "Replace..." Shows Ctrl+Alt+R (Query Replace, Not Basic Replace)
- Edit menu item "Replace..." shortcut = `Ctrl+Alt+R` = opens Query Replace (interactive mode)
- Basic "Replace" (Ctrl+R) is NOT in the Edit menu at all
- Command palette clearly shows two distinct commands: Replace (Ctrl+R) vs Query Replace (Ctrl+Alt+R)
- **Assessment:** May be intentional design, or documentation inconsistency
- **Note:** Already documented in learning_db.md as known behavior; re-verify whether it's a real bug
- **Filing blocked:** GitHub MCP token expired; assess in Run #13

### Key Learnings / Corrections
- Binary can be built from source via `cargo build --release --bin fresh`; binary is `target/release/fresh` not `fresh-editor`
- Binary installed by npm is at `/opt/node22/bin/fresh` (from previous runs); source build works too
- Session persistence confirmed: Unsaved buffers restored on relaunch (hot exit)
- Save/discard dialog confirmed: letter + Enter (not single keypress)
- Keyboard shortcuts buffer cannot be closed with 'q' despite the docs saying so
- Alt+W and Whole Word toggle conflict documented: Alt+W in search bar = toggle whole word; outside search = close tab
- Block selection tmux keys: `M-S-Down` appears to NOT trigger block select reliably in this tmux version (investigation needed)

### Cleanup
- tmux sessions `fresh-test` and `quit-test` both killed
- No test files left behind on disk (all were in /tmp)

---

## Run #11 — 2026-05-26

### Status: COMPLETED

### What Was Done
- Built Fresh 0.3.9 binary from `claude/ecstatic-mayer-5DivD` branch (6.5 min build)
- Checked out `tui-automated-testing-state` branch, loaded all prior state
- Launched tmux session `fresh-qa` (200×50)
- Executed 10+ test objectives covering bookmarks, Settings add/delete/reset, and LSP with fake-pylsp

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| TC-BOOKMARKS | **PASSED** | Alt+1/2/etc jump to bookmarks 1/2/etc; "not set" for missing; Ctrl+P → "Set Bookmark" |
| TC-SETTINGS-ADD-NEW | **PASSED** | Typing while focused on list header activates [+] Add new inline input; Enter confirms |
| TC-SETTINGS-CTRL-R | **RESOLVED/PARTIAL** | Ctrl+R is a NO-OP for field reset; Escape from field reverts pending changes; [ Reset ] button via Tab works |
| TC-SETTINGS-DEL-X | **PENDING** | [x] buttons appear mouse-only; keyboard navigation to sub-list items not confirmed |
| TC-FAKE-LSP | **PASSED** | fake-pylsp recognized as `pylsp`; LSP starts; connection handshake logged |
| TC-LSP-GOTO-DEF | **PASSED** | F12 Go to Definition works; navigates to LSP-returned location |
| TC-LSP-HOVER | **PARTIAL** | Alt+K shows "No hover info available" (expected with fake-pylsp null response) |
| TC-LSP-REFERENCES | **PASSED** | Find References opens dock panel with clickable results; Enter navigates correctly |
| TC-REFERENCES-NAV | **CONFIRMED** | References panel Enter WORKS (unlike *Quickfix* BUG #2124) |

### Issues Found This Run
- **0 new bugs filed**
- **1 important distinction**: References panel (from LSP Find References) correctly handles Enter navigation — this is DIFFERENT from *Quickfix* buffer (BUG #2124 which is from Live Grep Alt+M)
- **Ctrl+R in Settings**: Does NOT reset number fields — CHANGELOG claim may be incorrect for 0.3.9

### Key Learnings
- Binary 0.3.9 confirmed from `fresh --version`
- Bookmarks: `Ctrl+P → Set Bookmark → digit → Enter`; jump with `Alt+N`
- Settings list [+] Add new: type text directly while header is focused (no Enter needed to start)
- Settings [x] delete: likely mouse-only (no keyboard path found)
- Escape from Settings pending field: REVERTS changes (useful as keyboard reset)
- fake-pylsp setup: symlink `scripts/fake-lsp/bin/fake-pylsp` → `/usr/local/bin/pylsp`; set `FAKE_DEVCONTAINER_STATE` env
- LSP Find References panel IS keyboard-navigable (Enter works); bug is specific to *Quickfix*

---

## Run #10 — 2026-05-26

### Status: COMPLETED

### What Was Done
- Built Fresh binary from source (tui-automated-testing-state base = `88883dc`, v0.3.8)
- Launched tmux session `fresh-test` (200×50)
- Executed 7 test objectives: Alt+/, Markdown Preview, Keyboard Macros, Settings Ctrl+R, Review Diff regression check

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| TC-ALT-SLASH | **PASSED** | `M-/` opens Live Grep; 375 results for "fn main"; preview split works |
| TC-MARKDOWN | **PASSED** | Markdown Compose mode: ANSI bold/italic/headings; status "Markdown Compose: ON (soft breaks, centered)" |
| TC-MACRO-RECORD | **PASSED** | "Record Macro" prompt (0-9), F5 stops, macro saved with action count |
| TC-MACRO-PLAYBACK | **PASSED** | F4 plays macro correctly; all 3 test lines got " [MACRO]" appended |
| TC-MACROS-LIST | **PASSED** | "List Macros" opens `*Macros*` buffer; WARNING: buffer is editable (not strict RO) |
| TC-SETTINGS-CTRL-R | **PARTIAL** | Ctrl+R when field highlighted does NOT reset; `[ Reset ]` button reachable via Tab; full test inconclusive |
| TC-REVIEW-DIFF-CONTROLS | **FALSE POSITIVE CORRECTED** | All controls broken BY DESIGN — per `docs/internal/review-diff-feature-restoration-plan.md` (Status: Planned) |

### Issues Found This Run
- **0 new bugs filed**
- **1 false positive corrected**: Run #8 TC-REVIEW-DIFF-DISCARD "PASSED" was wrong; Review Diff panel controls were never implemented in this codebase version

### Key Learnings
- Version is 0.3.8 (not 0.3.9 as previously logged)
- Review Diff panel controls are planned-but-not-implemented features
- DECCKM `$'\033OB'` must be UNQUOTED in bash (not inside double quotes)
- `Explorer` menu item appears in menu bar when File Explorer is used
- `*Macros*` buffer is editable (different from strictly-RO Quickfix/Diagnostics)

---

## Run #9 — 2026-05-26

### Status: COMPLETED

### What Was Done
- Built Fresh 0.3.9 binary from source (`cargo build --release --bin fresh`, ~3 min)
- Checked out `tui-automated-testing-state` branch, loaded state from 8 prior runs
- Launched tmux session `fresh-test` (200×50)
- Executed 8+ test objectives covering LSP popup navigation, Quickfix navigation, shell commands, multi-cursor, diagnostics panel, and backlog items

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| TC-LSP-POPUP-NAV-2 | **CONFIRMED** | Plain `Up`/`Down` keys navigate popup; ANSI `[48;5;25m]` highlight confirms selection |
| TC-QUICKFIX-ENTER | **BUG FOUND** | Enter → "Editing disabled"; no navigation keybindings for Quickfix; BUG #2124 filed |
| TC-DIAG-PANEL-SHORTCUTS | **BUG FOUND** | q/a/Enter all → "Editing disabled"; status hints are non-functional; BUG #2125 filed |
| TC-SETTINGS-CTRL-R | **PARTIAL** | Ctrl+R in Settings closes the overlay; `[ Reset ]` footer button not reachable via Tab cycling |
| TC-SHELL-CMD | **PASSED** | `Alt+|` → "Shell command:" prompt → sort → `*Shell: sort*` tab with sorted output |
| TC-SHELL-CMD-REPLACE | **PASSED** | `Shell Command (Replace)` via palette → `sort -r` → in-place replacement confirmed |
| TC-MULTICURSOR-LINE-ENDS | **PASSED** | `M-I` (Alt+Shift+I) on 5 lines → "6 cursors | Added cursors to line ends (6)" |
| TC-BUG2122-RECHECK | **STILL OPEN** | `move_to_paragraph_down/up` still have no keybinding; select variants have `Ctrl+Shift+↓/↑` |

### Issues Found This Run
- **BUG #2124 filed**: Quickfix buffer `Enter` shows "Editing disabled" — no jump-to-match behavior despite design spec requiring it
- **BUG #2125 filed**: Diagnostics panel `q/a/RET` shortcuts are non-functional — status bar hints are misleading

### Key Discoveries This Run
1. **Quickfix buffer has no navigation keybindings**: Searching Keybinding Editor for `/quickfix` only shows export bindings (Alt+M, Alt+Q in `prompt` context). The design doc says Enter should navigate but this was never implemented.
2. **Diagnostics panel shortcuts don't work**: The `q: close | a: toggle filter | RET: goto` hints in the status bar and `Enter:select | Esc:close` panel body text are misleading — these shortcuts are not bound.
3. **Shell Command feature fully confirmed**: Both `Alt+|` (output to new buffer) and `Shell Command (Replace)` (output replaces selection) work correctly. Tested with `sort` and `sort -r`.
4. **Add Cursors to Line Ends (`M-I`) confirmed working**: 5-line selection → 6 cursors at line ends. Status bar shows confirmation message.
5. **Fake LSP (`scripts/fake-lsp/bin/fake-pylsp`) discovered**: Requires `FAKE_DEVCONTAINER_STATE` env var. Could unlock LSP feature testing in future runs.
6. **Settings UI Ctrl+R investigation**: The `Ctrl+R` key closes Settings overlay (routes to global Find & Replace). The `[ Reset ]` button is in the footer but not reachable via Tab cycling in the tested workflow. Needs further investigation.
7. **Settings keystroke leak confirmed**: Navigating Settings with Tab and search can leak keystrokes into editor. Config file was accidentally modified during testing (restored manually).

### Lessons Learned
See learning_db.md for additions: Lesson 44–50

---

## Run #8 — 2026-05-26

### Status: COMPLETED

### What Was Done
- Built Fresh 0.3.9 binary from source (`cargo build --release --bin fresh`, ~3 min)
- Pulled state from `tui-automated-testing-state` branch (7 prior runs)
- Launched tmux session `fresh-test` (200×50)
- Executed 10 test objectives covering 0.3.9 features, bug regression checks, and new discoveries

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| TC-LSP-STATUS | **PASSED** | LSP status popup shows server state; auto-opens log tab on failure; states: (off)/(error)/running |
| TC-LSP-POPUP-NAV | **DISCOVERED** | DECCKM sequences (ESC prefix) CLOSE popups; use plain Up/Down for popup nav |
| TC-LIVE-GREP-DIAG | **PASSED** | Alt+D toggles Diagnostics scope; "No matches" without LSP (expected); provider disappears |
| TC-LIVE-GREP-ALTM | **PASSED** | Alt+M saves to `*Quickfix*` [RO] buffer in split; format: `file:line:col  content` |
| TC-ORCHESTRATOR-0.3.9 | **PASSED** | New UI: Alt+P project scope, Alt+T show worktrees, `/` filter, session detail buttons |
| TC-C3-LANGUAGE | **PASSED** | C3 syntax highlighting fully working; `C3` status bar; folding at fn/struct |
| TC-REVIEW-DIFF-DISCARD | **BUG FIXED** | BUG #2117 CONFIRMED FIXED in 0.3.9 — discard works correctly; comment on GH issue |
| TC-WORKSPACE-RESTORE-2056 | **PASSED** | Session isolation by working directory confirmed; no cross-project tab mixing |
| TC-PLUGIN-API-DATADIRS | **DOCUMENTED** | getWorkingDataDir() and getTerminalDir() documented from API types |

### Issues Found This Run
- **None filed** — BUG #2117 resolved; all other behaviors working as expected or documented

### Key Discoveries This Run
1. **BUG #2117 (Review Diff discard) FIXED**: Confirmed working in 0.3.9 dev build. Tested twice. Comment posted on GitHub.
2. **Popup navigation insight**: DECCKM sequences (`$'\033OA'`, `$'\033OB'`) start with ESC which CLOSES any active overlay/popup. For popup list navigation, use plain tmux key names (`Up`, `Down`). DECCKM only applies to cursor movement inside the editor buffer.
3. **C3 language support**: Full syntax highlighting with Sublime syntax grammar. `.c3`, `.c3i`, `.c3t` extensions. c3lsp configured but not bundled.
4. **Orchestrator 0.3.9 UI**: New project scope filter (Alt+P), show-all-worktrees toggle (Alt+T), `/` filter search, session detail action buttons (Visit/Details/Stop/Archive/Delete).
5. **Live Grep Alt+M Quickfix buffer**: Saves all matches to `*Quickfix*` [RO] buffer with `file:line:col  content` format, 249 matches saved correctly.
6. **LSP (error) state**: When LSP binary missing: Fresh tries to start it, immediately opens the log file as a [RO] tab, status bar shows `LSP (error)`. Log shows the exact error (e.g., `Unknown binary 'rust-analyzer' in official toolchain`).

### Lessons Learned
See learning_db.md for additions: Lesson 35–43

---

## Run #7 — 2026-05-26

### Status: COMPLETED

### What Was Done
- Built Fresh 0.3.9 binary from source (`cargo build --release --bin fresh`)
- Pulled state from `tui-automated-testing-state` branch (6 prior runs)
- Launched tmux session `fresh-test` (200×50)
- Executed 12 test objectives covering 0.3.9 new features and backlog items

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| TC-DASHBOARD-DEFAULT | **CONFIRMED** | 0.3.9: Dashboard no longer opens by default with `--no-restore` |
| TC-PARA-SELECT | **PASSED** | select_to_paragraph_down/up work via Ctrl+Shift+↓/↑ (CSI 1;6B / CSI 1;6A escape sequences) |
| TC-SETTINGS-CHECKBOX | **RESOLVED** | Checkboxes ARE reachable: ↑↓ arrows navigate to them in the right panel; Enter toggles them |
| TC-CONFIRM-QUIT | **PASSED** | Enable in Settings → "Confirm Quit: [ ]" → Enter → Save; Ctrl+Q shows `Quit Fresh? (y)es, (N)o:` |
| TC-SCROLL-SYNC | **PASSED** | Both splits scroll together when Scroll Sync enabled; confirmed with CHANGELOG.md in both panes |
| TC-AUTO-REVERT | **PASSED** | External file append detected and reverted within ~3s (auto_revert_poll = 2000ms default) |
| TC-NEXT-WINDOW | **TESTED** | "Next Window" returns "Cancelled" when only 1 window open — correct single-window behavior |
| TC-LIVE-GREP-0.3.9 | **PASSED** | New toolbar working: scope toggles (Files/Buffers/Terminals), provider cycle, [buf] tag, Word mode |
| TC-PAGEDOWN-OVERSHOOT | **PASSED** | Basic PageDown/PageUp navigation correct; targeted fix hard to confirm without bug repro file |
| TC-COMPLETION-AUTO-SHOW | **PARTIAL** | Setting toggles correctly; popup requires LSP (off) — not testable without LSP server |
| TC-PARA-MOVE-BUG | **BUG CONFIRMED** | move_to_paragraph_down/up have NO default keybinding and are NOT in command palette → GitHub #2122 filed |
| TC-BUG-2117-CHECK | **STILL OPEN** | Review Diff discard bug NOT fixed in 0.3.9 (not in changelog fixes) |

### Issues Found This Run
- **BUG #2122 filed**: `move_to_paragraph_down/up` actions (0.3.9 feature) have no default keybinding and no command palette entry. Users cannot invoke the feature without manually binding it. Inconsistent with `select_to_paragraph_*` which have `Ctrl+Shift+↓/↑`.

### Key Discoveries This Run
1. **Settings checkboxes via keyboard**: Navigate with ↑↓ arrows (DECCKM) in the right panel, press Enter to toggle. This DOES work — previous run's concern was unfounded. Tab only reaches number/text inputs.
2. **select_to_paragraph escape sequences**: CSI 1;6B = Ctrl+Shift+Down, CSI 1;6A = Ctrl+Shift+Up — confirmed working
3. **Live Grep 0.3.9**: Provider shows as `[ git-grep ]`, `[ rg ]`, `[ grep ]` when cycling with Alt+P. File scope results untagged; Buffer scope results show `[buf]` prefix.
4. **confirm_quit prompt format**: Shows `Quit Fresh? (y)es, (N)o:` at bottom line, requires letter + Enter (N+Enter = stays open, Y+Enter = quits).
5. **Settings search**: Press `/` in Settings UI while in the LEFT panel to trigger search across all setting names (not just visible category).
6. **move_to_paragraph design intent** (from PR #2084): Author intentionally omitted palette commands but appears to have overlooked adding default keybindings — `select_to_paragraph` has bindings but the new `move_to_paragraph` does not.

### Lessons Learned
See learning_db.md for additions: Lesson 29–34

---

## Run #6 — 2026-05-26

### Status: COMPLETED

### What Was Done
- Built Fresh binary from source (`cargo build --release --bin fresh`, ~50s)
- Checked out `tui-automated-testing-state` branch, loaded state from 5 prior runs
- Launched tmux session `fresh_test` (200×50)
- Executed 7 test objectives covering theme editor, auto-save, env manager, tour, review diff, orchestrator, workspace trust

### Test Results Summary
| Test | Result | Notes |
|------|--------|-------|
| TC-THEME-EDITOR (complete) | **PASSED** | Color edit + Save As → custom theme created in ~/.config/fresh/themes/ |
| TC-AUTO-SAVE | **PASSED** | Enable in config; file auto-saved within 8s (5s interval); tab loses asterisk |
| TC-ENV-MANAGER | **PASSED** | Show Status → Activate (direnv) → Deactivate: all 3 commands working |
| TC-TOUR | **PASSED** | Load .fresh-tour.json; navigate Step 1→2→3→4→Exit; each step opens correct file |
| TC-REVIEWDIFF-STAGE | **PASSED** | Stage hunk with `s`: 3 added lines moved to STAGED section |
| TC-ORCHESTRATOR-NEW | **PASSED** | Alt+N → form → Tab×6 to Create Session → session-1 worktree created |
| TC-WORKSPACE-TRUST | **PASSED** | T to trust → status bar confirms "Workspace trusted" |

### Issues Found This Run
- **PENDING BUG INVESTIGATION**: Settings UI checkboxes NOT reachable via Tab key. Tab navigates to number/text inputs and footer buttons, skipping checkboxes (e.g., "Auto Save Enabled"). Needs investigation whether this is by design or a bug.
- **NOTE**: Orchestrator "Create Session" button requires exactly 6 Tab presses from the dialog open state to reach the button. More than 6 = cycles back to checkbox. Important UX discovery.
- **NOTE**: Tour panel button navigation: Tab focuses buttons, Up/Down navigates within tour panel. Pressing Enter when "Next →" is focused advances the tour.

### False Positive Rate: 0% (0 of 0 bugs filed)

### Settings Navigation Discovery
The Settings UI uses a complex navigation model:
- `↑↓` in left panel: navigate sections
- `Tab`: jump to next focusable widget IN THE RIGHT PANEL (number inputs and text inputs only; checkboxes are NOT tab-navigable)
- `Enter` on section: scrolls right panel to show that section
- Auto-save was enabled by directly editing /root/.config/fresh/config.json (demonstrated it persists and works)

---

## Run #1 — 2026-05-26

### Status: COMPLETED (with post-run self-correction)

### What Was Done
- Built Fresh binary from source (`cargo build --release --bin fresh`, 16s)
- Initialized all state files for the first time
- Launched tmux session, executed 30+ test cases across core launch, file ops, editing, search/replace, and views
- Filed 4 GitHub issues
- **Post-run:** Reviewed documentation, discovered 2 of 4 issues were false positives
- Closed #2108 and #2110, updated #2109 and #2111

### Test Results Summary
| Category | Passed | Failed | Notes |
|----------|--------|--------|-------|
| Core launch (TC-001–011) | 11 | 0 | |
| File operations (TC-020–026) | 7 | 0 | |
| Editing (TC-030–035) | 6 | 0 | |
| Search & replace (TC-040–049) | 8 | 1 | TC-043 Shift+F3 broken in tmux (terminal compat) |
| Views & layout (TC-050–058) | 9 | 0 | |
| Issues filed | 4 | — | 2 real (#2109, #2111); 2 false positives (#2108, #2110) |

### Lessons Learned (Run #1)
- Arrow key DECCKM requirement discovered
- Menu highlight verification requires `-e` ANSI capture
- Hot exit causes file restoration on re-launch — not a bug
- "Revert" vs "Reload with Encoding" distinction

---

## Run #29 — 2026-06-10

### Status: COMPLETED

### Objective
New-coverage testing of the brand-new `editor.auto_read_only` config option (commit 9738ac661, not yet in CHANGELOG). #2197 pyright recheck skipped — no fix landed (issue last updated 2026-06-07).

### Build
- Built v0.3.12 from **origin/master @ 2dee83697** in a dedicated worktree (`/home/user/fresh-build`), 6m37s. Master force-updated past Run #28's 67d0c6e6c; new commits: live-diff word-level highlight (2dee83697), auto_read_only (9738ac661), on-save view-keep (f099dd5c5), trust-level reset fix (86d58380b), lsp_enabled master switch (f4ee3630f).

### What Was Done
- Tested `auto_read_only` behavior matrix on a library-path file (`/usr/include/stdio.h`) and a binary (`/bin/true` copy) — all behavior PASSES (see learning_db "auto_read_only / read-only `[RO]` indicator (Run #29)").
- Discovered the documented `[RO]` status-bar indicator is never rendered for any read-only buffer.
- Verified the missing-indicator finding across 5 read-only buffer types (auto library, binary, manual toggle, side-by-side diff, HEAD version) to rule out false positives — `grep [RO]` = 0 on each (content-only false positives excluded).
- Searched GitHub (3 query variations, 0 results), confirmed not in github_issues.md / confirmed_bugs.md.
- **Filed #2309** (bug, tui-agent-auto-bug): "Read-only buffers show no `[RO]` status-bar indicator (documented but never rendered)".

### Result
- 1 NEW backlog item advanced (auto_read_only → DONE). 1 new confirmed bug (BUG-010 / #2309).
- No false positives filed; the broad "anywhere" claim in the issue was verified across all buffer types before finalizing.

### State updates
- github_issues.md (+#2309 row + detail), confirmed_bugs.md (+BUG-010), test_plan.md (priority 4b DONE + Run #29 note), learning_db.md (+Run #29 section), run_log.md (this entry).

---

## Run #30 — 2026-06-10

### Status: COMPLETED

### Objective
New-coverage testing of the brand-new **"Wave Animation" command palette effect** — the freshest commits on origin/master (66e1bcf06 "Add Wave Animation command palette effect" → 232eceed7 "Add wave-animation i18n keys to all locales", 5 commits, not in CHANGELOG). Black-box driven via tmux.

### Build
- Built v0.3.12 from **origin/master @ 232eceed7** in worktree `/home/user/fresh-build`, 6m53s. Master advanced past Run #29's 2dee83697 with the Wave Animation effect series (66e1bcf06, c757980cb, e3df33f6c, 9176f773d, 232eceed7) and a few earlier commits. Version string unchanged (0.3.12).

### What Was Done (black-box, tmux session `wave-run30-12532`, 200x50, `--no-restore wavedemo.txt`)
Read user-facing docs first: `docs/wave-animation-wireframe.txt` (expected behavior) + en.json i18n keys (`cmd.wave_animation`="Wave Animation"; desc="Send a wave through the editor — bounce all content up, down, and sideways"; `wave.triggered`="🌊 Wave! — press any key or move the mouse to stop").

Tested:
1. Palette presence — "Wave Animation" listed, source **builtin**, correct description. PASS.
2. Trigger (Enter) — full-screen particle animation: every painted cell (menu/tab chrome, gutter, text, status bar) snapshotted into "ink" particles; wave crest glyphs (`~ ≈ ∿`) rise from bottom; letters fan out (tight words become loose), drift up + sideways (L/R), bounce. ANSI/plain capture across 9 frames confirmed multiple distinct animation states. PASS.
3. Status message "🌊 Wave! — press any key or move the mouse to stop" rendered (status bar itself displaced as particles mid-flight, then legible once settled). PASS.
4. **Runs until input** — confirmed still animating >3s in; does NOT auto-stop (matches commit 9176f773d "run until input"; the wireframe's older 2.5s-cap note is superseded). PASS.
5. **Settle/restore** — pressing a key stops it and content settles back **exactly** to original: all 5 lines, gutter, chrome restored; cursor Ln 1 Col 1 unchanged; no leftover crest/particle artifacts. PASS (the core correctness claim).
6. **Stop-key consumption** — stop key is consumed, not leaked: stopping with printable `Z` did NOT insert `Z`; stopping with `Ctrl+P` stopped the wave WITHOUT opening the palette. Buffer never marked modified (no `*` on tab). No corruption. PASS.
7. **Empty buffer** ([No Name] via Ctrl+N) — wave runs + restores cleanly, no crash. PASS.

### Result
- 1 NEW backlog item advanced (Wave Animation → DONE, comprehensive PASS).
- **No bugs filed; no false positives.** Feature is correct and robust.
- Minor non-bug note: the "🌊 Wave!…" status message persists until the next status update (normal transient-message behavior, consistent with other Fresh statuses) — not worth filing.
- Open-issue rechecks skipped this run: none of the new commits touch the open agent-filed issues (#2301/#2309 display, #2197 pyright — no fix since 2026-06-07, #2221 ssh, #2307 keybinding, #2135/#2122/#2109/#2111). Per R1, re-verification deferred until a related fix lands. Run #29 already rechecked.

### State updates
- run_log.md (this entry), test_plan.md (new DONE item + Run #30 note), learning_db.md (+"Wave Animation (Run #30)" section), github_issues.md (Last updated bump only — no new issue).

---

## Run #34 — 2026-06-11 — Configurable indent rules `[languages.<id>.indent]` → BUG #2314

### Objective
Per R1 master unchanged at v0.4.0 (`1b5d7f8c8`, same as Runs #31–33) → skipped open-issue rechecks (no fix landed). Per R2 advanced new-coverage candidate (c) from the Run #33 NEXT list: **Configurable per-language indentation rules `[languages.<id>.indent]`** (0.4.0 headline feature).

### Setup
- Built `fresh 0.4.0` from `origin/master @ 1b5d7f8c8` in a throwaway git worktree (`/tmp/fresh-build`), `cargo build --release --bin fresh`.
- Read docs FIRST: CHANGELOG 0.4.0 + `docs/configuration/index.md` → "Customize Auto-Indentation" (5 fields, VS Code-style regex, step = one `tab_size` unit).
- Black-box harness: tmux session `fresh-indent-r34` (220x50). Project `.fresh/config.json` in `/tmp/indent-test` defining custom languages, each with a UNIQUE extension and a SINGLE indent rule, using **non-bracket/non-colon tokens (OPEN/CLOSE/HDR/RET)** that do NOT overlap Fresh's built-in heuristics — this isolates whether the *custom* rule actually fires.

### What Was Done
1. Custom languages load: opening `test.t1`/`test.t3`/… shows the custom filetype name (`incend`/`decr`/…) + `Trusted` in the status bar → language entry IS parsed.
2. `increase_indent_pattern` `OPEN\s*$` (end-anchored): `foo OPEN` → Enter → next line stayed col 0 (NO indent). FAIL.
3. `increase_indent_pattern` `^\s*OPEN\b` (start-anchored): `OPEN` → Enter → next line col 0. FAIL.
4. `decrease_indent_pattern` `^\s*CLOSE\b`: typed `CLOSE` on a built-in-indented line → stayed at indent 4 (no dedent). FAIL.
5. `indent_next_line_pattern` `^\s*HDR\b` (also tested HEADER): reference line → next line col 0. FAIL.
6. `dedent_next_line_pattern` `^\s*RET\b` (also tested return): following line stayed indented. FAIL.
7. **Built-in still fires:** a custom language with NO `indent` block still indents after `:` and `{`, dedents `}`. A custom language WITH an `indent` block STILL gets the built-in colon/brace indent → the custom block is NOT consulted (and does not replace built-in). This is why the doc examples (`[\{\[\(]\s*$`, `:\s*$`) appear to work — they coincide with built-in.
8. Confirmed at BOTH project `.fresh/config.json` AND user `~/.config/fresh/config.json` (filetype `userinc` loaded; `OPEN` increase still no-op).

### Result
- **1 NEW bug filed: [#2314](https://github.com/sinelaw/fresh/issues/2314)** (labels `bug`, `tui-agent-auto-bug`) — "Configurable indentation rules (`[languages.<id>.indent]`) have no effect — all custom patterns are ignored". Med/high value: a 0.4.0 *headline* feature, fully documented, is non-functional for any custom/unrecognized language.
- Searched 4 query variations before filing — no existing/duplicate issue. `self_close_pattern` unobservable (only cancels increase, which never fires).
- Advanced new coverage per R2; R1 honored (no idle re-verification).

### tmux lessons (durable, → learning_db "Configurable indent rules (Run #34)")
- The editor launches in the **tmux session shell's cwd**, not the Bash-tool cwd → must `send-keys 'cd /path' Enter` in the session BEFORE launching, or the project config isn't loaded (symptom: filetype `Text` + `Restricted`).
- `send-keys 'end'` / `'down'` / etc. are interpreted as **key names** — use `send-keys -l 'text'` for literal buffer text.
- Verify indentation with `capture-pane -p | cat -A` (gutter `│ N │ <content>`; spaces between `│ ` and content = indent).

### State updates
- run_log.md (this entry), test_plan.md (Run #34 note + advanced candidate (c)), learning_db.md (+"Configurable indent rules (Run #34)" section), github_issues.md (+#2314 row + Last-updated bump).

### Cleanup
- Killed tmux session `fresh-indent-r34`; removed `/tmp/indent-test`, `/tmp/u1test`, test user config `~/.config/fresh/config.json`; removed build worktree `/tmp/fresh-build`.

---

# Run #35 — 2026-06-11 — Review Diff reworked (0.4.0 flagship): comprehensive PASS + BUG #2315

**Build:** fresh **0.4.0** from `origin/master` @ `1b5d7f8c8` (UNCHANGED since Run #31; release build via worktree `/tmp/fresh-master`, 8m02s). Per **R1** skipped open-issue rechecks (no fix landed; 12 open agent issues, none with new maintainer activity). Per **R2** advanced top new-coverage candidate (a): **Review Diff reworked**.

**Preflight:** playbook intact (all sections present); lessons continuity OK (highest lesson present); GitHub MCP auth live (listed open agent issues). State branch pulled clean.

**Fixture:** real git repo `/tmp/rdiff` (commit signing disabled), 3 dirs (src/core, src/utils, docs), spanning STAGED (helpers.py), UNSTAGED (engine.py multi-change, readme.md), UNTRACKED (new_feature.py file + later assets/ dir).

**Tested (black-box, tmux `fresh-rdiff-r35`, 220×50, ANSI-verified):**
- 3-pane layout (FILES sidebar / diff / COMMENTS): PASS. Sidebar grouped by status→directory, per-file M/? + counts + comment badge `*N`.
- File nav `,`/`.`, hunk nav `n`/`p` (cross-file): PASS.
- Layout toggles `1`=side-by-side / `2`=unified / `0`=auto: PASS — side-by-side KEEPS sidebar+comments.
- Word-level intra-line diff highlight (sub→mul): PASS.
- `/` filter: PASS (applies on Enter, empty clears).
- `?` help → `*Review Keys*` full keyboard reference: PASS.
- Comments `c`: inline bordered box + COMMENTS panel + sidebar `*1` badge + wrapping: PASS. **Persist across editor restart** (per-repo `~/.local/share/fresh/audit/_tmp_rdiff/worktree.json`): PASS. **Export `e`** → `.review/session.md` Markdown (summary + per-file + code context): PASS.
- Stage/unstage `s`/`u` (verified via `git status`; sidebar regroups w/ brief render lag; badge follows file): PASS.
- Refresh `r`: PASS.
- **Review Stash** (dedicated buffer; contents ACCURATE vs `git stash show` — verified, avoided false positive): PASS.

**BUG FOUND + FILED → #2315 (med, `bug`+`tui-agent-auto-bug`):** Review Diff does not expand untracked **directories**. A new dir of files shows as `▾ dir/  +0 -0` + a **blank-named** `?   +0 -0` row (verified byte-for-byte); center shows `(untracked directory)` placeholder, no content; contained files unreviewable. Contrast: untracked file in a tracked dir renders correctly (`+2/-0` + content). Contradicts docs. Repro'd with 2 independent untracked dirs. 3 dup-search variations, none. → confirmed_bugs BUG-012, github_issues row, registry Last-updated bump.

**Pending (NOT filed — tmux harness friction, re-test next run):**
- Delete comment `x`: confirmation prompt appears but `y`/`y`+Enter reported "Delete cancelled"; couldn't confirm the deletion over tmux. Likely prompt-driving issue, not a product defect — needs cleaner key.
- Watch `W`: enables ("Watching for changes"); external edits correctly NOT auto-reloaded (save-triggered per docs); didn't complete in-editor save→reload round-trip.

**State updates:** run_log.md (this entry), learning_db.md (+"Review Diff — reworked (Run #35)"), confirmed_bugs.md (+BUG-012), github_issues.md (+#2315 row + Last-updated), test_plan.md (Run #35 note + candidate (a) marked done).

**Cleanup:** killed tmux `fresh-rdiff-r35`; removed `/tmp/rdiff`; left build worktree `/tmp/fresh-master` (reusable next run; remove if stale).

**NEXT new-coverage (Run #36+, top-down):** finish (a) Review Diff residue — delete-comment `x` confirm-key + watch `W` in-editor save→reload round-trip + **Review Range** (`main..HEAD`) entry + multi-line comment input + `v` line-selection staging + `d`/`D` discard (destructive; do last on a throwaway repo); then (d) '+' new-tab popup / terminal Ctrl+Click path open / OSC 7 cwd; (e) color-transition animation on theme switch; (f) GDScript language support (#2238). Then #2197 only if a fix lands.

---

## Run #36 — 2026-06-11 — Review Diff residue CLOSED (2 bugs filed) — v0.4.0 @ 1b5d7f8c8

**Preflight:** Synced state branch. Master force-updated back to `1b5d7f8c8` (v0.4.0) — UNCHANGED since Run #31 → per R1 skipped open-issue rechecks (13 open agent issues, no fix landed). Auth live (listed open issues). Built `fresh` 0.4.0 from `/tmp/fresh-master` worktree @ origin/master (6m28s). Per R2 advanced new coverage = finish Run #35 Review-Diff residue (a).

**Fixture:** `/tmp/rdiff36` real git repo — `src/calc.py` (MM staged+unstaged), `README.md` (M unstaged), `src/newfile.py` (?? untracked). Session `rdiff36_<pid>`, 200x50.

**Results:**
- **Multi-line comment** — single-line bottom prompt `Comment on L<n>:`, Enter submits; renders inline box + COMMENTS entry `calc.py:3` + `*1` badge. Docs call it a "line comment" → single-line is BY DESIGN, **not a bug**.
- **Delete comment `x` — RESOLVED, PASS.** Run #35 was wrong that it's a y/n prompt. With diff cursor on the commented line, `x` → **Delete / Cancel selectable menu** (Delete highlighted, ANSI `48;5;25m`) → **Enter** → `Deleted`, panel cleared, badge gone.
- **`v` line-level visual stage/unstage/discard — BUG → #2317 (med).** Cursor ANSI-verified on real +/- line: `v`+`s` & `v`+`d` → `Selection has no add/remove lines or crosses hunk boundary` (no-op); `v`+`u` → `Patch failed: … patch does not apply`. Tried single `+`, full `-`/`+` (v+j), pure single-add (README) — all fail. CONTROL: plain hunk `s` → `Hunk staged` (git-verified); `u`/`d` work too. Only the `v` path is dead. 3 dup-searches (only closed hunk-level #2117). → confirmed_bugs BUG-013, github_issues row.
- **Hunk discard `d` — PASS** (Discard/Cancel menu → Enter → `Hunk discarded`, README reverted on disk, git clean). Reconfirms #2117 fix holds in 0.4.0.
- **File discard `D` — BUG → #2318 (med).** On UNSTAGED file: PASS (reverts to HEAD). On UNTRACKED file: menu "Delete file". On **fully-staged** file (`M `): reports `Discarded: <file>` but staged change PERSISTS (git status / `git diff --cached` / disk all unchanged) — `D` only touches working tree, never index. 3 dup-searches, none. → confirmed_bugs BUG-014, github_issues row. (Note: `D`/`d`/`x` only fire when cursor on a hunk CONTENT row — group/blank row = silent no-op.)
- **Review Range — PASS.** Palette `Review Range (Commit or Branch)` → prompt `Review (range A..B or commit SHA):` prefilled `HEAD` + commit picker. `HEAD~1..HEAD` → buffer `*Review HEAD~1..HEAD*`, single `HEAD~1..HEAD (1)` group, content matches `git diff HEAD~1..HEAD` (+3/-0 power fn), status `… working tree not included`, legend OMITS `[s]/[u]/[d]`.
- **Watch `W` — PASS.** `W` → `Watching for changes`. Opened README in a buffer, edited, **Ctrl+S** → `*Review Diff*` auto-reloaded README +2→+4 live (no manual `r`). Save-triggered reload confirmed (matches docs; external fs edits ignored per Run #35).

**State updates:** run_log (this), learning_db (+"Review Diff — residue resolved (Run #36)"; Run #35 pending marked resolved), confirmed_bugs (+BUG-013/#2317, +BUG-014/#2318), github_issues (+2 rows + Last-updated bump), test_plan (Run #36 note; residue (a) fully done).

**Cleanup:** killed tmux `rdiff36_*`; removed `/tmp/rdiff36`; left build worktree `/tmp/fresh-master` (reusable; remove if stale).

**NEXT new-coverage (Run #37+, top-down):** (d) '+' new-tab popup / terminal Ctrl+Click path open / OSC 7 cwd; (e) color-transition animation on theme switch; (f) GDScript (#2238). Then #2197 only if a fix lands.

---

## Run #41 — 2026-06-22 — Clipboard strips ANSI on default copy (`7d636f1de`) — COMPREHENSIVE PASS, no bug — v0.4.1 @ 33e2ed130

> Note: run_log entries for Runs #37–#40 live in `test_plan.md` (top "Note:" block) + `learning_db.md` topic sections, not here (a prior run stopped appending to run_log). This entry resumes the run_log.

**Preflight:** Synced state branch (clean). Master UNCHANGED at **v0.4.1** (`33e2ed130`) since Run #40 → per **R1** skipped open-issue rechecks (no new fix landed; 11 open agent issues, none with new fix activity). Playbook intact (all sections). Lessons continuity OK (topic-based format; recent entries #2405/virtual-buffer/Review-Diff present). GitHub MCP auth live (listed 11 open `tui-agent-auto-bug` issues). Per **R2** advanced top new-coverage candidate: `7d636f1de` "fix(clipboard): strip ANSI escape codes from default copy".

**Build:** rebuilt release `fresh` 0.4.1 from origin/master worktree `/tmp/fresh-master` @ `33e2ed130` (container was reclaimed; worktree + binary gone, rebuilt from scratch ~7m, exit 0).

**Fixture:** `/tmp/ansi41/` text files with raw ESC bytes (verified via `cat -A`/`od -c`): `colored.txt` = `\033[31mRED\033[0m middle \033[32mGREEN\033[0m end` / `plain line two` / `\033[1;34mBOLDBLUE\033[0m tail`; plus `cf.txt`. Session `ansi_copy_r41`, 200×50, ANSI-verified.

**Black-box results (all default-copy paths strip ANSI; the styled command keeps it):**
- **ANSI-aware render confirmed** (`capture -e`): RED=`38;5;160`, GREEN=`38;5;2`, BOLDBLUE=bold+`38;5;25` — the file's escape bytes render as zero-width styling (not shown literally), so the buffer genuinely holds ESC bytes. This is the precondition the fix targets.
- **Selection copy** (Home→Shift+End→Ctrl+C, status "Copied"): pasted line is pure ASCII `RED middle GREEN end` (`od -c`: no `\033`), originals still carry `^[[31m`. PASS.
- **Whole-line copy** (no selection, Ctrl+C on BOLDBLUE line, status "Copied line"): pasted as `BOLDBLUE tail\n`, zero escape bytes. PASS.
- **Block/rectangular copy** (Alt+Shift+Down×2 + Alt+Shift+Right×8, status "Copied"): block fragments (`RED`/`plain li`/`B`) all escape-free; block PASTE did VS Code column-paste (#1057) inserting the rectangle across lines — only the 2 original colored lines retained ESC (`grep -c $'\033'`=2). PASS.
- **Copy with Formatting (contrast/control)** — palette builtin "Copy selection with syntax highlighting colors (as rich text)"; opens a **theme picker** (dark/dracula/high-contrast/light/nord/nostalgia/solarized-dark/terminal). Picked high-contrast → status "Copied as plain text" → pasted bytes RETAIN ANSI: `\033[31mRED\033[0m middle \033[32mGREEN\033[0m end`. Confirms the strip is specific to the default path, not a blanket removal. PASS.
- **Not separately exercised:** the commit's 4th path "composite-buffer copy" (constructing a composite/virtual buffer holding ANSI black-box is impractical); the 3 default paths above + the formatted-copy contrast give a confident overall PASS.

**Verdict:** `7d636f1de` works exactly as documented in its commit (default copy strips ANSI on selection/whole-line/block; "Copy with Formatting" keeps styling). **No bug, no false positive, no issue filed.** No notification (all-healthy run).

**State updates:** run_log (this entry), learning_db (+"Clipboard ANSI strip on default copy (Run #41)"), test_plan (Run #41 note; clipboard candidate marked done). No confirmed_bugs / github_issues change.

**Cleanup:** killed tmux `ansi_copy_r41`; removed `/tmp/ansi41`; left build worktree `/tmp/fresh-master` (reusable next run; remove if stale).

**NEXT new-coverage (Run #42+, top-down, prefer freshest 0.4.1):** `6df567f04` LSP `textDocument/implementation` (needs clangd on a small C iface/impl project); `77360e6c6` Shift+letter binding when terminal omits SHIFT; `e4a554347` terminal hides scrollbar/reclaims column; then (d) '+' new-tab popup / terminal Ctrl+Click / OSC 7; (e) theme color-transition animation; (f) GDScript (#2238). Then #2197 only if a fix lands.

---

## Run #42 — 2026-06-22 — vi mode `cw` eats trailing whitespace (Vim-compat) → BUG #2437 — v0.4.1 @ eb3a349e6

**Preflight:** Synced state branch (clean). Master FORCE-UPDATED past Run #41's `33e2ed130` → **`eb3a349e6`** (still v0.4.1; 5 new commits from merge-base `33e2ed130`: `b82b9b8b4` feat add Vim compatibility options to vi mode, `471826514` fix align vi compatibility motions with Vim, `5418668b5`/`99dca87ce` render refactors no user-facing change, `eb3a349e6` terminal scrollback backing-file in remote mode). Per **R1**: version changed but NONE of the 5 commits fixes an OPEN agent issue → skipped open-issue rechecks; per **R2** advanced top NEW-coverage candidate = the two vi-mode commits (freshest user-facing feature). Playbook intact (all sections). Lessons continuity OK (topic format; recent #2405/clipboard-ANSI/Review-Diff present). GitHub MCP auth live (search_issues returned). 

**Build:** rebuilt release `fresh` 0.4.1 from origin/master worktree `/tmp/fresh-master` @ `eb3a349e6` (container reclaimed; rebuilt from scratch, exit 0, 32 MB binary).

**Fixture:** `/tmp/vitest/motions.txt` = `hello world foo bar` / `  indented second line` / `third` / `last line of file`. Session `vimr42`, 200×50. Vi mode via `Ctrl+P`→"Toggle Vi mode".

**Black-box results:**
- **Settings discovery:** Settings → "Plugin: vi_mode" exposes only `ArrowKeys`/`AutoStart`/`SearchWordUnderCursor` — NO "Vim compatibility" toggle; no palette "compat" command; docs §Vim Mode is 1 line. The new "compatibility options" are not user-discoverable → IMP-022 (low, doc/UX batch).
- **Motions that PASS (Vim-correct):** `$` lands on the last char (col 19 of 19-char line, not col 20); `l` won't move past the last char; `dw` deletes word + trailing space (`hello world foo bar`→`world foo bar`); `gg`/`0`/`G` position correctly.
- **BUG → #2437 (med, `bug`+`tui-agent-auto-bug`): `cw` eats the trailing whitespace** instead of acting like `ce`. Vim `:help cw` special case: on a non-blank `cw`==`ce`. Fresh: `cw`+`X` → `Xworld foo bar` (space gone) vs `ce`+`X` → `X world foo bar` (correct). Reproduced 2/2 at word start; mid-word (col 3) `cw`+`X` → `heXworld foo bar` (Vim: `heX world foo bar`). Control `dw` correctly keeps the space-eating behavior — only `cw` deviates. 3 dup-search variations (`cw vi mode change word`, `vi mode cw trailing whitespace`, `vim compatibility motion`), 0 hits. → confirmed_bugs BUG-016, github_issues row, registry Last-updated bump.

**NOT filed (harness flakiness, see learning_db):** `de`/`d{motion}` sent over tmux gave inconsistent results — combined `de` arg = NO-OP; split `d`+`e` over-deleted. Cursor-position polling (`display-message #{cursor_x}`) lagged/duplicated. Only stable buffer-effect results were filed (`cw`/`ce`, 2/2). Did not chase the flaky operator timings into false positives (R4).

**State updates:** run_log (this entry), learning_db (+"vi mode Vim compatibility options + motions (Run #42)"), confirmed_bugs (+BUG-016/#2437), github_issues (+#2437 row + Last-updated bump), potential_improvements (+IMP-022), test_plan (Run #42 note + advanced vi-compat candidate).

**Cleanup:** killed tmux `vimr42`; removed nothing destructive; left `/tmp/vitest` + build worktree `/tmp/fresh-master` (reusable next run; remove if stale).

**NEXT new-coverage (Run #43+, top-down, prefer freshest 0.4.1):** finish vi-compat motion sweep with a more robust harness (text-objects `daw`/`diw`/`ci"`, `cc`, counts `3w`/`2dd`, `$`-sticky-column on `j`, `>>`/`<<` indent) — characterize whether other motions also deviate; `eb3a349e6` terminal scrollback backing-file (remote mode — hard to black-box); then (d) '+' new-tab popup / terminal Ctrl+Click / OSC 7; (e) theme color-transition animation; (f) GDScript (#2238). Then #2197 only if a fix lands.

---

## Run #43 — 2026-06-22 — vi-mode motion sweep: 2 Vim-compat bugs → #2438 (indent ops) + #2439 (quote text-objects) — v0.4.1 @ 8ee2baf31

**Preflight:** Synced state branch (clean). Master FORCE-UPDATED past Run #42's `eb3a349e6` → **`8ee2baf31`** (still v0.4.1; only 1 new commit: `8ee2baf31` "refactor(editor): extract plugin loading out of with_options God constructor" — pure refactor, no user-facing change). Per **R1**: that commit fixes no OPEN agent issue → skipped open-issue rechecks. Per **R2** advanced top NEW-coverage candidate from Run #42 NEXT = finish the vi-compat motion sweep (text-objects, counts, indent ops) with a more robust harness. Playbook intact (all sections). Lessons continuity OK (topic format; recent vi-mode #2437 / clipboard-ANSI / Review-Diff present). GitHub MCP auth live (search_issues returned).

**Build:** rebuilt release `fresh` 0.4.1 from origin/master worktree `/tmp/fresh-master` @ `8ee2baf31` (container reclaimed; rebuilt from scratch ~8m, exit 0, 32 MB binary).

**Fixture:** `/tmp/vitest43/motions.txt` = `hello world foo bar` / `the "quick" brown fox` / `alpha beta gamma delta epsilon` / `    indented line here` / `one`..`five`. Session `vimr43`, 200×50, ANSI-verified. Vi mode via `Ctrl+P`→"Toggle Vi mode".

**Black-box results (verify by BUFFER-TEXT EFFECT per Run #42 lesson; send operator keys SEPARATELY with ~0.4s gaps — combined args no-op):**
- **Text-objects `diw`/`daw` — PASS (Vim-correct).** `diw` on `world` → `hello  foo bar` (word removed, 2 spaces kept); `daw` → `hello foo bar` (word + 1 trailing space removed).
- **Count motions `3dw` / `2dd` — PASS.** `3dw` from line start → `bar` (3 words deleted); `2dd` on `one`/`two` → those 2 lines removed, `three` becomes line 5. (Per-line capture lags one render — re-capture the full pane to confirm.)
- **`ci"`/`di"` INSIDE quotes — PASS.** Cursor on `q` of `"quick"`: `di"` → `the "" brown fox`; `ci"` → enters INSERT, `the "WXYZ" brown fox`.
- **BUG → #2439 (med, `bug`+`tui-agent-auto-bug`): quote text-objects don't search forward on the line.** `di"`/`ci"` from column 1 (before the quote, same line) = silent NO-OP (2/2). Vim's `i"`/`a"` search forward on the current line for the quoted string, so `ci"` from a line's start (the common case) should work. Implemented but missing the forward-search rule. `"` key transmits fine (verified in INSERT). Workaround: move inside quotes first. 5 dup-search variations, 0 hits.
- **BUG → #2438 (med, `bug`+`tui-agent-auto-bug`): indent operators `>>`/`<<` + visual `>`/`<` are no-ops.** `>>` in NORMAL: line unchanged, no indent, status stays `-- NORMAL --`. `<<` on `    indented line here`: 4 leading spaces remain (cursor confirmed on Ln 4). Visual `V` then `>`: line unchanged AND editor stays `-- VISUAL LINE --` (Vim would indent + return to NORMAL). `>` key transmits fine (literal `>` inserts in INSERT mode) → indent operator family simply unhandled in vi mode, no feedback. Workaround: leave NORMAL and use editor indent (`Tab`). 5 dup-search variations, 0 hits.

**Harness notes (reconfirm Run #42 lessons):** operator+motion combined into ONE send-keys arg = NO-OP; send each key separately ~0.4s apart and read the resulting line. capture-pane lags ~1 render — re-capture after a beat or read full pane. Status-bar `Ln/Col` lags one keypress (#2301). Restore between trials with `u` bursts.

**Notification:** sent (2 new confirmed bugs filed) — a substantive find a maintainer would act on.

**State updates:** run_log (this entry), learning_db (+"vi mode motion sweep (Run #43)" topic section), confirmed_bugs (+BUG-017/#2438, +BUG-018/#2439), github_issues (+2 rows + Last-updated bump), test_plan (Run #43 note + advanced vi-compat sweep candidate).

**Cleanup:** killed tmux `vimr43`; removed `/tmp/vitest43`; left build worktree `/tmp/fresh-master` (reusable next run; remove if stale).

**NEXT new-coverage (Run #44+, top-down, prefer freshest 0.4.1):** finish remaining vi-compat motions NOT yet characterized — `$`-sticky/desired-column on `j`/`k`, `p`/`P` paste (charwise vs linewise), `r`/`R` replace, `>>`-with-count, `gU`/`gu`/`g~` case ops, `f`/`t`/`;`/`,` find-char, `%` match — to see if more deviate; then (d) '+' new-tab popup / terminal Ctrl+Click / OSC 7; (e) theme color-transition animation; (f) GDScript (#2238). Then #2197 only if a fix lands.

---

## Run #44 — 2026-06-22 — vi-mode sweep: find-char broken w/ operators (#2441) + `j`/`k` past-EOL → x joins lines (#2442) — v0.4.1 @ 3b8c2eca1

**Preflight:** Synced state branch (clean, up to date). Master FORCE-UPDATED past Run #43's `8ee2baf31` → **`3b8c2eca1`** (still v0.4.1; 5 new commits, ALL e2e test-deflake fixes — `7b95b41b2`/`a50cfc4a8` open_folder instrumentation, `a430d2d71` loaded-parent wait, `36678bf8f` vi paste/dot-repeat wait, `3b8c2eca1` vi paragraph-motion wait — NO user-facing change). Per **R1**: no commit fixes an OPEN agent issue → skipped open-issue rechecks (14 open agent issues, listed via MCP). Per **R2**: continued the vi-compat motion sweep from Run #43's NEXT. Playbook intact (all sections). Lessons continuity OK. GitHub MCP auth live (`get_me`/`list_issues`/`search_issues` returned).

**Build:** built release `fresh` 0.4.1 from a fresh origin/master worktree `/tmp/fresh-master` @ `3b8c2eca1` (container reclaimed; from-scratch build, exit 0). Removed worktree at cleanup.

**Fixture:** `/tmp/vi44/motions.txt` (10 lines: `hello world foo bar baz` / `short` / `a longer line with many words here` / `    indented four spaces` / `MixedCase Words HERE` / `find the comma, and stop here` / `nested (parens [and brackets] inside) end` / `line eight`..`line ten`). Session `vi44test` 220×50. Vi mode via `Ctrl+P`→"Toggle Vi mode"→Enter (status `Vi mode enabled - NORMAL`).

**Black-box results (verified by BUFFER-TEXT EFFECT; operator+motion keys sent SEPARATELY ~0.4–1.2s apart):**
- **BUG → #2441 (med): find-char motions broken with operators; `;`/`,` no-op.** Pure `f`/`t` cursor motions WORK (`fr`→col9 then `x` deletes `r` of world; `t,`→col before comma). But `d`/`c`+`f`/`t` HANG in `-- OPERATOR (d/c) --` forever — `dfr`/`dt,`/`cfr` all stuck, target char + even `Z` swallowed, only Esc recovers; reproduced at 0.4s AND deliberate 1.2s gaps → NOT a timing race. When the find target is itself a motion key the `f` is dropped and the op degrades: `dfw`→`world foo bar baz` (=plain `dw`, NOT `orld...`). `;`/`,` repeat-find = no-op (proved via `fo`(col5)+`;`+`x` deleting col5 `o` → `hell world...`, cursor never advanced). 3 dup-search variations, 0 hits.
- **BUG → #2442 (med): vertical `j`/`k` onto a shorter line parks cursor past EOL → `x` joins lines.** `$` on line1 (col23, correct) then `j` onto `short` (5 chars) → status `Ln2, Col6` (=len+1); `x` deletes the NEWLINE → `shorta longer line with many words here` (lines joined). 3/3: `$`+j, `l`-to-col10+j, `k` from line3. HORIZONTAL clamp correct: `$` direct on `short`→col5→`x`=`shor`; `l` stops at last char → vertical column-clamp off-by-one vs `$`/`l`. 2 dup-search variations, 0 hits.
- **PASS / Vim-correct (do NOT re-test, R1):** `%` match-bracket (on `(` of line7 → jumps to OUTER `)`, skips nested `[...]`, `x` confirmed); `r`+`X` (replace single char `h`→`X`); `~` (toggle case + advance cursor); `p`/`P` linewise (`yy`+`p` dup below / `P` above, cursor on new line, status `Pasted`); `p` charwise (`x`(`h`)→`ello`, `p`→`ehllo`); `$`/`l` horizontal clamp (no past-EOL).
- **NOT IMPLEMENTED (logged → IMP-023, NOT filed — missing-feature, candidate for one consolidated issue):** `R` Replace/overtype mode (stays NORMAL; next key runs as normal cmd — `R`+`A`→append-at-EOL, `R`+`x`→delete char); `gU`/`gu`/`g~` case OPERATORS (`gUw` no text change, `w` only moved cursor). Single-char `~` works.

**Harness notes (learning_db):** find-char+operator hang is REAL not timing (held 1.2s). Verify by buffer-text effect, not cursor polling. Heavy `u` undo across trials desynced the buffer (an `R` test got a corrupted baseline) → recovered via `Ctrl+Q`→`d` discard + relaunch (file on disk pristine, never saved) + re-Toggle Vi mode. Launch reliably via `send-keys -l '<binpath> --no-restore file'` + Enter (a `cd && bin` one-liner via send-keys sometimes didn't start the editor).

**Notification:** sent (2 new confirmed med-sev Vim-compat bugs filed) — substantive findings a maintainer actively working on vi compat would act on.

**State updates:** run_log (this entry), learning_db (+"vi mode motion sweep — find-char, paste, case ops, vertical clamp (Run #44)"), confirmed_bugs (+BUG-019/#2441, +BUG-020/#2442, +PENDING vi-missing-commands), github_issues (+2 rows + Last-updated bump), potential_improvements (+IMP-023), test_plan (Run #44 note + Run #45 NEXT).

**Cleanup:** killed tmux `vi44test`; removed `/tmp/vi44`; removed build worktree `/tmp/fresh-master`.

**NEXT new-coverage (Run #45+, top-down, prefer freshest 0.4.1):** finish characterizing the vi-compat gap set for a consolidated "missing vi commands" issue (`R`, `gU`/`gu`/`g~`, plus check count+`gg`/`G`, `*`/`#` word-search, `n`/`N`, `o`/`O`, `s`/`S`, `D`/`C`/`Y`, `.` dot-repeat — recent deflake commit implies it exists, `>>`-with-count after #2438); then (d) '+' new-tab popup / terminal Ctrl+Click / OSC 7; (e) theme color-transition; (f) GDScript (#2238). Then #2197 only if a fix lands.

---

## Run #45 — 2026-06-22 — vi-command sweep: dot-repeat of `o`/`O`/`a`/`A` corrupts buffer → BUG #2443; most common vi commands confirmed WORKING — v0.4.1 @ 1c6bd8ce9

**Preflight:** Synced state branch (clean, up to date). Master FORCE-UPDATED past Run #44's `3b8c2eca1` → **`1c6bd8ce9`** (still v0.4.1; only 1 new commit `1c6bd8ce9` "refactor(actions): extract inline editing arms from action_to_events" — pure refactor, no user-facing change). Per **R1**: that commit fixes no OPEN agent issue → skipped open-issue rechecks (16 open `tui-agent-auto-bug` issues, listed via MCP). Per **R2**: continued the vi-command sweep from Run #44's NEXT. Playbook intact (all sections). Lessons continuity OK (topic format; recent #2441/#2442 vi-mode entries present). GitHub MCP auth live (`list_issues`/`search_issues` returned).

**Build:** built release `fresh` 0.4.1 from a fresh origin/master worktree `/tmp/fresh-master` @ `1c6bd8ce9` (container reclaimed; from-scratch build, exit 0, 6m44s, 32 MB binary).

**Fixture:** `/tmp/vi45/motions.txt` (10 lines: `hello world foo bar baz` / `short` / `alpha beta gamma delta epsilon` / `    indented four spaces` / `MixedCase Words HERE` / `find the comma, and stop here` / `foo bar foo baz foo qux` / `line eight here`..`last line ten`). Session `vi45test` 220×50. Vi mode via `Ctrl+P`→"Toggle Vi mode"→Enter (status `Vi mode enabled - NORMAL`). Trust dialog appeared on first launch (cwd = repo root) → "Keep Restricted (K)"; Restricted is fine for vi-mode testing.

**Black-box results (verified by BUFFER-TEXT EFFECT; keys sent SEPARATELY ~0.3–0.5s apart):**
- **BUG → #2443 (med): dot-repeat (`.`) of cursor-repositioning insert commands corrupts the buffer.** Canonical: cursor on line 1, `o`+`abc`+`Esc` → line2=`abc` (correct); `.` → line2=`abhello world foo bar baz`, line3=`abcc` (line 1's content INJECTED; expected line3=`abc`). Reproduced across 4 commands: `A`+`X`→`shortX` then `.`→`shortshortXX` (exp `shortXX`); `A`+`QZQ`→`shortQZQ` then `.`→`shortQZshortQZQQ` (exp `shortQZQQZQ`); `a`+`Y` at col1→`sYhort` then `.`→`ssYYhort` (exp `sYYhort`); `o`+`NEW` then `.` also mangled. CONTROL works: `i`+`AB`→`ABalpha...` then `.`→`AABBalpha...` (correct); `x`,`.`,`.` deletes successive chars (correct). So `.` replay is broken ONLY for the insert commands that reposition the cursor before inserting (`o`/`O`/`a`/`A`); `i`/`x` fine. NOT timing (deterministic, selective by command). 3 dup-search variations (`vi mode dot repeat`, `vi mode repeat command corrupt`, `vi dot . insert append open line`), 0 hits. → confirmed_bugs BUG-021, github_issues row + Last-updated bump.
- **PASS / Vim-correct (do NOT re-test, R1):** `o`/`O` (open line below/above, enter INSERT — `o`+text below current line, `O` above); `s` (substitute char → delete char + INSERT, `s`+`X`→`Xello...`); `S` (substitute whole line → clears line + INSERT); `D` (delete to EOL — from col1 clears line keeping lines 2-10 intact, status `Cut`; mid-line at `world`→`hello `; undo restores); `C` (change to EOL — at `world`→INSERT, `NEWTAIL`→`hello NEWTAIL`); `Y` (linewise yank like Vim's `Y`==`yy` — `Y` then `p` duplicates the line); `3G`/count+`G` (jump to line 3, `x` confirmed); `*` (search word under cursor fwd — on line1 `foo`→jumps to line7 `foo`, `x` confirmed); `#` (search word backward); `n`/`N` (repeat search fwd/back — verified hitting successive `foo` on line7). `i`-dot-repeat + `x`-dot-repeat correct (controls above).
- **Still NOT implemented (unchanged from Run #44 → IMP-023, NOT filed):** `R` (Replace/overtype mode) and `gU`/`gu`/`g~` case operators. The sweep shows the gap list is now SMALL (only these two) — most common commands work — so a consolidated "missing vi commands" issue is not yet warranted; kept in IMP-023.

**Harness notes (reconfirm + new):** (1) `yy`/operator-doubled keys MUST be sent as SEPARATE keystrokes — `'yy'` in ONE send-keys arg stuck in `-- OPERATOR (y) --` (2nd `y` swallowed). (2) Status-bar `Ln`/`Col` lags ~1 keypress (#2301 family) — a motion's status often shows the PRE-move position; ALWAYS verify by buffer effect (`x`/`d` then read the line), never by the status line. (3) `u` over-undo across many trials desyncs the buffer; when state gets murky, `Ctrl+Q`→`d` (discard) + relaunch for a clean fixture (file on disk stays pristine if never saved) + re-Toggle Vi mode. (4) Relaunch flakiness: after a discard the editor sometimes drops to shell without restarting; a leftover `Toggle Vi mode` string from a failed palette-open can pollute the next shell line — `Ctrl+C`+`clear` before re-sending the launch command fixes it. (5) FALSE ALARM avoided: an early `w`→`D`→`u` sequence left the whole buffer apparently empty and undo seemed broken; a clean controlled re-test showed `D` and undo BOTH correct — the empty-buffer was a one-off undo/status desync, NOT a bug. Lesson: never file from a murky multi-undo state; reproduce from a fresh launch.

**Notification:** sent (1 new confirmed med-sev data-corruption bug — dot-repeat of the most common insert workflow corrupts the file — a maintainer actively on vi-compat would act on it).

**State updates:** run_log (this entry), learning_db (+"vi mode command sweep — o/O/s/S/D/C/Y, dot-repeat, search-word (Run #45)" topic section), confirmed_bugs (+BUG-021/#2443 + Run #45 update to the PENDING missing-commands note), github_issues (+#2443 row + Last-updated bump), test_plan (Run #45 note + Run #46 NEXT).

**Cleanup:** killed tmux `vi45test`; removed `/tmp/vi45`; removed build worktree `/tmp/fresh-master`.

**NEXT new-coverage (Run #46+, top-down, prefer freshest 0.4.1):** vi-compat is now well-characterized (6 bugs filed #2437–#2439, #2441–#2443; gap list = `R` + `gU`/`gu`/`g~` in IMP-023). Pivot to NON-vi coverage: (d) '+' new-tab popup / terminal Ctrl+Click / OSC 7 hyperlinks; (e) theme color-transition animation; (f) GDScript syntax (#2238); plus check if `R`/`gU` get a consolidated issue only once more gaps surface. Then #2197 pyright only if a fix lands.
